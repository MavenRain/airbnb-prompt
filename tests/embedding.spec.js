const { test, expect } = require("@playwright/test");

const stubEmbedJs = `
const EmbedBridge = (() => {
    const listeners = { response: [] };
    function emit(channel, payload) {
        for (const fn of listeners[channel] || []) fn(payload);
    }
    function hashVec(text, dim) {
        let h = 2166136261;
        for (let i = 0; i < text.length; i++) {
            h = (h ^ text.charCodeAt(i)) >>> 0;
            h = Math.imul(h, 16777619) >>> 0;
        }
        const v = new Array(dim);
        let s = h || 1;
        for (let i = 0; i < dim; i++) {
            s = (Math.imul(s, 1664525) + 1013904223) >>> 0;
            v[i] = ((s % 2000) / 1000) - 1;
        }
        const norm = Math.sqrt(v.reduce((a, x) => a + x * x, 0)) || 1;
        return v.map((x) => x / norm);
    }
    function embed({ id, text }) {
        Promise.resolve().then(() => {
            window.__embedCount = (window.__embedCount || 0) + 1;
            window.__embedTexts = window.__embedTexts || [];
            window.__embedTexts.push({ id, text });
            emit("response", { id, vector: hashVec(text, 384) });
        });
    }
    function on(channel, fn) {
        (listeners[channel] = listeners[channel] || []).push(fn);
    }
    return { embed, on };
})();
window.EmbedBridge = EmbedBridge;
`;

const stubLlmJs = `
const SlmBridge = (() => {
    const listeners = { status: [], response: [] };
    function emit(channel, payload) {
        for (const fn of listeners[channel] || []) fn(payload);
    }
    function on(channel, fn) {
        (listeners[channel] = listeners[channel] || []).push(fn);
    }
    function init(args) {
        window.__slmInitCalls = (window.__slmInitCalls || 0) + 1;
        Promise.resolve().then(() => emit("status", { event: "ready" }));
    }
    function generate(args) {
        window.__slmGenerateCalls = window.__slmGenerateCalls || [];
        window.__slmGenerateCalls.push(args);
    }
    return { init, generate, on, hasWebGPU: () => true };
})();
window.SlmBridge = SlmBridge;
`;

const stalledEmbedJs = `
window.__pendingEmbeds = [];
const EmbedBridge = {
    embed: ({ id, text }) => { window.__pendingEmbeds.push({ id, text }); },
    on: () => {},
};
window.EmbedBridge = EmbedBridge;
`;

async function routeFastEmbed(page) {
    await page.route("**/embed.js", (route) =>
        route.fulfill({ contentType: "application/javascript", body: stubEmbedJs })
    );
}

async function routeStalledEmbed(page) {
    await page.route("**/embed.js", (route) =>
        route.fulfill({ contentType: "application/javascript", body: stalledEmbedJs })
    );
}

test.beforeEach(async ({ page }) => {
    await page.route("**/llm.js", (route) =>
        route.fulfill({ contentType: "application/javascript", body: stubLlmJs })
    );
});

test("builds exemplar, ontology, and listing corpora", async ({ page }) => {
    await routeFastEmbed(page);
    await page.goto("/");

    await page.waitForFunction(() => (window.__embedCount || 0) >= 47, null, {
        timeout: 15000,
    });

    const texts = await page.evaluate(() =>
        window.__embedTexts.map((e) => e.text)
    );
    expect(texts.length).toBeGreaterThanOrEqual(47);
    expect(texts).toContain("long weekend in Lisbon for two in October");
    expect(texts).toContain("BCN means Barcelona");
    expect(
        texts.some((t) => t.startsWith("Modern machiya in central Kyoto in Kyoto"))
    ).toBe(true);
});

test("retrieves top-K exemplars and ontology before slm dispatch", async ({ page }) => {
    await routeFastEmbed(page);
    await page.goto("/");

    await page.waitForFunction(() => (window.__embedCount || 0) >= 47, null, {
        timeout: 15000,
    });

    await page.fill("textarea.prompt", "long weekend in Lisbon for two in October");

    await page.waitForFunction(
        () => (window.__slmGenerateCalls || []).length > 0,
        null,
        { timeout: 15000 }
    );

    const calls = await page.evaluate(() => window.__slmGenerateCalls);
    expect(calls).toHaveLength(1);

    const sys = calls[0].system;
    const exemplarCount = (sys.match(/^PROMPT:/gm) || []).length;
    expect(exemplarCount).toBeGreaterThan(0);
    expect(exemplarCount).toBeLessThanOrEqual(4);

    const ontologyBlock = sys.split("ONTOLOGY HINTS:\n")[1] || "";
    const ontologyLines = ontologyBlock
        .split("\n")
        .filter((l) => l.startsWith("- "));
    expect(ontologyLines.length).toBeGreaterThan(0);
    expect(ontologyLines.length).toBeLessThanOrEqual(6);

    const cardCount = await page.locator(".listing-card").count();
    expect(cardCount).toBeGreaterThan(0);
    expect(cardCount).toBeLessThanOrEqual(5);
});

test("falls back to full lists when corpora are not yet ready", async ({ page }) => {
    await routeStalledEmbed(page);
    await page.goto("/");

    await page.waitForFunction(
        () => (window.__pendingEmbeds || []).length >= 47,
        null,
        { timeout: 15000 }
    );

    await page.fill("textarea.prompt", "BCN early June, family of 4");

    await page.waitForFunction(
        () => (window.__slmGenerateCalls || []).length > 0,
        null,
        { timeout: 15000 }
    );

    const calls = await page.evaluate(() => window.__slmGenerateCalls);
    const sys = calls[0].system;
    const exemplarCount = (sys.match(/^PROMPT:/gm) || []).length;
    expect(exemplarCount).toBe(12);

    const ontologyBlock = sys.split("ONTOLOGY HINTS:\n")[1] || "";
    const ontologyLines = ontologyBlock
        .split("\n")
        .filter((l) => l.startsWith("- "));
    expect(ontologyLines.length).toBe(20);
});
