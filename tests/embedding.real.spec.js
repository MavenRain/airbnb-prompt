const { test, expect } = require("@playwright/test");

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

const probes = [
    { prompt: "chalet", needle: "chalet", scope: "every" },
    { prompt: "machiya", needle: "machiya", scope: "every" },
    { prompt: "casita", needle: "casita", scope: "some" },
    { prompt: "rooftop", needle: "rooftop", scope: "some" },
    { prompt: "luxury chalet spa sauna", needle: "luxury chalet", scope: "first" },
    { prompt: "pool barcelona", needle: "rooftop pool", scope: "first" },
    { prompt: "mountain valley views", needle: "mountain chalet", scope: "first" },
];

function matchesScope(top3, needle, scope) {
    switch (scope) {
        case "every":
            return top3.length >= 3 && top3.every((t) => t.includes(needle));
        case "some":
            return top3.some((t) => t.includes(needle));
        case "first":
            return top3.length >= 1 && top3[0].includes(needle);
    }
    return false;
}

test.describe.configure({ mode: "serial", timeout: 240000 });

test.describe("real MiniLM listing retrieval", () => {
    test.beforeEach(async ({ page }) => {
        await page.route("**/llm.js", (route) =>
            route.fulfill({ contentType: "application/javascript", body: stubLlmJs })
        );
    });

    test("semantic ranking surfaces matching listings for varied prompts", async ({ page }) => {
        await page.goto("/");

        await page.waitForFunction(() => window.EmbedBridge !== undefined);
        await page.evaluate(() => {
            window.__embedCount = 0;
            window.EmbedBridge.on("response", () => { window.__embedCount++; });
        });

        await page.waitForFunction(() => (window.__embedCount || 0) >= 47, null, {
            timeout: 180000,
        });

        await probes.reduce(async (prev, { prompt, needle, scope }, i) => {
            await prev;
            await page.locator("textarea.prompt").fill("");
            await page.locator("textarea.prompt").fill(prompt);
            await page.waitForFunction(
                (n) => (window.__slmGenerateCalls || []).length === n,
                i + 1,
                { timeout: 30000 }
            );
            const titles = await page.locator(".listing-card .card-title").allTextContents();
            const top3 = titles.slice(0, 3).map((t) => t.toLowerCase());
            const sys = await page.evaluate((idx) => window.__slmGenerateCalls[idx].system, i);
            const exemplarCount = (sys.match(/^PROMPT:/gm) || []).length;
            expect(
                exemplarCount,
                `'${prompt}' should use semantic retrieval (top-K=4 exemplars), got ${exemplarCount}`
            ).toBeLessThanOrEqual(4);
            expect(
                matchesScope(top3, needle, scope),
                `'${prompt}' top-3 ${scope} '${needle}', got ${JSON.stringify(top3)}`
            ).toBe(true);
        }, Promise.resolve());
    });
});
