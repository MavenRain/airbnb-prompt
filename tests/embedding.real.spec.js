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

test.describe.configure({ mode: "serial", timeout: 240000 });

test.describe("real MiniLM listing retrieval", () => {
    test.beforeEach(async ({ page }) => {
        await page.route("**/llm.js", (route) =>
            route.fulfill({ contentType: "application/javascript", body: stubLlmJs })
        );
    });

    test("type-keyword prompts bubble matching listings to top-3", async ({ page }) => {
        await page.goto("/");

        await page.waitForFunction(() => window.EmbedBridge !== undefined);
        await page.evaluate(() => {
            window.__embedCount = 0;
            window.EmbedBridge.on("response", () => { window.__embedCount++; });
        });

        await page.waitForFunction(() => (window.__embedCount || 0) >= 47, null, {
            timeout: 180000,
        });

        const probes = [
            { prompt: "chalet", keyword: "chalet" },
            { prompt: "machiya", keyword: "machiya" },
            { prompt: "casita", keyword: "casita" },
            { prompt: "rooftop", keyword: "rooftop" },
        ];

        await probes.reduce(async (prev, { prompt, keyword }, i) => {
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
            expect(
                top3.some((t) => t.includes(keyword)),
                `'${prompt}' top-3 should contain a listing whose title mentions '${keyword}', got ${JSON.stringify(top3)}`
            ).toBe(true);
        }, Promise.resolve());
    });
});
