const { defineConfig, devices } = require("@playwright/test");

module.exports = defineConfig({
    testDir: "./tests",
    fullyParallel: false,
    workers: 1,
    reporter: "list",
    use: {
        baseURL: "http://127.0.0.1:8765",
        trace: "retain-on-failure",
    },
    projects: [
        { name: "chromium", use: { ...devices["Desktop Chrome"] } },
    ],
    webServer: {
        command: "python3 -m http.server 8765 --directory public",
        url: "http://127.0.0.1:8765",
        reuseExistingServer: !process.env.CI,
        timeout: 30000,
    },
});
