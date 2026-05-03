import * as webllm from "https://esm.run/@mlc-ai/web-llm@0.2.79";

const SlmBridge = (() => {
    let engine = null;
    let initPromise = null;
    const listeners = { status: [], response: [] };

    function emit(channel, payload) {
        for (const fn of listeners[channel]) {
            try {
                fn(payload);
            } catch (e) {
                console.error("SlmBridge listener error:", e);
            }
        }
    }

    function hasWebGPU() {
        return typeof navigator !== "undefined" && "gpu" in navigator;
    }

    function on(channel, fn) {
        if (listeners[channel]) listeners[channel].push(fn);
    }

    function errString(err) {
        if (err == null) return "unknown";
        if (typeof err === "number") return "WASM error code " + err;
        if (err && err.message) return String(err.message);
        return String(err);
    }

    async function init({ modelId }) {
        if (!hasWebGPU()) {
            emit("status", { event: "error", message: "WebGPU unavailable" });
            return;
        }
        if (initPromise) return initPromise.catch(() => {});

        initPromise = (async () => {
            try {
                emit("status", { event: "loading", progress: 0, text: "starting" });
                engine = await webllm.CreateMLCEngine(modelId, {
                    initProgressCallback: (report) => {
                        emit("status", {
                            event: "loading",
                            progress: typeof report.progress === "number" ? report.progress : 0,
                            text: report.text || ""
                        });
                    }
                });
                emit("status", { event: "ready" });
            } catch (err) {
                console.error("SLM init failed:", err);
                emit("status", { event: "error", message: errString(err) });
                initPromise = null;
                throw err;
            }
        })();

        return initPromise.catch(() => {});
    }

    async function generate({ id, system, prompt, maxTokens }) {
        if (!engine) {
            emit("response", { id, error: "engine not initialized" });
            return;
        }

        try {
            const messages = [];
            if (system) messages.push({ role: "system", content: system });
            messages.push({ role: "user", content: prompt });

            console.log("SLM generate request id=" + id, { promptPreview: prompt });

            const reply = await engine.chat.completions.create({
                messages,
                response_format: { type: "json_object" },
                max_tokens: maxTokens || 512,
                temperature: 0.0
            });

            const content = reply && reply.choices && reply.choices[0]
                && reply.choices[0].message && reply.choices[0].message.content;
            console.log("SLM raw content for id=" + id + ":", content);

            if (!content) {
                emit("response", { id, error: "empty response from model" });
                return;
            }

            let parsed;
            try {
                parsed = JSON.parse(content);
            } catch (parseErr) {
                emit("response", { id, error: "JSON parse failed: " + content.slice(0, 200) });
                return;
            }
            emit("response", { id, result: parsed });
        } catch (err) {
            console.error("SLM generate failed:", err);
            emit("response", { id, error: errString(err) });
        }
    }

    return { init, generate, on, hasWebGPU };
})();

window.SlmBridge = SlmBridge;

window.addEventListener("unhandledrejection", (event) => {
    console.warn("Unhandled rejection in SlmBridge context:", event.reason);
});
