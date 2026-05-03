import { pipeline } from "https://cdn.jsdelivr.net/npm/@huggingface/transformers@3.0.0";

const EmbedBridge = (() => {
    let extractor = null;
    let initPromise = null;
    const listeners = { response: [] };

    function emit(channel, payload) {
        for (const fn of listeners[channel]) {
            try {
                fn(payload);
            } catch (e) {
                console.error("EmbedBridge listener error:", e);
            }
        }
    }

    async function ensureReady() {
        if (extractor) return;
        if (!initPromise) {
            initPromise = pipeline("feature-extraction", "Xenova/all-MiniLM-L6-v2");
        }
        extractor = await initPromise;
    }

    async function embed({ id, text }) {
        try {
            await ensureReady();
            const output = await extractor(text, { pooling: "mean", normalize: true });
            const vector = Array.from(output.data);
            emit("response", { id, vector });
        } catch (err) {
            emit("response", { id, error: String(err && err.message || err) });
        }
    }

    function on(channel, fn) {
        if (listeners[channel]) listeners[channel].push(fn);
    }

    return { embed, on };
})();

window.EmbedBridge = EmbedBridge;
