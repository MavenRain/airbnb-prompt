#!/usr/bin/env node
const fs = require("node:fs");
const path = require("node:path");
const { execFileSync } = require("node:child_process");

const ROOT = path.resolve(__dirname, "..");
const VENDOR = path.join(ROOT, "public", "vendor");
const NODE_MODULES = path.join(ROOT, "node_modules");

function ensureDir(p) {
    fs.mkdirSync(p, { recursive: true });
}

function fileAtLeast(p, minBytes) {
    try {
        return fs.statSync(p).size >= minBytes;
    } catch {
        return false;
    }
}

function bundleTransformers() {
    const src = path.join(NODE_MODULES, "@huggingface", "transformers", "dist", "transformers.web.js");
    const dst = path.join(VENDOR, "transformers", "transformers.bundle.js");
    if (fileAtLeast(dst, 100_000)) {
        return { dst, status: "skipped" };
    }
    ensureDir(path.dirname(dst));
    const esbuild = path.join(NODE_MODULES, ".bin", "esbuild");
    execFileSync(esbuild, [
        src,
        "--bundle",
        "--format=esm",
        "--target=es2022",
        "--minify",
        `--outfile=${dst}`,
    ], { stdio: "inherit" });
    return { dst, status: "bundled" };
}

function copyOrtFiles() {
    const src = path.join(NODE_MODULES, "onnxruntime-web", "dist");
    const dst = path.join(VENDOR, "ort");
    ensureDir(dst);
    const variants = ["", ".asyncify", ".jsep", ".jspi"];
    const exts = [".mjs", ".wasm"];
    const files = variants.flatMap((v) => exts.map((e) => `ort-wasm-simd-threaded${v}${e}`));
    return files.map((f) => {
        const srcPath = path.join(src, f);
        const dstPath = path.join(dst, f);
        const srcSize = fs.statSync(srcPath).size;
        if (fileAtLeast(dstPath, srcSize)) {
            return { file: f, status: "skipped" };
        }
        fs.copyFileSync(srcPath, dstPath);
        return { file: f, status: "copied" };
    });
}

async function downloadMiniLm() {
    const baseRemote = "https://huggingface.co/Xenova/all-MiniLM-L6-v2/resolve/main";
    const baseLocal = path.join(VENDOR, "models", "Xenova", "all-MiniLM-L6-v2");
    ensureDir(path.join(baseLocal, "onnx"));
    const files = [
        "config.json",
        "tokenizer.json",
        "tokenizer_config.json",
        "special_tokens_map.json",
        "onnx/model_quantized.onnx",
    ];
    return Promise.all(files.map(async (rel) => {
        const dst = path.join(baseLocal, rel);
        if (fileAtLeast(dst, 100)) {
            return { file: rel, status: "skipped" };
        }
        const res = await fetch(`${baseRemote}/${rel}`, { redirect: "follow" });
        if (!res.ok) {
            throw new Error(`HTTP ${res.status} fetching ${rel}`);
        }
        const buf = Buffer.from(await res.arrayBuffer());
        fs.writeFileSync(dst, buf);
        return { file: rel, status: "downloaded" };
    }));
}

function summarize(label, results) {
    const items = Array.isArray(results) ? results : [results];
    const grouped = items.reduce((m, r) => m.set(r.status, (m.get(r.status) || 0) + 1), new Map());
    const summary = [...grouped.entries()].map(([k, v]) => `${k}=${v}`).join(", ");
    console.log(`${label}: ${summary}`);
}

(async () => {
    summarize("transformers bundle", bundleTransformers());
    summarize("ort runtime", copyOrtFiles());
    summarize("MiniLM model", await downloadMiniLm());
})().catch((err) => {
    console.error("vendor failed:", err);
    process.exit(1);
});
