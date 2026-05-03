const http = require("http");
const fs = require("fs");
const path = require("path");
const url = require("url");

const root = path.join(__dirname, "public");
const port = Number(process.env.PORT) || 8765;

const ctMap = {
    ".html": "text/html; charset=utf-8",
    ".js": "application/javascript; charset=utf-8",
    ".mjs": "application/javascript; charset=utf-8",
    ".json": "application/json; charset=utf-8",
    ".wasm": "application/wasm",
    ".css": "text/css; charset=utf-8",
    ".png": "image/png",
    ".svg": "image/svg+xml",
};

http.createServer((req, res) => {
    let p = url.parse(req.url).pathname || "/";
    if (p.endsWith("/")) {
        p += "index.html";
    }
    const file = path.join(root, p);
    if (!file.startsWith(root)) {
        res.writeHead(403);
        res.end();
        return;
    }
    fs.readFile(file, (err, data) => {
        if (err) {
            res.writeHead(404);
            res.end("not found");
            return;
        }
        res.setHeader("Cross-Origin-Opener-Policy", "same-origin");
        res.setHeader("Cross-Origin-Embedder-Policy", "require-corp");
        res.setHeader("Cross-Origin-Resource-Policy", "same-origin");
        res.setHeader("Cache-Control", "no-store");
        res.setHeader("Content-Type", ctMap[path.extname(file)] || "application/octet-stream");
        res.end(data);
    });
}).listen(port, () => {
    console.log(`dev server listening on http://127.0.0.1:${port}`);
});
