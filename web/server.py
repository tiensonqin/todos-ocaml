from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
import os
from pathlib import Path


class CrossOriginIsolatedHandler(SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header("Cross-Origin-Opener-Policy", "same-origin")
        self.send_header("Cross-Origin-Embedder-Policy", "require-corp")
        self.send_header("Cross-Origin-Resource-Policy", "same-origin")
        super().end_headers()


if __name__ == "__main__":
    port = int(os.environ.get("PORT", "5178"))
    root = Path(__file__).resolve().parent
    handler = lambda *args, **kwargs: CrossOriginIsolatedHandler(
        *args, directory=str(root), **kwargs
    )
    server = ThreadingHTTPServer(("127.0.0.1", port), handler)
    print(f"Serving Melange React todos web app at http://127.0.0.1:{port}/")
    server.serve_forever()
