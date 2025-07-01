from http.server import HTTPServer, SimpleHTTPRequestHandler

PORT = 1234

class MyHandler(SimpleHTTPRequestHandler):
    def do_GET(self):
        print("============ HEADER =============")
        for header, value in self.headers.items():
            print(f"{header}: {value}")
        print("=================================")

        self.send_response(200)
        self.send_header("Content-type", "text/plain; charset=utf-8")
        self.end_headers()

        headers_text = ""
        for header, value in self.headers.items():
            headers_text += f"{header}: {value}\n"

        self.wfile.write(headers_text.encode("utf-8"))
        # Koniec — nie wywołujemy super().do_GET(), bo już odpowiedź poszła
        return


    def log_message(self, format, *args):
        print("=== REQUEST ===")
        print("Request:", self.requestline)
        print("===============")

httpd = HTTPServer(('localhost', PORT), MyHandler)
print(f"Serwer działa na http://localhost:{PORT}")
httpd.serve_forever()
