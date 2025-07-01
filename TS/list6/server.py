from http.server import HTTPServer, SimpleHTTPRequestHandler

PORT = 1234

class MyHandler(SimpleHTTPRequestHandler):
    def do_GET(self):

        print("============ HEADER =============")
        for header, value in self.headers.items():
            print(f"{header}: {value}")
        print("=================================")

        if self.path in ('/', '/main.html'):
            self.path = '/main.html'
        return super().do_GET()

    def log_message(self, format, *args):
        print("=== REQUEST ===")
        print("Request:", self.requestline)
        print("===============")

httpd = HTTPServer(('localhost', PORT), MyHandler)
print(f"Serwer działa na http://localhost:{PORT}")
httpd.serve_forever()
