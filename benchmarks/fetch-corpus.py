import hashlib
import json
import sys
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import quote
from urllib.request import Request, urlopen


class Paragraphs(HTMLParser):
    def __init__(self):
        super().__init__()
        self.depth = 0
        self.paragraph = False
        self.parts = []

    def handle_starttag(self, tag, attributes):
        if tag == "div":
            if self.depth:
                self.depth += 1
            elif "mw-parser-output" in dict(attributes).get("class", "").split():
                self.depth = 1
        if self.depth and tag == "p":
            self.paragraph = True

    def handle_endtag(self, tag):
        if tag == "div" and self.depth:
            self.depth -= 1
        if tag == "p" and self.paragraph:
            self.parts.append("\n\n")
            self.paragraph = False

    def handle_data(self, data):
        if self.depth and self.paragraph:
            self.parts.append(data)


def main():
    if len(sys.argv) != 2:
        raise SystemExit("Usage: fetch-corpus.py CORPUS_DIRECTORY")
    directory = Path(sys.argv[1])
    directory.mkdir(parents=True, exist_ok=True)
    sources = {
        "data.json": "https://raw.githubusercontent.com/miloyip/nativejson-benchmark/master/data/twitter.json",
        "page.html": "https://www.rfc-editor.org/rfc/rfc9110.html",
        "text-en.txt": "https://www.rfc-editor.org/rfc/rfc9110.txt",
        "text-ru.txt": quote("https://ru.wikisource.org/wiki/Фальшивый_купон_(Толстой)", safe=":/"),
    }
    manifest = {}
    for name, url in sources.items():
        request = Request(url, headers={"User-Agent": "wai-compression-benchmark/0.1"})
        with urlopen(request, timeout=60) as response:
            data = response.read(8 * 1024 * 1024 + 1)
        if len(data) > 8 * 1024 * 1024:
            raise SystemExit(f"Corpus too large: {name}")
        if name == "text-ru.txt":
            parser = Paragraphs()
            parser.feed(data.decode("utf-8"))
            data = "".join(parser.parts).strip().encode("utf-8")
        else:
            data.decode("utf-8")
        if len(data) < 65536:
            raise SystemExit(f"Corpus too small: {name}")
        (directory / name).write_bytes(data)
        manifest[name] = {
            "url": url,
            "bytes": len(data),
            "sha256": hashlib.sha256(data).hexdigest(),
        }
        print(f"{name}: {len(data)} bytes")
    (directory / "sources.json").write_text(
        json.dumps(manifest, indent=2, ensure_ascii=False) + "\n", encoding="utf-8"
    )


if __name__ == "__main__":
    main()
