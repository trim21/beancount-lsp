import json
import sys
from typing import Any

from beancount.loader import load_file


def main():
    if len(sys.argv) < 2:
        print("[]")
        return

    filename = sys.argv[1]
    try:
        _, errors, _ = load_file(filename)
    except Exception as exc:  # broad but we want to return diagnostics
        print(json.dumps([{"message": str(exc), "lineno": None, "filename": filename}]))
        return

    out: list[Any] = []
    for err in errors:
        out.append(
            {
                "message": err.message,
                "lineno": err.source.get("lineno"),
                "filename": err.source.get("filename"),
            }
        )

    print(json.dumps(out))


if __name__ == "__main__":
    main()
