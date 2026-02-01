import sys

from ._lsp import main as _main  # pyright: ignore[reportMissingModuleSource]


def main(argv: list[str] | None = None) -> None:
    _main(argv or sys.argv[1:])
