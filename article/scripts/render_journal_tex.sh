#!/usr/bin/env bash

# Need to run this to avoid journal submission systems flagging 
# `\\showhyphens` compatibility warnings
# Run `./scripts/render_journal_tex.sh` after rendering to PDF (`quarto render manuscript.qmd --to pdf`).
# It removes the auto-inserted `microtype` block from `manuscript.tex`.

set -euo pipefail

echo "[1/2] Checking for manuscript.tex"
if [[ ! -f manuscript.tex ]]; then
  echo "manuscript.tex not found. Render first with: quarto render manuscript.qmd --to pdf"
  exit 1
fi

echo "[2/2] Removing microtype block from manuscript.tex"
python3 - <<'PY'
from pathlib import Path

tex_path = Path("manuscript.tex")
text = tex_path.read_text(encoding="utf-8")

block = (
    "\\IfFileExists{microtype.sty}{% use microtype if available\n"
    "  \\usepackage[]{microtype}\n"
    "  \\UseMicrotypeSet[protrusion]{basicmath} % disable protrusion for tt fonts\n"
    "}{}\n"
)

if block not in text:
    raise SystemExit("Expected microtype block not found in manuscript.tex")

tex_path.write_text(text.replace(block, "", 1), encoding="utf-8")
PY

echo "Done. manuscript.tex is submission-safe for strict pipelines."
