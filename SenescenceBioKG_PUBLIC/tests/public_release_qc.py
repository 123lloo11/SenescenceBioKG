"""Security, integrity, and reproducibility checks for the public release."""
from __future__ import annotations
from pathlib import Path
import re
import subprocess
import sys
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
nodes = pd.read_csv(ROOT / "data/nodes/SenescenceBioKG_nodes_CANONICAL.csv")
edges = pd.read_csv(ROOT / "data/edges/SenescenceBioKG_edges_CANONICAL.csv")
evidence = pd.read_csv(ROOT / "data/edges/evidence_metadata_public.csv")
assert len(nodes) == 654 and len(edges) == 1868 and len(evidence) == 1868
assert edges.Edge_ID.is_unique and evidence.Evidence_ID.is_unique and nodes.Node_ID.is_unique
assert edges.Source_ID.isin(nodes.Node_ID).all() and edges.Target_ID.isin(nodes.Node_ID).all()
assert edges.Evidence_ID.isin(evidence.Evidence_ID).all()
assert (edges.Relation == "RESPONDS_TO").sum() == 64
assert (edges.Relation == "VALIDATED_AT").sum() == 567
assert "Evidence_excerpt" not in evidence.columns

files = [p for p in ROOT.rglob("*") if p.is_file() and ".git" not in p.parts and "outputs" not in p.parts]
forbidden_ext = {".pdf", ".doc", ".docx", ".pptx"}
assert not [p for p in files if p.suffix.lower() in forbidden_ext]
patterns = {
    "absolute_personal_path": re.compile(r"[A-Za-z]:[\\/]Users[\\/]", re.I),
    "codex_cache": re.compile("codex" + r"-runtimes|\\.cache[\\/]" + "codex", re.I),
    "secret_assignment": re.compile(r"(?i)(api[_-]?key|password|passwd|bearer|authorization|token)\s*[:=]\s*['\"][^<'\"]{8,}"),
    "email_address": re.compile(r"[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}", re.I),
    "private_ip": re.compile(r"\b(?:10\.\d{1,3}\.\d{1,3}\.\d{1,3}|192\.168\.\d{1,3}\.\d{1,3}|172\.(?:1[6-9]|2\d|3[01])\.\d{1,3}\.\d{1,3})\b"),
    "replacement_character": re.compile("\ufffd"),
    "local_username": re.compile("186" + "11"),
}
hits = {k: [] for k in patterns}
for p in files:
    try: text = p.read_text(encoding="utf-8")
    except (UnicodeDecodeError, OSError): continue
    for key, pat in patterns.items():
        if key == "email_address" and p.name == "renv.lock":
            continue  # Standard CRAN package metadata contains maintainer e-mail addresses.
        if pat.search(text): hits[key].append(str(p.relative_to(ROOT)))
assert not any(hits.values()), hits

subprocess.run([sys.executable, str(ROOT / "analysis/queries/run_cq_benchmark.py")], check=True)
print("Public release core QC: PASS")
