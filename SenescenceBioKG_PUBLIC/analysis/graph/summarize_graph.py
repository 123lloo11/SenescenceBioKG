"""Write reproducible graph counts from the public canonical node and edge tables."""
from pathlib import Path
import pandas as pd

ROOT = Path(__file__).resolve().parents[2]
nodes = pd.read_csv(ROOT / "data/nodes/SenescenceBioKG_nodes_CANONICAL.csv")
edges = pd.read_csv(ROOT / "data/edges/SenescenceBioKG_edges_CANONICAL.csv")
evidence = pd.read_csv(ROOT / "data/edges/evidence_metadata_public.csv")
rows = [
    {"Metric": "Canonical nodes", "Count": len(nodes)},
    {"Metric": "Supported edges", "Count": len(edges)},
    {"Metric": "Evidence metadata records", "Count": len(evidence)},
]
rows.extend({"Metric": f"Relation: {k}", "Count": int(v)} for k, v in edges.Relation.value_counts().sort_index().items())
out = ROOT / "outputs"; out.mkdir(exist_ok=True)
pd.DataFrame(rows).to_csv(out / "graph_summary.csv", index=False)
print(pd.DataFrame(rows).to_string(index=False))
