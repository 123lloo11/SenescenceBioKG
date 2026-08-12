"""Reproduce all 15 SenescenceBioKG competency queries by two independent methods."""
from __future__ import annotations

import sys
from pathlib import Path
import pandas as pd

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(Path(__file__).resolve().parent))
import cq_method_a_graph as method_a  # noqa: E402
import cq_method_b_reference as method_b  # noqa: E402


def keys(result):
    return set(result.keys())


def main() -> None:
    nodes = pd.read_csv(ROOT / "data/nodes/SenescenceBioKG_nodes_CANONICAL.csv")
    edges = pd.read_csv(ROOT / "data/edges/SenescenceBioKG_edges_CANONICAL.csv")
    published = pd.read_csv(ROOT / "data/queries/query_results_final.csv")
    source_pairs = edges[["Record_ID", "Source_ID"]].drop_duplicates().merge(
        nodes[["Node_ID", "Canonical_name"]], left_on="Source_ID", right_on="Node_ID", how="left"
    )
    a = method_a.run(edges, nodes)
    b = method_b.run(edges, nodes)
    rows = []
    for i in range(1, 16):
        cq = f"CQ{i:02d}"
        ka, kb = keys(a[cq]), keys(b[cq])
        pub = published.loc[published.CQ_ID.eq(cq), ["Record_ID", "Material"]].drop_duplicates()
        matched = pub.merge(source_pairs, left_on=["Record_ID", "Material"],
                            right_on=["Record_ID", "Canonical_name"], how="left")
        expected = set(zip(matched.Record_ID, matched.Source_ID))
        if matched.Source_ID.isna().any():
            raise RuntimeError(f"Unmapped published material in {cq}")
        tp = len(ka & kb); fp = len(ka - kb); fn = len(kb - ka)
        precision = tp / (tp + fp) if tp + fp else 1.0
        recall = tp / (tp + fn) if tp + fn else 1.0
        f1 = 2 * precision * recall / (precision + recall) if precision + recall else 1.0
        rows.append({"CQ_ID": cq, "Method_A_n": len(ka), "Method_B_n": len(kb),
                     "TP": tp, "FP": fp, "FN": fn, "Precision": precision,
                     "Recall": recall, "F1": f1, "Exact_set_match": ka == kb,
                     "Published_result_match": ka == expected})
    report = pd.DataFrame(rows)
    out = ROOT / "outputs"
    out.mkdir(exist_ok=True)
    report.to_csv(out / "public_cq_benchmark.csv", index=False)
    if not (report.Exact_set_match.all() and report.Published_result_match.all()):
        raise SystemExit("CQ benchmark failed")
    print("CQ exact-set matches: 15/15")


if __name__ == "__main__":
    main()
