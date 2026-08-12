from __future__ import annotations

from collections import defaultdict
import pandas as pd

ANIMAL = {"small animal", "large animal"}
HUMAN = {"human-derived", "clinical"}
RELS = {
    "CQ01": {"RESPONDS_TO", "TARGETS", "PROMOTES"}, "CQ02": {"RESPONDS_TO", "RELEASES"},
    "CQ03": {"RESPONDS_TO", "MODULATES", "PROMOTES"}, "CQ04": {"DELIVERS", "TARGETS", "VALIDATED_AT"},
    "CQ05": {"TARGETS", "MODULATES", "PROMOTES"}, "CQ06": {"COMPOSED_OF", "FABRICATED_BY", "VALIDATED_AT"},
    "CQ07": {"RELEASES", "MODULATES"}, "CQ08": {"DELIVERS", "PROMOTES"},
    "CQ09": {"TARGETS", "MODULATES"}, "CQ10": {"MODULATES", "PROMOTES", "VALIDATED_AT"},
    "CQ11": {"RESPONDS_TO", "TARGETS"}, "CQ12": {"TARGETS", "MODULATES"},
    "CQ13": {"DELIVERS", "PROMOTES"}, "CQ14": {"VALIDATED_AT", "TARGETS", "MODULATES", "PROMOTES"},
    "CQ15": set(),
}


def run(edges: pd.DataFrame, nodes: pd.DataFrame):
    """Method A: adjacency-list traversal. It does not call the reference implementation."""
    names = nodes.set_index("Node_ID")["Canonical_name"].to_dict()
    types = nodes.set_index("Node_ID")["Node_type"].to_dict()
    adjacency = defaultdict(list)
    for row in edges.to_dict("records"):
        adjacency[row["Source_ID"]].append(row)
    results = {f"CQ{i:02d}": {} for i in range(1, 16)}
    for source_id, edge_list in sorted(adjacency.items()):
        for record_id in sorted({x["Record_ID"] for x in edge_list}):
            local = pd.DataFrame([x for x in edge_list if x["Record_ID"] == record_id])
            local["Source_name"] = local["Source_ID"].map(names)
            local["Target_name"] = local["Target_ID"].map(names)
            local["Source_node_type"] = local["Source_ID"].map(types)
            local["Target_node_type"] = local["Target_ID"].map(types)
            rels = set(local["Relation"])
            stages = set(local.loc[local["Relation"].eq("VALIDATED_AT"), "Target_name"].str.casefold())
            tests = {
                "CQ01": {"RESPONDS_TO", "TARGETS", "PROMOTES"} <= rels,
                "CQ02": {"RESPONDS_TO", "RELEASES"} <= rels,
                "CQ03": {"RESPONDS_TO", "MODULATES", "PROMOTES"} <= rels,
                "CQ04": {"DELIVERS", "TARGETS"} <= rels and bool(stages & ANIMAL),
                "CQ05": {"TARGETS", "MODULATES", "PROMOTES"} <= rels,
                "CQ06": {"COMPOSED_OF", "FABRICATED_BY"} <= rels and bool(stages - {"cell"}),
                "CQ07": {"RELEASES", "MODULATES"} <= rels,
                "CQ08": "PROMOTES" in rels and local.loc[local["Relation"].eq("DELIVERS"), "Target_ID"].nunique() >= 2,
                "CQ09": "MODULATES" in rels and local.loc[local["Relation"].eq("TARGETS"), "Target_ID"].nunique() >= 2,
                "CQ10": {"MODULATES", "PROMOTES"} <= rels and bool(stages & ANIMAL),
                "CQ11": {"RESPONDS_TO", "TARGETS"} <= rels and "PROMOTES" not in rels,
                "CQ12": {"TARGETS", "MODULATES"} <= rels and "PROMOTES" not in rels,
                "CQ13": {"DELIVERS", "PROMOTES"} <= rels and "MODULATES" not in rels,
                "CQ14": bool(stages & HUMAN) and bool(rels & {"TARGETS", "MODULATES", "PROMOTES"}),
                "CQ15": len(local) >= 8 and local["Relation"].nunique() >= 5,
            }
            for cq_id, passed in tests.items():
                if not passed:
                    continue
                if cq_id == "CQ15":
                    selected = local
                elif cq_id in {"CQ04", "CQ10"}:
                    selected = local[local["Relation"].isin(RELS[cq_id]) & (~local["Relation"].eq("VALIDATED_AT") | local["Target_name"].str.casefold().isin(ANIMAL))]
                elif cq_id == "CQ14":
                    selected = local[local["Relation"].isin(RELS[cq_id]) & (~local["Relation"].eq("VALIDATED_AT") | local["Target_name"].str.casefold().isin(HUMAN))]
                elif cq_id == "CQ06":
                    selected = local[local["Relation"].isin(RELS[cq_id]) & (~local["Relation"].eq("VALIDATED_AT") | ~local["Target_name"].str.casefold().eq("cell"))]
                else:
                    selected = local[local["Relation"].isin(RELS[cq_id])]
                results[cq_id][(record_id, source_id)] = selected.drop_duplicates("Edge_ID")
    return results
