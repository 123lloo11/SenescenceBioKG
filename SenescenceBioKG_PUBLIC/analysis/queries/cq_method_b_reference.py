from __future__ import annotations

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
    """Method B: dataframe set/groupby computation. It builds no graph and calls no Method-A code."""
    names = nodes.set_index("Node_ID")["Canonical_name"].to_dict()
    types = nodes.set_index("Node_ID")["Node_type"].to_dict()
    table = edges.copy()
    table["Source_name"] = table["Source_ID"].map(names)
    table["Target_name"] = table["Target_ID"].map(names)
    table["Source_node_type"] = table["Source_ID"].map(types)
    table["Target_node_type"] = table["Target_ID"].map(types)
    results = {f"CQ{i:02d}": {} for i in range(1, 16)}
    for (record_id, source_id), frame in table.groupby(["Record_ID", "Source_ID"], sort=True):
        rels = set(frame["Relation"])
        stages = set(frame.loc[frame["Relation"].eq("VALIDATED_AT"), "Target_name"].str.casefold())
        facts = {
            "deliver_n": frame.loc[frame["Relation"].eq("DELIVERS"), "Target_ID"].nunique(),
            "target_n": frame.loc[frame["Relation"].eq("TARGETS"), "Target_ID"].nunique(),
            "edge_n": frame["Edge_ID"].nunique(), "rel_n": frame["Relation"].nunique(),
        }
        tests = {
            "CQ01": {"RESPONDS_TO", "TARGETS", "PROMOTES"} <= rels,
            "CQ02": {"RESPONDS_TO", "RELEASES"} <= rels,
            "CQ03": {"RESPONDS_TO", "MODULATES", "PROMOTES"} <= rels,
            "CQ04": {"DELIVERS", "TARGETS"} <= rels and len(stages.intersection(ANIMAL)) > 0,
            "CQ05": {"TARGETS", "MODULATES", "PROMOTES"} <= rels,
            "CQ06": {"COMPOSED_OF", "FABRICATED_BY"} <= rels and len(stages.difference({"cell"})) > 0,
            "CQ07": {"RELEASES", "MODULATES"} <= rels,
            "CQ08": "PROMOTES" in rels and facts["deliver_n"] >= 2,
            "CQ09": "MODULATES" in rels and facts["target_n"] >= 2,
            "CQ10": {"MODULATES", "PROMOTES"} <= rels and len(stages.intersection(ANIMAL)) > 0,
            "CQ11": {"RESPONDS_TO", "TARGETS"} <= rels and "PROMOTES" not in rels,
            "CQ12": {"TARGETS", "MODULATES"} <= rels and "PROMOTES" not in rels,
            "CQ13": {"DELIVERS", "PROMOTES"} <= rels and "MODULATES" not in rels,
            "CQ14": len(stages.intersection(HUMAN)) > 0 and len(rels.intersection({"TARGETS", "MODULATES", "PROMOTES"})) > 0,
            "CQ15": facts["edge_n"] >= 8 and facts["rel_n"] >= 5,
        }
        for cq_id in sorted(tests):
            if not tests[cq_id]:
                continue
            selected = frame if cq_id == "CQ15" else frame[frame["Relation"].isin(RELS[cq_id])]
            if cq_id in {"CQ04", "CQ10"}:
                selected = selected[~selected["Relation"].eq("VALIDATED_AT") | selected["Target_name"].str.casefold().isin(ANIMAL)]
            elif cq_id == "CQ14":
                selected = selected[~selected["Relation"].eq("VALIDATED_AT") | selected["Target_name"].str.casefold().isin(HUMAN)]
            elif cq_id == "CQ06":
                selected = selected[~selected["Relation"].eq("VALIDATED_AT") | ~selected["Target_name"].str.casefold().eq("cell")]
            results[cq_id][(record_id, source_id)] = selected.drop_duplicates("Edge_ID")
    return results
