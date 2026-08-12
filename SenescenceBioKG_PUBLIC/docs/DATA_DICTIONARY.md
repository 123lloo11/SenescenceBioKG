# Data dictionary

## Canonical nodes

`data/nodes/SenescenceBioKG_nodes_CANONICAL.csv`

| Field | Definition |
|---|---|
| `Node_ID` | Stable identifier of a canonical entity node. |
| `Node_type` | Ontology class assigned to the node. |
| `Canonical_name` | Preferred public display name. |
| `Synonyms` | Recorded alternative names, when available. |
| `Description` | Brief node description, when available. |

## Supported edges

`data/edges/SenescenceBioKG_edges_CANONICAL.csv`

| Field | Definition |
|---|---|
| `Edge_ID` | Unique identifier of a supported relation. |
| `Record_ID` | Stable study identifier. |
| `Source_ID` | Canonical source-node identifier. |
| `Source_type` | Ontology type of the source node. |
| `Relation` | Controlled relation label. |
| `Target_ID` | Canonical target-node identifier. |
| `Target_type` | Ontology type of the target node. |
| `Evidence_ID` | Identifier of the linked same-record evidence metadata. |
| `Evidence_strength` | Edge-level evidence assignment: High, Medium, or Low. It is not a study-quality grade. |

## Public evidence metadata

`data/edges/evidence_metadata_public.csv`

| Field | Definition |
|---|---|
| `Evidence_ID` | Unique evidence identifier. |
| `Edge_ID` | Supported edge linked to the evidence record. |
| `Record_ID` | Source study identifier. |
| `DOI` | DOI of the source study when available. |
| `Evidence_source` | Abstract, Methods, Results, Figure, Table, or Supplement. |
| `Evidence_location` | PDF page/section/figure location retained for source consultation. |
| `Evidence_strength` | Edge-level strength copied from the supported edge. |
| `Evidence_excerpt_available_locally` | Indicates that the private research workspace retained a source excerpt; the excerpt is not redistributed. |

## Competency queries and results

`competency_questions.csv` defines `CQ_ID`, the scientific question, rationale, required entity/relation types, formal query logic, expected output, and result count. `query_results_final.csv` reports `CQ_ID`, material, relation path, target entities, Record_ID, and Evidence_ID for returned records. Benchmark files compare independently implemented result sets.

## Counting rules

- Edge count: unique `Edge_ID`.
- Study count: unique `Record_ID`.
- Material count: unique canonical MaterialPlatform `Source_ID`.
- Multiple validation categories may occur in one study.
