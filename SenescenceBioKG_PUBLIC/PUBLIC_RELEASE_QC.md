# SenescenceBioKG public-release quality control

## Release scope

The public package reproduces analyses of the frozen graph, Figure 1–7 source-data processing, two independent competency-query implementations, and the metadata-only Explorer. It does not contain source PDFs, manuscript files, or verbatim evidence excerpts and does not reproduce copyrighted-PDF adjudication.

## Core integrity

| Check | Result | Status |
|---|---:|---|
| Canonical nodes | 654 | PASS |
| Supported edges | 1,868 | PASS |
| Public evidence-metadata records | 1,868 | PASS |
| RESPONDS_TO edges | 64 | PASS |
| VALIDATED_AT edges | 567 | PASS |
| Duplicate Node_ID | 0 | PASS |
| Duplicate Edge_ID | 0 | PASS |
| Duplicate Evidence_ID | 0 | PASS |
| Missing Source_ID/Target_ID links | 0 | PASS |
| Missing Edge_ID–Evidence_ID links | 0 | PASS |

## Competency-query reproducibility

- Method A: adjacency-list graph traversal.
- Method B: independently implemented dataframe grouping, joins, and set operations.
- Exact-set agreement: **15/15**.
- Agreement with published final query-result sets: **15/15**.

Status: **PASS**.

## Explorer public mode

The public Explorer was tested from the repository directory with `evidence_metadata_public.csv` and no `Evidence_excerpt` column.

| Check | Status |
|---|---|
| Public data load | PASS |
| Material/relation/entity filtering | PASS |
| Network payload construction | PASS |
| Evidence_ID display data | PASS |
| DOI and evidence-location display data | PASS |
| Fifteen competency-query results | PASS |
| Local HTTP startup | PASS |
| Input CSV modification | 0 files modified |

The interface displays: “Verbatim evidence excerpt is not redistributed in the public release. Please consult the cited source using the DOI and evidence location.”

## Figure reproducibility

- Figure 1–7 public source files: **37 CSV files, all readable and non-empty**.
- Figure 1–7 R plotting scripts: **all executed successfully from the public repository**.
- Generated PDFs/previews are ignored and were removed from the prepared repository; users can regenerate them under `outputs/`.
- Figure 7D accepts a locally generated public Explorer screenshot and otherwise renders a documented placeholder.

Status: **PASS**.

## Privacy and release hygiene

| Check | Result |
|---|---:|
| Secrets found | 0 |
| Absolute personal paths | 0 |
| Local usernames | 0 |
| Private network addresses | 0 |
| Source PDFs | 0 |
| DOC/DOCX/PPTX files | 0 |
| Private evidence excerpts | 0 |
| Replacement characters (U+FFFD) | 0 |
| Runtime/cache paths | 0 |

Standard maintainer e-mail addresses embedded in CRAN package metadata inside `renv.lock` are not personal project data and were excluded from the e-mail privacy check. Citation author fields remain `TO_BE_COMPLETED` templates.

## Environments

- Python test environment: Python 3.12.13, pandas 3.0.1.
- Public Python files: `requirements.txt` and `environment.yml`.
- R test environment: R 4.5.3.
- Public R environment: `renv.lock`, generated from the packages used by the Explorer, tests, and Figure 1–7 scripts.

## Release status

**PUBLIC PACKAGE QC: PASS**

No GitHub upload, remote creation, credential access, or Zenodo upload was performed.
