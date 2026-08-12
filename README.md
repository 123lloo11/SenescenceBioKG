# SenescenceBioKG

A full-text evidence-supported knowledge graph of senescence-oriented biomaterials in tissue engineering and regeneration.

## Overview

The frozen public graph represents 283 studies with 654 canonical nodes, 1,868 supported edges, and 1,868 evidence-metadata records. It supports material-centered retrieval across composition, fabrication, design, stimulus response, cargo, target cells, mechanisms, regenerative endpoints, and validation stages.

This repository reproduces analyses of the frozen graph, Figure 1–7 source-data processing, 15 competency queries, and the read-only Explorer. It does not redistribute source PDFs or bulk verbatim evidence excerpts and cannot reproduce full-text adjudication from the copyrighted articles.

## Repository structure

- `data/`: public canonical graph, evidence metadata, ontology tables, query outputs, and final figure-source CSVs.
- `analysis/graph/`: graph summary code.
- `analysis/queries/`: independent competency-query implementations and benchmark runner.
- `analysis/figures/`: Figure 1–7 plotting scripts and source-data checks.
- `analysis/qc/`: release-level quality-control utilities.
- `explorer/`: public R Shiny Explorer.
- `docs/`: data dictionary, ontology, reproducibility, versioning, citation, and release guidance.
- `tests/`: public integrity and Explorer tests.

## Data

The primary public tables are:

- `data/nodes/SenescenceBioKG_nodes_CANONICAL.csv`
- `data/edges/SenescenceBioKG_edges_CANONICAL.csv`
- `data/edges/evidence_metadata_public.csv`
- `data/queries/competency_questions.csv`
- `data/queries/query_results_final.csv`

Verbatim source passages and source PDFs are not redistributed in the public repository. Evidence_ID, DOI, source category, source location, and evidence strength are provided for provenance.

## Reproduce analyses

Create the Python environment with either:

```bash
python -m venv .venv
.venv\Scripts\activate
python -m pip install -r requirements.txt
```

or:

```bash
conda env create -f environment.yml
conda activate senescencebiokg
```

The release QC was run with Python 3.12.13 and pandas 3.0.1.

Then run:

```bash
python analysis/graph/summarize_graph.py
python analysis/queries/run_cq_benchmark.py
python tests/public_release_qc.py
```

For the R environment:

```r
install.packages("renv")
renv::restore()
```

See [REPRODUCIBILITY.md](docs/REPRODUCIBILITY.md) for figure and Explorer commands.

## Run SenescenceBioKG Explorer

From the repository root:

```bash
Rscript explorer/run_explorer.R
```

The Explorer loads only public evidence metadata. When a verbatim passage is unavailable it directs the user to the DOI and PDF location.

## Competency queries

Fifteen predefined questions test material–stimulus, material–cargo, material–cell, mechanism, regeneration, validation, and within-study multi-relation paths. Method A uses graph-style adjacency traversal; Method B independently uses dataframe grouping and set operations. Agreement evaluates implementation consistency, not biomedical truth.

## Evidence provenance

Every supported edge has an Evidence_ID linked to the same Record_ID. The public metadata supplies the DOI, evidence source category, and PDF location. Users should consult the cited publication for the source passage and scientific context.

## Citation

Citation information will be updated upon publication. See `CITATION.cff.template` and `docs/CITATION_SETUP.md`.

