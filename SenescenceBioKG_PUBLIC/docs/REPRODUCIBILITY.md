# Reproducibility

## Scope

This release reproduces frozen-graph analyses, figures, competency queries, and the Explorer. It does not reproduce full-text relation adjudication from source PDFs because copyrighted PDFs and bulk verbatim evidence excerpts are not redistributed.

## 1. Obtain the repository

```bash
git clone https://github.com/<USERNAME>/SenescenceBioKG.git
cd SenescenceBioKG
```

## 2. Configure Python

```bash
python -m venv .venv
.venv\Scripts\activate
python -m pip install -r requirements.txt
```

Validate the graph and the two independent query paths:

```bash
python analysis/graph/summarize_graph.py
python analysis/queries/run_cq_benchmark.py
python tests/public_release_qc.py
```

Expected core results are 654 nodes, 1,868 edges, 64 RESPONDS_TO edges, 567 VALIDATED_AT edges, and exact-set agreement for 15 of 15 competency questions.

## 3. Configure R

Install R 4.5.3 or a compatible release, then run:

```r
install.packages("renv")
renv::restore()
```

## 4. Check Figure 1–7 sources

```bash
Rscript analysis/figures/check_figure_sources.R
```

Each `plot_figureN.R` script reads from `data/figure_source/figureN/` and writes generated files under `outputs/figures/`. Figure 7D optionally uses `outputs/explorer_screenshot.png`; without that local screenshot, the script generates a clear placeholder panel while all data panels remain reproducible.

Example:

```bash
Rscript analysis/figures/Figure4/plot_figure4.R
```

## 5. Run the Explorer

```bash
Rscript explorer/run_explorer.R
```

Then open `http://127.0.0.1:3838`. The app displays evidence identifiers, DOI, source category, and source location without redistributing verbatim passages.

Run the automated Explorer test with:

```bash
Rscript tests/public_explorer_qc.R
```

## 6. Interpret outputs

Counts describe the frozen evidence graph. A missing supported edge is a validated-core missing link, not proof that the source publication omitted the corresponding experiment. Human-derived evidence is not Clinical, and query benchmark metrics measure implementation agreement rather than biomedical validity.
