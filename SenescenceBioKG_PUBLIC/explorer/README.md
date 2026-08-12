# SenescenceBioKG Explorer (public mode)

The read-only Shiny application loads canonical nodes, supported edges, public evidence metadata, and the 15 frozen competency-query results.

From the repository root:

```r
install.packages("renv")
renv::restore()
```

```bash
Rscript explorer/run_explorer.R
```

Open `http://127.0.0.1:3838` if a browser is not launched automatically.

Public mode displays Evidence_ID, DOI, source category, source location, and evidence strength. It intentionally does not display verbatim source passages. The application message directs users to consult the cited article. Filters and graphs operate on in-memory copies and do not modify the CSV files.
