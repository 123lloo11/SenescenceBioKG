# Quality-control code

Release integrity and privacy checks are implemented in `tests/public_release_qc.py` and `tests/public_explorer_qc.R`. They verify frozen graph counts, identifier linkage, relation counts, absence of private evidence excerpts and prohibited files, relative-path hygiene, two independent competency-query implementations, Explorer data operations, and local HTTP startup.
