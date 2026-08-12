# GitHub and Zenodo release guide (Windows)

This guide prepares a release; it does not authorize uploading credentials or unpublished files.

## A. Check Git

Open PowerShell or Git Bash:

```bash
git --version
```

Install Git for Windows if the command is unavailable, then configure the author identity according to institutional policy.

## B. Create an empty GitHub repository

Create a **Public** repository named `SenescenceBioKG`. Do not initialize it with a README, license, or `.gitignore`, because the local package already contains those files/templates.

## C. Initialize locally

```bash
cd %USERPROFILE%\Desktop\lsy\SenescenceBioKG_PUBLIC
git init
git add .
git status
git commit -m "Initial public release of SenescenceBioKG"
git branch -M main
```

Inspect `git status` and the staged-file list before committing. Confirm that no PDF, DOCX, excerpt-bearing evidence table, credential, or private note is present.

## D. Add the remote

```bash
git remote add origin https://github.com/<USERNAME>/SenescenceBioKG.git
git push -u origin main
```

Replace `<USERNAME>` only after the repository owner is confirmed. Use GitHub’s supported authentication workflow; never store a token in this repository.

## E. Create the GitHub release

Use tag `v1.0.0` and release title:

`SenescenceBioKG v1.0.0 — manuscript release`

Describe frozen counts, included files, known limitations, the lack of redistributed PDFs/excerpts, and the public QC result.

## F. Archive with Zenodo

1. Sign in to Zenodo using GitHub.
2. Enable the `SenescenceBioKG` repository in the Zenodo GitHub integration.
3. Create the GitHub `v1.0.0` release.
4. Confirm that Zenodo archives the release.
5. Record the version DOI and concept DOI.
6. Add the DOI badge and citation text to README.
7. Replace DOI placeholders in `CITATION.cff`, Zenodo metadata, and the manuscript Data Availability statement.

Run the complete public QC immediately before staging and again on the tagged release archive.
