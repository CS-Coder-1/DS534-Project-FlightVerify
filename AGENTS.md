# AGENTS

This file defines how Codex (and other agents) should work in this repo.

## Quick Start
- Read `README.md` first if present.
- Use R (tidyverse-friendly) conventions unless the repo specifies otherwise.
- Keep changes minimal and focused; avoid unrelated refactors.

## Project Summary
- Purpose: flight delay verification using OpenSky Network data.
- Language: R (package-style layout expected).

## Conventions
- Prefer `httr2`, `jsonlite`, `tibble`, `dplyr`, `lubridate`, `cli`.
- Use roxygen2 for documentation.
- Format: use `{styler}` if already part of the project.
- Tests: prefer `testthat` + `vcr` for API calls.

## Commands
- Create package: `usethis::create_package()`
- Run tests: `devtools::test()` or `testthat::test_dir("tests/testthat")`
- Check package: `devtools::check()`

## Secrets and API Keys
- Never commit secrets.
- Store credentials in `.Renviron` or OS env vars.
- Add any local secrets file to `.gitignore`.

## API Usage
- Use OpenSky Network REST API.
- Wrap authentication via OAuth2 client credentials.
- Handle rate limits and errors gracefully.

## Documentation
- Keep README and vignette examples current.
- Provide minimal runnable examples for key functions.

## Deliverables (MVP)
- Auth helpers
- Airport arrivals/departures
- Landing rate analysis
- Delay verification

## Notes for Agents
- Ask before adding new dependencies.
- If requirements are unclear, ask for clarification.
