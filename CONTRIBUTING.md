# Contributing

Thanks for your interest in contributing to flightverify.

## Development setup

- Install R (>= 4.1) and RTools (Windows) if needed.
- Install package dependencies listed in `DESCRIPTION`.

## Workflow

- Create a feature branch from `main`.
- Keep changes focused and small.
- Add or update tests for behavior changes.

## Style

- Use tidyverse-friendly style.
- Prefer `httr2` for HTTP and `cli` for messaging.
- Document exported functions with roxygen2.

## Tests

- Run `devtools::test()` before opening a PR.
- Use `vcr` to record API interactions where applicable.

## Reporting issues

Include clear steps to reproduce, expected behavior, and actual behavior.