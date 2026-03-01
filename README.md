# BARK — Bug And Report Keeper

A small Clojure toolkit that watches an IMAP mailbox, stores incoming
emails in a [Datalevin](https://github.com/juji-io/datalevin) database,
and digests them into actionable reports (bugs, patches, requests…).

## Components

- **bark-ingest** (JVM) — long-running service that connects to IMAP and
  stores emails in Datalevin via IDLE or polling.
- **bark-digest** (Babashka) — scans stored emails, detects report
  patterns (e.g. `[BUG]` in subject), and creates report entities.

## Setup

1. Copy and edit the config file:

```bash
cp config.edn.example config.edn
# Edit config.edn with your IMAP credentials and database path
```

2. Start ingesting emails:

```bash
clj -M:run
# Or with a custom config path:
clj -M:run /path/to/config.edn
```

3. Digest emails into reports:

```bash
bb digest           # scan new emails since last run
bb digest --all     # rescan all emails
bb bugs             # list all bug reports
```

## Architecture

```
IMAP Server
    ↓
bark-ingest (JVM, IDLE mode)
    ↓
Datalevin DB
    ↓
bark-digest (Babashka)
    ↓
Reports (bugs, patches, requests…)
```

## Report detection

Subjects are matched against patterns:

| Pattern                 | Report type |
|-------------------------|-------------|
| `[BUG]` / `[BUG 9.7]`  | `:bug`      |

More patterns (patch, request, announcement…) to come.

## Report states

Each report can have states, stored as refs to the email that triggered
the state change:

- **acked** — the report has been acknowledged
- **owned** — someone is working on it
- **closed** — resolved
- **urgent** / **important** — priority flags

## Database schema

Emails are stored with attributes under the `email/` namespace.
Reports are stored under `report/` with a ref to their originating email.

## Requirements

- Java 21+ (with `--add-opens` flags, configured in deps.edn)
- [Babashka](https://github.com/babashka/babashka) for scripts
- Datalevin 0.10.5

## License

Copyright © 2026 Bastien Guerry. EPL-2.0.
