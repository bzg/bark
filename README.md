# BARK — Bug And Report Keeper

BARK is an email-driven report tracker.

BARK monitors IMAP mailboxes, detects report types from subject tags,
applies state triggers from email body, and manages roles per mailbox.

## How it works

- **bark-ingest** — JVM process, connects to IMAP via IDLE, stores
  emails in Datalevin.
- **bark-digest** — Babashka script, scans new emails, creates/updates
  reports and roles in the DB.
- **bark-egest** — Babashka script, displays reports interactively or
  dumps them as JSON.

## Dependencies

- [Babashka](https://babashka.org/) (bb)
- [gum](https://github.com/charmbracelet/gum) — TUI components used by
  bark-egest for interactive display
- Java 17+ (for bark-ingest)
- Datalevin 0.10.5 (pod for bb, library for JVM)

## Quick start

```sh
# 1. Copy and edit config
cp config.edn.example config.edn

# 2. Start ingesting emails (runs in foreground, C-c to stop)
clj -M -m bark-ingest.main

# 3. Digest new emails into reports
bb digest          # incremental
bb digest --all    # rescan everything

# 4. Browse reports
bb bugs            # interactive table via gum
bb bugs json       # dump to bugs.json
bb reports         # all report types
bb roles           # per-mailbox admin/maintainer/ignored
```

## bb tasks

```
bb digest [--all]         Digest emails into reports
bb bugs [json]            Bug reports
bb patches [json]         Patch reports
bb requests [json]        Requests
bb announcements [json]
bb releases [json]
bb changes [json]
bb reports [json]         All reports
bb roles                  Per-mailbox roles
bb validate-config [path] Validate config.edn
```

## Report types

Detected from email subject tags:

| Tag                                                     | Type           |
|---------------------------------------------------------|----------------|
| `[BUG]` `[BUG version]`                                 | bug            |
| `[PATCH]` `[PATCH n/m]` `[PATCH topic n/m]`             | patch          |
| `[POLL]` `[FR]` `[RFC]`                                 | request        |
| `[ANN]` `[ANNOUNCEMENT]` `[BLOG]`                       | announcement * |
| `[REL]` `[RELEASE]` `[REL version]` `[RELEASE version]` | release *      |
| `[CHG]` `[CHANGE]` `[CHG version]` `[CHANGE version]`   | change *       |

\* Require admin or maintainer permission.

Patches are also detected from `.patch`/`.diff` attachments and inline
diffs (git format).

## Triggers

State changes detected from body lines (at start of line, followed by
`.` `,` `;` or `:`):

| Trigger          | Effect on report            |
|------------------|-----------------------------|
| `Approved.`      | acked (bug, patch, request) |
| `Confirmed.`     | acked (bug)                 |
| `Reviewed.`      | acked (patch)               |
| `Handled.`       | owned                       |
| `Fixed.`         | closed (bug)                |
| `Applied.`       | closed (patch)              |
| `Done.`          | closed (request)            |
| `Canceled.`      | closed (all)                |
| `Urgent.`        | urgent                      |
| `Important.`     | important                   |
| `Not urgent.`    | un-urgent                   |
| `Not important.` | un-important                |

## Roles

Per-mailbox, managed via email body commands:

- **Admin** — one per mailbox, set in `config.edn` (`:admin` field),
  changeable via `Add admin: new@addr`.  Can manage all roles.
- **Maintainers** — can add maintainers, ignore addresses, create
  announcements/releases/changes.
- **Ignored** — emails from these addresses are skipped.

Commands (admin-only unless noted):

```
Add admin: new-admin@example.com
Add maintainer: dev@example.com        (admin or maintainer)
Remove maintainer: dev@example.com
Ignore: spammer@example.com            (admin or maintainer)
Unignore: user@example.com
```

## Config

See `config.edn.example`.  Per-mailbox fields:

| Field                 | Required | Description                                           |
|-----------------------|----------|-------------------------------------------------------|
| `:host`               | yes      | IMAP server hostname                                  |
| `:user`               | yes      | IMAP login username                                   |
| `:password`           | *        | IMAP password (* or `:oauth2-token`)                  |
| `:folder`             | yes      | IMAP folder (usually `"INBOX"`)                       |
| `:email`              | yes      | Public email address for this mailbox                 |
| `:mailing-list-email` | no       | Associated mailing list address                       |
| `:admin`              | no       | Admin for this mailbox (fallback: top-level `:admin`) |

## License

Copyright © 2026 Bastien Guerry

Distributed under the Eclipse Public License 2.0.
