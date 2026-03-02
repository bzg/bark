# BARK — Bug And Report Keeper

BARK is an email-driven report tracker.

BARK monitors IMAP mailboxes, detects report types from subject tags,
applies state triggers from email body, and manages roles per mailbox.

## How it works

- **bark-ingest** — JVM process, connects to IMAP via IDLE, stores
  emails in Datalevin.
- **bark-digest** — Babashka script, scans new emails, creates/updates
  reports and roles in the DB.
- **bark-egest** — Babashka script, exports all reports as JSON, RSS,
  or Org.
- **bark-suggest** — Babashka script, displays reports interactively
  via fzf from a JSON file, URL, or stdin.
- **bark-html** — Babashka script, generates a static HTML page with
  embedded report data (Pico CSS, search, sortable columns).

## Dependencies

- [Babashka](https://babashka.org/) (bb)
- [fzf](https://github.com/junegunn/fzf) — fuzzy finder used by
  bark-suggest for interactive display
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

# 4. Browse reports interactively (exports JSON then opens fzf)
bb reports

# 5. Export in other formats
bb export json     # → reports.json
bb export rss      # → reports.rss
bb export org      # → reports.org

# 6. Generate a static HTML page
bb html            # → index.html
```

## bb tasks

```
bb digest [--all]              Digest emails into reports
bb export [json|rss|org]       Export all reports (default: json)
bb reports                     Export JSON + interactive display via fzf
bb suggest -f FILE | -u URL    Display reports from file, URL, or stdin
bb html [-o file]              Static HTML page (default: index.html)
bb config [path]               Validate config.edn
bb test                        Run integration tests
```

## Usage

### Creating reports and announcements

**Reports** (bug, patch, request) can be created by anyone sending an
email with the appropriate subject tag to a monitored mailbox.

**Announcements** (announcement, release, change) can only be created
by admins or maintainers.

See the [Report types](#report-types) table below for the full list of
subject tags.

Patches are also detected from `.patch`/`.diff` attachments and inline
diffs (git format).

### Updating reports

State changes happen when a reply in the same thread contains a
trigger word at the beginning of a line, followed by a punctuation
mark (`.` `,` `;` `:`).  For example, replying to a bug report with a
line starting with `Confirmed.` marks the bug as acked.

Multiple triggers can appear in the same email:

```
Confirmed.
Urgent.
Important.
```

This marks the bug as confirmed, urgent, and important (highest
priority).

Priority cannot be set directly — it is computed from the combination
of urgent and important flags.

See the [Triggers](#triggers) table below for the full list.

### Maintainer actions via X-Bark headers

Admins and maintainers can use triggers followed by an email address
to attribute the action to someone:

```
Owned: dev@example.com
Acked: reviewer@example.com
```

### Updating without notifying the list

Admins and maintainers can reply directly to the BARK inbox (adding it
in the `To:` field) instead of replying to the list.  This updates the
report without sending a message to the mailing list.

This is also useful for reclassifying emails: if someone forgot the
`[BUG]` prefix, a maintainer can edit the subject and resend the email
to the BARK inbox.

### Voting on requests

Requests (`[POLL]`, `[FR]`, `[RFC]`, etc.) support voting: reply with
`+1` or `-1` in the body.

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

### Report states

All reports can be in a combination of three states:

- **Acked** — someone took the next reasonable action (confirmed a
  bug, reviewed a patch, approved a request).
- **Owned** — someone claimed ownership of this report.
- **Closed** — the report is resolved (fixed, applied, done, or
  canceled).

Announcements, releases, and changes can only be closed.

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
