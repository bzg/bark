# 🐕 BARK — Bug And Report Keeper

BARK is an email-driven report tracker.

BARK monitors a single IMAP mailbox, classifies incoming emails into
sources by header matching, detects report types from subject tags,
applies state triggers from email body, and manages roles per source.

To browse BARK reports from the command line, you can use
[bone](https://codeberg.org/bzg/bone).

## How it works

- **bark-ingest** — Connects to IMAP via IDLE, stores emails in the db.
- **bark-digest** — Scans new emails and creates/updates reports/roles in the db.
- **bark-export** — Exports all reports as JSON, RSS, or Org.
- **bark-html**   — Generates a static HTML page embedding report data.
- **bark-notify** — Sends notification emails to admin/maintainers.

## Dependencies

- [Babashka](https://babashka.org/) (bb)
- Java 17+ (for bark-ingest)
- Datalevin (pod for bb, library for JVM)

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

# 7. Test SMTP configuration (optional)
bb test-smtp --dry-run

# 8. Send notification emails
bb notify          # send due notifications
bb notify --dry-run --debug  # preview without sending
```

## bb tasks

```
bb digest [--all]              Digest emails into reports
bb export [json|rss|org]       Export all reports (default: json)
bb reports                     Export JSON + interactive display via fzf
bb html [-o file]              Static HTML page (default: index.html)
bb notify [--dry-run]          Send notification emails
bb config [path]               Validate config.edn
bb test                        Run integration tests
bb test-smtp [--to addr]       Test SMTP configuration
```

The `notify` command also accepts `--force` (ignore interval, send
now) and `--debug` (print filtering diagnostics).

### Filtering options

Most commands accept filtering flags:

```
-n, --source NAME          Filter by source name
-p, --min-priority 1|2|3   Only show reports with priority >= N
-s, --min-status 1-7       Only show reports with status >= N
```

## Usage

### Creating reports and announcements

**Reports** (bug, patch, request) can be created by anyone sending an
email to the monitored mailbox.

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

### Notifications

BARK can send periodic email notifications to admins and maintainers
with a summary of open reports.  Notifications are never sent to
regular users.

Notifications require SMTP configuration (see [Config](#config)) and
are sent via `bb notify`, typically from a cron job.

Each notification email contains two sections: open bugs/patches/requests
owned by the recipient, and all unacked & unowned bugs/patches/requests.

#### Subscribing and unsubscribing

Admins and maintainers are subscribed by default when `bb digest` runs
for the first time.  To unsubscribe, send an email to the BARK inbox
with this body:

```
Notify: off
```

To re-subscribe:

```
Notify: on
```

#### Setting notification preferences

Preferences are set via the same `Notify:` command.  Parameters can be
combined on a single line:

| Parameter | Description                  | Default |
|-----------|------------------------------|---------|
| `d:N`     | Receive notifications every N days | 30  |
| `p:N`     | Minimum priority (0–3)       | 0       |
| `s:N`     | Minimum status (0–7)         | 0       |

Examples:

```
Notify: d:7                 — every 7 days
Notify: d:1                 — daily
Notify: d:3 s:4 p:2         — every 3 days, only open reports with priority >= 2
Notify: p:1                 — only important or urgent reports
```

Like role commands, `Notify:` commands are only processed from direct
emails (not from mailing list traffic).

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

Per-source, managed via email body commands:

- **Admin** — one per source, set in `config.edn` (`:admin` field),
  changeable via `Add admin: new@addr`.  Can manage all roles.
- **Maintainers** — can add maintainers, ignore addresses, create
  announcements/releases/changes.
- **Ignored** — emails from these addresses are skipped.

Role commands are only processed from direct emails (not from emails
delivered through a mailing list, i.e. those with a `List-Id` header).

Commands (admin-only unless noted):

```
Add admin: new-admin@example.com
Add maintainer: dev@example.com        (admin or maintainer)
Remove maintainer: dev@example.com
Ignore: spammer@example.com            (admin or maintainer)
Unignore: user@example.com
```

## Config

See `config.edn.example`.  The configuration has three main sections:

### IMAP connection (`:imap`)

Single IMAP connection shared by all sources.

| Field            | Required | Description                          |
|------------------|----------|--------------------------------------|
| `:host`          | yes      | IMAP server hostname                 |
| `:user`          | yes      | IMAP login username                  |
| `:password`      | *        | IMAP password (* or `:oauth2-token`) |
| `:oauth2-token`  | *        | OAuth2 token (* or `:password`)      |
| `:folder`        | yes      | IMAP folder (usually `"INBOX"`)      |

### Sources (`:sources`)

Each source classifies incoming emails by header matching.  Sources
are checked in order; the first match wins.  A source without `:match`
acts as a catch-all.

| Field                 | Required | Description                             |
|-----------------------|----------|-----------------------------------------|
| `:name`               | yes      | Unique source identifier                |
| `:match`              | no       | Header matching rules (see below)       |
| `:mailing-list-email` | no       | Associated mailing list address         |
| `:admin`              | no       | Admin for this source (fallback: global)|

Match rules (all optional, all must match if present):

| Key              | Matches against   | Example                  |
|------------------|-------------------|--------------------------|
| `:list-id`       | `List-Id` header  | `"bugs.example.org"`     |
| `:delivered-to`  | `Delivered-To`    | `"bugs@example.com"`     |
| `:to`            | `To` header       | `"inbox@example.com"`    |

### Example config

```clojure
{:admin "admin@example.com"
 :imap  {:host "imap.example.com" :port 993 :ssl true
         :user "imap-login@example.com" :password "secret"
         :folder "INBOX"}
 :sources [{:name "public-list"
            :match {:list-id "bugs.example.org"}
            :mailing-list-email "bugs@example.org"
            :admin "lead@example.org"}
           {:name "private"
            :match {:delivered-to "private@example.com"}}
           {:name "direct"}]
 :db {:path "data/bark-db"}
 :ingest {:initial-fetch 50}
 :notifications {:enabled true
                 :smtp {:host "smtp.example.com" :port 587 :tls true
                        :user "notify@example.com" :password "secret"
                        :from "bark@example.com"}}}
```

### Notifications (`:notifications`)

Optional.  When absent or `{:enabled false}`, `bb notify` exits
immediately.

| Field      | Required | Description                         |
|------------|----------|-------------------------------------|
| `:enabled` | yes      | Global kill switch (`true`/`false`) |
| `:smtp`    | yes      | SMTP connection settings (see below)|

SMTP fields:

| Field       | Required | Description                |
|-------------|----------|----------------------------|
| `:host`     | yes      | SMTP server hostname       |
| `:port`     | yes      | SMTP port (usually 587)    |
| `:tls`      | no       | Enable STARTTLS (`true`)   |
| `:user`     | yes      | SMTP login username        |
| `:password` | yes      | SMTP password              |
| `:from`     | yes      | Sender address for emails  |

Use `bb test-smtp --dry-run` to validate the configuration without
sending, or `bb test-smtp` to send a test email to the global admin.

## License

Copyright © 2026 Bastien Guerry

Distributed under the Eclipse Public License 2.0.
