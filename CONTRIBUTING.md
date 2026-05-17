# Contributing to BARK

This document is for people who want to read, modify, or extend the
BARK codebase.

## Reporting and patching

BARK uses its own conventions, on the project's own mailing list:

- Bug report: email
  [`~bzg/barkyard@lists.sr.ht`](mailto:~bzg/barkyard@lists.sr.ht) with
  subject `[BUG] bark: <short explicit description>`.
- Patch: email the same address with subject
  `[PATCH] bark: <commit summary>`.  Attach a `git format-patch` file
  or include the diff inline; either works.
- Feature request: subject `[FR] bark: <short description>`.
- General discussion: any other subject.

For private matters (security, personal context) you can write
directly to [bzg@bzg.fr](mailto:bzg@bzg.fr).

## Codebase

All database writes happen on the JVM side, in `src/`. The Babashka
scripts in `scripts/` are read-only.

- **JVM daemon** (`clj -M:run`) -- Connects to an IMAP server or a
  local Maildir, fetches new emails since the last run, digests them
  into reports, applies commands and roles, expires stale reports,
  exits.  With `--watch`, the process stays running and reacts to
  IMAP IDLE or Maildir filesystem events.

- **`bb export`** -- Reads the database and produces JSON, RSS, Org,
  ICS (calendar) feeds plus the HTML site and stats pages.

- **`bb notify`** -- Reads the database and sends notification
  emails to maintainers via SMTP.

### JVM namespaces (`src/bark/`)

| Namespace       | Role                                               |
|-----------------|----------------------------------------------------|
| `bark.main`     | Entry point (batch by default, `--watch` for IDLE) |
| `bark.ingest`   | Datalevin connection, email parsing and storage    |
| `bark.digest`   | Single-email processing (classify, detect, thread) |
| `bark.detect`   | Report type detection from subject + attachments   |
| `bark.commands` | Triggers, directives, votes                        |
| `bark.roles`    | Permission checks and role management              |
| `bark.relations`| Qualified relations between reports                |
| `bark.series`   | Patch series tracking                              |
| `bark.expire`   | Periodic report expiry (runs inside the daemon)    |
| `bark.periods`  | Per-source time-windowed config overrides          |
| `bark.tracking` | Change-tracking for incremental export             |
| `bark.common`   | Shared utilities (JVM + Babashka)                  |
| `bark.logging`  | File and email log appenders                       |

`bark.common` carries no Datalevin dependency, so it is loaded by both
the JVM and Babashka scripts. All other namespaces under `src/bark/`
are JVM-only.

### Babashka scripts (`scripts/`)

| Script                 | Role                                                  |
|------------------------|-------------------------------------------------------|
| `bark-export.clj`      | Report export (JSON, RSS, Org, patches, events, text) |
| `bark-notify.clj`      | Notification emails                                   |
| `bark-index.clj`       | HTML index page generation                            |
| `bark-stats.clj`       | Statistics and data page generation                   |
| `bark-docs.clj`        | Documentation page generation                         |
| `bark-maintenance.clj` | Orphan email purge                                    |
| `bark-email-test.clj`  | SMTP configuration test                               |
| `validate-config.clj`  | Config validation                                     |
| `bark`                 | Shell wrapper around the uberjar                      |

These scripts must never write to the Datalevin database.  Any
mutation goes through the JVM daemon.

## Development setup

Dependencies:

- [Clojure](https://clojure.org/) 1.12+ on **Java 17+** (the daemon
  needs `--add-opens` flags that are wired into `deps.edn`).
- [Babashka](https://babashka.org/) for the export, notify and
  maintenance scripts.
- [Datalevin](https://github.com/datalevin/datalevin) is pulled in as
  a library by `deps.edn` and as a pod by the bb scripts.

Clone the repository:

```sh
git clone https://codeberg.org/bzg/bark
cd bark
```

Run BARK locally:

```sh
cp config.edn.minimal config.edn      # or copy config.edn.example
$EDITOR config.edn                    # point :mailbox at your IMAP or Maildir
clj -M:run                            # single pass
clj -M:run -- --watch                 # stay running
```

To wipe the database and replay from scratch (prompts for
confirmation, destructive):

```sh
clj -M:run -- --fresh
```

## Running tests

Run the suite with `clj -M:test`.  Tests use `clojure.test` and live
in `test/bark/`; add a file `test/bark/<topic>_test.clj` to extend it
and the runner will pick it up.

Fixtures for integration tests live in `resources/emails.edn`.  When
you need to cover a new scenario in `digest_test.clj`, add a numbered
entry to that file with a brief header comment describing the case,
then reference it by message-id in the new assertion block.

## Debugging an ingest

When an email is not digested as expected:

- Run `bb maintenance --failures` to list recent command-parsing
  errors and other ingest-time warnings recorded by the daemon.
- Enable verbose logging by adding a `:logging` block to `config.edn`
  (see `config.edn.example`); the per-email trace lands in the file
  you point it at.
- Reproduce the case as a fixture: add the email to
  `resources/emails.edn` and write an assertion in
  `test/bark/digest_test.clj`.  Faster than cycling through an IMAP
  fetch, and the case stays covered in CI.

## Code conventions

- Pure functions where possible.  `bark.common`, `bark.detect`,
  `bark.commands.registry` and the tx-builders in `bark.relations`
  must stay pure (no `datalevin.core` import).
- Side-effecting functions end with `!`.  Predicates end with `?`.
- Comments explain *why*, not *what*.  Add a short docstring to
  public functions whose intent is not obvious from the name.
- ASCII punctuation in source and prose: `--` for an em-dash,
  `=>` for a logical arrow, never the Unicode glyphs.
- Commit messages follow [conventional
  commits](https://www.conventionalcommits.org/en/v1.0.0/) and the
  50/72 git rule: a ~50-character summary, a blank line, then a
  short rationale focused on the *why*.

## Versioning

BARK follows [Intentional Versioning](https://intver.org).  The
three audiences considered for every release are:

- *Users* -- end-users and sysadmins who operate BARK instances.
- *Integrators* -- consumers of the exported JSON, RSS and Org feeds.
- *Maintainers* -- maintainers of the codebase itself.

A version bump signals which audience is most affected by the change;
the changelog calls out the impact per audience.

## License

The Clojure code is distributed under the [Eclipse Public License
2.0](LICENSES/EPL-2.0.txt). The JavaScript code shipped with the HTML
export is distributed under the [Mozilla Public License
2.0](LICENSES/MPL-2.0.txt).

By sending a patch, you agree to license your contribution under the
same terms.
