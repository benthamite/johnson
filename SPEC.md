# johnson — multi-format dictionary UI for Emacs

Named after Samuel Johnson, author of *A Dictionary of the English Language* (1755).

`johnson` is a multi-format dictionary UI for Emacs, providing the functionality of programs such as GoldenDict and StarDict. It implements native Elisp parsing for all supported formats — no external tools required.

## Target environment

- **Emacs 30+** (required; leverages built-in sqlite, native JSON, native compilation)
- **No external dependencies** — pure Elisp, all format parsing done natively
- **Platform:** any platform Emacs 30 supports (macOS, Linux, Windows)

## Supported dictionary formats

Implementation order (by priority):

1. **DSL** (ABBYY Lingvo) — `.dsl` files — v0.1 (MVP)
2. **StarDict** — `.ifo`/`.idx`/`.dict`/`.dict.dz`/`.syn` files — v0.2
3. **MDict** — `.mdx`/`.mdd` files — v0.3
4. **BGL** (Babylon) — `.bgl` files — v0.4

Deferred to future versions:
- EPWING
- DICT protocol (RFC 2229)
- `.dsl.dz` compressed DSL files (dictzip random-access support)
- `*_abrv.dsl` abbreviation table files (for `[p]` tag expansion)

## Package structure

Modular design. Each format is a separate file that registers itself with the core.

```
johnson.el              ; core: lookup engine, results buffer, UI, history
johnson-db.el           ; sqlite index management
johnson-dsl.el          ; DSL format backend
johnson-stardict.el     ; StarDict format backend (v0.2)
johnson-mdict.el        ; MDict format backend (v0.3)
johnson-bgl.el          ; BGL format backend (v0.4)
```

Distribution: start as a personal package (elpaca), keep packaging conventions in mind for potential future MELPA/ELPA publication.

## Architecture

### Format backend interface

Each format backend registers itself with the core via a **plist-based registry**. A backend calls `johnson-register-format` with a plist of handler functions:

```elisp
(johnson-register-format
 :name "dsl"
 :extensions '("dsl")
 :detect #'johnson-dsl-detect
 :parse-metadata #'johnson-dsl-parse-metadata
 :build-index #'johnson-dsl-build-index
 :retrieve-entry #'johnson-dsl-retrieve-entry
 :render-entry #'johnson-dsl-render-entry)
```

The handler functions implement these operations:

1. **`:detect`** `(path) → boolean` — given a file path, determine if it is a dictionary in this format
2. **`:parse-metadata`** `(path) → plist` — extract dictionary name, source language, target language, and other metadata; returns `(:name STRING :source-lang STRING :target-lang STRING ...)`
3. **`:build-index`** `(path callback) → nil` — parse the dictionary file and call `(funcall callback headword byte-offset byte-length)` for each entry; the core handles sqlite insertion
4. **`:retrieve-entry`** `(path byte-offset byte-length) → string` — read the raw entry text from the dictionary file at the given location
5. **`:render-entry`** `(raw-text) → nil` — insert the rendered entry into the current buffer using text properties and faces (called with point at the insertion position)

### Indexing and storage

- **sqlite** (Emacs 30 built-in) — one `.sqlite` database file per dictionary
- **Index location** — user-configurable via `johnson-cache-directory` (default: `~/.cache/johnson/`); critical since dictionary source directories may be read-only or on cloud storage
- **Index file naming** — derive from dictionary file path hash to avoid collisions: `(md5 (expand-file-name path))` + `.sqlite`
- **Index structure** per database:
  - `metadata` table: `key TEXT PRIMARY KEY, value TEXT` — stores dictionary name, source/target language, format, source path, file modification time, entry count
  - `entries` table: `headword TEXT NOT NULL, headword_normalized TEXT NOT NULL, byte_offset INTEGER NOT NULL, byte_length INTEGER NOT NULL`
  - Index: `CREATE INDEX idx_normalized ON entries(headword_normalized)` for fast prefix matching
  - For entries with multiple headwords (consecutive flush-left lines mapping to the same definition body), each headword gets its own row in the `entries` table, all pointing to the same byte offset and length
- **Staleness detection** — on load, compare file modification time stored in metadata against the actual file's mtime; re-index if the source file has changed
- **Initial indexing** — reported in a dedicated `*johnson-indexing*` buffer showing one line per dictionary: `"Indexing Oxford English-Spanish... done (45,231 entries)"`. Errors during indexing are logged as warnings (malformed entries are skipped with a count: `"3 entries skipped due to parse errors"`)

### Headword normalization

For case-insensitive and accent-insensitive search, headwords are normalized before storage in `headword_normalized`:

1. Unicode NFKD decomposition (via `ucs-normalize-NFKD-string`)
2. Strip combining diacritical marks (Unicode category Mn — Nonspacing_Mark)
3. Downcase (via `downcase`)

This means searching `cafe` matches `café`, `CAFÉ`, etc.

User input in `completing-read` and `johnson-lookup` is normalized with the same function before querying.

### Encoding

Auto-detect encoding using:
1. BOM detection (UTF-16LE, UTF-16BE, UTF-8 with BOM)
2. Format-specific headers (DSL `#` headers are always on the first lines)
3. Default to UTF-8 if no BOM detected

### File I/O and buffer cache

For runtime entry retrieval, dictionary files are cached as Emacs buffers:

- Use `find-file-noselect` with raw/literal settings (`coding-system-for-read` set to `raw-text`, `inhibit-read-only` t, no undo tracking, no major mode)
- Buffers are kept alive for repeated lookups — the OS handles memory mapping
- Buffer names use a `" *johnson-cache: "` prefix (leading space hides from buffer lists)
- Provide `johnson-close-caches` command to manually kill all cache buffers if needed
- Entry retrieval: given byte-offset and byte-length, use `buffer-substring-no-properties` on the cached buffer with appropriate position math

## Dictionary configuration

### Discovery

- **Directory-based auto-discovery** — user sets `johnson-dictionary-directories` (list of paths); johnson recursively scans for recognized dictionary files using the registered format backends' `:detect` functions
- Default search path: `("~/.local/share/dictionaries/")`

### Groups

- **Auto-detected from metadata** — dictionaries are grouped by language pair, derived from dictionary metadata (`#INDEX_LANGUAGE`/`#CONTENTS_LANGUAGE` in DSL, equivalent fields in other formats)
- Group keys are `"SOURCE → TARGET"` strings (e.g., `"English → Spanish"`)
- **User refinement** — users can rename, merge, split, or create custom groups via `johnson-dictionary-groups`
- Each dictionary has a user-configurable **priority** (integer) that determines display order within results; default priority is 0; lower numbers display first

### Search scope

- **Configurable default** — `johnson-default-search-scope` can be `'all` (search all dictionaries) or `'group` (search within the currently active group)
- `johnson-select-group` shows all real groups plus an `"All"` pseudo-group; selecting `"All"` sets scope to `'all`; selecting a real group sets scope to `'group` and activates that group

## Search

### Search modes (v0.1)

- **Exact match** — case-insensitive, accent-insensitive (queries the normalized column)
- **Prefix matching** — used for completion; `SELECT headword FROM entries WHERE headword_normalized LIKE 'prefix%'`

Deferred:
- Wildcard patterns (`?`/`*`)
- Fuzzy/approximate matching
- Full-text search within definitions

### Lookup trigger

- **Primary command:** `johnson-lookup` — interactive command, prompts for a word with `completing-read`, defaults to word at point
- **Completion** — dynamic completion table backed by sqlite prefix queries; each keystroke triggers a `LIKE 'input%'` query against the normalized column; results are deduplicated across dictionaries
- **Completion annotations** — using `completion-extra-properties` with `:annotation-function`, each candidate shows the number of dictionaries containing it, e.g., `" (3 dicts)"`
- **History** — `johnson-history` variable stores recent lookups; accessible via `M-p`/`M-n` in the `completing-read` prompt; persisted across sessions if `savehist-mode` is active

### Completion at point (CAPF)

- `johnson-completion-at-point-function` — a CAPF backend that completes words from the dictionary index
- Respects the current search scope (group vs all)
- Users add it to `completion-at-point-functions` in desired modes

## DSL format — v0.1 specification

### File structure

DSL files are plain text (UTF-8 or UTF-16) with:

1. **Metadata headers** (lines starting with `#`):
   - `#NAME "Dictionary Name"`
   - `#INDEX_LANGUAGE "English"`
   - `#CONTENTS_LANGUAGE "Spanish"`
   - `#SOURCE_CODE_PAGE` (legacy encoding hint)
   - `#ICON` (icon file reference, ignored in v0.1)

2. **Entries**: headword on a line flush to the left margin, followed by indented definition lines (one tab or spaces)

### Headword parsing

Full DSL headword syntax:
- **Simple headwords** — plain text on a flush-left line
- **Alternation** — `colour{/}color` produces two index entries (both `colour` and `color`) pointing to the same definition
- **Optional parts** — `go(es)` produces entries for both `go` and `goes`
- **Multiple headwords** — consecutive flush-left lines before the indented body all map to the same entry; each headword gets its own row in the index, pointing to the same byte offset and length
- **Escaped characters** — `\[`, `\]`, `\{`, `\}`, `\(`, `\)` for literal brackets/braces/parens in headwords

### DSL markup tags

Full fidelity rendering of all standard DSL tags. Each tag is converted to Emacs text properties and faces (direct rendering, no shr).

| DSL tag | Meaning | Rendering |
|---------|---------|-----------|
| `[b]...[/b]` | Bold | `johnson-bold-face` (inherits `bold`) |
| `[i]...[/i]` | Italic | `johnson-italic-face` (inherits `italic`) |
| `[u]...[/u]` | Underline | `johnson-underline-face` (`:underline t`) |
| `[c]...[/c]` | Color (default green) | `johnson-color-default-face` |
| `[c NAME]...[/c]` | Named color | mapped to one of ~8 `johnson-color-*-face` faces (see Color faces below) |
| `[sup]...[/sup]` | Superscript | `display` property `(raise 0.3)` + smaller height |
| `[sub]...[/sub]` | Subscript | `display` property `(raise -0.3)` + smaller height |
| `[ex]...[/ex]` | Example | `johnson-example-face` (distinct styling, e.g., italic + dimmed) |
| `[*]...[/*]` | Optional/secondary | `johnson-optional-face` (reduced opacity — dimmed foreground) |
| `[ref]...[/ref]` | Cross-reference | `johnson-ref-face` + button with `johnson-lookup` action |
| `[url]...[/url]` | External URL | `johnson-url-face` + button with `browse-url` action |
| `[lang id=N]...[/lang]` | Language identifier | no visual change; preserved as text property for potential future use |
| `[trn]...[/trn]` | Translation block | block-level: ensure preceded/followed by blank line |
| `[!trn]...[/!trn]` | Non-translation block | block-level: ensure preceded/followed by blank line |
| `[com]...[/com]` | Comment | `johnson-comment-face` (annotation styling) |
| `[s]...[/s]` | Media/resource reference | ignored for v0.1 (text-only) |
| `[m]` / `[m1]`-`[m9]` | Margin/indentation level | `line-prefix` and `wrap-prefix` display properties; level N = N×2 spaces |
| `[p]` | Abbreviation marker | rendered as-is (no expansion in v0.1; `_abrv.dsl` support deferred) |
| `[']...[/']` | Stress mark / accent | `johnson-stress-face` (e.g., bold or colored accent) |
| `{{...}}` | Media reference | ignored for v0.1 |
| `<<...>>` | Alternate headword reference | resolved during indexing; rendered as cross-reference link |

### Color faces

DSL color names are mapped to a fixed set of customizable faces. The mapping covers the most common colors used in DSL dictionaries:

| DSL color name(s) | Face |
|--------------------|------|
| (default, no name) | `johnson-color-default-face` (green) |
| `green`, `darkgreen` | `johnson-color-green-face` |
| `red`, `darkred`, `crimson` | `johnson-color-red-face` |
| `blue`, `darkblue`, `steelblue` | `johnson-color-blue-face` |
| `gray`, `darkgray`, `dimgray` | `johnson-color-gray-face` |
| `brown`, `saddlebrown` | `johnson-color-brown-face` |
| `violet`, `purple`, `darkviolet` | `johnson-color-violet-face` |
| `orange`, `darkorange` | `johnson-color-orange-face` |
| (any unrecognized name) | `johnson-color-default-face` (fallback) |

All faces are defined with `defface` so users can customize them to match their theme.

### DSL tag parsing implementation

The DSL tag parser processes entry text in a single pass:

1. **Input**: raw entry text (the indented body lines of an entry, joined with newlines)
2. **Tag regex**: `\[/?[a-z!*'][^]]*\]` — matches opening and closing DSL tags
3. **Stack-based parsing**: maintain a stack of active tags; on opening tag push, on closing tag pop and apply the corresponding face/property to the enclosed region
4. **Output**: text inserted into the results buffer with appropriate text properties

### Encoding detection implementation

```
1. Read first 4 bytes of the file
2. Check for BOM:
   - FF FE → UTF-16LE
   - FE FF → UTF-16BE
   - EF BB BF → UTF-8 (with BOM)
3. If no BOM, default to UTF-8
4. Decode the file with the detected encoding
5. Validate: first non-empty line should start with '#' (DSL metadata header)
```

For UTF-16 files, the indexing byte offsets must account for the multi-byte encoding: offsets are stored as byte positions in the file, and retrieval decodes from raw bytes using the detected encoding.

## Results buffer

### Buffer name and mode

- Buffer: `*johnson*`
- Major mode: `johnson-mode` (derived from `special-mode`)
- Read-only buffer

### Layout

Unified buffer with collapsible sections — one section per dictionary that has results.

```
━━ Oxford English-Spanish ━━━━━━━━━━━━━━━━━━━━━━━━━━━━
[rendered definition content]

━━ Collins English-Spanish ━━━━━━━━━━━━━━━━━━━━━━━━━━━
[rendered definition content]

━━ VOX Advanced ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
[rendered definition content]
```

Section headers use Unicode box-drawing character `━` (U+2501). The header format is: `━━ DICTIONARY-NAME ━━━━━━━━` padded to `fill-column` width. Headers have a dedicated `johnson-section-header-face`.

### Section collapsing

- **Overlay-based hiding** — each dictionary section's content (everything between the header and the next header or end of buffer) is wrapped in an overlay
- Sections are **expanded by default**
- `TAB` on a section header toggles the overlay's `invisible` property
- Collapsed sections show an indicator (e.g., `[+]` appended to the header or `...` ellipsis via `display` property on the overlay)

### Display rules

- **Priority-based ordering** — dictionaries displayed in user-configured priority order (lower priority number first; default 0)
- **Hide empty** — dictionaries with no match for the query are not shown
- **Current word displayed** — the buffer header or mode-line shows the currently looked-up word

### Navigation keybindings

Single-key bindings (buffer is read-only, like Info mode):

| Key | Action |
|-----|--------|
| `n` | Next entry/section |
| `p` | Previous entry/section |
| `TAB` | Toggle collapse of section at point / jump to next section header |
| `S-TAB` | Previous dictionary section header |
| `RET` | Follow cross-reference at point |
| `l` | Back in navigation history |
| `r` | Forward in navigation history |
| `s` | New search (re-prompts for a word) |
| `g` | Refresh / re-display current word |
| `w` | Copy current entry as plain text |
| `q` | Quit (bury buffer) |

### Cross-references

- Cross-references rendered as clickable buttons (using `make-text-button`) with `johnson-ref-face`
- `RET` on a cross-reference triggers a new `johnson-lookup` for the referenced word
- Cross-reference search spans **all loaded dictionaries** (not just the one containing the reference), regardless of current scope
- **Navigation history** — eww/Info-style: `l` goes back, `r` goes forward
  - History stores search terms only (not rendered content)
  - Going back re-executes the lookup for the previous term
  - History is a simple list with a position index: `johnson--history-list` and `johnson--history-position`

### Copy

- `w` copies the current dictionary section's entry as **plain text** (all markup/properties stripped) to the kill ring
- "Current section" is determined by point position — whichever section the cursor is in
- Section boundaries are determined by the section header overlays

## History

- **Simple history ring** — standard Emacs-style, accessible via `M-p`/`M-n` in the `completing-read` prompt
- Backed by `johnson-history` variable, persisted across sessions if `savehist-mode` is active
- Maximum history length: `johnson-history-max` (default 100)

## Dictionary list buffer

`johnson-list-dictionaries` opens a `*johnson-dictionaries*` buffer using `tabulated-list-mode`.

### Columns

| Column | Description |
|--------|-------------|
| Name | Dictionary name from metadata |
| Format | Format identifier (e.g., "DSL") |
| Languages | "Source → Target" language pair |
| Entries | Number of indexed entries |
| Status | "Indexed", "Needs reindex", "Error" |
| Path | File path (truncated) |

### Keybindings

| Key | Action |
|-----|--------|
| `i` | Re-index the dictionary at point |
| `g` | Refresh the list |
| `RET` | Show dictionary details (path, full metadata) |
| `q` | Quit |

## Testing

### Unit tests

- ERT test suite with hand-crafted fixture dictionaries
- Test fixtures: small DSL files exercising all markup tags, headword alternation, encoding variants
- Test coverage for: parsing, indexing, search (exact, prefix), markup rendering, cross-reference resolution, encoding detection, normalization

### Test fixtures for v0.1

```
test/
  johnson-test.el           ; core tests (history, search, rendering integration)
  johnson-db-test.el        ; database tests (index creation, queries, staleness)
  johnson-dsl-test.el       ; DSL parser tests (headwords, markup, encoding)
  fixtures/
    test-dict.dsl           ; basic UTF-8 test dictionary with various markup tags
    test-dict-utf16.dsl     ; UTF-16LE encoded test dictionary (with BOM)
    test-dict-alternation.dsl ; headword alternation and optional parts tests
    test-dict-multihead.dsl ; multiple headwords per entry tests
    test-dict-crossref.dsl  ; cross-reference tests
```

Each fixture dictionary is minimal (5-20 entries) but exercises specific features.

### Integration tests

- Optional integration tests that run against real dictionary files when available
- Controlled by `johnson-test-dictionary-directory` variable; tests are skipped when the directory is not present
- Test actual lookup and rendering quality against known entries

## v0.1 milestone: DSL-only MVP

### Goal

Replace GoldenDict for DSL dictionaries. Must be able to:

1. Auto-discover and index all uncompressed DSL dictionaries from configured directories
2. Look up any word and see nicely formatted results from all matching DSL dictionaries
3. Navigate cross-references with back/forward history
4. Switch between dictionary groups (by language pair)
5. Use `completing-read` with dynamic sqlite-backed prefix matching for word input
6. Copy definitions as plain text

### Files for v0.1

```
johnson.el          ; core module
johnson-db.el       ; sqlite index management
johnson-dsl.el      ; DSL format backend
test/
  johnson-test.el           ; core tests
  johnson-db-test.el        ; database tests
  johnson-dsl-test.el       ; DSL parser tests
  fixtures/
    test-dict.dsl           ; basic test dictionary
    test-dict-utf16.dsl     ; UTF-16 encoded test dictionary
    test-dict-alternation.dsl ; headword alternation tests
    test-dict-multihead.dsl ; multiple headwords tests
    test-dict-crossref.dsl  ; cross-reference tests
```

### Interactive commands (v0.1)

| Command | Description |
|---------|-------------|
| `johnson-lookup` | Look up a word (prompts with completing-read, defaults to word at point) |
| `johnson-index` | Index/re-index all dictionaries (with progress buffer) |
| `johnson-list-dictionaries` | Show all discovered dictionaries with status in tabulated-list buffer |
| `johnson-select-group` | Switch the active dictionary group (includes "All" pseudo-group) |
| `johnson-close-caches` | Kill all dictionary file cache buffers |

### User options (v0.1)

| Variable | Default | Description |
|----------|---------|-------------|
| `johnson-dictionary-directories` | `("~/.local/share/dictionaries/")` | Directories to scan for dictionaries |
| `johnson-cache-directory` | `"~/.cache/johnson/"` | Where to store sqlite index files |
| `johnson-default-search-scope` | `'all` | Default search scope: `'all` or `'group` |
| `johnson-dictionary-groups` | `nil` | User-defined group overrides (auto-detected by default) |
| `johnson-dictionary-priorities` | `nil` | Alist of `(dictionary-name . priority)` for display ordering |
| `johnson-history-max` | `100` | Maximum number of entries in lookup history |

## Implementation notes

### Indexing flow (DSL)

```
1. Scan johnson-dictionary-directories recursively for .dsl files
2. For each .dsl file:
   a. Check if sqlite index exists and is fresh (mtime comparison)
   b. If fresh, skip
   c. If stale or missing:
      i.   Detect encoding (BOM detection → default UTF-8)
      ii.  Parse metadata headers (#NAME, #INDEX_LANGUAGE, #CONTENTS_LANGUAGE)
      iii. Create/recreate sqlite database
      iv.  Store metadata in metadata table
      v.   Parse entries: for each flush-left line (headword), record byte offset;
           read until next flush-left line or EOF to determine byte length
      vi.  Expand headword alternations and optional parts into multiple index entries
      vii. Normalize each headword (NFKD + strip diacritics + downcase)
      viii.Insert all (headword, headword_normalized, byte_offset, byte_length) rows
      ix.  Log result to *johnson-indexing* buffer
3. After all dictionaries processed, auto-detect groups from metadata
```

### Lookup flow

```
1. User invokes johnson-lookup
2. completing-read with dynamic completion:
   a. On each input change, normalize input
   b. Query all relevant sqlite databases: SELECT DISTINCT headword FROM entries
      WHERE headword_normalized LIKE 'normalized_input%' LIMIT 100
   c. Annotate each candidate with dictionary count
3. User selects a word
4. Query all relevant databases for exact match on headword_normalized
5. For each match, retrieve raw entry via buffer cache (byte-offset + byte-length)
6. Render each entry into *johnson* buffer:
   a. Insert section header (Unicode box-drawing)
   b. Parse DSL markup and insert text with faces/properties
   c. Create overlay for section collapsing
7. Push word onto navigation history
8. Display buffer
```

### Rendering flow (DSL → text properties)

```
1. Input: raw DSL entry text (indented body lines)
2. Strip leading indentation (tab or spaces) from each line
3. Single-pass tag parser:
   a. Scan for [tag] and [/tag] patterns
   b. Maintain a stack of active tags with their start positions
   c. On closing tag: pop stack, apply face/property to region [start, current-pos]
   d. Handle self-contained tags ([m], [m1]-[m9]) inline
   e. Handle special tags ([ref], [url]) by creating buttons
   f. Handle block tags ([trn], [!trn]) by ensuring blank line separation
   g. Skip [s], {{...}} media references
   h. Resolve <<ref>> as cross-reference buttons
4. Apply [m] indentation via line-prefix/wrap-prefix display properties
5. Plain text between tags is inserted verbatim
```

## Future versions

### v0.2 — StarDict support + .dsl.dz

- `johnson-stardict.el` backend
- Parse `.ifo` metadata, `.idx` index, `.dict`/`.dict.dz` entries
- Full dictzip random access for `.dict.dz` (parse chunk table from gzip extra field, per-chunk decompression with LRU cache)
- `.syn` synonym file support
- Handle StarDict content types: plain text, HTML, Pango markup, phonetic
- `.dsl.dz` support using dictzip random access (same implementation as StarDict)

### v0.3 — MDict support + abbreviations

- `johnson-mdict.el` backend
- Parse `.mdx` binary format (key blocks, record blocks)
- Handle MDict's HTML+CSS content
- `.mdd` resource files (deferred: media playback)
- `*_abrv.dsl` abbreviation table support for DSL dictionaries

### v0.4 — BGL support

- `johnson-bgl.el` backend
- Parse Babylon `.bgl` binary format

### Future features (unscheduled)

- Media support (images, audio pronunciation)
- EPWING format
- DICT protocol (RFC 2229) for remote dictionary servers
- Wildcard search (`?`/`*` patterns)
- Fuzzy/approximate matching
- Full-text search within definitions
- eldoc integration
- Scan-popup mode (auto-lookup on selection)
- Bookmarking entries
- Full browsable history buffer with timestamps
