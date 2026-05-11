# Repo clean-up — May 2026

**Source:** Audit of `KristofM854.github.io@main` after V3 small-fixes round.
**Audience:** Claude Code, working in the user's local clone.
**Goal:** A repo a non-engineer can navigate in 30 seconds. Move docs into `docs/`, group PDFs under `assets/pdf/`, delete dead canvas-source weight, add a README that tells future-you where text lives.

This is a structural change with no design or content edits. **Do not touch any JSX, CSS, or prose** in this PR — only file moves, deletions, reference updates, and the new README.

---

## 0. Read first

1. `_config.yml` — confirm what's currently in `exclude:`.
2. `cv.jsx`, `homepage.jsx`, and any other JSX that references files at the repo root (`/assets/cv.pdf`, `/ICHA_25_poster.pdf`, etc.) — these references must be updated when files move.
3. `Website/MIGRATION.md` and the contents of `Website/` — confirm everything worth keeping is already represented in `assets/`, `_pages/`, etc., before deleting the folder.

Run `git pull --ff-only` before branching.

---

## 1. The target structure

```
KristofM854.github.io/
├── README.md                     ← NEW
├── _config.yml
├── Gemfile
├── CNAME
├── .gitignore
│
├── index.html
├── 404.html
├── favicon.svg
├── robots.txt
│
├── _includes/
├── _layouts/
├── _pages/
├── _posts/
│
├── assets/
│   ├── css/
│   ├── js/
│   ├── images/
│   ├── data/                     ← haedat_filtered_export.csv lands here
│   └── pdf/                      ← NEW: all PDFs grouped
│       ├── cv-en.pdf             (renamed from assets/cv.pdf)
│       ├── cv-de.pdf             (renamed from Lebenslauf_…pdf)
│       └── icha-2025-poster.pdf  (renamed from ICHA_25_poster.pdf)
│
├── quiz/
├── quiz-src/
│
└── docs/                         ← NEW: all internal documentation
    ├── README.md                 (index of what's in docs/)
    ├── MIGRATION.md              (moved from Website/MIGRATION.md)
    ├── MIGRATION_FOLLOWUP.md     (moved from root)
    ├── V3_CHANGES.md             (moved from root)
    ├── V3_KICKOFF.md             (moved from root)
    └── archive/                  (kept for reference, not built)
        └── jekyll-migration-guide.md  (moved from notes/)
```

Deleted entirely: `Website/`, `notes/`, `Migration Follow-up.html`.

---

## 2. Step-by-step

Do these in order. Commit after each step that's labelled with a commit message.

### Step 1 — Create `docs/` and move internal docs

```bash
mkdir -p docs/archive
git mv MIGRATION_FOLLOWUP.md docs/
git mv V3_CHANGES.md docs/
git mv V3_KICKOFF.md docs/
git mv Website/MIGRATION.md docs/
git mv notes/CLAUDE_CODE_JEKYLL_MIGRATION_GUIDE.md docs/archive/jekyll-migration-guide.md
```

If `notes/` has other files, move anything worth keeping into `docs/archive/`. Anything that's a HAB-quiz implementation guide can be deleted — that work is done.

**Commit:** `chore: move internal docs to docs/`

### Step 2 — Delete `Website/` and `notes/`

Before deleting `Website/`: confirm nothing in `assets/` or `_pages/` references it. `git grep "Website/"` should return only matches inside markdown files (those will move with the docs) and inside `_config.yml`'s `exclude:` list.

```bash
git rm -r Website/
git rm -r notes/    # if empty after step 1
```

**Commit:** `chore: delete obsolete Website/ canvas source and notes/`

### Step 3 — Delete the one-off HTML render

```bash
git rm "Migration Follow-up.html"
```

**Commit:** `chore: remove one-off Migration Follow-up.html`

### Step 4 — Group PDFs under `assets/pdf/`

```bash
mkdir -p assets/pdf
git mv assets/cv.pdf assets/pdf/cv-en.pdf
git mv Lebenslauf_KristofMoeller_englisch.pdf assets/pdf/cv-de.pdf
git mv ICHA_25_poster.pdf assets/pdf/icha-2025-poster.pdf
```

**Filename note:** the user has flagged that `Lebenslauf_…englisch.pdf` may actually be the German CV (Lebenslauf = German for CV). **Open both PDFs and verify which is English and which is German** before renaming. If `Lebenslauf_…englisch.pdf` is genuinely English (unusual but possible), name it `cv-en.pdf` and rename `assets/cv.pdf` to `cv-de.pdf` instead. Ask the user if unclear.

Then update every reference. Run:
```bash
git grep "/assets/cv.pdf"
git grep "ICHA_25_poster"
git grep "Lebenslauf"
```

Likely hit list:
- `assets/js/cv.jsx` — the CV-download button (`/assets/cv.pdf` → `/assets/pdf/cv-en.pdf`)
- Possibly `homepage.jsx`, `about.jsx`, or `publications.jsx` — anywhere a CV link or poster link appears.

Update each hit. After: each of the three `git grep` commands above returns zero matches.

**Commit:** `chore: group PDFs under assets/pdf/`

### Step 5 — Move dataset to `assets/data/`

```bash
git mv haedat_filtered_export.csv assets/data/
git grep "haedat_filtered_export"
```

If the file is unreferenced (likely — it's a working dataset, not a published one), it can stay where it is or be removed from the repo entirely. **Ask the user:** is this dataset something readers should be able to download, or is it just a local working file that shouldn't be in git at all? If the latter, `git rm` it and add the filename to `.gitignore`.

Default action if no answer: move to `assets/data/` (it's hidden enough there) and leave it.

**Commit:** `chore: move haedat dataset to assets/data/`

### Step 6 — Update `_config.yml` exclude list

After the moves, the `exclude:` list will contain stale paths. Replace it with:

```yaml
exclude:
  - quiz-src/
  - node_modules/
  - vendor/
  - docs/
  - README.md
  - Gemfile
  - Gemfile.lock
```

`Website/` no longer exists, so it can drop out. `notes/` no longer exists. The old root-path markdown filenames are gone (they're in `docs/` now, which is itself excluded).

Run `bundle exec jekyll build` and confirm:
- No build errors
- `_site/docs/` does not appear in the build output
- `_site/Website/` does not appear
- `_site/assets/pdf/cv-en.pdf` exists

**Commit:** `chore: tidy _config.yml exclude list`

### Step 7 — Add root `README.md`

Create `README.md` at the repo root with this exact content:

```markdown
# kristofmoeller.com

Personal site for Kristof Moeller, marine biochemist at IAEA-MEL Monaco.
Jekyll + React (per-page JSX, no build step).

## Editing page text

Each page's content lives in a JSX file under `assets/js/`:

| Page          | File                          |
|---------------|-------------------------------|
| Home          | `assets/js/homepage.jsx`      |
| About         | `assets/js/about.jsx`         |
| CV            | `assets/js/cv.jsx`            |
| Publications  | `assets/js/publications.jsx`  |
| Software      | `assets/js/software.jsx`      |
| Services      | `assets/js/services.jsx`      |
| Writing index | `assets/js/writing.jsx`       |
| Blog posts    | markdown in `_posts/`         |

The files in `_pages/` are Jekyll shells that wire each JSX file into the
layout — you don't normally need to edit them.

Page styles live in `assets/css/{page-name}.css`.

## Local preview (Codespaces — works on locked-down laptops)

1. Open this repo on github.com → green **Code** button → **Codespaces**
   tab → **Create codespace on main**.
2. In the terminal:
   ```
   bundle install
   bundle exec jekyll serve --host 0.0.0.0
   ```
3. When the toast appears for port 4000, click **Open in Browser**.

## Documentation

Migration history, work-order docs, and design notes live in `docs/`.

## Blog & RSS

New posts: drop a markdown file in `_posts/` named
`YYYY-MM-DD-slug.md` with `title`, `date`, `excerpt`, `category`, and
`read_min` in the frontmatter. The RSS feed at `/feed.xml` regenerates
automatically on every build.
```

**Commit:** `docs: add root README`

### Step 8 — Add `docs/README.md` index

Create `docs/README.md`:

```markdown
# Internal docs

Not published to the website (the `docs/` folder is excluded in `_config.yml`).

- `MIGRATION.md` — original design-canvas-to-Jekyll migration plan.
- `MIGRATION_FOLLOWUP.md` — open items after the first migration round.
- `V3_CHANGES.md` — small-fixes work order (May 2026 review pass).
- `V3_KICKOFF.md` — Claude Code initiation prompt for V3.
- `archive/` — older reference material, kept but no longer active.
```

**Commit:** `docs: add docs/ index`

---

## 3. Verification

After all commits, confirm:

- [ ] `git ls-files` shows no `Website/`, no `notes/`, no root-level PDFs, no root-level work-order markdown.
- [ ] `bundle exec jekyll serve` runs cleanly.
- [ ] `/cv/` still has a working CV download link (verify the link points to `/assets/pdf/cv-en.pdf` and that the file exists).
- [ ] `/feed.xml` still generates.
- [ ] No 404s in DevTools Network tab on any page.
- [ ] `git grep "cv.pdf"` returns zero matches (everything should now reference `cv-en.pdf`).

---

## 4. Out of scope

- No design or prose changes. If you spot something while moving files, file an issue; don't fix it here.
- The two open items from `docs/MIGRATION_FOLLOWUP.md` (portrait optimisation, About-page DRAFTs) stay open after this PR — they're separate work.
- Quiz folder structure (`quiz/`, `quiz-src/`) is left alone — that's a separate concern.

---

## 5. PR shape

- **Branch:** `repo-cleanup`
- **Title:** `chore: repo clean-up — group docs, PDFs, drop dead weight`
- **Eight commits** in the order above.
- **PR body:** include a before/after `ls` of the repo root and the verification checklist above with each box ticked.
