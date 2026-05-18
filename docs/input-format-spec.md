# Input File Format Specification

Reference document for the Bioassay Suite plate reader file importer.
Covers accepted file types, plate layout requirements, encoding rules,
multi-wavelength conventions, and common failure modes.

---

## Supported File Types

| Extension | Format | Notes |
|-----------|--------|-------|
| `.xlsx`   | Excel (Open XML) | Recommended for multi-wavelength data |
| `.xls`    | Excel (legacy)   | Supported; use `.xlsx` where possible |
| `.csv`    | Comma-separated values | Recommended for single-plate exports |
| `.txt`    | Tab-separated values | Supported; tab delimiter assumed |

The importer auto-detects format from the file extension. Any other
extension is rejected at upload.

**File size limit:** 10 MB (enforced by `MAX_UPLOAD_SIZE_MB` in `global.R`).

---

## Plate Layout Requirement

The file must contain a contiguous numeric block corresponding to an
8-row x 12-column (96-well) plate, or a partial plate with at least
4 columns. The importer does not require a specific position within
the file — it scans for the plate block automatically.

### Detection Strategies

The importer uses two strategies in order:

**Strategy 1 — Row labels (A–H):** If the first column contains the
letters A through H in eight consecutive rows, the importer treats that
block as the plate. Column headers (1–12) in the row immediately above
are optional but improve detection confidence. At least 4 numeric
columns must follow the row labels.

**Strategy 2 — Unlabeled numeric array:** If no A–H labels are found,
the importer searches for an 8-row block where every row contains at
least 4 valid numeric values and more than 70% of cells are non-missing.
At least 32 valid cells are required in the block.

Both strategies accept partial plates (fewer than 12 columns). Missing
columns are padded with `NA`.

---

## CSV Specifics

- **Separator:** Comma (`,`) is assumed. Semicolons are not
  auto-detected; export from your plate reader software using comma
  delimiters.
- **Decimal separator:** Both `.` (dot) and `,` (comma) are supported.
  If all numeric values in the plate block use comma decimals and no dot
  decimals are present, the importer converts them automatically and
  issues a warning. Mixed formats within the same file are not supported.
- **Encoding:** UTF-8 and Latin-1 (ISO 8859-1) are handled by R's
  default `read.csv()`. Files with BOM markers are accepted.
- **Headers:** Column headers (row of integers 1–12) and row labels
  (column of letters A–H) are optional. The importer works with or
  without them.
- **Quotes:** Values may be quoted or unquoted. Quoted strings that
  cannot be parsed as numbers are treated as `NA`.

### Minimal CSV example

```
A,8468,8611,8742,8768,8601,8285,7342,5647,4043,2577,1280,612
B,7668,7691,7751,321,259,223,5123,3346,1552,924,501,340
C,7384,7260,7148,8024,7814,7458,6781,5291,3424,1965,1117,543
D,6144,6124,6078,318,244,213,8335,8100,7736,7017,5776,4089
E,3280,3334,3382,2615,1328,648,383,265,228,8689,8473,8124
F,1404,1410,1423,7395,6383,4733,2944,1538,779,425,296,239
G,332,326,321,8043,7909,7722,7367,6575,5137,3177,1698,885
H,253,251,249,489,312,238,209,4912,2772,1412,702,380
```

See `examples/rba_stx_example.csv` for a real RBA file and
`examples/elisa_cortisol_example.csv` for a real ELISA file.

---

## Excel Specifics

- **Sheet selection:** The first sheet (`sheet = 1`) is read by default
  for single-wavelength files. If no plate block is found on sheet 1,
  the importer tries all remaining sheets and uses the first one that
  contains a valid plate block.
- **Merged cells:** Merged cells are read as the value in the top-left
  cell; all other cells of the merged region appear as `NA`. This can
  cause detection failures if merged cells overlap the plate data block.
  Unmerge cells in the plate data region before uploading.
- **Formatting:** Cell background colour, fonts, and number formats are
  ignored. Only cell values are read.
- **Named ranges:** Not used. The importer always scans cell values.

---

## Multi-Wavelength (Excel Only)

Multi-wavelength files must be Excel (`.xlsx` or `.xls`). A single sheet
must contain multiple plate blocks, each preceded by a wavelength marker
cell in the format:

```
Raw Data (450)
```

The number in parentheses is the wavelength in nm. The importer uses
this exact pattern (case-insensitive). Each wavelength block must have:

- The marker cell in column B of the marker row.
- A header row one row below the marker with column numbers 1–12.
- Eight data rows (A–H) with numeric values starting two rows below the
  marker.

Detected wavelengths are sorted numerically (e.g. 450 nm before 630 nm).
Wavelength labels in the app use the format `450nm`, `630nm`, etc.

If the file contains only one wavelength block matching the pattern, it
is treated as a single-wavelength file (not a multi-wavelength import).

**Multi-wavelength is not supported for CSV or TXT files.**

---

## Overflow and Saturation Values

The following strings are recognised as overflow or saturation markers
and converted to `NA` automatically:

`#SAT`, `OVER`, `ERR`, `****`, `Overfl`, `Sat`

A warning is issued reporting how many wells were affected.

---

## File Size Limit

The maximum upload size is **10 MB** (`MAX_UPLOAD_SIZE_MB` in
`global.R`). This limit is enforced by Shiny at the HTTP request level
(`options(shiny.maxRequestSize)`). Files exceeding this limit are
rejected before the importer runs.

Typical plate reader export files are well under 1 MB. The 10 MB limit
accommodates Excel files containing many worksheets or embedded images.

---

## Example Files

| File | Assay | Description |
|------|-------|-------------|
| `examples/rba_stx_example.csv` | RBA | Saxitoxin 8-point standard curve, triplicate layout |
| `examples/elisa_cortisol_example.csv` | ELISA | Cortisol competitive ELISA, absorbance values |

Use these files with the Quick Start presets on the Configuration tab to
verify a complete analysis without external data.

---

## Common Problems

**No plate block detected.**
The importer requires at least 4 columns x 8 rows of numeric data. Files
containing only text, empty sheets, or data formatted as a wide
pivot table will fail. Ensure the plate reader software exports raw
numeric values, not formatted reports.

**Wrong plate region detected.**
If the file contains multiple numeric tables (e.g. a summary table above
the plate data), the importer may select the wrong block. Remove or
relocate non-plate tables, or place the plate data at the top of the
file.

**Merged cells in the plate block.**
Merged cells produce `NA` values in the merged region, breaking numeric
detection. Unmerge all cells within the 8x12 data area before uploading.

**European decimal format not converted.**
Conversion is applied only when the file contains no dot-decimal values
at all. If the file mixes dot and comma decimals (e.g. some values
formatted as `1.234,56`), conversion is skipped and those cells become
`NA`. Use consistent decimal formatting within the plate block.

**Excel file from a multi-plate run treated as single-wavelength.**
Multi-wavelength detection requires the exact marker string
`Raw Data (NNN)` in column B. Custom or translated plate reader software
may use different marker text. Rename the marker cells to match the
expected pattern, or export each wavelength to a separate file and import
individually.

**File size rejected.**
Files larger than 10 MB are rejected. If your Excel file is oversized,
remove unused sheets, embedded images, or formatting before uploading.
