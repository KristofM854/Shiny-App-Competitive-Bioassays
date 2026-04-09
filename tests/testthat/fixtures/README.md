# Test Fixtures

Representative plate reader files for automated testing and regression checking.

## Files

| File | Description | Use |
|------|-------------|-----|
| `rba_nominal.csv` | RBA saxitoxin plate (8×12, labeled A-H, CPM values) | Standard RBA import smoke test |
| `elisa_nominal.csv` | ELISA cortisol plate (8×12, labeled A-H, absorbance values) | Standard ELISA import smoke test |
| `partial_plate_6col.csv` | Partial plate (8×6, labeled A-H) | Partial-plate padding test |
| `flat_response.csv` | Flat/degenerate plate (all ~500, labeled A-H) | Interpolation-fallback trigger |

## Adding new fixtures

Place new `.csv`, `.txt`, or `.xlsx` files here. Tests reference them via
`file.path(fixture_dir, "filename.ext")` with `skip_if(!file.exists(...))` guards.
