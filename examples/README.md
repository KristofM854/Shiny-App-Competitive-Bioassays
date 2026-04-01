# Example Datasets

Test datasets for verifying the Bioassay Analysis App.

## Files

### rba_stx_example.csv
- **Assay:** RBA, Saxitoxin  
- **Layout:** 8 standards (triplicate, cols 1-3), samples (cols 4-12)
- **Expected:** R² > 0.99, sigmoidal DRC curve
- **Preset:** "RBA: STX 8 standards (triplicate)"

### elisa_cortisol_example.csv
- **Assay:** ELISA, Cortisol
- **Layout:** Col 1 controls (Blank/NSB/B0/TA), cols 2-3 standards, cols 4-11 samples
- **Expected:** R² > 0.98, %B/B0 normalization
- **Preset:** "ELISA: Cortisol (Cayman kit, 8-point, duplicate)"

## Usage

1. Launch the app
2. Select assay type and load the matching preset
3. Upload the corresponding example file
4. Click Generate Report

## Generating Fresh Data

To regenerate example datasets:
```r
source("examples/generate_example_data.R")
```
