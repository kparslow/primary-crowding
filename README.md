# primary-crowding
Author: Katherine Parslow  
Affiliation: Vanderbilt University, Department of Economics

## Status
Active research repository. Contents include the working paper, supporting technical notes, and computational simulations used for numerical results and diagnostics.

## Overview
This repository houses ongoing work on behavior distortion in two-stage electoral competition. The main artifact is the working paper **“Crowded Primaries and Weakened Nominees”**, supported by:

- **Technical notes** (LaTeX) that develop key building blocks and derivations
- A **full prose model description** (LaTeX) used in the paper draft
- **Computational code** (R) for numerical analysis, simulation, and figure/table generation

## Repository structure
- `paper/` — LaTeX sources for the paper and notes
  - `paper/sections/`
    - `paper/sections/model.tex` — more extensive prose description of the model
  - `paper/notes/tech/`
    - `paper/notes/tech/tech_building_blocks.tex` — technical building blocks notes
- `computational/` — R code for simulations and generating numerical outputs

## Building PDFs (GitHub Actions)
A GitHub Actions workflow can compile a chosen LaTeX root file manually:

1. Go to the **Actions** tab
2. Select the LaTeX build workflow
3. Click **Run workflow**
4. Set `root_file` (relative to repo root), e.g.:
   - `paper/notes/tech/tech_building_blocks.tex`
5. Download the compiled PDF from the run’s **Artifacts**

## Notes
- Technical notes may include internal links (via `hyperref`) to jump to longer sections included later in the same PDF.
- If compilation fails, check the workflow logs for the first LaTeX error (missing inputs/packages are common).
