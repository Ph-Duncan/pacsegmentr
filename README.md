<!-- Badges -->
![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)
<!-- Uncomment and update these once GitHub is set up -->
<!-- ![Build Status](https://github.com/Ph-Duncan/pacsegmentr/actions/workflows/R-CMD-check/badge.svg) -->
<!-- ![Coverage](https://codecov.io/gh/Ph-Duncan/pacsegmentr/branch/main/graph/badge.svg) -->

# pacsegmentr

**pacsegmentr** is a purpose built R package designed to standardize demographic categorization across surveys and datasets. It provides tools for consistent and rapid recoding of race/ethncity, income, disability, newcomer status, and more aligning with ParticipACTION's segmentation strategy despite inconsistent data collection, sources, and variable encoding. Functions must be maintained to align with ongoing updates to the segmentation strategy.


## Installation

```r
# remotes::install_github("Ph-Duncan/pacsegmentr")
```

## Available Functions

- `seg_racialized()`
- `seg_indigenous()`
- `seg_disability()`
- `seg_income()`
- `seg_gender()`
- `seg_trans_status()`
- `seg_sexual_orientation()`
- `seg_slgbtq()`
- `seg_age_numeric()`
- `seg_age_groups()`
- `seg_newcomer()`
- `seg_fsa_urban()`
- `recode_income()`
- `seg_default_mapping()`

See `?seg_default_mapping()` for information on modifiable default maps.

## Author

Markus Duncan  
[GitHub: @Ph-Duncan](https://github.com/Ph-Duncan)

## License

MIT

## Additional Recommended Packages:
**questionr** (https://cran.r-project.org/web/packages/questionr/index.html) is highly recommended for wrangling "select all that apply"-style questions where data are stored in a single variable separated by a character. Arranging data in this format is a pre-requisite for several functions in **pacsegmentr**.