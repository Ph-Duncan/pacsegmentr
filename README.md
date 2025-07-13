<!-- Badges -->
![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)
<!-- Uncomment and update these once GitHub is set up -->
<!-- ![Build Status](https://github.com/Ph-Duncan/pacsegmentr/actions/workflows/R-CMD-check/badge.svg) -->
<!-- ![Coverage](https://codecov.io/gh/Ph-Duncan/pacsegmentr/branch/main/graph/badge.svg) -->

# pacsegmentr

**pacsegmentr** is a flexible and inclusive R package designed to standardize demographic categorization across surveys and datasets. It provides tools for consistent recoding of race/ethncity, income, disability, newcomer status, and more aligning with ParticipACTION's segmentation strategy. Thus enabling clean, comparable and rapid analysis of self-report demographic data. Functions will be updated to reflect ongoing updates in the segmentation strategy.


## Installation

```r
# After publishing to GitHub:
# remotes::install_github("Ph-Duncan/pacsegmentr")
```

## Available Functions

- `categ_racialized()`
- `categ_indigenous()`
- `categ_disability()`
- `categ_income()`
- `categ_gender()`
- `categ_trans_status()`
- `categ_sexual_orientation()`
- `categ_slgbtq()`
- `categ_age_numeric()`
- `categ_age_groups()`
- `categ_newcomer()`
- `categ_fsa_urban()`
- `recode_income()`
- `get_default_mapping()`

See `?get_default_mapping()` for information on modifiable default maps.

## Author

Markus Duncan  
[GitHub: @Ph-Duncan](https://github.com/Ph-Duncan)

## License

MIT

## Additional Recommended Packages:
questionr is highly recommended for wrangling "select all that apply"-style questions where data are stored in a single variable separated by a character. Arranging data in this format is a pre-requisite for several functions in **pacsegmentr**