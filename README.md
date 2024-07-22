
# Travel Behavior Inventory Survey Helper

This repository contains a collection of R code for working with
Metropolitan Council Travel Behavior Inventory Household Survey data.

*If you came for a pretty R Data object of the TBI survey data, check
out the* `data` *folder or source from URL:*

``` r
load(url(paste0(
  "https://github.com/Metropolitan-Council/metc.tbi.helper/raw/main/data/",
  "tbi.rda"
)))
```

Other things you might find useful in here include R scripts to create
complex cross-tabs that require funky joins, some scripts for appending
auxiliary datasets & geographic information to the datasets, and more.

This repository is in active development, and anyone can contribute 🤝.
See the CONTRIBUTING page on the right to learn more.

## Organization: what’s here

All TBI survey years (2019, 2021, and 2023) are combined in the same
object, distinguished by the `survey_year` column.

- `metadata`: Read the documentation 💘
- `data`: compiled datasets generated from raw TBI data, that live in
  the Council’s database.
  - `tbi.rda` is a compressed `list` object containing:
    - `dictionary` of variable names, values, survey questions, and
      logic;
    - person-level records (`person`);
    - `day` records, for analyzing daily trends;
    - `trip` table;
    - household (`household`) records;
    - vehicle (`vehicle`) records, including fuel efficiency data; and
    - `trip_purpose`, for working with trip purpose data. This table has
      been specially weighted to attribute weights to either end of
      non-home-based trips, and to the non-home based end of home-based
      trips;
    - `mode_conflation`.

**tbi.rda is the only data we store in this GitHub repository.** Git
(even [Git LFS](https://git-lfs.github.com/)) is not ideal for storing
data. If you generate additional datasets in your work, please add them
to the .gitignore file. If you need to work with .csv data, please see
the script
[`data-raw/99-distribute-data.R`](data-raw/99-distribute-data.R), and
add the .csv’s to your `gitignore` file. If incorporating a new dataset,
write it to the database first (see @eroten for write access).

- `data-raw`: scripts to generate datasets. Work here if you want to add
  a new variable to the dataset(s) or incorporate a new dataset to the
  database and/or .RData object. You will need access to internal
  databases for this work and have certain environment variables in your
  `.Renviron`. Contact a project manager (@eroten, @Brandon-Whited) for
  assistance.
  - `data-raw/_data-compile.R` is the main script that sources all
    numbered .R scripts in this folder.
  - `derive-var-[variable-name].R`: derive new variables from TBI
    datasets.
  - `get-[dataset-name].R`: incorporate auxiliary datasets (e.g., fuel
    efficiency data). If external to the Council, they are added to the
    database. Wherever possible, spatial datasets are loaded from the
    Council’s internal GISLibrary.
- `R`: collection R code to access and perform analyses.
  - `R\previous_code` deprecated code best used as a starting point for
    new analyses.
    - `xtab-[unit of measurement]-[variable1]-by-[variable2].R`: custom
      crosstabs.
    - `fun-[does-this].R`: functions or processes.
    - `fig-[description].R`: create a figure or plot
    - `table-[description].R`: create a table
    - `var-[description].R`: create a new variable
    - `trim-[description].R`: reduce/filter the dataset based on a
      variable.
- Note that there are no folders for specific TBI-related
  projects/analyses. These should live in their own GitHub repositories,
  and source code from this repository.

## Creating a custom crosstab

We generally use the `tidyverse`-friendly package `srvyr` to create
weighted summaries and crosstabs from the TBI data. A simple example,
calculating average vehicle age by household income, follows:

``` r
library(dplyr)
library(purrr)
library(bit64) # for looking at big integers, like the person_ids
library(srvyr)

# load dataset
load(url(paste0(
  "https://github.com/Metropolitan-Council/metc.tbi.helper/raw/main/data/",
  "tbi.rda"
)))

# create 2021 only dataset
tbi21 <- purrr::map(tbi,
                    filter,
                    survey_year == 2021)

tbi21$vehicle %>%
  select(hh_id, veh_age) %>% 
  left_join(tbi21$hh) %>%
  as_survey_design(weights = "hh_weight") %>%
  group_by(income_broad) %>%
  summarize(veh_age_avg = survey_mean(veh_age, na.rm = T))
```
