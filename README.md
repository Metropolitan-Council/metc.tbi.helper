
# Travel Behavior Inventory Survey Helper

This repository contains a collection of R code for working with Travel
Behavior Inventory Household Survey data.

*If you came for a pretty R Data object of the 2019 or 2021 TBI survey
data, check out the* `data` *folder or source from URL:*

``` r
load(url(paste0(
  "https://github.com/Metropolitan-Council/metc.tbi.helper/raw/main/data/",
  "tbi21.rda"
)))

load(url(paste0(
  "https://github.com/Metropolitan-Council/metc.tbi.helper/raw/main/data/",
  "tbi19.rda"
)))
```

Other things you might find useful in here include R scripts to create
complex cross-tabs that require funky joins, some scripts for appending
auxiliary datasets & geographic information to the datasets, and more.

This repository is in active development, and anyone can contribute 🤝.
See the CONTRIBUTING page on the right to learn more. One day, this will
be an R package.

## Organization: what’s here

- `metadata`: Read the documentation 💘
- `data`: compiled datasets generated from raw TBI data, that live in
  the Council’s database.
  - `tbi19.rda` and `tbi21.rda` are compressed `list` objects for each
    survey containing:
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

**This is the only data we store in this GitHub repository.** Git (even
[Git LFS](https://git-lfs.github.com/)) is not ideal for storing data.
If you generate additional datasets in your work, please add them to the
.gitignore file. If you need to work with .csv data, please see the
script [`data-raw/99-distribute-data.R`](data-raw/99-distribute-data.R),
and add the .csv’s to your `gitignore` file. If incorporating a new
dataset, write it to the database first (see @eroten for write access).

- `data-raw`: scripts to generate datasets. Work here if you want to add
  a new variable to the dataset(s) or incorporate a new dataset to the
  database and/or .RData object. You may need access to internal
  databases for this work.
  - `data-raw/_data-compile.R` is the main script that sources all
    numbered .R scripts in this folder.
  - `derive-var-[variable-name].R`: derive new variables from TBI
    datasets.
  - `get-[dataset-name].R`: incorporate auxiliary datasets (e.g., fuel
    efficiency data). If external to the Council, they are added to the
    database. Wherever possible, spatial datasets are loaded from the
    Council’s internal GISLibrary.
- `R`: collection R code to access and perform analyses.
  - `R\previous_code` At the moment, this is mostly crosstabs.
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

## Using this code in other projects

One day, this repository might be an R package. For now, you can source
data, metadata and R scripts in two ways.

1)  Clone this repository to your machine *(Preferred method)* This
    method is preferred, though it will require you to do periodic pulls
    from the `main` branch to your machine.

``` r
source(
  paste0(
    "directory/where/your/repository/lives/",
    "R/previous_code/xtab-vehicle-stops.R"
  )
)
```

2)  Source files directly from GitHub.

Files that have “raw” output available directly on GitHub
([example](https://github.com/Metropolitan-Council/metc.tbi.helper/blob/main/R/previous_code/xtab-vmt-per-vehicle.R))
can be sourced directly with base R:

``` r
source(
  paste0(
    "https://raw.githubusercontent.com/Metropolitan-Council/metc.tbi.helper/main/",
    "R/previous_code/xtab-vehicle-stops.R"
  )
)
```

## Creating a custom crosstab

We generally use the `tidyverse`-friendly package `srvyr` to create
weighted summaries and crosstabs from the TBI data. A simple example,
calculating average vehicle age by household income, follows:

``` r
library(dplyr)
library(bit64) # for looking at big integers, like the person_ids
library(srvyr)

load(url(paste0(
  "https://github.com/Metropolitan-Council/metc.tbi.helper/raw/main/data/",
  "tbi21.rda"
)))

tbi21$vehicle %>%
  left_join(tbi21$household) %>%
  as_survey_design(weights = "hh_weight", strata = "sample_segment") %>%
  group_by(income_detailed) %>%
  summarize(veh_age_avg = survey_mean(veh_age, na.rm = T))
```
