# Travel Behavior Inventory Survey Helper
This repository contains a collection of R code for working with Travel Behavior Inventory Household Survey data. 

_If you came for a pretty R Data object of the 2019 or 2021 TBI survey data, check out the_ `data` _folder or source from URL:_

```r
load(url(paste0("https://github.com/Metropolitan-Council/metc.tbi.helper/raw/main/data/",
                "tbi21.rda")))
                
load(url(paste0("https://github.com/Metropolitan-Council/metc.tbi.helper/raw/main/data/",
                "tbi19.rda")))
```

Other things you might find useful in here include R scripts to create complex cross-tabs that require funky joins, some scripts for appending auxiliary datasets & geographic information to the datasets, and more. 

This repository is in active development, and anyone can contribute 🤝. See the CONTRIBUTING page on the right to learn more. One day, this will be an R package.

## Organization: what's here

* `metadata`: Read the documentation 💘 
* `data`: compiled datasets generated from raw TBI data, that live in the Council's database.
  * `tbi19.rda` and `tbi21.rda` are compressed `list` objects for each survey containing:
      * `dictionary` of variable names, values, survey questions, and logic;
      * person-level records (`per`);
      * `day` records, for analyzing daily trends;
      * `trip` table;
      * household (`hh`) records;
      * vehicle (`veh`) records, including fuel efficiency data; and
      * `trip_purpose`, for working with trip purpose data. This table has been specially weighted to attribute weights to either end of non-home-based trips, and to the non-home based end of home-based trips.

**This is the only data we store in this GitHub repository.**  Git (even [Git LFS](https://git-lfs.github.com/)) is not ideal for storing data. If you generate additional datasets in your work, please add them to the .gitignore file. If you need to work with .csv data, please see the script `data-raw/99-survey-data-to-csv.R`, and add the .csv's to your `gitignore` file. If incorporating a new dataset, write it to the database first (see @ashleyasmus for write access).

* `data-raw`: scripts to generate datasets. Work here if you want to add a new variable to the dataset(s) or incorporate a new dataset to the database and/or .RData object. You may need access to internal databases for this work.
  * `data-raw/_data-compile.R` is the main script that sources all numbered .R scripts in this folder. 
  * `derive-var-[variable-name].R`: derive new variables from TBI datasets. 
  * `get-[dataset-name].R`: incorporate auxillary datasets (e.g., fuel efficiency data). If external to the Council, they are added to the database. Wherever possible, spatial datasets are loaded from the Council's internal GISLibrary.
* `R`: collection R code. At the moment, this is mostly crosstabs.
  * `xtab-[unit of measurement]-[variable1]-by-[variable2].R`: custom crosstabs.
  * `fun-[does-this].R`: functions or processes.
* Note that there are no folders for specific TBI-related projects/analyses. These should live in their own GitHub repositories, and source code from this repository.

## Using this code in other projects
One day, this repository might be an R package. For now, you can source data, metadata and R scripts in two ways.

(1) Clone this repository to your machine _(Preferred method)_
  This method is preferred, though it will require you to do periodic pulls from the `main` branch to your machine.

```r
source(
  paste0(
    "directory/where/your/repository/lives/",
    "R/xtab-vehicle-stops.R" 
  )
)
```

(2) Source files directly from GitHub.

Files that have "raw" output available directly on GitHub ([example](https://github.com/Metropolitan-Council/metc.tbi.helper/blob/main/R/xtab-vmt-per-vehicle.R)) can be sourced directly with base R:

```r
source(
  paste0(
    "https://raw.githubusercontent.com/Metropolitan-Council/metc.tbi.helper/main/", 
    "R/xtab-vehicle-stops.R"
  )
)
```

## Creating a custom crosstab

We generally use the `tidyverse`-friendly package `srvyr` to create weighted summaries and crosstabs from the TBI data. A simple example, calculating average vehicle age by household income, follows:

```r
library(dplyr)
library(bit64) # for looking at big integers, like the person_ids

tbi_tables$veh %>%
  left_join(tbi_tables$hh) %>%
  as_survey_design(weights = "hh_weight", strata = "sample_segment") %>%
  group_by(income_detailed) %>%
  summarize(veh_age_avg = survey_mean(veh_age, na.rm = T))

```
