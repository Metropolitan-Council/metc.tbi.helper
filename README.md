# Travel Behavior Inventory Survey Helper
This repository contains a collection of R code for working with Travel Behavior Inventory Household Survey data. 

_If you came for a pretty R Data object of the TBI survey data, check out the_ `data` _folder!_

Other things you might find useful in here include R scripts to create complex cross-tabs that require funky joins, some scripts for appending auxiliary datasets & geographic information to the datasets, and more. 

This repository is in active development, and anyone can contribute 🤝. One day, this might (should?) be an R package.

## Contributing
This project uses a [feature-branch](https://deepsource.io/blog/git-branch-naming-conventions/) naming convention and workflow.

`main` is the main branch (not `master`), base your work off of `main`.
Contribute to the project by making changes to your own feature branch and issuing pull-requests when you're ready to integrate into the `main` branch:

* Pull the `main` branch; `git pull`, and if necessary `git checkout main` to switch to `main`
* Create a feature branch and check out your branch, e.g., `git checkout -b aa-xtab-vmt-by-race`
  * You can use your initials to prefix a feature branch, e.g.,
  `aa-xtab-vmt-by-race`.
  * Your feature branch should do one thing only, for example: 
    * create a new crosstab, 
    * create a new function,
    * create a new custom variable,  
    * integrate a new dataset, or
    * fix an issue - [please name your branch with the issue number](https://deepsource.io/blog/git-branch-naming-conventions/)
* Commit changes related to your feature and push them to GitHub.
* You can push changes to your feature branch at any time.
* So that anyone within the Met Council can build your reports, make sure you set your `keyring` keys to the MTS Oracle database to match those in the  `keyring_template.R` script. For database passwords, contact @ashleyasmus.
* When you're ready to have your work reviewed you create a pull-request on GitHub.
* You can issue a pull-request and request a review of work-in-progress if you want guidance on code or content.
* Make changes or respond to comments in your pull-request reviews.
  * New commits pushed to your branch will update the pull-request.
* When your pull request is approved the approver will merge your branch into main and may delete your branch from GitHub.
  * To remove deleted feature branches from your local repository run `git remote prune origin`.
  * Do not attempt to push additional commits to a merged pull-request.
  Instead, start a new feature branch and issue a new pull request.
* Remember to update and branch off of `main` whenever you start a new feature, e.g., `git checkout main; git pull origin main; git checkout -b a-new-feature`.


## Organization

* `metadata`: Read the documentation 💘 
* `data`: compiled datasets generated from raw TBI data, that live in the Council's Oracle database.
  * `tbi_tables.rda` is a compressed `list` object containing:
      * `dictionary` of variable names, values, survey questions, and logic;
      * person-level records (`per`);
      * `day` records, for analyzing daily trends;
      * `trip` table;
      * household (`hh`) records;
      * vehicle (`veh`) records, including fuel efficiency data; and
      * `trip_purpose`, for working with trip purpose data. This table has been specially weighted to attribute weights to either end of non-home-based trips, and to the non-home based end of home-based trips.

**This is the only data we store in this GitHub repository.**  Git (even [Git LFS](https://git-lfs.github.com/)) is not ideal for storing data. If you generate additional datasets in your work, please add them to the .gitignore file. If you need to work with .csv data, please see the script `data-raw/99-get-compiled-survey-data.R`, and add the .csv's to your `gitignore` file. If incorporating a new dataset, write it to the Oracle database (see @ashleyasmus for write access).

* `data-raw`: scripts to generate datasets. Work here if you want to add a new variable to the dataset(s) or incorporate a new dataset to the database and/or .RData object. You may need access to internal databases for this work.
  * `data-raw/_data-compile.R` is the main script that sources all numbered .R scripts in this folder. 
  * `derive-var-[variable-name].R`: derive new variables from TBI datasets. 
  * `get-[dataset-name].R`: incorporate auxillary datasets (e.g., fuel efficiency data). If external to the Council, they are added to the Oracle database. Wherever possible, spatial datasets are loaded from the Council's internal GISLibrary.
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

Data will need to be sourced with the R package `devtools`:

```r
library(devtools)
source_url(
  paste0(
    "https://github.com/Metropolitan-Council/metc.tbi.helper/raw/main/", 
    "data/tbi_tables.rda"
  )
)
```


## Creating a custom crosstab

We use the `tidyverse`-friendly package `srvyr` to create weighted summaries and crosstabs from the TBI data. A simple example, calculating average vehicle age by household income, follows:

```r
library(dplyr)
library(bit64) # for looking at big integers, like the person_ids

tbi_tables$veh %>%
  left_join(tbi_tables$hh) %>%
  as_survey_design(weights = "hh_weight") %>%
  group_by(income_detailed) %>%
  summarize(veh_age_avg = survey_mean(veh_age, na.rm = T))

```
