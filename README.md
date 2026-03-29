# WWM R Data Workflow

This repository hosts the R codebase to preprocess, analyze and visualize data from the WhereWeMove game sessions, facilitated using the WhereWeMove manual.
This can be done statically with RStudio or dynamically using a Shiny dashboard application.

## Project Organization

Below you can find the folder structure of this repository

    ├── CHANGELOG.md                <- List of changes made to the project.
    ├── LICENSE                     <- The license under which the project is released.
    ├── WAIVER                      <- For authors to claim back ownership of this software.
    ├── CITATION.cff                <- Citation file for the project.
    ├── README.md                   <- Top-level README for developers using this project.
    ├── CONTRIBUTING.md             <- Guidelines on how to contribute to this project
    ├── INSTALL.md                  <- Instructions for installing and using the project.
    ├── app.R                       <- Main script to run the Shiny dashboard application.
    ├── WWM-R-data-workflow.Rproj   <- R project file for easy access in RStudio created upon R project creation in RStudio.
    ├── .Rprofile                   <- To activate renv when opening the project in RStudio.
    ├── renv.lock                   <- Lock file for renv package management.
    ├── .gitignore                  <- Specifies files and directories to be ignored by Git.
    ├── config.yml                  <- Configuration file for the project, containing settings and parameters used across the codebase.
    |
    ├── data/                       <- Stores raw and processed datasets.
    |   ├── raw-dbtables/           <- Contains tables containing data from each game session downloaded from database. Also in excel format for visual inspection.
    |   ├── preprocessed-dbtables/      <- Contains preprocessed database tables fit for shiny dashboard. Also in Excel format for visual inspection.
    |   └── dependencies/           <- Contains external data dependencies required for the project.
    |
    ├── scripts/                    <- Contains R scripts for data analysis and visualization.
    ├── man/                        <- Contains documentation for package functions, ensuring proper descriptions and metadata.
    ├── R/                          <- R scripts for package functions  used in the project, e.g. analysis, dashboard.
    ├── tests/                      <- Tests for scripts in `scripts/`
    ├── renv/                       <- renv library for package management.
    ├── vignettes/                  <- Contains long-form documentation and examples using R Markdown to guide users in utilizing the
    |                                  project effectively.
    └── .github/                    <- GitHub-specific files, including workflows for continuous integration and issue templates.


Here we followed guidelines for structuring software projects from the following resources:
- [SS NES R Project Template](https://ss-nes.github.io/meta-template/r-case.html)
- [FAIR Code Template for TU Delft](https://ss-nes.github.io/meta-template/python-case.html#fair-code-template-for-tu-delft)
- [langtonhugh's reprod_r repository](https://github.com/langtonhugh/reprod_r)

.Rprofile, renv.lock, and renv/ are created when creating renv package management for the R project.

## Installation and Usage

For the system requirements to use the codebase in this repository, follow the instructions in [INSTALL.md](INSTALL.md) to get started.

### RStudio

To use the codebase in this repository in your own machine, we recommend using RStudio. After following the respective installation instructions in [INSTALL.md](./INSTALL.md#option-a-rstudio) and [copying the repository](./INSTALL.md#copy-the-repository-to-your-local-machine), Follow the instructions below:

- Open the project file `WWM-R-data-workflow.Rproj` to automatically activate the `renv` environment.

> **Note**: If you are either using R in another IDE or via the terminal, make sure to activate the `renv` environment in R by running `renv::activate()`.

- Install the required R packages by running `renv::restore()` in your R console. This will install all the packages specified in the `renv.lock` file, ensuring that you have the correct versions for reproducibility.

> **Note**: If you encounter any issues with the R version, please ensure that you are using a compatible version of R and that all dependencies are correctly installed.

- To run the Shiny dashboard application, simply run `app.R` in RStudio.

### Terminal

If you are using the terminal, e.g. in a server environment, make sure you have [R and the respective system libraries installed](./INSTALL.md#option-b-terminal), [the repository copied](./INSTALL.md#copy-the-repository-to-your-local-machine) to "path/to/WWM-R-data-workflow" and follow the instructions below:

- Move to the project directory in the terminal and run `R`.

```bash
cd "path/to/WWM-R-data-workflow"
R
```

> **Note**: If R does not recognize the project environment, make sure to activate the `renv` environment in R by running `renv::activate()`.

- Install the required R packages by running `renv::restore()` in your R console. This will install all the packages specified in the `renv.lock` file, ensuring that you have the correct versions for reproducibility.

> **Note**: If you encounter any issues with the R version, please ensure that you are using a compatible version of R and that all dependencies are correctly installed.

- Run the app by executing `Rscript -e "shiny::runApp('.', host='0.0.0.0', port=3838)"` and check the display in http://localhost:3838.

## Community
If you want to contribute with the development of WWM R Data Workflow, read our [contributing guidelines](CONTRIBUTING.md).

## License
This software is distributed under a [Apache Software License](LICENSE). 

## Copyright

Juliette Cortes Arevalo, The Netherlands. 
Alexander Verbraecken, The Netherlands.

[CITATION](./CITATION.cff)
[LICENSE](./LICENSE)
[WAIVER](./WAIVER.md)

## Citation

[Include a [CCF file (Citaion File Format)](https://citation-file-format.github.io/). See example in this repo: `CITATION.cff`. You can easily create the content of a CCF using [this tool](https://citation-file-format.github.io/cff-initializer-javascript/). ]

[CCF files can be converted to a multitude of formats, including BibTex,  EndNote, codemeta, plain JSON, schema.org, RIS, and Zenodo JSON ]

## Acknowlegdements

This repository was supported by [João Guimarães](https://www.tudelft.nl/digital-competence-centre/team/joao-guimaraes) from the Digital Competence Centre, Delft University of Technology. 
