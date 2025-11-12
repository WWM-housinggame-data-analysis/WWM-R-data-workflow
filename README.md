# WWM R Data Workflow

The purpose of WWM R Data Workflow is to process, analyze and plot WhereWeMove data

## Project Organization

Below you can find the folder structure of this repository

    ├── CHANGELOG.md                <- List of changes made to the project.
    ├── LICENSE                     <- The license under which the project is released.
    ├── WAIVER                      <- For authors to claim back ownership of this software.
    ├── CITATION.cff                <- Citation file for the project.
    ├── CONTRIBUTING.md             <- Guidelines on how to contribute to this project 
    ├── README.md                   <- Top-level README for developers using this project.
    ├──.gitignore                   <- Specifies files and directories to be ignored by Git.
    ├── data/                       <- Stores raw and processed datasets.
    ├── man/                        <- Contains documentation for package functions, ensuring proper descriptions and metadata.
    ├── scripts/                    <- Contains R scripts for functions and analyses.
    ├── tests/                      <- Tests for scripts in `scripts/`
    ├── vignettes/                  <- Contains long-form documentation and examples using R Markdown to guide users in utilizing the
    |                                  project effectively.
    |
    ├── WWM-R-data-workflow.Rproj   <- R project file for easy access in RStudio created upon R project creation in RStudio.
    ├── .Rprofile                   <- To activate renv when opening the project in RStudio.
    ├── renv.lock                   <- Lock file for renv package management.
    └── renv/                       <- renv library for package management.


Here we followed guidelines for structuring software projects from the following resources:
- [SS NES R Project Template](https://ss-nes.github.io/meta-template/r-case.html)
- [FAIR Code Template for TU Delft](https://ss-nes.github.io/meta-template/python-case.html#fair-code-template-for-tu-delft)
- [langtonhugh's reprod_r repository](https://github.com/langtonhugh/reprod_r)

.Rprofile, renv.lock, and renv/ are created when creating renv package management for the R project.

## Installation

[Describe the steps that users (not developers) should follow to install the code in each of the intended platforms (e.g. Window 10, MacOS, etc.).]

**Requirements** 
- [List the software, OS, and/or technologies on which the code depends, and add hyperlinks to the sources whenever possible.]
- [State any relevant hardware requirements.]


### Community
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

[Name anyone who deserves it.]