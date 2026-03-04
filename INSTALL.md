# 🔧 Tool Setup Instructions

To reproduce the analysis in scripts/ or the Shiny dashboard application in app.R, you can follow the instructions below.

## Requirements for R environment

### Option A: RStudio

- Install R from [CRAN](https://cran.r-project.org/).
- Install the `renv` package for R package management by running `install.packages("renv")` in your R console.
- Install RStudio from [Posit](https://posit.co/download/rstudio-desktop/) if you want to run the code locally in your machine. WE recommend RStudio, but you can also run the code in any other IDE that supports `R` and `renv`.

### Option B: Terminal

- Install R from [CRAN](https://cran.r-project.org/) in one of the [terminal options recommended bellow](#options-for-terminal-installation). For Linux, you can use the following commands:

```bash
sudo apt update
sudo apt install -y r-base
```
- Install system dependencies for R packages used in this project. For Linux, you can use the following command:

```bash
sudo apt install -y \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    build-essential
```

- Install the `renv` package for R package management by running `install.packages("renv")` in your R console.

## Requirements for version control and project contribution

To contribute to this project, you need to set up your local environment. You can either use:

- **GitHub Desktop** (recommended for beginners). Check [this guide](https://docs.github.com/en/desktop/overview/getting-started-with-github-desktop) for installation; or

- One of the [terminal options recommended bellow](#options-for-terminal-installation)

## Copy the repository to your local machine

You can copy the repository to your local machine by either:

- using **GitHub Desktop** directly through the [WWM-R-data-workflow Github](https://github.com/WWM-housinggame-data-analysis/WWM-R-data-workflow).

- or via one of the [terminal options recommended bellow](#options-for-terminal-installation):

```bash
git clone https://github.com/WWM-housinggame-data-analysis/WWM-R-data-workflow.git`
```

> **Note**: For reproducing the code it is possible to download the repository as a ZIP file and extract it in your local machine. However, we recommend using GitHub Desktop or terminal for easier access to contribute to the codebase and for version control.

## Options for terminal installation

### 🪟 Windows Subsystem for Linux (WSL)

WSL provides a full Linux environment. For installation check [the official Microsoft guidelines](https://learn.microsoft.com/en-us/windows/wsl/install) or [the Carpentries course setup instructions](https://carpentries.github.io/workshop-template/#shell-install-wsl).

---

### 🐧 Git Bash (Windows)

Simpler alternative to WSL for users comfortable with Bash. For installation check [the Carpentries course setup instructions](https://carpentries.github.io/workshop-template/#shell-install-gitbash)

---

### 🍏 macOS Terminal

macOS typically includes Git by default.

#### Verify Git:

``` bash
git --version
```

If prompted to install developer tools, accept.

> **Additional help:** If you need or prefer to communicate with the remote Github repo using **SSH connection**: https://coderefinery.github.io/installation/ssh/