# AALMR: All-Ages Lead Model in R

This is an R implementation of the All-Ages Lead Model (AALM), ported from the 2023 Fortran version at https://github.com/USEPA/AALM/tree/main/AALMv3-0_PublicRelease. The AALM is a physiologically-based pharmacokinetic model that simulates the dynamics of lead in the human body across the lifespan.

## Installation

### Option 1: Using devtools (requires system dependencies)

For MacOS users, you need to install system dependencies first:
```bash
brew install libgit2
```

You need to install the package from the local directory:

```r
# install.packages("devtools")
devtools::install_local("path/to/aalmtools")
```

### Option 2: Using remotes (no system dependencies required)

If you encounter issues with system dependencies, you can use the `remotes` package to install the package directly from GitHub:

```r
# install.packages("remotes")
remotes::install_github("mariacardelino/aalmtools")
```

## Usage

Basic usage:
```r
library(aalmtools)

# Run simulation
results <- run_AALM(
  parFile = "path/to/input.txt",
  outputDir = "results",
  verbose = TRUE
)

# Access results
blood_pb <- results$TS[results$const$Blood_conc,]
plot(blood_pb, type='l', xlab='Days', ylab='Blood Pb (μg/dL)')
```

## Model Structure
- 31 compartments representing organs and tissues
- Age-dependent physiological parameters
- Multiple exposure pathways (inhalation, ingestion)
- Detailed mass balance tracking
- Source-specific biokinetics

## Documentation
For detailed documentation:
```r
?run_AALM
browseVignettes("aalmtools")
```

## Contact
For bugs and feature requests, please use the [issue tracker](https://github.com/mariacardelino/aalmtools/issues).

## Contributing

If you would like to contribute to the R-based AALMR, please follow these steps:

1. Fork the repository on GitHub.
2. Create a new branch with a descriptive name.
3. Make your changes and commit them with clear and concise messages.
4. Push your changes to your forked repository.
5. Create a pull request to the main repository.

## License

This project is not yet licensed.