# AALMR: All-Ages Lead Model in R

This is an R implementation of the All-Ages Lead Model (AALM), ported from the 2023 Fortran version. The AALM is a physiologically-based pharmacokinetic model that simulates the dynamics of lead in the human body across the lifespan.

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
remotes::install_github("username/aalmtools")
```

## Usage

To run the model, you can use the following code:

```r
library(aalmtools)

# Load example data
data <- read.csv("example_data.csv")

# Run the model
results <- run_aalm(data)

# View the results
print(results)
```


## Contributing

If you would like to contribute to the development of AALMR, please follow these steps:

1. Fork the repository on GitHub.
2. Create a new branch with a descriptive name.
3. Make your changes and commit them with clear and concise messages.
4. Push your changes to your forked repository.
5. Create a pull request to the main repository.

## License

This project is licensed under the MIT License. See the LICENSE file for more details.