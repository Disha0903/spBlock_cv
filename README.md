# Foundation for Unbiased Cross-Validation of Spatiotemporal Models for Species Distribution Modeling

This repository contains the source code for the paper titled "Foundation for Unbiased Cross-Validation of Spatiotemporal Models for Species Distribution Modeling."

## Installation

To install `spBlock_cv` and ensure all the compatible dependencies are included without affecting your system R installation, follow the steps below:

1. **Ensure R is Installed**:
    Make sure R (version 4.3.1 (2023-06-16 ucrt)) is installed on your system. You can download it from [CRAN](https://cran.r-project.org/).

2. **Clone the Repository**:
    Clone this repository to your local machine:
    ```sh
    git clone https://github.com/Disha0903/spBlock_cv.git
    cd spBlock_cv
    ```

3. **Set Up `renv`**:
    Initialize `renv` to install the necessary packages with the correct versions:
    ```r
    install.packages("renv")
    library(renv)
    renv::restore()
    ```

4. **Install Necessary Packages**:
    Run the `install_packages.R` script to install all the required packages:
    ```r
    source("install_packages.R")
    ```

## Data

You can find the data in the `data` folder within this repository. Make sure to follow any specific instructions related to the data as mentioned in the paper.

## Usage

To use the scripts and run the models, follow these steps:

1. **Source the Scripts**:
    Ensure you have sourced the necessary scripts in your R session:
    ```r
    source("script1.R")  # Contains all the functions
    source("script2.R")  # Uses the functions defined in script1.R
    ```

2. **Run the Analysis**:
    Follow the instructions in the paper or any additional documentation provided in this repository to run the analysis.

## Hyperparameter Tuning

We utilized GridSearch for hyperparameter tuning. The `table_hyperparams.csv` file lists the hyperparameters for each model. From the set of possible hyperparameter combinations, we randomly selected 120 combinations for each model and used them in the GridSearch.

## Citation

If you use this code or data in your research, please cite our paper:


