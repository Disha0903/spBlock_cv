# Foundation for unbiased cross-validation of spatiotemporal models for species distribution modeling

This repository contains the source code for the paper titled "Foundation for unbiased cross-validation of spatio-temporal models for species distribution modeling".
![A) A general workflow of SDM focuses on predicting habitat suitability for species in future scenarios. B) Specific methodology of our study, including data collection across different time intervals, hyperparameter optimization, and validation processes through both traditional and spatial cross-validation techniques](workflow.png)

A) A general workflow of SDM focuses on predicting habitat suitability for species in future scenarios. B) Specific methodology of our study, including data collection across different time intervals, hyperparameter optimization, and validation processes through both traditional and spatial cross-validation techniques


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

 **Another option**:
    Or you can run the `install_packages.R` script to install all the required packages:
    ```r
    source("install_packages.R")
    ```

## Data

You can find the data in the `data` folder within this repository. Below is a brief description of the contents of each subfolder:

- **shapefiles**: Contains shapefiles of the area of study, which include Norway and Sweden.
- **train_2003**: Contains climate features for training the models.
- **test_2003**: Contains climate features for testing the models.
- **soil**: Includes soil features relevant to the study.
- **spatiotemp**: Includes climate features with different time periods for spatio-temporal cross-validation. For more details, see the description in the paper and follow the instructions in `code.R`.
- **elevation**: Contains elevation data.
- **species**: Contains species occurrence or absence data.



## Usage

To use the scripts and run the models, follow these steps:

**Source the necessary function files**:

```r
source("functions_spBlock.R")         # Contains model training functions
source("functions_preprocessing.R")   # Contains data preprocessing functions
source("hyperparams.R")               # Defines hyperparameter grids (optional)

source("main.R")   # Run main workflow 


    


## Citation

If you use this code or data in your research, please cite our paper:
TBD


