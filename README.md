![PackageDev](https://img.shields.io/badge/Package%20Development-%20R%20|%20Pipeline%20|%20Unit%20Tests%20-blue)
# aiinsurance  <img src="inst/figures/logo.png" align="right" style="width: 25%;"/>
Creator: Hamed Vaheb

Icon Source: [aiinsurance.io](https://aiinsurance.io/)

# Introduction

The `aiinsurance` package is dedicated to a project for the workshop of master of data science at university of Luxembourg.
In this package, functions are defined for various stages of classification of the `outcome` column of the [Car Insurance Data](https://www.kaggle.com/datasets/sagnik1511/car-insurance-data).
The raw dataset along with the processed train and test datasets, obtained by using the `aiinsurance` functions (prepared to be fed in models) are incorporated in the package.
The package's documentation, including documentation of functions and datasets, as well as unit tests for the functions are elaborated on [Documentation](#Documentation).
Using package's functions, a machine learning classification project was done on the Car Insurance Dataset. The link to project's implementation, and a detailed report is provided on [Report](#Report).
Moreover, a pipeline including main stages of the classification (refer to [`targets` Pipeline](#targets-Pipeline)), and a shiny interactive app (refer to [`shiny` App](#shiny-App)) visualizing evaluation plots are included in the package.

**Note**: All functions from this pakcage are suffixed with `hmd` so as not to confuse with other built-in packages.

# Install

To install this package in an R edtior (e.g., Rstudio), install [`devtools`](https://www.r-project.org/nosvn/pandoc/devtools.html) library and then install my package. Please follow the following commands:

```r
install.packages("devtools")
library(devtools)
install_github("berserkhmdvhb/aiinsurance")
```



# Usage

Please run the commands of this section in a console of an R editor (e.g., Rstudio)

## `renv` packages

Either use renv when creating a project, or if you haven't, install the [`renv`](https://rstudio.github.io/renv/articles/renv.html) library, load it, and then use the [`renv.lock`](https://github.com/berserkhmdvhb/aiinsurance/blob/main/renv.lock) file (by copying it to project's directory) to install the requied packages. Please follow the following commands in the console:

```r
install.packages("renv")
library(renv)
renv::restore()
```

To view the documentation my package, how to use its functions, and to read the report for a machine learning project that I did using this package, refer to [Documentation](#Documentation).



## `targets` Pipeline

To run the pipeline, adhere to the instructions provided below:

First clone the package's repository, using the following command in a command line:

```
git clone git@github.com:berserkhmdvhb/aiinsurance.git
```

Then navigate to to the cloned folder and open `aiinsurance.Rproj` in an R editor to create a project.
Install the packages required for the `aiinsurance` package from the `renv.lock` file (refer to [`renv`](#renv-packages)), then install the `aiinsurance` package itself (refer to [Install](#Install)), install and load the `targets` library, and the run `tar_make()` to run the pipeline.

**Note** : Make sure to install the `aiinsurance` package after restoring the `renv`, as restoring will only include the packages required for the `aiinsurance`, but no the package itself, and hence if one installs renv after, the `aiinsurance` will no longer exist.

After restoring the `renv` and installing the `aiinsurance` package, run the following in the console:
```r
library(aiinsurance)
library(targets)
targets::tar_make()
```

After the pipeline is successfully run, there should be now two plots called `plot_glm` and `plot_rf` (as can bee seen in the figure in [Visualize](#Visualize)). Both of the plots display ROC curve, while the former attributes to the logistic regression (implemented by the glm), and the latter attributes to random forest classifier. The two plots are very similar, as the models had very similar performance. Two view the two plots, run the following in the console:

```r
targets::tar_read(plot_glm)
targets::tar_read(plot_rf)
```


### Directory Tree 

```bash
├── R
│   ├── functions.R
├── run.R
├── run.sh
├── _targets.R
```

### Visualize 

To visalize the components of the pipeline, run the following:

```
targets::tar_visnetwork()
```

The following figure should be displayed:

![`tar_visnetwork`](https://github.com/berserkhmdvhb/aiinsurance/blob/main/inst/figures/tar_visnetwork.png)


### Pipeline Steps

Evidenced by the visualization, the two datasets used in the pipeline are `insurance_train` and `insurance_test`.
They are datasets processed from the raw `car_insurance_data`, and all the three mentioned datasets are incorporated in the package.
The steps of the pipeline are elaborated on in the following:

- Logistic Regression Part
    1. Access the `insurance_train` with `get_data_train()`, and insurance_test with `get_data_test()`.
    2. Store the `outcome` column (labels) from `insurance_test` for later evaluation in steps vi (and iii from Random Forest Part)
    3. Fit the `insurance_train` into the `glm_fit_hmd` function (from the package) so as to apply the logistic regression model on data ,and thn store the fitted object in `model_glm`
    4. Predict the `insurance_test` using the fitted object `model_glm` from step iii, by feeding both `insurance_test` and `model_glm` to the `glm_predict_hmd`, and store the prediction results in `predictions_glm`.
    5. Extract prediction probabilities (required for ROC curve) from `predictions_glm` and store them in `pred_proba_glm`
    6. Compute ROC metrics be feeding `actual` data (from step ii) and prediction probabilities `pred_proba_glm` to the `roc_obj_cal` function, store the result in `roc_obj_glm`
    7. Plot the roc curve by feeding `roc_obj_glm` to the `plot_roc_curve` function, store the plot in `plot_glm`
- Random Forest Part
    1. Fit the `insurance_train` into the `rf_fit_hmd` function (from the package) so as to apply the random forest model on data ,and thn store the fitted object in `model_rf`
    2. Predict the `insurance_test` using the fitted object `model_random_forest` from step iii, by feeding both `insurance_test` and `model_rf` to the `rf_predict_hmd`, and store the prediction results in `predictions_rf`.
    3. Extract prediction probabilities (required for ROC curve) from `predictions_rf` and store them in `pred_proba_rf`
    4. Compute ROC metrics be feeding `actual` data (from step ii) and prediction probabilities `pred_proba_rf` to the `roc_obj_cal` function, store the result in `roc_obj_rf`
    5. Plot the roc curve by feeding `roc_obj_rf` to the `plot_roc_curve` function, store the plot in `plot_rf`
    
    
## `shiny` App

Unlike the `targets` pipeline, the `shiny` is part of the packages' functions. 
To view the shiny app, adhere to the instructions provided below:
Simply install the `aiinsurance` package (refer to [Install](#Install)) and execute the following commands to display the app.
```r
library(aiinsurance)
shiny_run_hmd()
```

Although the shiny app could be based on `targets`, since I wanted the shiny app to work just by installing the package (and without the need to clone anything), I separated the `shiny` app and `targerts` pipeline.

### Directory Tree

```bash
├── inst
│   ├── plot_app
│   │   ├── app-cache
│   │   ├── global.R
│   │   ├── server.R
│   │   └── ui.R
├── R
│   ├── shiny_run_hmd.R
```



# Documentation

The documentation of the package can be accessed with the following commands.

```r
help(package = aiinsurance)
```

## `testthat` Unit Tests

All functions include type checking of inputs.
Some functions were supplied with unit tests. For purpose of illustration, I will describe the tests for the [`eval_hmd`](https://github.com/berserkhmdvhb/aiinsurance/blob/main/R/eval_hmd.R) function, which accepts `actual` and `predicted` objects, and then computes evaluation metrics suitable for a binary classified prediction. The function returns a hash containing various evaluation metrics, as well as a confusion matrix plot.
Since the inputs `actual` and `predicted` should have certain conditions, the following type checkings and other tests were embedded inside the `eval_hmd` function:

1. The actual input and predict input can be matrices, and if not, they should be of class either `numeric` or `factor`.
If they are of class `factor`, they will be converted to `numeric` class, as this makes later tests and computations more convenient.
2. The actual input and predict input should be binary, therefore if any of them contain more than 2 values, error will raise.
3. The predict input should contain any class that is not present in actual input. But since the user might not always put the arguments' names (actual and predicted), I will accept two cases, either the predict input is subset of actual, or actual is subset of predicted.

Moreover, using the [`testthat`](https://testthat.r-lib.org/) library the following unit tests have been added in the [`test-eval_hmd.R`](https://github.com/berserkhmdvhb/aiinsurance/blob/main/tests/testthat/test-eval_hmd.R) file (visible also in tree structure below):

1. `test_that("check value ranges",...` ensures all outputs' evaluation metrics, i.e., accuracy, precision, recall, fbeta_score, and f1_score have values in the range [0,1]
2. `test_that("check output class",...` ensures the output of the functions is of class hash.



### Directory Tree

```bash
└── tests
    ├── testthat
    │   ├── test-categoricals_hmd.R
    │   ├── test-eval_hmd.R
    │   ├── test-glm_fit_hmd.R
    │   └── test-normalizer_hmd.R
    └── testthat.R

```

# Report

1. The Rmarkdown for implementing the insurance claims' classification of the outcomes is provided in the following links:

[Link of Rmardkown file](https://github.com/berserkhmdvhb/aiinsurance/blob/main/inst/implement.Rmd)

[Link of HTML rendering](https://htmlpreview.github.io/?https://github.com/berserkhmdvhb/aiinsurance/blob/main/inst/implement.html)

2. The written report explaining both theory and implementation can is provided in the following links:

[Report PDF](https://github.com/berserkhmdvhb/aiinsurance/blob/main/inst/report/report.pdf)


### `rticles`
The [rticles](https://github.com/rstudio/rticles) library was used to produce the report. The [arXiv](https://arxiv.org/) pre-prints based on George Kour’s template is used as the template. Each time the Rmarkdown file is knitted, a .tex file and then .pdf file is generated. Since the .tex file is regenerated atuomatically, it is not possible to edit it. To edit the .tex file, first find the location of installed libraries with the following command in R console


```r
.libPaths()
```

Navigate to this directory in command line and insert the following commands:

```bash
cd ./rticles/rmarkdown/templates/arxiv/resources
nano template.tex
```
Now the edits in .tex file will be permanent.


