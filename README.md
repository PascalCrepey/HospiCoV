<!-- badges: start -->

[![travis build status](https://travis-ci.com/PascalCrepey/HospiCov.svg?branch=master)](https://travis-ci.com/PascalCrepey/HospiCov) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/PascalCrepey/HospiCov?branch=master&svg=true)](https://ci.appveyor.com/project/PascalCrepey/HospiCov) [![codecov](https://codecov.io/gh/PascalCrepey/HospiCov/branch/master/graphs/badge.svg)](https://codecov.io/gh/PascalCrepey/HospiCov)

<!-- badges: end -->

# COVID-19 epidemic impact on healthcare resources

This R package contains functions to help interested researchers to estimate hospital resources required to treat patients infected by SARS-CoV-2. 

This project is partly supported by the SPHINx project, funded by French National Research Agency.

### Step 0: Installing the package:

#### For MacOSX and linux users
- To install packages from GitHub, the package “devtools” needs to be installed first. Then install the package from GitHub. Update or install all the required packages.

```R
install.packages("devtools")
devtools::install_github("PascalCrepey/HospiCoV@*release")
```

This will install the latest "released" version of the package.

#### For Windows users

Download [windows binary package](https://github.com/PascalCrepey/HospiCoV/releases/download/v0.2.1/hospicov_windows_binary_0.2.1.zip). 
Ensure that the zip package is in your working directory, then: 

```R
install.packages("hospicov_0.2.1.zip", repos = NULL, type = "win.binary")

``` 


### Step 1: Preparing the data

To use run the model for a specific country, you must provide the following inputs:

- **Pre-infected table**: a table with a number of infected cases by location (it can be a unique location)
- **Population table**: a table of the age-stratified population for each location. The population must be stratified into the same 17 age groups as the contact matrix.

Age-specific **contact matrices** are made available for 152 countries by Prem et al.

The tables must follow a specific format for the model to run:

- **Pre-infected table** must have three columns (case sensitive):
  1. "Region": a name of region, or location
  2. "Date": must be identical for all rows, and in ISO-8601 format: "YYYY-MM-DD" (e.g. 2020-03-10)
  3. "preInfected": an integer, the number of infected cases.  


|Region                  |Date       | preInfected|
|:-----------------------|:----------|-----------:|
|Auvergne-Rhone-Alpes    |2020-03-10 |         211|
|Bourgogne-Franche-Comte |2020-03-10 |         146|
|Bretagne                |2020-03-10 |          81|

- **Population table** must have 18 columns:
  1. one column "Region", which matches the column "Region" of the Pre-infected table
  2. 17 columns corresponding to the 17 age groups below, with the corresponding population for each region

Age groups: 0-4, 5-9, 10-14, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44,  45-49, 50-54, 55-59, 60-64, 65-69, 70-74, 75-79, 80P

|Region                  |       0-4|       5-9|     10-14|     15-19|     20-24|     25-29|     30-34|     35-39|     40-44|    45-49|     50-54|     55-59|     60-64|     65-69|     70-74|     75-79|       80P|
|:-----------------------|---------:|---------:|---------:|---------:|---------:|---------:|---------:|---------:|---------:|--------:|---------:|---------:|---------:|---------:|---------:|---------:|---------:|
|Auvergne-Rhone-Alpes    | 457173.55| 479982.02| 473564.70| 446192.66| 451363.51| 479052.66| 480439.02| 514600.38| 527126.18| 514953.9| 480081.26| 484827.77| 459145.57| 438335.49| 316475.54| 255891.89| 463162.94|
|Bourgogne-Franche-Comte | 170161.14| 193332.63| 186968.52| 157957.75| 165824.19| 178446.31| 180879.02| 200356.16| 212177.87| 213846.9| 210669.37| 188609.66| 210515.08| 201724.68| 139177.96| 117005.82| 215064.96|
|Bretagne                | 182318.23| 211639.94| 208746.84| 177964.79| 172585.84| 188743.31| 198296.36| 221251.36| 226498.73| 225150.0| 222865.06| 204764.17| 222899.55| 217450.95| 145081.51| 126929.06| 227832.33|

### Step 2: run shiny app

```R
library(hospicov)
shiny_app()
```

## Publication

Massonnaud C, Roux J, Crépey P. COVID-19: Forecasting short term hospital needs in France.
2020. <a href="https://www.ea-reperes.com/wp-content/uploads/2020/03/PredictedFrenchHospitNeeds-EHESP-20200316.pdf." target="_blank">PredictedFrenchHospitNeeds-EHESP-20200316.pdf.</a>
