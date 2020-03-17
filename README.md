<!-- badges: start -->

[![travis build status](https://travis-ci.com/PascalCrepey/HospiCov.svg?branch=master)](https://travis-ci.com/PascalCrepey/HospiCov) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/PascalCrepey/HospiCov?branch=master&svg=true)](https://ci.appveyor.com/project/PascalCrepey/HospiCov) [![codecov](https://codecov.io/gh/PascalCrepey/HospiCov/branch/master/graphs/badge.svg)](https://codecov.io/gh/PascalCrepey/HospiCov)

<!-- badges: end -->

# COVID-19 epidemic impact on healthcare resources

This R package contains functions to help interested researchers to estimate hospital resources required to treat patients infected by SARS-CoV-2. 

The aim of the project is to provide a common framework to build and analyze hospital networks.

This project is partly supported by the SPHINx project, funded by French National Research Agency.

### Step 0: Installing the package:
- To install packages from GitHub, the package “devtools” needs to be installed first
```R
install.packages("devtools")
library(“devtools”)
```
- Then install the package from GitHub. Update or install all the required packages.
```R
install_github("PascalCrepey/HospiCov@*release")
```
This command will install the latest "released" version of the package.

