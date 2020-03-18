To use run the model for a specific country, you must provide the following inputs:

- **Pre-infected table**: a table with a number of infected cases by location (it can be a unique location)
- **Population table**: a table of the age-stratified population for each location. The population must be stratified into the same 17 age groups as the contact matrix.

Age-specific **contact matrices** are made available for 152 countries by <a href="https://doi.org/10.1371/journal.pcbi.1005697" target="_blank">Prem et al.</a>

The tables must follow a specific format for the model to run:

- **Pre-infected table** must have three columns (case sensitive):
  1. "Region": a name of region, or location
  2. "Date": must be identical for all rows, and in ISO-8601 format: "YYYY-MM-DD" (e.g. 2020-03-10)
  3. "preInfected": an integer, the number of infected cases.  


- **Population table** must have 18 columns:
  1. one column "Region", which matches the column "Region" of the Pre-infected table
  2. 17 columns corresponding to the 17 age groups below, with the corresponding population for each region

Age groups: 0-4, 5-9, 10-14, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44,  45-49, 50-54, 55-59, 60-64, 65-69, 70-74, 75-79, 80P
