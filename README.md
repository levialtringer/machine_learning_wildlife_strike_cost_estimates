# Estimating Wildlife Strike Costs at US Airports: A Machine Learning Approach

This repository contains the data and code for "Estimating Wildlife Strike Costs at US Airports: A Machine Learning Approach".

WILL PLACE CITATION AND LINK HERE

All data cleaning and analyses are performed in R statistical software. Certain Python environment dependencies are required to execute the neural network commands from the `keras` and `tensorflow` packages (see <https://tensorflow.rstudio.com/reference/keras/install_keras/>).

The files contained in this repository are:

  1. `strike_data_raw_1.csv`
  2. `strike_data_raw_2.csv`
  3. `inf_adjust.csv`
  4. `analysis.R`
  5. `clean_nwsd.R`
  6. `functions.R`

To perform the analysis and reproduce the objects presented in the paper, work through the `analysis.R` script. At the beginning, this script sources the `clean_nwsd.R` script to join and clean the `strike_data_raw_1.csv` and `strike_data_raw_2.csv` data. Then, the `analysis.R` script uses the user-defined functions sourced from `functions.R` to perfrom the analysis. The script is annotated to aid in replication.

Questions concerning the analysis and research can be sent to <levi.altringer@colostate.edu>.
