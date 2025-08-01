# The Impact of Multi-day Ahead Electricity Price Forecasts on Electric Vehicle Charging Optimisation in the Dutch Market

All code used in my thesis can be found in this repository. Each code block is labelled with a header to explain its purpose.  

## Main Notebooks

- **`data.ipynb`**  
  Combines all available datasets into a single DataFrame and includes initial data analysis.

- **`forecasting.ipynb`**  
  Contains all forecasting methods, both point and probabilistic, as well as the deterministic and stochastic optimisation approaches. 
  > Note: Tuning is sometimes included in the main method block, and sometimes implemented in separate blocks.

## R Scripts (for plotting)

- **`General_data_plots.R`**  
  Used to create:
  - Outlier and seasonality plots  
  - Mixture vs Johnson-SU distribution comparison  
  - General plots of all input variables

- **`Plots_charging.R`**  
  Used to generate:
  - Charging and availability plot for a week in August  
  - Zoom-in plot for August 24th

## Forecast Data
This repository also includes the collected 7-day ahead forecasts (and corresponding actuals) for:
- Solar generation  
- Wind onshore  
- Wind offshore
