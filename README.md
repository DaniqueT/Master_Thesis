# The Impact of Multi-day Ahead Electricity Price Forecasts on Electric Vehicle Charging Optimisation in the Dutch Market

All code used in the thesis can be found here. There are 2 main files: data.ipynb and forecasting.ipynb. The former is used to concatenate all data available into one dataframe and for some data analysis. In the latter all forecasting methods can be found, as well as the deterministic and stochastic optimisation. Above each code block, the header describes what the method does. Something tuning is included in the general method block and sometimes it is seperate. 

There are also two R files in this directory. These are used to create the plots for the thesis. File General_data_plots.R is used to create the outlier, seasonal, mixture vs Johnson-SU and general variable plot. Plots_charging.R is used to creat the availability and charging plot for the week in August and the specific plot for August 24th. 

Lastly, the collected 7-day ahead forecasts for solar, wind on shore and wind of shore are included in this directory together with their actual values. 


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

## ☀Forecast Data
This repository also includes the collected 7-day ahead forecasts (and corresponding actuals) for:
- Solar generation  
- Wind onshore  
- Wind offshore
