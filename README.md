# Australian Wines Forecasting Shiny App

##### This Shiny application was built for ADS-506: Time Series Analysis & Forecasting to explore and forecast monthly Australian wine sales by varietal. The app allows users to visualize historical sales, adjust modeling parameters, compare forecasting models, and evaluate accuracy.

## Features

- Interactive selection of wine varietals

- Adjustable date range and training/validation split

- User-defined forecast horizon (3–60 months)

- Three forecasting models: TSLM, ETS, ARIMA

##  Historical trend plots and forecast charts with 80% and 95% intervals

- Accuracy metrics (RMSE, MAE, MAPE) for training and validation

- Model specifications table with fallback handling

- About page with dataset details and reproducibility notes

## Dataset

Uses AustralianWines.csv, containing monthly sales data (1980–1994) for:
Dry White, Red, Sparkling, Sweet White, Fortified, and Rose.

## How to Run

### Clone the repository: git clone https://github.com/kpaz357/ADS506-Australian-Wines-Forecasting-ShinyApp.git

## Run the app in RStudio:
### shiny::runApp()

###### AI ACKNOWLEDGEMENT 
###### Minimal AI assistance was used for debugging and code clarity; all modeling and implementation were completed by the author.
