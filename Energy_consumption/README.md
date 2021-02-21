# Energy Consumption
The goal of the project was to show which useful information a family can get if they use submetering devices for monitoring the consumption of energy. We studied the data about the energy consumption of a family during four years. In particular, for every minute we knew the total energy consumption and the consumption in the three submetering devices of the house. After we had cleaned the data and we had taken care of the missing values, we did some intresting graphics about the consumption of energy using different time aggregation. Finally, we forecasted the monthly energy consumption applying different models on time series. <br>

<b> Problem:</b> Descriptive analysis and time series forecasting. <br>
<b> Data source:</b> https://archive.ics.uci.edu/ml/datasets/individual+household+electric+power+consumption <br>
<b> Link to visualize the notebook: </b> https://elenabonan.github.io/Ubiqum_projects/Energy.html

## Content of the repository
- <b> Analysis.Rmd </b> This is the code of the R notebook where you can find both the descriptive Analysis and the Forecast of the Energy consumption. For visualize the notebook, look at the following link https://elenabonan.github.io/Ubiqum_projects/Energy.html.
- <b> Analysis.nb.html </b> This is the html code given by the R Markdown, using as output html_notebook.
- <b> Presentation.pptx </b> This is a presentation I gave during the course showing the most important findings. 
- <b> household_power_consumption.7z </b> The dataset (compressed).

## Structure of the Code 
The code is divided in three parts:
- <b> Data Cleaning </b> We have taken care of the missing values and we have transformed the data in order to have the correct unit of measure.
- <b> Descriptive Analysis </b> We have explored the data and we have plot interesting graphs in order to show which useful information a customer can get from the submetering devices.
- <b> Forecasting </b> We have forecast the consumption of energy using the exponential smooth model and the ARIMA model.
