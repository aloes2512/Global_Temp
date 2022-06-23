Objective of the study:
To predict Global Temperature Change models are needed.
Mostly linear regression models are used because of their simplicity and interpretability. But such models are only usefull for short and medium range predictions. In addition they can not be used to efficiently predict  processes which are limitted by natural barriers as e.g. plant growth, percentage of people infected by a virus as covic 19, economic development, population growth....
A simple model for the prediction of world population growth was already published  by the Belgian Mathematician Verhulst in 1838 and 1847 see: https://en.wikipedia.org/wiki/Pierre_François_Verhulst.  He called the solution of the non linear differential equation logistic equation.
Linear and nonlinear models have been used by Wood: "https://fromthebottomoftheheap.net/2011/06/12/additive-modelling-and-the-hadcrut3v-global-mean-temperature-series/" to analyse a time series of instrumental measurements by the United Kingdom Metheorological Service. This time series is probably the longest instrumental record of temperature data. It is used for this study.
CO2-data timeseries are from NOAA ESRL and complemented with data from German UBA for 2 stations Schauinsland & Zugspitze. A total of 20 timeseries collected, formated and cleaned. Data are stored in a list:
                  CO2_data.lst
the data are saved: saveRDS(CO2_data.lst,file="~/projects/Global_Temp/data/NOAA_data.rds")

The linear trends of all stations is calculated: CO2_lin_trend.R and visualised

The residuals do show regular patterns. The pattern is fitted with gaussian basis functions.  A minimum number is looked for manually. (AIC does not seem suitable for selecting )