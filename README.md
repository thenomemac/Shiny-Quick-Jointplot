# Shiny Quick Plotting App

This repo provides the code for an R function that accepts  
a data.frame or data.table as input and launches a shiny app where the user 
can select an x and y variable from a list of columns in the data.frame. The purpose 
of the app is quick data exploration. In particular it was created to allow for
fast visualization of a new data set from kaggle.com The goal is to quickly
visuallize the x, y relationships in the data set when project time is at 
a minimum.

The plots produced on the shiny app by the function are:

* Scatter with marginal histograms (jointplot)
* x histogram w density fit
* y histogram w density fit
* scatter with smoother and linear fit

Other features of the shiny visualization app:

* Quick selection of different x, y data choices for your plot based on column names
* Ability to quickly test different transformations of the x, y data
* Automatic sampling of data for large data sets where graphs would load slowly
* Density and smoother fits (optional)
* Cap the right tail of the data for more informative histograms with skewed data sets
* Quickly view summary metrics for the data 
* Linear model summary output

The app uses the following packages:

* shiny
* ggplot2
* gridExtra
* data.table
