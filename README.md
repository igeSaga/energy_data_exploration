# Application for visually exploring energy data
The goal of this project is to develope an application for the visual exploration of energy data. As an example of energy data, the electricity consumption of households in the [Suurstoffi](https://www.suurstoffi.ch/) district is used. The data was collected over 2.5 years - from spring 2013 to autumn 2015 - with a temporal resolution of 15min (appr. 35'000 measurements per household). The application is implemented as a ShinyApp.

## Content
The project consists of three scripts:
* 99_preparingInput_fromSeperateFiles.R: Reads all CSV files in a folder and combines it to a matrix, with one row for each recorded time-stamp and one column for each sensor.
* ui.R: User interface of the ShinyApp
* server.R: Functionality of Shiny App

## Requironments
The input data is not attached to this repository, due to privacy issues. However, the application can be changed, such that different input data can be read and visualized.

The input data should be a matrix, with row-names refering to time-stamps and column-names representing labels of different sensors (or households, as in our case).

## Authors
* **Curdin Derungs** - *Initial work* - [curdon](https://github.com/curdon)

## License
This project and all incorporated data is property of the HSLU and can only be used upon request.

