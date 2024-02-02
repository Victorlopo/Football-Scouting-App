# Football Scouting App

## Overview

This Shiny application is developed for football analysis, focusing specifically on scouting tasks. It aims to provide scouts with a comprehensive tool to find players that best fit their requirements and to make informed decisions based on data-driven insights.

## Features

The application is structured into six tabs, each designed to address specific scouting questions and tasks:

### 1. Compare Players
Allows scouts to compare players based on multiple attributes using a radar chart, providing a visual representation of players' skills for easy comparison.

### 2. Similar Players
Displays clusters of similar players based on their characteristics, helping scouts find groups of players with similar skills and attributes.

### 3. Stats Correlation
Enables scouts to explore correlations or trends among players' characteristics, identify outliers, and discover which players stand out in their positions.

### 4. Nations
Visualizes the distribution of players' nationalities across different leagues, helping scouts identify countries rich in football talent for specific positions.

### 5. Offer Indicator
Provides scouts with an indication of the approximate salary and transfer fee for a player, based on the average market value of players with similar characteristics.

### 6. Database
Offers a simple and interactive way to explore the dataset, with functionalities such as sorting and filtering to perform easy queries.

## What is Shiny?

Shiny is an R package that makes it easy to build interactive web apps straight from R. This application leverages Shiny to present football scouting data in an interactive and user-friendly way, allowing scouts to perform complex analyses without the need for advanced programming skills.

## Dependencies

To run this application, you will need to install the following R packages:

- tidyverse
- plotly
- factoextra
- shiny
- shinydashboard
- shinyWidgets
- maps

You can install these packages using the following R command:

