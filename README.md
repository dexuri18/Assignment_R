# Assignment_R
## Authors
Deky and Tono

## Description
This repository was created as part of assignment on Big Data Analysis course, which will attempt to analyze a database of rodents which were collected from 1977 to 2002 in southern Arizona to determine the relationship between the weight and length of hindfoot, using R Studio.

## Prerequisite
Rstudio program
install.packages("gridExtra")
install.packages("ggpubr")
library(gridExtra)
library(tidyverse)
library("ggpubr")

## Contents of the repository
This repository consists of:
1. Script R (* .R). It is a raw script written by the authors (Deky and Tono) covering every command used in the analysis.
2. Rmarkdown (* .Rmd). It is a summary of the R analysis performed.
3. Pdf file (* .pdf). It is a knited result of Rmarkdown.
4. html file (* .html). It is a knited result of Rmarkdown.
5. Readme.md. An explanation of this repository.
6. The data folder containing the data files being analyzed.
7. scripts.Rproj. It is the project file for RStudio.

## How to
To replicate the process:
1. Install the RStudio
2. Install the packages
3. Load all the library()
4. Download all the file/folder on this repository
5. Run the scripts.Rproj that will open in RStudio
6. Open the * .R file (script file) 
7. Open the * .Rmd file (Rmarkdown file)

## Statistical analysis in this repository
Analysis conducted in this repository included:
1. Adding the trendline on the plot to see the R2
2. Linear model analysis to see the relationship between two variables
3. F-test (statistical test) to see the variances between two population
4. T-test (statistical test) to see the difference between two population

## Working procedures in this repository
Procedures performed in this repository included:
1. Cleaning up the incomplete data  
2. Plotting the general information about the data based on the number of samples for each species and genus, also the proportion of sex (female and male) for each category
3. Plotting the distribution of weight and hindfoot length for the overall sample and the distribution of the mean (weight) weight and hindfoot length for each species
4. Plotting the weight and hindfoot length relationship analysis for all complete dataset
5. Perform the linear model of weight and hindfoot length of the merriami species
6. Perform the T-test to see if the mean weight and the hindfoot length of female species are significantly different from the male species. 



