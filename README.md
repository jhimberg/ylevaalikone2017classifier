# ylevaalikone2017classifier
Demonstration of using R and a multinomial classifier in predicting candidates' party based on YLE election data.

Install R and R Studio. 

## Classification script
Open script `ml_classification.R` in R Studio editor and run it - in piece by piece, if you wish to follow what happens. 
You probably have to install some of the R libraries that the script loads. See `install.packages`.

The script will load the zipped data file from YLE site. 

## Shiny

Folder `shiny` contains Shiny application that includes interactive version of the classifier (in Finnish). Run the script `makemodel.R` first. It creates the classification results into file `malli.RData` that is then used by the shiny app. Model is loaded in `shiny/global.R` - make sure that `malli.RData` is on the right path.

