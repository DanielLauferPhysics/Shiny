#import libraries

library(shinydashboard)
library(tidyverse)
library(reshape2)
library(data.table)
library(leaflet)
library(htmltools)
library(rsconnect)

#read in the file
airbnb = fread(file='new-york-city-airbnb-open-data/AB_NYC_2019.csv', stringsAsFactors = FALSE)

#cast NA's in reviews_per_month column as zeros
airbnb$reviews_per_month[is.na(airbnb$reviews_per_month)] <- 0

#remove rows with zero price
airbnb <- airbnb %>% 
          filter(., price > 0)

#remove rows associated with inactive listing at the time of data collection
airbnb <- airbnb %>% 
          filter(., availability_365 > 0)

#filter out listings with no titles and assign the character count of the remaining to name.length
airbnb = filter(airbnb, name!='') 
airbnb['name.length'] = nchar(airbnb$name)

# create variable with colnames as choice
choice <- colnames(airbnb)[-1]


#helper functions:

#Removing the upper half of the correlation matrix entries
get_tri <- function(corrmatrix){
  corrmatrix[lower.tri(corrmatrix)] <- NA
  return(corrmatrix)
}

#Sort the correlation matrix in ascending order of correlation (not functioning presently)
reorder_cordata <- function(corrmatrix){
  dmatrix <- as.dist((1-corrmatrix)/2)
  hc <- hclust(dmatrix)
  corrmatrix <- corrmatrix[hc$order, hc$order]
}

#Label outlying points in box and whisker plots (didn't have time to implement)
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
