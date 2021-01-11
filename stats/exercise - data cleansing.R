
library(dplyr)
#library(mechkar)
source("../R/mechkar.R")

df <- read.csv("../data/bank-full.csv",sep = ";")
head(df)

####################
###   Functions  ###
####################

outlierMatrix <- function(data,threshold=1.5) {
    vn <- names(data)
    outdata <- data.frame(row1=1:nrow(data))
    for(v in vn) {
        if(is.numeric(data[[v]])) {
            outlow <- quantile(data[[v]],probs = 0.25,na.rm = T) 
            outhigh <- quantile(data[[v]],probs = 0.75, na.rm = T)
            irq_level <- (outhigh - outlow) * threshold
            outlow <- outlow - irq_level
            outhigh <- outhigh +  irq_level
            mv <- ifelse(data[[v]] < outlow | data[[v]] > outhigh, 1, 0)
            outdata[v] <- mv
        } else {
            mv <- rep(0,nrow(data))
        }
    }
    outdata$row1 <- NULL
    return(outdata)
}


missingMatrix <- function(data) {
    vn <- names(data)
    missdata <- data.frame(row1=1:nrow(data))
    for(v in vn) {
        mv <- ifelse(is.na(data[[v]]),1,0)
        missdata[v] <- mv
    }
    missdata$row1 <- NULL
    return(missdata)
}


############
## 1) Summarize the dataset
############


############
## 2) Analyze the data using statistical analysis (Table1)
############



############
## 3) Explore the data using graphics (exploreData / Sweetviz)
############


############
## 4) Create a correlation matrix 
############


############
## 5) Plot the correlation using a correlation plot
############


############
## 6) Which variables have outliers?
############


############
## 7) Create a missing matrix. Which variables have more missing values? Are there rows with many missings?
############



#############
## 8) Use logistic regression to check if the missingness is related to any other variable
#############


