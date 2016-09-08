
rm(list=ls())

gc()

suppressMessages({
    library(faoswsUtil)
    library(devtools)    
    library(data.table)
    library(magrittr)
    library(reshape2)
    library(plyr)
    library(dplyr)
    library(ggplot2)
    library(openxlsx)
    library(grid)
})

setwd("C:/cygwin-64/home/sorbara/gitlab/Data_Output_Validation")
source(file = "getDataFromServer.R")
source(file = "getNames.R")

if(CheckDebug()) {
    SetClientFiles(dir = "C:/cygwin-64/home/sorbara/gitlab/Data_Output_Validation/R certificate files/Production")
    files = dir("C:/cygwin-64/home/sorbara/gitlab/Data_Output_Validation/R",full.names = TRUE)
    
    GetTestEnvironment(
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "820dc6cb-16a5-40d5-8661-e5c60baf8d9d"
    )  
    sapply(files, source)
}

# This read a csv file of a preimputed session 
# It does not get the data but it is used only to store a list of (unique) timeseries to be checked later on
# This because after the imputation the session is expanded and I lose track of what was the original selection
dataPreImp <- read.csv("data_1.csv",colClasses=c("character","character","character","character","numeric","character","character"))
uniqueSeries <- unique(dplyr::select(dataPreImp,geographicAreaM49,measuredItemCPC))

# Read the imputed data from the session (exported data)
dataPostImp <- read.csv("data_imp.csv",colClasses=c("character","character","character","character","numeric","character","character"))
dataPostImp <- reshape(dataPostImp,
                                timevar = "measuredElement", 
                                idvar = c("geographicAreaM49", "measuredItemCPC","timePointYears"), 
                                direction = "wide")

mergedPostImp <- merge(dataPostImp,uniqueSeries,by = c("geographicAreaM49","measuredItemCPC"))

luppete <- nrow(uniqueSeries)

chartPath ="C:/cygwin-64/home/sorbara/gitlab/Data_Output_Validation/chart_run5_exp_ratio_5510"
dir.create(chartPath,showWarnings = FALSE)
setwd(chartPath)

# magic variable for the threshold, everything above this value will be charted
threshold <- -1
# these are counters
belowThreshold <- 0
aboveThreshold <- 0
results <- data.frame(area = character(), cpc = character(), prevobs = numeric(), impobs = numeric(), ratio = numeric())

# element to chart
analyzedElement <- "5312"
pValue  <- paste("Value",analyzedElement,"pre",sep=".")
pStatus <- paste("Status",analyzedElement,"pre",sep=".")
pMethod <- paste("Method",analyzedElement,"pre",sep=".")
aValue  <- paste("Value",analyzedElement,"pos",sep=".")
aStatus <- paste("Status",analyzedElement,"pos",sep=".")
aMethod <- paste("Method",analyzedElement,"pos",sep=".")


for (obs in 1:luppete) {
    #     print(uniqueSeries[obs]$geographicAreaM49)
    #obs <- 6
    
    years       <- as.character(1990:2014)
    country     <- uniqueSeries$geographicAreaM49[obs]
    commodity   <- uniqueSeries$measuredItemCPC[obs]
    elements    <- c("5510","5312","5421")
    
    serverData <- reshape(getDataFromServer(),
                            timevar = "measuredElement", 
                            idvar = c("geographicAreaM49", "measuredItemCPC","timePointYears"), 
                            direction = "wide")
    
    names(serverData)[names(serverData)==paste("Value",analyzedElement,sep=".")] <- pValue
    names(serverData)[names(serverData)==paste("flagObservationStatus",analyzedElement,sep=".")] <- pStatus
    names(serverData)[names(serverData)==paste("flagMethod",analyzedElement,sep=".")] <- pMethod

    mergedPostImp <- transform(mergedPostImp, timePointYears = as.integer(timePointYears))
    
    # draw only imputed from 1999 onwards
    obsPost <- filter(mergedPostImp,
                      mergedPostImp$geographicAreaM49 == uniqueSeries$geographicAreaM49[obs]
                      & mergedPostImp$measuredItemCPC == uniqueSeries$measuredItemCPC[obs]
                      & mergedPostImp$timePointYears >= 1999)

    names(obsPost)[names(obsPost)==paste("Value",analyzedElement,sep=".")] <- aValue
    names(obsPost)[names(obsPost)==paste("flagObservationStatus",analyzedElement,sep=".")] <- aStatus
    names(obsPost)[names(obsPost)==paste("flagMethod",analyzedElement,sep=".")] <- aMethod
    
    # draw data of the time serie before imputation
    chartData <- filter(
                    merge(serverData,obsPost,by=c("geographicAreaM49", "measuredItemCPC","timePointYears"),all.x = TRUE), 
                    timePointYears >= 1990)

    breakInSeries <- filter(
                        merge(serverData,obsPost,by=c("geographicAreaM49", "measuredItemCPC","timePointYears"),all.x = TRUE), 
                        timePointYears == 1999)

    # check the ratio
    if (breakInSeries[[aValue]] > 0 & breakInSeries[[pValue]] > 0) {
        absRatio <- exp(abs(log(breakInSeries[[aValue]])-log(breakInSeries[[pValue]])))
    } else {
        absRatio <- 0
    }

    # report only
    results <- rbind(results,
                 c(uniqueSeries$geographicAreaM49[obs],uniqueSeries$measuredItemCPC[obs],breakInSeries[[pValue]],breakInSeries[[aValue]],absRatio),
                 stringsAsFactors=FALSE)

    print(paste(uniqueSeries$geographicAreaM49[obs],uniqueSeries$measuredItemCPC[obs],"has threshold",absRatio))
    
    # draw if above threshold (defined above)
    if (absRatio >= threshold) {
        aboveThreshold <- aboveThreshold + 1
        
        chartDataBeforeBreak    <- filter(chartData, timePointYears <= 1999)
        chartDataAfterBreak     <- filter(chartData, timePointYears >= 1999)
        
        # draw points with different shapes
        protectedEstimates      <- filter(chartData,(trimws(chartData[[pStatus]]) == "E" & trimws(chartData[[pMethod]]) %in% c("f","c","h")))
        protectedImputed        <- filter(chartData,(trimws(chartData[[pStatus]]) == "I" & trimws(chartData[[pMethod]]) %in% c("c")))
        protectedZero           <- filter(chartData,(trimws(chartData[[pStatus]]) == "M" & trimws(chartData[[pMethod]]) %in% c("-")))
        protectedOfficial       <- filter(chartData, trimws(chartData[[pStatus]]) == "")
        protectedSemiOfficial   <- filter(chartData, trimws(chartData[[pStatus]]) == "T")
        
        fileName = paste(paste(uniqueSeries$geographicAreaM49[obs],uniqueSeries$measuredItemCPC[obs],analyzedElement,sep="_"),".png",sep="")
        elementLabel <- if(analyzedElement=="5510") "Production [t]" else if (analyzedElement=="5312") "Area Harvested [ha]" else "Yield [t/ha]"
        
        ggplot() +
            geom_line(aes(x=chartDataBeforeBreak$timePointYears,y=chartDataBeforeBreak[[pValue]]),color="blue") +
            geom_line(aes(x=chartDataAfterBreak$timePointYears,y=chartDataAfterBreak[[pValue]]),color="blue",linetype=2) +
            geom_line(aes(x=obsPost$timePointYears,y=obsPost[[aValue]]), color="red") +
            ylab(label = elementLabel) +
            xlab(label = "Year") +
            geom_point(aes(x=protectedEstimates$timePointYears,y=protectedEstimates[[pValue]]),shape=2,color="blue",size=2) +
            geom_point(aes(x=protectedOfficial$timePointYears,y=protectedOfficial[[pValue]]),shape=0,color="blue",size=2) +
            geom_point(aes(x=protectedSemiOfficial$timePointYears,y=protectedSemiOfficial[[pValue]]),shape=1,color="blue",size=2) +            
            geom_point(aes(x=protectedImputed$timePointYears,y=protectedImputed[[pValue]]),shape=3,color="darkgrey",size=2) +            
            geom_point(aes(x=protectedZero$timePointYears,y=protectedZero[[pValue]]),shape=3,color="darkgrey",size=2) +            
            geom_rect(aes(xmin = 1990, xmax = 1999, ymin = -Inf, ymax = Inf), fill = "darkgrey", alpha = 0.1) +
            ggtitle(
                paste(
                    paste(getCountryName(uniqueSeries$geographicAreaM49[obs]),"/",getCommodityName(uniqueSeries$measuredItemCPC[obs])),
                    paste("\nRatio = ",round(absRatio,2))
                )
            )        

        ggsave(filename = fileName, width = 6, height = 4)
        
    } else {
        belowThreshold <- belowThreshold + 1
        print(paste(obs,"out of",luppete))
        print(paste(uniqueSeries$geographicAreaM49[obs],uniqueSeries$measuredItemCPC[obs],"below threshold"))
    }
    
} 

print(paste("Above threshold", aboveThreshold))
print(paste("Below threshold", belowThreshold))
print(paste("Total", luppete))

colnames(results) <- c("geo","cpc","imputedvalue","prevvalue","ratio")

write.csv2(results, file="break_in_series_5510.csv",row.names = FALSE)
