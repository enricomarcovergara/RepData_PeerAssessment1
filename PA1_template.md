---
title: 'Reproducible Research: Peer Assessment 1'
author: 'Author: Vergara, Enrico Marco'
date: 'Date: May 1, 2019'
output: 
html_document:
keep_md: true
---

##------------------------------------------------------------------------------------------

## INTRODUCTION
As part of the completion for Reproducible Research Course, this course project entitled "Reproducible Research: Peer Assessment 1" aims to demonstrate the creation of an R Markdown document which reflects literate programming using markdown and R Programming environment. This assignment is described in multiple parts. A written report  that answers the required questions is to be provided.

##------------------------------------------------------------------------------------------

## DATA SET
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, cache = FALSE)
```
##------------------------------------------------------------------------------------------
## A. Loading and Preprocessing the Data

The first thing to do is to download and store the file to the working directory. If the file is already on the working directory, it is then unzipped and the data frame is stored to a variable name "activity" with the attributes forced to be on its proper classes (eg. steps and interval is numeric while data is Date)

```{r loading and extracting data} 
URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
Dataset <- "activity.zip"
if (!file.exists(Dataset)){
  download.file(URL, destfile = Dataset, mode='wb')
}
if (!file.exists(Dataset)){
  unzip(Dataset)
}

activity <- read.csv("activity.csv", sep = ",", colClasses = c("numeric", "Date", "numeric"))
```

Let us now look unto some of the pertinent details about our data.

```{r details about the data}
dim(activity)
names(activity)
head(activity[which(!is.na(activity$steps)), ]) # Data without NAs
str(activity)
```

##------------------------------------------------------------------------------------------
## B. What is mean total number of steps taken per day?

In order to calulate for the mean total number of steps taken per day, the total number of steps taken per day are firstly identified.
(NOTE: For this part, the missing values in the data set are ignored.)

```{r total number of steps per day}
TotalStepsPerDay <- aggregate(x = activity$steps, by = list(activity$date), FUN = sum)
names(TotalStepsPerDay) <- c("Date", "TotalSteps")
head(TotalStepsPerDay)
```

A useful to tool in order to see the data in a bigger picture is to use exploratory plots. This way, trends, potential patterns and discrepancies are easily determined. For the sample data, here is a histogram showing the frequency of total steps taken each day.

```{r histogram, Plot1, fig.height = 10, fig.width = 15, fig.align = "center"}
library(RColorBrewer)
Color1 <-  brewer.pal(9, "PuBu")
Plot1 <- barplot(height=TotalStepsPerDay$TotalSteps, 
                 names.arg=TotalStepsPerDay$Date,
                 col = Color1, ylim = c(0,25000),
                 xlab="DAY",
                 ylab = "TOTAL STEPS",
                 main = "TOTAL STEPS TAKEN PER DAY")
text(Plot1, TotalStepsPerDay$TotalSteps, 
     labels = TotalStepsPerDay$TotalSteps,
     pos = 3)
```

```{r save plot 1 on disk device, include=FALSE}
png('plot1.png', height = 750, width = 1500)
library(RColorBrewer)
Color1 <-  brewer.pal(9, "PuBu")
Plot1 <- barplot(height=TotalStepsPerDay$TotalSteps, 
                 names.arg=TotalStepsPerDay$Date,
                 col = Color1, ylim = c(0,25000),
                 xlab="DAY",
                 ylab = "TOTAL STEPS",
                 main = "HISTOGRAN OF TOTAL STEPS TAKEN PER DAY")
text(Plot1, TotalStepsPerDay$TotalSteps, 
     labels = TotalStepsPerDay$TotalSteps,
     pos = 3)
dev.off()
```

After grasping the totality of the data, here is the calulated mean and median of the total number of steps taken per day.

```{r mean and median}
Date <- data.frame(TotalStepsPerDay$Date)
Steps1 <- aggregate(x = activity$steps, by = list(activity$date), FUN = mean, na.rm=TRUE)
Steps2 <- aggregate(x = activity$steps, by = list(activity$date), FUN = median, na.rm=TRUE)
Steps <- cbind(Date, Steps1[,2], Steps2[,2])
names(Steps) <- c("Date", "Mean", "Median")
print(Steps,type="html")
```

##------------------------------------------------------------------------------------------
## C. What is the average daily activity pattern?

This time, in order to view the average daily activity pattern, another exploratory graph, time series, will be used. A time series table shows a sequence of numerical data points in successive order. Thus, the 5-minute interval and average number of steps taken averaged across all days will be plotted.

```{r mean steps per interval}
MeanStepsPerInteval <- aggregate(x = activity$steps, by = list(activity$interval), FUN = mean, na.rm=TRUE)
names(MeanStepsPerInteval) <- c("Interval", "MeanSteps")
head(MeanStepsPerInteval)
```

```{r time series, Plot2, fig.height = 10, fig.width = 15, fig.align = "center"}
Plot2 <- plot(x = MeanStepsPerInteval$Interval, 
              y = MeanStepsPerInteval$MeanSteps, 
              type = "l", col="darkslateblue", lwd=2.5,
              main="TIME SERIES PLOT OF AVERAGE STEPS TAKER PER INTERVAL",
              ylab="AVERAGE NUMBER OF STEPS", xlab="INTERVALS (PER 5 MINUTES)")
```

```{r save plot 2 on disk device, include=FALSE}
png('plot2.png', height = 750, width = 1500)
library(RColorBrewer)
plot(x = MeanStepsPerInteval$Interval, 
              y = MeanStepsPerInteval$MeanSteps, 
              type = "l", col="darkslateblue", lwd=2.5,
              main="TIME SERIES PLOT OF AVERAGE STEPS TAKER PER INTERVAL",
              ylab="AVERAGE NUMBER OF STEPS", xlab="INTERVALS (PER 5 MINUTES)")
dev.off()
```

Looking unto the data, to answer which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps, the following are made.

```{r max steps}
AverageMaxSteps <- max(MeanStepsPerInteval$MeanSteps)
MaxStepsInterval <- MeanStepsPerInteval$Interval[which.max(MeanStepsPerInteval$MeanSteps)]
AverageMaxSteps
MaxStepsInterval
```

Thus, the 5-minute interval, on average across all the days in the dataset, which contains the maximum number of steps is **835** with **206.1698** total of steps on average.

##------------------------------------------------------------------------------------------
## D. Imputing Missing Values

In order to confirm if there are missing values in the data.  
Is it true that there are missing values in the data?

```{r confirm NA}
anyNA(activity)
```

Now, it is time to look at each of the variable to grasp which of them is populated with missing values.

```{r NA per Variable}
NAValues <- data.frame(step=sum(is.na(activity$steps)), 
                       date=sum(is.na(activity$date)),
                       interval=sum(is.na(activity$interval)))
NAValues
```

It is very evident that the "steps" variable has all the missing values with a total of **2304**.
In order to solve this problem, an imputing strategy is to be deviced. There are lots of ways to deal with missing data in R and for this problem, "Mean/ Mode/ Median Imputation" strategy is to be used.  
To make the imputation possible, a loop is to be used in order to forced the missing values to be relaced with the corresponding mean that was calculated on the previous question.

```{r imputation}
ImputeActivity <- activity
for(x in 1:nrow(activity)) {
  if(is.na(ImputeActivity[x, 1])==TRUE) {
    ImputeActivity[x, 1] <- MeanStepsPerInteval[MeanStepsPerInteval$Interval %in% 
                                                  ImputeActivity[x, 3], 2]
  }
}
head(ImputeActivity)
```

Now to confirm if all the missing values in the data are replaced in the new data set.  
Is it true that there are still missing values in the data in the new data set?

```{r confirm NA again}
anyNA(ImputeActivity)
```

Since it is false, it is then confirmed that there are now no missing values on the new data set. However to really confirm, here is the summary per variable.

```{r NA per Variable again}
NAValues2 <- data.frame(step=sum(is.na(ImputeActivity$steps)), 
                       date=sum(is.na(ImputeActivity$date)),
                       interval=sum(is.na(ImputeActivity$interval)))
NAValues2
```

Now, to make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day, here are the actions taken.

```{r histogram, Plot3, fig.height = 10, fig.width = 15, fig.align = "center"}
TotalStepsPerDay2 <- aggregate(x = ImputeActivity$steps, by = list(ImputeActivity$date), FUN = sum)
names(TotalStepsPerDay2) <- c("Date", "TotalSteps")
head(TotalStepsPerDay2)
library(RColorBrewer)
Color2 <-  brewer.pal(9, "Purples")
Plot3 <- barplot(height=TotalStepsPerDay2$TotalSteps, 
                 names.arg=TotalStepsPerDay2$Date,
                 col = Color2, ylim = c(0,25000),
                 xlab="DAY",
                 ylab = "TOTAL STEPS",
                 main = "TOTAL STEPS TAKEN PER DAY")
text(Plot3, TotalStepsPerDay2$TotalSteps, 
     labels = round(TotalStepsPerDay2$TotalSteps, digits = 1),
     pos = 3)
```

```{r save plot 3 on disk device, include=FALSE}
png('plot3.png', height = 750, width = 1500)
library(RColorBrewer)
Color2 <-  brewer.pal(9, "Purples")
Plot3 <- barplot(height=TotalStepsPerDay2$TotalSteps, 
                 names.arg=TotalStepsPerDay2$Date,
                 col = Color2, ylim = c(0,25000),
                 xlab="DAY",
                 ylab = "TOTAL STEPS",
                 main = "TOTAL STEPS TAKEN PER DAY")
text(Plot3, TotalStepsPerDay2$TotalSteps, 
     labels = round(TotalStepsPerDay2$TotalSteps, digits = 1),
     pos = 3, digits = 2)
dev.off()
```

A big difference to the graph on the first part of this assignment can be seen for the latter has complete data. Here is the calculated mean and median per day, 

```{r mean and median 2}
Date2 <- data.frame(TotalStepsPerDay2$Date)
Steps3 <- aggregate(x = ImputeActivity$steps, by = list(ImputeActivity$date), FUN = mean, na.rm=TRUE)
Steps4 <- aggregate(x = ImputeActivity$steps, by = list(ImputeActivity$date), FUN = median, na.rm=TRUE)
Steps5 <- cbind(Date2, Steps3[,2], Steps4[,2])
names(Steps5) <- c("Date", "Mean", "Median")
print(Steps5,type="html")
```

Using the imputation strategy made, a big difference now can be seen from the histogram and table above. Firstly, it is now not populated by missing values. From the initial data set where missing values are existing, the histogram shows missing measurements (8 days) where there are no recordings. On the other hand, it rises to a certain value when the imputation was done. Same with the table, where calculations can't be done with existing missing values, now numerical values are observed and more useful.

##------------------------------------------------------------------------------------------
## E. Are there differences in activity patterns between weekdays and weekends?

To answer the question, it is needed first to identify the days of the week of the given date. The next step is to categorize if it falls on a weekend or a weekday. The following line of codes will give the desired factors for the date.

```{r date}
ImputeActivity2 <- ImputeActivity
ImputeActivity2$day <-weekdays(ImputeActivity2$date)
ImputeActivity2$typeofday <- ifelse (ImputeActivity2$day != "Saturday" & ImputeActivity2$day != "Sunday", "Weekday", "Weekend") 
head(ImputeActivity2)
```

To generate the a panel plot containing a time series plo of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days for the understanding of the patterns between weekdays and weekends, here is the comparison.

```{r mean steps per interval weekday vs weekeh}
Weekday <- subset(ImputeActivity2, ImputeActivity2$typeofday == "Weekday")
Weekend <- subset(ImputeActivity2, ImputeActivity2$typeofday == "Weekend")
MeanStepsPerInteval_Weekday <- aggregate(x = Weekday$steps, by = list(Weekday$interval), FUN = mean)
MeanStepsPerInteval_Weekend <- aggregate(x = Weekend$steps, by = list(Weekend$interval), FUN = mean)
names(MeanStepsPerInteval_Weekday) <- c("Interval", "MeanSteps")
MeanStepsPerInteval_Weekday$Type <- "Weekday"
names(MeanStepsPerInteval_Weekend) <- c("Interval", "MeanSteps")
MeanStepsPerInteval_Weekend$Type <- "Weekend"
head(MeanStepsPerInteval_Weekday)
head(MeanStepsPerInteval_Weekend)
```

Time Series:

```{r time series, fig.height = 10, fig.width = 15, fig.align = "center"}
par(mfrow=c(2, 1))
plot(x = MeanStepsPerInteval_Weekday$Interval, 
              y = MeanStepsPerInteval_Weekday$MeanSteps, 
              type = "l", col="purple", lwd=2.5,
              main="TIME SERIES PLOT OF AVERAGE STEPS TAKER PER INTERVAL (WEEKDAYS)",
              ylab="AVERAGE NUMBER OF STEPS", xlab="INTERVALS (PER 5 MINUTES)")
plot(x = MeanStepsPerInteval_Weekend$Interval, 
              y = MeanStepsPerInteval_Weekend$MeanSteps, 
              type = "l", col="blue", lwd=2.5,
              main="TIME SERIES PLOT OF AVERAGE STEPS TAKER PER INTERVAL (WEEKENDS)",
              ylab="AVERAGE NUMBER OF STEPS", xlab="INTERVALS (PER 5 MINUTES)")
```

```{r save plot 4 on disk device, include=FALSE}
png('plot4.png', height = 750, width = 1500)
par(mfrow=c(2, 1))
plot(x = MeanStepsPerInteval_Weekday$Interval, 
              y = MeanStepsPerInteval_Weekday$MeanSteps, 
              type = "l", col="purple", lwd=2.5,
              main="TIME SERIES PLOT OF AVERAGE STEPS TAKER PER INTERVAL (WEEKDAYS)",
              ylab="AVERAGE NUMBER OF STEPS", xlab="INTERVALS (PER 5 MINUTES)")
plot(x = MeanStepsPerInteval_Weekend$Interval, 
              y = MeanStepsPerInteval_Weekend$MeanSteps, 
              type = "l", col="blue", lwd=2.5,
              main="TIME SERIES PLOT OF AVERAGE STEPS TAKER PER INTERVAL (WEEKENDS)",
              ylab="AVERAGE NUMBER OF STEPS", xlab="INTERVALS (PER 5 MINUTES)")
dev.off()
```
