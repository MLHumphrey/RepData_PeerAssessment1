# Reproducible Research: Peer Assessment 1
M.L. Humphrey  
March 24, 2016  
Executive Summary
The following document walks through how to load and analyze the Activity Monitoring Data provided as part of the Reproducible Research course offered by Johns Hopkins University.  The below items are answers to the questions asked as part of the assignmmet.

Loading and Processing the Data
Assuming that the zip file is in your working directory, then the data can be loaded with the following R command:


```r
if(!file.exists("data")){
dir.create("data")
}
unzip("./activity.zip", exdir="./data")
ActivityData<-read.csv("./data/activity.csv")
```

Histogram Of Number Of Steps Per Day
The next step in the analysis is to create a histogram of the number of steps taken per day.  In order to do this, the first step is to take the data set, ActivityData, and calculcate the total number of steps taken each day.  This is done with the following R code and assumes that the package dplyr has already been installed by the user:


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
ActivityDate<-group_by(ActivityData,date)
ActivityGrouped<-summarize(ActivityDate,steps=sum(steps,na.rm=TRUE))
```

Now that we've created the new dataset, ActivityGrouped that combines all steps within a given day, we can create the histogram:


```r
hist(ActivityGrouped$steps, main="Histogram of Total Number Of Steps Taken Per Day", xlab="Number of Steps",ylab="Frequency Of Occurence")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

Mean Number of Steps Per Day and Median Number of Steps Per Day
The next question is what is the mean number of steps taken per day and what is the median number of steps taken per day?  This can be calculated using the following formulas, for mean:


```r
mean(ActivityGrouped$steps)
```

```
## [1] 9354.23
```

And for median:


```r
median(ActivityGrouped$steps)
```

```
## [1] 10395
```

Analysis of Average Number of Steps Taken
Next step is to look at the data from the perspective of time intervals.  On average, are the number of steps taken througout the day consistent, or is there a period of time when the number of steps is much higher than average?  In order to look at this, we'll first plot the average number of steps taken for each interval during the sample time period.

First, we need to group the data by time interval and calculate the average number of steps for each interval:


```r
ActivityInterval<-group_by(ActivityData,interval)
ActivIntGrpd<-summarize(ActivityInterval,steps=mean(steps,na.rm=TRUE))
```

Now that we have that, we can create our plot:


```r
with(ActivIntGrpd, plot(interval, steps,type="l",main="Time Interval vs. Steps Taken"))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)

It's pretty obvious from the plot that there's one interval where the number of steps is substantially higher than the rest of the day.  What is that time interval?  We can eyeball it on the graph or we can calculate it with the following R code.


```r
 x<-ActivIntGrpd[(which.max(ActivIntGrpd$steps)),]
 x$interval
```

```
## [1] 835
```

Imputing Missing Values
Now that we've conducted a preliminary analysis it's time to take a closer look at the data.  We noticed when we looked at the head and tail of the original data set that there were a number of NA values for step count.  But how many?  And when do they occur?

To get the number of NA values we can use the following:


```r
sum(is.na(ActivityData))
```

```
## [1] 2304
```

Now that we know that, we can see whether those values are for entire days or just time intervals during those days.  We do that with the following R code:


```r
x<-is.na(ActivityData)
unique(ActivityData$interval[x])
```

```
##   [1]    0    5   10   15   20   25   30   35   40   45   50   55  100  105
##  [15]  110  115  120  125  130  135  140  145  150  155  200  205  210  215
##  [29]  220  225  230  235  240  245  250  255  300  305  310  315  320  325
##  [43]  330  335  340  345  350  355  400  405  410  415  420  425  430  435
##  [57]  440  445  450  455  500  505  510  515  520  525  530  535  540  545
##  [71]  550  555  600  605  610  615  620  625  630  635  640  645  650  655
##  [85]  700  705  710  715  720  725  730  735  740  745  750  755  800  805
##  [99]  810  815  820  825  830  835  840  845  850  855  900  905  910  915
## [113]  920  925  930  935  940  945  950  955 1000 1005 1010 1015 1020 1025
## [127] 1030 1035 1040 1045 1050 1055 1100 1105 1110 1115 1120 1125 1130 1135
## [141] 1140 1145 1150 1155 1200 1205 1210 1215 1220 1225 1230 1235 1240 1245
## [155] 1250 1255 1300 1305 1310 1315 1320 1325 1330 1335 1340 1345 1350 1355
## [169] 1400 1405 1410 1415 1420 1425 1430 1435 1440 1445 1450 1455 1500 1505
## [183] 1510 1515 1520 1525 1530 1535 1540 1545 1550 1555 1600 1605 1610 1615
## [197] 1620 1625 1630 1635 1640 1645 1650 1655 1700 1705 1710 1715 1720 1725
## [211] 1730 1735 1740 1745 1750 1755 1800 1805 1810 1815 1820 1825 1830 1835
## [225] 1840 1845 1850 1855 1900 1905 1910 1915 1920 1925 1930 1935 1940 1945
## [239] 1950 1955 2000 2005 2010 2015 2020 2025 2030 2035 2040 2045 2050 2055
## [253] 2100 2105 2110 2115 2120 2125 2130 2135 2140 2145 2150 2155 2200 2205
## [267] 2210 2215 2220 2225 2230 2235 2240 2245 2250 2255 2300 2305 2310 2315
## [281] 2320 2325 2330 2335 2340 2345 2350 2355
```

```r
unique(ActivityData$date[x])
```

```
## [1] 2012-10-01 2012-10-08 2012-11-01 2012-11-04 2012-11-09 2012-11-10
## [7] 2012-11-14 2012-11-30
## 61 Levels: 2012-10-01 2012-10-02 2012-10-03 2012-10-04 ... 2012-11-30
```

What we find is that when data is missing it's missing for an entire day.  Now the question is how do we fill in those gaps.  If the data is consistent for the entire time period, then we can probably get away with using the overall average for each time interval.  If it's not consistent, then we'll need to take a different approach, likely the average of a few days on either side of our missing days.

We can plot the data to see if it changes significantly over the period using the following:


```r
with(ActivityGrouped, plot(date, steps))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)

The data is pretty consistent for the entire time period so we'll just replace the missing values with the average for each time interval for the entire sample.  We do that with the following:

First, we're going to create a new column where we can do the replacement.  This lets us maintain the original values and also do a visual check to make sure our transformation worked.


```r
ActivityData$newsteps<-ActivityData$steps
```

Next, we'll create a list of unique interval values and use a for loop to make our replacement:


```r
x<-unique(ActivityData$interval)
for(i in x) {
	ActivityData$newsteps[ActivityData$interval==i &is.na(ActivityData$steps)]<-ActivIntGrpd$steps[ActivIntGrpd$interval==i]
	}
```

Histogram of Number of Steps Per Day With Replaced Values
We can look at a histogram of this new dataset and see if the transformation changed things, which it should since we'll now have more values at the center of the histogram for each of the days that had missing values:


```r
ActivityRev<-group_by(ActivityData,date)
ActivityRevGrp<-summarize(ActivityRev,newsteps=sum(newsteps))
hist(ActivityRevGrp$newsteps,main="Adjusted Total Number of Steps Taken Per Day",xlab="Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)

We can also take the mean and median of our new dataset and see if those have changed.

First, the mean:


```r
mean(ActivityRevGrp$newsteps)
```

```
## [1] 10766.19
```

Next, the median:

```r
median(ActivityRevGrp$newsteps)
```

```
## [1] 10766.19
```

The result for the median is what we'd expect.  It's equal to the sum of the average steps per day for the time period.  This makes sense since we forced all the days with missing values to this number, it's the average so in the center of possible values, and those days were about 10% of the total days sampled.

The mean result is a little surprising, but if we play with the data a bit we'll see that the mean and median are only equal for this full sample. Numbers just do that sometimes.  If we'd taken a different approach to how we replaced the missing values I wouldn't expect the mean and median of the new dataset to be equal.

Also, as expected, these numbers are not the same as the values in the original dataset.

Weekdays Vs. Weekends
The next step is to look at our data to see if there's a difference between weekdays and weekends.  We need to make sure the date column is formattted as a date and then create a new column using the weekdays function that tells us which dates correspond to which day of the month.


```r
ActivityData$date<-as.Date(ActivityData$date)
ActivityData$weekdays<-weekdays(ActivityData$date)
```

Now, we want to assign Monday through Friday days to Weekdays and Saturday and Sunday to weekends.  There's probably an easier way to do it, but the following R code works:


```r
ActivityData$weekdays <- gsub("Monday", "Weekday", ActivityData$weekdays)
ActivityData$weekdays <- gsub("Tuesday", "Weekday", ActivityData$weekdays)
ActivityData$weekdays <- gsub("Wednesday", "Weekday", ActivityData$weekdays)
ActivityData$weekdays <- gsub("Thursday", "Weekday", ActivityData$weekdays)
ActivityData$weekdays <- gsub("Friday", "Weekday", ActivityData$weekdays)
ActivityData$weekdays <- gsub("Saturday", "Weekend", ActivityData$weekdays)
ActivityData$weekdays <- gsub("Sunday", "Weekend", ActivityData$weekdays)
```

Now we need to convert that column into factor variable and take the average number of steps per interval for the weekdays and weekends. Again, there's probably an easier way to do this, but the below code creates two separate datasets, one for weekend and weekdays that takes the average number of steps for each interval and then brings them back together with a column that labels the result either weekend or weekday.


```r
ActivityData$weekdays<-as.factor(ActivityData$weekdays)
ActivityDataWD<-subset(ActivityData,weekdays=="Weekday")
ActivityDataWE<-subset(ActivityData,weekdays=="Weekend")
ActivityWD<-group_by(ActivityDataWD,interval)
ActivityWDGrp<-summarize(ActivityWD,newsteps=sum(newsteps))
ActivityWE<-group_by(ActivityDataWE,interval)
ActivityWEGrp<-summarize(ActivityWE,newsteps=sum(newsteps))
ActivityWEGrp$weekdays<-"Weekend"
ActivityWDGrp$weekdays<-"Weekday"
PlotSet<-rbind(ActivityWEGrp,ActivityWDGrp)
```

Now we create a plot that compares the two to see if there's any significant difference between average steps per interval for weekends or weekdays.  The below code assumes the lattice package is installed:


```r
library(lattice) 
attach(PlotSet)
xyplot(newsteps~interval|weekdays,layout=c(1,2),type=c("l"),ylab="Number of Steps",xlab="Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-20-1.png)

From the plots we can see that the high number of steps we were seeing at 8:35 each morning are mainly from weekdays. Whoever this is appears to take a break from the gym on weekends and to generally be less active over the weekend throughout the day.  They almost never spike above 4,000 steps on a weekend, but look to approach that level at least three time on average during the week.
