---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

# Peer Assessment 1 - Reproducible Research

Coded by: José Robles

## Packages

It is important to notice that we requiere these two packages:


```r
library("plyr")
library("ggplot2")
```


## Loading and preprocessing the data

We include the script **PA1_script.R** that does the loading, preprocessing and analysis as requested.

First we need to set our working directory as the source file location, the script should be in the same directory where the data file activity.csv is located. In my case:


```r
setwd("~/Documents/DataScienceSpecialization/5-RepRes/RepData_PeerAssessment1")
activityData <- read.csv("~/Documents/DataScienceSpecialization/5-RepRes/RepData_PeerAssessment1/activity.csv")
```

We make sure that we have the correct datatype for each column:


```r
activityData$steps <- as.numeric(as.character(activityData$steps))
activityData$interval <- as.numeric(as.character(activityData$interval))
activityData$date <- as.Date(activityData$date,"%Y-%m-%d")
```


For the first part, we can ignore the missing values, so we will just extract all the *complete cases*:


```r
activityDataCompleteCases <- activityData[complete.cases(activityData),]
```

For each day, we calculate the total number of steps:


```r
activityDataCompleteCasesTotalSteps <- ddply(activityDataCompleteCases,  "date", summarise, steps = sum(steps))
```

And we plot the histogram:


```r
plot(activityDataCompleteCasesTotalSteps$date, activityDataCompleteCasesTotalSteps$steps,type="h",lwd=8,col="blue",xlab="Days", ylab="Total steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

## What is mean and median of total number of steps taken per day?


```r
mean(activityDataCompleteCasesTotalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(activityDataCompleteCasesTotalSteps$steps)
```

```
## [1] 10765
```

We can see that those values are almost the same


## Imputing missing values

The number of rows with missing values is:


```r
sum(!complete.cases(activityData))
```

```
## [1] 2304
```

We will calculate the mean for each day and that number will sustitute the missing value for that day:


```r
meansEachDay = ddply(activityData,  "date", summarise, steps = mean(steps))
```

Let's notice that for a full day, there are no records of activity, and for that reason, the mean is a missing value:


```r
meansEachDay
```

```
##          date      steps
## 1  2012-10-01         NA
## 2  2012-10-02  0.4375000
## 3  2012-10-03 39.4166667
## 4  2012-10-04 42.0694444
## 5  2012-10-05 46.1597222
## 6  2012-10-06 53.5416667
## 7  2012-10-07 38.2465278
## 8  2012-10-08         NA
## 9  2012-10-09 44.4826389
## 10 2012-10-10 34.3750000
## 11 2012-10-11 35.7777778
## 12 2012-10-12 60.3541667
## 13 2012-10-13 43.1458333
## 14 2012-10-14 52.4236111
## 15 2012-10-15 35.2048611
## 16 2012-10-16 52.3750000
## 17 2012-10-17 46.7083333
## 18 2012-10-18 34.9166667
## 19 2012-10-19 41.0729167
## 20 2012-10-20 36.0937500
## 21 2012-10-21 30.6284722
## 22 2012-10-22 46.7361111
## 23 2012-10-23 30.9652778
## 24 2012-10-24 29.0104167
## 25 2012-10-25  8.6527778
## 26 2012-10-26 23.5347222
## 27 2012-10-27 35.1354167
## 28 2012-10-28 39.7847222
## 29 2012-10-29 17.4236111
## 30 2012-10-30 34.0937500
## 31 2012-10-31 53.5208333
## 32 2012-11-01         NA
## 33 2012-11-02 36.8055556
## 34 2012-11-03 36.7048611
## 35 2012-11-04         NA
## 36 2012-11-05 36.2465278
## 37 2012-11-06 28.9375000
## 38 2012-11-07 44.7326389
## 39 2012-11-08 11.1770833
## 40 2012-11-09         NA
## 41 2012-11-10         NA
## 42 2012-11-11 43.7777778
## 43 2012-11-12 37.3784722
## 44 2012-11-13 25.4722222
## 45 2012-11-14         NA
## 46 2012-11-15  0.1423611
## 47 2012-11-16 18.8923611
## 48 2012-11-17 49.7881944
## 49 2012-11-18 52.4652778
## 50 2012-11-19 30.6979167
## 51 2012-11-20 15.5277778
## 52 2012-11-21 44.3993056
## 53 2012-11-22 70.9270833
## 54 2012-11-23 73.5902778
## 55 2012-11-24 50.2708333
## 56 2012-11-25 41.0902778
## 57 2012-11-26 38.7569444
## 58 2012-11-27 47.3819444
## 59 2012-11-28 35.3576389
## 60 2012-11-29 24.4687500
## 61 2012-11-30         NA
```

We will sustitute that missing value with zero (we can think that no activity took place in that day):


```r
meansEachDay$steps[is.na(meansEachDay$steps)] <- 0
```

Now the variable **activityData2** will hold the original dataset with the imputed missing values:


```r
activityData2 = activityData

j=1
for(i in 1:nrow(activityData2)){
  if(is.na(activityData2[i,1])){
    if(activityData2[i,2]==meansEachDay[j,1]){
      activityData2[i,1] = meansEachDay[j,2]
      
    }else{
      j = j+1
      activityData2[i,1] = meansEachDay[j,2]
      
    }
  }
}
```

We calculate the total number of steps for each day now for **activityData2** and we compare the mean and median with the previous case where we ignore missing values::


```r
activityData2TotalSteps = ddply(activityData2,  "date", summarise, steps = sum(steps))
mean(activityData2TotalSteps$steps)
```

```
## [1] 9386.709
```

```r
median(activityData2TotalSteps$steps)
```

```
## [1] 10395
```

```r
mean(activityDataCompleteCasesTotalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(activityDataCompleteCasesTotalSteps$steps)
```

```
## [1] 10765
```

We can see that the mean and median are lower when we impute the missing values. I think this is because as we saw previously, there is no activity for some full days and we just keep adding many zeros for a longer period of time, causing the mean to decrease.

## What is the average daily activity pattern?

For our second dataset, **activityData2**, we create a new column:


```r
activityData2$weekend = "Weekend"
```

And we add the proper label if it the day listed in the column *date* is  *Weekday* or *Weekend*:


```r
activityData2[weekdays(activityData2$date) %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")
      ,c("weekend") ] = "Weekday"
```

We calculate the average number of steps for every 5-minute interval, grouped by *Weekend* and *Weekday*:


```r
my5interval2 = ddply(activityData2,  c("weekend","interval"), summarise, steps = mean(steps))
```

And we change the column *weekend* to factor:

```r
my5interval2$weekend = as.factor(my5interval2$weekend)
```


## Are there differences in activity patterns between weekdays and weekends?

This is the information we will plot:


```r
my5interval2
```

```
##     weekend interval        steps
## 1   Weekday        0   4.80192901
## 2   Weekday        5   3.93333333
## 3   Weekday       10   4.55200617
## 4   Weekday       15   3.95902778
## 5   Weekday       20   2.91967593
## 6   Weekday       25   3.67106481
## 7   Weekday       30   2.39120370
## 8   Weekday       35   3.76226852
## 9   Weekday       40   2.67330247
## 10  Weekday       45   3.49305556
## 11  Weekday       50   2.18140432
## 12  Weekday       55   1.66334877
## 13  Weekday      100   2.11134259
## 14  Weekday      105   1.82430556
## 15  Weekday      110   1.40964506
## 16  Weekday      115   1.04467593
## 17  Weekday      120   0.19228395
## 18  Weekday      125   1.83410494
## 19  Weekday      130   2.75856481
## 20  Weekday      135   0.88410494
## 21  Weekday      140   0.58719136
## 22  Weekday      145   0.95763889
## 23  Weekday      150   1.50046296
## 24  Weekday      155   0.00000000
## 25  Weekday      200   0.00000000
## 26  Weekday      205   0.00000000
## 27  Weekday      210   1.24444444
## 28  Weekday      215   0.00000000
## 29  Weekday      220   0.00000000
## 30  Weekday      225   0.15555556
## 31  Weekday      230   0.00000000
## 32  Weekday      235   0.26666667
## 33  Weekday      240   0.00000000
## 34  Weekday      245   0.00000000
## 35  Weekday      250   1.82222222
## 36  Weekday      255   1.11111111
## 37  Weekday      300   0.00000000
## 38  Weekday      305   0.00000000
## 39  Weekday      310   0.00000000
## 40  Weekday      315   0.00000000
## 41  Weekday      320   0.00000000
## 42  Weekday      325   0.73333333
## 43  Weekday      330   1.02222222
## 44  Weekday      335   0.44444444
## 45  Weekday      340   0.35555556
## 46  Weekday      345   0.08888889
## 47  Weekday      350   0.00000000
## 48  Weekday      355   0.00000000
## 49  Weekday      400   0.11111111
## 50  Weekday      405   1.11111111
## 51  Weekday      410   1.88888889
## 52  Weekday      415   0.00000000
## 53  Weekday      420   0.40000000
## 54  Weekday      425   0.00000000
## 55  Weekday      430   2.82222222
## 56  Weekday      435   0.13333333
## 57  Weekday      440   3.31111111
## 58  Weekday      445   0.77777778
## 59  Weekday      450   1.93333333
## 60  Weekday      455   0.57777778
## 61  Weekday      500   0.00000000
## 62  Weekday      505   1.84444444
## 63  Weekday      510   3.53333333
## 64  Weekday      515   1.88888889
## 65  Weekday      520   3.77777778
## 66  Weekday      525   2.31111111
## 67  Weekday      530   2.46666667
## 68  Weekday      535   7.13333333
## 69  Weekday      540  18.26666667
## 70  Weekday      545  21.20000000
## 71  Weekday      550  45.08888889
## 72  Weekday      555  50.33333333
## 73  Weekday      600  37.08888889
## 74  Weekday      605  58.02222222
## 75  Weekday      610  62.91111111
## 76  Weekday      615  68.68888889
## 77  Weekday      620  57.26666667
## 78  Weekday      625  53.75555556
## 79  Weekday      630  59.48888889
## 80  Weekday      635  42.73333333
## 81  Weekday      640  49.80000000
## 82  Weekday      645  48.97777778
## 83  Weekday      650  42.06666667
## 84  Weekday      655  53.88888889
## 85  Weekday      700  44.75555556
## 86  Weekday      705  44.91111111
## 87  Weekday      710  55.28888889
## 88  Weekday      715  62.06666667
## 89  Weekday      720  56.44444444
## 90  Weekday      725  52.31111111
## 91  Weekday      730  58.80000000
## 92  Weekday      735  48.44444444
## 93  Weekday      740  55.75555556
## 94  Weekday      745  74.11111111
## 95  Weekday      750  60.02222222
## 96  Weekday      755  59.08888889
## 97  Weekday      800  72.93333333
## 98  Weekday      805  62.86666667
## 99  Weekday      810 126.75555556
## 100 Weekday      815 160.97777778
## 101 Weekday      820 177.75555556
## 102 Weekday      825 162.88888889
## 103 Weekday      830 175.24444444
## 104 Weekday      835 202.88888889
## 105 Weekday      840 192.77777778
## 106 Weekday      845 161.71111111
## 107 Weekday      850 166.77777778
## 108 Weekday      855 154.82222222
## 109 Weekday      900 148.53333333
## 110 Weekday      905 109.24444444
## 111 Weekday      910  79.40000000
## 112 Weekday      915  72.88888889
## 113 Weekday      920  89.71111111
## 114 Weekday      925  79.66666667
## 115 Weekday      930  49.68888889
## 116 Weekday      935  29.82222222
## 117 Weekday      940  24.15555556
## 118 Weekday      945  35.68888889
## 119 Weekday      950  34.46666667
## 120 Weekday      955  14.82222222
## 121 Weekday     1000  32.46666667
## 122 Weekday     1005  14.62222222
## 123 Weekday     1010  33.42222222
## 124 Weekday     1015  40.80000000
## 125 Weekday     1020  25.15555556
## 126 Weekday     1025  28.37777778
## 127 Weekday     1030  27.22222222
## 128 Weekday     1035  19.26666667
## 129 Weekday     1040  18.88888889
## 130 Weekday     1045  22.13333333
## 131 Weekday     1050  18.68888889
## 132 Weekday     1055  19.00000000
## 133 Weekday     1100  17.51111111
## 134 Weekday     1105  21.13333333
## 135 Weekday     1110   8.84444444
## 136 Weekday     1115  12.86666667
## 137 Weekday     1120  20.40000000
## 138 Weekday     1125  20.20000000
## 139 Weekday     1130  28.31111111
## 140 Weekday     1135  43.53333333
## 141 Weekday     1140  38.95555556
## 142 Weekday     1145  41.97777778
## 143 Weekday     1150  43.97777778
## 144 Weekday     1155  48.24444444
## 145 Weekday     1200  47.20000000
## 146 Weekday     1205  61.15555556
## 147 Weekday     1210  71.00000000
## 148 Weekday     1215  62.91111111
## 149 Weekday     1220  40.26666667
## 150 Weekday     1225  40.13333333
## 151 Weekday     1230  55.31111111
## 152 Weekday     1235  26.42222222
## 153 Weekday     1240  18.44444444
## 154 Weekday     1245  24.28888889
## 155 Weekday     1250  26.77777778
## 156 Weekday     1255  47.62222222
## 157 Weekday     1300  18.95555556
## 158 Weekday     1305  20.42222222
## 159 Weekday     1310  18.80000000
## 160 Weekday     1315  10.17777778
## 161 Weekday     1320  29.46666667
## 162 Weekday     1325  37.33333333
## 163 Weekday     1330  26.06666667
## 164 Weekday     1335  19.95555556
## 165 Weekday     1340  19.91111111
## 166 Weekday     1345  33.04444444
## 167 Weekday     1350  19.26666667
## 168 Weekday     1355  28.22222222
## 169 Weekday     1400  39.48888889
## 170 Weekday     1405  32.62222222
## 171 Weekday     1410  26.31111111
## 172 Weekday     1415  38.55555556
## 173 Weekday     1420  22.75555556
## 174 Weekday     1425  25.75555556
## 175 Weekday     1430  25.91111111
## 176 Weekday     1435  10.84444444
## 177 Weekday     1440   9.26666667
## 178 Weekday     1445  18.51111111
## 179 Weekday     1450  36.04444444
## 180 Weekday     1455  32.44444444
## 181 Weekday     1500  26.86666667
## 182 Weekday     1505  30.24444444
## 183 Weekday     1510  25.22222222
## 184 Weekday     1515  26.73333333
## 185 Weekday     1520  33.73333333
## 186 Weekday     1525  30.97777778
## 187 Weekday     1530  35.71111111
## 188 Weekday     1535  42.22222222
## 189 Weekday     1540  79.51111111
## 190 Weekday     1545  82.71111111
## 191 Weekday     1550  80.33333333
## 192 Weekday     1555  59.11111111
## 193 Weekday     1600  38.60000000
## 194 Weekday     1605  36.64444444
## 195 Weekday     1610  46.66666667
## 196 Weekday     1615  27.71111111
## 197 Weekday     1620  19.22222222
## 198 Weekday     1625  21.55555556
## 199 Weekday     1630  16.66666667
## 200 Weekday     1635  16.68888889
## 201 Weekday     1640  19.91111111
## 202 Weekday     1645  25.93333333
## 203 Weekday     1650  21.46666667
## 204 Weekday     1655  26.60000000
## 205 Weekday     1700  17.35555556
## 206 Weekday     1705  37.44444444
## 207 Weekday     1710  27.42222222
## 208 Weekday     1715  39.91111111
## 209 Weekday     1720  50.42222222
## 210 Weekday     1725  61.84444444
## 211 Weekday     1730  46.95555556
## 212 Weekday     1735  57.86666667
## 213 Weekday     1740  72.86666667
## 214 Weekday     1745  51.80000000
## 215 Weekday     1750  29.86666667
## 216 Weekday     1755  32.60000000
## 217 Weekday     1800  21.22222222
## 218 Weekday     1805  38.88888889
## 219 Weekday     1810  57.26666667
## 220 Weekday     1815  71.26666667
## 221 Weekday     1820  53.48888889
## 222 Weekday     1825  64.42222222
## 223 Weekday     1830  68.86666667
## 224 Weekday     1835  71.60000000
## 225 Weekday     1840  80.33333333
## 226 Weekday     1845 102.20000000
## 227 Weekday     1850  89.75555556
## 228 Weekday     1855  79.17777778
## 229 Weekday     1900  76.24444444
## 230 Weekday     1905  66.84444444
## 231 Weekday     1910  54.64444444
## 232 Weekday     1915  47.26666667
## 233 Weekday     1920  33.04444444
## 234 Weekday     1925  17.80000000
## 235 Weekday     1930  25.44444444
## 236 Weekday     1935  40.64444444
## 237 Weekday     1940  26.02222222
## 238 Weekday     1945  15.17777778
## 239 Weekday     1950  38.17777778
## 240 Weekday     1955  22.82222222
## 241 Weekday     2000  10.77777778
## 242 Weekday     2005   3.02222222
## 243 Weekday     2010   4.24444444
## 244 Weekday     2015   9.66666667
## 245 Weekday     2020   5.13333333
## 246 Weekday     2025   2.88888889
## 247 Weekday     2030   6.13333333
## 248 Weekday     2035   4.31111111
## 249 Weekday     2040   6.35555556
## 250 Weekday     2045  10.26666667
## 251 Weekday     2050  21.66666667
## 252 Weekday     2055  14.62222222
## 253 Weekday     2100   9.24444444
## 254 Weekday     2105  16.60000000
## 255 Weekday     2110  25.37777778
## 256 Weekday     2115  16.37777778
## 257 Weekday     2120  12.62222222
## 258 Weekday     2125   6.97777778
## 259 Weekday     2130  10.84444444
## 260 Weekday     2135  14.33333333
## 261 Weekday     2140   5.97777778
## 262 Weekday     2145   6.55555556
## 263 Weekday     2150   7.17777778
## 264 Weekday     2155   3.08888889
## 265 Weekday     2200   1.33333333
## 266 Weekday     2205   3.93333333
## 267 Weekday     2210   5.66666667
## 268 Weekday     2215  10.02222222
## 269 Weekday     2220   8.33333333
## 270 Weekday     2225   9.68888889
## 271 Weekday     2230  11.48888889
## 272 Weekday     2235   2.60000000
## 273 Weekday     2240   0.00000000
## 274 Weekday     2245   0.13333333
## 275 Weekday     2250   1.68888889
## 276 Weekday     2255   1.40000000
## 277 Weekday     2300   3.11111111
## 278 Weekday     2305   3.35555556
## 279 Weekday     2310   0.00000000
## 280 Weekday     2315   0.97777778
## 281 Weekday     2320   1.13333333
## 282 Weekday     2325   1.66666667
## 283 Weekday     2330   2.68888889
## 284 Weekday     2335   1.62222222
## 285 Weekday     2340   1.80000000
## 286 Weekday     2345   0.17777778
## 287 Weekday     2350   0.26666667
## 288 Weekday     2355   1.26666667
## 289 Weekend        0   2.30034722
## 290 Weekend        5   2.29405382
## 291 Weekend       10   0.00000000
## 292 Weekend       15   0.00000000
## 293 Weekend       20   0.00000000
## 294 Weekend       25   3.25000000
## 295 Weekend       30   0.00000000
## 296 Weekend       35   0.00000000
## 297 Weekend       40   0.00000000
## 298 Weekend       45   0.37500000
## 299 Weekend       50   0.00000000
## 300 Weekend       55   0.43750000
## 301 Weekend      100   0.00000000
## 302 Weekend      105   2.25000000
## 303 Weekend      110   0.00000000
## 304 Weekend      115   0.00000000
## 305 Weekend      120   0.00000000
## 306 Weekend      125   0.00000000
## 307 Weekend      130   0.50000000
## 308 Weekend      135   0.56250000
## 309 Weekend      140   0.00000000
## 310 Weekend      145   0.68750000
## 311 Weekend      150   0.00000000
## 312 Weekend      155   0.00000000
## 313 Weekend      200   0.00000000
## 314 Weekend      205   0.00000000
## 315 Weekend      210   0.25000000
## 316 Weekend      215   0.00000000
## 317 Weekend      220   0.00000000
## 318 Weekend      225   0.00000000
## 319 Weekend      230   0.00000000
## 320 Weekend      235   0.00000000
## 321 Weekend      240   0.00000000
## 322 Weekend      245   0.00000000
## 323 Weekend      250   0.00000000
## 324 Weekend      255   0.00000000
## 325 Weekend      300   0.00000000
## 326 Weekend      305   0.00000000
## 327 Weekend      310   0.00000000
## 328 Weekend      315   0.00000000
## 329 Weekend      320   0.68750000
## 330 Weekend      325   0.00000000
## 331 Weekend      330   2.50000000
## 332 Weekend      335   0.68750000
## 333 Weekend      340   0.62500000
## 334 Weekend      345   0.00000000
## 335 Weekend      350   0.00000000
## 336 Weekend      355   0.00000000
## 337 Weekend      400   3.62500000
## 338 Weekend      405   0.00000000
## 339 Weekend      410   3.18750000
## 340 Weekend      415   0.00000000
## 341 Weekend      420   0.00000000
## 342 Weekend      425   1.18750000
## 343 Weekend      430   5.68750000
## 344 Weekend      435   1.81250000
## 345 Weekend      440   2.25000000
## 346 Weekend      445   0.56250000
## 347 Weekend      450   4.87500000
## 348 Weekend      455   2.06250000
## 349 Weekend      500   0.00000000
## 350 Weekend      505   0.00000000
## 351 Weekend      510   0.00000000
## 352 Weekend      515   2.12500000
## 353 Weekend      520   0.37500000
## 354 Weekend      525   3.31250000
## 355 Weekend      530   0.00000000
## 356 Weekend      535   0.00000000
## 357 Weekend      540   1.68750000
## 358 Weekend      545   1.12500000
## 359 Weekend      550   3.87500000
## 360 Weekend      555   5.81250000
## 361 Weekend      600   0.00000000
## 362 Weekend      605   0.00000000
## 363 Weekend      610   1.18750000
## 364 Weekend      615  17.00000000
## 365 Weekend      620   4.43750000
## 366 Weekend      625   4.75000000
## 367 Weekend      630   5.43750000
## 368 Weekend      635  10.12500000
## 369 Weekend      640   5.75000000
## 370 Weekend      645   8.56250000
## 371 Weekend      650   5.43750000
## 372 Weekend      655  10.87500000
## 373 Weekend      700  19.25000000
## 374 Weekend      705  20.68750000
## 375 Weekend      710  11.81250000
## 376 Weekend      715   6.00000000
## 377 Weekend      720   6.62500000
## 378 Weekend      725  21.75000000
## 379 Weekend      730  19.06250000
## 380 Weekend      735  10.56250000
## 381 Weekend      740  16.31250000
## 382 Weekend      745  21.93750000
## 383 Weekend      750  22.81250000
## 384 Weekend      755  19.81250000
## 385 Weekend      800  37.93750000
## 386 Weekend      805  49.12500000
## 387 Weekend      810  72.25000000
## 388 Weekend      815  69.06250000
## 389 Weekend      820  67.00000000
## 390 Weekend      825  56.62500000
## 391 Weekend      830  94.43750000
## 392 Weekend      835 112.31250000
## 393 Weekend      840 106.81250000
## 394 Weekend      845 140.00000000
## 395 Weekend      850 138.43750000
## 396 Weekend      855 117.81250000
## 397 Weekend      900  57.43750000
## 398 Weekend      905 103.62500000
## 399 Weekend      910 138.12500000
## 400 Weekend      915 153.12500000
## 401 Weekend      920  91.25000000
## 402 Weekend      925  93.81250000
## 403 Weekend      930  79.56250000
## 404 Weekend      935  65.93750000
## 405 Weekend      940  14.18750000
## 406 Weekend      945  28.00000000
## 407 Weekend      950  18.93750000
## 408 Weekend      955  28.06250000
## 409 Weekend     1000  43.06250000
## 410 Weekend     1005  48.25000000
## 411 Weekend     1010  46.50000000
## 412 Weekend     1015  59.68750000
## 413 Weekend     1020  58.18750000
## 414 Weekend     1025  88.43750000
## 415 Weekend     1030  70.12500000
## 416 Weekend     1035  69.75000000
## 417 Weekend     1040  61.81250000
## 418 Weekend     1045  31.62500000
## 419 Weekend     1050  30.56250000
## 420 Weekend     1055  52.37500000
## 421 Weekend     1100  54.62500000
## 422 Weekend     1105  38.87500000
## 423 Weekend     1110  45.75000000
## 424 Weekend     1115  48.43750000
## 425 Weekend     1120  36.62500000
## 426 Weekend     1125  30.87500000
## 427 Weekend     1130  31.12500000
## 428 Weekend     1135  43.12500000
## 429 Weekend     1140  29.68750000
## 430 Weekend     1145  29.68750000
## 431 Weekend     1150  28.81250000
## 432 Weekend     1155  60.37500000
## 433 Weekend     1200  78.81250000
## 434 Weekend     1205 118.50000000
## 435 Weekend     1210 114.50000000
## 436 Weekend     1215 130.37500000
## 437 Weekend     1220  96.75000000
## 438 Weekend     1225  53.31250000
## 439 Weekend     1230  24.87500000
## 440 Weekend     1235  33.06250000
## 441 Weekend     1240  36.00000000
## 442 Weekend     1245  56.68750000
## 443 Weekend     1250  73.93750000
## 444 Weekend     1255  88.93750000
## 445 Weekend     1300  86.93750000
## 446 Weekend     1305  74.68750000
## 447 Weekend     1310  90.43750000
## 448 Weekend     1315 107.12500000
## 449 Weekend     1320  70.31250000
## 450 Weekend     1325  81.93750000
## 451 Weekend     1330  68.31250000
## 452 Weekend     1335  27.12500000
## 453 Weekend     1340  76.37500000
## 454 Weekend     1345  84.43750000
## 455 Weekend     1350 102.56250000
## 456 Weekend     1355 122.06250000
## 457 Weekend     1400  73.62500000
## 458 Weekend     1405  80.37500000
## 459 Weekend     1410  70.37500000
## 460 Weekend     1415  52.87500000
## 461 Weekend     1420  53.50000000
## 462 Weekend     1425  51.93750000
## 463 Weekend     1430  65.75000000
## 464 Weekend     1435  60.62500000
## 465 Weekend     1440  30.62500000
## 466 Weekend     1445  34.31250000
## 467 Weekend     1450  43.12500000
## 468 Weekend     1455  53.75000000
## 469 Weekend     1500  23.87500000
## 470 Weekend     1505  34.43750000
## 471 Weekend     1510  46.62500000
## 472 Weekend     1515  53.50000000
## 473 Weekend     1520  57.37500000
## 474 Weekend     1525  71.06250000
## 475 Weekend     1530  59.00000000
## 476 Weekend     1535  97.62500000
## 477 Weekend     1540  51.00000000
## 478 Weekend     1545  94.18750000
## 479 Weekend     1550 112.31250000
## 480 Weekend     1555 111.87500000
## 481 Weekend     1600  97.25000000
## 482 Weekend     1605 109.37500000
## 483 Weekend     1610 115.68750000
## 484 Weekend     1615 131.31250000
## 485 Weekend     1620 134.43750000
## 486 Weekend     1625 137.37500000
## 487 Weekend     1630  98.43750000
## 488 Weekend     1635  80.81250000
## 489 Weekend     1640  91.93750000
## 490 Weekend     1645  77.62500000
## 491 Weekend     1650  92.68750000
## 492 Weekend     1655  69.87500000
## 493 Weekend     1700 105.62500000
## 494 Weekend     1705  81.18750000
## 495 Weekend     1710  90.87500000
## 496 Weekend     1715  90.56250000
## 497 Weekend     1720  99.06250000
## 498 Weekend     1725  87.56250000
## 499 Weekend     1730  96.31250000
## 500 Weekend     1735  34.87500000
## 501 Weekend     1740  43.81250000
## 502 Weekend     1745  41.50000000
## 503 Weekend     1750  31.18750000
## 504 Weekend     1755  32.37500000
## 505 Weekend     1800  75.06250000
## 506 Weekend     1805  82.81250000
## 507 Weekend     1810  86.37500000
## 508 Weekend     1815  82.18750000
## 509 Weekend     1820  45.87500000
## 510 Weekend     1825  43.31250000
## 511 Weekend     1830  63.68750000
## 512 Weekend     1835  44.56250000
## 513 Weekend     1840  56.75000000
## 514 Weekend     1845  42.00000000
## 515 Weekend     1850  34.37500000
## 516 Weekend     1855  60.87500000
## 517 Weekend     1900  66.68750000
## 518 Weekend     1905  69.81250000
## 519 Weekend     1910  38.56250000
## 520 Weekend     1915  43.81250000
## 521 Weekend     1920  27.37500000
## 522 Weekend     1925  18.56250000
## 523 Weekend     1930  19.18750000
## 524 Weekend     1935  18.25000000
## 525 Weekend     1940  26.87500000
## 526 Weekend     1945  41.93750000
## 527 Weekend     1950  43.87500000
## 528 Weekend     1955  46.87500000
## 529 Weekend     2000  34.68750000
## 530 Weekend     2005  54.50000000
## 531 Weekend     2010  52.12500000
## 532 Weekend     2015  83.25000000
## 533 Weekend     2020  74.37500000
## 534 Weekend     2025  62.00000000
## 535 Weekend     2030  73.18750000
## 536 Weekend     2035  58.56250000
## 537 Weekend     2040  46.87500000
## 538 Weekend     2045  41.75000000
## 539 Weekend     2050  46.06250000
## 540 Weekend     2055  25.62500000
## 541 Weekend     2100  26.81250000
## 542 Weekend     2105  10.37500000
## 543 Weekend     2110   6.31250000
## 544 Weekend     2115  17.68750000
## 545 Weekend     2120   5.75000000
## 546 Weekend     2125   6.93750000
## 547 Weekend     2130  18.06250000
## 548 Weekend     2135  13.68750000
## 549 Weekend     2140  11.93750000
## 550 Weekend     2145   7.37500000
## 551 Weekend     2150   6.75000000
## 552 Weekend     2155   0.00000000
## 553 Weekend     2200   1.06250000
## 554 Weekend     2205   1.12500000
## 555 Weekend     2210   0.00000000
## 556 Weekend     2215   0.00000000
## 557 Weekend     2220   0.00000000
## 558 Weekend     2225   1.56250000
## 559 Weekend     2230   0.00000000
## 560 Weekend     2235   0.00000000
## 561 Weekend     2240   1.06250000
## 562 Weekend     2245   0.00000000
## 563 Weekend     2250   0.56250000
## 564 Weekend     2255  11.31250000
## 565 Weekend     2300   2.18750000
## 566 Weekend     2305   0.00000000
## 567 Weekend     2310   0.00000000
## 568 Weekend     2315   0.00000000
## 569 Weekend     2320   0.00000000
## 570 Weekend     2325   0.56250000
## 571 Weekend     2330   1.06250000
## 572 Weekend     2335  11.00000000
## 573 Weekend     2340   5.87500000
## 574 Weekend     2345   1.62500000
## 575 Weekend     2350   0.00000000
## 576 Weekend     2355   0.00000000
```


```r
ggplot(my5interval2, aes(interval, steps)) + geom_line() + facet_grid(weekend ~ .)
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20-1.png) 

We can see that there is a small difference where apparently, the total number of steps is a bit higher in the weekends.
