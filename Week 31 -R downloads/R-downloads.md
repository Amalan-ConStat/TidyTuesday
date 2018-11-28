R downloads
================
M.amalan
November 6, 2018

Introduction
============

[Tidy Tuesday](https://github.com/rfordatascience/tidytuesday) is a very good move to improve R programming for anyone who is interested in statistics. Datasets are uploaded every tuesday, and plots are published under the [\#tidytuesday](https://twitter.com/hashtag/TidyTuesday?src=hash). This is just me presenting a few accumulated plots for the dataset [r-downloads.csv](https://github.com/rfordatascience/tidytuesday/blob/master/data/2018-10-30/r-downloads.csv) and [r\_downloads\_year.csv](https://github.com/rfordatascience/tidytuesday/blob/master/data/2018-10-30/r_downloads_year.csv).

I shall be focusing on the data set provided on 2018 October 30th, Which is [R and Package download stats](https://github.com/rfordatascience/tidytuesday/tree/master/data/2018-10-30) My main objective is to understand how Sri Lankan users have behaved in in this dataset.

The packages used in R are ggplot2, lubridate, ggthemr, gridExtra, magrittr, knitr and kableExtra.

There are 701 downloads occured in between the given time limit of 2017 october 20th to 2018 october 20th in Sri Lanka. Similarly, if we look at the downloads on the day of 2018 October 23rd, which is 3. There are 7 variables to be concerend, which are

-   date - date of download (y-m-d)
-   time - time of download (in UTC)
-   size - size in bytes
-   version - R release version
-   os - Operating System
-   country - Two letter ISO country code
-   ip\_id - Anonymized dialy ip code(unique identifier)

``` r
# extracting the observations only if the country is Sri Lanka
r_downloads_year_LK<-subset.data.frame(r_downloads_year,country=="LK")
r_downloads_LK<-subset.data.frame(r_downloads,country=="LK")

# number of observations 
dim(r_downloads_year_LK)
```

    ## [1] 701   7

``` r
dim(r_downloads_LK)
```

    ## [1] 3 7

Operating Systems
=================

Windows is not a favourable operating system for open source programming was my myth. Well, No longer I shall believe that if it is considering Sri Lankans and R programming.

``` r
#checking what type of operating systems are in use
ggthemr("flat dark")
g1<-ggplot(r_downloads_year_LK,aes(x=os))+geom_bar()+
    geom_text(stat='count', aes(label=..count..), vjust=-0.5)+
    xlab("Operating System")+ylab("Frequency")+
    scale_y_continuous(breaks=seq(0,675,by=25))+
    ggtitle("Operating system preference of Sri lankans for R",
              subtitle = "2017 October 20th - 2018 October 20th")
      
g1
```

![](R-downloads_files/figure-markdown_github/Operating%20Systems-1.png)

``` r
tab1<-round(prop.table(table(r_downloads_year_LK$os)),4)
tab1<-as.data.frame(tab1)
names(tab1)<-c("Operating System","Frequency")
#kable(tab1) %>% 
#  kable_styling(bootstrap_options = "striped", full_width = F,position="left")
```

Majority of users have used Windows which is 93.58%, while Mac users are represented with 3.71% and finally 2.71% from the source file. Next, focusing on the R versions downloaded.

R versions
==========

Versions are updated regularly for R and a grand update occured on 2018 April for the version 3.5.0. Further, versions 3.4.3 and 3.4.4 were updated in the time gap considered. There are versions from 3.0.0 and higher for Sri Lankans. It is crucial to study this where we can understand how far does the user have knowledge about R and updating the version.

``` r
#checking what type of R versions were downloaded
g2<-ggplot(r_downloads_year_LK,aes(x=version))+geom_bar()+
    geom_text(stat='count', aes(label=..count..), vjust=-0.5)+
    xlab("R versions")+ylab("Frequency")+
    scale_y_continuous(breaks = seq(0,275,by=25))+
    ggtitle("R versions downloaded of Sri Lankans for R",
            subtitle = "2017 October 20th - 2018 October 20th")

g2
```

![](R-downloads_files/figure-markdown_github/R%20versions-1.png) Table shows that version 3.5.1 represents a 36.52% followed by version 3.4.3 of 27.96% and in the third place version 3.5.0 with 15.98%. Further, all downloads are occured for versions 3.0.0 or higher than it. People believed in 3.4.3 than 3.5.0, which could only mean that 3.4.3 was more stable for users requirements.

``` r
tab2<-sort(round(prop.table(table(r_downloads_year_LK$version)),4))
tab2<-as.data.frame(tab2)
names(tab2)<-c("R Version","Frequency")
cat("\n")
```

``` r
#kable(tab2) %>% 
#  kable_styling(bootstrap_options = "striped", full_width = F,position="left")
```

If we further divide the operating systems bar plot with respective to R version it is clearly seen that only versions 3.5.1, 3.5.0, 3.4.4, 3.4.3 and 3.4.2 have maintained importance for the windows operating system.

``` r
# Checking what type of operating system is used with R version
set_swatch(c("white","firebrick1","gold","darkorange","dodgerblue","darkblue",
             "forestgreen","green","grey","grey44","black"))
g1_R<-ggplot(r_downloads_year_LK,aes(x=os,fill=version))+geom_bar()+
    geom_text(stat='count', aes(y=..count..,label=..count..), position="stack",vjust=1)+
    xlab("Operating System")+ylab("Frequency")+
    scale_y_continuous(breaks=seq(0,675,by=25))+
    ggtitle("Operating system preference of Sri lankans for R",
              subtitle = "2017 October 20th - 2018 October 20th")
      
g1_R
```

![](R-downloads_files/figure-markdown_github/R%20versions%203-1.png)

``` r
cat("Contingency table for R version vs Operating System\n")
```

    ## Contingency table for R version vs Operating System

``` r
#kable(round(prop.table(table(r_downloads_year_LK$os,r_downloads_year_LK$version)),4))
```

Windows operating percentages indicate that 34.66% of users have choosen version 3.5.1, 26.53% have choosen version 3.4.3 and finally version 3.5.0 with 15.12%. Close to 9% is represented by version 3.4.2 and 3.4.4.

Date versus Operating System
============================

Date is a difficult variable in statistics therefore I have dissemininated the date into 4 types, which are month(January to December), day(1-31), hour(0-23) and minutes(0-59). Further, I have tried to understand what type of operating systems were used in the time perspective.

``` r
#checking which day the downloads occured of the months inrespecitive to operating system
ggthemr("flat dark")
g3<-ggplot(r_downloads_year_LK,aes(x=month(date),fill=os))+
    geom_bar()+ xlab("Months") +ylab("Frequency")+
    scale_y_continuous(breaks =seq(0,140,10))+
    scale_x_continuous(breaks=1:12) +
    ggtitle("Operating systems used in the months of R download",
              subtitle = "2017 October 20th - 2018 October 20th")
g3
```

![](R-downloads_files/figure-markdown_github/Date%20versus%20Operating%20system%201-1.png)

So, the first sub part of time is months. Here, we are considering 12 months of an year and months August and April reflects a no OS better than Windows property. While, August holding the least amount of downloads with slightly above frequency 20. Highest percentage occurs to October with count higher than 130 and significantly osx and src types of files also have higher amount than anyother month. Except this only the month of december has counts higher than 100.

``` r
#checking which day the downloads occured of the week inrespecitive to operating system
g4<-ggplot(r_downloads_year_LK,aes(x=day(date),fill=os))+
    geom_bar()+xlab("Days") +ylab("Frequency")+
    scale_y_continuous(breaks =seq(0,80,10))+
    scale_x_continuous(breaks=1:31)+
    ggtitle("Operating systems used in the days of R download",
              subtitle = "2017 October 20th - 2018 October 20th")
g4
```

![](R-downloads_files/figure-markdown_github/Date%20versus%20Operating%20system%202-1.png)

Next, focusing on the days it is clear that 10th and 11th have most downloads respectively reaching more than 60 and 70 in counts, while in other days it mostly less than 30%. Further, clearly on the 31st it includes less than frequency of 10 because 31st is not a part of all 12 months. It would be very tiring to focus on operating systems individually, but to be fair there is clear sign of few days with only the use of windows, and a few days with combination of other operating systems with windows.

``` r
#checking which hour the downloads occured of the day inrespecitive to operating system
g5<-ggplot(r_downloads_year_LK,aes(x=hour(time),fill=os))+
    geom_bar()+xlab("Hour") +ylab("Frequency")+
    scale_y_continuous(breaks = seq(0,100,5))+
    scale_x_continuous(breaks=0:23)+
    ggtitle("Operating systems used in the Hours of the day of R download",
              subtitle = "2017 October 20th - 2018 October 20th")
g5
```

![](R-downloads_files/figure-markdown_github/Date%20versus%20Operating%20system%203-1.png)

I have been curious at this part because of wanting to know at which hour of the day did our Sri Lankan users download R. Yet it should be noted that the hour of time could be local time of Sri Lanka or otherwise. Still according to the bar chart the hours 4 and 5 have most downloads with counts of above 80 and above 90 respectively. where in the 21st hour it reaches a very low amount of less than 5 counts. Most of the frequencies are in the range of 10 and 50.

``` r
#checking which minute the downloads occured of the day inrespecitive to operating system
g6<-ggplot(r_downloads_year_LK,aes(x=minute(time),fill=os))+
    geom_bar()+xlab("Minute") +ylab("Frequency")+
    scale_y_continuous(breaks = 0:25)+
    scale_x_continuous(breaks=0:59)+
    ggtitle("Operating systems used in the minutes of the day of R download",
              subtitle = "2017 October 20th - 2018 October 20th")+
    coord_flip()
g6
```

![](R-downloads_files/figure-markdown_github/Date%20versus%20Operating%20system2-1.png)

Looking at the minutes it is very spread out. Focusing on special occasions only four minutes which are 59, 46, 12 and 0 have counts more than 20. While 51st minute has a count of 2. Rather than this nothing more significant occurs here. I think considering these counts in perspective of specific operating systems is tedious amount of work and waste of time.

Download Size and IP ID
=======================

Packages were downloaded but none of their names were given in this dataset. Therefore we cannot know which package was downloaded. Yet we can identify the package sizes which were downloaded most. According to the table an R package with 82375220 bytes has most downloaded of 50, while second place goest to a size of 82375219 bytes and finally in third place is for 82375216 bytes with 39 counts.

``` r
tab3<-as.data.frame(sort(table(r_downloads_year_LK$size))%>% tail(5))
names(tab3)<-c("Size","Frequency")
#kable(tab3)  %>% 
#  kable_styling(bootstrap_options = "striped", full_width = F,position="left")
```

Looking at the IP ID it is clear that 334 has the highest downloads of 55, while second place goes to 1060 with 46 downloads. Finally, ID nunber 1286 has 16 downloads with third place.

``` r
tab4<-as.data.frame(sort(table(r_downloads_year_LK$ip_id))%>% tail(5))
names(tab4)<-c("IP ID","Frequency")
#kable(tab4)  %>% 
#  kable_styling(bootstrap_options = "striped", full_width = F,position="left")
```

Conclusion
==========

I shall conclude my findings in point form

-   Most of Sri lankans (93.58%) use windows as a OS for R downloads

-   Top three R versions are 3.5.1, 3.4.3 and 3.5.0 with percentages respectively 36.52% 27.96% and 15.98%.

-   Windows users use versions 3.5.1, 3.4.3 and 3.5.0 with percentages 34.56%, 26.53% and 3.5.0.

-   Most of the downloads occur in the months October and December, while days are 10th and 11th, while hours are 3rd and 4th and minutes of 59th, 46th, 12th and 0th.

-   Download size of 82375220 bytes happens with the highest count of 50, while the IP ID of 334 has most downloads of 55.
