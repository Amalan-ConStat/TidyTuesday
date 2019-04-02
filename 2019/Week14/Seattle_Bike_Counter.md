Seattle Bike Counter
================
M.Amalan
April 1, 2019

``` r
library(tidyverse)
library(dplyr)
library(gganimate)
library(ggthemr)
library(splitstackshape)
library(lubridate)
library(readr)

bike_traffic <- read_csv("bike_traffic.csv")

#bike_traffic <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")
```

Crossing vs Direction with Morning or Evening for Everyday
==========================================================

``` r
p<-bike_traffic %>%
      cSplit("date",sep=" ") %>%
      rename("DMY"="date_1") %>%
      rename("HMS"="date_2") %>%
      rename("MorE"="date_3") %>%
      mutate(DMY=mdy(DMY)) %>%
      mutate(HMS=hms(HMS)) %>%
      select(DMY,MorE,direction,crossing,bike_count) %>%
ggplot(.,aes(x=direction,y=str_wrap(crossing,15),
             color=MorE,size=bike_count))+
      geom_jitter()+
      xlab("Direction")+ylab("Crossing")+
      transition_time(DMY)+ease_aes("linear")+
      ggtitle("Crossing and Direction for Bike Count",
              subtitle = "Date: {frame_time}")

animate(p,nframes=1899,fps=1)      
```

![](Seattle_Bike_Counter_files/figure-markdown_github/crossing%20vs%20direction%20with%20M%20or%20E%20and%20bikecount-1.gif)

Averag Bike Count in Different Time Laps
========================================

Monthly Average Bike Count for Everyday with AM or PM
-----------------------------------------------------

``` r
p<-bike_traffic %>%
      cSplit("date",sep=" ") %>%
      rename("DMY"="date_1") %>%
      rename("HMS"="date_2") %>%
      rename("MorE"="date_3") %>%
      mutate(DMY=mdy(DMY)) %>%
      mutate(Year=year(DMY)) %>%
      mutate(Month=month(DMY)) %>%
      mutate(Day=day(DMY)) %>%
      group_by(MorE,Year,Month,Day) %>%
      summarise(Average=mean(bike_count,na.rm = TRUE)) %>%
ggplot(.,aes(y=Average,x=factor(Month),color=MorE))+
      geom_jitter()+transition_time(Year)+
      ease_aes("linear")+
      scale_y_continuous(labels=seq(0,175,5),breaks=seq(0,175,5))+
      xlab("Month")+ylab("Average Bike Count")+
      ggtitle("Average Bike Count changing with Time",
              subtitle = "Year : {frame_time}")
  
animate(p,nframes=7,fps=1)    
```

![](Seattle_Bike_Counter_files/figure-markdown_github/Monthly%20and%20Dayly%20Average%20Bike%20Count%20by%20M%20or%20E-1.gif)

Day by Average Bike Count for Everyday with AM or PM
----------------------------------------------------

``` r
p<-bike_traffic %>%
      cSplit("date",sep=" ") %>%
      rename("DMY"="date_1") %>%
      rename("HMS"="date_2") %>%
      rename("MorE"="date_3") %>%
      mutate(DMY=mdy(DMY)) %>%
      mutate(HMS=hms(HMS)) %>%
      mutate(Day=day(DMY)) %>%
      mutate(Year=year(DMY)) %>%
      mutate(Month=month(DMY)) %>%
      group_by(MorE,Year,Month,Day) %>%
      summarise(Average=mean(bike_count,na.rm = TRUE)) %>%
ggplot(.,aes(y=Average,x=factor(Day),color=MorE))+
      geom_jitter()+transition_time(Year)+
      ease_aes("linear")+coord_flip()+
      scale_y_continuous(labels=seq(0,170,10),breaks=seq(0,170,10))+
      xlab("Day")+ylab("Average Bike Count")+
      ggtitle("Average Bike Count changing with Time",
              subtitle = "Year : {frame_time}")
  
animate(p,nframes=7,fps=1)    
```

![](Seattle_Bike_Counter_files/figure-markdown_github/Day%20and%20Year%20Averge%20Bike%20Count%20by%20M%20or%20E-1.gif)

Hourly Average Bike Count for Every Month with AM or PM
-------------------------------------------------------

``` r
p<-bike_traffic %>%
      cSplit("date",sep=" ") %>%
      rename("DMY"="date_1") %>%
      rename("HMS"="date_2") %>%
      rename("MorE"="date_3") %>%
      mutate(DMY=mdy(DMY)) %>%
      mutate(HMS=hms(HMS)) %>%
      mutate(Hour=hour(HMS)) %>%
      mutate(Year=year(DMY)) %>%
      mutate(Month=month(DMY)) %>%
      group_by(MorE,Year,Month,Hour) %>%
      summarise(Average=mean(bike_count,na.rm = TRUE)) %>%
ggplot(.,aes(y=Average,x=factor(Hour),color=MorE))+
      geom_jitter()+transition_time(Year)+
      ease_aes("linear")+
      xlab("Hour")+ylab("Average Bike Count")+
      scale_y_continuous(labels=seq(0,55,5),breaks=seq(0,55,5))+
      ggtitle("Average Bike Count changing with Time",
              subtitle = "Year : {frame_time}")
  
animate(p,nframes=7,fps=1)    
```

![](Seattle_Bike_Counter_files/figure-markdown_github/Hourly%20and%20Year%20Average%20Bike%20Count%20by%20M%20or%20E-1.gif)

Pedestrian Count With Different Time Laps
=========================================

Monthly Pedestrian Count for Everyday with AM or PM when TRUE
-------------------------------------------------------------

``` r
p<-bike_traffic %>%
      cSplit("date",sep=" ") %>%
      rename("DMY"="date_1") %>%
      rename("HMS"="date_2") %>%
      rename("MorE"="date_3") %>%
      mutate(DMY=mdy(DMY)) %>%
      mutate(Year=year(DMY)) %>%
      mutate(Month=month(DMY)) %>%
      mutate(Day=day(DMY)) %>%
      group_by(MorE,Year,Month,Day) %>%
      subset(ped_count==TRUE) %>%
      summarise(Average=summary.factor(ped_count)) %>%
ggplot(.,aes(y=Average,x=factor(Month),color=MorE))+
      geom_jitter()+transition_time(Year)+
      ease_aes("linear")+
      xlab("Month")+ylab("Pedestrian Count")+
      scale_y_continuous(labels=seq(0,22.5,2.5),breaks=seq(0,22.5,2.5))+
      ggtitle("Pedestrian Count changing with Time",
              subtitle = "Year : {frame_time}")
  
animate(p,nframes=7,fps=1)    
```

![](Seattle_Bike_Counter_files/figure-markdown_github/Monthly%20and%20Daily%20Pedestrian%20Count%20by%20M%20or%20E%20when%20TRUE-1.gif)

Monthly Pedestrian Count for Everyday with AM or PM when FALSE
--------------------------------------------------------------

``` r
p<-bike_traffic %>%
      cSplit("date",sep=" ") %>%
      rename("DMY"="date_1") %>%
      rename("HMS"="date_2") %>%
      rename("MorE"="date_3") %>%
      mutate(DMY=mdy(DMY)) %>%
      mutate(Year=year(DMY)) %>%
      mutate(Month=month(DMY)) %>%
      mutate(Day=day(DMY)) %>%
      group_by(MorE,Year,Month,Day) %>%
      subset(ped_count==FALSE) %>%
      summarise(Average=summary.factor(ped_count)) %>%
ggplot(.,aes(y=Average,x=factor(Month),color=MorE))+
      geom_jitter()+transition_time(Year)+
      ease_aes("linear")+
      xlab("Month")+ylab("Pedestrian Count")+
      scale_y_continuous(labels=seq(0,75,5),breaks=seq(0,75,5))+
      ggtitle("Pedestrian Count changing with Time",
              subtitle = "Year : {frame_time}")
  
animate(p,nframes=7,fps=1)    
```

![](Seattle_Bike_Counter_files/figure-markdown_github/Monthly%20and%20Daily%20Pedestrian%20Count%20by%20M%20or%20E%20when%20FALSE-1.gif)

Day by Pedestrian Count for Everyday with AM or PM when TRUE
------------------------------------------------------------

``` r
p<-bike_traffic %>%
      cSplit("date",sep=" ") %>%
      rename("DMY"="date_1") %>%
      rename("HMS"="date_2") %>%
      rename("MorE"="date_3") %>%
      mutate(DMY=mdy(DMY)) %>%
      mutate(HMS=hms(HMS)) %>%
      mutate(Day=day(DMY)) %>%
      mutate(Year=year(DMY)) %>%
      mutate(Month=month(DMY)) %>%
      group_by(MorE,Year,Month,Day) %>%
      subset(ped_count==TRUE) %>%
      summarise(Average=summary.factor(ped_count)) %>%
ggplot(.,aes(y=Average,x=factor(Day),color=MorE))+
      geom_jitter()+transition_time(Year)+
      ease_aes("linear")+coord_flip()+
      xlab("Day")+ylab("Pedestrian Count")+
      scale_y_continuous(labels=seq(0,22.5,2.5),breaks=seq(0,22.5,2.5))+
      ggtitle("Pedestrian Count changing with Time",
              subtitle = "Year : {frame_time}")
  
animate(p,nframes=7,fps=1)    
```

![](Seattle_Bike_Counter_files/figure-markdown_github/Day%20and%20Year%20Pedestrian%20Count%20by%20M%20or%20E%20when%20TRUE-1.gif)

Day by Pedestrian Count for Everyday with AM or PM when FALSE
-------------------------------------------------------------

``` r
p<-bike_traffic %>%
      cSplit("date",sep=" ") %>%
      rename("DMY"="date_1") %>%
      rename("HMS"="date_2") %>%
      rename("MorE"="date_3") %>%
      mutate(DMY=mdy(DMY)) %>%
      mutate(HMS=hms(HMS)) %>%
      mutate(Day=day(DMY)) %>%
      mutate(Year=year(DMY)) %>%
      mutate(Month=month(DMY)) %>%
      group_by(MorE,Year,Month,Day) %>%
      subset(ped_count==FALSE) %>%
      summarise(Average=summary.factor(ped_count)) %>%
ggplot(.,aes(y=Average,x=factor(Day),color=MorE))+
      geom_jitter()+transition_time(Year)+
      ease_aes("linear")+coord_flip()+
      xlab("Day")+ylab("Pedestrian Count")+
      scale_y_continuous(labels=seq(0,70,5),breaks=seq(0,70,5))+
      ggtitle("Pedestrian Count changing with Time",
              subtitle = "Year : {frame_time}")
  
animate(p,nframes=7,fps=1)    
```

![](Seattle_Bike_Counter_files/figure-markdown_github/Day%20and%20Year%20Pedestrian%20Count%20by%20M%20or%20E%20when%20FALSE-1.gif)

Hourly Pedestrian Count for Every Month with AM or PM when TRUE
---------------------------------------------------------------

``` r
p<-bike_traffic %>%
      cSplit("date",sep=" ") %>%
      rename("DMY"="date_1") %>%
      rename("HMS"="date_2") %>%
      rename("MorE"="date_3") %>%
      mutate(DMY=mdy(DMY)) %>%
      mutate(HMS=hms(HMS)) %>%
      mutate(Hour=hour(HMS)) %>%
      mutate(Year=year(DMY)) %>%
      mutate(Month=month(DMY)) %>%
      group_by(MorE,Year,Month,Hour) %>%
      subset(ped_count==TRUE) %>%
      summarise(Average=summary.factor(ped_count)) %>%
ggplot(.,aes(y=Average,x=factor(Hour),color=MorE))+
      geom_jitter()+transition_time(Year)+
      ease_aes("linear")+
      xlab("Hour")+ylab("Pedestrian Count")+
      scale_y_continuous(labels=seq(0,60,5),breaks=seq(0,60,5))+
      ggtitle("Pedestrian Count changing with Time",
              subtitle = "Year : {frame_time}")
  
animate(p,nframes=7,fps=1)    
```

![](Seattle_Bike_Counter_files/figure-markdown_github/Hourly%20and%20Year%20Pedestrian%20Count%20by%20M%20or%20E%20when%20TRUE-1.gif)

Hourly Pedestrian Count for Every Month with AM or PM when FALSE
----------------------------------------------------------------

``` r
p<-bike_traffic %>%
      cSplit("date",sep=" ") %>%
      rename("DMY"="date_1") %>%
      rename("HMS"="date_2") %>%
      rename("MorE"="date_3") %>%
      mutate(DMY=mdy(DMY)) %>%
      mutate(HMS=hms(HMS)) %>%
      mutate(Hour=hour(HMS)) %>%
      mutate(Year=year(DMY)) %>%
      mutate(Month=month(DMY)) %>%
      group_by(MorE,Year,Month,Hour) %>%
      subset(ped_count==TRUE) %>%
      summarise(Average=summary.factor(ped_count)) %>%
ggplot(.,aes(y=Average,x=factor(Hour),color=MorE))+
      geom_jitter()+transition_time(Year)+
      ease_aes("linear")+
      xlab("Hour")+ylab("Pedestrian Count")+
      scale_y_continuous(labels=seq(0,60,5),breaks=seq(0,60,5))+
      ggtitle("Pedestrian Count changing with Time",
              subtitle = "Year : {frame_time}")
  
animate(p,nframes=7,fps=1)    
```

![](Seattle_Bike_Counter_files/figure-markdown_github/Hourly%20and%20Year%20Pedestrian%20Count%20by%20M%20or%20E%20when%20FALSE-1.gif)

Average Bike Count with Crossings
=================================

``` r
p<-bike_traffic %>%
      cSplit("date",sep=" ") %>%
      rename("DMY"="date_1") %>%
      rename("HMS"="date_2") %>%
      rename("MorE"="date_3") %>%
      mutate(DMY=mdy(DMY)) %>%
      mutate(HMS=hms(HMS)) %>%
      mutate(Year=year(DMY)) %>%
      mutate(Month=month(DMY)) %>%
      group_by(MorE,Year,Month,crossing) %>%
      summarise(Average=mean(bike_count)) %>%
ggplot(.,aes(y=Average,x=str_wrap(crossing,8),
             color=Month,shape=MorE))+
      geom_jitter()+transition_time(Year)+
      ease_aes("linear")+
      xlab("Crossing")+ylab("Average Bike Count")+
      ggtitle("Average Bike Count changing with Time",
              subtitle = "Year : {frame_time}")
  
animate(p,nframes=7,fps=1)    
```

![](Seattle_Bike_Counter_files/figure-markdown_github/Monthly%20and%20Yearly%20Average%20Bike%20Count%20by%20M%20or%20E%20for%20Crossings-1.gif)

Average Bike Count with Directions
==================================

``` r
p<-bike_traffic %>%
      cSplit("date",sep=" ") %>%
      rename("DMY"="date_1") %>%
      rename("HMS"="date_2") %>%
      rename("MorE"="date_3") %>%
      mutate(DMY=mdy(DMY)) %>%
      mutate(HMS=hms(HMS)) %>%
      mutate(Year=year(DMY)) %>%
      mutate(Month=month(DMY)) %>%
      group_by(MorE,Year,Month,direction) %>%
      summarise(Average=mean(bike_count)) %>%
ggplot(.,aes(y=Average,x=str_wrap(direction,10),
             color=Month,shape=MorE))+
      geom_jitter()+transition_time(Year)+
      ease_aes("linear")+
      xlab("Direction")+ylab("Average Bike Count")+
      ggtitle("Average Bike Count changing with Time",
              subtitle = "Year : {frame_time}")
  
animate(p,nframes=7,fps=1)    
```

![](Seattle_Bike_Counter_files/figure-markdown_github/Monthly%20and%20Yearly%20Average%20Bike%20Count%20by%20M%20or%20E%20for%20direction-1.gif)

Average Bike Count with Crossings by Day
========================================

``` r
p<-bike_traffic %>%
      cSplit("date",sep=" ") %>%
      rename("DMY"="date_1") %>%
      rename("HMS"="date_2") %>%
      rename("MorE"="date_3") %>%
      mutate(DMY=mdy(DMY)) %>%
      mutate(HMS=hms(HMS)) %>%
      mutate(Year=year(DMY)) %>%
      mutate(Month=month(DMY)) %>%
      mutate(Wday=wday(DMY,label=TRUE)) %>%
      group_by(Year,Month,Wday,crossing) %>%
      summarise(Average=mean(bike_count)) %>%
ggplot(.,aes(y=Average,x=Wday,color=Month))+
      geom_jitter()+transition_time(Year)+
      theme(legend.position = "bottom",axis.text.x = element_text(hjust=1,angle = 45))+
      ease_aes("linear")+facet_grid(~str_wrap(crossing,10))+
      xlab("Day of the week")+ylab("Average Bike Count")+
      ggtitle("Average Bike Count changing with Time",
              subtitle = "Year : {frame_time}")
  
animate(p,nframes=7,fps=1)    
```

![](Seattle_Bike_Counter_files/figure-markdown_github/Monthly%20and%20Yearly%20Average%20Bike%20Count%20by%20M%20or%20E%20with%20days%20for%20Crossings-1.gif)

Average Bike Count with Directions by Day
=========================================

``` r
p<-bike_traffic %>%
      cSplit("date",sep=" ") %>%
      rename("DMY"="date_1") %>%
      rename("HMS"="date_2") %>%
      rename("MorE"="date_3") %>%
      mutate(DMY=mdy(DMY)) %>%
      mutate(HMS=hms(HMS)) %>%
      mutate(Year=year(DMY)) %>%
      mutate(Month=month(DMY)) %>%
      mutate(Wday=wday(DMY,label=TRUE)) %>%
      group_by(Year,Month,Wday,direction) %>%
      summarise(Average=mean(bike_count)) %>%
ggplot(.,aes(y=Average,x=Wday,color=Month))+
      geom_jitter()+transition_time(Year)+
      theme(legend.position = "bottom")+
      ease_aes("linear")+facet_grid(~direction)+
      xlab("Day of the Week")+ylab("Average Bike Count")+
      ggtitle("Average Bike Count changing with Time",
              subtitle = "Year : {frame_time}")
  
animate(p,nframes=7,fps=1)    
```

![](Seattle_Bike_Counter_files/figure-markdown_github/Monthly%20and%20Yearly%20Average%20Bike%20Count%20by%20M%20or%20E%20with%20days%20for%20Direction-1.gif)

*THANK YOU*
