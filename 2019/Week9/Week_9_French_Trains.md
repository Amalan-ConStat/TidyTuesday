Week 9 French Trains
================
M.Amalan
February 26, 2019

``` r
library(readr)
library(tidyverse)
library(gganimate)
library(ggalluvial)
library(geomnet)
library(ggthemr)

ggthemr("fresh")

full_trains <- read_csv("full_trains.csv")
small_trains <- read_csv("small_trains.csv")
```

Network Graph for the French City Trains
========================================

``` r
ggplot(small_trains,aes(from_id=departure_station,to_id=arrival_station))+
          geom_net(directed = TRUE,labelon = TRUE,size=0.5,labelcolour = "black",
                   repel = FALSE,ecolour = "grey70", arrowsize = 0.75,
                   linewidth = 0.5,layout.alg = "fruchtermanreingold")+
          theme_net()+
          ggtitle("Network Graph Showing from City to City of French Trains")
```

![](Week_9_French_Trains_files/figure-markdown_github/network%20graph-1.png)

Paris Montparnasse
==================

Chosen City with Total Number of Trips
======================================

``` r
p<-ggplot(subset(small_trains,departure_station=="PARIS MONTPARNASSE"),
       aes(x=str_wrap(arrival_station,20),y=total_num_trips,color=month))+
       geom_jitter()+coord_flip()+ labs(color="Month")+
       transition_time(year)+ease_aes("linear")+
       scale_y_continuous(breaks = seq(0,800,100),labels=seq(0,800,100))+
       ylab("Arrival Station")+xlab("Total Number of Trips")+
       ggtitle("Paris Montparnasse and its arrival Station" ,subtitle ="Year :{frame_time}")

animate(p,nframes=4,fps=1)
```

![](Week_9_French_Trains_files/figure-markdown_github/Paris%20Montparnasse%20total%20num%20of%20trips-1.gif)

Chosen City with Average Journey Time
=====================================

``` r
p<-ggplot(subset(small_trains,departure_station=="PARIS MONTPARNASSE"),
       aes(x=str_wrap(arrival_station,20),y=journey_time_avg,color=month))+
       geom_jitter()+coord_flip()+ labs(color="Month")+
       transition_time(year)+ease_aes("linear")+
       xlab("Arrival Station")+ylab("Average Journey Time")+
       ggtitle("Paris Montparnasse and its arrival Station" ,subtitle ="Year :{frame_time}")

animate(p,nframes=4,fps=1)
```

![](Week_9_French_Trains_files/figure-markdown_github/Paris%20Montparnasse%20Journey%20time%20average-1.gif)

Chosen City with Average Delay with All Departing
=================================================

``` r
p<-ggplot(subset(small_trains,departure_station=="PARIS MONTPARNASSE"),
       aes(x=str_wrap(arrival_station,20),y= avg_delay_all_departing,color=month))+
       geom_jitter()+coord_flip()+ labs(color="Month")+
       transition_time(year)+ease_aes("linear")+
       geom_hline(yintercept = 0,color="red")+
       xlab("Arrival Station")+ylab("Average Delay All Departing")+
       ggtitle("Paris Montparnasse and its arrival Station" ,subtitle ="Year :{frame_time}")

animate(p,nframes=4,fps=1)
```

![](Week_9_French_Trains_files/figure-markdown_github/Paris%20Montparnasse%20avg%20delay%20all%20departing-1.gif)

Chosen City with Average Delay with All Arriving
================================================

``` r
p<-ggplot(subset(small_trains,departure_station=="PARIS MONTPARNASSE"),
       aes(x=str_wrap(arrival_station,20),y=avg_delay_all_arriving,color=month))+
       geom_jitter()+coord_flip()+ labs(color="Month")+
       transition_time(year)+ease_aes("linear")+
       geom_hline(yintercept = 0,color="red")+
       xlab("Arrival Station")+ylab("Average Delay All Arriving")+
       ggtitle("Paris Montparnasse and its arrival Station" ,subtitle ="Year :{frame_time}")

animate(p,nframes=4,fps=1)
```

![](Week_9_French_Trains_files/figure-markdown_github/Paris%20Montparnasse%20avg%20delay%20all%20arriving-1.gif)

Chosen City with Number of Late Departures
==========================================

``` r
p<-ggplot(subset(small_trains,departure_station=="PARIS MONTPARNASSE"),
       aes(x=str_wrap(arrival_station,20),y=num_late_at_departure,
           color=month))+
       geom_jitter()+coord_flip()+ labs(color="Month")+
       transition_time(year)+ease_aes("linear")+
       xlab("Arrival Station")+ylab("Number of Lates at Departure")+
       ggtitle("Paris Montparnasse and its arrival Station" ,subtitle ="Year :{frame_time}")

animate(p,nframes=4,fps=1)
```

![](Week_9_French_Trains_files/figure-markdown_github/Paris%20Montparnasse%20num%20late%20at%20departure-1.gif)

Chosen City with Number of Late Arrivals
========================================

``` r
p<-ggplot(subset(small_trains,departure_station=="PARIS MONTPARNASSE"),
       aes(x=str_wrap(arrival_station,20),y=num_arriving_late,
           color=month))+
       geom_jitter()+coord_flip()+ labs(color="Month")+
       transition_time(year)+ease_aes("linear")+
       xlab("Arrival Station")+ylab("Number of Lates at Arriving")+
       ggtitle("Paris Montparnasse and its arrival Station" ,subtitle ="Year :{frame_time}")

animate(p,nframes=4,fps=1)
```

![](Week_9_French_Trains_files/figure-markdown_github/Paris%20Montparnasse%20num%20arriving%20late-1.gif)

Departure Station with Average Journey Time and Total Number of Trips
=====================================================================

``` r
p<-ggplot(small_trains,aes(x=journey_time_avg,y=total_num_trips,color=month))+
      geom_point()+transition_states(departure_station)+labs(color="Month")+
      ggtitle("Average Journey Time and Total Number of Trips",
              subtitle="Departure Station : {closest_state}")+
      scale_y_continuous(breaks=seq(0,900,50),labels=seq(0,900,50))+
      scale_x_continuous(breaks=seq(0,500,50),labels=seq(0,500,50))+  
      xlab("Average Journey Time")+ylab("Total Number of Trips")+
      shadow_mark()

animate(p,nframes=59,fps=1)
```

![](Week_9_French_Trains_files/figure-markdown_github/departure%20station%20with%20journey%20time%20avg%20and%20total%20num%20trips-1.gif)

Departure Station with Average Delay All Departing and Number of Late at Departures
===================================================================================

``` r
p<-ggplot(small_trains,aes(x=num_late_at_departure,y=avg_delay_all_departing,
                           color=month))+
      geom_point()+transition_states(departure_station)+labs(color="Month")+
      ggtitle("Average Delay at All Departing and Number of Lates at Departure",
              subtitle="Departure Station : {closest_state}")+
      geom_vline(xintercept = 0,color="red")+
      geom_hline(yintercept = 0,color="red")+
      scale_y_continuous(breaks=seq(-5,175,5),labels=seq(-5,175,5))+
      scale_x_continuous(breaks=seq(0,500,50),labels=seq(0,500,50))+  
      xlab("Number of Lates at Departure")+ylab("Average Delays at all Departing")+
      shadow_mark()

animate(p,nframes=59,fps=1)
```

![](Week_9_French_Trains_files/figure-markdown_github/departure%20station%20with%20Number%20of%20late%20and%20average%20Delay-1.gif)

Departure Station with Average Delay All Arriving and Number of Arriving Late
=============================================================================

``` r
p<-ggplot(small_trains,aes(x=num_arriving_late,y=avg_delay_all_arriving,
                           color=month))+
      geom_point()+transition_states(departure_station)+labs(color="Month")+
      ggtitle("Average Delay at All Arriving and Number of Lates at Arriving",
              subtitle="Departure Station : {closest_state}")+
      geom_vline(xintercept = 0,color="red")+
      geom_hline(yintercept = 0,color="red")+
      scale_y_continuous(breaks=seq(-150,40,5),labels=seq(-150,40,5))+
      scale_x_continuous(breaks=seq(0,250,25),labels=seq(0,250,25))+  
      xlab("Number of Lates at Arriving")+ylab("Average Delays at all Arriving")+
      shadow_mark()

animate(p,nframes=59,fps=1)
```

![](Week_9_French_Trains_files/figure-markdown_github/departure%20station%20with%20Number%20of%20Arriving%20late%20and%20average%20Delay%20arriving-1.gif)

Delayed Cause and Delayed Number
================================

``` r
small_trains %>%
    mutate(delay_cause = str_remove(delay_cause,"delay_cause_")) %>%
ggplot(.,aes(x=delay_cause,y=delayed_number))+
      xlab("Delay Cause")+ylab("Delayed Number")+
      ggtitle("Delayed Causes and Delayed Number as percentage")+
      geom_jitter()+coord_flip()
```

![](Week_9_French_Trains_files/figure-markdown_github/Delayed%20No%20and%20Delayed%20cause-1.png)