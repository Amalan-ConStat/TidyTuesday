Board Games
================
M.Amalan
March 12, 2019

``` r
library(readr)
library(tidyverse)
library(tidylog)
library(gganimate)
library(splitstackshape)

board_games <- read_csv("board_games.csv")
```

Minimum No of Players and Minimum Play Time
===========================================

``` r
p<-ggplot(board_games,aes(min_players,min_playtime))+
          geom_jitter()+transition_time(year_published)+
          ease_aes("linear")+
          xlab("Minimum No of Players")+
          ylab("Minimum Playing Time")+
          scale_x_continuous(breaks=seq(0,9),labels = seq(0,9))+
          scale_y_continuous(breaks = seq(0,60000,5000),labels=seq(0,60000,5000))+
          ggtitle("Minimum No of Players vs Minimum Playing Time",
                  subtitle ="Year :{frame_time}" )

animate(p,nframes=67,fps=1)
```

![](Board_Games_files/figure-markdown_github/min%20players%20and%20min%20play%20time%201-1.gif)

``` r
p<-subset(board_games, min_playtime < 5000) %>%
   ggplot(.,aes(min_players,min_playtime))+
          geom_jitter()+transition_time(year_published)+
          ease_aes("linear")+
          xlab("Minimum No of Players")+
          ylab("Minimum Playing Time")+
          scale_x_continuous(breaks=seq(0,9),labels = seq(0,9))+
          scale_y_continuous(breaks = seq(0,5000,500),labels=seq(0,5000,500))+
          ggtitle("Minimum No of Players vs Minimum Playing Time",
                  subtitle ="Year :{frame_time}" )

animate(p,nframes=67,fps=1)
```

![](Board_Games_files/figure-markdown_github/min%20players%20and%20min%20play%20time%202-1.gif)

``` r
p<-subset(board_games, min_playtime < 500) %>%
   ggplot(.,aes(min_players,min_playtime))+
          geom_jitter()+transition_time(year_published)+
          ease_aes("linear")+
          xlab("Minimum No of Players")+
          ylab("Minimum Playing Time")+
          scale_x_continuous(breaks=seq(0,9),labels = seq(0,9))+
          scale_y_continuous(breaks = seq(0,500,50),labels=seq(0,500,50))+
          ggtitle("Minimum No of Players vs Minimum Playing Time",
                  subtitle ="Year :{frame_time}" )

animate(p,nframes=67,fps=1)
```

![](Board_Games_files/figure-markdown_github/min%20players%20and%20min%20play%20time%203-1.gif)

Maximum No of Players and Maximimum Play Time
=============================================

``` r
p<-ggplot(board_games,aes(max_players,max_playtime))+
          geom_jitter()+transition_time(year_published)+
          ease_aes("linear")+
          xlab("Maximum No of Players")+
          ylab("Maximum Playing Time")+
          scale_x_continuous(breaks=seq(0,1000,50),labels = seq(0,1000,50))+
          scale_y_continuous(breaks = seq(0,60000,5000),labels=seq(0,60000,5000))+
          ggtitle("Maximum No of Players vs Maximum Playing Time",
                  subtitle ="Year :{frame_time}" )

animate(p,nframes=67,fps=1)
```

![](Board_Games_files/figure-markdown_github/max%20players%20and%20max%20play%20time%201-1.gif)

``` r
p<-subset(board_games,max_players< 125 & max_playtime < 10000) %>%
   ggplot(.,aes(max_players,max_playtime))+
          geom_jitter()+transition_time(year_published)+
          ease_aes("linear")+
          xlab("Maximum No of Players")+
          ylab("Maximum Playing Time")+
          scale_x_continuous(breaks=seq(0,100,10),labels = seq(0,100,10))+
          scale_y_continuous(breaks = seq(0,6000,500),labels=seq(0,6000,500))+
          ggtitle("Maximum No of Players vs Maximum Playing Time",
                  subtitle ="Year :{frame_time}" )

animate(p,nframes=67,fps=1)
```

![](Board_Games_files/figure-markdown_github/max%20players%20and%20max%20play%20time%202-1.gif)

``` r
p<-subset(board_games,max_players< 15 & max_playtime < 1000) %>%
   ggplot(.,aes(max_players,max_playtime))+
          geom_jitter()+transition_time(year_published)+
          ease_aes("linear")+
          xlab("Maximum No of Players")+
          ylab("Maximum Playing Time")+
          scale_x_continuous(breaks=seq(0,15),labels = seq(0,15))+
          scale_y_continuous(breaks = seq(0,1000,50),labels=seq(0,1000,50))+
          ggtitle("Maximum No of Players vs Maximum Playing Time",
                  subtitle ="Year :{frame_time}" )

animate(p,nframes=67,fps=1)
```

![](Board_Games_files/figure-markdown_github/max%20players%20and%20max%20play%20time%203-1.gif)

Maximum No of Players and Average Rating
========================================

``` r
p<-ggplot(board_games,aes(max_players,average_rating))+
          geom_jitter()+transition_time(year_published)+
          ease_aes("linear")+
          xlab("Maximum No of Players")+
          ylab("Average Rating")+
          scale_x_continuous(breaks=seq(0,1000,50),labels = seq(0,1000,50))+
          scale_y_continuous(breaks = seq(0,10,.5),labels=seq(0,10,.5))+
          ggtitle("Maximum No of Players vs Average Rating",
                  subtitle ="Year :{frame_time}" )

animate(p,nframes=67,fps=1)
```

![](Board_Games_files/figure-markdown_github/maximum%20players%20and%20avg%20rating%201-1.gif)

``` r
p<-subset(board_games,max_players <75 ) %>%
   ggplot(.,aes(max_players,average_rating))+
          geom_jitter()+transition_time(year_published)+
          ease_aes("linear")+
          xlab("Maximum No of Players")+
          ylab("Average Rating")+
          scale_x_continuous(breaks=seq(0,75,5),labels = seq(0,75,5))+
          scale_y_continuous(breaks = seq(0,10,.5),labels=seq(0,10,.5))+
          ggtitle("Maximum No of Players vs Average Rating",
                  subtitle ="Year :{frame_time}" )

animate(p,nframes=67,fps=1)
```

![](Board_Games_files/figure-markdown_github/maximum%20players%20and%20avg%20rating%202-1.gif)

``` r
p<-subset(board_games,max_players <15 ) %>%
   ggplot(.,aes(max_players,average_rating))+
          geom_jitter()+transition_time(year_published)+
          ease_aes("linear")+
          xlab("Maximum No of Players")+
          ylab("Average Rating")+
          scale_x_continuous(breaks=seq(0,15),labels = seq(0,15))+
          scale_y_continuous(breaks = seq(0,10,.5),labels=seq(0,10,.5))+
          ggtitle("Maximum No of Players vs Average Rating",
                  subtitle ="Year :{frame_time}" )

animate(p,nframes=67,fps=1)
```

![](Board_Games_files/figure-markdown_github/maximum%20players%20and%20avg%20rating%203-1.gif)

Average Rating and Users Rated
==============================

``` r
p<-ggplot(board_games,aes(average_rating,users_rated))+
          geom_point()+transition_time(year_published)+
          ease_aes("linear")+
          ylab("Users Rated")+
          xlab("Average Rating")+
          scale_x_continuous(breaks=seq(0,10,.5),labels = seq(0,10,.5))+
          scale_y_continuous(breaks = seq(0,60000,5000),labels=seq(0,60000,5000))+
          ggtitle("Average Rating vs Users Rated",
                  subtitle ="Year :{frame_time}" )

animate(p,nframes=67,fps=1)
```

![](Board_Games_files/figure-markdown_github/Average%20Rating%20vs%20Users%20rated%201-1.gif)

``` r
ggplot(board_games,aes(average_rating,users_rated,color=year_published))+
          geom_point(alpha=0.85)+
          labs(color="Year")+
          ylab("Users Rated")+
          xlab("Average Rating")+
          scale_x_continuous(breaks=seq(0,10,.5),labels = seq(0,10,.5))+
          scale_y_continuous(breaks = seq(0,70000,2500),labels=seq(0,70000,2500))+
          ggtitle("Average Rating vs Users Rated",
                  subtitle ="Year : 1950 to 2016" )
```

![](Board_Games_files/figure-markdown_github/Average%20Rating%20vs%20Users%20rated%202-1.png)

Category and Rating
===================

``` r
p<-board_games %>%
   select(year_published,category,average_rating) %>%
   cSplit("category",sep=",") %>%
   gather(Groups,category,"category_01","category_02","category_03","category_04") %>%
   select(year_published,Groups,category,average_rating) %>%
   subset(Groups=="category_01" & average_rating > 7)%>%
   ggplot(.,aes(category,average_rating))+geom_jitter()+
   coord_flip()+xlab("Category")+ylab("Average Rating")+
   transition_time(year_published)+ease_aes("linear")+
   ggtitle("Average Rating for First Category",
           subtitle = "Year: {floor(frame_time)}")

animate(p,fps=1,nframes=54)
```

![](Board_Games_files/figure-markdown_github/Category%20and%20rating%201-1.gif)

``` r
p<-board_games %>%
   select(year_published,category,average_rating) %>%
   cSplit("category",sep=",") %>%
   gather(Groups,category,"category_01","category_02","category_03","category_04") %>%
   select(year_published,Groups,category,average_rating) %>%
   subset(Groups=="category_02" & average_rating >7)%>%
   ggplot(.,aes(category,average_rating))+geom_jitter()+
   coord_flip()+xlab("Category")+ylab("Average Rating")+
   transition_time(year_published)+ease_aes("linear")+
   ggtitle("Average Rating for Second Category",
           subtitle = "Year: {floor(frame_time)}")

animate(p,fps=1,nframes=54)
```

![](Board_Games_files/figure-markdown_github/Category%20and%20rating%202-1.gif)

*THANK YOU*
