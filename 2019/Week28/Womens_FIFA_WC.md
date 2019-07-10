Week 28: FIFA Womens World Cup
================

``` r
wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
squads <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/squads.csv")
codes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/codes.csv")

library(ggplot2)
library(gganimate)
library(dplyr)
library(tvthemes)
```

# Squads

## Captainship vs Goals

``` r
p1<-ggplot(squads,aes(caps,goals,color=pos,label1=player,label2=country,group=1))+geom_point()+
    ylab("No of Goals Scored")+xlab("No of Matches as Captains")+
    ggtitle("Relationship Between Goals Scoring and Captainship",
            subtitle = "This is plotly so click on the points on legend")+
    theme_parksAndRec()+labs(color="Position")

plotly::plotly_build(p1)
```

![](Womens_FIFA_WC_files/figure-gfm/Captiainship%20vs%20Goals-1.png)<!-- -->

## Age vs Goals

``` r
p1<-ggplot(squads,aes(age,goals,color=pos,label1=player,label2=country,group=1))+geom_point()+
    xlab("Age of the Player")+ylab("No of Goals Scored")+
    ggtitle("Relationship Between Goals Scoring and Age",
            subtitle = "This is plotly so click on the points on legend")+
    theme_parksAndRec()+labs(color="Position")

plotly::plotly_build(p1)
```

![](Womens_FIFA_WC_files/figure-gfm/Age%20vs%20Goals-1.png)<!-- -->

## Country and Positions

``` r
squads %>%
  group_by(country,pos) %>%
  count() %>%
  ggplot(.,aes(country,n,fill=pos,label=n))+geom_col()+
         theme_parksAndRec()+labs(fill="Position")+
         xlab("Country")+ylab("Count of Players")+geom_text(position = "stack",vjust=1)+
         ggtitle("Positions and their counts according to Country")+
         theme(axis.text.x = element_text(angle = 90,vjust=0))
```

![](Womens_FIFA_WC_files/figure-gfm/country%20and%20positions-1.png)<!-- -->

## Age and Positions

``` r
squads %>%
  group_by(age,pos) %>%
  count() %>%
  ggplot(.,aes(factor(age),n,fill=pos,label=n))+geom_col()+
         xlab("Age")+ylab("Count of Players")+geom_text(position = "stack",vjust=1,size=3.5)+
         ggtitle("Positions and their counts according to Age")+
         theme_parksAndRec()+labs(fill="Position")
```

![](Womens_FIFA_WC_files/figure-gfm/age%20and%20positions-1.png)<!-- -->

## Country and Age Distributions

``` r
ggplot(squads,aes(age,country))+
       ggridges::geom_density_ridges(scale=1.45,jittered_points=TRUE,fill="green",alpha=0.8)+
       theme_parksAndRec()+
       scale_x_continuous(breaks=seq(10,45,5),labels=seq(10,45,5))+
       scale_y_discrete(expand = c(0, 1))+
       xlab("Age")+ylab("Country")+
       ggtitle("Country vs Age Distribution")
```

![](Womens_FIFA_WC_files/figure-gfm/Country%20and%20age-1.png)<!-- -->

## Country and Goal Experience

``` r
squads %>%
  group_by(country=factor(country)) %>%
  na.omit() %>%
  summarise(goal_count=sum(goals)) %>%
ggplot(.,aes(x=country,y=goal_count,label=goal_count))+geom_col(fill="lightgreen")+
       theme_parksAndRec_light()+geom_text(vjust=-0.5,color=blues9[9],size=5)+
       xlab("Country")+ylab("No of Goals Scored by All Players")+
       ggtitle("No of Goals Scored with Country as in Experience")+
       theme(axis.text.x = element_text(angle = 90,vjust=0))
```

![](Womens_FIFA_WC_files/figure-gfm/Country%20and%20Goal%20Experience-1.png)<!-- -->

## Top 15 Highest Goal Scorers participated in the FIFA World Cup

``` r
squads %>%
  top_n(15,goals) %>%
  select(country,player,goals) %>% 
ggplot(.,aes(stringr::str_wrap(player,10),goals,fill=country,label=goals))+geom_col()+
       xlab("Player")+ylab("No of Goals")+theme_parksAndRec_light()+
       labs(fill="Country")+geom_text(vjust=-0.5,size=7,color="green")+
       theme(axis.text.x = element_text(angle = 90,vjust=0))+
       ggtitle("Top 15 Players who have played most number of Goals So Far")
```

![](Womens_FIFA_WC_files/figure-gfm/Top%2015%20Highest%20Goal%20scorers-1.png)<!-- -->

# WWC Outcomes

## Year vs Countries of participation

``` r
wwc_outcomes %>%
  group_by(year,team) %>%
  count(team) %>%
  select(year,team) %>%
  ungroup() %>%
  count(year) %>%
ggplot(.,aes(factor(year),n,label=n))+geom_col(fill=blues9[5])+
      geom_text(vjust=1,size=9,color=blues9[3])+theme_brooklyn99()+
      xlab("Year")+ylab("No of Countries Participated")+
      ggtitle("No of Countries participated by Year")
```

![](Womens_FIFA_WC_files/figure-gfm/Year%20vs%20country-1.png)<!-- -->

``` r
wwc_outcomes %>%
  group_by(year,team) %>%
  count(team) %>% 
ggplot(.,aes(stringr::str_wrap(team,12),n,label=n))+
      geom_col(fill=blues9[5])+
      geom_text(vjust=1,size=6,color=blues9[3])+theme_brooklyn99()+
      xlab("Country")+ylab("No of Matches")+
      transition_states(year)+ease_aes("linear")+
      theme(axis.text.x = element_text(angle = 90,vjust=0))+
      ggtitle("No of Matches played by a country changing over the Tournament",
              subtitle = "Year:{closest_state}")      
```

![](Womens_FIFA_WC_files/figure-gfm/Years%20vs%20Country%20gif-1.gif)<!-- -->

## Year vs Rounds of Matches

``` r
wwc_outcomes %>%
  group_by(year,round) %>%
  count(round) %>%
  select(year,round) %>%
  ungroup() %>%
  count(year) %>%
ggplot(.,aes(factor(year),n,label=n))+geom_col(fill=blues9[5])+
      geom_text(vjust=1,size=9,color=blues9[3])+theme_brooklyn99()+
      xlab("Year")+ylab("No of Rounds in the Tournament")+
      ggtitle("No of Rounds by Year")  
```

![](Womens_FIFA_WC_files/figure-gfm/Year%20vs%20Round-1.png)<!-- -->

``` r
wwc_outcomes %>%
  group_by(year,round) %>%
  count(round) %>%
  mutate(matches=n/2) %>%
ggplot(.,aes(stringr::str_wrap(round,12),matches,label=matches))+
      geom_col(fill=blues9[5])+
      geom_text(vjust=1,size=6,color=blues9[3])+theme_brooklyn99()+
      xlab("Rounds of the Tournament")+ylab("No of Matches")+
      transition_states(year)+ease_aes("linear")+
      ggtitle("No of Matches changing over the Tournament",
              subtitle = "Year:{closest_state}")    
```

![](Womens_FIFA_WC_files/figure-gfm/Year%20vs%20Round%20gif-1.gif)<!-- -->

## Years vs Goals

``` r
wwc_outcomes %>%
  group_by(year) %>%
  count(score) %>%
ggplot(.,aes(factor(score),n,label=n))+geom_col(fill=blues9[5])+
      geom_text(vjust=1,size=6,color=blues9[3])+theme_brooklyn99()+
      transition_states(year)+ease_aes("linear")+
      xlab("Goals Scored")+ylab("No of Situations")+
      ggtitle("No of Goals Scored by Year with Situations",
              subtitle = "Year :{closest_state}")  
```

![](Womens_FIFA_WC_files/figure-gfm/Years%20vs%20Goals%20gif-1.gif)<!-- -->

*THANK YOU*
