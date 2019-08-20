Week 34 Nuclear Explosions
================

``` r
nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

# load packages
library(dplyr)
library(ggplot2)
library(gganimate)
library(tvthemes)
```

# Magnitude Surface vs Magnitude Body

## All points

``` r
ggplot(nuclear_explosions,aes(magnitude_surface,magnitude_body))+
      geom_point(color=blues9[7])+
      xlab("Magnitude Surface")+ylab("Magnitude Body")+
      ggtitle("Magnitude Surface Changing with Magnitude Body")+labs(caption="TidyTuesday 34")+
      theme_simpsons()
```

![](Nuclear-Missiles_files/figure-gfm/Surface%20vs%20Body%20P1-1.png)<!-- -->

## Points where Magnitude surface and body are more than 0 measurement

``` r
nuclear_explosions %>%
  filter(magnitude_surface >0) %>%
  filter(magnitude_body >0) %>%
ggplot(.,aes(magnitude_surface,magnitude_body))+
      geom_point(color=blues9[7])+
      xlab("Magnitude Surface")+ylab("Magnitude Body")+
      ggtitle("Magnitude Surface Changing with Magnitude Body")+labs(caption="TidyTuesday 34")+
      theme_simpsons()
```

![](Nuclear-Missiles_files/figure-gfm/Surface%20vs%20Body%20P2-1.png)<!-- -->

## Magnitude Surface and Body but Based on Country

``` r
nuclear_explosions %>%
  filter(magnitude_surface >0) %>%
  filter(magnitude_body >0) %>%
ggplot(.,aes(magnitude_surface,magnitude_body,color=country))+
      geom_point()+
      xlab("Magnitude Surface")+ylab("Magnitude Body")+
      ggtitle("Magnitude Surface Changing with Magnitude Body",
              subtitle = "By Country")+labs(caption="TidyTuesday 34")+
      theme_simpsons()+scale_color_simpsons()
```

![](Nuclear-Missiles_files/figure-gfm/Surface%20vs%20Body%20P3-1.png)<!-- -->

## Magnitude Surface and Body but Based on Country with animation

``` r
nuclear_explosions %>%
  filter(magnitude_surface >0) %>%
  filter(magnitude_body >0) %>%
ggplot(.,aes(magnitude_surface,magnitude_body,color=country))+
      geom_point()+
      xlab("Magnitude Surface")+ylab("Magnitude Body")+
      ggtitle("Magnitude Surface Changing with Magnitude Body",
              subtitle = "By Country")+labs(caption="TidyTuesday 34")+
      transition_states(country)+theme_simpsons()+scale_color_simpsons()
```

![](Nuclear-Missiles_files/figure-gfm/Surface%20vs%20Body%20P4-1.gif)<!-- -->

# Yield Lower vs Yield Upper

## All points

``` r
ggplot(nuclear_explosions,aes(yield_lower,yield_upper))+
      geom_point(color=blues9[7])+
      xlab("Yield Lower")+ylab("Yield Upper")+
      ggtitle("Yield Lower Changing with Yield Upper")+labs(caption="TidyTuesday 34")+
      theme_simpsons()
```

![](Nuclear-Missiles_files/figure-gfm/Yield%20Lower%20vs%20Upper%20P1-1.png)<!-- -->

## Points where Yield Upper and Lower are more than 0 measurement

``` r
nuclear_explosions %>%
  filter(yield_lower >0) %>%
  filter(yield_upper >0) %>%
ggplot(.,aes(yield_lower,yield_upper))+
      geom_point(color=blues9[7])+
      xlab("Yield Lower")+ylab("Yield Upper")+
      ggtitle("Yield Lower Changing with Yield Upper")+labs(caption="TidyTuesday 34")+
      theme_simpsons()
```

![](Nuclear-Missiles_files/figure-gfm/Yield%20Lower%20vs%20Upper%20P2-1.png)<!-- -->

## Yield Upper and Lower but Based on Country

``` r
nuclear_explosions %>%
  filter(yield_lower >0) %>%
  filter(yield_upper >0) %>%
ggplot(.,aes(yield_lower,yield_upper,color=country))+
      geom_point()+
      xlab("Yield Lower")+ylab("Yield Upper")+
      ggtitle("Yield Lower Changing with Yield Upper",
              subtitle = "By Country")+labs(caption="TidyTuesday 34")+
      theme_simpsons()+scale_color_simpsons()
```

![](Nuclear-Missiles_files/figure-gfm/Yield%20Lower%20vs%20Upper%20P3-1.png)<!-- -->

## Yield Upper and Lower but Based on Country with animation

``` r
nuclear_explosions %>%
  filter(yield_lower >0) %>%
  filter(yield_upper >0) %>%
ggplot(.,aes(yield_lower,yield_upper,color=country))+
      geom_point()+
      xlab("Yield Lower")+ylab("Yield Upper")+
      ggtitle("Yield Lower Changing with Yield Upper",
              subtitle = "By Country")+labs(caption="TidyTuesday 34")+
       transition_states(country)+theme_simpsons()+scale_color_simpsons()
```

![](Nuclear-Missiles_files/figure-gfm/Yield%20Lower%20vs%20Upper%20P4-1.gif)<!-- -->

# Yearly progress based on Country

## Animated

``` r
nuclear_explosions %>%
   group_by(year,country) %>%
   summarise(counting=n()) %>%
ggplot(.,aes(country,counting,label=counting))+geom_col(fill=blues9[1])+
   xlab("Country")+ylab("Counts")+geom_text(vjust=-1)+
   ggtitle("Nuclear Explosions for Countries",
          subtitle = "Year :{closest_state}")+labs(caption="TidyTuesday 34")+
   transition_states(year)+
   scale_fill_simpsons()+theme_simpsons()
```

![](Nuclear-Missiles_files/figure-gfm/Year%20vs%20Country%20P1-1.gif)<!-- -->

## Stack graph

``` r
nuclear_explosions %>%
   group_by(year,country) %>%
   summarise(counting=n()) %>%
ggplot(.,aes(year,counting,label=counting,fill=country))+
   geom_col(position = "stack")+
   xlab("Year")+ylab("Counts")+geom_text(position = "stack",vjust=1,size=2)+
   ggtitle("Nuclear Explosions by Year",subtitle = "for Countries")+
   labs(caption="TidyTuesday 34")+
   theme_simpsons()+scale_fill_simpsons()
```

![](Nuclear-Missiles_files/figure-gfm/Year%20vs%20Country%20P2-1.png)<!-- -->

# Type vs Purpose by Country

## Animated

``` r
ggplot(nuclear_explosions,aes(type,purpose,color=country))+
  geom_jitter(alpha=0.7)+xlab("Type")+ylab("Purpose")+
  ggtitle("Purpose vs Type for Nuclear Missiles",subtitle = "By Country")+
  labs(caption="TidyTuesday 34")+ transition_states(country)+
  theme_simpsons()+scale_color_simpsons()+
  theme(axis.text.x = element_text(angle=60,hjust=1))
```

![](Nuclear-Missiles_files/figure-gfm/Country%20vs%20Purpose%20P1-1.gif)<!-- -->

# Source vs Year by Country

## Animated

``` r
nuclear_explosions %>%
  group_by(year,type,country) %>%
  summarise(counting=n()) %>%
  select(year,country,type,counting) %>%
  subset(type=="SHAFT"|type=="TUNNEL"|type=="ATMOSPH"|type=="SHAFT/GR"|type=="AIRDROP") %>%
ggplot(.,aes(year,counting,fill=type,label=counting))+
  geom_col()+geom_text(position = "stack",vjust=1,size=2)+
  transition_states(country)+theme_simpsons()+
  enter_fade() + exit_shrink() + ease_aes('sine-in-out')+
  scale_fill_simpsons()+xlab("Year")+ylab("Counts")+
  ggtitle("Yearly change by Countries for Types",
          subtitle = "Country:{closest_state}")+
  labs(caption="TidyTuesday 34")
```

![](Nuclear-Missiles_files/figure-gfm/Type%20vs%20year%20P1-1.gif)<!-- -->

*THANK YOU*
