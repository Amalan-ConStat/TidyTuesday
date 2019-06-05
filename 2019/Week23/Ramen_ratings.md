Ramen\_ratings
================

``` r
ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

library(tidyverse)
library(gganimate)
library(ggthemes)
```

I have not posted regarding \#TidyTuesday in a while, so here it is. It
is all about manipulating the dataset and generating necessary
plots.

[Data](https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-06-04)
[GitHub Code]()

``` r
ramen_ratings %>%
  count(stars,sort = TRUE) %>%
ggplot(.,aes(fct_inorder(as_factor(stars)),n,label=n))+
      geom_col(fill=blues9[5])+
      geom_text(vjust=-0.5,color=blues9[9])+ylab("Count / Frequency")+
      xlab("Stars")+ggtitle("Stars Distribution")+
      theme_economist()+
      theme(axis.text.x = element_text(angle = 45))
```

![](Ramen_ratings_files/figure-gfm/Stars%20all-1.png)<!-- -->

## Brand versus Stars

### Rating where stars is 4 or above 4

``` r
ramen_ratings %>%
  subset(stars >=4)%>%
  count(brand,sort = TRUE) %>%
  top_n(25) %>%
ggplot(.,aes(x=fct_inorder(brand),y=n,label=n))+geom_col(fill=blues9[5])+
      ylab("Count / Frequency")+xlab("Brand")+
      geom_text(hjust=-0.5,color=blues9[9])+
      ggtitle("Brands Distribution by Stars",
              subtitle = "Top 25")+
      theme_economist()+ coord_flip()
```

![](Ramen_ratings_files/figure-gfm/Rating%20stars%20of%204%20or%20above%204%20for%20Brands-1.png)<!-- -->

### Rating where stars is below 2

``` r
ramen_ratings %>%
  subset(stars < 2)%>%
  count(brand,sort = TRUE) %>%
  top_n(25) %>%
ggplot(.,aes(x=fct_inorder(brand),y=n,label=n))+geom_col(fill=blues9[5])+
      ylab("Count / Frequency")+xlab("Brand")+
      geom_text(hjust=-0.5,color=blues9[9])+
      ggtitle("Brands Distribution by Stars",
              subtitle = "Top 25")+
      theme_economist()+ coord_flip()
```

![](Ramen_ratings_files/figure-gfm/Rating%20stars%20of%202%20or%20below%202%20for%20Brands-1.png)<!-- -->

## Style versus Stars

### Rating where stars is 4 or above 4

``` r
ramen_ratings %>%
  subset(stars >=4)%>%
  count(style,sort = TRUE) %>%
  top_n(10) %>%
ggplot(.,aes(x=fct_inorder(style),y=n,label=n))+geom_col(fill=blues9[5])+
      ylab("Count / Frequency")+xlab("Style")+
      geom_text(hjust=-0.5,color=blues9[9])+
      ggtitle("Styles Distribution by Stars",
              subtitle = "Top 10")+
      theme_economist()+ coord_flip()
```

![](Ramen_ratings_files/figure-gfm/Rating%20stars%20of%204%20or%20above%204%20for%20Style-1.png)<!-- -->

### Rating where stars is below 2

``` r
ramen_ratings %>%
  subset(stars < 2)%>%
  count(style,sort = TRUE) %>%
  top_n(5) %>%
ggplot(.,aes(x=fct_inorder(style),y=n,label=n))+geom_col(fill=blues9[5])+
      ylab("Count / Frequency")+xlab("Style")+
      geom_text(hjust=-0.5,color=blues9[9])+
      ggtitle("Styles Distribution by Stars",
              subtitle = "Top 5")+
      theme_economist()+ coord_flip()
```

![](Ramen_ratings_files/figure-gfm/Rating%20stars%20of%202%20or%20below%202%20for%20Style-1.png)<!-- -->

## Country versus Stars

### Rating where stars is 4 or above 4

``` r
ramen_ratings %>%
  subset(stars >=4)%>%
  count(country,sort = TRUE) %>%
  top_n(25) %>%
ggplot(.,aes(x=fct_inorder(country),y=n,label=n))+geom_col(fill=blues9[5])+
      ylab("Count / Frequency")+xlab("Country")+
      geom_text(hjust=-0.5,color=blues9[9])+
      ggtitle("Countries Distribution by Stars",
              subtitle = "Top 25")+
      theme_economist()+ coord_flip()
```

![](Ramen_ratings_files/figure-gfm/Rating%20stars%20of%204%20or%20above%204%20for%20Country-1.png)<!-- -->

### Rating where stars is below 2

``` r
ramen_ratings %>%
  subset(stars < 2)%>%
  count(country,sort = TRUE) %>%
  top_n(25) %>%
ggplot(.,aes(x=fct_inorder(country),y=n,label=n))+geom_col(fill=blues9[5])+
      ylab("Count / Frequency")+xlab("Country")+
      geom_text(hjust=-0.5,color=blues9[9])+
      ggtitle("Countries Distribution by Stars",
              subtitle = "Top 20")+
      theme_economist()+ coord_flip()
```

![](Ramen_ratings_files/figure-gfm/Rating%20stars%20of%20below%202%20for%20Country-1.png)<!-- -->

## Brand and Style versus Stars

### Rating where stars is 4 or above 4

``` r
ramen_ratings %>%
  subset(stars >=4)%>%
  count(brand,style,sort = TRUE) %>%
  top_n(25) %>%
ggplot(.,aes(x=fct_inorder(brand),y=n,label=n,fill=style))+
      geom_col(position = "dodge")+
      ylab("Count / Frequency")+xlab("Brand")+
      geom_text(hjust=-0.25,color=blues9[9],
                position = position_dodge(width = 1))+
      ggtitle("Brand Distribution by Stars but for Styles",
              subtitle = "Top 25")+
      theme_economist()+ coord_flip()
```

![](Ramen_ratings_files/figure-gfm/Rating%20stars%20of%204%20or%20above%204%20for%20Brand%20and%20Style-1.png)<!-- -->

### Rating where stars is below 2

``` r
ramen_ratings %>%
  subset(stars <2)%>%
  count(brand,style,sort = TRUE) %>%
  top_n(25) %>%
ggplot(.,aes(x=fct_inorder(brand),y=n,label=n,fill=style))+
      geom_col(position = "dodge")+
      ylab("Count / Frequency")+xlab("Brand")+
      geom_text(hjust=-0.25,color=blues9[9],
                position = position_dodge(width = 1))+
      ggtitle("Brand Distribution by Stars but for Styles",
              subtitle = "Top 25")+
      theme_economist()+ coord_flip()
```

![](Ramen_ratings_files/figure-gfm/Rating%20stars%20of%20below%202%20for%20Brand%20and%20Style-1.png)<!-- -->

## Brand and Country versus Stars

### Rating where stars is 4 or above 4

``` r
ramen_ratings %>%
  subset(stars >=4)%>%
  count(brand,country,sort = TRUE) %>%
  top_n(25) %>%
ggplot(.,aes(x=fct_inorder(brand),y=n,label=n,fill=country))+
      geom_col(position = "dodge")+
      ylab("Count / Frequency")+xlab("Brand")+
      geom_text(hjust=-0.25,color=blues9[9],
                position = position_dodge(width = 1))+
      ggtitle("Brand Distribution by Stars but for Country",
              subtitle = "Top 25")+
      theme_economist()+ coord_flip()
```

![](Ramen_ratings_files/figure-gfm/Rating%20stars%20of%204%20or%20above%204%20for%20Brand%20and%20Country-1.png)<!-- -->

### Rating where stars is below 2

``` r
ramen_ratings %>%
  subset(stars <2)%>%
  count(brand,country,sort = TRUE) %>%
  top_n(25) %>%
ggplot(.,aes(x=fct_inorder(brand),y=n,label=n,fill=country))+
      geom_col(position = "dodge")+
      ylab("Count / Frequency")+xlab("Brand")+
      geom_text(hjust=-0.25,color=blues9[9],
                position = position_dodge(width = 1))+
      ggtitle("Brand Distribution by Stars but for Country",
              subtitle = "Top 25")+
      theme_economist()+ coord_flip()
```

![](Ramen_ratings_files/figure-gfm/Rating%20stars%20of%20below%202%20for%20Brand%20and%20Country-1.png)<!-- -->

## Style and Country versus Stars

### Rating where stars is 4 or above 4

``` r
ramen_ratings %>%
  subset(stars >=4)%>%
  count(style,country,sort = TRUE) %>%
  top_n(25) %>%
ggplot(.,aes(x=fct_inorder(country),y=n,label=n,fill=style))+
      geom_col(position = "dodge")+
      ylab("Count / Frequency")+xlab("Country")+
      geom_text(hjust=-0.25,color=blues9[9],
                position = position_dodge(width = 1))+
      ggtitle("Style Distribution by Stars but for Country",
              subtitle = "Top 25")+
      theme_economist()+ coord_flip()
```

![](Ramen_ratings_files/figure-gfm/Rating%20stars%20of%204%20or%20above%204%20for%20Style%20and%20Country-1.png)<!-- -->

### Rating where stars is below 2

``` r
ramen_ratings %>%
  subset(stars <2)%>%
  count(style,country,sort = TRUE) %>%
  top_n(25) %>%
ggplot(.,aes(x=fct_inorder(country),y=n,label=n,fill=style))+
      geom_col(position = "dodge")+
      ylab("Count / Frequency")+xlab("Country")+
      geom_text(hjust=-0.25,color=blues9[9],
                position = position_dodge(width = 1))+
      ggtitle("Style Distribution by Stars but for Country",
              subtitle = "Top 25")+
      theme_economist()+ coord_flip()
```

![](Ramen_ratings_files/figure-gfm/Rating%20stars%20of%20below%202%20for%20Style%20and%20Country-1.png)<!-- -->

## Brand, Style and Country versus Stars

### Rating where stars is 4 or above 4

``` r
ramen_ratings %>%
  subset(stars >=4)%>%
  count(brand,style,country,sort = TRUE) %>%
  top_n(25) %>%
ggplot(.,aes(x=country,y=n,label=n,fill=style))+
      geom_col(position = "dodge")+facet_wrap(~brand)+
      ylab("Count / Frequency")+xlab("Country")+
      geom_text(hjust=-0.05,color=blues9[9],size=3.5,
                position = position_dodge(width = 1))+
      ggtitle("Style Distribution by Stars but for Country",
              subtitle = "Top 25")+
      theme_economist()+ coord_flip()
```

![](Ramen_ratings_files/figure-gfm/Rating%20stars%20of%204%20or%20above%204%20for%20Brand,Style%20and%20Country-1.png)<!-- -->

### Rating where stars is below 2

``` r
ramen_ratings %>%
  subset(stars <2)%>%
  count(brand,style,country,sort = TRUE) %>%
  top_n(25) %>%
ggplot(.,aes(x=country,y=n,label=n,fill=style))+
      geom_col(position = "dodge")+facet_wrap(~brand)+
      ylab("Count / Frequency")+xlab("Country")+
      geom_text(hjust=-0.05,color=blues9[9],size=3.5,
                position = position_dodge(width = 1))+
      ggtitle("Style Distribution by Stars but for Country",
              subtitle = "Top 25")+
      theme_economist()+ coord_flip()
```

![](Ramen_ratings_files/figure-gfm/Rating%20stars%20of%20below%202%20for%20Brand,Style%20and%20Country-1.png)<!-- -->

# Special Plot

``` r
#summarytools::dfSummary(ramen_ratings)

spe_plot<-ramen_ratings %>%
          subset(brand=="Nissin" | brand=="Nongshim" |
                 brand=="Maruchan" | brand=="Myojo" |
                 brand=="Samyang Foods" | brand=="Mama"|
                 brand=="Paldo" | brand=="Indomie"|
                 brand=="Ottogi" | brand=="Sapporo Ichiban") %>%
                 remove_missing() %>%
          ggplot(.,aes(brand,as_factor(stars),color=style))+
          geom_jitter()+theme_economist()+
          xlab("Brand")+ylab("Stars")+
          ggtitle("How Stars change for Brands",subtitle="{closest_state}")+
          transition_states(country,transition_length = 1,state_length = 2)+
          enter_fade()+exit_fade()

animate(spe_plot,nframes=44,fps=2)
```

![](Ramen_ratings_files/figure-gfm/Special%20Plot-1.gif)<!-- -->

*THANK YOU*
