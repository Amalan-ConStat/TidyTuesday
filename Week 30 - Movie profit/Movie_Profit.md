Horror Movie Profit
================
M.amalan
October 7, 2018

Movie Profit, Not So Profit
===========================

Introduction
============

This is my second post on [Tidy Tuesday](https://github.com/rfordatascience/tidytuesday) and the dataset in question is [Movie profit](https://github.com/rfordatascience/tidytuesday/tree/master/data/2018-10-23) dataset. Even though the title of data says Movie profit I am going to focus on the movies which didnot generate any revenue domestic or otherwise.

Understand factors (Genre, Mpaa Rating)
---------------------------------------

3401 movies with 8 variables of information which include numeric and categorical. There are 202 distributors for movies of four types of ratings which are G, PG, PG-13 and R, but 137 movies have no record of them.

Also there are five categories for genre, where Drama with 1236, while horror with 298 movies. There is only one action movie for general audiences(for all) and obviously no horror film should be watched by children alone, yet there are 7 movies which you can watch with your parents.

``` r
ggplot(Movie,aes(genre))+
  geom_bar()+stat_count(aes(y=..count.., label=..count..), geom="text", vjust=-.5)+
  xlab("Genre")+ylab("Frequency")
```

![](Movie_Profit_files/figure-markdown_github/Genre%20bar%20plot-1.png)

There are five types of ratings but around half of them are R rated, while 1094 are PG-13. While 573 are in relative to PG and G rated movies are only 85. Finally, 137 movies do not have any ratings.

``` r
ggplot(Movie,aes(mpaa_rating))+
  geom_bar()+geom_bar()+stat_count(aes(y=..count.., label=..count..), geom="text", vjust=-.5)+
  xlab("Rating")+ylab("Frequency")
```

![](Movie_Profit_files/figure-markdown_github/Ratings%20Bar%20plot-1.png)

Comedy movies are mostly R rated(under 17 requires guardian) and PG-13 (some material is inappropriate to under 13). The frequencies are respectively 367 and 328.

Where 309 movies of adventure genre could be watched by children with accompanying parents and most(67) number of adventure movies can be watched by all ages. Yet 645 Drama movies are R-rated.

``` r
# checking for bias in mpaa rating and genre 
#kable(table(mpaa_rating,genre))
```

We think horror movies are mostly R-rated then it is true. But only is relative to percentage because considering the frequencies we make horror movies the least even according to this random sample.

Action and Comedy movies close to 50 or 40 percentages for for the ratings types of PG-13 and R.

``` r
# column percentage for above table
#kable(table(mpaa_rating,genre) %>%
#prop.table(margin=2) %>%
#round(digits = 2))
```

This dataset contains the release dates from 1936 to 2019. Even though it is not 2019 there is a movie which has been listed here. This explains the domestic and worldwide gross being zero as the minimum value of zero. Then again we have to be careful because there are movies which might not make profit at all.

Lets Focus of movies which has zero domestic gross
--------------------------------------------------

No revenue from 66 movies, that is interesting. So obviously Aqua man has a whopping more than 150 million dollars
budget and no profit because it was not released yet when this data set was compiled. Second rank is for "wonder park" with 100 million dollars.

Production budget Point of View
-------------------------------

Suprisingly there are movies without any production budget information because I am very sure No movie is done for free. Specially it is odd to see "12 Angry Men" in this list, which leads to the conclusion No all Movies in this are to be on it in the first place. We have 66 movies to consider.

``` r
Movie_domestic_zero<-subset.data.frame(Movie,c(domestic_gross==0)) 
#dim(Movie_domestic_zero)
attach(Movie_domestic_zero)
```

    ## The following objects are masked from Movie:
    ## 
    ##     distributor, domestic_gross, genre, movie, mpaa_rating,
    ##     production_budget, release_date, worldwide_gross

``` r
# histogram for production budget
ggplot(Movie_domestic_zero,aes(x=reorder(movie,production_budget),y=production_budget))+
  geom_point()+theme(axis.text.x =element_text(angle = 90, hjust = 1))+
  scale_y_continuous(labels = dollar_format())+ylab("Production budget")+xlab("Movie names")
```

![](Movie_Profit_files/figure-markdown_github/graphical%20analysis%20for%20domestic%20gross%20zero%20movies-1.png)

Genre and MPAA Rating Point of View
-----------------------------------

So movies with R rated have the most count and they are also action movies. This count is 10. Similarly R rated and Drama genre movies also hold the same count. Here also there are 11 movies with have not been classfied into any rating. Finally, there is no G rated movie in this graphical representation.

Majority of movies(31) from R rated and 24 of them are Drama genre. Action, Drama and Horror movies are not rated.

``` r
#plotting worldwide gross with genre
Movie_domestic_zero %>% 
  ggplot(aes(x=mpaa_rating,fill=genre)) +
  geom_bar(position = "stack")+ylab("Frequency")+xlab("MPAA Rating")+
  geom_text(aes(label=..count..),stat='count',position=position_stack(0.4))
```

![](Movie_Profit_files/figure-markdown_github/Domestic%20zero%20and%20genre%20with%20rating-1.png)

``` r
#plotting worldwide gross with mpaa rating
Movie_domestic_zero %>% 
  ggplot(aes(fill=mpaa_rating,x=genre)) +
  geom_bar(position = "stack")+ylab("Frequency")+xlab("Genre")+
  geom_text(aes(label=..count..),stat='count',position=position_stack(0.4))
```

![](Movie_Profit_files/figure-markdown_github/Domestic%20zero%20and%20genre%20with%20rating-2.png)

Finding Outliers in Perspective of Genre and MPAA Rating
--------------------------------------------------------

Considering the boxplot there are 3 outliers in Drama while plotting data in perspective genre. Most amount of production budget is concluded in Action genre, while the least is in Horror.

If we focus on the production budget with genre there are 7 outliers and one action movie has spent more than 1.5 billion dollars from the action genre while a movie from adventure category spent almost 1 billion USD. Others are less than 50 milion USD.

``` r
#plotting production budget with genre
Movie_domestic_zero %>% 
  ggplot(aes(genre,production_budget)) +
  geom_boxplot()+ylab("Production Budget")+
  xlab("Genre")+
  scale_y_continuous(labels = dollar_format())+
  expand_limits(y=0)+
  coord_flip()
```

![](Movie_Profit_files/figure-markdown_github/Domestic%20zero%20Boxplot%20production%20budget%20with%20genre-1.png)

Least amount budget is spent on movies of no rating mentioned while most is on PG-13 rated movies and it has one strong outlier. Previously with genre we had 7 outliers but according to MPAA rating there are only 6 outliers.

``` r
#plotting production budget with genre
Movie_domestic_zero %>% 
  ggplot(aes(mpaa_rating,production_budget)) +
  geom_boxplot()+ylab("Production Budget")+
  xlab("MPAA rating")+
  scale_y_continuous(labels = dollar_format())+
  expand_limits(y=0)+
  coord_flip()
```

![](Movie_Profit_files/figure-markdown_github/Domestic%20zero%20Boxplot%20production%20budget%20with%20rating-1.png)

Production Budget and Worldwide Gross
-------------------------------------

According to the descending order in the last 10 movies with lowest production budget only 2 have profitted. One movie (All the Boys Love Mandy Lane) has considereably done good, but if you consider this list of 10 movies we have 12 angry men as well.

``` r
#kable(Movie_domestic_zero[order(Movie_domestic_zero$production_budget),-c(1,4)] %>% head(10),
#      col.names = c("Movie Name","Production Budget","Wordlwide Gross","Distributor",
#                    "MPAA Rating","Genre"),align="c") %>% kable_styling(full_width = F,position = "left")
```

This indicates that we didn't have records how much of profit in home and away, because there is no way that people did not watch that movie and not make money. So we conclude that some movies even though they aired earlier in the century did not has to be recorded for domestic and worldwide profit.

Years, Months and Days versus Production Budget
-----------------------------------------------

There are 4 Movies before 1972 with zero for domestic gross which can conclude loss of information. Oddly in year 2014 there are 8 movies with zero domestic gross and most of the movies are after year 2000.

``` r
ggplot(Movie_domestic_zero,aes(x=year(release_date)))+
      geom_bar()+ylab("Frequency")+xlab("Years")+
      scale_x_continuous(label=1956:2019,breaks=1956:2019)+
      scale_y_continuous(labels = 0:8,breaks = 0:8)+
      stat_count(aes(y=..count.., label=..count..), geom="text", vjust=-.5)+
      theme(axis.text.x = element_text(angle = 90,hjust = 1))
```

![](Movie_Profit_files/figure-markdown_github/Confirm%20possibility%20of%20information%20loss%20Year-1.png)

Considering the months there is no speciality most of them inbetween 3 and 8. In january there are only two movies.

``` r
ggplot(Movie_domestic_zero,aes(x=month(release_date)))+
      geom_bar()+ylab("Frequency")+xlab("Months")+
      scale_x_continuous(label=1:12,breaks=1:12)+
      scale_y_continuous(labels = 0:8,breaks = 0:8)+
      stat_count(aes(y=..count.., label=..count..), geom="text", vjust=-.5)
```

![](Movie_Profit_files/figure-markdown_github/Confirm%20possibility%20of%20month%20-1.png)

Only the release days 19th and 25th have no domestic gross, while highest count of 6 occurs on 21st. Most of the days have the count of 1 movie.

``` r
ggplot(Movie_domestic_zero,aes(x=day(release_date)))+
      geom_bar()+ylab("Frequency")+xlab("Days")+
      scale_x_continuous(label=1:31,breaks=1:31)+
      scale_y_continuous(labels = 0:6,breaks = 0:6)+
      stat_count(aes(y=..count.., label=..count..), geom="text", vjust=-.5)
```

![](Movie_Profit_files/figure-markdown_github/Confirm%20possibility%20of%20day-1.png)

Conclusion
==========

-   In the complete data set Drama Genre has most (1236) counts and least count goest to Horror. While, most (1514) of the Movies are R rated. Considering Genre an Rating, it is true that Horror movies are R rated while it represents 76%.

-   While there are movies with No domestic gross some have not been released yet (Aqua man and Wonder Park). Further, some Movies do not even have worldwide gross. This causes missing information. Even though a famous movies such as "12 Angry Men".

-   This missing information could be the reason that there are 4 movies which were released before 1972. Oddly in 2014 there are 8 movies which does not contain domestic gross information.

-   Box plot indicates Adventure genre have spent more range in production budget, while in perspective of MPAA rating PG-13 movies have most range in production budget with a clear outlier.

Further Analysis
================

-   Similarly we can focus on movies of world wide gross equals to zero with other variables.

-   Conduct scrutinized interest with movies of world wide gross zero and domestic gross zero.
