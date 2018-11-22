ThanksGiving week 34
================
M.amalan
November 20, 2018

Packages : ggplot2, ggthemr, stringr, scales, gridExtra TidyTuesday : Week 34 Data : thanksgiving\_meals

Saying Prayer and Regions for all:

East South Central, South Atlantic, West South Central have respective percentages of 76.7%, 69.2% and 69.2% of saying prayers for Thanksgiving dinner. While East North Central, Middle Atlantic, Mountain and West North Central all have percentages for saying prayer above 50 but less than 65. Finally, regions New England and Pacific have the highest percentages of not saying prayer for Thanksgiving with 51.7 and 41.8. In a special note, 52.5% have not answered which region they are from, and in this lot 25.4% would pray and 22% would not.

``` r
plot_all<-ggplot(Thanksgiving,aes(x=str_wrap(us_region,7),fill=prayer))+
          xlab("Regions")+ylab("Percentage")+ scale_y_continuous(labels = percent)+
          geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="stack" ) +
          geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
                    stat="count", position=position_stack(1), vjust=1)+ggtitle("Thanksgiving and Praying with All",subtitle = "Some have not answered")

print(plot_all)
```

![](Thanksgiving_week34_files/figure-markdown_github/Prayer%20and%20Regions%20of%20all-1.png)

``` r
ggsave(plot_all,width = 10,height = 10,dpi = 300,filename = "Prayer_Regions_all.png")
```

Saying Prayer and Regions for people who celebrate Thanksgiving:

If we remove the people who do not celebrate Thanksgiving it is clear that the previous percentages have changed. Further, East South Central, South Atlantic, West South Central percentages have increased to respectively 82.1, 72.9 and 74.1. While East North Central, Middle Atlantic, Mountain and West North Central all have percentages for saying prayer above 60 but less than 70. Finally, regions New England and Pacific have the highest percentages of not saying prayer for Thanksgiving with 54.5 and 46.9. When we remove the people who do not celebrate thanksgiving the people who have not answered which region they are from has decreased to 42.9%. while others two have increased.

``` r
plot_yes<-ggplot(Thanksgiving_Yes,aes(x=str_wrap(us_region,7),fill=prayer))+
          xlab("Regions")+ylab("Percentage")+ scale_y_continuous(labels = percent)+
          geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="stack" ) +
          geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
                    stat="count", position=position_stack(1), vjust=1)+ggtitle("Everyone who celebrates Thanksgiving but praying !!!",
                                                                               subtitle ="Still some of them do not wish to say where they are from" )

print(plot_yes)
```

![](Thanksgiving_week34_files/figure-markdown_github/Prayer%20and%20Regions%20of%20people%20who%20celebrate%20Thanksgiving-1.png)

``` r
ggsave(plot_yes,width = 10,height = 10,dpi = 300,filename = "Prayer_Regions_Yes.png")
```

People who do not celebrate Thanksgiving:

20.5% people who do not celebrate thanksgiving are from pacific, while lease percentage amount goes to New England and West North Central with 3.8. Regions such as Middle Atlantic, Southern Atlantic show percentages respectively 17.9 and 14.1. Finally, Other areas show percentages above 5 and less than 8. Considerably 12.8% of people have not answered which region they are from.

You can look at the counts see if they are with the percentages.

``` r
plot_No<-ggplot(Thanksgiving_No,aes(x=str_wrap(us_region,7)))+
          xlab("Regions")+ylab("Frequency")+ 
          geom_bar(aes(y = (..count..)/sum(..count..))) +
          geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
          scale_y_continuous(labels = percent) +ggtitle("Percentage perspective look at Regions of who do not celebrate Thanksgiving")

ggsave(plot_No,width = 10,height = 10,dpi = 300,filename = "Prayer_Regions_No.png")

plot_No_count<-ggplot(Thanksgiving_No,aes(x=str_wrap(us_region,7)))+
          xlab("Regions")+ylab("Frequency")+ 
          geom_bar(aes(y = (..count..))) +
          geom_text(aes(y = ((..count..)), label = (..count..)), stat = "count", vjust = -0.25)+
          ggtitle("Counts perspective look at Regions of who do not celebrate Thanksgiving")

grid.arrange(plot_No,plot_No_count,ncol=1)
```

![](Thanksgiving_week34_files/figure-markdown_github/People%20who%20do%20not%20celebrate%20Thanksgiving-1.png)

``` r
ggsave(plot_No_count,width = 10,height = 10,dpi = 300,filename = "Prayer_Regions_No.png")
```
