Week 35 Bridges of Baltimore
================
M.amalan
November 27, 2018

Bridge Data and Baltimore
=========================

Data on bridges is from week 35 for TidyTuesday. Trying to explain the data using maps is obvious, yet I will use animated jitter plots. There are 13 variables and 2079 observations. Brave choice of limiting my self to less than 10 variables, where latitude, longitude and Vehicles will not be taken into account.

So with the help of packages tidyverse, ggthemr, gganimate,formattable and readr I will complete this analysis. Most of the bridges are owned by several agencies, but I will only focus on the top three ownership holders.

Counties which have bridges owned by State Highway Agency
---------------------------------------------------------

Close to 1000 bridges are owned by State Highway Agency, where most of them are in Baltimore County. High amount of bridges are in good condition, further more bridges are in Fair condition and only around 10 bridges in Poor condition.

Considering the Average Daily Traffic only one bridge in Poor condition has the amount of close to 110,000, while all the other poor condition bridges have Average Daily Traffic less than 30,000. While counties Anne Arundel and Hartford have no Poor condition bridges at all.

Most of the bridges are from Baltimore County and around 20 bridges have more than 150,000 Average Daily Traffic for both Fair and Good conditions. Hartford and Carroll Counties have their Average Daily Traffic which does not exceed 80,000 at any condition of the bridge.

``` r
ggplot(subset(bridges,Owner=="State Highway Agency")
       ,aes(color=Condition,y=`Average Daily Traffic`,x=str_wrap(County,7)))+xlab("County")+
  ggtitle("Condition of Bridges owned by State Highway Agency \nand their Average Daily Traffic")+
  scale_y_continuous(labels =seq(0,230000,10000) ,breaks = seq(0,230000,10000))+
  geom_jitter()+transition_states(Condition,transition_length = 2,state_length = 3)+
  enter_fade()+exit_shrink()+ease_aes("back-in")
```

![](bridges_baby_files/figure-markdown_github/County%20with%20Condition%20and%20Average%20Daily%20Traffic%20SHA-1.gif)

Counties which have bridges owned by County Highway Agency
----------------------------------------------------------

County Highway Agency owns the second most amount of bridges in this data-set. Therefore using jitter plots we are going to check how the condition of the bridge and counties are explained is the simplest manner.

Less amount of poor condition bridges in all counties except Anne Arundel County. All bridges owned by County Highway Agency have a limited Average Daily Traffic less than 50,000. Clearly we have More fair bridges than good ones. In the poor condition category only two points have Average Daily Traffic more than 20,000, while other two conditions have more than 10 bridges.

Most of these bridges are in Baltimore County even it is in any one of three conditions. There are few bridges which have more than 40,000 Average Daily Traffic and they are also in Baltimore County.

There are bridges which have Zero Average Daily Traffic. In all three Conditions only Hartford County has bridges which has Average Daily Traffic less than 10,000.

``` r
ggplot(subset(bridges,Owner=="County Highway Agency")
       ,aes(color=Condition,y=`Average Daily Traffic`,x=str_wrap(County,7)))+xlab("County")+
  ggtitle("Condition of Bridges owned by County Highway Agency \nand their Average Daily Traffic")+
  scale_y_continuous(labels =seq(0,40000,5000) ,breaks = seq(0,40000,5000))+
  geom_jitter()+transition_states(Condition,transition_length = 2,state_length = 3)+
  enter_fade()+exit_shrink()+ease_aes("back-in")
```

![](bridges_baby_files/figure-markdown_github/County%20with%20Condition%20and%20Average%20Daily%20Traffic%20CHA-1.gif)

Counties which have bridges owned by State Toll Authority
---------------------------------------------------------

There is only bridge which is in Poor condition and it is in Baltimore City, while Howard county has no Good condition bridges, but Anne Arundel county has one good condition bridge. Further there is only 3 Fair condition bridges in Howard County while there have an Average Daily Traffic less than 10,000.

The highest Average Daily Traffic is close to 170,000 which are only 4 and in Good and Fair conditions. Further, Hartford county has only six Good condition bridges. Only few of the bridges have Average Daily Traffic close to zero.

``` r
ggplot(subset(bridges,Owner=="State Toll Authority")
       ,aes(color=Condition,y=`Average Daily Traffic`,x=str_wrap(County,7)))+xlab("County")+
  ggtitle("Condition of Bridges owned by State Toll Authority \nand their Average Daily Traffic")+
  scale_y_continuous(labels =seq(0,170000,10000) ,breaks = seq(0,170000,10000))+
  geom_jitter()+transition_states(Condition,transition_length = 2,state_length = 3)+
  enter_fade()+exit_shrink()+ease_aes("back-in")
```

![](bridges_baby_files/figure-markdown_github/County%20with%20Condition%20and%20Average%20Daily%20Traffic%20STA-1.gif)

Most amount of bridges Built based on Year
------------------------------------------

Years 1957, 1970, 1975, 1991, 1963 and 1961 have the top 6 spots for building more than 50 bridges in those years. If we consider the conditions of Fair and Good only the year 1991 is suitable to mention, while all other years has at-least one Poor condition bridge. Further There are more Poor condition bridges in 1961 than in 1957. While all Poor condition bridges has Average Daily Traffic less than 50,000.

Finally, there are only a few bridges which have Average Daily Traffic above 100,000 and only 3 are in Good condition. There are Bridges which can have Average Daily Traffic close to zero in all 6 years.

``` r
ggplot(subset(bridges,`Year Built`=="1957" | `Year Built`=="1970" | `Year Built`=="1975" | `Year Built`=="1991" 
              | `Year Built`=="1963" | `Year Built`=="1961")
       ,aes(color=Condition,y=`Average Daily Traffic`,x=factor(`Year Built`)))+
  xlab("Year Built")+ylab("Average Daily Traffic")+
  ggtitle("Most amount of Bridges built based on Years \nand their Conditions")+
  geom_jitter()+legend_bottom()+transition_states(Condition,transition_length = 2,state_length = 3)+
  enter_fade()+exit_shrink()+ease_aes("back-in")
```

![](bridges_baby_files/figure-markdown_github/Year%20Built%20with%20Condition%20and%20Average%20Daily%20Traffic-1.gif)

Average Traffic Less than or equal to 100,000 for Counties with Bridge Condition
--------------------------------------------------------------------------------

While obtaining summary for county variable there is one issue because there are two observations which say "Baltimore city" than "Baltimore City" and I don't want to change them.

If we focus on Average Daily Traffic less than or equal to 100,000 based on County and Condition. It is clear that Poor condition bridges are part of this criteria and mostly Average Daily Traffic is less than 5000 for Counties Howard, Hartford and Carroll. While Baltimore City has highest amount up-to 75,000, but Baltimore County has highest amount close to 40,000. Finally Anne Arundel County has only one Poor condition bridge which has Average Daily Traffic Close to zero.

We can see that there are more Fair Condition bridges than Good ones. In Baltimore County most of the Fair condition bridges have Average Daily Traffic less than 15000. Similarly Carroll county and Hartford county also behave similarly. But for Good condition bridges this is not the case where there is no certain strong dense region as similar to Fair condition bridges.

Previously when we looked into Ownership we did not see Baltimore City as a factor until the ownership of State Toll Authority, but here that is not the case.

``` r
ggplot(subset(bridges, `Average Daily Traffic` <= 100000 & County != "Baltimore city"),aes(x=County,y=`Average Daily Traffic`,color=Condition))+
  xlab("County")+ylab("Averag Daily Traffic")+ggtitle("Average Daily Traffic Less than 100,000 \nFor Counties")+
  scale_y_continuous(labels = seq(0,100000,5000),breaks = seq(0,100000,5000))+coord_flip()+
  theme(axis.text.x = element_text(angle = -90))+
  geom_jitter()+transition_states(Condition,transition_length = 2,state_length = 3)+
  enter_fade()+exit_shrink()+ease_aes("back-in")
```

![](bridges_baby_files/figure-markdown_github/Average%20Traffic%20less%20than%20100000-1.gif)

Average Traffic More than 100,000 for Counties with Bridge Condition
--------------------------------------------------------------------

This Jitter plot is completely different than previous one, because there are no clear dense regions for any counties and conditions of the bridge. There is only one Poor condition bridge in Baltimore County where the Average Daily Traffic is close to 115,000. In Fair condition bridges also Baltimore County holds the most, while they are slightly dense in the region of 175,000 to 190,000. while for Howard County similar density occurs between 190,000 to 205,000. Bridges in Good condition have a more points in Baltimore County than Anne Arundel County because there are no bridges with Average Daily Traffic above 140,000.

``` r
ggplot(subset(bridges, `Average Daily Traffic` > 100000 & County != "Baltimore city"),aes(x=County,y=`Average Daily Traffic`,color=Condition))+
  xlab("County")+ylab("Average Daily Traffic")+ggtitle("Average Daily Traffic More than 100,000 \nFor Counties")+
  scale_y_continuous(labels = seq(100000,230000,5000),breaks = seq(100000,230000,5000))+coord_flip()+
  theme(axis.text.x = element_text(angle = -90))+
  geom_jitter()+transition_states(Condition,transition_length = 2,state_length = 3)+
  enter_fade()+exit_shrink()+ease_aes("back-in")
```

![](bridges_baby_files/figure-markdown_github/Average%20Traffic%20more%20than%20100000-1.gif)

Improvement and Bridge Conditions with Counties
-----------------------------------------------

In the variable of Total Improvement There are 1438 missing values, 42 values are zero and the rest are actual values. I am going to look at Total Improvement in two tables. First table will include where bridges have Total Improvement is higher than 9,999,000 and less than 30,000,000. Second table is for bridges which have Total Improvement higher than or equal to 30,000,000.

Further to make these tables interesting I will using the package formattable package and add colors and tiles for numerical values. In the first table there are 7 bridges while only Anne Arundel County holds 3 and Baltimore City holds 4. One bridge is from 1953, and others are from the period of 1977 to 1983. Conditions of these bridges are mostly Fair and two bridges are in Good condition. Lowest Average Daily Traffic is 11760, while highest is 124193, where both bridges are in Fair Condition, and the amount spent on them for Total Improvement are respectively 18,163,000 and 16,264,000. The bridge with Highest amount of traffic is built in 1953.

``` r
Top10<-subset(bridges[,c(-1,-2,-9,-10,-11,-12,-13)], `Total Improvement` > 9999 & `Total Improvement` < 30000)

customRed0 = "#FF8080"
customRed = "#7F0000"

customyellow0 = "#FFFF80"
customyellow = "#BFBF00"

customblue0 = "#6060BF"
customblue =  "#00007F"

formattable(Top10,align=c("l","l","c","c","c","c"),
            list(
              County =formatter("span",style= ~style(color="grey")),
            `Total Improvement`=color_tile(customblue0,customblue),
            `Average Daily Traffic`=color_tile(customyellow0,customyellow),
            `Year Built`=color_tile(customRed0,customRed)
            ))
```

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:left;">
County
</th>
<th style="text-align:left;">
Carries
</th>
<th style="text-align:center;">
Year Built
</th>
<th style="text-align:center;">
Condition
</th>
<th style="text-align:center;">
Average Daily Traffic
</th>
<th style="text-align:center;">
Total Improvement
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
<span style="color: grey">Anne Arundel County</span>
</td>
<td style="text-align:left;">
MD 2
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #7f0000">1983</span>
</td>
<td style="text-align:center;">
Fair
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #e7e750">53221</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #6060bf">13504</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
<span style="color: grey">Anne Arundel County</span>
</td>
<td style="text-align:left;">
US 50
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ff8080">1953</span>
</td>
<td style="text-align:center;">
Fair
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #bfbf00">124193</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #3a3aa5">16264</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
<span style="color: grey">Anne Arundel County</span>
</td>
<td style="text-align:left;">
UPPER LEVEL ROADWA
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #981919">1977</span>
</td>
<td style="text-align:center;">
Fair
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffff80">11760</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #1f1f94">18163</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
<span style="color: grey">Baltimore City </span>
</td>
<td style="text-align:left;">
IS 95 SB
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #981919">1977</span>
</td>
<td style="text-align:center;">
Fair
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #cfcf21">94765</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #4040aa">15785</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
<span style="color: grey">Baltimore City </span>
</td>
<td style="text-align:left;">
IS 95 VIADUCT SB
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #8b0c0c">1980</span>
</td>
<td style="text-align:center;">
Good
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #e1e144">63650</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #3c3ca7">16051</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
<span style="color: grey">Baltimore City </span>
</td>
<td style="text-align:left;">
IS 95 VIADUCT NB
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #8b0c0c">1980</span>
</td>
<td style="text-align:center;">
Fair
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #e7e751">52850</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #00007f">20484</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
<span style="color: grey">Baltimore City </span>
</td>
<td style="text-align:left;">
IS 95 SB
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #8b0c0c">1980</span>
</td>
<td style="text-align:center;">
Good
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #e6e64e">55621</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #00007f">20484</span>
</td>
</tr>
</tbody>
</table>
When I did try to plot the top ten bridges with most Total improvement there was one issue, which is the distance between first two values and the next 8 values. Therefore I divided the table into two.

In this second table We can see there are two bridges which are from Baltimore City and are built in 1980 and 1971, but the amount spent on Total Improvement is 300,000,000. But their Average Daily Traffic values are respectively 56280 and 30600.

While we have another bridge from Baltimore City and built in 1907, but Total Improvement amount is 35,026,000. But the Average Daily Traffic is 3,900.

``` r
Top3<-subset(bridges[,c(-1,-2,-9,-10,-11,-12,-13)], `Total Improvement` >= 30000)

customRed0 = "#FF8080"
customRed = "#7F0000"

customyellow0 = "#FFFF80"
customyellow = "#BFBF00"

customblue0 = "#6060BF"
customblue =  "#00007F"

formattable(Top3,align=c("l","l","c","c","c","c"),
            list(
              County =formatter("span",style= ~style(color="black")),
            `Total Improvement`=color_tile(customblue0,customblue),
            `Average Daily Traffic`=color_tile(customyellow0,customyellow),
            `Year Built`=color_tile(customRed0,customRed)
            ))
```

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:left;">
County
</th>
<th style="text-align:left;">
Carries
</th>
<th style="text-align:center;">
Year Built
</th>
<th style="text-align:center;">
Condition
</th>
<th style="text-align:center;">
Average Daily Traffic
</th>
<th style="text-align:center;">
Total Improvement
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
<span style="color: black">Baltimore City</span>
</td>
<td style="text-align:left;">
US 40 EDMONDSON AV
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ff8080">1907</span>
</td>
<td style="text-align:center;">
Poor
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffff80">3900</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #6060bf">35026</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
<span style="color: black">Baltimore City</span>
</td>
<td style="text-align:left;">
IS 95 VIADUCT SB
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #7f0000">1980</span>
</td>
<td style="text-align:center;">
Fair
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #bfbf00">56280</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #00007f">300000</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
<span style="color: black">Baltimore City</span>
</td>
<td style="text-align:left;">
EASTERN AVENUE
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #8e0f0f">1971</span>
</td>
<td style="text-align:center;">
Fair
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #dede3e">30600</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #00007f">300000</span>
</td>
</tr>
</tbody>
</table>
