Week 29: R4DS
================

``` r
r4ds_members <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-16/r4ds_members.csv")

# load the packages
library(tidyverse)
library(ggstatsplot)
library(lubridate)
```

# Timely Change

## Timely Change for Full Members

``` r
ggplot(r4ds_members,aes(date,full_members))+geom_point(color=blues9[7])+
      ggthemes::theme_stata()+
      xlab("Date")+ylab("Full Members")+
      scale_x_date(date_labels = "%y %b %d",breaks = '1 month')+
      theme(axis.text.x = element_text(angle = 60,hjust=1))+
      ggtitle("How R4DS Members have involved over the Years")
```

![](R4DS_files/figure-gfm/Timely%20Change-1.png)<!-- -->

# Daily Members

``` r
ggscatterstats(
               data = r4ds_members,
               x = daily_active_members,
               y = daily_members_posting_messages,
               xlab = "Daily Active Members",
               ylab = "Daily Active Members Posting Messages",
               title = "Relationship between Daily Active Members and them posting messages",
               messages = FALSE
               )
```

![](R4DS_files/figure-gfm/Daily%20members-1.png)<!-- -->

# Daily Members by Day of Week

``` r
grouped_ggscatterstats(
                       data = r4ds_members %>% mutate(day=wday(date,label=TRUE)),
                       x=daily_active_members,
                       y=daily_members_posting_messages,
                       grouping.var = day,
                       xlab = "Daily Active\nMembers",
                       ylab = "Daily Active Members\nPosting Messages",
                       title = "Relationship between Daily Active Members\nand them posting messages",
                       messages = FALSE,ncol=2
                       )
```

![](R4DS_files/figure-gfm/Daily%20members%20by%20week%20of%20day-1.png)<!-- -->

# Weekly Members

``` r
ggscatterstats(
               data = r4ds_members,
               x = weekly_active_members,
               y = weekly_members_posting_messages,
               xlab = "Weekly Active Members",
               ylab = "Weekly Active Members Posting Messages",
               title = "Relationship between Weekly Active Members and them posting messages",
               messages = FALSE
               )
```

![](R4DS_files/figure-gfm/Weekly%20members-1.png)<!-- -->

# Weekly Members by Day of Week

``` r
grouped_ggscatterstats(
                       data = r4ds_members %>% mutate(day=wday(date,label=TRUE)),
                       x = weekly_active_members,
                       y = weekly_members_posting_messages,
                       grouping.var=day,
                       xlab = "Weekly Active Members",
                       ylab = "Weekly Active Members\nPosting Messages",
                       title = "Relationship between Weekly Active Members\nand them posting messages",
                       messages = FALSE,ncol=2
                       )
```

![](R4DS_files/figure-gfm/Weekly%20members%20by%20week%20of%20day-1.png)<!-- -->

# Messages in Public, Private and DM Channels

``` r
ggbetweenstats(
               data = select(r4ds_members,"messages_in_public_channels",
                                          "messages_in_private_channels",
                                          "messages_in_d_ms") %>% 
                      gather("Type","Count"),
               x = Type,
               y = Count,
               xlab = "Type of Channel",
               ylab = "Count",
               title = "Comparing Daily Messages Between Channels",
               messages = FALSE
               )
```

![](R4DS_files/figure-gfm/public%20private%20shared%20channels-1.png)<!-- -->

# Percent Based Messages in Public, Private and DM Channels

``` r
ggbetweenstats(
               data = select(r4ds_members,"percent_of_messages_public_channels",
                                          "percent_of_messages_private_channels",
                                          "percent_of_messages_d_ms") %>% 
                      gather("Type","Count"),
               x = Type,
               y = Count,
               xlab = "Type of Channel",
               ylab = "Count",
               title = "Comparing Daily Messages Between Channels Percentage Wise",
               messages = FALSE
               )
```

![](R4DS_files/figure-gfm/percent%20public%20private%20shared%20channels-1.png)<!-- -->

# Messages in Public Channels by Day of Week

``` r
ggbetweenstats(
               data = r4ds_members %>% mutate(day=wday(date,label = TRUE)) %>%
                      select("messages_in_public_channels","day"), 
               x = day,
               y = messages_in_public_channels,
               xlab = "Week of the Day",
               ylab = "Count",
               title = "Comparing Daily Messages of Public Channels\nbetween Week of the Day",
               messages = FALSE
               )
```

![](R4DS_files/figure-gfm/public%20shared%20channels,%20by%20week%20of%20Day-1.png)<!-- -->

# Percent Based Messages in Public Channels by Day of Week

``` r
ggbetweenstats(
               data = r4ds_members %>% mutate(day=wday(date,label = TRUE)) %>%
                      select("percent_of_messages_public_channels","day"), 
               x = day,
               y = percent_of_messages_public_channels,
               xlab = "Week of the Day",
               ylab = "Count",
               title = "Comparing Daily Messages Percent of Public Channels\nbetween Week of the Day",
               messages = FALSE
               )
```

![](R4DS_files/figure-gfm/percent%20public%20shared%20channels,%20by%20week%20of%20Day-1.png)<!-- -->

# Messages in Private Channels by Day of Week

``` r
ggbetweenstats(
               data = r4ds_members %>% mutate(day=wday(date,label = TRUE)) %>%
                      select("messages_in_private_channels","day"), 
               x = day,
               y = messages_in_private_channels,
               xlab = "Week of the Day",
               ylab = "Count",
               title = "Comparing Daily Messages of Private Channels\nbetween Week of the Day",
               messages = FALSE
               )
```

![](R4DS_files/figure-gfm/private%20shared%20channels,%20by%20week%20of%20Day-1.png)<!-- -->

# Percent Based Messages in Private Channels by day of Week

``` r
ggbetweenstats(
               data = r4ds_members %>% mutate(day=wday(date,label = TRUE)) %>%
                      select("percent_of_messages_private_channels","day"), 
               x = day,
               y = percent_of_messages_private_channels,
               xlab = "Week of the Day",
               ylab = "Count",
               title = "Comparing Daily Messages Percent of Private Channels\nbetween Week of the Day ",
               messages = FALSE
               )
```

![](R4DS_files/figure-gfm/percent%20private%20shared%20channels,%20by%20week%20of%20Day-1.png)<!-- -->

# Messages in DM by Day of Week

``` r
ggbetweenstats(
               data = r4ds_members %>% mutate(day=wday(date,label = TRUE)) %>%
                      select("messages_in_d_ms","day"), 
               x = day,
               y = messages_in_d_ms,
               xlab = "Week of the Day",
               ylab = "Count",
               title = "Comparing Daily Messages of DM \nbetween Week of the Day",
               messages = FALSE
               )
```

![](R4DS_files/figure-gfm/DM%20channels,%20by%20week%20of%20Day-1.png)<!-- -->

# Percent Based Messages in DM by Day of Week

``` r
ggbetweenstats(
               data = r4ds_members %>% mutate(day=wday(date,label = TRUE)) %>%
                      select("percent_of_messages_d_ms","day"), 
               x = day,
               y = percent_of_messages_d_ms,
               xlab = "Week of the Day",
               ylab = "Count",
               title = "Comparing Daily Messages Percent of DM \nbetween Week of the Day ",
               messages = FALSE
               )
```

![](R4DS_files/figure-gfm/percent%20DM%20channels,%20by%20week%20of%20Day-1.png)<!-- -->

**THANK YOU**
