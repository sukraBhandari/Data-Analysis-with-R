Lesson 3 from EDA course Udacity
========================================================
## Reading in Data
```{r}
getwd()
setwd('C:/Users/..../EDA')
list.files()
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')

names(pf)

```

##histogram of Users' birthdays by days
```{r}
library(ggplot2)
qplot(data = pf, x = dob_day) + 
  scale_x_discrete(breaks=1:31)

```

##histogram of Users' birthdays by months
```{r}

qplot(data = pf, x = dob_day) + 
  scale_x_discrete(breaks=1:31)+
  facet_wrap(~dob_month, ncol = 3)

```

##friend counts without NA
```{r}
qplot(data = subset(pf,!is.na(gender)), x = friend_count, binwidth = 25)+
  geom_histogram()+
  scale_x_continuous(limits = c(0, 1000),breaks = seq(0,1000,50))+
  facet_wrap(~gender)

```

##or 
```{r}
qplot(data = pf, x = friend_count, xlim = c(0,1000))

```

## Statistics by Genger
```{r}
table(pf$gender)
by(pf$friend_count, pf$gender, summary)

```

## Tenure
```{r}
qplot(x = tenure/365, data = pf, binwidth = 0.25, color = I('black'), fill = I('#099009'))+
  scale_x_continuous(breaks = seq(1,7,1), limits = c(0,7))

```


##labeling Plots
```{r}
qplot(x = tenure/365, data = pf,
      xlab = 'No of years in FB',
      ylab = 'No. of users in sample',
      binwidth = 0.25, color = I('black'), fill = I('#099009'))+
  scale_x_continuous(breaks = seq(1,7,1), limits = c(0,7))

```


##Users Ages
```{r}
qplot(x = age, data = pf, binwidth = 1)+
  geom_histogram(color = 'black', fill = 'blue')+
  scale_x_continuous(breaks = seq(0, 113,5), limits = c(10,110))+
  xlab('Age of fb users')+
  ylab('no of users in sample')

```

## Transforming Data
```{r}
library(grid)
library(gridExtra)
p1 <- qplot(x = friend_count, data = pf)
p2 <- qplot(x = log10(pf$friend_count+1), data = pf)
p3 <- qplot(x = sqrt(pf$friend_count), data = pf)

grid.arrange(p1, p2, p3, ncol = 1)


```


##frequency polygons
```{r}
qplot(x = friend_count, y = ..count../sum(..count..),
      data = subset(pf, !is.na(gender)),
      binwidth = 10,
      xlab = 'Friend Count',
      ylab = 'Proportion of Users with friend count',
      geom = 'freqpoly', color = gender)+
  scale_x_continuous(lim = c(0,1000), breaks = seq(0, 1000,50))

```

##frequency polygons www_likes
```{r}
qplot(x = www_likes, data = subset(pf, !is.na(gender)),
      geom = 'freqpoly', color = gender)+
  scale_x_continuous()+
  scale_x_log10()

```

##likes on the web
```{r}
by(pf$www_likes, pf$gender, sum)

```

##box plots
```{r}
qplot(x = gender, y = friend_count,
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot')+
  scale_y_continuous(lim = c(0,1000))

```

## box plot with coord_cartesian
```{r}
qplot(x = gender, y = friend_count,
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot')+
  coord_cartesian(ylim = c(0,1000))
```

##box plot friendships_initiated
```{r}
qplot(x = gender, y = friendships_initiated,
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot')+
  coord_cartesian(ylim = c(0,250))


by(pf$friendships_initiated, pf$gender, summary)
```


##getting Logical
```{r}
summary(pf$mobile_likes)

summary(pf$mobile_likes > 0)

pf$mobile_check_in <- NA
pf$mobile_check_in <- ifelse(pf$mobile_likes > 0, 1, 0)


mobile_percent <- sum(pf$mobile_check_in > 0)/length(pf$mobile_check_in)


pf$mobile_check_in <- factor(pf$mobile_check_in)
summary(pf$mobile_check_in)
```






