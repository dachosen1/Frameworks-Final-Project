Frameworks Final Project: Data Exploration
================

``` r
library(knitr); library(Hmisc); library(DT); library(ggplot2); library(dplyr); 
library(reshape2); library(ggthemes); library(stringr); library(data.table); library(tidytext); library(recommenderlab); library(qdapTools); library(purrr); library(devtools)
```

``` r
name = "Name"
city.name = "City"
cuisine.name = "Cuisine Style"
ranking.name = "Ranking"
rating.name = "Rating"
price.name = "Price Range"
number.reviews.name = "Number of Reviews"
review.name = "Reviews"
```

``` r
dat <- fread(input ='Data/TA_restaurants_curated_clean.csv', verbose = FALSE)
```

``` r
city.dat <-  dat[,.(count = .N, 
                  `Average Price Range` = mean(get(price.name)), 
                  `Average Rating` = mean(get(rating.name)),
                  `Average number of reviews` = mean(get(number.reviews.name))),
                 by= city.name]

setorder(city.dat, -count)
```

``` r
ggplot(data = city.dat) + geom_bar(stat = 'identity', fill = 'firebrick4', 
                                     aes(x = reorder(City,count), y = count)) + 
 xlab('City') + ylab('Number of Restaurants')+coord_flip() + theme_get()  + 
 ggtitle('Number of Restaurants per City')
```

![](Data_Exploration_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
ggplot(data = city.dat) + geom_bar(stat = 'identity', fill = 'firebrick4', 
                                     aes(x = reorder(City,`Average Price Range`), y = `Average Price Range`)) + 
 xlab('City') + ylab('Average Price Range')+coord_flip() + theme_get()  + 
 ggtitle('Most Expensive City by Restaurant')
```

![](Data_Exploration_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
ggplot(data = city.dat) + geom_bar(stat = 'identity', fill = 'firebrick4', 
                                     aes(x = reorder(City,`Average number of reviews`), y = `Average number of reviews`)) + 
 xlab('City') + ylab('Average number of restaurants Reviews')+coord_flip() + theme_get()  + 
 ggtitle('Which City has the Most Restaurants Reviews')
```

![](Data_Exploration_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggplot(data = city.dat) + geom_bar(stat = 'identity', fill = 'firebrick4', 
                                     aes(x = reorder(City,`Average Rating`), y = `Average Rating`)) + 
 xlab('City') + ylab('Average Ratings')+coord_flip() + theme_get() + 
 ggtitle('Which City has the best Restaurants') 
```

![](Data_Exploration_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
#Correlation between review rating and longer reviews
cor_char <- cor(nchar(dat$Reviews),dat$Rating,use="complete.obs")
cor_char
```

    ## [1] 0.4377277

``` r
cor.test(nchar(dat$Reviews),dat$Rating)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  nchar(dat$Reviews) and dat$Rating
    ## t = 172.49, df = 125520, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.4332448 0.4421889
    ## sample estimates:
    ##       cor 
    ## 0.4377277

``` r
#Correlation with review length in words
cor_words= cor(str_count(string = dat$Reviews,pattern = '\\S+'),dat$Rating,use="complete.obs")
cor_words
```

    ## [1] 0.4200774

``` r
cor.test(str_count(string = dat$Reviews,pattern = '\\S+'),dat$Rating)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  str_count(string = dat$Reviews, pattern = "\\S+") and dat$Rating
    ## t = 164, df = 125520, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.4155110 0.4246226
    ## sample estimates:
    ##       cor 
    ## 0.4200774

``` r
#Correlation with review sentence
cor_sentence= cor(str_count(string = dat$Reviews,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"),dat$Rating,use="complete.obs")
cor_sentence
```

    ## [1] 0.179211

``` r
cor.test(str_count(string = dat$Reviews,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"),dat$Rating,use="complete.obs")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  str_count(string = dat$Reviews, pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]") and dat$Rating
    ## t = 64.538, df = 125520, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.1738514 0.1845600
    ## sample estimates:
    ##      cor 
    ## 0.179211

``` r
#Using lexicon bing
subdat <-  dat[,c("V1", "Rating", "Reviews")]
subdat%>%
  group_by(V1)%>%
  unnest_tokens(output = word, input = Reviews)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)
```

    ## # A tibble: 174,324 x 4
    ## # Groups:   sentiment [2]
    ##       V1 Rating word      sentiment
    ##    <int>  <dbl> <chr>     <chr>    
    ##  1     1    5   like      positive 
    ##  2     1    5   warm      positive 
    ##  3     1    5   welcome   positive 
    ##  4     2    4.5 great     positive 
    ##  5     2    4.5 perfect   positive 
    ##  6     3    4.5 delicious positive 
    ##  7     4    5   superb    positive 
    ##  8     4    5   fine      positive 
    ##  9     5    4.5 best      positive 
    ## 10     5    4.5 super     positive 
    ## # ... with 174,314 more rows

``` r
#Positive and Negative Words in Reviews
subdat%>%
  group_by(V1)%>%
  unnest_tokens(output = word, input = Reviews)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=sentiment,y=n,fill=sentiment))+geom_col()+theme_economist()+guides(fill=F)
```

![](Data_Exploration_files/figure-gfm/text%20analysis%20sentiment%20bing%20lexicon-1.png)<!-- -->

``` r
#Proportion of Positive words in Reviews
subdat %>%
  select(V1,Reviews)%>%
  group_by(V1)%>%
  unnest_tokens(output=word,input=Reviews)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))
```

    ## # A tibble: 2 x 3
    ##   sentiment      n proportion
    ##   <chr>      <int>      <dbl>
    ## 1 negative   21093      0.121
    ## 2 positive  153231      0.879

``` r
#Are positive reviews helpful?
subdat %>%
  select(V1,Reviews,Rating)%>%
  group_by(V1)%>%
  unnest_tokens(output=word,input=Reviews)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(Rating,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))%>%
  ggplot(aes(x=Rating,y=proportion,fill=sentiment))+geom_col()+theme_economist()
```

![](Data_Exploration_files/figure-gfm/text%20analysis%20sentiment%20bing%20lexicon-2.png)<!-- -->

``` r
#Emotions in ratings
subdat%>%
  group_by(V1)%>%
  unnest_tokens(output = word, input = Reviews)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+geom_col()+guides(fill=F)+coord_flip()+theme_get()
```

![](Data_Exploration_files/figure-gfm/text%20analysis%20sentiment%20nrc%20lexicon-1.png)<!-- -->

``` r
#Rating and emotions
subdat%>%
  group_by(V1)%>%
  unnest_tokens(output = word, input = Reviews)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(V1,sentiment,Rating)%>%
  count()
```

    ## # A tibble: 324,494 x 4
    ## # Groups:   V1, sentiment, Rating [324,494]
    ##       V1 sentiment    Rating     n
    ##    <int> <chr>         <dbl> <int>
    ##  1     2 anticipation    4.5     1
    ##  2     2 joy             4.5     2
    ##  3     2 positive        4.5     2
    ##  4     2 trust           4.5     2
    ##  5     3 joy             4.5     1
    ##  6     3 positive        4.5     1
    ##  7     3 trust           4.5     1
    ##  8     4 anticipation    5       1
    ##  9     4 joy             5       2
    ## 10     4 positive        5       4
    ## # ... with 324,484 more rows

``` r
#Correlation between emotion and rating
subdat%>%
  group_by(V1)%>%
  unnest_tokens(output = word, input = Reviews)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(V1,sentiment,Rating)%>%
  count()%>%
  ungroup()%>%
  group_by(sentiment)%>%
  summarize(correlation = cor(n,Rating))
```

    ## # A tibble: 10 x 2
    ##    sentiment    correlation
    ##    <chr>              <dbl>
    ##  1 anger            -0.141 
    ##  2 anticipation     -0.0182
    ##  3 disgust          -0.152 
    ##  4 fear             -0.164 
    ##  5 joy               0.0347
    ##  6 negative         -0.180 
    ##  7 positive          0.0542
    ##  8 sadness          -0.0984
    ##  9 surprise         -0.0137
    ## 10 trust             0.0196

``` r
subdat %>%
  select(V1,Reviews)%>%
  group_by(V1)%>%
  unnest_tokens(output=word,input=Reviews)%>%
  inner_join(get_sentiments('afinn'))%>%
  summarize(reviewSentiment = mean(score))%>%
  ungroup()%>%
  summarize(min=min(reviewSentiment),max=max(reviewSentiment),median=median(reviewSentiment),mean=mean(reviewSentiment))
```

    ## # A tibble: 1 x 4
    ##     min   max median  mean
    ##   <dbl> <dbl>  <dbl> <dbl>
    ## 1    -4     5      3  2.40

``` r
#Distribution of afinn lexicon scores
subdat %>%
  select(V1,Reviews)%>%
  group_by(V1)%>%
  unnest_tokens(output=word,input=Reviews)%>%
  inner_join(get_sentiments('afinn'))%>%
  summarize(reviewSentiment = mean(score))%>%
  ungroup()%>%
  ggplot(aes(x=reviewSentiment,fill=reviewSentiment>0))+
  geom_histogram(binwidth = 0.1)+
  scale_x_continuous(breaks=seq(-5,5,1))+scale_fill_manual(values=c('tomato','seagreen'))+
  guides(fill=F)+
  theme_get()
```

![](Data_Exploration_files/figure-gfm/text%20analysis%20sentiment%20affin%20lexicon-1.png)<!-- -->

``` r
subdat= dat[,c("City", "Rating", "Reviews")]

#Positive words and negative words in different cities
subdat%>%
  group_by(City)%>%
  unnest_tokens(output = word, input = Reviews)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(City, sentiment)%>%
  count()%>%
  ggplot(aes(x=City,y=n,fill=sentiment))+geom_col()+theme_economist()+guides(fill=F)+ coord_flip()
```

![](Data_Exploration_files/figure-gfm/text%20analysis%20by%20city-1.png)<!-- -->

``` r
#Similar distribution of emotions
subdat%>%
  group_by(City)%>%
  unnest_tokens(output = word, input = Reviews)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment, City)%>%
  count()%>%
  ggplot(aes(x=City,y=n,fill=sentiment))+geom_bar(position = "fill", stat='identity')+guides(fill=F)+coord_flip()+theme_get()
```

![](Data_Exploration_files/figure-gfm/text%20analysis%20by%20city-2.png)<!-- -->

``` r
#Sentiment mean, median, max and min 
subdat %>%
  select(City,Reviews)%>%
  group_by(City)%>%
  unnest_tokens(output=word,input=Reviews)%>%
  inner_join(get_sentiments('afinn'))%>%
  summarize(reviewMeanSentiment = mean(score), 
            reviewMedianSentiment = median(score), reviewMaxSentiment = max(score),
            reviewMinSentiment = min(score))
```

    ## # A tibble: 31 x 5
    ##    City  reviewMeanSenti~ reviewMedianSen~ reviewMaxSentim~
    ##    <chr>            <dbl>            <dbl>            <dbl>
    ##  1 Amst~             2.52                3                5
    ##  2 Athe~             2.56                3                5
    ##  3 Barc~             2.39                3                5
    ##  4 Berl~             2.46                3                5
    ##  5 Brat~             2.26                3                5
    ##  6 Brus~             2.27                3                5
    ##  7 Buda~             2.34                3                5
    ##  8 Cope~             2.47                3                5
    ##  9 Dubl~             2.39                3                5
    ## 10 Edin~             2.39                3                5
    ## # ... with 21 more rows, and 1 more variable: reviewMinSentiment <dbl>

``` r
#Look at correlation between rating and ranking
cor_rank=cor(dat$Ranking,dat$Rating,use="complete.obs")
cor_rank
```

    ## [1] -0.7200154

``` r
#Trends- number of vegan options and gluten-free options?
dat[get("Gluten Free Options") == 1, .N, by=city.name]
```

    ##          City N
    ##  1: Barcelona 2
    ##  2:  Budapest 1
    ##  3:    London 2
    ##  4:      Lyon 2
    ##  5:    Madrid 4
    ##  6:     Milan 3
    ##  7:    Oporto 1
    ##  8:     Paris 1
    ##  9:    Prague 1
    ## 10: Stockholm 1

``` r
dat[get("Vegan Options") == 1, .N, by=city.name]
```

    ##           City N
    ##  1:  Amsterdam 1
    ##  2:     Athens 2
    ##  3:  Barcelona 1
    ##  4:     Berlin 6
    ##  5: Copenhagen 2
    ##  6:     Geneva 1
    ##  7:    Hamburg 2
    ##  8:  Ljubljana 2
    ##  9:     London 1
    ## 10:     Madrid 5
    ## 11:      Milan 9
    ## 12:     Munich 2
    ## 13:      Paris 2
    ## 14:     Prague 1
    ## 15:       Rome 3
    ## 16:     Vienna 3
    ## 17:     Warsaw 1

``` r
dat[get( "Healthy") == 1, .N, by=city.name]
```

    ##           City  N
    ##  1:  Amsterdam 20
    ##  2:     Athens  5
    ##  3:  Barcelona 39
    ##  4:     Berlin 12
    ##  5: Bratislava  9
    ##  6:   Brussels 10
    ##  7:   Budapest 21
    ##  8: Copenhagen  9
    ##  9:     Dublin  2
    ## 10:  Edinburgh  4
    ## 11:     Geneva  4
    ## 12:    Hamburg  4
    ## 13:   Helsinki  3
    ## 14:     Krakow  7
    ## 15:     Lisbon 24
    ## 16:     London 59
    ## 17: Luxembourg  3
    ## 18:       Lyon  5
    ## 19:     Madrid 36
    ## 20:      Milan 16
    ## 21:     Munich 12
    ## 22:     Oporto  7
    ## 23:       Oslo  2
    ## 24:      Paris 25
    ## 25:     Prague  5
    ## 26:       Rome  9
    ## 27:  Stockholm  7
    ## 28:     Vienna  8
    ## 29:     Warsaw  9
    ## 30:     Zurich  8
    ##           City  N
