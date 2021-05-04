Project 2
================

## Introduction

As a college student there is no topic I am more familiar with than
cheap packaged foods. The most quintessential of which being ramen. Be
it cup, pack, bowl, tray, you name it i’ve probably tried it. Which
brings me to the topic of my research. The Ramen Rater is a product
review website for the hardcore ramen enthusiast (or “ramenphile”), with
thousands of reviews. The data set in question compiles the data from
“The Big List” of reviews and was found at this link:
<https://www.theramenrater.com/>.

Each observation in the dataset is a single ramen product review. The
variables include review numbers, brand, variety, country, style, and
stars. Review numbers are contiguous identification numbers with more
recently reviewed ramen varieties having higher numbers. Brand is the
company responsible for the product. Variety is also the product name
(i.e . curry chicken, yoba vegetable). Country, is the country in which
the product is distributed. Style is categorized into cup, bowl, tray,
and pack and is the way the ramen is packaged. Lastly, stars are the
quality of the ramen as assessed by a reviewer on the website.

In total the dataset covers 2577 reviews. Luckily tidying was minimal as
there are no duplicate reviews with the only the removal of 3
observations that were unrated. I’m interested in finding out what
countries have the highest rated ramen, considering the origin of the
food I would guess Japan. Additionally, it would be interesting to see
what the most common words are in advertising the variety of ramen and
other demographic information.

``` r
# Data overview
library(readr)
ramen.ratings <- read.csv("C:/Users/dango/Downloads/ramen-ratings.csv")
library(dplyr)

# Renaming Rating Column
ramen.ratings <-ramen.ratings %>%
  rename(Review = Review..)

# Cleaning Data, Removing 3 unrated Ramen
ramen.ratings <- ramen.ratings[-c(33,123,994), ]

# Cleaning Data, Fixing top 10 column to be numerical
ramen.ratings <- ramen.ratings %>%
  mutate(Top.Ten = ifelse(is.na(Top.Ten), 0, Top.Ten))

#Convert to STars to numeric
ramen.ratings<- transform(ramen.ratings, Stars = as.numeric(Stars))

# Top rated ramen
ramen.ratings %>%
  group_by(Variety, Stars) %>%
  arrange(desc(Stars))
```

    ## # A tibble: 2,577 x 7
    ## # Groups:   Variety, Stars [2,525]
    ##    Review Brand      Variety                        Style Country  Stars Top.Ten
    ##     <int> <chr>      <chr>                          <chr> <chr>    <dbl>   <dbl>
    ##  1   2570 Tao Kae N~ Creamy tom Yum Kung Flavour    Pack  Thailand     5       0
    ##  2   2569 Yamachan   Yokohama Tonkotsu Shoyu        Pack  USA          5       0
    ##  3   2566 Nissin     Demae Ramen Bar Noodle Aka To~ Pack  Hong Ko~     5       0
    ##  4   2563 Yamachan   Tokyo Shoyu Ramen              Pack  USA          5       0
    ##  5   2559 Jackpot T~ Beef Ramen                     Pack  USA          5       0
    ##  6   2558 KOKA       Creamy Soup With Crushed Nood~ Cup   Singapo~     5       0
    ##  7   2552 MyKuali    Penang White Curry Rice Vermi~ Bowl  Malaysia     5       0
    ##  8   2550 Samyang F~ Paegaejang Ramen               Pack  South K~     5       0
    ##  9   2545 KOKA       Instant Noodles Laksa Singapu~ Pack  Singapo~     5       0
    ## 10   2543 KOKA       Curry Flavour Instant Noodles  Cup   Singapo~     5       0
    ## # ... with 2,567 more rows

## EDA

I explored the various relationships in the data to find intersting
relationships and summary statistics for important variables.

``` r
# Summary Statistics of Major COuntries
summary1<- ramen.ratings %>%
  group_by(Country) %>%
  summarise(
          count = n(),
          min = min(Stars),
          mean = mean(Stars),
          sd = sd(Stars),
          max = max(Stars)
            )
head(summary1)
```

    ## # A tibble: 6 x 6
    ##   Country    count   min  mean    sd   max
    ##   <chr>      <int> <dbl> <dbl> <dbl> <dbl>
    ## 1 Australia     22  1     3.14 0.895     5
    ## 2 Bangladesh     7  3.25  3.71 0.366     4
    ## 3 Brazil         5  4     4.35 0.418     5
    ## 4 Cambodia       5  3.5   4.2  0.758     5
    ## 5 Canada        41  0     2.24 1.30      5
    ## 6 China        169  0     3.42 1.16      5

``` r
# Relationship between Brand and Rating
brandmeans<-ramen.ratings %>%
  group_by(Brand) %>%
  summarise(Mean = mean(Stars)) %>%
  arrange(desc(Mean))
head(brandmeans)
```

    ## # A tibble: 6 x 2
    ##   Brand             Mean
    ##   <chr>            <dbl>
    ## 1 ChoripDong           5
    ## 2 Daddy                5
    ## 3 Daifuku              5
    ## 4 Foodmon              5
    ## 5 Higashi              5
    ## 6 Jackpot Teriyaki     5

``` r
# Relationship between Style and Rating
stylemeans<-ramen.ratings %>%
  group_by(Style) %>%
  summarise(Mean = mean(Stars))
head(stylemeans)
```

    ## # A tibble: 6 x 2
    ##   Style  Mean
    ##   <chr> <dbl>
    ## 1 Bar    5   
    ## 2 Bowl   3.67
    ## 3 Box    4.29
    ## 4 Can    3.5 
    ## 5 Cup    3.50
    ## 6 Pack   3.70

``` r
library(ggplot2)
ggplot(data = ramen.ratings, 
       aes(x = Style, y = Stars, 
           color = Style)) +    
  geom_boxplot() +
  ggtitle("Relationship between Style and Rating")
```

![](boop_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
# Relationship between Country and Rating
library(tidyverse)
library(hrbrthemes)
library(viridis)
ramen.ratings %>%
  ggplot( aes(x=Country, y=Stars, fill=Country)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    geom_jitter(color="black", size=0.4, alpha=0.9)+
    theme(
      legend.position="center",
      plot.title = element_text(size=11), axis.text.x = element_text(angle = 60, hjust = 1)
    ) +
    ggtitle("Distribution of Ramen Ratings by Country") +
    xlab("Country")
```

![](boop_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
#Pearson χ2 test finding correlation between Types of Ramen and Country of distribution
chisq.test(ramen.ratings$Style, ramen.ratings$Country)
```

    ## Warning in chisq.test(ramen.ratings$Style, ramen.ratings$Country): Chi-squared
    ## approximation may be incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  ramen.ratings$Style and ramen.ratings$Country
    ## X-squared = 691.34, df = 216, p-value < 2.2e-16

``` r
# Correlation between Review(how recent the review was) and Star Rating)
data <- data.frame(ramen.ratings$Review, ramen.ratings$Stars)
names(data) <- c("Review", "Stars")
res <- cor(data)
round(res, 2)
```

    ##        Review Stars
    ## Review   1.00  0.24
    ## Stars    0.24  1.00

## MANOVA

Perform a MANOVA test and if significant perform ANOVAs and post-hoc t
tests. Interpret p-values after correction and briefly discuss
assumptions.WS13

First I performed a MANOVA test to find whether Recentness of Review or
Star Rating differ based on the Style of Ramen product. The null
hypothesis here is that the means of Review and Star rating are equal at
every Style of Ramen; the alternative is that at least one response
variable differs. I was able to analyze the results. Review had a
p-value of 2.2e-16 and Star rating had a p-value of 0.00375, both were
significant so I was conducted post-hoc analysis using pairwise t tests
to see how much of the actual variance was due to the variable. After
correcting for the assumptions and Type 1 error using Bonferroni, the
recentness of review was significant between bowl and box, bowl and cup,
bowl and tray, box and pack, box and tray, cup and pack, cup and tray,
and pack and tray. Meanwhile the star rating of the product was
significantly different between the styles cup and pack. Although we
were able to perform the test there are assumptions of equal variance
and normality that may not be met by the data.

``` r
# whether Recentness of Review or Star Rating differ based on the Style of Ramen product
summary(manova(cbind(Review,Stars)~Style,data=ramen.ratings))
```

    ##             Df   Pillai approx F num Df den Df    Pr(>F)    
    ## Style        6 0.047289   10.373     12   5140 < 2.2e-16 ***
    ## Residuals 2570                                              
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#yes, there is a mean difference in either Review Recentness or Stars for the Style of Ramen 

# Is the recentness significant?
summary(aov(Review~Style,data=ramen.ratings))
```

    ##               Df    Sum Sq Mean Sq F value Pr(>F)    
    ## Style          6 4.808e+07 8013334   14.92 <2e-16 ***
    ## Residuals   2570 1.380e+09  536969                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Is the Star Rating significant?
summary(aov(Stars~Style,data=ramen.ratings))
```

    ##               Df Sum Sq Mean Sq F value  Pr(>F)   
    ## Style          6   19.8   3.302   3.219 0.00375 **
    ## Residuals   2570 2635.8   1.026                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Since ANOVA is significant then we can perform post-hoc analysis
# Filter out any groups that occur only once before doing the t-test (such tests wouldn't be meaningful anyway) in order to perform post-hoc
ramen_filtered <- ramen.ratings %>%
  group_by(Style) %>%
  filter(n() >= 2) %>%
  ungroup()

# Type 1 error Calculation
1-(0.95^2)
```

    ## [1] 0.0975

``` r
# To interpret the p-values, I accounted for Type 1 error using Bonferroni alpha

# For Review Recentness
pairwise.t.test(ramen_filtered$Review,ramen_filtered$Style, p.adj="bonferroni")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  ramen_filtered$Review and ramen_filtered$Style 
    ## 
    ##      Bowl    Box     Cup     Pack   
    ## Box  0.00768 -       -       -      
    ## Cup  4.2e-10 0.20987 -       -      
    ## Pack 1.00000 0.01151 1.3e-11 -      
    ## Tray 0.03532 0.00055 4.4e-11 0.00269
    ## 
    ## P value adjustment method: bonferroni

``` r
# For Star Rating
pairwise.t.test(ramen_filtered$Stars,ramen_filtered$Style, p.adj="bonferroni")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  ramen_filtered$Stars and ramen_filtered$Style 
    ## 
    ##      Bowl   Box    Cup    Pack  
    ## Box  1.0000 -      -      -     
    ## Cup  0.0958 0.5679 -      -     
    ## Pack 1.0000 1.0000 0.0021 -     
    ## Tray 1.0000 0.7895 1.0000 1.0000
    ## 
    ## P value adjustment method: bonferroni

## Randomization Test

The randomization test I performed was a PERMANOVA. This is because the
data in question does not meet the all the assumptions required for
fully conclusive results with the MANOVA. If the actual F statistic is
far away from the majority of the F statistics that arise in the
simulation distribution, where there is no difference between the
Styles, then there is evidence that my response variables, Star rating
differ between those groups. For this test I focused in on the two most
popular forms of ramen available, cup and pack. The null hypothesis is
that there is no significant difference in Star rating between cup and
pack styles of ramen. The resulting f statistic was 4.732812. Since the
value is above 3.95 I can reject the null hypothesis and determine there
is a significant difference in star rating between cup and pack style
ramen. The resulting f statistics were also quite large, with the
original value not even appearing on the plot of the null
distribution.THis supports the conclusion that there is a difference
between the groups.

``` r
#permutation ANOVA
## f stat by hand
euc_dist<-dist(ramen.ratings[,c("Stars")],method="euclid")
euc_dist_auto<-dist(ramen.ratings[ramen.ratings$Style=="Pack",c("Stars")],method="euclid")
euc_dist_manual<-dist(ramen.ratings[ramen.ratings$Style=="Cup",c("Stars")],method="euclid")

SSE<-sum(euc_dist_auto^2)/13+sum(euc_dist_manual^2)/19
SST<-sum(euc_dist^2)/32

Fstat=(SST-SSE)/(SSE/30)
Fstat
```

    ## [1] 4.679319

``` r
## sampling distribution
set.seed(123)
sampdist<-replicate(1000,{

randramen<-ramen.ratings
randramen$Style<-sample(randramen$Style)

euc_dist<-dist(randramen[,c("Stars")],method="euclid")
euc_dist_cup<-dist(randramen[randramen$Style=="Cup",c("Stars")],method="euclid")
euc_dist_pack<-dist(randramen[randramen$Style=="Pack",c("Stars")],method="euclid")

SSR<-sum(euc_dist_cup^2)/13+sum(euc_dist_pack^2)/19
SST<-sum(euc_dist^2)/32

(SST-SSR)/(SSR/30)
} )

mean(sampdist>Fstat)
```

    ## [1] 1

``` r
# plot visualizing the null distribution and the test statistic
hist(sampdist, prob=T, main= "Distribution of Sampled F values");abline(v= Fstat,col='red')
```

![](boop_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
library(vegan)
```

    ## Warning: package 'vegan' was built under R version 4.0.5

    ## Warning: package 'permute' was built under R version 4.0.5

``` r
adonis(euc_dist~Style,data=ramen.ratings)
```

    ## 
    ## Call:
    ## adonis(formula = euc_dist ~ Style, data = ramen.ratings) 
    ## 
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ##             Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)  
    ## Style        6     19.81  3.3018  3.2194 0.00746  0.015 *
    ## Residuals 2570   2635.78  1.0256         0.99254         
    ## Total     2576   2655.59                 1.00000         
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Linear Regression

Next, I built a regression model to see if the ramen style or country
can predict the Star rating. In order to do this I In order to
compensate for the failed assumptions a robust SE and bootstrapped SE
was calculated.

``` r
# Mean Center Star Rating variable
ramen.ratings$centerStars <- ramen.ratings$Stars - mean(ramen.ratings$Stars, na.rm = TRUE)

## 1. Checking assumptions
### a. Visually
fit <- lm(centerStars ~ Style + Country, data=ramen.ratings)

# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) 
# optional 4 graphs/page
plot(fit)
```

    ## Warning: not plotting observations with leverage one:
    ##   67, 1423

    ## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced

    ## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced

![](boop_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
### b. Normality

# Shapiro-Wilk test
# H0: normality
shapiro.test(fit$residuals)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  fit$residuals
    ## W = 0.94051, p-value < 2.2e-16

``` r
# Kolmogorov-Smirnov test
# H0: normality
ks.test(fit$residuals, "pnorm", mean=0, sd(fit$residuals))
```

    ## Warning in ks.test(fit$residuals, "pnorm", mean = 0, sd(fit$residuals)): ties
    ## should not be present for the Kolmogorov-Smirnov test

    ## 
    ##  One-sample Kolmogorov-Smirnov test
    ## 
    ## data:  fit$residuals
    ## D = 0.087968, p-value < 2.2e-16
    ## alternative hypothesis: two-sided

``` r
#Data is not normally distributed

### c. Equal variance (homoscedasticity)

library(sandwich)
```

    ## Warning: package 'sandwich' was built under R version 4.0.5

``` r
library(lmtest)
```

    ## Warning: package 'lmtest' was built under R version 4.0.5

    ## Warning: package 'zoo' was built under R version 4.0.5

``` r
# Breusch-Pagan test
# H0: homoscedasticity
bptest(fit)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  fit
    ## BP = 75.79, df = 42, p-value = 0.001076

``` r
# Data is not homoscedastic
```

## 2. Robust Standard Errors

``` r
# Visualize the relationship


# Fit a regression model

# Breusch-Pagan test for homoscedasticity


# Uncorrected Standard Errors


# Robust Standard Errors
```

## 3. Bootstrap Standard Errors

``` r
# When assumptions are violated (homoscedasticity, normality, small sample size)
# use bootstrap samples to estimate coefficients, SEs, fitted values, ...

# Use the function replicate to repeat the process (similar to a for loop)

# Estimated SEs

  # Transpose the obtained matrices
 
  # Consider the matrix as a data frame

  # Compute the standard error (standard deviation of the sampling distribution)


# Compare to original fit
confint(fit, level = 0.95)
```

    ##                          2.5 %      97.5 %
    ## (Intercept)        -0.83348858  2.99104754
    ## StyleBowl          -3.39273589  0.34932648
    ## StyleBox           -3.21796091  0.82020960
    ## StyleCan           -4.13953370  1.13953370
    ## StyleCup           -3.49384990  0.24854201
    ## StylePack          -3.37023114  0.36986990
    ## StyleTray          -3.47797196  0.27421355
    ## CountryBangladesh  -0.33306082  1.29508256
    ## CountryBrazil       0.24017658  2.09125240
    ## CountryCambodia     0.03840742  1.89504289
    ## CountryCanada      -1.42914948 -0.43963244
    ## CountryChina       -0.21750370  0.64022554
    ## CountryColombia    -0.74047353  0.97973051
    ## CountryDubai       -0.80157066  1.50168763
    ## CountryEstonia     -1.11416026  1.64761057
    ## CountryFiji        -0.37609225  1.65954256
    ## CountryFinland     -0.80157066  1.50168763
    ## CountryGermany     -0.08195215  0.99297333
    ## CountryGhana       -1.11416026  1.64761057
    ## CountryHolland     -0.68859225  1.34704256
    ## CountryHong Kong    0.17691240  1.04052486
    ## CountryHungary     -0.36521368  1.12088622
    ## CountryIndia       -0.35148159  0.69895900
    ## CountryIndonesia    0.41615931  1.28825346
    ## CountryJapan        0.36124201  1.19305824
    ## CountryMalaysia     0.50426417  1.36520579
    ## CountryMexico       0.02429489  1.11612341
    ## CountryMyanmar      0.09855637  1.38023966
    ## CountryNepal       -0.32304342  0.96363659
    ## CountryNetherlands -1.35335714 -0.09753655
    ## CountryNigeria     -3.64341334  0.17686365
    ## CountryPakistan    -0.97632479  0.50977511
    ## CountryPhilippines -0.37581749  0.59884798
    ## CountryPoland      -0.62609225  1.40954256
    ## CountrySarawak     -0.05157066  2.25168763
    ## CountrySingapore    0.48620062  1.36535231
    ## CountrySouth Korea  0.16670616  1.00122719
    ## CountrySweden      -1.13490399  1.16835430
    ## CountryTaiwan       0.01381525  0.86246036
    ## CountryThailand    -0.23540096  0.61208873
    ## CountryUK          -0.63686210  0.27936153
    ## CountryUSA         -0.14962858  0.68271765
    ## CountryVietnam     -0.47279223  0.41205116

## Logistic Regression

Lastly, I built a regression model to predict a binary response.
