---
title: "Class 1-3: Review of Logistic Regression"
author: "Health Data Analysis Practicum (AS.280.347)"
date: "January 28, 2026"
output: 
  html_document:
    toc: true
    toc_float: 
      toc_collapsed: true
    toc_depth: 3
    number_sections: false
    keep_md: yes
---



## Preliminaries

First we load the packages that we will be using in this document. It's good practice to load packages as the beginning so they are all in the same place. If you decide later you need an additional package, add it to the top of the document!


``` r
library(tidyverse)  # core group of tidyverse packages
library(broom) # package to make tidy model output
library(kableExtra)  # to make nice tables
```

## Module 1: Smoking and the risk of disease

Questions of interest:

-   **Question 1.1:** How does the risk of disease compare for smokers and otherwise similar non-smokers?

<center>![](Q1_dag.png){width="500px"}</center>

-   **Queston 1.2:** Does the contribution of smoking to the risk of disease vary by sex or socio-economic status (SES)?

<center>![](Q2_dag.png){width="500px"}</center>

To address each question we want:

-   A data display (graph or table)
-   A statistical analysis (with interprepration)

We will answer these questions using data from the National Medical Expenditures Survey (NMES)

## Review of logistic regression

In Public Health Biostatistics, we asked the question:

**Is the infant mortality risk (or odds) higher for twins than singleton births?**

To answer this, we used a data set from a study of infant mortality in Nepal, specifically from the [NNIPS2 study](https://pmc.ncbi.nlm.nih.gov/articles/PMC27760/){target="_blank"}. 

We could answer this question using a 2x2 table:

|          | Singleton | Twin | Total |
|:---------|----------:|-----:|------:|
| Survived |      8899 |  187 |  9086 |
| Died     |       526 |   71 |   597 |
| Total    |      9425 |  258 |  9683 |

In this case when we talk about **risk**:

-   Risk of death for twins: 71/258 = 0.28
-   Risk of death for singletons: 526/9425 = 0.056
-   Relative risk (risk ratio) of deaths for twins as compared to singletons: $$RR = \frac{risk \ for \ twins}{risk \ for \ singletons} = \frac{71/258}{526/9425} = 4.93$$
-   The risk of death for twins is almost 5 times the risk of death for singletons.

In this case when we talk about **odds**:

-   Odds of death for twins: 71/187 = 0.38
-   Odds of death for singletons: 526/8899 = 0.059
-   Odds ratio of death for twins as compared to singletons: $$OR = \frac{odds \ for \ twins}{odds \ for \ singletons} = \frac{71/187}{526/8899} = 6.42$$
-   The odds of death for twins is more than 6 times the odds of death for singletons.

We could also answer this question using logistic regression. **Logistic regression is appropriate when the outcome variable of interest is binary (or dichotomous).** In this case our outcome variable is "whether the infant died or survived", which has two possible values (died/survived). Our logistic regression equation then looks at the relationship between the log odds of the outcome and the predictor variables of interest:

$$log(odds \ of \ death) = \beta_0 + \beta_1 \cdot twin$$

$$twin = \left\{
\begin{matrix}
1 & if \ twin\\
0 & if \ not \ twin
\end{matrix}
\right.$$

We can fit this logistic regression in R using the `glm()` function. First we read in the data (`nepalData2019.csv`) and then fit the model using the syntax `y ~ x`, where `y` is the outcome variable (`death` in this case) and `x` is the predictor variable (`twins` in this case).



``` r
nepal_data <- read_csv("module_1/nepalData2019.csv")
nepal_data
```

```
## # A tibble: 10,295 × 9
##    death  male parity gestage death180 twins nblind treat firstbirth
##    <dbl> <dbl>  <dbl>   <dbl>    <dbl> <dbl>  <dbl> <dbl>      <dbl>
##  1     0     1      0      37        0     0      0     3          1
##  2     0     1      0      37        0     0      0     1          1
##  3     0     0      0      36        0     0      0     3          1
##  4     1     1      0      33        1     0      0     2          1
##  5    NA    NA      0      36        0     0      1     1          1
##  6     1     1      0      32        1     1      0     1          1
##  7     0     1      0      32        0     0      0     2          1
##  8     0     1      0      41        0     0      0     3          1
##  9     1     1      0      40        1     0      0     2          1
## 10     0     1      0      38        0     0      0     2          1
## # ℹ 10,285 more rows
```



``` r
model1 <- glm(death ~ twins, 
              family=binomial(link="logit"), 
              data=nepal_data)
summary(model1)
```

```
## 
## Call:
## glm(formula = death ~ twins, family = binomial(link = "logit"), 
##     data = nepal_data)
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -2.66247    0.04181  -63.68   <2e-16 ***
## twins        1.85843    0.14007   13.27   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4984.5  on 9644  degrees of freedom
## Residual deviance: 4848.7  on 9643  degrees of freedom
##   (650 observations deleted due to missingness)
## AIC: 4852.7
## 
## Number of Fisher Scoring iterations: 5
```

From this output we can interpret as follows:

-   The "slope" parameter: \begin{align}
    1.86 &= \beta_1 \\
    &= log(odds \ of \ death \ for \ twin=1) - log(odds \ of \ death \ for \ twin=0) \\
    &= log(odds \ of \ death \ for \ twins) - log(odds \ of \ death \ for \ singletons) \\
    &= \log(\frac{odds \ for \ twins}{odds \ for \ singletons}) \\
    &= \log(OR) = \log(6.41)
    \end{align}

-   The difference in the log odds of death, comparing twins to singleton births, is 1.86.

We'd rather interpret our results on the odds rather than log odds scale. To do this we need exponentiate our coefficients:


``` r
exp(coef(model1))
```

```
## (Intercept)       twins 
##   0.0697754   6.4136334
```

-   $6.41 = e^{1.86} = e^{\beta_1} = OR$
-   The odds of death for twins is 6.41 times the odds of death for singleton births.
-   The odds of death is 541% higher for twins than for singleton births.

We also asked: **Does the odds of death increase with increasing gestational age?**

$$\log(odds \ of \ death) = \beta_0 + \beta_1 \cdot (gestational \ age)$$

From this equation:

-   $\log(odds \ of \ death | ga = 41 \ weeks) = \beta_0 + \beta_1 \cdot (41)$
-   $\log(odds \ of \ death | ga = 40 \ weeks) = \beta_0 + \beta_1 \cdot (40)$
-   Difference: $$\log(odds \ of \ death | ga = 41 \ weeks) - \log(odds \ of \ death | ga = 40 \ weeks)$$ $$= (\beta_0 + 41\beta_1) - (\beta_0 + 40\beta_1) = \beta_1$$
-   Log odds ratio: $$\log(OR) = \log(\frac{odds|ga=41}{odds|ga=40})$$ $$= \log(odds \ of \ death | ga = 41 \ weeks) - \log(odds \ of \ death | ga = 40 \ weeks) = \beta_1$$
-   Odds ratio: $OR = e^{\log(OR)} = e^{\beta_1}$

When we fit this logistic regression in R with `death ~ gestage` we get this:


``` r
model2 <- glm(death ~ gestage, 
              family=binomial(link="logit"), 
              data=nepal_data)
summary(model2)
```

```
## 
## Call:
## glm(formula = death ~ gestage, family = binomial(link = "logit"), 
##     data = nepal_data)
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  2.48651    0.38646   6.434 1.24e-10 ***
## gestage     -0.13665    0.01056 -12.934  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4607.6  on 9147  degrees of freedom
## Residual deviance: 4443.5  on 9146  degrees of freedom
##   (1147 observations deleted due to missingness)
## AIC: 4447.5
## 
## Number of Fisher Scoring iterations: 5
```

``` r
exp(coef(model2))
```

```
## (Intercept)     gestage 
##  12.0192084   0.8722779
```

From this output we can interpret as follows:

-   $-0.1367 = \beta_1 = \log(OR)$
-   An additional week of gestational age is associated with a decrease of 0.14 in the log odds of death.
-   $0.87 = e^{-0.1367} = e^{\beta_1} = OR$
-   An additional week of gestational age is associated with a 13% decrease in the odds of infant death.

### Getting nice regression output in R

One thing you might notice when fitting regression models in R is that the output isn't very nice for displaying in a report, especially when we want to then exponentiate the coefficients. For example in our logistic regression of mortality of age, our output looks like this:


``` r
model2 <- glm(death ~ gestage, 
              family=binomial(link="logit"), 
              data=nepal_data)
summary(model2)
```

```
## 
## Call:
## glm(formula = death ~ gestage, family = binomial(link = "logit"), 
##     data = nepal_data)
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  2.48651    0.38646   6.434 1.24e-10 ***
## gestage     -0.13665    0.01056 -12.934  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4607.6  on 9147  degrees of freedom
## Residual deviance: 4443.5  on 9146  degrees of freedom
##   (1147 observations deleted due to missingness)
## AIC: 4447.5
## 
## Number of Fisher Scoring iterations: 5
```

``` r
exp(coef(model2))
```

```
## (Intercept)     gestage 
##  12.0192084   0.8722779
```

But we really don't want to display all of this information in our report. It can be much easier to work with model output like this if we first tidy it into a data frame or tibble structure. The `tidy()` function in the `broom` package will do this for output from a `glm` object, and many of the other of the types of model outputs available in R. This is why we loaded the `broom` package in our preliminaries.

Look what happens when we use the `tidy()` function on our model:


``` r
model2 <- glm(death ~ gestage, 
              family=binomial(link="logit"), 
              data=nepal_data)
tidy(model2)
```

```
## # A tibble: 2 × 5
##   term        estimate std.error statistic  p.value
##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)    2.49     0.386       6.43 1.24e-10
## 2 gestage       -0.137    0.0106    -12.9  2.88e-38
```

Now our output is organized nicely in a table format that we could present in our report:


``` r
tidy(model2) %>% 
  kable(digits = 3, format = "markdown")
```



|term        | estimate| std.error| statistic| p.value|
|:-----------|--------:|---------:|---------:|-------:|
|(Intercept) |    2.487|     0.386|     6.434|       0|
|gestage     |   -0.137|     0.011|   -12.934|       0|

And since the output is organized into a tibble, we can easily `select()`, `filter()`, `mutate()`, and `rename()` to get our table to look the way we want!


``` r
model2 %>%
  tidy() %>%
  filter(term == "gestage") %>%
  select(Term = term, Coefficient = estimate, `p-value` = p.value) %>%
  kable(digits = 3, format = "markdown")
```



|Term    | Coefficient| p-value|
|:-------|-----------:|-------:|
|gestage |      -0.137|       0|

You can also use the `tidy()` function to exponentiate your coefficients and add confidence intervals to your table! You can type `?tidy.glm` to see the full range of options available for the `glm` version of the `tidy()` function:


``` r
?tidy.glm
```

In this case we can exponentiate using `exponentiate = TRUE` and add a 95% confidence interval using `conf.int = TRUE` and `conf.level = 0.95`:


``` r
model2 %>%
  tidy(exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95)
```

```
## # A tibble: 2 × 7
##   term        estimate std.error statistic  p.value conf.low conf.high
##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
## 1 (Intercept)   12.0      0.386       6.43 1.24e-10    5.62     25.6  
## 2 gestage        0.872    0.0106    -12.9  2.88e-38    0.854     0.891
```

And, again, since we can filter and rename the columns, we could make it look something like this:


``` r
model2 %>%
  tidy(exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>%
  filter(term == "gestage") %>%
  mutate(conf.int = paste0("(", round(conf.low, 2), ", ", round(conf.high,2), ")")) %>%
  select(Term = term, OR = estimate, `p-value` = p.value, `95% CI` = conf.int) %>%
  kable(digits = 3, format = "markdown")
```



|Term    |    OR| p-value|95% CI       |
|:-------|-----:|-------:|:------------|
|gestage | 0.872|       0|(0.85, 0.89) |

Part of doing a statistical analysis is thinking about how to present the results of your analysis to your audience: what information should you show them and how should you show it!

## Logistic regression to account for possible confounding

Thinking back to our questions of interest, **what about the otherwise similar part?** How could we account for any potential confounding variables in a logistic regression analysis?

We'll talk about three potential ways to do this:

(1) We could include potential confounding variables as covariates in our analysis using multivariable logistic regression.
(2) We could stratify by potential confounding variables to create "otherwise similar" groups
(3) We could use a measure called a "propensity score" to group people into "otherwise similar" groups

We will talk about methods (2) and (3) next week. Today, let's look at (1).

To account for confounding, we can include potential confounding variables as covariates in our analysis using multivariable logistic regression:

$$\log(odds \ of \ death) = \beta_0 + \beta_1 \cdot (gestational \ age) + \beta_2 \cdot twin + \beta_3 \cdot male$$

-   We interpret the regression coefficients in a multivariable model as **ceteris paribus** -- holding all other things equal
-   $\beta_1=\log(OR)$ for a one-unit change in gestational age, **holding twin status and sex constant**
-   $\beta_2=\log(OR)$ comparing twins to singleton births, **holding gestational age and sex constant**
-   $\beta_3=\log(OR)$ comparing male infants to female infants, **holding gestational age and twin status constant**

Let's fit this model and practice interpreting the results:


``` r
model3 <- glm(death ~ gestage + twins + male, 
              family=binomial(link="logit"), 
              data=nepal_data)

summary(model3)
```

```
## 
## Call:
## glm(formula = death ~ gestage + twins + male, family = binomial(link = "logit"), 
##     data = nepal_data)
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  2.09394    0.39596   5.288 1.23e-07 ***
## gestage     -0.12836    0.01068 -12.017  < 2e-16 ***
## twins        1.64831    0.15139  10.888  < 2e-16 ***
## male        -0.01347    0.08427  -0.160    0.873    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4596.6  on 9143  degrees of freedom
## Residual deviance: 4338.0  on 9140  degrees of freedom
##   (1151 observations deleted due to missingness)
## AIC: 4346
## 
## Number of Fisher Scoring iterations: 5
```

``` r
tidy(model3)
```

```
## # A tibble: 4 × 5
##   term        estimate std.error statistic  p.value
##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)   2.09      0.396      5.29  1.23e- 7
## 2 gestage      -0.128     0.0107   -12.0   2.88e-33
## 3 twins         1.65      0.151     10.9   1.31e-27
## 4 male         -0.0135    0.0843    -0.160 8.73e- 1
```

``` r
model3 %>%
  tidy(exponentiate = TRUE, conf.int = TRUE, conf.level = 0.96) %>%
  filter(term != "(Intercept)") %>%
  mutate(conf.int = paste0("(", round(conf.low, 2), ", ", round(conf.high,2), ")")) %>%
  select(Term = term, OR = estimate, `p-value` = p.value, `95% CI` = conf.int) %>%
  kable(digits = 3, format = "markdown")
```



|Term    |    OR| p-value|95% CI       |
|:-------|-----:|-------:|:------------|
|gestage | 0.880|   0.000|(0.86, 0.9)  |
|twins   | 5.198|   0.000|(3.79, 7.06) |
|male    | 0.987|   0.873|(0.83, 1.17) |

## Assignment 1.2

Do the following to address Question 1.1: How does the risk of disease compare for smokers and otherwise similar non-smokers?

-   Improve your data display from last week to answer this question.

-   Fit a logistic regression model to answer this question. Interpret the coefficients and either p-values or confidence intervals from this model to answer the question. That is, what does this model say about Question 1.1?

-   Submit your data display in R Markdown through Github by Sunday (February 1, 2026) at midnight. You can find a link to create this assignment in Github on Canvas.

-   Post a screenshot of your improved data display (just the graph or table) and your logistic regression model output on Piazza in the "Assignment 1-2 Results" thread. Add a sentence or two that interprets the logistic regression results to answer the question of interest. You are welcome to post this anonymously to your classmates. You can also include comments about what your chose to do or questions you had as you were making the display and fitting your model.

-   You may work together on this assignment, but you must submit your own assignment; please credit in your assignment anyone with whom you collaborated.

-   Next week in class we will start with discussion of your work.
