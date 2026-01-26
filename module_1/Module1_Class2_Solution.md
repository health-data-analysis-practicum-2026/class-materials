---
title: "Class 1-2: Discussion of data displays for Question 1-1"
author: "Health Data Analysis Practicum (AS.280.347)"
date: "January 26, 2026"
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

First we load the packages that we will be using in this document.  It's good practices to load packages as the beginning so they are all in the same place.  If you decide later you need an additional package, add it to the top of the document!

``` r
library(tidyverse)  # core group of tidyverse packages
library(kableExtra)  # to make nice tables
```

## Module 1: Smoking and the risk of disease

Questions of interest:

* **Question 1.1: ** How does the risk of disease compare for smokers and otherwise similar non-smokers?

<center>
![](Q1_dag.png){width=500px}
</center>

* **Queston 1.2: ** Does the contribution of smoking to the risk of disease vary by sex or socio-economic status (SES)?

<center>
![](Q2_dag.png){width=500px}
</center>

To address each question we want:

* A data display (graph or table)
* A statistical analysis (with interprepration)

We will answer these questions using data from the National Medical Expenditures Survey (NMES)

## Discussion of NMES data displays for Question 1-1

In your small groups, take 25 minutes to discuss the following 7 displays.  For each display, answer the following questions:

* Does this display effectively answer our question on interest: *How does the risk of disease compare for smokers and otherwise similar non-smokers?*
     * Does it clearly show the risk of disease?
     * Does it allow you to easily compare smokers to non-smokers? Why or why not?
     * Does it account for "otherwise similar"? Why or why not? Is there a major variable that is not included in the plot that you think is important or which may be explaining some of the patterns you see in the display?
     * What would your answer to the question be based on this display?  Does this answer match your intuition?
* How can the display be improved to more clearly answer our question of interest?
* What is something that you like about the display?
* Look at the code used to create the display. Is it easy to read and understand? Is there extra code that is not used?
* Pick a couple of displays and add code to make the improvements you have in mind.

**If you have additional time,** scroll through the post of all student work and make note of anything that you especially like about what any of your peers has done: https://piazza.com/class/m5lijvm8x3960v/post/7

### Display 1

![](Module1_Class2_Solution_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


Here, I cleaned up the code to make it easier to read and flipped the role of `eversmk` and `chd5`. I also made the levels of the age variable in a better order.


``` r
nmes_data <- read_csv("module_1/nmesUNPROC.csv")

### Recode eversmk into Never smoker and Ever smoker and recode chd5 into No chd and chd
nmes_data <- nmes_data %>%
  mutate(
    eversmk = factor(
      eversmk,
      levels = c("0", "1"),
      labels = c("Never smoker", "Ever smoker")
    ),
    chd5 = factor(
      chd5,
      levels = c("0", "1"),
      labels = c("No chd", "chd")
    ),
    ### Create new column to split individuals in age groups
    age_group = factor(
      ifelse(age < 60, "Under 60", "60+"), 
      levels = c("Under 60", "60+"))
  )

ggplot(data = nmes_data) + 
  geom_bar(mapping = aes(x = eversmk, fill = chd5),
                                    position = "fill") + 
  facet_wrap( ~ age_group)
```

![](Module1_Class2_Solution_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


### Display 2


```
## # A tibble: 25 × 6
## # Groups:   age, female, eversmk [16]
##    age   female lc5   eversmk          n    prop
##    <fct> <fct>  <fct> <fct>        <int>   <dbl>
##  1 1-20  Male   No LC Never smoker    44 1      
##  2 1-20  Male   No LC Ever smoker     10 1      
##  3 1-20  Female No LC Never smoker    54 1      
##  4 1-20  Female No LC Ever smoker     22 1      
##  5 21-40 Male   No LC Never smoker   316 1      
##  6 21-40 Male   No LC Ever smoker    311 0.994  
##  7 21-40 Male   LC    Ever smoker      2 0.00639
##  8 21-40 Female No LC Never smoker   579 1      
##  9 21-40 Female No LC Ever smoker    493 1      
## 10 41-60 Male   No LC Never smoker   138 0.993  
## # ℹ 15 more rows
```

![](Module1_Class2_Solution_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


Here, I filtered to only show the lc5 rate (not the No MSCD rate), modified the axis labels, and removed the legend. I also angled the axis labels, and made a change to remove the warning about missing values.


``` r
my_table <- nmes_data %>%
  count(age, female, lc5, eversmk, .drop = FALSE) %>%
  group_by(age, female, eversmk) %>%
  mutate(prop = n/sum(n))


my_table %>%
  filter(lc5 == "LC") %>%
  ggplot() + 
  geom_bar(aes(x = eversmk, y = prop, fill = lc5), stat = "identity", position = "stack") +
  facet_grid(female ~ age) +   
  labs(
    title = "Rate of Lung Cancer",
    subtitle = "Smokers vs non-smokers, stratified by sex and socio-economic status",
    x = "Smoking status",
    y = "Rate of Lung Cancer") +
  guides(fill="none") +
  theme(axis.text.x = element_text(angle = 22.5,
                                   hjust = 1,
                                   size = 9))
```

![](Module1_Class2_Solution_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


### Display 3


![](Module1_Class2_Solution_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


Here, I created a table, flipped the role of lc5 and smoking status, added labels, removed the legend. 


``` r
my_table <- nmes_data %>%
  count(bmi_cat, eversmk, lc5, .drop = FALSE) %>%
  group_by(bmi_cat, eversmk) %>%
  mutate(prop = n/sum(n))


my_table %>%
  filter(lc5 == "LC") %>%
  ggplot() + 
  geom_bar(aes(x = eversmk, y = prop, fill = lc5), stat = "identity", position = "stack") +
  facet_wrap(~bmi_cat) +   
  labs(
    title = "Rate of Lung Cancer",
    subtitle = "Smokers vs non-smokers, stratified by BMI",
    x = "Smoking status",
    y = "Rate of Lung Cancer") +
  guides(fill="none") +
  theme(axis.text.x = element_text(angle = 22.5,
                                   hjust = 1,
                                   size = 9))
```

![](Module1_Class2_Solution_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


### Display 4

![](Module1_Class2_Solution_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


Modified to include both chd and lc and cleaned up the code a bit.


``` r
nmes_data <- read_csv("module_1/nmesUNPROC.csv")

nmes_data <- nmes_data %>%
  mutate(
    smoker = factor(eversmk,
      levels = c("0", "1"),
      labels = c("Never smoker", "Ever smoker")),
    age_group = cut(
      age,
      breaks = c(0, 40, 55, 70, Inf),
      labels = c("<40", "40–54", "55–69", "70+")
    )
)

### Create age groups to compare smokers and non-smokers of similar age


plot_data <- nmes_data %>%
  group_by(age_group, smoker) %>%
  summarize(
    disease_risk = mean(chd5 | lc5, na.rm = TRUE),
    .groups = "drop" #ungroup the data after summarizing
  )

### Plot MSCD risk by smoking status, stratified by age

ggplot(plot_data, aes(x = smoker, y = disease_risk, fill = smoker)) +
  geom_col(position = "dodge") +
  facet_wrap(~ age_group) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "MSCD risk by smoking status and age",
    x = "Smoking status",
    y = "Percent with MSCD"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
```

![](Module1_Class2_Solution_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


### Display 5

![](Module1_Class2_Solution_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


Similar to the above, I made a table to make sure I was calculating things correctly for all the groups conditioned on, filtered to only show LC, shortened level names for education, angled labels, included new axis labels and titles.


``` r
nmes_data <- read_csv("module_1/nmesUNPROC.csv")

nmes_data <- nmes_data %>%
  mutate(educate = factor(educate,
                    levels = c("1", "2", "3", "4"),
                    labels = c("College Grad", "Some college", "HS Grad", "Other")),
         female = factor(female, 
                         levels = c("0", "1"),
                         labels = c("Male", "Female")),
         eversmk = factor(eversmk, 
                          levels = c("0", "1"), 
                          labels = c("Never smoker", "Ever smoker")),
         lc5 = factor(lc5, 
                      levels = c("0", "1"), 
                      labels = c("No LC", "LC")),
         poor = factor(poor,
                       levels = c("0", "1"),
                       labels = c("Not poor", "Poor"))
         )

my_table <- nmes_data %>%
  count(eversmk, lc5, educate, female) %>%
  group_by(eversmk, educate, female) %>%
  mutate(prop = n/sum(n)) 

my_table %>%
  filter(lc5 == "LC") %>%
  ggplot() + 
  geom_bar(mapping = aes(x = eversmk, y = prop), stat = "identity") +
  facet_grid(female~educate)  +   
  labs(
    title = "Rate of Lung Cancer",
    subtitle = "Smokers vs non-smokers, stratified by sex and education",
    x = "Smoking status",
    y = "Rate of Lung Cancer") +
  guides(fill="none") +
  theme(axis.text.x = element_text(angle = 22.5,
                                   hjust = 1,
                                   size = 9))
```

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

![](Module1_Class2_Solution_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


### Display 6


<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Lung Cancer (LC) Status </th>
   <th style="text-align:left;"> Smoking Status </th>
   <th style="text-align:left;"> Gender </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> prop </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> No LC </td>
   <td style="text-align:left;"> Never smoker </td>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:right;"> 618 </td>
   <td style="text-align:right;"> 0.997 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No LC </td>
   <td style="text-align:left;"> Never smoker </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:right;"> 1462 </td>
   <td style="text-align:right;"> 0.999 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No LC </td>
   <td style="text-align:left;"> Ever smoker </td>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:right;"> 933 </td>
   <td style="text-align:right;"> 0.969 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No LC </td>
   <td style="text-align:left;"> Ever smoker </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:right;"> 1020 </td>
   <td style="text-align:right;"> 0.989 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> Never smoker </td>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.003 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> Never smoker </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> Ever smoker </td>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 0.031 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> Ever smoker </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 0.011 </td>
  </tr>
</tbody>
</table>


I filtered to only show the rate for the LC people, and grouped to compare ever smokers to never smokers more directly:


``` r
# Table stratified by gender
my_table <- nmes_data %>%
  count(lc5, eversmk, female) %>%
  group_by(eversmk, female) %>%
  mutate(prop = n/sum(n)) %>%
  filter(lc5 == "LC") %>%
  arrange(female, eversmk)%>%
  rename(
    "Smoking Status" = eversmk, 
    "Lung Cancer (LC) Status" = lc5,
    "Gender" = female
  )
my_table %>%
  kable(digits = 3) %>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Lung Cancer (LC) Status </th>
   <th style="text-align:left;"> Smoking Status </th>
   <th style="text-align:left;"> Gender </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> prop </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> Never smoker </td>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.003 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> Ever smoker </td>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 0.031 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> Never smoker </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> Ever smoker </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 0.011 </td>
  </tr>
</tbody>
</table>
