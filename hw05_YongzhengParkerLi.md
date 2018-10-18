hw05\_YongzhengParkerLi
================
Yongzheng Parker Li
10/17/2018

This file is the Rmd file of *Homework 05* of the course STAT545A, taught by [Vincenzo Coia](https://github.com/vincenzocoia) at the University of British Columbia (UBC). The detailed requirements of this assignment could be found [here](http://stat545.com/Classroom/assignments/hw05/hw05.html). The STAT545A course page is [here](http://stat545.com/Classroom/). My own participation repository is [here](https://github.com/ParkerLi/STAT545_participation). The goal of this assignment is to

1.  Reorder a factor in a principled way based on the data and demonstrate the effect in arranged data and in figures.
2.  Write some data to file and load it back into R.
3.  Improve a figure (or make one from scratch), using new knowledge, e.g., control the color scheme, use factor levels, smoother mechanics.
4.  Make a plotly visual.
5.  Implement visualization design principles.

Bring Data In
=============

This section loads all the necessary packages. To disable all the conflicts/warning and other messges, I utilized the <code>suppressPackageStartupMessages</code> function I learned from class. Another way to do this is to use <code>Message=F</code> or <code>warn.conflicts=F</code> in the R chunk setting.

``` r
suppressPackageStartupMessages(library(gapminder))
suppressPackageStartupMessages(library(tidyverse))
```

    ## Warning: package 'dplyr' was built under R version 3.5.1

``` r
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(plotly))
```

Factor Management
=================

**Drop Oceania**. Filter the Gapminder data to remove observations associated with the continent of Oceania. Additionally, remove unused factor levels. Provide concrete information on the data before and after removing these rows and Oceania; address the number of rows and the levels of the affected factors.

**Before Drop Oceania**:

``` r
continent <- gapminder$continent
levels(continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"   "Oceania"

``` r
rowbefore <- nrow(gapminder)
levelbefore <- nlevels(continent)
kable(fct_count(continent), col.names = c('continet', 'number of rows'))
```

| continet |  number of rows|
|:---------|---------------:|
| Africa   |             624|
| Americas |             300|
| Asia     |             396|
| Europe   |             360|
| Oceania  |              24|

``` r
kable(data.frame(number_of_levels_before_dropping = levelbefore, number_of_rows_before_dropping = rowbefore))
```

|  number\_of\_levels\_before\_dropping|  number\_of\_rows\_before\_dropping|
|-------------------------------------:|-----------------------------------:|
|                                     5|                                1704|

**Drop Oceania**

``` r
noOceania <- gapminder %>% 
  filter(continent != 'Oceania') %>% 
  droplevels()
```

**After Drop Oceania**

``` r
noOceaniacontinent <- noOceania$continent
levels(noOceaniacontinent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"

``` r
rowafter <- nrow(noOceania)
levelafter <- nlevels(noOceaniacontinent)
kable(fct_count(noOceaniacontinent), col.names = c('continet', 'number of rows'))
```

| continet |  number of rows|
|:---------|---------------:|
| Africa   |             624|
| Americas |             300|
| Asia     |             396|
| Europe   |             360|

``` r
kable(data.frame(number_of_levels_after_dropping = levelafter, number_of_rows_after_dropping = rowafter))
```

|  number\_of\_levels\_after\_dropping|  number\_of\_rows\_after\_dropping|
|------------------------------------:|----------------------------------:|
|                                    4|                               1680|

We can tell that the number of levels drops 1: Oceania; number of rows decreases 24 and all of them are from the Oceania category which is droppbed.

**Reorder the levels of country or continent**. Use the forcats package to change the order of the factor levels, based on a principled summary of one of the quantitative variables. Consider experimenting with a summary statistic beyond the most basic choice of the median.

**reorder the levels of <code>continent</code>**

I reorder the levels of continent by the maximum of gdpPercap.

**Before Reorder**

``` r
beforereorder <- group_by(gapminder, continent) %>% 
  summarise(maxgdpPercap = max(gdpPercap)) #the reordering criteria
levels(beforereorder$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"   "Oceania"

``` r
kable(beforereorder)
```

| continent |  maxgdpPercap|
|:----------|-------------:|
| Africa    |      21951.21|
| Americas  |      42951.65|
| Asia      |     113523.13|
| Europe    |      49357.19|
| Oceania   |      34435.37|

**After Reorder**

``` r
afterreorder <- mutate(beforereorder, continent = fct_reorder(continent, maxgdpPercap))
levels(afterreorder$continent)
```

    ## [1] "Africa"   "Oceania"  "Americas" "Europe"   "Asia"

``` r
kable(afterreorder)
```

| continent |  maxgdpPercap|
|:----------|-------------:|
| Africa    |      21951.21|
| Americas  |      42951.65|
| Asia      |     113523.13|
| Europe    |      49357.19|
| Oceania   |      34435.37|

**We can tell that the <code>fct\_reorder</code> does not rearrange the dataframe. However, it does reorder the data in an elegant way**. See below:

``` r
ggplot(beforereorder, aes(x = maxgdpPercap, y = continent)) +
  geom_point()
```

![](hw05_YongzhengParkerLi_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
ggplot(afterreorder, aes(x = maxgdpPercap, y = continent)) +
  geom_point()
```

![](hw05_YongzhengParkerLi_files/figure-markdown_github/unnamed-chunk-6-2.png)

Taking it further: **what should we do if we want to reorder the dataframe too? <code>arrange</code> is a good function to work with**.

**<code>arrange()</code> Reorder the Dataframe**

``` r
reorderarrange <- beforereorder %>% 
  arrange(maxgdpPercap)
levels(reorderarrange$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"   "Oceania"

``` r
kable(reorderarrange)
```

| continent |  maxgdpPercap|
|:----------|-------------:|
| Africa    |      21951.21|
| Oceania   |      34435.37|
| Americas  |      42951.65|
| Europe    |      49357.19|
| Asia      |     113523.13|

**Combining <code>arrange</code> and <code>fct\_reorder</code> helps you reorder both the dataframe and plots/table**.

**Combing Reorder**

``` r
reordeboth <- beforereorder %>% 
  arrange(maxgdpPercap) %>% 
  mutate(continent=fct_reorder(continent, maxgdpPercap))
levels(reordeboth$continent)
```

    ## [1] "Africa"   "Oceania"  "Americas" "Europe"   "Asia"

``` r
kable(reordeboth)
```

| continent |  maxgdpPercap|
|:----------|-------------:|
| Africa    |      21951.21|
| Oceania   |      34435.37|
| Americas  |      42951.65|
| Europe    |      49357.19|
| Asia      |     113523.13|

``` r
ggplot(reordeboth, aes(x = maxgdpPercap, y = continent)) +
  geom_point()
```

![](hw05_YongzhengParkerLi_files/figure-markdown_github/unnamed-chunk-8-1.png)

File I/O
========

**Experiment with <code>write\_csv()</code>/<code>read\_csv()</code>**

``` r
fileio <- group_by(gapminder, continent) %>% 
  summarise(maxgdpPecap = max(gdpPercap)) %>% 
  mutate(continent=fct_reorder(continent, maxgdpPecap))
str(fileio) #will compare with the readcsv later
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    5 obs. of  2 variables:
    ##  $ continent  : Factor w/ 5 levels "Africa","Oceania",..: 1 3 5 4 2
    ##  $ maxgdpPecap: num  21951 42952 113523 49357 34435

``` r
levels(fileio$continent)
```

    ## [1] "Africa"   "Oceania"  "Americas" "Europe"   "Asia"

``` r
write_csv(fileio,"fileio.csv") #write csv
data <-read_csv("fileio.csv") #read csv
```

    ## Parsed with column specification:
    ## cols(
    ##   continent = col_character(),
    ##   maxgdpPecap = col_double()
    ## )

``` r
str(data)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    5 obs. of  2 variables:
    ##  $ continent  : chr  "Africa" "Americas" "Asia" "Europe" ...
    ##  $ maxgdpPecap: num  21951 42952 113523 49357 34435
    ##  - attr(*, "spec")=List of 2
    ##   ..$ cols   :List of 2
    ##   .. ..$ continent  : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ maxgdpPecap: list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
    ##   ..$ default: list()
    ##   .. ..- attr(*, "class")= chr  "collector_guess" "collector"
    ##   ..- attr(*, "class")= chr "col_spec"

Continent is a list (not a factor):<code>write\_csv()</code>/<code>read\_csv()</code> changes the variable characteristics.

**Experiment with <code>saveRDS()</code>/<code>readRDS()</code>**

``` r
saveRDS(fileio, "fileio.rds")
dataRDS <-readRDS("fileio.rds")
str(dataRDS)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    5 obs. of  2 variables:
    ##  $ continent  : Factor w/ 5 levels "Africa","Oceania",..: 1 3 5 4 2
    ##  $ maxgdpPecap: num  21951 42952 113523 49357 34435

``` r
levels(dataRDS$continent)
```

    ## [1] "Africa"   "Oceania"  "Americas" "Europe"   "Asia"

Nothing changes.<code>saveRDS()</code>/<code>readRDS()</code> retains the order of the factor.

**Experiment with <code>dput()</code>/<code>dget()</code>**

``` r
dput(fileio, "datar.R")
dataR <-dget("datar.R")
str(dataR)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    5 obs. of  2 variables:
    ##  $ continent  : Factor w/ 5 levels "Africa","Oceania",..: 1 3 5 4 2
    ##  $ maxgdpPecap: num  21951 42952 113523 49357 34435

``` r
levels(dataR$continent)
```

    ## [1] "Africa"   "Oceania"  "Americas" "Europe"   "Asia"

Nothing changes.<code>dput()</code>/<code>dget()</code> does not change anything.

Visualization Design
====================

Assignment requirement: Remake at least one figure or create a new one, in light of something you learned in the recent class meetings about visualization design and color. Maybe juxtapose your first attempt and what you obtained after some time spent working on it. Reflect on the differences. If using Gapminder, you can use the country or continent color scheme that ships with Gapminder.

For this section, I visualize the **reorder the levels of <code>continent</code>** process. A combined plot is shown here to compare and contrast different kinds of before/after reordering process.

**Making Plot**

``` r
vd1 <- group_by(gapminder, continent) %>% 
  summarise(maxgdpPercap = max(gdpPercap))
plot1 <- ggplot(vd1, aes(continent, maxgdpPercap))+
  geom_bar(aes(fill=continent), position="dodge", stat="identity")+
  labs(x="continent", y="maxgdpPercap", title="before order")

vd2 <- arrange(vd1, maxgdpPercap)
plot2 <- ggplot(vd2, aes(continent, maxgdpPercap))+
  geom_bar(aes(fill=continent), position="dodge", stat="identity")+
  labs(x="continent", y="maxgdpPercap", title="after order using arrange")

vd3 <- mutate(vd1, continent=fct_reorder(continent, maxgdpPercap))
plot3 <- ggplot(vd3, aes(continent, maxgdpPercap))+
  geom_bar(aes(fill=continent), position="dodge", stat="identity")+
  labs(x="continent", y="maxgdpPercap", title="after order using fctreorder")

vd4 <- arrange(vd1, maxgdpPercap) %>% 
  mutate(continent=fct_reorder(continent, maxgdpPercap))
plot4 <- ggplot(vd4, aes(continent, maxgdpPercap))+
  geom_bar(aes(fill=continent), position="dodge", stat="identity")+
  labs(x="continent", y="maxgdpPercap", title="after order using both")

finalplot <- grid.arrange(plot1, plot2, plot3, plot4, nrow=2)
```

![](hw05_YongzhengParkerLi_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
finalplot
```

    ## TableGrob (2 x 2) "arrange": 4 grobs
    ##   z     cells    name           grob
    ## 1 1 (1-1,1-1) arrange gtable[layout]
    ## 2 2 (1-1,2-2) arrange gtable[layout]
    ## 3 3 (2-2,1-1) arrange gtable[layout]
    ## 4 4 (2-2,2-2) arrange gtable[layout]

Assignment requirement: Then, make a new graph by converting this visual (or another, if you’d like) to a plotly graph. What are some things that plotly makes possible, that are not possible with a regular ggplot2 graph?

**Converting into a Plotly Graph**

``` r
#this is github_document, not html_document. but the code is here
#ggplotly(plot1)
# we can also try plot2, plot3, and plot4. 
#However, we cannot try finalplot. It will say "Error in UseMethod("ggplotly", p) 
#no applicable method for 'ggplotly' applied to an object of 
#class "c('gtable', 'gTree', 'grob', 'gDesc')""
```

**Benefits of <code>plotly</code> over <code>ggplot2</code>**:

I got great help from this [website](https://www.bryanwhiting.com/2017/02/the-dataviz-battle-plotly-vs-ggplot2/), detailing the difference between <code>plitly</code> and <code>ggplot2</code>. A brief summarization of the advantages of <code>plotly</code> has over <code>ggplot2</code>:

<code>Plotly</code> and <code>ggplot2</code> are inherently for different purposes. <code>plotly</code> allows you to quickly create beautiful, reactive D3 plots that are particularly powerful in websites and dashboards. You can hover your mouse over the plots and see the data values, zoom in and out of specific regions, and capture stills.

1.  Plotly handles multiple wide data columns.
2.  Customizing the layout (plot borders, y axis) is easier.
3.  Customizing the legend is easier
4.  Plotly syntax is very intuitive
5.  Plotly also works for Python, Matlab, and Excel, among other languages.
6.  It’s very easy to add new series and customize them (one line, one scatter, and one bar, for example)
7.  You can use other fonts
8.  You can toggle series on and off by clicking the series name in the legend

noted: some of the above functions could be utilized in <code>ggplot2</code> too, but much more complicated and time-consuming.

**Benefits of <code>ggplot2</code> over <code>plotly</code>**:

1.  Facet wrapping is very easy
2.  probably quicker for exploratory analysis.

Writing figures to file
=======================

Assignment Requirement: Use <code>ggsave()</code> to explicitly save a plot to file. Then load and embed it in your report.

``` r
ggsave("finalplot.png", plot = finalplot, width = 10,
       height = 7, scale = 1.3) #save final plot
```

![finalplot](finalplot.png)
