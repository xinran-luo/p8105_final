data clean\_jy2965
================
Jun Yin
11/22/2019

## Importing Data

``` r
squrriel = read_csv("./data/2018_Central_Park_Squirrel_Census_-_Squirrel_Data.csv") %>% 
janitor::clean_names()  
skimr::skim(squrriel) # observe the distribution of our dataset
```

## Data Cleaning

reclassify some variables with missing values. give character variables
different level. Create new variable to represent research date for
futher
work.

``` r
squrriel = read_csv("./data/2018_Central_Park_Squirrel_Census_-_Squirrel_Data.csv") %>% 
janitor::clean_names() %>% 
mutate(age_group = ifelse(age %in% "Adult","Adult",ifelse(age %in% "Juvenile","Juvenile","Unknown")),
      age_group = fct_relevel(as.factor(age_group),c("Juvenile","Adult","Unknown")),
      location = fct_relevel(as.factor(location),c("Above Ground","Ground Plane")),
      primary_fur_color = fct_relevel(as.factor(primary_fur_color),c("Cinnamon","Gray","Black"))) %>% 
separate(date,into=c('day','year'),sep=4) %>% 
separate(day,into = c('month','Date'), sep=2) %>% 
mutate(Day = recode(Date, "06"="Day1", "07"="Day2", "08"="Day3", "10"="Day4", "12"="Day5", "13"="Day6", "14"="Day7", "17"="Day8", "18"="Day9", "19"="Day10", "20"="Day11"),
       Day = fct_relevel(Day, c("Day1", "Day2", "Day3", "Day4", "Day5", "Day6", "Day7", "Day8", "Day9", "Day10", "Day11")))
```

## Data Visulization
