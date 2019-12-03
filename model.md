model
================
xinran
12/2/2019

## Importing Data

``` r
squrriel = read_csv("./data/2018_Central_Park_Squirrel_Census_-_Squirrel_Data.csv") %>% 
janitor::clean_names()  
skimr::skim(squrriel) # observe the distribution of our dataset
```

## Data Cleaning

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
       Day = fct_relevel(Day, c("Day1", "Day2", "Day3", "Day4", "Day5", "Day6", "Day7", "Day8", "Day9", "Day10", "Day11")))%>%
select(-Date,-month, -year)
```

``` r
hectare_data=read_csv("./data/2018_Central_Park_Squirrel_Census_-_Hectare_Data.csv") %>% 
janitor::clean_names()%>%
separate(date,into=c('day','year'),sep=4) %>% 
separate(day,into = c('month','Date'), sep=2)%>%
mutate(Day = recode(Date, "06"="Day1", "07"="Day2", "08"="Day3", "10"="Day4", "12"="Day5", "13"="Day6", "14"="Day7", "17"="Day8", "18"="Day9", "19"="Day10", "20"="Day11"),
       Day = fct_relevel(Day, c("Day1", "Day2", "Day3", "Day4", "Day5", "Day6", "Day7", "Day8", "Day9", "Day10", "Day11")))%>%
select(-Date,-month, -year)
```

    ## Parsed with column specification:
    ## cols(
    ##   Hectare = col_character(),
    ##   Shift = col_character(),
    ##   Date = col_double(),
    ##   X4 = col_double(),
    ##   `Sighter Observed Weather Data` = col_character(),
    ##   Litter = col_character(),
    ##   `Litter Notes` = col_character(),
    ##   `Other Animal Sightings` = col_character(),
    ##   `Hectare Conditions` = col_character(),
    ##   `Hectare Conditions Notes` = col_character(),
    ##   `Number of sighters` = col_double(),
    ##   `Number of Squirrels` = col_double(),
    ##   `Total Time of Sighting` = col_double()
    ## )

``` r
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
}

hectare_data = 
  hectare_data %>% 
  mutate(
      temperature = numextract(sighter_observed_weather_data),
      temperature=as.numeric(temperature)
)
```

``` r
total_data=
  left_join(squrriel, hectare_data, by=c("hectare","Day","shift"))
```

``` r
squ_ac=multinom(running+chasing+climbing+eating+foraging~age+hectare_conditions+temperature,data=total_data)
```

    ## # weights:  54 (40 variable)
    ## initial  value 4517.025622 
    ## iter  10 value 2515.075170
    ## iter  20 value 2439.927606
    ## iter  30 value 2427.629136
    ## iter  40 value 2427.281240
    ## iter  50 value 2427.259790
    ## final  value 2427.259491 
    ## converged

``` r
coef(squ_ac)%>%
broom::tidy() %>% 
  knitr::kable()
```

| X.Intercept. |  ageAdult | ageJuvenile | hectare\_conditionsCalm | hectare\_conditionsCalm..Busy | hectare\_conditionsMedium | hectare\_conditionsModerate | temperature |
| -----------: | --------: | ----------: | ----------------------: | ----------------------------: | ------------------------: | --------------------------: | ----------: |
|  \-0.5278835 |  3.219794 |    3.085625 |               0.1862248 |                   \-0.6486090 |                12.1890130 |                   0.9132907 | \-0.0055893 |
| \-19.8588545 | 21.581122 |   21.316471 |             \-0.0846792 |                   \-0.7935160 |                11.7573479 |                   1.0300399 | \-0.0045812 |
| \-17.9111586 | 18.324623 |   18.200866 |               0.1798581 |                   \-0.1345721 |               \-6.9447564 |                   0.9470001 | \-0.0096437 |
| \-13.2397482 | 10.338495 |   10.968140 |               0.0030361 |                  \-20.1587805 |               \-1.2521748 |                \-22.1074282 |   0.0030758 |
| \-12.5027421 |  7.739876 |  \-9.270774 |            \-20.1499151 |                   \-9.6006091 |                 0.6574015 |                \-12.1499769 |   0.0182334 |

``` r
interact=multinom(kuks+quaas+tail_flags+tail_twitches+approaches+indifferent+runs_from~age+litter+temperature,data=total_data)
```

    ## # weights:  42 (30 variable)
    ## initial  value 4174.799563 
    ## iter  10 value 2568.849171
    ## iter  20 value 2420.221530
    ## iter  30 value 2406.997818
    ## iter  40 value 2406.481420
    ## iter  50 value 2406.458578
    ## final  value 2406.457732 
    ## converged

``` r
coef(interact)%>%
broom::tidy()%>%
  knitr::kable()
```

| X.Intercept. |  ageAdult | ageJuvenile |  litterNone |   litterSome | temperature |
| -----------: | --------: | ----------: | ----------: | -----------: | ----------: |
|    0.3563525 |  1.897844 |     1.61190 | \-0.5138135 |  \-0.2075143 | \-0.0124009 |
| \-32.0653982 | 33.348215 |    33.50419 | \-0.1526341 |  \-0.1058666 | \-0.0202789 |
| \-22.3227625 | 22.645822 |    22.21704 | \-0.3599412 |  \-0.6589934 | \-0.0319366 |
| \-43.5726687 | 22.903566 |  \-48.76849 |  19.5312520 |   18.3260709 | \-0.0471355 |
| \-28.9740889 |  5.969466 |  \-12.67024 |  19.0409756 | \-20.0029491 | \-0.0216528 |
