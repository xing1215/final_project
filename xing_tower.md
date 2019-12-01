Tower
================
Xing Chen
12/1/2019

## read and clean the data

``` r
tower = read_csv(file = "./data/structures.csv") 
```

    ## Parsed with column specification:
    ## cols(
    ##   Address = col_character(),
    ##   Team = col_character(),
    ##   Time = col_double(),
    ##   Lane = col_character(),
    ##   Type = col_character()
    ## )

``` r
tower = 
  tower %>% 
  janitor::clean_names() %>% 
  filter(team == "bTowers", type == "OUTER_TURRET") %>% 
  mutate(
    address = str_remove(address, "http://matchhistory.na.leagueoflegends.com/en/#match-details/TRLH1/")
  )
```

calculate the mean time group by match id

``` r
tower = 
  tower %>% 
  group_by(address) %>% 
  mutate(
    outer_tower = mean(time, na.rm = TRUE)
  )
```
