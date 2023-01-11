classification with tidymodels
================
chad allison \| 12 december 2022

------------------------------------------------------------------------

### loading required libraries and setting options

``` r
library(tidyverse) # essential functions
library(tidymodels) # essential for tidy modeling
library(visdat) # visualising data class structure
library(skimr) # data skimming
library(GGally) # pairwise plots
library(ggmap) # geographical visualisation
library(ranger) # random forest
library(keras) # neural network

knitr::opts_chunk$set(message = F, warning = F)
options(scipen = 999)
theme_set(theme_minimal())

custom_red = "#FFCFCF"
custom_green = "#B7D1B3"
tictoc::tic()
```

------------------------------------------------------------------------

### importing data

``` r
link = "https://raw.githubusercontent.com/kirenz/datasets/master/housing_unclean.csv"
housing_df = read_csv(link, col_types = cols())
rm(link)

head(housing_df)
```

    ## # A tibble: 6 x 10
    ##   longitude latitude housing_m~1 total~2 total~3 popul~4 house~5 media~6 media~7
    ##       <dbl>    <dbl> <chr>         <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>  
    ## 1     -122.     37.9 41.0years       880     129     322     126    8.33 452600~
    ## 2     -122.     37.9 21.0           7099    1106    2401    1138    8.30 358500~
    ## 3     -122.     37.8 52.0           1467     190     496     177    7.26 352100~
    ## 4     -122.     37.8 52.0           1274     235     558     219    5.64 341300~
    ## 5     -122.     37.8 52.0           1627     280     565     259    3.85 342200~
    ## 6     -122.     37.8 52.0            919     213     413     193    4.04 269700~
    ## # ... with 1 more variable: ocean_proximity <chr>, and abbreviated variable
    ## #   names 1: housing_median_age, 2: total_rooms, 3: total_bedrooms,
    ## #   4: population, 5: households, 6: median_income, 7: median_house_value

------------------------------------------------------------------------

### cleaning `housing_median_age` and `median_house_value`

``` r
housing_df = housing_df |>
  mutate(housing_median_age = str_remove_all(housing_median_age, "[years]"),
         median_house_value = str_remove_all(median_house_value, "[$]"))

housing_df |>
  select(housing_median_age, median_house_value) |>
  head()
```

    ## # A tibble: 6 x 2
    ##   housing_median_age median_house_value
    ##   <chr>              <chr>             
    ## 1 41.0               452600.0          
    ## 2 21.0               358500.0          
    ## 3 52.0               352100.0          
    ## 4 52.0               341300.0          
    ## 5 52.0               342200.0          
    ## 6 52.0               269700.0

------------------------------------------------------------------------

### viewing data structure

``` r
glimpse(housing_df)
```

    ## Rows: 20,640
    ## Columns: 10
    ## $ longitude          <dbl> -122.23, -122.22, -122.24, -122.25, -122.25, -122.2~
    ## $ latitude           <dbl> 37.88, 37.86, 37.85, 37.85, 37.85, 37.85, 37.84, 37~
    ## $ housing_median_age <chr> "41.0", "21.0", "52.0", "52.0", "52.0", "52.0", "52~
    ## $ total_rooms        <dbl> 880, 7099, 1467, 1274, 1627, 919, 2535, 3104, 2555,~
    ## $ total_bedrooms     <dbl> 129, 1106, 190, 235, 280, 213, 489, 687, 665, 707, ~
    ## $ population         <dbl> 322, 2401, 496, 558, 565, 413, 1094, 1157, 1206, 15~
    ## $ households         <dbl> 126, 1138, 177, 219, 259, 193, 514, 647, 595, 714, ~
    ## $ median_income      <dbl> 8.3252, 8.3014, 7.2574, 5.6431, 3.8462, 4.0368, 3.6~
    ## $ median_house_value <chr> "452600.0", "358500.0", "352100.0", "341300.0", "34~
    ## $ ocean_proximity    <chr> "NEAR BAY", "NEAR BAY", "NEAR BAY", "NEAR BAY", "NE~

------------------------------------------------------------------------

### visualising data structure

``` r
vis_dat(housing_df) +
  scale_fill_manual(values = c("#8BB895", "#B494C6"))
```

![](tidymodels_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

------------------------------------------------------------------------

### exploring `ocean_proximity` variable

``` r
housing_df |>
  count(ocean_proximity,
        sort = T)
```

    ## # A tibble: 5 x 2
    ##   ocean_proximity     n
    ##   <chr>           <int>
    ## 1 <1H OCEAN        9136
    ## 2 INLAND           6551
    ## 3 NEAR OCEAN       2658
    ## 4 NEAR BAY         2290
    ## 5 ISLAND              5

------------------------------------------------------------------------

### reformatting variables

``` r
housing_df = housing_df |>
  mutate(housing_median_age = as.numeric(housing_median_age),
         median_house_value = as.numeric(median_house_value),
         across(where(is.character), as.factor))

vis_dat(housing_df) +
  scale_fill_manual(values = c("#8BB895", "#B494C6"))
```

![](tidymodels_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

------------------------------------------------------------------------

### checking missing data

``` r
housing_df |>
  vis_miss(sort_miss = T)
```

![](tidymodels_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

------------------------------------------------------------------------

### seeing exactly how many `NA` values we have

``` r
colSums(is.na(housing_df))
```

    ##          longitude           latitude housing_median_age        total_rooms 
    ##                  0                  0                  0                  0 
    ##     total_bedrooms         population         households      median_income 
    ##                207                  0                  0                  0 
    ## median_house_value    ocean_proximity 
    ##                  0                  0

------------------------------------------------------------------------

### creating new variables

``` r
housing_df = housing_df |>
  mutate(rooms_per_household = round(total_rooms / households, 4),
         bedrooms_per_room = round(total_bedrooms / total_rooms, 4),
         population_per_household = round(population / households, 4))

housing_df |>
  select(rooms_per_household, bedrooms_per_room, population_per_household) |>
  head()
```

    ## # A tibble: 6 x 3
    ##   rooms_per_household bedrooms_per_room population_per_household
    ##                 <dbl>             <dbl>                    <dbl>
    ## 1                6.98             0.147                     2.56
    ## 2                6.24             0.156                     2.11
    ## 3                8.29             0.130                     2.80
    ## 4                5.82             0.184                     2.55
    ## 5                6.28             0.172                     2.18
    ## 6                4.76             0.232                     2.14

------------------------------------------------------------------------

### creating dependent variable and dropping original numeric variable

``` r
housing_df = housing_df |>
  mutate(price_category = case_when(median_house_value < 150000 ~ "below",
                                    median_house_value >= 150000 ~ "above"),
         price_category = as.factor(price_category)) |>
  select(-median_house_value)

housing_df |>
  count(price_category) |>
  mutate(prop = round(n / sum(n), 3))
```

    ## # A tibble: 2 x 3
    ##   price_category     n  prop
    ##   <fct>          <int> <dbl>
    ## 1 above          13084 0.634
    ## 2 below           7556 0.366

------------------------------------------------------------------------

### data overview with `skimr`

``` r
skim(housing_df)
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | housing_df |
| Number of rows                                   | 20640      |
| Number of columns                                | 13         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| factor                                           | 2          |
| numeric                                          | 11         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: factor**

| skim_variable   | n_missing | complete_rate | ordered | n_unique | top_counts                                  |
|:----------------|----------:|--------------:|:--------|---------:|:--------------------------------------------|
| ocean_proximity |         0 |             1 | FALSE   |        5 | \<1H: 9136, INL: 6551, NEA: 2658, NEA: 2290 |
| price_category  |         0 |             1 | FALSE   |        2 | abo: 13084, bel: 7556                       |

**Variable type: numeric**

| skim_variable            | n_missing | complete_rate |    mean |      sd |      p0 |     p25 |     p50 |     p75 |     p100 | hist  |
|:-------------------------|----------:|--------------:|--------:|--------:|--------:|--------:|--------:|--------:|---------:|:------|
| longitude                |         0 |          1.00 | -119.57 |    2.00 | -124.35 | -121.80 | -118.49 | -118.01 |  -114.31 | ▂▆▃▇▁ |
| latitude                 |         0 |          1.00 |   35.63 |    2.14 |   32.54 |   33.93 |   34.26 |   37.71 |    41.95 | ▇▁▅▂▁ |
| housing_median_age       |         0 |          1.00 |   28.64 |   12.59 |    1.00 |   18.00 |   29.00 |   37.00 |    52.00 | ▃▇▇▇▅ |
| total_rooms              |         0 |          1.00 | 2635.76 | 2181.62 |    2.00 | 1447.75 | 2127.00 | 3148.00 | 39320.00 | ▇▁▁▁▁ |
| total_bedrooms           |       207 |          0.99 |  537.87 |  421.39 |    1.00 |  296.00 |  435.00 |  647.00 |  6445.00 | ▇▁▁▁▁ |
| population               |         0 |          1.00 | 1425.48 | 1132.46 |    3.00 |  787.00 | 1166.00 | 1725.00 | 35682.00 | ▇▁▁▁▁ |
| households               |         0 |          1.00 |  499.54 |  382.33 |    1.00 |  280.00 |  409.00 |  605.00 |  6082.00 | ▇▁▁▁▁ |
| median_income            |         0 |          1.00 |    3.87 |    1.90 |    0.50 |    2.56 |    3.53 |    4.74 |    15.00 | ▇▇▁▁▁ |
| rooms_per_household      |         0 |          1.00 |    5.43 |    2.47 |    0.85 |    4.44 |    5.23 |    6.05 |   141.91 | ▇▁▁▁▁ |
| bedrooms_per_room        |       207 |          0.99 |    0.21 |    0.06 |    0.10 |    0.18 |    0.20 |    0.24 |     1.00 | ▇▁▁▁▁ |
| population_per_household |         0 |          1.00 |    3.07 |   10.39 |    0.69 |    2.43 |    2.82 |    3.28 |  1243.33 | ▇▁▁▁▁ |

### data overview with pariwise plots from `GGally`

``` r
housing_df |>
  sample_n(1000) |> # sampling for script run time
  select(housing_median_age, median_income, rooms_per_household,
         ocean_proximity, price_category) |>
  ggpairs()
```

![](tidymodels_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### data splitting

``` r
set.seed(123)
data_split = initial_split(housing_df, prop = 0.75, strata = price_category)
train_data = training(data_split)
test_data = testing(data_split)

raw_counts = housing_df |>
  count(price_category) |>
  mutate(prop = round(n / sum(n), 4),
         set = "raw")

train_counts = train_data |>
  count(price_category) |>
  mutate(prop = round(n / sum(n), 4),
         set = "train")

test_counts = test_data |>
  count(price_category) |>
  mutate(prop = round(n / sum(n), 4),
         set = "test")

rbind(raw_counts, train_counts, test_counts) |>
  ggplot(aes(price_category, prop)) +
  geom_col(aes(fill = set), position = "dodge", alpha = 0.75) +
  scale_fill_manual(values = c("#828BA8", "#99BF9E", "#B293BD")) +
  theme_classic() +
  labs(title = "`price_category` equally distributed among full, training, and testing data") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](tidymodels_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
rm(raw_counts, train_counts, test_counts)
```

------------------------------------------------------------------------

### geographical overview

``` r
data_explore = train_data # so we don't alter our data

qmplot(x = longitude, y = latitude, data = data_explore,
       geom = "point", col = price_category, size = population, alpha = 0.25) +
  scale_alpha(guide = "none") +
  scale_color_manual(values = c("indianred3", "lightsteelblue3"))
```

![](tidymodels_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

------------------------------------------------------------------------

### exploring numeric variables

``` r
data_explore |>
  ggplot(aes(price_category, median_income)) +
  geom_boxplot(aes(fill = price_category),
               outlier.alpha = 0.25, alpha = 0.5) +
  scale_fill_manual(values = c("indianred3", "springgreen4"))
```

![](tidymodels_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

### creating function to print boxplot

``` r
print_boxplot = function(.y_var) {
  y_var = sym(.y_var)
  
  data_explore |>
    ggplot(aes(price_category, {{y_var}})) +
    geom_boxplot(aes(fill = price_category), outlier.alpha = 0.25, alpha = 0.5) +
    scale_fill_manual(values = c("indianred3", "springgreen4"))
}
```

### obtaining numeric y-variables

``` r
y_var = data_explore |>
  select(where(is.numeric), -longitude, -latitude) |>
  variable.names()
```

### printing boxplots

``` r
map(y_var, print_boxplot)
```

    ## [[1]]

![](tidymodels_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

    ## 
    ## [[2]]

![](tidymodels_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->

    ## 
    ## [[3]]

![](tidymodels_files/figure-gfm/unnamed-chunk-19-3.png)<!-- -->

    ## 
    ## [[4]]

![](tidymodels_files/figure-gfm/unnamed-chunk-19-4.png)<!-- -->

    ## 
    ## [[5]]

![](tidymodels_files/figure-gfm/unnamed-chunk-19-5.png)<!-- -->

    ## 
    ## [[6]]

![](tidymodels_files/figure-gfm/unnamed-chunk-19-6.png)<!-- -->

    ## 
    ## [[7]]

![](tidymodels_files/figure-gfm/unnamed-chunk-19-7.png)<!-- -->

    ## 
    ## [[8]]

![](tidymodels_files/figure-gfm/unnamed-chunk-19-8.png)<!-- -->

    ## 
    ## [[9]]

![](tidymodels_files/figure-gfm/unnamed-chunk-19-9.png)<!-- -->

### re-creating function to filter some extreme cases

``` r
print_boxplot_out = function(.y_var_out) {
  y_var = sym(.y_var_out)
  
  data_explore |>
    filter(rooms_per_household < 50, population_per_household < 20) |>
    ggplot(aes(price_category, {{y_var}})) +
    geom_boxplot(aes(fill = price_category), alpha = 0.5, outlier.alpha = 0.25) +
    scale_fill_manual(values = c("indianred3", "springgreen4"))
}

y_var_out = data_explore |>
  select(rooms_per_household, population_per_household) |>
  variable.names()

map(y_var_out, print_boxplot_out)
```

    ## [[1]]

![](tidymodels_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

    ## 
    ## [[2]]

![](tidymodels_files/figure-gfm/unnamed-chunk-20-2.png)<!-- -->

### using \``ggscatmat` to create more pairwise plots

``` r
data_explore |>
  sample_n(1000) |> # speed
  select(price_category, median_income, bedrooms_per_room,
         rooms_per_household, population_per_household) |>
  ggscatmat(color = "price_category", corMethod = "spearman", alpha = 0.25) +
  scale_color_manual(values = c("indianred3", "springgreen4"))
```

![](tidymodels_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

### exploring categorical variables

``` r
data_explore |>
  filter(ocean_proximity != "ISLAND") |>
  count(price_category, ocean_proximity) |>
  group_by(price_category) |>
  mutate(percent = n / sum(n) * 100,
         percent = round(percent, 2),
         percent = paste0(percent, "%")) |>
  ggplot(aes(ocean_proximity, n)) +
  geom_col(aes(fill = price_category), position = "dodge") +
  geom_text(aes(label = percent), position = position_dodge2(width = 0.9), vjust = -0.5, size = 3.25) +
  scale_fill_manual(values = c(custom_red, custom_green)) # this step is where i added these as variables
```

![](tidymodels_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

### creating heatmap

``` r
data_explore |>
  ggplot(aes(price_category, ocean_proximity)) +
  geom_bin2d() +
  stat_bin2d(geom = "text", aes(label = ..count..)) +
  scale_fill_continuous(type = "viridis")
```

![](tidymodels_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

### data preparation steps

- handle missing values
- fix or remove outliers
- feature selection
- feature engineering
- feature scaling
- create a validation set

### beginning data prep

``` r
housing_df_new = housing_df |>
  select(longitude, latitude,
         price_category, median_income, ocean_proximity,
         bedrooms_per_room, rooms_per_household, population_per_household)

glimpse(housing_df_new)
```

    ## Rows: 20,640
    ## Columns: 8
    ## $ longitude                <dbl> -122.23, -122.22, -122.24, -122.25, -122.25, ~
    ## $ latitude                 <dbl> 37.88, 37.86, 37.85, 37.85, 37.85, 37.85, 37.~
    ## $ price_category           <fct> above, above, above, above, above, above, abo~
    ## $ median_income            <dbl> 8.3252, 8.3014, 7.2574, 5.6431, 3.8462, 4.036~
    ## $ ocean_proximity          <fct> NEAR BAY, NEAR BAY, NEAR BAY, NEAR BAY, NEAR ~
    ## $ bedrooms_per_room        <dbl> 0.1466, 0.1558, 0.1295, 0.1845, 0.1721, 0.231~
    ## $ rooms_per_household      <dbl> 6.9841, 6.2381, 8.2881, 5.8174, 6.2819, 4.761~
    ## $ population_per_household <dbl> 2.5556, 2.1098, 2.8023, 2.5479, 2.1815, 2.139~

### making new data split

``` r
set.seed(123)
data_split = initial_split(housing_df_new, prop = 3 / 4, strata = price_category)
train_data = training(data_split)
test_data = testing(data_split)

paste0("training data: ", nrow(train_data), " observations")
```

    ## [1] "training data: 15480 observations"

``` r
paste0("testing data: ", nrow(test_data), " observations")
```

    ## [1] "testing data: 5160 observations"

### creating preprocessing recipe

``` r
housing_rec = recipe(price_category ~ ., data = train_data) |>
  update_role(longitude, latitude, new_role = "ID") |>
  # log transforms our skewed data
  step_log(median_income, bedrooms_per_room, rooms_per_household, population_per_household) |>
  # removes any NAs
  step_naomit(everything(), skip = T) |>
  # converts nominal variables to factors
  step_novel(all_nominal(), -all_outcomes()) |>
  # normalizes numeric variables, z-standardization
  step_normalize(all_numeric(), - all_outcomes(),
                 -longitude, -latitude) |>
  # converts `ocean_proximity` to numeric binary
  step_dummy(all_nominal(), -all_outcomes()) |>
  # removes numeric variables with zero variance
  step_zv(all_numeric(), -all_outcomes()) |>
  # removes predictor variables that are highly correlated with other predictor variables
  step_corr(all_predictors(), threshold = 0.7, method = "spearman")

summary(housing_rec)
```

    ## # A tibble: 8 x 4
    ##   variable                 type    role      source  
    ##   <chr>                    <chr>   <chr>     <chr>   
    ## 1 longitude                numeric ID        original
    ## 2 latitude                 numeric ID        original
    ## 3 median_income            numeric predictor original
    ## 4 ocean_proximity          nominal predictor original
    ## 5 bedrooms_per_room        numeric predictor original
    ## 6 rooms_per_household      numeric predictor original
    ## 7 population_per_household numeric predictor original
    ## 8 price_category           nominal outcome   original

### checking out the prepped data

``` r
prepped_data = housing_rec |>
  prep() |>
  juice()

glimpse(prepped_data)
```

    ## Rows: 15,325
    ## Columns: 10
    ## $ longitude                  <dbl> -122.23, -122.22, -122.24, -122.25, -122.25~
    ## $ latitude                   <dbl> 37.88, 37.86, 37.85, 37.85, 37.85, 37.84, 3~
    ## $ median_income              <dbl> 1.84205233775, 1.83600625624, 1.55216359856~
    ## $ rooms_per_household        <dbl> 1.06763882, 0.65677817, 1.69027265, 0.68222~
    ## $ population_per_household   <dbl> -0.3912991, -1.0997473, -0.0507250, -0.9762~
    ## $ price_category             <fct> above, above, above, above, above, above, a~
    ## $ ocean_proximity_INLAND     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ ocean_proximity_ISLAND     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ ocean_proximity_NEAR.BAY   <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1~
    ## $ ocean_proximity_NEAR.OCEAN <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~

### visualising the numeric prepped data

``` r
prepped_data |>
  sample_n(1000) |> # speed
  select(price_category, median_income, rooms_per_household, population_per_household) |>
  ggscatmat(corMethod = "spearman", alpha = 0.25)
```

![](tidymodels_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

### cross-validation

``` r
set.seed(100)
cv_folds = vfold_cv(train_data, v = 5, strata = price_category)
# will come back to this after specifying models
```

### specifying models

1.  pick a `model type`
2.  set the `engine`
3.  set the `mode` (regression or classification)

### specifying logistic regression model

``` r
log_spec = logistic_reg() |> # model type
  set_engine(engine = "glm") |> # model engine
  set_mode("classification") # model mode

log_spec
```

    ## Logistic Regression Model Specification (classification)
    ## 
    ## Computational engine: glm

### specifying random forest model

``` r
rf_spec = rand_forest() |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

rf_spec
```

    ## Random Forest Model Specification (classification)
    ## 
    ## Engine-Specific Arguments:
    ##   importance = impurity
    ## 
    ## Computational engine: ranger

### specifying boosted tree (XGBoost) model

``` r
xgb_spec = boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification")

xgb_spec
```

    ## Boosted Tree Model Specification (classification)
    ## 
    ## Computational engine: xgboost

### specifying k-nearest neighbor model

``` r
knn_spec = nearest_neighbor(neighbors = 4) |> # note that the number of neighbors can be specified
  set_engine("kknn") |>
  set_mode("classification")

knn_spec
```

    ## K-Nearest Neighbor Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   neighbors = 4
    ## 
    ## Computational engine: kknn

### specifying neural network model

``` r
nnet_spec = mlp() |>
  set_mode("classification") |>
  set_engine("keras", verbose = 0)

nnet_spec
```

    ## Single Layer Neural Network Specification (classification)
    ## 
    ## Engine-Specific Arguments:
    ##   verbose = 0
    ## 
    ## Computational engine: keras

### creating logistic regression workflow

``` r
log_wflow = workflow() |>
  add_recipe(housing_rec) |>
  add_model(log_spec)

log_wflow
```

    ## == Workflow ====================================================================
    ## Preprocessor: Recipe
    ## Model: logistic_reg()
    ## 
    ## -- Preprocessor ----------------------------------------------------------------
    ## 7 Recipe Steps
    ## 
    ## * step_log()
    ## * step_naomit()
    ## * step_novel()
    ## * step_normalize()
    ## * step_dummy()
    ## * step_zv()
    ## * step_corr()
    ## 
    ## -- Model -----------------------------------------------------------------------
    ## Logistic Regression Model Specification (classification)
    ## 
    ## Computational engine: glm

### creating random forest workflow

``` r
rf_wflow = workflow() |>
  add_recipe(housing_rec) |>
  add_model(rf_spec)

rf_wflow
```

    ## == Workflow ====================================================================
    ## Preprocessor: Recipe
    ## Model: rand_forest()
    ## 
    ## -- Preprocessor ----------------------------------------------------------------
    ## 7 Recipe Steps
    ## 
    ## * step_log()
    ## * step_naomit()
    ## * step_novel()
    ## * step_normalize()
    ## * step_dummy()
    ## * step_zv()
    ## * step_corr()
    ## 
    ## -- Model -----------------------------------------------------------------------
    ## Random Forest Model Specification (classification)
    ## 
    ## Engine-Specific Arguments:
    ##   importance = impurity
    ## 
    ## Computational engine: ranger

### creating XGBoost workflow

``` r
xgb_wflow = workflow() |>
  add_recipe(housing_rec) |>
  add_model(xgb_spec)

xgb_wflow
```

    ## == Workflow ====================================================================
    ## Preprocessor: Recipe
    ## Model: boost_tree()
    ## 
    ## -- Preprocessor ----------------------------------------------------------------
    ## 7 Recipe Steps
    ## 
    ## * step_log()
    ## * step_naomit()
    ## * step_novel()
    ## * step_normalize()
    ## * step_dummy()
    ## * step_zv()
    ## * step_corr()
    ## 
    ## -- Model -----------------------------------------------------------------------
    ## Boosted Tree Model Specification (classification)
    ## 
    ## Computational engine: xgboost

### creating k-nearest neighbor workflow

``` r
knn_wflow = workflow() |>
  add_recipe(housing_rec) |>
  add_model(knn_spec)

knn_wflow
```

    ## == Workflow ====================================================================
    ## Preprocessor: Recipe
    ## Model: nearest_neighbor()
    ## 
    ## -- Preprocessor ----------------------------------------------------------------
    ## 7 Recipe Steps
    ## 
    ## * step_log()
    ## * step_naomit()
    ## * step_novel()
    ## * step_normalize()
    ## * step_dummy()
    ## * step_zv()
    ## * step_corr()
    ## 
    ## -- Model -----------------------------------------------------------------------
    ## K-Nearest Neighbor Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   neighbors = 4
    ## 
    ## Computational engine: kknn

### creating neural network workflow

``` r
nnet_wflow = workflow() |>
  add_recipe(housing_rec) |>
  add_model(nnet_spec)

nnet_wflow
```

    ## == Workflow ====================================================================
    ## Preprocessor: Recipe
    ## Model: mlp()
    ## 
    ## -- Preprocessor ----------------------------------------------------------------
    ## 7 Recipe Steps
    ## 
    ## * step_log()
    ## * step_naomit()
    ## * step_novel()
    ## * step_normalize()
    ## * step_dummy()
    ## * step_zv()
    ## * step_corr()
    ## 
    ## -- Model -----------------------------------------------------------------------
    ## Single Layer Neural Network Specification (classification)
    ## 
    ## Engine-Specific Arguments:
    ##   verbose = 0
    ## 
    ## Computational engine: keras

### evaluating logistic regression

``` r
log_res = log_wflow |>
  fit_resamples(resamples = cv_folds,
                metrics = metric_set(recall, precision, f_meas,
                                     accuracy, kap, roc_auc, sens, spec),
                control = control_resamples(save_pred = T))

log_res
```

    ## # Resampling results
    ## # 5-fold cross-validation using stratification 
    ## # A tibble: 5 x 5
    ##   splits               id    .metrics         .notes           .predictions
    ##   <list>               <chr> <list>           <list>           <list>      
    ## 1 <split [12383/3097]> Fold1 <tibble [8 x 4]> <tibble [0 x 1]> <tibble>    
    ## 2 <split [12383/3097]> Fold2 <tibble [8 x 4]> <tibble [0 x 1]> <tibble>    
    ## 3 <split [12384/3096]> Fold3 <tibble [8 x 4]> <tibble [0 x 1]> <tibble>    
    ## 4 <split [12385/3095]> Fold4 <tibble [8 x 4]> <tibble [0 x 1]> <tibble>    
    ## 5 <split [12385/3095]> Fold5 <tibble [8 x 4]> <tibble [0 x 1]> <tibble>

``` r
tictoc::toc()
```

    ## 23.22 sec elapsed
