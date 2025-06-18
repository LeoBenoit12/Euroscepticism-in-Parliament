Regressions
================
Duarte Amaro
2025-06-18

# Descriptive Statistics

## Salience

### Plotting the frequency of speeches by party and country:

``` r
data_monthly <- read_csv("data_monthly.csv") |>
  mutate(year_month = factor(year_month))
```

    ## Rows: 6686 Columns: 25
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (11): country, name_short, name, name_english, year_month, last_electio...
    ## dbl  (13): partyfacts_id, parlgov_id, prop_eu_mention, prop_euroscepticism, ...
    ## date  (1): election_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
data_yearly <- read_csv("data_yearly.csv")
```

    ## Rows: 643 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): country
    ## dbl (4): partyfacts_id, year, prop_eu_mention, prop_euroscepticism
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
data_monthly |>
  mutate(next_election_year_month = as.Date(paste(next_election_year_month, "-01", sep = "")),
         year_month = as.Date(paste(year_month, "-01", sep = ""))
         ) |>
  filter(country == "AUS") |>
  ggplot(aes(x = year_month, y = prop_eu_mention)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(alpha = 0.2, colour = "blue", fill = "blue",
              span = 0.5) +
  geom_vline(aes(xintercept = next_election_year_month), linetype = "dashed", color = "black") +
  facet_wrap(~name_short) +
  ylim(0, 15) +
  labs(x = "Year",
       y = "Share of sentences about the EU",
       title = "Share of sentences about the EU in Austria by party") +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 5 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 5 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](regression_final_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
data_monthly |>
  mutate(next_election_year_month = as.Date(paste(next_election_year_month, "-01", sep = "")),
         year_month = as.Date(paste(year_month, "-01", sep = ""))
         ) |>
  filter(country == "DEU") |>
  ggplot(aes(x = year_month, y = prop_eu_mention)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(alpha = 0.2, colour = "blue", fill = "blue",
              span = 0.5) +
  geom_vline(aes(xintercept = next_election_year_month), linetype = "dashed", color = "black") +
  facet_wrap(~name_short) +
  ylim(0, 15) +
  labs(x = "Year",
       y = "Share of sentences about the EU",
       title = "Share of sentences about the EU in Germany by party") +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 4 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 4 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](regression_final_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
data_monthly |> 
  mutate(next_election_year_month = as.Date(paste(next_election_year_month, "-01", sep = "")),
         year_month = as.Date(paste(year_month, "-01", sep = ""))
         ) |>
  filter(country == "DNK") |>
  ggplot(aes(x = year_month, y = prop_eu_mention)) +
  #geom_path(group=1, alpha = 0.4) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(alpha = 0.2, colour = "blue", fill = "blue",
              span = 0.5) +
  geom_vline(aes(xintercept = next_election_year_month), linetype = "dashed", color = "black") +
  facet_wrap(~name_short) +
  ylim(0, 15) +
  labs(x = "Year",
       y = "Share of sentences about the EU",
       title = "Share of sentences about the EU in Denmark by party") +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 11 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 11 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 32 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

![](regression_final_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

``` r
data_monthly |>
  mutate(next_election_year_month = as.Date(paste(next_election_year_month, "-01", sep = "")),
         year_month = as.Date(paste(year_month, "-01", sep = ""))
         ) |>
  filter(country == "ESP") |>
  ggplot(aes(x = year_month, y = prop_eu_mention)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(alpha = 0.2, colour = "blue", fill = "blue",
              span = 0.5) +
  geom_vline(aes(xintercept = next_election_year_month), linetype = "dashed", color = "black") +
  facet_wrap(~name_short) +
  ylim(0, 15) +
  labs(x = "Year",
       y = "Share of sentences about the EU",
       title = "Share of sentences about the EU in Spain by party") +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 2 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 2 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](regression_final_files/figure-gfm/unnamed-chunk-2-4.png)<!-- -->

``` r
data_monthly |>
  mutate(next_election_year_month = as.Date(paste(next_election_year_month, "-01", sep = "")),
         year_month = as.Date(paste(year_month, "-01", sep = ""))
         ) |>
  filter(country == "NLD") |>
  ggplot(aes(x = year_month, y = prop_eu_mention)) +
  #geom_path(group=1, alpha = 0.4) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(alpha = 0.2, colour = "blue", fill = "blue",
              span = 0.5) +
  geom_vline(aes(xintercept = next_election_year_month), linetype = "dashed", color = "black") +
  facet_wrap(~name_short) +
  ylim(0, 15) +
  labs(x = "Year",
       y = "Share of sentences about the EU",
       title = "Share of sentences about the EU in the Netherlands by party") +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 5 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 5 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

![](regression_final_files/figure-gfm/unnamed-chunk-2-5.png)<!-- -->

``` r
data_monthly |>
  mutate(next_election_year_month = as.Date(paste(next_election_year_month, "-01", sep = "")),
         year_month = as.Date(paste(year_month, "-01", sep = ""))
         ) |>
  filter(country == "SWE") |>
  ggplot(aes(x = year_month, y = prop_eu_mention)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(alpha = 0.2, colour = "blue", fill = "blue",
              span = 0.5) +
  geom_vline(aes(xintercept = next_election_year_month),
             linetype = "dashed", color = "black") +
  facet_wrap(~name_short) +
  ylim(0, 15) +
  labs(x = "Year",
       y = "Share of sentences about the EU",
       title = "Share of sentences about the EU in Sweden by party") +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](regression_final_files/figure-gfm/unnamed-chunk-2-6.png)<!-- -->

``` r
data_monthly |>
  mutate(next_election_year_month = as.Date(paste(next_election_year_month, "-01", sep = "")),
         year_month = as.Date(paste(year_month, "-01", sep = ""))
         ) |>
  filter(country == "GBR") |>
  ggplot(aes(x = year_month, y = prop_eu_mention)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(alpha = 0.2, colour = "blue", fill = "blue",
              span = 0.5) +
  geom_vline(aes(xintercept = next_election_year_month), linetype = "dashed", color = "black") +
  facet_wrap(~name_short, scales = "free_y") +
  #ylim(0, 50) +
  labs(x = "Year",
       y = "Share of sentences about the EU",
       title = "Share of sentences about the EU in the UK by party") +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](regression_final_files/figure-gfm/unnamed-chunk-2-7.png)<!-- -->

``` r
data_monthly |>
  ggplot(aes(x = reorder(name_short, prop_eu_mention),
             y = prop_eu_mention,
             colour = country,
             fill = country)) +
  geom_boxplot(alpha = 0.2) +
  #facet_wrap(~ country, scales = "free")
  coord_flip() +
  scale_color_brewer(name = "", palette = "Dark2") +
  scale_fill_brewer(name = "", palette = "Dark2") +
  labs(x = "", y = "Share of sentences about the EU",
       title = "Monthly share of sentences about the EU by party")
```

![](regression_final_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Position

### Share of Eurosceptic sentences by country by party for each country:

``` r
data_monthly |>
  mutate(next_election_year_month = as.Date(paste(next_election_year_month, "-01", sep = "")),
         year_month = as.Date(paste(year_month, "-01", sep = ""))
         ) |>
  filter(country == "AUS") |>
  ggplot(aes(x = year_month, y = prop_euroscepticism)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(alpha = 0.2, colour = "blue", fill = "blue",
              span = 0.5) +
  geom_vline(aes(xintercept = next_election_year_month), linetype = "dashed", color = "black") +
  facet_wrap(~name_short) +
  #ylim(0, 60) +
  labs(x = "Year",
        y = "Share of Eurosceptic sentences",
       title = "Eurosceptic sentences in Austria as share of all sentences about the EU") +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 10 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 10 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](regression_final_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
data_monthly |>
  mutate(next_election_year_month = as.Date(paste(next_election_year_month, "-01", sep = "")),
         year_month = as.Date(paste(year_month, "-01", sep = ""))
         ) |>
  filter(country == "DEU") |>
  ggplot(aes(x = year_month, y = prop_euroscepticism)) +
  #geom_path(group=1, alpha = 0.4) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(alpha = 0.2, colour = "blue", fill = "blue",
              span = 0.5) +
  geom_vline(aes(xintercept = next_election_year_month), linetype = "dashed", color = "black") +
  facet_wrap(~name_short) +
  #ylim(0, 15) +
  labs(x = "Year",
        y = "Share of Eurosceptic sentences",
       title = "Eurosceptic sentences in Germany as share of all sentences about the EU") +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 10 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).
    ## Removed 10 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](regression_final_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
data_monthly |>
  mutate(next_election_year_month = as.Date(paste(next_election_year_month, "-01", sep = "")),
         year_month = as.Date(paste(year_month, "-01", sep = ""))
         ) |>
  filter(country == "DNK") |>
  ggplot(aes(x = year_month, y = prop_euroscepticism)) +
  #geom_path(group=1, alpha = 0.4) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(alpha = 0.2, colour = "blue", fill = "blue",
              span = 0.5) +
  geom_vline(aes(xintercept = next_election_year_month), linetype = "dashed", color = "black") +
  facet_wrap(~name_short) +
  #ylim(0, 15) +
  labs(x = "Year",
        y = "Share of Eurosceptic sentences",
       title = "Eurosceptic sentences in Denmark as share of all sentences about the EU") +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 75 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning in sqrt(sum.squares/one.delta): NaNs produced

    ## Warning in stats::qt(level/2 + 0.5, pred$df): NaNs produced

    ## Warning: Removed 75 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning
    ## -Inf

![](regression_final_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
data_monthly |> 
  mutate(next_election_year_month = as.Date(paste(next_election_year_month, "-01", sep = "")),
         year_month = as.Date(paste(year_month, "-01", sep = ""))
         ) |>
  filter(country == "ESP") |>
  ggplot(aes(x = year_month, y = prop_euroscepticism)) +
  #geom_path(group=1, alpha = 0.4) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(alpha = 0.2, colour = "blue", fill = "blue",
              span = 0.5) +
  geom_vline(aes(xintercept = next_election_year_month), linetype = "dashed", color = "black") +
  facet_wrap(~name_short) +
  #ylim(0, 15) +
  labs(x = "Year",
        y = "Share of Eurosceptic sentences",
       title = "Eurosceptic sentences in Spain as share of all sentences about the EU") +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](regression_final_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

``` r
data_monthly |>
  mutate(next_election_year_month = as.Date(paste(next_election_year_month, "-01", sep = "")),
         year_month = as.Date(paste(year_month, "-01", sep = ""))
         ) |>
  filter(country == "NLD") |>
  ggplot(aes(x = year_month, y = prop_euroscepticism)) +
  #geom_path(group=1, alpha = 0.4) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(alpha = 0.2, colour = "blue", fill = "blue",
              span = 0.5) +
  geom_vline(aes(xintercept = next_election_year_month), linetype = "dashed", color = "black") +
  facet_wrap(~name_short) +
  #ylim(0, 15) +
  labs(x = "Year",
        y = "Share of Eurosceptic sentences",
       title = "Eurosceptic sentences in the Netherlands as share of all sentences about the EU") +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 45 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 45 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](regression_final_files/figure-gfm/unnamed-chunk-4-5.png)<!-- -->

``` r
data_monthly |>
  mutate(next_election_year_month = as.Date(paste(next_election_year_month, "-01", sep = "")),
         year_month = as.Date(paste(year_month, "-01", sep = ""))
         ) |>
  filter(country == "SWE") |>
  ggplot(aes(x = year_month, y = prop_euroscepticism)) +
  #geom_path(group=1, alpha = 0.4) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(alpha = 0.2, colour = "blue", fill = "blue",
              span = 0.5) +
  geom_vline(aes(xintercept = next_election_year_month), linetype = "dashed", color = "black") +
  facet_wrap(~name_short) +
  #ylim(0, 15) +
  labs(x = "Year",
        y = "Share of Eurosceptic sentences",
       title = "Eurosceptic sentences in Sweden as share of all sentences about the EU") +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 28 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 28 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](regression_final_files/figure-gfm/unnamed-chunk-4-6.png)<!-- -->

``` r
data_monthly |>
  mutate(next_election_year_month = as.Date(paste(next_election_year_month, "-01", sep = "")),
         year_month = as.Date(paste(year_month, "-01", sep = ""))
         ) |>
  filter(country == "GBR") |>
  ggplot(aes(x = year_month, y = prop_euroscepticism)) +
  #geom_path(group=1, alpha = 0.4) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(alpha = 0.2, colour = "blue", fill = "blue",
              span = 0.5) +
  geom_vline(aes(xintercept = next_election_year_month), linetype = "dashed", color = "black") +
  facet_wrap(~name_short) +
  ylim(0, 100) +
  labs(x = "Year",
        y = "Share of Eurosceptic sentences",
       title = "Eurosceptic sentences in the UK as share of all sentences about the EU") +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 6 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 6 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 24 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

![](regression_final_files/figure-gfm/unnamed-chunk-4-7.png)<!-- -->

``` r
data_monthly |>
  group_by(country, name_short) |>
  mutate(mean_euroscepticism = mean(prop_euroscepticism, na.rm = TRUE)) |>
  ungroup() |>
  arrange(mean_euroscepticism) |>
  ggplot(aes(x = reorder(name_short, mean_euroscepticism),
             y = prop_euroscepticism,
             colour = country,
             fill = country)) +
  geom_boxplot(alpha = 0.2) +
  #facet_wrap(~ country, scales = "free")
  coord_flip() +
  scale_color_brewer(name = "", palette = "Dark2") +
  scale_fill_brewer(name = "", palette = "Dark2") +
  labs(x = "", y = "Share of Eurosceptic sentences",
       title = "Monthly share of Eurosceptic sentences by party")
```

    ## Warning: Removed 191 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](regression_final_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Plotting salience against position

``` r
data_monthly |>
  ggplot(aes(x = prop_eu_mention, y = prop_euroscepticism)) +
  geom_point(alpha = 0.1) +
  geom_smooth(colour = "blue", fill = "blue", alpha = 0.2) +
  facet_wrap(~ country, scales = "free") +
  labs(x = "Share of sentences about the EU",
       y = "Share of Eurosceptic sentences",
       title = "Overall share of Eurosceptic sentences against monthly share of sentences about the EU") +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 191 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 191 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](regression_final_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# Validating Against CHES

``` r
## Getting ches and partyfacts codes:

partyfacts_to_ches <- read_csv("partyfacts-external-parties.csv") |> 
  filter((partyfacts_id %in% unique(data_yearly$partyfacts_id) | partyfacts_id == 540) & # For some reason, PartyFacts codes LibDems differently for ParlGov
           dataset_key == "ches" &
           year_last >= 1998) |>
  rename(ches_id = dataset_party_id,
         pf_country = country) |>
  mutate(ches_id = as.numeric(ches_id))
```

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

    ## Rows: 45385 Columns: 17
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (8): country, dataset_key, dataset_party_id, name_short, name, name_eng...
    ## dbl  (6): year_first, year_last, share, share_year, external_id, partyfacts_id
    ## dttm (3): created, modified, linked
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ches <- haven::read_dta("1999-2019_CHES_dataset_means(v3).dta") |>
  rename(ches_id = party_id,
         ches_country = country) |>
  dplyr::select(ches_country, ches_id, year, party, eu_position, eu_salience, eu_blur)

joint_ches <- data_yearly |>
  left_join(partyfacts_to_ches, by = "partyfacts_id") |>
  left_join(ches, by = c("ches_id", "year")) |>
  mutate(
    standard_eu_mention = (prop_eu_mention - mean(prop_eu_mention, na.rm = TRUE))/sd(prop_eu_mention, na.rm = TRUE),
    standard_eu_stance = (prop_euroscepticism - mean(prop_euroscepticism, na.rm = TRUE))/sd(prop_euroscepticism, na.rm = TRUE),
    standard_eu_salience = (eu_salience - mean(eu_salience, na.rm = TRUE))/sd(eu_salience, na.rm = TRUE),
    standard_eu_position = (eu_position - mean(eu_position, na.rm = TRUE))/sd(eu_position, na.rm = TRUE)
  )


joint_ches |>
  ggplot(aes(x = eu_position, y = prop_euroscepticism)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", colour = "blue", fill = "blue", alpha = 0.2) +
  ggrepel::geom_text_repel(aes(label = ifelse(abs(prop_euroscepticism-(37 - 4*eu_position))>15,
                                               paste(party, year, sep = ", "),
                                               NA)),
                            size = 3, max.overlaps = 15) +
  ggpubr::stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~~")),
                                label.x = 1.1, label.y = 5) +
  labs(x = "CHES Position",
       y = "Share of Eurosceptic sentences",
       title = "CHES Position against Share of Eurosceptic Sentences")
```

    ## Warning: The dot-dot notation (`..eq.label..`) was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `after_stat(eq.label)` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 486 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 486 rows containing non-finite outside the scale range
    ## (`stat_regline_equation()`).

    ## Warning: Removed 486 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 637 rows containing missing values or values outside the scale range
    ## (`geom_text_repel()`).

![](regression_final_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# Regressions

## Using RRP voteshare in the last election

### Pooled

``` r
reg_data <- read_csv("reg_data.csv") |>
  mutate(year = year_month |>
           paste("01", sep = "-") |>
           as.Date() |>
           format("%Y"),
         year_month = factor(year_month),
         parlgov_id = factor(parlgov_id)
         )
```

    ## Rows: 3278 Columns: 30
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (7): country, name_short, name, name_english, year_month, family_name,...
    ## dbl  (22): parlgov_id, prop_eu_mention, prop_euroscepticism, time_from_last,...
    ## date  (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
m1 <- lm(data = reg_data,
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + country + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party)

m2 <- lm(data = reg_data,
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + country + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id)

m3 <- lm(data = reg_data,
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + country + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion)

m4 <- lm(data = reg_data,
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + country +
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               time_to_next*rrp_voteshare + parlgov_id + eu_public_opinion)

m5 <- lm(data = reg_data,
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + country +
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party + family_name +
               family_name*rrp_voteshare + parlgov_id + eu_public_opinion)

m6 <- lm(data = reg_data,
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + country +
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party + family_2 +
               family_2*rrp_voteshare + parlgov_id + eu_public_opinion)

m7 <- lm(data = reg_data,
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + country +
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party + parlgov_id +
               change_voteshare*rrp_voteshare + eu_public_opinion)

m8 <- lm(data = reg_data,
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + country +
               log(time_to_next + 1) + log(time_to_next_ep + 1) + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party + parlgov_id + eu_public_opinion)

stargazer::stargazer(m1, m2, m3, m4, m6, m7, 
                     omit.labels = c("Party", "Country"),
                     type = "text", title = "Table 1: Regression Using RRP Vote Share",
                     #out = "table.html",
                     dep.var.labels = "Share of Eurosceptic sentences",
                      covariate.labels = c("Time", "Lagged Euroscepticism",
                                          "Time to next election (months)",
                                          "Time to next EP election (months)",
                                           "Party size (vote share)",
                                           "Change in vote share",
                                           "RRP vote share",
                                           "Party in Government",
                                          "Centre-Right Party",
                                          "Pro EU Public Opinion",
                                          "Time to next election * RRP vote share",
                                          "RRP vote share * Centre-Right",
                                          "RRP vote share * Change in vote share"
                                           ),
                     omit = c("parlgov_id", "country"),
                     omit.stat = c("f", "ser"),
                     star.cutoffs = c(0.05, 0.01, 0.001))
```

    ## 
    ## Table 1: Regression Using RRP Vote Share
    ## ==================================================================================================
    ##                                                            Dependent variable:                    
    ##                                        -----------------------------------------------------------
    ##                                                      Share of Eurosceptic sentences               
    ##                                           (1)       (2)       (3)       (4)       (5)       (6)   
    ## --------------------------------------------------------------------------------------------------
    ## Time                                    0.008**   0.007**  0.010***  0.009***  0.010***  0.010*** 
    ##                                         (0.002)   (0.002)   (0.003)   (0.003)   (0.003)   (0.003) 
    ##                                                                                                   
    ## Lagged Euroscepticism                  0.109***  0.087***  0.086***  0.086***  0.086***  0.086*** 
    ##                                         (0.018)   (0.018)   (0.018)   (0.018)   (0.018)   (0.018) 
    ##                                                                                                   
    ## Time to next election (months)          -0.002    -0.003    -0.004    -0.010    -0.004    -0.004  
    ##                                         (0.009)   (0.009)   (0.009)   (0.013)   (0.009)   (0.009) 
    ##                                                                                                   
    ## Time to next EP election (months)       -0.008    -0.008    -0.012    -0.012    -0.012    -0.012  
    ##                                         (0.007)   (0.007)   (0.007)   (0.007)   (0.007)   (0.007) 
    ##                                                                                                   
    ## Party size (vote share)                -0.111*** -0.143*** -0.139*** -0.139*** -0.136*** -0.138***
    ##                                         (0.029)   (0.036)   (0.036)   (0.036)   (0.037)   (0.037) 
    ##                                                                                                   
    ## Change in vote share                    0.065*    0.063*    0.072*    0.073**   0.070*    0.088*  
    ##                                         (0.026)   (0.028)   (0.028)   (0.028)   (0.029)   (0.037) 
    ##                                                                                                   
    ## RRP vote share                           0.021     0.010     0.032     0.017     0.048     0.023  
    ##                                         (0.036)   (0.036)   (0.038)   (0.043)   (0.049)   (0.040) 
    ##                                                                                                   
    ## Party in Government                    -3.214*** -3.041*** -3.066*** -3.077*** -3.056*** -3.110***
    ##                                         (0.301)   (0.317)   (0.318)   (0.318)   (0.318)   (0.325) 
    ##                                                                                                   
    ## Centre-Right Party                                                               0.732            
    ##                                                                                 (1.064)           
    ##                                                                                                   
    ## Pro EU Public Opinion                                      -5.128**  -5.165**  -5.151**  -5.255** 
    ##                                                             (1.913)   (1.914)   (1.914)   (1.923) 
    ##                                                                                                   
    ## Time to next election * RRP vote share                                 0.001                      
    ##                                                                       (0.001)                     
    ##                                                                                                   
    ## RRP vote share * Centre-Right                                                   -0.029            
    ##                                                                                 (0.057)           
    ##                                                                                                   
    ## RRP vote share * Change in vote share                                                     -0.002  
    ##                                                                                           (0.003) 
    ##                                                                                                   
    ## Constant                               15.886*** 16.546*** 18.465*** 18.611*** 17.901*** 18.613***
    ##                                         (1.352)   (1.529)   (1.687)   (1.701)   (1.935)   (1.702) 
    ##                                                                                                   
    ## --------------------------------------------------------------------------------------------------
    ## Party                                     No        No        No        No        No        No    
    ## Country                                   Yes       Yes       Yes       Yes       Yes       Yes   
    ## --------------------------------------------------------------------------------------------------
    ## Observations                             3,153     3,153     3,140     3,140     3,140     3,140  
    ## R2                                       0.405     0.416     0.417     0.417     0.417     0.417  
    ## Adjusted R2                              0.402     0.412     0.413     0.413     0.413     0.413  
    ## ==================================================================================================
    ## Note:                                                                *p<0.05; **p<0.01; ***p<0.001

### Per country

``` r
m_aus1 <- lm(data = reg_data |> filter(country == "AUS"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion)

m_aus2 <- lm(data = reg_data |> filter(country == "AUS"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion +  time_to_next*rrp_voteshare)

m_aus3 <- lm(data = reg_data |> filter(country == "AUS"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + change_voteshare*rrp_voteshare)

m_deu1 <- lm(data = reg_data |> filter(country == "DEU"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion)

m_deu2 <- lm(data = reg_data |> filter(country == "DEU"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + time_to_next*rrp_voteshare)

m_deu3 <- lm(data = reg_data |> filter(country == "DEU"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + change_voteshare*rrp_voteshare)

m_den1 <- lm(data = reg_data |> filter(country == "DNK"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + eu_public_opinion + rrp_voteshare + cabinet_party +
               parlgov_id)

m_den2 <- lm(data = reg_data |> filter(country == "DNK"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + time_to_next*rrp_voteshare)

m_den3 <- lm(data = reg_data |> filter(country == "DNK"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + change_voteshare*rrp_voteshare)

m_swe1 <- lm(data = reg_data |> filter(country == "SWE"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion)

m_swe2 <- lm(data = reg_data |> filter(country == "SWE"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + time_to_next*rrp_voteshare)

m_swe3 <- lm(data = reg_data |> filter(country == "SWE"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + change_voteshare*rrp_voteshare)

m_esp1 <- lm(data = reg_data |> filter(country == "ESP"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion)

m_esp2 <- lm(data = reg_data |> filter(country == "ESP"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + time_to_next*rrp_voteshare)

m_esp3 <- lm(data = reg_data |> filter(country == "ESP"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + change_voteshare*rrp_voteshare)

m_nld1 <- lm(data = reg_data |> filter(country == "NLD"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion)

m_nld2 <- lm(data = reg_data |> filter(country == "NLD"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + time_to_next*rrp_voteshare)

m_nld3 <- lm(data = reg_data |> filter(country == "NLD"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + change_voteshare*rrp_voteshare)

m_uk1 <- lm(data = reg_data |> filter(country == "GBR"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion)

m_uk2 <- lm(data = reg_data |> filter(country == "GBR"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + time_to_next*rrp_voteshare)

m_uk3 <- lm(data = reg_data |> filter(country == "GBR"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + change_voteshare*rrp_voteshare)

stargazer::stargazer(m_aus1, m_deu1,m_den1, m_swe1, m_esp1, m_nld1, m_uk1,
                     omit.labels = c("Party"),
                     type = "text", title = "Table 2: Regression Using RRP Vote Share Per Country",
                     #out = "table.html",
                     dep.var.labels = "Share of Eurosceptic sentences",
                     column.labels = c("AUS", "DEU", "DNK", "SWE", "ESP", "NLD", "GBR"),
                     covariate.labels = c("Time", "Lagged Euroscepticism",
                                          "Time to next election (months)",
                                          "Time to next EP election (months)",
                                           "Party size (vote share)",
                                           "Change in vote share",
                                           "RRP vote share",
                                           "Party in Government",
                                          "Pro EU Public Opinion"
                                           ),
                     omit = c("parlgov_id"),
                     omit.stat = c("f", "ser"),
                     star.cutoffs = c(0.05, 0.01, 0.001))
```

    ## 
    ## Table 2: Regression Using RRP Vote Share Per Country
    ## =======================================================================================================
    ##                                                            Dependent variable:                         
    ##                                   ---------------------------------------------------------------------
    ##                                                      Share of Eurosceptic sentences                    
    ##                                      AUS       DEU       DNK       SWE       ESP       NLD       GBR   
    ##                                      (1)       (2)       (3)       (4)       (5)       (6)       (7)   
    ## -------------------------------------------------------------------------------------------------------
    ## Time                                0.006     0.012     0.030    -0.004    0.013*     0.014     0.001  
    ##                                    (0.008)   (0.007)   (0.019)   (0.013)   (0.005)   (0.007)   (0.006) 
    ##                                                                                                        
    ## Lagged Euroscepticism              -0.008   0.223***    0.071    -0.097     0.032     0.060     0.044  
    ##                                    (0.049)   (0.047)   (0.053)   (0.050)   (0.050)   (0.039)   (0.049) 
    ##                                                                                                        
    ## Time to next election (months)     -0.027   -0.057**    0.028    0.040*    -0.004    -0.010     0.010  
    ##                                    (0.025)   (0.018)   (0.048)   (0.018)   (0.019)   (0.023)   (0.018) 
    ##                                                                                                        
    ## Time to next EP election (months)  -0.018     0.018    -0.052   -0.031**   -0.011    -0.016    -0.007  
    ##                                    (0.022)   (0.013)   (0.036)   (0.012)   (0.014)   (0.019)   (0.017) 
    ##                                                                                                        
    ## Party size (vote share)           -0.449**   -0.162    -0.310   -0.202**    0.008     0.041     0.267  
    ##                                    (0.165)   (0.084)   (0.248)   (0.065)   (0.066)   (0.081)   (0.203) 
    ##                                                                                                        
    ## Change in vote share                0.116    0.128*     0.099    -0.083     0.069    -0.062     0.077  
    ##                                    (0.095)   (0.064)   (0.189)   (0.057)   (0.056)   (0.064)   (0.096) 
    ##                                                                                                        
    ## RRP vote share                     -0.112    0.284*     0.091    -0.195               0.109     0.241  
    ##                                    (0.101)   (0.121)   (0.271)   (0.142)             (0.090)   (0.135) 
    ##                                                                                                        
    ## Party in Government                -3.116*  -3.446***  -2.629*   -1.306*  -3.619*** -4.501*** -7.077***
    ##                                    (1.220)   (0.763)   (1.261)   (0.657)   (0.779)   (0.825)   (1.332) 
    ##                                                                                                        
    ## Pro EU Public Opinion              -17.662    1.825   -37.770*   -2.743   -11.187**  -1.523    -3.340  
    ##                                   (10.301)   (4.190)  (15.557)   (5.565)   (3.404)   (5.672)   (5.131) 
    ##                                                                                                        
    ## Constant                          42.121*** 14.901*** 47.637*** 16.305*** 12.585**  13.771**  18.004** 
    ##                                    (9.392)   (3.875)  (11.936)   (2.841)   (4.156)   (4.369)   (6.010) 
    ##                                                                                                        
    ## -------------------------------------------------------------------------------------------------------
    ## Party                                Yes       Yes       Yes       Yes       Yes       Yes       Yes   
    ## -------------------------------------------------------------------------------------------------------
    ## Observations                         430       424       372       401       413       666       434   
    ## R2                                  0.080     0.327     0.053     0.222     0.149     0.127     0.327  
    ## Adjusted R2                         0.058     0.310     0.027     0.202     0.130     0.112     0.311  
    ## =======================================================================================================
    ## Note:                                                                     *p<0.05; **p<0.01; ***p<0.001

``` r
stargazer::stargazer(m_aus2, m_deu2,m_den2, m_swe2, m_esp2, m_nld2, m_uk2,
                     omit.labels = c("Party"),
                     type = "text", title = "Table 3: Interaction between RRP Vote Share and Time to Next Election Per Country",
                     #out = "table.html",
                     dep.var.labels = "Share of Eurosceptic sentences",
                     column.labels = c("AUS", "DEU", "DNK", "SWE", "ESP", "NLD", "GBR"),
                      covariate.labels = c("Time", "Lagged Euroscepticism",
                                          "Time to next election (months)",
                                          "Time to next EP election (months)",
                                           "Party size (vote share)",
                                           "Change in vote share",
                                           "RRP vote share",
                                           "Party in Government",
                                          "Pro EU Public Opinion",
                                          "Time to next election * RRP vote share"
                                           ),
                     omit = c("parlgov_id"),
                     omit.stat = c("f", "ser"),
                     star.cutoffs = c(0.05, 0.01, 0.001))
```

    ## 
    ## Table 3: Interaction between RRP Vote Share and Time to Next Election Per Country
    ## ============================================================================================================
    ##                                                                 Dependent variable:                         
    ##                                        ---------------------------------------------------------------------
    ##                                                           Share of Eurosceptic sentences                    
    ##                                           AUS       DEU       DNK       SWE       ESP       NLD       GBR   
    ##                                           (1)       (2)       (3)       (4)       (5)       (6)       (7)   
    ## ------------------------------------------------------------------------------------------------------------
    ## Time                                     0.005     0.012     0.029    -0.004    0.013*     0.006    -0.0004 
    ##                                         (0.008)   (0.007)   (0.019)   (0.013)   (0.005)   (0.008)   (0.006) 
    ##                                                                                                             
    ## Lagged Euroscepticism                   -0.011   0.225***    0.068    -0.098*    0.032     0.060     0.042  
    ##                                         (0.049)   (0.047)   (0.053)   (0.050)   (0.050)   (0.039)   (0.049) 
    ##                                                                                                             
    ## Time to next election (months)          -0.093    -0.048    -0.140    0.049*    -0.004    -0.112*   -0.009  
    ##                                         (0.097)   (0.027)   (0.191)   (0.025)   (0.019)   (0.052)   (0.041) 
    ##                                                                                                             
    ## Time to next EP election (months)       -0.024     0.017    -0.062    -0.029*   -0.011    -0.017    -0.008  
    ##                                         (0.023)   (0.014)   (0.037)   (0.013)   (0.014)   (0.019)   (0.017) 
    ##                                                                                                             
    ## Party size (vote share)                -0.471**   -0.158    -0.301   -0.203**    0.008     0.049     0.240  
    ##                                         (0.167)   (0.084)   (0.249)   (0.065)   (0.066)   (0.081)   (0.210) 
    ##                                                                                                             
    ## Change in vote share                     0.139     0.121     0.094    -0.083     0.069    -0.061     0.083  
    ##                                         (0.100)   (0.065)   (0.189)   (0.057)   (0.056)   (0.064)   (0.096) 
    ##                                                                                                             
    ## RRP vote share                          -0.172     0.370    -0.229    -0.155              -0.022     0.162  
    ##                                         (0.132)   (0.212)   (0.444)   (0.163)             (0.108)   (0.207) 
    ##                                                                                                             
    ## Party in Government                    -3.270**  -3.443***  -2.671*   -1.305*  -3.619*** -4.554*** -6.927***
    ##                                         (1.240)   (0.764)   (1.262)   (0.658)   (0.779)   (0.822)   (1.366) 
    ##                                                                                                             
    ## Pro EU Public Opinion                   -18.009    2.047   -35.080*   -2.385   -11.187**  -2.300    -1.077  
    ##                                        (10.319)   (4.217)  (15.839)   (5.615)   (3.404)   (5.666)   (6.798) 
    ##                                                                                                             
    ## Time to next election * RRP vote share   0.003    -0.003     0.012    -0.002              0.012*     0.005  
    ##                                         (0.004)   (0.006)   (0.013)   (0.003)             (0.005)   (0.011) 
    ##                                                                                                             
    ## Constant                               44.730*** 14.407*** 50.228*** 15.911*** 12.585**  16.427*** 18.354** 
    ##                                        (10.104)   (4.004)  (12.273)   (2.950)   (4.156)   (4.522)   (6.055) 
    ##                                                                                                             
    ## ------------------------------------------------------------------------------------------------------------
    ## Party                                     Yes       Yes       Yes       Yes       Yes       Yes       Yes   
    ## ------------------------------------------------------------------------------------------------------------
    ## Observations                              430       424       372       401       413       666       434   
    ## R2                                       0.081     0.327     0.055     0.223     0.149     0.133     0.328  
    ## Adjusted R2                              0.057     0.309     0.026     0.201     0.130     0.117     0.310  
    ## ============================================================================================================
    ## Note:                                                                          *p<0.05; **p<0.01; ***p<0.001

``` r
stargazer::stargazer(m_aus3, m_deu3, m_den3, m_swe3, m_esp3, m_nld3, m_uk3,
                     omit.labels = c("Party"),
                     type = "text", title = "Table 4: Interaction between RRP Vote Share and Change in Vote Share Per Country",
                     #out = "table.html",
                     dep.var.labels = "Share of Eurosceptic sentences",
                     column.labels = c("AUS", "DEU", "DNK", "SWE", "ESP", "NLD", "GBR"),
                      covariate.labels = c("Time", "Lagged Euroscepticism",
                                          "Time to next election (months)",
                                          "Time to next EP election (months)",
                                           "Party size (vote share)",
                                           "Change in vote share",
                                           "RRP vote share",
                                           "Party in Government",
                                          "Pro EU Public Opinion",
                                          "RRP vote share * Change in vote share"
                                            ),
                     omit = c("parlgov_id"),
                     omit.stat = c("f", "ser"),
                     star.cutoffs = c(0.05, 0.01, 0.001))
```

    ## 
    ## Table 4: Interaction between RRP Vote Share and Change in Vote Share Per Country
    ## ===========================================================================================================
    ##                                                                Dependent variable:                         
    ##                                       ---------------------------------------------------------------------
    ##                                                          Share of Eurosceptic sentences                    
    ##                                          AUS       DEU       DNK       SWE       ESP       NLD       GBR   
    ##                                          (1)       (2)       (3)       (4)       (5)       (6)       (7)   
    ## -----------------------------------------------------------------------------------------------------------
    ## Time                                    0.006     0.013     0.027    -0.008    0.013*     0.014     0.001  
    ##                                        (0.008)   (0.007)   (0.019)   (0.014)   (0.005)   (0.007)   (0.006) 
    ##                                                                                                            
    ## Lagged Euroscepticism                  -0.009   0.224***    0.070    -0.099*    0.032     0.060     0.043  
    ##                                        (0.049)   (0.047)   (0.053)   (0.050)   (0.050)   (0.039)   (0.049) 
    ##                                                                                                            
    ## Time to next election (months)         -0.023   -0.057**    0.025    0.036*    -0.004    -0.010     0.011  
    ##                                        (0.027)   (0.018)   (0.048)   (0.018)   (0.019)   (0.023)   (0.019) 
    ##                                                                                                            
    ## Time to next EP election (months)      -0.017     0.019    -0.054   -0.032**   -0.011    -0.016    -0.007  
    ##                                        (0.022)   (0.014)   (0.036)   (0.012)   (0.014)   (0.019)   (0.017) 
    ##                                                                                                            
    ## Party size (vote share)                -0.419*   -0.159    -0.443    -0.148     0.008     0.041     0.258  
    ##                                        (0.180)   (0.084)   (0.270)   (0.082)   (0.066)   (0.082)   (0.205) 
    ##                                                                                                            
    ## Change in vote share                    0.032     0.162    -0.793    -0.126     0.069    -0.044     0.061  
    ##                                        (0.221)   (0.103)   (0.744)   (0.069)   (0.056)   (0.133)   (0.105) 
    ##                                                                                                            
    ## RRP vote share                         -0.096     0.239     0.213    -0.054               0.107     0.224  
    ##                                        (0.108)   (0.162)   (0.288)   (0.191)             (0.091)   (0.143) 
    ##                                                                                                            
    ## Party in Government                    -3.095*  -3.513***  -1.672    -2.091*  -3.619*** -4.513*** -7.086***
    ##                                        (1.222)   (0.781)   (1.477)   (0.970)   (0.779)   (0.829)   (1.334) 
    ##                                                                                                            
    ## Pro EU Public Opinion                  -19.581    1.832   -41.407**  -2.821   -11.187**  -1.441    -3.664  
    ##                                       (11.260)   (4.194)  (15.820)   (5.564)   (3.404)   (5.703)   (5.211) 
    ##                                                                                                            
    ## RRP vote share * Change in vote share   0.005    -0.008     0.064     0.020              -0.001     0.013  
    ##                                        (0.011)   (0.018)   (0.052)   (0.018)             (0.010)   (0.034) 
    ##                                                                                                            
    ## Constant                              41.741*** 14.864*** 52.511*** 15.615*** 12.585**  13.722**  18.427** 
    ##                                        (9.444)   (3.880)  (12.558)   (2.908)   (4.156)   (4.385)   (6.126) 
    ##                                                                                                            
    ## -----------------------------------------------------------------------------------------------------------
    ## Party                                    Yes       Yes       Yes       Yes       Yes       Yes       Yes   
    ## -----------------------------------------------------------------------------------------------------------
    ## Observations                             430       424       372       401       413       666       434   
    ## R2                                      0.080     0.327     0.057     0.225     0.149     0.127     0.327  
    ## Adjusted R2                             0.056     0.309     0.028     0.203     0.130     0.111     0.310  
    ## ===========================================================================================================
    ## Note:                                                                         *p<0.05; **p<0.01; ***p<0.001

``` r
bind_rows(
  plot_predictions(m_aus1, condition = "rrp_voteshare", draw = FALSE) |>
    mutate(country = "Austria"),
  plot_predictions(m_deu1, condition = "rrp_voteshare", draw = FALSE) |>
    mutate(country = "Germany"),
  plot_predictions(m_swe1, condition = "rrp_voteshare", draw = FALSE) |>
    mutate(country = "Sweden"),
  plot_predictions(m_den1, condition = "rrp_voteshare", draw = FALSE) |>
    mutate(country = "Denmark"),
  plot_predictions(m_nld1, condition = "rrp_voteshare", draw = FALSE) |>
    mutate(country = "Netherlands"),
  plot_predictions(m_uk1, condition = "rrp_voteshare", draw = FALSE) |>
    mutate(country = "UK")
) |>
  ggplot(
    aes(
      x = rrp_voteshare,
      y = estimate,
      ymin = conf.low,
      ymax = conf.high,
      colour = country,
      fill = country
    )
  ) +
  geom_ribbon(alpha = 0.2, colour = NA) +
  geom_line()
```

![](regression_final_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## Using Polled RRP Vote Intention

### Pooled

``` r
mp1 <- lm(data = reg_data,
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + country + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party)

mp2 <- lm(data = reg_data,
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + country + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               parlgov_id)

mp3 <- lm(data = reg_data,
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + country + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion)

mp4 <- lm(data = reg_data,
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + country + 
               log(time_to_next + 1) + log(time_to_next_ep + 1) + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion)

mp5 <- lm(data = reg_data,
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + country +
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               time_to_next*lag_rrp_polled_voteshare + parlgov_id + eu_public_opinion)

mp6 <- lm(data = reg_data,
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + country +
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + family_name +
               family_name*lag_rrp_polled_voteshare + parlgov_id + eu_public_opinion)

mp7 <- lm(data = reg_data,
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + country +
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + family_2 +
               family_2*lag_rrp_polled_voteshare + parlgov_id + eu_public_opinion)

mp8 <- lm(data = reg_data,
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + country +
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + parlgov_id +
               change_voteshare*lag_rrp_polled_voteshare + parlgov_id + eu_public_opinion)

stargazer::stargazer(mp1, mp2, mp3, mp5, mp7, mp8,
                     omit.labels = c("Party", "Country"),
                     type = "text", title = "Table 5: Regression Using Polled RRP Vote",
                     dep.var.labels = "Share of Eurosceptic sentences",
                    covariate.labels = c("Time", "Lagged Euroscepticism",
                                          "Time to next election (months)",
                                          "Time to next EP election (months)",
                                           "Party size (vote share)",
                                           "Change in vote share",
                                           "Polled RRP Vote",
                                           "Party in Government",
                                         "Centre-Right Party",
                                         "Pro EU Public Opinion",
                                         "Time to next election * Polled RRP vote share",
                                          "Polled RRP vote share * Centre-Right",
                                          "Polled RRP vote share * Change in vote share"
                                           ),
                     omit = c("parlgov_id", "country"),
                     omit.stat = c("f", "ser"),
                    star.cutoffs = c(0.05, 0.01, 0.001)
                    )
```

    ## 
    ## Table 5: Regression Using Polled RRP Vote
    ## =========================================================================================================
    ##                                                                   Dependent variable:                    
    ##                                               -----------------------------------------------------------
    ##                                                             Share of Eurosceptic sentences               
    ##                                                  (1)       (2)       (3)       (4)       (5)       (6)   
    ## ---------------------------------------------------------------------------------------------------------
    ## Time                                           0.006*     0.005    0.008*    0.008*    0.008*    0.009*  
    ##                                                (0.003)   (0.003)   (0.003)   (0.003)   (0.003)   (0.004) 
    ##                                                                                                          
    ## Lagged Euroscepticism                         0.097***  0.088***  0.085***  0.085***  0.085***  0.085*** 
    ##                                                (0.020)   (0.020)   (0.020)   (0.020)   (0.020)   (0.020) 
    ##                                                                                                          
    ## Time to next election (months)                 -0.005    -0.006    -0.005    -0.013    -0.005    -0.005  
    ##                                                (0.010)   (0.010)   (0.010)   (0.016)   (0.010)   (0.010) 
    ##                                                                                                          
    ## Time to next EP election (months)              -0.008    -0.008    -0.013    -0.013    -0.013    -0.013  
    ##                                                (0.008)   (0.008)   (0.008)   (0.008)   (0.008)   (0.008) 
    ##                                                                                                          
    ## Party size (vote share)                       -0.104**  -0.148*** -0.146*** -0.145*** -0.147*** -0.145***
    ##                                                (0.032)   (0.040)   (0.040)   (0.040)   (0.041)   (0.040) 
    ##                                                                                                          
    ## Change in vote share                            0.036     0.047     0.059     0.060     0.059     0.067  
    ##                                                (0.028)   (0.031)   (0.031)   (0.031)   (0.031)   (0.040) 
    ##                                                                                                          
    ## Polled RRP Vote                                 0.038     0.035     0.059     0.044     0.055     0.057  
    ##                                                (0.035)   (0.035)   (0.036)   (0.043)   (0.046)   (0.036) 
    ##                                                                                                          
    ## Party in Government                           -2.877*** -2.685*** -2.712*** -2.725*** -2.704*** -2.739***
    ##                                                (0.340)   (0.359)   (0.360)   (0.360)   (0.366)   (0.369) 
    ##                                                                                                          
    ## Centre-Right Party                                                                      0.289            
    ##                                                                                        (1.068)           
    ##                                                                                                          
    ## Pro EU Public Opinion                                             -7.022**  -7.075**  -7.011**  -7.154** 
    ##                                                                    (2.203)   (2.205)   (2.205)   (2.240) 
    ##                                                                                                          
    ## Time to next election * Polled RRP vote share                                 0.001                      
    ##                                                                              (0.001)                     
    ##                                                                                                          
    ## Polled RRP vote share * Centre-Right                                                    0.006            
    ##                                                                                        (0.051)           
    ##                                                                                                          
    ## Polled RRP vote share * Change in vote share                                                     -0.001  
    ##                                                                                                  (0.003) 
    ##                                                                                                          
    ## Constant                                      15.387*** 16.188*** 19.017*** 19.161*** 18.706*** 19.046***
    ##                                                (1.358)   (1.580)   (1.807)   (1.821)   (2.087)   (1.809) 
    ##                                                                                                          
    ## ---------------------------------------------------------------------------------------------------------
    ## Party                                            No        No        No        No        No        No    
    ## Country                                          Yes       Yes       Yes       Yes       Yes       Yes   
    ## ---------------------------------------------------------------------------------------------------------
    ## Observations                                    2,572     2,572     2,563     2,563     2,563     2,563  
    ## R2                                              0.308     0.313     0.316     0.316     0.316     0.316  
    ## Adjusted R2                                     0.304     0.308     0.310     0.310     0.310     0.310  
    ## =========================================================================================================
    ## Note:                                                                       *p<0.05; **p<0.01; ***p<0.001

### Per country

``` r
mp_aus1 <- lm(data = reg_data |> filter(country == "AUS"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion)

mp_aus2 <- lm(data = reg_data |> filter(country == "AUS"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + time_to_next*lag_rrp_polled_voteshare)

mp_aus3 <- lm(data = reg_data |> filter(country == "AUS"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + change_voteshare*lag_rrp_polled_voteshare)

mp_deu1 <- lm(data = reg_data |> filter(country == "DEU"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion)

mp_deu2 <- lm(data = reg_data |> filter(country == "DEU"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + time_to_next*lag_rrp_polled_voteshare)

mp_deu3 <- lm(data = reg_data |> filter(country == "DEU"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + change_voteshare*lag_rrp_polled_voteshare)

mp_den1 <- lm(data = reg_data |> filter(country == "DNK"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion)

mp_den2 <- lm(data = reg_data |> filter(country == "DNK"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + time_to_next*lag_rrp_polled_voteshare)

mp_den3 <- lm(data = reg_data |> filter(country == "DNK"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + change_voteshare*lag_rrp_polled_voteshare)

mp_swe1 <- lm(data = reg_data |> filter(country == "SWE"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion)

mp_swe2 <- lm(data = reg_data |> filter(country == "SWE"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + time_to_next*lag_rrp_polled_voteshare)

mp_swe3 <- lm(data = reg_data |> filter(country == "SWE"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + change_voteshare*lag_rrp_polled_voteshare)

mp_esp1 <- lm(data = reg_data |> filter(country == "ESP"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion)

mp_esp2 <- lm(data = reg_data |> filter(country == "ESP"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + time_to_next*lag_rrp_polled_voteshare)

mp_esp3 <- lm(data = reg_data |> filter(country == "ESP"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + change_voteshare*lag_rrp_polled_voteshare)

mp_nld1 <- lm(data = reg_data |> filter(country == "NLD"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion)

mp_nld2 <- lm(data = reg_data |> filter(country == "NLD"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + time_to_next*lag_rrp_polled_voteshare)

mp_nld3 <- lm(data = reg_data |> filter(country == "NLD"),
             prop_euroscepticism ~ as.numeric(year_month) + 
               lag_prop_euroscepticism + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
               parlgov_id + eu_public_opinion + change_voteshare*lag_rrp_polled_voteshare)

stargazer::stargazer(mp_aus1, mp_deu1,mp_den1, mp_swe1, mp_esp1, mp_nld1,
                     omit.labels = c("Party", "Country"),
                     type = "text", title = "Table 6: Regression Using Polled RRP Vote Per Country",
                     #out = "table.html",
                     dep.var.labels = "Share of Eurosceptic sentences",
                     column.labels = c("AUS", "DEU", "DNK", "SWE", "ESP", "NLD"),
                     covariate.labels = c("Time", "Lagged Euroscepticism",
                                          "Time to next election (months)",
                                          "Time to next EP election (months)",
                                           "Party size (vote share)",
                                           "Change in vote share",
                                           "Polled RRP Vote",
                                           "Party in Government",
                                          "Pro EU Public Opinion"
                                           ),
                     omit = c("parlgov_id", "country"),
                     omit.stat = c("f", "ser"),
                     star.cutoffs = c(0.05, 0.01, 0.001))
```

    ## 
    ## Table 6: Regression Using Polled RRP Vote Per Country
    ## ============================================================================================
    ##                                                      Dependent variable:                    
    ##                                   ----------------------------------------------------------
    ##                                                 Share of Eurosceptic sentences              
    ##                                     AUS       DEU      DNK       SWE       ESP        NLD   
    ##                                     (1)       (2)      (3)       (4)       (5)        (6)   
    ## --------------------------------------------------------------------------------------------
    ## Time                               0.003     0.010    0.010    -0.011     0.011*    0.028*  
    ##                                   (0.010)   (0.007)  (0.023)   (0.012)   (0.005)    (0.011) 
    ##                                                                                             
    ## Lagged Euroscepticism              0.003   0.228***   0.064    -0.095     0.022      0.058  
    ##                                   (0.052)   (0.047)  (0.054)   (0.050)   (0.050)    (0.041) 
    ##                                                                                             
    ## Time to next election (months)     -0.029  -0.049**   0.041    0.032*     0.008     -0.032  
    ##                                   (0.028)   (0.018)  (0.049)   (0.015)   (0.019)    (0.025) 
    ##                                                                                             
    ## Time to next EP election (months)  -0.031    0.019    -0.053  -0.036**    -0.003    -0.024  
    ##                                   (0.025)   (0.014)  (0.037)   (0.012)   (0.014)    (0.019) 
    ##                                                                                             
    ## Party size (vote share)            -0.120   -0.176*   -0.317  -0.183**    0.061      0.029  
    ##                                   (0.194)   (0.085)  (0.254)   (0.063)   (0.068)    (0.084) 
    ##                                                                                             
    ## Change in vote share               0.150     0.124    0.078    -0.069     0.034     -0.080  
    ##                                   (0.105)   (0.065)  (0.187)   (0.055)   (0.056)    (0.067) 
    ##                                                                                             
    ## Polled RRP Vote                    0.241*    0.119    0.615    -0.054    4.825***   -0.117  
    ##                                   (0.098)   (0.093)  (0.335)   (0.074)   (1.344)    (0.091) 
    ##                                                                                             
    ## Party in Government               -3.719** -3.361*** -2.612*   -1.498*  -3.734***  -4.391***
    ##                                   (1.377)   (0.773)  (1.297)   (0.629)   (0.785)    (0.877) 
    ##                                                                                             
    ## Pro EU Public Opinion             -20.481    4.705   -35.210*  -3.148   -11.758***  -1.874  
    ##                                   (11.376)  (4.059)  (14.967)  (5.614)   (3.391)    (6.092) 
    ##                                                                                             
    ## Constant                          26.778*  14.076*** 41.147** 16.818***  10.546*   15.403***
    ##                                   (10.359)  (4.012)  (12.602)  (2.851)   (4.256)    (4.456) 
    ##                                                                                             
    ## --------------------------------------------------------------------------------------------
    ## Party                               Yes       Yes      Yes       Yes       Yes        Yes   
    ## Country                              No       No        No       No         No        No    
    ## --------------------------------------------------------------------------------------------
    ## Observations                        368       420      360       399       405        611   
    ## R2                                 0.086     0.321    0.063     0.223     0.174      0.123  
    ## Adjusted R2                        0.061     0.304    0.036     0.203     0.153      0.107  
    ## ============================================================================================
    ## Note:                                                          *p<0.05; **p<0.01; ***p<0.001

``` r
stargazer::stargazer(mp_aus2, mp_deu2,mp_den2, mp_swe2, mp_esp2, mp_nld2,
                     omit.labels = c("Party", "Country"),
                     type = "text", title = "Table 7: Interaction between Polled RRP Vote and Time to Next Election Per Country",
                     #out = "table.html",
                     dep.var.labels = "Share of Eurosceptic sentences",
                     column.labels = c("AUS", "DEU", "DNK", "SWE", "ESP", "NLD"),
                      covariate.labels = c("Time", "Lagged Euroscepticism",
                                          "Time to next election (months)",
                                          "Time to next EP election (months)",
                                           "Party size (vote share)",
                                           "Change in vote share",
                                           "Polled RRP Vote",
                                           "Party in Government",
                                          "Pro EU Public Opinion",
                                          "Time to next election * Polled RRP Vote"
                                           ),
                     omit = c("parlgov_id", "country"),
                     omit.stat = c("f", "ser"),
                     star.cutoffs = c(0.05, 0.01, 0.001))
```

    ## 
    ## Table 7: Interaction between Polled RRP Vote and Time to Next Election Per Country
    ## ===================================================================================================
    ##                                                             Dependent variable:                    
    ##                                         -----------------------------------------------------------
    ##                                                       Share of Eurosceptic sentences               
    ##                                           AUS       DEU       DNK       SWE       ESP        NLD   
    ##                                           (1)       (2)       (3)       (4)       (5)        (6)   
    ## ---------------------------------------------------------------------------------------------------
    ## Time                                     0.002     0.010     0.010    -0.012     0.009      0.018  
    ##                                         (0.010)   (0.007)   (0.023)   (0.012)   (0.005)    (0.012) 
    ##                                                                                                    
    ## Lagged Euroscepticism                    -0.002  0.229***    0.062    -0.097     0.026      0.064  
    ##                                         (0.052)   (0.047)   (0.054)   (0.050)   (0.050)    (0.041) 
    ##                                                                                                    
    ## Time to next election (months)           -0.136   -0.044*   -0.151     0.041     0.010     -0.122* 
    ##                                         (0.103)   (0.021)   (0.212)   (0.022)   (0.019)    (0.055) 
    ##                                                                                                    
    ## Time to next EP election (months)        -0.044    0.018    -0.054    -0.033*    -0.002    -0.022  
    ##                                         (0.027)   (0.014)   (0.038)   (0.013)   (0.014)    (0.019) 
    ##                                                                                                    
    ## Party size (vote share)                  -0.132   -0.173*   -0.303   -0.185**    0.092      0.029  
    ##                                         (0.194)   (0.085)   (0.255)   (0.063)   (0.068)    (0.084) 
    ##                                                                                                    
    ## Change in vote share                     0.178     0.116     0.093    -0.071     0.020     -0.071  
    ##                                         (0.108)   (0.068)   (0.188)   (0.055)   (0.056)    (0.067) 
    ##                                                                                                    
    ## Polled RRP Vote                          0.158     0.160     0.322    -0.024     -3.614    -0.211* 
    ##                                         (0.124)   (0.135)   (0.460)   (0.091)   (3.195)    (0.104) 
    ##                                                                                                    
    ## Party in Government                     -3.907** -3.353***  -2.679*   -1.480*  -3.813***  -4.384***
    ##                                         (1.387)   (0.774)   (1.299)   (0.630)   (0.778)    (0.875) 
    ##                                                                                                    
    ## Pro EU Public Opinion                   -22.004    5.178   -34.043*   -2.646   -13.339***   0.548  
    ##                                         (11.459)  (4.218)  (15.022)   (5.686)   (3.403)    (6.222) 
    ##                                                                                                    
    ## Time to next election * Polled RRP Vote  0.005    -0.002     0.014    -0.001    1.564**     0.008  
    ##                                         (0.005)   (0.004)   (0.015)   (0.002)   (0.538)    (0.004) 
    ##                                                                                                    
    ## Constant                                30.359** 13.656**  44.127*** 16.341***  10.451*   16.111***
    ##                                         (10.867)  (4.140)  (13.007)   (2.971)   (4.217)    (4.464) 
    ##                                                                                                    
    ## ---------------------------------------------------------------------------------------------------
    ## Party                                     Yes       Yes       Yes       Yes       Yes        Yes   
    ## Country                                    No       No        No        No         No        No    
    ## ---------------------------------------------------------------------------------------------------
    ## Observations                              368       420       360       399       405        611   
    ## R2                                       0.089     0.321     0.066     0.224     0.191      0.128  
    ## Adjusted R2                              0.061     0.303     0.036     0.201     0.169      0.111  
    ## ===================================================================================================
    ## Note:                                                                 *p<0.05; **p<0.01; ***p<0.001

``` r
stargazer::stargazer(mp_aus3, mp_deu3, mp_den3, mp_swe3, mp_esp3, mp_nld3,
                     omit.labels = c("Party", "Country"),
                     type = "text", title = "Table 8: Interaction between Polled RRP Vote and Change in Vote Share Per Country",
                     #out = "table.html",
                     dep.var.labels = "Share of Eurosceptic sentences",
                     column.labels = c("AUS", "DEU", "DNK", "SWE", "ESP", "NLD"),
                      covariate.labels = c("Time", "Lagged Euroscepticism",
                                          "Time to next election (months)",
                                          "Time to next EP election (months)",
                                           "Party size (vote share)",
                                           "Change in vote share",
                                           "Polled RRP Vote",
                                           "Party in Government",
                                          "Pro EU Public Opinion",
                                          "Polled RRP Vote * Change in vote share"
                                            ),
                     omit = c("parlgov_id", "country"),
                     omit.stat = c("f", "ser"),
                     star.cutoffs = c(0.05, 0.01, 0.001)
                     )
```

    ## 
    ## Table 8: Interaction between Polled RRP Vote and Change in Vote Share Per Country
    ## ==================================================================================================
    ##                                                            Dependent variable:                    
    ##                                        -----------------------------------------------------------
    ##                                                      Share of Eurosceptic sentences               
    ##                                          AUS       DEU       DNK       SWE       ESP        NLD   
    ##                                          (1)       (2)       (3)       (4)       (5)        (6)   
    ## --------------------------------------------------------------------------------------------------
    ## Time                                    0.002     0.012     0.007    -0.012     0.010     0.028*  
    ##                                        (0.010)   (0.008)   (0.023)   (0.012)   (0.005)    (0.011) 
    ##                                                                                                   
    ## Lagged Euroscepticism                   0.001   0.228***    0.061    -0.098*    0.020      0.058  
    ##                                        (0.052)   (0.047)   (0.054)   (0.050)   (0.050)    (0.041) 
    ##                                                                                                   
    ## Time to next election (months)          -0.035  -0.051**    0.043    0.033*     0.008     -0.033  
    ##                                        (0.029)   (0.018)   (0.049)   (0.015)   (0.019)    (0.025) 
    ##                                                                                                   
    ## Time to next EP election (months)       -0.035    0.020    -0.054   -0.036**    -0.003    -0.024  
    ##                                        (0.025)   (0.014)   (0.037)   (0.012)   (0.014)    (0.019) 
    ##                                                                                                   
    ## Party size (vote share)                 -0.172   -0.170*   -0.451    -0.141     0.046      0.026  
    ##                                        (0.201)   (0.086)   (0.284)   (0.072)   (0.071)    (0.084) 
    ##                                                                                                   
    ## Change in vote share                    0.329     0.144    -0.826    -0.102     0.029     -0.137  
    ##                                        (0.213)   (0.075)   (0.879)   (0.061)   (0.057)    (0.125) 
    ##                                                                                                   
    ## Polled RRP Vote                         0.216*    0.105    0.668*    -0.005     3.474     -0.124  
    ##                                        (0.101)   (0.096)   (0.338)   (0.084)   (2.226)    (0.092) 
    ##                                                                                                   
    ## Party in Government                    -3.888** -3.462***  -1.649   -2.042**  -3.515***  -4.449***
    ##                                        (1.388)   (0.795)   (1.587)   (0.776)   (0.836)    (0.883) 
    ##                                                                                                   
    ## Pro EU Public Opinion                  -16.210    4.366   -35.118*   -3.131   -11.724***  -2.213  
    ##                                        (12.197)  (4.108)  (14.965)   (5.611)   (3.393)    (6.127) 
    ##                                                                                                   
    ## Polled RRP Vote * Change in vote share  -0.011   -0.006     0.072     0.011     0.528      0.004  
    ##                                        (0.012)   (0.010)   (0.069)   (0.009)   (0.694)    (0.007) 
    ##                                                                                                   
    ## Constant                               26.809*  14.093*** 44.203*** 15.997***  11.116*   15.806***
    ##                                        (10.360)  (4.016)  (12.931)   (2.931)   (4.324)    (4.519) 
    ##                                                                                                   
    ## --------------------------------------------------------------------------------------------------
    ## Party                                    Yes       Yes       Yes       Yes       Yes        Yes   
    ## Country                                   No       No        No        No         No        No    
    ## --------------------------------------------------------------------------------------------------
    ## Observations                             368       420       360       399       405        611   
    ## R2                                      0.089     0.321     0.066     0.226     0.175      0.124  
    ## Adjusted R2                             0.060     0.303     0.037     0.204     0.152      0.106  
    ## ==================================================================================================
    ## Note:                                                                *p<0.05; **p<0.01; ***p<0.001

``` r
bind_rows(
  plot_predictions(mp_aus1, condition = "lag_rrp_polled_voteshare", draw = FALSE) |>
    mutate(country = "Austria"),
  plot_predictions(mp_deu1, condition = "lag_rrp_polled_voteshare", draw = FALSE) |>
    mutate(country = "Germany"),
  plot_predictions(mp_swe1, condition = "lag_rrp_polled_voteshare", draw = FALSE) |>
    mutate(country = "Sweden"),
  plot_predictions(mp_den1, condition = "lag_rrp_polled_voteshare", draw = FALSE) |>
    mutate(country = "Denmark"),
  plot_predictions(mp_nld1, condition = "lag_rrp_polled_voteshare", draw = FALSE) |>
    mutate(country = "Netherlands"),
  plot_predictions(mp_esp1, condition = "lag_rrp_polled_voteshare", draw = FALSE) |>
    mutate(country = "Spain")
) |>
  ggplot(
    aes(
      x = lag_rrp_polled_voteshare,
      y = estimate,
      ymin = conf.low,
      ymax = conf.high,
      colour = country,
      fill = country
    )
  ) +
  geom_ribbon(alpha = 0.2, colour = NA) +
  geom_line()
```

![](regression_final_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
plot_slopes(mp_nld2, variables = "lag_rrp_polled_voteshare", 
            by = "family_name") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Effect of Polled RRP Vote Share on Euroscepticism by Party Family",
            xlab = "Polled RRP Vote Share (%)",
            ylab = "Euroscepticism (Share of Eurosceptic Sentences)")
```

![](regression_final_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

# Election window regression

``` r
# reg2_data <- reg_data |>
#   filter((time_from_last <= 12 | time_to_next <= 12)) |>
#   group_by(country, name_short, year_month) |> 
#   mutate(
#     prop_eu_mention = 100*mean(eu_mention),
#     prop_euroscepticism = 100*mean(eu_stance, na.rm = TRUE)
#   ) |> 
#   ungroup() |>
#   dplyr::select(country, parlgov_id, name_short, 
#                 name, name_english, year_month, 
#                 last_election_year_month, next_election_year_month,
#                 time_from_last, time_to_next, time_to_next_ep, 
#                 prop_eu_mention, prop_euroscepticism, vote_share, 
#                 lag_rrp_voteshare, change_voteshare, cabinet_party) |>
#   distinct() |>
#   # I want to add a variable that measures whether last_election_year_month is the first, second, third, etc., election in the data for each country
#   group_by(country) |>
#   arrange(last_election_year_month) |>
#   mutate(
#     election_number = as.numeric(factor(last_election_year_month, 
#                                         levels = unique(last_election_year_month))) |>
#       as.factor()
#   ) |>
#   ungroup() |>
#   group_by(country, name_short) |>
#   mutate(
#     lag__prop_eu_mention = lag(prop_eu_mention, order_by = year_month),
#     change_eu_mention = prop_eu_mention - lag__prop_eu_mention,
#     
#     lag_prop_euroscepticism = lag(prop_euroscepticism, order_by = year_month),
#     change_euroscepticism = prop_euroscepticism - lag_prop_euroscepticism
#   ) |>
#   ungroup() |>
#   left_join(read_csv("view_party.csv") |>
#               filter(country_name_short %in% c("AUT", "DEU", "DNK", "ESP", "GBR", "NLD", "SWE")) |>
#               mutate(
#                 country = case_when(
#                   country_name_short == "AUT" ~ "AUS",
#                   is.na(country_name_short) ~ NA,
#                   .default = country_name_short)
#                 ) |>
#               rename(parlgov_id = party_id) |>
#               dplyr::select(c(country, family_name, parlgov_id))
#             ) |>
#   mutate(
#     family_name = case_when(
#       family_name %in% c("Conservative", "Christian democracy") ~ "Con/CD",
#       family_name == "Liberal" ~ "Lib",
#       family_name == "Social democracy" ~ "SocDem",
#       .default = NA
#     ) |> as.factor(),
#     family_2 = case_when(
#       family_name %in% c("Con/CD", "Lib") ~ "Centre-right",
#       family_name %in% c("SocDem") ~ "Centre-left",
#       .default = NA
#     ) |> as.factor()
#   ) |> 
#   left_join(polls, 
#             by = c("country", "year_month")) |>
#   mutate(parlgov_id = as.factor(parlgov_id))
# 
# m1 <- lm(data = reg2_data,
#              prop_euroscepticism ~ time_from_last + 
#                lag_prop_euroscepticism + country + election_number +
#                time_to_next + time_to_next_ep + vote_share + 
#                change_voteshare + lag_rrp_polled_voteshare + cabinet_party)
# 
# m2 <- lm(data = reg2_data,
#              prop_euroscepticism ~ time_from_last + 
#                lag_prop_euroscepticism + country + election_number +
#                time_to_next + time_to_next_ep + vote_share + 
#                change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
#                parlgov_id)
# 
# m3 <- lm(data = reg2_data,
#              prop_euroscepticism ~ time_from_last + 
#                lag_prop_euroscepticism + country + election_number +
#                time_to_next + time_to_next_ep + vote_share + 
#                change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
#                time_to_next*lag_rrp_polled_voteshare + parlgov_id)
# 
# m4 <- lm(data = reg2_data,
#              prop_euroscepticism ~ time_from_last + 
#                lag_prop_euroscepticism + country + election_number +
#                time_to_next + time_to_next_ep + vote_share + 
#                change_voteshare + lag_rrp_polled_voteshare + cabinet_party + family_name +
#                family_name*lag_rrp_polled_voteshare)
# 
# m5 <- lm(data = reg2_data,
#              prop_euroscepticism ~ time_from_last + 
#                lag_prop_euroscepticism + country + election_number +
#                time_to_next + time_to_next_ep + vote_share + 
#                change_voteshare + lag_rrp_polled_voteshare + cabinet_party + family_2 +
#                family_2*lag_rrp_polled_voteshare)
# 
# m6 <- lm(data = reg2_data,
#              prop_euroscepticism ~ time_from_last + 
#                lag_prop_euroscepticism + country + election_number +
#                time_to_next + time_to_next_ep + vote_share + 
#                change_voteshare + lag_rrp_polled_voteshare + cabinet_party + parlgov_id +
#                change_voteshare*lag_rrp_polled_voteshare)
# 
# 
# m7 <- lm(data = reg2_data,
#              prop_euroscepticism ~ time_from_last + 
#                lag_prop_euroscepticism + country + election_number +
#                poly(time_to_next, 2) + time_to_next_ep + vote_share + 
#                change_voteshare + lag_rrp_polled_voteshare + cabinet_party +
#                parlgov_id)
# 
# # m5 <- lm(data = reg2_data,
# #              change_euroscepticism ~ time_from_last + 
# #                country + election_number + time_to_next + time_to_next_ep + vote_share + 
# #                change_voteshare + lag_rrp_voteshare + cabinet_party)
# # 
# # m6 <- lm(data = reg2_data,
# #              change_euroscepticism ~ time_from_last + 
# #                country + election_number + time_to_next + time_to_next_ep + vote_share + 
# #                change_voteshare + lag_rrp_voteshare + cabinet_party +
# #                parlgov_id)
# # 
# # m7 <- lm(data = reg2_data,
# #              change_euroscepticism ~ time_from_last + 
# #                country + election_number + time_to_next + time_to_next_ep + vote_share + 
# #                change_voteshare + lag_rrp_voteshare + cabinet_party +
# #                time_to_next*lag_rrp_voteshare)
# # 
# # m8 <- lm(data = reg2_data,
# #              change_euroscepticism ~ time_from_last + 
# #                country + election_number + time_to_next + time_to_next_ep + vote_share + 
# #                change_voteshare + lag_rrp_voteshare + cabinet_party + family_name +
# #                family_name*lag_rrp_voteshare)
# 
# stargazer::stargazer(m1, m2, m3, m4, m5, m6, m7,
#                      omit.labels = c("Party", "Country"),
#                      type = "text", title = "Preliminary regression results",
#                      #out = "table.html",
#                      #dep.var.labels = "Share of Eurosceptic sentences",
#                      # covariate.labels = c("Time to next election (days)", 
#                      #                      "Time to next EP election (days)",
#                      #                      "Vote share (%)", 
#                      #                      "Change in vote share (%)",
#                      #                      "RRP vote share (%)"),
#                      omit = c("parlgov_id", "country"),
#                      omit.stat = c("f", "ser"))
# 
# #plot_slopes(m4, variables = "rrp_voteshare", condition = c("family_name") ) 
# 
# 
# # newdata <- data.frame(
# #   lag_prop_euroscepticism = rep(mean(reg2_data$lag_prop_euroscepticism, na.rm = TRUE), 25),
# #   country = rep(factor("AUS"), 25),
# #   election_number = c(rep(1, 12), rep(2, 13)) |> as.factor(),
# #   time_from_last = c(seq(32, (32+11), by = 1), seq(0, 12, by = 1)),
# #   time_to_next = c(seq(12, 0, by = -1), seq(41, (41-11), by = -1)),
# #   time_to_next_ep = rep(28, 25),
# #   vote_share = rep(mean(reg2_data$vote_share, na.rm = TRUE), 25),
# #   change_voteshare = rep(mean(reg2_data$change_voteshare, na.rm = TRUE), 25),
# #   rrp_voteshare = rep(mean(reg2_data$rrp_voteshare, na.rm = TRUE), 25),
# #   cabinet_party = rep(0, 25),
# #   family_name = rep(factor("Con/CD"), 25)
# # )
# # 
# # plot_predictions(m1,
# #                  newdata = newdata,
# #                  by = "time_from_last",
# #                  draw = FALSE
# #                  ) |> View()
# 
# 
# ## Run time_to_next*rr_polled_voteshare per country
```
