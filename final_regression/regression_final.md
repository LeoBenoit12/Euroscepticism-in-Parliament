Regressions
================
Duarte Amaro
2026-07-23

``` r
data0 <- read_csv("data0.csv") |>
  mutate(partyfacts_id = as.factor(partyfacts_id))
```

    ## Rows: 34607675 Columns: 17
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (9): sentence_uid, speech_id, country, party, speaker, sentence, pred, ...
    ## dbl  (7): partyfacts_id, sent_pos, n_tokens, year, eu_mention, eu_support, e...
    ## date (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
tib <- data0 |>
    filter(partyfacts_id %in% c(1329, 1384, ## OVP and SPO
                             383, 211, ## SPD and CDU
                             379, 1204, ## Danish SDs and Venstre
                             441, 1338, ## PSOE and PP
                             1516, 1567, ## Labour and Conservatives
                             828, 1234, 1157, ## VVD, PvdA, and CDA
                             690, 487 ## SAP and Moderates
    ))|>
  count(partyfacts_id) |>
  full_join(data0 |>
    filter(partyfacts_id %in% c(1329, 1384, ## OVP and SPO
                             383, 211, ## SPD and CDU
                             379, 1204, ## Danish SDs and Venstre
                             441, 1338, ## PSOE and PP
                             1516, 1567, ## Labour and Conservatives
                             828, 1234, 1157, ## VVD, PvdA, and CDA
                             690, 487 ## SAP and Moderates
    ))|>
              count(partyfacts_id, eu_mention) |>
              rename(n_eu = n)) |>
  mutate(share = n_eu*100/n) |>
  full_join(data0 |>
              dplyr::select(party, country, partyfacts_id) |>
              distinct()) |>
  filter(eu_mention == 1)
```

    ## Joining with `by = join_by(partyfacts_id)`
    ## Joining with `by = join_by(partyfacts_id)`

``` r
tib |>
  dplyr::select(country, party, n_eu, share) |>
  arrange(country) |>
  print(n = Inf)
```

    ## # A tibble: 15 × 4
    ##    country party     n_eu share
    ##    <chr>   <chr>    <int> <dbl>
    ##  1 AUS     ÖVP      29185  6.20
    ##  2 AUS     SPÖ      23144  4.95
    ##  3 DEU     CDU/CSU  67375  5.57
    ##  4 DEU     SPD      50961  4.82
    ##  5 DNK     S        23151  2.89
    ##  6 DNK     V        26702  3.35
    ##  7 ESP     GPP      32142  6.85
    ##  8 ESP     GPSOE    26323  6.39
    ##  9 GBR     Lab     145172  2.80
    ## 10 GBR     Con     170853  3.60
    ## 11 NLD     VVD      43615  3.50
    ## 12 NLD     CDA      47462  3.94
    ## 13 NLD     PvdA     38054  3.43
    ## 14 SWE     S        58019  3.83
    ## 15 SWE     M        43566  4.16

``` r
data_trimester <- read_csv("data_trimester.csv") |>
  mutate(parlgov_id = as.factor(parlgov_id),
         partyfacts_id = as.factor(partyfacts_id))
```

    ## Rows: 4209 Columns: 38
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (7): country, name_short, name, name_english, tri, party_name_short, c...
    ## dbl  (24): partyfacts_id, parlgov_id, count, count_mention, count_support, c...
    ## date  (7): tri_date, last_election_date, next_election_date, next_election_d...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
data_yearly <- read_csv("data_yearly.csv")
```

    ## Rows: 1182 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): country
    ## dbl (10): partyfacts_id, year, count, count_mention, count_support, count_op...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
reg_data_tri <- read_csv("reg_data_tri.csv") |>
  mutate(parlgov_id = as.factor(parlgov_id),
         log_time_to_next = log(time_to_next + 1),
         log_time_to_next_ep = log(time_to_next_ep + 1)) |>
    filter(parlgov_id %in% c(1013, 973, ## OVP and SPO
                           558, 1727, ## SPD and CDU
                           1629, 1605, ## Danish SDs and Venstre
                           902, 645, ## PSOE and PP
                           1556, 773, ## Labour and Conservatives
                           1409, 742, 235, ## VVD, PvdA, and CDA
                           657, 904 ## SAP and Moderates
                           ))
```

    ## Rows: 4208 Columns: 61
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (7): country, name_short, name, name_english, tri, family_name, family_2
    ## dbl  (53): parlgov_id, count, eu_count, prop_eu_mention, count_support, prop...
    ## date  (1): tri_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# Descriptive Statistics

## Time series

``` r
data_trimester |> 
  filter(parlgov_id %in% reg_data_tri$parlgov_id)  |>
  filter(country == "AUS") |>
  ggplot(aes(x = tri_date, y = net_position)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey40") +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(alpha = 0.2, colour = "blue", fill = "blue",
              span = 0.5) +
  geom_vline(aes(xintercept = next_election_date), linetype = "dashed", color = "black") +
  facet_grid( ~name_short, drop = TRUE) +
  ylim(-1, 1) +
  labs(x = "Year",
        y = "Net Position",
       title = "Net Position in Austria as share of all sentences about the EU"
       ) +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](regression_final_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#ggsave("austria_descriptives.png", device = "png", width = 12, height = 6)

data_trimester |>
  filter(parlgov_id %in% reg_data_tri$parlgov_id)  |>
  filter(country == "DEU") |>
  ggplot(aes(x = tri_date, y = net_position)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey40") +
  #geom_path(group=1, alpha = 0.4) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(alpha = 0.2, colour = "blue", fill = "blue",
              span = 0.5) +
  geom_vline(aes(xintercept = next_election_date), linetype = "dashed", color = "black") +
  facet_grid( ~name_short, drop = TRUE) +
  ylim(-1, 1) +
  labs(x = "Year",
        y = "Net Position",
       title = "Net Position in Germany as share of all sentences about the EU"
       ) +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](regression_final_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
#ggsave("germany_descriptives.png", device = "png", width = 12, height = 6)

data_trimester |>
  filter(parlgov_id %in% reg_data_tri$parlgov_id)  |>
  filter(country == "DNK") |>
  ggplot(aes(x = tri_date, y = net_position)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey40") +
  #geom_path(group=1, alpha = 0.4) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(alpha = 0.2, colour = "blue", fill = "blue",
              span = 0.5) +
  geom_vline(aes(xintercept = next_election_date), linetype = "dashed", color = "black") +
  facet_grid( ~name_short, drop = TRUE) +
  ylim(-1, 1) +
  labs(x = "Year",
        y = "Net Position",
       title = "Net Position in Denmark as share of all sentences about the EU"
       ) +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 1 row containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](regression_final_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

``` r
#ggsave("denmark_descriptives.png", device = "png", width = 12, height = 6)

data_trimester |> 
  filter(parlgov_id %in% reg_data_tri$parlgov_id)  |>
  filter(country == "ESP") |>
  ggplot(aes(x = tri_date, y = net_position)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey40") +
  #geom_path(group=1, alpha = 0.4) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(alpha = 0.2, colour = "blue", fill = "blue",
              span = 0.5) +
  geom_vline(aes(xintercept = next_election_date), linetype = "dashed", color = "black") +
  facet_grid( ~name_short, drop = TRUE) +
  ylim(-1, 1) +
  labs(x = "Year",
        y = "Net Position",
       title = "Net Position in Spain as share of all sentences about the EU"
       ) +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 1 row containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](regression_final_files/figure-gfm/unnamed-chunk-3-4.png)<!-- -->

``` r
#ggsave("spain_descriptives.png", device = "png", width = 12, height = 6)

data_trimester |>
  filter(parlgov_id %in% reg_data_tri$parlgov_id)  |>
  filter(country == "NLD") |>
  ggplot(aes(x = tri_date, y = net_position)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey40") +
  #geom_path(group=1, alpha = 0.4) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(alpha = 0.2, colour = "blue", fill = "blue",
              span = 0.5) +
  geom_vline(aes(xintercept = next_election_date), linetype = "dashed", color = "black") +
  facet_grid( ~name_short, drop = TRUE) +
  ylim(-1, 1) +
  labs(x = "Year",
        y = "Net Position",
       title = "Net Position in the Netherlands as share of all sentences about the EU"
       ) +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](regression_final_files/figure-gfm/unnamed-chunk-3-5.png)<!-- -->

``` r
#ggsave("netherlands_descriptives.png", device = "png", width = 12, height = 6)

data_trimester |>
  filter(parlgov_id %in% reg_data_tri$parlgov_id)  |>
  filter(country == "SWE") |>
  ggplot(aes(x = tri_date, y = net_position)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey40") +
  #geom_path(group=1, alpha = 0.4) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(alpha = 0.2, colour = "blue", fill = "blue",
              span = 0.5) +
  geom_vline(aes(xintercept = next_election_date), linetype = "dashed", color = "black") +
  facet_grid( ~name_short, drop = TRUE) +
  ylim(-1, 1) +
  labs(x = "Year",
        y = "Net Position",
       title = "Net Position in Sweden as share of all sentences about the EU"
       ) +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 4 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 4 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](regression_final_files/figure-gfm/unnamed-chunk-3-6.png)<!-- -->

``` r
#ggsave("sweden_descriptives.png", device = "png", width = 12, height = 6)

data_trimester |>
  filter(parlgov_id %in% reg_data_tri$parlgov_id)  |>
  filter(country == "GBR") |>
  ggplot(aes(x = tri_date, y = net_position)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey40") +
  #geom_path(group=1, alpha = 0.4) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(alpha = 0.2, colour = "blue", fill = "blue",
              span = 0.5) +
  geom_vline(aes(xintercept = next_election_date), linetype = "dashed", color = "black") +
  facet_grid( ~name_short, drop = TRUE) +
  ylim(-1,1) +
  labs(x = "Year",
        y = "Net Position",
       title = "Net Position in the UK as share of all sentences about the EU"
       ) +
  theme(panel.spacing.x = unit(5, "mm"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](regression_final_files/figure-gfm/unnamed-chunk-3-7.png)<!-- -->

``` r
#ggsave("uk_descriptives.png", device = "png", width = 12, height = 6)
```

## Box plots

``` r
data_trimester |>
  filter(parlgov_id %in% reg_data_tri$parlgov_id)  |>
  group_by(country, name_short) |>
  mutate(mean_net_position = mean(net_position, na.rm = TRUE)) |>
  ungroup() |>
  arrange(mean_net_position) |>
  ggplot(aes(x = reorder(name_short, mean_net_position),
             y = net_position,
             colour = country,
             fill = country)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey40") +
  geom_boxplot(alpha = 0.2) +
  #facet_wrap(~ country, scales = "free")
  coord_flip() +
  scale_color_brewer(name = "", palette = "Dark2") +
  scale_fill_brewer(name = "", palette = "Dark2") +
  labs(x = "", y = "Net Position",
       title = "Quarterly net position by party")
```

    ## Warning: Removed 6 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](regression_final_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
#ggsave("quarterly_boxplot.png", device = "png", width = 12, height = 6)
```

# Validating the Measure

## Against CHES

``` r
## Getting ches and partyfacts codes:

partyfacts_to_ches <- read_csv("partyfacts-external-parties.csv") |> 
  filter((partyfacts_id %in% unique(data_yearly$partyfacts_id) | 
            partyfacts_id == 540 | # For some reason, PartyFacts codes LibDems differently for ParlGov
            partyfacts_id == 1375) & # CHES distinguishes between CDU and CSU
           dataset_key == "ches" &
           year_last >= 1998 &
           !is.na(name)) |>
  rename(ches_id = dataset_party_id,
         pf_country = country) |>
  mutate(ches_id = as.numeric(ches_id),
         partyfacts_id = case_when(
           partyfacts_id == 1375 ~ 211,
           .default = partyfacts_id
         ))
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
  dplyr::select(ches_country, ches_id, year, party, eu_position, eu_salience, eu_dissent, eu_blur, family)

joint_ches <- data_yearly |>
  left_join(partyfacts_to_ches, by = "partyfacts_id") |> 
  left_join(ches, by = c("ches_id", "year"))
```

``` r
parties <- c(1329, 1384, 211, 383, 379, 1204, 
             441, 1338, 1516, 1567, 828, 1157, 
             1234, 487, 690)

joint_ches |>
  filter(partyfacts_id %in% parties &
           !is.na(eu_position)) |>
  ggplot(aes(x = eu_position, y = net_position)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", colour = "blue", fill = "blue", alpha = 0.2) +
  ggpubr::stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~~")),
                                label.x = 1.7, label.y = 0.32) +
  labs(x = "CHES Position",
       y = "Net Position",
       title = "CHES Position against Net Position (Regression Parties Only)")
```

    ## Warning: The dot-dot notation (`..eq.label..`) was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `after_stat(eq.label)` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## `geom_smooth()` using formula = 'y ~ x'

![](regression_final_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
#ggsave("ches_cross_verification.png", device = "png", width = 9.5, height = 6)

joint_ches |>
  filter(partyfacts_id %in% parties) |>
  summarise(Correlation = cor(eu_position, net_position, use = "complete.obs"))
```

    ## # A tibble: 1 × 1
    ##   Correlation
    ##         <dbl>
    ## 1       0.781

``` r
joint_ches |>
  ggplot(aes(x = eu_position, y = net_position)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", colour = "blue", fill = "blue", alpha = 0.2) +
  ggpubr::stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~~")),
                                label.x = 1.1, label.y = 0.32) +
  labs(x = "CHES Position",
       y = "Net Position",
       title = "CHES Position against Net Position (All Parties)")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 953 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 953 rows containing non-finite outside the scale range
    ## (`stat_regline_equation()`).

    ## Warning: Removed 953 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](regression_final_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
#ggsave("ches_cross_verification_all.png", device = "png", width = 9.5, height = 6)

joint_ches |>
  summarise(Correlation = cor(eu_position, net_position, use = "complete.obs"))
```

    ## # A tibble: 1 × 1
    ##   Correlation
    ##         <dbl>
    ## 1       0.731

## UK MPs and Brexit Vote

``` r
d_uk_mp_yearly <- data0 |> filter(country == "GBR") |>
    group_by(speaker, year) |>
    summarise(
        count = n(),
        count_mention = sum(eu_mention == 1, na.rm = TRUE),

        count_support = sum(eu_support == 1, na.rm = TRUE),

        count_oppose = sum(eu_opposition == 1, na.rm = TRUE),

        prop_support = count_support/count_mention,

        prop_oppose = count_oppose/count_mention,

        net_position = prop_support - prop_oppose,
    ) |>
    ungroup()
```

    ## `summarise()` has grouped output by 'speaker'. You can override using the
    ## `.groups` argument.

``` r
mps2 <- read_csv("MP_EU_Referendum_Positions.csv") |>
  mutate(
    speaker = sub("^\\s*([^,]+),\\s*(.+?)\\s*$", "\\2 \\1", MP) 
  ) |>
  rename(vote = `Referendum vote`,
         party = Party) |>
  dplyr::select(speaker, vote, party) |>
  group_by(party, vote) |>
  mutate(n_mps = n()) |>
  ungroup()
```

    ## New names:
    ## Rows: 566 Columns: 8
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (6): MP, Party, Year first elected to Parliament, Constituency, Referend... dbl
    ## (1): Born lgl (1): ...8
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...8`

``` r
width_range <- c(0.1, 0.9) 
width_power <- 1           

box_widths <- data.frame(
  n_mps = unique(mps2$n_mps),
  box_width = rescale(unique(mps2$n_mps)^width_power, to = width_range)
)

d_uk_mp_yearly |> 
  filter(year > 2014 & year < 2018) |> 
  left_join(mps2, by = "speaker") |> 
  left_join(box_widths) |> 
  filter(!is.na(vote) & !is.na(party)) |>
  ggplot(aes(x = as.factor(year), 
             y = net_position, 
             colour = vote, 
             fill = vote)) + 
  geom_hline(aes(yintercept = 0), 
             linetype = "dashed") + 
  geom_boxplot(alpha = 0.2,
               aes(width = box_width)) + 
  scale_color_brewer(palette = "Set1", name = "Referendum Vote") + 
  scale_fill_brewer(palette = "Set1", name = "Referendum Vote") + 
  labs(x = "", y = "Net Position", title = "Net Position on European Integration of UK MPs (2015-2017 Parliament)", subtitle = "Box width is proportional to the number of MPs. Data on MPs' referendum vote taken from Cygan, Lynch, and Whitaker (2019).") + 
  facet_wrap(~ party)
```

    ## Joining with `by = join_by(n_mps)`

![](regression_final_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
#ggsave("uk_mp_net_position_boxplot.png", device = "png", width = 12, height = 6)
```

## Against MARPOR

``` r
marpor <- read_csv("MPDataset_MPDS2025a.csv") |>
  filter(countryname %in% c("Austria", "Germany", "Denmark", "Spain", 
                            "Sweden", "United Kingdom", "Netherlands")) |>
  mutate(date = as.Date(edate, format = "%d/%m/%Y"),
         country = case_when(
           countryname == "Austria" ~ "AUS",
           countryname == "Germany" ~ "DEU",
           countryname == "Denmark" ~ "DNK",
           countryname == "Spain" ~ "ESP",
           countryname == "Sweden" ~ "SWE",
           countryname == "United Kingdom" ~ "GBR",
           countryname == "Netherlands" ~ "NLD"
         )) |>
  filter(date >= as.Date("1999-01-01") & date <= as.Date("2019-12-01")) |>
  mutate(
    lowe = log(per108 + 0.5) - log(per110 + 0.5),
    year = format(date, "%Y") |>
      as.numeric()
  ) |>
  dplyr::select(country, date, year, party, partyname, partyabbrev, parfam, per108, per110, lowe) |>
  rename(marpor_id = party)
```

    ## Rows: 5285 Columns: 175
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (8): countryname, edate, partyname, partyabbrev, candidatename, corpus...
    ## dbl (167): country, oecdmember, eumember, date, party, parfam, coderid, manu...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
partyfacts_to_marpor <- read_csv("partyfacts-external-parties.csv") |> 
  filter((partyfacts_id %in% unique(data_yearly$partyfacts_id) | 
            partyfacts_id == 540 | # For some reason, PartyFacts codes LibDems differently for ParlGov
            partyfacts_id == 1375) & # CHES distinguishes between CDU and CSU
           dataset_key == "manifesto" &
           year_last >= 1998 &
           !is.na(name)) |>
  rename(marpor_id = dataset_party_id,
         pf_country = country) |>
  mutate(marpor_id = as.numeric(marpor_id),
         partyfacts_id = case_when(
           partyfacts_id == 1375 ~ 211,
           .default = partyfacts_id
         ))
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
joint_marpor <- data_yearly |>
  left_join(partyfacts_to_marpor, by = "partyfacts_id") |> 
  left_join(marpor, by = c("marpor_id", "year", "country")) |>
  filter(!is.na(per108))
```

    ## Warning in left_join(data_yearly, partyfacts_to_marpor, by = "partyfacts_id"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 135 of `x` matches multiple rows in `y`.
    ## ℹ Row 4 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
joint_marpor |>
  ggplot(aes(x = lowe, y = net_position)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", colour = "blue", fill = "blue", alpha = 0.2) +
  #ggrepel::geom_text_repel(aes(label = ifelse(abs(net_position-(-31 + 6.4*eu_position))>16.68,
            #                                   paste(party, year, sep = ", "),
              #                                 NA)),
             #               size = 3, max.overlaps = 15) +
  ggpubr::stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~~")),
                                label.x = -3.4, label.y = 0.35) +
  #facet_wrap(~ model, scales = "free") +
  labs(x = "MARPOR Logit Scale of Position",
       y = "Net Position",
       title = "MARPOR Logit Scale of Position against Net Position (All Parties)")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](regression_final_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
#ggsave("marpor_cross_verification_all.png", device = "png", width = 9.5, height = 6)

joint_marpor |>
  summarise(Correlation = cor(lowe, net_position, use = "complete.obs"))
```

    ## # A tibble: 1 × 1
    ##   Correlation
    ##         <dbl>
    ## 1       0.692

# Regressions

## RRP Vote Share

``` r
reg_data_tri <- read_csv("reg_data_tri.csv") |>
  mutate(parlgov_id = as.factor(parlgov_id),
         log_time_to_next = log(time_to_next + 1),
         log_time_to_next_ep = log(time_to_next_ep + 1)) |>
  filter(!is.na(family_name)) |>
  filter(parlgov_id %in% c(1013, 973, ## OVP and SPO
                           558, 1727, ## SPD and CDU
                           1629, 1605, ## Danish SDs and Venstre
                           902, 645, ## PSOE and PP
                           1556, 773, ## Labour and Conservatives
                           1409, 742, 235, ## VVD, PvdA, and CDA
                           657, 904 ## SAP and Moderates
                           ))
```

    ## Rows: 4208 Columns: 61
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (7): country, name_short, name, name_english, tri, family_name, family_2
    ## dbl  (53): parlgov_id, count, eu_count, prop_eu_mention, count_support, prop...
    ## date  (1): tri_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
tri_position_1 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + country + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament)

tri_position_2 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_3 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)


tri_position_4 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               log_time_to_next + log_time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion
         )

tri_position_5 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               log_time_to_next + log_time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion + log_time_to_next*rrp_lag
         )

tri_position_6 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position +
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               time_to_next*rrp_lag + parlgov_id + lag_eu_public_opinion
         )

tri_position_7 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position +
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament + parlgov_id +
               rrp_in_parliament*rrp_lag + parlgov_id + lag_eu_public_opinion
         )

modelsummary(list(tri_position_1, tri_position_2, tri_position_3,
                  tri_position_4, tri_position_5, tri_position_6, 
                  tri_position_7
                  ), 
             vcov = "robust", 
             stars = TRUE,
             coef_omit = "parlgov_id|country",
             coef_map = c(
               "(Intercept)" = "Constant",
               "date_numeric" = "Time",
               "lag_net_position" = "Lag Net Position",
               "time_to_next" = "Time to next election",
               "time_to_next_ep" = "Time to next EP election",
               "log_time_to_next" = "Log time to next election",
               "log_time_to_next_ep" = "Log time to next EP election",
               "vote_share" = "Vote share",
               "change_voteshare" = "Change in vote share",
               "rrp_lag" = "RRP Vote Share",
               "cabinet_party" = "Cabinet party",
               "rrp_in_parliament" = "RRP in Parliament",
               "lag_eu_public_opinion" = "Lag EU Public Opinion",
               "log_time_to_next:rrp_lag" = "Log time to next election * RRP vote share",
               "time_to_next:rrp_lag" = "Time to next election * RRP vote share",
               "rrp_lag:rrp_in_parliament" = "RRP vote share * RRP in Parliament"
             ),
             gof_map = tibble::tribble(~raw, ~clean, ~fmt,
                                       "nobs", "N", 0,
                                       "r.squared", "R^2", 2,
                                       "adj.r.squared", "Adj. R^2", 2),
             add_rows = tibble::tribble(
               ~term, ~tri_position_1, ~tri_position_2, ~tri_position_3, 
               ~tri_position_4, ~tri_position_5, ~tri_position_6, ~tri_position_7,
               "Party FE", "\\xmark", "\\cmark", "\\cmark", "\\cmark", "\\cmark", "\\cmark","\\cmark", 
               "Country FE", "\\cmark", "\\xmark", "\\xmark", "\\xmark", "\\xmark", "\\xmark", "\\xmark",
             ),
             output = "latex"
             )
```

    ## Warning: To compile a LaTeX document with this table, the following commands must be placed in the document preamble:
    ## 
    ## \usepackage{tabularray}
    ## \usepackage{float}
    ## \usepackage{graphicx}
    ## \usepackage{codehigh}
    ## \usepackage[normalem]{ulem}
    ## \UseTblrLibrary{booktabs}
    ## \UseTblrLibrary{siunitx}
    ## \newcommand{\tinytableTabularrayUnderline}[1]{\underline{#1}}
    ## \newcommand{\tinytableTabularrayStrikeout}[1]{\sout{#1}}
    ## \NewTableCommand{\tinytableDefineColor}[3]{\definecolor{#1}{#2}{#3}}
    ## 
    ## To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries in `\num{}`, call:
    ## 
    ## options("modelsummary_format_numeric_latex" = "plain")
    ##  This warning appears once per session.

<table style="width:97%;">
<colgroup>
<col style="width: 19%" />
<col style="width: 11%" />
<col style="width: 11%" />
<col style="width: 11%" />
<col style="width: 11%" />
<col style="width: 11%" />
<col style="width: 11%" />
<col style="width: 11%" />
</colgroup>
<thead>
<tr>
<th></th>
<th><ol type="1">
<li></li>
</ol></th>
<th><ol start="2" type="1">
<li></li>
</ol></th>
<th><ol start="3" type="1">
<li></li>
</ol></th>
<th><ol start="4" type="1">
<li></li>
</ol></th>
<th><ol start="5" type="1">
<li></li>
</ol></th>
<th><ol start="6" type="1">
<li></li>
</ol></th>
<th><ol start="7" type="1">
<li></li>
</ol></th>
</tr>
</thead>
<tbody>
<tr>
<td>Constant</td>
<td>***</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time</td>
<td>**</td>
<td>*</td>
<td>*</td>
<td>*</td>
<td>*</td>
<td>*</td>
<td>*</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag Net Position</td>
<td>**</td>
<td>+</td>
<td>+</td>
<td>+</td>
<td>+</td>
<td>+</td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time to next election</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time to next EP election</td>
<td></td>
<td>+</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Log time to next election</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
<td></td>
<td></td>
</tr>
<tr>
<td>Log time to next EP election</td>
<td></td>
<td></td>
<td></td>
<td>+</td>
<td>+</td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
<td></td>
<td></td>
</tr>
<tr>
<td>Vote share</td>
<td></td>
<td>+</td>
<td>+</td>
<td>+</td>
<td>+</td>
<td>+</td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Change in vote share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP Vote Share</td>
<td>+</td>
<td>*</td>
<td>*</td>
<td>*</td>
<td></td>
<td>+</td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Cabinet party</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP in Parliament</td>
<td>**</td>
<td>**</td>
<td>**</td>
<td>**</td>
<td>**</td>
<td>**</td>
<td>**</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag EU Public Opinion</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Log time to next election * RRP vote share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td></td>
<td></td>
</tr>
<tr>
<td>Time to next election * RRP vote share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td></td>
</tr>
<tr>
<td>RRP vote share * RRP in Parliament</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td>()</td>
</tr>
<tr>
<td>N</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>R2</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Adj. R2</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Party FE</td>
<td>xmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
</tr>
<tr>
<td>Country FE</td>
<td>cmark</td>
<td>xmark</td>
<td>xmark</td>
<td>xmark</td>
<td>xmark</td>
<td>xmark</td>
<td>xmark</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="8"><ul>
<li>p , * p , ** p , *** p |</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>

## RRP Polled Vote Share

``` r
tri_position_polled_1 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + country + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament)

tri_position_polled_2 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_polled_3 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

tri_position_polled_4 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               log_time_to_next + log_time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion
         )

tri_position_polled_5 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               log_time_to_next + log_time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion + log_time_to_next*lag_rrp_polled_voteshare
         )

tri_position_polled_6 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position +
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               time_to_next*lag_rrp_polled_voteshare + parlgov_id + lag_eu_public_opinion 
         )

tri_position_polled_7 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position +
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               rrp_in_parliament*lag_rrp_polled_voteshare + parlgov_id + lag_eu_public_opinion
         )

modelsummary(list(tri_position_polled_1, tri_position_polled_2, tri_position_polled_3,
                  tri_position_polled_4, tri_position_polled_5,
                  tri_position_polled_6, tri_position_polled_7
                  ), 
             vcov = "robust", 
             stars = TRUE,
             coef_omit = "parlgov_id|country",
             coef_map = c(
               "(Intercept)" = "Constant",
               "date_numeric" = "Time",
               "lag_net_position" = "Lag Net Position",
               "time_to_next" = "Time to next election",
               "time_to_next_ep" = "Time to next EP election",
               "log_time_to_next" = "Log time to next election",
               "log_time_to_next_ep" = "Log time to next EP election",
               "vote_share" = "Vote share",
               "change_voteshare" = "Change in vote share",
               "lag_rrp_polled_voteshare" = "RRP Polled Vote Share",
               "cabinet_party" = "Cabinet party",
               "rrp_in_parliament" = "RRP in Parliament",
               "lag_eu_public_opinion" = "Lag EU Public Opinion",
               "log_time_to_next:lag_rrp_polled_voteshare" = "Log time to next election * RRP Polled vote share",
               "time_to_next:lag_rrp_polled_voteshare" = "Time to next election * RRP Polled vote share",
               "lag_rrp_polled_voteshare:rrp_in_parliament" = "RRP Polled vote share * RRP in Parliament"
             ),
             gof_map = tibble::tribble(~raw, ~clean, ~fmt,
                                       "nobs", "N", 0,
                                       "r.squared", "R^2", 2,
                                       "adj.r.squared", "Adj. R^2", 2),
             add_rows = tibble::tribble(
               ~term, ~tri_position_polled_1, ~tri_position_polled_2, ~tri_position_polled_3,
               ~tri_position_polled_4, ~tri_position_polled_5,
               ~tri_position_polled_6, ~tri_position_polled_17,
               "Party FE", "\\xmark", "\\cmark", "\\cmark", "\\cmark", "\\cmark", "\\cmark","\\cmark", 
               "Country FE", "\\cmark", "\\xmark", "\\xmark", "\\xmark", "\\xmark", "\\xmark", "\\xmark",
             ),
             output = "latex"
             )
```

<table style="width:97%;">
<colgroup>
<col style="width: 22%" />
<col style="width: 10%" />
<col style="width: 10%" />
<col style="width: 10%" />
<col style="width: 10%" />
<col style="width: 10%" />
<col style="width: 10%" />
<col style="width: 10%" />
</colgroup>
<thead>
<tr>
<th></th>
<th><ol type="1">
<li></li>
</ol></th>
<th><ol start="2" type="1">
<li></li>
</ol></th>
<th><ol start="3" type="1">
<li></li>
</ol></th>
<th><ol start="4" type="1">
<li></li>
</ol></th>
<th><ol start="5" type="1">
<li></li>
</ol></th>
<th><ol start="6" type="1">
<li></li>
</ol></th>
<th><ol start="7" type="1">
<li></li>
</ol></th>
</tr>
</thead>
<tbody>
<tr>
<td>Constant</td>
<td>***</td>
<td></td>
<td>+</td>
<td></td>
<td></td>
<td>+</td>
<td>+</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time</td>
<td>+</td>
<td>+</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag Net Position</td>
<td>**</td>
<td>+</td>
<td>+</td>
<td>+</td>
<td>+</td>
<td>+</td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time to next election</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time to next EP election</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Log time to next election</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
<td></td>
<td></td>
</tr>
<tr>
<td>Log time to next EP election</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
<td></td>
<td></td>
</tr>
<tr>
<td>Vote share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Change in vote share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP Polled Vote Share</td>
<td>*</td>
<td>*</td>
<td>**</td>
<td>**</td>
<td></td>
<td>+</td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Cabinet party</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP in Parliament</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>**</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag EU Public Opinion</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Log time to next election * RRP Polled vote share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td></td>
<td></td>
</tr>
<tr>
<td>Time to next election * RRP Polled vote share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td></td>
</tr>
<tr>
<td>RRP Polled vote share * RRP in Parliament</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td>()</td>
</tr>
<tr>
<td>N</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>R2</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Adj. R2</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Party FE</td>
<td>xmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
</tr>
<tr>
<td>Country FE</td>
<td>cmark</td>
<td>xmark</td>
<td>xmark</td>
<td>xmark</td>
<td>xmark</td>
<td>xmark</td>
<td>xmark</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="8"><ul>
<li>p , * p , ** p , *** p |</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>

``` r
plot_log_time <- plot_slopes(tri_position_polled_5, 
            variables = "lag_rrp_polled_voteshare", vcov = "robust",
            condition = "log_time_to_next", draw = F) |>
  bind_rows(plot_slopes(tri_position_5, 
            variables = "rrp_lag", vcov = "robust",
            condition = "log_time_to_next", draw = F)) |>
  mutate(time_to_next = expm1(log_time_to_next)) |> 
  ggplot(aes(x = time_to_next, 
             y = estimate, ymin = conf.low, ymax = conf.high,
             colour = term,
             fill = term)) +
  geom_ribbon(alpha = 0.2, colour = NA) +
  geom_line() + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_color_brewer(name = "", palette = "Set1", labels = c("Polled RRP Vote Share", "Previous RRP Vote Share")) +
  scale_fill_brewer(name = "", palette = "Set1", labels = c("Polled RRP Vote Share", "Previous RRP Vote Share")) +
  labs(x = "Time to Next Election",
       y = "Marginal Effect",
       title = "Marginal Effect of RRP Support on Mainstream Parties' Net EU Positions by Time to Next Election",
       subtitle = "Models originally estimated with log time to next election")

plot_time <- plot_slopes(tri_position_6, variables = "rrp_lag", vcov = "robust",
            condition = "time_to_next", draw = F) |>
  bind_rows(plot_slopes(tri_position_polled_6, variables = "lag_rrp_polled_voteshare", vcov = "robust",
            condition = "time_to_next", draw = F)) |>
  ggplot(aes(x = time_to_next, 
             y = estimate, ymin = conf.low, ymax = conf.high,
             colour = term,
             fill = term)) +
  geom_ribbon(alpha = 0.2, colour = NA) +
  geom_line() + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_color_brewer(name = "", palette = "Set1", labels = c("Polled RRP Vote Share", "Previous RRP Vote Share")) +
  scale_fill_brewer(name = "", palette = "Set1", labels = c("Polled RRP Vote Share", "Previous RRP Vote Share")) +
  labs(x = "Time to Next Election",
       y = "Marginal Effect",
       title = "Marginal Effect of RRP Support on Mainstream Parties' Net EU Positions by Time to Next Election",
       subtitle = "Model originally estimated with linear time to next elections")

cowplot::plot_grid(plot_time, plot_log_time,
                   labels = c("A", "B"),
                   nrow = 2)
```

![](regression_final_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
#ggsave("marginal_effects_time.png", device = "png", width = 9.5, height = 12)
```

``` r
model.frame(tri_position_6) |>
              dplyr::select(time_to_next) |>
              mutate(model = "Actual") |>
  bind_rows(model.frame(tri_position_polled_6) |>
              dplyr::select(time_to_next) |>
              mutate(model = "Polled")) |>
  ggplot(aes(x = time_to_next, fill = model)) +
  geom_histogram(binwidth = 5, colour = NA, alpha = 0.2) +
  scale_fill_brewer(name = "", palette = "Set1", labels = c("Polled" = "Polled RRP Vote Share", "Actual" = "Previous RRP Vote Share")) +
  facet_wrap(~ model) +
  labs(x = "Time to Next Election", y = "Count",
       title = "Distribution of Time to Next Election in the Sample")
```

![](regression_final_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
#ggsave("hist_time.png", device = "png", width = 9.5, height = 6)
```

``` r
plot_slopes(tri_position_7, variables = "rrp_lag", vcov = "robust",
            condition = "rrp_in_parliament", conf_level = 0.95, draw = F) |> 
  mutate(level = 0.95) |>
  bind_rows(
    plot_slopes(tri_position_polled_7, variables = "lag_rrp_polled_voteshare", vcov = "robust",
                condition = "rrp_in_parliament", conf_level = 0.95, draw = F) |>
      mutate(level = 0.95)) |>
  bind_rows(
    plot_slopes(tri_position_7, variables = "rrp_lag", vcov = "robust",
                condition = "rrp_in_parliament", conf_level = 0.90, draw = F) |>
      mutate(level = 0.90)) |>
  bind_rows(
    plot_slopes(tri_position_polled_7, variables = "lag_rrp_polled_voteshare", vcov = "robust",
                condition = "rrp_in_parliament", conf_level = 0.90, draw = F) |>
      mutate(level = 0.90)) |> 
  mutate(level = as.factor(level)) |>
  ggplot() +
  geom_pointrange(aes(x = rrp_in_parliament, 
             y = estimate, ymin = conf.low, ymax = conf.high, 
             color = term, shape = term, group = term, linewidth = level),
             position = position_dodge(width = 0.25)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_linewidth_manual(values = c(1.25, 0.5), guide = "none") +
  scale_color_brewer(name = "", palette = "Set1", labels = c("Polled RRP Vote Share", "Previous RRP Vote Share")) +
  scale_shape_discrete(name = "", labels = c("Polled RRP Vote Share", "Previous RRP Vote Share")) +
  scale_x_discrete(breaks = c(0, 1),
                     labels = c("No", "Yes")) +
  labs(x = "RRP in Parliament", y = "Marginal Effect",
       title = "Marginal Effect of RRP Support on Mainstream Parties' Net EU Positions by RRP in Parliament",
       caption = "Point estimates with 90% and 95% confidence intervals")
```

![](regression_final_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
#ggsave("marginal_effects_rrp_in_parl.png", device = "png", width = 9.5, height = 6)
```

``` r
tibble("Polled/Actual" = c("Actual", "Actual", "Polled", "Polled"),
       "RRP in Parliament" = c(0, 1, 0, 1),
       "Number of Obs" = c(as.numeric(nrow(model.frame(tri_position_7) |> 
                                             filter(rrp_in_parliament == 0))),
                           as.numeric(nrow(model.frame(tri_position_7) |> 
                                             filter(rrp_in_parliament == 1))),
                           as.numeric(nrow(model.frame(tri_position_polled_7) |> 
                                             filter(rrp_in_parliament == 0))),
                           as.numeric(nrow(model.frame(tri_position_polled_7) |> 
                                             filter(rrp_in_parliament == 1)))
                           )
       ) |>
  kableExtra::kable(format = "latex")
```

``` r
ggplot(reg_data_tri,
       aes(x = tri_date,
           y = name_short,
           fill = factor(rrp_in_parliament),
           colour = factor(rrp_in_parliament))) +
  #geom_tile(height = 0.9) +
  geom_line(linewidth = 1.5) +
  scale_color_brewer(palette = "Set1", name = "RRP in Parliament") +
  facet_grid(rows = vars(country), scale = "free_y") +
  labs(fill = "RRP in parliament") +
  labs(x = "", y = "", title = "Distribution of RRP Parliamentary Presence Across Parties in the Sample")
```

![](regression_final_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
#ggsave("var_rrp_in_parl.png", device = "png", width = 9.5, height = 10)

model.frame(tri_position_7) |> 
              dplyr::select(rrp_lag, rrp_in_parliament) |>
              mutate(model = "Actual") |>
  bind_rows(model.frame(tri_position_polled_7) |>
              dplyr::select(lag_rrp_polled_voteshare, rrp_in_parliament) |>
              rename(rrp_lag = lag_rrp_polled_voteshare) |>
              mutate(model = "Polled")) |>
  mutate(rrp_in_parliament = as.factor(rrp_in_parliament)) |>
  ggplot(aes(x = rrp_lag, fill = rrp_in_parliament)) +
  geom_histogram(binwidth = 1, colour = NA, alpha = 0.2) +
  scale_fill_brewer(name = "", palette = "Set1"#, 
                    # labels = c("Polled" = "Polled RRP Vote Share", 
                    #            "Actual" = "Previous RRP Vote Share")
                    ) +
  facet_grid(rrp_in_parliament ~ model) #+
```

![](regression_final_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

``` r
  labs(x = "Time to Next Election", y = "Count",
       title = "Distribution of Time to Next Election in the Sample")
```

    ## <ggplot2::labels> List of 3
    ##  $ x    : chr "Time to Next Election"
    ##  $ y    : chr "Count"
    ##  $ title: chr "Distribution of Time to Next Election in the Sample"

``` r
overall_var <- var(reg_data_tri$rrp_in_parliament)

within_country_var <- var(demean(reg_data_tri$rrp_in_parliament, 
                               reg_data_tri$country), 
                        na.rm = TRUE)

within_party_var <- var(demean(reg_data_tri$rrp_in_parliament, 
                               reg_data_tri$parlgov_id), 
                        na.rm = TRUE)

100*within_party_var/overall_var
```

    ##          [,1]
    ## [1,] 32.12839

## Both Polled & Actual

``` r
tri_position_both_1 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + country + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament)

tri_position_both_2 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_both_3 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

tri_position_both_4 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               log_time_to_next + log_time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

modelsummary(list(tri_position_both_1, tri_position_both_2, tri_position_both_3,
                  tri_position_both_4), vcov = "robust", 
             stars = TRUE,
             coef_omit = "parlgov_id|country",
             coef_map = c(
               "(Intercept)" = "Constant",
               "date_numeric" = "Time",
               "lag_net_position" = "Lag Net Position",
               "time_to_next" = "Time to next election",
               "time_to_next_ep" = "Time to next EP election",
               "log_time_to_next" = "Log time to next election",
               "log_time_to_next_ep" = "Log time to next EP election",
               "vote_share" = "Vote share",
               "change_voteshare" = "Change in vote share",
               "rrp_lag" = "RRP Vote Share",
               "lag_rrp_polled_voteshare" = "RRP Polled Vote Share",
               "cabinet_party" = "Cabinet Party",
               "rrp_in_parliament" = "RRP in Parliament",
               "lag_eu_public_opinion" = "Lag EU Public Opinion",
             "log_time_to_next:lag_rrp_polled_voteshare" = "Log time to next election * RRP Polled vote share",
             "time_to_next:lag_rrp_polled_voteshare" = "Time to next election * RRP Polled vote share"
             ),
             gof_map = tibble::tribble(~raw, ~clean, ~fmt,
                                       "nobs", "N", 0,
                                       "r.squared", "R$^2$", 2,
                                       "adj.r.squared", "Adj. R$^2$", 2),
             add_rows = tibble::tribble(
               ~term, ~tri_position_both_1, ~tri_position_both_2, ~tri_position_both_3,
                  ~tri_position_both_4,
               "Party FE", "\\xmark", "\\cmark", "\\cmark", "\\cmark",
               "Country FE", "\\cmark", "\\xmark", "\\xmark", "\\xmark",
             ),
             #output = "table2.tex",
             output = "latex"
             )
```

<table style="width:98%;">
<colgroup>
<col style="width: 25%" />
<col style="width: 18%" />
<col style="width: 18%" />
<col style="width: 18%" />
<col style="width: 18%" />
</colgroup>
<thead>
<tr>
<th></th>
<th><ol type="1">
<li></li>
</ol></th>
<th><ol start="2" type="1">
<li></li>
</ol></th>
<th><ol start="3" type="1">
<li></li>
</ol></th>
<th><ol start="4" type="1">
<li></li>
</ol></th>
</tr>
</thead>
<tbody>
<tr>
<td>Constant</td>
<td>***</td>
<td></td>
<td>+</td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time</td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag Net Position</td>
<td>**</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time to next election</td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td></td>
</tr>
<tr>
<td>Time to next EP election</td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td></td>
</tr>
<tr>
<td>Log time to next election</td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td></td>
<td>()</td>
</tr>
<tr>
<td>Log time to next EP election</td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td></td>
<td>()</td>
</tr>
<tr>
<td>Vote share</td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Change in vote share</td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP Vote Share</td>
<td>+</td>
<td>*</td>
<td>*</td>
<td>*</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP Polled Vote Share</td>
<td>*</td>
<td>**</td>
<td>**</td>
<td>**</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Cabinet Party</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP in Parliament</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag EU Public Opinion</td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>N</td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>R$2$</td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Adj. R$2$</td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Party FE</td>
<td>xmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
</tr>
<tr>
<td>Country FE</td>
<td>cmark</td>
<td>xmark</td>
<td>xmark</td>
<td>xmark</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="5"><ul>
<li>p , * p , ** p , *** p |</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>

``` r
tibble(
  "SD RRP Vote Share" = sd(reg_data_tri$rrp_lag, na.rm = TRUE),
  "SD Net Position" = sd(reg_data_tri$net_position, na.rm = TRUE),
  "Coefficient" = coef(tri_position_3)["rrp_lag"],
  "Effect of SD change in RRP Vote Share" = coef(tri_position_3)["rrp_lag"]*sd(reg_data_tri$rrp_lag, na.rm = TRUE),
  "Effect of SD change in RRP Vote Share as % of SD Net Position" = 100*abs((coef(tri_position_3)["rrp_lag"]*sd(reg_data_tri$rrp_lag, na.rm = TRUE)))/sd(reg_data_tri$net_position, na.rm = TRUE),
  "Difference in Net Position between Venstre and Social Democrats (Denmark)" = mean(reg_data_tri$net_position[reg_data_tri$name_short == "V"], na.rm = TRUE) - mean(reg_data_tri$net_position[reg_data_tri$name_short == "Sd"], na.rm = TRUE)
  ) |>
  pivot_longer(cols = everything(), names_to = "Metric", values_to = "Value") |>
  kableExtra::kable(format = "latex", digits = 3)
```

# VIFs

``` r
vif(tri_position_1) |>
  as.data.frame() |>
  rownames_to_column("variable") |>
  mutate(model = "1") |>
  bind_rows(vif(tri_position_2) |>
              as.data.frame() |>
              rownames_to_column("variable") |>
              mutate(model = "2")) |>
  bind_rows(vif(tri_position_3) |>
              as.data.frame() |>
              rownames_to_column("variable") |>
              mutate(model = "3")) |>
  bind_rows(vif(tri_position_4) |>
              as.data.frame() |>
              rownames_to_column("variable") |>
              mutate(model = "4")) |>
  bind_rows(vif(tri_position_5) |>
              as.data.frame() |>
              rownames_to_column("variable") |>
              mutate(model = "5")) |>
  bind_rows(vif(tri_position_6) |>
              as.data.frame() |>
              rownames_to_column("variable") |>
              mutate(model = "6")) |>
  bind_rows(vif(tri_position_7) |>
              as.data.frame() |>
              rownames_to_column("variable") |>
              mutate(model = "7")) |>
  dplyr::select(variable, `GVIF^(1/(2*Df))`, model) |>
  kableExtra::kable("latex", digits = 2)
```

    ## there are higher-order terms (interactions) in this model
    ## consider setting type = 'predictor'; see ?vif
    ## there are higher-order terms (interactions) in this model
    ## consider setting type = 'predictor'; see ?vif
    ## there are higher-order terms (interactions) in this model
    ## consider setting type = 'predictor'; see ?vif

``` r
vif(tri_position_polled_1) |>
  as.data.frame() |>
  rownames_to_column("variable") |>
  mutate(model = "1") |>
  bind_rows(vif(tri_position_polled_2) |>
              as.data.frame() |>
              rownames_to_column("variable") |>
              mutate(model = "2")) |>
  bind_rows(vif(tri_position_polled_3) |>
              as.data.frame() |>
              rownames_to_column("variable") |>
              mutate(model = "3")) |>
  bind_rows(vif(tri_position_polled_4) |>
              as.data.frame() |>
              rownames_to_column("variable") |>
              mutate(model = "4")) |>
  bind_rows(vif(tri_position_polled_5) |>
              as.data.frame() |>
              rownames_to_column("variable") |>
              mutate(model = "5")) |>
  bind_rows(vif(tri_position_polled_6) |>
              as.data.frame() |>
              rownames_to_column("variable") |>
              mutate(model = "6")) |>
  bind_rows(vif(tri_position_polled_7) |>
              as.data.frame() |>
              rownames_to_column("variable") |>
              mutate(model = "7")) |>
  dplyr::select(variable, `GVIF^(1/(2*Df))`, model) |>
  kableExtra::kable("latex", digits = 2)
```

    ## there are higher-order terms (interactions) in this model
    ## consider setting type = 'predictor'; see ?vif
    ## there are higher-order terms (interactions) in this model
    ## consider setting type = 'predictor'; see ?vif
    ## there are higher-order terms (interactions) in this model
    ## consider setting type = 'predictor'; see ?vif

``` r
vif(tri_position_both_1) |>
  as.data.frame() |>
  rownames_to_column("variable") |>
  mutate(model = "1") |>
  bind_rows(vif(tri_position_both_2) |>
              as.data.frame() |>
              rownames_to_column("variable") |>
              mutate(model = "2")) |>
  bind_rows(vif(tri_position_both_3) |>
              as.data.frame() |>
              rownames_to_column("variable") |>
              mutate(model = "3")) |>
  bind_rows(vif(tri_position_both_4) |>
              as.data.frame() |>
              rownames_to_column("variable") |>
              mutate(model = "4")) |>
  dplyr::select(variable, `GVIF^(1/(2*Df))`, model) |>
  kableExtra::kable("latex", digits = 2)
```

# Different Standard Errors

``` r
set.seed(50)
boot_vote_95 <- marginaleffects::avg_slopes(lm(data = reg_data_tri,
                                        net_position ~ date_numeric +
                                          lag_net_position +
                                          time_to_next + time_to_next_ep +
                                          vote_share + change_voteshare +
                                          rrp_lag + cabinet_party +
                                          rrp_in_parliament + parlgov_id +
                                          lag_eu_public_opinion),
                                     variables = "rrp_lag",
                                     conf_level = 0.95,
                                     vcov = TRUE) |>
       inferences(method = "boot", R = 1000)

set.seed(50)
boot_vote_90 <- marginaleffects::avg_slopes(lm(data = reg_data_tri,
                                        net_position ~ date_numeric +
                                          lag_net_position +
                                          time_to_next + time_to_next_ep +
                                          vote_share + change_voteshare +
                                          rrp_lag + cabinet_party +
                                          rrp_in_parliament + parlgov_id +
                                          lag_eu_public_opinion),
                                     variables = "rrp_lag",
                                     conf_level = 0.90,
                                     vcov = TRUE) |>
       inferences(method = "boot", R = 1000)

set.seed(50)
boot_poll_95 <- marginaleffects::avg_slopes(lm(data = reg_data_tri,
                                        net_position ~ date_numeric +
                                          lag_net_position +
                                          time_to_next + time_to_next_ep +
                                          vote_share + change_voteshare +
                                          lag_rrp_polled_voteshare + cabinet_party +
                                          rrp_in_parliament + parlgov_id +
                                          lag_eu_public_opinion),
                                     variables = "lag_rrp_polled_voteshare",
                                     conf_level = 0.95,
                                     vcov = TRUE) |>
       inferences(method = "boot", R = 1000)

set.seed(50)
boot_poll_90 <- marginaleffects::avg_slopes(lm(data = reg_data_tri,
                                        net_position ~ date_numeric +
                                          lag_net_position +
                                          time_to_next + time_to_next_ep +
                                          vote_share + change_voteshare +
                                          lag_rrp_polled_voteshare + cabinet_party +
                                          rrp_in_parliament + parlgov_id +
                                          lag_eu_public_opinion),
                                     variables = "lag_rrp_polled_voteshare",
                                     conf_level = 0.90,
                                     vcov = TRUE) |>
       inferences(method = "boot", R = 1000)

set.seed(50)
boot_vote_both_95 <- marginaleffects::avg_slopes(lm(data = reg_data_tri,
                                        net_position ~ date_numeric +
                                          lag_net_position +
                                          time_to_next + time_to_next_ep +
                                          vote_share + change_voteshare +
                                          rrp_lag + lag_rrp_polled_voteshare + cabinet_party +
                                          rrp_in_parliament + parlgov_id +
                                          lag_eu_public_opinion),
                                     variables = "rrp_lag",
                                     conf_level = 0.95,
                                     vcov = TRUE) |>
       inferences(method = "boot", R = 1000)

set.seed(50)
boot_vote_both_90 <- marginaleffects::avg_slopes(lm(data = reg_data_tri,
                                        net_position ~ date_numeric +
                                          lag_net_position +
                                          time_to_next + time_to_next_ep +
                                          vote_share + change_voteshare +
                                          rrp_lag + lag_rrp_polled_voteshare + cabinet_party +
                                          rrp_in_parliament + parlgov_id +
                                          lag_eu_public_opinion),
                                     variables = "rrp_lag",
                                     conf_level = 0.90,
                                     vcov = TRUE) |>
       inferences(method = "boot", R = 1000)

set.seed(50)
boot_poll_both_95 <- marginaleffects::avg_slopes(lm(data = reg_data_tri,
                                        net_position ~ date_numeric +
                                          lag_net_position +
                                          time_to_next + time_to_next_ep +
                                          vote_share + change_voteshare +
                                          rrp_lag + lag_rrp_polled_voteshare + cabinet_party +
                                          rrp_in_parliament + parlgov_id +
                                          lag_eu_public_opinion),
                                     variables = "lag_rrp_polled_voteshare",
                                     conf_level = 0.95,
                                     vcov = TRUE) |>
       inferences(method = "boot", R = 1000)

set.seed(50)
boot_poll_both_90 <- marginaleffects::avg_slopes(lm(data = reg_data_tri,
                                        net_position ~ date_numeric +
                                          lag_net_position +
                                          time_to_next + time_to_next_ep +
                                          vote_share + change_voteshare +
                                          rrp_lag + lag_rrp_polled_voteshare + cabinet_party +
                                          rrp_in_parliament + parlgov_id +
                                          lag_eu_public_opinion),
                                     variables = "lag_rrp_polled_voteshare",
                                     conf_level = 0.90,
                                     vcov = TRUE) |>
       inferences(method = "boot", R = 1000)

beepr::beep()

boot_sep <- bind_rows(
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion),
                              variables = "rrp_lag",
                              conf_level = 0.95) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Default",
           level = as.factor(0.95),
           order = 1),
  
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion),
                              variables = "rrp_lag",
                              conf_level = 0.90) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Default",
           level = as.factor(0.90),
           order = 1),
    marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "rrp_lag", 
                            vcov = "robust",
                            conf_level = 0.95) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Robust",
           level = as.factor(0.95),
           order = 2),
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "rrp_lag", 
                            vcov = "robust",
                            conf_level = 0.90) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Robust",
           level = as.factor(0.90),
           order = 2),
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "rrp_lag", 
                            vcov = "HAC",
                            conf_level = 0.95) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "HAC",
           level = as.factor(0.95),
           order = 3),
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "rrp_lag", 
                            vcov = "HAC",
                            conf_level = 0.90) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "HAC",
           level = as.factor(0.90),
           order = 3),
  boot_vote_95 |>
    as.data.frame() |>
    dplyr::select(term, estimate, conf.low = conf.low, conf.high = conf.high) |>
    mutate(se = "Bootstrap",
           level = as.factor(0.95),
           order = 4),
  boot_vote_90 |>
    as.data.frame() |>
    dplyr::select(term, estimate, conf.low = conf.low, conf.high = conf.high) |>
    mutate(se = "Bootstrap",
           level = as.factor(0.90),
           order = 4),
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "rrp_lag", 
                            vcov = clubSandwich::vcovCR(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position +  
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion),
                               cluster = reg_data_tri$parlgov_id,
                               type = "CR2"),
                            conf_level = 0.95) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Party clustered (CR2)",
           level = as.factor(0.95),
           order = 5),
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "rrp_lag", 
                            vcov = clubSandwich::vcovCR(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position +  
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion),
                               cluster = reg_data_tri$parlgov_id,
                               type = "CR2"),
                            conf_level = 0.90) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Party clustered (CR2)",
           level = as.factor(0.90),
           order = 5),
  # marginaleffects::avg_slopes(lm(data = reg_data_tri,
  #                              net_position ~ date_numeric + 
  #                                lag_net_position + country + 
  #                                time_to_next + time_to_next_ep + vote_share + 
  #                                change_voteshare + rrp_lag + 
  #                                cabinet_party + rrp_in_parliament +
  #                                parlgov_id + lag_eu_public_opinion), 
  #                           variables = "rrp_lag", 
  #                           vcov = clubSandwich::vcovCR(lm(data = reg_data_tri,
  #                              net_position ~ date_numeric + 
  #                                lag_net_position + country + 
  #                                time_to_next + time_to_next_ep + vote_share + 
  #                                change_voteshare + rrp_lag + 
  #                                cabinet_party + rrp_in_parliament +
  #                                parlgov_id + lag_eu_public_opinion),
  #                              cluster = reg_data_tri$country,
  #                              type = "CR2")) |> 
  #   as.data.frame() |> 
  #   dplyr::select(term, estimate, conf.low, conf.high) |>
  #   mutate(se = "Country clustered (CR2)",
  #          order = 6),
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion),
                              variables = "lag_rrp_polled_voteshare",
                              conf_level = 0.95) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Default",
           level = as.factor(0.95),
           order = 1),
    marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion),
                              variables = "lag_rrp_polled_voteshare",
                              conf_level = 0.90) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Default",
           level = as.factor(0.90),
           order = 1),
    marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "lag_rrp_polled_voteshare", 
                            vcov = "robust",
                            conf_level = 0.95) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Robust",
           level = as.factor(0.95),
           order = 2),
     marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "lag_rrp_polled_voteshare", 
                            vcov = "robust",
                            conf_level = 0.90) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Robust",
           level = as.factor(0.90),
           order = 2),
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "lag_rrp_polled_voteshare", 
                            vcov = "HAC",
                            conf_level = 0.95) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "HAC",
           level = as.factor(0.95),
           order = 3),
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "lag_rrp_polled_voteshare", 
                            vcov = "HAC",
                            conf_level = 0.90) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "HAC",
           level = as.factor(0.90),
           order = 3),
  boot_poll_95 |>
    as.data.frame() |>
    dplyr::select(term, estimate, conf.low = conf.low, conf.high = conf.high) |>
    mutate(se = "Bootstrap",
           level = as.factor(0.95),
           order = 4),
  boot_poll_90 |>
    as.data.frame() |>
    dplyr::select(term, estimate, conf.low = conf.low, conf.high = conf.high) |>
    mutate(se = "Bootstrap",
           level = as.factor(0.90),
           order = 4),
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "lag_rrp_polled_voteshare", 
                            vcov = clubSandwich::vcovCR(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion),
                               cluster = reg_data_tri$parlgov_id,
                               type = "CR2"),
                            conf_level = 0.95) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Party clustered (CR2)",
           level = as.factor(0.95),
           order = 5),
    marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "lag_rrp_polled_voteshare", 
                            vcov = clubSandwich::vcovCR(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion),
                               cluster = reg_data_tri$parlgov_id,
                               type = "CR2"),
                            conf_level = 0.90) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Party clustered (CR2)",
           level = as.factor(0.90),
           order = 5),
  # marginaleffects::avg_slopes(lm(data = reg_data_tri,
  #                              net_position ~ date_numeric + 
  #                                lag_net_position + country + 
  #                                time_to_next + time_to_next_ep + vote_share + 
  #                                change_voteshare + lag_rrp_polled_voteshare + 
  #                                cabinet_party + rrp_in_parliament +
  #                                parlgov_id + lag_eu_public_opinion), 
  #                           variables = "lag_rrp_polled_voteshare", 
  #                           vcov = clubSandwich::vcovCR(lm(data = reg_data_tri,
  #                              net_position ~ date_numeric + 
  #                                lag_net_position + country + 
  #                                time_to_next + time_to_next_ep + vote_share + 
  #                                change_voteshare + lag_rrp_polled_voteshare + 
  #                                cabinet_party + rrp_in_parliament +
  #                                parlgov_id + lag_eu_public_opinion),
  #                              cluster = reg_data_tri$country,
  #                              type = "CR2")) |> 
  #   as.data.frame() |> 
  #   dplyr::select(term, estimate, conf.low, conf.high) |>
  #   mutate(se = "Country clustered (CR2)",
  #          order = 6),
  ) |> 
  ggplot(aes(x = reorder(se, order), y = estimate, ymin = conf.low, ymax = conf.high, 
             colour = term, shape = term, group = term, linewidth = level)) + 
  geom_pointrange(position = position_dodge(0.25)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_linewidth_manual(values = c(0.5, 1.25), guide = "none"
                         ) +
  scale_color_brewer(name = "", palette = "Set1", labels = c("Polled RRP Vote Share", "Previous RRP Vote Share")) +
  scale_shape_discrete(name = "", labels = c("Polled RRP Vote Share", "Previous RRP Vote Share")) +
  labs(x = "", y = "Marginal Effect",
       title = "Marginal Effect of RRP Support on Mainstream Parties' Net EU Positions",
       subtitle = "Models originally estimated with either polled or actual support",
       caption = "Point estimates with 90% and 95% confidence intervals") + 
  coord_flip()
```

    ## Registered S3 method overwritten by 'clubSandwich':
    ##   method    from    
    ##   bread.mlm sandwich

``` r
boot_both <- bind_rows(
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion),
                              variables = "rrp_lag",
                              conf_level = 0.95) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Default",
           level = as.factor(0.95),
           order = 1),
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion),
                              variables = "rrp_lag",
                              conf_level = 0.90) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Default",
           level = as.factor(0.90),
           order = 1),
    marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "rrp_lag", 
                            conf_level = 0.95,
                            vcov = "robust") |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Robust",
           level = as.factor(0.95),
           order = 2),
    marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "rrp_lag", 
                            conf_level = 0.90,
                            vcov = "robust") |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Robust",
           level = as.factor(0.90),
           order = 2),
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "rrp_lag", 
                            conf_level = 0.95,
                            vcov = "HAC") |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "HAC",
           level = as.factor(0.95),
           order = 3),
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "rrp_lag", 
                            conf_level = 0.90,
                            vcov = "HAC") |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "HAC",
           level = as.factor(0.90),
           order = 3),
  boot_vote_95 |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low = conf.low, conf.high = conf.high) |>
    mutate(se = "Bootstrap",
           level = as.factor(0.95),
           order = 4),
  boot_vote_90 |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low = conf.low, conf.high = conf.high) |>
    mutate(se = "Bootstrap",
           level = as.factor(0.90),
           order = 4),
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "rrp_lag", 
                            conf_level = 0.95,
                            vcov = clubSandwich::vcovCR(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion),
                               cluster = reg_data_tri$parlgov_id,
                               type = "CR2")) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Party clustered (CR2)",
           level = as.factor(0.95),
           order = 5),
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "rrp_lag", 
                            conf_level = 0.90,
                            vcov = clubSandwich::vcovCR(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position + 
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion),
                               cluster = reg_data_tri$parlgov_id,
                               type = "CR2")) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Party clustered (CR2)",
           level = as.factor(0.90),
           order = 5),
  # marginaleffects::avg_slopes(lm(data = reg_data_tri,
  #                              net_position ~ date_numeric + 
  #                                lag_net_position + country + 
  #                                time_to_next + time_to_next_ep + vote_share + 
  #                                change_voteshare + rrp_lag + lag_rrp_polled_voteshare + 
  #                                cabinet_party + rrp_in_parliament +
  #                                parlgov_id + lag_eu_public_opinion), 
  #                           variables = "rrp_lag", 
  #                           vcov = clubSandwich::vcovCR(lm(data = reg_data_tri,
  #                              net_position ~ date_numeric + 
  #                                lag_net_position + country + 
  #                                time_to_next + time_to_next_ep + vote_share + 
  #                                change_voteshare + rrp_lag + lag_rrp_polled_voteshare + 
  #                                cabinet_party + rrp_in_parliament +
  #                                parlgov_id + lag_eu_public_opinion),
  #                              cluster = reg_data_tri$country,
  #                              type = "CR2")) |> 
  #   as.data.frame() |> 
  #   dplyr::select(term, estimate, conf.low, conf.high) |>
  #   mutate(se = "Country clustered (CR2)",
  #          order = 6),
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position +  
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion),
                              variables = "lag_rrp_polled_voteshare",
                              conf_level = 0.95) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Default",
           level = as.factor(0.95),
           order = 1),
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position +  
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion),
                              variables = "lag_rrp_polled_voteshare",
                              conf_level = 0.90) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Default",
           level = as.factor(0.90),
           order = 1),
    marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position +  
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "lag_rrp_polled_voteshare", 
                            conf_level = 0.95,
                            vcov = "robust") |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Robust",
           level = as.factor(0.95),
           order = 2),
    marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position +  
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "lag_rrp_polled_voteshare", 
                            conf_level = 0.90,
                            vcov = "robust") |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Robust",
           level = as.factor(0.90),
           order = 2),
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position +  
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "lag_rrp_polled_voteshare", 
                            conf_level = 0.95,
                            vcov = "HAC") |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "HAC",
           level = as.factor(0.95),
           order = 3),
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position +  
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "lag_rrp_polled_voteshare", 
                            conf_level = 0.90,
                            vcov = "HAC") |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "HAC",
           level = as.factor(0.90),
           order = 3),
  boot_poll_both_95 |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low = conf.low, conf.high = conf.high) |>
    mutate(se = "Bootstrap",
           level = as.factor(0.95),
           order = 4),
  boot_poll_both_90 |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low = conf.low, conf.high = conf.high) |>
    mutate(se = "Bootstrap",
           level = as.factor(0.90),
           order = 4),
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position +  
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "lag_rrp_polled_voteshare", 
                            conf_level = 0.95,
                            vcov = clubSandwich::vcovCR(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position +  
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion),
                               cluster = reg_data_tri$parlgov_id,
                               type = "CR2")) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Party clustered (CR2)",
           level = as.factor(0.95),
           order = 5),
  marginaleffects::avg_slopes(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position +  
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion), 
                            variables = "lag_rrp_polled_voteshare", 
                            conf_level = 0.90,
                            vcov = clubSandwich::vcovCR(lm(data = reg_data_tri,
                               net_position ~ date_numeric + 
                                 lag_net_position +  
                                 time_to_next + time_to_next_ep + vote_share + 
                                 change_voteshare + rrp_lag + lag_rrp_polled_voteshare + 
                                 cabinet_party + rrp_in_parliament +
                                 parlgov_id + lag_eu_public_opinion),
                               cluster = reg_data_tri$parlgov_id,
                               type = "CR2")) |> 
    as.data.frame() |> 
    dplyr::select(term, estimate, conf.low, conf.high) |>
    mutate(se = "Party clustered (CR2)",
           level = as.factor(0.90),
           order = 5),
  # marginaleffects::avg_slopes(lm(data = reg_data_tri,
  #                              net_position ~ date_numeric + 
  #                                lag_net_position + country + 
  #                                time_to_next + time_to_next_ep + vote_share + 
  #                                change_voteshare + lag_rrp_polled_voteshare + 
  #                                cabinet_party + rrp_lag + lag_rrp_polled_voteshare +
  #                                parlgov_id + lag_eu_public_opinion), 
  #                           variables = "lag_rrp_polled_voteshare", 
  #                           vcov = clubSandwich::vcovCR(lm(data = reg_data_tri,
  #                              net_position ~ date_numeric + 
  #                                lag_net_position + country + 
  #                                time_to_next + time_to_next_ep + vote_share + 
  #                                change_voteshare + lag_rrp_polled_voteshare + 
  #                                cabinet_party + rrp_lag + lag_rrp_polled_voteshare +
  #                                parlgov_id + lag_eu_public_opinion),
  #                              cluster = reg_data_tri$country,
  #                              type = "CR2")) |> 
  #   as.data.frame() |> 
  #   dplyr::select(term, estimate, conf.low, conf.high) |>
  #   mutate(se = "Country clustered (CR2)",
  #          order = 6),
  ) |> 
  ggplot(aes(x = reorder(se, order), y = estimate, ymin = conf.low, ymax = conf.high, 
             colour = term, shape = term, group = term, linewidth = level)) + 
  geom_pointrange(position = position_dodge(0.25)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_linewidth_manual(values = c(0.5, 1.25), guide = "none"
                         ) +
  scale_color_brewer(name = "", palette = "Set1", labels = c("Polled RRP Vote Share", "Previous RRP Vote Share")) +
  scale_shape_discrete(name = "", labels = c("Polled RRP Vote Share", "Previous RRP Vote Share")) +
  labs(x = "", y = "Marginal Effect",
       title = "Marginal Effect of RRP Support on Mainstream Parties' Net EU Positions",
       subtitle = "Models originally estimated with both polled and actual support",
       caption = "Point estimates with 90% and 95% confidence intervals") + 
  coord_flip()

cowplot::plot_grid(boot_sep, boot_both,
                   labels = c("A", "B"),
                   nrow = 2)
```

![](regression_final_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
#ggsave("marginal_effects_SE.png", device = "png", width = 9.5, height = 12)
```

# Centre-right/-left heterogeneity

``` r
reg_data_tri <- read_csv("reg_data_tri.csv") |>
  mutate(parlgov_id = as.factor(parlgov_id),
         log_time_to_next = log(time_to_next + 1),
         log_time_to_next_ep = log(time_to_next_ep + 1)) |>
  filter(!is.na(family_name)) |>
  filter(parlgov_id %in% c(1013, 973, ## OVP and SPO
                           558, 1727, ## SPD and CDU
                           1629, 1605, ## Danish SDs and Venstre
                           902, 645, ## PSOE and PP
                           1556, 773, ## Labour and Conservatives
                           1409, 742, 235, ## VVD, PvdA, and CDA
                           657, 904 ## SAP and Moderates
                           ))
```

    ## Rows: 4208 Columns: 61
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (7): country, name_short, name, name_english, tri, family_name, family_2
    ## dbl  (53): parlgov_id, count, eu_count, prop_eu_mention, count_support, prop...
    ## date  (1): tri_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
tri_position_2 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag*family_2 + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_3 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag*family_2 + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

tri_position_polled_2 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare*family_2 + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_polled_3 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare*family_2 + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

tri_position_both_2 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag*family_2 + lag_rrp_polled_voteshare*family_2 + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_both_3 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag*family_2 + lag_rrp_polled_voteshare*family_2 + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

modelsummary(list(tri_position_2, tri_position_3,
                  tri_position_polled_2, tri_position_polled_3,
                  tri_position_both_2, tri_position_both_3), vcov = "robust", 
             stars = TRUE,
             coef_omit = "parlgov_id|country",
             coef_map = c(
               "(Intercept)" = "Constant",
               "date_numeric" = "Time",
               "lag_net_position" = "Lag Net Position",
               "time_to_next" = "Time to next election",
               "time_to_next_ep" = "Time to next EP election",
               "vote_share" = "Vote share",
               "change_voteshare" = "Change in vote share",
               "family_2Centre-right" = "Centre-right",
               "rrp_lag" = "RRP Vote Share",
               "lag_rrp_polled_voteshare" = "RRP Polled Vote Share",
               "cabinet_party" = "Cabinet Party",
               "rrp_in_parliament" = "RRP in Parliament",
               "lag_eu_public_opinion" = "Lag EU Public Opinion",
               "family_2Centre-right:rrp_lag" = "Centre-right * RRP Vote share",
             "rrp_lag:family_2Centre-right" = "Centre-right * RRP Vote share",
             "family_2Centre-right:lag_rrp_polled_voteshare" = "Centre-right * RRP Polled vote share",
             "lag_rrp_polled_voteshare:family_2Centre-right" = "Centre-right * RRP Polled vote share"
             ),
             gof_map = tibble::tribble(~raw, ~clean, ~fmt,
                                       "nobs", "N", 0,
                                       "r.squared", "R$^2$", 2,
                                       "adj.r.squared", "Adj. R$^2$", 2),
             add_rows = tibble::tribble(
               ~term, ~tri_position_2, ~tri_position_3,
               ~tri_position_polled_2, ~tri_position_3,
               ~tri_position_both_2, ~tri_position_both_3,
               "Party FE", "\\cmark", "\\cmark", "\\cmark", 
               "\\cmark", "\\cmark", "\\cmark"
             ),
             output = "latex"
             )
```

<table style="width:97%;">
<colgroup>
<col style="width: 20%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
</colgroup>
<thead>
<tr>
<th></th>
<th><ol type="1">
<li></li>
</ol></th>
<th><ol start="2" type="1">
<li></li>
</ol></th>
<th><ol start="3" type="1">
<li></li>
</ol></th>
<th><ol start="4" type="1">
<li></li>
</ol></th>
<th><ol start="5" type="1">
<li></li>
</ol></th>
<th><ol start="6" type="1">
<li></li>
</ol></th>
</tr>
</thead>
<tbody>
<tr>
<td>Constant</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time</td>
<td>*</td>
<td>*</td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag Net Position</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time to next election</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time to next EP election</td>
<td>+</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Vote share</td>
<td>+</td>
<td></td>
<td>+</td>
<td></td>
<td>+</td>
<td>+</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Change in vote share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Centre-right</td>
<td>*</td>
<td>*</td>
<td></td>
<td></td>
<td></td>
<td>+</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP Vote Share</td>
<td>+</td>
<td>+</td>
<td></td>
<td></td>
<td>*</td>
<td>*</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP Polled Vote Share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Cabinet Party</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP in Parliament</td>
<td>**</td>
<td>**</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag EU Public Opinion</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td>()</td>
<td></td>
<td>()</td>
<td></td>
<td>()</td>
</tr>
<tr>
<td>Centre-right * RRP Vote share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Centre-right * RRP Polled vote share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td>+</td>
<td>+</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>N</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>R$2$</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Adj. R$2$</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Party FE</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="7"><ul>
<li>p , * p , ** p , *** p |</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>

``` r
a <- plot_slopes(tri_position_3, variables = "rrp_lag", vcov = "robust",
                condition = "family_2", conf_level = 0.95, draw = F) |>
      mutate(level = 0.95) |>
    bind_rows(
    plot_slopes(tri_position_polled_3, variables = "lag_rrp_polled_voteshare", vcov = "robust",
                condition = "family_2", conf_level = 0.95, draw = F) |>
      mutate(level = 0.95)) |>
bind_rows(
    plot_slopes(tri_position_3, variables = "rrp_lag", vcov = "robust",
                condition = "family_2", conf_level = 0.90, draw = F) |>
      mutate(level = 0.90)) |>
    bind_rows(
    plot_slopes(tri_position_polled_3, variables = "lag_rrp_polled_voteshare", vcov = "robust",
                condition = "family_2", conf_level = 0.90, draw = F) |>
      mutate(level = 0.90)) |>
  mutate(level = as.factor(level)) |>
  ggplot() +
  geom_pointrange(aes(x = family_2, 
             y = estimate, ymin = conf.low, ymax = conf.high, 
             color = term, shape = term, group = term, linewidth = level),
             position = position_dodge(width = 0.25)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_linewidth_manual(values = c(1.25, 0.5), guide = "none") +
  scale_color_brewer(name = "", palette = "Set1", labels = c("Polled RRP Vote Share", "Previous RRP Vote Share")) +
  scale_shape_discrete(name = "", labels = c("Polled RRP Vote Share", "Previous RRP Vote Share")) +
  labs(x = "", y = "Marginal Effect",
       title = "Marginal Effect of RRP Support on Mainstream Parties' Net EU Positions",
       subtitle = "Models originally estimated with either polled or actual support")
  
b <- plot_slopes(tri_position_both_3, variables = "rrp_lag", vcov = "robust",
                condition = "family_2", conf_level = 0.95, draw = F) |>
      mutate(level = 0.95) |>
    bind_rows(
    plot_slopes(tri_position_both_3, variables = "lag_rrp_polled_voteshare", vcov = "robust",
                condition = "family_2", conf_level = 0.95, draw = F) |>
      mutate(level = 0.95)) |>
  bind_rows(
    plot_slopes(tri_position_both_3, variables = "rrp_lag", vcov = "robust",
                condition = "family_2", conf_level = 0.90, draw = F) |>
      mutate(level = 0.90)) |>
    bind_rows(
    plot_slopes(tri_position_both_3, variables = "lag_rrp_polled_voteshare", vcov = "robust",
                condition = "family_2", conf_level = 0.90, draw = F) |>
      mutate(level = 0.90)) |>
  mutate(level = as.factor(level)) |>
  ggplot() +
  geom_pointrange(aes(x = family_2, 
             y = estimate, ymin = conf.low, ymax = conf.high, 
             color = term, shape = term, group = term, linewidth = level),
             position = position_dodge(width = 0.25)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_linewidth_manual(values = c(1.25, 0.5), guide = "none") +
  scale_color_brewer(name = "", palette = "Set1", labels = c("Polled RRP Vote Share", "Previous RRP Vote Share")) +
  scale_shape_discrete(name = "", labels = c("Polled RRP Vote Share", "Previous RRP Vote Share")) +
  labs(x = "", y = "Marginal Effect",
       title = "Marginal Effect of RRP Support on Mainstream Parties' Net EU Positions",
       subtitle = "Models originally estimated with both polled and actual support",
       caption = "Point estimates with 90% and 95% confidence intervals")

cowplot::plot_grid(a, b,
                   labels = c("A", "B"),
                   nrow = 2)
```

![](regression_final_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
#ggsave("marginal_effects_party_family.png", device = "png", width = 9.5, height = 12)
```

# Without UK

``` r
reg_data_tri <- read_csv("reg_data_tri.csv") |>
  mutate(parlgov_id = as.factor(parlgov_id),
         log_time_to_next = log(time_to_next + 1),
         log_time_to_next_ep = log(time_to_next_ep + 1)) |>
  filter(!is.na(family_name)) |>
  filter(parlgov_id %in% c(1013, 973, ## OVP and SPO
                           558, 1727, ## SPD and CDU
                           1629, 1605, ## Danish SDs and Venstre
                           902, 645, ## PSOE and PP
                           #1556, 773, ## Labour and Conservatives
                           1409, 742, 235, ## VVD, PvdA, and CDA
                           657, 904 ## SAP and Moderates
                           ))
```

    ## Rows: 4208 Columns: 61
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (7): country, name_short, name, name_english, tri, family_name, family_2
    ## dbl  (53): parlgov_id, count, eu_count, prop_eu_mention, count_support, prop...
    ## date  (1): tri_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
tri_position_2 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_3 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

tri_position_polled_2 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_polled_3 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

tri_position_both_2 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_both_3 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

modelsummary(list(tri_position_2, tri_position_3,
                  tri_position_polled_2, tri_position_polled_3,
                  tri_position_both_2, tri_position_both_3), vcov = "robust", 
             stars = TRUE,
             coef_omit = "parlgov_id|country",
             coef_map = c(
               "(Intercept)" = "Constant",
               "date_numeric" = "Time",
               "lag_net_position" = "Lag Net Position",
               "time_to_next" = "Time to next election",
               "time_to_next_ep" = "Time to next EP election",
               "log_time_to_next" = "Log time to next election",
               "log_time_to_next_ep" = "Log time to next EP election",
               "vote_share" = "Vote share",
               "change_voteshare" = "Change in vote share",
               "rrp_lag" = "RRP Vote Share",
               "lag_rrp_polled_voteshare" = "RRP Polled Vote Share",
               "cabinet_party" = "Cabinet Party",
               "rrp_in_parliament" = "RRP in Parliament",
               "lag_eu_public_opinion" = "Lag EU Public Opinion",
             "log_time_to_next:lag_rrp_polled_voteshare" = "Log time to next election * RRP Polled vote share",
             "time_to_next:lag_rrp_polled_voteshare" = "Time to next election * RRP Polled vote share"
             ),
             gof_map = tibble::tribble(~raw, ~clean, ~fmt,
                                       "nobs", "N", 0,
                                       "r.squared", "R$^2$", 2,
                                       "adj.r.squared", "Adj. R$^2$", 2),
             add_rows = tibble::tribble(
               ~term, ~tri_position_2, ~tri_position_3,
               ~tri_position_polled_2, ~tri_position_polled_3,
               ~tri_position_both_2, ~tri_position_both_3,
               "Party FE", "\\cmark", "\\cmark", "\\cmark", "\\cmark", "\\cmark", "\\cmark",
             ),
             output = "latex"
             )
```

<table style="width:97%;">
<colgroup>
<col style="width: 18%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 12%" />
</colgroup>
<thead>
<tr>
<th></th>
<th><ol type="1">
<li></li>
</ol></th>
<th><ol start="2" type="1">
<li></li>
</ol></th>
<th><ol start="3" type="1">
<li></li>
</ol></th>
<th><ol start="4" type="1">
<li></li>
</ol></th>
<th><ol start="5" type="1">
<li></li>
</ol></th>
<th><ol start="6" type="1">
<li></li>
</ol></th>
</tr>
</thead>
<tbody>
<tr>
<td>Constant</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time</td>
<td>*</td>
<td>*</td>
<td>+</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag Net Position</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time to next election</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time to next EP election</td>
<td>+</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Vote share</td>
<td>+</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Change in vote share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP Vote Share</td>
<td>*</td>
<td>*</td>
<td></td>
<td></td>
<td>*</td>
<td>*</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP Polled Vote Share</td>
<td></td>
<td></td>
<td>+</td>
<td>*</td>
<td>*</td>
<td>*</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Cabinet Party</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP in Parliament</td>
<td>*</td>
<td>+</td>
<td>*</td>
<td>*</td>
<td>**</td>
<td>*</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag EU Public Opinion</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td>()</td>
<td></td>
<td>()</td>
<td></td>
<td>()</td>
</tr>
<tr>
<td>N</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>R$2$</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Adj. R$2$</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Party FE</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="7"><ul>
<li>p , * p , ** p , *** p |</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>

# Log ratio of support to opposition

``` r
reg_data_tri <- read_csv("reg_data_tri.csv") |> 
  mutate(parlgov_id = as.factor(parlgov_id),
         log_time_to_next = log(time_to_next + 1),
         log_time_to_next_ep = log(time_to_next_ep + 1)) |> 
  filter(!is.na(family_name)) |>
  filter(parlgov_id %in% c(1013, 973, ## OVP and SPO
                           558, 1727, ## SPD and CDU
                           1629, 1605, ## Danish SDs and Venstre
                           902, 645, ## PSOE and PP
                           1556, 773, ## Labour and Conservatives
                           1409, 742, 235, ## VVD, PvdA, and CDA
                           657, 904 ## SAP and Moderates
                           ))
```

    ## Rows: 4208 Columns: 61
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (7): country, name_short, name, name_english, tri, family_name, family_2
    ## dbl  (53): parlgov_id, count, eu_count, prop_eu_mention, count_support, prop...
    ## date  (1): tri_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
tri_position_2 <- lm(data = reg_data_tri,
             lowe ~ date_numeric + 
               lag_lowe + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_3 <- lm(data = reg_data_tri,
             lowe ~ date_numeric + 
               lag_lowe + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

tri_position_polled_2 <- lm(data = reg_data_tri,
             lowe ~ date_numeric + 
               lag_lowe + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_polled_3 <- lm(data = reg_data_tri,
            lowe ~ date_numeric + 
               lag_lowe + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

tri_position_both_2 <- lm(data = reg_data_tri,
             lowe ~ date_numeric + 
               lag_lowe + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_both_3 <- lm(data = reg_data_tri,
             lowe ~ date_numeric + 
               lag_lowe + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

modelsummary(list(tri_position_2, tri_position_3,
                  tri_position_polled_2, tri_position_polled_3,
                  tri_position_both_2, tri_position_both_3), vcov = "robust", 
             stars = TRUE,
             coef_omit = "parlgov_id|country",
             coef_map = c(
               "(Intercept)" = "Constant",
               "date_numeric" = "Time",
               "lag_lowe" = "Lag Logit Scale of Position",
               "time_to_next" = "Time to next election",
               "time_to_next_ep" = "Time to next EP election",
               "vote_share" = "Vote share",
               "change_voteshare" = "Change in vote share",
               "rrp_lag" = "RRP Vote Share",
               "lag_rrp_polled_voteshare" = "RRP Polled Vote Share",
               "cabinet_party" = "Cabinet Party",
               "rrp_in_parliament" = "RRP in Parliament",
               "lag_eu_public_opinion" = "Lag EU Public Opinion"
             ),
             gof_map = tibble::tribble(~raw, ~clean, ~fmt,
                                       "nobs", "N", 0,
                                       "r.squared", "R$^2$", 2,
                                       "adj.r.squared", "Adj. R$^2$", 2),
             add_rows = tibble::tribble(
               ~term, ~tri_position_2, ~tri_position_3,
               ~tri_position_polled_2, ~tri_position_polled_3,
               ~tri_position_both_2, ~tri_position_both_3,
               "Party FE", "\\cmark", "\\cmark", "\\cmark", "\\cmark", "\\cmark", "\\cmark",
             ),
             output = "latex"
             )
```

<table style="width:97%;">
<colgroup>
<col style="width: 18%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 12%" />
</colgroup>
<thead>
<tr>
<th></th>
<th><ol type="1">
<li></li>
</ol></th>
<th><ol start="2" type="1">
<li></li>
</ol></th>
<th><ol start="3" type="1">
<li></li>
</ol></th>
<th><ol start="4" type="1">
<li></li>
</ol></th>
<th><ol start="5" type="1">
<li></li>
</ol></th>
<th><ol start="6" type="1">
<li></li>
</ol></th>
</tr>
</thead>
<tbody>
<tr>
<td>Constant</td>
<td></td>
<td>**</td>
<td></td>
<td>**</td>
<td></td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time</td>
<td>**</td>
<td></td>
<td>*</td>
<td></td>
<td>+</td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag Logit Scale of Position</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time to next election</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time to next EP election</td>
<td>+</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Vote share</td>
<td>*</td>
<td>*</td>
<td>*</td>
<td></td>
<td>*</td>
<td>+</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Change in vote share</td>
<td>+</td>
<td>+</td>
<td></td>
<td></td>
<td>+</td>
<td>+</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP Vote Share</td>
<td>*</td>
<td>+</td>
<td></td>
<td></td>
<td>*</td>
<td>+</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP Polled Vote Share</td>
<td></td>
<td></td>
<td></td>
<td>*</td>
<td></td>
<td>*</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Cabinet Party</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP in Parliament</td>
<td>**</td>
<td>+</td>
<td>**</td>
<td>*</td>
<td>***</td>
<td>**</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag EU Public Opinion</td>
<td></td>
<td>***</td>
<td></td>
<td>***</td>
<td></td>
<td>***</td>
</tr>
<tr>
<td></td>
<td></td>
<td>()</td>
<td></td>
<td>()</td>
<td></td>
<td>()</td>
</tr>
<tr>
<td>N</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>R$2$</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Adj. R$2$</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Party FE</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="7"><ul>
<li>p , * p , ** p , *** p |</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>

# Minimum number of EU sentences

``` r
reg_data_tri <- read_csv("reg_data_tri.csv") |>
  mutate(parlgov_id = as.factor(parlgov_id),
         log_time_to_next = log(time_to_next + 1),
         log_time_to_next_ep = log(time_to_next_ep + 1)) |>
  filter(!is.na(family_name)) |>
  filter(parlgov_id %in% c(1013, 973, ## OVP and SPO
                           558, 1727, ## SPD and CDU
                           1629, 1605, ## Danish SDs and Venstre
                           902, 645, ## PSOE and PP
                           1556, 773, ## Labour and Conservatives
                           1409, 742, 235, ## VVD, PvdA, and CDA
                           657, 904 ## SAP and Moderates
                           ))
```

    ## Rows: 4208 Columns: 61
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (7): country, name_short, name, name_english, tri, family_name, family_2
    ## dbl  (53): parlgov_id, count, eu_count, prop_eu_mention, count_support, prop...
    ## date  (1): tri_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
events <- tibble::tibble(
  prop_eu_mention = c(quantile(reg_data_tri$prop_eu_mention, 0.05, na.rm = TRUE), 
                      quantile(reg_data_tri$prop_eu_mention, 0.10, na.rm = TRUE), 
                      quantile(reg_data_tri$prop_eu_mention, 0.25, na.rm = TRUE)),
  n_obs = c(reg_data_tri |>
              filter(prop_eu_mention > quantile(reg_data_tri$prop_eu_mention, 0.05, na.rm = TRUE)) |>
              dplyr::select(net_position, date_numeric, lag_net_position,
                            time_to_next, time_to_next_ep, vote_share, 
                            change_voteshare, lag_rrp_polled_voteshare, 
                            cabinet_party, rrp_in_parliament, parlgov_id) |>
              na.omit() |> 
              nrow(),
            reg_data_tri |>
              filter(prop_eu_mention > quantile(reg_data_tri$prop_eu_mention, 0.10, na.rm = TRUE)) |>
              dplyr::select(net_position, date_numeric, lag_net_position,
                            time_to_next, time_to_next_ep, vote_share, 
                            change_voteshare, lag_rrp_polled_voteshare, 
                            cabinet_party, rrp_in_parliament, parlgov_id) |>
              na.omit() |> 
              nrow(),
            reg_data_tri |>
              filter(prop_eu_mention > quantile(reg_data_tri$prop_eu_mention, 0.25, na.rm = TRUE)) |>
              dplyr::select(net_position, date_numeric, lag_net_position,
                            time_to_next, time_to_next_ep, vote_share, 
                            change_voteshare, lag_rrp_polled_voteshare, 
                            cabinet_party, rrp_in_parliament, parlgov_id) |>
              na.omit() |> 
              nrow()),
  percentile = c("5th percentile", "10th percentile", "25th percentile"),
  label = paste(percentile, " (", n_obs, " observations)", sep = "") |>
    as.factor() |>
    relevel("5th percentile (1092 observations)")
)

reg_data_tri |> 
  ggplot(aes(x = prop_eu_mention)) + 
  geom_density() +
  geom_vline(data = events, aes(xintercept = prop_eu_mention, 
                                colour = label,
                                linetype = label),
             linewidth = 0.75) +
  scale_color_brewer(name = "", palette = "Set1") +
  scale_linetype_manual(name = "", values = c("solid", "dashed", "dotdash")) +
  labs(x = "Share of Sentences Mentioning the EU", y = "Density", title = "Distribution of Share of EU Mentions")
```

![](regression_final_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
#ggsave("density_percentiles.png", device = "png", width = 9.5, height = 6)
```

``` r
reg_data_tri <- read_csv("reg_data_tri.csv") |>
  mutate(parlgov_id = as.factor(parlgov_id),
         log_time_to_next = log(time_to_next + 1),
         log_time_to_next_ep = log(time_to_next_ep + 1)) |>
  filter(!is.na(family_name)) |>
  filter(parlgov_id %in% c(1013, 973, ## OVP and SPO
                           558, 1727, ## SPD and CDU
                           1629, 1605, ## Danish SDs and Venstre
                           902, 645, ## PSOE and PP
                           1556, 773, ## Labour and Conservatives
                           1409, 742, 235, ## VVD, PvdA, and CDA
                           657, 904 ## SAP and Moderates
                           ) &
           prop_eu_mention > events$prop_eu_mention[1]) 
```

    ## Rows: 4208 Columns: 61
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (7): country, name_short, name, name_english, tri, family_name, family_2
    ## dbl  (53): parlgov_id, count, eu_count, prop_eu_mention, count_support, prop...
    ## date  (1): tri_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
tri_position_2 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_3 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

tri_position_polled_2 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_polled_3 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

tri_position_both_2 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_both_3 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

modelsummary(list(tri_position_2, tri_position_3,
                  tri_position_polled_2, tri_position_polled_3,
                  tri_position_both_2, tri_position_both_3), vcov = "robust", 
             stars = TRUE,
             coef_omit = "parlgov_id|country",
             coef_map = c(
               "(Intercept)" = "Constant",
               "date_numeric" = "Time",
               "lag_net_position" = "Lag Net Position",
               "time_to_next" = "Time to next election",
               "time_to_next_ep" = "Time to next EP election",
               "log_time_to_next" = "Log time to next election",
               "log_time_to_next_ep" = "Log time to next EP election",
               "vote_share" = "Vote share",
               "change_voteshare" = "Change in vote share",
               "rrp_lag" = "RRP Vote Share",
               "lag_rrp_polled_voteshare" = "RRP Polled Vote Share",
               "cabinet_party" = "Cabinet Party",
               "rrp_in_parliament" = "RRP in Parliament",
               "lag_eu_public_opinion" = "Lag EU Public Opinion"
             ),
             gof_map = tibble::tribble(~raw, ~clean, ~fmt,
                                       "nobs", "N", 0,
                                       "r.squared", "R$^2$", 2,
                                       "adj.r.squared", "Adj. R$^2$", 2),
             add_rows = tibble::tribble(
               ~term, ~tri_position_2, ~tri_position_3,
               ~tri_position_polled_2, ~tri_position_polled_3,
               ~tri_position_both_2, ~tri_position_both_3,
               "Party FE", "\\cmark", "\\cmark", "\\cmark", "\\cmark", "\\cmark", "\\cmark",
             ),
             output = "latex"
             )
```

<table style="width:97%;">
<colgroup>
<col style="width: 18%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 12%" />
</colgroup>
<thead>
<tr>
<th></th>
<th><ol type="1">
<li></li>
</ol></th>
<th><ol start="2" type="1">
<li></li>
</ol></th>
<th><ol start="3" type="1">
<li></li>
</ol></th>
<th><ol start="4" type="1">
<li></li>
</ol></th>
<th><ol start="5" type="1">
<li></li>
</ol></th>
<th><ol start="6" type="1">
<li></li>
</ol></th>
</tr>
</thead>
<tbody>
<tr>
<td>Constant</td>
<td></td>
<td>+</td>
<td></td>
<td>*</td>
<td></td>
<td>*</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time</td>
<td>**</td>
<td>*</td>
<td>+</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag Net Position</td>
<td>*</td>
<td>*</td>
<td>*</td>
<td>*</td>
<td>*</td>
<td>*</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time to next election</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time to next EP election</td>
<td>**</td>
<td>**</td>
<td>**</td>
<td>*</td>
<td>**</td>
<td>*</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Vote share</td>
<td>+</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Change in vote share</td>
<td>+</td>
<td>+</td>
<td></td>
<td></td>
<td>+</td>
<td>+</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP Vote Share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP Polled Vote Share</td>
<td></td>
<td></td>
<td>*</td>
<td>**</td>
<td>*</td>
<td>**</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Cabinet Party</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP in Parliament</td>
<td>**</td>
<td>*</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag EU Public Opinion</td>
<td></td>
<td>*</td>
<td></td>
<td>**</td>
<td></td>
<td>**</td>
</tr>
<tr>
<td></td>
<td></td>
<td>()</td>
<td></td>
<td>()</td>
<td></td>
<td>()</td>
</tr>
<tr>
<td>N</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>R$2$</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Adj. R$2$</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Party FE</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="7"><ul>
<li>p , * p , ** p , *** p |</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>

``` r
reg_data_tri <- read_csv("reg_data_tri.csv") |>
  mutate(parlgov_id = as.factor(parlgov_id),
         log_time_to_next = log(time_to_next + 1),
         log_time_to_next_ep = log(time_to_next_ep + 1)) |>
  filter(!is.na(family_name)) |>
  filter(parlgov_id %in% c(1013, 973, ## OVP and SPO
                           558, 1727, ## SPD and CDU
                           1629, 1605, ## Danish SDs and Venstre
                           902, 645, ## PSOE and PP
                           1556, 773, ## Labour and Conservatives
                           1409, 742, 235, ## VVD, PvdA, and CDA
                           657, 904 ## SAP and Moderates
                           ) &
           prop_eu_mention > events$prop_eu_mention[2]) 
```

    ## Rows: 4208 Columns: 61
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (7): country, name_short, name, name_english, tri, family_name, family_2
    ## dbl  (53): parlgov_id, count, eu_count, prop_eu_mention, count_support, prop...
    ## date  (1): tri_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
tri_position_2 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_3 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

tri_position_polled_2 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_polled_3 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

tri_position_both_2 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_both_3 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

modelsummary(list(tri_position_2, tri_position_3,
                  tri_position_polled_2, tri_position_polled_3,
                  tri_position_both_2, tri_position_both_3), vcov = "robust", 
             stars = TRUE,
             coef_omit = "parlgov_id|country",
             coef_map = c(
               "(Intercept)" = "Constant",
               "date_numeric" = "Time",
               "lag_net_position" = "Lag Net Position",
               "time_to_next" = "Time to next election",
               "time_to_next_ep" = "Time to next EP election",
               "log_time_to_next" = "Log time to next election",
               "log_time_to_next_ep" = "Log time to next EP election",
               "vote_share" = "Vote share",
               "change_voteshare" = "Change in vote share",
               "rrp_lag" = "RRP Vote Share",
               "lag_rrp_polled_voteshare" = "RRP Polled Vote Share",
               "cabinet_party" = "Cabinet Party",
               "rrp_in_parliament" = "RRP in Parliament",
               "lag_eu_public_opinion" = "Lag EU Public Opinion"
             ),
             gof_map = tibble::tribble(~raw, ~clean, ~fmt,
                                       "nobs", "N", 0,
                                       "r.squared", "R$^2$", 2,
                                       "adj.r.squared", "Adj. R$^2$", 2),
             add_rows = tibble::tribble(
               ~term, ~tri_position_2, ~tri_position_3,
               ~tri_position_polled_2, ~tri_position_polled_3,
               ~tri_position_both_2, ~tri_position_both_3,
               "Party FE", "\\cmark", "\\cmark", "\\cmark", "\\cmark", "\\cmark", "\\cmark",
             ),
             output = "latex"
             )
```

<table style="width:97%;">
<colgroup>
<col style="width: 18%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 12%" />
</colgroup>
<thead>
<tr>
<th></th>
<th><ol type="1">
<li></li>
</ol></th>
<th><ol start="2" type="1">
<li></li>
</ol></th>
<th><ol start="3" type="1">
<li></li>
</ol></th>
<th><ol start="4" type="1">
<li></li>
</ol></th>
<th><ol start="5" type="1">
<li></li>
</ol></th>
<th><ol start="6" type="1">
<li></li>
</ol></th>
</tr>
</thead>
<tbody>
<tr>
<td>Constant</td>
<td></td>
<td>+</td>
<td></td>
<td>**</td>
<td></td>
<td>**</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time</td>
<td>**</td>
<td>*</td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag Net Position</td>
<td>*</td>
<td>*</td>
<td>*</td>
<td>*</td>
<td>*</td>
<td>*</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time to next election</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time to next EP election</td>
<td>**</td>
<td>*</td>
<td>*</td>
<td></td>
<td>*</td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Vote share</td>
<td>*</td>
<td>*</td>
<td>+</td>
<td></td>
<td>+</td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Change in vote share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP Vote Share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP Polled Vote Share</td>
<td></td>
<td></td>
<td>**</td>
<td>***</td>
<td>**</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Cabinet Party</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP in Parliament</td>
<td>**</td>
<td>*</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag EU Public Opinion</td>
<td></td>
<td>*</td>
<td></td>
<td>**</td>
<td></td>
<td>*</td>
</tr>
<tr>
<td></td>
<td></td>
<td>()</td>
<td></td>
<td>()</td>
<td></td>
<td>()</td>
</tr>
<tr>
<td>N</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>R$2$</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Adj. R$2$</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Party FE</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="7"><ul>
<li>p , * p , ** p , *** p |</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>

``` r
reg_data_tri <- read_csv("reg_data_tri.csv") |>
  mutate(parlgov_id = as.factor(parlgov_id),
         log_time_to_next = log(time_to_next + 1),
         log_time_to_next_ep = log(time_to_next_ep + 1)) |>
  filter(!is.na(family_name)) |>
  filter(parlgov_id %in% c(1013, 973, ## OVP and SPO
                           558, 1727, ## SPD and CDU
                           1629, 1605, ## Danish SDs and Venstre
                           902, 645, ## PSOE and PP
                           1556, 773, ## Labour and Conservatives
                           1409, 742, 235, ## VVD, PvdA, and CDA
                           657, 904 ## SAP and Moderates
                           ) &
           prop_eu_mention > events$prop_eu_mention[3]) 
```

    ## Rows: 4208 Columns: 61
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (7): country, name_short, name, name_english, tri, family_name, family_2
    ## dbl  (53): parlgov_id, count, eu_count, prop_eu_mention, count_support, prop...
    ## date  (1): tri_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
tri_position_2 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_3 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

tri_position_polled_2 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_polled_3 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

tri_position_both_2 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_both_3 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

modelsummary(list(tri_position_2, tri_position_3,
                  tri_position_polled_2, tri_position_polled_3,
                  tri_position_both_2, tri_position_both_3), vcov = "robust", 
             stars = TRUE,
             coef_omit = "parlgov_id|country",
             coef_map = c(
               "(Intercept)" = "Constant",
               "date_numeric" = "Time",
               "lag_net_position" = "Lag Net Position",
               "time_to_next" = "Time to next election",
               "time_to_next_ep" = "Time to next EP election",
               "log_time_to_next" = "Log time to next election",
               "log_time_to_next_ep" = "Log time to next EP election",
               "vote_share" = "Vote share",
               "change_voteshare" = "Change in vote share",
               "rrp_lag" = "RRP Vote Share",
               "lag_rrp_polled_voteshare" = "RRP Polled Vote Share",
               "cabinet_party" = "Cabinet Party",
               "rrp_in_parliament" = "RRP in Parliament",
               "lag_eu_public_opinion" = "Lag EU Public Opinion"
             ),
             gof_map = tibble::tribble(~raw, ~clean, ~fmt,
                                       "nobs", "N", 0,
                                       "r.squared", "R$^2$", 2,
                                       "adj.r.squared", "Adj. R$^2$", 2),
             add_rows = tibble::tribble(
               ~term, ~tri_position_2, ~tri_position_3,
               ~tri_position_polled_2, ~tri_position_polled_3,
               ~tri_position_both_2, ~tri_position_both_3,
               "Party FE", "\\cmark", "\\cmark", "\\cmark", "\\cmark", "\\cmark", "\\cmark",
             ),
             output = "latex"
             )
```

<table style="width:97%;">
<colgroup>
<col style="width: 18%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 12%" />
</colgroup>
<thead>
<tr>
<th></th>
<th><ol type="1">
<li></li>
</ol></th>
<th><ol start="2" type="1">
<li></li>
</ol></th>
<th><ol start="3" type="1">
<li></li>
</ol></th>
<th><ol start="4" type="1">
<li></li>
</ol></th>
<th><ol start="5" type="1">
<li></li>
</ol></th>
<th><ol start="6" type="1">
<li></li>
</ol></th>
</tr>
</thead>
<tbody>
<tr>
<td>Constant</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time</td>
<td>***</td>
<td>**</td>
<td>+</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag Net Position</td>
<td>*</td>
<td>*</td>
<td>*</td>
<td>*</td>
<td>*</td>
<td>*</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time to next election</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time to next EP election</td>
<td>*</td>
<td>*</td>
<td>+</td>
<td></td>
<td>+</td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Vote share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Change in vote share</td>
<td>+</td>
<td>+</td>
<td>+</td>
<td>+</td>
<td>+</td>
<td>+</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP Vote Share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP Polled Vote Share</td>
<td></td>
<td></td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Cabinet Party</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP in Parliament</td>
<td>*</td>
<td>*</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag EU Public Opinion</td>
<td></td>
<td></td>
<td></td>
<td>*</td>
<td></td>
<td>*</td>
</tr>
<tr>
<td></td>
<td></td>
<td>()</td>
<td></td>
<td>()</td>
<td></td>
<td>()</td>
</tr>
<tr>
<td>N</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>R$2$</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Adj. R$2$</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Party FE</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="7"><ul>
<li>p , * p , ** p , *** p |</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>

# Salience

``` r
reg_data_tri <- read_csv("reg_data_tri.csv") |>
  #filter(!is.na(family_name)) |>
  #filter(parlgov_id %in% unique(reg_data_tri_og$parlgov_id) #| parlgov_id == 659
        # ) |>
  mutate(parlgov_id = as.factor(parlgov_id),
         log_time_to_next = log(time_to_next + 1),
         log_time_to_next_ep = log(time_to_next_ep + 1),
         prop_eu_mention = 100*prop_eu_mention,
         lag_prop_eu_mention = 100*lag_prop_eu_mention) |> 
  filter(
    parlgov_id %in% c(1013, 973,
                      1629, 211, 590, 1324, 1331, 1605,
                      1727, 558, 543, 772,
                      235, 742, 1409, 345, 1206,
                      902, 645,
                      904, 1461, 892, 657, 282,
                      773, 1556, 659)
  )
```

    ## Rows: 4208 Columns: 61
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (7): country, name_short, name, name_english, tri, family_name, family_2
    ## dbl  (53): parlgov_id, count, eu_count, prop_eu_mention, count_support, prop...
    ## date  (1): tri_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
tri_position_2 <- lm(data = reg_data_tri,
             prop_eu_mention ~ date_numeric + 
               lag_prop_eu_mention + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_3 <- lm(data = reg_data_tri,
             prop_eu_mention ~ date_numeric + 
               lag_prop_eu_mention + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

tri_position_4 <- lm(data = reg_data_tri,
             prop_eu_mention ~ date_numeric + 
               lag_prop_eu_mention + 
               log_time_to_next + log_time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion + rrp_lag
         )

tri_position_5 <- lm(data = reg_data_tri,
             prop_eu_mention ~ date_numeric + 
               lag_prop_eu_mention + 
               log_time_to_next + log_time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion + log_time_to_next*rrp_lag
         )

tri_position_6 <- lm(data = reg_data_tri,
             prop_eu_mention ~ date_numeric + 
               lag_prop_eu_mention +
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               time_to_next*rrp_lag + parlgov_id + lag_eu_public_opinion 
         )

tri_position_polled_2 <- lm(data = reg_data_tri,
             prop_eu_mention ~ date_numeric + 
               lag_prop_eu_mention + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_polled_3 <- lm(data = reg_data_tri,
             prop_eu_mention ~ date_numeric + 
               lag_prop_eu_mention + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

tri_position_polled_4 <- lm(data = reg_data_tri,
             prop_eu_mention ~ date_numeric + 
               lag_prop_eu_mention + 
               log_time_to_next + log_time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion + lag_rrp_polled_voteshare
         )

tri_position_polled_5 <- lm(data = reg_data_tri,
             prop_eu_mention ~ date_numeric + 
               lag_prop_eu_mention + 
               log_time_to_next + log_time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion + log_time_to_next*lag_rrp_polled_voteshare
         )

tri_position_polled_6 <- lm(data = reg_data_tri,
             prop_eu_mention ~ date_numeric + 
               lag_prop_eu_mention +
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               time_to_next*lag_rrp_polled_voteshare + parlgov_id + lag_eu_public_opinion 
         )

# tri_position_both_2 <- lm(data = reg_data_tri,
#              prop_eu_mention ~ date_numeric + 
#                lag_prop_eu_mention + 
#                time_to_next + time_to_next_ep + vote_share + 
#                change_voteshare + rrp_lag + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
#                parlgov_id)
# 
# tri_position_both_3 <- lm(data = reg_data_tri,
#              prop_eu_mention ~ date_numeric + 
#                lag_prop_eu_mention + 
#                time_to_next + time_to_next_ep + vote_share + 
#                change_voteshare + rrp_lag + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
#                parlgov_id + lag_eu_public_opinion)

modelsummary(list(tri_position_3, tri_position_4,
                  tri_position_5, tri_position_6,
                  tri_position_polled_3, tri_position_polled_4,
                  tri_position_polled_5, tri_position_polled_6), vcov = "robust", 
             stars = TRUE,
             coef_omit = "parlgov_id|country",
             coef_map = c(
               "(Intercept)" = "Constant",
               "date_numeric" = "Time",
               "lag_prop_eu_mention" = "Lag Share of EU Sentences",
               "time_to_next" = "Time to next election",
               "time_to_next_ep" = "Time to next EP election",
               "log_time_to_next" = "Log time to next election",
               "log_time_to_next_ep" = "Log time to next EP election",
               "vote_share" = "Vote share",
               "change_voteshare" = "Change in vote share",
               "rrp_lag" = "RRP Vote Share",
               "lag_rrp_polled_voteshare" = "RRP Polled Vote Share",
               "cabinet_party" = "Cabinet Party",
               "rrp_in_parliament" = "RRP in Parliament",
               "lag_eu_public_opinion" = "Lag EU Public Opinion",
               "log_time_to_next:rrp_lag" = "Log time to next election * RRP vote share",
               "time_to_next:rrp_lag" = "Time to next election * RRP vote share",
               "log_time_to_next:lag_rrp_polled_voteshare" = "Log time to next election * RRP Polled vote share",
               "time_to_next:lag_rrp_polled_voteshare" = "Time to next election * RRP Polled vote share"
             ),
             gof_map = tibble::tribble(~raw, ~clean, ~fmt,
                                       "nobs", "N", 0,
                                       "r.squared", "R$^2$", 2,
                                       "adj.r.squared", "Adj. R$^2$", 2),
             add_rows = tibble::tribble(
               ~term, ~tri_position_3,
               ~tri_position_4, ~tri_position_5, ~tri_position_6,
               ~tri_position_polled_3,
               ~tri_position_polled_4, ~tri_position_polled_5, ~tri_position_polled_6,
               "Party FE", 
               "\\cmark", "\\cmark", "\\cmark", 
               "\\cmark", "\\cmark", 
               "\\cmark", "\\cmark", "\\cmark",
             ),
             output = "latex"
             )
```

<table style="width:97%;">
<colgroup>
<col style="width: 20%" />
<col style="width: 9%" />
<col style="width: 9%" />
<col style="width: 9%" />
<col style="width: 9%" />
<col style="width: 9%" />
<col style="width: 9%" />
<col style="width: 9%" />
<col style="width: 9%" />
</colgroup>
<thead>
<tr>
<th></th>
<th><ol type="1">
<li></li>
</ol></th>
<th><ol start="2" type="1">
<li></li>
</ol></th>
<th><ol start="3" type="1">
<li></li>
</ol></th>
<th><ol start="4" type="1">
<li></li>
</ol></th>
<th><ol start="5" type="1">
<li></li>
</ol></th>
<th><ol start="6" type="1">
<li></li>
</ol></th>
<th><ol start="7" type="1">
<li></li>
</ol></th>
<th><ol start="8" type="1">
<li></li>
</ol></th>
</tr>
</thead>
<tbody>
<tr>
<td>Constant</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag Share of EU Sentences</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time to next election</td>
<td></td>
<td></td>
<td></td>
<td>*</td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
<td></td>
<td></td>
<td>()</td>
</tr>
<tr>
<td>Time to next EP election</td>
<td>***</td>
<td></td>
<td></td>
<td>***</td>
<td>***</td>
<td></td>
<td></td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
<td></td>
<td></td>
<td>()</td>
</tr>
<tr>
<td>Log time to next election</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
<td></td>
</tr>
<tr>
<td>Log time to next EP election</td>
<td></td>
<td>***</td>
<td>***</td>
<td></td>
<td></td>
<td>***</td>
<td>***</td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
<td></td>
</tr>
<tr>
<td>Vote share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Change in vote share</td>
<td>**</td>
<td>**</td>
<td>**</td>
<td>**</td>
<td>*</td>
<td>*</td>
<td>*</td>
<td>*</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP Vote Share</td>
<td>*</td>
<td>*</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>RRP Polled Vote Share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Cabinet Party</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP in Parliament</td>
<td>**</td>
<td>**</td>
<td>**</td>
<td>**</td>
<td>**</td>
<td>**</td>
<td>**</td>
<td>**</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag EU Public Opinion</td>
<td>**</td>
<td>**</td>
<td>**</td>
<td>**</td>
<td>**</td>
<td>**</td>
<td>**</td>
<td>**</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Log time to next election * RRP vote share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Time to next election * RRP vote share</td>
<td></td>
<td></td>
<td></td>
<td>*</td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Log time to next election * RRP Polled vote share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td></td>
</tr>
<tr>
<td>Time to next election * RRP Polled vote share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td>+</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td>()</td>
</tr>
<tr>
<td>N</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>R$2$</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Adj. R$2$</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Party FE</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="9"><ul>
<li>p , * p , ** p , *** p |</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>

``` r
plot_log_time <- plot_slopes(tri_position_polled_5, 
            variables = "lag_rrp_polled_voteshare", vcov = "robust",
            condition = "log_time_to_next", draw = F) |>
  bind_rows(plot_slopes(tri_position_5, 
            variables = "rrp_lag", vcov = "robust",
            condition = "log_time_to_next", draw = F)) |>
  mutate(time_to_next = expm1(log_time_to_next)) |> 
  ggplot(aes(x = time_to_next, 
             y = estimate, ymin = conf.low, ymax = conf.high,
             colour = term,
             fill = term)) +
  geom_ribbon(alpha = 0.2, colour = NA) +
  geom_line() + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_color_brewer(name = "", palette = "Set1", labels = c("Polled RRP Vote Share", "Previous RRP Vote Share")) +
  scale_fill_brewer(name = "", palette = "Set1", labels = c("Polled RRP Vote Share", "Previous RRP Vote Share")) +
  labs(x = "Time to Next Election",
       y = "Marginal Effect",
       title = "Marginal Effect of RRP Support on Mainstream Parties' EU Salience by Time to Next Election",
       subtitle = "Models originally estimated with log time to next election")

plot_time <- plot_slopes(tri_position_6, variables = "rrp_lag", vcov = "robust",
            condition = "time_to_next", draw = F) |>
  bind_rows(plot_slopes(tri_position_polled_6, variables = "lag_rrp_polled_voteshare", vcov = "robust",
            condition = "time_to_next", draw = F)) |>
  ggplot(aes(x = time_to_next, 
             y = estimate, ymin = conf.low, ymax = conf.high,
             colour = term,
             fill = term)) +
  geom_ribbon(alpha = 0.2, colour = NA) +
  geom_line() + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_color_brewer(name = "", palette = "Set1", labels = c("Polled RRP Vote Share", "Previous RRP Vote Share")) +
  scale_fill_brewer(name = "", palette = "Set1", labels = c("Polled RRP Vote Share", "Previous RRP Vote Share")) +
  labs(x = "Time to Next Election",
       y = "Marginal Effect",
       title = "Marginal Effect of RRP Support on Mainstream Parties' EU Salience by Time to Next Election",
       subtitle = "Model originally estimated with linear time to next elections")

cowplot::plot_grid(plot_time, plot_log_time,
                   labels = c("A", "B"),
                   nrow = 2)
```

![](regression_final_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
ggsave("marginal_effects_salience_time.png", device = "png", width = 9.5, height = 12)
```

# Extended Sample

``` r
reg_data_tri <- read_csv("reg_data_tri.csv") |>
  mutate(parlgov_id = as.factor(parlgov_id),
         log_time_to_next = log(time_to_next + 1),
         log_time_to_next_ep = log(time_to_next_ep + 1)) |>
    filter(parlgov_id %in% c(1013, 973, ## OVP and SPO
                           558, 1727, ## SPD and CDU
                           1629, 1605, ## Danish SDs and Venstre
                           902, 645, ## PSOE and PP
                           1556, 773, ## Labour and Conservatives
                           1409, 742, 235, ## VVD, PvdA, and CDA
                           657, 904 ## SAP and Moderates
                           ))
```

    ## Rows: 4208 Columns: 61
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (7): country, name_short, name, name_english, tri, family_name, family_2
    ## dbl  (53): parlgov_id, count, eu_count, prop_eu_mention, count_support, prop...
    ## date  (1): tri_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
reg_data_tri_extended <- read_csv("reg_data_tri.csv") |>
  #filter(!is.na(family_name)) |>
  #filter(parlgov_id %in% unique(reg_data_tri_og$parlgov_id) #| parlgov_id == 659
        # ) |>
  mutate(parlgov_id = as.factor(parlgov_id),
         log_time_to_next = log(time_to_next + 1),
         log_time_to_next_ep = log(time_to_next_ep + 1)) |>
  mutate(isLib = case_when(family_name == "Lib" | parlgov_id == 1461 ~ 1,
                           family_name %in% c("Con/CD", "SocDem") ~ 0,
                           .default = NA))
```

    ## Rows: 4208 Columns: 61
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (7): country, name_short, name, name_english, tri, family_name, family_2
    ## dbl  (53): parlgov_id, count, eu_count, prop_eu_mention, count_support, prop...
    ## date  (1): tri_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
reg_data_tri_extended |> 
  mutate(is_extended = case_when(
    parlgov_id %in% unique(reg_data_tri$parlgov_id) ~ FALSE, 
    !(parlgov_id %in% unique(reg_data_tri$parlgov_id)) ~ TRUE),
    family = case_when(
      is_extended == FALSE ~ "Original Sample",
      is_extended == TRUE &  parlgov_id %in% c(1013, 973,
                      1629, 211, 590, 1324, 1331, 1605,
                      1727, 558, 543, 772,
                      235, 742, 1409, 345, 1206,
                      902, 645,
                      904, 1461, 892, 657, 282,
                      773, 1556, 659) ~ "In Meijers' (2017) Sample and Non-Radical",
      parlgov_id %in% c(50, 1536, 306, 756) ~ "In Meijers' (2017) Sample and Radical",
      is_extended == TRUE & !( parlgov_id %in% c(1013, 973,
                      1629, 211, 590, 1324, 1331, 1605,
                      1727, 558, 543, 772,
                      235, 742, 1409, 345, 1206,
                      902, 645,
                      904, 1461, 892, 657, 282,
                      773, 1556, 659)) & !is.na(family_name) ~ "Centre-right/-left party in neither the original sample nor Meijers' (2017)",
      is_extended == TRUE & is.na(family_name) ~ "Other party in neither the original sample nor Meijers' (2017)") |> 
      as.factor() |>
      relevel("Original Sample")
    ) |> 
  group_by(parlgov_id) |> 
  mutate(count_mean = mean(count, na.rm = TRUE)) |> 
  ungroup() |>
  filter(!is.na(name_short)) |>
  ggplot(aes(x = reorder(name_short, count_mean), 
             y = count, 
             colour = family,
             fill = family)) + 
  geom_boxplot(alpha = 0.2) + 
  scale_color_manual(values = c("#4daf4a", "#377eb8", #"#20496B", 
                               "#142D42",
                               "#ff7f00", "#B83C37"),
                     name = "") +
  scale_fill_manual(values = c("#4daf4a", "#377eb8", #"#20496B", 
                               "#142D42",
                               "#ff7f00", "#B83C37"),
                    name = "") +
  facet_wrap(~country, scales = "free") +
  coord_flip() +
  labs(x = "", y = "Number of Sentences", title = "Number of Sentences by Party and Country", subtitle = "Y axis scale varies to show intra-country variation.") +
  guides(
  colour = guide_legend(nrow = 3, byrow = TRUE),
  fill   = guide_legend(nrow = 3, byrow = TRUE)
)
```

![](regression_final_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

``` r
#ggsave("sentence_counts.png", device = "png", width = 9, height = 10)
```

``` r
reg_data_tri_extended <- read_csv("reg_data_tri.csv") |>
  #filter(!is.na(family_name)) |>
  #filter(parlgov_id %in% unique(reg_data_tri_og$parlgov_id) #| parlgov_id == 659
        # ) |>
  mutate(parlgov_id = as.factor(parlgov_id),
         log_time_to_next = log(time_to_next + 1),
         log_time_to_next_ep = log(time_to_next_ep + 1)) |> 
  filter(
    parlgov_id %in% c(1013, 973,
                      1629, 211, 590, 1324, 1331, 1605,
                      1727, 558, 543, 772,
                      235, 742, 1409, 345, 1206,
                      902, 645,
                      904, 1461, 892, 657, 282,
                      773, 1556, 659) |
      !is.na(family_name)
  )
```

    ## Rows: 4208 Columns: 61
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (7): country, name_short, name, name_english, tri, family_name, family_2
    ## dbl  (53): parlgov_id, count, eu_count, prop_eu_mention, count_support, prop...
    ## date  (1): tri_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
reg_data_tri_extended |>
   mutate(is_extended = case_when(
    parlgov_id %in% unique(reg_data_tri$parlgov_id) ~ FALSE, 
    !(parlgov_id %in% unique(reg_data_tri$parlgov_id)) ~ TRUE)) |>
  ggplot(aes(x = tri_date, y = net_position, colour = is_extended, group = parlgov_id)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c("#377eb8", "#ff7f00"), 
                     name = "", 
                     labels = c("FALSE" = "Original Sample", "TRUE" = "Extended Sample")) +
  facet_wrap(~country) +
  labs(x = "Date", y = "Net EU Position", title = "Net EU Positions of Parties in Original and Extended Sample")
```

    ## Warning: Removed 7 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](regression_final_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

``` r
#ggsave("sentence_counts_ts.png", device = "png", width = 12, height = 10)
```

``` r
reg_data_tri <- read_csv("reg_data_tri.csv") |>
  #filter(!is.na(family_name)) |>
  #filter(parlgov_id %in% unique(reg_data_tri_og$parlgov_id) #| parlgov_id == 659
        # ) |>
  mutate(parlgov_id = as.factor(parlgov_id),
         log_time_to_next = log(time_to_next + 1),
         log_time_to_next_ep = log(time_to_next_ep + 1)) |> 
  filter(
    parlgov_id %in% c(1013, 973,
                      1629, 211, 590, 1324, 1331, 1605,
                      1727, 558, 543, 772,
                      235, 742, 1409, 345, 1206,
                      902, 645,
                      904, 1461, 892, 657, 282,
                      773, 1556, 659) |
      !is.na(family_name)
  )
```

    ## Rows: 4208 Columns: 61
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (7): country, name_short, name, name_english, tri, family_name, family_2
    ## dbl  (53): parlgov_id, count, eu_count, prop_eu_mention, count_support, prop...
    ## date  (1): tri_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
tri_position_2 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_3 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

tri_position_polled_2 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_polled_3 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

tri_position_both_2 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id)

tri_position_both_3 <- lm(data = reg_data_tri,
             net_position ~ date_numeric + 
               lag_net_position + 
               time_to_next + time_to_next_ep + vote_share + 
               change_voteshare + rrp_lag + lag_rrp_polled_voteshare + cabinet_party + rrp_in_parliament +
               parlgov_id + lag_eu_public_opinion)

modelsummary(list(tri_position_2, tri_position_3,
                  tri_position_polled_2, tri_position_polled_3,
                  tri_position_both_2, tri_position_both_3), vcov = "robust", 
             stars = TRUE,
             coef_omit = "parlgov_id|country",
             coef_map = c(
               "(Intercept)" = "Constant",
               "date_numeric" = "Time",
               "lag_net_position" = "Lag Net Position",
               "time_to_next" = "Time to next election",
               "time_to_next_ep" = "Time to next EP election",
               "log_time_to_next" = "Log time to next election",
               "log_time_to_next_ep" = "Log time to next EP election",
               "vote_share" = "Vote share",
               "change_voteshare" = "Change in vote share",
               "rrp_lag" = "RRP Vote Share",
               "lag_rrp_polled_voteshare" = "RRP Polled Vote Share",
               "cabinet_party" = "Cabinet Party",
               "rrp_in_parliament" = "RRP in Parliament",
               "lag_eu_public_opinion" = "Lag EU Public Opinion"
             ),
             gof_map = tibble::tribble(~raw, ~clean, ~fmt,
                                       "nobs", "N", 0,
                                       "r.squared", "R$^2$", 2,
                                       "adj.r.squared", "Adj. R$^2$", 2),
             add_rows = tibble::tribble(
               ~term, ~tri_position_2, ~tri_position_3,
               ~tri_position_polled_2, ~tri_position_polled_3,
               ~tri_position_both_2, ~tri_position_both_3,
               "Party FE", "\\cmark", "\\cmark", "\\cmark", "\\cmark", "\\cmark", "\\cmark",
             ),
             #output = "table2.tex",
             output = "latex"
             )
```

<table style="width:97%;">
<colgroup>
<col style="width: 18%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 12%" />
</colgroup>
<thead>
<tr>
<th></th>
<th><ol type="1">
<li></li>
</ol></th>
<th><ol start="2" type="1">
<li></li>
</ol></th>
<th><ol start="3" type="1">
<li></li>
</ol></th>
<th><ol start="4" type="1">
<li></li>
</ol></th>
<th><ol start="5" type="1">
<li></li>
</ol></th>
<th><ol start="6" type="1">
<li></li>
</ol></th>
</tr>
</thead>
<tbody>
<tr>
<td>Constant</td>
<td>+</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time</td>
<td>***</td>
<td>**</td>
<td>*</td>
<td>+</td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag Net Position</td>
<td>**</td>
<td>**</td>
<td>**</td>
<td>**</td>
<td>**</td>
<td>**</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time to next election</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Time to next EP election</td>
<td>+</td>
<td>+</td>
<td>+</td>
<td>+</td>
<td>+</td>
<td>+</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Vote share</td>
<td>*</td>
<td>*</td>
<td>*</td>
<td>*</td>
<td>*</td>
<td>*</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Change in vote share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP Vote Share</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td>+</td>
<td>+</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP Polled Vote Share</td>
<td></td>
<td></td>
<td>+</td>
<td>+</td>
<td>*</td>
<td>*</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Cabinet Party</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>RRP in Parliament</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
<td>***</td>
</tr>
<tr>
<td></td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
<td>()</td>
</tr>
<tr>
<td>Lag EU Public Opinion</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td>()</td>
<td></td>
<td>()</td>
<td></td>
<td>()</td>
</tr>
<tr>
<td>N</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>R$2$</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Adj. R$2$</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Party FE</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
<td>cmark</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="7"><ul>
<li>p , * p , ** p , *** p |</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>
