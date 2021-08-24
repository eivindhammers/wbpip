<!-- badges: start -->
[![R-CMD-check](https://github.com/PIP-Technical-Team/wbpip/workflows/R-CMD-check/badge.svg)](https://github.com/PIP-Technical-Team/wbpip/actions?workflow=R-CMD-check)
[![test-coverage](https://github.com/PIP-Technical-Team/wbpip/workflows/test-coverage/badge.svg)](https://github.com/PIP-Technical-Team/wbpip/actions)
[![pkgdown](https://github.com/PIP-Technical-Team/wbpip/workflows/pkgdown/badge.svg)](https://github.com/PIP-Technical-Team/wbpip/actions)
<!-- badges: end -->

# Basic end-user workflows
Basic workflows are based on single data files (micro- or grouped-data).
The assumption for these workflows is that they can be satisfied by feeding a 
single micro- / grouped-data file to different functions.

## Single stats
* `get_poverty_rate(df, povline)`
  `compute_poverty_rate(welfare, weight, povline)`
* `get_number_poor(df, povline, pop)`
* `get_poverty_gap(df, povline)`
* `get_poverty_severity(df, povline)`
* `get_watts_index(df, povline)`
* `get_mean(df)`
* `get_median(df)`
* `get_gini(df)`
* `get_polarization(df)`
* `get_mld(df)`
* `get_quantile(n = 10)`

# Multiple stats
* `get_distributional_stats()`
* `get_poverty_stats()`
