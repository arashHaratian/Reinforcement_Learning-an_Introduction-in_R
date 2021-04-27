
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Reinforcement Learning: An Introduction, 2nd edition

<!-- badges: start -->
<!-- badges: end -->

The goal of this repo is to generate the examples and figures of Sutton
& Barto’s book [Reinforcement Learning: An Introduction (2nd
Edition)](http://incompleteideas.net/book/the-book.html) in R.

## Prerequisites and How to Use

All codes are self-contained and in order to execute any of the code you
need to install the [R](https://www.r-project.org/) and the following
packages:

-   [Tidyverse](https://github.com/tidyverse/tidyverse)
-   [reshape2](https://cran.r-project.org/web/packages/reshape2/index.html)
-   [patchwork](https://github.com/thomasp85/patchwork)
-   [plotly](https://plotly.com/) (only for 3D plot in chapter 10)

You can check [my session info](#session-info) for more detail.

You can find the code usage in `.Rmd` files or in `.R` files.

## RMarkdown Outputs

-   [**Chapter 1**](Chapter1/Chapter1.html)
-   [**Chapter 2**](Chapter2/Chapter2.html)
-   [**Chapter 3**](Chapter3/Chapter3.html)
-   [**Chapter 4**](Chapter4/Chapter4.html)
-   [**Chapter 5**](Chapter5/Chapter5.html)
-   [**Chapter 6**](Chapter6/Chapter6.html)
-   [**Chapter 7**](Chapter7/Chapter7.html)
-   [**Chapter 8**](Chapter8/Chapter8.html)
-   [**Chapter 9**](Chapter9/Chapter9.html)
-   [**Chapter 10**](Chapter10/Chapter10.html)
-   [**Chapter 11**](Chapter11/Chapter11.html)
-   [**Chapter 12**](Chapter12/Chapter12.html)
-   [**Chapter 13**](Chapter13/Chapter13.html)

## Session Info

``` r
library(tidyverse)
library(reshape2)
library(patchwork)
library(plotly)

sessioninfo::session_info()
#> - Session info ---------------------------------------------------------------
#>  setting  value                       
#>  version  R version 4.0.3 (2020-10-10)
#>  os       Windows 10 x64              
#>  system   x86_64, mingw32             
#>  ui       RTerm                       
#>  language (EN)                        
#>  collate  English_United States.1252  
#>  ctype    English_United States.1252  
#>  tz       Asia/Tehran                 
#>  date     2021-04-28                  
#> 
#> - Packages -------------------------------------------------------------------
#>  package     * version date       lib source        
#>  assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.0.3)
#>  backports     1.2.1   2020-12-09 [1] CRAN (R 4.0.3)
#>  broom         0.7.4   2021-01-29 [1] CRAN (R 4.0.3)
#>  cellranger    1.1.0   2016-07-27 [1] CRAN (R 4.0.3)
#>  cli           2.3.0   2021-01-31 [1] CRAN (R 4.0.3)
#>  colorspace    2.0-0   2020-11-11 [1] CRAN (R 4.0.3)
#>  crayon        1.4.1   2021-02-08 [1] CRAN (R 4.0.3)
#>  data.table    1.13.6  2020-12-30 [1] CRAN (R 4.0.3)
#>  DBI           1.1.1   2021-01-15 [1] CRAN (R 4.0.3)
#>  dbplyr        2.1.0   2021-02-03 [1] CRAN (R 4.0.3)
#>  digest        0.6.27  2020-10-24 [1] CRAN (R 4.0.5)
#>  dplyr       * 1.0.4   2021-02-02 [1] CRAN (R 4.0.3)
#>  ellipsis      0.3.1   2020-05-15 [1] CRAN (R 4.0.3)
#>  evaluate      0.14    2019-05-28 [1] CRAN (R 4.0.3)
#>  forcats     * 0.5.1   2021-01-27 [1] CRAN (R 4.0.3)
#>  fs            1.5.0   2020-07-31 [1] CRAN (R 4.0.3)
#>  generics      0.1.0   2020-10-31 [1] CRAN (R 4.0.3)
#>  ggplot2     * 3.3.3   2020-12-30 [1] CRAN (R 4.0.3)
#>  glue          1.4.2   2020-08-27 [1] CRAN (R 4.0.3)
#>  gtable        0.3.0   2019-03-25 [1] CRAN (R 4.0.3)
#>  haven         2.3.1   2020-06-01 [1] CRAN (R 4.0.3)
#>  hms           1.0.0   2021-01-13 [1] CRAN (R 4.0.3)
#>  htmltools     0.5.1.1 2021-01-22 [1] CRAN (R 4.0.3)
#>  htmlwidgets   1.5.3   2020-12-10 [1] CRAN (R 4.0.3)
#>  httr          1.4.2   2020-07-20 [1] CRAN (R 4.0.3)
#>  jsonlite      1.7.2   2020-12-09 [1] CRAN (R 4.0.3)
#>  knitr         1.31    2021-01-27 [1] CRAN (R 4.0.3)
#>  lazyeval      0.2.2   2019-03-15 [1] CRAN (R 4.0.3)
#>  lifecycle     0.2.0   2020-03-06 [1] CRAN (R 4.0.3)
#>  lubridate     1.7.9.2 2020-11-13 [1] CRAN (R 4.0.3)
#>  magrittr      2.0.1   2020-11-17 [1] CRAN (R 4.0.3)
#>  modelr        0.1.8   2020-05-19 [1] CRAN (R 4.0.3)
#>  munsell       0.5.0   2018-06-12 [1] CRAN (R 4.0.3)
#>  patchwork   * 1.1.1   2020-12-17 [1] CRAN (R 4.0.3)
#>  pillar        1.4.7   2020-11-20 [1] CRAN (R 4.0.3)
#>  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.0.3)
#>  plotly      * 4.9.3   2021-01-10 [1] CRAN (R 4.0.3)
#>  plyr          1.8.6   2020-03-03 [1] CRAN (R 4.0.3)
#>  purrr       * 0.3.4   2020-04-17 [1] CRAN (R 4.0.3)
#>  R6            2.5.0   2020-10-28 [1] CRAN (R 4.0.3)
#>  Rcpp          1.0.6   2021-01-15 [1] CRAN (R 4.0.3)
#>  readr       * 1.4.0   2020-10-05 [1] CRAN (R 4.0.3)
#>  readxl        1.3.1   2019-03-13 [1] CRAN (R 4.0.3)
#>  reprex        1.0.0   2021-01-27 [1] CRAN (R 4.0.3)
#>  reshape2    * 1.4.4   2020-04-09 [1] CRAN (R 4.0.3)
#>  rlang         0.4.10  2020-12-30 [1] CRAN (R 4.0.3)
#>  rmarkdown     2.6     2020-12-14 [1] CRAN (R 4.0.3)
#>  rstudioapi    0.13    2020-11-12 [1] CRAN (R 4.0.3)
#>  rvest         0.3.6   2020-07-25 [1] CRAN (R 4.0.3)
#>  scales        1.1.1   2020-05-11 [1] CRAN (R 4.0.3)
#>  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 4.0.3)
#>  stringi       1.5.3   2020-09-09 [1] CRAN (R 4.0.3)
#>  stringr     * 1.4.0   2019-02-10 [1] CRAN (R 4.0.3)
#>  tibble      * 3.0.6   2021-01-29 [1] CRAN (R 4.0.3)
#>  tidyr       * 1.1.2   2020-08-27 [1] CRAN (R 4.0.3)
#>  tidyselect    1.1.0   2020-05-11 [1] CRAN (R 4.0.3)
#>  tidyverse   * 1.3.0   2019-11-21 [1] CRAN (R 4.0.3)
#>  vctrs         0.3.6   2020-12-17 [1] CRAN (R 4.0.3)
#>  viridisLite   0.3.0   2018-02-01 [1] CRAN (R 4.0.3)
#>  withr         2.4.1   2021-01-26 [1] CRAN (R 4.0.3)
#>  xfun          0.21    2021-02-10 [1] CRAN (R 4.0.3)
#>  xml2          1.3.2   2020-04-23 [1] CRAN (R 4.0.3)
#>  yaml          2.2.1   2020-02-01 [1] CRAN (R 4.0.3)
#> 
#> [1] C:/Users/arashharatian/Documents/R/win-library/4.0
#> [2] C:/Program Files/R/R-4.0.3/library
```

## Contributing

Pull requests are welcome. For major changes, you may want to open an
issue first to discuss what you would like to change. If you want to add
some missing figures or make any improvement, you are welcomed to make a
pull request or open an issue.
