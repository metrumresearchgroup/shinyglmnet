# shinyglmnet

Shiny application that runs `glmnet` and `cv.glmnet` and produces standard plots within an interactive framework.

## Installation

```r
remotes::install_github('metrumresearchgroup/shinyglmnet')
```

## Usage

```r
library('shinyglmnet')

#example data
  shiny_glmnet()

#your data
  shiny_glmnet(data = list(y=y,x=x))

```

![](https://ghe.metrumrg.com/metrumresearchgroup/shinyglmnet/blob/master/misc/shinyglmnet.gif?raw=true)
