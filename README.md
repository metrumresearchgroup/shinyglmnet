# shinyglmnet

Shiny application that runs `glmnet` and `cv.glmnet` and produces standard plots within an interactive framework.

## Installation

```r
remotes::install_github('yonicd/shinyglmnet')
```

## Usage

```r
library('shinyglmnet')

#example data
  shiny_glmnet()

#your data
list(y=y,x=x)
  shiny_glmnet(data)

```
