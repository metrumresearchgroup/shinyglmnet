# shinyglmnet

Shiny application that runs `glmnet` and `cv.glmnet` and produces standard plots within an interactive framework.

## Installation

```r
remotes::install_github('yoni/shinyglmnet',host = 'github.metrumrg.com/api/v3',auth_token = Sys.getenv('GHE_PAT'))
```

## Usage

```r
library('shinyglmnet')

#example data
  shiny_glmnet()

#your data
  shiny_glmnet(data = list(y=y,x=x))

```

![](https://github.com/yonicd/shinyglmnet/blob/master/misc/shinyglmnet.gif?raw=true)
