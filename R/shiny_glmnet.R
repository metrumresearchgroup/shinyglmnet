#' @title Shiny application for glmnet
#' @description Interactive application that runs the glmnet package functions
#' @param data list, containing y and x matrices to run glmnet on, Default: NULL
#' @param family character, Response type
#' @return NULL
#' @details if data is null then sample data is used
#' @examples
#' \dontrun{
#' if(interactive()){
#' shiny_glmnet()
#'  }
#' }
#' @rdname shiny_glmnet
#' @export
#' @import shiny
#' @importFrom shinyWidgets dropdownButton switchInput tooltipOptions
#' @importFrom purrr map map_df set_names
#' @importFrom glmnet cv.glmnet glmnet
#' @importFrom broom tidy glance
#' @importFrom shinyCanvas renderCanvas canvas canvasOutput
#' @importFrom jsonlite fromJSON
#' @import dplyr
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon scale_x_log10 geom_vline labs geom_hline facet_wrap
#' @importFrom shinydashboard dashboardHeader dashboardSidebar dashboardBody tabBox dashboardPage
shiny_glmnet <- function(data = NULL , family = 'gaussian'){
# server ----

server <- function(input, output, session) {

  ENV <- new.env()

  if(is.null(data)){
    observeEvent(input$data,{
      load(system.file(sprintf("data/%sExample.RData",input$data),package = 'glmnet'),envir = ENV)
    })
  }else{
    ENV$x <- data$x
    ENV$y <- data$y
  }


  shiny::observeEvent(input$data,{
    output$dropdown <- shiny::renderUI({

      sliders <-  lapply(1:ncol(ENV$x),function(x){
        shiny::sliderInput(inputId = sprintf('slider_%02d',x),
                           label = sprintf('Column %02d',x),
                           min = 0,
                           max = 10,
                           value = 1)
      })

      shinyWidgets::dropdownButton(tags$h3("Penalty Factor"),
                                   up = TRUE,
                                   label = 'Individual Covariate Penalty',
                                   sliders,
                                   circle = FALSE,
                                   width = "300px",
                                   tooltip = shinyWidgets::tooltipOptions(title = "Click to set Penalty Factors")
      )
    })
  })

  fit       <- shiny::reactive({

    pf <- rep(1,ncol(ENV$x))

    if(!is.null(input$slider_01))
        pf <- sapply(1:ncol(ENV$x),function(i) input[[sprintf('slider_%02d',i)]])

    purrr::map(penalties()$alpha,function(alpha){

      glmnet::cv.glmnet(ENV$x,
                        ENV$y,
                        alpha = alpha,
                        standardize = input$standardize,
                        standardize.response = input$standardize.response,
                        intercept = input$intercept,
                        penalty.factor = pf,
                        family=input$family)
    })
  })

  tidy_cv   <- shiny::eventReactive(penalties(),{
    ret <- fit()%>%
      purrr::map_df(broom::tidy,.id = 'alpha')

    ret$alpha <- factor(ret$alpha,labels = penalties()$alpha)

    ret
  })

  glance_cv <- eventReactive(penalties(),{
    ret <- fit()%>%
      purrr::map_df(broom::glance,.id = 'alpha')

    ret$alpha <- factor(ret$alpha,labels = penalties()$alpha)

    ret
  })

  predicted <- eventReactive(penalties(),{

    ret <- fit()%>%
      purrr::map_df(.f = function(x,newx,y){

        cbind(y,predict(x,newx))%>%
          dplyr::as_data_frame()%>%
          purrr::set_names(c('y','yhat'))

      } ,newx = ENV$x, y = ENV$y,.id = 'alpha')

    ret$alpha <- factor(ret$alpha,labels = penalties()$alpha)

    ret
  })

  output$d3 <- shinyCanvas::renderCanvas({

      shinyCanvas::canvas(obj  = data.frame(lambda=0.5,alpha=1),
                          xlim = c(0,1),
                          ylim = c(0,1),
                          showSelect = FALSE,
                          interpolate = 'none',
                          width = '100%'
                          )

  })

  network <- shiny::reactiveValues()

  shiny::observeEvent(input$d3_update,{

    network$penalties <- input$d3_update$.pointsData

      if(!is.null(network$penalties)){

        network$penalties <- network$penalties%>%
          jsonlite::fromJSON()%>%
          dplyr::as_data_frame()%>%
          purrr::set_names(c('lambda','alpha'))%>%
          dplyr::mutate(alpha = round(alpha,3))
    }

  })

  penalties_raw <- reactive({
    network$penalties
  })

  penalties <- shiny::debounce(penalties_raw,1000)

  map_lambda <- function(penalty,lambda){
    ecdf(x = lambda)(penalty)
  }

  output$fit_plot <- shiny::renderPlot({

      vline_data <- glance_cv()

      tidy_cv()%>%
        ggplot2::ggplot(ggplot2::aes(lambda, estimate)) +
        ggplot2::geom_line(ggplot2::aes(color = alpha)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high,fill = alpha), alpha = .2) +
        ggplot2::scale_x_log10() +
        ggplot2::geom_vline(ggplot2::aes(xintercept = lambda.min,colour = alpha), data = vline_data,show.legend = FALSE) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = lambda.1se,colour = alpha), data = vline_data, lty = 2,show.legend = FALSE) +
        ggplot2::labs(x='log Lambda', y = 'Estimated Deviance')

    })

  output$nz_plot <- shiny::renderPlot({

    tidy_cv()%>%
      ggplot2::ggplot(ggplot2::aes(lambda, nzero,colour=alpha)) +
      ggplot2::labs(x= 'Log Lambda',y='Nonzero Variables') +
      ggplot2::geom_line() +
      ggplot2::scale_x_log10()

  })

  output$pred_plot <- shiny::renderPlot({

    predicted()%>%
      ggplot2::ggplot(ggplot2::aes(x=y,y=yhat,colour = alpha)) +
      ggplot2::geom_point()

  })

  output$coef_plot <- shiny::renderPlot({

      t2 <- purrr::map_df(fit(),function(x) broom::tidy(x$glmnet.fit),.id = 'alpha')

      # t2$term <- gsub('\\(Intercept\\)','V0',t2$term)
      #
      # t2$term <- as.numeric(gsub('V','',t2$term))
      #
      # t2$term <- sprintf('V%02d',t2$term)

      t2$alpha <- factor(t2$alpha,labels = penalties()$alpha)

      vline_data <- glance_cv()

      lam <- penalties()
      lam$alpha <- factor(lam$alpha)

      term_labs <- t2%>%
        dplyr::group_by(alpha)%>%
        dplyr::filter(lambda==min(lambda))

      if(input$coef_type){
        t2%>%
          ggplot2::ggplot() +
          ggplot2::scale_x_log10() +
          ggplot2::geom_line(ggplot2::aes(x = lambda, y = estimate, group=term)) +
          ggplot2::geom_vline(ggplot2::aes(xintercept = lambda),linetype = 2,alpha = 0.5, data = lam) +
          ggplot2::geom_hline(ggplot2::aes(yintercept = 0 )) +
          ggplot2::geom_text(ggplot2::aes(x=lambda,y=estimate,label = term),data = term_labs,nudge_x = -0.1) +
          ggplot2::labs(x = 'log Lambda',y = 'Coefficient Estimate') +
          ggplot2::facet_wrap(~alpha)
      }else{
        t2%>%
          ggplot2::ggplot() +
          ggplot2::scale_x_log10() +
          ggplot2::geom_line(ggplot2::aes(x = lambda, y = estimate, colour=alpha)) +
          ggplot2::geom_vline(ggplot2::aes(xintercept = lambda,colour=alpha), data = lam,show.legend = FALSE) +
          ggplot2::geom_hline(ggplot2::aes(yintercept = 0 )) +
          ggplot2::labs(x = 'log Lambda',y = 'Coefficient Estimate') +
          ggplot2::facet_wrap(~term)
      }

    })

}

# UI ----
header <- shinydashboard::dashboardHeader(
    title = 'GLMNET'
  )

side <- shinydashboard::dashboardSidebar(width = 350,
  shiny::selectInput(inputId = 'data',
                     label = 'Select Example Data',
                     choices = gsub('Example(.*?)$','',list.files(system.file("data",package = 'glmnet'),pattern = 'Example')),
                     selected = 'QuickStart'),
  shiny::selectInput(inputId = 'family',label = 'Family',choices = eval(formals(glmnet::glmnet)$family),selected = family),
  shinyCanvas::canvasOutput(outputId="d3",width = 'auto'),
  shinyWidgets::switchInput(inputId = 'standardize',onLabel = 'Standardized',offLabel = 'Not Standardized',label = 'Covariates',value = FALSE),
  shinyWidgets::switchInput(inputId = 'standardize.response',onLabel = 'Standardized',offLabel = 'Not Standardized',label = 'Response',value = FALSE),
  shinyWidgets::switchInput(inputId = 'intercept',onLabel = 'Included',offLabel = 'Not Included',label = 'Intercept',value = TRUE),
  shiny::uiOutput('dropdown')
)

body <- shinydashboard::dashboardBody(
  tags$head(tags$style(HTML('
      .skin-blue .main-sidebar {
        background-color: #888888;
      }

      .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
        background-color: #444444;
      }

      .dropdown-menu {
        background-color: #444444;
        overflow-y:scroll;
        max-height: 300px;
      }

    '))),
      shiny::fluidRow(
        shinydashboard::tabBox(width = 'auto',
          shiny::tabPanel(
          title = 'Coefficient Estimates',
          shinyWidgets::switchInput(input = 'coef_type',label = 'Terms',onLabel = 'Combine',offLabel = 'Separate',value = TRUE),
          shiny::plotOutput(outputId = 'coef_plot',height = '800px')
        ),
        shiny::tabPanel(
          title = 'Crossvalidation',
          shiny::plotOutput(outputId = 'fit_plot' , height = '400px'),
          shiny::plotOutput(outputId = 'nz_plot'  , height = '400px')
      ),
      shiny::tabPanel(
        title = 'Prediction',
        shiny::plotOutput(outputId = 'pred_plot' , height = '800px')
      )
    )
  )
)

ui <- shinydashboard::dashboardPage(
  header,
  side,
  body
)

ui$attribs$style <- sprintf('%s width: auto;',ui$attribs$style)

# run gadget ----

shiny::shinyApp(ui = ui,server = server)
}
