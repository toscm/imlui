model_analysis_page <- function(data) {
  tabsetPanel(
    id="MA_T_CS",
    tabPanel("Descriptions",
      verbatimTextOutput(outputId="MA_MD_TO")
    ),
    tabPanel("Predictions",
      uiOutput(outputId="MA_MP_T"),
      if (length(data$r$model$symbol_list()) == 0) {
        div(HTML("Please choose a dataset and model first"))
      } else if (length(DD()) == 0) {
        div(HTML("Please choose a dataset and model first"))
      } else {
        div(
          fluidRow(column(12, withSpinner(plotOutput(outputId="PHP")))),
          fluidRow(
              column(2, checkboxInput(inputId="PHP_R", label="Rug", value=TRUE)),
              column(2, checkboxInput(inputId="PHP_DL", label="Density", value=TRUE)),
              column(3, textInput(inputId="PHP_BW", label="Binwidth", value="0.1"))
          )
        )
      }
    ),
    tabPanel("Survival Curves",
      div(
        fluidRow(column(12, withSpinner(plotOutput(outputId="SCP")))),
        fluidRow(column(3, selectInput(inputId="survPlotM", label="Model", choices=model$symbols())))
      )
    ),
    tabPanel("Feature Effects",
      if (length(data$r$model$symbol_list()) < 1) {
        div(HTML("Please choose a model first"))
      } else if (length(data$r$model$symbol_list()) > 1) {
        div(HTML("Usage of multiple models is not yet implemented"))
      } else {
        if (length(DD()) < 1) {
          div(HTML("Please choose a dataset first"))
        } else {
          list(
            fluidRow(
              column(12, withSpinner(plotOutput(outputId="FEP")))
            ),
            fluidRow(
              column(2, checkboxInput(inputId="FEP_DBG", label="Debug")),
              column(4, sliderInput(inputId="FEP_N", label="N Samples", value=0, min=0, max=100)),
              column(4, sliderInput(inputId="FEP_T", label="Threshold", value=0.991, min=-3, max=3, step=0.01))
            ),
            fluidRow(
              column(2, selectInput(inputId="FEP_D", label="Dataset", choices=DD())),
              column(2, selectInput(inputId="FEP_S", label="Sample", choices=SS()[[input$FEP_D]])),
              column(2, selectInput(inputId="FEP_F", label="Feature", choices=names(data$r$model$params_list()()[[1]]))),
              {
                d <- input$FEP_D # dataset
                s <- input$FEP_S # sample
                f <- input$FEP_F # feature
                if ( none( list(d,f,s), is.null )) {
                  X <- df[[d]]()
                  cn <- colnames(X)
                  rn <- rownames(X)
                  if ( (!is.null(cn)) && (!is.null(rn)) && (f %in% cn) && (s %in% rn) ) {
                    x_ij <- X[s, f] # value of feature f, sample s
                    x__j <- X[ , f] # value of feature f over all samples
                    step <- ceiling10(diff(range(x__j))) / 100
                    column(4, sliderInput(inputId="FEP_V", label="Value", min=min(x__j) , max=max(x__j), step=step, value=x_ij))
                  }
                }
              }
            )
          )
        }
      }
    )
  )
}
