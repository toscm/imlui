init_size_reactives <- function(input) {
  if (missing(input)) {
    stop("Argument `input` is missing")
  }

  # Browser / Plot Dimensions (alphabetically)
  logsne("Initializing Browser / Plot Dimensions reactives ...")

  # Manual Plot Area Width / Height in pixels
  MPAW <- reactive(input$MPAW)
  MPAH <- reactive(input$MPAH)
  # Browser Width / Height in pixels
  BW <- reactive(if (is.null(input$dim[1])) 640 else input$dim[1])
  BH <- reactive(if (is.null(input$dim[2])) 480 else input$dim[2])
  # Plot Area Size Calculation (either "Fixed" or "Auto")
  PASC <- reactive(input$PASC)

  # Main Panel Width / Height (approximately) in pixels TODO: Improve precision
  MPW <- reactive(BW() * 0.75)
  MPH <- reactive(BH() - 42.0)

  # Automatic Plot Area Width / Height in pixels
  APAW <- reactive(PAWS[max(which(PAWS < (MPW() - 0)), 1)])
  APAH <- reactive(PAHS[max(which(PAHS < (MPH() - 120)), 1)])

  # Plot Area Widht/Height (approximately) in pixels
  PAH <- reactive({
    if (PASC() == "Fixed" && !(is.null(MPAH()))) MPAH() else APAH()
  })
  PAW <- reactive({
    if (PASC() == "Fixed" && !(is.null(MPAW()))) MPAW() else APAW()
  })

  # Plot Area Widht/Height (approximately) in pixels as string
  PAWPX <- reactive(paste0(PAW(), "px"))
  PAHPX0 <- reactive(paste0(PAH() - 0, "px"))
  PAHPX1 <- reactive(paste0(PAH() - 120, "px"))
  PAHPX1_05 <- reactive(paste0((PAH() - 120) * 0.5, "px"))
  PAHPX2 <- reactive(paste0(PAH() - 240, "px"))
  PAHPX3 <- reactive(paste0(PAH() - 360, "px"))

  return(function_locals())
}
