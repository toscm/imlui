init_size_reactives <- function(input) {
  # Browser / Plot Dimensions (alphabetically)
  logsne("Initializing Browser / Plot Dimensions reactives ...")

  # Manual Plot Area Width/Height in pixels
  MPAH <- reactive(input$MPAH)
  MPAW <- reactive(input$MPAW)
  # Browser Height / Width in pixels
  BH <- reactive(if (is.null(input$dim[2])) 480 else input$dim[2])
  BW <- reactive(if (is.null(input$dim[1])) 640 else input$dim[1])
  # Plot Area Size Calculation (either "fixed" or "auto")
  PASC <- reactive(input$PASC)

  # Automatic Plot Area Width / Height in pixels
  APAH <- reactive(PAHS[max(which(PAHS < (MPH() - 120)), 1)])
  APAW <- reactive(PAWS[max(which(PAWS < (MPW() - 0)), 1)])

  # Main Panel Height (approximately) in pixels (TODO: Improve precision)
  MPH <- reactive(BH() - 42.00)
  MPW <- reactive(BW() * 0.75)

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
}
