# See README/#ui-layout for an overview
imlui_ui <- function(request) {
  infomsg("Starting ui func from process ID:", Sys.getpid(), "...")
  tagList(
    tags$head(tags$style(HTML("html, body {height: 100%; margin: 0;}"))),
    fluidPage(
      title="IML-UI",
      style="min-height: calc(100vh - 50px);",
      htmltools::htmlDependency(
        name = "imlui-assets",
        version = imlui_version(),
        package = "imlui",
        src = "assets",
        script = c(
          "js/js-cookie-3.0.1/js.cookie.min.js",
          "js/window-dimensions-1.0.0/window-dimensions-1.0.0.js"
        ),
        stylesheet = c("css/styles.css")
      ), # loads js/css files from <R-Lib>/imlui/assets
      shinyjs::useShinyjs(), # loads js/css files from <R-Lib>/shinyjs/srcjs
      shinyjs::extendShinyjs(
        text = js_cookie_to_r_code(),
        functions = c("getcookie", "setcookie", "rmcookie")
      ),
      shinyjs::extendShinyjs(text = js_return_click(), functions = c()),
      # uiOutput(outputId="webapp")
      fluidPage("Helloworld Helloworld v5")
    )
  )
}
