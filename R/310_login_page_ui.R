login_page <- function(data) {
  div(
    id = "login_panel",
    style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
    tags$h1("Login"),
    wellPanel(
      textInput("login_user_name", tagList(icon("user"), "Username or email")),
      passwordInput("login_password", tagList(icon("unlock-alt"), "Password")),
      div(
        style = "text-align: center;",
        actionButton(
          inputId = "login_button",
          label = "Sign in",
          class = "btn btn-primary btn-block"
        )
      ),
      shinyjs::hidden(
        div(
          id = "login_error",
          tags$p(
            "Login failed!",
            style = "color: red; font-weight: bold; padding-top: 5px;",
            class = "text-center"
          )
        )
      )
    ),
    wellPanel(
      div(
        style = "text-align: left;",
        tags$div(
          tagList(
            icon("sign-in-alt"),
            tags$b("Sign in with")
          ),
          style = "text-align: left;"
        ),
        actionButton(
          inputId = "login_button_auth_spang_lab",
          label = tagList(
            img(
              src = "imlui/assets/png/spang-lab-logo-64x64.png",
              alt = "spang-lab-logo.png",
              style = "height: 1.5em;"
            ),
            "Spang Auth"
          ),
          class = "btn-block"
        ),
        actionButton(
          inputId = "login_button_gitlab_spang_lab",
          label = tagList(
            img(
              src = "imlui/assets/png/spang-lab-logo-64x64.png",
              alt = "spang-lab-logo.png",
              style = "height: 1.5em;"
            ),
            "Spang Gitlab"
          ),
          class = "btn-block"
        ),
        actionButton(
          inputId = "login_button_google",
          label = tagList(
            img(
              src = "imlui/assets/png/google-logo-48x48.png",
              alt = "google-logo.png",
              style = "height: 1.5em;"
            ),
            "Google"
          ),
          class = "btn-block"
        ),
        actionButton(
          inputId = "login_button_github",
          label = tagList(
            img(
              src = "imlui/assets/png/github-mark-32px.png",
              alt = "github-mark.png",
              style = "height: 1.5em;"
            ),
            "GitHub"
          ),
          class = "btn-block"
        ),
        actionButton(
          inputId = "login_button_gitlab",
          label = tagList(
            img(
              src = "imlui/assets/png/gitlab-icon-rgb.svg",
              alt = "gitlab-icon.svg",
              style = "height: 1.5em;"
            ),
            "Gitlab"
          ),
          class = "btn-block"
        )
      )
    )
  )
}
