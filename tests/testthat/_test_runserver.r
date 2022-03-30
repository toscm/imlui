# testthat::test_that(
#     desc = "summary print respects max value (shiny::testServer impl.)",
#     code = {
#         shiny::testServer(
#             app = imlui:::server,
#             expr = {
#                 session$setInputs(
#                     dataset = "esoph",
#                     max = 5
#                 )
#                 testthat::expect_match(
#                     object = output$summary,
#                     regexp = "reached 'max'"
#                 )
#             }
#         )
#     }
# )

# testthat::test_that(
#     desc="summary print respects max value (shinytest::ShinyDriver impl.)",
#     code = {
#         app <- shinytest::ShinyDriver$new(
#             shinyApp(
#                 ui = imlui:::ui,
#                 server = imlui:::server
#             )
#         )
#         app$setInputs(
#             dataset = "esoph"
#         )
#         testthat::expect_match(
#             object = app$getValue("summary"),
#             regexp = "reached 'max'"
#         )
#     }
# )
