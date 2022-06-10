$root = $(git rev-parse --show-toplevel)
docker run -it --rm `
    -v "${root}/inst/mre:/srv/shiny-server/apps" `
    -v "${root}/dev/shiny-server.conf:/etc/shiny-server/shiny-server.conf" `
    -p "80:3838" `
    -e "SHINY_LOG_LEVEL=TRACE" `
    "rocker/shiny-verse:4.1.2"
