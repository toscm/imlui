$root = $(git rev-parse --show-toplevel)
docker run -it --rm `
    -v "${root}/inst:/srv/shiny-server/inst" `
    -v "${root}:/srv/shiny-server/imlui" `
    -v "${root}/dev/shiny-server.conf:/etc/shiny-server/shiny-server.conf" `
    -v "${root}/dev/.Rprofile:/root/.Rprofile" `
    -v "${root}/dev/Rprofile.site:/usr/local/lib/R/etc/Rprofile.site" `
    -p "80:3838" `
    -p "8080:8080" `
    -e "SHINY_LOG_LEVEL=TRACE" `
    "toscm/r-dev:1.1.0" # "rocker/shiny-verse:4.2.1"
