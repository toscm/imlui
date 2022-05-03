FROM rocker/shiny-verse:4.1.2
# Usage:
#   docker build -t toscm/install-and-run-imlui-behind-shinyserver:latest --no-cache inst/examples/install-and-run-imlui-behind-shinyserver
#   docker run -it -p 3838:3838 toscm/install-and-run-imlui-behind-shinyserver:latest
# Access via:
#   localhost:3838/shinyserver/imlui
RUN Rscript -e "devtools::install_github('toscm/imlui')"
COPY \
    /inst/examples/install-and-run-imlui-behind-shinyserver/shiny-server.conf \
    /etc/shiny-server/shiny-server.conf
RUN mkdir /srv/shiny-server/imlui
COPY \
    /inst/examples/install-and-run-imlui-behind-shinyserver/app.R \
    /srv/shiny-server/imlui/app.R
COPY \
    /inst/assets \
    /srv/shiny-server/imlui/www/imlui/assets
USER shiny
EXPOSE 3838
CMD ["/init"]
