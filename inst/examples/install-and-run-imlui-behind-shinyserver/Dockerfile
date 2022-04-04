FROM toscm/install-and-run-imlui:latest
# Usage:
#   docker build -t toscm/install-and-run-imlui-behind-shinyserver:latest --no-cache inst/examples/install-and-run-imlui-behind-shinyserver
#   docker run -it -p 3838:3838 toscm/install-and-run-imlui-behind-shinyserver:latest
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
