FROM toscm/install-and-run-imlui:latest
# Usage: docker compose {build|up|push}
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
