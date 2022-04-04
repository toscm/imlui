FROM rocker/shiny-verse:4.1.2
# Usage:
#   docker build -t toscm/install-and-run-imlui:latest --no-cache inst/examples/install-and-run-imlui
#   docker run -it -p 8080:8080 toscm/install-and-run-imlui:latest
RUN Rscript -e "devtools::install_github('toscm/imlui')"
EXPOSE 8080
CMD ["Rscript", "-e", "imlui:::runserver()"]
