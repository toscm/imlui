FROM rocker/shiny-verse:4.1.2
# Usage: docker compose {build|run --rm imlui|push}
# Parents: ubuntu:focal (20.4) > rocker/r-ver:4.1.2 > rocker/shiny:4.1.2 > rocker/shiny-verse:4.1.2
# Sets env vars: R_VERSION=4.1.2, TERM=xterm, R_HOME=/usr/local/lib/R, TZ=Etc/UTC, CRAN=https://packagemanager.rstudio.com/cran/__linux__/focal/2022-03-09, LANG=en_US.UTF-8, S6_VERSION=v2.1.0.2, SHINY_SERVER_VERSION=latest, PANDOC_VERSION=default
# Makes dirs: /rocker_scripts/
# Runs in /rocker_scripts: install_R_source.sh, setup_R.sh, install_shiny_server.sh, install_tidyverse.sh
# See <https://github.com/rocker-org/rocker-versioned2/tree/master/scripts> for scripts
RUN Rscript -e "devtools::install_github('toscm/imlui')"
EXPOSE 8080
CMD ["Rscript", "-e", "imlui:::runserver()"]
