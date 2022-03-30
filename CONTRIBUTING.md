# Contribute

Since I do this project as PhD thesis, there shouldn't be many contributions from other people, so the emphasis of this document is not to define rules for collaboration but to document the technical details of IMLUI (variable naming schemes, app layout, which file implements which functionality etc.).

In case you are reading this document, because you want to contribute, just create an issue (and maybe a even a pull request) or write me directly in slack, zoom or via mail (<tobias2.schmidt@ukr.de>).

## How-tos

All the following commands should be entered in `bash` or `pwsh` (or some other reasonable shell) from the repository root folder (i.e. the folder containing this file...).

1. How to build the docker image?
   ```bash
   docker build -t imlui_prod .
   ```
2. How to run the docker image as container?
   ```bash
   docker run -it -p 80:80 imlui_prod
   ```
3. How to update the package version?

   *Update VERSION field in files `DESCRIPTION` and `VERSION`.*

4. How to install packages directly from any gitlab server?

   ```R
   # works only for public repositories
   remotes::install_gitlab(
       repo="sct39258/stringr@v1.4.0",
       host="gitlab.spang-lab.de"
   )
   # works also for internal repositories if git clone over ssh is setup
   # correctly BUT the version cannot be specified in URL, i.e. this cannot
   # be used to declare an outdated version as dependency in the package
   # DESCRIPTION file (see
   # https://cran.r-project.org/web/packages/devtools/vignettes/dependencies.html)
   remotes::install_git(
       url="ssh://git@gitlab.spang-lab.de:222/sct39258/imlui.git",
       ref="v0.0.0.9005",
       git="external",
       upgrade="never"
   )
   remotes::install_git(
       url="ssh://git@gitlab.spang-lab.de:222/glg58630/dataloading.lfs.git",
       ref="32c3d42a5a7f8f5daae7ce0d86a5ac107a6f6854",
       git="external",
       upgrade="never"
   )
   # works also for internal repositories if project access or user access token is
   # set up in gitlab BUT the host cannot be specified in the `repo` parameters, i.e.
   # this can also not be used to declare an outdated version as dependency in the
   # package DESCRIPTION file...
   access_token = "asdfqwerzxcv12345678"
   Sys.setenv("GITLAB_PAT", "82qtuM4Wkcac37DDAmJs")
   remotes::install_gitlab(
       repo="sct39258/stringr@v1.4.0",
       host="gitlab.spang-lab.de"
   )
   # ==> For now we have to use the newest versions of gitlab.spang-lab packages as
   # dependencies.
   ```

5. Where to store package data?

   - `data/<obj>.rda`: exported bin data, creation through `usethis::use_data(<obj>)`
   - `inst/extdata/<obj>.<ext>`: exported raw data, creation through asdf
   - `R/sysdata.rda`: internal bin data, creation through `usethis::use_data(<obj>, internal = TRUE)`
   - `data-raw/**/<obj>.<ext>`: ignored raw data, creation through `usethis::use_data_raw()`
   - `R\data.R`

   for details see <https://r-pkgs.org/data.html>
