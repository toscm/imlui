# IML-UI <img src="inst/assets/png/imlui_logo.png" align="right" width="100" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/toscm/imlui/workflows/R-CMD-check/badge.svg)](https://github.com/toscm/imlui/actions)
[![Codecov test coverage](https://codecov.io/gh/toscm/imlui/branch/main/graph/badge.svg)](https://app.codecov.io/gh/toscm/imlui?branch=main)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/imlui)](https://cran.r-project.org/package=imlui)
<!-- badges: end -->

A user interface (UI) for Interpretable Machine Learning (IML) methods.

## Table of Contents

- [Table of Contents](#table-of-contents)
- [Purpose](#purpose)
- [Installation](#installation)
- [Usage](#usage)
- [Configuration](#configuration)
- [Documentation](#documentation)
  - [Data Storage](#data-storage)
  - [User Authentication](#user-authentication)
  - [User Authorization](#user-authorization)
  - [External User Authorization](#external-user-authorization)
- [Frequently Asked Questions (FAQ)](#frequently-asked-questions-faq)
  - [How to add new datasets?](#how-to-add-new-datasets)
  - [How to merge user accounts?](#how-to-merge-user-accounts)
  - [What happens when the imlui server is started?](#what-happens-when-the-imlui-server-is-started)
  - [What happens when a user connects to the imlui server?](#what-happens-when-a-user-connects-to-the-imlui-server)
- [Developer Guidelines](#developer-guidelines)
  - [Variable Naming Conventions](#variable-naming-conventions)
  - [Variable Documentation](#variable-documentation)
  - [UI Principles](#ui-principles)
  - [UI Layout](#ui-layout)
  - [How to submit to CRAN?](#how-to-submit-to-cran)
  - [Idea: high performance Architecture](#idea-high-performance-architecture)
  - [Idea: Improve current architecture](#idea-improve-current-architecture)
- [Todos](#todos)
  - [By File](#by-file)

## Purpose

Over the last three decades dozens of research papers have proposed different classification methods for Diffuse Large B-Cell Lymphoma (DLBCL). The goal of this app is to implement all these methods and to provide a unified interface for applying, interpreting and comparing them.

## Installation

For Installation as standalone app in Windows, Linux or Mac, see [Installation](INSTALLATION.md). For installation as R-package, run one of the following commands:

```R
# From CRAN (stable version)
install.packages("imlui")
# From Github (development version)
devtools::install_github("toscm/imlui")
```

## Usage

From commandline:

```bash
imlui [--help|--version]
imlui runserver [--port=<port>] [--config-file=<config-file>] [--config-dir=<config-dir>]
```

From R:

```R
library(imlui)
# Option 1 (directly starts a webserver)
imlui:::runserver(port=8080, config_file=NULL, config_dir=NULL)
# Option 2 (returns a shinyApp object that can be used with shinyserver)
imlui:::imluiApp(port=8080, config_file=NULL, config_dir=NULL)
```

## Configuration

In this documentation the directories `IMLUI_PACKAGE_DIR` and `IMLUI_CONFIG_DIR` are referenced. The path of `IMLUI_PACKAGE_DIR` depends on the system defaults and parameters handed over to `devtools::install()` / `install.packages()` during installation. The path of `IMLUI_CONFIG_DIR` is determined as follows: `IMLUI_CONFIG_DIR` =

1. `<config-dir>` commandline argument, if specified by user, else
2. `<$IMLUI_CONFIG_DIR>` environment var, if existing and not empty, else
3. `<$XDG_CONFIG_HOME>/imlui` environment var, if existing and not empty, else
4. `<$HOME>/.config/imlui` environment var, if existing and not empty, else
5. `<$USERPROFILE>/.config/imlui` environment var, if existing and not empty, else
6. `<current-working-directory>`

All configuration options for imlui live in a single yaml configuration file `IMLUI_CONFIG_FILE`. The following paths are checked for such a file:

1. `<config-file>`
2. `$IMLUI_CONFIG_FILE`
3. `<config-dir>`/imlui_config.yml
4. `$IMLUI_CONFIG_DIR`/imlui_config.yml
5. `${PWD}`/imlui_config.yml
6. `$XDG_CONFIG_HOME`/imlui/imlui_config.yml
7. `$HOME`/.config/imlui/imlui_config.yml
8. `$USERPROFILE`/.config/imlui/imlui_config.yml

If multiple config files exist, only the first one is used. If no config file exists, [`<IMLUI_PACKAGE_DIR>`/assets/yml/imlui_config.yml](inst/assets/yml/imlui_config.yml) is copied to `<IMLUI_CONFIG_DIR>` and that path is returned.

## Documentation

### Data Storage

Imlui uses a relational database system for storing all information except raw datasets. Supported database types are [PostgreSQL](https://www.postgresql.org/) (recommended for multi user instances) and [SQLite](https://www.sqlite.org/index.html) (default for local usage). The corresponding database type, hostname, port, username and password must be specified in `<IMLUI_CONFIG_FILE>`, section `database` (see [Configuration](#configuration) for details). If no `database` configuration is provided, a new SQLite database is created at `<IMLUI_DATA_DIR>/imlui_database.sqlite`.

For storing raw datasets, imlui uses a [datatomb](https://gitlab.spang-lab.de/containers/datatomb/) server like <data.spang-lab.de>. The server URL, server port and the access token to be used for the connection must be specified in `<IMLUI_CONFIG_FILE>` section `datatomb_server` . If no `datatomb_server` configuration is provided, a new datatomb server is started at `localhost:4141`.

### User Authentication

Imlui supports classic user/password authentication as well as authentication via OpenID Connect, i.e. the option to *Sign in with Google/Github/Facebook etc*. Currently supported OpenID Connect providers are: [auth.spang-lab.de](https://auth.spang-lab.de/), [gitlab.spang-lab.de](https://gitlab.spang-lab.de/), [github.de](https://github.com/), [google.com](https://www.google.com/), [facebook.com](https://www.facebook.com/) and [twitter.com](https://twitter.com/). Should you opt for authentication via OpenID Connect, at least a unique ID will be retrieved from the respective identity provider. If available, your name, email and profile picture will be retrieved as well.

### User Authorization

By default, user authorization, i.e. the management of user permissions, is handled by imlui directly. For information on how to configure an external identity provider as authorization source, please refer to section [External User Authorization]. Imlui's internal authorization mechanism works as follows: for each user an entry is created in table `users`¹ after the user's first login. Permission to access specific datasets/services, is stored as a property of the dataset/service in table `dataset_permissions`/`resource_permissions`². Example: it is possible to grant `use` access for dataset `Lenz_2008_train` to groups `spang-lab-phd-students` and `spang-lab-master-students` and to grant `download` access to group `imlui-admins` and user `uid12345`.

¹ Columns of table `users` are `user_id`, `google_id`, `github_id`, `email`, `password`, `name`, ...
² Columns of table `dataset_permissions`/`resource_permissions` are `dataset_id`/`resource_id` and `permission_id`. Possible values for `permission_id` are: `list`, `view` and `download`. Side-note: there is no `write` or `change` permissions, because datasets are immutable objects. If anything has to be changed, that new dataset must obtain a different ID.

### External User Authorization

Imlui can be configured to use an external identity provider such as [auth.spang-lab.de](https://auth.spang-lab.de/) for authorization. This is implemented as follows: when user `uidX` logs in via identity provider `authY.com`, then

1. All groups `G={g1, g2, ...}` assigned to `uidX` are retrieved from `authY.com`
2. New rows
   + `group_id=g1, user_id=uidX, granted_by=authY.com`
   + `group_id=g2, user_id=uidX, granted_by=authY.com`
   + `...`
   are added to table `groups`, if they don't exist yet.
3. All rows matching `group_id=.*, user_id=uidX, granted_by=authY.com` are retrieved from table `users`. If any `group_id` is found, that is no longer an element of `G`, those entries are removed.

This way, access to resources can be given to groups defined by `authY.com` and whenever a user signs in via `authY.com` he automatically is added to those groups (or removed from them).

To enable autorization via an external identity provider, login as admin and go to `Settings` > `Authorization` > `Enabled external authorization providers` and tick the checkbox of the external identity provider you want to enable.

## Frequently Asked Questions (FAQ)

### How to add new datasets?

There are currently three different ways to add new datasets to imlui. Method 1 and 2 require access to `imlui_config.yml` (see section [Configuration](#configuration)), i.e. they are only available for imlui hosters, but not for mere imlui users. Method 3 requires a user to upload its dataset into imlui's data storage. Therefore, if you are prohibited by law from uploading your data to third-party servers, hosting your own imlui instance might be your only option. For details regarding imlui's security mechanisms for protecting your data please see section [Data Storage](#data-storage).

### How to merge user accounts?

If a user logs in via different authentication mechanisms, e.g. once via *Sign in with Google* and once via *Sign in with username/password* two seperate accounts are created. In the followin we assume, that the first created account has received user ID *uid1* and the second account has received user ID *uid2*. To merge those accounts later on, login as uid1 and click on `Settings` > `Profile` > `Merge Account`. Then enther uid2 as ID of your second user account. Afterwards you will be prompted to login via the preferred login method of uid2. After completing the login process, your two accounts will be merged, i.e:

1. All permissions granted to uid2 will be granted to uid1
2. All profile information available for uid2, but missing for uid1, will be copied to uid1
3. Profile information that is different between uid1 and uid2, will be kept as is, unless checkbox *overwrite current profile information* was checked (in this case the version from uid2 will be used to overwrite the uid1 version)
4. The entry for uid2 in table `users` gets deleted.

### What happens when the imlui server is started?

TODO

### What happens when a user connects to the imlui server?

TODO

## Developer Guidelines

### Variable Naming Conventions

```txt
M  <- Vector of Model Names                         [lamis,             tric,            ...]
MF  <- List of Vectors of Feature Names             [lamis=(MYC, BCR1), tric=(ABC, DEF), ...]
MP  <- List of Vectors of Parameter Values          [lamis=(0.1, -1.3), tric=(2, 3),     ...]
D   <- Vector of Dataset Names                      [lamis_test,             lamis_train,             ...]
DS  <- List of Vectors of Sample Names              [lamis_test=(s1, s2),    lamis_train=(a1, a2),    ...]
DF  <- List of Vectors of Feature Names             [lamis_test=(ABC, BCL2), lamis_train=(ABC, BCL2), ...]
DX  <- List of Dataframes of covariate values       [lamis_test= data.frame(ABC=c(s1=2, s2=3), BCL2=c(a1=3, a2=4)), .]
MDY <- Dataframe of Dataframes of prediction values
 ___________________________________________
|       | (Models)lamis_test1 | tric_train  |
|_______|_____________________| ____________|
|(Data- |  _________________  |  _________  |
| sets) | |  | classA|classB| | |   | A| B| |
|       | |s1| 0.7   |0.3   | | |x12| 0| 1| |
|lamis  | |s2| 0.2   |0.8   | | |x14| 0| 1| |
|       | |_________________| | |_________| |
|       |  _________________  |  _________  |
| tric  | |  | Score        | | |   | Sc. | |
|       | |s1|   122        | | |x12|  40 | |
|       | |s2|    88        | | |x14|  30 | |
|       | |_________________| | |_________| |
|_______|_____________________|_____________|
```

### Variable Documentation

+ `pkg`: *environment*. Process/package environment. Initalized during package loading in function `.onLoad()`.
  + `datasets`: `mcache` object. Initalized during package loading in function `.onLoad()`. Can be updated only by function `getdata()` which is called only by reactives `ses$r$model$params[<model_symbol>]()` and `ses$r$dataset$df[dataset_symbol]()`.
  + `models`: `list`.
  + `db`: *DB object*. Used for interacting with IMLUI_DB. Initalized in `init_server_data` by calling `DB$new()`.
+ `ses`: *environment*. Session environment. Contains all data specific for a single user session. Initalized in `server()` by calling `init_server_data()`. Updated by almost every other function during a user session. For details see below.
  + `input`: *reactiveValues*. Populated by `shiny::runApp` based on messages from browser via websocket and passed as readonly object to `server`.
    + `dim`: *2 dimensional numeric vector*. Contains width and height of HTML body.
  + `output`: *reactiveValues object*. Populated by `server` and passed as readonly object to `shiny::runApp` where it (potentially) triggers messages via websocker to browser.
  + `session`: *ShinySession object*. Populated by `shiny::runApp` based on messages from browser via websocket and passed as readonly object to `server`.
    + `clientData`: *reactiveValues object*.
  + `const`: *list*. Values constant for a single session, like `url_hostname`, `url_port` determined at runtime after server has started.
  + `rv`: *reactiveValues object*.
    + `user`: *list of strings*. Elements are: id, group_ids, display_name, github_id, avatar_url, password, gitlab_id, google_id, spanglab_gitlab_id, spanglab_auth_id.
    + `db`: *list of dataframes*. Each dataframe corresponds to a table in IMLUI_DB.
  + `r`: *list of reactives and other lists*.
    + `model`: *list*.
      + `ids`: *reactive*. Returns a vector of all model IDs from IMLUI_DB for which the current user has access.
      + `symbols`: *reactive*. Returns a vector of all model symbols from IMLUI_DB for which the current user has access.
      + `displaynames`: *reactive*. Returns a vector of all model displaynames from IMLUI_DB for which the current user has access. The vector names are set to `ses$r$model$symbols()`.
      + `pkgs`: *reactive*. Returns a vector of all model pkgs from IMLUI_DB for which the current user has access. The vector names are set to `ses$r$model$symbols()`.
      + `params`: *list* **TODO: remove and use `pkg$datasets` for caching instead**
        + `<model_symbol_1>`: *reactive*.
        + `<model_symbol_2>`: *reactive*.
        + ...
      + `features`: *list* **TODO: remove and use `pkg$datasets` for caching instead**
        + `<model_symbol_1>`: *reactive*.
        + `<model_symbol_2>`: *reactive*.
        + ...
      + `symbols_list`: *reactive*.
      + `params_list`: *reactive*.
      + `features_list`: *reactive*.
    + `dataset`: *list*
      + ... **TODO**
    + `size`: *list*
      + ... **TODO**
  + `app` **not yet implemented, just an idea (use shiny modules and nest their environment)**
    + `input`
    + `output`
    + `session`
    + `rv`
    + `r`
    + `login_page`
    + `model_analysis_page`
    + `dataset_analysis_page`
    + `dataset_analysis_page`

### UI Principles

+ **Dont' make me think**: the webapp should be self-explanatory and intuitive to use, i.e. users should understand the purpose of the app and each component it it without having to think about it.
+ **Names should simple and concise**.
+ **Clarity trumps consistency**.
+ **Make it obvious what's clickable**: everything that can be clicked must be clearly recognizable as such.
+ The site must work on all common devices, i.e. Smartphones, Tablets and Monitors.
+ **Keep the noise down**: remove (or hide) ALL unnecessary information from a page. Omit needless words.
+ **Format text to support scanning**:
  + Use plenty of heading
  + Format heading correctly, i.e. huge size differences and much closer to the section they introduce than to the section they follow.
  + Avoid long paragraphs (known as "wall of words")
  + Use listings wherever possible
  + Highlight key terms (sparingly)
+ **Add help Icons to every (plain text) user Input**
+ **Prefer examples over theoretic explanations**
+ The Homepage must answer the following four questions:
  + What is this site?
  + What can I do here?
  + What do they have here?
  + Why should I be here and not somewhere else?
  + Where to I start?
  Ideas:
    + Carousel of DLBCL, machine learning, etc. and overlay text "Predict, Compare, Explain - use machine learning methods to make prediction for DLBLC. Accompanied with intuitive visualizations and explanations". Figure caption should contain links to "Predict", "Compare", "Explain".
    + Page Header with
      + a tagline (h1): "Making machine learning interpretable"
      + a welcome blurb (h2), i.e., a terse description of the site, displayed in a prominent block on the Home page: "IMLUI provides a grafical user interface to apply and explain machine learning methods to diffuse large b-cell (DLBCL) data"
      + learn more content, i.e., multiple images or videos with short texts like
        + VID: "[Get Stared](lnk). Short introduction video explaining how to get the most out of the IML UI for DLBCL."
        + IMG: "[Compare](link) multiple models on the same set of samples."
        + IMG: "[Predict](lnk) new samples. Choose from more than 15 different models published from 1998-2022."

The above principles are inspired by / derived from the direct citations of the book "Don't make me think" from Krug, 2014, 3rd edition:

+ *[...] as far as is humanly possible, when I look at a Web page it should be self-evident. Obvious. Self-explanatory. I should be able to "get it" - what it is and how to use it - without expending any effort thinking about it.*
+ Things to avoid are:
  - *cute or clever names, marketing-induced names, company-specific names, and unfamiliar technical names.*
  - *links and buttons that aren't obviously clickable*
+ *The fact that the people who built the site didn't care enough to make things obvious - and easy - can erode our confidence in the site and the organization behind it.*
+ *We don't read pages. We scan them.*
+ *We don't make optimal choices. We satisfice.* (I.e., *In reality, [...] we don't choose the best option - we choose the first reasonable option [...].)
+ *Faced with the fact that your users are whizzing by, there are some important things you can do to make sure they see and understand as much of what they need to know - and of what you want them to know - as possible:*
  - *Take advantage of conventions*
    - *Users expect the logo identifying the site to be in the top-left corner [...] and the primary navigation to be across the top or down the left side*.
    - *Many elements have a standardized appearance, like the icon that tells you it's a link to a video, the search icon, and the social networking sharing options.*
  - *Create effective visual hierarchies*
  - *Break pages up into clearly defined areas*
  - *Make it obvious what's clickable*
  - *Eliminate distractions*
  - *Format content to support scanning*
+ *Eye-tracking studies of Web page scanning suggest that users decide very quickly in their initial glances which parts of the page are likely to have useful information and then rarely look at the other parts - almost as though they weren't there. (Banner blindness - the ability of users to completely ignore areas they think will contain ads - is just the extreme case.)*

CONTINUE READING ON PAGE 30

### UI Layout

```txt
x Web App (web_app, WA)
  x Home Page (home_page, HP)
    x Sidebar (side_bar, SB)
      i Models Menu (models_menu, MM)
      i Datasets Menu (datasets_menu, DD)
      o Plot Area Size Widget (plot_area_size_widget, PAS_W)
      o App Info Text Output (app_info_text_output, AI_TO)
      i Clear Cache Button (clear_cache_button, CC_B)
      i Redraw Button (redraw_button, R_B)
    xc Mainpanel Tab (main_panel, MP)
      oc Settings Tab (settings_tab, S_T)
        o Papers (papers_tab, S_PA_T)
        o Models (models_tab, S_MO_T)
        o Datasets (datasets_tab, S_DS_T)
        o Settings (settings_tab, S_SE_T)
        o Samples (samples_tab, S_SA_T)
        o Datatypes (datatypes_tab, S_DT_T)
        o Methods (methods_tab, S_ME_T)
        o Platforms (platforms_tab, S_PF_T)
      oc Data Analysis Tab (data_analysis_tab, DA_T)
        o Descriptions Tab (descriptions_tab, DA_DD_TO)  [1, 4]
        o MSD Plot Tab (msd-plot_tab, DA_MSDP_T) [2]
      oc Model Analysis Tab (model_analysis_tab, MA_T)
        o Descriptions Tab (descriptions_tab, MA_MD_TO) [1]
        o Predictions Tab (predictions_tab, MA_MP_T) [5]
        o Survival Curves Tab (survival_curves_tab, MA_SC_T)
        o Feature Effect Plot Tab (feature_effect_plot_tab, MA_FE_T) [6]
      o Session Info Tab (session_info_tab, SI_TO) [1]
  x Login Page (login_page, LP)
```

+ x: Neither input nor output ID
+ i: Displays and provides a input ID
+ o: Displays and requires a output ID
+ c: Current Selection of subtabs is given through `<outputID>_CS`, e.g. `MP_CS`, `S_T_CS`, `DA_T_CS`, `MA_T_CS`
+ [1] TODO: Currently implemented as Text Output. Make similar to other tabs.
+ [2] TODO: Renamed to 'Numerical Features'
+ [4] TODO: Rename to Overview. Add options to display as description or corner matrix with dynamic 'n'.
+ [5] TODO: Rename ID to MA_P_T
+ [6] TODO: Renamed ID to MA_FEP_T

### How to submit to CRAN?

According to <https://r-pkgs.org/release.html> the following steps are necessary

```R
devtools::document() # Update documentation
rcmdcheck::rcmdcheck( # Run `R CMD check` for this package
    args=c("--no-manual", "--as-cran"),
    build_args=c("--no-manual"),
    check_dir="check"
)
devtools::revdep() # Run `R CMD check` for all dependencies
devtools::spell_check() # Check spelling of package
devtools::release() # Builds, tests and submits the package to CRAN.
# Manual submission can be done at: https://cran.r-project.org/submit.html
```

### Idea: high performance Architecture

1. Seperate UI `imlui-js` with [webhooks](https://zapier.com/blog/what-are-webhooks/) programmed in React Tailwind or similar and hosted via `webpack-cli serve --mode=production` or similar, implementing the following logic:
   1. If neither session-token-cookie nor access-token-cookie is present send `GET start-r-session` to backend server and display "Connecting to IMLUI Server ..." or something
   2. After session-token is retrieved, store session-token as cookie. Now user can interact with the API as unauthenticaed user or he can authenticate himself via `GET access-token ? username=x & password=y`. In that case
      1. The retrieved access-token is stored as cookie.
      2. The latest appstate of that user (URL encoded) is retrieved with `GET app-state`
      3. The private plus the personal `session-process`.
      4. TO BE CONTINUED
2. Seperate backend `imlservr`, programmed with R `plumber`, `breakr` or [fiery](https://fiery.data-imaginist.com/index.html) OR seperate backend `imlservjs` programmed in Type-/Javascript, implementing the following API:
   1. `GET start-session"` --> starts new R process which listens on `"https://.../<session-token>"` for commands and returns session-token (to be stored as cookie)
   2. `GET stop-session ? session-token=<session-token>"` --> stops the R process (automatically called when browser session, i.e., tab, is closed)
   3. `GET login ? username=asdf & password=qwer"` --> returns access_token (to be stored as cookie)
   4. `GET start-private-session?session-token=<session-token>"` --> stops the R process (automatically called when browser session, i.e., tab, is closed)
   5. `GET login?username=asdf&password=qwer"` --> returns access_token (to be stored as cookie)
   6. `GET get-fep?session-token=<session-token>&access-token=<asdf>&"`

### Idea: Improve current architecture

Create separate environments for process data, e.g. `pdat` and session data, e.g. `sdat`. Then replace list of reactives for datasets with normal functions. The normal function stores the dataset in `pdat` if not yet available and then returns it. If already available, it directly returns it. Furthermore it stores the timestamp of the last access to that dataset. When a session is closed, datasets that haven't been used for 7 days or longer get deleted.

## Todos

### By File

| File                               | R    | Test    |
| ---------------------------------- | ---- | ------- |
| 900_mocks.R                        | ok   |         |
| 800_print_funcs.R                  | ok   |         |
| 760_pkg_onload.R                   | ok   |         |
| 740_pkg_utils.R                    | ok   |         |
| 730_pkg_opts.R                     | ok   |         |
| 720_pkg_imports.R                  | ok   |         |
| 710_pkg_datasets.R                 | ok   |         |
| 700_pkg_constants.R                | ok   |         |
| 520_php_plot.R                     | ok   |         |
| 510_msdp_plot.R                    | ok   |         |
| 500_fep_plot.R                     | ok   |         |
| 340_settings_page_ui.R             |      |         |
| 340_settings_page_server.R         |      |         |
| 330_dataset_analysis_page_ui.R     |      |         |
| 330_dataset_analysis_page_server.R |      |         |
| 321_model_analysis_page_server.R   |      |         |
| 320_model_analysis_page_ui.R       |      |         |
| 311_login_page_server.R            |      |         |
| 310_login_page_ui.R                |      |         |
| 301_web_app_server.R               | 6    |         |
| 300_web_app_ui.R                   | 5    |         |
| 190_server_utils.R                 | 4    |         |
| 140_init_observers.R               | next |         |
| 135_init_prediction_reactives.R    | 1    |         |
| 134_init_dataset_reactives.R       | ok   | ok      |
| 133_init_size_reactives.R          | ok   | ok      |
| 132_init_model_reactives.R         | ok   | ok      |
| 130_init_reactives.R               | 2    |         |
| 120_init_reactive_values.R         | ok   | trivial |
| 110_init_server_constants.R        | ok   | ok      |
| 100_init_server_data.R             | 3    |         |
| 050_config.R                       | ok   |         |
| 040_db.R                           | ok   |         |
| 030_ui.R                           |      |         |
| 020_server.R                       |      |         |
| 010_cli.R                          |      |         |
| 000_app.R                          | ok   | trivial |

Note: use mock functions to create testcases for trivial function:
<https://cran.r-project.org/web/packages/mockery/vignettes/mocks-and-testthat.html>.
