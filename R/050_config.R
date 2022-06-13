# Return content `IMLUI_CONFIG_FILE`` as list
read_imlui_config_file <- function(create_if_missing = TRUE) {
  imlui_config.yml <- get_imlui_config_file(
    create_if_missing = create_if_missing
  )
  logsne("Reading", imlui_config.yml, "...")
  yml_raw <- yaml::read_yaml(imlui_config.yml)
  yml <- gsub_yml(yml = yml_raw, IMLUI_CONFIG_DIR = get_imlui_config_dir())
  return(yml)
}

# Replaces all occurences of string "${IMLUI_CONFIG_DIR}" in file `yml` with
# the actual value of `IMLUI_CONFIG_DIR`, which is determined at runtime. This
# function is intended to be used by function `read_imlui_config_file` for
# patching the contents of `IMLUI_CONFIG_FILE`.
gsub_yml <- function(yml, IMLUI_CONFIG_DIR) {
  if (is.list(yml)) {
    return(lapply(yml, gsub_yml, IMLUI_CONFIG_DIR))
  } else {
    return(
      gsub(
        pattern = "${IMLUI_CONFIG_DIR}",
        replacement = IMLUI_CONFIG_DIR,
        x = yml,
        fixed = TRUE
      )
    )
  }
}

# Return path to <IMLUI_CONFIG_DIR> as string
get_imlui_config_dir <- function() {
  toscutil::config_dir(
    app_name = "imlui",
    env_var = Sys.getenv("IMLUI_CONFIG_DIR"),
    create = TRUE,
    sep = "/"
  )
}

# Return path to <IMLUI_CONFIG_FILE> as string
get_imlui_config_file <- function(create_if_missing) {
  norm <- function(...) {
    toscutil::norm_path(..., sep = sep)
  }
  file_cl_arg <- {
    x <- commandArgs()
    x[grep("--config-file", x) + 1]
  }
  dir_cl_arg <- {
    x <- commandArgs()
    x[grep("--config-dir", x) + 1]
  }
  file_env_var <- Sys.getenv("IMLUI_CONFIG_FILE")
  dir_env_var <- Sys.getenv("IMLUI_CONFIG_DIR")
  sep <- "/"
  # 1. Check <config-file>
  if (is.non.empty.string(file_cl_arg)) {
    if (!file.exists(file_cl_arg)) {
      stop(file_cl_arg, " does not exist")
    }
    return(norm(file_cl_arg))
  }
  # 2. Check $IMLUI_CONFIG_FILE
  if (is.non.empty.string(file_env_var)) {
    if (!file.exists(file_env_var)) {
      stop(file_env_var, " does not exist")
    }
    return(norm(file_env_var))
  }
  # 3. Check <config-dir>/imlui_config.yml
  if (is.non.empty.string(dir_cl_arg)) {
    x <- norm(dir_cl_arg, "imlui_config.yml")
    if (file.exists(x)) {
      return(x)
    }
  }
  # 4. Check $IMLUI_CONFIG_DIR/imlui_config.yml
  if (is.non.empty.string(dir_env_var)) {
    x <- norm(dir_env_var, "imlui_config.yml")
    if (file.exists(x)) {
      return(x)
    }
  }
  # 5. Check ${PWD}/imlui_config.yml
  x <- norm(getwd(), "imlui_config.yml")
  if (file.exists(x)) {
    return(x)
  }
  # 6. Check $XDG_CONFIG_HOME/imlui/imlui_config.yml
  x <- norm(Sys.getenv("XDG_CONFIG_HOME"), "imlui/imlui_config.yml")
  if (Sys.getenv("XDG_CONFIG_HOME") != "" && file.exists(x)) {
    return(x)
  }
  # 7. Check $HOME/.config/imlui/imlui_config.yml
  x <- norm(Sys.getenv("HOME"), ".config/imlui/imlui_config.yml")
  if (Sys.getenv("HOME") != "" && file.exists(x)) {
    return(x)
  }
  # 8. Check $USERPROFILE/.config/imlui/imlui_config.yml
  x <- norm(Sys.getenv("USERPROFILE"), ".config/imlui/imlui_config.yml")
  if (Sys.getenv("USERPROFILE") != "" && file.exists(x)) {
    return(x)
  }
  # 9. No config exists, copy <IMLUI_PACKAGE_DIR>/assets/yml/imlui_config.yml to
  # <IMLUI_CONFIG_DIR> and return <IMLUI_CONFIG_DIR>/imlui_config.yml
  s <- system.file("assets/yml/imlui_config.yml", package = "imlui")
  d <- toscutil::config_dir(
    "imlui",
    cl_arg = dir_cl_arg,
    env_var = dir_env_var,
    create = TRUE,
    sep = "/"
  )
  if (create_if_missing) {
    file.copy(s, d)
  }
  return(norm(d, "imlui_config.yml"))
}

# Return path to cache directory (<home>/.imlui/cache). TODO: XDG compliance.
get_imlui_cache_dir <- function() {
  cacheDir <- file.path(get_home_dir(), ".imlui", "cache")
  if (!file.exists(cacheDir)) {
    dir.create(cacheDir, recursive = TRUE)
  }
  cacheDir
}

# Return path to USERPROFILE or HOME
get_home_dir <- function() {
  b2f(Sys.getenv("USERPROFILE") %d% Sys.getenv("HOME"))
}
