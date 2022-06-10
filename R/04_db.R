DB <- R6::R6Class(
  classname = "DB",
  lock_objects = FALSE,
  lock_class = FALSE,
  public = list(
    type=NULL,
    filepath=NULL,
    hostname=NULL,
    port=NULL,
    database=NULL,
    username=NULL,
    password=NULL,
    conn=NULL,
    default_filepath = system.file(
      "assets/sqlite/imlui_db.sqlite", package = "imlui"
    ),
    initialize = function(config = read_imlui_config_file()) {
      do.call(rlang::env_bind, c(self, config$dbms))
      return(self$connect())
    },
    connect = function() {
      if (self$type == "sqlite") {
        if (!file.exists(self$filepath)) {
          logsne("File", self$filepath, "does not exist")
          catsne("Copying", self$default_filepath, "to", self$filepath, "...")
          file.copy(default_filepath, self$filepath)
        }
        self$conn <- dbConnect(RSQLite::SQLite(), self$filepath)
      } else if (self$type == "postgres") {
        self$conn <- dbConnect(
          drv = RPostgres::Postgres(),
          host = self$hostname,
          port = self$port,
          dbname = self$database,
          user = self$username,
          password = self$password
        )
        if (length(dbListTables(self$conn)) == 0) {
          log0ne("Found no tables in `", self$hostname, ":", self$port, "/",
                  self$database, "`:\nInitializing with default values ...")
          default_db <- dbConnect(RSQLite::SQLite(), self$default_filepath)
          on.exit(dbDisconnect(default_db))
          for (tbl in dbListTables(default_imlui_db)) {
            dbWriteTable(self$conn, tbl, value = dbReadTable(default_db, tbl))
          }
        }
      }
      return(self)
    },
    finalize = function() {
      self$disconnect()
    },
    get_table = function(tbl) {
      dbReadTable(self$conn, tbl)
    },
    get_tables = function(key) {
      logsne("Reading tables...")
      tbls <- list()
      for (tbl in TABLES) {
        tbls[[tbl]] <- dbReadTable(self$conn, tbl)
      }
      rownames(tbls$users) <- tbls$users$user_id
      rownames(tbls$Models) <- tbls$Models$ID
      rownames(tbls$Datasets) <- tbls$Datasets$ID
      return(tbls)
    },
    disconnect = function() {
      logsne("Disconnecting DB ...")
      dbDisconnect(self$conn)
      return(self)
    },
    execute = function(query, params = list(), ...) {
      # Execute SQL query. Examples:
      # send("INSERT INTO cars (speed, dist) VALUES (1, 1), (2, 2), (3, 3)") or
      # send(query = "UPDATE Appstate SET resource_value = ? WHERE
      #   user_id = ? AND resource_id = ?", params = list(url, user_id, "url"))
      res <- dbExecute(self$conn, query, params, ...)
      return(res)
    },
    get_valid_cookies = function(expiry = 7) {
      # This function must return a data.frame with columns user and sessionid.
      # Other columns are also okay and will be made available to the app after
      # log in as columns in credentials()$user_auth TODO: Based on shinyauthr
      # example (cite!)
      login_time <- NULL # TODO: remove tidyverse NSE stuff
      self$get_table("mapping_users_sessions") %>%
        dplyr::mutate(login_time = lubridate::ymd_hms(login_time)) %>%
        tibble::as_tibble() %>%
        dplyr::filter(login_time > lubridate::now() - lubridate::days(expiry))
    },
    insert_cookie = function(user_id, session_id) {
      # This function must accept two parameters: user and sessionid. It will be
      # called whenever the user successfully logs in with a password. This function
      # saves to your database.
      # TODO: Based on shinyauthr example (cite!)
      tibble::tibble(
        user_id = user_id,
        session_id = session_id,
        login_time = as.character(lubridate::now())
      ) %>%
        dbWriteTable(self$conn, "mapping_users_sessions", ., append = TRUE)
    },
    # Auto generated and not tested
    appendTable = function(...) DBI::dbAppendTable(self$conn, ...),
    canConnect = function(...) DBI::dbCanConnect(self$conn),
    connect = function(...) DBI::dbConnect(self$conn),
    createTable = function(...) DBI::dbCreateTable(self$conn),
    existsTable = function(...) DBI::dbExistsTable(self$conn),
    fetch = function(...) DBI::dbFetch(self$conn, ...),
    getInfo = function(...) DBI::dbGetInfo(self$conn, ...),
    getQuery = function(...) DBI::dbGetQuery(self$conn, ...),
    getRowCount = function(...) DBI::dbGetRowCount(self$conn, ...),
    getRowsAffected = function(...) DBI::dbGetRowsAffected(self$conn, ...),
    getStatement = function(...) DBI::dbGetStatement(self$conn, ...),
    hasCompleted = function(...) DBI::dbHasCompleted(self$conn, ...),
    isReadOnly = function(...) DBI::dbIsReadOnly(self$conn, ...),
    isValid = function(...) DBI::dbIsValid(self$conn, ...),
    listConnections = function(...) DBI::dbListConnections(self$conn, ...),
    listFields = function(...) DBI::dbListFields(self$conn, ...),
    listObjects = function(...) DBI::dbListObjects(self$conn, ...),
    listResults = function(...) DBI::dbListResults(self$conn, ...),
    listTables = function(...) DBI::dbListTables(self$conn, ...),
    quoteIdentifier = function(...) DBI::dbQuoteIdentifier(self$conn, ...),
    quoteLiteral = function(...) DBI::dbQuoteLiteral(self$conn, ...),
    quoteString = function(...) DBI::dbQuoteString(self$conn, ...),
    readTable = function(...) DBI::dbReadTable(self$conn, ...),
    removeTable = function(...) DBI::dbRemoveTable(self$conn, ...),
    rollback = function(...) DBI::dbRollback(self$conn, ...),
    sendQuery = function(...) DBI::dbSendQuery(self$conn, ...),
    sendStatement = function(...) DBI::dbSendStatement(self$conn, ...),
    setDataMappings = function(...) DBI::dbSetDataMappings(self$conn, ...),
    writeTable = function(...) DBI::dbWriteTable(self$conn, ...)
  )
)
