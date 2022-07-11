DB <- R6::R6Class(
  classname = "DB",
  lock_objects = FALSE,
  lock_class = FALSE,
  public = list(
    type = NULL,
    filepath = NULL,
    hostname = NULL,
    port = NULL,
    database = NULL,
    username = NULL,
    password = NULL,
    conn = NULL,
    default_filepath = system.file(
      "assets/sqlite/imlui_db.sqlite",
      package = "imlui"
    ),

    #' @description #' Create and connect a new DB object
    #' @param config Either list(type="sqlite", filepath="...") or
    #' list(type="postgres", hostname="...", port="...", database="...",
    #' username="...", password="...")
    #' @return A new `DB` object.
    initialize = function(config = pkg$imlui_config$dbms) {
      do.call(rlang::env_bind, c(self, config))
      return(invisible(self$connect()))
    },
    connect = function() {
      logsne("Connecting to", as.character(self), "...")
      if (!self$is_connected()) {
        if (self$type == "sqlite") {
          private$connect_to_sqlite_dbms()
        } else if (self$type == "postgres") {
          private$connect_to_postgres_dbms()
        } else {
          stop("`type` must be either 'sqlite' or 'postgres', not:", self$type)
        }
      }
      return(invisible(self))
    },
    disconnect = function() {
      if (self$is_connected()) {
        logsne("Disconnecting from", as.character(self), "...")
        DBI::dbDisconnect(self$conn)
      }
      return(invisible(self))
    },
    is_connected = function() {
      return(!is.null(self$conn) && DBI::dbIsValid(self$conn))
    },
    finalize = function() {
      self$disconnect()
    },
    get_column = function(col, tbl) {
      self$execute(sprintf("SELECT `%s` FROM `%s`", col, tbl))
    },
    get_colnames = function(tbl) {
      DBI::dbListFields(self$conn, tbl)
    },
    get_table = function(tbl) {
      DBI::dbReadTable(self$conn, tbl)
    },
    get_tables = function() {
      logsne("Reading tables...")
      tbls <- list()
      for (tbl in TABLES) {
        tbls[[tbl]] <- DBI::dbReadTable(self$conn, tbl)
      }
      rownames(tbls$users) <- tbls$users$user_id
      rownames(tbls$Models) <- tbls$Models$ID
      rownames(tbls$Datasets) <- tbls$Datasets$ID
      return(tbls)
    },
    execute = function(query, params = NULL) {
      # Execute SQL query. Examples:
      # send("INSERT INTO cars (speed, dist) VALUES (1, 1), (2, 2), (3, 3)") or
      # send(query = "UPDATE Appstate SET resource_value = ? WHERE
      #   user_id = ? AND resource_id = ?", params = list(url, user_id, "url"))
      if (grepl("SELECT", query)) {
        return(DBI::dbGetQuery(self$conn, query, params = params))
      } else {
        return(DBI::dbExecute(self$conn, query, params))
      }
    },
    get_valid_cookies = function(expiry = 7) {
      expiry_date <- as.character(lubridate::now() - lubridate::days(expiry))
      df <- self$execute(
        query = "SELECT * FROM mapping_users_sessions WHERE login_time > ?",
        params = list(expiry_date)
      )
      return(df)
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
        DBI::dbWriteTable(self$conn, "mapping_users_sessions", ., append = TRUE)
    },
    # Auto generated and not tested
    appendTable = function(...) DBI::dbAppendTable(self$conn, ...),
    canConnect = function(...) DBI::dbCanConnect(self$conn),
    createTable = function(...) DBI::dbCreateTable(self$conn),
    existsTable = function(...) DBI::dbExistsTable(self$conn),
    fetch = function(...) DBI::dbFetch(self$conn, ...),
    getInfo = function(...) DBI::dbGetInfo(self$conn, ...),
    getRowCount = function(...) DBI::dbGetRowCount(self$conn, ...),
    getRowsAffected = function(...) DBI::dbGetRowsAffected(self$conn, ...),
    getStatement = function(...) DBI::dbGetStatement(self$conn, ...),
    hasCompleted = function(...) DBI::dbHasCompleted(self$conn, ...),
    isReadOnly = function(...) DBI::dbIsReadOnly(self$conn, ...),
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
  ),
  private = list(
    connect_to_sqlite_dbms = function() {
      if (!file.exists(self$filepath)) {
        logsne("File", self$filepath, "does not exist")
        catsne("Copying", self$default_filepath, "to", self$filepath, "...")
        file.copy(default_filepath, self$filepath)
      }
      self$conn <- DBI::dbConnect(
        drv = RSQLite::SQLite(),
        dbname = self$filepath
      )
      return(invisible(self))
    },
    connect_to_postgres_dbms = function() {
      self$conn <- DBI::dbConnect(
        drv = RPostgres::Postgres(),
        host = self$hostname,
        port = self$port,
        dbname = self$database,
        user = self$username,
        password = self$password
      )
      if (length(DBI::dbListTables(self$conn)) == 0) {
        init_postgres_db_with_default_values()
      }
      return(invisible(self))
    },
    init_postgres_db_with_default_values = function() {
      log0ne(
        "Found no tables in `", self$hostname, ":", self$port, "/",
        self$database, "`:\nInitializing with default values ..."
      )
      default_db <- DBI::dbConnect(RSQLite::SQLite(), self$default_filepath)
      on.exit(DBI::dbDisconnect(default_db))
      for (tbl in DBI::dbListTables(default_imlui_db)) {
        DBI::dbWriteTable(self$conn, tbl, value = DBI::dbReadTable(default_db, tbl))
      }
    }
  )
)

as.character.DB <- function(db) {
  if (db$type == "sqlite") {
    db$filepath
  } else {
    paste0(db$dbname, "@", db$hostname, ":", db$port)
  }
}
