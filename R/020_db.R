#' @title R6 class representing a database
DB <- R6::R6Class(
  classname = "DB",
  lock_objects = FALSE,
  lock_class = FALSE,
  public = list(
    #' @field type todo
    type = NULL,
    #' @field filepath todo
    filepath = NULL,
    #' @field hostname todo
    hostname = NULL,
    #' @field port todo
    port = NULL,
    #' @field database todo
    database = NULL,
    #' @field username todo
    username = NULL,
    #' @field password todo
    password = NULL,
    #' @field conn todo
    conn = NULL,
    #' @field default_filepath todo
    default_filepath = system.file(
      "assets/sqlite/imlui_db.sqlite",
      package = "imlui"
    ),

    #' @description Create and connect a new DB object
    #' @param config Either list(type="sqlite", filepath="...") or
    #' list(type="postgres", hostname="...", port="...", database="...",
    #' username="...", password="...")
    #' @return A new `DB` object.
    initialize = function(config = pkg$imlui_config$dbms) {
      do.call(rlang::env_bind, c(self, config))
      return(invisible(self$connect()))
    },
    #' @description Connect to database
    connect = function() {
      infomsg("Connecting to", as.character(self), "...")
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
    #' @description todo
    disconnect = function() {
      if (self$is_connected()) {
        infomsg("Disconnecting from", as.character(self), "...")
        DBI::dbDisconnect(self$conn)
      }
      return(invisible(self))
    },
    #' @description todo
    is_connected = function() {
      return(!is.null(self$conn) && DBI::dbIsValid(self$conn))
    },
    #' @description todo
    finalize = function() {
      self$disconnect()
    },
    #' @description todo
    #' @param col column name
    #' @param tbl table name
    get_column = function(col, tbl) {
      self$execute(sprintf("SELECT `%s` FROM `%s`", col, tbl))
    },
    #' @description todo
    #' @param tbl table name
    get_colnames = function(tbl) {
      DBI::dbListFields(self$conn, tbl)
    },
    #' @description todo
    #' @param tbl table name
    get_table = function(tbl) {
      DBI::dbReadTable(self$conn, tbl)
    },
    #' @description todo
    get_tables = function() {
      infomsg("Reading tables...")
      tbls <- list()
      for (tbl in TABLES) {
        tbls[[tbl]] <- DBI::dbReadTable(self$conn, tbl)
      }
      rownames(tbls$users) <- tbls$users$user_id
      rownames(tbls$Models) <- tbls$Models$ID
      rownames(tbls$Datasets) <- tbls$Datasets$ID
      return(tbls)
    },
    #' @description todo
    #' @param query SQL query as charactern string (can include ?)
    #' @param params parameters for query used as substitute for ? in `query`
    #' @examples \dontrun{
    #'   execute("INSERT INTO cars (speed, dist) VALUES (1, 1), (2, 2), (3, 3)")
    #'   execute(
    #'     query = "UPDATE Appstate SET resource_value = ? WHERE
    #'              user_id = ? AND resource_id = ?",
    #'     params = list(url, user_id, "url"))
    #' }
    execute = function(query, params = NULL) {
      # Execute SQL query. Examples:
      if (grepl("SELECT", query)) {
        return(DBI::dbGetQuery(self$conn, query, params = params))
      } else {
        return(DBI::dbExecute(self$conn, query, params))
      }
    },
    #' @description todo
    #' @param expiry Expiry date in days
    get_valid_cookies = function(expiry = 7) {
      expiry_date <- as.character(lubridate::now() - lubridate::days(expiry))
      df <- self$execute(
        query = "SELECT * FROM mapping_users_sessions WHERE login_time > ?",
        params = list(expiry_date)
      )
      return(df)
    },
    #' @description This function must accept two parameters: user_id and
    #' session_id. It will be called whenever the user successfully logs in with
    #' a password. This function saves to your database. TODO: Based on
    #' shinyauthr example (cite!).
    #' @param user_id user_id
    #' @param session_id sessiond_id
    insert_cookie = function(user_id, session_id) {
      #
      tibble::tibble(
        user_id = user_id,
        session_id = session_id,
        login_time = as.character(lubridate::now())
      ) %>%
        DBI::dbWriteTable(self$conn, "mapping_users_sessions", ., append = TRUE)
    },
    # Auto generated and not tested
    #' @description todo
    #' @param ... todo
    appendTable = function(...) DBI::dbAppendTable(self$conn, ...),
    #' @description todo
    #' @param ... todo
    canConnect = function(...) DBI::dbCanConnect(self$conn),
    #' @description todo
    #' @param ... todo
    createTable = function(...) DBI::dbCreateTable(self$conn),
    #' @description todo
    #' @param ... todo
    existsTable = function(...) DBI::dbExistsTable(self$conn),
    #' @description todo
    #' @param ... todo
    fetch = function(...) DBI::dbFetch(self$conn, ...),
    #' @description todo
    #' @param ... todo
    getInfo = function(...) DBI::dbGetInfo(self$conn, ...),
    #' @description todo
    #' @param ... todo
    getRowCount = function(...) DBI::dbGetRowCount(self$conn, ...),
    #' @description todo
    #' @param ... todo
    getRowsAffected = function(...) DBI::dbGetRowsAffected(self$conn, ...),
    #' @description todo
    #' @param ... todo
    getStatement = function(...) DBI::dbGetStatement(self$conn, ...),
    #' @description todo
    #' @param ... todo
    hasCompleted = function(...) DBI::dbHasCompleted(self$conn, ...),
    #' @description todo
    #' @param ... todo
    isReadOnly = function(...) DBI::dbIsReadOnly(self$conn, ...),
    #' @description todo
    #' @param ... todo
    listObjects = function(...) DBI::dbListObjects(self$conn, ...),
    #' @description todo
    #' @param ... todo
    listResults = function(...) DBI::dbListResults(self$conn, ...),
    #' @description todo
    #' @param ... todo
    listTables = function(...) DBI::dbListTables(self$conn, ...),
    #' @description todo
    #' @param ... todo
    quoteIdentifier = function(...) DBI::dbQuoteIdentifier(self$conn, ...),
    #' @description todo
    #' @param ... todo
    quoteLiteral = function(...) DBI::dbQuoteLiteral(self$conn, ...),
    #' @description todo
    #' @param ... todo
    quoteString = function(...) DBI::dbQuoteString(self$conn, ...),
    #' @description todo
    #' @param ... todo
    readTable = function(...) DBI::dbReadTable(self$conn, ...),
    #' @description todo
    #' @param ... todo
    removeTable = function(...) DBI::dbRemoveTable(self$conn, ...),
    #' @description todo
    #' @param ... todo
    rollback = function(...) DBI::dbRollback(self$conn, ...),
    #' @description todo
    #' @param ... todo
    sendQuery = function(...) DBI::dbSendQuery(self$conn, ...),
    #' @description todo
    #' @param ... todo
    sendStatement = function(...) DBI::dbSendStatement(self$conn, ...),
    #' @description todo
    #' @param ... todo
    setDataMappings = function(...) DBI::dbSetDataMappings(self$conn, ...),
    #' @description todo
    #' @param ... todo
    writeTable = function(...) DBI::dbWriteTable(self$conn, ...)
  ),
  private = list(
    connect_to_sqlite_dbms = function() {
      if (!file.exists(self$filepath)) {
        infomsg("File", self$filepath, "does not exist")
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
      infomsg(glue(
        "Found no tables in `{self$hostname}:{self$port}/{self$database}`. ",
        "Doing initializing with default values ..."
      ))
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
