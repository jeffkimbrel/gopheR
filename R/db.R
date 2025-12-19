#' Get path to gopheR database
#'
#' @param path (*string*) path to db
#' @param db (*string*) database name
#'
#' @returns file path to gopheR_db.sqlite
#' @export

gopher_db_path <- function(path = here::here("data"), db = "gopheR_db.sqlite") {
  gopher_path = file.path(path, db)
  if (file.exists(gopher_path)) {
    return(gopher_path)
  } else {
    stop(glue::glue("no file found at {gopher_path}"), call. = F)
  }
}


#' Database connection
#'
#' @export

conn <- function() {
  DBI::dbConnect(RSQLite::SQLite(), gopher_db_path())
}

#' Use a connection
#'
#' @param .f
#'
#' @export

with_con <- function(.f) {
  c <- conn()
  on.exit(DBI::dbDisconnect(c), add = TRUE)
  .f(c)
}

#' Extract native id from a UID like "mag:M001" (R-side)
#'
#' @param uid
#'
#' @export

uid_native <- function(uid) {
  sub("^.*?:", "", uid)
}

#' DBI helper
#'
#' @param c connection
#' @param sql db statement
#'
#' @export

q <- function(c, sql, ...) {
  DBI::dbGetQuery(c, glue::glue(sql, .open = "{", .close = "}", ...))
}
