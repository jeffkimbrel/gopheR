read_bundle <- function(bundle_path,
                        db_path = NULL,
                        backup = TRUE,
                        ...) {

  db_path <- gopher_db_path(db_path)

  if (!file.exists(bundle_path)) {
    cli::cli_abort("Bundle file does not exist: {.path {bundle_path}}")
  }

  if (isTRUE(backup)) {
    backup_info <- backup_db(db_path = db_path)
  } else {
    backup_info <- NULL
  }

  invisible(list(
    bundle_path = normalizePath(bundle_path, mustWork = TRUE),
    db_path = normalizePath(db_path, mustWork = TRUE),
    backup = backup_info
  ))
}



backup_db <- function(db_path = NULL,
                      backup_dir = NULL,
                      timestamp = format(Sys.time(), "%Y%m%dT%H%M%S"),
                      overwrite = FALSE) {

  if (is.null(db_path)) {
    db_path <- gopher_db_path()
  }

  if (!file.exists(db_path)) {
    cli::cli_abort("Database file does not exist: {.path {db_path}}")
  }

  if (is.null(backup_dir)) {
    backup_dir <- file.path(dirname(db_path), "backups")
  }

  dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)

  db_name <- basename(db_path)
  db_stem <- sub("\\.[^.]+$", "", db_name)
  db_ext  <- sub("^.*(\\.[^.]+)$", "\\1", db_name)
  if (identical(db_ext, db_name)) db_ext <- ""

  backup_path <- file.path(
    backup_dir,
    paste0(db_stem, ".pre_ingest.", timestamp, db_ext)
  )

  if (file.exists(backup_path) && !isTRUE(overwrite)) {
    cli::cli_abort("Backup file already exists: {.path {backup_path}}")
  }

  ok <- file.copy(
    from = db_path,
    to = backup_path,
    overwrite = overwrite,
    copy.mode = TRUE,
    copy.date = TRUE
  )

  if (!ok || !file.exists(backup_path)) {
    cli::cli_abort("Failed to create backup: {.path {backup_path}}")
  }

  src_md5 <- unname(tools::md5sum(db_path))
  bak_md5 <- unname(tools::md5sum(backup_path))

  if (!identical(src_md5, bak_md5)) {
    cli::cli_abort(c(
      "Backup checksum mismatch.",
      "x" = "Source: {src_md5}",
      "x" = "Backup: {bak_md5}"
    ))
  }

  cli::cli_inform(c(
    "v" = "Database backup verified.",
    "i" = "MD5: {src_md5}",
    " " = "{.path {backup_path}}"
  ))

  list(
    db_path = normalizePath(db_path, mustWork = TRUE),
    backup_path = normalizePath(backup_path, mustWork = TRUE),
    md5 = src_md5,
    timestamp = timestamp
  )
}
