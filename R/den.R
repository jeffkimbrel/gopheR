#' Find the root of a gopherDen project
#'
#' Walks up the directory tree from a given path looking for a `den.yaml` file.
#' Returns the den root path if found, `NULL` otherwise.
#'
#' @param path Character. Starting path to search from. Defaults to the
#'   directory of the active gopheR database.
#'
#' @returns Character path to the den root, or `NULL` if not inside a den.
#' @export

find_den_root <- function(path = NULL) {
  if (is.null(path)) {
    path <- getwd()
  }

  path <- normalizePath(path, mustWork = FALSE)

  while (TRUE) {
    if (file.exists(file.path(path, "den.yaml"))) return(path)
    parent <- dirname(path)
    if (identical(parent, path)) return(NULL)
    path <- parent
  }
}

#' Initialize a new gopherDen project
#'
#' Creates a new den folder with the standard structure: database, config,
#' RStudio project file, git repository, and archive folders. The den is the
#' unit of work for a gopheR project -- a single folder that holds the database,
#' bundle archives, and provenance history.
#'
#' @param path Character. Parent directory where the den folder will be created.
#' @param name Character. Name of the den. Used as the folder name, database
#'   filename (`{name}.den`), and RStudio project name.
#' @param create_examples Logical. If `TRUE` (default), populates an `examples/`
#'   folder with a starter bundle and pre-built example database.
#'
#' @returns Invisibly returns the path to the new den folder.
#' @export
#'
#' @examples
#' \dontrun{
#' initialize_den("~/projects", "ARW_metagenomics")
#' initialize_den("~/projects", "my_project", create_examples = FALSE)
#' }

initialize_den <- function(path, name, create_examples = FALSE) {

  path     <- normalizePath(path, mustWork = FALSE)
  den_path <- file.path(path, name)

  if (dir.exists(den_path)) {
    cli::cli_abort(c(
      "Directory already exists: {.path {den_path}}",
      "i" = "Choose a different name or remove the existing folder."
    ))
  }

  # --- Folder structure ---
  dirs <- c(
    den_path,
    file.path(den_path, "archive", "dens"),
    file.path(den_path, "archive", "bundles")
  )
  # examples/ intentionally omitted until create_examples is implemented

  for (d in dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  # --- Starter database as {name}.den ---
  starter <- system.file("extdata", "starter_db.sqlite", package = "gopheR")
  if (!nzchar(starter) || !file.exists(starter)) {
    cli::cli_abort(c(
      "Could not find starter database in {.pkg gopheR}.",
      "i" = "Try reinstalling gopheR."
    ))
  }

  den_file <- file.path(den_path, paste0(name, ".den"))
  ok <- file.copy(starter, den_file)
  if (!ok) cli::cli_abort("Failed to create database at {.path {den_file}}")

  # --- den.yaml ---
  yaml_lines <- c(
    paste0("name: ", name),
    paste0("database: ", basename(den_file)),
    paste0("created: ", format(Sys.Date(), "%Y-%m-%d"))
  )
  writeLines(yaml_lines, file.path(den_path, "den.yaml"))

  # --- .Rproj ---
  rproj_lines <- c(
    "Version: 1.0",
    "",
    "RestoreWorkspace: Default",
    "SaveWorkspace: Default",
    "AlwaysSaveHistory: Default",
    "",
    "EnableCodeIndexing: Yes",
    "UseSpacesForTab: Yes",
    "NumSpacesForTab: 2",
    "Encoding: UTF-8",
    "",
    "RnwWeave: Sweave",
    "LaTeX: pdfLaTeX"
  )
  writeLines(rproj_lines, file.path(den_path, paste0(name, ".Rproj")))

  # --- .gitignore ---
  gitignore_lines <- c(
    ".DS_Store",
    "*.tmp",
    ".Rproj.user/",
    ".Rhistory",
    ".RData",
    "",
    "# .den database and archive dumps are tracked in git.",
    "# To exclude the binary database uncomment the line below:",
    "# *.den"
  )
  writeLines(gitignore_lines, file.path(den_path, ".gitignore"))

  # --- Claude skill ---
  skill_src <- system.file(".claude", "skills", "fill-bundle.md", package = "gopheR")
  if (nzchar(skill_src) && file.exists(skill_src)) {
    skill_dir <- file.path(den_path, ".claude", "skills")
    dir.create(skill_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(skill_src, file.path(skill_dir, "fill-bundle.md"))
  }

  # --- git init ---
  git_out <- system2("git", c("init", den_path), stdout = TRUE, stderr = TRUE)
  git_ok  <- !inherits(git_out, "error") && !any(grepl("^fatal:", git_out))
  if (!git_ok) {
    cli::cli_alert_warning("git init failed -- initialize manually if needed.")
  }

  # --- Summary ---
  cli::cli_alert_success("Den initialized: {.path {den_path}}")
  cli::cli_bullets(c(
    " " = "Database : {.path {den_file}}",
    " " = "Config   : {.path {file.path(den_path, 'den.yaml')}}",
    " " = "RStudio  : {.path {file.path(den_path, paste0(name, '.Rproj'))}}",
    " " = "Archive  : {.path {file.path(den_path, 'archive/')}}"
  ))

  invisible(den_path)
}
