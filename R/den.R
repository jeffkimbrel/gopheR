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
#' @param template_den Character. Optional path to an existing `.den` file or
#'   den folder. If provided, the new den's spec tables (`object_type`,
#'   `object_subtype`, `edge_spec`, `object_result_spec`, `edge_result_spec`,
#'   `object_file_type_spec`, `workflow_file_type_spec`) are replaced with
#'   those from the template. Actual data (objects, edges, workflows, results,
#'   files, people) is never copied.
#'
#' @returns Invisibly returns the path to the new den folder.
#' @export
#'
#' @examples
#' \dontrun{
#' initialize_den("~/projects", "ARW_metagenomics")
#' initialize_den("~/projects", "my_project", create_examples = FALSE)
#' initialize_den("~/projects", "new_project", template_den = "~/projects/ARW_metagenomics/")
#' }

initialize_den <- function(path, name, create_examples = FALSE, template_den = NULL) {

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
    file.path(den_path, "archive", "bundles"),
    file.path(den_path, "archive", "agent")
  )
  # examples/ intentionally omitted until create_examples is implemented

  for (d in dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  # --- Starter database as {name}.den ---
  starter <- system.file("extdata", "starter_db.den", package = "gopheR")
  if (!nzchar(starter) || !file.exists(starter)) {
    cli::cli_abort(c(
      "Could not find starter database in {.pkg gopheR}.",
      "i" = "Try reinstalling gopheR."
    ))
  }

  den_file <- file.path(den_path, paste0(name, ".den"))
  ok <- file.copy(starter, den_file)
  if (!ok) cli::cli_abort("Failed to create database at {.path {den_file}}")

  # --- Template spec copy ---
  template_agent_context <- NULL
  if (!is.null(template_den)) {
    template_path <- resolve_den_path(template_den)
    copy_den_spec(from = template_path, to = den_file)
    template_agent_context <- read_yaml_block(
      file.path(dirname(template_path), "den.yaml"),
      key = "agent_context"
    )
    cli::cli_alert_info("Spec tables copied from template: {.path {template_path}}")
  }

  # --- den.yaml ---
  agent_context_lines <- if (!is.null(template_agent_context)) {
    c("", "agent_context: |", template_agent_context)
  } else {
    c(
      "",
      "# agent_context is read by the fill-bundle agent (/fill-bundle in Claude Code).",
      "# Describe your project's naming conventions, pipeline steps, and ID formats",
      "# so the agent can make accurate first-pass inferences from your files.",
      "# agent_context: |",
      "#   Samples from the XYZ site (IDs: XYZ_S01-XYZ_S20).",
      "#   Pipeline: MEGAHIT assembly → MetaWRAP binning → CheckM2 quality → GTDB-Tk taxonomy.",
      "#   Genome IDs: m{site}_{bin_zero_padded_3}  e.g. mXYZ_001",
      "#   Workflow IDs: {tool}_{site}_{YYYY-MM}  e.g. megahit_XYZ_2025-03",
      "#   Files live at: /data/XYZ/"
    )
  }

  yaml_lines <- c(
    paste0("name: ", name),
    paste0("database: ", basename(den_file)),
    paste0("created: ", format(Sys.Date(), "%Y-%m-%d")),
    agent_context_lines
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

  # --- Claude custom command ---
  cmd_src <- system.file(".claude", "commands", "fill-bundle.md", package = "gopheR")
  if (nzchar(cmd_src) && file.exists(cmd_src)) {
    cmd_dir <- file.path(den_path, ".claude", "commands")
    dir.create(cmd_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(cmd_src, file.path(cmd_dir, "fill-bundle.md"))
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

# Extract a YAML block scalar value (key: |) as a character vector of indented lines.
# Returns NULL if the key is absent or has no content.
read_yaml_block <- function(yaml_path, key) {
  if (!file.exists(yaml_path)) return(NULL)
  lines     <- readLines(yaml_path, warn = FALSE)
  key_idx   <- grep(paste0("^", key, ":\\s*\\|"), lines)
  if (length(key_idx) == 0) return(NULL)
  start     <- key_idx[1] + 1
  if (start > length(lines)) return(NULL)
  block <- character(0)
  for (i in start:length(lines)) {
    if (nzchar(lines[i]) && !grepl("^\\s", lines[i])) break
    block <- c(block, lines[i])
  }
  if (length(block) == 0) NULL else block
}

# Resolve a den directory or .den file path to the actual .den file path.
resolve_den_path <- function(path) {
  path <- normalizePath(path, mustWork = FALSE)

  if (file.exists(path) && !dir.exists(path)) {
    return(path)
  }

  if (dir.exists(path)) {
    yaml_path <- file.path(path, "den.yaml")
    if (!file.exists(yaml_path)) {
      cli::cli_abort("No den.yaml found in template directory: {.path {path}}")
    }
    lines   <- readLines(yaml_path, warn = FALSE)
    db_line <- grep("^database:", lines, value = TRUE)
    if (length(db_line) == 0) {
      cli::cli_abort("den.yaml in {.path {path}} has no 'database:' field.")
    }
    db_file <- trimws(sub("^database:\\s*", "", db_line[1]))
    full    <- file.path(path, db_file)
    if (!file.exists(full)) {
      cli::cli_abort("Template database not found: {.path {full}}")
    }
    return(full)
  }

  cli::cli_abort("Template den not found: {.path {path}}")
}

# Copy spec tables from one .den file to another, replacing existing rows.
copy_den_spec <- function(from, to) {
  spec_tables <- c(
    "object_type", "object_subtype",
    "edge_spec",
    "object_result_spec", "edge_result_spec",
    "object_file_type_spec", "workflow_file_type_spec"
  )

  con_from <- DBI::dbConnect(RSQLite::SQLite(), from, extended_types = TRUE)
  con_to   <- DBI::dbConnect(RSQLite::SQLite(), to,   extended_types = TRUE)
  on.exit({
    DBI::dbDisconnect(con_from)
    DBI::dbDisconnect(con_to)
  }, add = TRUE)

  from_tables <- DBI::dbListTables(con_from)
  to_tables   <- DBI::dbListTables(con_to)

  for (tbl in spec_tables) {
    if (!tbl %in% from_tables) next
    if (!tbl %in% to_tables)   next
    data <- DBI::dbReadTable(con_from, tbl)
    DBI::dbExecute(con_to, paste0("DELETE FROM \"", tbl, "\""))
    if (nrow(data) > 0) DBI::dbAppendTable(con_to, tbl, data)
  }
}

#' Migrate an existing den database to the current schema
#'
#' Applies schema changes that cannot be handled by simple column additions,
#' such as dropping NOT NULL constraints. Safe to run on dens that are already
#' up to date — changes are only applied when needed.
#'
#' @param db_path Character. Path to a `.den` SQLite file or den directory.
#' @returns Invisibly returns `db_path`.
#' @export
migrate_den_schema <- function(db_path) {
  db_path <- resolve_den_path(db_path)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path, extended_types = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  nullable_workflow_id <- list(
    object_result = "
      CREATE TABLE object_result_new (
        result_id    INTEGER PRIMARY KEY AUTOINCREMENT,
        object_id    TEXT NOT NULL,
        workflow_id  TEXT,
        key          TEXT NOT NULL,
        value        TEXT NOT NULL,
        unit         TEXT,
        FOREIGN KEY (object_id)   REFERENCES object(object_id)   ON DELETE CASCADE,
        FOREIGN KEY (workflow_id) REFERENCES workflow(workflow_id)
      )",
    edge_result = "
      CREATE TABLE edge_result_new (
        edge_result_id  INTEGER PRIMARY KEY AUTOINCREMENT,
        edge_id         INTEGER NOT NULL,
        workflow_id     TEXT,
        key             TEXT NOT NULL,
        value           TEXT NOT NULL,
        unit            TEXT,
        FOREIGN KEY (edge_id)     REFERENCES edge(edge_id)         ON DELETE CASCADE,
        FOREIGN KEY (workflow_id) REFERENCES workflow(workflow_id)
      )"
  )

  DBI::dbExecute(con, "PRAGMA foreign_keys = OFF")
  migrated <- character()

  for (tbl in names(nullable_workflow_id)) {
    schema <- DBI::dbGetQuery(con, sprintf(
      "SELECT sql FROM sqlite_master WHERE type='table' AND name='%s'", tbl
    ))$sql
    if (length(schema) == 0 || !grepl("workflow_id\\s+TEXT\\s+NOT NULL", schema)) next

    new_tbl  <- paste0(tbl, "_new")
    DBI::dbExecute(con, nullable_workflow_id[[tbl]])
    DBI::dbExecute(con, sprintf("INSERT INTO %s SELECT * FROM %s", new_tbl, tbl))
    DBI::dbExecute(con, sprintf("DROP TABLE %s", tbl))
    DBI::dbExecute(con, sprintf("ALTER TABLE %s RENAME TO %s", new_tbl, tbl))
    migrated <- c(migrated, tbl)
  }

  if ("edge_result" %in% migrated) {
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_edge_result_edge ON edge_result(edge_id)")
  }

  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")

  if (length(migrated) > 0) {
    cli::cli_alert_success("Migrated {.path {db_path}}: nullable workflow_id in {.val {migrated}}")
  } else {
    cli::cli_alert_info("{.path {db_path}} is already up to date.")
  }

  invisible(db_path)
}
