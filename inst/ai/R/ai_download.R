## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~ Ask Atlantis: download CSIRO user guides   ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##
## The PDFs are CSIRO copyright. This helper fetches them into a local
## directory for indexing. It never copies them into the git tree.

ra_ai_atlantis_manuals_catalog_path <- function() {
  file.path(ra_ai_paths()$ai, "atlantis-manuals.json")
}

##' Load the CSIRO manuals catalog (URLs only; not the PDF bytes).
ra_ai_load_atlantis_manuals_catalog <- function(catalog = NULL) {
  if (is.list(catalog) && !is.null(catalog$manuals)) {
    return(catalog)
  }
  path <- if (is.character(catalog) && length(catalog) > 0 &&
              nzchar(catalog[1])) {
    catalog[1]
  } else {
    ra_ai_atlantis_manuals_catalog_path()
  }
  if (!file.exists(path)) {
    stop("Atlantis manuals catalog not found: ", path, call. = FALSE)
  }
  man <- ra_ai_read_json(path)
  if (is.null(man$manuals) || length(man$manuals) == 0) {
    stop("Atlantis manuals catalog has no 'manuals' entries: ", path,
         call. = FALSE)
  }
  man
}

ra_ai_looks_like_pdf <- function(path) {
  if (!file.exists(path)) return(FALSE)
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  magic <- readBin(con, what = "raw", n = 5L)
  if (length(magic) < 4L) return(FALSE)
  identical(rawToChar(magic[1:4]), "%PDF")
}

##' Fetch one URL to destfile. Tests may replace this with
##' options(reactiveatlantis.download_file = function(url, destfile) ...).
ra_ai_http_download <- function(url, destfile, timeout_s = 180, expect = "pdf") {
  check_type <- function(path) {
    if (identical(expect, "pdf")) return(ra_ai_looks_like_pdf(path))
    if (!file.exists(path) || file.size(path) == 0) return(FALSE)
    head <- tolower(paste(readLines(path, n = 20, warn = FALSE), collapse = " "))
    grepl("<html|<!doctype html", head)
  }
  transport <- getOption("reactiveatlantis.download_file", default = NULL)
  if (is.function(transport)) {
    return(transport(url = url, destfile = destfile))
  }

  if (ra_ai_need_pkg("httr2")) {
    req <- httr2::request(url)
    req <- httr2::req_timeout(req, timeout_s)
    req <- httr2::req_error(req, is_error = function(resp) FALSE)
    resp <- tryCatch(httr2::req_perform(req, path = destfile),
                     error = function(e) e)
    if (inherits(resp, "error")) {
      if (file.exists(destfile)) unlink(destfile)
      return(list(ok = FALSE, status = NA_integer_, url = url,
                  error = resp$message, bytes = NA_real_))
    }
    status <- httr2::resp_status(resp)
    bytes <- suppressWarnings(file.size(destfile))
    if (status < 200 || status >= 300 || !check_type(destfile)) {
      if (file.exists(destfile)) unlink(destfile)
      return(list(ok = FALSE, status = status, url = url,
                  error = paste("HTTP", status, "or not a", toupper(expect), "file"),
                  bytes = bytes))
    }
    return(list(ok = TRUE, status = status, url = url,
                error = NULL, bytes = bytes))
  }

  ok <- tryCatch({
    utils::download.file(url, destfile, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) e)
  if (!isTRUE(ok)) {
    if (file.exists(destfile)) unlink(destfile)
    msg <- if (inherits(ok, "error")) ok$message else "download.file failed"
    return(list(ok = FALSE, status = NA_integer_, url = url,
                error = msg, bytes = NA_real_))
  }
  bytes <- suppressWarnings(file.size(destfile))
  if (!check_type(destfile)) {
    unlink(destfile)
    return(list(ok = FALSE, status = NA_integer_, url = url,
                error = paste("downloaded file is not a", toupper(expect), "file"), bytes = bytes))
  }
  list(ok = TRUE, status = 200L, url = url, error = NULL, bytes = bytes)
}

ra_ai_manual_urls <- function(entry) {
  listed <- as.character(entry$listed_url %||% character())
  extras <- as.character(entry$urls %||% character())
  unique(c(listed, extras)[nzchar(c(listed, extras))])
}

ra_ai_write_corpus_notice <- function(dest_dir, catalog) {
  notice <- c(
    "CSIRO Atlantis user-guide PDFs",
    "",
    paste("Listed from:", catalog$listed_from %||%
            "https://research.csiro.au/atlantis/home/links/"),
    "",
    "Copyright: Commonwealth Scientific and Industrial Research Organisation (CSIRO).",
    "These files are for local information, research or study only.",
    "Do not change them, do not strip notices, and do not commit them to git.",
    "See https://www.csiro.au/en/about/Policies/Legal/Copyright",
    "",
    as.character(catalog$copyright %||% "")
  )
  writeLines(notice, file.path(dest_dir, "NOTICE.txt"))
}

##' True when a retrieved chunk came from a downloaded CSIRO user-guide PDF.
ra_ai_is_csiro_manual_chunk <- function(path, source_id = "",
                                        version = "") {
  hay <- paste(path, source_id, version)
  grepl("AtlantisUserGuide|atlantis-user-guide", hay, ignore.case = TRUE) &&
    !grepl("FIXTURE", hay, ignore.case = TRUE)
}

##' Download CSIRO Atlantis user-guide PDFs into dest_dir.
##'
##' @param dest_dir Local corpus directory. Default RA_AI_CORPUS_DIR or
##'   the ReactiveAtlantis user data dir.
##' @param catalog Catalog list or path to atlantis-manuals.json
##' @param overwrite Re-download files that already exist
##' @param verbose Print one line per file
##' @return A list with dest_dir and a data.frame of file results
ra_ai_download_atlantis_manuals <- function(dest_dir = NULL,
                                           catalog = NULL,
                                           overwrite = FALSE,
                                           verbose = FALSE) {
  catg <- ra_ai_load_atlantis_manuals_catalog(catalog)
  if (is.null(dest_dir) || !nzchar(as.character(dest_dir)[1])) {
    dest_dir <- ra_ai_default_corpus_dir()
  }
  dest_dir <- normalizePath(dest_dir, winslash = "/", mustWork = FALSE)
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }
  ra_ai_write_corpus_notice(dest_dir, catg)

  rows <- list()
  for (entry in catg$manuals) {
    filename <- as.character(entry$filename %||% basename(entry$listed_url))[1]
    destfile <- file.path(dest_dir, filename)
    urls <- ra_ai_manual_urls(entry)
    status <- "failed"
    used_url <- NA_character_
    err <- "no URLs listed"
    bytes <- NA_real_
    http_status <- NA_integer_

    if (!isTRUE(overwrite) && ra_ai_looks_like_pdf(destfile)) {
      status <- "skipped"
      used_url <- NA_character_
      err <- NA_character_
      bytes <- suppressWarnings(file.size(destfile))
    } else {
      for (u in urls) {
        got <- ra_ai_http_download(u, destfile)
        http_status <- got$status
        if (isTRUE(got$ok)) {
          status <- "downloaded"
          used_url <- got$url
          err <- NA_character_
          bytes <- got$bytes
          break
        }
        err <- got$error %||% "download failed"
        used_url <- u
      }
    }

    if (isTRUE(verbose)) {
      msg <- paste0(filename, ": ", status)
      if (!is.na(used_url)) msg <- paste0(msg, " (", used_url, ")")
      if (!is.na(err)) msg <- paste0(msg, " — ", err)
      message(msg)
    }

    rows[[length(rows) + 1]] <- data.frame(
      id = as.character(entry$id %||% filename)[1],
      filename = filename,
      path = destfile,
      url = if (is.na(used_url)) "" else used_url,
      status = status,
      http_status = http_status,
      bytes = bytes,
      error = if (is.na(err)) "" else err,
      stringsAsFactors = FALSE
    )
  }

  files <- do.call(rbind, rows)
  list(
    dest_dir = dest_dir,
    ok = all(files$status %in% c("downloaded", "skipped")),
    files = files
  )
}

##' Fetch the maintained HTML manual pages into the local corpus dir.
##'
##' Not used at question time. Pages are not committed. Failures are
##' recorded; the rest of the app is unaffected.
ra_ai_download_html_manual <- function(dest_dir = NULL, overwrite = FALSE,
                                       verbose = TRUE) {
  if (is.null(dest_dir) || !nzchar(as.character(dest_dir)[1])) {
    dest_dir <- file.path(ra_ai_default_corpus_dir(), "AtlantisManual-html")
  }
  dest_dir <- normalizePath(dest_dir, winslash = "/", mustWork = FALSE)
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }
  pages <- list(
    list(id = "index", url = "https://atlantis-ecosystem-model.github.io/AtlantisManual/",
         file = "index.html"),
    list(id = "part1-index",
         url = "https://atlantis-ecosystem-model.github.io/AtlantisManual/user_guides/quarto_site/part1/",
         file = "part1-index.html")
  )
  rows <- lapply(pages, function(pg) {
    dest <- file.path(dest_dir, pg$file)
    status <- "error"
    err <- NA_character_
    if (file.exists(dest) && !isTRUE(overwrite)) {
      status <- "skipped"
    } else {
      ok <- tryCatch({
        res <- ra_ai_http_download(pg$url, dest, expect = "html")
        ## The downloader reports failure as a value, not an error (S22).
        if (is.list(res) && !isTRUE(res$ok)) {
          err <- as.character(res$error %||% "download failed")
          FALSE
        } else {
          TRUE
        }
      }, error = function(e) {
        err <<- e$message
        FALSE
      })
      if (isTRUE(ok) && file.exists(dest) && file.size(dest) > 0) {
        status <- "downloaded"
      } else if (!isTRUE(ok)) {
        status <- "error"
      }
    }
    if (isTRUE(verbose)) {
      message(pg$id, ": ", status, if (!is.na(err)) paste0(" (", err, ")") else "")
    }
    data.frame(
      id = pg$id, url = pg$url, path = dest, status = status,
      error = err, stringsAsFactors = FALSE
    )
  })
  files <- do.call(rbind, rows)
  invisible(list(dest_dir = dest_dir, ok = all(files$status %in% c("downloaded", "skipped")),
                 files = files))
}
