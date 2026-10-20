## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~ Ask Atlantis exported indexing commands    ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

.ra_ai_ensure_loaded <- local({
  loaded <- FALSE
  function() {
    if (isTRUE(loaded) && exists("ra_ai_index", mode = "function",
                                 inherits = TRUE)) {
      return(invisible(TRUE))
    }
    load_r <- system.file("ai", "load.R", package = "ReactiveAtlantis")
    if (!nzchar(load_r) || !file.exists(load_r)) {
      cand <- c(
        file.path(getwd(), "inst", "ai", "load.R"),
        file.path(getwd(), "..", "inst", "ai", "load.R"),
        file.path(getwd(), "..", "..", "inst", "ai", "load.R")
      )
      hit <- cand[file.exists(cand)]
      if (length(hit) == 0) {
        stop("Could not find inst/ai/load.R. Reinstall ReactiveAtlantis.",
             call. = FALSE)
      }
      load_r <- hit[1]
    }
    sys.source(load_r, envir = topenv(), keep.source = FALSE)
    loaded <<- TRUE
    invisible(TRUE)
  }
})

##' Build or incrementally update the Ask Atlantis local index
##'
##' Reads the JSON sources manifest, chunks manuals/code/calibration notes,
##' and stores passages in a local SQLite database with FTS5 search. Ollama
##' embeddings are requested when \code{embed = TRUE} and Ollama is reachable.
##' Unchanged files (matching hash and mtime) are skipped.
##'
##' The default manifest indexes a clearly marked fixture corpus plus this
##' package's own source. Download the CSIRO user-guide PDFs with
##' \code{download_atlantis_manuals()} and point
##' \code{RA_AI_SOURCES_CONFIG} at
##' \code{inst/ai/sources.atlantis-manuals.json}.
##'
##' @param sources_config Path to the JSON manifest. Default
##'   \code{RA_AI_SOURCES_CONFIG} or \code{inst/ai/sources.json}.
##' @param index_dir Directory for the SQLite index. Default
##'   \code{RA_AI_INDEX_DIR} or the ReactiveAtlantis user cache.
##' @param embed Request local Ollama embeddings (\code{/api/embed}).
##' @return Invisibly, a list of ingest diagnostics.
##' @export
##' @examples
##' \dontrun{
##' index_ask_atlantis()
##' index_ask_atlantis(embed = FALSE)
##' }
index_ask_atlantis <- function(sources_config = NULL, index_dir = NULL,
                               embed = TRUE) {
  .ra_ai_ensure_loaded()
  ra_ai_index(sources_config = sources_config, index_dir = index_dir,
              embed = embed)
}

##' Print Ask Atlantis index health
##'
##' Lists indexed sources, versions, chunk counts, ignored paths and
##' ingestion failures.
##'
##' @param index_dir Directory for the SQLite index
##' @return Invisibly, the diagnostics list
##' @export
##' @examples
##' \dontrun{
##' diagnose_ask_atlantis_index()
##' }
diagnose_ask_atlantis_index <- function(index_dir = NULL) {
  .ra_ai_ensure_loaded()
  diag <- ra_ai_diagnose(index_dir = index_dir)
  ra_ai_print_diagnose(diag)
}

##' Download CSIRO Atlantis user-guide PDFs into a local corpus directory
##'
##' Fetches the user-guide PDFs listed on the public Accessing Atlantis
##' page into \code{dest_dir}. The files are CSIRO copyright and are
##' \strong{not} redistributed with this package. Index them afterwards
##' with \code{index_ask_atlantis()} pointed at
##' \code{inst/ai/sources.atlantis-manuals.json}.
##'
##' @param dest_dir Local corpus directory. Default \code{RA_AI_CORPUS_DIR}
##'   or the ReactiveAtlantis user data directory.
##' @param overwrite Re-download files that already exist.
##' @return Invisibly, a list with \code{dest_dir} and a file-status table.
##' @export
##' @examples
##' \dontrun{
##' download_atlantis_manuals()
##' Sys.setenv(RA_AI_SOURCES_CONFIG =
##'              system.file("ai", "sources.atlantis-manuals.json",
##'                          package = "ReactiveAtlantis"))
##' index_ask_atlantis(embed = FALSE)
##' }
download_atlantis_manuals <- function(dest_dir = NULL, overwrite = FALSE) {
  .ra_ai_ensure_loaded()
  ra_ai_download_atlantis_manuals(dest_dir = dest_dir,
                                  overwrite = overwrite,
                                  verbose = TRUE)
}
