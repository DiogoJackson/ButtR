# Interface with Zenodo

# The deposition ID of the ButtR repo in Zenodo
BUTTR_DEPOSITION <- "15881960"

# Zenodo API constants
ZENODO_REC_URL <- "https://zenodo.org/api/records/"



# Given a URL, downloads it and interprets it as JSON.
getJSON <- function(url) {
  resp <- httr::GET(url)
  httr::stop_for_status(resp)
  txt <- httr::content(resp, "text", encoding = "UTF-8")
  jsonlite::fromJSON(txt, flatten = TRUE)
}

# Given a Dryad dataset deposition ID, returns a data frame with the names of all files
# available for download, together with their download URLs
#
# @returns data frame with 2 columns, `file` with the name of the file stored in
#   Dryad, and `url` with the URL that can be used to retrieve the file.
#
listFilesInZenodo <- function(deposition) {
  # List the Zenodo record
  rec <- getJSON(paste0(ZENODO_REC_URL, deposition))

  # Available files
  stats::setNames(rec$files[, c("key", "links.self", "size")],
                  c("file", "url", "size"))
}

# Function for testing without using Dryad. This is a drop-in replacement for
# listFilesInDryad. It may not work correctly for files with spaces in their
# names.
#
# @examples
# # To use in testing, do something like:
# testthat::local_mocked_bindings(ListDbsFiles = function() {
#    listLocalFiles(testthat::test_path("testdata/repo"))
# })
# get_species(...)
#
listLocalFiles <- function(path, encode = FALSE) {
  files <- list.files(path)
  # Remove unwanted files
  files <- grep("^~\\$", files, invert = TRUE, value = TRUE)
  # Construct URLs
  urls <- paste0("file://", normalizePath(file.path(path, files), winslash = "/"))
  if (encode) {
    urls <- utils::URLencode(urls)
  }
  data.frame(
    file = files,
    url = urls
  )
}

#################################################################################

# Private function to get list of available files in the database. This is the
# function to call to obtain formation about the database contents. Done this
# way to facilitate testing without using Dryad, but without exposing something
# publicly
ListDbsFiles <- function() listFilesInZenodo(BUTTR_DEPOSITION)


#Uncomment this for debugging/development using local repository. For unit
#tests, use testthat::local_mocked_bindings instead
# ListDbsFiles = function() listLocalFiles(testthat::test_path("testdata/repo"))
