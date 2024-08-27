# Interface with Dryad

# The DOI of the ButtR repo in Dryad
BUTTR_DOI <- "doi:10.5061/dryad.s1rn8pkdv" # TODO For now this is just some random project

# Dryad API constants
DRYAD_API_SITE <- "https://datadryad.org"
DRYAD_API_BASE <- paste0(DRYAD_API_SITE, "/api/v2/")

# Given a URL, downloads it and interprets it as JSON.
getJSON <- function(url) {
  resp <- httr::GET(url)
  httr::stop_for_status(resp)
  txt <- httr::content(resp, "text", encoding = "UTF-8")
  jsonlite::fromJSON(txt, flatten = TRUE)
}

# Given a Dryad dataset DOI, returns a data frame with the names of all files
# available for download, together with their download URLs
#
# @returns data frame with 2 columns, `file` with the name of the file stored in
#   Dryad, and `url` with the URL that can be used to retrieve the file.
#
listFilesInDryad <- function(doi) {
  # Given DOI, get link for data set
  edoi <- utils::URLencode(doi, reserved = TRUE)
  url <- paste0(DRYAD_API_BASE,"datasets/", edoi)
  vj <- getJSON(url)
  # Now query for the files in the dataset
  filesURL <- paste0(DRYAD_API_SITE, vj$`_links`$`stash:version`, "/files")
  fj <- getJSON(filesURL)

  # Available files
  data.frame(
    file = fj$`_embedded`[[1]]$path,
    url = paste0(DRYAD_API_SITE, fj$`_embedded`[[1]]$`_links.stash:download.href`)
  )
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
ListDbsFiles <- function() listFilesInDryad(BUTTR_DOI)


#Uncomment this for debugging/development using local repository. For unit
#tests, use testthat::local_mocked_bindings instead ListDbsFiles = function()
#listLocalFiles(testthat::test_path("testdata/repo"))
