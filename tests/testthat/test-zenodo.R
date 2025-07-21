library(testthat)

test_that("zenodo interface works", {
  # Get list of files from an arbitrary repo (animaltraits)
  # Use an arbitrary repository so we can test before the real repository exists
  # (https://zenodo.org/records/6468938)
  f <- listFilesInZenodo("6468938")

  # This is what we expect to get. Don't assume order
  # NOTE that testing uses locale C, which is different from interactive use
  #Sys.setlocale("LC_COLLATE", "C")
  f <- f[order(f$file), ]
  expected <- structure(list(file = c("LICENSE", "animaltraits.github.io-1.0.7.zip",
                                      "column-documentation.csv", "observations.csv", "observations.xlsx"),
                             url = c("https://zenodo.org/api/records/6468938/files/LICENSE/content",
                                     "https://zenodo.org/api/records/6468938/files/animaltraits.github.io-1.0.7.zip/content",
                                     "https://zenodo.org/api/records/6468938/files/column-documentation.csv/content",
                                     "https://zenodo.org/api/records/6468938/files/observations.csv/content",
                                     "https://zenodo.org/api/records/6468938/files/observations.xlsx/content"
                             )),
                        row.names = c(2L, 4L, 5L, 1L, 3L),
                        class = "data.frame")

  expect_equal(f, expected)

  # Download column-documentation.csv
  tmpnm <- tempfile(f$file[3])
  expect_error(utils::download.file(f$url[3], destfile = tmpnm, quiet = TRUE), NA)
  # Check it has the expected content
  line1 <- readLines(tmpnm, n = 1)
  # This is the first line in the column-documentation.csv file from the animaltraits dataset
  expect_equal(line1, "\"Column\",\"Description\",\"Defined by\"")
})

test_that("correct version", {
  # Check that we are finding the latest version from Dryad
  f <- listFilesInZenodo("6468938")
  # There should be 1 file called animaltraits.github.io-1.0.7.zip
  zip <- grep(".*\\.zip$", f$file, value = TRUE)
  # Earlier versions had different file names
  expect_equal(length(zip), 1)
  expect_equal(zip, "animaltraits.github.io-1.0.7.zip")
})

test_that("local file interface works", {
  # This is to test that the test mock Dryad interface works. It is tested
  # because it is the basis of further tests, so if it doesn't work, later tests
  # will fail spuriously

  # This should list the files in the testdata folder
  f <- listLocalFiles(testthat::test_path("testdata/db"))
  # Check that various files exist
  expect_true("README.txt" %in% f$file)

  # Download the README.txt file
  readmeIdx <- which(f$file == "README.txt")
  origContent <- readLines(file.path(testthat::test_path("testdata/db"), f$file[readmeIdx]))
  # Check that it can be downloaded and has the same contents as the original
  withr::with_dir(tempdir(), {
    utils::download.file(url = f$url[readmeIdx], destfile = f$file[readmeIdx], quiet = TRUE)
    expect_true(file.exists(f$file[readmeIdx]))
    newContent <- readLines(f$file[readmeIdx])
    expect_equal(newContent, origContent)
  })
})
