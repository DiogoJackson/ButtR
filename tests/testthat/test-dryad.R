library(testthat)

test_that("dryad interface works", {
  # Get list of files from an arbitrary repo
  # (https://datadryad.org/stash/landing/show?id=doi%3A10.5061%2Fdryad.s1rn8pkdv)
  f <- listFilesInDryad("doi:10.5061/dryad.s1rn8pkdv")

  # This is what we expect to get. This may be a bit brittle because Dryad might reorder files
  expected <- structure(list(file = c("Ocp_liver_genotypes_307snps.vcf",
                                      "Ocp_scat_archival_genotypes_307snps.vcf",
                                      "snp_metadata.csv", "sample_metadata.csv", "README.md"),
                             url = c("https://datadryad.org/api/v2/files/2967593/download",
                                     "https://datadryad.org/api/v2/files/2967594/download",
                                     "https://datadryad.org/api/v2/files/2967595/download",
                                     "https://datadryad.org/api/v2/files/2967596/download",
                                     "https://datadryad.org/api/v2/files/2967601/download"
                                      )),
                        class = "data.frame", row.names = c(NA, -5L))
  expect_equal(f, expected)

  # Download the README.md
  tmpnm <- tempfile(f$file[5])
  expect_error(utils::download.file(f$url[5], destfile = tmpnm, quiet = TRUE), NA)
  # Check it has the expected content
  line1 <- readLines(tmpnm, n = 1)
  # This is the first line in the README.md file from the random dataset
  expect_equal(line1, "# Data from: Evaluating genotyping-in-thousands by sequencing as a genetic monitoring tool for a climate sentinel mammal using non-invasive and archival samples")
})

test_that("correct version", {
  # Check that we are finding the latest version from Dryad
  f <- listFilesInDryad("doi:10.5061/dryad.15dv41nwj")
  # There should be 1 file called mimicry-in-motion-main.v2.zip.
  # Earlier versions had different file names
  expect_equal(nrow(f), 1)
  expect_equal(f$file, "mimicry-in-motion-main.v2.zip")
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
