test_that("dryad interface works", {
  # Get files from an arbitrary repo
  f <- listFilesInDryad("doi:10.5061/dryad.s1rn8pkdv")

  # This is what we expect to get
  expected <- structure(list(file = c("Ocp_liver_genotypes_307snps.vcf", "Ocp_scat_archival_genotypes_307snps.vcf",
                                    "snp_metadata.csv", "sample_metadata.csv", "README.md"),
                           url = c("https://datadryad.org",
                                   "https://datadryad.org", "https://datadryad.org", "https://datadryad.org",
                                   "https://datadryad.org")),
                      class = "data.frame",
                      row.names = c(NA, -5L))
  expect_equal(f, expected)
})

test_that("local file interface works", {
  # This is to test that the test mock Dryad interface works. It is tested
  # because it is the basis of further tests, so if it doesn't work, later tests
  # will fail spuriously

  # This should list the files in the testdata folder
  f <- listLocalFiles(testthat::test_path("testdata"))
  # Check that various files exist
  expect_true("ButtR.csv" %in% f$file)
  expect_true("README.txt" %in% f$file)
  expect_true("Hesperiidae-Ochlodes-sylvanus.zip" %in% f$file)
  expect_true("Papilionidae-Papilio-aegeus.zip" %in% f$file)

  # Download the README.txt file
  readmeIdx <- which(f$file == "README.txt")
  origContent <- readLines(file.path(testthat::test_path("testdata"), f$file[readmeIdx]))
  # Check that it can be downloaded and has the same contents as the original
  withr::with_dir(tempdir(), {
    utils::download.file(url = f$url[readmeIdx], destfile = f$file[readmeIdx], quiet = TRUE)
    expect_true(file.exists(f$file[readmeIdx]))
    newContent <- readLines(f$file[readmeIdx])
    expect_equal(newContent, origContent)
    })
})
