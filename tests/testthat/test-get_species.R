# Test the get_species function

test_that("get_species", {
  # IMPORTANT: since the dryad repository doesn't exist yet, download from a local copy in the test data
  testthat::local_mocked_bindings(ListDbsFiles = function() {
    listLocalFiles(testthat::test_path("testdata/repo"))
  })

  dbDir <- testthat::test_path("tempdb")
  withr::defer(unlink(dbDir, recursive = TRUE)) # Cleanup after running the test

  get_species(species = c("Telicota mesoptis"), db_folder = dbDir)

  expect_true(dir.exists(dbDir))
  expect_true(file.exists(file.path(dbDir, "Oz_butterflies.xlsx")))

  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/S16/S16(RGB).jpg")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/S16/S16(UV).jpg")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/S19/S19(RGB).jpg")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/S19/S19(UV).jpg")))


})
