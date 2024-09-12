# Test the get_species function

test_that("test get_species", {
  # IMPORTANT: since the dryad repository doesn't exist yet, download from a local copy in the test data
  testthat::local_mocked_bindings(ListDbsFiles = function() {
    listLocalFiles(testthat::test_path("testdata/repo"))
  })

  dbDir <- testthat::test_path("tempdb")
  withr::defer(unlink(dbDir, recursive = TRUE)) # Cleanup after running the test

  get_species(species = c("Telicota mesoptis", "Papilio aegeus"), db_folder = dbDir)

  expect_true(dir.exists(dbDir))
  expect_true(file.exists(file.path(dbDir, "Oz_butterflies.xlsx")))
  expect_true(file.exists(file.path(dbDir, "Oz_butterflies.csv")))
  expect_true(file.exists(file.path(dbDir, "Oz_butterflies.json")))

  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/16/16RGB.jpg")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/16/16UV.jpg")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/19/19RGB.jpg")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/19/19UV.jpg")))

  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361RGB.jpg")))
  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361UV.jpg")))
  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361RGB.arw")))
  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361UV.arw")))

  # Unrequested species should not have been downloaded
  expect_true(!dir.exists(file.path(dbDir, "Hesperiidae/Notocrypta_waigensis")))
})

test_that("image types 1", {
  # IMPORTANT: since the dryad repository doesn't exist yet, download from a local copy in the test data
  testthat::local_mocked_bindings(ListDbsFiles = function() {
    listLocalFiles(testthat::test_path("testdata/repo"))
  })

  dbDir <- testthat::test_path("tempdb")
  withr::defer(unlink(dbDir, recursive = TRUE)) # Cleanup after running the test

  get_species(species = c("Telicota mesoptis", "Papilio aegeus"), download_images = "jpeg", db_folder = dbDir)

  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/16/16RGB.jpg")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/16/16UV.jpg")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/19/19RGB.jpg")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/19/19UV.jpg")))

  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361RGB.jpg")))
  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361UV.jpg")))
  # Raw files shouldn't be downloaded
  expect_true(!file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361RGB.arw")))
  expect_true(!file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361UV.arw")))
})

test_that("image types 2", {
  # IMPORTANT: since the dryad repository doesn't exist yet, download from a local copy in the test data
  testthat::local_mocked_bindings(ListDbsFiles = function() {
    listLocalFiles(testthat::test_path("testdata/repo"))
  })

  dbDir <- testthat::test_path("tempdb")
  withr::defer(unlink(dbDir, recursive = TRUE)) # Cleanup after running the test

  get_species(species = c("Telicota mesoptis", "Papilio aegeus"), download_images = "raw", db_folder = dbDir)

  # Only raw files should be downloaded
  expect_true(!file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/16/16RGB.jpg")))
  expect_true(!file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/16/16UV.jpg")))
  expect_true(!file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/19/19RGB.jpg")))
  expect_true(!file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/19/19UV.jpg")))

  expect_true(!file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361RGB.jpg")))
  expect_true(!file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361UV.jpg")))
  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361RGB.arw")))
  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361UV.arw")))
})
