# Test the get_Oz_butterflies function

# To manually check test coverage, run
# covr::report()
# You might need to clear the environment and restart the R session first.

# Sets up the environment in preparation for a test.
# 1. Makes buttR use a local testing repository rather than the Dryad repository
# 2. Returns the name of a directory to be used to install into. The directory
# will be deleted once the test is complete
prepareTest <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(ListDbsFiles = function() {
    listLocalFiles(testthat::test_path("testdata/repo"))
  }, .env = env)

  dbDir <- testthat::test_path("tempdb")
  withr::defer_parent(unlink(dbDir, recursive = TRUE)) # Cleanup after running the test
  dbDir
}

# Returns a sorted vector of the names of directories contained some number of
# levels down a directory hierarchy.
dirsAtLevel <- function(dir, level) {
  while (level > 0) {
    level <- level - 1

    dir <- list.dirs(dir, full.names = level > 0, recursive = FALSE)
  }
  sort(dir)
}


#############################################################
# tests start here
test_that("test get_Oz_butterflies", {
  dbDir <- prepareTest()

  get_Oz_butterflies(species = c("Telicota mesoptis", "Papilio aegeus"), db_folder = dbDir)

  expect_true(dir.exists(dbDir))
  expect_true(file.exists(file.path(dbDir, "Oz_butterflies.xlsx")))
  expect_true(file.exists(file.path(dbDir, "Oz_butterflies.csv")))
  expect_true(file.exists(file.path(dbDir, "Oz_butterflies.json")))

  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/16/16_RGB.ARW")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/16/16_UV.ARW")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/19/19_RGB.ARW")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/19/19_UV.ARW")))

  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361_RGB.ARW")))
  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361_UV.ARW")))

  # Unrequested species should not have been downloaded
  expect_false(dir.exists(file.path(dbDir, "Hesperiidae/Notocrypta_waigensis")))
  gotFams <- list.dirs(dbDir, full.names = FALSE, recursive = FALSE)
  expect_equal(gotFams, c("Hesperiidae", "Papilionidae"))
  gotSp <- sapply(gotFams, function(fam) list.dirs(file.path(dbDir, fam), full.names = FALSE, recursive = FALSE))
  expect_equal(sort(unname(gotSp)), sort(c("Papilio_aegeus", "Telicota_mesoptis")))
})

test_that("image types 1", {
  dbDir <- prepareTest()

  get_Oz_butterflies(species = c("Suniana sunias", "Papilio aegeus"), download_images = "jpeg", db_folder = dbDir)

  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Suniana_sunias/186/186L.jpg")))
  # Raw files shouldn't be downloaded
  expect_false(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361RGB.ARW")))
  expect_false(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361UV.ARW")))
})

test_that("image types 2", {
  dbDir <- prepareTest()

  get_Oz_butterflies(species = c("Suniana sunias", "Telicota mesoptis", "Papilio aegeus"), download_images = "raw", db_folder = dbDir)

  # Only raw files should be downloaded
  expect_false(file.exists(file.path(dbDir, "Hesperiidae/Suniana_sunias/186/186L.jpg")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/16/16_RGB.ARW")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/16/16_UV.ARW")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/19/19_RGB.ARW")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/19/19_UV.ARW")))

  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361_RGB.ARW")))
  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361_UV.ARW")))
})

test_that("get family", {
  dbDir <- prepareTest()

  # Invalid family name should fail
  expect_error(get_Oz_butterflies(family = "Bad one", db_folder = dbDir), "requested family does not")
  expect_error(get_Oz_butterflies(family = c("Bad one", "Bad two"), db_folder = dbDir), "requested families do not")

  fams <- c("Hesperiidae", "Nymphalidae")
  get_Oz_butterflies(family = fams, db_folder = dbDir)
  got <- dirsAtLevel(dbDir, 1)
  expect_equal(got, fams)

})

test_that("get genus", {
  dbDir <- prepareTest()

  # Invalid genus name should fail
  expect_error(get_Oz_butterflies(genus = "Bad one", db_folder = dbDir))

  genera <- c("Notocrypta", "Telicota", "Euploea")
  get_Oz_butterflies(genus = genera, db_folder = dbDir)
  got <- dirsAtLevel(dbDir, 2)
  expect_equal(sort(got), sort(c("Notocrypta_waigensis", "Telicota_mesoptis", "Euploea_darchia")))

})

test_that("get combines", {
  # Test that calling get_Oz_butterflies twice combines the data from both calls into the local database
  dbDir <- prepareTest()
  # Get one species
  get_Oz_butterflies(species = "Suniana sunias", db_folder = dbDir)
  expect_equal(dirsAtLevel(dbDir, 2), "Suniana_sunias")
  # Now get a second species
  get_Oz_butterflies(species = "Papilio aegeus", db_folder = dbDir)
  # Both species should now be in the database
  expect_equal(dirsAtLevel(dbDir, 2), sort(c("Suniana_sunias", "Papilio_aegeus")))
})


test_that("get site", {
  dbDir <- prepareTest()

  get_Oz_butterflies(site = c("BG"), db_folder = dbDir)

  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Notocrypta_waigensis/4/4_RGB.ARW")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Notocrypta_waigensis/4/4_UV.ARW")))

  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/16/16_RGB.ARW")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/16/16_UV.ARW")))

  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/19/19_RGB.ARW")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/19/19_UV.ARW")))

  # Unrequested species should not have been downloaded
  expect_false(dir.exists(file.path(dbDir, "Hesperiidae/Suniana_sunias")))
  gotFams <- list.dirs(dbDir, full.names = FALSE, recursive = FALSE)
  expect_equal(gotFams, c("Hesperiidae"))
  gotSp <- sapply(gotFams, function(fam) list.dirs(file.path(dbDir, fam), full.names = FALSE, recursive = FALSE))
  expect_equal(sort(unname(gotSp)), sort(c("Notocrypta_waigensis", "Telicota_mesoptis")))
})

test_that("get reflectance", {
  dbDir <- prepareTest()

  get_Oz_butterflies(spectra = c("y"), db_folder = dbDir)

  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Notocrypta_waigensis/4/4_RGB.ARW")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Notocrypta_waigensis/4/4_UV.ARW")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Suniana_sunias/186")))
  expect_true(file.exists(file.path(dbDir, "Nymphalidae/Euploea_darchia/551")))
  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361")))

  # Unrequested species should not have been downloaded
  expect_false(dir.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis")))
  gotFams <- list.dirs(dbDir, full.names = FALSE, recursive = FALSE)
  expect_equal(gotFams, c("Hesperiidae", "Nymphalidae", "Papilionidae"))

  gotSp <- sapply(gotFams, function(fam) list.dirs(file.path(dbDir, fam), full.names = FALSE, recursive = FALSE))

  # Correção: garantir que gotSp seja um vetor atômico
  gotSp <- unlist(gotSp, use.names = FALSE)

  # Debug opcional caso o erro persista
  print(gotSp)

  expect_equal(sort(gotSp), sort(c("Notocrypta_waigensis", "Suniana_sunias", "Euploea_darchia", "Papilio_aegeus")))
})

test_that("get sex", {
  dbDir <- prepareTest()

  get_Oz_butterflies(sex = c("Male"), db_folder = dbDir)

  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/16/16_RGB.ARW")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/16/16_UV.ARW")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/19/19_RGB.ARW")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/19/19_UV.ARW")))

  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361_RGB.ARW")))
  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361_UV.ARW")))
  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/spec1.txt")))
  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/spec2.txt")))
  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/spec3.txt")))

  # Unrequested species should not have been downloaded
  expect_false(dir.exists(file.path(dbDir, "Hesperiidae/Notocrypta_waigensis")))
  expect_false(dir.exists(file.path(dbDir, "Hesperiidae/Suniana_sunias")))
  expect_false(dir.exists(file.path(dbDir, "Hesperiidae/Euploea_darchia")))
  gotFams <- list.dirs(dbDir, full.names = FALSE, recursive = FALSE)
  expect_equal(gotFams, c("Hesperiidae", "Papilionidae"))
  gotSp <- sapply(gotFams, function(fam) list.dirs(file.path(dbDir, fam), full.names = FALSE, recursive = FALSE))
  expect_equal(sort(unname(gotSp)), sort(c("Papilio_aegeus", "Telicota_mesoptis")))
})

#Need expect_error to test invalid year ####
test_that("get year", {
  dbDir <- prepareTest()

  get_Oz_butterflies(year = c("2022"), db_folder = dbDir)

  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/16/16_RGB.ARW")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/16/16_UV.ARW")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/19/19_RGB.ARW")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Telicota_mesoptis/19/19_UV.ARW")))

  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Notocrypta_waigensis")))
  expect_true(file.exists(file.path(dbDir, "Hesperiidae/Suniana_sunias")))
  expect_true(file.exists(file.path(dbDir, "Nymphalidae/Euploea_darchia")))

  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361_RGB.ARW")))
  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/1361_UV.ARW")))
  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/spec1.txt")))
  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/spec2.txt")))
  expect_true(file.exists(file.path(dbDir, "Papilionidae/Papilio_aegeus/1361/spec3.txt")))

  # Unrequested species should not have been downloaded
  gotFams <- list.dirs(dbDir, full.names = FALSE, recursive = FALSE)
  expect_equal(gotFams, c("Hesperiidae","Nymphalidae", "Papilionidae"))
  gotSp <- unlist(lapply(gotFams, function(fam) list.dirs(file.path(dbDir, fam), full.names = FALSE, recursive = FALSE)))
  expect_equal(sort(unname(gotSp)), sort(c("Papilio_aegeus",
                                           "Telicota_mesoptis",
                                           "Notocrypta_waigensis",
                                           "Suniana_sunias",
                                           "Euploea_darchia")))
})

test_that("get id", {
  dbDir <- prepareTest()

  # Invalid genus name should fail
  expect_error(get_Oz_butterflies(sampleIDs = "bad one", db_folder = dbDir))

  ids <- c("16")
  get_Oz_butterflies(sampleIDs = ids, db_folder = dbDir)
  got <- dirsAtLevel(dbDir, 3)
  expect_equal(sort(got), sort(c("16")))
  expect_false(dir.exists(file.path(dbDir, "19")))
})
