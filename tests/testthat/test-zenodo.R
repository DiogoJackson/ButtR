library(testthat)

test_that("zenodo interface works", {
  # Get list of files from the Zenodo repo (OzButterflies)
  # (https://doi.org/10.5281/zenodo.15881960)
  f <- listFilesInZenodo(BUTTR_DEPOSITION)

  # This is what we expect to get. Don't assume order
  # NOTE that testing uses locale C, which is different from interactive use
  #Sys.setlocale("LC_COLLATE", "C")
  f <- f[order(f$file), ]
  expected <- structure(list(file = c("Hesperiidae_Arrhenes.zip", "Hesperiidae_Cephrenes.zip",
                                      "Hesperiidae_Hesperilla.zip", "Hesperiidae_Mesodina.zip", "Hesperiidae_Netrocoryne.zip",
                                      "Hesperiidae_Notocrypta.zip", "Hesperiidae_Ocybadistes.zip",
                                      "Hesperiidae_Parnara.zip", "Hesperiidae_Pelopidas.zip", "Hesperiidae_Sabera.zip",
                                      "Hesperiidae_Suniana.zip", "Hesperiidae_Tagiades.zip", "Hesperiidae_Taractrocera.zip",
                                      "Hesperiidae_Telicota.zip", "Hesperiidae_Toxidia.zip", "Hesperiidae_Trapezites.zip",
                                      "Lycaenidae_Arhopala.zip", "Lycaenidae_Candalides.zip", "Lycaenidae_Catochrysops.zip",
                                      "Lycaenidae_Catopyrops.zip", "Lycaenidae_Deudorix.zip", "Lycaenidae_Erysichton.zip",
                                      "Lycaenidae_Euchrysops.zip", "Lycaenidae_Famegana.zip", "Lycaenidae_Hypochrysops.zip",
                                      "Lycaenidae_Hypolycaena.zip", "Lycaenidae_Jamides.zip", "Lycaenidae_Lampides.zip",
                                      "Lycaenidae_Leptotes.zip", "Lycaenidae_Megisba.zip", "Lycaenidae_Nacaduba.zip",
                                      "Lycaenidae_Neolucia.zip", "Lycaenidae_Paralucia.zip", "Lycaenidae_Prosotas.zip",
                                      "Lycaenidae_Psychonotis.zip", "Lycaenidae_Theclinesthes.zip",
                                      "Lycaenidae_Zizina.zip", "Lycaenidae_Zizula.zip", "Nymphalidae_Acraea.zip",
                                      "Nymphalidae_Cethosia.zip", "Nymphalidae_Cupha.zip", "Nymphalidae_Danaus.zip",
                                      "Nymphalidae_Doleschallia.zip", "Nymphalidae_Euploea.zip", "Nymphalidae_Heteronympha.zip",
                                      "Nymphalidae_Hypocysta.zip", "Nymphalidae_Hypolimnas.zip", "Nymphalidae_Junonia.zip",
                                      "Nymphalidae_Melanitis.zip", "Nymphalidae_Mycalesis.zip", "Nymphalidae_Mynes.zip",
                                      "Nymphalidae_Neptis.zip", "Nymphalidae_Pantoporia.zip", "Nymphalidae_Phaedyma.zip",
                                      "Nymphalidae_Tirumala.zip", "Nymphalidae_Tisiphone.zip", "Nymphalidae_Vagrans.zip",
                                      "Nymphalidae_Vanessa.zip", "Nymphalidae_Yoma.zip", "Nymphalidae_Ypthima.zip",
                                      "Oz_butterflies.csv", "Oz_butterflies.json", "Oz_butterflies.xlsx",
                                      "Oz_butterflies_summary.csv", "Oz_butterflies_summary.json",
                                      "Oz_butterflies_summary.xlsx", "Papilionidae_Cressida.zip", "Papilionidae_Graphium.zip",
                                      "Papilionidae_Pachliopta.zip", "Papilionidae_Papilio.zip", "Pieridae_Belenois.zip",
                                      "Pieridae_Catopsilia.zip", "Pieridae_Cepora.zip", "Pieridae_Delias.zip",
                                      "Pieridae_Elodina.zip", "Pieridae_Eurema.zip", "Pieridae_Pieris.zip",
                                      "README.txt", "standard-blue.ProcSpec", "standard-green.ProcSpec",
                                      "standard-red.ProcSpec"), url = c("https://zenodo.org/api/records/15881961/files/Hesperiidae_Arrhenes.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Hesperiidae_Cephrenes.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Hesperiidae_Hesperilla.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Hesperiidae_Mesodina.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Hesperiidae_Netrocoryne.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Hesperiidae_Notocrypta.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Hesperiidae_Ocybadistes.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Hesperiidae_Parnara.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Hesperiidae_Pelopidas.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Hesperiidae_Sabera.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Hesperiidae_Suniana.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Hesperiidae_Tagiades.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Hesperiidae_Taractrocera.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Hesperiidae_Telicota.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Hesperiidae_Toxidia.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Hesperiidae_Trapezites.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Arhopala.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Candalides.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Catochrysops.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Catopyrops.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Deudorix.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Erysichton.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Euchrysops.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Famegana.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Hypochrysops.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Hypolycaena.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Jamides.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Lampides.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Leptotes.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Megisba.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Nacaduba.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Neolucia.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Paralucia.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Prosotas.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Psychonotis.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Theclinesthes.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Zizina.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Lycaenidae_Zizula.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Acraea.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Cethosia.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Cupha.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Danaus.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Doleschallia.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Euploea.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Heteronympha.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Hypocysta.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Hypolimnas.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Junonia.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Melanitis.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Mycalesis.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Mynes.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Neptis.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Pantoporia.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Phaedyma.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Tirumala.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Tisiphone.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Vagrans.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Vanessa.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Yoma.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Nymphalidae_Ypthima.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Oz_butterflies.csv/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Oz_butterflies.json/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Oz_butterflies.xlsx/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Oz_butterflies_summary.csv/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Oz_butterflies_summary.json/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Oz_butterflies_summary.xlsx/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Papilionidae_Cressida.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Papilionidae_Graphium.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Papilionidae_Pachliopta.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Papilionidae_Papilio.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Pieridae_Belenois.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Pieridae_Catopsilia.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Pieridae_Cepora.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Pieridae_Delias.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Pieridae_Elodina.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Pieridae_Eurema.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/Pieridae_Pieris.zip/content",
                                                                        "https://zenodo.org/api/records/15881961/files/README.txt/content",
                                                                        "https://zenodo.org/api/records/15881961/files/standard-blue.ProcSpec/content",
                                                                        "https://zenodo.org/api/records/15881961/files/standard-green.ProcSpec/content",
                                                                        "https://zenodo.org/api/records/15881961/files/standard-red.ProcSpec/content"
                                      )), row.names = c(7L, 6L, 9L, 10L, 12L, 8L, 11L, 13L, 14L, 15L,
                                                        16L, 27L, 18L, 17L, 28L, 23L, 24L, 22L, 32L, 25L, 19L, 34L, 29L,
                                                        35L, 38L, 39L, 21L, 20L, 30L, 40L, 31L, 33L, 43L, 41L, 42L, 26L,
                                                        44L, 45L, 51L, 47L, 46L, 52L, 48L, 49L, 50L, 36L, 37L, 54L, 53L,
                                                        55L, 56L, 63L, 58L, 65L, 68L, 70L, 67L, 57L, 61L, 66L, 72L, 75L,
                                                        69L, 77L, 74L, 59L, 62L, 76L, 71L, 60L, 64L, 79L, 78L, 80L, 81L,
                                                        73L, 1L, 5L, 3L, 2L, 4L), class = "data.frame")
  # Ignore the size column
  expect_equal(f[, c("file", "url")], expected)

  # Download README.txt
  ri <- which(f$file == "README.txt")
  tmpnm <- tempfile(f$file[ri])
  expect_error(utils::download.file(f$url[ri], destfile = tmpnm, quiet = TRUE), NA)
  # Check it has the expected content
  line1 <- readLines(tmpnm, n = 1)
  # This is the first line in the README.txt file
  expect_equal(line1, "OzButterflies database")
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
