get_species <- function(species = c(),
                        family = c("Pieridae",
                                   "Lycanidae",
                                   "Papilionidae",
                                   "Hesperiidae",
                                   "Nymphalidae"),
                        place = c("Brisbane",
                                  "Cairns",
                                  "Sydney")) {

  # URLs das espécies
  species_urls <- list(
    "Delia_cristata" = "https://datadryad.org/stash/downloads/file_stream/3048006",
    "Delia_crassipes" = "https://datadryad.org/stash/downloads/file_stream/3048006",
    "Amata_leptodactyla" = "https://datadryad.org/stash/downloads/file_stream/3048005",
    "Leptuca_albugularis" = "https://datadryad.org/stash/downloads/file_stream/3048005"
    # Adicione mais URLs conforme necessário
  )

  # Mapeamento de espécies para famílias
  species_families <- list(
    "Delia_cristata" = "Pieridae",
    "Delia_crassipes" = "Pieridae",
    "Amata_leptodactyla" = "Nymphalidae",
    "Leptuca_albugularis" = "Nymphalidae"
    # Adicione mais mapeamentos conforme necessário
  )

  # Criação da pasta "butterfly_data" para os downloads
  species_folder <- "butterfly_data"
  dir.create(species_folder, showWarnings = FALSE)

  for (sp in species) {
    if (sp %in% names(species_urls)) {
      zip_file <- paste0(sp, ".zip")
      zip_path <- file.path(species_folder, zip_file)
      download.file(url = species_urls[[sp]], destfile = zip_path, mode = "wb")
      unzip(zip_path, exdir = file.path(species_folder, sp))
      cat("Espécie", sp, "baixada com sucesso no diretório:\n", getwd(), "\n")
    } else {
      cat("Espécie", sp, "não encontrada\n")
    }
  }

  for (fam in family) {
    if (fam %in% unique(unlist(species_families))) {
      family_folder <- paste0("butterfly_data_", fam)
      dir.create(family_folder, showWarnings = FALSE)
      species_in_family <- names(Filter(function(x) x == fam, species_families))

      for (sp in species_in_family) {
        if (sp %in% names(species_urls)) {
          zip_file <- paste0(sp, ".zip")
          zip_path <- file.path(family_folder, zip_file)
          download.file(url = species_urls[[sp]], destfile = zip_path, mode = "wb")
          unzip(zip_path, exdir = file.path(family_folder, sp))
          cat("Espécie", sp, "baixada com sucesso para a família", fam, "\n")
        } else {
          cat("Espécie", sp, "não encontrada para a família", fam, "\n")
        }
      }
    } else {
      cat("Família", fam, "não encontrada\n")
    }
  }
}

# Exemplo de uso da função
get_species(family = c("Pieridae"),
            species = c("Delia_cristata", "Amata_leptodactyla"),
            place = "Brisbane")
