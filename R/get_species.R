get_species <- function(species = c(), place = c("Brisbane", "Cairns", "Sydney")) {
  # Lista de espécies e seus URLs
  species_urls <- list(
    "Delia_cristata" = "https://datadryad.org/stash/downloads/file_stream/3048006",
    "Delia_crassipes" = "https://datadryad.org/stash/downloads/file_stream/3048006",
    "Amata_leptodactyla" = "https://datadryad.org/stash/downloads/file_stream/3048005",
    "Leptuca_albugularis" = "https://datadryad.org/stash/downloads/file_stream/3048005"
    # Adicione mais espécies conforme necessário
  )

  place_list <- list(
    "Brisbane",
    "Cairns",
    "Sydney"
    # Adicione mais lugares conforme necessário
  )
  # Criação da pasta "species" para os downloads
  species_folder <- "butterfly_data_species"
  dir.create(species_folder, showWarnings = FALSE)

  for (sp in species) {
    if (sp %in% names(species_urls)) {
      # Nome do arquivo ZIP
      zip_file <- paste0(sp, ".zip")
      # Caminho completo para o arquivo ZIP
      zip_path <- file.path(species_folder, zip_file)

      # Download do arquivo ZIP
      download.file(url = species_urls[[sp]], destfile = zip_path, mode = "wb")

      # Descompactar o arquivo ZIP
      unzip(zip_path, exdir = file.path(species_folder, sp))

      cat("Espécie", sp, "baixada com sucesso no diretorio:\n", getwd())
    } else {
      cat("Espécie", sp, "não encontrada!\n")
    }
  }
}

get_species(species = c("Delia_cristata", "Amata_leptodactyla"), place = "Brisbane")
