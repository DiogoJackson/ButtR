#############
get_family <- function(species = c(), families = c(), place =c()) {
  # Lista de espécies e seus URLs
  species_urls <- list(
    "Delia_cristata" = "https://datadryad.org/stash/downloads/file_stream/3048006",
    "Delia_crassipes" = "https://datadryad.org/stash/downloads/file_stream/3048006",
    "Amata_leptodactyla" = "https://datadryad.org/stash/downloads/file_stream/3048005",
    "Leptuca_albugularis" = "https://datadryad.org/stash/downloads/file_stream/3048005"
    # Adicione mais espécies conforme necessário
  )

  # Mapeamento de espécies para famílias
  species_families <- list(
    "Delia_cristata" = "family1",
    "Delia_crassipes" = "family1",
    "Amata_leptodactyla" = "family2",
    "Leptuca_albugularis" = "family2"
    # Adicione mais mapeamentos conforme necessário
  )

  for (family in families) {
    # Verifica se a família está mapeada
    if (family %in% unique(unlist(species_families))) {
      # Cria uma pasta para a família
      family_folder <- paste0("butterfly_data_", family)
      dir.create(family_folder, showWarnings = FALSE)

      # Obtém as espécies associadas à família
      species_in_family <- names(Filter(function(x) x == family, species_families))

      # Baixa cada espécie associada à família
      for (sp in species_in_family) {
        if (sp %in% names(species_urls)) {
          # Nome do arquivo ZIP
          zip_file <- paste0(sp, ".zip")
          # Caminho completo para o arquivo ZIP
          zip_path <- file.path(family_folder, zip_file)

          # Download do arquivo ZIP
          download.file(url = species_urls[[sp]], destfile = zip_path, mode = "wb")

          # Descompactar o arquivo ZIP
          unzip(zip_path, exdir = file.path(family_folder, sp))

          cat("Espécie", sp, "baixada com sucesso!\n")
        } else {
          cat("Espécie", sp, "não encontrada!\n")
        }
      }
    } else {
      cat("Família", family, "não encontrada!\n")
    }
  }
}
