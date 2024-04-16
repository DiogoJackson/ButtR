#'
#'
get_species <- function(species = c(),
                        family = c("Pieridae",
                                   "Lycanidae",
                                   "Papilionidae",
                                   "Hesperiidae",
                                   "Nymphalidae"),
                        place = c("Brisbane",
                                  "Cairns",
                                  "Sydney")) {

  # Pieridae list ----
  species_urls <- list(
    "Delia_cristata" = "https://datadryad.org/stash/downloads/file_stream/3048006",
    "Delia_crassipes" = "https://datadryad.org/stash/downloads/file_stream/3048006",
    "Amata_leptodactyla" = "https://datadryad.org/stash/downloads/file_stream/3048005",
    "Leptuca_albugularis" = "https://datadryad.org/stash/downloads/file_stream/3048005"
    # Add species here
  )

  # Mapeamento de espécies para famílias
  species_families <- list(
    "Delia_cristata" = "Pieridae",
    "Delia_crassipes" = "Pieridae",
    "Amata_leptodactyla" = "Nymphalidae",
    "Leptuca_albugularis" = "Nymphalidae"
    # Adicione mais mapeamentos conforme necessário
  )

  place_list <- list(
    "Brisbane",
    "Cairns",
    "Sydney"
    # Add places
  )

  # Criação da pasta "species" para os downloads
  species_folder <- "butterfly_data"
  dir.create(species_folder, showWarnings = FALSE)

  for (sp in species) {
    if (sp %in% names(species_urls)) {

      zip_file <- paste0(sp, ".zip")
      zip_path <- file.path(species_folder, zip_file)
      download.file(url = species_urls[[sp]], destfile = zip_path, mode = "wb")
      unzip(zip_path, exdir = file.path(species_folder, sp))
      cat("Espécie", sp, "baixada com sucesso no diretorio:\n", getwd())

    } else {
      cat("Espécie", sp, "nao encontrada\n")
    }
  }

      for (fam in family) {
        # Verifica se a família está mapeada
        if (fam %in% unique(unlist(species_families))) {
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
              download.file(url = species_in_family[[sp]], destfile = zip_path, mode = "wb")

              # Descompactar o arquivo ZIP
              unzip(zip_path, exdir = file.path(family_folder, sp))

              cat(family, "baixada com sucesso!\n")
            } else {
              cat(family,"não encontrada!\n")
            }
          }
        }
      }
  }

get_species(family = c("none"),
            species = c("Delia_cristata", "Amata_leptodactyla"),
            place = "Brisbane")
