#organizacao
#' as pastas no dryad serao colocadas por zips de especies.
#' cada pasta vai ser nomeada da seguinte forma: exemplo: Pieridae_Delia_crassipes
get_butterfly <- function(families = c("family1", "family2", "family3", "family4"), species = c()) {
  ## Usando dados de: https://datadryad.org/stash/dataset/doi:10.5061/dryad.7wm37pw1d

  for (family in families) {
    if (family == "family1") {
      download.file(url = "https://datadryad.org/stash/downloads/file_stream/3048006",
                    destfile = paste0("family1", ".zip"),
                    mode = "wb")
    } else if (family == "family2") {
      download.file(url = "https://datadryad.org/stash/downloads/file_stream/3048005",
                    destfile = paste0("family2", ".zip"),
                    mode = "wb")
    } else if (family == "family3") {
      download.file(url = "https://datadryad.org/stash/downloads/file_stream/3048019",
                    destfile = paste0("family3", ".zip"),
                    mode = "wb")
    } else {
      print("Invalid family:", family)
    }

    # Descompactar o arquivo zip
    unzip(paste0(family, ".zip"))

    # Excluir pastas de espécies não selecionadas
    if (!is.null(species)) {
      all_species <- list.files(paste0(family, "/"))
      species_to_delete <- setdiff(all_species, species)
      for (specie in species_to_delete) {
        file.remove(paste0(family, "/", specie))
      }
    }
  }
}

# Exemplo de uso: baixando várias famílias e mantendo apenas pastas de espécies selecionadas
get_butterfly(c("family1", "family2"), c("leptuca_leptodactyla", "leptuca_cumulanta"))

# Se nenhum argumento for passado, nada será baixado
get_butterfly()
get_butterfly()
