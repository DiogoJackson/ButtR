library(readxl)

# Database structure (unpacked)
# Oz_butterflies.xlsx
# Oz_butterflies.csv (TBD)
# Oz_butterflies.json (TBD)
# Folder for each family, which contains
# Folder for each species, which contains
# Folder for each specimen
#
# Specimen folder
# TODO Name of specimen folder?
# Family/species/specimen
# Within each specimen folder:
# Always contains
# "<ID>(RGB).arw"
# "<ID>(UV).arw"
# May contain
# Sequence file
# 2 x linear TIFFs (with corrected specimen IDs), named as for raw files
# 1 x TIFF with spec locations highlighted (name TBD)
# Spec files ("<ID><species initials><a|n><spot ID>.rspec") (a|n = angle or normal))
# CSV equivalent of all spec files

# In Dryad:
# Oz_butterflies.xlsx
# Oz_butterflies.csv (TBD)
# Oz_butterflies.json (TBD)
# Lots of species zip files
#



#' @export
get_species <- function(species = NULL,
                        db_folder = "Oz_butterflies",
                        family = NULL,
                        location = c("Sydney", "Brisbane", "Cairns")) {

  # Verifica se a pasta "db_folder" (onde os dados serão armazenados) existe.
  # Se não existir, cria a pasta. O simbolo de exclamacao significa "nao"
  if(!dir.exists(db_folder)) {
    dir.create(db_folder)
  }

  # Lista todos os arquivos locais presentes na pasta especificada (neste caso, "data_buttr")
  files <- listLocalFiles("C:\\Users\\Diogo Silva\\Desktop\\data_buttr")

  # Filtra a URL do arquivo "Oz_butterflies.xlsx" a partir dos arquivos listados
  url <- files$url[files$file == "Oz_butterflies.xlsx"]

  # Define o caminho onde a planilha será salva localmente
  # db_folder eh um argumento da funcao.
  spread_sheet <- file.path(db_folder, "Oz_butterflies.xlsx")

  # Baixa a planilha a partir da URL e a salva no caminho especificado
  utils::download.file(url = url, destfile = spread_sheet, mode = "wb")

  # Lê os dados da planilha, pulando a primeira linha
  meta_data <- read_excel(spread_sheet, skip = 1)

  # Cria uma nova coluna "full_species" que combina as colunas "genus" e "species" em um único nome completo
  meta_data$full_species <- paste(meta_data$genus, meta_data$species)

  # Cria uma nova coluna "zipname" que combina "Family", "genus" e "species" em um único nome, separado por "_"
  meta_data$zipname <- paste(meta_data$Family, meta_data$genus, meta_data$species, sep = "_")

  # Adiciona a extensão ".zip" ao final dos nomes de arquivo na coluna "zipname"
  meta_data$zipname <- paste0(meta_data$zipname, ".zip")

  # Se nenhuma espécie ou família for especificada, seleciona todas as linhas
  rows <- rep(TRUE, nrow(meta_data))

  # Se a família for especificada, atualiza a filtragem
  if (length(family) > 0) {
    rows <- rows & meta_data$Family %in% family
  }

  # Se a espécie for especificada, atualiza a filtragem
  if (length(species) > 0) {
    rows <- rows & meta_data$full_species %in% species
  }

  # Se a espécie for especificada, atualiza a filtragem
  if (length(location) > 0) {
    rows <- rows & meta_data$location %in% location
  }

  # Obtém os nomes dos arquivos zip únicos que correspondem às famílias filtradas
  zips <- unique(meta_data$zipname[rows])

  # Cria um diretório temporário para armazenar os arquivos zip baixados
  tempdir <- tempdir()

  # Itera por cada arquivo zip selecionado, baixando e descompactando os arquivos
  for (zip in zips) {
    # Define o caminho onde o arquivo zip será salvo temporariamente
    zip_path <- file.path(tempdir, zip)

    # Obtém a URL do arquivo zip correspondente
    url <- files$url[files$file == zip]

    # Baixa o arquivo zip da URL e salva no caminho temporário
    utils::download.file(url = url, destfile = zip_path, mode = "wb")

    # Descompacta o arquivo zip no diretório de destino especificado
    utils::unzip(zip_path, exdir = file.path(db_folder, sp))
  }
}

  # Download individual species by name ----
  # for (sp in species) {
  #   if (sp %in% names(species_urls)) {
  #     zip_file <- paste0(sp, ".zip")
  #     zip_path <- file.path(species_folder, zip_file)
  #     utils::download.file(url = species_urls[[sp]], destfile = zip_path, mode = "wb")
  #     utils::unzip(zip_path, exdir = file.path(species_folder, sp))

      # Delete location files
  #     folders_to_keep <- intersect(location, c("__MACOSX", "sample frames, landmark data"))
  #     folders_to_delete <- setdiff(c("__MACOSX", "sample frames, landmark data"), folders_to_keep)
  #
  #     for (folder in folders_to_delete) {
  #       folder_path <- file.path(species_folder, sp, folder)
  #       if (file.exists(folder_path)) {
  #         unlink(folder_path, recursive = TRUE)
  #       }
  #     }
  #
  #     cat("Species", sp, "was successfully downloaded in:\n", getwd(), "\n")
  #
  #   } else {
  #     cat("Species", sp, "not found. Please check the species list in the dataset\n")
  #   }
  # }
  #
  # # Download species by family if family is selected ----
  # if (!is.null(family)) {
  #   for (fam in family) {
  #     if (fam %in% unique(unlist(species_families))) {
  #       family_folder <- paste0("Family_", fam)
  #       dir.create(family_folder, showWarnings = FALSE)
  #       species_in_family <- names(Filter(function(x) x == fam, species_families))
  #
  #       for (sp in species_in_family) {
  #         if (sp %in% names(species_urls)) {
  #           zip_file <- paste0(sp, ".zip")
  #           zip_path <- file.path(family_folder, zip_file)
  #           utils::download.file(url = species_urls[[sp]], destfile = zip_path, mode = "wb")
  #           utils::unzip(zip_path, exdir = file.path(family_folder, sp))
  #
  #           # Delete location files
  #           folders_to_keep <- intersect(location, c("__MACOSX", "sample frames, landmark data"))
  #           folders_to_delete <- setdiff(c("__MACOSX", "sample frames, landmark data"), folders_to_keep)
  #
  #           for (folder in folders_to_delete) {
  #             folder_path <- file.path(family_folder, sp, folder)
  #             if (file.exists(folder_path)) {
  #               unlink(folder_path, recursive = TRUE)
  #             }
  #           }
  #
  #           cat("Family", fam, "was successfully downloaded in:\n", getwd(), "\n")
  #         } else {
  #           cat("Family", fam, "not found. Please check the family list in the dataset\n")
  #         }
  #       }
  #     }
  #   }
  # }

# get_species(species = c("Delia_cristata"),
#             location = c("__MACOSX"),
#             family = c("Pieridae"))
