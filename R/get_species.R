library(readxl)

#' Download files from Dryad...
#'
#' This function allows downloading files from Dryad for a specific butterfly species or family...
#'
#' @param species Character vector containing the names of the species to be downloaded. Default is empty.
#' @param family Character vector containing the names of the families from which all species will be downloaded. Default is NULL.
#' @param place Character vector containing the names of places to be excluded after download. Default is c("__MACOSX", "sample frames, landmark data").
#'
#' @examples
#'\dontrun{
#' # Download files for a single species
#' get_species(species = "Delia_cristata")
#'
#' # Download files for a more the one species
#' get_species(species = c("Delia_cristata","Delia_cumulanta"))
#'
#' # Download files for an entire family
#' get_species(family = "Pieridae")
#'
#' # Download files for species and families of a specific place
#' get_species(species = c("Delia_cristata", "Delia_cumulanta"),
#'             family = c("Pieridae", "Hesperidae"),
#'             place = "Brisbane")
#'}
#' @export
get_species <- function(species = c(),
                        db_folder = "australian_butterflies",
                        family = NULL,
                        place = c("__MACOSX",
                                  "sample frames, landmark data")) {

  if (length(species) == 0 && is.null(family)) {
    cat("Please, insert a species name and/or a family name to download.\n Use the get_splist() function to see the names.\n")
  }

  # Verifica se a pasta "db_folder" (onde os dados serão armazenados) existe.
  # Se não existir, cria a pasta.
  if(!dir.exists(db_folder)) {
    dir.create(db_folder)
  }

  # Lista todos os arquivos locais presentes na pasta especificada (neste caso, "data_buttr")
  files <- listLocalFiles("C:\\Users\\Diogo Silva\\Desktop\\data_buttr")

  # Filtra a URL do arquivo "australian_butterflies.xlsx" a partir dos arquivos listados
  url <- files$url[files$file == "australian_butterflies.xlsx"]

  # Define o caminho onde a planilha será salva localmente
  spread_sheet <- file.path(db_folder, "australian_butterflies.xlsx")

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

  # Filtra as linhas da tabela onde a família corresponde a uma das famílias de interesse
  rows <- meta_data$Family %in% family

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

  # Download individual species by name ----
  # for (sp in species) {
  #   if (sp %in% names(species_urls)) {
  #     zip_file <- paste0(sp, ".zip")
  #     zip_path <- file.path(species_folder, zip_file)
  #     utils::download.file(url = species_urls[[sp]], destfile = zip_path, mode = "wb")
  #     utils::unzip(zip_path, exdir = file.path(species_folder, sp))

      # Delete place files
  #     folders_to_keep <- intersect(place, c("__MACOSX", "sample frames, landmark data"))
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
  #           # Delete place files
  #           folders_to_keep <- intersect(place, c("__MACOSX", "sample frames, landmark data"))
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
}

# get_species(species = c("Delia_cristata"),
#             place = c("__MACOSX"),
#             family = c("Pieridae"))
