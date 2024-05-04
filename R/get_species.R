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
                        family = NULL,
                        place = c("__MACOSX",
                                  "sample frames, landmark data")) {

  if (length(species) == 0 && is.null(family)) {
    cat("Please, insert a species name and/or a family name to download.\n Use the get_splist() function to see the names.\n")
  }

  # URLs of all species ----
  species_urls <- list(
    "Delia_cristata" = "https://datadryad.org/stash/downloads/file_stream/3103278"
    # >>> Add more species here <<<
  )

  # Species for families
  species_families <- list(
    "Delia_cristata" = "Pieridae"
    # >>> Add more mapping here <<<
  )

  # Creating "butterfly_data" file for downloads ----
  species_folder <- "Butterfly_species"
  dir.create(species_folder, showWarnings = FALSE)

  # Download individual species by name ----
  for (sp in species) {
    if (sp %in% names(species_urls)) {
      zip_file <- paste0(sp, ".zip")
      zip_path <- file.path(species_folder, zip_file)
      utils::download.file(url = species_urls[[sp]], destfile = zip_path, mode = "wb")
      utils::unzip(zip_path, exdir = file.path(species_folder, sp))

      # Delete place files
      folders_to_keep <- intersect(place, c("__MACOSX", "sample frames, landmark data"))
      folders_to_delete <- setdiff(c("__MACOSX", "sample frames, landmark data"), folders_to_keep)

      for (folder in folders_to_delete) {
        folder_path <- file.path(species_folder, sp, folder)
        if (file.exists(folder_path)) {
          unlink(folder_path, recursive = TRUE)
        }
      }

      cat("Species", sp, "was successfully downloaded in:\n", getwd(), "\n")

    } else {
      cat("Species", sp, "not found. Please check the species list in the dataset\n")
    }
  }

  # Download species by family if family is selected ----
  if (!is.null(family)) {
    for (fam in family) {
      if (fam %in% unique(unlist(species_families))) {
        family_folder <- paste0("Family_", fam)
        dir.create(family_folder, showWarnings = FALSE)
        species_in_family <- names(Filter(function(x) x == fam, species_families))

        for (sp in species_in_family) {
          if (sp %in% names(species_urls)) {
            zip_file <- paste0(sp, ".zip")
            zip_path <- file.path(family_folder, zip_file)
            utils::download.file(url = species_urls[[sp]], destfile = zip_path, mode = "wb")
            utils::unzip(zip_path, exdir = file.path(family_folder, sp))

            # Delete place files
            folders_to_keep <- intersect(place, c("__MACOSX", "sample frames, landmark data"))
            folders_to_delete <- setdiff(c("__MACOSX", "sample frames, landmark data"), folders_to_keep)

            for (folder in folders_to_delete) {
              folder_path <- file.path(family_folder, sp, folder)
              if (file.exists(folder_path)) {
                unlink(folder_path, recursive = TRUE)
              }
            }

            cat("Family", fam, "was successfully downloaded in:\n", getwd(), "\n")
          } else {
            cat("Family", fam, "not found. Please check the family list in the dataset\n")
          }
        }
      }
    }
  }
}

# get_species(species = c("Delia_cristata"),
#             place = c("__MACOSX"),
#             family = c("Pieridae"))
