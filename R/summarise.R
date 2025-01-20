

#' Summarise contents of the Oz butterflies database
#'
#' @param db_folder Path of folder that contains the Oz_butterflies database.
#'
#' @returns Data frame with 1 row and columns that summarise the database
#'   contents. All summary statistics, apart from the `Images` count, describe
#'   the entire database, regardless of whether the entire database or a subset
#'   is installed locally.
#'
#' @export
Oz_butterflies_summary <- function(db_folder = "Oz_butterflies") {
  # Read meta data
  descr <- read.csv(file.path(db_folder, "Oz_butterflies.csv"))

  imgs <- list.files(db_folder, pattern = "\\.JPG$|\\.jpg$|\\.PNG$|\\.png$", recursive = TRUE)

  # Individuals per species
  ips <- aggregate(list(Count = descr$ID), by = list(Species = descr$Binomial), FUN = length)

  data.frame(Families = length(unique(descr$Family)),
             Genera = length(unique(descr$Genus)),
             Species = length(unique(descr$Binomial)),
             Specimens = length(unique(descr$ID)),
             Females = sum(descr$Sex == "Female"),
             Males = sum(descr$Sex == "Male"),
             Images = length(imgs),
             Sites = length(unique(descr$Site)),
             "Ind./species max" = max(ips$Count),
             "Ind./species mean" = mean(ips$Count),
             "Ind./species median" = median(ips$Count),
             check.names = FALSE)
}
