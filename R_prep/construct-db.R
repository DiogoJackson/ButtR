# Rearrange files from the temporary development structure into correct dbs structure
library(JUtils)
library(readxl)

# Basename of the metadata spreadsheet file
METADATA_BASENAME <- "Oz_butterflies"

maybeCreateDir <- function(dir) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}

readDbMetadata <- function(dir) {
  meta <- file.path(dir, paste0(METADATA_BASENAME, ".csv"))
  if (!file.exists(meta)) {
    stop(sprintf("Metadata spreadsheet (%s) not found", meta))
  }
  utils::read.csv(meta, check.names = FALSE)
}

# Returns a list of the samples that have files in the specified directory.
# Assumes that file names start with the sample ID
getFileSamples <- function(dir, ext, recursive = FALSE) {
  psf <- list.files(dir, ext, full.names = FALSE, recursive = recursive)
  sample <- sub("[^[:digit:]].*$", "", psf)
  sample <- unique(sample)
  sample <- sample[sample != ""]
}

# Copy files from indir to dest
# Files are named using fmt as an sprintf format with one argument, id.
copySampleFiles <- function(id, indir, fmt, dest, min) {
  f <- list.files(indir, sprintf(fmt, id), full.names = TRUE)
  # Replace "-" in original file name with "_"
  outfile <- file.path(dest, gsub("-", "_", basename(f)))
  for (i in seq_along(f)) {
    file.copy(f[i], outfile[i])
  }
  if (length(f) < min) {
    stop(sprintf("Missing files for sample ID %s: need %d, found %d", id, min, length(f)))
  }
}

# Copy and rename spec legend file and convert Excel to CSV and rename it for a specimen
copySpecFiles <- function(id, indir, dest) {
  # Image file
  refImg <- file.path(indir, sprintf("reflectance_%s.png", id))

  # Spec file
  specFile <- file.path(indir, sprintf("reflectances_%s.xlsx", id))

  if (file.exists(refImg) && !file.exists(specFile)) stop(sprintf("Spec plot exists but Excel spec file missing in %s", indir))
  if (!file.exists(refImg) && file.exists(specFile)) stop(sprintf("Excel spec file exists but Spec plot missing in %s", indir))

  if (file.exists(refImg)) {
    file.copy(refImg, file.path(dest, sprintf("%s_reflectance.png", id)))
  }

  if (file.exists(specFile)) {
    spec <- read_excel(specFile)
    write.csv(spec, file.path(dest, sprintf("%s_reflectance.csv", id)), row.names = FALSE)
  }
}


organiseDB <- function(indir, outdir) {
  md <- readDbMetadata(indir)

  # Remove some columns
  md$Exclude <- NULL
  md$Pinned <- NULL

  # Check images exist for every sample
  bad <- FALSE
  samples <- unique(md$ID)
  img <- table(sub("-.*", "", list.files(indir, "*.ARW")))
  noImg <- which(!samples %in% names(img))
  if (length(noImg) > 0) {
    cat("Samples without images\n")
    print(samples[noImg])
    bad <- TRUE
  }
  noSam <- which(!names(img) %in% samples)
  if (length(noSam) > 0) {
    cat("Images without samples\n")
    print(names(img)[noSam])
    bad <- TRUE
  }
  if (any(img < 2)) {
    cat("Sample with < 2 images\n")
    print(names(img)[which(img < 2)])
    bad <- TRUE
  }
  if (bad) stop("Giving up!")

  # Update metadata to record presence of spec files
  specs <- getFileSamples(file.path(indir, "ProcSpec files"), ".ProcSpec", recursive = TRUE)
  md$Spectra <- ifelse(md$ID %in% specs, "y", "n")
  if (sum(md$Spectra == "y") != length(specs)) stop(sprintf("Speced samples not all found in spreadsheet! Missing from spreadsheet: ",
                                                           specs[!specs %in% md$ID]))

  # Update metadata to record presence of DNA files
  dna <- getFileSamples(file.path(indir, "DNA files"), ".ab1")
  md$DNA <- ifelse(md$ID %in% dna, "y", "n")
  if (sum(md$DNA == "y") != length(dna)) {
    for (s in dna) {
      if (!s %in% unique(md$ID)) {
        message(sprintf("DNA for unknown sample %s", s))
      }
    }
  }


  # Now create the correctly structured database
  maybeCreateDir(outdir)
  write.csv(md, file = file.path(outdir, paste0(METADATA_BASENAME, ".csv")), row.names = FALSE)

  pb <- JBuildProgressBar("win", numItems = length(unique(md$ID)), title = "Progress")
  on.exit(pb(close = TRUE))

  # For each sample...
  for (s in samples) {
    r <- which(md$ID == s)[1]
    sampleDir <- file.path(outdir, md$Family[r], paste(md$Genus[r], md$Species[r], sep = "_"), s)
    maybeCreateDir(sampleDir)

    # Copy image files
    copySampleFiles(s, indir, "^%s-.*\\.ARW$", sampleDir, min = 2)

    # Copy DNA files
    copySampleFiles(s, file.path(indir, "DNA files"), "^%s-f\\.ab1$", sampleDir, min = 0)

    # Copy Spec files
    copySampleFiles(s, file.path(indir, "ProcSpec files", s), "^%s_.*\\.ProcSpec$", sampleDir, min = 0)
    # Copy and rename spec legend images and Excel spec file
    copySpecFiles(s, file.path(indir, "ProcSpec files", s), sampleDir)

    # Copy Spec labels. They can be named "<id>.jpg", "<id>_v.jpg" or "<id>_d.jpg"
    copySampleFiles(s, file.path(indir, "Spec labels"), "^%s(-[d|v])?\\.jpg$", sampleDir, min = 0)

    # Update progress bar
    pb()
  }
}


indir <- "D:/Update all things database"
outdir <- "D:/Oz_Butterflies"

organiseDB(indir, outdir)
