#' Download Course Materials from GitHub
#'
#' This function downloads the data folder, the main course script, 
#' and the RProject file into a structured local directory.
#'
#' @param owner String. The GitHub username or organization (e.g., "GSK-Biostatistics").
#' @param repo String. The name of the repository.
#' @param path String. The folder containing datasets (e.g., "data").
#' @param branch String. The branch name (default is "main").
#' @param dest_dir String. The local "Base" folder (e.g., "~/tidyverse_training").
#'
#' @export
#'
#' @examples
#' \dontrun{
#' download_data(
#'   owner = "GSK-Biostatistics",
#'   repo = "r_graphics_ggplot2_training",
#'   path = "data",
#'   dest_dir = "~/Tidyverse_Training"
#' )
#' }
download_data <- function(owner, repo, path, branch = "main", dest_dir = "directory") {
  
  # 1. Create the BASE directory (where .Rproj and scripts go)
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }
  
  # 2. Create the DATA sub-folder
  data_dir <- file.path(dest_dir, "data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir)
  }
  
  # 3. Download the root files (into the BASE folder)
  root_files <- c("full_script.R", "r_graphics_ggplot2_training.Rproj")
  
  message("--- Setting up Project Root ---")
  for (f in root_files) {
    raw_url <- paste0("https://raw.githubusercontent.com/", owner, "/", repo, "/", branch, "/", f)
    dest_path <- file.path(dest_dir, f) # Stays in the main folder
    
    tryCatch({
      download.file(raw_url, destfile = dest_path, mode = "wb", quiet = TRUE)
      message("Downloaded: ", f)
    }, error = function(e) message("Skipping ", f, " (not found in root)."))
  }
  
  # 4. Download the data folder contents (into the /data folder)
  api_url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/contents/", path, "?ref=", branch)
  
  res <- httr::GET(api_url)
  httr::stop_for_status(res)
  files <- jsonlite::fromJSON(httr::content(res, as = "text"))
  
  message("\n--- Downloading Data Files ---")
  for (i in 1:nrow(files)) {
    if (files$type[i] == "file") {
      file_url  <- files$download_url[i]
      file_name <- files$name[i]
      dest_path <- file.path(data_dir, file_name) # Put in /data/
      
      download.file(file_url, destfile = dest_path, mode = "wb", quiet = TRUE)
      message("Downloaded to /data: ", file_name)
    }
  }
  
  message("\nSetup Complete!")
  message("Location: ", normalizePath(dest_dir))
}