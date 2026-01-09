library(knitr)

# List of file names to combine
file_names <- c(
  "01_materials",
  "02_recap",
  "03_principles",
  "04_layering",
  "05_aesthetics",
  "06_graph_types",
  "07_panelling",
  "08_themes_and_styling",
  "09_quick_plots",
  "10_utilities",
  "11_harp"
)

# Initialize an empty string to store the combined content
combined_content <- ""

# Loop through each file, read its content, and append to combined_content
for (file_name in file_names) {
  
  # Define input
  input <- paste0(file_name, ".qmd")
  
  # Define output
  output <- paste0(file_name, ".R")
  
  # Create R script for chapter
  purl(input, documentation = 0)
  
  # Read the content of the current file
  file_content <- readLines(output)
  # 
  # # Add the file name as a comment before the content
  # combined_content <- paste(combined_content, paste0("\n\n#### ", output, "\n"))
  
  # Append the content of the current file to combined_content
  combined_content <- paste(combined_content, "\n\n", paste(file_content, collapse = "\n"), sep = "\n")
  
  file.remove(output)
}

# Write the combined content to a new file
writeLines(combined_content, "full_script.R")