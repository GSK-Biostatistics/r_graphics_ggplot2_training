# Training Materials

This repo contains the source for the R materials deployed on [WARP View](https://warp-view.gsk.com/training/r/ggplot2-graphics/).

# Editing the Materials

NEVER edit the 'main' or 'preview' branch directly.  Always create a new branch by forking the 'preview' branch and work on changes there.

1. Make your updates to the qmd files
2. Render/serve the book locally
3. Generate the course script
4. Create the manifest file 
5. Commit and push changes
6. Copy the new course script to the shared location on WARP (if you have permission, else contact the directory owner)

## Updating the Rmd files

The training materials are compiled as a Quarto book document.  Everything is now triggered from `_quarto.yml`.

Note that exercise solutions are provided within the qmd files but they are not included in the published bookdown (`echo=FALSE`).  In order to update the solutions in the course script (`full_script.R`) you must update these hidden code chunks.

## Local rendering

Perhaps obvious, but always check that your changes don't break anything and that the book runs locally.
To render the book locally run the following command in the terminal:

```{r}
Quarto Render
```

## Generating the course script

The course script is contained in `full_script.R`.  This is file must NEVER BE UPDATED MANUALLY.  To update the course script run Create_full_script.R:

```{r}
source("create_full_script.R", echo=TRUE)
```

## Create the Manifest

The training materials are automatically deployed to RStudio Connect once a change is detected on either the master (for the primary materials) or test/preview (for testing) branch in GitHub.  You *must* update the manifest before pushing changes for this to work as expected:

```{r}
rsconnect::writeManifest()
```

