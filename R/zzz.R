# as explained here https://cran.r-project.org/web/packages/reticulate/vignettes/package.html
# https://github.com/rstudio/reticulate/issues/337#issuecomment-563077433

combine_pdfs_py <- NULL

.onLoad <- function(libname, pkgname) {
  # load `combine_pdfs_py` into environment
  reticulate::source_python(
    file = system.file("python/combine_pdfs.py", package = "demUtils")
  )
  combine_pdfs_py <<- combine_pdfs_py
}
