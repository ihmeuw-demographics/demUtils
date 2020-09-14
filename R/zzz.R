# as explained here https://cran.r-project.org/web/packages/reticulate/vignettes/package.html
# https://cran.r-project.org/web/packages/reticulate/vignettes/python_dependencies.html
# https://github.com/rstudio/reticulate/issues/337#issuecomment-563077433

.onLoad <- function(libname, pkgname) {

  reticulate::configure_environment(pkgname, force = TRUE)

}
