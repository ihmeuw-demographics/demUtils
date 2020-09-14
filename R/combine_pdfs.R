#' @title Combine pdfs togethers
#'
#' @description Use PyPDF2 python package to combine multiple pdfs into one pdf.
#'
#' @param input_paths \[`character()`\] \cr
#'   Existing filepaths to the pdfs that will be combined.
#' @param output_path \[`character(1)`\] \cr
#'   Output filepath for the combined pdf to code file. The directory must
#'   exist.
#'
#' @return None. `output_path` will be created.
#'
#' @examples
#' \dontrun{
#' combine_pdfs(
#'   input_paths = c("diagnostics/USA.pdf", "diagnostics/ZAF.pdf"),
#'   output_paths = c("diagnostics/combined.pdf")
#' )
#' }
#'
#' @export
combine_pdfs <- function(input_paths, output_path) {

  assertthat::assert_that(all(fs::file_exists(input_paths)))
  assertthat::assert_that(fs::dir_exists(fs::path_dir(output_path)))

  combine_pdfs_PyPDF2(input_paths, output_path)
}
