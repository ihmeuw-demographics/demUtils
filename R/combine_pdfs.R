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
#' @return Invisibly returns `output_path`..
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

  # eliminate any symbolic links and special references like '~' or '..' for PyPDF2
  input_paths <- fs::path_real(input_paths)
  assertthat::assert_that(all(fs::file_exists(input_paths)))

  output_file <- fs::path_file(output_path)
  output_dir <- fs::path_dir(output_path)
  output_dir <- fs::path_real(output_dir)
  assertthat::assert_that(fs::dir_exists(output_dir))
  output_path <- fs::path(output_dir, output_file)

  PyPDF2 <- reticulate::import("PyPDF2")
  combined_pdf <- PyPDF2$PdfFileMerger()
  for (path in input_paths) {
    pdf <- PyPDF2$PdfFileReader(path)
    combined_pdf$append(pdf)
  }
  combined_pdf$write(output_path)

  return(invisible(output_path))
}
