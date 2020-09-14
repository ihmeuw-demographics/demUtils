import PyPDF2

def combine_pdfs_py(input_paths, output_path):
  """Combine PDFs into a single file
    Parameters
    ----------
    output_path : str
        Filepath for combined PDF output
    input_paths : str
        List of input PDFs in order to be combined
    Returns
    -------
    Nothing. Combined PDF saved to `output_path`.
  """

  combined_pdf = PyPDF2.PdfFileMerger()

  for input_path in input_paths:

    with open(input_path, 'rb') as input_file:
      pdf = PyPDF2.PdfFileReader(input_file)
      combined_pdf.append(pdf)

  combined_pdf.write(output_path)
