testthat::test_that("combine_pdfs works", {

  testdir <- tempdir()
  testfiles <- paste0("testfile", 1:5, ".pdf")
  testfullpaths <- paste0(testdir, "/", testfiles)

  # create pdfs with example histogram
  set.seed(3)
  for (fullpath in testfullpaths) {
    pdf(fullpath)

    # create plot
    testdata <- rnorm(1000)
    hist(testdata)
    title(sub = fullpath)

    dev.off()
  }

  output_path <- paste0(testdir, "/combined_testfile.pdf")
  combine_pdfs(
    input_paths = testfullpaths,
    output_path = output_path
  )

  testthat::expect_identical(unname(fs::file_exists(output_path)), TRUE)
})
