# Create a docx file and open it
apa_to_docx <- function(fun, x, ...)
{
  tmp <- tempfile("to_apa", fileext = ".md")
  sink(tmp)
  do.call(fun, list(x, format = "rmarkdown", ...))
  sink()
  outfile <- render(tmp, output_format = "word_document", quiet = TRUE)

  sys_open(outfile)
}

# Open a file with standard application on different operating systems
sys_open <- function(filename)
{
  sys <- Sys.info()[['sysname']]

  if (sys == "Windows")
  {
    shell(paste0("\"", filename, "\""))
  }
  else if (sys == "Linux")
  {
    system(paste0("xdg-open \"", filename, "\""))
  }
  else if (sys == "Darwin")
  {
    system(paste0("open \"", filename, "\""))
  }
}
