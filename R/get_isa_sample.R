#' Get ISA Sample File
#'
#' @param study_id A character string of a valid MTBLS study id
#' @param the study filename fo the ISA sample file
#' @param outpath A character string of a filepath where the ISA sample `.txt` file will be saved to
#' @param overwrite Default FALSE. Set to TRUE to overwrite an existing file.
#' @export

get_isa_sample <- function(study_id, filename, outpath, overwrite=FALSE)
{
  response <-
    httr::GET(
      paste0(BASE_URL,
        '/studies/',
        study_id,
        '/sample'),query=list(sample_filename=filename),
      httr::add_headers(user_token = getOption('MTBLS_API_KEY'))
    )

  response_parse <-
    response %>% httr::content('text')

  fn = paste0(outpath, '/', study_id, '_ISA_sample.txt')
  if (file.exists(fn) && !overwrite) {
    stop('File already exists. Use overwrite = TRUE to override.')
  }

  writeLines(response_parse, con = fn)
  return(invisible(NULL))
}
