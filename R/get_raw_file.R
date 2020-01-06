#' Get raw file
#'
#' @param study_id A character string of a valid MTBLS study id
#' @param filename A character string of a study file to read as a table
#' @param outpath A character string of a path to save the raw file
#' @param overwrite Default FALSE. Set to TRUE to overwrite an existing file.
#' @export

get_raw_file <- function(study_id, filename, outpath, overwrite=FALSE)
{
  response <-
    httr::GET(
      paste0(BASE_URL,
        '/studies/',
        study_id,
        '/download/',URLencode(filename)),
      httr::add_headers(user_token = getOption('MTBLS_API_KEY'),
        Accept ='application/json',
        `Content-Type`='application/json')
    )

  if (file.exists(file.path(outpath,filename)) && !overwrite) {
    stop('File already exists. Use overwrite = TRUE to override.')
  }

  bin = httr::content(response,'raw')
  writeBin(bin,file.path(outpath,filename))

  return(invisible(NULL))
}
