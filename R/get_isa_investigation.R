#' Get ISA Investigation File
#'
#' @param study_id A character string of a valid MTBLS study id
#' @param outpath A character string of a filepath where the ISA Investigation `.txt` file will be saved to
#' @param overwrite Default FALSE. Set to TRUE to overwrite an existing file.
#' @export

get_isa_investigation <- function(study_id, outpath, overwrite=FALSE)
{
  isa_investigation <-
    httr::GET(
      paste0(BASE_URL,
             '/studies/',
             study_id,
             '/investigation'),
      httr::add_headers(user_token = getOption('MTBLS_API_KEY'))
    )


  isa_investigation_parse <-
    isa_investigation %>% httr::content('text')

  isa_clean <-
    rvest::html_text(xml2::read_html(isa_investigation_parse))


  fn = paste0(outpath, '/', study_id, '_ISA_investigation.txt')
  if (file.exists(fn) && !overwrite) {
    stop('File already exists. Use overwrite = TRUE to override.')
  }

  writeLines(isa_clean, con = fn)

  return(invisible(NULL))
}
