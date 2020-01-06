#' Get tsv file from study as a tibble
#'
#' @param study_id A character string of a valid MTBLS study id
#' @param tsv_file A character string of the filename to get
#' @return a `tibble` of tsv data
#' @export
get_tsv<- function(study_id, tsv_file)
{
  tsv_response <-
    httr::GET(
      paste0(BASE_URL,
        '/studies/',
        study_id,
        '/',URLencode(tsv_file)),
      httr::add_headers(user_token = getOption('MTBLS_API_KEY'))
    )


  tsv_response_parse <-
    tsv_response %>% httr::content('parsed')

  tsv_clean=purrr::map(tsv_response_parse$data$rows, dplyr::as_tibble) %>% dplyr::bind_rows()

  return(tsv_clean)
}
