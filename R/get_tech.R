#' Get list of public studies, with technology used
#'
#' @return a `tibble` of studies and technologies
#' @export

get_tech <- function()
{
    study_tech <-
        httr::GET(
            paste0(BASE_URL,
                '/studies',
                '/technology'),
            httr::add_headers(user_token = getOption('MTBLS_API_KEY'))
        )

    study_tech_parse <- study_tech %>% httr::content('parsed')
    
    study_tech_tibble <-
        purrr::map(study_tech_parse, ~ {
            unlist(.) %>% t() %>% dplyr::as_tibble()
        }) %>% dplyr::bind_rows()
    
    return(study_tech_tibble)
    
}