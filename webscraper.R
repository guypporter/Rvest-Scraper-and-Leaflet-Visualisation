library(pacman)
pacman::p_load(rvest, dplyr, stringr, purrr, lubridate, tibble, tidyr, stringi, stringr)

#Insert Query into search field on Change.Org
url <- 'https://www.change.org/search?q='

scrape_change_page <- function(url)
{ webpage  <- xml2::read_html(url)
  
  get_text <- function(css) 
  {
    vec <- rvest::html_text(rvest::html_nodes(webpage, css), trim = TRUE)
    if(length(vec) < 10) c(vec, rep("", 10 - length(vec))) else vec
  }

  get_attr <- function(css, attr)  #to scrape urls
    vec <- rvest::html_attr(rvest::html_nodes(webpage, css), attr)
    if(length(vec) < 10) c(vec, rep("", 10 - length(vec))) else vec
  }
  
  dplyr::tibble(
    title         = get_text('.xs-mbs'),
    date          = gsub("Created", "", get_text('.symbol-clock+ span')),
    supporters    = gsub(" supporters", "", get_text('.symbol-supporters+ span')),
    addressee     = gsub("Petition to ", "", get_text('.xs-mbn .type-s')),
    location      = get_text('.plxxs'),
    link          = get_attr('.search-results .list-rule a.link-block.js-click-search-result', 'href')
  )
}

scrape_change_page(url)

#select number of pages (3 as a test run before running across the whole set)
n_pages <- 164
urls    <- paste0(url, "&offset=", 10 * (seq(n_pages)) - 1)
petitions1  <- do.call(rbind, lapply(urls, scrape_change_page))

#add base url to link column
petitions1$link <- paste0("https://www.change.org", statues$link )

# option to add multiple searches e.g. 'ppe' and 'personal protective equipment' together
petitions_final <- rbind(petitions1, petitions2)

#Dedupe by column and option to save as CSV (deudupe by unique 'link')
result_final_all <- statues_final %>% distinct(link, .keep_all = TRUE)
write.csv(result_final_all,'result_final.csv')
