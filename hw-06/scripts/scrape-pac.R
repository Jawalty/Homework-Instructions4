## load packages ----------------------------------------------------------------
#
library(tidyverse)
library(rvest)
library(here) 
#
## function: scrape_pac ---------------------------------------------------------
#
scrape_pac <- function(url) {
  
  # read the page
  page <- read_html(
    httr::content(
      httr::GET(url, httr::user_agent("Mozilla/5.0")),
     as = "text", encoding = "UTF-8"
    )
  )
  
  # extract the table
  pac <- page |>
    html_node(".DataTable-Partial") |>
    html_table(header = TRUE, fill = TRUE) |>
    as_tibble()
  
  # rename variables
  pac <- pac |>
    rename(
      name = 1,
      country_parent = 2,
      total = 3,
      dems = 4,
      repubs = 5
    )
  
  # fix name
  pac <- pac |>
    # remove extraneous whitespaces from the name column
    mutate(name = str_squish(name))
  
  # add year
  pac <- pac |>
    # extract last 4 characters of the URL and save as year
    mutate(year = str_sub(url, -4))
  
  # return data frame
  pac
  
}
#
## test function ----------------------------------------------------------------
#
url_2024 <- "https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2024"
pac_2024 <- scrape_pac(url_2024)
#
url_2020 <- "https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2020"
pac_2020 <- scrape_pac(url_2020)
#
url_2000 <- "https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2000"
pac_2000 <- scrape_pac(url_2000)
#
## list of urls -----------------------------------------------------------------
#
## first part of url
#root <- "https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/"
#
## second part of url (election years as a sequence)
#year <- seq(from = ___, to = ___, by = ___)
#
## construct urls by pasting first and second parts together
#urls <- paste0(___, ___)
#
## map the scrape_pac function over list of urls --------------------------------
#
#pac_all <- ___(___, ___)
#
## write data -------------------------------------------------------------------
#
#write_csv(___, file = here::here("data/pac-all.csv"))