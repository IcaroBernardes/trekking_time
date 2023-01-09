# 1. Initial setup ##############
## Loads libraries
library(dplyr)
library(forcats)
library(glue)
library(httr)
library(polite)
library(purrr)
library(readr)
library(rtrek)
library(rvest)
library(stringi)
library(stringr)
library(xml2)

## Downloads the data
tlBooks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-27/tlBooks.csv')
tlFootnotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-27/tlFootnotes.csv')

## Creates a polite version of GET
pGET <- polite::politely(httr::GET)

# 2. Scrapping image names and articles urls ##############
## Gets unique names of the novels and translates them
## to the general style of url found in Memory Alpha
unique_titles <- tlBooks %>% 
  dplyr::distinct(title) %>% 
  dplyr::mutate(url = stringr::str_replace_all(title, " ", "_"),
                url = stringi::stri_trans_general(url, "latin-ascii"),
                url = glue::glue("https://memory-alpha.fandom.com/wiki/{url}"))

## Tries to get the url of the images for each novel
unique_titles <- unique_titles %>% 
  dplyr::mutate(img = purrr::imap_chr(url, function(url, i) {
    
    print(glue::glue("scrapping title #{i}..."))
    
    ### Asks for the 'src' attribute of the novel thumbnail
    path = url %>% 
      pGET() %>% 
      httr::content() %>% 
      rvest::html_element(".pi-image-thumbnail") %>% 
      xml2::xml_attr("src")
    
    ### Tries another format of url if the default didn't work
    if (stringr::str_detect(url, ":") & is.na(path)) {
      
      path = url %>% 
        stringr::str_remove("(?<=https://memory-alpha.fandom.com/wiki/)(.+):_") %>% 
        pGET() %>% 
        httr::content() %>% 
        rvest::html_element(".pi-image-thumbnail") %>% 
        xml2::xml_attr("src")
      
    }
    
    return(path)
    
  }))

## Keeps only the url of articles on which the scrap was successful
unique_titles <- unique_titles %>% 
  dplyr::mutate(url = ifelse(is.na(img), NA, url))

## Extracts the image name from the 'src'
unique_titles <- unique_titles %>% 
  dplyr::mutate(img = stringr::str_remove(img, "(?<=(jpg|png)).+"),
                img = stringr::str_split_i(img, "/", -1))

## Saves the data
saveRDS(unique_titles, "data/titles_img.RDS")

# 3. Downloading images ##############
## Lists the names of images of the articles that were found
article_found <- unique_titles %>% 
  dplyr::filter(!is.na(img)) %>% 
  dplyr::pull(img)

## Download the images from Memory Alpha with the aid of rtrek::ma_image()
article_found %>% 
  purrr::iwalk(function (x, y) {
    
    print(glue::glue("downloading image #{y}..."))
    
    try({
      rtrek::ma_image(glue::glue("File:{x}"),
                      file = glue::glue("www/images/{x}"),
                      keep = TRUE)
    })
    
  })

## Identifies failed downloads and articles not found
## and gives them a placeholder image
no_image <- article_found[!(article_found %in% list.files("www/images"))]
no_image <- unique_titles %>%
  dplyr::filter(is.na(img) | img %in% no_image) %>% 
  dplyr::pull(title)
unique_titles <- unique_titles %>% 
  dplyr::mutate(img = ifelse(title %in% no_image, "placeholder.png", img))

## Saves the data
saveRDS(unique_titles, "data/titles_img.RDS")

# 4. Handling the data ##############
## Applies the categorization of Memory Beta to the years
## (https://memory-beta.fandom.com/wiki/Memory_Beta_Chronology)
timeline <- tlBooks %>% 
  dplyr::mutate(
    era = case_when(year < 0 ~ "Distant Past",
                    year >= 0 & year < 1900 ~ "Recent Past",
                    year >= 1900 & year < 2000 ~ "20th Century",
                    year >= 2000 & year < 2100 ~ "21th Century",
                    year >= 2100 & year < 2200 ~ "22th Century",
                    year >= 2200 & year < 2300 ~ "23th Century",
                    year >= 2300 & year < 2400 ~ "24th Century",
                    year >= 2400 & year < 2500 ~ "25th Century",
                    year >= 2500 ~ "Far Future"),
    era = factor(era, levels = c("Distant Past", "Recent Past",
                                 "20th Century", "21th Century", "22th Century",
                                 "23th Century", "24th Century", "25th Century",
                                 "Far Future"))
  )

## Makes explicit when a novel does not belong to any series
timeline <- timeline %>% 
  dplyr::mutate(series = factor(series),
                series = forcats::fct_explicit_na(series, na_level = "-"))

## Associates the series' names with their abbreviations
abbrev <- tibble(
  series = factor(c("TNG", "TOS", "VOY", "DS9", "ST",
                    "NF", "TLE", "ENT", "TAS", "SCE",
                    "CHA", "SGZ", "TTN", "-"),
                  levels = levels(timeline$series)),
  series_name = c("The Next Generation",
                  "The Original Series",
                  "Voyager",
                  "Deep Space Nine",
                  "All-series/ Crossover",
                  "New Frontier",
                  "The Lost Era",
                  "Enterprise",
                  "The Animated Series",
                  "Starfleet Corps of Engineers",
                  "Challenger",
                  "Stargazer",
                  "Titan",
                  "No series")
)
timeline <- timeline %>% dplyr::left_join(abbrev)

## Converts 'format' and 'setting' to factor
timeline <- timeline %>% 
  dplyr::mutate(format = factor(format),
                setting = factor(setting, levels = c("primary", "secondary")))

## Saves the data
saveRDS(timeline, "data/timeline.RDS")
