# required libraries
library(rvest)
library(xml2)
library(dplyr)


#' Get Eurovision final results from history
#'
#' @param year date in the form YYYY-MM-DD
#' @return a dataframe of Eurovision results


get_eurovision <- function(year) {

  # get url from input and read html
  input <- paste0("https://en.wikipedia.org/wiki/Eurovision_Song_Contest_", year) 
  chart_page <- xml2::read_html(input, fill = TRUE)

  
  # scrape data from any sortable table
  chart <- chart_page %>% 
    rvest::html_nodes("#mw-content-text") %>% 
    xml2::xml_find_all("//table[contains(@class, 'sortable')]")

  charts <- list()
  chartvec <- vector()
  
  for (i in 1:length(chart)) {
    assign(paste0("chart", i),
           # allow for unexpected errors but warn user
           tryCatch({rvest::html_table(chart[[i]], fill = TRUE)}, error = function (e) {print("Potential issue discovered in this year!")})
    )
                    
                    
    charts[[i]] <- get(paste0("chart", i))
    # only include tables that have Points
    chartvec[i] <- sum(grepl("Points", colnames(get(paste0("chart", i))))) == 1 & sum(grepl("Category|Venue|Broadcaster", colnames(get(paste0("chart", i))))) == 0 
  }
    
  results_charts <- charts[chartvec]
  
  # account for move to semifinals and qualifying rounds
  if (year < 1956) {
    stop("Contest was not held before 1956!")
  } else if (year == 1956) {
    stop("Contest was held in 1956 but no points were awarded!")
  } else if (year %in% c(1957:1995)) {
    results_charts[[1]]
  } else if (year == 1996) {
    results_charts[[2]]
  } else if (year %in% 1997:2003) {
    results_charts[[1]]
  } else if (year %in% 2004:2007) {
    results_charts[[2]]
  } else {
    results_charts[[3]]
  }
  
}

# test function in case of weird changes in Wikipedia format

test_eurovision <- function() {

  for (i in 1957:2018) {
    message(paste("Testing", i))
    test <- get_eurovision(i)
    if (sum(!(grepl("Points", colnames(test)))) == 0) {
      stop(paste("Error detected at year", i))
    }
  }
}

# now scrape all historic data

df <- get_eurovision(1957)

colnames(df)[grepl("Country", colnames(df))] <- "Country"
colnames(df)[grepl("Points", colnames(df))] <- "Points"

df <- df %>% 
  dplyr::select(Country, Points) %>% 
  dplyr::mutate(Year = 1957,
                Country = gsub("\\[.*?\\]", "", Country),
                Points = gsub("\\[.*?\\]", "", Points))

all_time_eurovision <- df


for (i in 1958:2018) {
  
  message(i)
  df <-  get_eurovision(i) 
  
  colnames(df)[grepl("Country", colnames(df))] <- "Country"
  colnames(df)[grepl("Points", colnames(df))] <- "Points"
  
  df <- df %>% 
    dplyr::select(Country, Points) %>% 
    dplyr::mutate(Year = i,
                  Country = gsub("\\[.*?\\]", "", Country),
                  Points = gsub("\\[.*?\\]", "", Points))

  all_time_eurovision <- all_time_eurovision %>% 
    dplyr::bind_rows(df) %>% 
    dplyr::select(Year, Country, Points) 
  
}

all_time_eurovision <- all_time_eurovision %>% 
  dplyr::mutate(Points = as.integer(Points)) %>% 
  dplyr::arrange(Year, Points, Country)

winners <- all_time_eurovision %>% 
  dplyr::arrange(Year, -Points) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::mutate(Order = row_number()) %>% 
  dplyr::filter(Order == 1) %>% 
  dplyr::select(Year, Winner = Country)

winners[winners$Year == 1969, c("Winner")] <- "Spain, UK, Netherlands, France"

# now gather cumulative totals removing superscript 2's

eurovision_total <- all_time_eurovision %>% 
  dplyr::filter(Year == 1957) %>% 
  dplyr::arrange(-Points) %>% 
  dplyr::mutate(Order = row_number()) %>% 
  dplyr::filter(Order <= 10) %>% 
  dplyr::mutate(Order = 11 - Order)


for (i in 1958:2018) {
  eurovision_total <- all_time_eurovision %>% 
    dplyr::mutate(Country = gsub("2", "", Country)) %>% 
    dplyr::filter(Year <= i) %>% 
    dplyr::group_by(Country) %>% 
    dplyr::summarise(Points = sum(Points)) %>% 
    dplyr::mutate(Year = i) %>% 
    dplyr::arrange(-Points, Country) %>% 
    dplyr::mutate(Order = row_number()) %>%
    dplyr::filter(Order <= 10) %>% 
    dplyr::mutate(Order = 11 - Order) %>% 
    dplyr::bind_rows(eurovision_total) %>% 
    dplyr::select(Year, Country, Points, Order) %>% 
    dplyr::arrange(Year, -Order) 
}

eurovision_total <- dplyr::inner_join(eurovision_total, winners)

saveRDS(eurovision_total, "eurovision_total.RDS")


