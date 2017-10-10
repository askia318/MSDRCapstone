#' eq_clean_data() that takes raw NOAA data frame and returns a clean data frame.
#' The clean data frame should have the following:
#' A date column created by uniting the year, month, day and converting it to the Date class.
#' LATITUDE and LONGITUDE columns converted to numeric class
#'
#' @importFrom readr read_delim
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @return a data frame created by uniting the year, month, day and converting it to the Date class. LATITUDE and LONGITUDE columns converted to numeric class
#'
#' @examples eq_clean_data()
#'
#' @export
# setwd("/Users/ccli/Documents/MasterR/Capstone/MSDRCapstone/data")
eq_clean_data <- function(){
    dataurl <-"https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt"

    download.file(dataurl, destfile = "NOAA.txt")

    library(readr)
    noaa <- read_delim(file = "../data/NOAA.txt", delim = "\t")

    df <- noaa %>% dplyr::mutate_(date = ~stringr::str_c(YEAR, "/", MONTH, "/", DAY)) %>%
        dplyr::select_(.dots = c('date', 'LONGITUDE', 'LATITUDE'))

    df$date <- as.Date(df$date, format = "%Y/%m/%d")
    df$LONGITUDE <- as.numeric(df$LONGITUDE)
    df$LATITUDE <- as.numeric(df$LATITUDE)
}

#' eq_location_clean() that cleans the LOCATION_NAME column by stripping out the country
#' name (including the colon) and converts names to title case (as opposed to all caps).
#'
#' @importFrom readr read_delim
#' @importFrom stringr str_split
#' @importFrom stringi stri_trim
#'
#' @return a data frame that cleans the LOCATION_NAME column by stripping out the country name (including the colon) and converts names to title case (as opposed to all caps).
#'
#' @examples eq_location_clean()
#'
#' @export

titlecase <- function (x) {
    # TODO: use Rex package
    gsub("\\b([a-z])([a-z]+)", "\\U\\1\\L\\2", tolower(x), perl = TRUE)
}

firstUpper <- function(x){
    paste(toupper(substr(x, start = 1, stop = 1)),substr(x, start = 2, stop= nchar(x)),sep = "")
}

eq_location_clean <- function(){
    dataurl <-"https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt"

    download.file(dataurl, destfile = "NOAA.txt")

    library(readr)
    noaa <- read_delim(file = "../data/NOAA.txt", delim = "\t")

    library(stringr)
    library(stringi)

    for (i in 1: nrow(noaa)){
        if (is.na(stringr::str_split(noaa$LOCATION_NAME[i], ":")[[1]][2])){
            noaa$LOCATION_NAME[i] <- titlecase(stringi::stri_trim(stringr::str_split(noaa$LOCATION_NAME[i], ":")[[1]][1]))
        }else{
            noaa$LOCATION_NAME[i] <- titlecase(stringi::stri_trim(stringr::str_split(noaa$LOCATION_NAME[i], ":")[[1]][2]))
        }
        noaa$LOCATION_NAME[i] <- firstUpper(noaa$LOCATION_NAME[i])
    }
}
