# This suite of functions is meant to make it easy to scrape and munge results from cyclocross
# races archived at https://www.crossresults.com/ in R.

# function to pull and clean results for a single race by id no
cx_results <- function(idno) {

    require(jsonlite)

    df <- fromJSON(sprintf("https://www.crossresults.com/downloadrace.php?raceID=%s&json=1", idno))

    if (length(df) == 0) {

        df <- NULL

    } else {

        # remove the duplicate cols, which have numbers as names
        df <- df[,grepl("^[A-z]+", colnames(df))]

        # add id no to df as col to facilitate merging with event metadata
        df$event_id <- idno

    }

    return(df)

}

# function to scrape metadata for a single race by id no
cx_metadata <- function(idno) {

    require(dplyr)
    require(stringr)
    require(rvest)
  
    url <- sprintf("https://www.crossresults.com/race/%s", idno)

    page <- read_html(url)

    date <- page %>%
        html_nodes("div.headerrow1") %>%
        html_text() %>%
        # This returns several items, and I can't figure out how to select the one I want, so
        # we'll use a regex to find and extract the date in that vector of strings instead.
        str_extract("[A-z]{3} [0-9]{1,2} [0-9]{4}") %>%
        .[!is.na(.)] %>%
        as.Date(., format = "%b %d %Y")

    if (length(date) == 0) { date <- NA }

    # would like to include region but can't find it on race page...

    metadata <- list(url = url, date = date, event_id = idno)

    return(metadata)

}

# function to scrape id no for most recent race; useful for establishing end of a seq of race ids
# or just fetching latest results
latest_race <- function() {

    require(dplyr)
    require(rvest)

    read_html("https://www.crossresults.com/?n=results&sn=all") %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        str_subset(., "/race/[0-9]{1,}") %>%
        str_replace(., "/race/", "") %>%
        as.numeric(.) %>%
        max(.)

}

# Not run:
# results_list <- lapply(seq(10), cx_results)
# metadata_list <- lapply(seq(10), cx_metadata)
# latest_race_results <- cx_results(latest_race())
