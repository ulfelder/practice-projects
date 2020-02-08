library(tidyverse)
library(lubridate)
library(rvest)
library(forecast)

options(stringsAsFactors = FALSE)

# HELPER FUNCTIONS ----

# helper function to scrape a single text report and extract the desired table in long format
corn_scraper <- function(url) {

    require(dplyr)
    require(stringr)

    raw_text <- data.frame(text = readLines(url))

    # mark the line where the table we want starts
    refline <- which(grepl("corn planted - selected states", raw_text$text, ignore.case = TRUE))

    dat <- raw_text %>%
        # next two lines get rid of filler, but they assume format is consistent across reports; will need to check
        slice((refline + 11):(refline + 30)) %>% 
        slice(-18, -20) %>%
        separate(text, c("state", "values"), sep = ":") %>%
        mutate(values = str_squish(values)) %>%
        separate(values, c("1_year_ago", "1_week_ago", "this_week", "5_yr_avg"), sep = " ") %>%
        mutate_at(vars(contains("_")), function(x) gsub("-", "0", x)) %>%
        mutate_at(vars(contains("_")), as.numeric) %>%
        mutate(state = str_trim(gsub("\\.", "", state))) %>%
        pivot_longer(contains("_"), names_to = "time_frame", values_to = "value")

    return(dat)

}

# helper function to download and clean corn progress data using .zip download url
corn_reaper <- function(x) {

    require(dplyr)
    require(stringr)
    require(rvest)

    tmp <- tempfile()
    download.file(x, tmp)

    # in may 2010, the name of the index file changes, and it's not always lowercase, so we need to do this to cover that
    filelist <- unzip(tmp, list = TRUE)
    myfile <- grep("prog_index|indxprog", filelist$Name, value = TRUE, ignore.case = TRUE)

    # now get the name of the csv we want by scraping it from the index file in the zip archive
    index <- read_html(unz(tmp, myfile))
    table <- index %>%
        html_nodes("td") %>%
        html_text()
    filename <- table[which(grepl("Corn Planted", table)) - 1]

    # some reports don't have a table on corn planted, so filename comes back as an empty vector.
    # in these cases, let's get a NULL in our list instead of an error message.
    if (length(filename) == 0) {

        dat <- NULL

        unlink(tmp)

    } else {

        dat <- read.csv(unz(tmp, filename), skip = 10, header = FALSE, stringsAsFactors = FALSE)
        
        unlink(tmp)
 
        dat <- select(dat, 3:7)  # drop leading cols with no info
    
        colnames(dat) <- c("state", "1_year_ago", "1_week_ago", "this_week", "5_yr_avg")

        dat <- filter(dat, state != "", !grepl("Not available", state))  # drop junk rows

        dat <- mutate_at(dat, vars(contains("_")), as.numeric)  # convert data from string to numeric

        dat <- pivot_longer(dat, contains("_"), names_to = "time_frame", values_to = "value")  # shift to tidy long format

    }

    return(dat)

}

# LINK SCRAPING ----

main_url <- "https://usda.library.cornell.edu/concern/publications/8336h188j?locale=en#release-items"

current_urls <- read_html(main_url) %>%
    html_nodes(".btn") %>%
    html_attr("href") %>%
    .[grepl("CropProg", .) | grepl("prog", .)] %>%
    .[grepl("\\.zip", .)]

# the 89 is from eyeballing, and it will break as more reports come out, so in future, need to find
# a way to scrape the max page number from the home page and do that first
archive_urls <- map(seq(89), function(i) {

    url <- sprintf("https://usda.library.cornell.edu/concern/publications/8336h188j?locale=en&page=%s#release-items", i)

    base_page <- read_html(url)

    link_vector <- base_page %>%
        html_nodes(".btn") %>%
        html_attr("href") %>%
        .[grepl("CropProg", .) | grepl("prog", .)] %>%
        .[grepl("\\.zip", .)]

    Sys.sleep(10)

    return(link_vector)

})

archive_urls <- unlist(archive_urls)

# zips only available back to 2001-05-14 (page 67 right now), but process is robust to this
zip_urls <- unique(c(current_urls, archive_urls))

# DATA LOADING ----

corn_list <- lapply(zip_urls, corn_reaper)

# check which ones got tables: which(unlist(lapply(corn_list, function(i) !is.null(i))))

# now need to figure out how to pull dates associated with those reports to label the list/them
# after labeling, filter for not null, then concatenate
# then consider modeling/ML approaches for panel data that can handle bounded scale of dv
# maybe survival model? but sample is so small, just 18 years...
