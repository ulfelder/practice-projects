# functions to load ACLED data by year or country and then to summarize loaded data at country-month level
#
# NOTE: use of the data-loading functions implies acceptance of ACLED's terms of use; see
# https://www.acleddata.com/wp-content/uploads/dlm_uploads/2018/12/TermsofUseAttributionPolicy_4.2019-1.pdf

acled_year <- function(year = substr(Sys.Date(), 1, 4)) {

    require(httr)
    require(jsonlite)

    options(stringsAsFactors = FALSE)
 
    my_query <- sprintf("http://api.acleddata.com/acled/read?terms=accept&limit=0&year=%s", year)

    X <- GET(my_query)

    X2 <- content(X, as = "text")

    X3 <- fromJSON(X2, flatten = TRUE)

    df <- X3[['data']]

    return(df)

}

# load all acled data for a single acled iso numeric code; defaults to Nigeria
acled_country <- function(iso = 566) {

    require(httr)
    require(jsonlite)

    options(stringsAsFactors = FALSE)

    my_query <- sprintf("http://api.acleddata.com/acled/read?terms=accept&limit=0&iso=%s", iso)

    X <- GET(my_query)

    X2 <- content(X, as = "text")

    X3 <- fromJSON(X2, flatten = TRUE)

    df <- X3[['data']]

    return(df)

}

# summarize downloaded ACLED event file in country-month counts; assumed acled_df is in structure
# returned by call to acled_year or acled_country, and uses master grid created below
acled_como <- function(acled_df, windows = TRUE) {

    require(dplyr)
    require(tidyr)
    require(purrr)
    require(stringr)
    require(lubridate)

    options(stringsAsFactors = FALSE)

    # add or tweak some variables to facilitate merging with master grid
    acled_df <- mutate(acled_df, month = month(event_date),
                                 year = as.numeric(year),
                                 iso = as.numeric(iso))
               
    # Counts of events by type
    df_types <- acled_df %>%
        mutate(event_type = make.names(str_trim(tolower(event_type)))) %>% 
        group_by(iso, year, month, event_type) %>% 
        tally() %>% 
        pivot_wider(names_from = event_type, values_from = n)

    # Counts of events by subtype
    df_subtypes <- acled_df %>%
        mutate(sub_event_type = make.names(str_trim(tolower(sub_event_type)))) %>%
        group_by(iso, year, month, sub_event_type) %>% 
        tally() %>% 
        pivot_wider(names_from = sub_event_type, values_from = n)

    # Death counts
    df_deaths_total <- acled_df %>%
        group_by(iso, year, month) %>%
        summarise(deaths_total = sum(as.numeric(fatalities), na.rm = TRUE))

    # Civilian death counts (excluding repression during protest or riots)
    df_deaths_civilian <- acled_df %>%
        group_by(iso, year, month) %>%
        filter(event_type == "Violence against civilians" | (event_type == "Explosions/Remote violence" & grepl("Civilians", actor2))) %>%
        summarise(deaths_civilian = sum(as.numeric(fatalities), na.rm = TRUE))

    # Battle death counts
    df_deaths_battle <- acled_df %>%
        group_by(iso, year, month) %>%
        filter(event_type == "Battles") %>%
        summarise(deaths_battle = sum(as.numeric(fatalities), na.rm=TRUE))

    df_full <- Reduce(function(...) merge(..., all = TRUE),
                      list(como_grid, df_types, df_subtypes, df_deaths_total, df_deaths_civilian, df_deaths_battle))

    df_full <- pivot_longer(df_full, cols = 4:ncol(df_full), names_to = "category", values_to = "n")
    
    df_full$n[is.na(df_full$n)] <- 0

    # lop off excess country-months in special case where user input only covers a single country
    if (length(unique(acled_df$iso)) == 1) {

        df_full <- filter(df_full, iso == unique(acled_df$iso))

    }

    # lop off excess years when user input created by acled_year is limited on that dimension
    df_full <- filter(df_full, year %in% unique(acled_df$year))

    return(df_full)

}


# --- MASTER COUNTRY-MONTH GRID TO FRAME COUNT DATA ---

acled_grid <- function(windows = TRUE) {

  require(dplyr)
  require(tidyr)
  require(purrr)
  require(readxl)
  require(lubridate)

  # load and prep standalone table of start and end dates of coverage by country
  tmp <- paste0(tempfile(), ".xlsx")

  # try to make this robust to Windows vs. iOS issues with user control
  if (windows == TRUE) {

    download.file("https://www.acleddata.com/download/3987/", tmp, mode = "wb")

  } else {

    download.file("https://www.acleddata.com/download/3987/", tmp)

  }

  M <- read_excel(tmp)
  names(M) <- make.names(tolower(colnames(M)))
  unlink(tmp)

  # make a master country-month table into which we can merge tallies with no obs for 0s
  como_grid <- map_dfr(split(M, seq(nrow(M))), function(dfrow) {

    df <- with(dfrow, expand.grid(iso = iso.code,
  	                          year = seq(year(start.date), year(Sys.Date())), 
  	                          month = seq(12)))  # for now, all countries start observation in Jan

    # lop off excess months in current year w/two-week buffer for delayed updating
    df <- filter(df, date(paste(year, month, "01", sep="-")) <= Sys.Date() - 14)
      
    df <- arrange(df, iso, year, month)
  	
    return(df)
    
  })
  
  # assign that object to the global environment so we can use it in acled_como()
  assign("como_grid", como_grid, envir=globalenv())

}

# --- USEFUL NAMED VECTORS ---
# named vector of acled's numeric country codes
# pulled from https://www.acleddata.com/download/3987/ to ensure matching
# named_acled_isos <- unlist(M[,'ISO Code'])
# names(named_acled_isos) <- unlist(M[,'Country']
# named_acled_isos <- isos[order(factor(names(named_acled_isos)))]
named_acled_isos <- c(Afghanistan = 4, Albania = 8, Algeria = 12, Angola = 24, Bahrain = 48, 
    Bangladesh = 50, Belarus = 112, Benin = 204, `Bosnia and Herzegovina` = 70, 
    Botswana = 72, Bulgaria = 100, `Burkina Faso` = 854, Burundi = 108, 
    Cambodia = 116, Cameroon = 120, `Central African Republic (CAR)` = 140, 
    Chad = 148, `Cote d'Ivoire` = 384, Croatia = 191, Cyprus = 196, 
    `Democratic Republic of Congo (DRC)` = 180, Djibouti = 262, Egypt = 818, 
    `Equatorial Guinea` = 226, Eritrea = 232, Eswatini = 748, Ethiopia = 231, 
    Gabon = 266, Gambia = 270, Ghana = 288, Greece = 300, Guinea = 324, 
    `Guinea-Bissau` = 624, India = 356, Indonesia = 360, Iran = 364, 
    Iraq = 368, Israel = 376, Jordan = 400, Kenya = 404, Kosovo = 0, 
    Kuwait = 414, Laos = 418, Lebanon = 422, Lesotho = 426, Liberia = 430, 
    Libya = 434, Madagascar = 450, Malawi = 454, Malaysia = 458, 
    Mali = 466, Mauritania = 478, Moldova = 498, Montenegro = 499, 
    Morocco = 504, Mozambique = 508, Myanmar = 104, Namibia = 516, 
    Nepal = 524, Niger = 562, Nigeria = 566, `North Macedonia` = 807, 
    Oman = 512, Pakistan = 586, Palestine = 275, Philippines = 608, 
    Qatar = 634, `Republic of Congo` = 178, Romania = 642, Russia = 643, 
    Rwanda = 646, `Saudi Arabia` = 682, Senegal = 686, Serbia = 688, 
    `Sierra Leone` = 694, Somalia = 706, `South Africa` = 710, `South Sudan` = 728, 
    `Sri Lanka` = 144, Sudan = 729, Syria = 760, Tanzania = 834, 
    Thailand = 764, Togo = 768, Tunisia = 788, Turkey = 792, Uganda = 800, 
    Ukraine = 804, `United Arab Emirates` = 784, Vietnam = 704, Yemen = 887, 
    Zambia = 894, Zimbabwe = 716)

# named vector of acled indicators
named_acled_indicators <- c(`deaths` = "deaths_total", `deaths in battle` = "deaths_battle",
    `civilian deaths` = "deaths_civilian", battles = "battles", protests = "protests", riots = "riots",
    `violence against civilians` = "violence.against.civilians",
    `explosions/remote violence` = "explosions.remote.violence")
