#' Converts scrapePerson data w/ raw = TRUE into 30 or 60 epoch data
#'
#' @param data data frame from within scrapePerson with raw = TRUE
#' @param epoch whether 30 or 60 epoch is requested
#'
#' @return data frame converted to 30 or 60 second epoch
#'
expandEpoch <- function(data, epoch){
  ##Add a unique "epoch" ID for each row
  #Note that this is currently just the row number, so it won't be unique across data files

  #data$epochID <- paste0(data$id, "_", seq.int(nrow(data)))
  data$epochID <- seq.int(nrow(data))

  ##Duplicate each row "seconds/30" number of times
  data30sec <- tidyr::uncount(data, .data$seconds/30)


  dfEpoch <- data30sec %>%

    ###Add a datetime_seq variable that starts with the original
    #datetime value and increases in 30 second increments

    #Group by epoch
    dplyr::group_by(.data$epochID) %>%
    #convert dateTime to date-time (from character) %>%
    dplyr::mutate(dateTime = as.POSIXct(.data$dateTime)) %>%
    ##Number of seconds to be added
    dplyr::mutate(seconds_seq = 30*(dplyr::row_number()-1)) %>%
    ##Time when current 30-second increment began
    dplyr::mutate(dateTime_seq = .data$dateTime + .data$seconds_seq)



  if(epoch == 60){
    #####Collapse back down to 60-second cases (each row represents 60 seconds)

    dfEpoch <- dfEpoch %>%

      #When a 30-second "sleep" or "wake" event is isolated/sandwiched, recode
      dplyr::ungroup() %>%
      dplyr::arrange(.data$dateTime_seq) %>%
      dplyr::rename(sleepWake30 = .data$sleepWake) %>%
      dplyr::mutate(sleepWake = dplyr::case_when(dplyr::lag(.data$sleepWake30)=="sleep" & dplyr::lead(.data$sleepWake30)=="sleep" ~ "sleep",
                                                 dplyr::lag(.data$sleepWake30)=="wake" & dplyr::lead(.data$sleepWake30)=="wake" ~ "wake",
                                                 TRUE ~ sleepWake30)) %>%

      #Drop all rows that begin at the 30 second mark (1 minute intervals only)
      dplyr::mutate(seconds_end = lubridate::second(.data$dateTime_seq)) %>%
      dplyr::filter(.data$seconds_end == 0) %>%
      #Drop unneeded column
      dplyr::select(-c(.data$sleepWake30,.data$seconds_end))
  }

  return(dfEpoch)
}
