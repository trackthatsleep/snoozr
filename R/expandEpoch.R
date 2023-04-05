#' Converts scrapePerson data w/ raw = TRUE into 30 or 60 epoch data
#'
#' @param data data frame from within scrapePerson with raw = TRUE
#' @param epoch whether 30 or 60 epoch is requested
#'
#' @return data frame converted to 30 or 60 second epoch
#'
expandEpoch <- function(data, epoch){

  if(epoch == 30){
    ##Add a unique "epoch" ID for each row
    #Note that this is currently just the row number, so it won't be unique across data files

    #data$epochID <- paste0(data$id, "_", seq.int(nrow(data)))
    data$eventID <- seq.int(nrow(data))
    eventNum <-  length(unique(data$eventID))

    ##Duplicate each row "seconds/30" number of times
    data30sec <- tidyr::uncount(data, .data$seconds/30)

    dfEpoch <- data30sec

    # Sequentially count observations within each level of clustering variable
    dfEpoch$eventObsID <- ave(dfEpoch$days_dif, dfEpoch$eventID, FUN = seq_along)
    dfEpoch$obsID <- as.numeric(rownames(dfEpoch))

    dfEpoch <- dfEpoch %>%
      ###Add a datetime_seq variable that starts with the original
      #datetime value and increases in 30 second increments

      #Group by epoch
      dplyr::group_by(.data$eventID) %>%
      #convert dateTime to date-time (from character) %>% (no longer necessary)
      #dplyr::mutate(dateTime = as.POSIXct(.data$dateTime)) %>%
      ##Number of seconds to be added
      dplyr::mutate(eventObsEndDateTime = (lubridate::seconds(30)*dplyr::row_number())+.data$eventDateTime)
      ##Time when current 30-second increment began
      #dplyr::mutate(eventTime_seq = .data$eventTime + .data$seconds_seq)
  } else if(epoch == 60){
    #data$epochID <- paste0(data$id, "_", seq.int(nrow(data)))
    data$epochID <- seq.int(nrow(data))

    ##Duplicate each row "seconds/60" number of times
    data60sec <- tidyr::uncount(data, round(.data$seconds/60))


    dfEpoch <- data60sec %>%

      ###Add a datetime_seq variable that starts with the original
      #datetime value and increases in 30 second increments

      #Group by epoch
      dplyr::group_by(.data$epochID) %>%
      #convert dateTime to date-time (from character) %>% (no longer necessary)
      #dplyr::mutate(dateTime = as.POSIXct(.data$dateTime)) %>%
      ##Number of seconds to be added
      dplyr::mutate(seconds_seq = 60*(dplyr::row_number()-1)) %>%
      ##Time when current 60-second increment began
      dplyr::mutate(eventTime_seq = .data$eventTime + .data$seconds_seq)

    #####Collapse back down to 60-second cases (each row represents 60 seconds)

    #dfEpoch <- dfEpoch %>%

      #When a 30-second "sleep" or "wake" event is isolated/sandwiched, recode
      #dplyr::ungroup() %>%
      #dplyr::arrange(.data$eventTime_seq) %>% toggle for test
      #dplyr::rename(sleepWake30 = .data$sleepWake) %>%
      #dplyr::mutate(sleepWake = dplyr::case_when(dplyr::lag(.data$sleepWake30)=="sleep" & dplyr::lead(.data$sleepWake30)=="sleep" ~ "sleep",
      #                                           dplyr::lag(.data$sleepWake30)=="wake" & dplyr::lead(.data$sleepWake30)=="wake" ~ "wake",
      #                                           TRUE ~ sleepWake30)) %>%

      #Drop all rows that begin at the 30 second mark (1 minute intervals only)
      #dplyr::mutate(seconds_end = lubridate::second(.data$eventTime_seq)) %>%
      #dplyr::filter(.data$seconds_end == 0) %>%
      #Drop unneeded column
      #dplyr::select(-c(.data$sleepWake30,.data$seconds_end))
  }else{
    stop("epoch must be either 30 or 60")
  }

  return(dfEpoch)
}
