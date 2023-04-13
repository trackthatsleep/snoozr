#' Converts scrapePerson data w/ raw = TRUE into 30 or 60 epoch data
#'
#' @param data data frame from within scrapePerson with raw = TRUE
#' @param epoch currently only 30 is supported
#'
#' @return data frame converted to 30 or 60 second epoch
#'
expandEpoch <- function(data, epoch){

  if(epoch == 30){
    ##Add a unique "epoch" ID for each row
    data$eventID <- seq.int(nrow(data))
    eventNum <-  length(unique(data$eventID))

    ##Duplicate each row "seconds/30" number of times
    data30sec <- tidyr::uncount(data, .data$seconds/30)

    #create dfEpoch
    dfEpoch <- data30sec

    # Sequentially count observations within each level of clustering variable
    dfEpoch$eventObsID <- stats::ave(dfEpoch$days_dif, dfEpoch$eventID, FUN = seq_along)
    dfEpoch$obsID <- as.numeric(rownames(dfEpoch))

    dfEpoch <- dfEpoch %>%
      #Group by eventID
      dplyr::group_by(.data$eventID) %>%
      ##create eventObsEndDateTime
      dplyr::mutate(eventObsEndDateTime = (lubridate::seconds(30)*(dplyr::row_number()-1))+.data$eventDateTime)

    #retain cases of shortdata over data when at same eventObsEndDateTime
    dfEpoch <- dfEpoch %>%
      dplyr::group_by(.data$eventObsEndDateTime) %>%
      dplyr::slice(which.max(source == "shortdata"))
  }
else{
    stop("epoch must be 30 (for now)")
  }

  return(dfEpoch)
}
