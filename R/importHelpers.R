#' @name importHelpers
#' @rdname importHelpers
#'
#' @title Helper-functions for noninvariance effect size functions
#'
#' @param fjson.df flattened json file

#' @family helpers
#'
#' @export

importClassic2 <- function(fjson.df){

  fjsonClassic2 <- fjson.df %>%
    dplyr::filter(.data$type == "classic") %>%
    #Keep only the needed columns (different names than type == "stages")
    dplyr::select(.data$type,
                  .data$dateOfSleep,
                  .data$startTime,
                  .data$endTime,
                  .data$minutesAsleep,
                  .data$minutesAwake,
                  .data$levels.summary.awake.count,
                  .data$timeInBed) %>%
    #label the columns exactly as they appear in .csv
    dplyr::rename(StartTime = .data$startTime,
                  EndTime = .data$endTime,
                  MinutesAsleep = .data$minutesAsleep,
                  MinutesAwake = .data$minutesAwake,
                  NumberofAwakenings = .data$levels.summary.awake.count,
                  TimeinBed = .data$timeInBed)

  return(fjsonClassic2)
}

#' @rdname importHelpers
importStages2 <- function(fjson.df){

  fjsonStages2 <- fjson.df %>%
    dplyr::filter(.data$type == "stages") %>%
    #Keep only the needed columns (different names than type == "classic")
    dplyr::select(.data$type,
                  .data$dateOfSleep,
                  .data$startTime,
                  .data$endTime,
                  .data$minutesAsleep,
                  .data$minutesAwake,
                  .data$levels.summary.wake.count,
                  .data$timeInBed) %>%
    #label the columns exactly as they appear in .csv
    dplyr::rename(StartTime = .data$startTime,
                  EndTime = .data$endTime,
                  MinutesAsleep = .data$minutesAsleep,
                  MinutesAwake = .data$minutesAwake,
                  NumberofAwakenings = .data$levels.summary.wake.count,
                  TimeinBed = .data$timeInBed)

  return(fjsonStages2)
}

#' @rdname importHelpers

importClassic21 <- function(fjson.df){

    ####Retaining Level 2 Variables
    #NEW: logId and mainsleep are added
    fjson_L2 <- fjson.df %>%
      dplyr::select(.data$logId,
                    .data$type,
                    .data$dateOfSleep,
                    .data$startTime,
                    .data$endTime,
                    .data$minutesAsleep,
                    .data$minutesAwake,
                    .data$levels.summary.awake.count,
                    .data$timeInBed,
                    .data$mainSleep) %>%
      #label the columns exactly as they appear in .csv
      dplyr::rename(StartTime =.data$startTime,
                    EndTime = .data$endTime,
                    MinutesAsleep = .data$minutesAsleep,
                    MinutesAwake = .data$minutesAwake,
                    NumberofAwakenings = .data$levels.summary.awake.count,
                    TimeinBed = .data$timeInBed,
                    mainsleep = .data$mainSleep)


    ####Retaining Level 1 Variables - NEW

    ##Unnest Level 1 data, select required variables plus logId,
    #all data scraped from source = "data",
    #and mutate sleepWake for classic and stages coding
    fjson_L1 <- tidyr::unnest(fjson.df, .data$levels.data, names_sep=".") %>%
      dplyr::select(.data$logId,
                    .data$levels.data.dateTime,
                    .data$levels.data.level,
                    .data$levels.data.seconds) %>%
      dplyr::rename(logId = .data$logId,
                    dateTime = .data$levels.data.dateTime,
                    level = .data$levels.data.level,
                    seconds = .data$levels.data.seconds) %>%
      dplyr::mutate(source="data",
                    sleepWake = dplyr::case_when(level == "asleep" ~ "sleep",
                                                 level == "restless" ~ "wake",
                                                 level == "awake" ~ "wake",
                                                 level == "light" ~ "sleep",
                                                 level == "deep" ~ "sleep",
                                                 level == "rem" ~ "sleep",
                                                 level == "wake" ~ "wake"))

    ###Combine Level 1 and Level 2 data###

    fjsonClassic21 <- dplyr::full_join(fjson_L2,fjson_L1, by="logId")

    return(fjsonClassic21)
}

#' @rdname importHelpers

importStages21 <- function(fjson.df){

  #####Retaining Level 2 Variables

  #NEW: logId and mainsleep are added
  fjson_L2 <- fjson.df %>%
    dplyr::select(.data$logId,
                  .data$type,
                  .data$dateOfSleep,
                  .data$startTime,
                  .data$endTime,
                  .data$minutesAsleep,
                  .data$minutesAwake,
                  .data$levels.summary.wake.count,
                  .data$timeInBed,
                  .data$mainSleep) %>%
    #label the columns exactly as they appear in .csv
    dplyr::rename(StartTime = .data$startTime,
                  EndTime = .data$endTime,
                  MinutesAsleep = .data$minutesAsleep,
                  MinutesAwake = .data$minutesAwake,
                  NumberofAwakenings = .data$levels.summary.wake.count,
                  TimeinBed = .data$timeInBed,
                  mainsleep = .data$mainSleep)


  ####Retaining Level 1 Variables - NEW

  ##There are two different Level 1 lists: levels.data and levels.shortData
  #I unnested them separately and then combined with a "source" variable to differentiate

  ##Unnest Level 1 data, select required variables plus logId
  fjson_L1data <- tidyr::unnest(fjson.df, .data$levels.data, names_sep=".") %>%
    dplyr::select(.data$logId,
                  .data$levels.data.dateTime,
                  .data$levels.data.level,
                  .data$levels.data.seconds) %>%
    dplyr::rename(logId = .data$logId,
                  dateTime = .data$levels.data.dateTime,
                  level = .data$levels.data.level,
                  seconds = .data$levels.data.seconds) %>%
    dplyr::mutate(source="data",
                  sleepWake = dplyr::case_when(level == "asleep" ~ "sleep",
                                               level == "restless" ~ "wake",
                                               level == "awake" ~ "wake",
                                               level == "light" ~ "sleep",
                                               level == "deep" ~ "sleep",
                                               level == "rem" ~ "sleep",
                                               level == "wake" ~ "wake"))

  ##Unnest Level 1 SHORT data, select required variables plus logId
  fjson_L1shortdata <- tidyr::unnest(fjson.df, .data$levels.shortData, names_sep=".") %>%
    dplyr::select(.data$logId,
                  .data$levels.shortData.dateTime,
                  .data$levels.shortData.level,
                  .data$levels.shortData.seconds) %>%
    dplyr::rename(logId = .data$logId,
                  dateTime = .data$levels.shortData.dateTime,
                  level = .data$levels.shortData.level,
                  seconds = .data$levels.shortData.seconds) %>%
    dplyr::mutate(source="shortdata",
                  sleepWake = dplyr::case_when(level == "asleep" ~ "sleep",
                                               level == "restless" ~ "wake",
                                               level == "awake" ~ "wake",
                                               level == "light" ~ "sleep",
                                               level == "deep" ~ "sleep",
                                               level == "rem" ~ "sleep",
                                               level == "wake" ~ "wake"))

  fjson_L1 <- dplyr::bind_rows(fjson_L1data,fjson_L1shortdata)


  ###Combine Level 1 and Level 2 data###

  fjsonStages21 <- dplyr::full_join(fjson_L2,fjson_L1, by="logId")

  return(fjsonStages21)

}
