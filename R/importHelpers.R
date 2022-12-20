#' @name importHelpers
#' @rdname importHelpers
#'
#' @title Helper-functions for noninvariance effect size functions
#'
#' @param fjson.df flattened json file

#' @family helpers
#'
#' @export

#' @rdname importHelpers
importClassic2 <- function(fjson.df){

  fjsonClassic2 <- fjson.df %>%
    dplyr::filter(type == "classic") %>%
    #Keep only the needed columns (different names than type == "stages")
    dplyr::select(type, dateOfSleep, startTime,endTime,minutesAsleep,minutesAwake,
                  levels.summary.awake.count,timeInBed) %>%
    #label the columns exactly as they appear in .csv
    dplyr::rename(StartTime = startTime,
                  EndTime = endTime,
                  MinutesAsleep = minutesAsleep,
                  MinutesAwake = minutesAwake,
                  NumberofAwakenings = levels.summary.awake.count,
                  TimeinBed = timeInBed)

  return(fjsonClassic2)
}

#' @rdname importHelpers
importStages2 <- function(fjson.df){

  fjsonStages2 <- fjson.df %>%
    dplyr::filter(type == "stages") %>%
    #Keep only the needed columns (different names than type == "classic")
    dplyr::select(type, dateOfSleep,startTime,endTime,minutesAsleep,minutesAwake,
                  levels.summary.wake.count,timeInBed) %>%
    #label the columns exactly as they appear in .csv
    dplyr::rename(StartTime = startTime,
                  EndTime = endTime,
                  MinutesAsleep = minutesAsleep,
                  MinutesAwake = minutesAwake,
                  NumberofAwakenings = levels.summary.wake.count,
                  TimeinBed = timeInBed)

  return(fjsonStages2)
}

#' @rdname importHelpers

importClassic21 <- function(fjson.df){

    ####Retaining Level 2 Variables
    #NEW: logId and mainsleep are added
    fjson_L2 <- fjson.df %>%
      dplyr::select(logId,
                    type,
                    dateOfSleep,
                    startTime,
                    endTime,
                    minutesAsleep,
                    minutesAwake,
                    levels.summary.awake.count,
                    timeInBed,
                    mainSleep) %>%
      #label the columns exactly as they appear in .csv
      dplyr::rename(StartTime=startTime,
                    EndTime=endTime,
                    MinutesAsleep = minutesAsleep,
                    MinutesAwake=minutesAwake,
                    NumberofAwakenings=levels.summary.awake.count,
                    TimeinBed=timeInBed,
                    mainsleep=mainSleep)


    ####Retaining Level 1 Variables - NEW

    ##Unnest Level 1 data, select required variables plus logId,
    #all data scraped from source = "data",
    #and mutate sleepWake for classic and stages coding
    fjson_L1 <- tidyr::unnest(fjson.df, levels.data, names_sep=".") %>%
      dplyr::select(logId,
                    levels.data.dateTime,
                    levels.data.level,
                    levels.data.seconds) %>%
      dplyr::rename(logId=logId,
                    dateTime=levels.data.dateTime,
                    level=levels.data.level,
                    seconds=levels.data.seconds) %>%
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
    dplyr::select(logId,
                  type,
                  dateOfSleep,
                  startTime,
                  endTime,
                  minutesAsleep,
                  minutesAwake,
                  levels.summary.wake.count,
                  timeInBed,
                  mainSleep) %>%
    #label the columns exactly as they appear in .csv
    dplyr::rename(StartTime = startTime,
                  EndTime = endTime,
                  MinutesAsleep = minutesAsleep,
                  MinutesAwake = minutesAwake,
                  NumberofAwakenings = levels.summary.wake.count,
                  TimeinBed = timeInBed,
                  mainsleep = mainSleep)


  ####Retaining Level 1 Variables - NEW

  ##There are two different Level 1 lists: levels.data and levels.shortData
  #I unnested them separately and then combined with a "source" variable to differentiate

  ##Unnest Level 1 data, select required variables plus logId
  fjson_L1data <- tidyr::unnest(fjson.df, levels.data, names_sep=".") %>%
    dplyr::select(logId,levels.data.dateTime,levels.data.level,levels.data.seconds) %>%
    dplyr::rename(logId=logId,dateTime=levels.data.dateTime,level=levels.data.level,seconds=levels.data.seconds) %>%
    dplyr::mutate(source="data",
                  sleepWake = dplyr::case_when(level == "asleep" ~ "sleep",
                                               level == "restless" ~ "wake",
                                               level == "awake" ~ "wake",
                                               level == "light" ~ "sleep",
                                               level == "deep" ~ "sleep",
                                               level == "rem" ~ "sleep",
                                               level == "wake" ~ "wake"))

  ##Unnest Level 1 SHORT data, select required variables plus logId
  fjson_L1shortdata <- tidyr::unnest(fjson.df, levels.shortData, names_sep=".") %>%
    dplyr::select(logId,levels.shortData.dateTime,levels.shortData.level,levels.shortData.seconds) %>%
    dplyr::rename(logId=logId,dateTime=levels.shortData.dateTime,level=levels.shortData.level,seconds=levels.shortData.seconds) %>%
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
