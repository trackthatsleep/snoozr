#' Reads, flattens, and wrangles JSON fitbit sleep data
#'
#' @param path character of absolute file path to data/participant/sleep subdirectory
#' @param level1 logical (default FALSE) of whether to scrape level-1 "30 sec epoch" data (nested within level-2 of day)
#'
#' @return df of wrangled sleep data (date-times, minutes asleep, minutes awake, number of times awake, time in bed)
#' @export
#'
#' @examples

importJSON <- function(path, level1 = FALSE){
  #import for level2 (and maybe level1)
  fjson <-jsonlite::fromJSON(path)

  #reduce the nesting for level2 within the file by converting to columns, then convert to a dataframe
  fjson.df <- as.data.frame(jsonlite::flatten(fjson))

  #Checks for classic and stages
  fjsonType <- names(table(fjson$type))

  fjsonTypeChkClass <- stringr::str_detect(fjsonType, "classic")
  fjsonTypeChkClassSum <- sum(fjsonTypeChkClass)

  fjsonTypeChkStages <- stringr::str_detect(fjsonType, "stages")
  fjsonTypeChkStagesSum <- sum(fjsonTypeChkStages)

  #If level 1 data is not requested, .json can be flatened/level 2 extracted out
  if(!isTRUE(level1)){

    #Extracts classic if present
    if(fjsonTypeChkClassSum > 0){
      fjsonClassic <- fjson.df %>%
        dplyr::filter(type == "classic") %>%
        #Keep only the needed columns (different names than type == "stages")
        dplyr::select(type, dateOfSleep, startTime,endTime,minutesAsleep,minutesAwake,
                      levels.summary.awake.count,timeInBed) %>%
        #label the columns exactly as they appear in .csv
        dplyr::rename(StartTime=startTime,EndTime=endTime,MinutesAsleep =minutesAsleep,MinutesAwake=minutesAwake,
                      NumberofAwakenings=levels.summary.awake.count,TimeinBed=timeInBed)
    }

    #Extracts stages if present
    if(fjsonTypeChkStagesSum > 0){
      #For type == "stages"
      fjsonStages <- fjson.df %>%
        dplyr::filter(type == "stages") %>%
        #Keep only the needed columns (different names than type == "classic")
        dplyr::select(type, dateOfSleep,startTime,endTime,minutesAsleep,minutesAwake,
                      levels.summary.wake.count,timeInBed) %>%
        #label the columns exactly as they appear in .csv
        dplyr::rename(StartTime=startTime,EndTime=endTime,MinutesAsleep =minutesAsleep,MinutesAwake=minutesAwake,
                      NumberofAwakenings=levels.summary.wake.count,TimeinBed=timeInBed)
    }

    #If both classic and stages present, merge, otherwise return one or other
    if(fjsonTypeChkClassSum > 0 & fjsonTypeChkStagesSum > 0){
      fjsonFinal <- rbind(fjsonClassic, fjsonStages)
    }else if(fjsonTypeChkClassSum > 0 & fjsonTypeChkStagesSum == 0){
      fjsonFinal <- fjsonClassic
    }else if(fjsonTypeChkClassSum == 0 & fjsonTypeChkStagesSum > 0){
      fjsonFinal <- fjsonStages
    }
  }else if(isTRUE(level1)){#if level 1 is requested, retrieve and stitch both sources together
    #Extracts classic if present
    if(fjsonTypeChkClassSum > 0){
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

      fjsonClassic <- dplyr::full_join(fjson_L2,fjson_L1, by="logId")
    }#Extracts stages if present
    if(fjsonTypeChkStagesSum > 0){
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

      fjson_L1 <- rbind(fjson_L1data,fjson_L1shortdata)


      ###Combine Level 1 and Level 2 data###

      fjsonStages <- dplyr::full_join(fjson_L2,fjson_L1, by="logId")
    }
    #If both classic and stages present, merge, otherwise return one or other
    if(fjsonTypeChkClassSum > 0 & fjsonTypeChkStagesSum > 0){
      fjsonFinal <- rbind(fjsonClassic, fjsonStages)
    }else if(fjsonTypeChkClassSum > 0 & fjsonTypeChkStagesSum == 0){
      fjsonFinal <- fjsonClassic
    }else if(fjsonTypeChkClassSum == 0 & fjsonTypeChkStagesSum > 0){
      fjsonFinal <- fjsonStages
    }
  }


  return(fjsonFinal)
}
