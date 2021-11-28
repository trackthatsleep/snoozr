#' Reads, flattens, and wrangles JSON fitbit sleep data
#'
#' @param path character of absolute file path to data/participant/sleep subdirectory
#'
#' @return df of wrangled sleep data (date-times, minutes asleep, minutes awake, number of times awake, time in bed)
#' @export
#'
#' @examples

importJSON <- function(path){
  #import
  fjson <-jsonlite::fromJSON(path)
  #reduce the nesting within the file by converting to columns, then convert to a dataframe
  fjson <- as.data.frame(jsonlite::flatten(fjson))

  #Checks for classic and stages
  fjsonType <- names(table(fjson$type))

  fjsonTypeChkClass <- stringr::str_detect(fjsonType, "classic")
  fjsonTypeChkClassSum <- sum(fjsonTypeChkClass)

  fjsonTypeChkStages <- stringr::str_detect(fjsonType, "stages")
  fjsonTypeChkStagesSum <- sum(fjsonTypeChkStages)

  #Extracts classic if present
  if(fjsonTypeChkClassSum > 0){
    fjsonClassic <- fjson %>%
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
    fjsonStages <- fjson %>%
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

  return(fjsonFinal)
}
