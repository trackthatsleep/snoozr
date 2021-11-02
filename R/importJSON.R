

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

  #Need different handling based on classic vs. stages
  if(fjson$type=="classic"){
    fjson <- fjson %>%
      #Keep only the needed columns (different names than type == "stages")
      dplyr::select(type, startTime,endTime,minutesAsleep,minutesAwake,
                    levels.summary.awake.count,timeInBed) %>%
      #label the columns exactly as they appear in .csv
      dplyr::rename(StartTime=startTime,EndTime=endTime,MinutesAsleep =minutesAsleep,MinutesAwake=minutesAwake,
                    NumberofAwakenings=levels.summary.awake.count,TimeinBed=timeInBed)

  }else if(fjson$type=="stages"){
    fjson <- fjson %>%
      #Keep only the needed columns (different names than type == "classic")
      dplyr::select(type, startTime,endTime,minutesAsleep,minutesAwake,
                    levels.summary.wake.count,timeInBed) %>%
      #label the columns exactly as they appear in .csv
      dplyr::rename(StartTime=startTime,EndTime=endTime,MinutesAsleep =minutesAsleep,MinutesAwake=minutesAwake,
                    NumberofAwakenings=levels.summary.wake.count,TimeinBed=timeInBed)
  }
  return(fjson)
}
