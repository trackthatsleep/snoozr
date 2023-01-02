#' Reads, flattens, and wrangles JSON fitbit sleep data
#'
#' @param path character of absolute file path to data/participant/sleep subdirectory
#' @param level1 logical (default FALSE) of whether to scrape level-1 "30 sec epoch" data (nested within level-2 of day)
#'
#' @return df of wrangled sleep data (date-times, minutes asleep, minutes awake, number of times awake, time in bed)
#' @export
#'

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
      #For type == "classic"
      fjsonClassic <- importClassic2(fjson.df)
    }

    #Extracts stages if present
    if(fjsonTypeChkStagesSum > 0){
      #For type == "stages"
      fjsonStages <- importStages2(fjson.df)
    }

    #If both classic and stages present, merge, otherwise return one or other
    if(fjsonTypeChkClassSum > 0 & fjsonTypeChkStagesSum > 0){
      fjsonFinal <- dplyr::bind_rows(fjsonClassic, fjsonStages)
    }else if(fjsonTypeChkClassSum > 0 & fjsonTypeChkStagesSum == 0){
      fjsonFinal <- fjsonClassic
    }else if(fjsonTypeChkClassSum == 0 & fjsonTypeChkStagesSum > 0){
      fjsonFinal <- fjsonStages
    }
  }else if(isTRUE(level1)){#if level 1 is requested, retrieve and stitch both sources together
    #Extracts classic if present
    if(fjsonTypeChkClassSum > 0){
      #For type == "classic"
      fjsonClassic <- importClassic21(fjson.df)

    }#Extracts stages if present
    if(fjsonTypeChkStagesSum > 0){
      #For type == "stages"
      fjsonStages <- importStages21(fjson.df)
    }
    #If both classic and stages present, merge, otherwise return one or other
    if(fjsonTypeChkClassSum > 0 & fjsonTypeChkStagesSum > 0){
      fjsonFinal <- dplyr::bind_rows(fjsonClassic, fjsonStages)
    }else if(fjsonTypeChkClassSum > 0 & fjsonTypeChkStagesSum == 0){
      fjsonFinal <- fjsonClassic
    }else if(fjsonTypeChkClassSum == 0 & fjsonTypeChkStagesSum > 0){
      fjsonFinal <- fjsonStages
    }
  }


  return(fjsonFinal)
}
