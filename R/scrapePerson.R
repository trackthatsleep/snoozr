
#' Scrapes fitbit JSON sleep data from a particular participant in data folder
#'
#' @param idTarget character of participant (subdirectory) name to scrape
#' @param type character input for whether "event"-level (default) or "day"-level data is desired
#' @param sleepdate character input for whether sleep events straddling two dates (i.e., during the night) are coded to have occured based on "start" date (default) or "end" date. df returned will have a different number of rows depending on which option is selected, but sum(nsleep_day) (i.e., total number of events captured) will be consistent
#' @param birthdf optional character file name (w/ extension) of supplemental data file of id and child birth date (babybirthdarte format: mm/dd/yyyy) in "data" subdirectory
#' @param anon whether an anonymized df (and .csv, if export == TRUE) is desired
#' @param export whether to export .csv of file to "processed data" subdirectory
#' @param rawdata whether to scrape raw data (default is false)
#' @param epoch whether to return 30 or 60 second epoch data (requires rawdata = TRUE)
#'
#' @return df of scraped .json sleep data from subdirectory in data matching idTarget
#' @export
#'
#' @examples
#' \dontrun{
#'TL.out.raw <- scrapePerson(idTarget ="TL001",
#'birthdf = "participants.csv",
#'rawdata = TRUE,
#'export = FALSE)
#'}
scrapePerson <- function(idTarget,
                         type = "event",
                         sleepdate = "start",
                         birthdf = NULL,
                         anon = FALSE,
                         export = FALSE,
                         rawdata = FALSE,
                         epoch = NULL){

  #Error if "data" directory not present
  if(dir.exists("data")==FALSE){
    stop("snoozr cannot detect a folder called 'data' in your current working directory")
  }

  #Get sub-directories and scrape IDs from folder name
  subdirs <- list.dirs(here::here("data"),
                       recursive = FALSE)

  subdirMatch <- stringr::str_detect(subdirs, idTarget)#match idTarget to path

  #Error if no subdirectory matches user name
  if(!any(subdirMatch) == TRUE){
    stop(paste("there is no subdirectory in 'data' for user", idTarget))
  }

  subdir <- as.data.frame(cbind(subdirs, subdirMatch))#combine paths and match
  subdir <- subdir %>% #rename to path and only retain
    dplyr::rename(path = subdirs) %>% #matching path
    dplyr::filter(subdirMatch == TRUE)

  #Error if there is no "Sleep" subdirectory
  if(dir.exists(paste("data/", idTarget, "/Sleep", sep = "")) == FALSE){
    stop(paste("there is no 'Sleep' directory for user", idTarget))
  }

  #Get file names from subdir
  files <- as.data.frame(list.files(stringr::str_c("./data/", idTarget, "/sleep/")))
  names(files) <- c("file")#rename horrific vector name

  #Reduce to only those include "sleep" and ".json"
  jsons <- files %>%
    dplyr::filter(stringr::str_detect(file, "json")) %>%
    dplyr::filter(stringr::str_detect(file, "sleep"))

  n_jsons <- nrow(jsons)
  #Warn if there are no .json files
  if(n_jsons < 1){
    warning(paste("there are no .json files for user", idTarget))
  }

  #Import first .json
  path <- stringr::str_c("./data/", idTarget, "/sleep/", jsons$file[[1]])

  if(!isTRUE(rawdata)){
    dfPerson <- importJSON(path, level1 = FALSE)

    #for each json after the 1st...(2nd onward)
    for (i in 2:n_jsons) {

      tryCatch(
        {
          newpath <- stringr::str_c("./data/", idTarget, "/sleep/", jsons$file[[i]])#get the ith path
          newdf <- importJSON(newpath, level1 = FALSE)#import json from ith path
          dfPerson <- dplyr::bind_rows(dfPerson, newdf)
        },
        error = function(e){
          error_message <- paste("There was an error importing file", jsons$file[[i]], "for participant", idTarget, "; it was skipped.")
          message(error_message)
        }
      )
    }
  }else if(isTRUE(rawdata)){
    dfPerson <- importJSON(path, level1 = TRUE)

    #for each json after the 1st...(2nd onward)
    for (i in 2:n_jsons) {
      tryCatch(
        {
          newpath <- stringr::str_c("./data/", idTarget, "/sleep/", jsons$file[[i]])#get the ith path
          newdf <- importJSON(newpath, level1 = TRUE)#import json from ith path
          dfPerson <- dplyr::bind_rows(dfPerson, newdf)
        },
        error = function(e){
          error_message <- paste("There was an error importing file", jsons$file[[i]], "for participant", idTarget, "; it was skipped.")
          message(error_message)
        }
      )
    }
  }


  #Remove duplicate entries
  dfPerson <- dplyr::distinct(dfPerson)

  #Need to split dates and times in Start/EndTime
  StartTime_Split <-  stringr::str_split(dfPerson$StartTime, "T")
  EndTime_Split <-  stringr::str_split(dfPerson$EndTime, "T")

  dfPerson$newStartDate <- NA
  dfPerson$newStartTime <- NA
  dfPerson$newEndDate <- NA
  dfPerson$newEndTime <- NA

  for(i in 1:nrow(dfPerson)){
    dfPerson$newStartDate[[i]] <- StartTime_Split[[i]][1]
    dfPerson$newStartTime[[i]] <- StartTime_Split[[i]][2]
    dfPerson$newEndDate[[i]] <- EndTime_Split[[i]][1]
    dfPerson$newEndTime[[i]] <- EndTime_Split[[i]][2]
  }

  #' @importFrom rlang .data
  #Rearrange and recreate dateOfSleep based on sleepdate
  if(sleepdate == "start"){
    dfPerson <- dfPerson %>%
      dplyr::mutate(newStartDate= lubridate::as_date(.data$newStartDate),
                    newEndDate= lubridate::as_date(.data$newEndDate),
                    dateOfSleep = .data$newStartDate) %>%
      dplyr::rename(startDate = .data$newStartDate,
             startTime = .data$newStartTime,
             endDate = .data$newEndDate,
             endTime = .data$newEndTime) %>%
      dplyr::select(-.data$StartTime, -.data$EndTime) %>%
      dplyr::relocate(type, .data$dateOfSleep, .data$startDate, .data$startTime,
                      .data$endDate, .data$endTime, .data$MinutesAsleep, .data$MinutesAwake,
                      .data$NumberofAwakenings, .data$TimeinBed)
  }else if(sleepdate == "end"){
    dfPerson <- dfPerson %>%
      dplyr::mutate(newStartDate= lubridate::as_date(.data$newStartDate),
                    newEndDate= lubridate::as_date(.data$newEndDate),
                    dateOfSleep = .data$newEndDate) %>%
      dplyr::rename(startDate = .data$newStartDate,
                    startTime = .data$newStartTime,
                    endDate = .data$newEndDate,
                    endTime = .data$newEndTime) %>%
      dplyr::select(-.data$StartTime, -.data$EndTime) %>%
      dplyr::relocate(type, .data$dateOfSleep, .data$startDate, .data$startTime,
                      .data$endDate, .data$endTime, .data$MinutesAsleep, .data$MinutesAwake,
                      .data$NumberofAwakenings, .data$TimeinBed)
  }

  #Provide day-level output instead if requests
  if(type == "day"){
    dfPerson <- dfPerson %>%
      dplyr::group_by(.data$dateOfSleep) %>%
      dplyr::summarise(nsleep_day = dplyr::n(),
                MinutesAsleep = sum(.data$MinutesAsleep),
                MinutesAwake = sum(.data$MinutesAwake),
                NumberofAwakenings = sum(.data$NumberofAwakenings),
                TimeinBed = sum(.data$TimeinBed))
  }

  #Add id (cryptographic if anon == TRUE)
  if(anon == FALSE){
    dfPerson <- dfPerson %>%
      dplyr::mutate(id = idTarget) %>%
      dplyr::relocate(.data$id)
  }else if(anon == TRUE){
    #Create cryptographic hash for anonymized participant ids
    anon_id <- openssl::sha256(idTarget)
    dfPerson <- dfPerson %>%
      dplyr::mutate(id = anon_id) %>%
      dplyr::relocate(.data$id)#TODO: add exported pair of de-anon id?
  }

  #Append phases if birthdf file is supplied
  if(!is.null(birthdf)){
    #import birthdf
    birthdates <- readr::read_csv(here::here("data",birthdf), show_col_types = FALSE)

    #bring over birthday info from birthdates
    dfPerson <- dplyr::left_join(dfPerson, birthdates, by = "id")

    #Convert dates to dates (orig. as character())
    dfPerson <- dfPerson %>%
      dplyr::mutate(baby_date = as.Date(.data$babybirthdate, "%m/%d/%Y")) %>%
      dplyr::select(-.data$babybirthdate)

    #Calculate the difference between birth date and survey start date in days
    dfPerson <- dfPerson %>%
      dplyr::mutate(days_dif = as.numeric(difftime(.data$dateOfSleep,.data$baby_date,units=c("days"))))

    ##Using the difference between birth date and start date, create new column called phase
    #according to Teresa's specified thresholds:
    dfPerson <- dfPerson %>%
      dplyr::mutate(phase= as.factor(dplyr::case_when(dplyr::between(days_dif,-730,-365) ~ "seasonal control",
                                                      dplyr::between(days_dif,-364,-274) ~ "pre-conception",
                                                      dplyr::between(days_dif,-273,-1) ~ "pregnancy",
                                                      days_dif==0 ~ "birth",
                                                      dplyr::between(days_dif,1,364) ~ "postpartum")))
  }

  if(isTRUE(rawdata)){
    dfPerson <- dfPerson %>%
      dplyr::filter(!is.na(.data$NumberofAwakenings)) %>%
      dplyr::arrange(dplyr::desc(.data$dateTime))
  }

  if(!is.null(epoch)){
    if(isTRUE(rawdata) & epoch == 30){
      dfPerson<- expandEpoch(dfPerson, epoch = 30)
    }else if (isTRUE(rawdata) & epoch == 60){
      dfPerson<- expandEpoch(dfPerson, epoch = 60)
    }else if(!isTRUE(rawdata) & epoch == 30){
      stop("epoch = 30 requires rawdata = TRUE")
    }else if(!isTRUE(rawdata) & epoch == 60){
      stop("epoch = 60 requires rawdata = TRUE")
    }
  }

  #Export to .csv if desired
  if(export == TRUE){
    dir.create("./processed data")
    dir.create("./processed data/individual files")
    readr::write_csv(dfPerson, file = stringr::str_c("./processed data/individual files/", idTarget,".csv"))
  }


  return(dfPerson)
}
