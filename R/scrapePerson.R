
#' Scrapes fitbit JSON sleep data from a particular participant in data folder
#'
#' @param idTarget character of participant (subdirectory) name to scrape
#' @param anon whether an anonymized df (and .csv, if export == TRUE) is desired
#' @param export whether to export .csv of file to "processed data" subdirectory
#'
#' @return
#' @export
#'
#' @examples
scrapePerson <- function(idTarget, anon = FALSE, export = FALSE){

  #Get sub-directories and scrape IDs from folder name
  subdirs <- list.dirs(here::here("data"),
                       recursive = FALSE)

  subdirMatch <- stringr::str_detect(subdirs, idTarget)#match idTarget to path
  subdir <- as.data.frame(cbind(subdirs, subdirMatch))#combine paths and match
  subdir <- subdir %>% #rename to path and only retain
    rename(path = subdirs) %>% #matching path
    filter(subdirMatch == TRUE)

  #Get file names from subdir
  files <- as.data.frame(list.files(stringr::str_c("./data/", idTarget, "/sleep/")))
  names(files) <- c("file")#rename horrific vector name

  #Reduce to only those include "sleep" and ".json"
  jsons <- files %>%
    dplyr::filter(stringr::str_detect(file, "json")) %>%
    dplyr::filter(stringr::str_detect(file, "sleep"))

  #Import first .json
  path <- stringr::str_c("./data/", idTarget, "/sleep/", jsons$file[[1]])
  dfPerson <- importJSON(path)

  #for each json after the 1st...(2nd onward)
  for (i in 2:nrow(jsons)) {
    newpath <- stringr::str_c("./data/", idTarget, "/sleep/", jsons$file[[i]])#get the ith path
    newdf <- importJSON(newpath)#import json from ith path
    dfPerson <- rbind(dfPerson, newdf)#new rows from ith json are appended
  }

  #Add id (cryptographic if anon == TRUE)
  if(anon == FALSE){
    dfPerson <- dfPerson %>%
      dplyr::mutate(id = idTarget) %>%
      dplyr::relocate(id)
  }else if(anon == TRUE){
    #Create cryptographic hash for anonymized participant ids
    anon_id <- openssl::sha256(idTarget)
    dfPerson <- dfPerson %>%
      dplyr::mutate(id = anon_id) %>%
      dplyr::relocate(id)#TODO: add exported pair of de-anon id?
  }

  #Export to .csv if desired
  if(export == TRUE){
    dir.create("./processed data")
    readr::write_csv(dfPerson, file = stringr::str_c("./processed data/", idTarget,".csv"))
  }

  return(dfPerson)
}
