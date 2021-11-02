
#' Scrapes fitbit JSON sleep data from all participants (subdirectories) in a data directory
#'
#' @param anon whether an anonymized df (and .csv, if export == TRUE) is desired
#' @param export whether to export .csv of file to "processed data" subdirectory
#'
#' @return
#' @export
#'
#' @examples
scrapeSample <- function(anon = FALSE, export = FALSE){
  #Get sub-directories and scrape IDs from folder name
  subdirs <- list.dirs(here::here("data"),
                       recursive = FALSE)

  ids <- list.dirs(here::here("data"),
                  full.names = FALSE,#nix full file-path
                  recursive = FALSE)

  #scrape first id
  dfSample <- scrapePerson(idTarget = ids[[1]])

  #loop through scrapePerson, and append to dfSample
  for(i in 2:length(ids)){
    newdf <- scrapePerson(idTarget = ids[[i]])
    dfSample <- rbind(dfSample, newdf)
  }

  #anonymize w/ cryptographic id if necessary
  if(anon ==TRUE){
    #Create cryptographic hash for anonymized participant ids
    anon_ids <- openssl::sha256(ids)
    #loop through base replacement of ids with anon_ids
    for(i in 1:length(anon_ids)){
      dfSample$id[dfSample$id == ids[[i]]] <- anon_ids[[i]]
    }
    #create id matching file for admin (do not share w externals)
    dir.create("./confidential data")
    id_matchsheet <- as.data.frame(cbind(ids, anon_ids))
    readr::write_csv(id_matchsheet, file = stringr::str_c("./confidential data/", "anon_id_key.csv"))
  }

  #Export to .csv if desired
  if(export == TRUE){
    dir.create("./processed data")
    readr::write_csv(dfSample, file = stringr::str_c("./processed data/", "sample",".csv"))
  }

  return(dfSample)
}