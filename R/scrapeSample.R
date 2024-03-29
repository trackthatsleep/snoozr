
#' Scrapes fitbit JSON sleep data from all participants (subdirectories) in a data directory
#' @param sampbirthdf optional character file name (w/ extension) of supplemental data file of id and child birth date (babybirthdarte format: mm/dd/yyyy) in "data" subdirectory
#' @param anon whether an anonymized df (and .csv, if export == TRUE) is desired
#' @param export whether to export .csv of file to "processed data" subdirectory
#' @param ... Additional arguments passed to each loop of scrapePerson (e.g., type, sleepdate)
#'
#' @return df of scraped .json sleep data from all subdirectories in data
#' @export
#'
#' @examples
#' \dontrun{
#'samp.out <- scrapeSample(rawdata = TRUE)
#'}

scrapeSample <- function(sampbirthdf = NULL, anon = FALSE,
                         export = FALSE, ...){
  #Get sub-directories and scrape IDs from folder name
  subdirs <- list.dirs(here::here("data"),
                       recursive = FALSE)

  ids <- list.dirs(here::here("data"),
                  full.names = FALSE,#nix full file-path
                  recursive = FALSE)

  n_ids <- length(ids)
  #scrape (and append birth info if sampbirthdf not NULL)
  if(is.null(sampbirthdf)){
    #scrape first id
    dfSample <- scrapePerson(idTarget = ids[[1]], ...)
    #loop through scrapePerson, and append to dfSample
    for(i in 2:n_ids){

      skip_to_next <- FALSE

      tryCatch({
        newdf <- scrapePerson(idTarget = ids[[i]], ...)
        dfSample <- dplyr::bind_rows(dfSample, newdf)
      },
      error = function(e) { skip_to_next <<- TRUE
      }
      )

      if(skip_to_next) { next }

    }
  }else if(!is.null(sampbirthdf)){
    #scrape first id
    dfSample <- scrapePerson(idTarget = ids[[1]], birthdf = sampbirthdf, ...)

    #loop through scrapePerson, and append to dfSample
    for(i in 2:n_ids){
      skip_to_next <- FALSE

      tryCatch({
        newdf <- scrapePerson(idTarget = ids[[i]], ...)
        dfSample <- dplyr::bind_rows(dfSample, newdf)
      },
      error = function(e) {
        skip_to_next <<- TRUE
      }
      )

      if(skip_to_next) { next }

    }
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

    #solution adapted from https://www.statology.org/r-split-data-frame/
    #define number of data frames to split into
    n <- round((nrow(dfSample)/500000)+1)

    #split data frame into n equal-sized data frames
    dfSampleSplit <- split(dfSample, factor(sort(rank(row.names(dfSample))%%n)))

    dir.create("./processed data")

    for(i in 0:(n-1)){
      filenum <- as.character(i)
      readr::write_csv(dfSampleSplit[[filenum]], file = stringr::str_c("./processed data/", "sample","_", i, ".csv"))

    }

  }

  return(dfSample)
}
