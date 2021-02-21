#'Download a file from a GENESIS database
#'
#'Wrapper for download.file to download any export table  from a GENESIS database.
#'
#'@param db The database to download from. See options below.
#'@param table the code of the table to download
#'@param format the format in which to download the table. Options are "csv", "ffcsv", "xlsx", "xml". Defaults to "ffcsv".
#'@param destfile file in which to store downloaded data. Defaults to table code.
#'@param path where to store downloaded data.
#
#'@details
#'
#'Databases currently available:
#'
#''\itemize{
#'\item www-genesis.destatis.de for results for Germany on NUTS levels 0 and 1
#'\item www.regionalstatistik.de for results for Germany on NUTS levels 1, 2 and 3 and for LAU levels
#'\item www.landesdatenbank.nrw.de for results for North Rhine-Westfalia  on NUTS levels 1, 2 and 3 and for LAU levels
#'}
#'
#'
#'@return a downloaded file with statistical data.
#'
#'@examples
#'#Download population data for North Rhine-Westphalia from landesdatenbank.nrw.de
#'deeplink_download(db = "NRW" , table= "12411-01i")
#'
#'@export

deeplink_download <- function(db = "NRW", table = "", format="ffcsv", destfile = "" , path =""){

  if(db == "NRW"){
    url <- paste0("https://www.landesdatenbank.nrw.de/ldbnrw/online?operation=download&code=",table,"&option=",format)
  }else if(db == "DE"){
    url <- paste0("https://www-genesis.destatis.de/genesis/online?operation=download&code=",table,"&option=",format)
  }else if(db == "REG"){
    url <- paste0("https://www.regionalstatistik.de/genesis/online?operation=download&code=",table,"&option=",format)
  }else if(db == "BIM"){
    url <- paste0("https://www.bildungsmonitoring.de/bildung/online?operation=download&code=",table,"&option=",format)
  }

  #ensure that path has a slash at the end
  path <- ifelse(nchar(path) > 0 & substr(path, nchar(path), nchar(path))=="/",path, paste0(path,"/"))


  if(destfile == "") {

    if(is.element(format, c("ffcsv","csv"))){

      destfile <- paste0(path,table,".csv")

    }else{

      destfile <- paste0(path,table, format)

    }
  }



  download.file(destfile = destfile, url)

}






