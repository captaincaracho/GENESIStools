#'Clean ffcsv for further use
#'
#'Reduce downloaded flatfile csv (ffcsv) to clean dataset on the most granular level possible.
#'
#'@param import a flatfile csv downloaded from a GENESIS system
#'@param removetotal logical value whether to include total columns. Defaults to FALSE.
#'@param removestat logical value whether to include info on the statistic itself. Defaults to FALSE.
#'@param ags a character string to indicate what to do with the regional code. Options are 'remove','keep', 'combine', 'replace'.
#
#'@details Takes exported flatfile csv (ffcsv) from GENESIS systems and cleans them for further use. This involves:
#'
#'\itemize{
#'\item Removal of info related to the source statistic (optional),
#'\item Removal of aggregated "Insgesamt" values (optional),
#'\item Extraction of time variable and conversion into date,
#'\item Extraction of spatial variable with different conversion options (see below)
#'\item Formatting of variables by removing placeholder values.
#'}
#'
#'Dealing with the AGS: The "Amtlicher Gemeindeschlüssel" (AGS) is a unique key identifier for German local authorities. As a hierarchical classification system
#'the number of digits represents the administration level.
#'
#'\itemize{
#'\item 2 digits: federal state (NUTS-1),
#'\item 3 digits: government district or statistical region (NUTS-2),
#'\item 5 digits: district (NUTS-3)
#'\item 8 digits: municipality (LAU)
#'}
#'
#'The different options to process the AGS are:
#'
#'\itemize{
#'\item remove - only the name of the local authority is kept in a variable with the name representing the administration level,
#'\item replace - removes the name of the local authority and adds a variable "AGS" containing a key identifier,
#'\item keep - keeps the name of the local authority and adds an "AGS" variable containing a key identifier,
#'\item combine - patches together a string with the key identifier before the name of the local authority in one variable.
#'
#'@return a data.frame.
#'
#'@examples
#'#A table from Regionalstatistik.de
#'
#'GENESIStools::deeplink_download(db = "NRW" , table= "12411-01i")
#'
#'input   <- read.csv2("12411-01i.csv", colClasses = "character",encoding = "Latin-1")
#'
#'output  <- ffcsv_clean(input, ags ="keep")
#'
#'@export


norm <- read.csv2("a.csv", colClasses = "character",encoding = "Latin-1")

#To do
#check charset
#check different db: rdb, bdb, destatis, zensus
#choose regional level
#Zusammengesetzte Tabellen! 62321-01i
#Doppelte Spalten



ffcsv_clean <- function(import,
                     removetotal = TRUE,
                     removestat  = TRUE,
                     ags         = "remove"
                     ) {

  #remove statinfo
  if(removestat) {
    import <- import[,!grepl("Statistik_",names(import))]
  }


  ####Time Variable ####

  #extract time variable
  zeitlabel <- import$Zeit_Label[1]

  #remove redundant time info
  import <- import[,!grepl("Zeit_",names(import))]

  #label time variable
  names(import)[which(names(import)=="Zeit")] <- zeitlabel

  #convert time variable to date, if reference date
  if(zeitlabel=="Stichtag"){
    import[,zeitlabel] <- as.Date(import[,zeitlabel], format = "%d.%m.%Y")
  }



  #check categorical variables
  cats <- unique.data.frame(as.data.frame(import[,names(import)[grepl("_Merkmal_Label",names(import))]]))

  #count number of unique values in supposedly constant columns
  num_unique <- sapply(cats, function(x) length(unique(x)))


  if(any(num_unique>1)) {
    warning("At least one column contains more than one variable. Columns 'Variable' and 'Property' are created. The resulting dataset should be partitioned by 'Variable' to produce tidy datasets.")
  }


  #extract names for variables as if all were constant columns
  klassm <- t(import[1,names(import)[grepl("_Merkmal_Label",names(import))]])

  #create renaming vector for categorical variables
  catnames <- ifelse(num_unique>1,"Property",klassm)

  #name categorical variables. If one columns contains more than on variable, name column "Property"
  names(import)[grepl("_Auspraegung_Label",names(import))] <- catnames

  #rename the column to differentiate between them "Variable"
  names(import)[grepl("_Merkmal_Label",names(import))] <- ifelse(num_unique>1,"Variable",names(import)[grepl("_Merkmal_Label",names(import))])

  #get label
  labelaus <- names(import)[grepl("_Auspraegung_Code",names(import))]

  #get regional level
  if(klassm[1]=="Gemeinden"){reglevel <- 4}
  if(is.element(klassm[1],c("Kreisfreie Städte und Kreise","Kreise und kreisfreie Städte"))){reglevel <- 3}
  if(is.element(klassm[1],c("Regierungsbezirke","Regierungsbezirke / Statistische Regionen"))){reglevel <- 2}
  if(klassm[1]=="Land"){reglevel <- 1}
  if(klassm[1]=="Deutschland insgesamt"){reglevel <- 0}

  if(reglevel  == 4) {
    import <- import[nchar(import[,labelaus[1]])==8,]
  }else if(reglevel  == 3){
    import <- import[nchar(import[,labelaus[1]])==5,]
  }else if(reglevel  == 2){
    import <- import[nchar(import[,labelaus[1]])==3,]
  }else if(reglevel  == 1){
    import <- import[nchar(import[,labelaus[1]])==2,]
  }

  #remove totals
  for(c in 1:ncol(import)){

    if(is.character(import[,c])){

      if(removetotal) {
        import <- import[import[,c]!="Insgesamt",]
      }

      import[,c] <- trimws(import[,c])

    }
  }

  #ags
  if(ags == "keep"){
    import$AGS <- import[,labelaus[1]]
  }else if(ags == "replace"){
    import[,klassm[1]]     <- import[,labelaus[1]]
  }else if(ags == "combine"){
    import[,klassm[1]]     <- paste0(import[,labelaus[1]], "_", import[,klassm[1]])
  }


  #remove not needed variables
  import <- import[,!grepl("_Code|_Label",names(import))]

  #get metric variables
  metrics <- names(import)[!is.element(names(import),c(zeitlabel, catnames, c("Variable","Property","AGS")))]

  #clean metric variables
  for(m in 1:length(metrics)){

    #replace statistical zeros
    import[import[,metrics[m]]=="-",metrics[m]]                               <- "0"

    #replace unknown, disclosed values, placeholders for values expected in the future and insecure values
    import[import[,metrics[m]]=="..."| import[,metrics[m]] == "."|import[,metrics[m]] == "/",metrics[m]] <- NA

    #use decimal point
    import[,metrics[m]]                                                       <- gsub(",",".",import[,metrics[m]])

    #make variable numeric
    import[,metrics[m]]                                                       <- as.numeric(import[,metrics[m]])
  }



return(import)

}

