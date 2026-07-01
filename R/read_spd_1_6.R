
# read.spd function ver 1.3 February 2015
# 
# revision 1.1b (TV)
# corrected to fix bug for calculation of western longitudes and southern latitudes
# Wfile default set to FALSE instead of TRUE
#
# revision 1.2 (TV)
# corrected to fix bug in calculations of duration and starttime (problem with year/millenium and 2400h)
#
#
# revision 1.3
# by Tone, february 2015
# include column "id" for spd-files with multiple years. id=year*100000+ser_no
# corrected "year" to 4-digit year (ex. 1994)
# removed trailing white spaces in "species" column. NOTE: number of characters is still limited to 12 
# included selection of "id", "Condition" and "St_type"
#
#
# revision 1.4
# by Tone, may 2015
# changed error in starttime following the change in year done in v 1.3. 
#
# revision 1.5 
# by Elvar and Tone, aug 2018
# Added stoptime
#
# revision 1.6
# by Mikko
# Replaced arbitrary temporary file writing by readr::read_fwf. Made column classes more explicit. Added tidyverse orientated read_spd function which returns data in Biotic format similar to BioticExplorerServer::bioticToDatabase

#' @title Read old IMR spd data files
#' @description The original function as written by Tone and Elvar with small improvements
#' @param filename Character specifying the path to the file which should be read 
#' @return A list of data from an spd file 
#' @importFrom readr read_fwf fwf_widths
#' @author Tone Vollen, Elvar Hallfredsson, Mikko Vihtakari

read.spd <- function(
    filename, Sfile = TRUE, Tfile = TRUE, Ufile = TRUE, Vfile=TRUE,
    Wfile = FALSE, Species = {}, Month = {}, Year = {}, Condition = {}, Quality={},
    St_type = {}, Area = {}, Gearcode = {}, Country = {}, Ser_no = {}, ID={}
) {
  # current.wd=getwd()
  # setwd(workdir)
  x <- scan(filename, what = "", sep = "\n", fileEncoding = "latin1", skipNul = TRUE)	
  x <- x[substr(x,1,1) %in% c("S","T","U","V","W")]
  # setwd(current.wd)
  
  ## S ####
  
  xS <- x[substr(x,1,1)=="S"]
  if((length(xS)>0)&(Sfile))
  {
    Sdata <- 
      readr::read_fwf(
        file = I(xS),
        col_positions = 
          readr::fwf_widths(
            c(1,3,2,1,6,2,2,4,5,1,2,2,1,3,2,1,1,1,2,3,4,2,4,2,2,2,4,4,4,3,1,1,
              4,4,3,2,3,3,2,4,18,1,2,2,2,2), 
            c("linetype", "year", "country", "shipcode", "ship", "month", "day",
              "station", "ser_no", "st_type", "lat_deg", "lat_min", "lat_decmin",
              "lon_deg", "lon_min", "lon_decmin", "nsew", "system", "area", "location",
              "b_depth", "noofgears", "gearcode", "gear_no", "gear_direction", 
              "speed_KNx10", "starttime_utc", "log_nmx10", "stoptime_utc", "distancex10",
              "condition", "quality", "maxdepth", "mindepth", "openingx10", 
              "std_openingx10", "spread", "std_spreadx10", "special_code", "wire_length",
              "comment", "qual_mark", "qual_proc", "recodingprog", "orig_format", 
              "format"
            )
          ),
        show_col_types = FALSE,
        col_types = cols(.default = "c")
      )
    
    # write.table(xS,file='xS.txt', quote=FALSE, row.names=FALSE,col.names=FALSE)
    # Sdata <- read.fwf(
    #   'xS.txt', 
    #   width = c(1,3,2,1,6,2,2,4,5,1,2,2,1,3,2,1,1,1,2,3,4,2,4,2,2,2,4,4,4,3,1,1,
    #             4,4,3,2,3,3,2,4,18,1,2,2,2,2),
    #   fileEncoding = "latin1")
    # names(Sdata) <- c(
    #   "linetype", "year", "country", "shipcode", "ship", "month", "day", "station",
    #   "ser_no", "st_type", "lat_deg", "lat_min", "lat_decmin", "lon_deg", "lon_min",
    #   "lon_decmin", "nsew", "system", "area", "location", "b_depth", "noofgears", 
    #   "gearcode", "gear_no", "gear_direction", "speed_KNx10", "starttime_utc", 
    #   "log_nmx10", "stoptime_utc", "distancex10", "condition", "quality", "maxdepth",
    #   "mindepth", "openingx10", "std_openingx10", "spread", "std_spreadx10", 
    #   "special_code", "wire_length", "comment", "qual_mark", "qual_proc", "recodingprog",
    #   "orig_format", "format"
    # )
    
    Sdata$year <- as.integer(Sdata$year)
    Sdata$year<-ifelse(Sdata$year>500,Sdata$year+1000,Sdata$year+2000)
    Sdata$ser_no <- as.character(Sdata$ser_no)
    Sdata$id <- Sdata$year*100000+as.integer(Sdata$ser_no)  #adding column "id" to make unique id for spd-files with multiple years
    
    if(!is.null(Year))
      Sdata=subset(Sdata,year%in%Year)
    if(!is.null(Month))
      Sdata=subset(Sdata,month%in%Month)
    if(!is.null(Condition))
      Sdata=subset(Sdata,condition%in%Condition)
    if(!is.null(Quality))
      Sdata=subset(Sdata,quality%in%Quality)
    if(!is.null(St_type))
      Sdata=subset(Sdata,as.character(st_type)%in%as.character(St_type))
    if(!is.null(Area))
      Sdata=subset(Sdata,area%in%Area)
    if(!is.null(Gearcode))
      Sdata=subset(Sdata,gearcode%in%Gearcode)
    if(!is.null(Country))
      Sdata=subset(Sdata,country%in%Country)
    if(!is.null(Ser_no))
      Sdata=subset(Sdata,ser_no%in%Ser_no)
    if(!is.null(ID))                  
      Sdata=subset(Sdata,id%in%ID)            
    
    # class handling 
    Sdata$b_depth=as.numeric(Sdata$b_depth)
    Sdata$maxdepth=as.numeric(Sdata$maxdepth)
    Sdata$mindepth=as.numeric(Sdata$mindepth)
    Sdata$starttime_utc <- as.numeric(Sdata$starttime_utc)
    Sdata$stoptime_utc <- as.numeric(Sdata$stoptime_utc)
    Sdata$area <- as.integer(Sdata$area)
    Sdata$location <- as.integer(Sdata$location)
    Sdata$spread <- as.numeric(Sdata$spread)
    Sdata$wire_length <- as.numeric(Sdata$wire_length)
    
    # Additional computation for:
    # longitude and latitude in decimal format
    NScoef=rep(1,dim(Sdata)[1])
    NScoef[Sdata$nsew>1]=-1
    EWcoef=rep(1,dim(Sdata)[1])
    EWcoef[(Sdata$nsew==1)|(Sdata$nsew==3)]=-1
    
    Sdata$longitude <- 
      (as.numeric(Sdata$lon_deg)+(as.numeric(Sdata$lon_min)+
                                    (as.numeric(Sdata$lon_decmin)/10))/60)*EWcoef
    Sdata$latitude <- 
      as.numeric(Sdata$lat_deg)+(as.numeric(Sdata$lat_min)+
                                   (as.numeric(Sdata$lat_decmin)/10))/60*NScoef
    
    # sampling time and duration in R format
    
    Sdata$starttime_utc[Sdata$starttime_utc==2400]<-0
    Sdata$stoptime_utc[Sdata$stoptime_utc==2400]<-0
    
    Start.sampling <- 
      strptime(paste(Sdata$year,'-',Sdata$month,'-',Sdata$day,' ', 
                     substr(as.numeric(Sdata$starttime_utc) + 10000,2,3),
                     ':',substr(as.numeric(Sdata$starttime_utc) + 10000,4,5), sep=''),
               format="%Y-%m-%d %H:%M") 
    Stop.sampling <- 
      strptime(paste(Sdata$year,'-',Sdata$month,'-',Sdata$day,' ',
                     substr(as.numeric(Sdata$stoptime_utc) + 10000,2,3),
                     ':',substr(as.numeric(Sdata$stoptime_utc) + 10000,4,5), sep=''),
               format="%Y-%m-%d %H:%M")
    
    Sampling.duration=difftime(Stop.sampling,Start.sampling,units="hours")
    Stop.sampling[Sampling.duration < 0 & !is.na(Sampling.duration)] <- 
      Stop.sampling[Sampling.duration < 0 & !is.na(Sampling.duration)] + 24*60*60  # add a day to stations going over the day break
    Sampling.duration=difftime(Stop.sampling,Start.sampling,units="hours")
    
    Sdata$starttime <- Start.sampling
    Sdata$stoptime <- Stop.sampling
    Sdata$duration <- as.numeric(Sampling.duration)
    
    # Kan skrive ut en *.csv fil for plotting i GIS etc.                  
    #write.table(Sdata,file='stations5.csv', quote=F, sep=',', row.names=F,col.names=T)
  }
  else
    Sdata={}
  
  #function for removing blank spaces after species names
  # trim <- function( x ) {
  #   gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
  # } # trimws() does the same thing
  
  ## T ####
  
  xT <- x[substr(x,1,1)=="T"]
  if((length(xT)>0)&(Tfile))
  {
    
    # Hack to make Norwegian letters work
    xT <- gsub("(Å)|(\u008f)", "x", xT)
    xT <- gsub("(Ø)|(\u009d)", "z", xT)
    xT <- gsub("Æ", "q", xT)
    
    Tdata <- 
      readr::read_fwf(
        I(xT),
        readr:::fwf_widths(
          c(1,3,2,1,6,2,2,4,5,1,12,1,2,2,1,1,7,6,1,1,6,4,4,1,1,1,1),
          c("linetype", "year", "country", "shipcode", "ship", "month", "day",
            "station", "ser_no", "sp_code", "species", "sample_no", "sample_type",
            "group", "conservation", "catch_measure", "catchweightx1000",
            "catchsize", "sample_measure", "L_measure_method", "sampleweightx1000",
            "samplesize","indsamplesize", "scal_oto", "parasite", "stomach", "genetics")
        ),
        show_col_types = FALSE,
        col_types = cols(.default = "c")
      )
    
    # Convert back to Norwegian letters
    Tdata$species <- gsub("x", "Å", Tdata$species)
    Tdata$species <- gsub("z", "Ø", Tdata$species)
    Tdata$species <- gsub("q", "Æ", Tdata$species)
    Tdata$species <- tolower(trimws(Tdata$species))
    
    unique(Tdata$species)
    
    # write.table(xT,file='xS.txt', quote=FALSE, row.names=FALSE,col.names=FALSE)
    # Tdata <- read.fwf(file='xS.txt', 
    #                   width = c(1,3,2,1,6,2,2,4,5,
    #                             1,12,1,2,2,
    #                             1,1,7,6,
    #                             1,1,6,4,
    #                             4,1,1,1,1))
    # names(Tdata) <- 
    #   c("linetype", "year", "country", "shipcode", "ship", "month", "day", "station",
    #     "ser_no", "sp_code", "species", "sample_no", "sample_type", "group",
    #     "conservation", "catch_measure", "catchweightx1000", "catchsize",
    #     "sample_measure", "L_measure_method", "sampleweightx1000", "samplesize",
    #     "indsamplesize", "scal_oto", "parasite", "stomach", "genetics")
    
    Tdata$year <- as.integer(Tdata$year)
    Tdata$year <- ifelse(Tdata$year>500,Tdata$year+1000,Tdata$year+2000)
    Tdata$ser_no <- as.character(Tdata$ser_no)
    Tdata$id<-Tdata$year*100000 + as.integer(Tdata$ser_no)
    
    # Column classes
    
    Tdata$catchweightx1000 <- as.numeric(Tdata$catchweightx1000) 
    Tdata$catchsize <- as.numeric(Tdata$catchsize) 
    Tdata$sampleweightx1000 <- as.numeric(Tdata$sampleweightx1000) 
    Tdata$samplesize <- as.numeric(Tdata$samplesize) 
    
    # Subset
    
    Tdata = subset(Tdata,id %in% Sdata$id)  
    
    if(!is.null(Species))  Tdata=subset(Tdata,species%in%Species)
    # additional code to insert the linetype properly because 'T' is interpreted by R as the logical true and not the letter T
    Tdata[,1]=rep('T',dim(Tdata)[1])
    
  }
  else
    Tdata={}
  
  ## U ####
  
  xU <- x[substr(x,1,1)=="U"]
  
  if(length(xU) > 0 & Ufile) {
    
    # Hack to make Norwegian letters work
    xU <- gsub("(Å)|(\u008f)", "x", xU)
    xU <- gsub("(Ø)|(\u009d)", "z", xU)
    xU <- gsub("Æ", "q", xU)
    
    Udata <- 
      readr::read_fwf(
        I(xU),
        readr:::fwf_widths(
          c(1,3,2,1,6,2,2,4,5,1,12,1,1,1,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
            2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2),
          c("linetype", "year", "country", "shipcode", "ship", "month", "day", 
            "station", "ser_no", "sp_code", "species", "sample_no", "interval",
            "lengthsample_sex", "leastLgroup","L1","L2","L3","L4","L5","L6","L7",
            "L8","L9","L10","L11","L12","L13","L14","L15","L16","L17","L18","L19",
            "L20","L21","L22","L23","L24","L25","L26","L27","L28","L29","L30",
            "L31","L32","L33","L34","L35","L36","L37")
        ),
        show_col_types = FALSE,
        col_types = cols(.default = "c")
      )
    
    # Convert back to Norwegian letters
    Udata$species <- gsub("x", "Å", Udata$species)
    Udata$species <- gsub("z", "Ø", Udata$species)
    Udata$species <- gsub("q", "Æ", Udata$species)
    Udata$species <- tolower(trimws(Udata$species))
    
    # write.table(xU,file='xU.txt', quote=FALSE, row.names=FALSE,col.names=FALSE)
    # Udata <- read.fwf('xU.txt', width = c(1,3,2,1,6,2,2,4,5,
    #                                       1,12,1,
    #                                       1,1,
    #                                       3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    #                                       2,2,2,2,2,2,2,2,2,2,2,2,2,2))
    # names(Udata) <- c("linetype", "year", "country", "shipcode", "ship", "month", "day", "station",
    #                   "ser_no",
    #                   
    #                   "sp_code", "species", "sample_no",
    #                   
    #                   "interval", "lengthsample_sex",
    #                   
    #                   "leastLgroup","L1","L2","L3","L4","L5","L6","L7","L8","L9","L10",
    #                   "L11","L12","L13","L14","L15","L16","L17","L18","L19","L20",
    #                   "L21","L22","L23","L24","L25","L26","L27","L28","L29","L30",
    #                   "L31","L32","L33","L34","L35","L36","L37")
    
    Udata$year <- as.integer(Udata$year)
    Udata$year <- ifelse(Udata$year>500,Udata$year+1000,Udata$year+2000)
    Udata$ser_no <- as.character(Udata$ser_no)
    Udata$id <- Udata$year*100000 + as.integer(Udata$ser_no)
    
    Udata <- subset(Udata, id %in% Sdata$id)
    
    if(is.null(Species)==FALSE)
      Udata=subset(Udata,species%in%Species)
  }
  else
    Udata={}
  
  ## V ####
  
  xV <- x[substr(x,1,1)=="V"]
  if((length(xV)>0)&(Vfile==TRUE))
  {
    # Hack to make Norwegian letters work
    xV <- gsub("(Å)|(\u008f)", "x", xV)
    xV <- gsub("(Ø)|(\u009d)", "z", xV)
    xV <- gsub("Æ", "q", xV)
    
    Vdata <- 
      readr::read_fwf(
        I(xV),
        readr:::fwf_widths(
          c(1,3,2,1,6,2,2,4,5,1,12,1,3,1,5,1,3,1,1,1,2,1,1,1,1,4,2,2,2,2,1,1,
            1,1,2,2,2,2,2,2,2,2,2,2,2,1,2,6,1,4,4,5),
          c("linetype", "year", "country", "shipcode", "ship", "month", "day", "station",
            "ser_no", "sp_code", "species", "sample_no", "fish_no", "w_or_vol", "weight",
            "L_unit", "bodylength", "fat", "sex", "stage", "spec_stage", "stom_fullness",
            "digestion", "liver", "parasite", "spec_code", "vertebr", "age", "spawn_age",
            "spawn_marks", "readqual", "struct_type", "edge", "core", "calibration",
            "gz1", "gz2", "gz3", "gz4", "gz5", "gz6", "gz7", "gz8", "gz9", "gz_total",
            "tagtype", "tag_ser_code", "tagnumber", "w_or_vol2", "gonad", "liverweight",
            "gutted_weight")
        ),
        show_col_types = FALSE #,
        #col_types = cols(.default = "c")
      )
    
    # Convert back to Norwegian letters
    Vdata$species <- gsub("x", "Å", Vdata$species)
    Vdata$species <- gsub("z", "Ø", Vdata$species)
    Vdata$species <- gsub("q", "Æ", Vdata$species)
    Vdata$species <- tolower(trimws(Vdata$species))
    
    # write.table(xV,file='xV.txt', quote=FALSE, row.names=FALSE,col.names=FALSE)
    # Vdata <- read.fwf(
    #   'xV.txt', 
    #   width = c(1,3,2,1,6,2,2,4,5,1,12,1,3,1,5,1,3,1,1,1,2,1,1,1,1,4,2,2,2,2,1,1,
    #             1,1,2,2,2,2,2,2,2,2,2,2,2,1,2,6,1,4,4,5)
    #   )
    # 
    # names(Vdata) <- 
    #   c("linetype", "year", "country", "shipcode", "ship", "month", "day", "station",
    #     "ser_no", "sp_code", "species", "sample_no", "fish_no", "w_or_vol", "weight",
    #     "L_unit", "bodylength", "fat", "sex", "stage", "spec_stage", "stom_fullness",
    #     "digestion", "liver", "parasite", "spec_code", "vertebr", "age", "spawn_age",
    #     "spawn_marks", "readqual", "struct_type", "edge", "core", "calibration",
    #     "gz1", "gz2", "gz3", "gz4", "gz5", "gz6", "gz7", "gz8", "gz9", "gz_total",
    #     "tagtype", "tag_ser_code", "tagnumber", "w_or_vol2", "gonad", "liverweight",
    #     "gutted_weight")
    
    Vdata$year <- as.integer(Vdata$year)
    Vdata$year<-ifelse(Vdata$year>500, Vdata$year+1000, Vdata$year+2000)  
    Vdata$ser_no <- as.character(Vdata$ser_no)
    Vdata$id <- Vdata$year*100000 + as.integer(Vdata$ser_no)
    
    
    
    Vdata=subset(Vdata,id%in%Sdata$id) 
    if(is.null(Species)==FALSE)
      Vdata=subset(Vdata,species%in%Species)
    
  }
  else
    Vdata={}
  
  ## W ####
  
  xW <- x[substr(x,1,1)=="W"]
  if((length(xW)>0)&(Wfile==TRUE))
  {
    
    # Hack to make Norwegian letters work
    xW <- gsub("(Å)|(\u008f)", "x", xW)
    xW <- gsub("(Ø)|(\u009d)", "z", xW)
    xW <- gsub("Æ", "q", xW)
    
    Wdata <- 
      readr::read_fwf(
        I(xW),
        readr:::fwf_widths(
          c(1,3,2,1,6,2,2,4,5,1,12,1,3,1,12,1,1,4,1,6,1,1,3,2,2,2,2,2,2,2,
            2,2,2,2,2,2,2,2,2,2,2,2,2,2,2),
          c("linetype", "year", "country", "shipcode", "ship", "month", "day", 
            "station", "ser_no", "sp_code", "species", "sample_no", "fish_no", 
            "prey_code", "prey", "digestive_stage", "Unit4number", "Weight_of_prey",
            "interv_stage", "length", "leastLgroup","L1","L2","L3","L4","L5","L6",
            "L7","L8","L9","L10","L11","L12","L13","L14","L15","L16","L17","L18",
            "L19","L20","L21","L22")
        ),
        show_col_types = FALSE,
        col_types = cols(.default = "c")
      )
    
    # Convert back to Norwegian letters
    Wdata$species <- gsub("x", "Å", Wdata$species)
    Wdata$species <- gsub("z", "Ø", Wdata$species)
    Wdata$species <- gsub("q", "Æ", Wdata$species)
    Wdata$species <- tolower(trimws(Wdata$species))
    
    # write.table(xW,file='xW.txt', quote=FALSE, row.names=FALSE,col.names=FALSE)
    # Wdata <- 
    #   read.fwf(
    #     'xW.txt', 
    #     width = c(1,3,2,1,6,2,2,4,5,1,12,1,3,1,12,1,1,4,1,6,1,1,3,2,2,2,2,2,2,2,
    #               2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
    #     )
    # 
    # names(Wdata) <- 
    #   c("linetype", "year", "country", "shipcode", "ship", "month", "day", 
    #     "station", "ser_no", "sp_code", "species", "sample_no", "fish_no", 
    #     "prey_code", "prey", "digestive_stage", "Unit4number", "Weight_of_prey",
    #     "interv_stage", "length", "leastLgroup","L1","L2","L3","L4","L5","L6",
    #     "L7","L8","L9","L10","L11","L12","L13","L14","L15","L16","L17","L18",
    #     "L19","L20","L21","L22")
    
    Wdata$year <- as.integer(Wdata$year)
    Wdata$year <- ifelse(Wdata$year>500,Wdata$year+1000,Wdata$year+2000)  
    Wdata$ser_no <- as.character(Wdata$ser_no)
    Wdata$id <- Wdata$year*100000 + as.integer(Wdata$ser_no)
    
    Wdata=subset(Wdata,id%in%Sdata$id) 
    if(!is.null(Species))
      Wdata=subset(Wdata,species%in%Species)
    
  }
  else
    Wdata={}
  
  out <- list(S = Sdata, `T` = Tdata, U = Udata, V = Vdata, W = Wdata)
  
  return(out)
}

#' @title Read old IMR spd files to a BioticExplorerServer compatible format
#' @param filename Character specifying the file path. Can be linked to the server
#' @param verbose Logical indicating whether to return progress messages while the function reads and manipulates data
#' @details The function uses \code{read.spd} function written by Tone Vollen and Elvar Hallfredsson to read old IMR spd files. It then combines S (Mission and Fishstation) and T (Catchsample) to station level data as well as the above mentioned and U (Individual) and V (Agedetermination) to individual level data frames such that the data are compatible with the output of \href{https://github.com/MikkoVihtakari/BioticExplorerServer/blob/master/R/bioticToDatabase.R}{BioticExplorerServer::bioticToDatabase}.
#' @import dplyr tibble tidyr BioticExplorerServer
#' @author Mikko Vihtakari

read_spd <- function(filename, verbose = FALSE) {
  
  # Translation dictionary ####
  
  S_translations <- c(
    startyear = "year", 
    callsignal = "ship", 
    serialnumber = "ser_no", 
    nation = "country", 
    station = "station", 
    stationstartdate = "starttime", 
    stationstopdate = "stoptime",
    stationtype = "st_type", 
    longitudestart = "longitude", 
    latitudestart = "latitude", 
    system = "system", 
    area = "area",
    location = "location",
    bottomdepthstart = "b_depth",
    fishingdepthmax = "maxdepth", 
    fishingdepthmin = "mindepth",
    gearno = "gear_no", 
    gear = "gearcode",
    gearcount = "noofgears",
    direction = "gear_direction", 
    vesselspeed = "speed_KNx10",
    logstart = "log_nmx10", 
    distance = "distancex10",
    gearcondition = "condition",
    samplequality = "quality",
    verticaltrawlopening = "openingx10",
    verticaltrawlopeningsd = "std_openingx10",
    trawldoorspread = "spread",
    trawldoorspreadsd = "std_spreadx10", 
    wirelength ="wire_length",
    soaktime = "duration",
    stationcomment = "comment",
    dataquality = "qual_mark",
    "NA" = "shipcode", 
    "NA" = "month", 
    "NA" = "day", 
    "NA" = "lat_deg",
    "NA" = "lat_min", 
    "NA" = "lat_decmin",
    "NA" = "lon_deg",
    "NA" = "lon_min",
    "NA" = "lon_decmin",
    "NA" = "nsew",
    "NA" = "starttime_utc",
    "NA" = "stoptime_utc",
    "NA" = "special_code",
    "NA" = "qual_proc", 
    "NA" = "recodingprog",
    "NA" = "orig_format",
    "NA" = "format",
    "NA" = "id"
  )
  
  T_translations <- c(
    identification = "sp_code",
    commonname = "species",
    catchsampleid = "sample_no", 
    sampletype = "sample_type",
    group = "group",
    conservation = "conservation",
    catchproducttype = "catch_measure",
    catchweight = "catchweightx1000", 
    catchcount = "catchsize",
    sampleproducttype = "sample_measure",
    lengthmeasurement = "L_measure_method",
    lengthsampleweight = "sampleweightx1000", 
    lengthsamplecount = "samplesize",
    specimensamplecount = "indsamplesize",
    agingstructure = "scal_oto",
    parasite = "parasite",
    stomach = "stomach", 
    tissuesample = "genetics"
  )
  
  V_translations <- c(
    startyear = "year", 
    callsignal = "ship", 
    serialnumber = "ser_no", 
    catchsampleid = "sample_no",
    commonname = "species",
    specimenid = "fish_no", 
    individualproducttype = "w_or_vol", 
    individualweight = "weight", # divide
    lengthresolution = "L_unit", 
    length = "bodylength", 
    fat = "fat", 
    sex = "sex", 
    maturationstage = "stage", 
    specialstage = "spec_stage", 
    stomachfillfield = "stom_fullness", 
    digestion = "digestion", 
    liver = "liver", 
    liverparasite = "parasite", 
    vertebraecount = "vertebr",
    gonadweight = "gonad", 
    liverweigh = "liverweight", 
    age = "age",
    spawningage = "spawn_age", 
    spawningzones = "spawn_marks", 
    readability = "readqual",
    otolithtype = "struct_type",
    otolithedge = "edge",
    otolithcentre = "core", 
    calibration = "calibration", 
    growthzone1 = "gz1", 
    growthzone2 = "gz2",
    growthzone3 = "gz3",
    growthzone4 = "gz4",
    growthzone5 = "gz5",
    growthzone6 = "gz6", 
    growthzone7 = "gz7", 
    growthzone8 = "gz8",
    growthzone9 = "gz9", 
    growthzonestota = "gz_total", 
    tagtype = "tagtype",
    tagid = "tag_ser_code", 
    tagnumber = "tagnumber",
    "NA" = "linetype", 
    "NA" = "country", 
    "NA" = "shipcode", 
    "NA" = "month", 
    "NA" = "day",
    "NA" = "station", 
    "NA" =  "sp_code",
    "NA" = "spec_code", 
    "NA" = "w_or_vol2", 
    "NA" = "gutted_weight",
    "NA" = "id"
  )
  
  U_translations <- c(
    startyear = "year", 
    callsignal = "ship", 
    serialnumber = "ser_no", 
    catchsampleid = "sample_no",
    commonname = "species",
    lengthresolution = "interval",
    length = "length",
    sex = "lengthsample_sex",
    "NA" = "leastLgroup"
  )
  
  if(verbose) message("Reading the spd file")
  
  # Read the file ####
  
  x <- read.spd(filename)
  
  # mission ####
  
  # Stnall ####
  
  if(verbose) message("Compiling station data")
  
  if(!is.null(x$T)) {
    stn <- dplyr::full_join(
      dplyr::select(x$S, -linetype),
      dplyr::select(x$T, -linetype),
      by = dplyr::join_by(year, country, shipcode, ship, month, day, station, ser_no, id)
    )
  } else {
    message("The file does not contain T element (fishstation). Returning nothing. If you want to open the file, use the read.spd function.")
    return(NULL)
  }
  
  ## Correct multiplied fields
  
  stn[grepl("x10$", colnames(stn))] <- 
    lapply(stn[grepl("x10$", colnames(stn))], function(k) as.numeric(k)/10)
  stn[grepl("x1000$", colnames(stn))] <- 
    lapply(stn[grepl("x1000$", colnames(stn))], function(k) as.numeric(k)/1000)
  
  ## Rename columns
  
  # colnames(stn) %in% c(unname(S_translations), unname(T_translations))
  
  stn <- stn[,c(unname(S_translations), unname(T_translations))]
  colnames(stn) <- c(names(S_translations), names(T_translations))
  stn <- stn[!colnames(stn) %in% "NA"]
  
  ## Add gear category
  
  data(gearList, package = "BioticExplorerServer")
  
  stn <- dplyr::left_join(
    stn,
    dplyr::select(gearList, -description),
    by = join_by(gear == code)
  )
  
  stn <- dplyr::relocate(stn, c(gear, gearname, gearcategory), .after = "gearno")
  
  # Indall ####
  
  if(verbose) message("Compiling individual data")
  
  if(!is.null(x$V)) {
    
    ind <- x$V
    
    ind <- ind[,unname(V_translations)]
    colnames(ind) <- names(V_translations)
    ind <- ind[!colnames(ind) %in% "NA"]
    
    ind$maturationstage <- as.integer(ind$maturationstage)
    ind$specialstage <- as.integer(ind$specialstage)
    ind$gonadweight <- as.numeric(ind$gonadweight)
    ind$individualweight <- as.numeric(ind$individualweight)/1000 # to kg
    ind$length <- as.numeric(ind$length)/100 # to m
    ind$catchsampleid <- as.character(ind$catchsampleid)
    ind$specimenid <- as.integer(ind$specimenid)
    ind$age <- as.numeric(ind$age)
    
    indall <- try({
      dplyr::right_join(
        stn, ind,
        by = join_by(startyear, callsignal, serialnumber, commonname, catchsampleid),
        relationship = "one-to-many")
    },
    silent = TRUE
    )
    
    if(inherits(indall, "try-error")){
      
      # This case happens when stn contains duplicate rows by startyear, callsignal, serialnumber, commonname, catchsampleid
      
      group_cols <- c("startyear", "callsignal", "serialnumber", "commonname", "catchsampleid")
      
      tmp <- stn %>% 
        unite("id", all_of(group_cols), remove = FALSE) %>% 
        mutate(rownumber = 1:nrow(.))
      
      unique_cols <- 
        c("nation", "station", "stationstartdate", "stationstopdate", 
          "stationtype", "longitudestart", "latitudestart", "system", "area", 
          "location", "bottomdepthstart", "fishingdepthmax", "fishingdepthmin", 
          "gearno", "gear", "gearname", "gearcategory", "gearcount", "direction", 
          "vesselspeed", "logstart", "distance", "gearcondition", "samplequality", 
          "verticaltrawlopening", "verticaltrawlopeningsd", "trawldoorspread", 
          "trawldoorspreadsd", "wirelength", "soaktime", "stationcomment", 
          "dataquality", "identification", "sampletype", "group", "conservation", 
          "catchproducttype",  "sampleproducttype", "lengthmeasurement",   
          "specimensamplecount", "agingstructure", "parasite", "stomach", 
          "tissuesample", "rownumber")
      
      sum_cols <- setdiff(colnames(stn), c(group_cols, unique_cols))
      
      dup_dat <- tmp %>% filter(id %in% tmp$id[duplicated(tmp$id)])
      
      message(sum(duplicated(tmp$id)), " duplicate rows in stn. They have been collated. " ,
              ifelse(any(grepl("blak|blåk", dup_dat$commonname, ignore.case = TRUE)),
                     paste(sum(grepl("blak|blåk", dup_dat$commonname, ignore.case = TRUE))/2, 
                           "stations with Greenland halibut in the collated rows."),
                     "No Greenland halibut in the collated rows."))
      
      dup_dat <-  dup_dat %>% 
        group_by(across(c(id, all_of(group_cols)))) %>% 
        reframe(across(unique_cols, function(k) {
          if(all(is.na(k))) NA else {
            out <- unique(k[!is.na(k)])
            if(length(out > 1)) out[1] else out
          }
        }),
        across(sum_cols, ~ sum(.x, na.rm = TRUE))) 
      
      # New stn with duplicate rows removed
      
      stn <- 
        bind_rows(
        tmp %>% filter(!id %in% tmp$id[duplicated(tmp$id)]),
        dup_dat) %>% 
        arrange(rownumber) %>% 
        dplyr::select(-id, -rownumber)
        
      # Fixed indall
      
      indall <- dplyr::right_join(
        stn, ind,
        by = join_by(startyear, callsignal, serialnumber, commonname, catchsampleid),
        relationship = "one-to-many")
      
    }
  } else {
    indall <- NULL
  }
  
  # Length distributions (ldist) ####
  
  if(verbose) message("Compiling length distribution data")
  
  if(!is.null(x$U)) {
    
    ind <- dplyr::select(x$U, c(year, ship, ser_no, sample_no, species, interval, lengthsample_sex, leastLgroup, grep("^L\\d", colnames(x$U), value = TRUE)))
    
    ## Convert lengths
    
    ind <- pbapply::pblapply(1:nrow(ind), function(i) {
      tmp <- ind[i,]
      tmp <- tidyr::pivot_longer(tmp, cols = matches("^L\\d"), names_to = "length") 
      tmp$value <- as.integer(tmp$value)
      
      ### output lengths in m
      if(as.integer(unique(tmp$interval)) == 7) {
        tmp$length <- (as.numeric(tmp$leastLgroup) +
                         as.numeric(gsub("L", "", tmp$length)) - 1)/1000
      } else if(as.integer(unique(tmp$interval)) == 6) {
        tmp$length <- (as.numeric(tmp$leastLgroup) +
                         as.numeric(gsub("L", "", tmp$length))*5 - 5)/1000
      } else if(as.integer(unique(tmp$interval)) == 3) {
        tmp$length <- (as.numeric(tmp$leastLgroup) +
                         as.numeric(gsub("L", "", tmp$length)) - 1)/100
      } else if(as.integer(unique(tmp$interval)) == 2) {
        tmp$length <- (as.numeric(tmp$leastLgroup) +
                         as.numeric(gsub("L", "", tmp$length))*5 - 5)/1000
      } else if(as.integer(unique(tmp$interval)) == 1) {
        
        if(any(tmp$leastLgroup < 10)) {
          warning("Detecting < 10 mm least group length for ", unique(tmp$species))
        }
        
        tmp$length <- (as.numeric(tmp$leastLgroup) +
                         as.numeric(gsub("L", "", tmp$length)) - 1)/1000
      } else {
        stop("The interval column has a value of ", unique(tmp$interval), 
             " which is not defined in the handbook. Returning 0 lengths")
        tmp$length <- 0
      }
      
      tmp <- tmp[!is.na(tmp$value),]
      
      tidyr::uncount(tmp, value)
    }) %>% 
      dplyr::bind_rows()
    
    ## Rename columns
    
    # colnames(stn) %in% c(unname(S_translations), unname(T_translations))
    
    ind <- ind[,unname(U_translations)]
    colnames(ind) <- names(U_translations)
    ind <- ind[!colnames(ind) %in% "NA"]
    
    ## Merge
    
    ldist <- dplyr::right_join(
      stn, ind,
      by = join_by(startyear, callsignal, serialnumber, commonname, catchsampleid))
  } else {
    ldist <- NULL
  }
  
  # Return ####
  
  if(verbose) message("Done")
  
  list(stnall = stn, indall = indall, ldist = ldist)
  
}