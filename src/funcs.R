compCheckSum <- function(sentance){
  
  checkSum <- sub("\\*", "", str_extract(sentance, "\\*[0-9A-F]{2}$"))
  
  #get the sentance
  nmea.sentance <- str_extract(sentance, "\\$.*\\*")
  nmea.sentance <- sub("[\\$\\*]", "", nmea.sentance)
  nmea.sentance <- sub("[\\$\\*]", "", nmea.sentance)
  
  calcCheckSum <- function(stringToCheck){
    
    #if(is.na(stringToCheck)){
    #  return(NA)
    #} else {
    
    checksum <- as.raw(0)  
    
    if(is.na(stringToCheck)){
      return(as.raw(0))
    }
    
    if(nchar(stringToCheck)==0){
      return(as.raw(0))
    }
    
    for(i in 1:nchar(stringToCheck)){
      x <- substr(stringToCheck, start = i, stop = i)
      x <- charToRaw(x)
      
      checksum <- xor(checksum, x)
      
    }
    
    return(checksum)
    #}
  }
  
  calcedCheckSum <- toupper(calcCheckSum(nmea.sentance))
  
  results <- checkSum == calcedCheckSum
  return(results) 
}

rad2deg <- function(rad){
  
  deg <- (rad * 180)/pi
  return(deg)
}

deg2rad <- function(deg){
  
  rad <- deg * (pi/180)
  return(rad)
}

getTrueWind <- function(AWS, AWA, SOG) { 
  #outputs a list with 
  # $tws = True Wind Speed
  # $twa = True Wind Angle
  
  #  B = AWA in radians
  b <- AWA * (pi/180)
  
  #true wind speed
  TWS <- sqrt(AWS^2 + SOG^2 - 2*AWS*SOG*cos(b))
  
  x <- ifelse(AWS*cos(b)>SOG, 0, -pi)
  
  TWA <- atan((AWS*sin(b))/(AWS*cos(b)-SOG))
  
  TWA <- TWA+x
  
  TWA <- ifelse(TWA < 0, TWA + 2*pi, TWA)
  
  TWA <- (TWA * 180)/pi
  
  #TWA <- ifelse(TWA > 90 , 360-TWA, TWA)
  
  x <- list(TWS=TWS, TWA=TWA, x=x)
  return(x)
}


parse.Coord <- function(coord){
  #takes a coordinate in the system xx xx.xxxx and converts it to xx.xxxxxx

  
  subParseCoord <- function(coord){
    if(substr(coord, 1, 1)=="0"){
      #it's a longitude
      deg <- as.numeric(substr(coord, 1, 3))
      min <- substr(coord, 4, nchar(coord)-2)
      min <- as.numeric(min)
      min <- min/60
      out <- deg + min
      
      if(substr(coord, nchar(coord), nchar(coord))=="W"){
        out <- out * -1
      }
      
      
    } else {
      #it's a latitude
      deg <- as.numeric(substr(coord, 1, 2))
      min <- substr(coord, 3, nchar(coord)-2)
      min <- as.numeric(min)
      min <- min/60
      out <- deg + min
      
      if(substr(coord, nchar(coord), nchar(coord))=="S"){
        out <- out * -1
      }
      
    }
    
    return(out)
    
  }  
  
  sapply(coord, subParseCoord, USE.NAMES = FALSE)

  
}

parse.GPGGA <- function(x){
  #x = character vector containing raw NMEA sentances
  
  pattern <- "\\\\[cC]:([0-9]*)\\*[0-9A-F]{2}\\\\\\$GPGGA,(?<time>[0-9.]+),(?<latitude>[0-9.]+,[NS]),(?<longitude>[0-9.]+,[EW]),(?<quality>[0-8]),.*,.*,([0-9]+)"
  
  matches <- as.data.frame(str_match(x, pattern), stringsAsFactors = FALSE)[,-1 ]
  
  names(matches) <- c("ts", "gga.time", "gga.lat", "gga.lon", "gps.qual", "gps.alt")
  
  #parse lat / lon
  matches$lat <- parse.Coord(matches$lat)
  matches$lon <- parse.Coord(matches$lon)
  
  #parse GPS time
  
  
  return(matches)
  
}


parse.IIMWV <- function(x){
  # Wind Speed and Angle
  
  ms.patt <- "\\\\[cC]:([0-9]*)\\*[0-9A-F]{2}\\\\"
  sent.patt <- "\\$IIMWV,([0-9.]+),[RT],([0-9.]+),[KMN]"
  
  pattern <- paste0(ms.patt, sent.patt)
  
  
  matches <- as.data.frame(str_match(x, pattern), stringsAsFactors = FALSE)[,-1 ]
  
  names(matches) <- c("ts", "AWA", "AWS")
  
  #Clean up data types
  matches$AWA <- as.numeric(matches$AWA)
  matches$AWS <- as.numeric(matches$AWS)
  
  return(matches)
}

parse.IIVHW <- function(x){
  pattern <- "\\\\[cC]:([0-9]*)\\*[0-9A-F]{2}\\\\\\$GPGGA,(?<time>[0-9.]+),(?<latitude>[0-9.]+,[NS]),(?<longitude>[0-9.]+,[EW]),(?<quality>[0-8]),.*,.*,([0-9]+)"
  
  matches <- as.data.frame(str_match(x, pattern), stringsAsFactors = FALSE)[,-1 ]
  
  names(matches) <- c("ts", "gga.time", "gga.lat", "gga.lon", "gps.qual", "gps.alt")
  
}

parse.IIVLW <- function(x){
  #$--VHW,x.x,T,x.x,M,x.x,N,x.x,K*hh<CR><LF>
  # Degress True
  # T = True
  # Degrees Magnetic
  # M = Magnetic
  # Knots (speed of vessel relative to the water)
  # N = Knots
  # Kilometers (speed of vessel relative to the water)
  # K = Kilometers
  # Checksum
  
  out <- str_split_fixed(x, "[$,]", n=10)
  
  out <- as.data.frame(out[, c(1, 5, 7)], stringsAsFactors = FALSE)
  
  
  names(out) <- c("ts", "vlw.cog", "vlw.bps")
  
  ms.patt <- "\\\\[cC]:([0-9]*)\\*[0-9A-F]{2}\\\\"
  
  out$ts <- str_match(out$ts, ms.patt)[,2]
  out$vlw.bps <- as.numeric(out$vlw.bps)
  out$vlw.cog <- as.numeric(out$vlw.cog)
  
  return(out)

}

parse.IIMTW <- function(x){
  pattern <- "\\\\[cC]:([0-9]*)\\*[0-9A-F]{2}\\\\\\$GPGGA,(?<time>[0-9.]+),(?<latitude>[0-9.]+,[NS]),(?<longitude>[0-9.]+,[EW]),(?<quality>[0-8]),.*,.*,([0-9]+)"
  
  matches <- as.data.frame(str_match(x, pattern), stringsAsFactors = FALSE)[,-1 ]
  
  names(matches) <- c("ts", "gga.time", "gga.lat", "gga.lon", "gps.qual", "gps.alt")
  
}

parse.GPBOD <- function(x){
  pattern <- "\\\\[cC]:([0-9]*)\\*[0-9A-F]{2}\\\\\\$GPGGA,(?<time>[0-9.]+),(?<latitude>[0-9.]+,[NS]),(?<longitude>[0-9.]+,[EW]),(?<quality>[0-8]),.*,.*,([0-9]+)"
  
  matches <- as.data.frame(str_match(x, pattern), stringsAsFactors = FALSE)[,-1 ]
  
  names(matches) <- c("ts", "gga.time", "gga.lat", "gga.lon", "gps.qual", "gps.alt")
  
}

parse.GPBWC <- function(x){
  pattern <- "\\\\[cC]:([0-9]*)\\*[0-9A-F]{2}\\\\\\$GPGGA,(?<time>[0-9.]+),(?<latitude>[0-9.]+,[NS]),(?<longitude>[0-9.]+,[EW]),(?<quality>[0-8]),.*,.*,([0-9]+)"
  
  matches <- as.data.frame(str_match(x, pattern), stringsAsFactors = FALSE)[,-1 ]
  
  names(matches) <- c("ts", "gga.time", "gga.lat", "gga.lon", "gps.qual", "gps.alt")
  
}

parse.GPRMB <- function(x){
  pattern <- "\\\\[cC]:([0-9]*)\\*[0-9A-F]{2}\\\\\\$GPGGA,(?<time>[0-9.]+),(?<latitude>[0-9.]+,[NS]),(?<longitude>[0-9.]+,[EW]),(?<quality>[0-8]),.*,.*,([0-9]+)"
  
  matches <- as.data.frame(str_match(x, pattern), stringsAsFactors = FALSE)[,-1 ]
  
  names(matches) <- c("ts", "gga.time", "gga.lat", "gga.lon", "gps.qual", "gps.alt")
  
}

parse.GPRMC <- function(x){
  pattern <- "\\\\[cC]:([0-9]*)\\*[0-9A-F]{2}\\\\\\$GPGGA,(?<time>[0-9.]+),(?<latitude>[0-9.]+,[NS]),(?<longitude>[0-9.]+,[EW]),(?<quality>[0-8]),.*,.*,([0-9]+)"
  
  matches <- as.data.frame(str_match(x, pattern), stringsAsFactors = FALSE)[,-1 ]
  
  names(matches) <- c("ts", "gga.time", "gga.lat", "gga.lon", "gps.qual", "gps.alt")
  
}

parse.GPVTG <- function(x){
  pattern <- "\\\\[cC]:([0-9]*)\\*[0-9A-F]{2}\\\\\\$GPGGA,(?<time>[0-9.]+),(?<latitude>[0-9.]+,[NS]),(?<longitude>[0-9.]+,[EW]),(?<quality>[0-8]),.*,.*,([0-9]+)"
  
  matches <- as.data.frame(str_match(x, pattern), stringsAsFactors = FALSE)[,-1 ]
  
  names(matches) <- c("ts", "gga.time", "gga.lat", "gga.lon", "gps.qual", "gps.alt")
  
}

parse.PGRME <- function(x){
  pattern <- "\\\\[cC]:([0-9]*)\\*[0-9A-F]{2}\\\\\\$GPGGA,(?<time>[0-9.]+),(?<latitude>[0-9.]+,[NS]),(?<longitude>[0-9.]+,[EW]),(?<quality>[0-8]),.*,.*,([0-9]+)"
  
  matches <- as.data.frame(str_match(x, pattern), stringsAsFactors = FALSE)[,-1 ]
  
  names(matches) <- c("ts", "gga.time", "gga.lat", "gga.lon", "gps.qual", "gps.alt")
  
}
















