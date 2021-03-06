#Create performance data frame for analysis
# 1. Remove non-race data
# 2. Summarise by 5 seconds
# 3. Calculate TWA, TWS, and TWD
# 4. Apply Wind & Speed Brackets
# 5. Calculate Performance Vs. POlar


#### Library & Source ####

library(dplyr)
library(tidyr)
library(akima)

#load("./output/parsed_nmea_data.RData")

#source("./src/funcs.R")


## 1. Remove non-race data  ===================================================
elapsed <- 1*3600 + 43*60 + 37

EndTime <- as.POSIXct("2017-08-18 21:03:37", tz="America/Detroit")

StartTime <- EndTime - elapsed

race <- dat[dat$TIME.S > StartTime & dat$TIME.S < EndTime, ] 


#round up to nearest 5 second interval
race$TIME.5S <- as.POSIXct((round(race$TIME.MS/5000)*5000)/1000, origin="1970-01-01")


#write.csv(race, "./output/fullrace_byMS.csv")


# 2. summarise Performance Data over 5 seconds =========================================

perf <- race %>% group_by(TIME.5S) %>% summarise(SOG=mean(VTG.SOG, na.rm = TRUE), 
                                                 AWS=mean(AWS, na.rm = TRUE), 
                                                 AWA=mean(AWA, na.rm = TRUE), 
                                                 COG=mean(VTG.COG, na.rm=TRUE), 
                                                 LAT=mean(GGA.LAT, na.rm=TRUE),
                                                 LON=mean(GGA.LON, na.rm=TRUE))

# Remove any rows with NA data
perf <- na.omit(perf)

# Remove any leftover data from 


#check for any negative COG or AWD
stopifnot(all(perf$AWA > 0))
stopifnot(all(perf$COG > 0))

#create a mirrored AWA 



### #3. Calculate True Wind Direction, Speed, and Course


#convert Apparent to True

tw <- getTrueWind(perf$AWS, perf$AWA, perf$SOG)
perf$TWS <- tw$TWS
perf$TWA <- tw$TWA


#Apparent Wind Direction
perf$AWD <- perf$COG + perf$AWA
perf$TWD <- perf$COG + perf$TWA

mirrorAngle <- function(ang){
  
  awa.mirror <- ifelse(ang > 180, 
                       360 - ang,
                       ang)
  
  return(awa.mirror)
  
}

perf$AWA.mir <- mirrorAngle(perf$AWA)
perf$TWA.mir <- mirrorAngle(perf$TWA)

# 4. Apply Wind & Speed Brackets ================================================
#break wind speed into bins: 
perf$AWS.range <- cut(perf$AWS, breaks=c(0, 6, 8, 10, 12, 16, 20, 24, 35))
perf$AWS.range <- factor(perf$AWS.range, levels=rev(levels(perf$AWS.range)))

perf$TWS.range <- cut(perf$TWS, breaks=c(0, 6, 8, 10, 12, 16, 20, 24, 35))
perf$TWS.range <- factor(perf$TWS.range, levels=rev(levels(perf$TWS.range)))

#break wind angle into bins:
perf$AWA.range <- cut(perf$AWA.mir, breaks=seq(0,180, by=30))
perf$TWA.range <- cut(perf$TWA.mir, breaks=seq(0,180, by=30))

perf <- perf[perf$AWA < 360, ]



# 5. Calculate Performance Vs. Polar ============================================

#source("../src/loadPolars.R")

perf$target.SOG <- bilinear(x= trueangle, y= c(0, truewind), z=zv, x0=perf$TWA.mir, y0=perf$TWS)$z
perf$diff.SOG <- perf$SOG - perf$target.SOG

perf$pol.perc <- (perf$SOG / perf$target.SOG) * 100

x <- is.na(perf$pol.perc)

print(sum(x))

if(sum(x)>0){
  
 perf$pol.perc[x]  <- (perf$SOG[x] / naperf$target.SOG[x]) * 100
  
  
}

# perf$target.SOG[perf$AWA < 36] <- NA
# perf$diff.SOG[perf$AWA < 36] <- NA
# perf$pol.perc[perf$AWA < 36] <- NA


#anything over 120% is an apparition...set to NA
perf$pol.perc[perf$pol.perc > 300] <- NA

#find the tacks of the race

COGdis <- dist(perf$COG, method="euclidean")
COGclust <- hclust(COGdis, method="ward.D")
legs <- cutree(COGclust, h=10000)

perf$legs <- legs


