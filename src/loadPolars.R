#load polar table

library(dplyr)
library(tidyr)


#load Polars =============================================================================
#polars <- read.csv("../polar_data/fullpolar.csv")

widths <- c("VTW"=4, "BTW"=7, "VAW"= 7 ,"BAW"=8, "V"=7, "VMG"=8, "PHI"=6, "REEF"=8, "FLAT"=8, "CL"=8, "IT"=4, "SAIL"=17)

fullpolars <- read.fwf("./data/fullpolar.txt", widths = widths, stringsAsFactors=FALSE)

rm(widths)

#Trim whitespace
fullpolars <- apply(X = fullpolars, MARGIN = 2, FUN=trimws, which="both")
fullpolars <- as.data.frame(fullpolars, col.names = fullpolars[1,], stringsAsFactors=FALSE)

#Rename Columns
names(fullpolars) <- fullpolars[1, ]
fullpolars <- fullpolars[-1,]

#convert columns
SAIL <- fullpolars %>% select(SAIL)

fullpolars <- fullpolars %>% select(-SAIL) %>% mutate_all(as.numeric) %>% cbind(SAIL)
fullpolars <- unique(fullpolars)
rm(SAIL)

#remove optimum points
#OptimumUpDown Angles:
optimumUpDown <- fullpolars[grep("OPT", fullpolars$SAIL), ]

#remove optimum points
pol <- fullpolars[-grep("OPT", fullpolars$SAIL),  ]


#get the maximum V (velocity) for each wind speed and angle
pol <- pol %>% group_by(VTW, BTW) %>% summarise(V=head(max(V),1), SAIL=head(SAIL,1))

#rejoin to the sail and heel data
optpolars <- left_join(pol, fullpolars, by=c("VTW", "BTW", "SAIL"))
optpolars$V <- optpolars$V.x
optpolars <- optpolars %>% select(-V.y, -V.x)

#Remove points of sail below 35 degrees (to make it easier for interpolation)
optpolars <- optpolars[optpolars$BTW > 35, ]

rm(pol)

trueangle = unique(optpolars$BTW)
truewind = unique(optpolars$VTW) 


#ZV = the optimum speed at a given angle & wind speed

zv <- matrix(data = optpolars$V, 
             nrow=length(unique(optpolars$BTW)), 
             ncol=length(unique(optpolars$VTW)))

addZero <- function(mat){
  mat.out <- cbind(matrix(rep(0, dim(mat)[1])), mat)
  return(mat.out)
}

zv <- addZero(zv)





