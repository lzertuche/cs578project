#CS578 final project
#####preloads####

#function takes list of packages and loads them OR installs them if not present
package.loader <- function(list.of.packages){
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if (length(new.packages)) install.packages(new.packages)
  # load packages that are already installed
  lapply(list.of.packages, library, character.only = TRUE)
  }
#list of packages, feel free to add anything you use as another element in the list
package.vector <- c(
  "openxlsx", # For .xlsx extraction. 
  "data.table", "reshape2", "dplyr", "tidyr", # Data manipulation
  "e1071", # SVM and other machine learning tools
  "ggplot2", "ggRandomForests", # Graphing
  "randomForest",  # RF 
  "MASS",
  "geosphere") # Chi-squared functions)
  
package.loader(package.vector) ; rm(list = c("package.vector", "package.loader"))

####Input Data clean and merge####

dt_faa <- read.xlsx("cy14-commercial-service-enplanements.xlsx")
dt_faa <- subset(dt_faa, Rank <= 30) #keep only top 30 airports by Rank

#for distance computation
dt_coord  <- read.xlsx("USA_airport_coordinates.xlsx")
coord_idx <- dt_coord$locationID  %in%  dt_faa$Locid #gets index of top 30
dt_coord<- dt_coord[coord_idx,]  #subset dt_coord
#merge with dt_faa
dt_faa <- merge(x = dt_faa, y = dt_coord, by.x ="Locid", by.y="locationID" )

#Test function for distance
if (FALSE) {
#distance between LGA and ATL
c1 <- dt_faa[dt_faa$Locid=="LGA", c("Longitude","Latitude") ]
c2 <- dt_faa[dt_faa$Locid=="ATL", c("Longitude","Latitude") ]
dist <- distCosine(c1, c2, r=6378.137) #earth raidus in KM, assumes earth is spherical
# 1226.24km,  google maps distance = 1,224.52km 
c1 <- dt_faa[dt_faa$Locid=="LGA", c("Longitude","Latitude") ]
c2 <- dt_faa[dt_faa$Locid=="ORD", c("Longitude","Latitude") ]
dist <- distCosine(c1, c2, r=6378.137) #earth raidus in KM, assumes earth is spherical
# 1178.544  google maps distance = 1,177.27km
}

####Exploratory Analysis####

####Modeling####