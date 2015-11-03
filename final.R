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
  "MASS", # for chi-squared functions)
  "geosphere",
  "R.matlab",
  "stringdist") 
  
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
dist <- distCosine(c1, c2)/1000 #earth raidus in KM, assumes earth is spherical
# 1178.544  google maps distance = 1,177.27km
}

#create a function that takes 3 LETTER airport ids and returns distanc
city.dist <- function(id1,id2){
  c1 <- dt_faa[dt_faa$Locid==id1, c("Longitude","Latitude") ]
  c2 <- dt_faa[dt_faa$Locid==id2, c("Longitude","Latitude") ]
  return (distCosine(c1, c2, r=6378.137))
}
  
#read population
population_dt =  read.csv("./populationdata.csv")
#add 3 letter airport identifier to population data
#CHECK one airport missing
for (i in 1:nrow(population_dt)) {
  idx <- which.min(stringdist(population_dt$City[i],dt_faa$City)) #find city based on str similarity
  population_dt$faa_city[i]=dt_faa$City[idx]
  population_dt$Locid[i]=dt_faa$Locid[idx]
}

#read income and category data data
income_dt <- read.xlsx("Income.xlsx")
categ_dt <-  read.xlsx("Income.xlsx", sheet=2)

#add the numeric ID for routes to airport data set
airport_id <- read.csv("./BTS Data/airportsList.csv")
dt_faa <- merge(x = dt_faa, y = airport_id, by.x ="Locid", by.y="Airport_Code" )


##create a route based data set (instead of airport based)
#dt_mat = readMat("./BTS Data/matlab.mat") NOT USED 

#create combination of airports for routes
route_matrix <- combn(dt_faa$Locid,2)

dt_routes <- data.frame(dummy=matrix(NA, nrow = ncol(route_matrix)))
dt_routes$origin <- route_matrix[1,] #set origin
dt_routes$dest <- route_matrix[2,] #set destiny
dt_routes$dummy <- NULL #get rid of dummy var
dt_routes$distcalc <- mapply(city.dist, dt_routes$origin, dt_routes$dest )  #add distance
#bring route ids
for (i in 1:nrow(dt_routes)) {
  dt_routes$origID[i] <- dt_faa[dt_faa$Locid==dt_routes$origin[i], "Airport_ID"]}
for (i in 1:nrow(dt_routes)) {
  dt_routes$destID[i] <- dt_faa[dt_faa$Locid==dt_routes$dest[i], "Airport_ID"]}
#save original length
single_year_length=nrow(dt_routes)
#repeat data framee 11 times for 11 years, 2004-2014
dt_routes <- do.call("rbind", replicate(n=11, dt_routes, simplify = FALSE)) 

#loop over years to construct all 11 year data set, 
k=0 #extra counter
for (i in 2004:2014) {
  csvloc <- paste(c("./BTS Data/finalData", as.character(i),".csv"), collapse="")
  oneyrdata <- read.csv(csvloc)
  for (j in 1:single_year_length) {
    #two possible route ids
    routeIDs <- c(paste(c(dt_routes$origID[k+j], dt_routes$destID[k+j]), collapse=""),
                 (paste(c(dt_routes$destID[k+j], dt_routes$origID[k+j]), collapse="")))
    idx = which(oneyrdata$possRoutes==routeIDs[1])
    if (idx==0) {  idx = which(oneyrdata$possRoutes==routeIDs[2])}
    dt_routes$year[k+j] <- i
    dt_routes$dist_mi[k+j] <- oneyrdata[idx,"DISTANCE"]
    dt_routes$dcost[k+j] <- oneyrdata[idx,"DIRECT_COST"]
    dt_routes$segpax[k+j] <- oneyrdata[idx,"SEG_PAX"]
    dt_routes$mktpax[k+j] <- oneyrdata[idx,"MKT_PAX"]
  
  }
  k=k+j
}

#add population

#add income 








####Exploratory Analysis####

####Modeling####

#####Results and Graphics####