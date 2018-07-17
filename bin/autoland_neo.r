######################################################################
# Autoland Machine Learning Classification
# - Kneans method
# 
# 2do:
# * Flights Listing -> ELDV -> Export all autoland flights
#
#


# Clean workspace (& environment data)
rm(list=ls(all=TRUE))

library(ggplot2)
library(brew)
library(tools)
library(data.table)
library(psych)
library(lubridate)
library(xtable)
#library(stargazer)


# Input Flight Data
#setwd("C:/Users/210906/Dropbox/AUTOLAND/FlightDB")
setwd("K:/Ground_Station/VOOS_AGS/AUTOLAND/HISTORICO")
flightdata <- read.table("AUTOLAND_320RAW_HISTORIC.CSV",sep=",",header=F, fill=T)
setwd("D:/AUTOLAND/FlightDB")
cnames <- readLines("head10344.csv", 1)
cnames <- strsplit(cnames, ",", fixed = TRUE)
names(flightdata) <- make.names(cnames[[1]])
rm(cnames)
setwd("D:/AUTOLAND/bin")


### CLEANING
# keep only unique observations - no vector repetitions
flightdata <- unique(flightdata)
# convert dates
#flightdata$DATE_TO <- as.POSIXct(flightdata$DATE_TO, format='%d/%m/%y')
flightdata$DATE_TO <- as.Date(flightdata$DATE_TO,format='%d/%m/%y')
flightdata$LAND_DIST <- as.numeric(flightdata$LAND_DIST)
# clean flightdata
remove_observations <- which(is.na(flightdata$DATE_TO))
flightdata <- flightdata[-remove_observations,]
# old_dates removal
old_dates <- which(year(flightdata$DATE_TO)<2008)
# protection for *void* old_dates
if (length(old_dates)!=0){
  flightdata <- flightdata[-old_dates,]
}

# clean non-existing File.No's (old format), flag = 999999
#index2flag <- which(is.na(flightdata$FILE_No))
index2flag <- which(as.numeric(flightdata$FILE_No)==1)
flightdata$FILE_No[index2flag] <- 999999


####
min_date <- min(flightdata$DATE_TO)
chmin_date <- as.character(min_date)
max_date <- max(flightdata$DATE_TO)
chmax_date <- as.character(max_date)
#
nrows <- dim(flightdata)[1]
#
a319 <- sum(flightdata$AC_TYPE==319)
a320 <- sum(flightdata$AC_TYPE==320)
a321 <- sum(flightdata$AC_TYPE==321)


# removing Dates Flightnum and Factors -> not suitable to cluster data
remove_index <- as.integer(c(1,3,4,5,6,7,8,20,49,78,107,136,165,260,279,280))
# parametros 194 e 223 alterm completamente a análise,ie, desaparecem atipicos e outliers!!?????
# foram retirados da lista acima
# 194 = ALT_TH
# 223 = ALT_TD
#remove_index <- c(remove_index,grep("ALT",names(flightdata)))
#grep("ALT",names(flightdata)) 
flightdata2 <- flightdata[,-remove_index]

##$##
dest1 <- data.frame(table(flightdata$DESTINATION))
names(dest1) <- c("Airport", "Frequency" )
dest1 <- print(xtable(dest1, format="latex"), include.rownames=FALSE, size="tiny")
#outliers2print <- print(xtable(outliers2print, format="latex"), include.rownames=FALSE, size="tiny") 

setwd("D:/AUTOLAND/bin/figures")
png("auto_byairp.png", width=1200, height=600)
barplot( table(flightdata$DESTINATION), main="Autoland Flights Airport Distribution", 
         cex.names=1.4, cex.axis=1.4, col="blue", cex.main=2.2, las=2)
grid(,,col="dark red") 
dev.off()
# reset bin directory
setwd("D:/AUTOLAND/bin")

##################
## KMEANS Criteria
#
# http://www.r-statistics.com/2013/08/k-means-clustering-from-r-in-action/
#
set.seed(1234)
kmeansobj <- kmeans(as.matrix(flightdata2), centers=3)

cluster1 <- sum(kmeansobj$cluster==1)
cluster2 <- sum(kmeansobj$cluster==2)
cluster3 <- sum(kmeansobj$cluster==3)

clusters <- matrix(3,1, data=c(cluster1,cluster2,cluster3))
max_cluster<- max(clusters)

flightdata$kmean <- kmeansobj$cluster
label <- vector("numeric", length=nrows)
to_label_one <- which(clusters[,1]==max_cluster)

if (to_label_one==1){
  label[(which(flightdata$kmean==1))] <- 1
} else {
  label[(which(flightdata$kmean==1))] <- -1
}

if (to_label_one==2){
  label[(which(flightdata$kmean==2))] <- 1
} else {
  label[(which(flightdata$kmean==2))] <- -1
}

if (to_label_one==3){
  label[(which(flightdata$kmean==3))] <- 1
} else {
  label[(which(flightdata$kmean==3))] <- -1
}

flightdata <- cbind(flightdata, label)
flightdata2 <- cbind(flightdata2, label) # without qualitative features

atypfd2 <- flightdata2[flightdata2$label==-1,]
atypfd <- flightdata[flightdata$label==-1,]
typfd2  <- flightdata2[flightdata2$label==1,]

descfd2 <- describe(typfd2) # typical flights define the model (mean, std)

dimensions <- dim(flightdata2)[2]
nrows_atyp <- NROW(atypfd2)
nrows_typ <- NROW(typfd2)

setwd("D:/AUTOLAND/bin/figures")
png("bar_clusters.png", width=1200, height=600)
barplot(table(kmeansobj$cluster), main="KMeans cluster distribution", 
        cex.names = 2, cex.axis = 2, cex.main=2.5)
dev.off()
# reset bin directory
setwd("D:/AUTOLAND/bin")


# reporting outliers cases
count <- 0
sig <- 6  # standard deviations to consider
outliers1 <-data.frame()
for (r in 1:nrows_atyp){
  for (dim in 2:(dimensions-19)){  
    if ( (atypfd2[r,dim] >= (descfd2[dim,3] + sig*descfd2[dim,4])) | (atypfd2[r,dim] <= (descfd2[dim,3] - sig*descfd2[dim,4]))){
      count <- count + 1
      # count and line detected
      outliers1[count, 1] <- count  # count
      outliers1[count, 2] <- r      # line
      # Flight ID
      outliers1[count, 3] <- atypfd[r,1]  #AC_TAIL
      outliers1[count, 4] <- atypfd[r,2]  #AC_TYPE
      outliers1[count, 5] <- atypfd[r,3]  #FLTNUM
      outliers1[count, 6] <- atypfd[r,4]  #ORIGIN
      outliers1[count, 7] <- atypfd[r,6]  #DESTINATION
      outliers1[count, 8] <- atypfd[r,7]  #RUNWAY_LD
      outliers1[count, 9] <- as.character(atypfd[r,8])  #DATE_TO->converted to character
      # Value detected
      outliers1[count, 10] <- names(atypfd2)[dim] #parameter
      outliers1[count, 11] <- atypfd2[r,dim] #param_value
      outliers1[count, 12] <- round(descfd2[dim,3],2) #mean
      outliers1[count, 13] <- round(descfd2[dim,4],2) #sd
      outliers1[count, 14] <- round((descfd2[dim,3] - sig*descfd2[dim,4]),2) #mean-Nsigma
      outliers1[count, 15] <- round((descfd2[dim,3] + sig*descfd2[dim,4]),2) #mean+Nsigma
    }
  }  
}

names(outliers1) <- c("count", "line.atypfd", "ac.tail", "ac.type", "flight.num", "origin","destination","land.rwy", "takeoff.date","parameter", "par.value","par.mean", "par.sd", "par.min", "par.max")

n.outliers1 <- unique(outliers1$line.atypfd)
n.outliers1 <- length(n.outliers1)

# CLEANING UNCONSISTENT CRITERIA
# On Dataframe 10344 ALT found not to be a good criteria due to the 
# different airport altitudes
# (consider to use Height or RALT instead)
alt_criteria <- grep("^ALT", outliers1$parameter)
outliers2 <- outliers1[-alt_criteria,]

n.outliers2 <- dim(outliers2)[1]

nflight.outliers2 <- unique(outliers2$line.atypfd) 
nflight.outliers2 <- length(nflight.outliers2) # number of flights!

outliers2print <- cbind(outliers2[,3:15])
names(outliers2print) <- c("AC Tail","AC Type","FLTNUM","ORIG","DEST", "LAND_RWY","TO.DATE", "PARAM","PAR.VALUE","PAR.MEAN", "PAR.SD","MIN6SDs", "PLUS6SDs" )

outliers2print <- print(xtable(outliers2print, format="latex"), include.rownames=FALSE, size="tiny") 
#the output of xtable is meant to be printed!!!


flights.outliers <- unique(outliers1$line.atypfd)
#find the real atypical flights
atypfd <- atypfd[-flights.outliers,]
atypfd2 <- atypfd2[-flights.outliers,]

nrows_atyp <- NROW(atypfd2)
#nrows_typ <- NROW(typfd2)


# reporting atypical cases
count <- 0
sig1 <- 3  # standard deviations to consider
sig2 <- 6
positive_atypical <-data.frame()
for (r in 1:nrows_atyp){
  for (dim in 2:(dimensions-18)){  
    if ( ((atypfd2[r,dim] >= (descfd2[dim,3] + sig1*descfd2[dim,4])) & (atypfd2[r,dim] <= (descfd2[dim,3] + sig2*descfd2[dim,4])) ) | ( (atypfd2[r,dim] <= (descfd2[dim,3] - sig1*descfd2[dim,4]))  & (atypfd2[r,dim] >= (descfd2[dim,3] - sig2*descfd2[dim,4])) ) ){
      count <- count + 1
      # count and line detected
      positive_atypical[count, 1] <- count  # count
      positive_atypical[count, 2] <- r      # line
      # Flight ID
      positive_atypical[count, 3] <- atypfd[r,1]  #AC_TAIL
      positive_atypical[count, 4] <- atypfd[r,2]  #AC_TYPE
      positive_atypical[count, 5] <- atypfd[r,3]  #FLTNUM
      positive_atypical[count, 6] <- atypfd[r,4]  #ORIGIN
      positive_atypical[count, 7] <- atypfd[r,6]  #DESTINATION
      positive_atypical[count, 8] <- atypfd[r,7]  #RUNWAY_LD
      positive_atypical[count, 9] <- as.character(atypfd[r,8])  #DATE_TO->converted to character
      # Value detected
      positive_atypical[count, 10] <- names(atypfd2)[dim] #parameter
      positive_atypical[count, 11] <- atypfd2[r,dim] #param_value
      positive_atypical[count, 12] <- round(descfd2[dim,3],2) #mean
      positive_atypical[count, 13] <- round(descfd2[dim,4],2) #sd
      positive_atypical[count, 14] <- round((descfd2[dim,3] - sig1*descfd2[dim,4]),2) #mean-Nsigma
      positive_atypical[count, 15] <- round((descfd2[dim,3] + sig1*descfd2[dim,4]),2) #mean+Nsigma
    }
  }  
}

names(positive_atypical) <- c("count", "line.atypfd", "ac.tail", "ac.type", "flight.num", "origin","destination","land.rwy", "takeoff.date","parameter", "par.value","par.mean", "par.sd", "par.min", "par.max")

n.positive_atypical <- unique(positive_atypical$line.atypfd)
n.positive_atypical <- length(n.positive_atypical)

# ALT found not to be a good criteria due to the different airport altitudes
# Use Height or RALT instead
alt_criteria <- grep("^ALT", positive_atypical$parameter)
posatyp2 <- positive_atypical[-alt_criteria,]


n.posatyp2 <- dim(posatyp2)[1]

nflights.posatyp2 <- unique(posatyp2$line.atypfd)
nflights.posatyp2 <- length(nflights.posatyp2)


posatyp2print <- cbind(posatyp2[,3:15])
names(posatyp2print) <- c("AC Tail","AC Type","FLTNUM","ORIG","DEST", "LAND_RWY","TO.DATE", "PARAM","PAR.VALUE","PAR.MEAN", "PAR.SD","MIN3SDs", "PLUS3SDs" )

posatyp2print1 <- print(xtable(posatyp2print[1:100,], format="latex"), include.rownames=FALSE, size="tiny") 
posatyp2print2 <- print(xtable(posatyp2print[101:n.posatyp2,], format="latex"), include.rownames=FALSE, size="tiny") 

#the output of xtable is meant to be printed!!!


setwd("D:/AUTOLAND/bin/figures")
png("positive_atyp.png", width=1200, height=600)
barplot( table(posatyp2$destination), main="Airport Distribution - # Atypical Cases", 
         cex.names=1.2, cex.axis=1.2, col="blue", cex.main=2, las=2 )
grid(,,col="dark red") 
dev.off()


png("atypbytail.png", width=1200, height=600)
barplot( table(posatyp2$ac.tail), main="Tail Number - # Atypical Cases", 
         cex.names=1, cex.axis=1.2, col="blue", cex.main=2, las=2 )
grid(,,col="dark red") 
dev.off()

# reset bin directory
setwd("D:/AUTOLAND/bin")


  
  # PDF Reporting
  #setwd("C:/Users/210906/Dropbox/R/test_brew_dataframe")
  brew("autoland_neo.brew", "autoland_neo.tex")
  texi2dvi("autoland_neo.tex", pdf = TRUE)


# Post Analysis Testing
# GVA <- subset(positive_atypical,positive_atypical$destination=="GVA" )
# head(GVA)
#
# grep("ALT",names(flightdata))


