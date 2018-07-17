# clean workspace
rm(list=ls(all=TRUE))

timech1_0 <- proc.time()

# Load Libraries
library(ggplot2)
library(brew)
library(tools)
library(data.table)


# finding parameters - Trouble-shooting
# use:
# > testpar("EXAMPLE")
testpar <- function(str){
  name <- vector(mode='character')
  pars <- grep(str, names(flightdata))
  for (i in 1:nrow(as.matrix(pars))){
    name[i] <- names(flightdata)[pars[i]]  
    #print(name)
  }
  return(name)
}


# Get File List
setwd('D:/EASA/lift_off_v3_measur/FlightDB')
fileList <- list.files(path="D:/EASA/lift_off_v3_measur/FlightDB", pattern=".csv")


# Variable and Vector Initialization
#
#flight_measurements <- mat.or.vec(NROW(fileList),20) # Matrix of dimensions: [fligths x Measurements]
flight_measurements <- data.frame()
mean_phase_store <- mat.or.vec(NROW(fileList),1)


for (s in 1:NROW(fileList)) {
  
  ## LOAD Flight From List
  flightdata <- fread(fileList[s],sep=",",header=T, stringsAsFactors = F, verbose=FALSE,
                colClasses = c(ACT='character', AC_TYPE='character', ORIGIN='character', RUNWAY_TO='character',
                RUNWAY_LD='character', DESTINATION='character', DATE='Date')) 
    
  ## Check CSV File Integrity
  # - 1st 100 samples of flight phase has be average lower than 2 (criteria #1)
  #
  phase <- flightdata$FLIGHT_PHASE[which(!is.na(flightdata$FLIGHT_PHASE))]
  mean_phase_store[s] <- mean(phase[1:100])
  #
  nrows <- NROW(flightdata)
    
  ##$##
  if (mean_phase_store[s]<2){  # File Integrity
    # Produce the expected result....
    
    setwd("D:/EASA/lift_off_v3_measur/results")
    write(paste(fileList[s]), file="FLIGHTS_OK.csv", append=TRUE)
    print(paste(fileList[s], "- OK"))
    setwd('D:/EASA/lift_off_v3_measur/FlightDB')
  

    ## Flight Data Interpolation
    LG_left <- approx(seq(1:nrows), flightdata$LDG_ON_1,xout=c(1:nrows), method="linear",n=nrows)$y
    LG_nose <- approx(seq(1:nrows), flightdata$LDGN_ON,xout=c(1:nrows), method="linear",n=nrows)$y
    LG_right <- approx(seq(1:nrows), flightdata$LDG_ON_2,xout=c(1:nrows), method="linear",n=nrows)$y
    pitch <- approx(seq(1:nrows), flightdata$PITCH,xout=c(1:nrows), method="linear",n=nrows)$y
    pitch_capt <- approx(seq(1:nrows), flightdata$PITCH_CPT,xout=c(1:nrows), method="linear",n=nrows)$y
    pitch_fo <- approx(seq(1:nrows), flightdata$PITCH_FO,xout=c(1:nrows), method="linear",n=nrows)$y
    pitch_rate <- approx(seq(1:nrows), flightdata$PTCR,xout=c(1:nrows), method="linear",n=nrows)$y
    ralt1 <- approx(seq(1:nrows), flightdata$RALTD1,xout=c(1:nrows), method="linear",n=nrows)$y
    ralt2 <- approx(seq(1:nrows), flightdata$RALTD2,xout=c(1:nrows), method="linear",n=nrows)$y
    ivv <- approx(seq(1:nrows), flightdata$IVV,xout=c(1:nrows), method="linear",n=nrows)$y
    long <- approx(seq(1:nrows), flightdata$LONG,xout=c(1:nrows), method="linear",n=nrows)$y
    latg <- approx(seq(1:nrows), flightdata$LATG,xout=c(1:nrows), method="linear",n=nrows)$y
    n11 <- approx(seq(1:nrows), flightdata$N11C,xout=c(1:nrows), method="linear",n=nrows)$y
    n12 <- approx(seq(1:nrows), flightdata$N12C,xout=c(1:nrows), method="linear",n=nrows)$y
    roll <- approx(seq(1:nrows), flightdata$ROLL,xout=c(1:nrows), method="linear",n=nrows)$y
    ias <- approx(seq(1:nrows), flightdata$IASC,xout=c(1:nrows), method="linear",n=nrows)$y
    gs <- approx(seq(1:nrows), flightdata$GSC,xout=c(1:nrows), method="linear",n=nrows)$y
    
  
    vrtg <- flightdata$VRTG
    pitch_acceleration <- diff(pitch_rate)
  
  # CORE FUNCTION
  ################################################################################################
  # FIRST METHOD => LANDING GEAR SQUAT SWITCH
  # the **lo_mlg** is the main reference for lift-off
  # It is considered as the instant when Left Gear indicates Air
  # starting from this point other fine-tunning determinations
  # are established
  #
  
  tlg <- min(which(LG_left==0))
  trg <- min(which(LG_right==0))
  
  lo_mlg <- max(tlg,trg) # Lift Off according to MLG criteria
  
  
  lift_off <- lo_mlg
  lo_ralt <- lo_mlg
  method = 1
  
  
  # SECOND METHOD => FINE TUNING WITH RALT
  #  using Radio Altitude If lo_mlg is not a good determination
  # fine tune is performed if RALT[lo_mlg]>1ft
  
  r1 <- ralt1[lo_mlg]
  r2 <- ralt2[lo_mlg]
  
  if ( r1 > 1  || r2 > 1) {
    
    min <- lo_mlg - 20
    max <- lo_mlg + 10
    
    r1_positive <- min(which(ralt1[min:max]>0))
    lo_r1 <- min + r1_positive
    
    r2_positive <- min(which(ralt2[min:max]>0))
    lo_r2 <- min + r2_positive
    
    #lo_ralt <- min(lo_r1,lo_r2)
    lo_ralt <- max(lo_r1,lo_r2)
    
    lift_off <- lo_ralt
    method = 2
    
  } 
 
 
# Store Flight Measurements  
  pitch_liftoff_store <- pitch[lift_off]
#  pitch_liftoff_store[s] <- pitch[lift_off]
  ptcr_liftoff_store <- pitch_rate[lift_off]
  ptacc_liftoff_store <- pitch_acceleration[lift_off]
  vrtg_liftoff_store <- vrtg[lift_off]
  long_liftoff_store <- long[lift_off]
  latg_liftoff_store <- latg[lift_off]
  ralt1_liftoff_store <- ralt1[lift_off]
  ralt2_liftoff_store <- ralt2[lift_off]
  ias_liftoff_store <- ias[lift_off]
  gs_liftoff_store <- gs[lift_off]
  roll_liftoff_store <- roll[lift_off]
  liftoff_method <- method

  }  else {
    # Log unexpected flights... and go on to the next flight
    setwd("D:/EASA/lift_off_v3_measur/results")
    write(paste(fileList[s]), file="FLIGHTS_NOT_OK.csv", append=TRUE)
    print(paste(fileList[s], "- NOT OK"))
    setwd('D:/EASA/lift_off_v3_measur/FlightDB')
    
    # Store Flight Measurements 
    pitch_liftoff_store <- NA
    #pitch_liftoff_store[s] <- NA
    ptcr_liftoff_store <- NA
    ptacc_liftoff_store <- NA
    vrtg_liftoff_store <- NA
    long_liftoff_store <- NA
    latg_liftoff_store <- NA
    ralt1_liftoff_store <- NA
    ralt2_liftoff_store <- NA
    ias_liftoff_store <- NA
    gs_liftoff_store <- NA
    roll_liftoff_store <- NA
    liftoff_method <- NA
            
  }

# END CORE FUNCTION
################################################################################################


## Create Flight Measurements Vector
flight_measurements[s,1] <- substr(fileList[s],1,6) # A/C TAIL
flight_measurements[s,2] <- substr(fileList[s],17,22) # AGS file number
flight_measurements[s,3] <- flightdata$AC_TYPE[which(flightdata$AC_TYPE!="")][100] # A/C TYPE
flight_measurements[s,4] <- flightdata$FLTNUM[which(flightdata$FLTNUM!="NA")][100]
flight_measurements[s,5] <- flightdata$ORIGIN[which(flightdata$ORIGIN!="")][100]
flight_measurements[s,6] <- flightdata$RUNWAY_TO[which(flightdata$RUNWAY_TO!="")][100]
flight_measurements[s,7] <- flightdata$DESTINATION[which(flightdata$DESTINATION!="")][100]
flight_measurements[s,8] <- flightdata$RUNWAY_LD[which(flightdata$RUNWAY_LD!="")][100]
flight_measurements[s,9] <- flightdata$DATE[which(flightdata$DATE!="")][100]
# Time (?)
flight_measurements[s,10] <- liftoff_method
flight_measurements[s,11] <- pitch_liftoff_store
flight_measurements[s,12] <- ptcr_liftoff_store
flight_measurements[s,13] <- ptacc_liftoff_store
flight_measurements[s,14] <- vrtg_liftoff_store
flight_measurements[s,15] <- long_liftoff_store 
flight_measurements[s,16] <- latg_liftoff_store
flight_measurements[s,17] <- ralt1_liftoff_store
flight_measurements[s,18] <- ralt2_liftoff_store
flight_measurements[s,19] <- ias_liftoff_store
flight_measurements[s,20] <- gs_liftoff_store
flight_measurements[s,21] <- roll_liftoff_store


} #End FOR


names(flight_measurements) <- c("ac.tail", "ags.file","ac.type", "fltnum", "origin", "runway.to","destination", "runway.ld", 
                                "date.to", "lo.method", "pitch.lo", "ptcr.lo", "ptacc.lo", "vrtg.lo","long.lo",
                                "latg.lo", "ralt1.lo", "ralt2.lo", "ias.lo", "gs.lo", "roll.lo")





# to source the code, print has to be explicitly called
#
print(ggplot(flight_measurements, aes(x=lo.method)) + geom_histogram(binwidth=.5, colour="black", fill="green"))


print(ggplot(flight_measurements, aes(x=pitch.lo)) + geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="green") + 
  geom_density(colour="blue", size=1) + geom_vline(aes(xintercept=mean(pitch.lo, na.rm=T)), colour="red", size=1.4, linetype=2))

print(ggplot(flight_measurements, aes(x=ptcr.lo)) + geom_histogram(aes(y=..density..),binwidth=.25, colour="black", fill="green") +
        geom_density(colour="blue", size=1) + geom_vline(aes(xintercept=mean(ptcr.lo, na.rm=T)), colour="red", size=1.4, linetype=2))

##$##



print(ggplot(flight_measurements, aes(x=vrtg.lo)) + geom_histogram(binwidth=.01, colour="black", fill="green"))
print(ggplot(flight_measurements, aes(x=long.lo)) + geom_histogram(binwidth=.01, colour="black", fill="green"))
print(ggplot(flight_measurements, aes(x=latg.lo)) + geom_histogram(binwidth=.01, colour="black", fill="green"))
print(ggplot(flight_measurements, aes(x=ralt1.lo)) + geom_histogram(binwidth=.5, colour="black", fill="green"))
print(ggplot(flight_measurements, aes(x=ralt2.lo)) + geom_histogram(binwidth=.5, colour="black", fill="green"))
print(ggplot(flight_measurements, aes(x=ias.lo)) + geom_histogram(binwidth=1, colour="black", fill="green"))
print(ggplot(flight_measurements, aes(x=gs.lo)) + geom_histogram(binwidth=1, colour="black", fill="green"))
print(ggplot(flight_measurements, aes(x=roll.lo)) + geom_histogram(binwidth=.25, colour="black", fill="green"))


print(ggplot(flight_measurements, aes(x=pitch.lo)) + geom_histogram(binwidth=.5, colour="black", fill="green"))
print(ggplot(flight_measurements, aes(x=ptacc.lo)) + geom_histogram(binwidth=.05, colour="black", fill="green"))


print("-------------------------------------------------")
print(paste("Total Flights Processed: ",s))
print("-------------------------------------------------")




timech1 <- proc.time() - timech1_0
tsec <- timech1[3]
tmin <- timech1[3]/60


cat("-------------------------------------------------", "
    Processing Time [seconds]", "    ", tsec," 
    Processing Time [minutes]", "    ", tmin," 
-------------------------------------------------")




## Generate Plots NOTES
##
# Overlaping histograms - help
# http://stackoverflow.com/questions/6957549/overlaying-histograms-with-ggplot2-in-r
# http://www.r-bloggers.com/overlapping-histogram-in-r/
# http://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r
# http://www.r-bloggers.com/ggplot2-cheatsheet-for-visualizing-distributions/
# http://www.r-bloggers.com/my-commonly-done-ggplot2-graphs-part-2/
# http://r4stats.com/examples/graphics-ggplot2/
#

