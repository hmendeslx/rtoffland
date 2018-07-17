######################################################################
# Take-Off and Landing-> One Case Study
# This script produces:
# - TO Report
# - LND Report
# for one selected flight. The reports can be enable one or both for
# the flight by setting the variables
#  take_off_study = <0,1>
#  landing_study = <0,1>
#
#


# Clean workspace (& environment data)
rm(list=ls(all=TRUE))

library(ggplot2)
library(grid)
library(brew)
library(tools)
library(data.table)
library(psych)
library(lubridate)
library(xtable)
library(data.table)
library(psych)

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


# derivative of a vector
derivative <- function(vector,time_interval) {
  #f_i1 is the parameter value at instant i
  #f_i2 is the parameter value at instant i+1
  #time_interval is the total row number of the parameter for the derivative
  f_derivative=mat.or.vec(NROW(time_interval),1)
  f_i1=0
  f_i2=0
  instant=seq(from=0,to=time_interval,by=1)
  
  for (k in 1:(time_interval-1)) {
    f_i1[k]=vector[k]
    f_i2[k]=vector[k+1]
    f_derivative[k]=(f_i2[k]-f_i1[k])/((instant[k+1])-instant[k])
  }
  
  f_derivative[time_interval]=f_i2[k]
  
  return(f_derivative)
}


to_plots <- function(){

  # Take-Off Start and End Points determination
  t0 <- min(which(flightdata$FM_FWC==3)) - 200
  t1 <- max(which(flightdata$FM_FWC==4)) + 50
  
  ptcr_dot <-derivative(ptcr[t0:t1],NROW(ptcr[t0:t1]))
  gs_ms <- gs[t0:t1]*c_knot_ms
  gs_kmh <- gs_ms*c_ms_kmh
  gs_dot <-derivative(gs_ms,NROW(gs[t0:t1]))
  gs_dot[NROW(gs_dot)] <- 0
    
  # data colected and calculated for take-off analysis
  data_takeoff <- data.frame()
  
  #time <- vector()
  time <- c(0,seq(t1-t0)/8)

  # filtering gs_dot
  gsdot.loess <- loess(y ~x, span=0.1, data.frame(x=time, y=gs_dot))
  gsdot.filtered <- predict(gsdot.loess, data.frame(x=time))
  
  long_ms2 <- long[t0:t1]*9.8
  
  ##$##
  data_takeoff <- as.data.frame(cbind(time, gs[t0:t1],raltd1[t0:t1],
                      n11[t0:t1], n21[t0:t1],n12[t0:t1], n22[t0:t1],
                      ff1[t0:t1], ff2[t0:t1], raltd2[t0:t1],
                      egt1[t0:t1], egt2[t0:t1], long[t0:t1], ptcr_dot,
                      pitch_cpt[t0:t1],pitch_fo[t0:t1],pitch[t0:t1],
                      ptcr[t0:t1], gs_ms, gs_dot, gsdot.filtered, long_ms2,
                      flx1_temp[t0:t1], flx2_temp[t0:t1],sat[t0:t1],
                      tat[t0:t1], tla1c[t0:t1], tla2c[t0:t1],
                      q1[t0:t1], q2[t0:t1], pt1[t0:t1], pt2[t0:t1],
                      p0_1[t0:t1], p0_2[t0:t1], gw1kg[t0:t1],
                      p2_1[t0:t1], p2_2[t0:t1],p31[t0:t1], p32[t0:t1],
                      t12_1[t0:t1], t12_2[t0:t1],t25_1[t0:t1], t25_2[t0:t1],
                      head_mag[t0:t1], fm_fwc[t0:t1], alt_std[t0:t1]))
  
  names(data_takeoff) <- c("time", "GS", "RALTD1", "N11", "N21", "N12", "N22",
                           "FF1", "FF2", "RALTD2", "EGT1", "EGT2", "LONG",
                           "PTCR_DOT","PITCH_CPT","PITCH_FO", "PITCH",
                           "PTCR", "GS_MS", "GS_DOT", "GSDOT_FILT", "LONG_MS2",
                           "FLX1_TEMP", "FLX2_TEMP", "SAT", "TAT", "TLA1", "TLA2",
                           "Q1", "Q2", "PT1", "PT2", "P0_1", "P0_2", "GW1KG",
                           "P2_1", "P2_2", "P31", "P32", "T12_1", "T12_2",
                           "T25_1", "T25_2", "HDG", "FM_FWC", "ALT_STD")
  
  # Parameter Description
  par.names <- as.vector(names(data_takeoff))
  par.origin <- c("calcul", "aircraft", "aircraft","aircraft","aircraft",
                "aircraft","aircraft","aircraft","aircraft","aircraft",
                "aircraft","aircraft","aircraft","calcul","aircraft",
                "aircraft","aircraft","aircraft","calcul","calcul","calcul","calcul",
                "aircraft","aircraft","aircraft", "aircraft","aircraft", "aircraft",
                "aircraft","aircraft", "aircraft","aircraft", "aircraft",
                "aircraft", "aircraft","aircraft", "aircraft","aircraft", "aircraft",
                "aircraft", "aircraft","aircraft", "aircraft","aircraft","aircraft",
                "aircraft")
  
  par.descr <- c("time [second]", 
                 "Ground Speed [knot]", 
                 "Radio Altitude System 1 [ft]", 
                 "N1 Rotation Engine 1  [percentage]", 
                 "N2 Rotation Engine 1  [percentage]", 
                 "N1 Rotation Engine 2  [percentage]", 
                 "N2 Rotation Engine 2  [percentage]2",
                 "Fuel Flow Engine 1 [Kg/h]", 
                 "Fuel Flow Engine 2 [Kg/h]", 
                 "Radio Altitude System 2 [ft]", 
                 "Exahust Gas Temperature Engine 1 [degC]", 
                 "Exahust Gas Temperature Engine 2 [degC]", 
                 "Longitudinal Acceleration [g]",
                 "Pitch Acceleration [deg/sec^2}",
                 "Pitch Command Captain [deg]",
                 "Pitch Command First Officer [deg]", 
                 "Pitch Angle [deg]",
                 "Pitch Rate [deg/sec]", 
                 "Ground Speed SI Units [m/sec]", 
                 "Acceleration derived from Ground Speed [m/sec^2]", 
                 "Filtered acceleration from Ground Speed [m/sec^2]", 
                 "Longitudinal Acceleration SI units [m/sec^2]",
                 "Flex temperature System 1",
                 "Flex temperature System 2",
                 "Static Air Temperature",
                 "Total Air Temperature",
                 " Thrust Lever Angle Eng 1",
                 " Thrust Lever Angle Eng 2",
                 "Dinamic Pressure Sys 1 [mBar]",
                 "Dinamic Pressure Sys 2 [mBar]",
                 "Total Pressure Sys 1 [mBar]",
                 "Total Pressure Sys 2 [mBar]",
                 "Selected P0 Eng1 (Amb Press) [psia]",
                 "Selected P0 Eng2 (Amb Press) [psia]",
                 "Gross Weight [Kg]",
                 "PT2 Fan Inlet Press Eng 1 [psia]",
                 "PT2 Fan Inlet Press Eng 2 [psia]",
                 "PS3 HPC Exit Press Eng 1 [psia]",
                 "PS3 HPC Exit Press Eng 2 [psia]",
                 "Fan Inlet Temp Eng 1 [degC]",
                 "Fan Inlet Temp Eng 2 [degC]",
                 "LPC Exit Temp Eng 1 [degC]",
                 "LPC Exit Temp Eng 1 [degC]",
                 "Magnetic Heading [deg]",
                 "Flight Phase ( From FWC)",
                 "Altitude Standard [ft]")
  
  par.print <- as.data.frame(cbind(par.names, par.descr, par.origin))
  names(par.print) <- c("Menemonic", "Description", "Origin")
  
  par.table <- print(xtable(par.print, format="latex"), include.rownames=FALSE, 
               size="scriptsize") 
           
  # Lift-Off detection
  tlg <- min(which(LG_left[t0:t1]==0))
  trg <- min(which(LG_right[t0:t1]==0))
  
  loff <- max(tlg,trg) # Lift Off according to MLG criteria
  
  r1 <- raltd1[t0:t1]
  r2 <- raltd2[t0:t1]
  
  # loff radio alt correction
  if ( r1[loff] > 1  || r2[loff] > 1) {
    
    min <- loff - 20
    max <- loff + 10
    
    r1_positive <- min(which(r1[min:max]>0))
    lo_r1 <- min + r1_positive
    
    r2_positive <- min(which(r2[min:max]>0))
    lo_r2 <- min + r2_positive
    
    #lo_ralt <- min(lo_r1,lo_r2)
    loff <- max(lo_r1,lo_r2)    
}
  
  loff_sec <- loff/8 # in seconds

  # Rotation point
  rot <- min(which(ptcr[t0:t1]>1))
  rotation_time <- (loff - rot)/8 
  rot <- rot/8 # seconds

  # Values to include in the report 
  # integrar mais tarde em "flight_measurements"
  flxtemp1 <- round(data_takeoff$FLX1_TEMP[(t1-t0)/2],1)
  flxtemp2 <- round(data_takeoff$FLX2_TEMP[(t1-t0)/2],1)
  tla1_pos <- data_takeoff$TLA1[2*(t1-t0)/3]
  tla2_pos <- data_takeoff$TLA2[2*(t1-t0)/3]
  gw_ini <- data_takeoff$GW1KG[1]
  gw_final <- data_takeoff$GW1KG[(t1-t0)+1]
  fuel_gw <- gw_ini - gw_final
  # calculo de consumo através do FF - comparar com o de cima

  # Graphics ##$##
  # Fast Testing Command:
  # > with(data_takeoff, plot(FF1, N21, type="l", col="blue")); grid(,,col="dark red")
  setwd(figurepath)


  png("to_t25.png", width=960)
  p1 <- qplot(time, T25_1, data=data_takeoff,  col=I("blue"),
            xlab="Time [seconds]",geom=c("line"), size=I(1)) +
            geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  #dev.off()

  #png("to_gsms.png")
  p2 <- qplot(time, T25_2, data=data_takeoff,  col=I("blue"),
            xlab="Time [seconds]", geom=c("line"), size=I(1)) +
            geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  arrange(p1,p2)
  dev.off()


  png("to_t12.png", width=960)
  p1 <- qplot(time, T12_1, data=data_takeoff,  col=I("blue"),
            xlab="Time [seconds]",geom=c("line"), size=I(1)) +
            geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  #dev.off()

  #png("to_gsms.png")
  p2 <- qplot(time, T12_2, data=data_takeoff,  col=I("blue"),
            xlab="Time [seconds]", geom=c("line"), size=I(1)) +
            geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  arrange(p1,p2)
  dev.off()


  png("to_p3.png", width=960)
  p1 <- qplot(time, P31, data=data_takeoff,  col=I("blue"),
            xlab="Time [seconds]",geom=c("line"), size=I(1)) +
            geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  #dev.off()

  #png("to_gsms.png")
  p2 <- qplot(time, P32, data=data_takeoff,  col=I("blue"),
            xlab="Time [seconds]", geom=c("line"), size=I(1)) +
            geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  arrange(p1,p2)
  dev.off()

  png("to_p2.png", width=960)
  p1 <- qplot(time, P2_1, data=data_takeoff,  col=I("blue"),
            xlab="Time [seconds]",geom=c("line"), size=I(1)) +
            geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  #dev.off()

  #png("to_gsms.png")
  p2 <- qplot(time, P2_2, data=data_takeoff,  col=I("blue"),
            xlab="Time [seconds]", geom=c("line"), size=I(1)) +
            geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  arrange(p1,p2)
  dev.off()


  png("to_gw.png")
  p <- qplot(time, GW1KG, data=data_takeoff,  col=I("blue"),
          xlab="Time [seconds]", geom=c("line"), size=I(1)) + 
          geom_vline(xintercept=loff_sec, color="dark red", size=1) +
          geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()

  png("to_hdg.png")
  p <- qplot(time, HDG, data=data_takeoff,  col=I("blue"),
          xlab="Time [seconds]", geom=c("line"), size=I(1)) + 
          geom_vline(xintercept=loff_sec, color="dark red", size=1) +
          geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()

  png("to_phase.png")
  p <- qplot(time, FM_FWC, data=data_takeoff,  col=I("blue"),
          xlab="Time [seconds]", geom=c("line"), size=I(1)) + 
          geom_vline(xintercept=loff_sec, color="dark red", size=1) +
          geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()


  png("to_p0.png", width=960)
  p1 <- qplot(time, P0_1, data=data_takeoff,  col=I("blue"),
            xlab="Time [seconds]", ylab="P01"  ,geom=c("line"), size=I(1)) +
            geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  #dev.off()

  #png("to_gsms.png")
  p2 <- qplot(time, P0_2, data=data_takeoff,  col=I("blue"),
            xlab="Time [seconds]", ylab="P02", geom=c("line"), size=I(1)) +
            geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  arrange(p1,p2)
  dev.off()


  png("to_pt.png", width=960)
  p1 <- qplot(time, PT1, data=data_takeoff,  col=I("blue"),
            xlab="Time [seconds]",  ylab="Total Press Sys 1" ,geom=c("line"), size=I(1)) +
            geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  #dev.off()

  #png("to_gsms.png")
  p2 <- qplot(time, PT2, data=data_takeoff,  col=I("blue"),
            xlab="Time [seconds]",  ylab="Total Press Sys 2" ,geom=c("line"), size=I(1)) +
            geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  arrange(p1,p2)
  dev.off()


  png("to_q.png", width=960)
  p1 <- qplot(time, Q1, data=data_takeoff,  col=I("blue"),
            xlab="Time [seconds]",  ylab="Dinamic Press Sys 1" ,geom=c("line"), size=I(1)) +
            geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  #dev.off()

  #png("to_gsms.png")
  p2 <- qplot(time, Q2, data=data_takeoff,  col=I("blue"),
            xlab="Time [seconds]",  ylab="Dinamic Press Sys 2" ,geom=c("line"), size=I(1)) +
            geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  arrange(p1,p2)
  dev.off()


  png("to_tla.png", width=960)
  p1 <- qplot(time, TLA1, data=data_takeoff,  col=I("blue"),
            xlab="Time [seconds]",  ylab="Thrust Lever Eng 1" ,geom=c("line"), size=I(1)) +
            geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  #dev.off()

  #png("to_gsms.png")
  p2 <- qplot(time, TLA2, data=data_takeoff,  col=I("blue"),
            xlab="Time [seconds]",  ylab="Thrust Lever Eng 2" ,geom=c("line"), size=I(1)) +
            geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  arrange(p1,p2)
  dev.off()


  png("to_temp.png", width=960)
  p1 <- qplot(time, SAT, data=data_takeoff,  col=I("blue"),
            xlab="Time [seconds]",  ylab="Static Air Temperature [degC]" ,geom=c("line"), size=I(1)) +
            geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  #dev.off()

  #png("to_gsms.png")
  p2 <- qplot(time, TAT, data=data_takeoff,  col=I("blue"),
            xlab="Time [seconds]",  ylab="Total Air Temperature [degC]" ,geom=c("line"), size=I(1)) +
            geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  arrange(p1,p2)
  dev.off()


  png("to_flxtemp.png", width=960)
  p1 <- qplot(time, FLX1_TEMP, data=data_takeoff,  col=I("blue"),
            xlab="Time [seconds]",  ylab="Flex Temperature [degC]" ,geom=c("line"), size=I(1)) +
            geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  #dev.off()

  #png("to_gsms.png")
  p2 <- qplot(time, FLX2_TEMP, data=data_takeoff,  col=I("blue"),
            xlab="Time [seconds]",  ylab="Flex Temperature [degC]" ,geom=c("line"), size=I(1)) +
            geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  arrange(p1,p2)
  dev.off()


  png("to_gs.png", width=960)
  p1 <- qplot(time, GS, data=data_takeoff,  col=I("blue"),
             xlab="Time [seconds]",  ylab="Ground Speed [Knot]" ,geom=c("line"), size=I(1)) +
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
             geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  #dev.off()
  
  #png("to_gsms.png")
  p2 <- qplot(time, GS_MS, data=data_takeoff,  col=I("blue"),
           xlab="Time [seconds]",  ylab="Ground Speed [m/s]" ,geom=c("line"), size=I(1)) +
           geom_vline(xintercept=loff_sec, color="dark red", size=1) +
           geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  arrange(p1,p2)
  dev.off()

  png("to_raltd1.png", width=960)
  p1 <- qplot(time, RALTD1, data=data_takeoff,  col=I("blue"),
             xlab="Time [seconds]", geom=c("line"), size=I(1)) +
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
             geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  #dev.off()

  #png("to_raltd2.png")
  p2 <- qplot(time, RALTD2, data=data_takeoff, col=I("blue"),
           xlab="Time [seconds]", geom=c("line"), size=I(1)) +
           geom_vline(xintercept=loff_sec, color="dark red", size=1) +
           geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  arrange(p1,p2)
  dev.off()

  png("to_n11.png", width=960)
  p1 <- qplot(time, N11, data=data_takeoff,  col=I("blue"),
             xlab="Time [seconds]", geom=c("line"), size=I(1)) + 
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
             geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  #dev.off()

  #png("to_n12.png")
  p2 <- qplot(time, N12, data=data_takeoff, col=I("blue"),
           xlab="Time [seconds]", geom=c("line"), size =I(1)) +
           geom_vline(xintercept=loff_sec, color="dark red", size=1) +
           geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  arrange(p1,p2)
  dev.off()


  png("to_n21.png", width=960)
  p1 <- qplot(time, N21, data=data_takeoff, col=I("blue"),
             xlab="Time [seconds]", geom=c("line"), size=I(1)) +
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
             geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  #dev.off()
  
  #png("to_n22.png")
  p2 <- qplot(time, N22, data=data_takeoff, col=I("blue"),
             xlab="Time [seconds]", geom=c("line"), size=I(1)) +
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
             geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  arrange(p1,p2)
  dev.off()
  

  png("to_ff1.png", width=960)
  p1 <- qplot(time, FF1, data=data_takeoff, col=I("blue"),
             xlab="Time [seconds]", geom=c("line"), size=I(1)) +
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
             geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  #dev.off()
  
  #png("to_ff2.png")
  p2 <- qplot(time, FF2, data=data_takeoff, col=I("blue"),
             xlab="Time [seconds]", geom=c("line"), size=I(1)) + 
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
             geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  arrange(p1,p2)
  dev.off()
  
  
  png("to_egt1.png", width=960)
  p1 <- qplot(time, EGT1, data=data_takeoff, col=I("blue"),
             xlab="Time [seconds]", geom=c("line"), size=I(1)) + 
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
             geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  #dev.off()
  
  #png("to_egt2.png")
  p2 <- qplot(time, EGT2, data=data_takeoff, col=I("blue"),
             xlab="Time [seconds]", geom=c("line"), size=I(1)) +
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  arrange(p1,p2)
  dev.off()
  
  
  png("to_long.png", width=960)
  p1 <- qplot(time, LONG, data=data_takeoff,  col=I("blue"),
             xlab="Time [seconds]", ylab="LONG [g]", ylim=c(-0.05,0.35) ,geom=c("line"), size=I(1)) +
             geom_vline(xintercept=loff_sec, color="dark red", size=1) + 
             geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  #dev.off()

  #png("to_gsdot.png")
  p2 <- qplot(time, GSDOT_FILT, data=data_takeoff,  col=I("blue"),
           xlab="Time [seconds]", ylim=c(-0.05,0.35), geom=c("line"), size=I(1)) +
           geom_vline(xintercept=loff_sec, color="dark red", size=1) + 
           geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  arrange(p1,p2)
  dev.off()


  png("to_ptcr_dot.png")
  p <- qplot(time, PTCR_DOT, data=data_takeoff,  col=I("blue"),
             xlab="Time [seconds]", geom=c("line"), size=I(1)) + 
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
             geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()


  png("to_pitch_cpt.png", width=960)
  p1 <- qplot(time, PITCH_CPT, data=data_takeoff,  col=I("blue"),
           xlab="Time [seconds]", geom=c("line"), size=I(1)) + 
           geom_vline(xintercept=loff_sec, color="dark red", size=1) +
           geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  #dev.off()

  #png("to_pitch_fo.png")
  p2 <- qplot(time, PITCH_FO, data=data_takeoff,  col=I("blue"),
           xlab="Time [seconds]", geom=c("line"), size=I(1)) + 
           geom_vline(xintercept=loff_sec, color="dark red", size=1) +
           geom_vline(xintercept=rot, color="green", size=1)
  #print(p)
  arrange(p1,p2)
  dev.off()


  png("to_pitch.png")
  p <- qplot(time, PITCH, data=data_takeoff,  col=I("blue"),
           xlab="Time [seconds]", geom=c("line"), size=I(1)) + 
           geom_vline(xintercept=loff_sec, color="dark red", size=1) +
           geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()


  png("to_ptcr.png")
  p <- qplot(time, PTCR, data=data_takeoff,  col=I("blue"),
           xlab="Time [seconds]", geom=c("line"), size=I(1)) + 
           geom_vline(xintercept=loff_sec, color="dark red", size=1) +
           geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()


  
  # PDF Reporting
  #setwd(binpath)
  setwd(resultpath)
  brew("Rtoffrep.brew", paste0('Rtoffrep',s,".tex"))
  texi2dvi(paste0('Rtoffrep',s,".tex"), pdf = TRUE)
  ##$##  
}

landing_plots <- function(){
  
  # cleaning NANs fron TOUCHDOWNC
  flightdata$TOUCH_DOWNC[which(is.na(flightdata$TOUCH_DOWNC))] <- 0
  td_instant=min(which(flightdata$TOUCH_DOWNC != 0))
  td_instant <- td_instant -2
  
  
  # Landing Initial and Final points
  delta_ti <- 60
  delta_tf <- 60
  init_t <- td_instant-delta_ti
  final_t <- td_instant+delta_tf
  
  t_ldg_lft <- init_t + min(which(flightdata$LDG_ON_1[init_t:final_t]==1))
  t_ldg_rgt <- init_t + min(which(flightdata$LDG_ON_2[init_t:final_t]==1))
  t_mlg <- min(t_ldg_lft, t_ldg_rgt)
  
  t_ldg_nos <- init_t + min(which(flightdata$LDGN_ON[init_t:final_t]==1))
  delta_t_ldg <- (t_ldg_nos - t_mlg)/8
  
  max_vrtg <- max(vrtg[init_t:final_t])
  min_ptcr <- min(ptcr[init_t:final_t])
  min_long <- min(long[init_t:final_t])
  
  max_latg <- max(latg[init_t:final_t])
  min_latg <- min(latg[init_t:final_t])
  latg_ampl <- max_latg - min_latg
  
  max_hdg <- max(head_mag[init_t:final_t])
  min_hdg <- min(head_mag[init_t:final_t])
  hdg_ampl <- max_hdg - min_hdg
  
  gs_at_td <- gs[td_instant]
  pitch_at_td <- pitch[td_instant]
  ivv_at_td <- ivv[td_instant]
  ptcr_at_td <- ptcr[td_instant]
  e_at_td <- gs[td_instant]*c_knot_ms*gw1kg[td_instant]/2

  setwd(figurepath)  
  
  png("pitch.png")
  plot(pitch[init_t:final_t],type="l", col="blue", main="pitch", lwd=1.5)
  abline(v=delta_ti,col="green",lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("vrtg.png")
  plot(vrtg[init_t:final_t],type="l", col="blue",main="vrtg", lwd=1.5)
  abline(v=delta_ti,col="green",lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("long.png")
  plot(long[init_t:final_t],type="l", col="blue",main="long", lwd=1.5)
  abline(v=delta_ti,col="green",lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("latg.png")
  plot(latg[init_t:final_t],type="l", col="blue",main="latg", lwd=1.5)
  abline(v=delta_ti,col="green",lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("head_mag.png")
  plot(head_mag[init_t:final_t],type="l", col="blue",main="head_mag", lwd=1.5)
  abline(v=delta_ti,col="green",lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("gs.png")
  plot(gs[init_t:final_t],type="l", col="blue",main="gs", lwd=1.5)
  abline(v=delta_ti,col="green",lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("ptcr.png")
  plot(ptcr[init_t:final_t],type="l", col="blue",main="ptcr", lwd=1.5)
  abline(v=delta_ti,col="green", lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("raltd1.png")
  plot(raltd1[init_t:final_t],type="l", col="blue" ,main="raltd1", lwd=1.5)
  abline(v=delta_ti,col="green", lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("raltd2.png")
  plot(raltd2[init_t:final_t],type="l", col="blue" ,main="raltd2", lwd=1.5)
  abline(v=delta_ti,col="green", lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("lg_left.png")  
  plot(LG_left[init_t:final_t],type="l", col="blue", main="LG Left", lwd=1.5)
  abline(v=delta_ti,col="green", lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("lg_right.png")  
  plot(LG_right[init_t:final_t],type="l", col="blue", main="LG right", lwd=1.5)
  abline(v=delta_ti,col="green", lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("lg_nose.png")
  plot(LG_nose[init_t:final_t],type="l", col="blue", main="LG nose", lwd=1.5)
  abline(v=delta_ti,col="green", lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  # PDF Reporting
  #setwd(binpath)
  setwd(resultpath)
  brew("autorep.brew", paste0('autorep',s,".tex"))
  texi2dvi(paste0('autorep',s,".tex"), pdf = TRUE)
    
}

vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
arrange <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
  dots <- list(...)
  n <- length(dots)
  if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}
  if(is.null(nrow)) { nrow = ceiling(n/ncol)}
  if(is.null(ncol)) { ncol = ceiling(n/nrow)}
  ## NOTE see n2mfrow in grDevices for possible alternative
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )
  ii.p <- 1
  for(ii.row in seq(1, nrow)){
    ii.table.row <- ii.row
    if(as.table) {ii.table.row <- nrow - ii.table.row + 1}
    for(ii.col in seq(1, ncol)){
      ii.table <- ii.p
      if(ii.p > n) break
      print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))
      ii.p <- ii.p + 1
    }
  }
}


#### START MAIN PROCEDURE
#################################################################################
### Choose the type of analysis:
###  * set = 1 at the report desired (both =1 allowed)
#################################################################################
take_off_study = 1
landing_study = 0

## Constants
c_knot_ms <- 1852/3600
c_ms_kmh <- 3.6
area <- 138 # [m^2] - surface of the wing with flaps 

## paths
flightpath <- "C:/FlightDB/TTD"    ## Insert case into the respective folder
binpath <- "C:/Users/210906/Dropbox/EASA/flightRtools/Rtoff/bin"
resultpath <- "C:/Users/210906/Dropbox/EASA/flightRtools/Rtoff/results"
figurepath <- "C:/Users/210906/Dropbox/EASA/flightRtools/Rtoff/results/figures"

# file list
setwd(flightpath)
fileList <- list.files(path=flightpath, pattern=".csv")

## Escolher o ficheiro pelo nº "s"
#s = 1
s=10

# alternativa - fazer o enable deste ciclo FOR para todos os ficheiros do folder
#for (s in 1:NROW(fileList)) {

# Variable and Vector Initialization
#
#flight_measurements <- mat.or.vec(NROW(fileList),20) # Matrix of dimensions: [fligths x Measurements]
#flight_measurements <- data.frame()
#mean_phase_store <- mat.or.vec(NROW(fileList),1)

  
  ## LOAD Flight From List
  setwd(flightpath)
  flightdata <- fread(fileList[s],sep=",",header=T, stringsAsFactors = F, verbose=FALSE,
                      colClasses = c(ACT='character', AC_TYPE='character', ORIGIN='character', 
                      RUNWAY_TO='character', RUNWAY_LD='character', DESTINATION='character', 
                      DATE='Date')) 
  
  
    nrows <- NROW(flightdata)
        
## Interpolations
    pitch <- approx(seq(1:nrows), flightdata$PITCH,xout=c(1:nrows), method="linear",n=nrows)$y
    ptcr <- approx(seq(1:nrows), flightdata$PTCR,xout=c(1:nrows), method="linear",n=nrows)$y
    pitch_cpt <- approx(seq(1:nrows), flightdata$PITCH_CPT,xout=c(1:nrows), method="linear",n=nrows)$y
    pitch_fo <- approx(seq(1:nrows), flightdata$PITCH_FO,xout=c(1:nrows), method="linear",n=nrows)$y
    alt_std <- approx(seq(1:nrows), flightdata$ALT_STDC,xout=c(1:nrows), method="linear",n=nrows)$y
    raltd1 <- approx(seq(1:nrows), flightdata$RALTD1,xout=c(1:nrows), method="linear",n=nrows)$y
    raltd2 <- approx(seq(1:nrows), flightdata$RALTD2,xout=c(1:nrows), method="linear",n=nrows)$y
    head_mag <- approx(seq(1:nrows), flightdata$HEAD_MAG,xout=c(1:nrows), method="linear",n=nrows)$y
    ivv <- approx(seq(1:nrows), flightdata$IVV,xout=c(1:nrows), method="linear",n=nrows)$y
    long <- approx(seq(1:nrows), flightdata$LONG,xout=c(1:nrows), method="linear",n=nrows)$y
    latg <- approx(seq(1:nrows), flightdata$LATG,xout=c(1:nrows), method="linear",n=nrows)$y
    roll <- approx(seq(1:nrows), flightdata$ROLL,xout=c(1:nrows), method="linear",n=nrows)$y
    ias <- approx(seq(1:nrows), flightdata$IASC,xout=c(1:nrows), method="linear",n=nrows)$y
    gs <- approx(seq(1:nrows), flightdata$GSC,xout=c(1:nrows), method="linear",n=nrows)$y
    gw1kg <- approx(seq(1:nrows), flightdata$GW1KG,xout=c(1:nrows), method="linear",n=nrows)$y
    fpa <- approx(seq(1:nrows), flightdata$FPA,xout=c(1:nrows), method="linear",n=nrows)$y
    fpac <- approx(seq(1:nrows), flightdata$FPAC,xout=c(1:nrows), method="linear",n=nrows)$y
    LG_left <- approx(seq(1:nrows), flightdata$LDG_ON_1,xout=c(1:nrows), method="linear",n=nrows)$y
    LG_nose <- approx(seq(1:nrows), flightdata$LDGN_ON,xout=c(1:nrows), method="linear",n=nrows)$y
    LG_right <- approx(seq(1:nrows), flightdata$LDG_ON_2,xout=c(1:nrows), method="linear",n=nrows)$y
    aoal <- approx(seq(1:nrows), flightdata$AOAL,xout=c(1:nrows), method="linear",n=nrows)$y
    aoar <- approx(seq(1:nrows), flightdata$AOAR,xout=c(1:nrows), method="linear",n=nrows)$y
    n11 <- approx(seq(1:nrows), flightdata$N11C,xout=c(1:nrows), method="linear",n=nrows)$y
    n12 <- approx(seq(1:nrows), flightdata$N12C,xout=c(1:nrows), method="linear",n=nrows)$y
    n21 <- approx(seq(1:nrows), flightdata$N21C,xout=c(1:nrows), method="linear",n=nrows)$y
    n22 <- approx(seq(1:nrows), flightdata$N22C,xout=c(1:nrows), method="linear",n=nrows)$y
    ff1 <- approx(seq(1:nrows), flightdata$FF1C,xout=c(1:nrows), method="linear",n=nrows)$y
    ff2 <- approx(seq(1:nrows), flightdata$FF2C,xout=c(1:nrows), method="linear",n=nrows)$y
    egt1 <- approx(seq(1:nrows), flightdata$EGT1C,xout=c(1:nrows), method="linear",n=nrows)$y
    egt2 <- approx(seq(1:nrows), flightdata$EGT2C,xout=c(1:nrows), method="linear",n=nrows)$y
    flx1_temp <- approx(seq(1:nrows), flightdata$FLX1_TEMP,xout=c(1:nrows), method="linear",n=nrows)$y
    flx2_temp <- approx(seq(1:nrows), flightdata$FLX2_TEMP,xout=c(1:nrows), method="linear",n=nrows)$y
    at_flx <- approx(seq(1:nrows), flightdata$AT_FLX,xout=c(1:nrows), method="linear",n=nrows)$y
    q1 <- approx(seq(1:nrows), flightdata$Q1,xout=c(1:nrows), method="linear",n=nrows)$y
    q2 <- approx(seq(1:nrows), flightdata$Q2,xout=c(1:nrows), method="linear",n=nrows)$y
    pt1 <- approx(seq(1:nrows), flightdata$PT1,xout=c(1:nrows), method="linear",n=nrows)$y
    pt2 <- approx(seq(1:nrows), flightdata$PT2,xout=c(1:nrows), method="linear",n=nrows)$y
    tla1 <- approx(seq(1:nrows), flightdata$TLA1,xout=c(1:nrows), method="linear",n=nrows)$y
    tla1c <- approx(seq(1:nrows), flightdata$TLA1C,xout=c(1:nrows), method="linear",n=nrows)$y
    tla2 <- approx(seq(1:nrows), flightdata$TLA2,xout=c(1:nrows), method="linear",n=nrows)$y
    tla2c <- approx(seq(1:nrows), flightdata$TLA2C,xout=c(1:nrows), method="linear",n=nrows)$y
    sat <- approx(seq(1:nrows), flightdata$SAT,xout=c(1:nrows), method="linear",n=nrows)$y
    tat <- approx(seq(1:nrows), flightdata$TAT,xout=c(1:nrows), method="linear",n=nrows)$y
    p0_1 <- approx(seq(1:nrows), flightdata$P0_1,xout=c(1:nrows), method="linear",n=nrows)$y
    p0_2 <- approx(seq(1:nrows), flightdata$P0_2,xout=c(1:nrows), method="linear",n=nrows)$y
    p2_1 <- approx(seq(1:nrows), flightdata$P2_1,xout=c(1:nrows), method="linear",n=nrows)$y
    p2_2 <- approx(seq(1:nrows), flightdata$P2_2,xout=c(1:nrows), method="linear",n=nrows)$y
    p31 <- approx(seq(1:nrows), flightdata$P31,xout=c(1:nrows), method="linear",n=nrows)$y
    p32 <- approx(seq(1:nrows), flightdata$P32,xout=c(1:nrows), method="linear",n=nrows)$y
    t12_1 <- approx(seq(1:nrows), flightdata$T12_1,xout=c(1:nrows), method="linear",n=nrows)$y
    t12_2 <- approx(seq(1:nrows), flightdata$T12_2,xout=c(1:nrows), method="linear",n=nrows)$y
    t25_1 <- approx(seq(1:nrows), flightdata$T25_1,xout=c(1:nrows), method="linear",n=nrows)$y
    t25_2 <- approx(seq(1:nrows), flightdata$T25_2,xout=c(1:nrows), method="linear",n=nrows)$y
    fm_fwc <- approx(seq(1:nrows), flightdata$FM_FWC,xout=c(1:nrows), method="linear",n=nrows)$y

# sempre a zero!????
#ps13_1<- approx(seq(1:nrows), flightdata$PS13_1,xout=c(1:nrows), method="linear",n=nrows)$y
#ps13_2 <- approx(seq(1:nrows), flightdata$PS13_2,xout=c(1:nrows), method="linear",n=nrows)$y


    vrtg <- flightdata$VRTG
    #flight_phase <- approx(seq(1:nrows), flightdata$FLIGHT_PHASE,xout=c(1:nrows), method="linear",n=nrows)$y
    #apflare <- approx(seq(1:nrows), flightdata$APFLARE,xout=c(1:nrows), method="linear",n=nrows)$y


## Support calcs and figs
# colocar mais tarde nos "flight_measurements"
orig <- which(!flightdata$ORIGIN=="")
origin <- flightdata$ORIGIN[orig[1000]]

dest <- which(!flightdata$DESTINATION=="")
destination <- flightdata$DESTINATION[dest[1000]]

dat <- which(!flightdata$DATE=="")
dat <- flightdata$DATE[dat[1000]]

rwhdg <- which(!flightdata$RUNWAY_TO=="")
rwhdg <- flightdata$RUNWAY_TO[rwhdg[1000]]
##


if(take_off_study==1){
  to_plots()
}

if(landing_study==1){
  landing_plots()
}


#}

