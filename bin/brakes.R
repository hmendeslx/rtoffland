#Clear Workspace
rm(list=ls(all=TRUE))

library(data.table)

#Flight data files directory
setwd('M:/PEDRO_SOARES/Alpha Prot/FlightDB2')

#Filelist to read
fileList <- list.files(path=getwd(), pattern=".csv")

flt_number=vector(mode="numeric")
date_to=vector(mode="character")
leg_from=vector(mode="character")
leg_to=vector(mode="character")
liftoff=vector(mode="numeric")
takeoff_time=vector(mode="numeric")
brake_p1_avg=vector(mode="numeric")
brake_p2_avg=vector(mode="numeric")
brake_p3_avg=vector(mode="numeric")
brake_p4_avg=vector(mode="numeric")
brake_lpa_avg=vector(mode="numeric")
brake_rpa_avg=vector(mode="numeric")
brake_p1_max=vector(mode="numeric")
brake_p2_max=vector(mode="numeric")
brake_p3_max=vector(mode="numeric")
brake_p4_max=vector(mode="numeric")
brake_lpa_max=vector(mode="numeric")
brake_rpa_max=vector(mode="numeric")

#Brake Pedals/Pressure


for (s in 1:NROW(fileList)) {
  
  ## Data Input
  flightdata <- fread(fileList[s],sep=",",header=T, stringsAsFactors = F, verbose=FALSE) 
  
  ## Flight Info
  flt_number[s]=names(sort(summary(as.factor(flightdata$FLTNUM)), decreasing=T)[1])
  leg_from[s]=flightdata$ORIGIN[min(which(flightdata$ORIGIN!=""))]
  leg_to[s]=flightdata$DESTINATION[min(which(flightdata$DESTINATION!=""))]
  date_to[s]=flightdata$DATE[min(which(flightdata$DATE!=""))]
  
  ## Lift Off Instant
  liftoff[s]=min(which((flightdata$FLIGHT_PHASE==3 | flightdata$FLIGHT_PHASE==4) & flightdata$HEIGHT>=1.5))
  
  takeoff_time[s]=NROW(which(flightdata$FLIGHT_PHASE==3 & flightdata$GSC>10))
  ## Check Brake Pressure and Brake Pedals

  brake_p1_avg[s]=sum(flightdata$BRAKE_P1[which(flightdata$FLIGHT_PHASE==3)])/takeoff_time[s]
  brake_p1_max[s]=max(flightdata$BRAKE_P1[which(flightdata$FLIGHT_PHASE==3)])  
  
  
  brake_p2_avg[s]=sum(flightdata$BRAKE_P2[which(flightdata$FLIGHT_PHASE==3)])/takeoff_time[s]
  brake_p2_max[s]=max(flightdata$BRAKE_P2[which(flightdata$FLIGHT_PHASE==3)])  
  
  
  brake_p3_avg[s]=sum(flightdata$BRAKE_P3[which(flightdata$FLIGHT_PHASE==3)])/takeoff_time[s]
  brake_p3_max[s]=max(flightdata$BRAKE_P3[which(flightdata$FLIGHT_PHASE==3)])  
  
  
  brake_p4_avg[s]=sum(flightdata$BRAKE_P4[which(flightdata$FLIGHT_PHASE==3)])/takeoff_time[s]
  brake_p4_max[s]=max(flightdata$BRAKE_P4[which(flightdata$FLIGHT_PHASE==3)])  
  
  
  brake_lpa_avg[s]=sum(flightdata$BRAKE_LPA[which(flightdata$FLIGHT_PHASE==3)])/takeoff_time[s]
  brake_lpa_max[s]=max(flightdata$BRAKE_LPA[which(flightdata$FLIGHT_PHASE==3)])  
  
  
  brake_rpa_avg[s]=sum(flightdata$BRAKE_RPA[which(flightdata$FLIGHT_PHASE==3)])/takeoff_time[s]
  brake_rpa_max[s]=max(flightdata$BRAKE_RPA[which(flightdata$FLIGHT_PHASE==3)])  
    
}

#Plots
library(ggvis)
library(ggplot2)

#Data Selection
#which(is.na(brake_p1_avg)==TRUE | is.nan(brake_p1_avg)==TRUE | is.na(brake_lpa_avg)==TRUE | is.nan(brake_lpa_avg)==TRUE | brake_p1_avg==0 | )
#which(is.na(brake_lpa_avg)==TRUE | is.nan(brake_lpa_avg)==TRUE)
fileList <- fileList[-which(is.na(brake_p1_avg)==TRUE | is.nan(brake_p1_avg)==TRUE | is.na(brake_lpa_avg)==TRUE | is.nan(brake_lpa_avg)==TRUE | brake_p1_avg==0 | brake_p2_avg==0 | brake_p3_avg==0 | brake_p4_avg==0)]
date_to <- date_to[-which(is.na(brake_p1_avg)==TRUE | is.nan(brake_p1_avg)==TRUE | is.na(brake_lpa_avg)==TRUE | is.nan(brake_lpa_avg)==TRUE | brake_p1_avg==0 | brake_p2_avg==0 | brake_p3_avg==0 | brake_p4_avg==0)]
flt_number <- flt_number[-which(is.na(brake_p1_avg)==TRUE | is.nan(brake_p1_avg)==TRUE | is.na(brake_lpa_avg)==TRUE | is.nan(brake_lpa_avg)==TRUE | brake_p1_avg==0 | brake_p2_avg==0 | brake_p3_avg==0 | brake_p4_avg==0)]
leg_from <- leg_from[-which(is.na(brake_p1_avg)==TRUE | is.nan(brake_p1_avg)==TRUE | is.na(brake_lpa_avg)==TRUE | is.nan(brake_lpa_avg)==TRUE | brake_p1_avg==0 | brake_p2_avg==0 | brake_p3_avg==0 | brake_p4_avg==0)]
leg_to <- leg_to[-which(is.na(brake_p1_avg)==TRUE | is.nan(brake_p1_avg)==TRUE | is.na(brake_lpa_avg)==TRUE | is.nan(brake_lpa_avg)==TRUE | brake_p1_avg==0 | brake_p2_avg==0 | brake_p3_avg==0 | brake_p4_avg==0)]
liftoff <- liftoff[-which(is.na(brake_p1_avg)==TRUE | is.nan(brake_p1_avg)==TRUE | is.na(brake_lpa_avg)==TRUE | is.nan(brake_lpa_avg)==TRUE | brake_p1_avg==0 | brake_p2_avg==0 | brake_p3_avg==0 | brake_p4_avg==0)]
takeoff_time <- takeoff_time[-which(is.na(brake_p1_avg)==TRUE | is.nan(brake_p1_avg)==TRUE | is.na(brake_lpa_avg)==TRUE | is.nan(brake_lpa_avg)==TRUE | brake_p1_avg==0 | brake_p2_avg==0 | brake_p3_avg==0 | brake_p4_avg==0)]

BRAKE_P1_AVG <- brake_p1_avg[-which(is.na(brake_p1_avg)==TRUE | is.nan(brake_p1_avg)==TRUE | is.na(brake_lpa_avg)==TRUE | is.nan(brake_lpa_avg)==TRUE | brake_p1_avg==0 | brake_p2_avg==0 | brake_p3_avg==0 | brake_p4_avg==0)]
BRAKE_P1_MAX <- brake_p1_max[-which(is.na(brake_p1_avg)==TRUE | is.nan(brake_p1_avg)==TRUE | is.na(brake_lpa_avg)==TRUE | is.nan(brake_lpa_avg)==TRUE | brake_p1_avg==0 | brake_p2_avg==0 | brake_p3_avg==0 | brake_p4_avg==0)]

BRAKE_P2_AVG <- brake_p2_avg[-which(is.na(brake_p1_avg)==TRUE | is.nan(brake_p1_avg)==TRUE | is.na(brake_lpa_avg)==TRUE | is.nan(brake_lpa_avg)==TRUE | brake_p1_avg==0 | brake_p2_avg==0 | brake_p3_avg==0 | brake_p4_avg==0)]
BRAKE_P2_MAX <- brake_p2_max[-which(is.na(brake_p1_avg)==TRUE | is.nan(brake_p1_avg)==TRUE | is.na(brake_lpa_avg)==TRUE | is.nan(brake_lpa_avg)==TRUE | brake_p1_avg==0 | brake_p2_avg==0 | brake_p3_avg==0 | brake_p4_avg==0)]

BRAKE_P3_AVG <- brake_p3_avg[-which(is.na(brake_p1_avg)==TRUE | is.nan(brake_p1_avg)==TRUE | is.na(brake_lpa_avg)==TRUE | is.nan(brake_lpa_avg)==TRUE | brake_p1_avg==0 | brake_p2_avg==0 | brake_p3_avg==0 | brake_p4_avg==0)]
BRAKE_P3_MAX <- brake_p3_max[-which(is.na(brake_p1_avg)==TRUE | is.nan(brake_p1_avg)==TRUE | is.na(brake_lpa_avg)==TRUE | is.nan(brake_lpa_avg)==TRUE | brake_p1_avg==0 | brake_p2_avg==0 | brake_p3_avg==0 | brake_p4_avg==0)]

BRAKE_P4_AVG <- brake_p4_avg[-which(is.na(brake_p1_avg)==TRUE | is.nan(brake_p1_avg)==TRUE | is.na(brake_lpa_avg)==TRUE | is.nan(brake_lpa_avg)==TRUE | brake_p1_avg==0 | brake_p2_avg==0 | brake_p3_avg==0 | brake_p4_avg==0)]
BRAKE_P4_MAX <- brake_p4_max[-which(is.na(brake_p1_avg)==TRUE | is.nan(brake_p1_avg)==TRUE | is.na(brake_lpa_avg)==TRUE | is.nan(brake_lpa_avg)==TRUE | brake_p1_avg==0 | brake_p2_avg==0 | brake_p3_avg==0 | brake_p4_avg==0)]

BRAKE_LPA_AVG <- brake_lpa_avg[-which(is.na(brake_p1_avg)==TRUE | is.nan(brake_p1_avg)==TRUE | is.na(brake_lpa_avg)==TRUE | is.nan(brake_lpa_avg)==TRUE | brake_p1_avg==0 | brake_p2_avg==0 | brake_p3_avg==0 | brake_p4_avg==0)]
BRAKE_LPA_MAX <- brake_lpa_max[-which(is.na(brake_p1_avg)==TRUE | is.nan(brake_p1_avg)==TRUE | is.na(brake_lpa_avg)==TRUE | is.nan(brake_lpa_avg)==TRUE | brake_p1_avg==0 | brake_p2_avg==0 | brake_p3_avg==0 | brake_p4_avg==0)]

BRAKE_RPA_AVG <- brake_rpa_avg[-which(is.na(brake_p1_avg)==TRUE | is.nan(brake_p1_avg)==TRUE | is.na(brake_lpa_avg)==TRUE | is.nan(brake_lpa_avg)==TRUE | brake_p1_avg==0 | brake_p2_avg==0 | brake_p3_avg==0 | brake_p4_avg==0)]
BRAKE_RPA_MAX <- brake_rpa_max[-which(is.na(brake_p1_avg)==TRUE | is.nan(brake_p1_avg)==TRUE | is.na(brake_lpa_avg)==TRUE | is.nan(brake_lpa_avg)==TRUE | brake_p1_avg==0 | brake_p2_avg==0 | brake_p3_avg==0 | brake_p4_avg==0)]

rm(brake_p1_avg,brake_p1_max,brake_p2_avg,brake_p2_max,brake_p3_avg,brake_p3_max,brake_p4_avg,brake_p4_max,brake_lpa_avg,brake_lpa_max,brake_rpa_avg,brake_rpa_max)



##PLOTS GGVIS (Under Construction)

df=data.frame(BRAKE_P1_MAX,BRAKE_P2_MAX,BRAKE_P3_MAX,BRAKE_P4_MAX)
#df=data.frame(time,IASC,VLS,TAS)

df %>% ggvis(~BRAKE_P1_MAX) %>% layer_histograms() %>%
  layer_densities()
  add_axis("x", title = "Pressure (psi)") %>%
  add_axis("y", title = "Count") %>%
  add_axis("x", orient = "top", ticks = 0, title = "BRAKE_P1_MAX",
           properties = axis_props(
             axis = list(stroke = "white"),
             labels = list(fontSize = 0)))
  add_legend("stroke")

df2=data.frame(BRAKE_P1_AVG,BRAKE_P2_AVG,BRAKE_P3_AVG,BRAKE_P4_AVG)

df2 %>% ggvis(~BRAKE_P1_AVG) %>% layer_histograms() %>%
  add_axis("x", title = "Pressure (psi)") %>%
  add_axis("y", title = "Count") %>%
  add_legend("stroke")  

df3=data.frame(BRAKE_LPA_AVG,BRAKE_RPA_AVG)

df3 %>% ggvis(~BRAKE_LPA_AVG) %>% layer_histograms() %>%
  add_axis("x", title = "Pressure (psi)") %>%
  add_axis("y", title = "Count")  %>%
  add_legend("stroke")  

df4=data.frame(BRAKE_LPA_MAX,BRAKE_RPA_MAX)

df4 %>% ggvis(~BRAKE_LPA_MAX) %>% layer_histograms() %>%
  add_axis("x", title = "Pressure (psi)") %>%
  add_axis("y", title = "Count") %>%
  add_axis("x", orient = "top", ticks = 0, title = "BRAKE_P1_MAX",
           properties = axis_props(
             axis = list(stroke = "white"),
             labels = list(fontSize = 0)))


df5=data.frame(BRAKE_LPA_MAX,BRAKE_RPA_MAX,BRAKE_LPA_AVG,BRAKE_RPA_AVG)

df5 %>% ggvis(~BRAKE_LPA_MAX) %>% layer_histograms() %>%
  add_axis("x", title = "Angle (degrees)") %>%
  add_axis("y", title = "Count", title_offset = 50) %>%
  add_axis("x", orient = "top", ticks = 0, title = "BRAKE LPA MAX",
           properties = axis_props(
             axis = list(stroke = "white"),
             labels = list(fontSize = 0)))

df5 %>% ggvis(~BRAKE_RPA_MAX) %>% layer_histograms() %>%
  add_axis("x", title = "Angle (degrees)") %>%
  add_axis("y", title = "Count", title_offset = 50) %>%
  add_axis("x", orient = "top", ticks = 0, title = "BRAKE RPA MAX",
           properties = axis_props(
             axis = list(stroke = "white"),
             labels = list(fontSize = 0)))

df5 %>% ggvis(~BRAKE_LPA_AVG) %>% layer_histograms() %>%
  add_axis("x", title = "Angle (degrees)") %>%
  add_axis("y", title = "Count", title_offset = 50) %>%
  add_axis("x", orient = "top", ticks = 0, title = "BRAKE LPA AVG",
           properties = axis_props(
             axis = list(stroke = "white"),
             labels = list(fontSize = 0)))

df5 %>% ggvis(~BRAKE_RPA_AVG) %>% layer_histograms() %>%
  add_axis("x", title = "Angle (degrees)") %>%
  add_axis("y", title = "Count", title_offset = 50) %>%
  add_axis("x", orient = "top", ticks = 0, title = "BRAKE RPA AVG",
           properties = axis_props(
             axis = list(stroke = "white"),
             labels = list(fontSize = 0)))
 



##Plots GGPLOT2
theme_set(theme_bw())

##MAXIMUM BRAKE DURING TAKEOFF

p0 = qplot(BRAKE_P1_MAX, geom = 'blank') +   
  geom_line(aes(y = ..density.., colour = 'Density Line'), stat = 'density') +  
  geom_histogram(aes(y = ..density..), alpha = 0.4) +                        
  scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
  ylab("Density") +
  xlab("Pressure (psi)") + 
  ggtitle("MAX BRAKE P1") +
  theme(legend.key = element_rect(colour = NA),legend.position=c(0.9,0.9))

p1 = qplot(BRAKE_P2_MAX, geom = 'blank') +   
  geom_line(aes(y = ..density.., colour = 'Density Line'), stat = 'density') +  
  geom_histogram(aes(y = ..density..), alpha = 0.4) +                        
  scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
  ylab("Density") +
  xlab("Pressure (psi)") + 
  ggtitle("MAX BRAKE P2") +
  theme(legend.key = element_rect(colour = NA),legend.position=c(0.9,0.9))

p2 = qplot(BRAKE_P3_MAX, geom = 'blank') +   
  geom_line(aes(y = ..density.., colour = 'Density Line'), stat = 'density') +  
  geom_histogram(aes(y = ..density..), alpha = 0.4) +                        
  scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
  ylab("Density") +
  xlab("Pressure (psi)") + 
  ggtitle("MAX BRAKE P3") +
  theme(legend.key = element_rect(colour = NA),legend.position=c(0.9,0.9))

p3 = qplot(BRAKE_P4_MAX, geom = 'blank') +   
  geom_line(aes(y = ..density.., colour = 'Density Line'), stat = 'density') +  
  geom_histogram(aes(y = ..density..), alpha = 0.4) +                        
  scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
  ylab("Density") +
  xlab("Pressure (psi)") + 
  ggtitle("MAX BRAKE P4") +
  theme(legend.key = element_rect(colour = NA),legend.position=c(0.9,0.9))

##AVERAGE BRAKE DURING TAKEOFF

p0 = qplot(BRAKE_P1_AVG, geom = 'blank') +   
  geom_line(aes(y = ..density.., colour = 'Density Line'), stat = 'density') +  
  geom_histogram(aes(y = ..density..), alpha = 0.4) +                        
  scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
  ylab("Density") +
  xlab("Pressure (psi)") + 
  ggtitle("AVG BRAKE P1") +
  theme(legend.key = element_rect(colour = NA),legend.position=c(0.9,0.9))

p1 = qplot(BRAKE_P2_AVG, geom = 'blank') +   
  geom_line(aes(y = ..density.., colour = 'Density Line'), stat = 'density') +  
  geom_histogram(aes(y = ..density..), alpha = 0.4) +                        
  scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
  ylab("Density") +
  xlab("Pressure (psi)") + 
  ggtitle("AVG BRAKE P2") +
  theme(legend.key = element_rect(colour = NA),legend.position=c(0.9,0.9))

p2 = qplot(BRAKE_P3_AVG, geom = 'blank') +   
  geom_line(aes(y = ..density.., colour = 'Density Line'), stat = 'density') +  
  geom_histogram(aes(y = ..density..), alpha = 0.4) +                        
  scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
  ylab("Density") +
  xlab("Pressure (psi)") + 
  ggtitle("AVG BRAKE P3") +
  theme(legend.key = element_rect(colour = NA),legend.position=c(0.9,0.9))

p3 = qplot(BRAKE_P4_AVG, geom = 'blank') +   
  geom_line(aes(y = ..density.., colour = 'Density Line'), stat = 'density') +  
  geom_histogram(aes(y = ..density..), alpha = 0.4) +                        
  scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
  ylab("Density") +
  xlab("Pressure (psi)") + 
  ggtitle("AVG BRAKE P4") +
  theme(legend.key = element_rect(colour = NA),legend.position=c(0.9,0.9))



##BRAKE PEDAL ANGLES DURING TAKEOFF

p0 = qplot(BRAKE_LPA_MAX, geom = 'blank') +   
  geom_line(aes(y = ..density.., colour = 'Density Line'), stat = 'density') +  
  geom_histogram(aes(y = ..density..), alpha = 0.4) +                        
  scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
  ylab("Density") +
  xlab("Angle (degrees)") + 
  ggtitle("BRAKE LPA MAX") +
  theme(legend.key = element_rect(colour = NA),legend.position=c(0.9,0.9))

p1 = qplot(BRAKE_RPA_MAX, geom = 'blank') +   
  geom_line(aes(y = ..density.., colour = 'Density Line'), stat = 'density') +  
  geom_histogram(aes(y = ..density..), alpha = 0.4) +                        
  scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
  ylab("Density") +
  xlab("Angle (Degrees)") + 
  ggtitle("BRAKE RPA MAX") +
  theme(legend.key = element_rect(colour = NA),legend.position=c(0.9,0.9))

p2 = qplot(BRAKE_LPA_AVG, geom = 'blank') +   
  geom_line(aes(y = ..density.., colour = 'Density Line'), stat = 'density') +  
  geom_histogram(aes(y = ..density..), alpha = 0.4) +                        
  scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
  ylab("Density") +
  xlab("Angle (degrees)") + 
  ggtitle("BRAKE LPA AVG") +
  theme(legend.key = element_rect(colour = NA),legend.position=c(0.9,0.9))

p3 = qplot(BRAKE_RPA_AVG, geom = 'blank') +   
  geom_line(aes(y = ..density.., colour = 'Density Line'), stat = 'density') +  
  geom_histogram(aes(y = ..density..), alpha = 0.4) +                        
  scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
  ylab("Density") +
  xlab("Angle (Degrees)") + 
  ggtitle("BRAKE RPA AVG") +
  theme(legend.key = element_rect(colour = NA),legend.position=c(0.9,0.9))


p4 = qplot(BRAKE_P4_MAX, geom = 'blank') +   
  geom_line(aes(y = ..density.., colour = 'Density Line'), stat = 'density') +  
  geom_histogram(aes(y = ..density..), alpha = 0.4) +                        
  scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
  ylab("Density") +
  xlab("Pressure (psi)") + 
  ggtitle("BRAKE LPA") +
  theme(legend.key = element_rect(colour = NA))


#Exceedance

u1=mean(BRAKE_P1_MAX)
u2=mean(BRAKE_P2_MAX)
u3=mean(BRAKE_P3_MAX)
u4=mean(BRAKE_P4_MAX)
sd1=sd(BRAKE_P1_MAX)
sd2=sd(BRAKE_P2_MAX)
sd3=sd(BRAKE_P3_MAX)
sd4=sd(BRAKE_P4_MAX)

BRAKE_P1_MAX_EXC=u1+3*sd1
BRAKE_P2_MAX_EXC=u2+3*sd2
BRAKE_P3_MAX_EXC=u3+3*sd3
BRAKE_P4_MAX_EXC=u4+3*sd4


p1=which(BRAKE_P1_MAX>BRAKE_P1_MAX_EXC)
p2=which(BRAKE_P2_MAX>BRAKE_P2_MAX_EXC)
p3=which(BRAKE_P3_MAX>BRAKE_P3_MAX_EXC)
p4=which(BRAKE_P4_MAX>BRAKE_P4_MAX_EXC)

which(BRAKE_P1_MAX>BRAKE_P1_MAX_EXC & BRAKE_P2_MAX>BRAKE_P2_MAX_EXC & BRAKE_P3_MAX>BRAKE_P3_MAX_EXC & BRAKE_P4_MAX>BRAKE_P4_MAX_EXC)
files_exceedance=which(BRAKE_P1_MAX>BRAKE_P1_MAX_EXC | BRAKE_P2_MAX>BRAKE_P2_MAX_EXC | BRAKE_P3_MAX>BRAKE_P3_MAX_EXC | BRAKE_P4_MAX>BRAKE_P4_MAX_EXC)


files_exceed=fileList[which(BRAKE_P1_MAX>BRAKE_P1_MAX_EXC | BRAKE_P2_MAX>BRAKE_P2_MAX_EXC | BRAKE_P3_MAX>BRAKE_P3_MAX_EXC | BRAKE_P4_MAX>BRAKE_P4_MAX_EXC)]

exceedance1=vector(mode="numeric")
exceedance2=vector(mode="numeric")
exceedance3=vector(mode="numeric")
exceedance4=vector(mode="numeric")
i=1
j=1
l=1
m=1
data_export=matrix(data=NA,nrow=NROW(files_exceedance),ncol=4)
for (k in 1:NROW(files_exceedance)) {
  
  if(p1[i]==files_exceedance[k]) {
    data_export[k,1]=1 
    i=i+1
  } else {
    data_export[k,1]=0
  }
  
  if(p2[j]==files_exceedance[k]) {
    data_export[k,2]=1 
    j=j+1
  } else {
    data_export[k,2]=0
  }
  
  if(p3[l]==files_exceedance[k]) {
    data_export[k,3]=1 
    l=l+1
  } else {
    data_export[k,3]=0
  }
  
  if(p4[m]==files_exceedance[k]) {
    data_export[k,4]=1 
    m=m+1
  } else {
    data_export[k,4]=0
  }
    
}

#ac-tails
ind <- substr(fileList,1,6)

ac_tails=ind[files_exceedance]

data_export=cbind(ac_tails,flt_number[files_exceedance],leg_from[files_exceedance],leg_to[files_exceedance],date_to[files_exceedance],data_export,BRAKE_P1_MAX[files_exceedance],BRAKE_P2_MAX[files_exceedance],BRAKE_P3_MAX[files_exceedance],BRAKE_P4_MAX[files_exceedance])
  

colnames(data_export) = c("AC_TAIL","FLT_NUM","ORIGIN","DESTINATION","DATE_TO","BRAKE_P1_EXC","BRAKE_P2_EXC","BRAKE_P3_EXC","BRAKE_P4_EXC","BRAKE_P1","BRAKE_P2","BRAKE_P3","BRAKE_P4")

write.csv(data_export,file="M:/PEDRO_SOARES/Brake During TakeOff/results/exceedance_table.csv",row.names=FALSE)