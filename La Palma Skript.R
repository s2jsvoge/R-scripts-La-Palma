######################################
##### Fog precipitation efficiency ###
##### in different forest types ######
##### on La Palma, Canary Islands#####
######################################

# Hypothesis: Fog precipitation efficiency in cloud influenced forests on La Palma 
#             is determinded by vertical vegetation structure and leaf morphology.
#             Pine forest is most efficient in absorbing precipitation from cloud layer.


# Dear La Palma Mates, unter "##"Überschriften bitte den Code einfügen. 
# Falls euch noch weitere Dinge einfallen, die wir berechnen könnten, einfach mit einer Überschrift hinzufügen.
# Bitte nicht vergessen kurz mit "#" zu schreiben, was die neu kreierte Variable ist
# z.b.: b1 - fog prec. Standort 1 Tag 1 oder entsprechende Überschriften ;)



####################
### Load in Data ###
####################
setwd("D:/Studium/Master Bayreuth/Science Schools/La Palma Schience School/results/csv")

descr_nets<-read.table(file="descriptions_nets.csv",sep=";",dec=",", head=TRUE)

descr_control<-read.table(file="descriptions_control.csv",sep=";",dec=",", head=TRUE)

descr_devices<-read.table(file="descriptions_devices.csv",sep=";",dec=",", head=TRUE)



#########################
### Site descriptions ###
#########################
# subsets per location
dev_site1<-subset(descr_devices, descr_devices$Location==1)
dev_site2<-subset(descr_devices, descr_devices$Location==2)
dev_site3<-subset(descr_devices, descr_devices$Location==3)


## Altitude
alt_site1_mean<-mean(dev_site1$Altitude.device)    # 1225.11 m
alt_site2_mean<-mean(dev_site2$Altitude.device)    # 1241.33 m
alt_site3_mean<-mean(dev_site3$Altitude.device)    # 1482.44 m


## Exposition
exp_site1<-dev_site1$Aspect.of.Location
exp_site2<-dev_site2$Aspect.of.Location
exp_site3<-dev_site3$Aspect.of.Location


## average Canopy cover
cc_site1_mean<-mean(dev_site1$Canopy.density)  #79.28
cc_site2_mean<-mean(dev_site2$Canopy.density)  # 82.19
cc_site3mean<-mean(dev_site3$Canopy.density)   # 51.86

cc_site1<-subset(descr_devices$Canopy.density, descr_devices$Location==1)
cc_site2<-subset(descr_devices$Canopy.density, descr_devices$Location==2)
cc_site3<-subset(descr_devices$Canopy.density, descr_devices$Location==3)


## stand density
sd_site1_mean<-mean(dev_site1$Stand.density..Bitterlich.)  # 15.33
sd_site2_mean<-mean(dev_site2$Stand.density..Bitterlich.)  # 20.78
sd_site3_mean<-mean(dev_site3$Stand.density..Bitterlich.)  # 18.89

sd_site1<-subset(descr_devices$Stand.density..Bitterlich., descr_devices$Location==1)
sd_site2<-subset(descr_devices$Stand.density..Bitterlich., descr_devices$Location==2)
sd_site3<-subset(descr_devices$Stand.density..Bitterlich., descr_devices$Location==3)


## volume of crown


## average height of trees



## variability in height of trees



## average vertical structure



## variability in vertical structure



## number of species



## which species






####################################
### Measured daily precipitation ###
####################################
prec<-read.csv("prec_devices.csv",header=TRUE,sep=";", dec=",")


## daily precipitation per device

daily_prec_dev<-subset(prec, prec$Precipitation..ml.>0)

# daily_prec_dev is subset of precipitation measurements for each day 
# for sampling devices without NA and without 0ml
# !!! CHANGE: include 0 ml but exclude NA

plot(daily_prec_dev$Precipitation..ml.~daily_prec_dev$Device, 
     col=as.factor(daily_prec_dev$Date))



## mean daily precipitation per site
?tapply
tapply(X=daily_prec_dev$Precipitation..ml., 
       INDEX=as.numeric(daily_prec_dev$Location), as.numeric(daily_prec_dev$Date), 
       FUN=mean)
# funktioniert noch nicht 


## mean precipitation per device over all measurement days




## daily precipitation in control devices




# Are there differences in control devices of each site? (canpoy cover correlation?)



## mean daily precipitation in control devices per site




## device prec. substracted with control device average per day of corresponding site




## daily fog precipitation per device



## mean daily fog precipitation per site



## mean fog precipitation per device over all measurement days





################################################################
### Correlations of vegetation parameters with precipitation ###
###############################################################

## with canopy cover


## with species


## with vertical structure --> Index???


# meterzählungen joscha


# tree hights




## with crown volume


## with forest type



## with altitude


## with observed weather conditions



######################################################################
### Precipitation captured by nets as validation of cloud presence ###
######################################################################
# As discussed with Anke we need to standardize somehow the potential fog precipitaion
# of each site and day to cheack whether the cloud presence/ density etc is comparable
# at a certain day between the different sites to exclude the possibility that 
# we captured differences of meso-meteorology instead of vegetation driven differences.


## Correlation between net density and fog precipitation?



## with exposition of nets
















