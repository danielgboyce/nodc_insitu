#reads in wood and nodc chl data (2011) and examines similarities of different chl datatypes
#set initial wds
setwd('C:\\Users\\sailfish\\Downloads\\nodc')
load('UOR_out.RData');uor<-out2;uor<-subset(uor,Depth<=20)
uorn<-uor

setwd('I://Data//chl//insitu//nodc//csv_files//data_2011')
setwd('E://Data//chl//insitu//nodc//csv_files//data_2011')
list.files()
load('CTD_out.RData');ctd1<-out2;ctd1<-subset(ctd1,Depth<=20);ctd1$dataset<-rep('CTD',length(ctd1$Year))
load('CTD2_out.RData');ctd2<-out2;ctd2<-subset(ctd2,Depth<=20);ctd2$dataset<-rep('CTD',length(ctd2$Year))
load('CTD3_out.RData');ctd3<-out2;ctd3<-subset(ctd3,Depth<=20);ctd3$dataset<-rep('CTD',length(ctd3$Year))
load('OSD_out.RData');osd<-out2;osd<-subset(osd,Depth<=20)
load('SUR_out.RData');sur<-out2;sur<-subset(sur,Depth<=20)
load('UOR_out.RData');uor<-out2;uor<-subset(uor,Depth<=20)


nodc_chl_20m<-rbind.fill(ctd1,ctd2,ctd3,osd,uor,sur)
save(nodc_chl_20m,file='nodc_chl_20m.RData')
rm(list=c('ctd1','ctd2','ctd3','osd','uor','sur'));gc()

####################################################################################
#############################   reads in wood chl data   ###########################
library(plotrix)
library("geneplotter")  ## from BioConductor
library(RColorBrewer)
library(IDPmisc)
library(gmt)
library(plyr)
library(smatr)
library(akima)
library(fossil)
library(mgcv)
library(smatr)
library(rtv)
library(plotrix)
library(fields)
wd<-'E://Data//chl//insitu//wood//csv_files'
wd2<-'C:\\Users\\diatom\\Documents\\scripts\\phytoplankton\\chl_compare\\1_4degree'

wd<-'I://Data//chl//insitu//wood//csv_files'
wd2<-'C:\\Users\\sailfish\\Documents\\scripts\\phytoplankton\\chl_compare\\1_4degree'

#loads WOOD data
setwd(wd)
load('wood_chl_2010.RData')
print(length(dat$Year))

data2<-subset(dat,Depth<=20)
rm(dat)

#subsets based on if data is from NODC or not
#data2<-subset(data2,(regexpr('NODC01',Header)>0)==TRUE | (regexpr('NODC05',Header)>0)==TRUE | (regexpr('NODC',Header)>0)==TRUE)
#data2<-subset(data2,(regexpr('NODC01',Newhead)>0)==FALSE & (regexpr('NODC05',Newhead)>0)==FALSE & (regexpr('NODC',Newhead)>0)==FALSE & (regexpr('Seabass',Newhead)>0)==FALSE & (regexpr('SeaBASS',Newhead)>0)==FALSE & (regexpr('seabass',Newhead)>0)==FALSE)#removes NODC and seabass; marlon says seabass data are not so bad

#a<-subset(data2,(regexpr('Seabass',Newhead)>0)==TRUE | (regexpr('SeaBASS',Newhead)>0)==TRUE | (regexpr('seabass',Newhead)>0)==TRUE)#extracts Seabass data:32538
data2<-subset(data2,(regexpr('NODC01',Newhead)>0)==FALSE & (regexpr('NODC05',Newhead)>0)==FALSE & (regexpr('NODC',Newhead)>0)==FALSE)#removes NODC only

#reads in wood metadata
setwd('I:/Data/metadata/wood')
setwd('E:/Data/metadata/wood')
authors<-read.table('author.codes.csv',sep=',',header=T)
names(authors)<-c('Author','Source')

cruises<-read.csv('cruise.id.codes.csv',header=T)
cruises<-subset(cruises,select=c('ID.','TITLE'))
names(cruises)<-c('CruiseID','Cruisedata')

sensor<-read.csv('instrument.codes.csv',header=T)
names(sensor)<-c('SensorID','Instrument')

data2<-merge(authors,data2,by='Author',all.y=T,all.x=F)
data2<-merge(cruises,data2,by='CruiseID',all.y=T,all.x=F)
data2<-merge(sensor,data2,by='SensorID',all.y=T,all.x=F)

#creates variable 'sensor' corresponding to major sensor types: fluorometer, HPLC
data2$Instrument<-as.character(data2$Instrument)
data2$sensor<-ifelse((regexpr("Fluor",data2$Instrument)>0)==TRUE, 'Fluorometer','Unknown')
data2$sensor<-ifelse((regexpr("fluor",data2$Instrument)>0)==TRUE, 'Fluorometer',data2$sensor)
data2$sensor<-ifelse((regexpr("HPLC",data2$Instrument)>0)==TRUE, 'HPLC',data2$sensor)
dum<-data.frame(Source=sort(unique(data2$Source)),AccessionNumber=seq(1,length(unique(data2$Source)),1))
data2<-merge(data2,dum,by=c('Source'),all=F)

dum<-data.frame(Instrument=sort(unique(data2$Instrument)),sensor2=seq(1,length(unique(data2$Instrument))))
data2<-merge(data2,dum,by=c('Instrument'),all.x=T,all.y=F)

#                                        Instrument sensor2
#                                    APL Fluorometer       1
#                             Calibrated Fluorometer       2
#                                               HPLC       3
# Laser fluorescence (e.g. LIDAR fluorosensor "ELF")       4
#                                SeaTech Fluorometer       5
#                          Un-Calibrated Fluorometer       6
#                                            Unknown       7
#         WETLabs ECO FL/NTU (CHL_a fluor/turbidity)       8

####################################
#reads in nodc chl data
wd<-'E://Data//chl//insitu//nodc//csv_files//data_2011'
wd2<-'C:\\Users\\diatom\\Documents\\scripts\\phytoplankton\\chl_compare\\1_4degree'

wd<-'I://Data//chl//insitu//nodc//csv_files//data_2011'
wd2<-'C:\\Users\\sailfish\\Documents\\scripts\\phytoplankton\\chl_compare\\1_4degree'
setwd(wd)
load('nodc_chl_20m.RData');data<-nodc_chl_20m
print(length(data$Year))
data<-subset(data,Depth>=0 & Fdata==0 & Fdepth==0 & Longitude> -181)#takes only highest quality nodc data
data<-data[,!(names(data) %in% c('Time','Fdepth','Fdata'))]

##reads in metadata
setwd('E://Data//metadata//nodc')
setwd('I://Data//metadata//nodc')
platform<-read.csv('nodc.platform.codes.csv',header=F,col.names=c('Platform','wodcode','platform.name'),fill=F)
platform<-subset(platform,is.na(Platform)==F,select=c('Platform','platform.name'))

inst<-read.csv('nodc.institution.codes.csv',header=F,col.names=c('Institute','wod','institution.name'),fill=T)
inst<-subset(inst,Institute!='#NAME?' & Institute!='- N/')
#data2<-merge(data,inst,by=c('Institute'),all.x=T,all.y=F)

instrument<-read.csv('nodc.instrument.codes.csv',header=F,col.names=c('Instrument','Instrumentname','Instrumentmore'),fill=T,skip=1)

#methods<-read.csv('nodc.methods.codes.csv',header=F,col.names=c('Measuremethod','Measurename','Measuremore'),skip=2,fill=T)#same as instrument

project<-read.csv('nodc.project.codes.csv',header=F,col.names=c('Project','projectshort','projectname'),fill=T)
project<-subset(project,Project %in% unique(data$Project))

#accnum<-read.csv('nodc.accession.codes.txt',header=F,col.names=c('AccessionNumber','date','dateentered','submitting.person','institution'),fill=F,skip=3)
#accnum<-subset(accnum,AccessionNumber %in% unique(data$AccessionNumber) & is.na(institution)==F,select=c('AccessionNumber','institution'))
#accnum<-data.table(accnum,key='AccessionNumber')
#data<-data.table(data,key='AccessionNumber')

#data<-merge(data,project,by=c('Project'),all.x=T,all.y=F) #need to get updated project tables
#data<-unique(data)
data<-merge(data,instrument,by=c('Instrument'),all.x=T,all.y=F)

#creates variable 'sensor' corresponding to major sensor types: fluorometer, HPLC
data$Instrumentmore<-as.character(data$Instrumentmore)
data$sensor<-ifelse((regexpr("Fluor",data$Instrumentmore)>0)==TRUE, 'Fluorometer','Unknown')
data$sensor<-ifelse((regexpr("HPLC",data$Instrumentmore)>0)==TRUE, 'HPLC',data$sensor)
data$sensor<-ifelse(is.na(data$Instrumentmore), 'Unknown',data$sensor)

dum<-data.frame(Instrumentmore=sort(unique(data$Instrumentmore)),sensor2=seq(2,length(unique(data$Instrumentmore))))
data<-merge(data,dum,by=c('Instrumentmore'),all.x=T,all.y=F)

data$sensor3<-data$Instrumentname

rm(list=c('nodc_chl_20m','platform','latref.25','lonref.25','cruises','latref','lonref','methods','coords','authors','accnum','sensor','inst','instrument'))
gc()

data<-data.frame(CAST=data$CAST,OriginatorsStationID=data$OriginatorsStationID,Latitude=data$Latitude,Longitude=data$Longitude,Year=data$Year,Month=data$Month,Dayofmonth=data$Day,SeaState=data$SeaState,Barometricpressure=data$Barometricpressure,Watercolor=data$Watercolor,Instrument1=data$Instrumentname,Instrument2=data$Instrumentmore,Sensor=data$sensor,Sensor2=data$sensor2,Sensor3=data$sensor3,Secchi=data$Secchidiskdepth,Depth=data$Depth,Data=data$data,dataset=data$dataset,AccessionNumber=data$AccessionNumber,Profile=data$profile)
data$Extracted<-rep('NODC',length(data$Data))
data$Day<-date2doy(data$Year,data$Month,data$Dayofmonth)#calculates day of the year from month, year, and day of month
data<-subset(data,Day!=0)

data2<-data.frame(CAST=data2$Profile,Instrument1=data2$Instrument,Cruisedata=data2$Cruisedata,Source=data2$Source,Depth=data2$Depth,Data=data2$Data,Latitude=data2$Latitude,Longitude=data2$Longitude,N=data2$N,Year=data2$Year,Day=data2$Day,Bathy=data2$Bathy,Newhead=data2$Newhead,Sensor=data2$sensor,Sensor2=data2$sensor2,AccessionNumber=data2$AccessionNumber,Profile=data2$Profile)
data2$Extracted<-rep('WOOD',length(data2$Data))
a<-doy2date(data2$Year,data2$Day)#calculates day of the month from year and day of year
data2$Dayofmonth<-a$day
data2$Month<-a$month

data<-rbind.fill(data,data2);gc()

data<-subset(data,Latitude<=90 & Dayofmonth!=32 & Longitude>= -180 & Dayofmonth!=0)


#checks to make sure casts originate from either NODC or WOOD; takes awhile to run
#summary(tapply(data$dataset,data$CAST,function(x) length(unique(x))))

#makes sure casts from nodc and wood dont' overlap
a<-subset(data,Extracted=='WOOD')
b<-subset(data,Extracted=='NODC')
a$CAST<-a$CAST+(max(b$CAST))
data<-rbind(a,b)

#####adds geographic bins
#bins for 1 degrees
lat<-ifelse(data$Latitude== -90, -89.99,data$Latitude)
lon<-ifelse(data$Longitude== -180, -179.99,data$Longitude)
latref<-floor((lat - (90 + 1))/1*-1);
lonref<-floor((lon - (180 + 1))/1*-1);
data$newarea1<-format(latref + (lonref*1000),nsmall=0)

#bins for .25 degrees
latref<-floor((lat - (90 + 1))/1*-2);
lonref<-floor((lon - (180 + 1))/1*-2);
data$newarea.25<-latref + (lonref*1000)

#reads in geographic bins for .25 degrees and merges
setwd(wd2)
coords<-read.table('coords.25',header=F,col.names=c('lon','lat'))
latref.25<-floor((coords$lat - (90 + 1))/1*-2);
lonref.25<-floor((coords$lon - (180 + 1))/1*-2);
coords$newarea.25<-latref.25 + (lonref.25*1000)

#data2<-merge(coords,data,by=c('newarea.25'),all=F)
data<-merge(coords,data,by=c('newarea.25'),all=F)
gc()

data$yr.mnth<-format((data$Month/100)+data$Year,nsmall=2)
data$dataset<-ifelse(is.na(data$dataset)==T,'WOOD',as.character(data$dataset))
data$dataset<-as.factor(data$dataset)


#########################################################
#########################################################
#calculates means for each cast - just to compare rough accuracy of chl measurement types
#function makes sure that cast variables aren't duplicated among datasets
data<-subset(data,dataset!='SUR' & dataset!='UOR')#removes surface and underway data
rm(list=c('a','coords','data2','dum','lat','latref','latref.25','lonref','lonref.25','project'));gc()

fn<-function(dat){
zeros<-subset(dat,Data==0)
return(c(CAST=unique(dat$CAST),n=length(dat$Data),nzeros=length(zeros$Data),prop=length(zeros$Data)/length(dat$Data),accnum=unique(dat$AccessionNumber)))
}
out<-dlply(data,.(CAST),.fun=fn)
out2<-data.frame(do.call('rbind',out))
data<-merge(data,out2,by=c('CAST'),all=F)


#function adds flag if any values in cast are outside range values
fn<-function(dat){
mn<-min(dat$Data)
mx<-max(dat$Data)
if (mn<=0) fcast<-1 else fcast<-0
if (mx>60) fcast<-1 else fcast<-fcast
return(c(CAST=unique(dat$CAST),fcast=fcast))
}
out<-dlply(data,.(CAST),.fun=fn)
out2<-data.frame(do.call('rbind',out))
data<-merge(data,out2,by=c('CAST'),all=F)

##############################################################
##############################################################
#function adjusts 0-chl readings
data2<-subset(data,prop==0)#extracts all casts containing no 0-chl measurements
data2$flag<-rep(0,length(data2$Year))
data<-subset(data,prop!=0 & prop!=1)#extracts all casts containing some 0-chl measurements, but eliminates casts where all measurements are 0;removes 15205 data

fn<-function(dat){
#dat<-subset(data,CAST==3339621)
print(unique(dat$CAST))
dat<-subset(dat,Data<60 & Data>=0)
zeros<-subset(dat,Data==0)#subsets all 0-chl measurements
dat2<-subset(dat,Data!=0)#subsets all non-0-chl measurements
dat2$flag<-rep(0,length(dat2$Year))
n<-length(dat$Data)#number of measurements in cast
prop<-length(zeros$Data)/length(dat$Data)#proportion of 0-chl readings in cast

#interpolates values in cast; if 0-chl values are within confidence intervals then sets a value, otherwise sets to -9
dpfn1<-function(){
mn<-sort(unique(dat$Data),decreasing=F)[2]
mod<-gam(Data ~ s(Depth,k=5),family=gaussian(link='identity'),data=dat)#fits interpolating model to all data in case
p<-predict(mod,newdata=data.frame(Depth=zeros$Depth),se.fit=T)
zeros2<-data.frame(Depth=zeros$Depth,Data=zeros$Data,Datap=p$fit,Datase=p$se,ciup=p$fit+(1.96*p$se),cidn=p$fit-(1.96*p$se))
zeros2$flag<-ifelse(zeros2$Data<= zeros2$ciup & zeros2$Data >=zeros2$cidn | mn<=.05,0,1)#if data is outside CI of model or if min value in cast is larger than .05 then sets flag to 1
zeros$flag<-zeros2$flag
zeros$Data<-ifelse(min(zeros$flag)==0,min(dat2$Data)/2,-9)#if any data among the zeros in the cast are flagged then all zeros are set to -9

zs<-subset(dat,Data==0)
plot(dat$Depth,dat$Data,pch=16)
points(zs$Depth,zs$Data,pch=16,col='red')
points(zeros$Depth,zeros$Data)
d<-seq(min(dat$Depth),max(dat$Depth),.1)
p<-predict(mod,newdata=data.frame(Depth=d),se.fit=T)
lines(d,p$fit,col='red')
#lines(d,p$fit+(1.645*p$se),lty=2)#90% CI
#lines(d,p$fit-(1.645*p$se),lty=2)
lines(d,p$fit+(1.96*p$se),lty=2)#95% CI
lines(d,p$fit-(1.96*p$se),lty=2)
return(zeros)
}

#if cast has <6 data, uses different algorithm to determine non-zeros; if min value in cast is <.05 then sets a value, otherwise sets value to -9
dpfn2<-function(){
mn<-sort(unique(dat$Data),decreasing=F)[2]#establishes minimum non 0 value for cast
zeros$Data<-ifelse(mn<.05,mn/2,zeros$Data)#if are >2 data, and min chl is <.05, set chl to 50% of min chl value in cast
zeros$Data<-ifelse(mn>.05,-9,zeros$Data)#if are >2 data, and min chl is >.05, set chl to -9
zeros$flag<-ifelse(mn>.05,1,0)#if are >2 data, and min chl is >.05, set flag to 1

zs<-subset(dat,Data==0)
plot(dat$Depth,dat$Data,pch=16)
points(zs$Depth,zs$Data,pch=16,col='red')
points(zeros$Depth,zeros$Data)
mtext(unique(dat$Sensor),side=3,line=-2,outer=F)
return(zeros)
}

ifelse(n>=5,zeros<-dpfn1(),zeros<-dpfn2())
dato<-rbind(zeros,dat2)
}
setwd(wd)
postscript('zero_removal_plots.ps')
par(mfrow=c(4,4),mar=c(2,2,1,1))
out<-dlply(data,.(CAST),.fun=fn)
dev.off()
out2<-data.frame(do.call('rbind',out))

data3<-rbind(data2,out2)
data<-subset(data3,Data!=-9 & Data!=0)#removes 1586 problematic chl readings
rm(list=c('data2','data3'));gc()


#wd<-'E://Data//chl//insitu//nodc//csv_files//data_2011'
wd<-'I://Data//chl//insitu//nodc//csv_files//data_2011'
setwd(wd)
save(data,file='data_merged.RData')
write.csv(data,'data_merged.csv',row.names=F)
data<-read.csv('data_merged.csv')
load('data_merged.RData')


##########################################################################
###########################################################################
###function to compare NODC datatypes(SUR,UOR,CTD,OSD): several SUR values are repeated in OSD so eliminated
#depth interpolation to 10m depth; uses mean for <2, quadratic for <4 & >2, spline(k=6) for >4
#stinterp in stinepack package also interpolates well; ref: stineman 1980

#27% of <=0 chl readings from same accnum; 6% of all data from this accnum; most zeroes in same location: off coast of NFLD in spring(may), in upper 5m-unlikely
#a<-subset(data,AccessionNumber==2023)#accnum=0002023, 9/15/2004, 3/10/2005,SPEAR; DONALD,INTEGRATED SCIENCE DATA MANAGEMENT (ISDM); Former MEDS

#datsur<-subset(data,dataset=='SUR')#SUR contains many data repeated in OSD and are highly variable
#datsur<-data.frame(newarea.25=datsur$newarea.25,lon=datsur$lon,lat=datsur$lat,CAST=datsur$CAST,Year=datsur$Year,Month=datsur$Month,Day=datsur$Day,Data=datsur$Data,Depth=datsur$Depth,dataset=datsur$dataset,yr.mnth=datsur$yr.mnth,interp=0,n=1,Longitude=datsur$Longitude,Latitude=datsur$Latitude,Sensor=datsur$Sensor)

data<-subset(data,Data>0 & Data<70)#removes 9 data

#lower chl detection limit: .2-.5 for spectrometer Wiltshire et al. J. Exp. Mar. Biol. Ecol., 1998
intfun<-function(dat){
#dat<-subset(data,CAST==240275)
print(unique(dat$CAST))

n=length(dat$Data)#number of points in cast
if (length(unique(dat$Depth))<6) k<-4 else k<-6#knots for gam interpolant
zz<-(min(dat$Depth)+ max(dat$Depth))/2#sets average depth
un<-length(unique(dat$Data))
un2<-length(unique(dat$Depth))
Datamean<-mean(dat$Data)
Datamedian<-median(dat$Data)

ak<-function(dd){int<-aspline(dd$Depth,dd$Data,xout=10,method='improved')
    return(int$y)}

gm<-function(dd){mod<-gam(Data~s(Depth,bs='cs',k=k),family=gaussian('identity'),gamma=1.2,data=dd)
p<-predict(mod,newdata=data.frame(Depth=zz),type='response',se.fit=T)
return(c(p$fit,p$se))}

qd<-function(dd){mod<-lm(Data ~ Depth + I(Depth^2),data=dd)
p<-predict(mod,newdata=data.frame(Depth=zz),se.fit=T)
return(c(p$fit,p$se))}

#weighted mean function; weights based on how far from 10m the measurements are
mn<-function(dd){
dd$dp.weight<-(abs((11^2) - (((10-dd$Depth))^2))/abs((11^2) + (((10-dd$Depth))^2)))
dd$Data<-(dd$dp.weight*dd$Data)/sum(dd$dp.weight)
mn<-mean(dd$Data)
msd<-sd(dd$Data)
return(c(mn,msd))}

#interpolates based on amount of measurements in cast; m specifies the interpolant used
if (un<=3) z<-mn(dat) else if (un>3 & un<=6) z<-qd(dat) else z<-gm(dat)#
if (n==1) z[2]<-0#if only 1 unique value in cast, set SD to 0
#if (z[1]<0 & un>1) z<-mn(dat) else z<-z
#if interpolated value is outside range of raw data then takes max/min value of cast
if(z[1]< min(dat$Data)) z[1]<-min(dat$Data) else z<-z
#if(z[1]> max(dat$Data)) z[1]<-max(dat$Data) else z<-z
#if(z[1]<.005) z<-mn(dat) else z<-z#if interpolated value is <.005, defaults to raw mean
#if(z[1]<.005) z[1]<-min(dat$Data) else z<-z
if(z[1]> max(dat$Data)) z<-mn(dat) else z<-z

#outputs variable to identify type of interpolation used
if (un<=3) m<-1 else if (un>3 & un<=6) m<-2 else if (un>6) m<-3 else m<-0
if (z[1]<0) m<-1 else m<-m

#plots out chosen interpolations
gm2<-function(dd){d<-seq(from=min(dd$Depth),to=max(dd$Depth),by=.1)
    mod<-gam(Data~s(Depth,bs='cs',k=k),family=gaussian('identity'),gamma=1.2,data=dd)
    mod2<-gam(Data~s(Depth,bs='cs',k=4),family=gaussian('identity'),gamma=1.2,data=dd)
    mod3<-lm(Data ~ Depth + I(Depth^2),data=dd)
    int<-aspline(dd$Depth,dd$Data,xout=d,method='improved',degree=1)
plot(dd$Depth,dd$Data,pch=16,ylim=c(0,max(dd$Data)),cex=.5);abline(h=0,lty=2)
mtext(paste('Cast=',unique(dat$CAST)), side=3,outer=F,adj=0,cex=.5)
mtext(paste('Method=',unique(dat$Sensor)), side=3,outer=F,adj=1,cex=.5)
points(zz,Datamean,pch=1,col='red')
points(zz,Datamedian,pch=1,col='blue')
points(zz,z[1],pch=1)
lines(d,(predict(mod,newdata=data.frame(Depth=d),type='response')),col='green')
lines(d,(predict(mod2,newdata=data.frame(Depth=d),type='response')),col='red')
lines(d,(predict(mod3,newdata=data.frame(Depth=d))),col='blue')
#lines(d,int$y)#plots akima interpolant, not used
}

qd2<-function(dd){d<-seq(from=min(dd$Depth),to=max(dd$Depth),by=.1)
    mod<-lm(Data ~ Depth + I(Depth^2),data=dd)
    int<-aspline(dd$Depth,dd$Data,xout=d,method='improved',degree=1)
plot(dd$Depth,dd$Data,pch=16,ylim=c(0,max(dd$Data)),cex=.5);abline(h=0,lty=2)
mtext(paste('Cast=',unique(dat$CAST)), side=3,outer=F,adj=0,cex=.5)
mtext(paste('Method=',unique(dat$Sensor)), side=3,outer=F,adj=1,cex=.5)
points(zz,Datamean,pch=1,col='red')
points(zz,Datamedian,pch=1,col='blue')
points(zz,z[1],pch=1)
lines(d,(predict(mod,newdata=data.frame(Depth=d))),col='blue')}

#if (un>3 & un<=6) qd2(dat) else if (un>6) gm2(dat)
#if (is.na(z[2])==T) print(unique(dat$CAST))

return(data.frame(newarea.25=unique(dat$newarea.25),lon=unique(dat$lon),lat=unique(dat$lat),CAST=unique(dat$CAST),Year=unique(dat$Year),Month=unique(dat$Month),Day=unique(dat$Day),Data=z[1],Data.se=z[2],Depth=zz,dataset=unique(dat$dataset),yr.mnth=unique(dat$yr.mnth),interp=m,n=n,un=un,Longitude=unique(dat$Longitude),Latitude=unique(dat$Latitude),Sensor=unique(dat$Sensor),Accnum=unique(dat$AccessionNumber),Dayofmonth=unique(dat$Dayofmonth),Datamean=Datamean,Datamedian=Datamedian,Dataset=unique(dat$dataset),Extracted=unique(dat$Extracted),Sensor2=unique(dat$Sensor2),Sensor3=unique(dat$Sensor3),fcast=unique(dat$fcast)))
}
setwd('C:\\Users\\sailfish\\Data\\chl\\insitu\\nodc\\csv_files\\data_2011')
setwd('E:\\Data\\chl\\insitu\\nodc\\csv_files\\data_2011')
pdf('cast.interpolation.pdf')
par(mfrow=c(4,4),mar=c(2,2,1,1))
system.time(out<-ddply(data,.(CAST),.fun=intfun))
dev.off()
data<-out
gc()

save(data,file='data_interp.RData')
setwd('I:\\Data\\chl\\insitu\\nodc\\csv_files\\data_2011')
load('data_interp.RData')


###adds bathymetry and coastal distance
coords<-subset(data,select=c('Longitude','Latitude'))
coords<-unique(coords)
setwd(wd)
r2gmt(coords,datafile='coords.insitu.2011.txt')

bathy<-read.table('insitu.2011.bathy.txt',header=F,sep='',col.names=c('Longitude','Latitude','Bathy'))
dist<-read.table('insitu.2011.distance.txt',header=F,sep='',col.names=c('Longitude','Latitude','Dist','lon','lat'))
dist<-subset(dist,Longitude %in% coords$Longitude & Latitude %in% coords$Latitude,select=c('Longitude','Latitude','Dist'))

data<-merge(data,bathy,by=c('Longitude','Latitude'),all.x=T,all.y=F)
data<-merge(data,dist,by=c('Longitude','Latitude'),all.x=T,all.y=F)


#removes duplicates after cast interpolation: reduces that chance to removing non-duplicates by accident
#to accurately detect duplicates, need to first make sure precision is standardized for all variables, then merge back with original data to recover precision
data$mvar<-seq(1,length(data$Year),1)
data2<-data.frame(Latitude=round(data$Latitude,2),Longitude=round(data$Longitude,2),Year=data$Year,Month=round(data$Month,0),Day=data$Day,Data=round(data$Data,2),Depth=round(data$Depth,0),mvar=data$mvar,Dayofmonth=data$Dayofmonth)
data2<-data2[!(duplicated(data2[,c('Latitude','Longitude','Year','Month','Day','Dayofmonth','Data','Depth')])),]#~31478 rows duplicated
mvar<-subset(data2,select=c('mvar'))
data<-merge(data,mvar,by=c('mvar'),all=F)#merges back so that highest precision is taken, but duplicates are removed
rm(data1,data2);gc()

#yr.mnth variable comes out strangely formatted; this fixes it
data$yr.mnth<-as.character(data$yr.mnth);data$yr.mnth<-as.numeric(data$yr.mnth)
data<-subset(data,Bathy<= -20 & Dist>1)#removes nearshore measurements


#############################################################
#############################################################
#removes outlying CTD from database based on comparing to insitu prior to fitting spatial surface

#bins for 1 degrees
latref<-floor((data$Latitude - (90 + 1))/1*-1);
lonref<-floor((data$Longitude - (180 + 1))/1*-1);
data$newarea1<-latref + (lonref*1000)
data$cell<-latref + (lonref*1000)

a<-subset(data,dataset=='CTD')
b<-subset(data,dataset!='CTD')
dat<-merge(a,b,by=c('newarea1','yr.mnth'),all=F)


#makes figure showing relationship between osd and ctd with cutoff values selected#cl<-colorRampPalette(brewer.pal(11,"YlOrRd"))
cl<-colorRampPalette(c('cornflowerblue','yellow','orange','red','darkred'))
postscript('ctd.vs.osd.ps')
par(oma=c(3,5,1,5))
plot(0,0,xlim=c(-8,4),ylim=c(-8,4),pch=16,cex=.05,xlab='',ylab='')
ipanel.smooth(log(dat$Data.x),log(dat$Data.y),pixs=1,colramp=cl,col=NA)
abline(a=0,b=1)
gradient.rect(-1,-6.5,3.5,-7,col=(c('cornflowerblue','yellow','orange','red','darkred')),gradient='x',border='black',nslices=10)
#p<-subset(dat,abs(log(Data.y)-log(Data.x))>2.25 | abs((Data.y)-(Data.x))>7)#removes extreme outliers on log scale and narrower band of outliers on raw scale
#points(log(p$Data.x),log(p$Data.y),pch=16,cex=.5,col='red')
x<-seq(min(dat$Data.x),max(dat$Data.x),length.out=1000)
y<-seq(min(dat$Data.x),max(dat$Data.x),length.out=1000)
y2<-log(y)+2.25;y3<-log((y)+7)
up<-data.frame(x=rep(x,2),y=c(y2,y3))
t<-tapply(up$y,up$x,function(x) min(x))
lines((sort(unique(log(up$x)))),(t),lty=2)
x<-seq(min(dat$Data.x),max(dat$Data.x),length.out=1000)
y<-seq(min(dat$Data.x),max(dat$Data.x),length.out=1000)
y2<-log(y)-2.25;y3<-log((y)-7)
dn<-data.frame(x=rep(x,2),y=c(y2,y3))
dn<-na.omit(dn)
t<-tapply(dn$y,dn$x,function(x) max(x))
lines((sort(unique(log(dn$x)))),(t),lty=2)
dev.off()
##this does the actual removal of outlying CTD observations
p<-subset(dat,abs(log(Data.y)-log(Data.x))>2.25 | abs((Data.y)-(Data.x))>7)#removes extreme outliers on log scale and narrower band of outliers on raw scale
data$o<-ifelse(data$CAST %in% unique(p$CAST.x),1,0)#removes all CTD data which are identified as outliers = 971 CTD measurements
data<-subset(data,o!=1)


a<-subset(data,dataset=='CTD')
b<-subset(data,dataset!='CTD')

#setwd('C:\\Users\\sailfish\\Documents\\scripts\\phytoplankton\\chl_compare\\1_4degree')
#load('seawifs2.Rdata')#has swfs data to 2010
#seawifs<-subset(seawifs2,Chl<=50)
#seawifs$yr.mnth<-as.character(seawifs$yr.mnth)
#seawifs$yr.mnth<-as.numeric(seawifs$yr.mnth)

#filters ctd data by comparing against czcs and seawifs; more conservative then comparisons against insitu
cl<-colorRampPalette(c('cornflowerblue','yellow','orange','red','darkred'))
postscript('ctd.vs.swfs.ps')
a<-subset(data,dataset=='CTD')
dat<-merge(a,seawifs,by=c('cell','yr.mnth'),all=F)
cl<-colorRampPalette(c('cornflowerblue','yellow','orange','red','darkred'))
par(oma=c(3,5,1,5))
plot(0,0,xlim=c(-4,4),ylim=c(-4,4),pch=16,cex=.05,xlab='',ylab='')
ipanel.smooth(log(dat$Chl),log(dat$Data),pixs=1.2,colramp=cl,col=NA)
abline(a=0,b=1)
gradient.rect(1,-3.5,3.5,-3.75,col=(c('cornflowerblue','yellow','orange','red','darkred')),gradient='x',border='black',nslices=10)

x<-seq(min(dat$Data),max(dat$Chl),length.out=1000)
y<-seq(min(dat$Data),max(dat$Chl),length.out=1000)
y2<-log(y)+3.75;y3<-log((y)+25)
up<-data.frame(x=rep(x,2),y=c(y2,y3))
t<-tapply(up$y,up$x,function(x) min(x))
lines((sort(unique(log(up$x)))),(t),lty=2)
x<-seq(min(dat$Data),max(dat$Chl),length.out=1000)
y<-seq(min(dat$Data),max(dat$Chl),length.out=1000)
y2<-log(y)-3.75;y3<-log((y)-25)
dn<-data.frame(x=rep(x,2),y=c(y2,y3))
dn<-na.omit(dn)
t<-tapply(dn$y,dn$x,function(x) max(x))
lines((sort(unique(log(dn$x)))),(t),lty=2)
dev.off()


#not a good idea to filter using swfs because relationship does not appear to be 1:1 so filtering may be inaccurate
p<-subset(dat,abs(log(Chl)-log(Data))>3 | abs((Chl)-(Data))>20)#removes extreme outliers on log scale and narrower band of outliers on raw scale; more conservatie for swfs=can't verify accuracy
data$o<-ifelse(data$CAST %in% unique(p$CAST),1,0)#removes all CTD data which are identified as outliers = 504 CTD measurements
data<-subset(data,o!=1)
a<-subset(data,o==1)#748

setwd("C:\\Users\\sailfish\\Data\\chl/insitu\\nodc\\csv_files\\data_2011")
save(data,file='data.spatially.filtered3.RData')

#################################################################
#################################################################
#spatial filters
season=data.frame(Month=seq(1,12,by=1),Season=c(1,1,2,2,2,3,3,3,4,4,4,1))
data<-merge(data,season,by=c('Month'),all=F)

fn<-function(a,b,z){
    dat<-subset(data,Season==a)
system.time(mod<-gam(log(Data)~ s(Longitude,Latitude,k=1200,bs='ts'),family=gaussian('identity'),data=dat))
vis.gam(mod,view=c('Longitude','Latitude'),plot.type='contour',n.grid=140,contour.col='darkgray',main=b)
map('world',add=T,fill=T,col='black')
box()
p<-predict(mod,newdata=data.frame(Longitude=dat$Longitude,Latitude=dat$Latitude),se.fit=T)
dat$pred.s<-exp(p$fit)
dat$se.s<-exp(p$se.fit)
save(mod,file=paste(z))
return(dat)
}
pdf('spatial.filter.pdf')
par(mfrow=c(2,2),mar=c(3,3,1,1),oma=c(1,1,1,1))
dat1<-fn(1,'Winter','wintermod.RData')
dat2<-fn(2,'Spring','springmod.RData')
dat3<-fn(3,'Summer','summermod.RData')
dat4<-fn(4,'Autumn','fallmod.RData')
dev.off()
data<-rbind(dat1,dat2,dat3,dat4)
ct<-12
data$flag<-ifelse(data$Data<(data$pred.s+(ct*data$se.s)) & data$Data> (data$pred.s-(ct*data$se.s)),0,1)
data<-subset(data,flag==0)#removes 1788 observations
setwd('C:\\Users\\sailfish\\Data\\chl\\insitu\\nodc\\csv_files\\data_2011')
save(data,file='data.spatially.filtered.RData')
#load('data.spatially.filtered.RData')

a<-subset(data,flag==1)#removes 217 observations


###############################################
#makes figure showing spatially filtered data
load('wintermod.RData');mod1<-mod
load('springmod.RData');mod2<-mod
load('summermod.RData');mod3<-mod
load('fallmod.RData');mod4<-mod
cl<-palette(c('royalblue','cornflowerblue','yellow2','orange','orangered','darkred'))
gradient.rect(-150,-40,0,-50,color='heat',gradient='x')

setwd('C:\\Users\\sailfish\\Documents\\literature\\manuscripts\\phytoplankton\\chl_compare\\figures\\figures')
postscript('spatial.filters.ps')
par(mfrow=c(2,2),mar=c(2,2,1,1),oma=c(8,2,8,2))
vis.gam(mod1,view=c('Longitude','Latitude'),plot.type='contour',n.grid=40,contour.col='grey20',main='Winter',color='terrain')
map('world',add=T,fill=T,col='black')
box()
vis.gam(mod2,view=c('Longitude','Latitude'),plot.type='contour',n.grid=40,contour.col='grey20',main='Spring',color='terrain')
map('world',add=T,fill=T,col='black')
box()
vis.gam(mod3,view=c('Longitude','Latitude'),plot.type='contour',n.grid=40,contour.col='grey20',main='Summer',color='terrain')
map('world',add=T,fill=T,col='black')
box()
vis.gam(mod4,view=c('Longitude','Latitude'),plot.type='contour',n.grid=40,contour.col='grey20',main='Fall',color='terrain')
map('world',add=T,fill=T,col='black')
box()
dev.off()


##############################################################################
##########                reads in remote sensing data
setwd('C:\\Users\\diatom\\Documents\\scripts\\phytoplankton\\chl_compare\\1_4degree')
setwd('C:\\Users\\sailfish\\Documents\\scripts\\phytoplankton\\chl_compare\\1_4degree')
czcs<-read.table('cscs.csv',header=T,sep=",")
names(czcs)<-c('x','Longitude','Latitude','Chl','Year','Month','newarea.25','yr.mnth')
czcs$yr.mnth<-as.factor(czcs$yr.mnth)
seawifs<-read.table('seawifs.csv',header=T,sep=",")
names(seawifs)<-c('Longitude','Latitude','Chl','Year','Month','Bathy','Dist','newarea.25','yr.mnth')
seawifs$yr.mnth<-as.factor(seawifs$yr.mnth)
load('czcs.1deg.Rdata')
load('swfs.1deg.Rdata')


data2<-subset(data,Sensor!='Fluorometer')#all unknown
data2<-subset(data,dataset!='CTD' & dataset!='UOR' & Bathy< -20)#all unknown
data2<-subset(data,Bathy< -20 | Dist>1)#all unknown

dat<-merge(data,czcs,by=c('yr.mnth','newarea.25'),all=F)
dat<-merge(data2,seawifs,by=c('Year','Month','newarea.25'),all=F)
plot(log(dat$Data),log(dat$Chl),pch=16,cex=.5)
points(log(dat$Data),log(dat$Chl),pch=16,cex=.5,col='red')

a<-subset(dat,abs(log(dat$Chl)-log(dat$Data))>2)
points(log(a$Data),log(a$Chl),pch=16,cex=.5,col='red')





f<-function(dat){
    t<-tapply(dat$Data,dat$yr.mnth,mean)
    dum<-data.frame(newarea25=unique(dat$newarea.25),yr.mnth=sort(unique(dat$yr.mnth)),data=unlist(t))
return(dum)}
ctd<-data.frame(do.call('rbind',(dlply(subset(data,dataset=='CTD'),.(newarea.25),.fun=f))))
osd<-data.frame(do.call('rbind',(dlply(subset(data,dataset=='OSD'),.(newarea.25),.fun=f))))
uor<-data.frame(do.call('rbind',(dlply(subset(data,dataset=='UOR'),.(newarea.25),.fun=f))))
























