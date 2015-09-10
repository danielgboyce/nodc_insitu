############################################################
#writen by Daniel Boyce; January 1, 2011
#reads NODC  and WOOD .csv files, converts to flat file and writes as RData object; depends: plyr(); data.table()
#can extract >10M rows X 30 columns of data at a time
#works in R2.13 on 64 bit windows platform with 8GB RAM, 4 cores, 8 threads; parellizing will not improve
#possible to use ff() and bit() to use less resources
library(plyr)
library(data.table)

###########################################################
# function to extract WOOD chl data
getdata<-function(z){
#setwd('I://Data//chl//insitu//wood//csv_files')
setwd('C://Users//sailfish//Data//chl//insitu//wood//csv_files')
#    data<-read.table(paste(x),header=T,sep=paste(y))
#    data<-read.csv(paste(x),header=T,sep=paste(y))
    data<-read.csv('wood_chl_2010.csv',header=T)
    data2<-read.csv('wood_chl_20102.csv',header=T)
data<-rbind(data,data2)
rm(data2);gc()
names(data)<-c('Latitude','Longitude','CruiseID','N','Centwave','Year','Day','Time','TimeF','Ptype','Author','Cast','SensorID','Bathy','Dcode','Ledit','Lequal','Bandwidth','Vertres','Profile','Newhead','Datetime','Depth','Data')
data<-unique(data)
print(summary(data$Year))
gc()
data$mvar<-seq(from=1,to=length(data[,1]),by=1)

#function extracts numeric data from profiles
profiles<-subset(data,select=c('mvar','Data','Depth'))
gtprof<-function(dat){dat<-dat
#dat<-subset(profiles,mvar==1)
dat[,2]<-as.character(dat[,2])
ch<-unlist(strsplit(dat[,2], split=","));ch<-as.numeric(ch)
dat[,3]<-as.character(dat[,3])
dp<-unlist(strsplit(dat[,3], split=","));dp<-as.numeric(dp)

if (length(ch)==0 || length(dp)==0) dp<-NA else dp<-dp
if (length(ch)==0 | length(dp)==0) ch<-NA else ch<-ch
d<-data.frame(dat$mvar,dp,ch);names(d)<-c('mvar','Depth','Data')
return(d)
}
print(system.time(out<-dlply(profiles,.(mvar),.fun=gtprof)))
out2<-rbind.fill(out)
dat<-data.table(out2,key='mvar')
data<-data[,!(names(data) %in% c('Depth','Data'))]
data<-data.table(data,key='mvar')
dat<-merge(dat,data,by=c('mvar'),all=F)
rm(list=c('data','out2'));gc()
save(dat,file=paste(z))
print(summary(dat$Data))
}
getdata('wood_chl_2010.RData')
load('wood_chl_2010.RData')

x<-'I://Data//chl//insitu//nodc//csv_files//data_2011'
y<-'OSD.csv'
x<-'C://Users\\sailfish\\Data//chl//insitu//nodc//csv_files//data_2011'
x<-'C:\\Users\\sailfish\\Downloads\\nodc'
y<-'OSD.csv'

#######################################################################
#  functions to extract NODC data
getdata<-function(x,y,z,zz){
library(plyr)
library(data.table)
setwd(paste(x))
data<-read.csv(paste(y),header=F,fill=T,col.names=seq(from=1,to=17,by=1),na.strings="")
data<-data[,1:7]

##section adds unique variable to identify each profile
data$mvar<-seq(from=1,to=length(data$X1),by=1)#adds a column ('mvar') of numeric row identifiers so that I can later extract profiles
casts<-subset(data,X1=='#--------------------------------------------------------------------------------')#extracts all rows identifying the start of each individual profile
casts$profile<-seq(from=1,to=length(casts$mvar),by=1)#adds a numeric value for each cast ('profile')
casts<-casts[,-1]

#selects all rows where cast starts or ends
casts<-subset(data,X1=='#--------------------------------------------------------------------------------',select=c('mvar'))#extracts all rows iden
casts<-subset(casts,mvar!=max(casts$mvar))
names(casts)<-c('a')
casts2<-subset(data,X1=='#--------------------------------------------------------------------------------',select=c('mvar'))
casts2<-subset(casts2,mvar!=1)
names(casts2)<-c('b')
casts3<-cbind(casts,casts2)
casts3$n<-casts3$b-casts3$a
casts3$profile<-seq(from=1,to=length(casts3$a),by=1)

#function to add cast identifier to each row
fn<-function(dat){
    aa<-rep(dat$profile,dat$n)
    aa<-data.frame(aa)
    names(aa)<-c('profile')
aa$prac<-'.'
return(aa)
}
out<-dlply(casts3,.(profile),.fun=fn)
out2<-rbind.fill(out)
data<-data[1:length(out2$profile),]
data$profile<-out2$profile
rm(list=c('casts','casts2','casts3','fn','out','out2'))
gc()
#data<-subset(data,profile<=1000)


#########################################################################
#extracts numeric profile data
b<-data
b$X1<-as.character(b$X1)
b$X1<-as.numeric(b$X1)
b<-subset(b,is.na(X1)==FALSE,select=c('X2','X3','X5','X6','profile'))
names(b)<-c('Depth','Fdepth','data','Fdata','profile')
gc()
b<-subset(b,data!='**********')#symbol messes coding up, need to remove

#data2<-read.csv('probdat.csv',header=T)
#data3<-data
#data3$X3<-as.character(data3$X3)
#data3$X3<-as.numeric(data3$X3)
#data3<-subset(data3,X3 %in% unique(data2$CAST))
#b<-subset(b,profile %in% unique(data3$profile))
#data<-subset(data,profile %in% unique(b$profile))
#bb<-b

#load('OSD_OUT.RData')
#dat<-out2
#cst<-c( 13052201, 13052227, 13052291, 13052338, 13052365, 13052385, 13052386, 13052388)
#a<-subset(dat,CAST %in%cst)


write.csv(b,'C:\\Users\\sailfish\\Downloads\\prac.csv',row.names=F)
b<-read.csv('C:\\Users\\sailfish\\Downloads\\prac.csv',header=T)

#b<-subset(b,Fdata==0)
#bb<-subset(bb,Fdata==0)

#cor(as.numeric(bb$data),b$data)
#a<-data.frame(data=bb$data,data2=b$data,data3=as.numeric(as.character(bb$data)))

#f<-function(ff){ff<-gsub('\\.','',ff);ff<-as.numeric(gsub(' +','',ff))}
#if (paste(y) %in% c('SUR.csv','OSD.csv')) b$Depth<-f(b$Depth)
#b$Depth<-as.character(b$Depth);b$Depth<-as.numeric(b$Depth)
#print(b$Depth[1:50])
#print(summary(b$Depth))#just make sure subset on depth works
#b$Fdata<-as.character(b$Fdata);b$Fdata<-as.numeric(b$Fdata)
#b$Fdepth<-as.character(b$Fdepth);b$Fdepth<-as.numeric(b$Fdepth)
#print(b$data[1:10])
#b$data<-as.character(b$data);b$data<-as.numeric(b$data)
#print(b$data[1:10])
#b<-subset(b,Fdata==0 & Fdepth==0)#just take best data
gc()


#################################################
#extracts profile metadata
options(warn=-1)#turn warnings off
data<-subset(data,X1!='#--------------------------------------------------------------------------------',select=c('X1','X3','profile'))
m<-function(dat){
#dat<-subset(data,profile==65091)
dat<-dat

a<-as.character(t(dat[1:29,2]))
a<-gsub(' +','',a)
c<-data.frame(t(as.numeric(a)))

#creates headers
aa<-paste(as.character(t(dat[1:29,1])))
aa<-gsub(' NA','',aa)
aa<-gsub(' +','',aa)
names(c)<-c(as.vector(aa))
c$profile<-unique(dat$profile)
#dum<-c('X1','X2','X3','X4','X5','X6','X7','X8','X9','X10','X11','X12','NA.','NA.1','NA.2','NA.3','NA.4','NA.5','NA.6','NA.7','VARIABLES','UNITS','METADATA','Castduration','NODCCruiseID','Country','Bottomdepth','ENDOFVARIABLESSECTION','Cloudtype','WaveHeight','Weathercondition','Airtemperature.drybulb.','Airtemperature.wetbulb.','WindDirection','NA','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','Airtemperature(wetbulb)','Airtemperature(drybulb)','Wavedirection','Investigator.1','Investigator.2','Investigator.3','Investigator.4','Investigator.5','WavePeriod','BUTNOTAT','DiscreteWaterSampler','Cast/TowNumber','HighresolutionCTD-Bottle','Investigator.5','Investigator','Prof-Flag','Originatorsflagsettouse','Reference/Seasurfacetemper','Seasurfacesalinity','Castdirection','Salinometer!!DO','Incubationtime','Realtimedata','Filtertype/size','uncalibrated','BUTWITHRAINAND/ORSNOWATTIMEOFOBSER.-','Originalunits','BUTW/RAINAND/ORSNOWATTIME','Visibility','Windspeed','WindForce')
dum<-c('X1','X2','X3','X4','X5','X6','X7','X8','X9','X10','X11','X12','NA.','NA.1','NA.2','NA.3','NA.4','NA.5','NA.6','NA.7','VARIABLES','UNITS','METADATA','Castduration','Country','Bottomdepth','ENDOFVARIABLESSECTION','Cloudtype','WaveHeight','Weathercondition','Airtemperature.drybulb.','Airtemperature.wetbulb.','WindDirection','NA','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','Airtemperature(wetbulb)','Airtemperature(drybulb)','Wavedirection','Investigator.1','Investigator.2','Investigator.3','Investigator.4','Investigator.5','WavePeriod','BUTNOTAT','DiscreteWaterSampler','Cast/TowNumber','HighresolutionCTD-Bottle','Investigator.5','Investigator','Prof-Flag','Originatorsflagsettouse','Reference/Seasurfacetemper','Seasurfacesalinity','Castdirection','Salinometer!!DO','Incubationtime','Realtimedata','Filtertype/size','uncalibrated','BUTWITHRAINAND/ORSNOWATTIMEOFOBSER.-','Originalunits','BUTW/RAINAND/ORSNOWATTIME','Visibility','Windspeed','WindForce')
c<-c[,!(names(c) %in% dum)]
c$NODCCruiseID<-a[2]
return(c)
}
print(system.time(out<-dlply(data,.(profile),.fun=m)))
options(warn=0)#turn warnings back on
out2<-rbind.fill(out)#quick, memory efficient alternative to rbind
rm(list=c('out','data','m'));gc()
out2<-subset(out2,profile %in% unique(b$profile))

#relies on package data.table; makes merging way quicker and less resource intensive
b<-data.table(b,key='profile')
out2<-data.table(out2,key='profile')
gc()

system.time(out2<-merge(out2,b,by=c('profile'),all=F))
rm(b);gc()

out2$dataset<-rep(zz,length(out2$profile))
save(out2,file=paste(z))#can write as text file  but nukes RAM
#write.csv(out2,paste(z),row.names=F)
rm(out2);gc()
}



##############################################################
###############################################################
getsurdata<-function(x,y,z,zz){
library(plyr)
library(data.table)
setwd(paste(x))
data<-read.csv(paste(y),header=F,fill=T,col.names=seq(from=1,to=17,by=1),na.strings="")
data<-data[,1:11]

##section adds unique variable to identify each profile
data$mvar<-seq(from=1,to=length(data$X1),by=1)#adds a column ('mvar') of numeric row identifiers so that I can later extract profiles
casts<-subset(data,X1=='#--------------------------------------------------------------------------------')#extracts all rows identifying the start of each individual profile
casts$profile<-seq(from=1,to=length(casts$mvar),by=1)#adds a numeric value for each cast ('profile')
casts<-casts[,-1]

#selects all rows where cast starts or ends
casts<-subset(data,X1=='#--------------------------------------------------------------------------------',select=c('mvar'))#extracts all rows iden
casts<-subset(casts,mvar!=max(casts$mvar))
names(casts)<-c('a')
casts2<-subset(data,X1=='#--------------------------------------------------------------------------------',select=c('mvar'))
casts2<-subset(casts2,mvar!=1)
names(casts2)<-c('b')
casts3<-cbind(casts,casts2)
casts3$n<-casts3$b-casts3$a
casts3$profile<-seq(from=1,to=length(casts3$a),by=1)

#function to add cast identifier to each row
fn<-function(dat){
    aa<-rep(dat$profile,dat$n)
    aa<-data.frame(aa)
    names(aa)<-c('profile')
aa$prac<-'.'
return(aa)
}
out<-dlply(casts3,.(profile),.fun=fn)
out2<-rbind.fill(out)
data<-data[1:length(out2$profile),]
data$profile<-out2$profile
rm(list=c('casts','casts2','casts3','fn','out','out2'))
gc()
#data<-subset(data,profile<=1000)


#########################################################################
#extracts numeric profile data
b<-data
b$X1<-as.character(b$X1)
b$X1<-as.numeric(b$X1)
b<-subset(b,is.na(X1)==FALSE,select=c('X2','X3','X5','X6','X11','X8','profile'))
names(b)<-c('Depth','Fdepth','data','Fdata','Longitude','Latitude','profile')
gc()

write.csv(b,'C:\\Users\\sailfish\\Downloads\\prac.csv',row.names=F)
b<-read.csv('C:\\Users\\sailfish\\Downloads\\prac.csv',header=T)

#f<-function(ff){ff<-gsub('\\.','',ff);ff<-as.numeric(gsub(' +','',ff))}
#if (paste(y) %in% c('SUR.csv','OSD.csv')) b$Depth<-f(b$Depth)
#b$Depth<-as.character(b$Depth);b$Depth<-as.numeric(b$Depth)
b<-subset(b,Depth<=20)#just take data from upper 20m
print(summary(b$Depth))#just make sure subset on depth works
b$Fdata<-as.character(b$Fdata);b$Fdata<-as.numeric(b$Fdata)
b$Fdepth<-as.character(b$Fdepth);b$Fdepth<-as.numeric(b$Fdepth)
b$Latitude<-as.character(b$Latitude);b$Latitude<-as.numeric(b$Latitude)
b$Longitude<-as.character(b$Longitude);b$Longitude<-as.numeric(b$Longitude)
print(b$data[1:10])
b$data<-as.character(b$data);b$data<-as.numeric(b$data)
print(b$data[1:10])
#b<-subset(b,Fdata==0 & Fdepth==0)#just take data from upper 20m
gc()


#################################################
#extracts profile metadata
options(warn=-1)#turn warnings off
data<-subset(data,X1!='#--------------------------------------------------------------------------------',select=c('X1','X3','profile'))
m<-function(dat){
dat<-dat

a<-as.character(t(dat[1:29,2]))
a<-gsub(' +','',a)
c<-data.frame(t(as.numeric(a)))

#creates headers
aa<-paste(as.character(t(dat[1:29,1])))
aa<-gsub(' NA','',aa)
aa<-gsub(' +','',aa)
names(c)<-c(as.vector(aa))
c$profile<-unique(dat$profile)
dum<-c('X1','X2','X3','X4','X5','X6','X7','X8','X9','X10','X11','X12','NA.','NA.1','NA.2','NA.3','NA.4','NA.5','NA.6','NA.7','VARIABLES','UNITS','METADATA','Castduration','NODCCruiseID','Country','Bottomdepth','ENDOFVARIABLESSECTION','Cloudtype','WaveHeight','Weathercondition','Airtemperature.drybulb.','Airtemperature.wetbulb.','WindDirection','NA','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','Airtemperature(wetbulb)','Airtemperature(drybulb)','Wavedirection','Investigator.1','Investigator.2','Investigator.3','Investigator.4','Investigator.5','WavePeriod','BUTNOTAT','DiscreteWaterSampler','Cast/TowNumber','HighresolutionCTD-Bottle','Investigator.5','Investigator','Prof-Flag','Originatorsflagsettouse','Reference/Seasurfacetemper','Seasurfacesalinity','Castdirection','Salinometer!!DO','Incubationtime','Realtimedata','Filtertype/size','uncalibrated','BUTWITHRAINAND/ORSNOWATTIMEOFOBSER.-','Originalunits','BUTW/RAINAND/ORSNOWATTIME','Visibility','Windspeed','WindForce','Latitude','Longitude')
c<-c[,!(names(c) %in% dum)]
return(c)
}
print(system.time(out<-dlply(data,.(profile),.fun=m)))
options(warn=0)#turn warnings back on
out2<-rbind.fill(out)#quick, memory efficient alternative to rbind
rm(list=c('out','data','m'));gc()
out2<-subset(out2,profile %in% unique(b$profile))

#relies on package data.table; makes merging way quicker and less resource intensive
b<-data.table(b,key='profile')
out2<-data.table(out2,key='profile')
gc()

system.time(out2<-merge(out2,b,by=c('profile'),all=F))
rm(b);gc()

out2$dataset<-rep(zz,length(out2$profile))
save(out2,file=paste(z))#can write as text file  but nukes RAM
#write.csv(out2,paste(z),row.names=F)
rm(out2);gc()
}
system.time(getdata('C://Users//sailfish//Data//chl//insitu//nodc//csv_files//data_2011','OSD.csv','OSD_out.RData','OSD'))
system.time(getdata('C://Users//sailfish//Data//chl//insitu//nodc//csv_files//data_2011','CTD.csv','CTD_out.RData','CTD'))
system.time(getdata('C://Users//sailfish//Data//chl//insitu//nodc//csv_files//data_2011','CTD2.csv','CTD2_out.RData','CTD'))
system.time(getdata('C://Users//sailfish//Data//chl//insitu//nodc//csv_files//data_2011','CTD3.csv','CTD3_out.RData','CTD'))
system.time(getdata('C://Users//sailfish//Data//chl//insitu//nodc//csv_files//data_2011','UOR.csv','UOR_out.RData','UOR'))
system.time(getsurdata('I://Data//chl//insitu//nodc//csv_files//data_2011','SUR.csv','SUR_out.RData','SUR'))


system.time(getdata('C:\\Users\\sailfish\\Downloads\\nodc','OSD.csv','OSD_out.RData','OSD'))
system.time(getdata('C:\\Users\\sailfish\\Downloads\\nodc','CTD.csv','CTD_out.RData','CTD'))
system.time(getdata('C:\\Users\\sailfish\\Downloads\\nodc','CTD2.csv','CTD2_out.RData','CTD'))
system.time(getdata('C:\\Users\\sailfish\\Downloads\\nodc','CTD3.csv','CTD3_out.RData','CTD'))
system.time(getdata('C:\\Users\\sailfish\\Downloads\\nodc','UOR.csv','UOR_out.RData','UOR'))
system.time(getsurdata('C:\\Users\\sailfish\\Downloads\\nodc','SUR.csv','SUR_out.RData','SUR'))



load('OSD_out.RData')
load('CTD_out.RData')



system.time(getdata('C://Users//sailfish//Data//chl//insitu//nodc//csv_files//data_2011','OSD.csv','OSD_out.RData','OSD'))
system.time(getdata('C://Users//sailfish//Data//chl//insitu//nodc//csv_files//data_2011','CTD.csv','CTD_out.RData','CTD'))
system.time(getdata('C://Users//sailfish//Data//chl//insitu//nodc//csv_files//data_2011','CTD2.csv','CTD2_out.RData','CTD'))
system.time(getdata('C://Users//sailfish//Data//chl//insitu//nodc//csv_files//data_2011','CTD3.csv','CTD3_out.RData','CTD'))
system.time(getdata('C://Users//sailfish//Data//chl//insitu//nodc//csv_files//data_2011','UOR.csv','UOR_out.RData','UOR'))
system.time(getsurdata('C://Users//sailfish//Data//chl//insitu//nodc//csv_files//data_2011','SUR.csv','SUR_out.RData','SUR'))







library(RpgSQL)
con<-dbConnect(pgSQL(),user='postgres', password='phyto00',dbname='postgres')





















