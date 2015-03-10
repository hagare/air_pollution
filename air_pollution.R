#Given airpollution data in specdata dir create 
#function for mean of specfified pollutant #id=1:332
pollutantmean <- function(directory,pollutant,id) {
#directory location of file
filenames=list.files(directory) #list of files with airpollution data

#pollutant sulfate or nitrate
if(pollutant=="sulfate"){colnum=2} #column of sulfate
if(pollutant=="nitrate"){colnum=3} #column of nitrate
data.location=NA*id #vector for mean of each datafile
data.all=NA
#initializae necessary variables
mean.location=data.location
sd.location=data.location

#id location of monitor
for (i in id) { #loop thru list and retrieve mean of each location


#read in datafile
data=read.csv(sprintf("%s/%s",directory,filenames[i]),header=T)[,colnum]

#merge data into superdata set
data.all=c(data.all,data)

#Calculate mean and sd method 1
#mean.location[i]=mean(data,na.rm=T) #read.csv(sprintf("%s/%s",directory,filenames[i]))[,colnum],na.rm=T)
#sd.location[i]=sd(data,na.rm=T) #read.csv(sprintf("%s/%s",directory,filenames[i]))[,colnum],na.rm=T)
}

#Calculate mean and sd method 1
#mean.overall=mean(mean.location,na.rm=T)
#sd.overall=mean(sd.location,na.rm=T)

#Calculate mean and sd method 2
mean.all=mean(data.all,na.rm=T)
sd.all=sd(data.all,na.rm=T)

#function returns mean of pollutant across all monitor location 
#return(c(mean.overall,sd.overall,mean.all,sd.all))
return(mean.all)
}


