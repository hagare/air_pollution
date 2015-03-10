#Given airpollution data in specdata dir create 
## function 1 for mean of specfified pollutant #id=1:332
pollutantmean <- function(directory="specdata",pollutant,id=1:332) {
#directory location of file
filenames=list.files(directory) #list of files with airpollution data

#pollutant sulfate or nitrate
if(pollutant=="sulfate"){colnum=2} #column of sulfate
if(pollutant=="nitrate"){colnum=3} #column of nitrate
data.location=NA*c(1:332) #vector for mean of each datafile
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
mean.location[i]=mean(data,na.rm=T) #read.csv(sprintf("%s/%s",directory,filenames[i]))[,colnum],na.rm=T)
sd.location[i]=sd(data,na.rm=T) #read.csv(sprintf("%s/%s",directory,filenames[i]))[,colnum],na.rm=T)
}

#Calculate mean and sd method 1
#Method not used because values differ from method 2
#mean.overall=mean(mean.location,na.rm=T)
#sd.overall=mean(sd.location,na.rm=T)

#Calculate mean and sd method 2
mean.all=mean(data.all,na.rm=T)
sd.all=sd(data.all,na.rm=T)

#function returns mean of pollutant across all monitor location and overall
DF=data.frame(id=c(0,id),mean.id=c(mean.all,mean.location[!is.na(mean.location)]),sd.id=c(sd.all,sd.location[!is.na(mean.location)]))
return(DF)

}

#Input to run for all 332 files and both pollutants and save output to csv files
#DF.mean_sd.sulfate=pollutantmean("specdata","sulfate",1:332)
#DF.mean_sd.nitrate=pollutantmean("specdata","nitrate",1:332)

#write.csv(DF.mean_sd.sulfate, "df_mean_sd_sulfate.csv")
#write.csv(DF.mean_sd.nitrate, "df_mean_sd_nitrate.csv")

## function 2 output valid readings per location as dataframe

complete <- function(directory="specdata",id=1:332) {

#directory location of file
filenames=list.files(directory) #list of files with airpollution data
obs=NA*c(1:332) #vector for mean of each datafile

#id location of monitor
for (i in id) { #loop thru list and retrieve mean of each location

#read in datafile
data=read.csv(sprintf("%s/%s",directory,filenames[i]),header=T)[,2:3]
#Calculate complete observations
obs[i]=dim(data[complete.cases(data),])[1]
}
obs.tot=sum(obs,na.rm=T)

DF=data.frame(id=c(0,id),obs.id=c(obs.tot,obs[!is.na(obs)]))
return(DF)

}

#Input to run for all 332 files and both pollutants and save output to csv files
#DF.complete.sulfate=complete("specdata","sulfate",1:332)
#DF.complete.nitrate=complete("specdata","nitrate",1:332)

#write.csv(DF.complete.sulfate, "df_complete_sulfate.csv")
#write.csv(DF.complete.nitrate, "df_complete_nitrate.csv")

# function 3 calculates correlatioon between sulfate and nitrate for complete observations for stations above specified threshold
pollutantcor <- function(directory="specdata", threshold=0,id=1:332) {
#directory location of file
filenames=list.files(directory) #list of files with airpollution data

#initializae necessary variables
cor.id=NA*id #vector for mean of each datafile
#id location of monitor
for (i in id) { #loop thru list and retrieve mean of each location

#read in datafile
data=read.csv(sprintf("%s/%s",directory,filenames[i]),header=T)[,2:3]
count_2 =length(data[!is.na(data[,2]),1]) #number of complete sulfate observations
count_3=length(data[!is.na(data[,2]),2]) #number of complete nitrate observations

#read in datafile
if (min(c(count_2,count_3)) > threshold) {
cor.id[i]=cor(data,use="complete.obs")[1,2]

}

}

#function returns corr coefficient across all monitor location and overall
DF=data.frame(id=id,cor.id=cor.id)
return(DF)

}

#DF.cor=pollutantcor()
#write.csv(DF.cor,"df_cor.csv")

