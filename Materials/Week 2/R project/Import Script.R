# Clear workspace
rm(list=ls())

# Load required libraries
ReqdLibs = c("here","purrr","readxl","Hmisc","chron","ggplot2")

lapply(ReqdLibs, library, character.only = TRUE)

#Check to see how many directories are in the Raw Data folder
#Should populate 13 sub folders.
dir(here("Raw Data"))

#Let's read in one file to see how ugly the data are
files.test=list.files(here("Raw Data","Sub1"))
temp=read_excel(here("Raw Data","Sub1",files.test[1]))
#very ugly need to modify so we only import a certain range
#Let's just do the first 5 rows where the data is in long format
temp=read_excel(here("Raw Data","Sub1",files.test[1]),range = cell_cols("J:O"))
temp=temp[-c(1,2),-2]

#Create empty data frame which we will iteratively build
data.all=data.frame(t=numeric(),
                    Rf=numeric(),
                    VT=numeric(),
                    VE=numeric(),
                    VO2=numeric(),
                    Sub=character(),
                    trial=character())

dir.list=dir(here("Raw Data"))
for(i in 1:length(dir.list)){
  files.import=list.files(here("Raw Data",dir.list[i]))
  for(j in 1:length(files.import)){
    #Give me only the rows I need
    temp=read_excel(here("Raw Data",dir.list[i],files.import[j]),range = cell_cols("J:O"))
    #Remove the random stuff
    temp=temp[-c(1,2),-2]
    #Convert to numeric
    temp[,c(2:5)]=apply(temp[,c(2:5)],2,as.numeric)
    #Covert to seconds
    temp$t=seconds(times(temp$t))+(minutes(times(temp$t))*60)
    #Assign Sub id
    temp$Sub=dir.list[i]
    #Assign trial id
    if(nchar(files.import[j])<16){
    temp$trial="rest"
    }else{
      temp$trial=paste("trial",as.numeric(substr(files.import[j],nchar(files.import[j])-5,nchar(files.import[j])-5)))
    }
    # this final step is where the 'stacking' happens
    data.all=rbind(data.all,temp)
    
  }
}

dim(data.all)

#Visualize raw data by subject
ggplot(data.all,aes(x=t,y=VO2,color=trial))+
  geom_point()+
  geom_line()+
  facet_wrap(~Sub)

#Visualize data using loess by subject
ggplot(data.all,aes(x=t,y=VO2,color=trial))+
  geom_smooth()+
  facet_wrap(~Sub)

#create a color gradient
colfunc <- colorRampPalette(c("orange", "purple"))
#Visualize each trial across all participants
ggplot(data.all,aes(x=t,y=VO2,color=trial))+
  geom_smooth()+
  scale_colour_manual(values = c(colfunc(7)))
