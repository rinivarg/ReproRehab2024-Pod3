rm(list=ls())

ReqdLibs = c("here","purrr","readxl","Hmisc","chron","ggplot2","ggthemes","dplyr")
invisible(lapply(ReqdLibs, library, character.only = TRUE))

thm = theme(
          strip.text.x=element_text(size=20,face="bold"),
          strip.text.y=element_text(size=20,face="bold"),
          legend.text=element_text(size=16,face="bold"),
          legend.position = "top",
          legend.title=element_text(size=16,face="bold"),
          title =element_text(size=14, face='bold'),
          text = element_text(colour = "black",size=18), 
          plot.title = element_text(colour = "black",size = 22, face = "bold"),
          axis.ticks.length = unit(0.3,"cm"),
          axis.line = element_line(colour = "black",size=0.85),
          axis.ticks = element_line(colour = "black",size=0.85),
          axis.text = element_text(colour = "black",size=24),
          axis.title=element_text(size=25))

folder_path = here("Materials/Week 2/R project", "Raw Data")
# output the folder name
print(folder_path)
# make sure it exists
dir.exists(folder_path)


options(warn=0)
subfolder_path = here(folder_path,'Sub1')

files.test=list.files(subfolder_path)
files.test

#Let's read in one file to see how ugly the data are
temp0=suppressMessages(read_excel(here(subfolder_path,files.test[1]),))
head(temp0)

temp=suppressMessages(read_excel(here(subfolder_path,files.test[1]),range = cell_cols("J:O")))
head(temp)
# the first two rows are header-like information so remove it
temp=temp[-c(1,2),-2]
head(temp)


dir.list = dir(folder_path)
dir.list

data.all = data.frame(list())

for(i in 1:length(dir.list)){
  files.import=list.files(here(folder_path,dir.list[i]))
  for(j in 1:length(files.import)){
    #Give me only the rows I need
    temp=suppressMessages(read_excel(here(folder_path,dir.list[i],files.import[j]),
                                     range = cell_cols("J:O")))
    #Remove the random stuff
    temp=temp[-c(1,2),-2]
    #Convert to numeric
    temp[,c(2:5)]=apply(temp[,c(2:5)],2,as.numeric)
    #Covert to seconds
    temp$t <- seconds(times(temp$t)) + (minutes(times(temp$t)) * 60)
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

head(data.all)
dim(data.all)

data.all <- map_df(dir.list, function(dir_name) {
  # List all files in the current directory
  files.import <- list.files(here(folder_path, dir_name))
  
  map_df(files.import, function(file_name) {
    # Read the Excel file
    temp <- suppressMessages(read_excel(here(folder_path, dir_name, file_name), range = cell_cols("J:O")))
    
    # Clean the data
    temp <- temp[-c(1, 2), -2]
    temp[, 2:5] <- apply(temp[, 2:5], 2, as.numeric)
    
    # Convert time to seconds
    temp$t <- seconds(times(temp$t)) + (minutes(times(temp$t)) * 60)
    
    # Assign Sub id
    temp$Sub <- dir_name
    
    # Assign trial id
    temp$trial <- ifelse(nchar(file_name) < 16, "rest", paste("trial", as.numeric(substr(file_name, nchar(file_name) - 5, nchar(file_name) - 5))))
    
    return(temp)
  })
})

# Now data.all contains all the processed data
head(data.all)
dim(data.all)

library(lubridate)
library(tidyr)

data.all <- map_df(dir.list, function(dir_name) {
  # List all files in the current directory
  files.import <- list.files(here(folder_path, dir_name))
  
  map_df(files.import, function(file_name) {
      
    # Read the Excel file
    temp <- suppressMessages(read_excel(here(folder_path, dir_name, file_name), range = cell_cols("J:O"))) %>%
      
      # Clean and transform the data
      slice(-c(1, 2)) %>%                   # Remove the first two rows
      select(-2) %>%                        # Remove the second column
      mutate(across(2:5, as.numeric),       # Convert columns 2 to 5 to numeric
             t = as.numeric(hms(t)),        # Convert time to seconds
             id = file_name) %>%            # Assign file name as identifier
      separate(id,into = c("Sub","trial","extn"),sep = "[_\\.]") %>%  # Now separate the identifier into sub & trial
      select(!c("extn"))                    # Don't need file extensions in the table!
    
    return(temp)
  })
})

# Now data.all contains all the processed data
head(data.all)
dim(data.all)


options(repr.plot.width = 12, repr.plot.height = 10)

#Visualize raw data by subject
ggplot(data.all,aes(x=t,y=VO2,color=trial))+
  geom_point()+
  geom_line()+
  facet_wrap(~Sub) + theme_wsj() + thm


#Visualize data using loess by subject
ggplot(data.all,aes(x=t,y=VO2,color=trial))+
  geom_smooth(method = 'loess', formula = 'y~x')+
  facet_wrap(~Sub) + theme_wsj() + thm

options(repr.plot.width = 8, repr.plot.height = 8)
#create a color gradient
colfunc <- colorRampPalette(c("orange", "purple"))
#Visualize each trial across all participants
ggplot(data.all,aes(x=t,y=VO2,color=trial))+
  geom_smooth(method = 'loess', formula = 'y~x')+
  scale_colour_manual(values = c(colfunc(7))) + theme_wsj() + thm
