rm(list=ls())

ReqdLibs = c("here","purrr","readxl","Hmisc","chron","ggplot2","ggthemes","dplyr")
invisible(lapply(ReqdLibs, library, character.only = TRUE))

folder_path = here("Materials/Week 2/R project", "Raw Data")
# output the folder name
print(folder_path)
# make sure it exists
dir.exists(folder_path)

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

# METHOD 1: For loop
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

head(data.all)
dim(data.all)

# METHOD 2: map_df function in Purrr package
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

# METHOD 3: map_df & dplyr combined
data.all <- map_df(dir.list, function(dir_name) {
  # List all files in the current directory
  files.import <- list.files(here(folder_path, dir_name))
  
  map_df(files.import, function(file_name) {
    # Read the Excel file
    temp <- suppressMessages(read_excel(here(folder_path, dir_name, file_name), range = cell_cols("J:O"))) %>%
      # Clean and transform the data
      slice(-c(1, 2)) %>%                   # Remove the first two rows
      select(-2) %>%                        # Remove the second column
      mutate(across(2:5, as.numeric),      # Convert columns 2 to 5 to numeric
             t = seconds(times(t)) + (minutes(times(t)) * 60),  # Convert time to seconds
             Sub = dir_name,               # Assign Sub id
             trial = ifelse(nchar(file_name) < 16, "rest", 
                            # Assign trial id below by concatening diff pieces of info
                             paste("trial", as.numeric(substr(file_name, nchar(file_name) - 5, nchar(file_name) - 5)))))  
    
    return(temp)
  })
})

# Now data.all contains all the processed data
head(data.all)
dim(data.all)

#Visualize raw data by subject
ggplot(data.all,aes(x=t,y=VO2,color=trial))+
  geom_point()+
  geom_line()+
  facet_wrap(~Sub) + theme_wsj()


#Visualize data using loess by subject
ggplot(data.all,aes(x=t,y=VO2,color=trial))+
  geom_smooth(method = 'loess', formula = 'y~x')+
  facet_wrap(~Sub) + theme_wsj()

#create a color gradient
colfunc <- colorRampPalette(c("orange", "purple"))
#Visualize each trial across all participants
ggplot(data.all,aes(x=t,y=VO2,color=trial))+
  geom_smooth(method = 'loess', formula = 'y~x')+
  scale_colour_manual(values = c(colfunc(7))) + theme_wsj()
