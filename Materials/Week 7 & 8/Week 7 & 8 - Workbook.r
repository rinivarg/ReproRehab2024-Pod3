rm(list=ls())

ReqdLibs = c("here","ggplot2","dplyr","tidyr","stringr","janitor","broom","emmeans")
invisible(lapply(ReqdLibs, library, character.only = TRUE))

# here()
folder_path = getwd()
# folder_path

subfolder_path = paste0(folder_path,'/data/')
# subfolder_path
# dir(subfolder_path)

file_list = list.files(subfolder_path)

file_list

file_list = list.files(subfolder_path)
file_list

file_path = paste0(subfolder_path,file_list[1])
# file_path
temp0 = read.delim(file_path)
# temp0
head(temp0,5)

new_names = paste(temp0[1,],temp0[4,])

colnames(temp0) = new_names

temp1 = temp0[-c(1:4),]
head(temp1,5)

file_list = list.files(subfolder_path)
file_list

new_names = paste(temp0[1,],temp0[4,])


data.all = data.frame(list())

for (i in 1:length(file_list)) {
    
file_path = paste0(subfolder_path,file_list[i])
    
    
temp = read.delim(file_path)    
colnames(temp) = paste(temp[1,],temp[4,])   
temp = temp[-c(1:4),]
    
temp = clean_names(temp)
colnames(temp)[1] = "perc_gait"
temp[,-1] = apply(temp[,-1],2,as.double)
    
temp$fileName = substr(file_list[i],1,nchar(file_list[i])-4)
     
data.all = rbind(data.all, temp)    
}

head(data.all,6)

data.all %>% 
separate(fileName,sep = "_", into = c("prefix","session","trial"), remove = FALSE) %>% 
select(!prefix) %>% 
{.->>data.clean}

head(data.clean)

data.clean %>% 
pivot_longer(cols = where(is.numeric), names_to = "measure", values_to = "value") %>% 
{.->> data.clean.longPlot}

data.clean.longPlot$perc_gait = as.double(data.clean.longPlot$perc_gait)
head(data.clean.longPlot)

thm = theme(plot.title = element_text(size = 40),
          legend.title = element_text(size = 25),
          legend.position = "top",
          legend.text = element_text(size = 20),
          strip.text = element_text(size = 35),
          axis.ticks.length = unit(0.3,"cm"),
          axis.line = element_line(colour = "black",linewidth = 1),
          axis.ticks = element_line(colour = "black",linewidth = 1),
          axis.text = element_text(colour = "black",size = 40),
          axis.text.x = element_text(lineheight = 1.1, margin = margin(t = 10)),
          axis.title.x = element_text(size=40, colour = "grey35", face = "plain",
                                     lineheight = 1.1, margin = margin(r = 10)),
          axis.title.y = element_text(size=40, colour = "grey35", face = "plain",
                                     lineheight = 1.1, margin = margin(r = 10)))

custom_colors <- c("#e41a1c", "#13388e", "#03ac13")

library("ggthemes")

options(repr.plot.width = 40, repr.plot.height = 40)
all_vars = 
ggplot(data.clean.longPlot, aes(x = perc_gait,y = value, 
                                group = session, col = session, fill = session)) + 
stat_summary(geom = "line", fun = mean, na.rm = TRUE) + 
stat_summary(geom = "ribbon",fun.data = mean_se, na.rm = TRUE,alpha=0.3) + 
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
xlab("% gait cycle") + ylab("") + 
facet_wrap(~measure, scales = "free")  +
theme_clean() + thm
all_vars

# ggsave(file='all_vars.svg', plot=all_vars, width=35, height=35)









