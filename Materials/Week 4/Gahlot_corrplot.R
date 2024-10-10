
#Custom method for correlation matrix

library(ggplot2)
library(reshape2)
library(Hmisc)
library(grid)

# Example: Using the mtcars dataset
data(mtcars)

data=mtcars
pairs(data)
# Compute the correlation matrix
#Assign matrix of just rho (r)
cor_matrix <- rcorr(as.matrix(data),type = "pearson")$r
n_matrix <- rcorr(as.matrix(data),type = "pearson")$n
#Just get the lower triangle of matrix and replace with NA
cor_matrix[lower.tri(cor_matrix,diag = F)] <- NA 
n_matrix[lower.tri(n_matrix,diag = F)] <- NA 
#Assign matrix of just p-values (P)
p_matrix <- rcorr(as.matrix(data),type = "pearson")$P
#Have NA values be less than .001, NAs represent correlation along the diagonal
p_matrix[is.na(p_matrix)]=.0000001
#Just get the lower triangle of matrix and replace with NA
p_matrix[lower.tri(p_matrix,diag = F)] <- NA

# Melt the correlation matrix for ggplot
melted_cor <- melt(cor_matrix,na.rm = T) #pivot correlations to long format
melted_p = melt(p_matrix,na.rm = T) #pivot p-values to long format
melted_n = melt(n_matrix,na.rm = T)

#P-value adjustment using a false discovery rate correction
melted_cor$p=p.adjust(melted_p$value,method = 'fdr',n=length(melted_p$value))
#Assign symbol dependent on significance level: ns = , <.05 = *, <.01 = **, <.001 = ***
melted_cor$psig=""
melted_cor$psig[melted_cor$p<.05]="*"
melted_cor$psig[melted_cor$p<.01]="**"
melted_cor$psig[melted_cor$p<.001]="***"

melted_cor$n=melted_n$value

#This code creates a subplot within the heatmap to guide a reader on what each
#element within the individuals tiles in the heatmap
legend_plot <- ggplot() + 
  geom_tile(aes(x = .5, y = .5), fill = "white", color = "black", width = 2, height = 1.5) +  # Create the tile
  geom_text(aes(x = .5, y = .5),size=3, label = "Tile Guide:\nSymbol = FDR P-value\nNumber = Correlation\n(N = Sample Size)") + 
  theme_void()  # Remove all background elements

legend_plot

# Create a heatmap using ggplot2
#Var1 and Var2 are variable names in the correlation matrix, value is the rho
ggplot(melted_cor, aes(x=Var1, y=Var2, fill = value)) + 
  geom_tile(color = "white") + #border of tiles are white
  #VJUST AND HJUST ARGUEMENTS FOR TEXT WILL NEED TO BE ADJUSTED FOR GRAPH SIZE
  geom_text(aes(label = round(value,2)), vjust = 1) + #Insert rho value as label for corresponding tile
  geom_text(aes(label = psig), vjust = .25,size=5) + #Insert p-adjusted value as label for corresponding tile
  geom_text(aes(label = paste("(N = ",n,")")),vjust=2.65,size=3)+
  scale_fill_gradient2(low = "purple2", mid = "white", high = "orange", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") + #Customize tile color and gradient and rename legend title to "Correlation"
  theme_classic() + #Have blank space be white
  theme(axis.text.x = element_text(angle = 45, hjust = 1), #rotate x axis variables names by 45 degrees
        axis.text.y = element_text(angle = 45, hjust = 1))+ #rotate y axis variables names by 45 degrees
  labs(caption = "P-values are FDR corrected\n<.05 = *, <.01 = **, <.001 = ***")+ #Insert caption at bottom of graph
  xlab("")+ #Remove x axis title
  ylab("")+ #Remove y axis title
  ggtitle("Correlation Matrix")+ #Add title
  scale_y_discrete(limits=rev)+ #Reverse order so base of triangle is in bottom left
  annotation_custom(grob = ggplotGrob(legend_plot), xmin = 7.5, xmax = 10, ymin = 7.5, ymax = 10)
