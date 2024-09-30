# Filamentous Fungal Germination

# This script is designed for the quantification of germination based on size and shape. 

# Designed by Sébastien C. Ortiz
# Last updated: 28-September-2024


##### USAGE INFORMATION #####

# This R script is designed for organization of "Data" files (.csv) produced from 
# the BatchFungus.txt FIJI Macro. 

# This script will only work if the .csv files are the correct size, so ensure that 
# your FIJI measurements settings are correctly set: The only 5 measurements that 
# should be selected are Area, Min & max gray value, Shape descriptors, Area fraction 
# and Fit ellipse. Update: The BatchFungus.txt macro will change your FIJI measurements
# settings to the correct selections.

# You will be prompted to select a .csv file, which will select all .csv files in that folder for analysis


##### Input Parameters for Sample ####

# Fill out the following values for your sample.
# The Name assignment will be added to all files produced.
# PPM used to convert from pixel^2 to µm^2 will vary depending on microscope/camera/objective setup.
# All other values will be fungus dependent - please see associated paper for published values.

# Current parameters for Aspergillus fumigatus

Name <- '0H Test'         #Name your sample here
PPM <- 0.1024             #Pixel^2 / µm^2
cARMin <- 0.75            #Resting Conidia Min Aspect Ratio
cARMax <- 1               #Resting Conidia Max Aspect Ratio
scARMin <- 0.8            #Swollen Conidia Min Aspect Ratio
scARMax <- 1              #Swollen Conidia Max Aspect Ratio
gARMin <- 0               #Germling Min Aspect Ratio
gARMax <- 0.8             #Germling Min Aspect Ratio
cSizeMin <- 0             #Resting Conidia Min Size
cSizeMax <- 10            #Resting Conidia Max Size
scSizeMin <- 10           #Swollen Conidia Min Size
scSizeMax <- 60           #Swollen Conidia Max Size
gSizeMin <- 15            #Germling Min Size
gSizeMax <- 60            #Germling Max Size

#######################################

##### Import all .csv files from folder into a single Data table - START #####

Folder <- dirname(file.choose())
files <- list.files(Folder)
Locations <- list.files(path=Folder, pattern=".csv", all.files=TRUE,
                      full.names=TRUE)
Data <- data.frame()
for (i in Locations) {
  j <- read.csv(i, header = TRUE)
  Data <- rbind(Data,j)
}

##### Import all .csv files from folder into a single Data table - END #####

##### Clean the raw data - START #####

Clean <- data.frame()
for (k in 1:nrow(Data)) {
      if (Data[k,12] >= 0.6 && Data[k,6] >= 6 && Data[k,2] <= (gSizeMax/PPM)) 
      Clean <- rbind(Clean, Data[k,1:12])}
# Select only area and aspect ratio #
SizeShape <- data.frame()
SizeShape <- Clean[,c(2,11)]

##### Clean the raw data - END  #####

##### Identifying Morphotypes - START #####

conidia_index <- data.frame()
swollenconidia_index <- data.frame()
germi_index <- data.frame()

labelTable <- list()
labelName <- list()

for (k in 1:nrow(SizeShape)) {
  if (SizeShape[k, 1] < (cSizeMax / PPM) &&
      SizeShape[k, 1] > (cSizeMin / PPM) && 
      SizeShape[k, 2] > cARMin &&
      SizeShape[k, 2] <= cARMax) {
    conidia_index <- rbind(conidia_index, SizeShape[k, 1:2])
    labelName[k] <- "conidia"
  }
  
  else if (SizeShape[k, 1] < (scSizeMax / PPM) &&
           SizeShape[k, 1] >= (scSizeMin / PPM) &&
           SizeShape[k, 2] > scARMin &&
           SizeShape[k,2] <= scARMax) {
    swollenconidia_index <- rbind(swollenconidia_index, SizeShape[k, 1:2])
    labelName[k] <- "swollenconidia"
  }
  
  else if (SizeShape[k, 1] >= (gSizeMin / PPM) &&
           SizeShape[k, 1] < (gSizeMax / PPM) &&
           SizeShape[k, 2] < gARMax &&
           SizeShape[k, 2] >= gARMin) {
    germi_index <- rbind(germi_index, SizeShape[k, 1:2])
    labelName[k] <- "germi"
  }
}

##### Identifying Morphotypes - END #####

##### Plotting data with ggplot - START #####

AllCells <- rbind(conidia_index, swollenconidia_index, germi_index) # combine all cleaned values in one table

AllCells[,1] <- AllCells[,1]*PPM # convert values to um

library(ggplot2)

ggplot(AllCells, aes(x=AllCells[,1], y=AllCells[,2])) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette=16, direction=-1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  annotate("segment", x = cSizeMin, xend = cSizeMax , y = cARMin, yend = cARMin, colour = "white", size=1, alpha=0.6) +
  annotate("segment", x = gSizeMin, xend = gSizeMax , y = gARMax, yend = gARMax, colour = "white", size=1, alpha=0.6) +
  annotate("segment", x = cSizeMax, xend = cSizeMax , y = cARMin, yend = 1, colour = "white", size=1, alpha=0.6) +
  annotate("segment", x = gSizeMin, xend = gSizeMin , y = 0, yend = gARMax, colour = "white", size=1, alpha=0.6) +
  labs(y = "Aspect Ratio" , x = 'Area µm ^ 2') +
  theme(
    legend.position='none'
  )

##### Plotting data with ggplot - END #####

##### Printing ggplot - START #####

pdf(paste0(Name, ".pdf"))
ggplot(AllCells, aes(x=AllCells[,1], y=AllCells[,2])) +
       stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
       scale_fill_distiller(palette=16, direction=-1) +
       scale_x_continuous(expand = c(0, 0)) +
       scale_y_continuous(expand = c(0, 0)) +
      annotate("segment", x = cSizeMin, xend = cSizeMax , y = cARMin, yend = cARMin, colour = "white", size=1, alpha=0.6) +
      annotate("segment", x = gSizeMin, xend = gSizeMax , y = gARMax, yend = gARMax, colour = "white", size=1, alpha=0.6) +
      annotate("segment", x = cSizeMax, xend = cSizeMax , y = cARMin, yend = 1, colour = "white", size=1, alpha=0.6) +
      annotate("segment", x = gSizeMin, xend = gSizeMin , y = 0, yend = gARMax, colour = "white", size=1, alpha=0.6) +
       labs(y = "Aspect Ratio" , x = 'Area µm ^ 2') +
       theme(
         axis.title = element_text(size = 16),
         axis.text = element_text(size = 12),
         legend.position='none'
)
dev.off() 

##### Printing ggplot - END #####

##### % Population Calculations - START #####

c_count <- nrow(conidia_index)
sc_count <- nrow(swollenconidia_index)
g_count <- nrow(germi_index)

total_count <-c_count + sc_count + g_count

c_percent <- c_count / total_count * 100
sc_percent <- sc_count / total_count * 100
g_percent <- g_count / total_count * 100

results <- c(c_percent, sc_percent, g_percent, total_count)
names(results)[1] <- "% Resting Conidia"
names(results)[2] <- "% Swollen Conidia"
names(results)[3] <- "% Germling/Hyphae"
names(results)[4] <- "Total Count"

##### % Population Calculations - END #####

##### Print Files - START #####

NameSummary = paste(Name, "Summary.csv")
NameC = paste(Name, "Conidia.csv")
NameSC = paste(Name, "SwollenConidia.csv")
NameG = paste(Name, "Germling.csv")

write.csv(results, file = NameSummary)
write.csv(conidia_index, file = NameC)
write.csv(swollenconidia_index, file = NameSC)
write.csv(germi_index, file = NameG)

##### Print Files - END #####
