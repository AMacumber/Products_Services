# Programmable Slides: EMMA Results

## ReadMe
# Run this script after you have run the EMMA script
## It will collect data from the global environment (R Studio)
# Create standardized slides for iterative analysis results
##

## Load in data

# Sub Sample Data
dietze <- rbind(mm_bins, emma)  # emma is raw PSA data no headers; mm_bins is the headers
dietze <- t(dietze)
##

## Data Analysis: Assign Means and Modes

# Means
robust <- rbind(mm_bins, EM.rob$loadings$mean)  # EM.rob is the result of robust.EM()
robust <- t(robust)
robust.sd <- t(EM.rob$loadings$sd)  # EM.rob is the result of robust.EM()

# Modes
mode_01 <- mm_bins[EM.rob$modes[1]]
mode_01_text <- "Fine Silt"
mode_01_legend <- "Fine Silt (6 microns; 24%)"

mode_02 <- mm_bins[EM.rob$modes[2]]
mode_02_text <- "V Coarse Silt"
mode_02_legend <- "V Coarse Silt (40 microns; 42%)"

mode_03 <- mm_bins[EM.rob$modes[3]]
mode_03_text <- "Fine Sand"
mode_03_legend <- "Fine Sand (180 microns; 22%)"

mode_04 <- mm_bins[EM.rob$modes[4]]
mode_04_text <- "Medium Sand"
mode_04_legend <- "Medium Sand (450 microns; 9%)"
##


## Generic Plot Parameters

### Set window width and height

# Wix Webpage
width <- 9.8
height <- 4.4

# Labeling the grain size ranges
labels_grain_adj = c('Clay', "V Fine", "Fine", "Medium", "Coarse", "V Coarse", "V Fine", "Fine", "Medium", "Coarse")
labels_grain_group = c('', "Silt", "Silt", "Silt", "Silt", "Silt", "Sand", "Sand", "Sand", "Sand")

# Coordinates for the above labels
lines_grain_x_coors = c(2, 4, 8, 15, 31, 63 , 125, 250, 500)
labels_grain_x_coors = c(0.4, 2.75, 5.5, 11, 21.5, 45 , 90, 175, 350, 1100) 

labels_grain_adj_y_coors = rep(10, length(labels_grain_x_coors))
labels_grain_group_y_coors = rep(9.5, length(labels_grain_x_coors))

# Chart Titles
chart_title <- "Harvey Lake Surface Samples: EMMA"
##



## The Plots

### Sub Samples - Black

# Create a powerpoint sized window
windows(width, height)

# Create the plot
plot(dietze[,1],dietze[,2],  # first column is bin size; second column first sample
     type = "l", col="grey",  
     ylim = c(0,10), ylab = "Frequency (%)",
     xlim = c(0.1,2000), log ="x", xaxt="n", xlab = "",
     main = chart_title)

# Grain Size Ranges (dividers and labels)
abline(v = lines_grain_x_coors, col="grey",lty = 2, lwd = 2) # add vertical lines to demarcate grain size ranges
text(labels_grain_x_coors, labels_grain_adj_y_coors, labels_grain_adj, cex = 0.75)  # add labels for grain size ranges
text(labels_grain_x_coors, labels_grain_group_y_coors, labels_grain_group, cex = 0.75)  # add labels for grain size ranges

# Add samples as grey lines
for (i in 3:ncol(dietze)) lines(dietze[,1],dietze[,i],col="black") # plot for every column

# Graph Details
axis (side=1, at = c(0.1,1,10,100,1000)) # add major tick marks
axis (side=1, at = c(seq(0.2,0.9,by = 0.1), seq(2,9, by =1), # add minor tick marks
                     seq(20,90, by = 10), seq(200,900,by=100))
      , labels = NA, tcl=-0.25, lwd=0, lwd.ticks=1) # no labels and shorten tick marks
title(xlab = "Grain size (microns)")

# Add a legend
legend(0.09,9,c("Samples","End Member Mean","End Member St. Deviation"), # add a legend at specific spot with labels
       lty=c(1,1,2), col=c("grey","black","black"), bg = "white" , cex = 0.90) # ensure same as lines in plot
###


### EM 01 + 02 + 03 + 04 & Sub Samples

#### Create a powerpoint sized window
windows(width, height)

#### create a plot
plot(dietze[,1],dietze[,2],  # first column is bin size; second column first sample
     type = "l", col="grey",  
     ylim = c(0,10), ylab = "Frequency (%)",
     xlim = c(0.1,2000), log ="x", xaxt="n", xlab = "",
     main = chart_title)

#### add vertical lines to demarcate grain size ranges
abline(v = lines_grain_x_coors, col="grey",lty = 2, lwd = 2)

#### add labels for grain size ranges
text(labels_grain_x_coors, labels_grain_adj_y_coors, labels_grain_adj, cex = 0.75)
text(labels_grain_x_coors, labels_grain_group_y_coors, labels_grain_group, cex = 0.75)

#### add sub samples as grey lines
for (i in 3:ncol(dietze)) lines(dietze[,1],dietze[,i],col="grey") # plot for every column

#### End Member 1 (mean + std)
lines(robust[,1],robust[,2],type="l",col="red",lty = 1, lwd = 2)
lines(robust[,1],robust[,2]-robust.sd[,1],type="l",col="red",lty = 2, lwd = 2)
lines(robust[,1],robust[,2]+robust.sd[,1],type="l",col="red",lty = 2, lwd = 2)

#### End Member 2 (mean + std)
lines(robust[,1],robust[,3],type="l",col="blue",lty = 1, lwd = 2)
lines(robust[,1],robust[,3]-robust.sd[,2],type="l",col="blue",lty = 2, lwd = 2)
lines(robust[,1],robust[,3]+robust.sd[,2],type="l",col="blue",lty = 2, lwd = 2)

#### End Member 3 (mean + std)
lines(robust[,1],robust[,4],type="l",col="green",lty = 1, lwd = 2)
lines(robust[,1],robust[,4]-robust.sd[,3],type="l",col="green",lty = 2, lwd = 2)
lines(robust[,1],robust[,4]+robust.sd[,3],type="l",col="green",lty = 2, lwd = 2)

#### End Member 4 (mean + std)
lines(robust[,1],robust[,5],type="l",col="orange",lty = 1, lwd = 2)
lines(robust[,1],robust[,5]-robust.sd[,4],type="l",col="orange",lty = 2, lwd = 2)
lines(robust[,1],robust[,5]+robust.sd[,4],type="l",col="orange",lty = 2, lwd = 2)

#### Graph Details
axis (side=1, at = c(0.1,1,10,100,1000)) # add major tick marks
axis (side=1, at = c(seq(0.2,0.9,by = 0.1), seq(2,9, by =1), # add minor tick marks
                     seq(20,90, by = 10), seq(200,900,by=100))
      , labels = NA, tcl=-0.25, lwd=0, lwd.ticks=1) # no labels and shorten tick marks
title(xlab = "Grain size (microns)")

#### Add a legend
legend(0.09,9,c("Samples","End Member Mean","End Member St. Deviation"), # add a legend at specific spot with labels
       lty=c(1,1,2), col=c("grey","black","black"), bg = "white", cex = 0.90) # ensure same as lines in plot



### EM 01 + 02 + 03 + 04

#### Create a powerpoint sized window
windows(width, height)

#### Create a plot
plot(robust[,1],robust[,2],  # first column is bin size; second column first EM
     type = "l", col="red", lwd = 2, 
     ylim = c(0,10), ylab = "Frequency (%)",
     xlim = c(0.1,2000), log ="x", xaxt="n", xlab = "",
     main = chart_title)

#### add vertical lines to demarcate grain size ranges
abline(v = lines_grain_x_coors, col="grey",lty = 2, lwd = 2)

#### add labels for grain size ranges
text(labels_grain_x_coors, labels_grain_adj_y_coors, labels_grain_adj, cex = 0.75)
text(labels_grain_x_coors, labels_grain_group_y_coors, labels_grain_group, cex = 0.75)

#### End Member (mean)
lines(robust[,1],robust[,3],type="l",col="blue",lty = 1, lwd = 2)
lines(robust[,1],robust[,4],type="l",col="green",lty = 1, lwd = 2)
lines(robust[,1],robust[,5],type="l",col="orange",lty = 1, lwd = 2)

#### End Member (text)
text(mode_01, max(robust[,2]) + 0.55, mode_01_text)
text(mode_02, max(robust[,3]) + 0.55, mode_02_text)
text(mode_03, max(robust[,4]) + 0.55, mode_03_text)
text(mode_04, max(robust[,5]) + 0.55, mode_04_text)

#### Graph Details
axis (side=1, at = c(0.1,1,10,100,1000)) # add major tick marks
axis (side=1, at = c(seq(0.2,0.9,by = 0.1), seq(2,9, by =1), # add minor tick marks
                     seq(20,90, by = 10), seq(200,900,by=100))
      , labels = NA, tcl=-0.25, lwd=0, lwd.ticks=1) # no labels and shorten tick marks
title(xlab = "Grain size (microns)")

#### add a legend
legend(0.075,9,  # coordinates
       cex = 0.8,
       c(mode_01_legend, mode_02_legend, mode_03_legend, mode_04_legend), 
       lwd=c(2,2,2,2),  # thickness
       col=c("red","blue","green","orange"),  # colour
       bg = "white") 
####
###
##
#




