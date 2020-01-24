breaks = c(0,.01,seq(0.02,1.01,.01))
number = 0:100
colors = paste("grey",number,sep="")

post_harvest_angle = "random"    #direction for after harvesting, can be "random", "away" or "toward"
date = "temp1"           #fill in with today's date (year_month_day) and create this folder in Data Files
nsim = 1
n_bees = 50
source("/home/macquees/Desktop/Resubmission2/Code/Functions.R")                    #source the functions file


###############################################################################################
landscape = "2"                 #change as needed for appropriate landscape (2,3,4,6,7)

source("/home/macquees/Desktop/Resubmission2/Code/Simulations_Master_Nohup.R")

#make plots from the last simulation 

#determine the color scheme for the heatmaps

png(filename = paste("/home/macquees/Desktop/Resubmission2/Code/Plot_Intensity_random_Landscape_",landscape,sep=""),width=2000,height=2000)
image.plot(t(apply(Count_blueberry_visits/max(Count_blueberry_visits), 2, rev)), col = rev(colors), breaks = breaks, axes=FALSE,
           xlab = "", ylab = "", main = "Intensity of Flowers Visited on Each Blueberry Bush")

# plot(proportion3*100,xlab="number of visits", ylab = "percent of blueberry bushes",pch = 20,ylim = c(0,50))
dev.off()

###############################################################################################
###############################################################################################
landscape = "3"                 #change as needed for appropriate landscape (2,3,4,6,7)

source("/home/macquees/Desktop/Resubmission2/Code/Simulations_Master_Nohup.R")

#make plots from the last simulation 

#determine the color scheme for the heatmaps

png(filename = paste("/home/macquees/Desktop/Resubmission2/Code/Plot_Intensity_random_Landscape_",landscape,sep=""),width=2000,height=2000)
image.plot(t(apply(Count_blueberry_visits/max(Count_blueberry_visits), 2, rev)), col = rev(colors), breaks = breaks, axes=FALSE,
           xlab = "", ylab = "", main = "Intensity of Flowers Visited on Each Blueberry Bush")

# plot(proportion3*100,xlab="number of visits", ylab = "percent of blueberry bushes",pch = 20,ylim = c(0,50))
dev.off()

###############################################################################################
###############################################################################################
landscape = "4"                 #change as needed for appropriate landscape (2,3,4,6,7)

source("/home/macquees/Desktop/Resubmission2/Code/Simulations_Master_Nohup.R")

#make plots from the last simulation 

#determine the color scheme for the heatmaps

png(filename = paste("/home/macquees/Desktop/Resubmission2/Code/Plot_Intensity_random_Landscape_",landscape,sep=""),width=2000,height=2000)
image.plot(t(apply(Count_blueberry_visits/max(Count_blueberry_visits), 2, rev)), col = rev(colors), breaks = breaks, axes=FALSE,
           xlab = "", ylab = "", main = "Intensity of Flowers Visited on Each Blueberry Bush")

# plot(proportion3*100,xlab="number of visits", ylab = "percent of blueberry bushes",pch = 20,ylim = c(0,50))
dev.off()

###############################################################################################
###############################################################################################
landscape = "6"                 #change as needed for appropriate landscape (2,3,4,6,7)

source("/home/macquees/Desktop/Resubmission2/Code/Simulations_Master_Nohup.R")

#make plots from the last simulation 

#determine the color scheme for the heatmaps

png(filename = paste("/home/macquees/Desktop/Resubmission2/Code/Plot_Intensity_random_Landscape_",landscape,sep=""),width=2000,height=2000)
image.plot(t(apply(Count_blueberry_visits/max(Count_blueberry_visits), 2, rev)), col = rev(colors), breaks = breaks, axes=FALSE,
           xlab = "", ylab = "", main = "Intensity of Flowers Visited on Each Blueberry Bush")

# plot(proportion3*100,xlab="number of visits", ylab = "percent of blueberry bushes",pch = 20,ylim = c(0,50))
dev.off()

###############################################################################################
###############################################################################################
landscape = "7"                 #change as needed for appropriate landscape (2,3,4,6,7)

source("/home/macquees/Desktop/Resubmission2/Code/Simulations_Master_Nohup.R")

#make plots from the last simulation 

#determine the color scheme for the heatmaps

png(filename = paste("/home/macquees/Desktop/Resubmission2/Code/Plot_Intensity_random_Landscape_",landscape,sep=""),width=2000,height=2000)
image.plot(t(apply(Count_blueberry_visits/max(Count_blueberry_visits), 2, rev)), col = rev(colors), breaks = breaks, axes=FALSE,
           xlab = "", ylab = "", main = "Intensity of Flowers Visited on Each Blueberry Bush")

# plot(proportion3*100,xlab="number of visits", ylab = "percent of blueberry bushes",pch = 20,ylim = c(0,50))
dev.off()

###############################################################################################























breaks = c(0,.01,seq(0.02,1.01,.01))
number = 0:100
colors = paste("grey",number,sep="")

post_harvest_angle = "away"    #direction for after harvesting, can be "random", "away" or "toward"
angle_distance = 3       #distance from the edge at which direction after harvesting comes into play
date = "2019_12_09_away"           #fill in with today's date (year_month_day) and create this folder in Data Files
nsim = 20
source("/home/macquees/Desktop/Resubmission2/Code/Functions.R")                    #source the functions file


###############################################################################################
landscape = "2"                 #change as needed for appropriate landscape (2,3,4,6,7)

source("/home/macquees/Desktop/Resubmission2/Code/Simulations_Master_Nohup.R")

#make plots from the last simulation 

#determine the color scheme for the heatmaps

png(filename = paste("/home/macquees/Desktop/Resubmission2/Code/Plot_Intensity_away_Landscape_",landscape,sep=""),width=2000,height=2000)
image.plot(t(apply(Count_blueberry_visits/max(Count_blueberry_visits), 2, rev)), col = rev(colors), breaks = breaks, axes=FALSE,
           xlab = "", ylab = "", main = "Intensity of Flowers Visited on Each Blueberry Bush")

# plot(proportion3*100,xlab="number of visits", ylab = "percent of blueberry bushes",pch = 20,ylim = c(0,50))
dev.off()

###############################################################################################
###############################################################################################
landscape = "3"                 #change as needed for appropriate landscape (2,3,4,6,7)

source("/home/macquees/Desktop/Resubmission2/Code/Simulations_Master_Nohup.R")

#make plots from the last simulation 

#determine the color scheme for the heatmaps

png(filename = paste("/home/macquees/Desktop/Resubmission2/Code/Plot_Intensity_away_Landscape_",landscape,sep=""),width=2000,height=2000)
image.plot(t(apply(Count_blueberry_visits/max(Count_blueberry_visits), 2, rev)), col = rev(colors), breaks = breaks, axes=FALSE,
           xlab = "", ylab = "", main = "Intensity of Flowers Visited on Each Blueberry Bush")

# plot(proportion3*100,xlab="number of visits", ylab = "percent of blueberry bushes",pch = 20,ylim = c(0,50))
dev.off()

###############################################################################################
###############################################################################################
landscape = "4"                 #change as needed for appropriate landscape (2,3,4,6,7)

source("/home/macquees/Desktop/Resubmission2/Code/Simulations_Master_Nohup.R")

#make plots from the last simulation 

#determine the color scheme for the heatmaps

png(filename = paste("/home/macquees/Desktop/Resubmission2/Code/Plot_Intensity_away_Landscape_",landscape,sep=""),width=2000,height=2000)
image.plot(t(apply(Count_blueberry_visits/max(Count_blueberry_visits), 2, rev)), col = rev(colors), breaks = breaks, axes=FALSE,
           xlab = "", ylab = "", main = "Intensity of Flowers Visited on Each Blueberry Bush")

# plot(proportion3*100,xlab="number of visits", ylab = "percent of blueberry bushes",pch = 20,ylim = c(0,50))
dev.off()

###############################################################################################
###############################################################################################
landscape = "6"                 #change as needed for appropriate landscape (2,3,4,6,7)

source("/home/macquees/Desktop/Resubmission2/Code/Simulations_Master_Nohup.R")

#make plots from the last simulation 

#determine the color scheme for the heatmaps

png(filename = paste("/home/macquees/Desktop/Resubmission2/Code/Plot_Intensity_away_Landscape_",landscape,sep=""),width=2000,height=2000)
image.plot(t(apply(Count_blueberry_visits/max(Count_blueberry_visits), 2, rev)), col = rev(colors), breaks = breaks, axes=FALSE,
           xlab = "", ylab = "", main = "Intensity of Flowers Visited on Each Blueberry Bush")

# plot(proportion3*100,xlab="number of visits", ylab = "percent of blueberry bushes",pch = 20,ylim = c(0,50))
dev.off()

###############################################################################################
###############################################################################################
landscape = "7"                 #change as needed for appropriate landscape (2,3,4,6,7)

source("/home/macquees/Desktop/Resubmission2/Code/Simulations_Master_Nohup.R")

#make plots from the last simulation 

#determine the color scheme for the heatmaps

png(filename = paste("/home/macquees/Desktop/Resubmission2/Code/Plot_Intensity_away_Landscape_",landscape,sep=""),width=2000,height=2000)
image.plot(t(apply(Count_blueberry_visits/max(Count_blueberry_visits), 2, rev)), col = rev(colors), breaks = breaks, axes=FALSE,
           xlab = "", ylab = "", main = "Intensity of Flowers Visited on Each Blueberry Bush")

# plot(proportion3*100,xlab="number of visits", ylab = "percent of blueberry bushes",pch = 20,ylim = c(0,50))
dev.off()

###############################################################################################




















breaks = c(0,.01,seq(0.02,1.01,.01))
number = 0:100
colors = paste("grey",number,sep="")

post_harvest_angle = "toward"    #direction for after harvesting, can be "random", "away" or "toward"
angle_distance = 3       #distance from the edge at which direction after harvesting comes into play
date = "2019_12_09_toward"           #fill in with today's date (year_month_day) and create this folder in Data Files
nsim = 20
source("/home/macquees/Desktop/Resubmission2/Code/Functions.R")                    #source the functions file


###############################################################################################
landscape = "2"                 #change as needed for appropriate landscape (2,3,4,6,7)

source("/home/macquees/Desktop/Resubmission2/Code/Simulations_Master_Nohup.R")

#make plots from the last simulation 

#determine the color scheme for the heatmaps

png(filename = paste("/home/macquees/Desktop/Resubmission2/Code/Plot_Intensity_toward_Landscape_",landscape,sep=""),width=2000,height=2000)
image.plot(t(apply(Count_blueberry_visits/max(Count_blueberry_visits), 2, rev)), col = rev(colors), breaks = breaks, axes=FALSE,
           xlab = "", ylab = "", main = "Intensity of Flowers Visited on Each Blueberry Bush")

# plot(proportion3*100,xlab="number of visits", ylab = "percent of blueberry bushes",pch = 20,ylim = c(0,50))
dev.off()

###############################################################################################
###############################################################################################
landscape = "3"                 #change as needed for appropriate landscape (2,3,4,6,7)

source("/home/macquees/Desktop/Resubmission2/Code/Simulations_Master_Nohup.R")

#make plots from the last simulation 

#determine the color scheme for the heatmaps

png(filename = paste("/home/macquees/Desktop/Resubmission2/Code/Plot_Intensity_toward_Landscape_",landscape,sep=""),width=2000,height=2000)
image.plot(t(apply(Count_blueberry_visits/max(Count_blueberry_visits), 2, rev)), col = rev(colors), breaks = breaks, axes=FALSE,
           xlab = "", ylab = "", main = "Intensity of Flowers Visited on Each Blueberry Bush")

# plot(proportion3*100,xlab="number of visits", ylab = "percent of blueberry bushes",pch = 20,ylim = c(0,50))
dev.off()

###############################################################################################
###############################################################################################
landscape = "4"                 #change as needed for appropriate landscape (2,3,4,6,7)

source("/home/macquees/Desktop/Resubmission2/Code/Simulations_Master_Nohup.R")

#make plots from the last simulation 

#determine the color scheme for the heatmaps

png(filename = paste("/home/macquees/Desktop/Resubmission2/Code/Plot_Intensity_toward_Landscape_",landscape,sep=""),width=2000,height=2000)
image.plot(t(apply(Count_blueberry_visits/max(Count_blueberry_visits), 2, rev)), col = rev(colors), breaks = breaks, axes=FALSE,
           xlab = "", ylab = "", main = "Intensity of Flowers Visited on Each Blueberry Bush")

# plot(proportion3*100,xlab="number of visits", ylab = "percent of blueberry bushes",pch = 20,ylim = c(0,50))
dev.off()

###############################################################################################
###############################################################################################
landscape = "6"                 #change as needed for appropriate landscape (2,3,4,6,7)

source("/home/macquees/Desktop/Resubmission2/Code/Simulations_Master_Nohup.R")

#make plots from the last simulation 

#determine the color scheme for the heatmaps

png(filename = paste("/home/macquees/Desktop/Resubmission2/Code/Plot_Intensity_toward_Landscape_",landscape,sep=""),width=2000,height=2000)
image.plot(t(apply(Count_blueberry_visits/max(Count_blueberry_visits), 2, rev)), col = rev(colors), breaks = breaks, axes=FALSE,
           xlab = "", ylab = "", main = "Intensity of Flowers Visited on Each Blueberry Bush")

# plot(proportion3*100,xlab="number of visits", ylab = "percent of blueberry bushes",pch = 20,ylim = c(0,50))
dev.off()

###############################################################################################
###############################################################################################
landscape = "7"                 #change as needed for appropriate landscape (2,3,4,6,7)

source("/home/macquees/Desktop/Resubmission2/Code/Simulations_Master_Nohup.R")

#make plots from the last simulation 

#determine the color scheme for the heatmaps

png(filename = paste("/home/macquees/Desktop/Resubmission2/Code/Plot_Intensity_toward_Landscape_",landscape,sep=""),width=2000,height=2000)
image.plot(t(apply(Count_blueberry_visits/max(Count_blueberry_visits), 2, rev)), col = rev(colors), breaks = breaks, axes=FALSE,
           xlab = "", ylab = "", main = "Intensity of Flowers Visited on Each Blueberry Bush")

# plot(proportion3*100,xlab="number of visits", ylab = "percent of blueberry bushes",pch = 20,ylim = c(0,50))
dev.off()

###############################################################################################

Sys.time()