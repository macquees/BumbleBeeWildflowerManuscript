#Run simulations from this file



##### Set all of the parameters and things that hold for all simulations ####


# Note: n_bees is set separately in each Landscape file becase it is different for the landscape with no wildflowers

gamma1 = .03                  #rate of becoming scout (switching to scouting from harvesting) - controls length of time harvesting
mu = 1                        #rate of switching angle while scouting  (lower rate = higher times more likely) - controls amount of time flying in a particular direction
a =   4.5                     #advection speed (m/s) observed in a paper
D =   .01                     #diffusion coefficient (variance)
full = 47.9                   #mg of forage (pollen and/or nectar collected) observed in a paper
resource_unit = 1.2           #weight of resource collected per flower
bouts = 7                     #number of bouts to make (7=1 single day, average from Woodgate)
depletion_dist = 0.0076       #the distance the bee must be from a depleted flower
max_dist = 2000               #max distance the bee will go from the nest (meters)
memory_radius = 5             #how close bee must get to memory location to start harvesting
blueberry_preference = 0.4    #proportion of bees that prefer blueberries
too_long_f = 100              #define how many scouting steps without finding resource is too many (this is different for exploration and foraging)
handling_time = 8.4           #flower handling time
deltat = 0.1                  #time step size for the random walk/diffusion 
kappa_s = 30                  #vonMises parameter
distance_to_nest = 65            #distance of nest from blb rows
angle_distance = 3       #distance from the edge at which direction after harvesting comes into play
bouts_e = 5


nx = 75          #coordinates of nest
ny = 10

n_bushes_x = 50
n_bushes_y = 150
n_bushes = n_bushes_x*n_bushes_y
bush_width_x = 2                    #width of bushes in meters (x-dimension)
bush_width_y = 1                    #width of bushes in meters (y-dimension)
x_meters = 150   #x dimension of landscape in meters
y_meters = 225  #y dimension of landscape in meters


wildflower_preference = 1-blueberry_preference

##############


filename_landscape = paste("Scaled Blueberry ",landscape,".R",sep="")                      #name to use if landscape file changes
source(paste("/home/macquees/Desktop/Resubmission2/Code/",filename_landscape,sep=""))    #source the right landscape file
#remember to uncomment "Evaluate the flower field" for plotting



PROPORTION_all = numeric(nsim)
PROPORTION_blue = numeric(nsim)

AREAS_all = numeric(nsim)       #the area of the convex hull/minimum bounding polygon of each simulation (all flower types)
AREAS_blue = numeric(nsim)       #the area of the convex hull/minimum bounding polygon of each simulation (blueberries only)

Total_Bouts = numeric(nsim)       #save the total and incomplete bouts for each simulation
Total_Collected = numeric(nsim)
Incomplete_Bouts = numeric(nsim)
All_Bees_Memories = matrix(NA, ncol = 3, nrow = n_bees)     #save each bee's actual memory point

for(sim_number in 1:nsim){
  
  #first have the bees explore, and save the memory locations for this simulation
  source("/home/macquees/Desktop/Resubmission2/Code/ExploringBee.R")
  
  
  #then have the bees do their foraging thing and save a bunch of stuff
  source("/home/macquees/Desktop/Resubmission2/Code/Multiple_Bees.R")
  
  #The actual memory points for each bee
  filename_memory = paste("Engage","MemoryPoints","Landscape",landscape,"Simulation",sim_number,sep="_")
  write.csv(All_Bees_Memories, file=paste("/home/macquees/Desktop/Resubmission2/Code",date,filename_memory,sep="/"))
  
  #The complete/incomplete bouts and the amount of pollen
  Total_Bouts[sim_number] = total_bouts
  Total_Collected[sim_number] = total_collected
  Incomplete_Bouts[sim_number] = incomplete_bouts
  
  #the number of unique bees visiting each bush
  filename3 = paste("Engage","TotalBeesOnFields","Landscape",landscape,"Simulation",sim_number,sep="_")
  dput(Total_Bees_on_Bushes,file=paste("/home/macquees/Desktop/Resubmission2/Code",date,filename3,sep="/"))
  
  #the total number of visits to each blueberry bush
  filename4a = paste("Engage","TotalBlueberryFlowerVisits","Landscape",landscape,"Simulation",sim_number,sep="_")
  dput(Count_blueberry_visits,file=paste("/home/macquees/Desktop/Resubmission2/Code",date,filename4a,sep="/"))    
  
  #the percent of blueberry bushes with at least x flower visits
  filename6a = paste("Engage","ProportionBlueberryVisits","Landscape",landscape,"Simulation",sim_number,sep="_")
  write.csv(proportion3,file=paste("/home/macquees/Desktop/Resubmission2/Code",date,filename6a,sep="/"))
  
  PROPORTION_blue[sim_number] = proportion3[1]   #percent of blueberry bushes where at least one flower was visited
  
  
}

filename8 = paste("Engage","PROPORTION","Landscape",landscape,sep="_")
write.csv(cbind(PROPORTION_blue),file=paste("/home/macquees/Desktop/Resubmission2/Code",date,filename8,sep="/"))



