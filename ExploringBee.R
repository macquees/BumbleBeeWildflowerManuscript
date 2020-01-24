
### Parameters

# mu_e = 1            #rate of switching angle while scouting  (lower rate = higher times more likely)
# a_e =   4.5             #advection speed (m/s) observed in a paper
delta_X = 0.1       #increment size for finding memory locations

####################################################
memory_locations = c(0,0,0,0,0)


for(bee in 1:n_bees){
  print(paste("landscape", landscape,"simulation ", sim_number, "exploring bee ", bee))
  
  #################################Do the thing####################################################
  ### Initial values and storage

    
  bouts_e_counter=1
  i = 2
  X = rep(NA,100*bouts_e)                 #fill X and Y with the initial point (nest coordinates)
  Y = rep(NA,100*bouts_e)
  X[1] = nx 
  Y[1] = ny      
  Theta = numeric(100*bouts_e)           #store all of the turning angles
  Status = numeric(100*bouts_e)            #store which population the bee is in
  Status[1]=0                         #initially the bee is a scout
  Times = numeric(100*bouts_e)             #store the inter-event intervals
  Steps = rep(0,100*bouts_e)               #store how many steps the bee took when it was diffusing
  diffusion_counter = 0               #how many times the bee has been in harvester mode
  Type = 0
  Patch = 0
  Counter = 0
  walks = 0                 #track how many scouting steps the bee has made
  too_long = 150            #define how many steps to take
  max1 = 30             #initial radii of looping
  max2 = 35


  BeeID = c(0, nx, ny, runif(1,0,2*pi),0,1,0)
  #1 population status: 1=Harvester; 0=Scout; 2 = returning to nest
  #2,3 location: x & y coordinates of bee (nest is at (nx,ny))
  #4 angle: direction of travel (initial angle from memory direction)
  #5 amount of resource
  #6 flower type to search for; 1 for wildflower and 2 for blueberry
  #7 whether or not the bee has encountered flowers since leaving the nest (0=no,1=yes)
  Theta[1] = BeeID[4]
  
  
  
  ### Make the bee move around! (in scouting mode only)
  
  while(bouts_e_counter<bouts_e){   ## do things for bouts_e steps ##
    theta = BeeID[4]                #the angle the bee is going at
    

    if(BeeID[1] == 2){  ## what to do if the bee is done with an exploring bout
            
      new_y = ny
      new_x = nx                           #send bee back to nest
      Times[i-1] = sqrt(BeeID[1]^2+BeeID[2]^2)/a    #calculate time to get back to nest
      Status[i] = 0                                #make the bee a scout
      BeeID[4] = runif(1,0,2*pi)                 #new random angle 
      bouts_e_counter = bouts_e_counter+1            #the bee has completed another bout
      BeeID[7] = 0                               #reset tracker for whether or not bee has found flowers since leaving nest
      max1 = 35*(bouts_e_counter-1)+100             #increase initial radii of looping
      max2 = 35*(bouts_e_counter-1)+105
      too_long = too_long+20
      
    }else if(BeeID[1]==0){## The bee is still exploring
      theta = BeeID[4]                             #the angle the bee travels at
      advect_time = rexp(1,mu)                     #the time to travel for
      Times[i-1] = advect_time                       #store the time
      new_x = BeeID[2]+a*cos(theta)*advect_time    #the new location the bee travels to (velocity*time)
      new_y = BeeID[3]+a*sin(theta)*advect_time    #the new location the bee travels to (velocity*time)		
      where = sqrt((new_x-nx)^2+(new_y-ny)^2)  #where the bee is
      
      ## Then check distance from nest to decide what the next move will be
      if(walks >= too_long){  #the bee has been scouting long enough, so send it back to the nest 
        Status[i] = 2
        walks = 0                               #reset how many scouting steps the bee has made

      }else {  #the bee has not been scouting long enough, so keep going
        BeeID[4] = Turning3(new_x,new_y,theta,nx,ny,max_dist)   #choose a new angle 
        Status[i] = 0                            #stay a scout	
        walks = walks+1				
      }
      
    }
    
    ## Store/update things
    X[i] = new_x            #bee's location at end of event
    Y[i] = new_y            #bee's location at end of event
    BeeID[1] = Status[i]    #bee's population status
    BeeID[2] = new_x		
    BeeID[3] = new_y
    Theta[i] = BeeID[4]  
        
    
    
    ## Now calculate any potential memory locations 
    
   m = (Y[i]-Y[i-1])/(X[i]-X[i-1])
   order = sort(c(round(X[i-1],2),round(X[i],2)))
   X_values = seq(order[1],order[2],delta_X)
   Y_values = m*(X_values-X[i-1])+Y[i-1]
   flower_values = numeric(length(X_values))
   for(index in 1:length(X_values)){
   	flower_values[index] = flowers(X_values[index],Y_values[index])
   }
   bee_number = rep(bee,length(X_values))   #so that I can double check that all bees are getting added to memory_locations
   bout_number = rep(bouts_e_counter,length(X_values))  #same for all bouts_e
   
   All = cbind(X_values,Y_values,flower_values,bee_number,bout_number)
    
   ##this needs to give x and y values, not flower field values##
   memory_locations =  rbind(memory_locations,All[which(All[,3]>0),])
   i = i+1
 
   
    
    
  }

}




memory_locations = memory_locations[-1,]


