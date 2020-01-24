#Movement from MultipleBees5
#Bee does not harvest initially until close enough to memory spot, but then goes anywhere
#most parameters and storage set in All_The_Simulations_Raster

Nest_Resource = c(1,1)  #amount of (wild/yellow, blueberry/white) resource stored in the nest (set to (1,1) initially to avoid divide by 0 errors) - needs to be reset for each simulation
NEST_RESOURCE = matrix(NA, nrow = nsim, ncol = 2)    #store the nest resource from each simulation


#### Storage 
start = numeric(n_bees) #store the starting location indices for each bee's locations
end = numeric(n_bees)   #store the ending location indices for each bee's locations
starth = numeric(n_bees) #store the starting location indices for each bee's harvested locations
endh = numeric(n_bees)   #store the ending location indices for each bee's harvested locations

DATAX=rep(NA,n_bees*10000)                   #All X locations of all bees
DATAY=rep(NA,n_bees*10000)                   #All Y locations of all bees
DATAXharvested=rep(NA,n_bees*10000)                   #All X locations of all harvested flowers
DATAYharvested=rep(NA,n_bees*10000)                   #All Y locations of all harvested flowers
DATAharvested = rep(0,n_bees*10000)         #Whether harvested there
DATAbee_number=rep(NA,n_bees*10000)          #Which bee this is
DATAbee_number_harvested=rep(NA,n_bees*10000) #bee number at only harvested flowers
DATAtype = rep(NA,n_bees*10000)              #Which flower type at all locations
DATAtype_harvested=rep(NA,n_bees*10000)      #flower type at only harvested flowers
DATAcounter = rep(NA,n_bees*10000)           #How many times the bee has been a harvester so far
locations = 1                                #number of locations that have been visited, to index through above vectors

Bees_on_bushes=list(0)

incomplete_bouts = 0  #count the number of incomplete bouts made by all bees
total_bouts = 0       #count the total number of bouts made by all bees (just to be safe)

total_collected = 0   #total amount of resource collected this simulation

for(bee in 1:n_bees){
  print(paste("landscape", landscape,"simulation ", sim_number, "foraging bee ", bee))
  start[bee] = locations    #this is where this bee starts moving
  starth[bee] = sum(DATAharvested,na.rm=T)+1   #this is where this bee starts harvesting
  
  #################################Do the thing####################################################
  ### Initial values and storage
  
  bouts_counter=0
  i = 2
  X = rep(NA,1000*bouts)                 #Create vectors for X and Y location of bee at end of each segment (might not need these)
  Y = rep(NA,1000*bouts)
  diffusion_counter = 0                  #how many times the bee has been in harvester mode
  
  DATAX[locations] = nx                  #bee starts at the nest
  DATAY[locations] = ny  
  # DATAharvested[locations] = 0          #the bee does not harvest at the nest
  DATAbee_number[locations] = bee       #which bee it is
  DATAtype[locations] = 0               #we'll pretend there's no resource, whether or not that's true, because the bee doesn't harvest right at the nest
  DATAcounter[locations] = diffusion_counter     #how many times the  bee has been a harvester
  locations = locations + 1             #increase 'locations' after every movement of any type (being at the nest counts as a movement here)
  
  Status = numeric(1000*bouts)         #store which movement mode the bee is in (not sure if I need this -- maybe add to DATA)
  Status[1]=0                          #initially the bee is a scout
  walks = 0                 #track how many scouting steps the bee has made --  maybe add this to DATA
  
  memory_this_bee = matrix(memory_locations[which(memory_locations[,4]==bee),],ncol=5)  #the memory locations for this bee
  where = where_to_go()                      #decide where to remember
  mx = where[1]                 #x and y coordinates of remembered location
  my = where[2]
  All_Bees_Memories[bee,] = where
  
  BeeID = c(0, nx, ny, Turning2(nx,ny,Memory(nx,ny,mx,my),nx,ny,max_dist),0,where[3],0)  
  #1 population status: 1=Harvester; 0=Scout; 2 = returning to nest
  #2,3 location: x & y coordinates of bee (nest is at (nx,ny))
  #4 angle: direction of travel (initial angle based on memory direction)
  #5 amount of resource
  #6 flower type to search for; 1 for wildflower and 2 for blueberry
  #7 whether or not the bee has encountered flowers since leaving the nest (0=no,1=yes)
  
  
  
  
  ### Make the bee move around!
  
  while(bouts_counter<bouts){   ## do things for bouts steps ##
    
    if(BeeID[1]==0){  ## what to do if the bee is a scout ##
      
      ## The bee will advect first
      theta = BeeID[4]                             #the angle the bee travels at
      advect_time = rexp(1,mu)                     #the time to travel for
      new_x = DATAX[locations-1]+a*cos(theta)*advect_time    #the new location the bee travels to (velocity*time)
      new_y = DATAY[locations-1]+a*sin(theta)*advect_time    #the new location the bee travels to (velocity*time)		
      walks = walks+1	                                   #increase number of scouting steps
      
      memory = Memory(new_x,new_y,mx,my)           #the direction of the remembered location
      memory_distance = sqrt((new_x-mx)^2+(new_y-my)^2)  #the distance from the memory spot
      nest_distance = sqrt((new_x-nx)^2+(new_y-ny)^2)    #the distance from the nest
      Fl = flowers(new_x, new_y)       #the flower value
      
      #then the bee will decide what to do next 
      if(walks<=too_long_f){ #not scouting for too long, so check other stuff
        if(nest_distance<=max_dist && BeeID[7]==1 && Fl[1]>0){ #condition 1
          BeeID[1] = 1       #the bee becomes a harvester
        }else if(nest_distance<=max_dist && BeeID[7]==1 && Fl[1]==0){ #condition 2
          BeeID[1] = 0                                      #stay a scout	
          BeeID[4] = Turning2(new_x,new_y,theta,nx,ny,max_dist)   #choose a new angle based on previous direction of travel
        }else if(nest_distance<=max_dist && BeeID[7]==0 && memory_distance>=memory_radius){ #condition 3
          BeeID[1] = 0                                      #stay a scout
          BeeID[4] = Turning2(new_x,new_y,memory,nx,ny,max_dist)  #choose a new angle based on memory
        }else if(nest_distance<=max_dist && BeeID[7]==0 && memory_distance<memory_radius && Fl[1]==BeeID[6]){ #condition 4
          BeeID[1] = 1       #the bee becomes a harvester
        }else if(nest_distance<=max_dist && BeeID[7]==0 && memory_distance<memory_radius && Fl[1]!=BeeID[6]){ #condition 5
          BeeID[1] = 0                                      #stay a scout	
          BeeID[4] = Turning2(new_x,new_y,theta,nx,ny,max_dist)   #choose a new angle based on previous direction of travel
        }else{ #condition 6
          BeeID[1] = 0                                       #stay a scout	
          BeeID[4] = Turning2(new_x,new_y,memory,nx,ny,max_dist)   #choose a new angle (will be back towards nest)
        }
      }else{ #condition 7 (scouting too long)
        BeeID[1] = 2                              #become a returner
        incomplete_bouts=incomplete_bouts+1
        Nest_Resource = Nest_Resource + resource     #store the amount of resource collected in this segment
        BeeID[5] = BeeID[5]+sum(resource)            #total amount of resource collected on this bout. 
        
      }
      
      #Store the results of the Scouting segment
      DATAX[locations] = new_x              #the new x and y locations    
      DATAY[locations] = new_y  
      DATAbee_number[locations] = bee       #which bee this is 
      DATAtype[locations] = Fl[1]           #what resource type the bee ended the segment in               
      DATAcounter[locations] = diffusion_counter    #how many times the bee has been a harvester so far
      locations = locations + 1             #increase 'locations' after every movement of any type
      Status[i] = BeeID[1]
      #end of Scouting 
      
    }else if(BeeID[1]==1){  ## what to do if the bee is a harvester ##
      walks = 0                                           #reset the number of scouting steps the bee has made
      diffusion_counter = diffusion_counter+1             #how many times the bee has been a harvester
      BeeID[7]=1                                          #bee has been a harvester now
      harvested = sum(DATAharvested)                      #the number of flowers the bee has harvested from 
      resource = c(0,0)              #the bee hasn't collected anything yet this harvesting segment    -- maybe add this to DATA 
      
      #Check all the stuff for the current location to determine if the bee harvests here
      distancex_indices = which(abs(DATAX[locations-1]-DATAXharvested)<depletion_dist)   #just the points too close to the location in the x direction
      harvestedy = DATAYharvested[distancex_indices]                                     #the corresponding y points
      distancey_indices=which(abs(DATAY[locations-1]-harvestedy)<depletion_dist)         #which of those points is also too close in the y direction
      depleted = length(distancey_indices)                                               #how many depleted spots the bee is too close to
      Fl = flowers(DATAX[locations-1],DATAY[locations-1])                #flower value at location
      
           
      if(depleted==0){ #the flowers are undepleted
        resource[Fl] = resource[Fl] + resource_unit                    #add resource unit
        DATAXharvested[harvested+1] = DATAX[locations-1]       #harvested+1 is the number of flowers harvested from including this one
        DATAYharvested[harvested+1] = DATAY[locations-1]       #locations-1 is the index of the location it landed
        DATAbee_number_harvested[harvested+1] = bee
        DATAtype_harvested[harvested+1] = Fl[1]
        DATAharvested[locations-1] = 1
        harvested = harvested+1     #the bee has harvested at one more flower
      }else{ #the flowers are depleted
        DATAharvested[locations-1] = 0                 #have to reset this here, because it was set to 1 at the end of scouting
      }
      
      #end of checking the location the bee arrived to the flower patch at
      
      #do the rest of the harvesting stuff
      harvest_time = sum(rexp(2,gamma1))               #how long the bee will stay a harvester for
      steps = max(1,floor(harvest_time/(handling_time+deltat)))   #how many steps the bee will make 
      
      for(j in 2:(steps+1)){   ## the bee diffuses/random walks for a while ##
        walk = rnorm(2,0,1)                                                              #random distances to travel in x and y directions
        grad = flowers_gradient(DATAX[locations-1], DATAY[locations-1])                  # "gradient" of flower field at current location
        DATAX[locations] = DATAX[locations-1]+sqrt(2*D*deltat)*walk[1] + grad[1]*deltat        #where the bee travels to        							
        DATAY[locations] = DATAY[locations-1]+sqrt(2*D*deltat)*walk[2] + grad[2]*deltat        #where the bee travels to
        Fl = flowers(DATAX[locations], DATAY[locations])                                   #flower value at the new location

        if(Fl>0){ #the bee is in flowers, so check for depletion
          distancex_indices = which(abs(DATAX[locations]-DATAXharvested)<depletion_dist)     #just the points too close to the location in the x direction
          harvestedy = DATAYharvested[distancex_indices]                                     #the corresponding y points
          distancey_indices=which(abs(DATAY[locations]-harvestedy)<depletion_dist)           #which of those points is also too close in the y direction
          depleted = length(distancey_indices)                                               #how many depleted spots the bee is too close to
        
          if(depleted==0){ #the  flowers are undepleted
            resource[Fl] = resource[Fl] + resource_unit                    #add resource unit
            DATAXharvested[harvested+1] = DATAX[locations]         #this is a location where the bee harvested
            DATAYharvested[harvested+1] = DATAY[locations]
            DATAharvested[locations] = 1                      #harvested here 
            DATAbee_number_harvested[harvested+1] = bee
            DATAtype_harvested[harvested+1] = Fl
            harvested = harvested+1
          }else{ #the flowers are depleted
            DATAharvested[locations] = 0              #the bee did not collect resource here
                        
          }
        
       }else{ #the bee is not in flowers
          DATAharvested[locations] = 0              #the bee did not collect resource here
        }
        
        DATAbee_number[locations] = bee        #which bee this is 
        DATAtype[locations] = Fl[1]            #which type of flowers it just harvested    
        DATAcounter[locations] = diffusion_counter   #how many times it's been a harvester
        locations = locations + 1             #increase 'locations' after every movement of any type
        
      }	#end of harvesting movement loop 
      
      Nest_Resource = Nest_Resource + resource     #store the amount of resource collected in this segment
      BeeID[5] = BeeID[5]+sum(resource)            #total amount of resource collected on this bout. 
      
      if(BeeID[5] >= full){ #the bee is full
        BeeID[1] = 2                    #become a returner
      }else{ #the bee is not full
        BeeID[1] = 0                    #switch back to beeing a scout
        BeeID[4] = Turning4(DATAX[locations-1], DATAY[locations-1],post_harvest_angle)       #pick a new angle to travel at as scout
        
      }
      Status[i] = BeeID[1]
      #done doing the harvesting stuff
      
    }else if(BeeID[1] == 2){  ## what to do if the bee is a returner
      #count the bouts complete/incomplete
      total_bouts = total_bouts+1
      total_collected = total_collected + BeeID[5]
      
      walks = 0                                   #reset number of scouting steps bee has made
      DATAX[locations] = nx                                  #send bee back to nest
      DATAY[locations] = ny                                  #send bee back to nest
      DATAharvested[locations] = 0              #the bee did not collect resource here
      DATAbee_number[locations] = bee           #which bee this is 
      DATAtype[locations] = 0                   #no harvesting at the nest    
      DATAcounter[locations] = diffusion_counter   #how many times it's been a harvester
      locations = locations + 1                    #increase 'locations' after every movement of any type
      
      # Times[i] = sqrt(BeeID[1]^2+BeeID[2]^2)/a    #calculate time to get back to nest
      BeeID[5] = 0                                  #empty the resource
      BeeID[1] = 0                                  #make the bee a scout
      BeeID[4] = Turning2(nx,ny,Memory(nx,ny,mx,my),nx,ny,max_dist)    #new angle based on memory
      bouts_counter = bouts_counter+1               #the bee has completed another bout
      BeeID[7] = 0                                  #reset tracker for whether or not bee has found flowers since leaving nest
      Status[i] = BeeID[1]
    }
    
    #Store/update things
    BeeID[2] = DATAX[locations-1]		    #bee's location
    BeeID[3] = DATAY[locations-1]		    #bee's location
    # Theta[i] = BeeID[4]               #bee's direction of travel     
    i = i+1
    
  } #end of movement loops
  
  end[bee] = locations-1 #don't include the +1 from the very last return to nest
  endh[bee] = sum(DATAharvested,na.rm=T)  
  
  ## Make a matrix of the data for just this bee
  start1 = start[bee]
  end1 = end[bee]
  starth1 = starth[bee]
  endh1 = endh[bee]
  
  DATA = matrix(c(DATAX[start1:end1],DATAY[start1:end1],DATAharvested[start1:end1],DATAbee_number[start1:end1],DATAtype[start1:end1]),ncol=5)
  HARVESTED = matrix(c(DATAXharvested[starth1:endh1],DATAYharvested[starth1:endh1],DATAbee_number_harvested[starth1:endh1],DATAtype_harvested[starth1:endh1]),ncol=4)
  HARVESTED_blueberry = matrix(HARVESTED[HARVESTED[,4]==2,],ncol=4)  #just the rows that are blueberry
   
   
    
  # ### Show a plot of where the bee went!
  # Xp = DATA[,1]
  # Yp = DATA[,2]
  # xmin = min(0,min(Xp))     #so that the whole path will show on the plot
  # ymin = min(0,min(Yp))
  # xmax = max(153,max(Xp))
  # ymax = max(225,max(Yp))
  # 
  # s = 1:(end1-1)
  # x_plot = seq(x_min,x_max,.1)  #just in case
  # y_plot = seq(y_min,y_max,.1)
  # 
  # image(x_plot,y_plot,field, xlab = "x", ylab = "y", xlim = c(0,150), ylim = c(0,225),
  # col=c("grey85","yellow1","lightcyan"),asp=TRUE)
  # segments(Xp[s],Yp[s],Xp[s+1],Yp[s+1], lty=1,col="grey")     #add lines for the foraging path
  # 
  # points(HARVESTED[,1],HARVESTED[,2], col = "blue",pch=20,cex=.1)    #add the flower visit points
  # 
  # points(mx,my, col = "red",cex=1)          #make the memory spot obvious
  # points(nx,ny,pch=20,cex=1.5)              #make the nest obvious

  
  ## Determine which bushes  were visited by this bee
  PP <- ppp(x=HARVESTED_blueberry[,1], y=HARVESTED_blueberry[,2],window = W_b)   #the planar point pattern of the blueberry visited points
  if(length(PP$x)>0){ #if the bee visited any blueberries, count it up
    Count = quadratcount(PP, xbreaks = sort(c(left_edges_b,right_edges_b)), ybreaks=ybreaks_b)        #uses x and y breaks specific to the field, set in landscape file
    Count[which(Count>=1)]=1      #just set the field to 1 if the bee visited it at all
    Bees_on_bushes[[bee]] = Count    #save the data for this bee
  }
  
  
} #end of bumble bees loops


#Save the nest resource for this simulation
NEST_RESOURCE[nsim,] = Nest_Resource

##### Save the points of all bees into a single matrix
DATA_All_Bees_harvested = cbind(DATAXharvested,DATAYharvested,DATAbee_number_harvested,DATAtype_harvested)            #only the points where resource was collected
DATA_All_Bees_blueberry= DATA_All_Bees_harvested[DATAtype_harvested==2,]                                 #only the points where blueberry was collected
DATA_All_Bees_harvested = na.omit(DATA_All_Bees_harvested)                                               #remove all the extra NAs at the end
DATA_All_Bees_blueberry = na.omit(DATA_All_Bees_blueberry)                                               #remove all the extra NAs at the end

##Count how many bees visited each "bush"
Total_Bees_on_Bushes = Reduce("+",Bees_on_bushes)     #add together all of the bee field visit counts


##Count how many blueberry flowers were visited (same as above, but blueberry only)
PP_blueberry_visits <- ppp(x=DATA_All_Bees_blueberry[,1], y=DATA_All_Bees_blueberry[,2],window = W_b)   #the planar point pattern
Count_blueberry_visits = quadratcount(PP_blueberry_visits, xbreaks = sort(c(left_edges_b,right_edges_b)), ybreaks = 0:225)

#the percent of bushes with at least x flower visits
proportion3 = numeric(max(Count_blueberry_visits))
for(i in 1:(max(Count_blueberry_visits)+1)){
  proportion3[i] = length(which(Count_blueberry_visits>(i-1)))/(n_bushes)
}













