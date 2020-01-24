# 
#  install.packages("circular")
#  install.packages("spatstat")
#  install.packages("fields")
#  install.packages("car")


library(fields)
library(circular)
library(spatstat)
library(car)




Turning2 = function(x,y,theta,n1=0,n2=0,max=1000){ ## the turning angle distribution ##
	t_x = x-n1            #transform so that (n1,n2) = (0,0)
	t_y = y-n2
	dist = sqrt(t_x^2+t_y^2)
	if(dist <= max){
		angle = rvonmises(1,circular(theta),kappa_s,control.circular=list(units="radians"))             
					#von Mises, centered at the angle the bee is traveling at, to make small angle changes more likely
		      #3rd entry controls how "focused" the direction is

	}else if(t_x<0 && t_y<0){
		angle = runif(1,acos(-t_x/dist)-pi/16, acos(-t_x/dist)+pi/16)
	}else if(t_x>0 && t_y<0){
		angle = runif(1,pi - acos(t_x/dist)-pi/16, pi - acos(t_x/dist)+pi/16)
	}else if(t_x>0 && t_y>0){
		angle = runif(1,pi + acos(t_x/dist)-pi/16,pi + acos(t_x/dist)+pi/16)
	}else if(t_x<0 && t_y>0){
		angle = runif(1,2*pi - acos(-t_x/dist)-pi/16,2*pi - acos(-t_x/dist)+pi/16)
	}	
	return(angle)
}

Turning3 = function(x,y,theta,n1=0,n2=0,max=11){ ## the turning angle distribution for exploration ##
  t_x = x-n1            #transform so that (n1,n2) = (0,0)
  t_y = y-n2
  dist = sqrt(t_x^2+t_y^2)
  if(dist <=max1){    
    #not very far away from nest, so continue in initial direction of travel
    angle = rvonmises(1,circular(theta),40,control.circular=list(units="radians"))             
    #von Mises, centered at the angle the bee is traveling at, to make small angle changes more likely
    
  }else if(max1 <= dist && dist <= max2){   
    #some distance away from the nest, but not too far away, so travel perpendicularly to nest direction
    
    #calculate direction to nest
    if(t_x<0 && t_y<0){
      angle1 = acos(-t_x/dist)
    }else if(t_x>0 && t_y<0){
      angle1 = pi - acos(t_x/dist)
    }else if(t_x>0 && t_y>0){
      angle1 = pi + acos(t_x/dist)
    }else if(t_x<0 && t_y>0){
      angle1 = 2*pi - acos(-t_x/dist)
    }
    angle2 = sample(x = c(angle1+(pi/2),angle1,angle1+pi), size = 1, prob = c(.6,.35,.05))    #perpendicular to nest direction; continue in same direction; towards nest
    angle = rvonmises(1,circular(angle2),40,control.circular=list(units="radians"))   #use vonM for randomness
    
  }else{
    #too far away, so go back towards the nest
    if(t_x<0 && t_y<0){
      angle1 = acos(-t_x/dist)
    }else if(t_x>0 && t_y<0){
      angle1 = pi - acos(t_x/dist)
    }else if(t_x>0 && t_y>0){
      angle1 = pi + acos(t_x/dist)
    }else if(t_x<0 && t_y>0){
      angle1 = 2*pi - acos(-t_x/dist)
    }
    angle2 = sample(x=c(angle1,angle1+pi),size=1,prob=c(.7,.3))  #back towards nest; continue in same direction
    angle = rvonmises(1,circular(angle2),40,control.circular=list(units="radians"))   #use vonM for randomness
    
  }	
  return(angle)
}

Turning4 = function(x,y,post_harvest_direction){
  if(post_harvest_direction=="random"){
    angle = runif(1,0,2*pi)
  }else if(post_harvest_direction=="toward"){
    
    if(x<=angle_distance){
      theta = 0
      angle = rvonmises(1,circular(theta),kappa_s,control.circular=list(units="radians")) 
    }else if(x>=(max(row_max_x, wild_max_x)-angle_distance)){
      theta = pi
      angle = rvonmises(1,circular(theta),kappa_s,control.circular=list(units="radians")) 
    }else if(y<=angle_distance){
      theta = pi/2
      angle = rvonmises(1,circular(theta),kappa_s,control.circular=list(units="radians")) 
    }else if(y>=(max(row_max_y, wild_max_y)-angle_distance)){
      theta=(3*pi)/2
      angle = rvonmises(1,circular(theta),kappa_s,control.circular=list(units="radians")) 
    }else{
      angle = runif(1,0,2*pi)
    }

  }else if(post_harvest_direction=="away"){
    
    if(x<=angle_distance){
      theta = pi
      angle = rvonmises(1,circular(theta),40,control.circular=list(units="radians")) 
    }else if(x>=(max(row_max_x, wild_max_x)-angle_distance)){
      theta = 0
      angle = rvonmises(1,circular(theta),40,control.circular=list(units="radians")) 
    }else if(y<=angle_distance){
      theta = 3*pi/2
      angle = rvonmises(1,circular(theta),40,control.circular=list(units="radians")) 
    }else if(y>=(max(row_max_y, wild_max_y)-angle_distance)){
      theta=(pi)/2
      angle = rvonmises(1,circular(theta),40,control.circular=list(units="radians")) 
    }else{
      angle = runif(1,0,2*pi)
    }

  }
  return(angle)
}

Memory = function(x,y,m1,m2){ ## the memory angle calculator 
	#(x,y) is location of bee
	#(m1,m2) is memory
	t_1 = m1-x    #memory location transformed so that bee is at (0,0)
	t_2 = m2-y
	h = sqrt(t_1^2+t_2^2)
	
	if(t_1>=0 && t_2>=0){
		angle = acos(t_1/h)
	}else if(t_1<0 && t_2>=0){
		angle = pi - acos(-t_1/h)
	}else if(t_1<=0 && t_2<0){
		angle = pi + acos(-t_1/h)
	}else if(t_1>0 && t_2<=0){
		angle = 3*pi/2 + acos(-t_2/h)
		}	
	return(angle)
}


where_to_go = function(){
  
  total = sum(Nest_Resource)
  
  blueberry_proportion = Nest_Resource[2]/total
  wildflower_proportion = Nest_Resource[1]/total
  
  blueberry_locations = which(memory_this_bee[,3]==2,arr.ind=TRUE)
  wildflower_locations = which(memory_this_bee[,3]==1,arr.ind=TRUE)
  
  blueberry_length = length(blueberry_locations)
  wildflower_length = length(wildflower_locations)
  
  if(length(blueberry_locations)>0 && length(wildflower_locations)>0){
    #The bee has found both wildflowers and blueberries in its searching, so pick from the one the nest needs
      if(blueberry_proportion<blueberry_preference){
        #the nest doesn't have enough blueberry pollen, so pick memory point from blueberries
        memory_type = 2
        if(blueberry_length>1){
          location = sample(blueberry_locations,1)
          mx = memory_this_bee[location,1]
          my = memory_this_bee[location,2]
        }else{
          location = blueberry_locations
          mx = memory_this_bee[location,1]
          my = memory_this_bee[location,2]
        }

    }else if(wildflower_proportion<wildflower_preference){
        #the nest doesn't have enough wildflower pollen, so pick memory point from wildflowers
        memory_type = 1
        if(wildflower_length>1){
         location = sample(wildflower_locations,1)
          mx = memory_this_bee[location,1]
          my = memory_this_bee[location,2]         
        }else{
          location = wildflower_locations
          mx = memory_this_bee[location,1]
          my = memory_this_bee[location,2]
        }

    }else{
        #the two types are magically equal, so pick randomly
        memory_type = sample(c(1,2),1)
        locations = which(memory_this_bee[,3]==memory_type,arr.ind=TRUE)
        if(length(locations)>1){
          location = sample(locations,1)
          mx = memory_this_bee[location,1]
          my = memory_this_bee[location,2]          
        }else{
          location = locations
          mx = memory_this_bee[location,1]
          my = memory_this_bee[location,2]          
        }

    }
  }else if(length(blueberry_locations)==0 & length(wildflower_locations)>0){
    #the bee has not found any blueberries, so it can only harvest from wildflowers
    memory_type = 1
    if(wildflower_length>1){
      location = sample(wildflower_locations,1)
      mx = memory_this_bee[location,1]
      my = memory_this_bee[location,2]         
    }else{
      location = wildflower_locations
      mx = memory_this_bee[location,1]
      my = memory_this_bee[location,2]
      
    }
  }else if(length(blueberry_locations)>0 & length(wildflower_locations)==0){
    #the bee has not found any wildflowers, so it can only harvest from blueberries
    memory_type = 2
    if(blueberry_length>1){
      location = sample(blueberry_locations,1)
      mx = memory_this_bee[location,1]
      my = memory_this_bee[location,2]
    }else{
      location = blueberry_locations
      mx = memory_this_bee[location,1]
      my = memory_this_bee[location,2]
    }
  }else{
    #the bee has not found any flowers, so I'm not sure what should happen
    mx = 78
    my = 150
    memory_type = 2  #I think this will make the bee harvest blueberries
  }

  return(c(mx,my,memory_type))
  
}

sides = matrix(c(1,0,-1,0,0,1,0,-1,2,0,-2,0,0,2,0,-2),nrow=8,byrow=TRUE)

Edge_Checker = function(x,y){
  #checks the four direct neighbor cells to see if any of them are non-flower
  #uses random order of checking, and uses the first found non-flower neighbor cell for gradient direction 
  #to avoid bias in gradient direction at corners
  
  #x and y are grid cell values of bee's current location
  
  order = sample(1:8,8,replace=FALSE)
  b=c(0,0)
  i = 1
  while(i<=8 && b==c(0,0)){
    side = sides[order[i],]
    cell_to_check = c(x,y)+side
    if(landscape_values[cell_to_check[1],cell_to_check[2]]==0){
      b = -side
    }
    i = i+1
  }
  return(b)
}




