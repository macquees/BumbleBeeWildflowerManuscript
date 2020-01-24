#creates a field to the specified dimensions
#vertical blueberry rows
#vertical wildflower row on each side
#nest coordinates and max distance from nest should be set here



x_min = -3
y_min = 0
x_max = 152
y_max = 225
row_min_x = 0
row_max_x = 150
row_min_y = 75
row_max_y = 225
wild_min_x = c(-3,150)
wild_max_x = c(-1,152)
wild_min_y = 75
wild_max_y = 225


nx = 75          #coordinates of nest
ny = 10

n_bushes = 50*150    #the number of bushes in this field
# n_bees=50      #scaled by 1/2 according to honeybee results, and 40% due to lack of wildflowers
W_b = owin(xrange = c(0,149), yrange = c(75,225))    #the window for counting number of bees on a bush
ybreaks_b=75:225

initial_nest_resource = c(300,200)  #initial amount of resources in the nest

delete = -seq(4,102,2)

# shiftx = 11       #how much the points will need to be shifted and scaled to plot on a 0 to 1 scale
# shifty = 11
# scalex = 22   
# scaley = 22
# 
# nx = 0          #coordinates of nest
# ny = -4
# max = 14      #max distance the bee will go from the nest (meters)


left_edges = seq(row_min_x,row_max_x-3,3)
right_edges = seq(row_min_x+2,row_max_x,3)

left_edges_b = left_edges
right_edges_b = right_edges

flowers = function(x,y){
  #returns 2 for blueberries (white), 1 for wild flowers (yellow), and 0 for no flowers (green)
  temp = 0
  i = 1
  if(row_min_y <= y && y <= row_max_y && row_min_x <=x && x <= row_max_x){ #in the blueberry fields
    while(temp == 0 && i <=length(left_edges)){
      if(left_edges[i]<=x && x<=right_edges[i]){
        temp = 2
      }
      i = i+1	
    }
  }else if(wild_min_y<=y && y<=wild_max_y){ #in wildflowers
    while(temp == 0 && i <=2){
      if(wild_min_x[i]<=x && x<=wild_max_x[i]){
        temp = 1
      }
      i = i+1	
    }
  }else{ #no flowers
    temp = 0
  }
  return(temp)
}



#gradients
l_left_edges = c(0,left_edges,148)
l_right_edges = c(0,left_edges,148)+0.5
r_left_edges = c(2,right_edges,150)-0.5
r_right_edges = c(2,right_edges,150)

flowers_gradient = function(x,y){
  tempx = 0
  tempy = 0
  fg = 1
  
  if(row_min_y<=y && y <= row_min_y+0.5){ #bottom of the rows (incl. wildflowers)
    tempy = 1
  }else if(row_max_y-0.5<=y && y <=row_max_y){ #top of the rows (incl. wildflowers)
    tempy = -1
  }else if(row_min_y+0.5 <= y && y <= row_max_y-0.5){ #rest of the rows (incl. wildflowers)
    while(tempx == 0 && tempy == 0 && fg <= length(l_left_edges)){
      if(l_left_edges[fg]<=x && x<=l_right_edges[fg]){ #left sides of rows (incl. wildflowers)
        tempx = 1
      }else if(r_left_edges[fg]<=x && x<=r_right_edges[fg]){ #right sides of rows (incl. wildflowers)
        tempx = -1
      }
      fg = fg+1	
    }
  }else{ #no gradient/no flowers
    tempx = 0
    tempy = 0
  }
  return(c(tempx,tempy))
}


# # Uncomment this to plot the landscape
# ### Evaluate the flower field
# x = seq(x_min,x_max,.1)  #to evaluate the function
# y = seq(y_min,y_max,.1)
# field = matrix(0,nrow = length(x), ncol = length(y))  #to store the function values
# for(u in 1:length(x)){  #evaluate 'flowers' at each point in the matrix
# for(v in 1:length(y)){
# field[u,v] = flowers(x[u],y[v])
# }
# }
# 
# image(x,y,field,col = terrain.colors(12), axes=FALSE,xlab = "",ylab ="",asp=TRUE)
# points(nx,ny,pch=20,cex=2)
# 
# # png("Landscape3.png",width = 1024, height = 768)
# # image(x,y,field,col=c("grey85","yellow1","lightcyan"),asp=TRUE, axes=FALSE,main = "wild flowers both sides",cex.main=2,
# #       xlab="",ylab="")
# # points(nx,ny,pch=20,cex=2)
# # dev.off()

# #### Uncomment this for random memory generation ####
# 
# blueberry_preference = 0.4
# wildflower_preference = 0.6
# n_blueberry_bees = blueberry_preference*n_bees
# n_wildflower_bees = wildflower_preference*n_bees
# 
# Preference = c(rep(2,n_blueberry_bees), rep(1,n_wildflower_bees))
# 
# blueberry_x_values = left_edges+1
# 
# wildflower_patches = sample(c(1,2),n_wildflower_bees,replace=TRUE)
# 
# 
# 
# blueberry_memory_x = sample(blueberry_x_values,n_blueberry_bees,replace=TRUE)
# blueberry_memory_y = sample(row_min_y:row_max_y,n_blueberry_bees)
# wildflower_memory_x = numeric(n_wildflower_bees)
# wildflower_memory_y = runif(n_wildflower_bees,wild_min_y,wild_max_y)
# 
# for(k in 1:n_wildflower_bees){
#   patch = wildflower_patches[k]
#   wildflower_memory_x[k] = runif(1,wild_min_x[patch],wild_max_x[patch])
# }
# 
# 
# 
# 
# memory_x = c(blueberry_memory_x,wildflower_memory_x)
# memory_y = c(blueberry_memory_y,wildflower_memory_y)
# 

#plot(memory_x,memory_y)