#creates a field to the specified dimensions
#vertical bluebery rows
#no wildflowers
#nest coordinates and max distance from nest should be set here
#last updated: May 14 2018



x_min = 0
y_min = 0
x_max = 150
y_max = 225
row_min_x = 0
row_max_x = 150
row_min_y = 75
row_max_y = 225
wild_min_x = 0
wild_max_x = 150
wild_min_y = 67
wild_max_y = 70


n_bushes = 50*150    #the number of bushes in this field
n_bees=20      #scaled by 1/2 according to honeybee results, and 40% due to lack of wildflowers = 20
W_b = owin(xrange = c(0,149), yrange = c(75,225))    #the window for counting number of bees on a bush
ybreaks_b=75:225

delete = -seq(2,100,2)    #rows between bushes to delete for entropy calculations

initial_nest_resource = c(300,200)  #initial amount of resources in the nest



left_edges = seq(row_min_x,row_max_x-3,3)
right_edges = seq(row_min_x+2,row_max_x,3)

left_edges_b = left_edges
right_edges_b = right_edges


flowers = function(x,y){
	#returns 2 for blueberries (white), 1 for wild flowers (yellow), and 0 for no flowers (green)
	temp = 0
	i = 1
	if(row_min_y <= y && y <= row_max_y && row_min_x <= x && x <= row_max_x ){ #in the blueberry fields
		while(temp == 0 && i <=length(left_edges)){
			if(left_edges[i]<=x && x<=right_edges[i]){
				temp = 2
			}
			i = i+1	
		}
	}else{ #no flowers
		temp = 0
	}
	return(temp)
}



#gradients
l_left_edges = left_edges
l_right_edges = left_edges+0.5
r_left_edges = right_edges-0.5
r_right_edges = right_edges

flowers_gradient = function(x,y){
  #returns 2 for blueberries (white), 1 for wild flowers (yellow), and 0 for no flowers (green)
  tempx = 0
  tempy = 0
  fg = 1
  
  if(row_min_y<=y && y <= row_min_y+0.5){ #bottom of the rows
    tempy = 1
  }else if(row_max_y-0.5<=y && y <=row_max_y){ #top of the rows
    tempy = -1
  }else if(row_min_y+0.5 <= y && y <= row_max_y-0.5){ #rest of the rows
    while(tempx == 0 && tempy == 0 && fg <= length(left_edges)){
      if(l_left_edges[fg]<=x && x<=l_right_edges[fg]){ #left sides of rows
        tempx = 1
      }else if(r_left_edges[fg]<=x && x<=r_right_edges[fg]){ #right sides of rows
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

# png("Landscape1.png",width = 1024, height = 768)
# image(x,y,field,col=c("grey85","yellow1","lightcyan"),asp=TRUE, axes=FALSE,main = "No Wildflowers",cex.main=1.5)
# points(nx,ny,pch=20)
# dev.off()


