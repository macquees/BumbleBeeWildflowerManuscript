#creates a field to the specified dimensions
#vertical blueberry rows
#vertical wildflower row in middle of blueberry rows
#nest coordinates and max distance from nest should be set here
#last updated: May 14 2018


x_min = 0      #absolute min and max for x
x_max = 165
y_min = 0
y_max = 225

x_min_1 = 0    #blueberries to left of wildflowers
x_max_1 = 74   #blueberries to left of wildflowers
x_min_2 = 91   #blueberries to right of wildflowers
x_max_2 = 165  #blueberries to right of wildflowers

row_min_x = 0
row_max_x = 165
row_min_y = 75
row_max_y = 225

wild_min_x = 75
wild_max_x = 90
wild_min_y = 140
wild_max_y = 160


nx = 75          #coordinates of nest
ny = 10

n_bushes = 50*150    #the number of bushes in this field
W_b = owin(xrange = c(0,165), yrange = c(75,225))    #the window for counting number of bees on a bush
ybreaks_b=75:225
# W_a = owin(xrange = c(0,149), yrange = c(75,225))    #the window for counting number of bees on blueberries and wildflowers
# ybreaks_a=75:225

initial_nest_resource = c(300,200)  #initial amount of resources in the nest

delete = -seq(2,102,2)



left_edges = seq(row_min_x,row_max_x-3,3)     #all of the rows, including the wildflower row
right_edges = seq(row_min_x+2,row_max_x,3)

left_edges_b = c(seq(x_min_1,x_max_1-2,3),seq(x_min_2,x_max_2,3))     #only the blueberry rows
right_edges_b = left_edges_b+2


flowers = function(x,y){
  #returns 2 for blueberries (white), 1 for wild flowers (yellow), and 0 for no flowers (green)
  if(row_min_x<=x && x < x_max_1 && row_min_y <= y && y <= row_max_y){ #in the blueberry fields to left of wildflowers
    temp = 0
    i = 1
    while(temp == 0 && i <=25){
      if(left_edges_b[i]<=x && x<=right_edges_b[i]){
        temp = 2
      }
      i = i+1	
    }
  }else if(x_min_2<x && x <= row_max_x && row_min_y <= y && y <= row_max_y){ #in the blueberry fields to right of wildflowers
    temp = 0
    i = 26
    while(temp == 0 && i <=50){
      if(left_edges_b[i]<=x && x<=right_edges_b[i]){
        temp = 2
      }
      i = i+1	
    }
  }else if(wild_min_x<=x && x<=wild_max_x && wild_min_y<=y && y<=wild_max_y){ #in wildflowers
    temp = 1
  }else{ #no flowers
    temp = 0
  }
  return(temp)
}



#gradients
l_left_edges = seq(row_min_x,row_max_x-3,3)
l_right_edges = seq(row_min_x,row_max_x-3,3)+0.5
r_left_edges = seq(row_min_x+2,row_max_x,3)-0.5
r_right_edges = seq(row_min_x+2,row_max_x,3)

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


# #Uncomment this to plot the landscape
# ### Evaluate the flower field (only necessary for plotting)
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


# png("Landscape4.png",width = 1024, height = 768)
# image(x,y,field,col=c("grey85","yellow1","lightcyan"),asp=TRUE, axes=FALSE,main = "Wildflowers in the Centre",cex.main=1.5)
# points(nx,ny,pch=20)
# dev.off()
