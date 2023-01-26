#AKANSHA -
#STUDENT ID : A124058

#Project-2

install.packages("ggplot2")
library(ggplot2)

readMap <- function(path) {
  con = file(path, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    print(line)
  }
  
  close(con)
}

findWay <- function(maze_data) {
  # find the starting point
  start_x <- 0
  start_y <- 0
  for(i in 1:length(maze_data)){
    for(j in 1:length(maze_data[[i]])){
      if(maze_data[[i]][j] == "s"){
        start_x <- i
        start_y <- j
        break
      }
    }
  }
  # use a search algorithm to find a path from the starting point to the exit point
  # store the path in a data structure
  path_data <- list()
  # you can use any search algorithm like BFS, DFS, A*
  # return the path data structure
  return(path_data)
}

plotMap <- function(maze_data) {
  # create a plot of the maze using the maze data
  
  plot_data <- data.frame(x = 1:length(maze_data[[1]]), y = 1:length(maze_data), type = as.vector(maze_data))
  ggplot(plot_data, aes(x, y, fill = type)) +
    geom_tile()
}

plotPath <- function(path_data) {
  # create a plot of the path using the path data
  
  path_data$type <- "path"
  ggplot(path_data, aes(x, y, fill = type)) +
    geom_tile()
}

m <- readMap("C:/Users/akans/Advanced Analytics- Big data Course/Basic R Programming/maze_0.map")

m <- readMap("C:/Users/akans/Advanced Analytics- Big data Course/Basic R Programming/maze_1.map")


a <- findWay(m)
plotMap(m)
plotPath(a)



