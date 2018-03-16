# Author : Ewen Gallic http://editerna.free.fr/wp/
# Date : 2014-08-03

# Any comments are welcome.

library(grid)
library(ggplot2)
library(gridExtra)
library(gtable)


###############
## FUNCTIONS ##
###############

# http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
# Function to create circles
circle_fun <- function(center=c(0,0), diameter=1, npoints=500, start=0, end=2){
  tt <- seq(start*pi, end*pi, length.out=npoints)
  data.frame(
    x = center[1] + diameter / 2 * cos(tt),
    y = center[2] + diameter / 2 * sin(tt)
  )
}

# Gives y coordinates of the opposite side
rev_y <- function(y) 94-y

# Converts inches to feet
inches_to_feet <- function(x) x/12

# Given the angle theta and the court data frame,
# rotates the coordinates of the court by an angle theta
rotate_court <- function(court, theta=pi/2){
  court_r <- court
  court_r$x <- court_r$x / 180 * pi
  court_r$y <- court_r$y / 180 * pi
  matrice_r <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2)
  coords_r <- apply(court_r[,c("x","y")], 1, function(x) x %*% matrice_r)
  court_r$x <- coords_r[1,] ; court_r$y <- coords_r[2,]
  court_r$x <- court_r$x * 180 / pi
  court_r$y <- court_r$y * 180 / pi
  return(court_r)
}

# From x and y coordinates for a line (represented by a polygon here),
# a number of group and a short description
# creates a data.frame for this line
# in order to use it with ggplot2.
new_coords <- function(x, y, group, descri){
  new_coords_df <- data.frame(x = x, y = y)
  new_coords_df$group <- group
  new_coords_df$side <- 1
  group <- group + 1
  
  # The same thing for the opposite side
  new_coords_df2 <- data.frame(x = x, y = rev_y(y))
  new_coords_df2$group <- group
  new_coords_df2$side <- 2
  group <<- group + 1
  
  # On reunit les donnees
  new_coords_df <- rbind(new_coords_df, new_coords_df2)
  new_coords_df$descri <- descri
  
  return(new_coords_df)
}

###############
## THE COURT ##
###############

# 3 pts circle
cercle_3pts.out <- circle_fun(center = c(25,inches_to_feet(63)), diameter = (20+inches_to_feet(9))*2)
cercle_3pts.in <- circle_fun(center = c(25,inches_to_feet(63)), diameter = (20+inches_to_feet(7))*2)
# Basket circle
cercle_ce <- circle_fun(center = c(25,5+3/12), diameter = 1.5)
# Free throw circle
cercle_lf.out <- circle_fun(center = c(25,19), diameter = 6*2)
cercle_lf.in <- circle_fun(center = c(25,19), diameter = (6-1/6)*2)
# Middle circle
cercle_mil.out <- circle_fun(center = c(25,47), diameter = 6*2)
cercle_mil.in <- circle_fun(center = c(25,47), diameter = (6-1/6)*2)


group <- 1 # We assign the first group, and it gets incremented with each use of new_coords()
court <- new_coords(c(0-1/6,0-1/6,53 + 1/6,53 + 1/6), c(0 - 1/6,0,0,0 - 1/6), group = group, descri = "ligne de fond")
court <- rbind(court, new_coords(x = c(0-1/6,0-1/6,0,0), y = c(0,47-1/12,47-1/12,0), group = group, descri = "ligne gauche"))
court <- rbind(court, new_coords(x = c(50,50,50+1/6,50+1/6), y = c(0,47-1/12,47-1/12,0), group = group, descri = "ligne droite"))
court <- rbind(court, new_coords(x = c(47,47,53,53), y = c(28,28+1/6,28+1/6,28), group = group, descri = "marque entraineur droite"))
court <- rbind(court, new_coords(x = c(inches_to_feet(51),inches_to_feet(51),inches_to_feet(51)+1/6,inches_to_feet(51)+1/6), y = c(0,inches_to_feet(63),inches_to_feet(63),0), group = group, descri = "3pts bas gauche"))
court <- rbind(court, new_coords(x = c(50-inches_to_feet(51)-1/6,50-inches_to_feet(51)-1/6,50-inches_to_feet(51),50-inches_to_feet(51)), y = c(0,inches_to_feet(63),inches_to_feet(63),0), group = group, descri = "3pts bas droit"))
court <- rbind(court, new_coords(x = c(19,19,19+1/6,19+1/6), y = c(0,19,19,0), group = group, descri = "LF bas gauche"))
court <- rbind(court, new_coords(x = c(31-1/6,31-1/6,31,31), y = c(0,19,19,0), group = group, descri = "LF bas droit"))
court <- rbind(court, new_coords(x = c(19,19,31,31), y = c(19-1/6,19,19,19-1/6), group = group, descri = "LF tireur"))
court <- rbind(court, new_coords(x = c(22, 22, 28, 28), y = c(4-1/6,4,4,4-1/6), group = group, descri = "planche"))
court <- rbind(court, new_coords(x = c(cercle_3pts.out[1:250,"x"], rev(cercle_3pts.in[1:250,"x"])),
                                y = c(cercle_3pts.out[1:250,"y"], rev(cercle_3pts.in[1:250,"y"])), group = group, descri = "cercle 3pts"))
court <- rbind(court, new_coords(x = c(cercle_lf.out[1:250,"x"], rev(cercle_lf.in[1:250,"x"])),
                                y = c(cercle_lf.out[1:250,"y"], rev(cercle_lf.in[1:250,"y"])), group = group, descri = "cercle LF haut"))
court <- rbind(court, new_coords(x = c(19-0.5,19-0.5,19,19), y = c(7,8,8,7), group = group, descri = "marque 1 LF gauche"))
court <- rbind(court, new_coords(x = c(19-0.5,19-0.5,19,19), y = c(11,11+inches_to_feet(2),11+inches_to_feet(2),11), group = group, descri = "marque 2 LF gauche"))
court <- rbind(court, new_coords(x = c(19-0.5,19-0.5,19,19), y = c(14+inches_to_feet(2),14+inches_to_feet(4),14+inches_to_feet(2),14+inches_to_feet(2)), group = group, descri = "marque 3 LF gauche"))
court <- rbind(court, new_coords(x = c(19-0.5,19-0.5,19,19), y = c(17+inches_to_feet(4),17+inches_to_feet(6),17+inches_to_feet(6),17+inches_to_feet(4)), group = group, descri = "marque 4 LF gauche"))
court <- rbind(court, new_coords(x = c(31,31,31+0.5,31+0.5), y = c(7,8,8,7), group = group, descri = "marque 1 LF droite"))
court <- rbind(court, new_coords(x = c(31,31,31+0.5,31+0.5), y = c(11,11+inches_to_feet(2),11+inches_to_feet(2),11), group = group, descri = "marque 2 LF droite"))
court <- rbind(court, new_coords(x = c(31,31,31+0.5,31+0.5), y = c(14+inches_to_feet(2),14+inches_to_feet(4),14+inches_to_feet(4),14+inches_to_feet(2)), group = group, descri = "marque 3 LF droite"))
court <- rbind(court, new_coords(x = c(0-1/6,0-1/6,50+1/6,50+1/6), y = c(94/2-1/12,94/2, 94/2, 94/2-1/12), group = group, descri = "ligne mediane"))
court <- rbind(court, new_coords(x = c(31,31,31+0.5,31+0.5), y = c(17+inches_to_feet(4),17+inches_to_feet(6),17+inches_to_feet(6),17+inches_to_feet(4)), group = group, descri = "marque 4 LF droite"))
court <- rbind(court, new_coords(x = c(cercle_mil.out[250:500,"x"], rev(cercle_mil.in[250:500,"x"])),
                                y = c(cercle_mil.out[250:500,"y"], rev(cercle_mil.in[250:500,"y"])), group = group, descri = "cercle milieu grand"))
court <- rbind(court, new_coords(x = cercle_ce[,"x"], y = cercle_ce[,"y"], group = group, descri = "anneau"))

# The data.frame containing all coordinates for the court is now complete.
head(court)
tail(court)

###########
## PLOTS ##
###########

# Whole court 
P <- ggplot() + geom_polygon(data = court, aes(x = x, y = y, group = group), col = "gray") +
  coord_equal() +
  ylim(-2,96) +
  xlim(-5,55) +
  scale_x_continuous(breaks = c(0, 12.5, 25, 37.5, 50)) +
  scale_y_continuous(breaks = c(0, 23.5, 47, 70.5, 94)) +
  xlab("") + ylab("") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), axis.title = element_blank()
  )
  



# Whole court with rotation
P_180 <- ggplot() + geom_polygon(data = rotate_court(court, theta = pi/2), aes(x = x, y = y, group = group), col = "black") +
  coord_equal() +
  xlim(-2,96) +
  ylim(-55,2) +
  scale_x_continuous(breaks = c(0, 23.5, 47, 70.5, 94)) +
  scale_y_continuous(breaks = c(0, -12.5, -25, -37.5, -50))

# Half court
P_half <- ggplot() + geom_polygon(data = court[court$side==1,], aes(x = x, y = y, group = group), col = "black") +
  coord_equal() +
  xlim(-3,54) +
  ylim(-3,50) +
  scale_y_continuous(breaks = c(0, 23.5, 47)) +
  scale_x_continuous(breaks = c(0, 12.5, 25, 37.5, 50)) +
  xlab("") + ylab("") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), axis.title = element_blank()
  )