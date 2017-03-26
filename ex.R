source("smooth_demo.R")

attach(cars)

smooth_demo(x = cars$speed, y = cars$dist)
