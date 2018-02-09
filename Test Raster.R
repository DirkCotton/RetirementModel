library(rgdal)
library(png)

# load icons in PNG format
iconfile1 <- download.file('http://icons.iconarchive.com/icons/oxygen-icons.org/oxygen/256/Status-weather-clouds-icon.png', destfile = 'icon1.png', mode = 'wb')
icon1 <- readPNG('icon1.png')

iconfile2 <- download.file('http://icons.iconarchive.com/icons/oxygen-icons.org/oxygen/256/Status-weather-showers-scattered-icon.png', destfile = 'icon2.png', mode = 'wb')
icon2 <- readPNG('icon2.png')

# load shapefile
# al <- readOGR(dsn=path.expand("DZA"), "DZA_adm0")

# make up some points
library(sp) 
set.seed(613) 
# dat <- spsample (al, 3, type='random')
dat <- matrix(c(-5,-5,0,0,5,5),3,2)
# need to offset the x/y location for the icon (depends on desired icon size)
offset <- 2

# plot(al)
plot(0,0)
rasterImage(icon1, coordinates(dat)[1,1]-offset, coordinates(dat)[1,2]-offset, coordinates(dat)[1,1]+offset, coordinates(dat)[1,2]+offset)
rasterImage(icon2, coordinates(dat)[2,1]-offset, coordinates(dat)[2,2]-offset, coordinates(dat)[2,1]+offset, coordinates(dat)[2,2]+offset)
rasterImage(icon1, coordinates(dat)[3,1]-offset, coordinates(dat)[3,2]-offset, coordinates(dat)[3,1]+offset, coordinates(dat)[3,2]+offset)
points(dat)