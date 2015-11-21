
setwd("~/Spatial Segmentation/Actual Data")

delhi <- read.delim("clipboard")
View(delhi)


## Put that first number in a new column
delhi$Type <- as.character(delhi$Type)
delhi$rooms <- substring(delhi$Type,1,2)
delhi$rooms <- as.numeric(delhi$rooms)

for (i in 1:nrow(delhi)) {
  if (delhi$rooms == 10) {
    delhi$property <- substring(delhi$Type,9,30)
  }
  else {
    delhi$property <- substring(delhi$Type,7,29)
  }
}

write.csv(delhi,"whoaboy.csv")

## Fix some things in Excel
### 10 room places still have a space
### fill property column for 'NA' room places

delhi <- read.csv("delhi_rent_fixed.csv")

hedonic <- lm(hp ~ CBD + property + rooms, data = delhi)

summary(hedonic)

### Map to find CBDs

myMap <- get_map(location = "Delhi, India", source = "stamen", maptype="watercolor", crop = FALSE, zoom = 11)

ggmap(myMap) + geom_point(aes(x=Longitude,y=Latitude),data=delhi,
                          alpha=0.05,color="darkblue")

