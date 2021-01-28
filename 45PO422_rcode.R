library("factoextra")
library("ggplot2")
library("dplyr")
library("tidyr")
library("fpc")
library("ggpubr")
library("RColorBrewer")

#Create color palette for visualizing paleobotanical data
color <- get_palette(palette = "RdYlBu", 16) 

#seed density visualization
#data prep
seeds <- read.csv(file.choose(), fileEncoding="UTF-8-BOM")#read in seeds.csv
row.names(seeds) <- seeds$taxa
seeds[1] <- NULL
#translate seed density data to percentages
seed_percentage <- apply(seeds,  2, function(x){x*100/sum(x,na.rm=T)})
#Fig 4.2a barchart for seeds
plot.new()
barplot(seed_percentage, col = color)
#Visualize legend; legends are added in an image processing program since legend is large
plot.new()
legend("center", legend = c("Amaranth", "Bearberry", "Goosefoot",
                            "Hawthorn", "Sedge", "Spikerush", "Wild Buckwheet", 
                            "Frogbit", "Sandwort", "Ponderosa pine", "Knotweed", 
                            "Chokecherry", "Buttercup", "Dock", "Huckleberry", 
                            "Vetch"),
       col = c("#A50026", "#C62026", "#E04430", "#F46D43", "#FA9856", "#FDBE70",
               "#FEE090", "#FEF4AF", "#F4FBD2", "#E0F3F7", "#BCE1EE", "#98CAE1",
               "#74ADD1", "#5487BD", "#3E60A9", "#313695"),
       pch=20 , pt.cex = 3, cex = 1)

#Vegetation data visualization
#data prep
veg <- read.csv(file.choose(), fileEncoding="UTF-8-BOM") #read in veg.csv
row.names(veg) <- veg$taxa
veg[1] <- NULL
#translate veg density data to percentages
veg_percentage <- apply(veg,  2, function(x){x*100/sum(x,na.rm=T)})
#Fig 4.2b barchart for vegetation
plot.new()
barplot(veg_percentage, col = color)
#Visualize legend; legends are added in an image processing program since legend is large
plot.new()
legend("center", legend = c("Nodding onion", "Camas", "Pine needle fasicle", "Whitebark pine shoots",
                            "Whole lodgepole pine cones", "Fragmented lodgepole pine cones", "Lodgepole pine needles",
                            "Ponderosa pine needles", "Undet bulb tissue", "Undet fruit tissue"),
       col = c("#A50026", "#C62026", "#E04430", "#F46D43", "#FA9856", "#FDBE70",
               "#FEE090", "#FEF4AF", "#F4FBD2", "#E0F3F7", "#BCE1EE", "#98CAE1",
               "#74ADD1", "#5487BD", "#3E60A9", "#313695"),
       pch=20 , pt.cex = 3, cex = 1)

#Wood charcoal visualization
wood <- read.csv(file.choose(), fileEncoding="UTF-8-BOM") #read in wood.csv
row.names(wood) <- wood$taxa
wood[1] <- NULL
#translate wood char density data to percentages
wood_percentage <- apply(wood,  2, function(x){x*100/sum(x,na.rm=T)}) 
#Fig 4.2c wood charcoal bar graphs
plot.new()
barplot(wood_percentage, col=color)
#Visualize legend; legends are added in an image processing program since legend is large
plot.new()
legend("right", legend = c("Softwood", "Pine family", "Pine genera", "Lodgepole pine", 
                         "Ponderosa pine", "Doug fir", "Hemlock", "Fir/Hemlock",
                         "Spruce/Larch", "Hardwood", "Willow/Poplar", "Willow", "Undet Angiosperm"),
                         col = c("#A50026", "#C62026", "#E04430", "#F46D43", "#FA9856", "#FDBE70",
                                 "#FEE090", "#FEF4AF", "#F4FBD2", "#E0F3F7", "#BCE1EE", "#98CAE1",
                                 "#74ADD1", "#5487BD", "#3E60A9", "#313695"),
       pch=20 , pt.cex = 3, cex = 1 )

#K-means Analyses

#wood charcoal k-means analysis
wood_df <- scale(wood) #standardize density data
fviz_nbclust(wood_df, kmeans, method = "silhouette", k.max = 6) #check for clusters
wood.km <- eclust(wood_df, "kmeans", k=2, nstart = 25) #k-means analysis
fviz_cluster(wood.km, ellipse.type = "norm") #visualize k-means
#Remove/transpose data or re-run without outliers or run by features

#Seed data k-means
seeds_df <- scale(seeds) #standardize density data
fviz_nbclust(seeds_df, kmeans, method = "silhouette", k.max = 6) #find number of clusters
seeds.km <- eclust(seeds_df, "kmeans", k=2, nstart = 25) #k-means analysis
fviz_cluster(seeds.km, frame.type = "norm") #visualize kmeans
#Remove/transpose data or re-run without outliers or run by features

#Vegetation k-means analysis
veg_df <- scale(veg) #scale data
fviz_nbclust(veg_df, kmeans, method = "silhouette", k.max=6) #check number of clusters
veg.km <- eclust(veg_df, "kmeans", k=3, nstart = 25) #k-means analysis
fviz_cluster(veg.km, frame.type = "norm") #visualizing k-means
#Remove/transpose data or re-run without outliers or run by features

#Fischer's exact test for willow wood and geophytes
willow_geo <- c(3, 0, 1, 6) #create vector and matrix for presence/absence of willow & geophytes
dim(willow_geo) <- c(2, 2)
fisher.test(willow_geo)
