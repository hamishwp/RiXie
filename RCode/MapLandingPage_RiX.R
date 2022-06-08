# Call all the required packages for RiXie from the GetPackages.R file
callLibs()
# Read in the list of countries to plot on the map
countries<-read.csv(file = "./RiX_Countries_Red.csv", head = TRUE, sep=",")
names(countries)[c(1,ncol(countries))]<-c("iso3","Now")
# Read in the country shapefiles (Official UNGIS maps)
ADM<-as(sf::st_read("./UNmap0_shp/BNDA_CTY.shp"),"Spatial")
# Create a unique ID for each country, to be matched to the list of RiX countries
ADM@data$id <- rownames(ADM@data)
# Convert SF-Spatial data into a model wrt the ID variable
redmapdata <- fortify(ADM, region = "id")
# Create a data.frame ready for plotting
redmapdf <- merge(redmapdata, ADM@data,
                  by = "id")
# Create the status variable of which countries are currently and are forseen to be in RiX 
redmapdf$status<-0
redmapdf$status[redmapdf$ISO3CD%in%countries$iso3]<-1
redmapdf$status[redmapdf$ISO3CD%in%countries$iso3[countries$Now==1]]<-2
redmapdf$status%<>%as.factor()
# Spring cleaning!
redmapdf%<>%dplyr::select(c(id,long,lat,group,status,ISO3CD))
# Plot the map!
p<-redmapdf%>%ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(mapping=aes(x = long, y = lat, group = group,fill = status))+
  geom_path(data=redmapdf,mapping=aes(x = long, y = lat, group = group),color = "#BABABA", size = 0.2) +
  xlab("Longitude")+ylab("Latitude")+labs(fill="RiX - Progression")+coord_equal()+xlim(c(-180,180))+ylim(c(-90,90))+
  scale_fill_manual(breaks = c(2,1,0),
                    labels=c("Available","Coming Soon","Not Available"),
                    values=c("#004F91","#9abbd6","#E3E3E3"))+
  theme(
    # Hide panel borders and remove grid lines
    panel.background = element_rect(fill = "#DDE8F0", colour = "black",
                                    size = 1, linetype = "solid"),
    # axis.title=element_text(size=16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.2, 0.1),
    legend.background = element_rect(fill="lightblue",
                                     size=0.5, linetype="solid", 
                                     colour ="darkblue"),
    plot.margin=grid::unit(c(0,0,0,0), "mm"),
    # Change axis line
    axis.line = element_line(colour = "black")
  );p
ggsave("./RiX_Map.eps",plot = p,device = "eps",scale = 2)
ggsave("./RiX_Map.png",plot = p,device = "png",scale = 2)

