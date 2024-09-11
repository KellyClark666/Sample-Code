#This code creates a map with cases of X in Philadelphia, highlighting cases in zip code of concern
#The case data is pre-compiled was pre-compiled in SAS using handed down code and geocoded in arcGIS

#Packages for handling mapping data and data viz
library(sf)
library(tigris)
library(openxlsx)
library(tidyverse)
library(ggnewscale)
library(RColorBrewer)
library(ggpubr)

#Read in geocoded case data. Longitude/latitude was assigned to cases in ArcGIS (this can be done in R but takes a long time unless it's a batched job)
dat = readxl::read_excel("/path/to/Geocoded/Case/Data.XLS")

#Read in philly census tract shape files (from open data philly)
philly_sf_CT <- st_read("/path/to/Census_Tracts_2010-shp_unz/")

#Read in philly zip shape files (from data.gov)
philly_sf_ZIP <- st_read("/path/to/Zipcodes_Poly-shp/") %>% 
  #Create binary variable indicating zip of interest. This will be used to color zip on map
  mutate(zip_color = ifelse(CODE=="191XX",1,0))

#Create a list of zip code centroid coordinates to use for labeling
zip_labs <- st_centroid(philly_sf_ZIP)

Cases <- ggplot()+
  #plot zip shape file (each zip corresponds to a series of coordinates that make up a multipolygon)
  geom_sf(data=philly_sf_ZIP,
                 linewidth =.7,
                 #fill zipcode area based on binary variable indicating zip of interest
                 aes(fill= as.factor(zip_color)))+
  scale_fill_manual(values = c( "#00000000","goldenrod"))+
  
  #remove busy background
  coord_sf(datum=NA)+
  new_scale_color() +
  
  #plot the actual cases
  geom_point(data = dat %>%
               #filter on cases of interest
               filter(DX==1), 
             aes(x=as.numeric(GEOCODE_LON),
                 y=as.numeric(GEOCODE_LAT),
                 
                 #color cases based on year of diagnosis
                 color=as.factor(mmwr_year),
                 
                 #point shape based on year of diagnosis
                 shape = as.factor(mmwr_year)),
             size = 1,
             
             #spread overlapping points out a little
             position = "jitter",
             alpha= 0.7)+
  
  #Specify color and shape of points
  scale_color_manual(values = c("dodgerblue", "firebrick3"))+
  scale_shape_manual(values = c(16,17))+
  
  #Remove axis labels for cleaner look
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  
  #Clean up legend labels
  labs(color = "MMWR Year", shape = "MMWR Year")

#Save map 
ggsave(path = "/path/to/folder", filename = "Cases_23_24.tiff",plot = Cases,width = 8, height = 5, device='tiff', dpi=700)


