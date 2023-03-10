
setwd('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Analyse Quantitative des données BIO6077/Projet final/data/bird')

#================
# load R packages
#================

library(sf)
library(sp)
library(reshape2)
library(rgdal)
library(mapview)
library(dplyr)
library(gridExtra)
library(maptools)
library(cleangeo)
library(geosphere)
library(vegan)
library(VennDiagram)
library(ggtern)
library(tiff)
library(raster)
library(terra)
#library(corrplot)

#================
# bird data
#================

pass <- readRDS("pass.RDS") #raw data

pass[which(pass$taxa_valid_scientific_name=='Columba livia'),] ## check if there is pigeons 

#construct space ID
spID <- as.numeric(as.factor(as.character(pass$geom$geometry)))
pass$spID <- spID #add new column of constructed id to pass data frame 

# coordinates and spID
coordSpIDAll <- st_sf(data.frame(geom = pass$geom, spID = pass$spID)) 
coordSpIDSel <- coordSpIDAll[!duplicated(coordSpIDAll$spID),]
coordSpID <- coordSpIDSel[order(coordSpIDSel$spID),]

# Select nesting
passApril <- pass[which(pass$month_obs == 4),]
passMay <- pass[which(pass$month_obs == 5),]
passJune <- pass[which(pass$month_obs == 6 & pass$day_obs <= 15),]

passNest <- rbind(passApril, passMay, passJune)

# Select roaming
passJuly <- pass[which(pass$month_obs == 7 & pass$day_obs > 15),]
passAug <- pass[which(pass$month_obs == 8),]
passSept <- pass[which(pass$month_obs == 9),]

passRoam <- rbind(passJuly, passAug, passSept)

#=================================
# Build community matrix (Nesting)
#=================================
# Build basic community matrix
commNestRaw <- dcast(passNest, spID ~ taxa_valid_scientific_name,
                     value.var = "value", fun.aggregate = sum)

# Convert counts to 0 - 1
commNestPA <- ifelse(commNestRaw[,-1] > 0, 1, 0)

# transform into sf object
coordNest <- coordSpID$geom[which(coordSpID$spID %in% commNestRaw$spID)]
commNest <- st_sf(data.frame(geom = coordNest, commNestPA)) 

# Check
apply(st_drop_geometry(commNest),2,unique) # Good

#=================================
# Build community matrix (Roaming)
#=================================
# Build basic community matrix
commRoamRaw <- dcast(passRoam, spID ~ taxa_valid_scientific_name,
                     value.var = "value", fun.aggregate = sum)

# Convert counts to 0 - 1
commRoamPA <- ifelse(commRoamRaw[,-1] > 0, 1, 0)

# transform into sf object
coordRoam <- coordSpID$geom[which(coordSpID$spID %in% commRoamRaw$spID)]
commRoam <- st_sf(data.frame(geom = coordRoam, commRoamPA)) 

# Check
apply(st_drop_geometry(commRoam),2,unique) # Good

# Make sf object with the community matrix
commNestData <- st_drop_geometry(commNest) #use for the analysis 
commRoamData <- st_drop_geometry(commRoam) #use for the analysis


#=========================
#Manipulations on all data
#=========================

#match observations for each period 
comm_nest <- commNest[,which(colnames(commNest) %in% colnames(commRoam))]
comm_roam <- commRoam[,which(colnames(commRoam) %in% colnames(commNest))]

#=======================
#extract of green areas
#=======================

#see under green area data

#================
#green area data
#================

g1<-st_read('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Analyse Quantitative des données BIO6077/Projet final/data/Environmental variables/green spaces/raw/66007-US-2020')
g2<-st_read('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Analyse Quantitative des données BIO6077/Projet final/data/Environmental variables/green spaces/raw/66023-US-2020')
g3<-st_read('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Analyse Quantitative des données BIO6077/Projet final/data/Environmental variables/green spaces/raw/66032-US-2020')
g4<-st_read('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Analyse Quantitative des données BIO6077/Projet final/data/Environmental variables/green spaces/raw/66047-US-2020')
g5<-st_read('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Analyse Quantitative des données BIO6077/Projet final/data/Environmental variables/green spaces/raw/66058-US-2020')
g6<-st_read('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Analyse Quantitative des données BIO6077/Projet final/data/Environmental variables/green spaces/raw/66062-US-2020')
g7<-st_read('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Analyse Quantitative des données BIO6077/Projet final/data/Environmental variables/green spaces/raw/66072-US-2020')
g8<-st_read('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Analyse Quantitative des données BIO6077/Projet final/data/Environmental variables/green spaces/raw/66087-US-2020')
g9<-st_read('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Analyse Quantitative des données BIO6077/Projet final/data/Environmental variables/green spaces/raw/66092-US-2020')
g10<-st_read('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Analyse Quantitative des données BIO6077/Projet final/data/Environmental variables/green spaces/raw/66097-US-2020')
g11<-st_read('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Analyse Quantitative des données BIO6077/Projet final/data/Environmental variables/green spaces/raw/66102-US-2020')
g12<-st_read('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Analyse Quantitative des données BIO6077/Projet final/data/Environmental variables/green spaces/raw/66107-US-2020')
g13<-st_read('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Analyse Quantitative des données BIO6077/Projet final/data/Environmental variables/green spaces/raw/66112-US-2020')
g14<-st_read('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Analyse Quantitative des données BIO6077/Projet final/data/Environmental variables/green spaces/raw/66117-US-2020')
g15<-st_read('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Analyse Quantitative des données BIO6077/Projet final/data/Environmental variables/green spaces/raw/66127-US-2020')
g16<-st_read('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Analyse Quantitative des données BIO6077/Projet final/data/Environmental variables/green spaces/raw/66142-US-2020')


#rename because it's not the right name
colnames(g2)<-c('CODEGEOG','ZONE_CPTAQ','ETAGE_MIN','ETAGE_MAX','ETAGE_MOYE','ETAGE_MEDI',
                'ETAGE_TOTA','AIRE_MIN', 'AIRE_MAX','AIRE_MOYEN','AIRE_MEDIA','AIRE_TOTAL',
                'LOG_MIN','LOG_MAX','LOG_MOYEN','LOG_MEDIAN','LOG_TOTAL','ANNEE_MIN',
                'ANNEE_MAX','ANNEE_MOYE','ANNEE_MEDI','ANNEE_TOTA','TERRAIN_MI', 'TERRAIN_MA',
                'TERRAIN_MO','TERRAIN_ME','TERRAIN_TO','TERRM2_MIN','TERRM2_MAX','TERRM2_MOY','TERRM2_MED',
                'BAT_MIN','BAT_MAX','BAT_MOYEN','BAT_MEDIAN','BAT_TOTAL','ID','UTIL_SOL','geometry')

#REORDERING TO MATCH OTHER DATASETS
col_order<-match(colnames(g1),colnames(g2))
g2<-g2[,col_order]
 
green_all<-rbind(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16)

sum(is.na(green_all$ID)) #are there NA's?

g_greens<-green_all[green_all$UTIL_SOL==600,] 
g_golf<-green_all[green_all$UTIL_SOL==1100,]

greens<-rbind(g_greens,g_golf)


#open data from donn/es ouvertes montreal
setwd('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Analyse Quantitative des données BIO6077/Projet final/data/Environmental variables/green spaces')
green_mtl<-readRDS(file='greens')


#extract polygons that are not 
g_obs<-greens[,c(1,39)]
g_mtl<-green_mtl[,c(1,12)]
names(g_mtl)[names(g_mtl)=='OBJECTID']<-'ID' #change the name so it matches the other df

g_mtl<-st_transform(g_mtl,4326)
g_obs<-st_transform(g_obs,4326)

greens_intersection<-st_intersection(g_mtl,g_obs)
green_unique<-g_obs[!(g_obs$ID %in% greens_intersection$ID.1),] #garder seulement un des deux ID

green_unique<-st_transform(green_unique,4326)
greens_combine<-rbind(green_unique,g_mtl)

##merge adjacent polygons
greens_union<-st_union(greens_combine)
greens_cast<-st_cast(greens_union,"POLYGON")
#mapview(plok) ##it worked
green_areas<-st_as_sf(greens_cast)

#we have to calculate area again
green_areas$area<-st_area(green_areas)
class(green_areas$area) #units so I must change to numeric

green_areas$area<-as.numeric(green_areas$area)
green_areas<-green_areas[green_areas$area>5000,]

#saveRDS(green_areas, file = "green_areas.rds")

#=============
#Extract bird 
#=============

#check projection
st_crs(comm_nest)
st_crs(comm_roam)
st_crs(green_areas) #same projection 


#extract for nesting
bird_nest_green <- st_intersection(comm_nest,green_areas)

#saveRDS(bird_nest_green,file='bird_nest_green.RDS')
#mapview(bird_nest_green)+green_areas #it works

#extract for roaming
bird_roam_green<-st_intersection(comm_roam, green_areas)

#saveRDS(bird_roam_green,file='bird_roam_green.RDS')

#=================
#tree canopy data
#=================

tree_cover<-rast('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Analyse Quantitative des données BIO6077/Projet final/data/Environmental variables/tree cover/660_IndiceCanopee_2021.tif')

#tree cover lower than 3m 
low_tree_cover<-tree_cover
low_tree_masked<-ifel(low_tree_cover!=3,NA,low_tree_cover) #turn every value that is not 3 into NA
plot(low_tree_masked,col='yellow')

#tree cover higher than 3 m 
high_tree_cover<-tree_cover
high_tree_masked<-ifel(high_tree_cover!=4,NA,high_tree_cover)#turn every value that is not 4 into NA
plot(high_tree_masked,col='green') 

#match raster extent to polygon raster
r_extent<-ext(tree_cover)

green_areas_crs<-st_transform(green_areas,crs(tree_cover)) #transform polygon to match projection of raster

green_bbox<-st_bbox(green_areas_crs) #get coordinates of the bounding box of green areas

green_bbox_sfc<-st_as_sfc(green_bbox) #transform bbox object as sfc

bbox_crs<-st_transform(green_bbox_sfc,crs(tree_cover)) #make it the same crs as raster layer

green_res<-bbox_crs/dim(tree_cover) #get resolution of polygon in the same unit as raster

ext(tree_cover)<-bbox_crs


low_cover=data.frame() #create empty dataframe 

for(i in 1:nrow(green_areas)){
  poly<-green_areas[i,] #subset for each green area
  crp<-crop(low_tree_masked,poly)
  msk<-mask(crp,poly)#mask values outside polygons
  tbl<-data.frame(freq(msk)) #count number of pixels in each polygon
  tbl<-tbl[!is.na(tbl$value),]
  low_cover<-rbind(low_cover,tbl)
}
