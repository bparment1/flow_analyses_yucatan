#################################    FLOW RESEARCH  #######################################
########################### SPACE-TIME QUINTINARO PRODUCT DATABASE  #############################################
#This script generates figures from the Quintinaro database of flow (food, wood etc) analyzed previoulsy
#Data was collected by Marco Millones.
#
#AUTHOR: Benoit Parmentier                                                                       
#DATE CREATED:08/30/2016 
#DATE MODIFIED: 08/30/2016
#
#PROJECT: Flow, land cover change with Marco Millones
#COMMIT: reading land data and comparing available land to land consumed
#
##################################################################################################
#
###Loading r library and packages

library(raster)                            # loading the raster package
library(gtools)                            # loading ...
library(sp)                                # spatial objects in R
library(gplots)                            # 
library(rgdal)                             # gdal driver for R
library(RColorBrewer)                      # color scheme, palettes used for plotting
library(gdata)                             # read different format (including .xlsx)
library(plotrix)                           # plot options and functions including plotCI
library(rasterVis)                         # raster visualization
library(gridExtra)                         # graphic package
library(latticeExtra)                      # graphic package
library(colorRamps)                        # contains matlab.like palette
library(lsr)                               #
library(psych)                             # PCA
library(GPArotation)                       # PCA rotation
library(zoo)                               # Time series object and functions
library(xts)                               # Time series object and functions
library(remote)                            # EOT implementation in R/cpp
library(XML)                               # HTML funcitons
library(plyr)

#################################################
###### Functions  used in the script  ##########

create_dir_fun <- function(outDir,out_suffix){
  #if out_suffix is not null then append out_suffix string
  if(!is.null(out_suffix)){
    out_name <- paste("output_",out_suffix,sep="")
    outDir <- file.path(outDir,out_name)
  }
  #create if does not exists
  if(!file.exists(outDir)){
    dir.create(outDir)
  }
  return(outDir)
}

load_obj <- function(f){
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

#infile1_function <- file.path("/home/bparmentier/Google Drive/Papers_writing_MEOT/R_scripts/",
#                              "PCA_EOT_comparison_data_update_function_07152016.R")
#source(infile1_function)

#############################################
######## Parameters and arguments  ########

#Input file name with the flow data
#filename_flow <- "MO1-9_ALL.txt"

inDir <- "/home/bparmentier/Google Drive/000_Flow_and_LUD_research/Quintana_Roo_Research/Data/output_flow_08302016/"
#filename_flow_cleaned <- "/home/bparmentier/Google Drive/000_Flow_and_LUD_research/Quintana_Roo_Research/Data/output_flow_08302016/tb_overall_flow_data_clean_flow_08302016.txt"

filename_flow_cleaned <- "tb_overall_flow_data_clean_flow_08302016.txt"

flow_by_product_filename <- "test_aggregated_data_by_flow_by_product_year_flow_08302016.txt"
flow_by_product_consumption_filename <- "test3_aggregated_data_by_comsumption_by_product_year_flow_08302016.txt"
flow_by_product_extraction_filename <- "test2_aggregated_data_by_exraction_by_product_year_flow_08302016.txt"

#tb_overall_flow_data_clean_flow_08302016.txt

CRS_WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0" #Station coords WGS84 # CONST 2
proj_str<- CRS_WGS84 #param 2
CRS_reg <- CRS_WGS84 # PARAM 3

file_format <- ".txt" #PARAM 4
NA_value <- -9999 #PARAM5
NA_flag_val <- NA_value #PARAM6
out_suffix <-"flow_08302016" #output suffix for the files and ouptu folder #PARAM 7
create_out_dir_param=TRUE #PARAM8
num_cores <- 4 #PARAM 9

#inDir <- "/home/bparmentier/Google Drive/000_Flow_and_LUD_research/Quintana_Roo_Research/Data"
setwd(inDir)

outDir <- inDir

create_outDir_param = FALSE

#Create output directory

if(create_outDir_param==TRUE){  
  outDir <- create_dir_fun(outDir,out_suffix)
  setwd(outDir)
}else{
  setwd(outDir) #use previoulsy defined directory
}

########################################################
##############  Start of th script  ##############

###########################################
### PART 0: READ IN DATASETS RELATED TO FLOWS 


#tb <- read.table(file.path(inDir,filename_flow),sep=",")
#tb <- read.table(file.path(inDir,filename_flow),fill=T,sep=",")
tb <- read.table(file.path(inDir,filename_flow_cleaned), 
                 sep=',', quote=NULL,fill=TRUE, comment='', 
                 #as.is=F,
                 stringsAsFactors = F,
                 header=TRUE)

test <- read.table(file.path(inDir,flow_by_product_filename),header=T,sep=",") 
test3 <- read.table(file.path(inDir,flow_by_product_consumption_filename),header=T,sep=",") 
test2 <- read.table(file.path(inDir,flow_by_product_extraction_filename),header=T,sep=",") 

#############################
##### Figure 2

#slide 4,5,6

### Need to improve this code later on!!!


p1 <- xyplot(NV_CANT ~ year | flow_direction,subset(test,test$product_cat=="livestock"),
       type="b",
       #type="h",
       ylab="Head", 
       main="Livestock flows total by year ")

p2 <- xyplot(NV_CANT ~ year | flow_direction,subset(test,test$product_cat=="meat"),
       type="b",
       ylab="Tons",
       main="Meat flows total by year ")

p3 <- xyplot(NV_CANT ~ year | flow_direction,subset(test,test$product_cat=="agri"),
       type="b",
       ylab="Tons",
       main="Agricultural flows total by year ")

png_filename1 <- paste("Figure","_2a_","livestock_flow_directions_",out_suffix,".png", sep="")
png_filename2 <- paste("Figure","_2b_","meat_flow_directions_",out_suffix,".png", sep="")
png_filename3 <- paste("Figure","_2c_","agri_flow_directions_",out_suffix,".png", sep="")

layout_m <- c(1.5,1)

png(png_filename1,
    height=480*layout_m[2],width=480*layout_m[1])
print(p1)
dev.off()

png(png_filename2,
    height=480*layout_m[2],width=480*layout_m[1])
print(p2)
dev.off()

png(png_filename3,
    height=480*layout_m[2],width=480*layout_m[1])
print(p3)
dev.off()

#############################
##### Figure 3: Extraction B+C

#slide 8,9,10

test2$extraction_val <- as.character(test2$extraction)

test2$extraction_cat <- revalue(test2$extraction_val,
                            c("0"  = "outside", 
                              "1" = "inside")) 

p4 <- xyplot(NV_CANT ~ year | extraction_cat,subset(test2,test2$product_cat=="livestock"),
       type="b",
       ylab="Head", 
       main="Livestock flow extraction total by year ")

p5 <- xyplot(NV_CANT ~ year | extraction_cat,subset(test2,test2$product_cat=="meat"),
       type="b",
       ylab="Tons", 
       main="Meat flow extraction total by year ")

p6 <- xyplot(NV_CANT ~ year | extraction_cat,subset(test2,test2$product_cat=="agri"),
       type="b",
       ylab="Tons",
       main="Agriculture flow extraction total by year ")

png_filename4 <- paste("Figure","_3a_","livestock_extraction_cat_",out_suffix,".png", sep="")
png_filename5 <- paste("Figure","_3b_","meat_extraction_cat_",out_suffix,".png", sep="")
png_filename6 <- paste("Figure","_3c_","agri_extraction_cat_",out_suffix,".png", sep="")

layout_m <- c(1.5,1)

png(png_filename4,
    height=480*layout_m[2],width=480*layout_m[1])
print(p4)
dev.off()

png(png_filename5,
    height=480*layout_m[2],width=480*layout_m[1])
print(p5)
dev.off()

png(png_filename6,
    height=480*layout_m[2],width=480*layout_m[1])
print(p6)
dev.off()

#############################
##### Figure 4: consumption A+C

#slide 8,9,10

test3$consumption_val <- as.character(test3$consumption)

test3$consumption_cat <- revalue(test3$consumption_val,
                                c("0"  = "outside", 
                                  "1" = "inside")) 

## Local Consumption is A+C (defined as comsumption of from import and locally produced food)
#this is external produciton of food which is imported (A) or consumed locally (C)
# B is outflow (outside)
# A+C is 

p7 <- xyplot(NV_CANT ~ year | consumption_cat,subset(test3,test3$product_cat=="livestock"),
             type="b",
             ylab="Head", 
             main="Livestock flow consumption total by year ")

p8 <- xyplot(NV_CANT ~ year | consumption_cat,subset(test3,test3$product_cat=="meat"),
             type="b",
             ylab="Tons", 
             main="Meat flow consumption total by year ")

p9 <- xyplot(NV_CANT ~ year | consumption_cat,subset(test3,test3$product_cat=="agri"),
             type="b",
             ylab="Tons",
             main="Agriculture flow consumption total by year ")

png_filename7 <- paste("Figure","_4a_","livestock_consumption_cat_",out_suffix,".png", sep="")
png_filename8 <- paste("Figure","_4b_","meat_consumption_cat_",out_suffix,".png", sep="")
png_filename9 <- paste("Figure","_4c_","agri_consumption_cat_",out_suffix,".png", sep="")

layout_m <- c(1.5,1)

png(png_filename7,
    height=480*layout_m[2],width=480*layout_m[1])
print(p7)
dev.off()

png(png_filename8,
    height=480*layout_m[2],width=480*layout_m[1])
print(p8)
dev.off()

png(png_filename9,
    height=480*layout_m[2],width=480*layout_m[1])
print(p9)
dev.off()

#############################
##### Figure 5: consumption A+C

#slide 17,18,19
