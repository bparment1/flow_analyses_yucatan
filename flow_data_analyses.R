#################################    FLOW RESEARCH  #######################################
########################### SPACE-TIME QUINTINARO PRODUCT DATABASE  #############################################
#This script explores and analyzes data from the Quintinaro database of flow (food, wood etc).
#Data was collected by Marco Millones
#The goal is to run EOT, MEOT and PCA on the updated SST dataset using the IDRISI and the R package "remote".
#
#AUTHOR: Benoit Parmentier                                                                       #
#DATE CREATED:07/11/2016 
#DATE MODIFIED: 07/16/2016
#
#PROJECT: MEOT/EOT climate variability extraction
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

#################################################
###### Functions  used in the script  ##########

infile1_function <- file.path("/home/bparmentier/Google Drive/Papers_writing_MEOT/R_scripts/",
                              "PCA_EOT_comparison_data_update_function_07152016.R")
source(infile1_function)

#############################################
######## Parameters and arguments  ########

CRS_WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0" #Station coords WGS84 # CONST 2
proj_str<- CRS_WGS84 #param 2
CRS_reg <- CRS_WGS84 # PARAM 3

file_format <- ".rst" #PARAM 4
NA_value <- -9999 #PARAM5
NA_value_SST <- 32767
NA_flag_val <- NA_value #PARAM6
out_suffix <-"flow_07162016" #output suffix for the files and ouptu folder #PARAM 7
create_out_dir_param=TRUE #PARAM8
num_cores <- 4 #PARAM 9

#station_data_fname <- file.path("/home/bparmentier/Google Drive/NEST/", "MHB_data_2006-2015.csv") #PARAM 11

#years_to_process <- 2003:2016
years_to_process <- 1982:2015
#start_date <- "2012-01-01" #PARAM 12
#end_date <- "2012-12-31" #PARAM 13 #should process by year!!!
var_name <- "sst" #PARAM 14, Name of variable of interest: bacteria measurement (DMR data)
scaling <- 1/0.0099999998

r_mask_filename <- "/home/bparmentier/Google Drive/Papers_writing_MEOT/MEOT_paper/SST_data_update_1982_2015/lsmask.nc"

#out_suffix <- "eot_pca_1982_2015_anom_07152016"
inDir <- "/home/bparmentier/Google Drive/000_Flow_and_LUD_research/Quintana_Roo_Research/Data"
setwd(inDir)

#outDir <- "/Users/benoitparmentier/Dropbox/Data/Dissertation_paper2_04142012"
#outDir <- "/home/bparmentier/Google Drive/Papers_writing_MEOT/EOT_paper"
outDir <- inDir

create_outDir_param = TRUE

#Create output directory

if(create_outDir_param==TRUE){  
  outDir <- create_dir_fun(outDir,out_suffix)
  setwd(outDir)
}else{
  setwd(outDir) #use previoulsy defined directory
}

filename_flow <- "MO1-9_ALL.txt"


########################################################
##############  Start of th script  ##############

### PART 0: READ IN DATASETS RELATED TO TELECONNECTION AND PREVIOUS LOADINGS
#http://geog.uoregon.edu/bartlein/courses/geog607/Rmd/netCDF_01.htm

#tb <- read.table(file.path(inDir,filename_flow),sep=",")
#tb <- read.table(file.path(inDir,filename_flow),fill=T,sep=",")
tb <- read.table(file.path(inDir,filename_flow), 
           sep=',', quote=NULL,fill=TRUE, comment='', header=FALSE)

#scrping tables from html pages
#http://stackoverflow.com/questions/1395528/scraping-html-tables-into-r-data-frames-using-the-xml-package
