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
library(plyr)

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
           sep=',', quote=NULL,fill=TRUE, comment='', 
           #as.is=F,
           stringsAsFactors = F,
           header=TRUE)

names(tb)

tb$X1.9_ORIG_CVE_HINT[tb$X1.9_ORIG_CVE_HINT=="MEXICO"] <- "MEX"
tb$X1.9_DEST_CVE_HINT[tb$X1.9_DEST_CVE_HINT=="MEXICO"] <- "MEX"

#> unique(tb$X1.9_ORIG_CVE_HINT)
#[1] "QR"  "MEX" "GYR" NA    ""    "W"  

table(tb$X1.9_ORIG_CVE_HINT)
#[1] "QR"  "MEX" "GYR" NA    ""    "W"  

table(tb$X1.9_ORIG_CVE_HINT)

#GYR    MEX     QR      W 
#141 185025 181118  88711      2 

table(tb$X1.9_DEST_CVE_HINT)

#GYR    MEX     QR      W 
#141  41583  17261 395450     11 
x <- (table(tb$X1.9_DEST_CVE_HINT))
#barplot(formatC(x, format = "d"))
#barplot(as.integer(tb$X1.9_DEST_CVE_HINT))
barplot(x)
#histogram((tb$X1.9_DEST_CVE_HINT))
tb <- subset(tb,tb$X1.9_ORIG_CVE_HINT!="" & tb$X1.9_DEST_CVE_HINT!="")
dim(tb)
x <- (table(tb$X1.9_ORIG_CVE_HINT))
print(x)
x <- (table(tb$X1.9_DEST_CVE_HINT))
print(x)
#barplot(formatC(x, format = "d"))
#barplot(as.integer(tb$X1.9_DEST_CVE_HINT))
barplot(x)

### Now prepare the data:
#Use 
#"ORIG_CLAVE","DEST_CLAVE"
#X1.9_ORIG_CVE_HINT,X1.9_DEST_CVE_HINT

#Outflow: need to recode hinterland with: 1 if HINT is GYR, MEX, MEXICO and Inflow: Examine X1.9_ORIG_CVE_HINT and X1.9_DEST_CVE_HINT, recode 1 if: 
#  MEX-QR
#  MEXICO-QR
#  GYR-QR
#Internal consumption: QR-WR

#a) QR<->QR
#b) QR->GYR
#c) QR<-GYR
#d) QR->MX
#e) QR<-MX
#f) QR->W
#g) QR-<W


### Do a quick crosstab
xtb <- table(tb$X1.9_ORIG_CVE_HINT,tb$X1.9_DEST_CVE_HINT)
print(xtb)

#       GYR    MEX     QR      W
#GYR    153   2656 182190      3
#MEX   1232     29 179836      7
#QR   40184  14564  33122      1
#W        0      0      2      0

tb$ORIG_DEST_HINT <- paste(tb$X1.9_ORIG_CVE_HINT,tb$X1.9_DEST_CVE_HINT,sep="_")
unique(tb$ORIG_DEST_HINT)
#[1] "QR_QR"   "MEX_QR"  "QR_GYR"  "QR_MEX"  "GYR_QR"  "GYR_MEX" "MEX_GYR" "GYR_GYR" "MEX_MEX" "MEX_W"  
#[11] "GYR_W"   "QR_W"    "W_QR"
#only 13 of the combination are realized compared to the crosstab

## Reclass 7 options out of 16, this is centering on options from QR point of view only!!!

#QR_QR <- a #internal consuption: C
#QR_GYR <- b #QR->GYR outflow: B
#GYR_QR <- c # inflow: A 
#QR_MEX <- d #d) QR->MX: B (outflow)
#MEX_QR <- e #e) QR<- MX: A (inflow) 
#QR_W <- f #f) QR->W: B (outflow)
#W_QR <- g #g) QR-<W: A (inflow)

##Using revalue from plyr package!
tb$flow_types <- revalue(tb$ORIG_DEST_HINT,
                         c("QR_QR"  = "a", # internal consuption: C
                           "QR_GYR" = "b", # QR->GYR outflow: B
                           "GYR_QR" = "c", # inflow: A 
                           "QR_MEX" = "d", # d) QR->MX: B (outflow)
                           "MEX_QR" = "e", # e) QR<- MX: A (inflow) 
                           "QR_W" = "f",   # f) QR->W: B (outflow)
                           "W_QR" = "g")) # g) QR-<W: A (inflow)

#Reclassify this and then classify in inflow and outlfow + 

tb$flow_direction <- revalue(tb$ORIG_DEST_HINT,
                             c("QR_QR"  = "C", # internal consuption: C
                               "QR_GYR" = "B", # QR->GYR outflow: B
                               "GYR_QR" = "A", # inflow: A 
                               "QR_MEX" = "B", # d) QR->MX: B (outflow)
                               "MEX_QR" = "A", # e) QR<- MX: A (inflow) 
                               "QR_W" = "B",   # f) QR->W: B (outflow)
                               "W_QR" = "A")) # g) QR-<W: A (inflow)


#### NOW LOOKING INTO REGRESSIONS

# ANOVA/MANOVA comparing
# 
# Interaction between hinterland scales
# 
#Regression 1: a vs b+c
#Regression 2: a vs d+e
#Regression 3: a vs f+g
#Regression 4: b+c vs d+e
#Regression 5: b+c vs f+g
#Regression 6: d+e vs f+g


##Regression 1: a vs b+c

#"QR_QR"  = "a", # internal consuption: C
#"QR_GYR" = "b", # QR->GYR outflow: B
#"GYR_QR" = "c", # inflow: A 

tb$flow_types




#################################  END OF FILE ###########################################


# ANOVA/MANOVA comparing
# 
# Interaction between hinterland scales
# 
# a vs b+c
# a vs d+e
# a vs f+g
# 
# b+c vs d+e
# b+c vs f+g
# 
# d+e vs f+g
# 
# Same but taking direction in mind
# 
# outflows only
# 
# b vs d vs f
# 
# inflows only
# 
# c, vs e vs g
# 
# all comparisons done in three types
# 
# movilizacion (=transactions, unitless, counts, we can put it in %)
# meat (kg)
# livestock (heads)
# agrucultural producs (tones)