#################################    FLOW RESEARCH  #######################################
########################### SPACE-TIME QUINTINARO PRODUCT DATABASE  #############################################
#This script generates figures and tables from the processed Quintinaro database of flow (food, wood etc) analyzed previoulsy
#Data was collected by Marco Millones.
#
#AUTHOR: Benoit Parmentier                                                                       
#DATE CREATED:08/30/2016 
#DATE MODIFIED: 10/17/2016
#
#PROJECT: Flow and land cover change in QR and GYR with Marco Millones
#COMMIT: debuggin function for barplots figures and moving it to function script

## Code used in the current workflow:
#flow_data_analyses_10132016.R : this generates cleaned table of flows and data table used in analyses and figures
#flow_data_analyses_function_09162016.R: function script used in analyses and figures
#flow_data_analyses_production_of_tables_figures_10142016.R: figures and tables creation
#flow_data_analyses_production_of_tables_figures_functions_10142016.R: functions figures and tables 
#

##################################################################################################

######### Listing of outputs as it relates to flow draft:

######## TABLES

#Table 1. (Outside R) Datasets and sources a) GIS and Remotely Sensed Data b) Alphanumeric and flow data
#Table 2. (Outside R) Definition  and description of the different measures of land use displacment generated from flow data.
#Table 3. (Outside R) Flow definitions for Quintina Roo study area in relation to GYR (Peninsula), Mexico (MEX), World (W). We only consider flows to and from QR in this study.

#Table 4. Total count  of flow transaction by type all years aggregated. Include flows by type AG, LIVE, MEAT, by trans units and % total of each type
#Table 5. Flow quantities by type all years aggregated. Include flows by type AG, LIVE, MEAT, by NVQUANT units and % total of each type

####### FIGURES

# Figure 1: a) (not in R) Study area, Yucatan and Quintinaro with b) (not in R) Hinterland and origin/destination: Figure 3. Flows and distance used in generating the “Hinterland” varialbe. All flow transactions were summarized in four categories: 1) QR corresponding to flow internal to Quintina Roo, 2)  GYR corresponding to flow from or to Yucatan peninsula (excluding QR) , 3) MEX corresponding to flow from or to Mexico (excluding GYR and QR); W corresponding to flows from or to the rest of the world (excluding GYR and MEX. 
# Figure 2: a) (not in R) Flow type direction A, B and C (outside R) and b) (not in R) Flows and land use (this also shows A,B and C)
# Figure 3a: Flows quantities and import, export and internal flows.
# Figure 3b. Consumption dervied from flow using quantities for the 2001-2009 time period and three type of food products.
# Figure 3c: Production derived from flow using quantities for the 2001-2009 time period and three type of food products.
# Figure 4: Decoupling by year  for Quintana Roo expressed by quantities of flows by distance (Hinterland variable).
# Figure 5: Decoupling of Quintana Roo expressed by total number of flows (top) and total quantities (bottom) by distance (Hinterland variable) for three type of products: agriculture, meat and livestock.

#Figure 6: Land consumption expressed by percentage of landscape converted for crop and cattle activities aggregated by year. 
#Figure 7: Combined total percent land consumption year for both crop and cattle

#Supplementary documents:
#Figure 1s: Flow conversions (A:inflow, B:outflows, C:internal flow) converted into land consumption expressed by percentage of landscape converted for crop and cattle activities aggregated by year.

### Loading r library and packages

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

script_dir <- "/home/bparmentier/Google Drive/000_Flow_and_LUD_research/Quintana_Roo_Research/scripts"
infile1_function <- "flow_data_analyses_production_of_tables_figures_functions_10172016.R"

infile1_function <- file.path(script_dir,infile1_function)
source(infile1_function)

#############################################
######## Parameters and arguments  ########

#Input file name with the flow data
#filename_flow <- "MO1-9_ALL.txt"
CRS_WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0" #Station coords WGS84 # CONST 2
proj_str<- CRS_WGS84 #param 2
CRS_reg <- CRS_WGS84 # PARAM 3

file_format <- ".txt" #PARAM 4
NA_value <- -9999 #PARAM5
NA_flag_val <- NA_value #PARAM6
out_suffix <-"flow_10132016" #output suffix for the files and ouptu folder #PARAM 7

create_out_dir_param=TRUE #PARAM8
num_cores <- 4 #PARAM 9

#inDir <- "/home/bparmentier/Google Drive/000_Flow_and_LUD_research/Quintana_Roo_Research/Data"
#inDir <- "/home/bparmentier/Google Drive/000_Flow_and_LUD_research/Quintana_Roo_Research/Data/output_flow_08302016/"
#inDir <- NULL #if NULL then generate input dir from out_suffix
#inDir <- file.path(inDir, paste0("output_flow_",out_suffix))
inDir <- "/home/bparmentier/Google Drive/000_Flow_and_LUD_research/Quintana_Roo_Research/Data/output_flow_10132016"
setwd(inDir)

outDir <- inDir
#filename_flow_cleaned <- "/home/bparmentier/Google Drive/000_Flow_and_LUD_research/Quintana_Roo_Research/Data/output_flow_08302016/tb_overall_flow_data_clean_flow_08302016.txt"

create_outDir_param = FALSE

#Create output directory

if(create_outDir_param==TRUE){  
  outDir <- create_dir_fun(outDir,out_suffix)
  setwd(outDir)
}else{
  setwd(outDir) #use previoulsy defined directory
}

### file inputs created earlier from processing the flow data:

#filename_flow_cleaned <- "tb_overall_flow_data_clean_flow_08302016.txt"
#flow_by_product_filename <- "test_aggregated_data_by_flow_by_product_year_flow_08302016.txt"
#flow_by_product_consumption_filename <- "test3_aggregated_data_by_comsumption_by_product_year_flow_08302016.txt"
#flow_by_product_extraction_filename <- "test2_aggregated_data_by_exraction_by_product_year_flow_08302016.txt"

filename_flow_cleaned <- paste("tb_overall_flow_data_clean_",out_suffix,".txt",sep="")

flow_by_product_tb_summary1_filename <- paste("tb_summary1_aggregated_data_by_flow_by_product_year_",out_suffix,".txt",sep="")
flow_by_product_extraction_tb_summary2_filename <- paste("tb_summary2_aggregated_data_by_production_by_product_year_",out_suffix,".txt",sep="")
flow_by_product_consumption_tb_summary3_filename <- paste("tb_summary3_aggregated_data_by_comsumption_by_product_year_",out_suffix,".txt",sep="")

flow_aggregated_by_product_origin_dest_tb_summary4_filename <- paste("tb_summary4_aggregated_data_by_origin_dest_by_product_",out_suffix,".txt",sep="")
flow_aggregated_by_product_quantity_by_A_B_C_by_year_tb_summary5_filename <- paste("tb_summary5_aggregated_data_by_quantity_by_A_B_C_by_product_year",out_suffix,".txt",sep="")

#hinterland_by_product_sum_filename <- "hinterland_tb_product_sum_flow_08302016.txt"
#hinterland_year_by_product_sum_filename <- "hinterland_year_tb_product_sum_flow_08302016.txt"
hinterland_by_product_sum_filename <- paste("hinterland_tb_product_sum_",out_suffix,".txt",sep="")
hinterland_year_by_product_sum_filename <- paste("hinterland_year_tb_product_sum_",out_suffix,".txt",sep="")
hinterland_tb_quant_trans_year_product_sum_filename <- paste("hinterland_tb_quant_trans_year_tb_product_sum_",out_suffix,".txt",sep="")

### Land consumption related files
#agri
tb_land_summarized_agri_filename <- paste("tb_land_summarized_","agri","_by_product_year",out_suffix,".txt",sep="")
tb_land_summarized2_A_B_C_agri_filename <- paste("tb_land_summarized2_","agri","_by_flow_A_B_C_year",out_suffix,".txt",sep="")
tb_land_agri_filename <- paste("tb_land_","agri", out_suffix,".txt",sep="")
#livestock
tb_land_summarized_livestock_filename <- paste("tb_land_summarized_","livestock","_by_product_year",out_suffix,".txt",sep="")
tb_land_summarized2_A_B_C_livestock_filename <- paste("tb_land_summarized2_","livestock","_by_flow_A_B_C_year",out_suffix,".txt",sep="")
tb_land_livestock_filename <- paste("tb_land_","livestock", out_suffix,".txt",sep="")

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

#test <- read.table(file.path(inDir,flow_by_product_filename),header=T,sep=",") 
#test3 <- read.table(file.path(inDir,flow_by_product_consumption_filename),header=T,sep=",") 
#test2 <- read.table(file.path(inDir,flow_by_product_extraction_filename),header=T,sep=",") 

tb_summary1 <- read.table(file.path(inDir,flow_by_product_tb_summary1_filename),header=T,sep=",") 
tb_summary3 <- read.table(file.path(inDir,flow_by_product_consumption_tb_summary3_filename),header=T,sep=",") 
tb_summary2 <- read.table(file.path(inDir,flow_by_product_extraction_tb_summary2_filename),header=T,sep=",") 
tb_summary4 <- read.table(file.path(inDir,flow_aggregated_by_product_origin_dest_tb_summary4_filename),header=T,sep=",") 
tb_summary5 <- read.table(file.path(inDir,flow_aggregated_by_product_quantity_by_A_B_C_by_year_tb_summary5_filename ),header=T,sep=",") 

#hinterland_by_product_sum_filename <- "hinterland_tb_product_sum_flow_08302016.txt"
#hinterland_year_by_product_sum_filename <- "hinterland_year_tb_product_sum_flow_08302016.txt"

hinterland_year_tb  <- read.table(file.path(inDir,hinterland_by_product_sum_filename),header=T,sep=",") 
hinterland_tb  <- read.table(file.path(inDir,hinterland_year_by_product_sum_filename),header=T,sep=",")
hinterland_tb_quant_trans <- read.table(file.path(inDir,hinterland_tb_quant_trans_year_product_sum_filename),header=T,sep=",")
            
#### Land consumption from conversion rates

#agri
tb_land_summarized_agri <- read.table(file.path(inDir,tb_land_summarized_agri_filename),header=T ,sep=",")
tb_land_summarized2_agri <- read.table(file.path(inDir,tb_land_summarized2_A_B_C_agri_filename),header=T,sep=",")
tb_land_agri <- read.table(file.path(outDir,tb_land_agri_filename),header=T,sep=",")

#livestock
tb_land_summarized_livestock <- read.table(file=file.path(inDir,tb_land_summarized_livestock_filename),header=T ,sep=",")
tb_land_summarized2_livestock <- read.table(file=file.path(inDir,tb_land_summarized2_A_B_C_livestock_filename),header=T,sep=",")
tb_land_livestock <- read.table(file.path(inDir,tb_land_livestock_filename),header=T,sep=",")

#################################
############## Part 0: Generate table ################

options(scipen=999) #don't use scientific notation

##########
### Generate table 4
#Table 4. Total count  of flow transaction by type all years aggregated. Include flows by type AG, LIVE, MEAT, by trans units and % total of each type

rows_to_remove <- c("QR_W","W_QR")
#length(unique(tb_summary4$ORIG_DEST_HINT)) #from 7 drop 5 of them
table4_tmp <- subset(tb_summary4, !ORIG_DEST_HINT %in% rows_to_remove) 

table4_agri <- subset(table4_tmp,product_cat=="agri")
table4_livestock <- subset(table4_tmp,product_cat=="livestock")
table4_meat <- subset(table4_tmp,product_cat=="meat")

col_agri <- format_column_for_table(table4_agri$transaction_bool,"agri")
col_livestock <- format_column_for_table(table4_livestock$transaction_bool,"livestock")
col_meat <- format_column_for_table(table4_meat$transaction_bool,"meat")

df_table4 <- cbind(c(as.character(table4_agri$flow_direction),"TOTAL"),c(as.character(table4_agri$ORIG_DEST_HINT)," "),
                   col_agri,col_livestock,col_meat)
names(df_table4) <- c("Flow","Direction","agri","livestock","meat")

table4_filename <- file.path(inDir,paste0("df_table4_",out_suffix,".txt"))
write.table(df_table4,file=table4_filename,sep=",")

##########
### Generate table 5
#Table 5. Flow quantities by type all years aggregated. Include flows by type AG, LIVE, MEAT, by NVQUANT units and % total of each type

##Make this a function...

rows_to_remove <- c("QR_W","W_QR")
#length(unique(tb_summary4$ORIG_DEST_HINT)) #from 7 drop 5 of them
table5_tmp <- subset(tb_summary5, !ORIG_DEST_HINT %in% rows_to_remove) 

table5_agri <- subset(table5_tmp,product_cat=="agri")
table5_livestock <- subset(table5_tmp,product_cat=="livestock")
table5_meat <- subset(table5_tmp,product_cat=="meat")

col_agri <- format_column_for_table(table5_agri$NV_CANT,"agri")
col_livestock <- format_column_for_table(table5_livestock$NV_CANT,"livestock")
col_meat <- format_column_for_table(table5_meat$NV_CANT,"meat")

df_table5 <- cbind(c(as.character(table5_agri$flow_direction),"TOTAL"),c(as.character(table5_agri$ORIG_DEST_HINT)," "),
                   col_agri,col_livestock,col_meat)
names(df_table5) <- c("Flow","Direction","agri","livestock","meat")

table5_filename <- file.path(inDir,paste0("df_table5_",out_suffix,".txt"))
write.table(df_table5,file=table5_filename,sep=",")


#################################
############## Part 1: Figure generation ################

# Figure 1: a) (not in R) Study area, Yucatan and Quintinaro with b) (not in R) Hinterland and origin/destination: Figure 3. Flows and distance used in generating the “Hinterland” varialbe. All flow transactions were summarized in four categories: 1) QR corresponding to flow internal to Quintina Roo, 2)  GYR corresponding to flow from or to Yucatan peninsula (excluding QR) , 3) MEX corresponding to flow from or to Mexico (excluding GYR and QR); W corresponding to flows from or to the rest of the world (excluding GYR and MEX. 
# Figure 2: a) (not in R) Flow type direction A, B and C (outside R) and b) (not in R) Flows and land use (this also shows A,B and C)
# Figure 3a: Flows quantities and import, export and internal flows.
# Figure 3b. Consumption dervied from flow using quantities for the 2001-2009 time period and three type of food products.
# Figure 3c: Production derived from flow using quantities for the 2001-2009 time period and three type of food products.
# Figure 4: Decoupling by year  for Quintana Roo expressed by quantities of flows by distance (Hinterland variable).
# Figure 5: Decoupling of Quintana Roo expressed by total number of flows (top) and total quantities (bottom) by distance (Hinterland variable) for three type of products: agriculture, meat and livestock.

#Figure 6: Land consumption expressed by percentage of landscape converted for crop and cattle activities aggregated by year. 
#Figure 7: Combined total percent land consumption year for both crop and cattle

#Supplementary material
#Figure s1: Flow conversions (A:inflow, B:outflows, C:internal flow) converted into land consumption expressed by percentage of landscape converted for crop and cattle activities aggregated by year.

#########
##### Figure 3: Flows quantities and import, export and internal flows.

### Need to improve this code later on!!!

#p1 <- xyplot(NV_CANT ~ year | flow_direction,subset(tb_summary1,tb_summary1$product_cat=="agri"),
#             type="b",
#             ylab="Tons",
#             main="AGRI flows total by year ")
title_str <- "Flows total by year" # use this title because it is the one used in the combined plot!! (first plot)

p1 <- xyplot(NV_CANT ~ year ,groups=flow_direction,subset(tb_summary1,tb_summary1$product_cat=="agri"),
             type="b",
             ylab="Tons",
             main=title_str)

##This combines everything but can't see the differences within the categories
#p1_test <- xyplot(NV_CANT ~ year| product_cat ,groups=flow_direction,tb_summary1,
#                  #y.same=FALSE, option only use for as.layer and c.trellis
#             type="b",
#            ylab="Tons",
#             main="AGRI flows total by year ")

#xyplot(y~x, groups=z, df)

p2 <- xyplot(NV_CANT ~ year, groups= flow_direction,subset(tb_summary1,tb_summary1$product_cat=="meat"),
             type="b",
             ylab="Tons",
             main="MEAT flows total by year ")

p3 <- xyplot(NV_CANT ~ year, groups = flow_direction,subset(tb_summary1,tb_summary1$product_cat=="livestock"),
       type="b",
       #type="h",
       ylab="Head", 
       main="LIVESTOCK flows total by year ")

png_filename3a1 <- paste("Figure","_3a1_","agri_flow_directions_",out_suffix,".png", sep="")
png_filename3a2 <- paste("Figure","_3a2_","meat_flow_directions_",out_suffix,".png", sep="")
png_filename3a3 <- paste("Figure","_3a3_","livestock_flow_directions_",out_suffix,".png", sep="")

layout_m <- c(1.5,1)

png(png_filename3a1,
    height=480*layout_m[2],width=480*layout_m[1])
print(p1)
dev.off()

png(png_filename3a2,
    height=480*layout_m[2],width=480*layout_m[1])
print(p2)
dev.off()

png(png_filename3a3,
    height=480*layout_m[2],width=480*layout_m[1])
print(p3)
dev.off()

#http://www.magesblog.com/2015/04/combining-several-lattice-charts-into.html
#test_ p <- c(Busses=pltBusses, Trains=pltTrains, 
#  y.same=TRUE,layout=c(4,2))
#c.trellis

p_3a_combined <- c(AGRI=p1, MEAT=p2, LIVESTOCK=p3,layout=c(3,1))#,main="CONSUMPTION")
#             y.same=FALSE,layout=c(4,2))

png_filename3a <- paste("Figure","_3a_combined_","agri_meat_livectock_flow_directions_A_B_C_",out_suffix,".png", sep="")

layout_m <- c(1.5,1)

## Improve size of the font!!!
png(png_filename3a,
    #height=480*layout_m[2],width=480*layout_m[1])
    height=480*1,width=480*3)
print(p_3a_combined)
dev.off()

#############################
##### Figure 3b: consumption A+C
#Figure 3b. Consumption dervied from flow using quantities for the 2001-2009 time period and three type of food products.
#slides 8,9,10

tb_summary3$consumption_val <- as.character(tb_summary3$consumption)

tb_summary3$consumption_cat <- revalue(tb_summary3$consumption_val,
                                 c("0"  = "B", 
                                   "1" = "A + C")) 

## Local Consumption is A+C (defined as comsumption of from import and locally produced food)
#this is external produciton of food which is imported (A) or consumed locally (C)
# B is outflow (outside)
# A+C is 

p7 <- xyplot(NV_CANT ~ year, groups= consumption_cat,subset(tb_summary3,tb_summary3$product_cat=="agri"),
             type="b",
             ylab="Tons",
             main="Flow consumption total by year ")

#p8 <- xyplot(NV_CANT ~ year | consumption_cat,subset(tb_summary3,tb_summary3$product_cat=="meat"),
#             type="b",
#             ylab="Tons", 
#             main="MEAT flow consumption total by year ")

p8 <- xyplot(NV_CANT ~ year, groups= consumption_cat,subset(tb_summary3,tb_summary3$product_cat=="meat"),
             type="b",
             ylab="Tons", 
             main="MEAT flow consumption total by year ")

p9 <- xyplot(NV_CANT ~ year, groups= consumption_cat,subset(tb_summary3,tb_summary3$product_cat=="livestock"),
             type="b",
             ylab="Head", 
             main="LIVESTOCK flow consumption total by year ")

png_filename3b1 <- paste("Figure","_3b1_","agri_consumption_cat_",out_suffix,".png", sep="")
png_filename3b2 <- paste("Figure","_3b2_","meat_consumption_cat_",out_suffix,".png", sep="")
png_filename3b3 <- paste("Figure","_3b3_","livestock_consumption_cat_",out_suffix,".png", sep="")

layout_m <- c(1.5,1)

png(png_filename3b1,
    height=480*layout_m[2],width=480*layout_m[1])
print(p7)
dev.off()

png(png_filename3b2,
    height=480*layout_m[2],width=480*layout_m[1])
print(p8)
dev.off()

png(png_filename3b3,
    height=480*layout_m[2],width=480*layout_m[1])
print(p9)
dev.off()

#http://www.magesblog.com/2015/04/combining-several-lattice-charts-into.html
#test_ p <- c(Busses=pltBusses, Trains=pltTrains, 
#  y.same=TRUE,layout=c(4,2))
#c.trellis

p_3b_combined <- c(AGRI=p7, MEAT=p8, LIVESTOCK=p9,layout=c(3,1))#,main="CONSUMPTION")
#             y.same=FALSE,layout=c(4,2))

png_filename3b <- paste("Figure","_3b_combined_","agri_meat_livectock_consumption_cat_",out_suffix,".png", sep="")

layout_m <- c(1.5,1)

## Improve size of the font!!!
png(png_filename3b,
    #height=480*layout_m[2],width=480*layout_m[1])
    height=480*1,width=480*3)
print(p_3b_combined)
dev.off()

#############################
##### Figure 3c: Production B+C
# Figure 7: Production derived from flow using quantities for the 2001-2009 time period and three type of food products.

#slide 8,9,10

tb_summary2$extraction_val <- as.character(tb_summary2$extraction)

tb_summary2$extraction_cat <- revalue(tb_summary2$extraction_val,
                            c("0"  = "A", 
                              "1" = "B + C")) 

#p4 <- xyplot(NV_CANT ~ year | extraction_cat,subset(tb_summary2,tb_summary2$product_cat=="agri"),
#             type="b",
#             ylab="Tons",
#             main="AGRI flow production total by year ")
#xyplot(mpg~disp, data=mtcars, 
#       scales=list(tck=c(1,0), x=list(cex=1.2), y=list(cex=1.5)))

#p4 <- xyplot(NV_CANT ~ year, groups=extraction_cat,subset(tb_summary2,tb_summary2$product_cat=="agri"),
#             pch=1:2,pch.cex=3,
#             auto.key=list(columns=1,space="right",title="Method",cex=1.2,font=2), #Legend information
#             type="b",
#             ylab="Tons",
#             main="AGRI flow production total by year ")

#corner=c(0,1)

### Make this a function later
p4 <- xyplot(NV_CANT ~ year, groups=extraction_cat,subset(tb_summary2,tb_summary2$product_cat=="agri"),
             pch=1:2,pch.cex=3,
             auto.key=list(columns=1,corner=c(0,1),cex=1.2,font=2), #Legend information
             type="b",
             ylab=list(label="Tons", cex=2, font=2),
             xlab=list(label="year", cex=2,font=2),
             main=list("AGRI flow production total by year ",cex=2),
             scales=list(x=list(cex=1.5), y=list(cex=1.5)))

p5 <- xyplot(NV_CANT ~ year, groups=extraction_cat,subset(tb_summary2,tb_summary2$product_cat=="meat"),
             pch=1:2,pch.cex=3,
             auto.key=list(columns=1,corner=c(0,1),cex=1.2,font=2), #Legend information
             type="b",
             ylab=list(label="Tons", cex=2, font=2),
             xlab=list(label="year", cex=2,font=2),
             main=list("MEAT flow production total by year ",cex=2),
             scales=list(x=list(cex=1.5), y=list(cex=1.5)))

p6 <- xyplot(NV_CANT ~ year, groups=extraction_cat,subset(tb_summary2,tb_summary2$product_cat=="livestock"),
       #main="LIVESTOCK flow production total by year ")
       pch=1:2,pch.cex=3,
       auto.key=list(columns=1,corner=c(0,1),cex=1.2,font=2), #Legend information
       type="b",
       ylab=list(label="Head", cex=2, font=2),
       xlab=list(label="year", cex=2,font=2),
       main=list("LIVESTOCK flow production total by year ",cex=2),
       scales=list(x=list(cex=1.5), y=list(cex=1.5)))


png_filename3c1 <- paste("Figure","_3c1_","agri_production_cat_",out_suffix,".png", sep="")
png_filename3c2 <- paste("Figure","_3c2_","meat_production_cat_",out_suffix,".png", sep="")
png_filename3c3 <- paste("Figure","_3c3_","livestock_production_cat_",out_suffix,".png", sep="")

layout_m <- c(1.5,1)

png(png_filename3c1,
    height=480*layout_m[2],width=480*layout_m[1])
print(p4)
dev.off()

png(png_filename3c2,
    height=480*layout_m[2],width=480*layout_m[1])
print(p5)
dev.off()

png(png_filename3c3,
    height=480*layout_m[2],width=480*layout_m[1])
print(p6)
dev.off()

p_3c_combined <- c(AGRI=p4, MEAT=p5, LIVESTOCK=p6,layout=c(3,1))#,main="CONSUMPTION")
#             y.same=FALSE,layout=c(4,2))

png_filename3c <- paste("Figure","_3c_combined_","agri_meat_livectock_production_cat_",out_suffix,".png", sep="")

layout_m <- c(1.5,1)

## Improve size of the font!!!
png(png_filename3c,
    #height=480*layout_m[2],width=480*layout_m[1])
    height=480*1,width=480*3)
print(p_3c_combined)
dev.off()

###################### combine figure 3 #########

p_3_combined <- c(AGRI=p_3a_combined, MEAT=p_3b_combined, LIVESTOCK=p_3c_combined,layout=c(3,3))#,main="CONSUMPTION")
#             y.same=FALSE,layout=c(4,2))

png_filename3 <- paste("Figure","_3_combined_","agri_meat_livectock_flows_consumption_production_cat_",out_suffix,".png", sep="")

layout_m <- c(1.5,1)

## Improve size of the font!!!
png(png_filename3,
    #height=480*layout_m[2],width=480*layout_m[1])
    height=480*3,width=480*3)
print(p_3_combined)
dev.off()

#############################
##### Figure 4: Decoupling from distance flow and years
# Figure 4: Decoupling by year  for Quintana Roo expressed by quantities of flows by distance (Hinterland variable).
#http://stackoverflow.com/questions/17721126/simplest-way-to-do-grouped-barplot
#slide 17,18,19

## Add colors to barplots
#options(scipen=999)

p6 <- barchart(NV_CANT ~ year, groups=hinterland_year_tb$labels ,
               subset(hinterland_year_tb,hinterland_year_tb$product_cat=="livestock"),
             type="b",
             ylab="Head", 
             horizontal=FALSE,
             xlab = hinterland_year_tb$labels,
             main="LIVESTOCK flow production total by year ")

png_filename4a <- paste("Figure","_4a_","agri_hinterland_cat_sum_",out_suffix,".png", sep="")
png_filename4b <- paste("Figure","_4b_","meat_hinterland_cat_sum_",out_suffix,".png", sep="")
png_filename4c <- paste("Figure","_4c_","livestock_hinterland_cat_sum_",out_suffix,".png", sep="")

hinterland_tb  <- read.table(file.path(inDir,hinterland_by_product_sum_filename),header=T,sep=",") 
hinterland_year_tb  <- read.table(file.path(inDir,hinterland_year_by_product_sum_filename),header=T,sep=",")

hinterland_year_tb$flow_dist_cat_val <- as.character(hinterland_year_tb$flow_dist_cat)

hinterland_year_tb$flow_dist_hint <- revalue(hinterland_year_tb$flow_dist_cat_val,
                         c("0" = "QR", 
                           "1" = "GYR",
                           "2" = "MEX", 
                           "3" = "W")) 
hinterland_year_tb$labels <- paste(hinterland_year_tb$year,hinterland_year_tb$flow_dist_hint)
hinterland_year_tb <- subset(hinterland_year_tb, hinterland_year_tb$flow_dist_hint!="W")

layout_m <- c(1.5,1)

col_palette <- c("red","blue","darkgreen")

#### Agri hinterland

#dates_val <- hinterland_year_tb$labels

hint_tmp <- subset(hinterland_year_tb,hinterland_year_tb$product_cat=="agri")
x_labels <- hint_tmp$labels

l_df <- lapply(unique(hint_tmp$flow_dist_hint),FUN=function(x){subset(hint_tmp,hint_tmp$flow_dist_hint==x)})
l_heights <- lapply(l_df, FUN=function(x){x$NV_CANT})
heights <- do.call(cbind , l_heights)
heights <-as.matrix(t(heights))

png(png_filename4a,
    height=480*layout_m[2],width=480*layout_m[1])

#barplot(hint_tmp$NV_CANT,names.arg=x_labels,las=2,
#        main="AGRI flows total quantities by hinterland category")
barplot(heights,names.arg=x_labels,las=2,
        main="AGRI flows total quantities by hinterland category",
        #names.arg=names_ind,
        cex.names=1.1,   
        col=col_palette, 
        beside=TRUE) # see two barplots for comparisons...
dev.off()

#### Meat hinterland

hint_tmp <- subset(hinterland_year_tb,hinterland_year_tb$product_cat=="meat")
x_labels <- hint_tmp$labels

#hint_tmp
l_df <- lapply(unique(hint_tmp$flow_dist_hint),FUN=function(x){subset(hint_tmp,hint_tmp$flow_dist_hint==x)})
l_heights <- lapply(l_df, FUN=function(x){x$NV_CANT})
heights <- do.call(cbind , l_heights)
heights <-as.matrix(t(heights))

png(png_filename4b,
    height=480*layout_m[2],width=480*layout_m[1])

barplot(heights,names.arg=x_labels,las=2,
        main="MEAT flows total quantities by hinterland category",
        #names.arg=names_ind,
        cex.names=1.1,   
        col=col_palette, 
        beside=TRUE) # see two barplots for comparisons...

dev.off()


#### Livestock hinterland

#dates_val <- sort(rep(2001:2009,3))
#x_labels <- paste(dates_val,rep(c("QR","GYR","MEX"),9),sep=" ")
hint_tmp <- subset(hinterland_year_tb,hinterland_year_tb$product_cat=="livestock")
x_labels <- hint_tmp$labels

hint_tmp
l_df <- lapply(unique(hint_tmp$flow_dist_hint),FUN=function(x){subset(hint_tmp,hint_tmp$flow_dist_hint==x)})
l_heights <- lapply(l_df, FUN=function(x){x$NV_CANT})
heights <- do.call(cbind , l_heights)
heights <-as.matrix(t(heights))

### Make this a function

png(png_filename4c,
    height=480*layout_m[2],width=480*layout_m[1])
barplot(heights,names.arg=x_labels,las=2,
        main="LIVESTOCK flows total quantities by hinterland category",
        #names.arg=names_ind,
        cex.names=1.1,   
        col=c("red","blue","darkgreen"), 
        beside=TRUE) # see two barplots for comparisons...
dev.off()

##### Use function to generate barplot

options(scipen=999)
layout_m <- c(1.5,1)

col_palette <- c("red","blue","darkgreen")

#### Agri hinterland

#dates_val <- hinterland_year_tb$labels

product_cat_val <- "agri"
title_str <- "AGRI flows total quantities by hinterland category"
hint_tmp <- subset(hinterland_year_tb,hinterland_year_tb$product_cat=="livestock")
x_labels <- hint_tmp$labels
png_filename4a <- paste("Figure","_4a_","agri_hinterland_cat_sum_",out_suffix,".png", sep="")
#png_filename4b <- paste("Figure","_4b_","meat_hinterland_cat_sum_",out_suffix,".png", sep="")
#png_filename4c <- paste("Figure","_4c_","livestock_hinterland_cat_sum_",out_suffix,".png", sep="")

out_barplot_png <- generate_barplot_flows(df_tb=hinterland_year_tb,
                                           x_labels=x_labels,
                                           col_palette=col_palette,
                                           layout_m=layout_m,
                                           product_cat_val=product_cat_val,
                                           title_str=title_str,
                                           out_filename=png_filename4a)
#### Now meat

product_cat_val <- "meat"
title_str <- "MEAT flows total quantities by hinterland category"
hint_tmp <- subset(hinterland_year_tb,hinterland_year_tb$product_cat=="livestock")
x_labels <- hint_tmp$labels
png_filename4b <- paste("Figure","_4b_","meat_hinterland_cat_sum_",out_suffix,".png", sep="")


out_barplot_png <- generate_barplot_flows(df_tb=hinterland_year_tb,
                                          x_labels=x_labels,
                                          col_palette=col_palette,
                                          layout_m=layout_m,
                                          product_cat_val=product_cat_val,
                                          title_str=title_str,
                                          out_filename=png_filename4a)



######### Figure 5: decoupling: aggregated flows by total numbers and quantities for three products
# Figure 5: Decoupling of Quintana Roo expressed by total number of flows (top) and total quantities (bottom) by distance (Hinterland variable) for three type of products: agriculture, meat and livestock.

hinterland_tb_quant_trans
#hinterland_tb_quant_trans$flow_dist_label <- revalue(hinterland_tb_quant_trans$flow_dist_cat,
#                                                     c("0"  = "QR", # internal consuption: C
#                                                       "1" = "GYR", # QR->GYR outflow: B
#                                                       "2" = "MEX")) # g) QR-<W: A (inflow) and W->QR

x_label <- c("QR","GYR","MEX")


hinterland_tb_quant <- aggregate(NV_CANT ~ flow_dist_cat + product_cat ,data=hinterland_tb_quant_trans,sum)
hinterland_tb_trans <- aggregate(transaction_bool ~ flow_dist_cat + product_cat ,data=hinterland_tb_quant_trans,sum)


### Now plot the figures
png_filename5_bottom <- paste("Figure","_5_bottom_","agri_hinterland_cat_sum_",out_suffix,".png", sep="")
### Now plot the figures
png_filename5_top <- paste("Figure","_5_top_","agri_hinterland_cat_sum_",out_suffix,".png", sep="")

res_pix<-480*0.9
col_mfrow<- 3
row_mfrow<- 1
m <- rbind(c(1, 3))
#print(m)

png(filename=png_filename5_bottom,
    width=col_mfrow*res_pix,height=row_mfrow*res_pix)

par(mfrow=c(row_mfrow,col_mfrow))

hinterland_tb_tmp <- subset(hinterland_tb_quant,product_cat=="agri")
barplot(hinterland_tb_tmp$NV_CANT,main="AGRI volumes",names.arg=x_label,cex.axis=1.1,cex.lab=1.3,ylab="Tons")

hinterland_tb_tmp <- subset(hinterland_tb_quant,product_cat=="meat")
barplot(hinterland_tb_tmp$NV_CANT,main="MEAT volumes",names.arg=x_label,cex.axis=1.1,cex.lab=1.3,ylab="Tons")

hinterland_tb_tmp <- subset(hinterland_tb_quant,product_cat=="livestock")
barplot(hinterland_tb_tmp$NV_CANT,main="LIVESTOCK volumes",names.arg=x_label,cex.axis=1.1,cex.lab=1.3,ylab="Heads")

dev.off()

png(filename=png_filename5_top,
    width=col_mfrow*res_pix,height=row_mfrow*res_pix)

par(mfrow=c(row_mfrow,col_mfrow))

hinterland_tb_tmp <- subset(hinterland_tb_trans,product_cat=="agri")
barplot(hinterland_tb_tmp$transaction_bool,main="AGRI transactions",names.arg=x_label,cex.axis=1.1,cex.lab=1.3,ylab="Number of transactions")

hinterland_tb_tmp <- subset(hinterland_tb_trans,product_cat=="meat")
barplot(hinterland_tb_tmp$transaction_bool,main="MEAT transactions",names.arg=x_label,cex.axis=1.1,cex.lab=1.3,ylab="Number of transactions")

hinterland_tb_tmp <- subset(hinterland_tb_trans,product_cat=="livestock")
barplot(hinterland_tb_tmp$transaction_bool,main="LIVESTOCK transactions",names.arg=x_label,cex.axis=1.1,cex.lab=1.3,ylab="Number of transactions")

dev.off()

######### Figure 6: Total land consumption for crop and livestock

#tb_land_agri$total_land_consumed <- total_land_consumed_qr

tb_land_summarized_agri_filename <- paste("tb_land_summarized_","agri","_by_product_year",out_suffix,".txt",sep="")
tb_land_summarized2_A_B_C_agri_filename <- paste("tb_land_summarized2_","agri","_by_flow_A_B_C_year",out_suffix,".txt",sep="")
tb_land_agri_filename <- paste("tb_land_","agri", out_suffix,".txt",sep="")


#### Writing the tables
tb_land_summarized_livestock_filename <- paste("tb_land_summarized_","livestock","_by_product_year",out_suffix,".txt",sep="")
tb_land_summarized2_A_B_C_livestock_filename <- paste("tb_land_summarized2_","livestock","_by_flow_A_B_C_year",out_suffix,".txt",sep="")
tb_land_livestock_filename <- paste("tb_land_","livestock", out_suffix,".txt",sep="")

png_filename6a <- paste("Figure","_6a_","agri_total_percent_land_consumption_by_year",out_suffix,".png", sep="")
png_filename6c <- paste("Figure","_6c_","livestock_total_percent_land_consumption_by_year",out_suffix,".png", sep="")

layout_m <- c(1.5,1)

png(png_filename6a,
    height=480*layout_m[2],width=480*layout_m[1])

plot(percent_land_consumption ~ year,data=tb_land_summarized_agri ,type="b",
     ylab="% of land in QR",
     main="AGRI land consumption as percentage of land")

dev.off()

png(png_filename6c,
    height=480*layout_m[2],width=480*layout_m[1])

plot(percent_land_consumption ~year,data=tb_land_summarized_livestock,type="b",
     ylab="% of land in QR",
     main="LIVESTOCK land consumption as percentage of land")

dev.off()

#################
####Figure 7: combine agri and livestock land consumption

tb_land_summarized_livestock$percent_land_consumption
tb_land_summarized_agri$percent_land_consumption

tb_land_summarized_combined <- subset(tb_land_summarized_livestock,select=c("year","percent_land_consumption"))
names(tb_land_summarized_combined) <- c("year","percent_land_consumption_livestock")
tb_land_summarized_combined$percent_land_consumption_agri <- tb_land_summarized_agri$percent_land_consumption
tb_land_summarized_combined$percent_land_consumption_total <- tb_land_summarized_combined$percent_land_consumption_agri + tb_land_summarized_combined$percent_land_consumption_livestock

#plot(tb_land_summarized_combined$percent_land_consumption_total ~ tb_land_summarized_combined$year)

out_filename <- file.path(inDir,paste("tb_land_consumption_summarized_",out_suffix,".txt",sep=""))
write.table(tb_land_summarized_combined,file= out_filename,sep=",")

png_filename7 <- paste("Figure","_7_","combined_total_percent_land_consumption_year",out_suffix,".png", sep="")

png(png_filename7,
    height=480*layout_m[2],width=480*layout_m[1])

plot(percent_land_consumption_total ~ year,data=tb_land_summarized_combined ,type="b",
     ylim=c(0,250),
     ylab="% of land in QR",
     main="Total land consumption as percentage of land")

abline(h=100,col=4,lty=2)

dev.off()

#########################################
######## Supplementary Figure #######

#####################
######### Figure s1:

#Figure s1. Flow conversions (A:inflow, B:outflows, C=internal flow) converted into land consumption expressed by percentage of landscape converted for crop and cattle activities aggregated by year.

#plot(tb_summarized$land_consumption ~year,data=tb_summarized,type="b",main="crop")

tb_land_summarized2_agri <- aggregate(percent_land_consumption ~ flow_direction + year, data =  tb_land_agri, sum)

p10 <- xyplot(percent_land_consumption ~ year | flow_direction ,data=tb_land_summarized2_agri,
              type="b",
              ylab="% of land in QR", 
              main="AGRI land consumption as percentage of land")

#p10


tb_land_summarized2_livestock <- aggregate(percent_land_consumption ~ flow_direction + year, data = tb_land_livestock, sum)

p11 <- xyplot(percent_land_consumption ~ year | flow_direction ,data=tb_land_summarized2_livestock,
              type="b",
              ylab="% of land in QR", 
              main="Livestock land consumption as percentage of land")

#p11

png_filename_s1a <- paste("Figure","_s1a_","agri_total_percent_land_consumption_A_B_C_by_year",out_suffix,".png", sep="")
#png_filename7b <- paste("Figure","_7b_","meat_production_cat_",out_suffix,".png", sep="")
png_filename_s1c <- paste("Figure","_s1c_","livestock_total_percent_land_consumption_A_B_C_by_year",out_suffix,".png", sep="")

layout_m <- c(1.5,1)

png(png_filename_s1a,
    height=480*layout_m[2],width=480*layout_m[1])
print(p10)
dev.off()

png(png_filename_s1c,
    height=480*layout_m[2],width=480*layout_m[1])
print(p11)
dev.off()


################## END OF SCRIPT #####################




