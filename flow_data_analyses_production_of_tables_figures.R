#################################    FLOW RESEARCH  #######################################
########################### SPACE-TIME QUINTINARO PRODUCT DATABASE  #############################################
#This script generates figures and tables from the processed Quintinaro database of flow (food, wood etc) analyzed previoulsy
#Data was collected by Marco Millones.
#
#AUTHOR: Benoit Parmentier                                                                       
#DATE CREATED:08/30/2016 
#DATE MODIFIED: 09/16/2016
#
#PROJECT: Flow and land cover change in QR and GYR with Marco Millones
#COMMIT: #more changes to generate figures for land consumption for livestock and agri

##################################################################################################

######### Listing of outputs as it relates to flow draft:

######## TABLES

#Table 1. (Outside R) Datasets and sources a) GIS and Remotely Sensed Data b) Alphanumeric and flow data
#Table 2. (Outside R) Definition  and description of the different measures of land use displacment generated from flow data.
#Table 3. (Outside R) Flow definitions for Quintina Roo study area in relation to GYR (Peninsula), Mexico (MEX), World (W). We only consider flows to and from QR in this study.

#Table 4. Total count  of flow transaction by type all years aggregated. Include flows by type AG, LIVE, MEAT, by trans units and % total of each type
#Table 5. Flow quantities by type all years aggregated. Include flows by type AG, LIVE, MEAT, by NVQUANT units and % total of each type

####### FIGURES

# Figure 1: (not in R) Study area, Yucatan and Quintinaro 
# Figure 2: (not in R) Flow type direction A, B and C (outside R)
# Figure 3: (not in R) Hinterland and origin/destination: Figure 3. Flows and distance used in generating the “Hinterland” varialbe. All flow transactions were summarized in four categories: 1) QR corresponding to flow internal to Quintina Roo, 2)  GYR corresponding to flow from or to Yucatan peninsula (excluding QR) , 3) MEX corresponding to flow from or to Mexico (excluding GYR and QR); W corresponding to flows from or to the rest of the world (excluding GYR and MEX. 
# Figure 4: (not in R) Flows and land use (this also shows A,B and C)
# Figure 5: Flows quantities and import, export and internal flows.
# Figure 6. Consumption dervied from flow using quantities for the 2001-2009 time period and three type of food products.
# Figure 7: Production derived from flow using quantities for the 2001-2009 time period and three type of food products.
# Figure 8: Decoupling by year  for Quintana Roo expressed by quantities of flows by distance (Hinterland variable).
# Figure 9: Decoupling of Quintana Roo expressed by total number of flows (top) and total quantities (bottom) by distance (Hinterland variable) for three type of products: agriculture, meat and livestock.

#Figure 10: Land consumption expressed by percentage of landscape converted for crop and cattle activities aggregated by year. 
#Figure 11: Flow conversions (A:inflow, B:outflows, C:internal flow) converted into land consumption expressed by percentage of landscape converted for crop and cattle activities aggregated by year.

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

format_column_for_table <- function(col_val,name_col=NULL){
  
  col_tot <- sum(col_val)
  col_prop <-  100*(col_val/col_tot)
  col_prop      <- format(round(col_prop, 2), nsmall = 2)
  element <-paste(col_val," (",col_prop,")",sep="")
  col_formatted <- c(element,col_tot)
  #col_formatted <- cbind(element,col_tot)
  #col_formatted <- cbind(element)
  
  if(is.null(name_col)){
    name_col <- "col"
  }
  df <- data.frame(name_col=col_formatted)
  names(df)<- name_col #make sure the name is assigned
  return(df)
}

#infile1_function <- file.path("/home/bparmentier/Google Drive/Papers_writing_MEOT/R_scripts/",
#                              "PCA_EOT_comparison_data_update_function_07152016.R")
#source(infile1_function)

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
out_suffix <-"flow_09162016" #output suffix for the files and ouptu folder #PARAM 7

create_out_dir_param=TRUE #PARAM8
num_cores <- 4 #PARAM 9

#inDir <- "/home/bparmentier/Google Drive/000_Flow_and_LUD_research/Quintana_Roo_Research/Data"
#inDir <- "/home/bparmentier/Google Drive/000_Flow_and_LUD_research/Quintana_Roo_Research/Data/output_flow_08302016/"
#inDir <- NULL #if NULL then generate input dir from out_suffix
#inDir <- file.path(inDir, paste0("output_flow_",out_suffix))
inDir <- "/home/bparmentier/Google Drive/000_Flow_and_LUD_research/Quintana_Roo_Research/Data/output_flow_09162016"
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

# Figure 1: (not in R) Study area, Yucatan and Quintinaro 
# Figure 2: (not in R) Flow type direction A, B and C (outside R)
# Figure 3: (not in R) Hinterland and origin/destination: Figure 3. Flows and distance used in generating the “Hinterland” varialbe. All flow transactions were summarized in four categories: 1) QR corresponding to flow internal to Quintina Roo, 2)  GYR corresponding to flow from or to Yucatan peninsula (excluding QR) , 3) MEX corresponding to flow from or to Mexico (excluding GYR and QR); W corresponding to flows from or to the rest of the world (excluding GYR and MEX. 
# Figure 4: (not in R) Flows and land use (this also shows A,B and C)

# Figure 5: Flows quantities and import, export and internal flows.
# Figure 6. Consumption dervied from flow using quantities for the 2001-2009 time period and three type of food products.
# Figure 7: Production derived from flow using quantities for the 2001-2009 time period and three type of food products.
# Figure 8: Decoupling by year  for Quintana Roo expressed by quantities of flows by distance (Hinterland variable).
# Figure 9: Decoupling of Quintana Roo expressed by total number of flows (top) and total quantities (bottom) by distance (Hinterland variable) for three type of products: agriculture, meat and livestock.

#Figure 10: Land consumption expressed by percentage of landscape converted for crop and cattle activities aggregated by year. 
#Figure 11: Flow conversions (A:inflow, B:outflows, C:internal flow) converted into land consumption expressed by percentage of landscape converted for crop and cattle activities aggregated by year.

#########
##### Figure 5: # Figure 5: Flows quantities and import, export and internal flows.

### Need to improve this code later on!!!

p1 <- xyplot(NV_CANT ~ year | flow_direction,subset(tb_summary1,tb_summary1$product_cat=="agri"),
             type="b",
             ylab="Tons",
             main="AGRI flows total by year ")

p2 <- xyplot(NV_CANT ~ year | flow_direction,subset(tb_summary1,tb_summary1$product_cat=="meat"),
             type="b",
             ylab="Tons",
             main="MEAT flows total by year ")

p3 <- xyplot(NV_CANT ~ year | flow_direction,subset(tb_summary1,tb_summary1$product_cat=="livestock"),
       type="b",
       #type="h",
       ylab="Head", 
       main="LIVESTOCK flows total by year ")

png_filename5a <- paste("Figure","_5a_","agri_flow_directions_",out_suffix,".png", sep="")
png_filename5b <- paste("Figure","_5b_","meat_flow_directions_",out_suffix,".png", sep="")
png_filename5c <- paste("Figure","_5c_","livestock_flow_directions_",out_suffix,".png", sep="")

layout_m <- c(1.5,1)

png(png_filename5a,
    height=480*layout_m[2],width=480*layout_m[1])
print(p1)
dev.off()

png(png_filename5b,
    height=480*layout_m[2],width=480*layout_m[1])
print(p2)
dev.off()

png(png_filename5c,
    height=480*layout_m[2],width=480*layout_m[1])
print(p3)
dev.off()

#############################
##### Figure 6: consumption A+C
#Figure 6. Consumption dervied from flow using quantities for the 2001-2009 time period and three type of food products.
#slides 8,9,10

tb_summary3$consumption_val <- as.character(tb_summary3$consumption)

tb_summary3$consumption_cat <- revalue(tb_summary3$consumption_val,
                                 c("0"  = "outside", 
                                   "1" = "inside")) 

## Local Consumption is A+C (defined as comsumption of from import and locally produced food)
#this is external produciton of food which is imported (A) or consumed locally (C)
# B is outflow (outside)
# A+C is 

p7 <- xyplot(NV_CANT ~ year | consumption_cat,subset(tb_summary3,tb_summary3$product_cat=="agri"),
             type="b",
             ylab="Tons",
             main="AGRI flow consumption total by year ")

p8 <- xyplot(NV_CANT ~ year | consumption_cat,subset(tb_summary3,tb_summary3$product_cat=="meat"),
             type="b",
             ylab="Tons", 
             main="MEAT flow consumption total by year ")

p9 <- xyplot(NV_CANT ~ year | consumption_cat,subset(tb_summary3,tb_summary3$product_cat=="livestock"),
             type="b",
             ylab="Head", 
             main="LIVESTOCK flow consumption total by year ")

png_filename6a <- paste("Figure","_6a_","agri_consumption_cat_",out_suffix,".png", sep="")
png_filename6b <- paste("Figure","_6b_","meat_consumption_cat_",out_suffix,".png", sep="")
png_filename6c <- paste("Figure","_6c_","livestock_consumption_cat_",out_suffix,".png", sep="")

layout_m <- c(1.5,1)

png(png_filename6a,
    height=480*layout_m[2],width=480*layout_m[1])
print(p7)
dev.off()

png(png_filename6b,
    height=480*layout_m[2],width=480*layout_m[1])
print(p8)
dev.off()

png(png_filename6c,
    height=480*layout_m[2],width=480*layout_m[1])
print(p9)
dev.off()

#############################
##### Figure 7: Production B+C
# Figure 7: Production derived from flow using quantities for the 2001-2009 time period and three type of food products.

#slide 8,9,10

tb_summary2$extraction_val <- as.character(tb_summary2$extraction)

tb_summary2$extraction_cat <- revalue(tb_summary2$extraction_val,
                            c("0"  = "outside", 
                              "1" = "inside")) 

p4 <- xyplot(NV_CANT ~ year | extraction_cat,subset(tb_summary2,tb_summary2$product_cat=="agri"),
             type="b",
             ylab="Tons",
             main="AGRI flow production total by year ")

p5 <- xyplot(NV_CANT ~ year | extraction_cat,subset(tb_summary2,tb_summary2$product_cat=="meat"),
             type="b",
             ylab="Tons", 
             main="MEAT flow production total by year ")

p6 <- xyplot(NV_CANT ~ year | extraction_cat,subset(tb_summary2,tb_summary2$product_cat=="livestock"),
       type="b",
       ylab="Head", 
       main="LIVESTOCK flow production total by year ")

png_filename7a <- paste("Figure","_7a_","agri_production_cat_",out_suffix,".png", sep="")
png_filename7b <- paste("Figure","_7b_","meat_production_cat_",out_suffix,".png", sep="")
png_filename7c <- paste("Figure","_7c_","livestock_production_cat_",out_suffix,".png", sep="")

layout_m <- c(1.5,1)

png(png_filename7a,
    height=480*layout_m[2],width=480*layout_m[1])
print(p4)
dev.off()

png(png_filename7b,
    height=480*layout_m[2],width=480*layout_m[1])
print(p5)
dev.off()

png(png_filename7c,
    height=480*layout_m[2],width=480*layout_m[1])
print(p6)
dev.off()


#############################
##### Figure 8: Decoupling from distance flow and years
# Figure 8: Decoupling by year  for Quintana Roo expressed by quantities of flows by distance (Hinterland variable).

#slide 17,18,19

png_filename8a <- paste("Figure","_8a_","agri_hinterland_cat_sum_",out_suffix,".png", sep="")
png_filename8b <- paste("Figure","_8b_","meat_hinterland_cat_sum_",out_suffix,".png", sep="")
png_filename8c <- paste("Figure","_8c_","livestock_hinterland_cat_sum_",out_suffix,".png", sep="")

hinterland_tb  <- read.table(file.path(inDir,hinterland_by_product_sum_filename),header=T,sep=",") 
hinterland_year_tb  <- read.table(file.path(inDir,hinterland_year_by_product_sum_filename),header=T,sep=",")

hinterland_year_tb$flow_dist_cat_val <- as.character(hinterland_year_tb$flow_dist_cat)

hinterland_year_tb$flow_dist_hint <- revalue(hinterland_year_tb$flow_dist_cat_val,
                         c("0" = "QR", 
                           "1" = "GYR",
                           "2" = "MEX", 
                           "3" = "W")) 
hinterland_year_tb$labels <- paste(hinterland_year_tb$year,hinterland_year_tb$flow_dist_hint)
  
layout_m <- c(1.5,1)

#### Agri hinterland

#dates_val <- hinterland_year_tb$labels

hint_tmp <- subset(hinterland_year_tb,hinterland_year_tb$product_cat=="agri")
x_labels <- hint_tmp$labels

png(png_filename8a,
    height=480*layout_m[2],width=480*layout_m[1])

barplot(hint_tmp$NV_CANT,names.arg=x_labels,las=2,
        main="AGRI flows total quantities by hinterland category")

dev.off()


#### Meat hinterland

#dates_val <- c(sort(rep(2001:2008,3)),rep(2009,4))
#x_labels <- paste(dates_val,c(rep(c("QR","GYR","MEX"),8),c("QR","GYR","MEX","W")),sep=" ")
hint_tmp <- subset(hinterland_year_tb,hinterland_year_tb$product_cat=="meat")
x_labels <- hint_tmp$labels

png(png_filename8b,
    height=480*layout_m[2],width=480*layout_m[1])

barplot(hint_tmp$NV_CANT,names.arg=x_labels,las=2,
        main="MEAT flows total quantities by hinterland category")
dev.off()

#### Livestock hinterland

#dates_val <- sort(rep(2001:2009,3))
#x_labels <- paste(dates_val,rep(c("QR","GYR","MEX"),9),sep=" ")
hint_tmp <- subset(hinterland_year_tb,hinterland_year_tb$product_cat=="livestock")
x_labels <- hint_tmp$labels

png(png_filename8c,
    height=480*layout_m[2],width=480*layout_m[1])
barplot(hint_tmp$NV_CANT,names.arg=x_labels,las=2,
        main="LIVESTOCK flows total quantities by hinterland category")

dev.off()

######### Figure 9: decoupling: aggregated flows by total numbers and quantities for three products
# Figure 9: Decoupling of Quintana Roo expressed by total number of flows (top) and total quantities (bottom) by distance (Hinterland variable) for three type of products: agriculture, meat and livestock.


######### Figure 10:

#tb_land_agri$total_land_consumed <- total_land_consumed_qr

tb_land_summarized_agri_filename <- paste("tb_land_summarized_","agri","_by_product_year",out_suffix,".txt",sep="")
tb_land_summarized2_A_B_C_agri_filename <- paste("tb_land_summarized2_","agri","_by_flow_A_B_C_year",out_suffix,".txt",sep="")
tb_land_agri_filename <- paste("tb_land_","agri", out_suffix,".txt",sep="")


#### Writing the tables
tb_land_summarized_livestock_filename <- paste("tb_land_summarized_","livestock","_by_product_year",out_suffix,".txt",sep="")
tb_land_summarized2_A_B_C_livestock_filename <- paste("tb_land_summarized2_","livestock","_by_flow_A_B_C_year",out_suffix,".txt",sep="")
tb_land_livestock_filename <- paste("tb_land_","livestock", out_suffix,".txt",sep="")

png_filename10a <- paste("Figure","_10a_","agri_total_percent_land_consumption_by_year",out_suffix,".png", sep="")
png_filename10c <- paste("Figure","_10c_","livestock_total_percent_land_consumption_by_year",out_suffix,".png", sep="")

layout_m <- c(1.5,1)

png(png_filename10a,
    height=480*layout_m[2],width=480*layout_m[1])

plot(percent_land_consumption ~ year,data=tb_land_summarized_agri ,type="b",
     ylab="% of land in QR",
     main="AGRI land consumption as percentage of land")

dev.off()

png(png_filename10c,
    height=480*layout_m[2],width=480*layout_m[1])

plot(percent_land_consumption ~year,data=tb_land_summarized_livestock,type="b",
     ylab="% of land in QR",
     main="LIVESTOCK land consumption as percentage of land")

dev.off()

######### Figure 11:

#Figure 11. Flow conversions (A:inflow, B:outflows, C=internal flow) converted into land consumption expressed by percentage of landscape converted for crop and cattle activities aggregated by year.

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

png_filename11a <- paste("Figure","_11a_","agri_total_percent_land_consumption_A_B_C_by_year",out_suffix,".png", sep="")
#png_filename7b <- paste("Figure","_7b_","meat_production_cat_",out_suffix,".png", sep="")
png_filename11c <- paste("Figure","_11c_","livestock_total_percent_land_consumption_A_B_C_by_year",out_suffix,".png", sep="")

layout_m <- c(1.5,1)

png(png_filename11a,
    height=480*layout_m[2],width=480*layout_m[1])
print(p10)
dev.off()

png(png_filename11c,
    height=480*layout_m[2],width=480*layout_m[1])
print(p11)
dev.off()

#Figure 12: combine agri and livestock

tb_land_summarized_livestock$percent_land_consumption
tb_land_summarized_agri$percent_land_consumption

tb_land_summarized_combined <- subset(tb_land_summarized_livestock,select=c("year","percent_land_consumption"))
names(tb_land_summarized_combined) <- c("year","percent_land_consumption_livestock")
tb_land_summarized_combined$percent_land_consumption_agri <- tb_land_summarized_agri$percent_land_consumption
tb_land_summarized_combined$percent_land_consumption_total <- tb_land_summarized_combined$percent_land_consumption_agri + tb_land_summarized_combined$percent_land_consumption_livestock

#plot(tb_land_summarized_combined$percent_land_consumption_total ~ tb_land_summarized_combined$year)

out_filename <- file.path(inDir,paste("tb_land_consumption_summarized_",out_suffix,".txt",sep=""))
write.table(tb_land_summarized_combined,file= out_filename,sep=",")

png_filename12 <- paste("Figure","_12_","combined_total_percent_land_consumption_year",out_suffix,".png", sep="")

png(png_filename12,
    height=480*layout_m[2],width=480*layout_m[1])

plot(percent_land_consumption_total ~ year,data=tb_land_summarized_combined ,type="b",
     ylim=c(0,250),
     ylab="% of land in QR",
     main="Total land consumption as percentage of land")

abline(h=100,col=4,lty=2)

dev.off()

################## END OF SCRIPT #####################




