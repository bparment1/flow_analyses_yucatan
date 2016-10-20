#################################    FLOW RESEARCH  #######################################
########################### SPACE-TIME QUINTINARO PRODUCT DATABASE  #############################################
#This script contains function used in analyzes data from the Quintinaro database of flow (food, wood etc).
#Data was collected by Marco Millones
#
#AUTHOR: Benoit Parmentier                                                                       
#DATE CREATED: 09/15/2016 
#DATE MODIFIED: 10/20/2016
#
#PROJECT: Flow, land cover change with Marco Millones
#COMMIT: adding convert_to_land function for all type of flows
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
library(reshape2)

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


convert_product_to_land_unit <- function(conversion_rate_val,nom_product,col_product_name,tb_products){
  #conversion_rate_val: numeric value to translate value to ha
  #nom_product: 
  #col_product_name: name of the column matching the coding or naming convention
  #tb_products: selected product
  #
  
  #First match product:
  #tb_to_convert <- subset(tb_products,tb_products$NOMPRODUCT== nom_product)
  tb_to_convert <- subset(tb_products,tb_products[[col_product_name]]== nom_product) # select relevant product
  tb_to_convert$land_consumption <- tb_to_convert$NV_CANT*conversion_rate_val # convert into land

  #tb_summarized$percent_land_consumption1 <- (tb_summarized$land_consumption1/total_land_consumed_qr)*100 
  #tb_summarized <- aggregate(NV_CANT ~ year + product_cat , data = tb_to_convert, sum)
  return(tb_to_convert)
}

apply_conversion_rate <- function(i,tb_conversion_rate,tb_products,col_product_name){
  #conversion_rate_val: numeric value to translate value to ha
  #col_product_name: name of the column matching the coding or naming convention
  #tb_products: selected product
  #
  
  #debug(convert_product_to_land_unit)
  tb_to_convert <- convert_product_to_land_unit(conversion_rate_val=tb_conversion_rate$conversion_rate[i],
                               #tb_conversion_rate$NOMPRODUCT[i],
                               nom_product=tb_conversion_rate[[col_product_name]][i],
                               col_product_name = col_product_name,
                               tb_products = tb_products)
  return(tb_to_convert)
  
}

convert_to_land <- function(filename_conversion_rate,tb,flow_type,out_suffix,out_dir){
  #
  #This function convert flow to land unit given conversion rate.
  #
  #INPUTS:
  #filename_conversion_rate
  #tb
  #flow
  #out_suffix
  #out_dir
  #OUTPUTS
  #
  
  ### Start script ####
  
  options(scipen=999)
  
  #filename_conversion_rate_meat <- file.path(conversion_rate_dir,"Meat_conversion_10182016.csv")
  
  tb_conversion_rate <- read.table(filename_conversion_rate,
                                   header=T,stringsAsFactors = F,sep=",")
  #tb_conversion_rate$NOMPRODUCT
  #names(tb_conversion_rate)
  
  #select relevant product
  #selected_nom_product <- tb_conversion_rate$NOMPRODUCT
  selected_nom_product <- tb_conversion_rate$CODPROD
  tb_to_convert <- subset(tb , tb$CODPROD %in% selected_nom_product)
  #table(tb_to_convert$NV_UMEDIDA) #OK all TONELADA
  #barplot(table(tb_to_convert$CODPROD),names.arg=names(table(tb_to_convert$CODPROD)),las=2)
  
  names(tb_conversion_rate) <- c("CODPROD","NOMPRODUCT","conversion_rate")
  
  #debug(apply_conversion_rate)
  #list_converted_tb_products <- lapply(1:1,
  #                                     FUN=apply_conversion_rate,
  #                                     tb_conversion_rate=tb_conversion_rate_crop,
  #                                     tb_products=tb_to_convert,
  #                                     col_product_name="CODPROD")
  
  list_converted_tb_products <- lapply(1:nrow(tb_conversion_rate),
                                       FUN=apply_conversion_rate,
                                       tb_conversion_rate=tb_conversion_rate,
                                       tb_products=tb_to_convert,
                                       col_product_name="CODPROD")
  
  sum(unlist(lapply(list_converted_tb_products,FUN=nrow)))
  
  tb_land <- do.call(rbind,list_converted_tb_products)
  dim(tb_land)
  
  ### Get data for A and B etc...?
  
  #tb_summarized <- aggregate(land_consumption ~ year + product_cat , data = tb_land_crops , sum)
  tb_land$percent_land_consumption <- (tb_land$land_consumption/total_land_consumed_qr)*100
  
  ### table 2
  tb_land_summarized <- aggregate(land_consumption ~ year + product_cat , data = tb_land, sum)
  tb_land_summarized$total_land_consumed <- total_land_consumed_qr
  tb_land_summarized$percent_land_consumption <- (tb_land_summarized$land_consumption/total_land_consumed_qr)*100
  
  tb_land_summarized2 <- aggregate(land_consumption ~ year + NOMPRODUCT , data = tb_land , sum)
  tb_land_summarized2$percent_land_consumption <- (tb_land_summarized2$land_consumption/total_land_consumed_qr)*100
  tb_land_summarized2$total_land_consumed <- total_land_consumed_qr
  
  #### For the results and figures
  
  plot(tb_land_summarized$percent_land_consumption ~ year,
       data=tb_land_summarized,type="b",
       ylab="% of land",
       main=paste(flow_type, "land consumption as percentage of land",sep=" "))
  
  
  #### Writing the tables
  tb_land$total_land_consumed <- total_land_consumed_qr
  
  #flow_type can be meat, agri, livestock
  tb_land_summarized_filename <- paste("tb_land_summarized_",flow_type,"_by_product_year",out_suffix,".txt",sep="")
  tb_land_summarized2_A_B_C_filename <- paste("tb_land_summarized2_",flow_type,"_by_flow_A_B_C_year",out_suffix,".txt",sep="")
  tb_land_filename <- paste("tb_land_",flow_type, out_suffix,".txt",sep="")
  
  write.table(tb_land_summarized,file= file.path(outDir,tb_land_summarized_filename) ,sep=",")
  write.table(tb_land_summarized2,file=file.path(outDir,tb_land_summarized2_A_B_C_filename),sep=",")
  write.table(tb_land,file=file.path(outDir,tb_land_filename),sep=",")
  
  #### Prepare return object
  
  convert_to_land_obj <- list(tb_land_summarized_filename, 
                              tb_land_summarized2_A_B_C_filename,
                              tb_land_filename) 
  names(convert_to_land_obj) <- c("tb_land_summarized_filename", 
                                  "tb_land_summarized2_A_B_C_filename",
                                  "tb_land_filename")
  
  return(convert_to_land_obj)
  
}

#################################  END OF FILE ###########################################
