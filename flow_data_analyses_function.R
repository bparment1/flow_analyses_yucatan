#################################    FLOW RESEARCH  #######################################
########################### SPACE-TIME QUINTINARO PRODUCT DATABASE  #############################################
#This script contains function used in analyzes data from the Quintinaro database of flow (food, wood etc).
#Data was collected by Marco Millones
#
#AUTHOR: Benoit Parmentier                                                                       
#DATE CREATED: 09/15/2016 
#DATE MODIFIED: 10/19/2016
#
#PROJECT: Flow, land cover change with Marco Millones
#COMMIT: editing conversion rate function to allow code name
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

#################################  END OF FILE ###########################################
