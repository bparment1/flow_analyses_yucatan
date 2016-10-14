#################################    FLOW RESEARCH  #######################################
########################### SPACE-TIME QUINTINARO PRODUCT DATABASE  #############################################
#This script collects functions to generate figures and tables from the processed Quintinaro database of flow (food, wood etc) analyzed previoulsy
#Data was collected by Marco Millones.
#
#AUTHOR: Benoit Parmentier                                                                       
#DATE CREATED:10/14/2016 
#DATE MODIFIED: 10/14/2016
#
#PROJECT: Flow and land cover change in QR and GYR with Marco Millones
#COMMIT: generating figure 9 related to hinterland in the production script

## Code used in the current workflow:
#flow_data_analyses_10132016.R : this generates cleaned table of flows and data table used in analyses and figures
#flow_data_analyses_production_of_tables_figures_10132016.R: figure and table creation
#flow_data_analyses_function_09162016.R: function script used in analyses and figures
#flow_data_analyses_production_of_tables_figures_functions_10142016.R: functions used for figure production
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


## Function to generate barplot for flows
generate_barplot_flows <- function(df_tb,x_labels,col_palette,layout_m,product_cat_val,title_str,out_filename){
  #
  #
  
  hint_tmp <- subset(hinterland_year_tb,df_tb$product_cat==product_cat_val)
  x_labels <- hint_tmp$labels
  l_df <- lapply(unique(hint_tmp$flow_dist_hint),FUN=function(x){subset(hint_tmp,hint_tmp$flow_dist_hint==x)})
  l_heights <- lapply(l_df, FUN=function(x){x$NV_CANT})
  heights <- do.call(cbind , l_heights)
  heights <-as.matrix(t(heights))
  
  png(out_filename,
      height=480*layout_m[2],width=480*layout_m[1])
  
  barplot(heights,names.arg=x_labels,las=2,
          main= title_str,
          #names.arg=names_ind,
          cex.names=1.1,   
          col=col_palette, 
          beside=TRUE) # see two barplots for comparisons...
  
  dev.off()
  
  return(out_filename)
}

################## END OF SCRIPT #####################




