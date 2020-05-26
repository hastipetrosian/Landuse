###
library(tidyverse)
library(raster)
library(rasterVis) ## for levelplot
library(purrr)
library(cowplot)
library(lattice)
library(sp)

source("functions.R")

years <- c(1987,1997,2003,2008,2014,2018)

##change name of included map
at <- sprintf("Average_temperature/%dAT.tif",years)
pr <- sprintf("precipitation/%dPR.tif",years)
ws <- sprintf("Wind/%dW.tif",years) 

##make raster layer from other kinds of classes
AT_list <- map(at, raster)
PR_list <- map(pr, ~ { cat(.,"\n"); raster(.) })
WS_list <- map(ws, ~ { cat(.,"\n"); raster(.) })

##make plot
ATplots <- map(AT_list, levelplot, margin=FALSE)
PRplots <- map(PR_list, levelplot, margin=FALSE)
WSplots <- map(WS_list, levelplot, margin=FALSE)

#PLOT_GRID:all plots together
plot_grid(plotlist=ATplots)
plot_grid(plotlist=PRplots)
plot_grid(plotlist=WSplots)

##1997-1987=+ 2003-1197=- 2008-2003=+ 2014-2008=-
pr_before=PR_list[1:5]
pr_after=PR_list[2:6]
differ=function(x,y){differ=x-y}
## system.time(pr_changes <- map2(pr_before, pr_after, ~ overlay(.x,.y,fun=diff)))
## try faster version
pr_changes <- map2(pr_before, pr_after, ~.y-.x)
pr_change_tbl <- map(pr_changes, conv_tbl, newname="precipchange")

at_before=AT_list[1:5]
at_after=AT_list[2:6]
at_changes <- map2(at_before, at_after, ~ .y - .x)
at_change_tbl <- map(at_changes, conv_tbl, newname="averagetemchange")

ws_before=WS_list[1:5]
ws_after=WS_list[2:6]
ws_changes=map2(ws_before, ws_after, ~ .y - .x)
ws_change_tbl <- map(ws_changes, conv_tbl, newname="windchange")

save("ws_change_tbl","at_change_tbl","pr_change_tbl", file="climate.RData")
## save.image("climate.RData")
