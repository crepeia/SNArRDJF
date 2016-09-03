# SNA Reciprocity Distance Path 24_CONFIANÇA G) Está satisfeito com as ações desempenhadas por este serviço (var22).
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 24_CONFIANÇA G) Está satisfeito com as ações desempenhadas por este serviço (var22).

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_var22.RData")
```
##Reload packages

```r
suppressMessages(library(RColorBrewer))
suppressMessages(library(car))
suppressMessages(library(xtable))
suppressMessages(library(igraph))
suppressMessages(library(miniCRAN))
suppressMessages(library(magrittr))
suppressMessages(library(keyplayer))
suppressMessages(library(dplyr))
suppressMessages(library(feather))
suppressMessages(library(visNetwork))
suppressMessages(library(knitr))
suppressMessages(library(DT))
```
##Adding phantom tools

```r
#In order to get dinamic javascript object install those ones. If you get problems installing go to Stackoverflow.com and type your error to discover what to do. In some cases the libraries need to be intalled in outside R libs.
#devtools::install_github("wch/webshot")
#webshot::install_phantomjs()
```
##Setting a random seed - this is a good strategy to keep the same graph pattern layout in a new report generation

```r
set.seed(123)
```

##Simplify Graph - removing loops and duble edges 

```r
#var22<-simplify(var22) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var22, directed=T, unconnected = T)
```

```
## [1] 2.512914
```
##Shortest Paths

```r
#Shortest Paths
var22_sp_in <- shortest.paths(var22, mode='in', weights=E(var22)$var22) #in

var22_sp_out <- shortest.paths(var22, mode='out', weights=E(var22)$var22) # out

var22_sp_all <- shortest.paths(var22, mode='all', weights=E(var22)$var22) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var22_sp_in[which(var22_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   4.000   5.000   5.273   6.000  14.000
```

```r
sd(var22_sp_in[which(var22_sp_in != Inf)])
```

```
## [1] 2.124353
```
##Descriptive  Shortest Paths - OUT

```r
summary(var22_sp_out[which(var22_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   4.000   5.000   5.273   6.000  14.000
```

```r
sd(var22_sp_out[which(var22_sp_out != Inf)])
```

```
## [1] 2.124353
```

##Descriptive  Shortest Paths - ALL

```r
summary(var22_sp_all[which(var22_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   3.000   4.000   4.313   6.000  12.000
```

```r
sd(var22_sp_all[which(var22_sp_all != Inf)])
```

```
## [1] 1.718695
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var22<-distances(var22, mode="all", weights=E(var22)$var22)
#distances_sp_all_var22

distances_dist_all_var22[distances_dist_all_var22=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var22_vec <- vector()
for (i in 1:vcount(var22)) {
    distances_sp_all_var22_vec[i] <- 
    mean(distances_dist_all_var22[i,],na.rm=T)
}
#Adding to igraph object
V(var22)$sp_all<-distances_sp_all_var22_vec
```

#In shortest paths 

```r
distances_dist_in_var22<-distances(var22, mode="in",weights=E(var22)$var22)
#distances_sp_in_var22

distances_dist_in_var22[distances_dist_in_var22=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var22_vec <- vector()
for (i in 1:vcount(var22)) {
    distances_sp_in_var22_vec[i] <- mean(distances_dist_in_var22[i,], na.rm=T)
}

#Adding to igraph object
V(var22)$sp_in<-distances_sp_in_var22_vec
```

#Out shortest paths 

```r
distances_dist_out_var22<-distances(var22, mode="out", weights=E(var22)$var22)

distances_dist_out_var22[distances_dist_out_var22=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var22_vec <- vector()
for (i in 1:vcount(var22)) {
    distances_sp_out_var22_vec[i] <- 
    mean(distances_dist_out_var22[i,], na.rm = T)
}

#Adding to igraph object
V(var22)$sp_out<-distances_sp_out_var22_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var22_shortpath_df <- data.frame(distances_sp_in_var22_vec, distances_sp_out_var22_vec, distances_sp_all_var22_vec) %>% round(3)

#Adding type
var22_shortpath_df <-cbind(var22_shortpath_df, V(var22)$LABEL_COR)

#Adding names
names(var22_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var22_shortpath_df<-var22_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var22_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-ea5852317ad1dae98a03" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-ea5852317ad1dae98a03">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"10.207\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"8.962\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.54\" data-max=\"8.503\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[3.121,4.034,2.597,4.477,4.215,3.926,4.812,3.349,4.255,3.517,3.819,4.161,5.148,5.101,4.644,4.423,4.221,3.584,5.289,4.671,3.966,4.188,4.121,9.074,8.174,4.53,6.208,4.215,4.477,4.342,4.235,4.517,4.134,7.329,4.477,4.168,3.443,5.812,5.886,4.141,3.899,4.919,4.859,3.322,4.456,4.268,9.302,5.034,5.034,5.034,5.034,5.034,5.188,4.342,7.275,4.463,4.349,7.154,6.564,3.671,4.273,8.02,4.503,4.517,4.215,0,10.207,4.436,5.423,4.758,4.094,4.436,4.497,4.463,3.987,4.49,4.443,3.725,8.087,8.087,8.087,8.087,4.564,6.114,5.188,4.081,3.913,7.04,4.611,4.691,4.685,8.054,7.06,5.101,4.369,4.221,7.04,4.282,4.054,4.302,7.973,5.443,5.887,5.887,4.523,4.584,4.497,5.887,5.473,4.416,4.45,4.034,4.383,4.074,4.081,4.564,4.591,4.584,4.557,4.389,4.416,4.188,4.483,4.537,4.483,4.51,4.53,4.517,4.503,4.732,4.564,4.523,4.503,4.53,4.483,4.034,4.262,4.463,4.369,4.658,4.577,4.45,4.678,4.671,5.53,4.007,4.711,4.47,6.081,4.799,4.564,4.799,4.564,4.799,4.275,7.839,4.767,4.54,4.56,7.04,8.18,6.88,6.781,7.278,9.313,10.159,6.553,9.313,9.313,4.987,7.18,9.313,5.387,6.253,6.973,6.613,5.353,9.313,5.353,7.24,7.247,7.247,5.54,7.133,7.273,7.167,5.56],[3.522,4.247,3.661,4.118,4.597,6.016,5.672,3.414,4.806,5.844,4.511,5.457,3.919,5.242,4.161,5.387,4.032,4.435,5.387,4.065,4.457,5.011,4.839,5.145,4.285,7.199,5.371,5.301,4.409,3.898,4.28,5.36,4.07,8.962,7.925,4.269,5.108,5.032,6.183,4.022,4.489,4.392,4.398,4.828,5.887,3.93,5.855,5.366,5.366,5.366,5.366,5.366,6.059,5.29,4.554,5,4.527,4.602,6.919,4.978,0,8.102,3.914,4.349,3.704,1.5,2,3.726,5.5,4.618,4.629,6.21,3.898,4.081,4.344,4.387,3.952,4.543,7.36,7.36,7.36,7.36,5.995,8.113,4.441,4.086,5.876,4.452,5.683,6.317,4.016,4.661,5.258,6.849,5.075,4.484,7.419,7.215,5.527,4.516,6.93,5.349,0,0,4.005,5.323,6.505,0,0,4.027,3.871,5.242,4.941,4.043,4.5,3.957,3.839,4.038,3.946,3.946,4.903,4.011,5.624,5.624,5.624,6.183,6.629,5.409,6.629,6.183,5.312,5.312,5.237,4.64,4.64,5.516,6.57,6.629,5.634,4.312,6.43,6.629,6.484,4.64,6.629,6.629,5.306,8.586,6.199,6.226,6.226,6.226,6.226,6.226,7.274,6.258,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[2.674,3.273,2.54,3.203,3.61,3.647,3.92,2.909,3.829,3.481,3.235,3.754,3.578,4.636,3.342,3.979,3.465,3.299,4.059,3.476,3.492,3.866,3.529,4.599,3.497,4.417,4.465,3.717,3.818,3.166,3.278,3.963,3.214,6.545,4.305,3.374,3.262,4.091,5.07,3.439,3.54,3.936,3.727,3.176,4.364,3.096,4.888,3.802,3.802,3.802,3.802,3.802,4.214,3.882,3.85,3.898,3.882,3.513,5.513,3.39,4.299,7,3.112,3.305,3.23,8.503,6.786,3.134,4.102,3.471,3.321,4.385,3.187,3.225,3.23,3.374,3.193,3.503,6.337,6.337,6.337,6.337,4.481,5.273,3.417,3.369,3.588,3.636,4.257,4.508,3.257,4.209,4.332,4.679,4.31,3.348,5.551,3.882,3.856,3.775,6,4.668,5.497,5.497,3.203,4.011,4.005,5.497,5.166,3.171,3.209,3.866,3.872,3.182,3.257,3.193,3.102,3.225,3.182,3.182,3.829,3.209,4.198,4.225,4.198,4.396,4.487,4.016,4.428,4.396,4.23,4.219,4.155,3.46,3.476,3.829,3.925,4.075,3.92,3.342,4.487,4.15,4.535,3.481,5.487,3.749,4.144,4.144,5.112,4.556,4.492,4.556,4.492,4.556,4.048,4.909,4.551,4.487,4.492,5.337,6.348,5.679,5.535,5.834,6.759,6.802,5.203,6.759,6.759,4.214,6.519,6.759,4.086,4.936,6.171,5.652,4.273,6.759,4.273,5.77,6.283,6.283,5.144,5.588,6.358,6.198,5.508]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var22_shortpath_df, by=list(var22_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var22_shortpath_df, by=list(var22_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

names(aggdata_sd) <- c("Group","Type", "Short Path IN(SD)", "Short Path OUT(SD)","Short Path ALL(SD)")

#Removing Type variable
aggdata_sd<-aggdata_sd[,-c(2)]
```
##Merging mean and standart deviation

```r
total_table <- merge(aggdata_mean,aggdata_sd,by="Group")

#Rounding
Group<-total_table[,c(1)] #Keeping group
total_table<-total_table[,-c(1)] %>% round(3) #Rouding
total_table<-cbind(Group,total_table) #Binding toghter

#Organizing Variabels
total_table<-total_table[,c("Group","Short Path IN(M)","Short Path IN(SD)","Short Path OUT(M)","Short Path OUT(SD)","Short Path ALL(M)","Short Path ALL(SD)")]
```
##Final table with round - Short Path

```r
datatable(total_table, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-51f8924c5323f88a5959" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-51f8924c5323f88a5959">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.597\" data-max=\"7.22\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.042\" data-max=\"1.807\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.409\" data-max=\"5.306\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.004\" data-max=\"3.214\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.54\" data-max=\"5.486\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.13\" data-max=\"1.42\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[4.705,7.22,4.034,4.463,4.077,2.597,4.889,4.216,4.604,5.433,5.369,4.48,3.584],[1.198,1.52,null,0.84,0.113,null,0.042,0.253,1.807,1.403,0.499,0.194,null],[4.919,2.409,4.247,4.229,5.306,3.661,4.395,4.824,5.079,5.185,3.619,4.972,4.435],[1.128,3.214,null,2.16,0.947,null,0.004,1.116,1.524,1.071,2.686,1.481,null],[3.955,5.486,3.273,3.917,3.622,2.54,3.832,3.537,4.179,4.191,4.484,3.856,3.299],[1.015,1.026,null,0.926,0.13,null,0.148,0.35,1.42,0.915,0.776,0.514,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var22, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-d3e680bee83fd2c44265" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-d3e680bee83fd2c44265">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["7214","1555","14084","8470","3153","305","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

var22_dist.from.CAPSAD <- distances(var22, v=V(var22)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(var22), weights=E(var22)$var22)

#Saving distance on igraph object 
V(var22)$var22_dist.from.CAPSAD<-var22_dist.from.CAPSAD

var22_dist.from.CAPSAD[var22_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(var22_dist.from.CAPSAD)+1)
col <- col[var22_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(var22)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(var22, es=E(var22), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var22))
maxC <- rep(Inf, vcount(var22))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var22, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var22)$var22)

#Plotting distance from CAPSAD 
plot(var22, 
     layout=co,
     edge.color=V(var22)$col[edge.start],
     edge.arrow.size=(betweenness(var22, weights = E(var22)$var22)+1)/100000,
     edge.width=E(var22)$var22/10*mean(E(var22)$var22),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(var22))*5,
     vertex.label=var22_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(var22))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var22)$var22_dist.from.CAPSAD
b<-V(var22)$col
c<-table(a,b)
d<-as.data.frame(c)
e<-subset(d, d$Freq>0)
f<-t(e$a)
g<-t(e$b)

#Adding Legend
legend(x=range(co[,1])[2], 
       y=range(co[,2])[2],
       legend=as.character(f),
       pch=21,
       col = "#777777", 
       pt.bg=as.character(g),
       pt.cex=2,
       bty="n", 
       ncol=1,
       lty=1,
       cex = .3)

#Adding Title
  title("Distance from CAPS AD - 24_CONFIANÇA G) Está satisfeito com as ações desempenhadas por este serviço (var22).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(var22, directed=T, unconnected = T)
     )
             )
```

![](24_CONFIANÇA_G_Está_satisfeito_com_as_ações_desempenhadas_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_var22.RData") 
```

