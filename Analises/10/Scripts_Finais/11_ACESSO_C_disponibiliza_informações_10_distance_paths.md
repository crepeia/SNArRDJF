# SNA Reciprocity Distance Path 11_ACESSO C) Este serviço disponibiliza informações sobre suas ativades para o seu serviço. (var9)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 11_ACESSO C) Este serviço disponibiliza informações sobre suas ativades para o seu serviço. (var9)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_var9.RData")
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
#var9<-simplify(var9) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var9, directed=T, unconnected = T)
```

```
## [1] 2.512914
```
##Shortest Paths

```r
#Shortest Paths
var9_sp_in <- shortest.paths(var9, mode='in', weights=E(var9)$var9) #in

var9_sp_out <- shortest.paths(var9, mode='out', weights=E(var9)$var9) # out

var9_sp_all <- shortest.paths(var9, mode='all', weights=E(var9)$var9) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var9_sp_in[which(var9_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   4.000   5.000   5.451   7.000  14.000
```

```r
sd(var9_sp_in[which(var9_sp_in != Inf)])
```

```
## [1] 2.195687
```
##Descriptive  Shortest Paths - OUT

```r
summary(var9_sp_out[which(var9_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   4.000   5.000   5.451   7.000  14.000
```

```r
sd(var9_sp_out[which(var9_sp_out != Inf)])
```

```
## [1] 2.195687
```

##Descriptive  Shortest Paths - ALL

```r
summary(var9_sp_all[which(var9_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   3.000   4.000   4.177   5.000  10.000
```

```r
sd(var9_sp_all[which(var9_sp_all != Inf)])
```

```
## [1] 1.587092
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var9<-distances(var9, mode="all", weights=E(var9)$var9)
#distances_sp_all_var9

distances_dist_all_var9[distances_dist_all_var9=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var9_vec <- vector()
for (i in 1:vcount(var9)) {
    distances_sp_all_var9_vec[i] <- 
    mean(distances_dist_all_var9[i,],na.rm=T)
}
#Adding to igraph object
V(var9)$sp_all<-distances_sp_all_var9_vec
```

#In shortest paths 

```r
distances_dist_in_var9<-distances(var9, mode="in",weights=E(var9)$var9)
#distances_sp_in_var9

distances_dist_in_var9[distances_dist_in_var9=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var9_vec <- vector()
for (i in 1:vcount(var9)) {
    distances_sp_in_var9_vec[i] <- mean(distances_dist_in_var9[i,], na.rm=T)
}

#Adding to igraph object
V(var9)$sp_in<-distances_sp_in_var9_vec
```

#Out shortest paths 

```r
distances_dist_out_var9<-distances(var9, mode="out", weights=E(var9)$var9)

distances_dist_out_var9[distances_dist_out_var9=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var9_vec <- vector()
for (i in 1:vcount(var9)) {
    distances_sp_out_var9_vec[i] <- 
    mean(distances_dist_out_var9[i,], na.rm = T)
}

#Adding to igraph object
V(var9)$sp_out<-distances_sp_out_var9_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var9_shortpath_df <- data.frame(distances_sp_in_var9_vec, distances_sp_out_var9_vec, distances_sp_all_var9_vec) %>% round(3)

#Adding type
var9_shortpath_df <-cbind(var9_shortpath_df, V(var9)$LABEL_COR)

#Adding names
names(var9_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var9_shortpath_df<-var9_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var9_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-6e24120ec6b1e31a4464" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-6e24120ec6b1e31a4464">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"10.393\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"9.167\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.449\" data-max=\"6.947\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[3.309,4.094,2.832,4.644,4.074,4.396,5.463,4.242,4.966,5.181,4.047,4.396,4.987,5.168,4.799,5.557,4.336,3.591,7.013,4.282,4.275,5.027,4.483,7.711,9.738,5.745,6.987,4.342,4.711,4.134,4.094,4.799,4.06,7.503,4.658,4.913,4.517,6.55,6.651,4.275,4.094,5.141,5.013,4.315,4.779,4.597,9.43,5.362,5.362,5.362,5.362,5.362,5.859,4.289,7.463,4.638,4.141,7.436,6.698,3.799,4.82,8.221,4.758,4.792,4.879,0,10.393,4.651,7.456,4.846,4.47,4.51,4.611,4.591,4.403,4.772,4.732,4.034,7.819,7.819,7.819,7.819,4.705,4.752,4.919,4.456,4.49,7.879,4.779,4.826,4.725,8.114,6.839,5.362,4.081,4.732,7.879,5.181,4.416,4.128,7.812,4.926,6.633,6.633,4.812,4.678,4.779,6.633,6.513,4.597,4.611,4.423,4.523,4.45,4.443,4.738,4.745,4.752,4.544,4.047,4.805,4.255,4.604,4.651,4.604,4.658,4.658,4.644,4.638,5.027,4.691,4.651,4.624,4.651,4.611,4.322,4.128,4.779,4.503,4.792,4.517,4.517,4.718,4.805,5.738,4.221,4.658,4.141,4.866,5.054,4.685,5.054,4.685,5.054,4.94,7.899,5.027,4.66,4.68,7.86,8.833,7.48,5.589,6.503,9.433,10.344,6.653,9.433,9.433,5.36,7.267,9.433,5.513,5.46,5.207,4.907,5.633,9.433,5.633,7.527,5.573,5.573,6.74,6.433,7.553,5.84,5.793],[3.425,3.613,4.129,3.935,5.554,6.075,5.656,3.505,5.231,5.833,6.301,6.909,4.14,5.156,5.511,5.731,4.699,3.392,5.747,3.952,5.747,3.978,3.973,5.188,5.957,5.441,5.016,4.194,3.167,4.769,4.452,5.597,3.769,8.215,9.167,5.935,5.43,6.624,4.478,3.833,3.441,4.215,4.226,3.624,5.21,3.667,5.812,5.645,5.645,5.645,5.645,5.645,5,6.301,3.93,5.301,3.516,4.398,6.425,5.011,0,7.108,5.629,6.392,3.661,1.5,2,5.532,5.613,5.086,5.086,6.167,3.957,4.333,6.661,6.14,6.145,4.608,8.672,8.672,8.672,8.672,6.242,8.156,5.753,4.715,3.968,4.355,4.452,6.312,4.371,5.726,4.188,6.559,3.898,4.258,7.323,6.925,4.806,3.661,8.446,4.806,0,0,4.263,4.263,6.522,0,0,3.694,3.984,4.392,6.242,5.355,5.823,4.065,5.559,4.285,4.258,5.72,4.93,5.285,7.075,7.075,7.075,5.237,6.102,4.371,6.102,5.237,4.29,4.29,5.973,7.097,5.108,5.306,6.731,7.097,6.102,4.586,6.747,7.097,5.07,5.108,7.097,7.097,6.274,8.059,6.398,5.591,5.591,5.591,5.591,5.591,8.231,7.22,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[2.449,2.92,2.706,3.455,3.433,3.615,4.914,2.845,3.674,3.727,3.385,4.07,3.321,3.759,3.989,4.93,3.497,2.668,4.401,3.043,3.476,3.193,3.134,4.674,5.139,4.759,4.305,3.316,2.759,3.316,3.503,4.294,3.021,5.845,4.305,3.679,3.439,5.54,3.866,3.267,2.877,3.877,3.652,3.048,3.845,2.93,4.914,3.904,3.904,3.904,3.904,3.904,4.086,3.551,3.23,3.92,3.037,3.433,5.214,3.086,4.005,4.968,4.225,4.369,3.102,6.947,6.16,3.556,4.465,3.62,3.332,4.102,3.118,3.615,3.759,4.064,4.257,3.326,6.701,6.701,6.701,6.701,4.332,3.583,3.781,3.786,2.984,3.428,3.947,4.369,3.332,5.064,3.316,4.722,3.16,3,6.278,3.834,3.326,3.016,6.647,3.62,5.679,5.679,3.219,3.572,4.364,5.679,5.636,3.011,3.107,3.294,3.941,3.786,3.786,3.07,3.738,3.134,3.273,3.487,4.086,3.428,4.289,4.299,4.289,4.107,4.134,3.642,4.128,4.107,3.369,3.364,4.305,4.31,3.583,3.684,3.594,4.374,3.904,3.786,3.84,3.733,3.524,3.631,5.551,3.84,4.134,3.594,3.588,4.342,4.316,4.342,4.316,4.342,3.888,4.914,4.337,4.31,4.316,6.011,6.171,6.053,3.979,5.262,6.139,6.182,5.433,6.139,6.139,4.294,6.053,6.139,4.091,4.503,4.209,3.791,4.882,6.139,4.882,6.112,3.727,3.727,5.711,5.219,6.102,4.091,5.674]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var9_shortpath_df, by=list(var9_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var9_shortpath_df, by=list(var9_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-4dc60df4c9aeed6a0bda" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-4dc60df4c9aeed6a0bda">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.832\" data-max=\"7.103\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.091\" data-max=\"1.809\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.526\" data-max=\"5.8\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.008\" data-max=\"3.408\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.668\" data-max=\"5.125\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.159\" data-max=\"1.232\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[4.34,7.103,4.094,4.591,4.33,2.832,5.077,4.366,5.225,5.936,5.998,4.631,3.591],[0.456,1.601,null,0.947,0.113,null,0.091,0.264,1.777,1.809,0.634,0.222,null],[4.536,2.526,3.613,4.038,5.253,4.129,4.22,5.277,4.796,5.8,3.755,5.217,3.392],[1.621,3.408,null,2.112,1.461,null,0.008,1.584,1.438,1.205,2.811,1.525,null],[3.162,5.125,2.92,3.843,3.442,2.706,3.764,3.501,3.823,4.368,4.711,3.831,2.668],[0.376,1.049,null,1.232,0.3,null,0.159,0.42,1.103,1.041,0.872,0.433,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var9, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-0811df4ae92da58b984c" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-0811df4ae92da58b984c">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["7214","1555","14084","8470","3153","305","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

var9_dist.from.CAPSAD <- distances(var9, v=V(var9)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(var9), weights=E(var9)$var9)

#Saving distance on igraph object 
V(var9)$var9_dist.from.CAPSAD<-var9_dist.from.CAPSAD

var9_dist.from.CAPSAD[var9_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(var9_dist.from.CAPSAD)+1)
col <- col[var9_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(var9)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(var9, es=E(var9), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var9))
maxC <- rep(Inf, vcount(var9))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var9, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var9)$var9)

#Plotting distance from CAPSAD 
plot(var9, 
     layout=co,
     edge.color=V(var9)$col[edge.start],
     edge.arrow.size=(betweenness(var9, weights = E(var9)$var9)+1)/100000,
     edge.width=E(var9)$var9/10*mean(E(var9)$var9),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(var9))*5,
     vertex.label=var9_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(var9))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var9)$var9_dist.from.CAPSAD
b<-V(var9)$col
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
  title("Distance from CAPS AD - 11_ACESSO C) Este serviço disponibiliza informações sobre suas ativades para o seu serviço. (var9)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(var9, directed=T, unconnected = T)
     )
             )
```

![](11_ACESSO_C_disponibiliza_informações_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_var9.RData") 
```

