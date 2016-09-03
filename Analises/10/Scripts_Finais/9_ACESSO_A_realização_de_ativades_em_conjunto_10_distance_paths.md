# SNA Reciprocity Distance Path 9_ACESSO A) Este serviço mostra-se disponível para realização de ativades em conjunto. (var7)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 9_ACESSO A) Este serviço mostra-se disponível para realização de ativades em conjunto. (var7)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_var7.RData")
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
#var7<-simplify(var7) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var7, directed=T, unconnected = T)
```

```
## [1] 2.512914
```
##Shortest Paths

```r
#Shortest Paths
var7_sp_in <- shortest.paths(var7, mode='in', weights=E(var7)$var7) #in

var7_sp_out <- shortest.paths(var7, mode='out', weights=E(var7)$var7) # out

var7_sp_all <- shortest.paths(var7, mode='all', weights=E(var7)$var7) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var7_sp_in[which(var7_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   4.000   6.000   6.046   8.000  16.000
```

```r
sd(var7_sp_in[which(var7_sp_in != Inf)])
```

```
## [1] 2.275283
```
##Descriptive  Shortest Paths - OUT

```r
summary(var7_sp_out[which(var7_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   4.000   6.000   6.046   8.000  16.000
```

```r
sd(var7_sp_out[which(var7_sp_out != Inf)])
```

```
## [1] 2.275283
```

##Descriptive  Shortest Paths - ALL

```r
summary(var7_sp_all[which(var7_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0     4.0     5.0     4.9     6.0    11.0
```

```r
sd(var7_sp_all[which(var7_sp_all != Inf)])
```

```
## [1] 1.884209
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var7<-distances(var7, mode="all", weights=E(var7)$var7)
#distances_sp_all_var7

distances_dist_all_var7[distances_dist_all_var7=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var7_vec <- vector()
for (i in 1:vcount(var7)) {
    distances_sp_all_var7_vec[i] <- 
    mean(distances_dist_all_var7[i,],na.rm=T)
}
#Adding to igraph object
V(var7)$sp_all<-distances_sp_all_var7_vec
```

#In shortest paths 

```r
distances_dist_in_var7<-distances(var7, mode="in",weights=E(var7)$var7)
#distances_sp_in_var7

distances_dist_in_var7[distances_dist_in_var7=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var7_vec <- vector()
for (i in 1:vcount(var7)) {
    distances_sp_in_var7_vec[i] <- mean(distances_dist_in_var7[i,], na.rm=T)
}

#Adding to igraph object
V(var7)$sp_in<-distances_sp_in_var7_vec
```

#Out shortest paths 

```r
distances_dist_out_var7<-distances(var7, mode="out", weights=E(var7)$var7)

distances_dist_out_var7[distances_dist_out_var7=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var7_vec <- vector()
for (i in 1:vcount(var7)) {
    distances_sp_out_var7_vec[i] <- 
    mean(distances_dist_out_var7[i,], na.rm = T)
}

#Adding to igraph object
V(var7)$sp_out<-distances_sp_out_var7_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var7_shortpath_df <- data.frame(distances_sp_in_var7_vec, distances_sp_out_var7_vec, distances_sp_all_var7_vec) %>% round(3)

#Adding type
var7_shortpath_df <-cbind(var7_shortpath_df, V(var7)$LABEL_COR)

#Adding names
names(var7_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var7_shortpath_df<-var7_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var7_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-999fd079cf2efcf9428b" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-999fd079cf2efcf9428b">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"10.867\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"10.478\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.963\" data-max=\"7.984\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[3.181,4.409,3.081,5.054,4.805,4.98,5.436,4.557,5.456,4.872,4.711,4.966,6.846,7.134,5.409,5.678,4.98,3.342,6.953,5.221,4.785,5.289,4.987,9.141,9.691,4.034,6.966,4.886,5.168,4.933,4.819,5.47,4.779,8.128,5.201,5.336,4.671,6.497,6.577,5.034,3.993,5.396,5.47,4.329,5.644,5.027,9.45,6.483,6.483,6.483,6.483,6.483,7.255,4.181,7.94,5.141,4.148,8.101,6.685,3.805,5.493,8.584,5.369,5.456,5.289,0,10.867,5.383,7.121,5.523,5.141,5.121,5.329,5.309,5.067,5.47,5.396,4.638,8.651,8.651,8.651,8.651,5.403,6.181,5.752,5.134,4.732,9.671,5.409,5.517,5.329,10.074,7.275,5.698,4.128,5.322,9.671,5.45,5.067,4.128,8.57,6.275,6.56,6.56,5.47,5.235,5.45,6.56,6.433,5.215,5.329,5.087,5.289,5.134,5.128,5.436,5.456,5.45,5.181,4.134,5.47,5.027,5.262,5.389,5.262,5.369,5.389,5.369,5.356,5.872,5.423,5.369,5.289,5.389,5.268,4.926,4.161,5.45,5.06,5.463,5.235,5.195,5.477,5.463,6,4.671,5.336,4.174,6.591,5.919,5.409,5.919,5.409,5.919,3.859,9.235,5.887,5.393,5.413,9.62,9.647,5.58,6.629,6.881,9.46,10.815,8.213,9.46,9.46,6.967,7.833,9.46,8.06,6.987,6.653,6.48,7.233,9.46,7.233,8.127,6.96,6.96,5.913,6.047,8.16,6.247,6.04],[4.634,4.72,3.989,4.505,6.274,6.247,6.097,4.204,5.608,6.522,5.774,5.785,4.946,5.629,5.516,6.194,4.801,4.376,6.613,6.661,6.124,5.086,4.812,5.409,6.973,7.446,6.269,5.57,5.075,4.769,5.968,4.737,4.457,8.468,8.435,6.624,6.027,5.419,5.005,4.925,4.903,4.866,4.871,5.317,4.489,5.581,7.737,6.22,6.22,6.22,6.22,6.22,6.269,5.591,5.565,5.22,4.86,5.844,7.86,5.629,0,9.161,4.392,6.108,4.806,1.5,2,4.683,5.645,4.946,5.935,6.57,6.613,5.317,6.608,4.828,5.833,5.269,8.586,8.586,8.586,8.586,5.5,8.376,5.667,5.333,4.871,7.511,6.194,4.737,4.887,4.618,6.452,7.484,4.828,4.898,10.478,8,5.984,5.339,9.247,5.919,0,0,4.855,5.14,6.688,0,0,4.602,4.683,4.769,6.29,4.903,4.624,4.79,4.419,5.71,5.231,4.849,4.651,4.812,6.941,6.941,6.941,5.72,6.957,6.231,6.957,6.715,5.376,6.731,5.204,4.968,5.962,5.839,6.892,6.957,4.968,5.371,6.785,6.957,6.86,6.957,6.957,6.957,6.371,8.919,7.78,7.688,7.688,7.688,7.688,7.688,9.333,7.306,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[3.005,3.578,2.963,3.786,4.128,4.086,4.198,3.321,4.219,4.727,4.032,4.476,4.316,5.048,4.674,4.989,4.021,3.102,5.187,4.348,4.144,3.754,3.69,4.861,5.684,3.888,4.759,4.128,4.096,3.898,4.369,3.674,3.513,6.294,5.193,4.267,4.064,4.749,4.091,4.139,3.684,4.07,4.155,3.567,3.599,4.439,6.048,5,5,5,5,5,4.882,3.989,4.781,4.23,3.695,4.599,5.642,3.583,5.23,7.679,3.364,4.31,3.561,7.984,7.481,3.727,4.727,3.941,4.209,4.54,5.267,4.321,4.278,3.866,4.455,4.059,7.433,7.433,7.433,7.433,3.979,5.155,4.513,4.171,3.439,5.92,5.08,3.743,3.62,3.567,5.38,5.096,3.647,3.471,7.267,4.294,4.299,3.92,7.241,5.107,6.337,6.337,3.631,3.984,5.46,6.337,6.171,3.439,3.594,3.599,4.695,3.529,3.412,3.604,3.39,4.551,3.893,3.647,3.508,3.845,5.401,5.519,5.401,4.679,5.519,5.032,5.465,5.455,4.289,5.508,4.037,3.947,4.77,4.016,4.005,5.497,3.829,4.273,5.214,5.23,5.545,5.535,5.888,4.829,4.936,4.027,5.904,5.834,5.529,5.834,5.529,5.834,3.604,5.642,5.824,5.519,5.529,6.749,6.519,4.219,5.016,5.449,7.551,7.733,6.39,7.551,7.551,5.824,6.701,7.551,7.353,5.856,5.711,5.134,5.524,7.551,5.524,6.797,5.674,5.674,5.503,4.225,6.08,4.551,5.93]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var7_shortpath_df, by=list(var7_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var7_shortpath_df, by=list(var7_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-2958a0c20be4e3cb25df" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-2958a0c20be4e3cb25df">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.081\" data-max=\"7.762\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.052\" data-max=\"2.193\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.744\" data-max=\"6.058\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.004\" data-max=\"3.648\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.963\" data-max=\"5.908\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.06\" data-max=\"1.449\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[5.121,7.762,4.409,4.787,4.832,3.081,5.433,4.956,5.881,6.458,6.44,5.264,3.342],[1.332,1.638,null,1.079,0.135,null,0.052,0.173,2.193,1.516,0.244,0.403,null],[5.713,2.744,4.72,5.121,5.866,3.989,4.868,5.669,5.995,6.058,3.976,5.643,4.376],[1.804,3.648,null,2.438,0.905,null,0.004,1.004,1.934,1.4,2.961,1.605,null],[4.367,5.908,3.578,4.297,4.198,2.963,4.113,4.216,4.798,4.916,5.334,4.518,3.102],[1.334,1.218,null,0.969,0.47,null,0.06,0.356,1.449,1.095,0.756,0.802,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var7, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-ab005e8a02689a3b22d4" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-ab005e8a02689a3b22d4">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["7214","1555","14084","8470","3153","305","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

var7_dist.from.CAPSAD <- distances(var7, v=V(var7)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(var7), weights=E(var7)$var7)

#Saving distance on igraph object 
V(var7)$var7_dist.from.CAPSAD<-var7_dist.from.CAPSAD

var7_dist.from.CAPSAD[var7_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(var7_dist.from.CAPSAD)+1)
col <- col[var7_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(var7)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(var7, es=E(var7), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var7))
maxC <- rep(Inf, vcount(var7))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var7, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var7)$var7)

#Plotting distance from CAPSAD 
plot(var7, 
     layout=co,
     edge.color=V(var7)$col[edge.start],
     edge.arrow.size=(betweenness(var7, weights = E(var7)$var7)+1)/100000,
     edge.width=E(var7)$var7/10*mean(E(var7)$var7),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(var7))*5,
     vertex.label=var7_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(var7))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var7)$var7_dist.from.CAPSAD
b<-V(var7)$col
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
  title("Distance from CAPS AD - 9_ACESSO A) Este serviço mostra-se disponível para realização de ativades em conjunto. (var7)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(var7, directed=T, unconnected = T)
     )
             )
```

![](9_ACESSO_A_realização_de_ativades_em_conjunto_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_var7.RData") 
```

