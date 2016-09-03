# SNA Reciprocity Distance Path 23_CONFIANÇA F) Confia em realizar ações conjuntas com este serviço (var21).
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 23_CONFIANÇA F) Confia em realizar ações conjuntas com este serviço (var21).

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_var21.RData")
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
#var21<-simplify(var21) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var21, directed=T, unconnected = T)
```

```
## [1] 2.512914
```
##Shortest Paths

```r
#Shortest Paths
var21_sp_in <- shortest.paths(var21, mode='in', weights=E(var21)$var21) #in

var21_sp_out <- shortest.paths(var21, mode='out', weights=E(var21)$var21) # out

var21_sp_all <- shortest.paths(var21, mode='all', weights=E(var21)$var21) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var21_sp_in[which(var21_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    5.00    6.00    6.53    8.00   16.00
```

```r
sd(var21_sp_in[which(var21_sp_in != Inf)])
```

```
## [1] 2.301884
```
##Descriptive  Shortest Paths - OUT

```r
summary(var21_sp_out[which(var21_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    5.00    6.00    6.53    8.00   16.00
```

```r
sd(var21_sp_out[which(var21_sp_out != Inf)])
```

```
## [1] 2.301884
```

##Descriptive  Shortest Paths - ALL

```r
summary(var21_sp_all[which(var21_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   4.000   5.000   5.533   7.000  12.000
```

```r
sd(var21_sp_all[which(var21_sp_all != Inf)])
```

```
## [1] 1.872647
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var21<-distances(var21, mode="all", weights=E(var21)$var21)
#distances_sp_all_var21

distances_dist_all_var21[distances_dist_all_var21=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var21_vec <- vector()
for (i in 1:vcount(var21)) {
    distances_sp_all_var21_vec[i] <- 
    mean(distances_dist_all_var21[i,],na.rm=T)
}
#Adding to igraph object
V(var21)$sp_all<-distances_sp_all_var21_vec
```

#In shortest paths 

```r
distances_dist_in_var21<-distances(var21, mode="in",weights=E(var21)$var21)
#distances_sp_in_var21

distances_dist_in_var21[distances_dist_in_var21=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var21_vec <- vector()
for (i in 1:vcount(var21)) {
    distances_sp_in_var21_vec[i] <- mean(distances_dist_in_var21[i,], na.rm=T)
}

#Adding to igraph object
V(var21)$sp_in<-distances_sp_in_var21_vec
```

#Out shortest paths 

```r
distances_dist_out_var21<-distances(var21, mode="out", weights=E(var21)$var21)

distances_dist_out_var21[distances_dist_out_var21=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var21_vec <- vector()
for (i in 1:vcount(var21)) {
    distances_sp_out_var21_vec[i] <- 
    mean(distances_dist_out_var21[i,], na.rm = T)
}

#Adding to igraph object
V(var21)$sp_out<-distances_sp_out_var21_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var21_shortpath_df <- data.frame(distances_sp_in_var21_vec, distances_sp_out_var21_vec, distances_sp_all_var21_vec) %>% round(3)

#Adding type
var21_shortpath_df <-cbind(var21_shortpath_df, V(var21)$LABEL_COR)

#Adding names
names(var21_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var21_shortpath_df<-var21_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var21_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-0d7e6fe7a2454f7650bc" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-0d7e6fe7a2454f7650bc">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"11\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"10.113\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.241\" data-max=\"8.791\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[4.322,5.074,3.523,5.289,5.188,5.228,6.114,4.295,7.141,4.389,5.161,5.094,7.732,6.664,5.55,5.329,5.101,4.765,7.557,6.161,5.201,6.275,5.101,9.49,10.295,5.45,8.235,5.134,5.282,5.309,5.208,5.517,5.235,8.168,5.282,6.953,4.416,7.356,7.49,5.06,5.604,5.98,5.906,4.389,5.423,5.188,10.342,6.98,6.98,6.98,6.98,6.98,7.503,6.013,8.074,5.275,5.416,8.06,7.705,4.839,5.133,9.128,5.503,5.51,6.53,0,11,5.315,7.215,5.577,5.49,5.275,5.45,5.342,5.336,5.523,5.47,5.141,9.611,9.611,9.611,9.611,5.416,6.53,6.711,5.383,6.443,10.557,5.51,5.577,5.544,9.604,8.812,6.228,5.208,6.718,10.557,7.101,5.356,5.282,9.423,6.597,7.48,7.48,5.523,5.295,5.503,7.48,7.347,5.463,5.503,5.322,5.456,5.477,5.383,5.517,5.517,5.517,5.221,5.128,5.523,5.255,5.456,5.497,5.456,5.483,5.497,5.477,5.47,5.966,5.523,5.477,5.47,5.49,5.45,5.208,5.517,5.517,5.282,5.544,5.423,5.537,5.544,5.523,6.443,4.966,5.651,5.483,7.839,6.013,5.517,6.013,5.517,6.013,5.174,9.148,5.973,5.5,5.52,10.46,10.273,7.793,7.013,7.113,10.353,10.947,8.293,10.353,10.353,7.267,7.993,10.353,8.207,8.02,7.853,7.753,7.3,10.353,7.3,8.227,8.033,8.033,7.713,7.047,8.233,9.467,6.48],[3.817,5.124,4.194,5.016,5.941,6.441,6.29,5.71,5.091,6.457,5.634,6.952,5.29,5.565,5.613,6.43,5.065,5.785,6.763,6.382,6.027,5.462,5.935,5.753,6.478,8.349,5.731,5.989,5.387,5.978,5.672,5.941,4.43,9.323,8.285,5.462,5.597,7.441,6.763,5.608,5.371,5.161,5.167,6.237,6.355,6.038,7.403,6.43,6.43,6.43,6.43,6.43,6.554,6.704,5.575,5.839,5.968,6.973,7.935,5.597,0,9.511,6.118,6.828,6.14,1.5,2,5.892,6.613,7.129,6.14,6.78,6.151,6.565,6.855,5.892,6.323,5.849,8.753,8.753,8.753,8.753,6.355,8.72,5.833,6.14,6.075,6.694,5.973,6.839,6.274,6.032,6.935,7.452,5.511,6.285,9.661,8.403,6.177,5.839,8.849,6.113,0,0,5.312,6.581,6,0,0,6.199,6.188,6.301,5.608,5.398,6.371,4.312,6.199,6.188,6.301,6.301,5.199,4.204,7.134,7.134,7.134,5.86,7.161,7.022,7.161,6.855,5.919,6.914,6.887,6.167,7.161,6.602,7.086,7.161,7.161,5.919,6.914,7.161,7.081,7.161,7.161,7.161,6.914,10.113,8.296,8.091,8.091,8.091,8.091,8.091,8.876,7.855,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[3.241,4.059,3.551,4.396,4.717,4.781,5.572,4.209,4.743,4.455,4.476,4.647,5.07,5.283,4.802,5.305,4.444,4.417,5.941,5.578,4.781,4.877,4.417,5.369,5.904,5.273,4.936,4.69,4.642,4.706,4.556,4.888,3.909,6.989,4.813,4.845,4.374,6.46,6.203,4.872,5.112,4.791,4.77,4.433,5.283,4.738,6.048,5.775,5.775,5.775,5.775,5.775,5.791,4.925,5.144,4.594,4.898,5.93,6.048,4.246,5.027,7.834,4.936,4.989,5.428,8.791,7.872,5.225,5.251,5.048,4.797,4.872,4.834,4.802,4.936,4.722,4.807,4.738,7.904,7.904,7.904,7.904,5.011,5.727,4.984,5.005,5.401,6.118,4.85,5.048,5.043,5.668,6.134,5.909,4.781,5.38,8.021,6.877,4.882,4.717,7.69,5.551,6.936,6.936,4.802,4.84,4.727,6.936,6.872,4.824,5.032,4.947,4.904,4.813,5,3.845,5.016,5.011,4.797,4.733,4.727,3.738,5.032,5.043,5.032,4.743,5.053,5.021,4.995,5.011,4.797,5.037,5.043,4.791,5.032,4.829,5.053,5.016,4.733,4.765,5.005,5.059,5.059,5.048,6.465,4.706,5.658,4.968,6.963,5.171,5.059,5.171,5.059,5.171,4.824,6.219,5.166,5.053,5.059,7.299,8.545,6.856,5.824,5.925,7.979,8.096,7.027,7.979,7.979,6.219,6.866,7.979,6.989,6.904,6.781,6.684,6.182,7.979,6.182,6.973,6.781,6.781,6.706,5.872,6.904,8.396,6.519]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var21_shortpath_df, by=list(var21_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var21_shortpath_df, by=list(var21_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-43cf8af6e578273f7595" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-43cf8af6e578273f7595">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.523\" data-max=\"8.465\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.052\" data-max=\"2.46\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.856\" data-max=\"6.45\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.004\" data-max=\"3.778\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.551\" data-max=\"6.704\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.015\" data-max=\"1.538\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[6.226,8.465,5.074,5.632,5.101,3.523,5.943,5.211,6.611,6.749,7.072,5.503,4.765],[1.409,1.506,null,0.822,0.099,null,0.052,0.073,2.46,1.755,0.596,0.183,null],[6.032,2.856,5.124,5.054,6.278,4.194,5.164,6.007,6.244,6.45,4.399,6.299,5.785],[2.016,3.778,null,2.484,0.59,null,0.004,0.858,1.748,1.131,3.261,1.615,null],[5.328,6.704,4.059,5.112,4.649,3.551,4.78,4.634,5.688,5.598,6.21,4.925,4.417],[1.538,1.049,null,1.114,0.159,null,0.015,0.133,1.324,0.984,0.591,0.184,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var21, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-5037ebd6f922b1fc86d0" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-5037ebd6f922b1fc86d0">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["7214","1555","14084","8470","3153","305","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

var21_dist.from.CAPSAD <- distances(var21, v=V(var21)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(var21), weights=E(var21)$var21)

#Saving distance on igraph object 
V(var21)$var21_dist.from.CAPSAD<-var21_dist.from.CAPSAD

var21_dist.from.CAPSAD[var21_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(var21_dist.from.CAPSAD)+1)
col <- col[var21_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(var21)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(var21, es=E(var21), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var21))
maxC <- rep(Inf, vcount(var21))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var21, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var21)$var21)

#Plotting distance from CAPSAD 
plot(var21, 
     layout=co,
     edge.color=V(var21)$col[edge.start],
     edge.arrow.size=(betweenness(var21, weights = E(var21)$var21)+1)/100000,
     edge.width=E(var21)$var21/10*mean(E(var21)$var21),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(var21))*5,
     vertex.label=var21_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(var21))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var21)$var21_dist.from.CAPSAD
b<-V(var21)$col
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
  title("Distance from CAPS AD - 23_CONFIANÇA F) Confia em realizar ações conjuntas com este serviço (var21).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(var21, directed=T, unconnected = T)
     )
             )
```

![](23_CONFIANÇA_F_Confia_em_realizar_ações_conjuntas_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_var21.RData") 
```

