# SNA Reciprocity Distance Path 30_ACESSO_NEGATIVO
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 30_ACESSO_NEGATIVO

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_acesso_negativo.RData")
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
#acesso_negativo<-simplify(acesso_negativo) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(acesso_negativo, directed=T, unconnected = T)
```

```
## [1] 2.512914
```
##Shortest Paths

```r
#Shortest Paths
acesso_negativo_sp_in <- shortest.paths(acesso_negativo, mode='in', weights=E(acesso_negativo)$acesso_negativo) #in

acesso_negativo_sp_out <- shortest.paths(acesso_negativo, mode='out', weights=E(acesso_negativo)$acesso_negativo) # out

acesso_negativo_sp_all <- shortest.paths(acesso_negativo, mode='all', weights=E(acesso_negativo)$acesso_negativo) # all
```
##Descriptive Shortest Paths - IN

```r
summary(acesso_negativo_sp_in[which(acesso_negativo_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00   11.00   13.00   13.75   17.00   32.00
```

```r
sd(acesso_negativo_sp_in[which(acesso_negativo_sp_in != Inf)])
```

```
## [1] 4.540017
```
##Descriptive  Shortest Paths - OUT

```r
summary(acesso_negativo_sp_out[which(acesso_negativo_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00   11.00   13.00   13.75   17.00   32.00
```

```r
sd(acesso_negativo_sp_out[which(acesso_negativo_sp_out != Inf)])
```

```
## [1] 4.540017
```

##Descriptive  Shortest Paths - ALL

```r
summary(acesso_negativo_sp_all[which(acesso_negativo_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00   10.00   10.00   11.52   14.00   27.00
```

```r
sd(acesso_negativo_sp_all[which(acesso_negativo_sp_all != Inf)])
```

```
## [1] 3.52961
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_acesso_negativo<-distances(acesso_negativo, mode="all", weights=E(acesso_negativo)$acesso_negativo)
#distances_sp_all_acesso_negativo

distances_dist_all_acesso_negativo[distances_dist_all_acesso_negativo=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_acesso_negativo_vec <- vector()
for (i in 1:vcount(acesso_negativo)) {
    distances_sp_all_acesso_negativo_vec[i] <- 
    mean(distances_dist_all_acesso_negativo[i,],na.rm=T)
}
#Adding to igraph object
V(acesso_negativo)$sp_all<-distances_sp_all_acesso_negativo_vec
```

#In shortest paths 

```r
distances_dist_in_acesso_negativo<-distances(acesso_negativo, mode="in",weights=E(acesso_negativo)$acesso_negativo)
#distances_sp_in_acesso_negativo

distances_dist_in_acesso_negativo[distances_dist_in_acesso_negativo=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_acesso_negativo_vec <- vector()
for (i in 1:vcount(acesso_negativo)) {
    distances_sp_in_acesso_negativo_vec[i] <- mean(distances_dist_in_acesso_negativo[i,], na.rm=T)
}

#Adding to igraph object
V(acesso_negativo)$sp_in<-distances_sp_in_acesso_negativo_vec
```

#Out shortest paths 

```r
distances_dist_out_acesso_negativo<-distances(acesso_negativo, mode="out", weights=E(acesso_negativo)$acesso_negativo)

distances_dist_out_acesso_negativo[distances_dist_out_acesso_negativo=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_acesso_negativo_vec <- vector()
for (i in 1:vcount(acesso_negativo)) {
    distances_sp_out_acesso_negativo_vec[i] <- 
    mean(distances_dist_out_acesso_negativo[i,], na.rm = T)
}

#Adding to igraph object
V(acesso_negativo)$sp_out<-distances_sp_out_acesso_negativo_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
acesso_negativo_shortpath_df <- data.frame(distances_sp_in_acesso_negativo_vec, distances_sp_out_acesso_negativo_vec, distances_sp_all_acesso_negativo_vec) %>% round(3)

#Adding type
acesso_negativo_shortpath_df <-cbind(acesso_negativo_shortpath_df, V(acesso_negativo)$LABEL_COR)

#Adding names
names(acesso_negativo_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
acesso_negativo_shortpath_df<-acesso_negativo_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(acesso_negativo_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-534d3d4812d514bb65f4" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-534d3d4812d514bb65f4">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"22.52\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"21.484\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6.743\" data-max=\"20.663\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[8.732,10.027,7.765,10.725,10.389,10.617,11.255,11.007,16.611,11.315,10.443,10.403,15.57,14.074,12.51,11.383,10.43,9.799,14.792,11.215,10.477,16.322,10.43,18.302,19.235,11.584,19.315,10.383,10.785,10.705,10.557,12.557,10.403,16.745,10.758,16.114,11.188,13.866,13.926,10.799,10.624,11.107,11.168,11.087,11.537,10.443,19.403,13.913,13.913,13.913,13.913,13.899,20.591,13.02,15.416,10.765,11.919,15.725,16.544,9.98,10.873,18.141,12.43,12.409,13.671,0,20.28,11.295,15.201,12.812,12.275,12.181,12.107,12.054,11.973,12.403,12.248,10.161,18.55,18.55,18.55,18.55,12.201,13.369,13.718,12.168,12.94,22.208,12.295,12.664,12.369,18.973,17.906,11.295,12.013,14.336,22.208,16.483,12.081,10.611,17.906,14.074,13.9,13.9,12.53,12.456,12.49,13.9,13.713,12.235,12.315,11.987,12.174,12.114,12.188,12.349,12.403,12.416,12.208,11.839,12.544,11.101,12.181,12.356,12.181,12.289,12.302,12.235,12.161,13.208,12.456,12.221,12.228,12.289,12.128,12.074,12.094,12.403,12.242,12.51,12.074,12.349,12.477,12.523,11.664,9.98,10.705,12.221,14.738,13.409,12.463,13.409,12.463,13.409,11.114,18.087,13.2,12.32,12.46,21.42,18.48,15.48,16.113,16.252,19.46,20.172,17.047,19.46,19.46,16.627,16.527,19.46,16.553,16.287,16.7,16.687,18.32,19.46,18.32,19.16,16.26,16.26,14.5,15.927,17.713,22.52,11.687],[9.468,9.43,7.661,10.403,12.333,11.575,11.194,11.849,11.457,13.102,13.844,12.306,11.828,10.704,11.156,13.935,9.274,11.151,11.091,12.839,10.597,12.634,9.882,10.065,13.887,18.296,17.269,14.505,10.581,11.86,10.398,12.575,11.199,16.892,16.828,11.753,15.011,14.935,11.72,9.978,10.328,9.624,9.624,12.425,12.618,11.871,20.344,12.484,12.484,12.484,12.484,13.919,13.737,15.468,11.452,11.011,15.425,14.349,15.161,12.194,0,18,11.688,16.129,14.102,3.5,2.667,12.581,13.129,12.543,14.522,11.866,13.785,15.478,16.016,14.269,15.989,11.554,18.785,18.785,18.785,18.785,12.941,15.011,13.919,14.71,12.312,14.204,11.231,14.263,12.817,11.672,13.29,17.828,14.946,13.457,19.151,16.64,16.36,10.726,15.672,13.747,0,0,16.269,11.457,16.36,0,0,15.231,13.704,14.022,13.398,15.468,13.989,14.306,13.78,11.769,15.371,16.22,11.887,10.823,14.376,14.376,14.376,14.909,16.581,12.145,16.581,15.903,15,15,12.22,12.602,13.597,12.919,16.516,12.602,13.597,15.989,14.57,14.591,14.414,14.591,11.618,16.591,12.022,21.484,15.022,16.366,16.366,16.366,16.366,16.366,17.941,14.909,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[7.545,8.214,6.743,9.326,9.492,9.551,9.588,9.241,10.738,10.209,9.85,9.701,11.503,10.08,10.257,10.241,8.476,9.144,10.134,9.845,9.492,10.594,8.807,9.572,12.193,10.053,12.824,9.914,9.508,9.834,9.267,11.332,9.283,12.968,10.187,9.818,10.321,11.615,10.727,8.973,9.401,9.043,8.995,9.834,10.267,9.765,13.545,10.84,10.84,10.84,10.84,11.583,12.257,11.235,10.353,9.283,11.08,11.807,11.861,9.048,9.428,14.15,10.433,11.679,11.759,20.166,15.016,10.128,11.775,11.193,11.369,10.893,10.604,10.604,11,11.422,10.701,9.513,15.909,15.909,15.909,15.909,10.979,10.588,11.043,11.588,10.31,12.38,10.16,11.781,10.925,10.786,12.23,10.487,11.439,10.658,16.786,13.818,11.455,9.214,13.893,11.893,12.134,12.134,11.749,10.54,11.695,12.134,12.005,10.807,11.273,11.294,11.235,11.428,11.572,11.364,11.326,10.695,11.46,11.235,10.706,9.759,11.412,11.492,11.412,11.433,11.642,10.54,11.476,11.973,11.684,11.449,10.936,11.037,11.396,11.422,11.476,11.091,11.278,11.631,11.273,11.535,11.711,11.706,10.583,9.733,9.936,11.551,13.193,12.353,11.684,12.353,11.684,12.353,9.519,11.38,12.289,11.642,11.684,14.283,14.059,12.882,13.241,13.374,14.016,15.241,13.733,14.016,14.016,13.529,13.46,14.016,13.519,13.396,13.663,14.134,13.888,14.016,13.888,13.995,13.257,13.257,12.369,12.684,13.674,20.663,10.701]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(acesso_negativo_shortpath_df, by=list(acesso_negativo_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(acesso_negativo_shortpath_df, by=list(acesso_negativo_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-e56285cbf05d4648b7a8" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-e56285cbf05d4648b7a8">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7.765\" data-max=\"17.267\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.043\" data-max=\"5.015\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.98\" data-max=\"13.853\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"7.991\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6.743\" data-max=\"13.275\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.034\" data-max=\"2.718\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[11.922,17.267,10.027,10.682,10.317,7.765,11.137,10.552,14.83,13.483,13.678,12.377,9.799],[2.442,2.492,null,1.025,0.228,null,0.043,0.191,5.015,3.002,0.725,0.339,null],[12.183,5.98,9.43,10.607,12.894,7.661,9.624,11.834,13.113,12.47,8.704,13.853,11.151],[2.497,7.991,null,5.382,3.194,null,0,1.882,3.219,1.616,6.485,3.505,null],[10.626,13.275,8.214,9.49,9.486,6.743,9.019,9.519,11.94,10.867,11.328,11.33,9.144],[2.224,2.031,null,1.053,0.485,null,0.034,0.408,2.718,1.414,0.678,0.472,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(acesso_negativo, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-8c33a4db94a3c8717f3b" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-8c33a4db94a3c8717f3b">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["7214","1555","14084","8470","3153","305","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

acesso_negativo_dist.from.CAPSAD <- distances(acesso_negativo, v=V(acesso_negativo)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(acesso_negativo), weights=E(acesso_negativo)$acesso_negativo)

#Saving distance on igraph object 
V(acesso_negativo)$acesso_negativo_dist.from.CAPSAD<-acesso_negativo_dist.from.CAPSAD

acesso_negativo_dist.from.CAPSAD[acesso_negativo_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(acesso_negativo_dist.from.CAPSAD)+1)
col <- col[acesso_negativo_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(acesso_negativo)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(acesso_negativo, es=E(acesso_negativo), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acesso_negativo))
maxC <- rep(Inf, vcount(acesso_negativo))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acesso_negativo, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acesso_negativo)$acesso_negativo)

#Plotting distance from CAPSAD 
plot(acesso_negativo, 
     layout=co,
     edge.color=V(acesso_negativo)$col[edge.start],
     edge.arrow.size=(betweenness(acesso_negativo, weights = E(acesso_negativo)$acesso_negativo)+1)/100000,
     edge.width=E(acesso_negativo)$acesso_negativo/10*mean(E(acesso_negativo)$acesso_negativo),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(acesso_negativo))*5,
     vertex.label=acesso_negativo_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(acesso_negativo))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
```

```
## Warning in mean.default(E(acesso_negativo)$acesso_negativo): argument is
## not numeric or logical: returning NA
```

```r
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(acesso_negativo)$acesso_negativo_dist.from.CAPSAD
b<-V(acesso_negativo)$col
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
  title("Distance from CAPS AD - 30_ACESSO_NEGATIVO", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(acesso_negativo, directed=T, unconnected = T)
     )
             )
```

![](30_ACESSO_NEGATIVO_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_acesso_negativo.RData") 
```

