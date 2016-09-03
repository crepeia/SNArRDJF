# SNA Reciprocity Distance Path 10_ACESSO B) O horário de funcionamento deste serviço possibilita a realização de ações em conjunto. (var8)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 10_ACESSO B) O horário de funcionamento deste serviço possibilita a realização de ações em conjunto. (var8)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_var8.RData")
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
#var8<-simplify(var8) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var8, directed=T, unconnected = T)
```

```
## [1] 2.512914
```
##Shortest Paths

```r
#Shortest Paths
var8_sp_in <- shortest.paths(var8, mode='in', weights=E(var8)$var8) #in

var8_sp_out <- shortest.paths(var8, mode='out', weights=E(var8)$var8) # out

var8_sp_all <- shortest.paths(var8, mode='all', weights=E(var8)$var8) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var8_sp_in[which(var8_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   6.000   6.000   6.857   9.000  17.000
```

```r
sd(var8_sp_in[which(var8_sp_in != Inf)])
```

```
## [1] 2.325807
```
##Descriptive  Shortest Paths - OUT

```r
summary(var8_sp_out[which(var8_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   6.000   6.000   6.857   9.000  17.000
```

```r
sd(var8_sp_out[which(var8_sp_out != Inf)])
```

```
## [1] 2.325807
```

##Descriptive  Shortest Paths - ALL

```r
summary(var8_sp_all[which(var8_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   5.000   6.000   5.802   7.000  12.000
```

```r
sd(var8_sp_all[which(var8_sp_all != Inf)])
```

```
## [1] 1.790929
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var8<-distances(var8, mode="all", weights=E(var8)$var8)
#distances_sp_all_var8

distances_dist_all_var8[distances_dist_all_var8=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var8_vec <- vector()
for (i in 1:vcount(var8)) {
    distances_sp_all_var8_vec[i] <- 
    mean(distances_dist_all_var8[i,],na.rm=T)
}
#Adding to igraph object
V(var8)$sp_all<-distances_sp_all_var8_vec
```

#In shortest paths 

```r
distances_dist_in_var8<-distances(var8, mode="in",weights=E(var8)$var8)
#distances_sp_in_var8

distances_dist_in_var8[distances_dist_in_var8=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var8_vec <- vector()
for (i in 1:vcount(var8)) {
    distances_sp_in_var8_vec[i] <- mean(distances_dist_in_var8[i,], na.rm=T)
}

#Adding to igraph object
V(var8)$sp_in<-distances_sp_in_var8_vec
```

#Out shortest paths 

```r
distances_dist_out_var8<-distances(var8, mode="out", weights=E(var8)$var8)

distances_dist_out_var8[distances_dist_out_var8=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var8_vec <- vector()
for (i in 1:vcount(var8)) {
    distances_sp_out_var8_vec[i] <- 
    mean(distances_dist_out_var8[i,], na.rm = T)
}

#Adding to igraph object
V(var8)$sp_out<-distances_sp_out_var8_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var8_shortpath_df <- data.frame(distances_sp_in_var8_vec, distances_sp_out_var8_vec, distances_sp_all_var8_vec) %>% round(3)

#Adding type
var8_shortpath_df <-cbind(var8_shortpath_df, V(var8)$LABEL_COR)

#Adding names
names(var8_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var8_shortpath_df<-var8_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var8_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-d25401387286258ddef3" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-d25401387286258ddef3">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"11.407\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"10.613\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.449\" data-max=\"9.225\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[4.329,5.114,3.584,5.604,5.564,5.644,6.034,5.718,8.195,6.087,5,5.51,8.389,7.725,6.074,6.208,5.517,4.805,8.067,5.792,5.638,8.268,5.584,9.745,10.557,6.503,8.02,5.537,5.705,5.161,5.067,6.027,5.591,6.812,5.685,7.886,5.926,7.497,7.678,5.785,5.711,5.631,6.134,5.805,6.134,5.483,10.436,7.383,7.383,7.383,7.383,7.383,10.349,5.43,8.483,5.685,5.195,8.617,7.839,4.973,6.013,9.161,6.013,6.027,7.389,0,11.407,5.966,6.779,6.114,5.966,5.933,5.926,5.886,5.866,6.04,5.966,5.456,9.664,9.664,9.664,9.664,5.933,6.785,6.765,5.933,6.597,11.201,5.973,6.101,5.846,10.678,8.785,6.268,5.268,7.678,11.201,8.463,5.886,5.43,8.651,7.275,7.667,7.667,6.04,6.02,6.007,7.667,7.527,5.946,5.987,5.564,5.94,5.94,5.926,6.013,6.034,6.034,6,5.215,6.027,5.302,5.658,5.705,5.658,5.691,5.705,5.685,5.671,6.289,5.732,5.691,5.953,5.973,5.933,5.866,5.228,6.027,5.926,6.054,5.933,6.04,6.067,6.054,6.51,5.067,5.678,5.248,8.208,6.336,5.732,6.336,5.732,6.336,5.966,9.913,6.28,5.707,5.727,11.013,10.133,6.793,7.291,7.417,10.453,11.351,6.92,10.453,10.453,8.64,8.607,10.453,8.64,7.607,8.227,8.153,6.847,10.453,6.847,8.64,6.527,6.527,7.007,6.573,8.66,10.32,6.54],[4.78,4.914,4.468,5.441,6.376,6.715,6.57,6.183,6.091,7.177,6.129,7.237,5.699,5.903,5.86,6.823,5.285,5.108,7.005,7.134,6.747,7.258,6.667,5.876,7.032,6.661,6.591,5.306,5.688,6.113,6.312,6.597,6.027,9.258,10.194,7.258,6.306,6.204,6.618,5.962,5.253,6.118,6.108,6.694,6.57,6.645,7.952,6.812,6.812,6.812,6.812,6.812,6.478,6.726,5.887,6.269,6.468,5.296,8.059,6.108,0,9.425,6.473,6.774,6.742,1.5,2,6.511,6.484,7.403,7.403,6.833,6.774,6.554,7.194,5.059,7.242,5.667,8.925,8.925,8.925,8.925,7.274,8.844,6.005,5.71,7.091,7.645,6.328,6.962,7.124,4.683,6.828,7.876,5.177,7.231,10.613,10.194,7.323,6.188,9.452,6.177,0,0,5.091,6.457,7.14,0,0,6.758,6.995,6.898,7.274,5.333,6.941,6.946,6.694,6.753,6.086,6.876,6.688,6.355,7.376,7.376,7.376,6.903,7.435,6.995,7.435,6.903,6.919,6.919,6.629,6.441,7.435,6.667,7.167,7.435,7.435,4.925,7.419,7.435,7.317,7.435,6.441,7.435,6.914,8.398,7.946,7.887,7.887,7.887,7.887,7.887,8.871,7.849,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[3.722,4.037,3.449,4.861,5.198,5.444,5.556,5.241,4.93,4.963,4.481,5.155,5.187,5.572,5.043,5.754,4.722,4.064,6.118,5.471,5.465,6.321,5.455,5.299,6.187,5.267,5.262,4.38,5.086,4.727,4.513,5.84,5.091,6.503,5.727,5.011,4.941,5.524,5.995,5.241,4.743,4.957,5.428,4.749,5.674,5.471,6.444,5.85,5.85,5.85,5.85,5.85,5.652,4.797,5.193,5.364,4.765,4.342,5.989,4.289,5.626,8.107,5.513,5.829,4.947,9.225,7.957,5.69,4.989,5.984,5.995,5.963,5.69,5.535,5.829,4.23,5.717,4.824,7.824,7.824,7.824,7.824,5.909,5.743,4.904,4.984,4.818,6.594,5.604,5.941,5.963,4.102,5.92,5.807,4.337,5.743,8.139,7.273,5.952,5.091,6.807,5.578,6.909,6.909,4.225,5.749,6.032,6.909,6.824,5.652,6.064,5.342,5.561,4.262,5.85,5.979,5.914,5.979,5.171,4.92,5.904,4.882,5.524,5.561,5.524,5.492,5.561,5.444,5.497,5.492,5.578,5.545,5.856,5.257,6.005,5.877,4.856,6.037,5.963,4.144,6.075,6.139,6.182,6.102,5.412,5.102,5.535,4.807,6.936,5.733,5.578,5.733,5.578,5.733,5.455,6.128,5.717,5.561,5.578,7.396,6.289,6.155,6.257,6.38,7.995,8.144,6.578,7.995,7.995,7.717,7.658,7.995,7.679,6.866,6.668,6.556,5.021,7.995,5.021,7.711,5.802,5.802,6.166,5.877,7.62,7.914,6.417]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var8_shortpath_df, by=list(var8_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var8_shortpath_df, by=list(var8_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-3b35d3e551a28d37edbe" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-3b35d3e551a28d37edbe">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.584\" data-max=\"8.557\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.239\" data-max=\"2.475\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.816\" data-max=\"6.964\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.007\" data-max=\"3.751\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.449\" data-max=\"6.582\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.333\" data-max=\"1.322\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[6.503,8.557,5.114,5.624,5.457,3.584,5.883,5.468,7.622,6.973,7.402,5.882,4.805],[1.478,1.631,null,0.773,0.263,null,0.356,0.239,2.475,1.575,0.398,0.259,null],[6.409,2.816,4.914,5.522,6.539,4.468,6.113,6.467,6.964,6.809,4.475,6.588,5.108],[1.386,3.751,null,2.616,0.891,null,0.007,1.244,1.905,0.938,3.31,1.599,null],[5.59,6.582,4.037,5.193,5.101,3.449,5.192,5.044,5.965,5.647,6.173,5.549,4.064],[1.179,1.161,null,0.92,0.509,null,0.333,0.391,1.322,0.558,0.539,0.509,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var8, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-3f70f7bfd6e4e6db03d5" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-3f70f7bfd6e4e6db03d5">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["7214","1555","14084","8470","3153","305","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

var8_dist.from.CAPSAD <- distances(var8, v=V(var8)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(var8), weights=E(var8)$var8)

#Saving distance on igraph object 
V(var8)$var8_dist.from.CAPSAD<-var8_dist.from.CAPSAD

var8_dist.from.CAPSAD[var8_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(var8_dist.from.CAPSAD)+1)
col <- col[var8_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(var8)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(var8, es=E(var8), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var8))
maxC <- rep(Inf, vcount(var8))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var8, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var8)$var8)

#Plotting distance from CAPSAD 
plot(var8, 
     layout=co,
     edge.color=V(var8)$col[edge.start],
     edge.arrow.size=(betweenness(var8, weights = E(var8)$var8)+1)/100000,
     edge.width=E(var8)$var8/10*mean(E(var8)$var8),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(var8))*5,
     vertex.label=var8_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(var8))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var8)$var8_dist.from.CAPSAD
b<-V(var8)$col
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
  title("Distance from CAPS AD - 10_ACESSO B) O horário de funcionamento deste serviço possibilita a realização de ações em conjunto. (var8)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(var8, directed=T, unconnected = T)
     )
             )
```

![](10_ACESSO_B_realização_de_ações_em_conjunto_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_var8.RData") 
```

