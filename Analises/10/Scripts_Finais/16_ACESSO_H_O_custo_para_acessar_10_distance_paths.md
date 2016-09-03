# SNA Reciprocity Distance Path 16_ACESSO H) O custo para acessar este serviço impede a realização de ativades em conjunto. (var14)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 16_ACESSO H) O custo para acessar este serviço impede a realização de ativades em conjunto. (var14)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_var14.RData")
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
#var14<-simplify(var14) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var14, directed=T, unconnected = T)
```

```
## [1] 2.512914
```
##Shortest Paths

```r
#Shortest Paths
var14_sp_in <- shortest.paths(var14, mode='in', weights=E(var14)$var14) #in

var14_sp_out <- shortest.paths(var14, mode='out', weights=E(var14)$var14) # out

var14_sp_all <- shortest.paths(var14, mode='all', weights=E(var14)$var14) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var14_sp_in[which(var14_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   3.000   3.000   3.544   4.000  10.000
```

```r
sd(var14_sp_in[which(var14_sp_in != Inf)])
```

```
## [1] 1.392776
```
##Descriptive  Shortest Paths - OUT

```r
summary(var14_sp_out[which(var14_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   3.000   3.000   3.544   4.000  10.000
```

```r
sd(var14_sp_out[which(var14_sp_out != Inf)])
```

```
## [1] 1.392776
```

##Descriptive  Shortest Paths - ALL

```r
summary(var14_sp_all[which(var14_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   2.000   2.574   3.000   9.000
```

```r
sd(var14_sp_all[which(var14_sp_all != Inf)])
```

```
## [1] 0.8682944
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var14<-distances(var14, mode="all", weights=E(var14)$var14)
#distances_sp_all_var14

distances_dist_all_var14[distances_dist_all_var14=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var14_vec <- vector()
for (i in 1:vcount(var14)) {
    distances_sp_all_var14_vec[i] <- 
    mean(distances_dist_all_var14[i,],na.rm=T)
}
#Adding to igraph object
V(var14)$sp_all<-distances_sp_all_var14_vec
```

#In shortest paths 

```r
distances_dist_in_var14<-distances(var14, mode="in",weights=E(var14)$var14)
#distances_sp_in_var14

distances_dist_in_var14[distances_dist_in_var14=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var14_vec <- vector()
for (i in 1:vcount(var14)) {
    distances_sp_in_var14_vec[i] <- mean(distances_dist_in_var14[i,], na.rm=T)
}

#Adding to igraph object
V(var14)$sp_in<-distances_sp_in_var14_vec
```

#Out shortest paths 

```r
distances_dist_out_var14<-distances(var14, mode="out", weights=E(var14)$var14)

distances_dist_out_var14[distances_dist_out_var14=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var14_vec <- vector()
for (i in 1:vcount(var14)) {
    distances_sp_out_var14_vec[i] <- 
    mean(distances_dist_out_var14[i,], na.rm = T)
}

#Adding to igraph object
V(var14)$sp_out<-distances_sp_out_var14_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var14_shortpath_df <- data.frame(distances_sp_in_var14_vec, distances_sp_out_var14_vec, distances_sp_all_var14_vec) %>% round(3)

#Adding type
var14_shortpath_df <-cbind(var14_shortpath_df, V(var14)$LABEL_COR)

#Adding names
names(var14_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var14_shortpath_df<-var14_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var14_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-e7aca9a67383b7673769" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-e7aca9a67383b7673769">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"6.53\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"6.672\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.615\" data-max=\"5.733\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[2.248,2.584,2,2.758,2.711,2.711,2.879,2.779,3.752,2.859,2.698,2.698,3.732,3.597,3.47,2.913,2.698,2.57,3.584,2.919,2.745,3.678,2.752,4.295,4.45,2.96,4.537,2.725,2.779,2.779,2.752,3.57,2.738,4.154,2.779,3.671,2.852,3.443,3.456,2.765,2.785,2.846,2.832,2.852,2.946,2.705,4.362,3.336,3.336,3.336,3.336,3.336,6.436,3.389,3.705,2.772,3.416,3.799,3.537,2.584,2.773,3.899,3.443,3.523,3.463,0,4.673,2.926,3.631,3.705,3.282,3.275,3.295,3.255,3.121,3.537,3.483,2.631,6.53,6.53,6.53,6.53,3.43,3.309,3.577,3.248,3.329,5.638,3.362,3.591,3.403,4.577,3.846,2.919,3.356,3.47,5.638,3.678,3.208,2.805,4.289,3.463,3.447,3.447,3.564,3.463,3.544,3.447,3.373,3.349,3.403,3.154,3.329,3.248,3.262,3.477,3.51,3.51,3.309,3.248,3.497,2.866,3.275,3.376,3.275,3.389,3.336,3.275,3.309,3.383,3.45,3.268,3.309,3.342,3.221,3.208,3.523,3.537,3.322,3.53,3.342,3.383,3.45,3.51,2.973,2.564,2.779,3.523,3.557,3.456,3.456,3.456,3.456,3.456,2.852,4.268,3.347,3.347,3.453,5.173,4.547,3.707,3.669,3.669,4.387,4.649,4.22,4.387,4.387,4.013,3.987,4.387,4.027,3.873,4.367,4.593,4.36,4.387,4.36,4.367,3.68,3.68,3.493,3.633,4.353,6.42,2.98],[2.833,2.022,2.027,2.075,3.554,2.699,2.618,2.446,2.656,3.07,3.258,2.925,3.07,2.425,2.742,3.005,2.011,3.505,2.833,3.134,2.688,3.64,2.672,2.333,2.919,4.183,5.785,4.231,2.194,4.22,2.328,3.608,2.409,3.968,3.952,3.478,4.688,3.511,2.656,3.355,2.113,2.285,2.285,3.005,2.774,2.849,6.672,2.973,2.973,2.973,2.973,2.973,3.511,4.613,2.409,2.371,4.554,3.005,3.204,2.21,0,5.091,2.651,4.882,4.715,1.5,0.667,2.796,3.473,2.995,4.973,2.677,2.694,4.677,4.747,3.941,4.699,2.597,3.763,3.763,3.763,3.763,2.978,3.323,2.801,4.651,2.962,3.763,2.505,4.823,3.349,2.769,4.968,4.134,4.747,2.978,4.753,4.457,4.962,2.484,3.554,4.306,0,0,4.941,2.473,4.952,0,0,4.78,2.769,4.715,3.941,4.995,3.688,4.882,2.667,2.661,4.919,4.919,2.715,2.339,2.86,2.86,2.86,3.704,5,2.941,5,4.699,4.726,4.726,2.731,3.011,4.005,2.935,4.995,3.011,3.011,4.72,3.005,3.011,4.935,5,3.016,5.005,2.737,5.973,3.091,4.005,4.005,4.005,4.005,4.005,4.715,3.71,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[1.904,1.77,1.615,1.968,2.305,2.128,2.294,2.016,2.348,2.412,2.299,2.139,2.893,2.225,2.401,2.305,1.84,2.182,2.364,2.374,2.091,2.604,2.123,2.112,2.503,2.39,2.995,2.209,2.048,2.321,2.096,2.791,1.93,2.829,2.348,2.572,2.38,2.642,2.348,2.385,2.005,2.07,2.096,2.294,2.401,2.241,2.957,2.353,2.353,2.353,2.353,2.353,2.85,2.556,2.102,2.064,2.583,2.39,2.412,1.856,2.139,2.995,2.176,2.813,2.765,5.583,3.053,2.412,2.69,2.513,2.535,2.353,2.198,2.401,2.353,2.786,2.503,2.064,3.246,3.246,3.246,3.246,2.369,2.225,2.257,2.561,2.273,2.877,2.134,2.829,2.567,2.428,3.198,2.46,2.749,2.257,3.856,2.963,2.551,2.112,2.904,2.663,2.722,2.722,2.829,2.23,2.818,2.722,2.663,2.374,2.358,2.428,2.61,2.513,2.561,2.706,2.342,2.337,2.674,2.62,2.235,2.128,2.348,2.353,2.348,2.642,2.727,2.337,2.668,2.642,2.759,2.61,2.385,2.396,2.588,2.519,2.807,2.471,2.364,2.733,2.422,2.422,2.754,2.754,2.578,2.134,2.326,2.738,2.856,2.759,2.759,2.759,2.759,2.759,2.316,2.679,2.727,2.727,2.759,3.086,3.353,2.695,2.615,2.62,2.989,3.086,2.93,2.989,2.989,2.856,2.824,2.989,2.824,2.802,2.989,3.08,2.979,2.989,2.979,2.989,2.636,2.636,2.62,2.561,2.973,5.733,2.604]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var14_shortpath_df, by=list(var14_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var14_shortpath_df, by=list(var14_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-9a7790660232f8b417ad" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-9a7790660232f8b417ad">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2\" data-max=\"4.31\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.01\" data-max=\"1.332\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.404\" data-max=\"3.765\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1.948\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.615\" data-max=\"2.85\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.018\" data-max=\"0.799\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[3.027,4.31,2.584,2.762,2.696,2,2.839,2.729,3.64,3.334,3.35,3.391,2.57],[0.46,0.921,null,0.261,0.089,null,0.01,0.045,1.332,0.602,0.148,0.118,null],[2.538,1.404,2.022,2.488,3.649,2.027,2.285,2.849,3.536,3.1,2.003,3.765,3.505],[0.502,1.948,null,1.26,1.163,null,0,0.712,1.008,0.537,1.491,1.241,null],[2.264,2.85,1.77,2.229,2.139,1.615,2.083,2.143,2.808,2.487,2.491,2.555,2.182],[0.514,0.547,null,0.282,0.05,null,0.018,0.153,0.799,0.212,0.182,0.191,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var14, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-01427f498aa37fc08ede" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-01427f498aa37fc08ede">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["7214","1555","14084","8470","3153","305","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

var14_dist.from.CAPSAD <- distances(var14, v=V(var14)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(var14), weights=E(var14)$var14)

#Saving distance on igraph object 
V(var14)$var14_dist.from.CAPSAD<-var14_dist.from.CAPSAD

var14_dist.from.CAPSAD[var14_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(var14_dist.from.CAPSAD)+1)
col <- col[var14_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(var14)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(var14, es=E(var14), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var14))
maxC <- rep(Inf, vcount(var14))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var14, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var14)$var14)

#Plotting distance from CAPSAD 
plot(var14, 
     layout=co,
     edge.color=V(var14)$col[edge.start],
     edge.arrow.size=(betweenness(var14, weights = E(var14)$var14)+1)/100000,
     edge.width=E(var14)$var14/10*mean(E(var14)$var14),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(var14))*5,
     vertex.label=var14_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(var14))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var14)$var14_dist.from.CAPSAD
b<-V(var14)$col
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
  title("Distance from CAPS AD - 16_ACESSO H) O custo para acessar este serviço impede a realização de ativades em conjunto. (var14)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(var14, directed=T, unconnected = T)
     )
             )
```

![](16_ACESSO_H_O_custo_para_acessar_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_var14.RData") 
```

