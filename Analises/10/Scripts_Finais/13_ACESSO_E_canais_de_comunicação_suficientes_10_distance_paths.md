# SNA Reciprocity Distance Path 13_ACESSO E) Os canais de comunicação com este serviço são suficientes para realização de ações em conjunto. (var11)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 13_ACESSO E) Os canais de comunicação com este serviço são suficientes para realização de ações em conjunto. (var11)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_var11.RData")
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
#var11<-simplify(var11) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var11, directed=T, unconnected = T)
```

```
## [1] 2.511789
```
##Shortest Paths

```r
#Shortest Paths
var11_sp_in <- shortest.paths(var11, mode='in', weights=E(var11)$var11) #in

var11_sp_out <- shortest.paths(var11, mode='out', weights=E(var11)$var11) # out

var11_sp_all <- shortest.paths(var11, mode='all', weights=E(var11)$var11) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var11_sp_in[which(var11_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   4.000   5.000   5.275   7.000  14.000
```

```r
sd(var11_sp_in[which(var11_sp_in != Inf)])
```

```
## [1] 2.15897
```
##Descriptive  Shortest Paths - OUT

```r
summary(var11_sp_out[which(var11_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   4.000   5.000   5.275   7.000  14.000
```

```r
sd(var11_sp_out[which(var11_sp_out != Inf)])
```

```
## [1] 2.15897
```

##Descriptive  Shortest Paths - ALL

```r
summary(var11_sp_all[which(var11_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   3.000   4.000   3.969   5.000  11.000
```

```r
sd(var11_sp_all[which(var11_sp_all != Inf)])
```

```
## [1] 1.636404
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var11<-distances(var11, mode="all", weights=E(var11)$var11)
#distances_sp_all_var11

distances_dist_all_var11[distances_dist_all_var11=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var11_vec <- vector()
for (i in 1:vcount(var11)) {
    distances_sp_all_var11_vec[i] <- 
    mean(distances_dist_all_var11[i,],na.rm=T)
}
#Adding to igraph object
V(var11)$sp_all<-distances_sp_all_var11_vec
```

#In shortest paths 

```r
distances_dist_in_var11<-distances(var11, mode="in",weights=E(var11)$var11)
#distances_sp_in_var11

distances_dist_in_var11[distances_dist_in_var11=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var11_vec <- vector()
for (i in 1:vcount(var11)) {
    distances_sp_in_var11_vec[i] <- mean(distances_dist_in_var11[i,], na.rm=T)
}

#Adding to igraph object
V(var11)$sp_in<-distances_sp_in_var11_vec
```

#Out shortest paths 

```r
distances_dist_out_var11<-distances(var11, mode="out", weights=E(var11)$var11)

distances_dist_out_var11[distances_dist_out_var11=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var11_vec <- vector()
for (i in 1:vcount(var11)) {
    distances_sp_out_var11_vec[i] <- 
    mean(distances_dist_out_var11[i,], na.rm = T)
}

#Adding to igraph object
V(var11)$sp_out<-distances_sp_out_var11_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var11_shortpath_df <- data.frame(distances_sp_in_var11_vec, distances_sp_out_var11_vec, distances_sp_all_var11_vec) %>% round(3)

#Adding type
var11_shortpath_df <-cbind(var11_shortpath_df, V(var11)$LABEL_COR)

#Adding names
names(var11_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var11_shortpath_df<-var11_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var11_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-32daf21e7377e61d92e6" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-32daf21e7377e61d92e6">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"9.887\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"9.194\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.455\" data-max=\"7.31\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[3.174,3.953,2.57,4.483,3.899,4.282,4.846,4.141,4.899,4.987,3.852,4.262,5.537,5.523,4.53,5.383,4.262,3.463,5.886,4.107,4.342,4.872,4.282,7.879,8.711,5.477,5.839,4.342,4.544,3.993,3.919,4.651,3.933,7.221,4.503,4.779,4.376,6.322,6.423,4.094,4,4.993,4.745,4.322,4.658,4.302,8.383,5.134,5.134,5.134,5.134,5.134,6,4.188,6.953,4.497,3.98,5.913,6.51,3.624,4.9,8.221,4.631,4.658,5.027,0,9.887,5.047,7.282,4.732,4.295,4.329,4.409,4.47,4.235,4.436,4.403,3.879,8.604,8.604,8.604,8.604,4.517,4.919,5.705,4.282,4.537,8.403,4.638,4.691,4.564,8.463,6.738,5.396,3.993,4.852,8.403,5.047,4.255,3.946,8.174,5.094,6.42,6.42,4.664,4.664,4.631,6.42,5.867,4.577,4.617,4.228,4.45,4.383,4.362,4.584,4.604,4.591,4.517,4,4.658,4.134,4.389,4.544,4.389,4.53,4.43,4.396,4.416,4.886,4.577,4.416,4.329,4.409,4.315,4.315,3.993,4.416,4.309,4.671,4.523,4.517,4.564,4.664,5.477,4.188,4.886,4,5.423,4.919,4.564,4.919,4.564,4.919,4.779,8.161,4.607,4.427,4.567,8.38,7.76,5.447,5.682,6.318,8.387,9.841,5.58,8.387,8.387,5.14,7.027,8.387,5.267,5.333,5.16,5.067,6.24,8.387,6.173,5.267,5.38,5.38,5.947,5.38,7.333,5.987,5.533],[3.317,3.634,4.086,4.602,5.624,4.22,5.618,3.323,5.199,5.704,5.145,4.806,4.763,5.118,4.86,4.398,4.183,3.441,5.543,5.903,4.683,5.833,4.274,5.134,3.957,7.043,5.204,4.194,2.876,4.774,3.543,5.667,3.769,9.108,7.081,4.14,5.323,6.167,6.097,5.145,4.586,4.989,5,3.613,5.688,3.672,5.672,4.409,4.409,4.409,4.409,4.409,5.839,5.129,5.441,5.263,4.22,5.414,5.957,4.516,0,8.898,3.677,4.484,3.634,1.5,2,3.57,5.909,5.043,5.043,6.091,3.817,3.962,5.36,4.231,3.769,3.5,7.586,7.586,7.586,7.586,6.14,8.102,4.667,3.661,4.118,6.226,3.737,6.371,4.21,5.672,6.242,6.425,4.704,4.177,9.194,7.091,5.011,4,6.753,4.091,0,0,4.177,5.726,6.645,0,0,3.925,4.048,3.925,5.22,4.194,3.758,4.011,3.435,5.462,3.758,3.758,3.935,4.108,6.048,6.048,6.048,6.038,5.065,6.253,5.065,6.038,4.306,4.306,5.011,7.054,5.065,6.054,5.71,7.054,5.065,4.301,7.038,7.054,4.543,7.054,7.054,7.054,6.29,9.011,6.419,6.608,6.608,6.608,6.608,6.608,8.247,7.231,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[2.471,2.791,2.455,3.342,3.246,3.267,3.695,2.69,3.535,3.487,3.102,3.016,3.615,4.38,3.684,3.631,3.219,2.663,3.781,3.294,3.519,3.658,3.155,4.572,3.267,5.251,4.299,3.07,2.524,3.283,2.866,4.289,2.984,5.695,3.476,3.193,3.321,5.075,5.123,3.374,3.07,4.39,3.706,3.075,3.85,2.834,4.561,3.545,3.545,3.545,3.545,3.545,4.08,3.567,4.358,3.428,3.31,3.781,3.775,2.893,4.412,7.102,2.84,3.209,3.096,6.92,7.235,2.85,4.914,3.401,3.075,3.642,2.936,3.037,3.535,3.257,2.973,2.866,6.198,6.198,6.198,6.198,4.128,3.487,3.278,3.027,3.064,5.39,2.968,4.353,3.342,5.021,5.016,4.775,3.513,2.904,6.567,3.679,3.198,2.941,5.872,3.342,5.54,5.54,3.176,4.332,4.337,5.54,5.241,3.198,3.144,3.107,3.781,3.118,2.973,2.984,2.909,3.513,3.021,3.021,3.032,2.925,3.824,4.123,3.824,4.278,3.326,3.947,3.321,4.278,3.273,3.214,3.594,3.93,3.299,3.61,3.465,3.829,3.38,3.267,4.123,3.781,3.332,4.358,5.273,3.829,4.663,3.599,3.503,4.342,4.299,4.342,4.299,4.342,3.733,5.358,3.947,3.947,4.299,6.262,5.738,3.786,3.952,5.171,5.722,7.31,4.235,5.722,5.722,3.775,5.599,5.722,3.652,3.818,3.786,3.663,4.888,5.722,4.545,3.679,3.273,3.273,5.07,3.567,5.695,4.086,5.422]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var11_shortpath_df, by=list(var11_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var11_shortpath_df, by=list(var11_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-4f53de54205beee0d105" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-4f53de54205beee0d105">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.57\" data-max=\"6.792\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.073\" data-max=\"1.876\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.505\" data-max=\"5.283\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.008\" data-max=\"3.327\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.455\" data-max=\"4.805\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.277\" data-max=\"1.392\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[4.452,6.792,3.953,4.375,4.288,2.57,4.869,4.206,5.298,5.768,5.744,4.476,3.463],[0.842,1.497,null,0.901,0.073,null,0.175,0.264,1.876,1.591,0.616,0.214,null],[4.925,2.505,3.634,4.254,5.051,4.086,4.995,4.561,5.26,5.283,3.226,4.977,3.441],[1.357,3.327,null,2.176,1.352,null,0.008,1.098,1.857,1.202,2.466,1.599,null],[3.186,4.805,2.791,3.642,3.393,2.455,4.048,3.113,4.146,4.01,4.451,3.578,2.663],[0.278,1.096,null,1.08,0.35,null,0.484,0.277,1.392,1.015,0.944,0.486,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var11, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-a1c7b72f9469536b5dde" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-a1c7b72f9469536b5dde">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["7214","1557","14096","8471","3138","305","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

var11_dist.from.CAPSAD <- distances(var11, v=V(var11)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(var11), weights=E(var11)$var11)

#Saving distance on igraph object 
V(var11)$var11_dist.from.CAPSAD<-var11_dist.from.CAPSAD

var11_dist.from.CAPSAD[var11_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(var11_dist.from.CAPSAD)+1)
col <- col[var11_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(var11)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(var11, es=E(var11), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var11))
maxC <- rep(Inf, vcount(var11))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var11, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var11)$var11)

#Plotting distance from CAPSAD 
plot(var11, 
     layout=co,
     edge.color=V(var11)$col[edge.start],
     edge.arrow.size=(betweenness(var11, weights = E(var11)$var11)+1)/100000,
     edge.width=E(var11)$var11/10*mean(E(var11)$var11),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(var11))*5,
     vertex.label=var11_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(var11))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var11)$var11_dist.from.CAPSAD
b<-V(var11)$col
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
  title("Distance from CAPS AD - 13_ACESSO E) Os canais de comunicação com este serviço são suficientes para realização de ações em conjunto. (var11)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(var11, directed=T, unconnected = T)
     )
             )
```

![](13_ACESSO_E_canais_de_comunicação_suficientes_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_var11.RData") 
```

