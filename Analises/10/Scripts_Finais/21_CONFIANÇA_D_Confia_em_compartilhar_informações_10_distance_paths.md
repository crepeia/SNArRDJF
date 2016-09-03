# SNA Reciprocity Distance Path 21_CONFIANÇA D) Confia em compartilhar informações internas com este serviço. (var19) 
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 21_CONFIANÇA D) Confia em compartilhar informações internas com este serviço. (var19) 

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_var19.RData")
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
#var19<-simplify(var19) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var19, directed=T, unconnected = T)
```

```
## [1] 2.512914
```
##Shortest Paths

```r
#Shortest Paths
var19_sp_in <- shortest.paths(var19, mode='in', weights=E(var19)$var19) #in

var19_sp_out <- shortest.paths(var19, mode='out', weights=E(var19)$var19) # out

var19_sp_all <- shortest.paths(var19, mode='all', weights=E(var19)$var19) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var19_sp_in[which(var19_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   5.000   6.000   5.883   7.000  14.000
```

```r
sd(var19_sp_in[which(var19_sp_in != Inf)])
```

```
## [1] 2.094623
```
##Descriptive  Shortest Paths - OUT

```r
summary(var19_sp_out[which(var19_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   5.000   6.000   5.883   7.000  14.000
```

```r
sd(var19_sp_out[which(var19_sp_out != Inf)])
```

```
## [1] 2.094623
```

##Descriptive  Shortest Paths - ALL

```r
summary(var19_sp_all[which(var19_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   4.000   4.000   4.684   6.000  10.000
```

```r
sd(var19_sp_all[which(var19_sp_all != Inf)])
```

```
## [1] 1.493962
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var19<-distances(var19, mode="all", weights=E(var19)$var19)
#distances_sp_all_var19

distances_dist_all_var19[distances_dist_all_var19=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var19_vec <- vector()
for (i in 1:vcount(var19)) {
    distances_sp_all_var19_vec[i] <- 
    mean(distances_dist_all_var19[i,],na.rm=T)
}
#Adding to igraph object
V(var19)$sp_all<-distances_sp_all_var19_vec
```

#In shortest paths 

```r
distances_dist_in_var19<-distances(var19, mode="in",weights=E(var19)$var19)
#distances_sp_in_var19

distances_dist_in_var19[distances_dist_in_var19=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var19_vec <- vector()
for (i in 1:vcount(var19)) {
    distances_sp_in_var19_vec[i] <- mean(distances_dist_in_var19[i,], na.rm=T)
}

#Adding to igraph object
V(var19)$sp_in<-distances_sp_in_var19_vec
```

#Out shortest paths 

```r
distances_dist_out_var19<-distances(var19, mode="out", weights=E(var19)$var19)

distances_dist_out_var19[distances_dist_out_var19=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var19_vec <- vector()
for (i in 1:vcount(var19)) {
    distances_sp_out_var19_vec[i] <- 
    mean(distances_dist_out_var19[i,], na.rm = T)
}

#Adding to igraph object
V(var19)$sp_out<-distances_sp_out_var19_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var19_shortpath_df <- data.frame(distances_sp_in_var19_vec, distances_sp_out_var19_vec, distances_sp_all_var19_vec) %>% round(3)

#Adding type
var19_shortpath_df <-cbind(var19_shortpath_df, V(var19)$LABEL_COR)

#Adding names
names(var19_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var19_shortpath_df<-var19_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var19_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-3479e92922c00f727814" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-3479e92922c00f727814">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"10.82\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"9.478\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.786\" data-max=\"7.663\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[3.691,4.966,3.161,4.966,4.96,4.463,5.765,3.859,4.745,4.074,4.45,4.792,5.51,6.611,5.255,4.02,4.785,4.181,7.456,5.537,4.456,4.678,4.96,9.04,8.564,4.121,7.993,4.919,5.04,5.081,4.973,5.221,4.812,7.973,5.04,4.691,3.98,6.631,6.758,5.248,4.275,5.45,5.678,3.913,5.624,4.886,9.832,6.477,6.477,6.477,6.477,6.477,5.651,5.47,7.893,5.04,5.141,8.523,7.141,4.208,3.893,8.745,5.141,5.208,4.671,0,10.82,5.745,7.698,5.282,4.57,5.06,5.047,4.993,4.51,5.228,5.168,4.201,7.698,7.698,7.698,7.698,5.148,6.081,4.799,4.557,4.658,8.362,5.215,5.282,4.664,9.55,6.617,5.919,5.134,4.732,8.362,4.799,4.523,4.913,5.745,5.96,6.733,6.733,5.215,5.221,5.201,6.733,6.567,5.161,5.201,4.49,5.148,4.564,4.544,5.201,5.228,5.228,5.208,5.141,5.221,5.523,5.054,5.094,5.054,5.081,5.094,5.074,5.067,5.322,5.128,5.081,5.074,5.081,5.047,4.51,5.228,5.228,4.886,5.201,5.101,5.228,5.255,5.235,6.094,4.671,5.121,5.195,6.765,5.376,5.128,5.376,5.128,5.376,3.926,9.087,5.34,5.1,5.12,8.4,10.22,6.793,5.464,6.848,9.84,10.768,8.06,9.84,9.84,7.42,7.84,9.84,7.867,7.327,5.807,5.213,7.06,9.84,7.06,7.987,6.887,6.887,6.713,6.78,8,5.633,6.12],[3.613,4.032,3.699,4.978,4.629,6.204,5.925,3.538,4.543,4.446,5.495,6.489,5.043,5.339,5.387,6.188,4.925,5.059,5.505,5.038,5.78,4.172,5.79,3.71,6.177,8.194,4.559,5.462,5.048,5.108,5.452,5.747,4.194,7.048,8,4.462,5.097,5.704,4.172,5.419,4.828,5.468,5.468,3.823,4.548,5.715,5.914,6.038,6.038,6.038,6.038,6.038,5.785,6.237,3.855,5.027,5.634,6.468,6.328,5.091,0,7.328,5.763,6.457,4.328,0.5,2,5.742,5.113,6.634,6.645,6.167,5.801,6.097,6.522,6.204,6.043,5.07,7.382,7.382,7.382,7.382,6.07,6.677,4.462,5.306,5.312,6.511,5.409,6.527,5.984,5.79,5.478,6.962,5.855,6.048,9.478,7.113,5.118,5.532,8.242,5.618,0,0,6.048,5.957,6.591,0,0,5.925,5.93,6.005,5.075,5.113,6.065,5.968,5.844,5.855,5.995,4.005,5.828,5.554,6.645,6.645,6.645,6.215,6.667,6.554,6.667,6.215,5.237,6.231,6.183,6.667,6.667,6.091,6.608,6.667,6.667,6.226,6.651,6.667,6.591,6.667,6.667,6.667,6.231,8.624,7.704,6.011,6.011,6.011,6.011,6.011,8.199,7.172,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[2.786,3.455,2.93,4.193,3.957,3.829,5.144,2.947,3.813,3.406,3.856,4.283,4.332,4.989,4.449,3.802,4.048,3.759,4.738,4.321,3.786,3.332,4.433,3.353,5.433,3.904,3.754,4.193,4.321,4.118,4.406,4.524,3.326,5.283,4.492,3.396,3.535,4.615,3.567,4.599,3.626,4.642,4.765,3.219,3.749,4.171,5.112,4.963,4.963,4.963,4.963,4.963,4.652,4.497,3.471,4.262,4.551,5.529,5.519,3.417,3.679,5.588,4.203,4.604,3.674,5.561,6.39,4.984,4.31,4.642,3.829,4.508,3.882,3.85,3.802,4.503,3.936,3.674,6.652,6.652,6.652,6.652,4.594,4.824,3.733,3.84,3.738,5.658,4.497,4.647,4.182,5.23,4.765,5.219,4.519,3.701,7.283,3.936,3.818,4.337,4.701,4.973,5.401,5.401,4.626,4.578,4.615,5.401,5.358,4.059,4.62,3.802,4.294,3.834,3.84,4.588,4.615,4.615,4.615,3.369,4.594,4.813,4.513,4.535,4.513,4.513,4.545,4.519,4.508,4.513,4.337,4.529,4.535,4.519,4.524,3.824,4.652,4.631,4.096,4.535,4.551,4.642,4.663,4.647,5.866,4.064,4.829,4.561,5.973,4.62,4.551,4.62,4.551,4.62,3.631,5.417,4.615,4.545,4.551,6.155,7.663,6.118,4.572,5.733,6.225,6.422,6.198,6.225,6.225,6.187,6.166,6.225,6.187,6.16,4.973,4.476,5.807,6.225,5.807,6.193,5.642,5.642,5.684,5.578,6.144,4.663,5.898]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var19_shortpath_df, by=list(var19_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var19_shortpath_df, by=list(var19_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-ae468f5973c3a5de3824" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-ae468f5973c3a5de3824">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.161\" data-max=\"7.646\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.161\" data-max=\"1.997\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.389\" data-max=\"5.925\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"3.194\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.93\" data-max=\"5.538\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.087\" data-max=\"1.449\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[5.284,7.646,4.966,5.165,4.752,3.161,5.564,4.821,5.193,6.253,6.38,5.09,4.181],[1.31,1.76,null,0.925,0.235,null,0.161,0.279,1.997,1.128,0.753,0.237,null],[5.575,2.389,4.032,4.594,5.925,3.699,5.468,5.549,5.176,5.892,3.854,5.859,5.059],[1.871,3.194,null,2.265,0.518,null,0,0.907,1.818,0.965,2.896,1.407,null],[4.308,5.538,3.455,4.446,4.119,2.93,4.704,4.124,4.291,4.818,4.863,4.397,3.759],[1.449,1.016,null,1.083,0.27,null,0.087,0.242,1.115,0.547,0.605,0.31,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var19, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-96b93398353f8a3ba81c" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-96b93398353f8a3ba81c">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["7214","1555","14084","8470","3153","305","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

var19_dist.from.CAPSAD <- distances(var19, v=V(var19)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(var19), weights=E(var19)$var19)

#Saving distance on igraph object 
V(var19)$var19_dist.from.CAPSAD<-var19_dist.from.CAPSAD

var19_dist.from.CAPSAD[var19_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(var19_dist.from.CAPSAD)+1)
col <- col[var19_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(var19)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(var19, es=E(var19), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var19))
maxC <- rep(Inf, vcount(var19))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var19, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var19)$var19)

#Plotting distance from CAPSAD 
plot(var19, 
     layout=co,
     edge.color=V(var19)$col[edge.start],
     edge.arrow.size=(betweenness(var19, weights = E(var19)$var19)+1)/100000,
     edge.width=E(var19)$var19/10*mean(E(var19)$var19),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(var19))*5,
     vertex.label=var19_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(var19))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var19)$var19_dist.from.CAPSAD
b<-V(var19)$col
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
  title("Distance from CAPS AD - 21_CONFIANÇA D) Confia em compartilhar informações internas com este serviço. (var19) ", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(var19, directed=T, unconnected = T)
     )
             )
```

![](21_CONFIANÇA_D_Confia_em_compartilhar_informações_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_var19.RData") 
```

