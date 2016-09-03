# SNA Reciprocity Distance Path 3_REFERENCIA DE ENVIO (var1)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 3_REFERENCIA DE ENVIO (var1)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_var1.RData")
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
#var1<-simplify(var1) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var1, directed=T, unconnected = T)
```

```
## [1] 2.64715
```
##Shortest Paths

```r
#Shortest Paths
var1_sp_in <- shortest.paths(var1, mode='in', weights=E(var1)$var1) #in

var1_sp_out <- shortest.paths(var1, mode='out', weights=E(var1)$var1) # out

var1_sp_all <- shortest.paths(var1, mode='all', weights=E(var1)$var1) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var1_sp_in[which(var1_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   2.000   2.626   3.000   7.000
```

```r
sd(var1_sp_in[which(var1_sp_in != Inf)])
```

```
## [1] 0.968143
```
##Descriptive  Shortest Paths - OUT

```r
summary(var1_sp_out[which(var1_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   2.000   2.626   3.000   7.000
```

```r
sd(var1_sp_out[which(var1_sp_out != Inf)])
```

```
## [1] 0.968143
```

##Descriptive  Shortest Paths - ALL

```r
summary(var1_sp_all[which(var1_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   2.000   2.451   3.000   6.000
```

```r
sd(var1_sp_all[which(var1_sp_all != Inf)])
```

```
## [1] 0.7912736
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var1<-distances(var1, mode="all", weights=E(var1)$var1)
#distances_sp_all_var1

distances_dist_all_var1[distances_dist_all_var1=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var1_vec <- vector()
for (i in 1:vcount(var1)) {
    distances_sp_all_var1_vec[i] <- 
    mean(distances_dist_all_var1[i,],na.rm=T)
}
#Adding to igraph object
V(var1)$sp_all<-distances_sp_all_var1_vec
```

#In shortest paths 

```r
distances_dist_in_var1<-distances(var1, mode="in",weights=E(var1)$var1)
#distances_sp_in_var1

distances_dist_in_var1[distances_dist_in_var1=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var1_vec <- vector()
for (i in 1:vcount(var1)) {
    distances_sp_in_var1_vec[i] <- mean(distances_dist_in_var1[i,], na.rm=T)
}

#Adding to igraph object
V(var1)$sp_in<-distances_sp_in_var1_vec
```

#Out shortest paths 

```r
distances_dist_out_var1<-distances(var1, mode="out", weights=E(var1)$var1)

distances_dist_out_var1[distances_dist_out_var1=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var1_vec <- vector()
for (i in 1:vcount(var1)) {
    distances_sp_out_var1_vec[i] <- 
    mean(distances_dist_out_var1[i,], na.rm = T)
}

#Adding to igraph object
V(var1)$sp_out<-distances_sp_out_var1_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var1_shortpath_df <- data.frame(distances_sp_in_var1_vec, distances_sp_out_var1_vec, distances_sp_all_var1_vec) %>% round(3)

#Adding type
var1_shortpath_df <-cbind(var1_shortpath_df, V(var1)$LABEL_COR)

#Adding names
names(var1_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var1_shortpath_df<-var1_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var1_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-6c2bca2f7b458ff92fba" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-6c2bca2f7b458ff92fba">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"4.948\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"4.529\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"4.925\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[1.604,1.91,1.321,2.179,2.097,2.172,2.194,2.731,3.679,3.037,2.09,2.119,4,3.022,2.112,2.978,2.172,1.918,3.149,2.851,2.022,3.627,2.03,0,3.955,2.296,3.044,2.007,2.194,2.142,2.112,2.097,2.149,3.044,2.201,3.56,2.776,2.97,3.082,2.709,2.119,2.201,2.209,2.852,3.052,2.075,0.667,2.978,2.978,2.978,2.978,2.978,0,2.448,3.125,2.179,2.082,3.813,0.5,1.91,2.14,4.582,2.097,2.097,2.926,0,0,3.015,3.037,2.142,2.119,2.112,2.112,2.097,2.082,2.104,2.09,2.082,4.03,4.03,4.03,4.03,2.097,2.728,3.067,2.097,2.888,4.91,2.104,2.127,2.119,3.985,4.575,3.597,2.09,3.075,4.948,3.694,2.082,2.104,3.679,2.649,3.081,3.081,2.097,2.097,2.097,3.081,3.081,2.097,2.097,2.082,2.09,2.097,2.097,2.09,2.097,2.097,2.104,2.052,2.104,2.585,2.112,2.142,2.112,2.112,2.134,2.127,2.119,2.522,2.149,2.127,2.119,2.134,2.112,2.09,2.097,2.104,2.09,2.104,2.082,2.112,2.112,2.104,2.296,1.918,2.122,2.104,2.963,2.582,2.142,2.582,2.142,2.582,2.184,3.956,2.548,2.133,2.148,4.933,3.993,3.044,3.058,3.14,0.667,0.5,3.088,0.667,0.667,3.066,3.059,0.667,3.066,3.029,2.956,2.926,3.051,0.667,3.051,3.044,3.14,3.14,2.971,2.971,3.044,3.897,0],[1.868,2.5,1.638,2.023,2.362,3.069,2.328,2.178,2.54,2.776,2.851,2.58,2.172,2.121,2.621,2.511,1.839,2.201,2.46,2.753,2.425,2.741,2.575,2.143,2.776,0,0,2.218,2.04,2.356,2.385,2.42,2.425,0,3.517,2.747,2.333,2.759,2.494,2.109,2.178,2.379,2.379,0,2.632,2.42,0,2.506,2.506,2.506,2.506,2.506,2.84,2.42,0,2.483,2.247,2.563,1,2.626,0,4.523,2.425,2.586,1,0.5,0.667,2.276,2.443,2.615,2.615,2.58,2.494,2.517,2.557,2.466,2.494,2.224,3.46,3.46,3.46,3.46,2.54,0,2.489,2.414,2.517,2.833,2.437,2.569,2.517,2.713,4.529,3.54,2.5,2.598,3.793,3.707,2.626,2.31,3.747,2.213,0,0,2.494,2.431,2.592,0,0,2.425,2.454,2.477,2.54,2.557,2.517,2.523,2.351,2.368,2.5,2.5,2.557,0,2.626,2.626,2.626,2.58,2.626,2.529,2.626,2.575,2.615,2.615,2.546,2.626,2.626,2.54,2.598,2.626,2.626,2.615,2.626,2.626,2.575,2.626,0,2.626,0,3.609,0,3.483,3.483,3.483,3.483,3.483,0.5,0.5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[1.505,1.973,1.441,1.989,2.016,2.188,2.161,1.892,2.274,2.269,2.156,2.156,2.199,2.118,2.285,2.269,1.817,1.989,2.296,2.263,2.113,2.306,2.113,1.984,2.382,2.274,2.833,2.032,2.032,2.102,2.097,2.21,2.14,2.876,2.28,2.29,2.124,2.651,2.296,2,2.016,2.242,2.231,2.57,2.425,2.091,2.968,2.28,2.28,2.28,2.28,2.28,2.489,2.21,2.844,2.204,2.177,2.355,2.651,1.968,2.177,3.758,2.204,2.247,2.683,3.909,3.935,2.183,2.333,2.29,2.263,2.269,2.118,2.118,2.22,2.242,2.102,2.005,3.21,3.21,3.21,3.21,2.269,2.446,2.237,2.258,2.188,2.446,2.215,2.285,2.28,2.366,3.758,2.774,2.263,2.172,3.183,2.849,2.263,2.129,3.086,2.108,3.118,3.118,2.253,2.22,2.253,3.118,3.118,2.253,2.258,2.253,2.253,2.263,2.263,2.226,2.242,2.242,2.274,2.242,2.263,2.419,2.29,2.306,2.29,2.237,2.306,2.285,2.29,2.237,2.301,2.29,2.28,2.306,2.296,2.253,2.269,2.274,2.231,2.237,2.258,2.29,2.29,2.274,2.409,2.113,2.242,2.285,2.919,2.462,2.301,2.462,2.301,2.462,2.194,2.715,2.478,2.306,2.312,2.806,3.237,2.833,2.919,2.957,2.968,4.925,2.898,2.968,2.968,2.898,2.882,2.968,2.882,2.86,2.72,2.935,2.866,2.968,2.866,2.86,2.72,2.72,2.747,2.661,2.833,3.672,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var1_shortpath_df, by=list(var1_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var1_shortpath_df, by=list(var1_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-59a94233ae16e0b652f9" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-59a94233ae16e0b652f9">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.321\" data-max=\"3.204\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.006\" data-max=\"1.343\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.574\" data-max=\"2.55\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1.334\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.441\" data-max=\"2.875\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.008\" data-max=\"0.89\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[2.41,2.656,1.91,2.037,1.994,1.321,2.205,2.14,3.204,2.932,3.02,2.146,1.918],[0.479,1.266,null,1.116,0.052,null,0.006,0.046,1.343,0.547,0.054,0.128,null],[1.534,0.574,2.5,2.204,2.461,1.638,2.379,2.473,2.55,1.878,1.691,2.511,2.201],[1.334,1.149,null,1.1,0.183,null,0,0.456,1.169,1.282,1.251,0.619,null],[2.358,2.875,1.973,1.829,2.093,1.441,2.236,2.087,2.604,2.353,2.591,2.269,1.989],[0.489,0.473,null,0.89,0.04,null,0.008,0.118,0.611,0.279,0.403,0.066,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var1, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-034632462ce1e1f78e03" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-034632462ce1e1f78e03">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8"],["unconnected","one","two","three","four","five","six","unconnected"],["11554","1158","10897","7437","2645","926","140","25"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

var1_dist.from.CAPSAD <- distances(var1, v=V(var1)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(var1), weights=E(var1)$var1)

#Saving distance on igraph object 
V(var1)$var1_dist.from.CAPSAD<-var1_dist.from.CAPSAD

var1_dist.from.CAPSAD[var1_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(var1_dist.from.CAPSAD)+1)
col <- col[var1_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(var1)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(var1, es=E(var1), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var1))
maxC <- rep(Inf, vcount(var1))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var1, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var1)$var1)

#Plotting distance from CAPSAD 
plot(var1, 
     layout=co,
     edge.color=V(var1)$col[edge.start],
     edge.arrow.size=(betweenness(var1, weights = E(var1)$var1)+1)/100000,
     edge.width=E(var1)$var1/10*mean(E(var1)$var1),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(var1))*5,
     vertex.label=var1_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(var1))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var1)$var1_dist.from.CAPSAD
b<-V(var1)$col
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
  title("Distance from CAPS AD - 3_REFERENCIA DE ENVIO (var1)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(var1, directed=T, unconnected = T)
     )
             )
```

![](3_REFERENCIA_DE_ENVIO_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_var1.RData") 
```

