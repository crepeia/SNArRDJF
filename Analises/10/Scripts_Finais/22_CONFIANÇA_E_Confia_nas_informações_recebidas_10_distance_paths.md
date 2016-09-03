# SNA Reciprocity Distance Path 22_CONFIANÇA E) Confia nas informações recebas deste serviço (var20).
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 22_CONFIANÇA E) Confia nas informações recebas deste serviço (var20).

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_var20.RData")
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
#var20<-simplify(var20) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var20, directed=T, unconnected = T)
```

```
## [1] 2.512515
```
##Shortest Paths

```r
#Shortest Paths
var20_sp_in <- shortest.paths(var20, mode='in', weights=E(var20)$var20) #in

var20_sp_out <- shortest.paths(var20, mode='out', weights=E(var20)$var20) # out

var20_sp_all <- shortest.paths(var20, mode='all', weights=E(var20)$var20) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var20_sp_in[which(var20_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   5.000   6.000   6.358   8.000  15.000
```

```r
sd(var20_sp_in[which(var20_sp_in != Inf)])
```

```
## [1] 2.282566
```
##Descriptive  Shortest Paths - OUT

```r
summary(var20_sp_out[which(var20_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   5.000   6.000   6.358   8.000  15.000
```

```r
sd(var20_sp_out[which(var20_sp_out != Inf)])
```

```
## [1] 2.282566
```

##Descriptive  Shortest Paths - ALL

```r
summary(var20_sp_all[which(var20_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   4.000   5.000   5.343   6.000  12.000
```

```r
sd(var20_sp_all[which(var20_sp_all != Inf)])
```

```
## [1] 1.835107
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var20<-distances(var20, mode="all", weights=E(var20)$var20)
#distances_sp_all_var20

distances_dist_all_var20[distances_dist_all_var20=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var20_vec <- vector()
for (i in 1:vcount(var20)) {
    distances_sp_all_var20_vec[i] <- 
    mean(distances_dist_all_var20[i,],na.rm=T)
}
#Adding to igraph object
V(var20)$sp_all<-distances_sp_all_var20_vec
```

#In shortest paths 

```r
distances_dist_in_var20<-distances(var20, mode="in",weights=E(var20)$var20)
#distances_sp_in_var20

distances_dist_in_var20[distances_dist_in_var20=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var20_vec <- vector()
for (i in 1:vcount(var20)) {
    distances_sp_in_var20_vec[i] <- mean(distances_dist_in_var20[i,], na.rm=T)
}

#Adding to igraph object
V(var20)$sp_in<-distances_sp_in_var20_vec
```

#Out shortest paths 

```r
distances_dist_out_var20<-distances(var20, mode="out", weights=E(var20)$var20)

distances_dist_out_var20[distances_dist_out_var20=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var20_vec <- vector()
for (i in 1:vcount(var20)) {
    distances_sp_out_var20_vec[i] <- 
    mean(distances_dist_out_var20[i,], na.rm = T)
}

#Adding to igraph object
V(var20)$sp_out<-distances_sp_out_var20_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var20_shortpath_df <- data.frame(distances_sp_in_var20_vec, distances_sp_out_var20_vec, distances_sp_all_var20_vec) %>% round(3)

#Adding type
var20_shortpath_df <-cbind(var20_shortpath_df, V(var20)$LABEL_COR)

#Adding names
names(var20_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var20_shortpath_df<-var20_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var20_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-31cf0fae2726055c8d26" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-31cf0fae2726055c8d26">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"10.947\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"10.054\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.166\" data-max=\"8.572\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[4.107,5.054,3.416,5.201,5.06,5.161,5.966,4.141,6.94,4.289,5.054,4.953,7.322,6.832,5.463,5.852,4.966,4.611,7.45,5.913,5.128,6.087,5.094,9.174,10.174,5.342,8.114,5.101,5.228,5.201,5.128,5.443,4.966,8.141,5.215,6.752,4.295,6.631,6.852,4.98,5.322,5.671,5.812,4.215,5.315,5.054,10.262,6.644,6.644,6.644,6.644,6.644,7.624,5.779,8.02,5.221,5.295,7.96,7.638,4.772,4.987,8.926,5.403,5.423,6.651,0,10.947,5.188,7.852,5.497,5.275,5.262,5.255,5.195,5.168,5.436,5.396,4.886,8.698,8.698,8.698,8.698,5.349,6.215,5.799,5.235,6.074,10.161,5.43,5.49,5.456,9.772,8.597,6.013,5.309,6.483,10.161,6.926,5.195,5.074,8.617,6.49,6.84,6.84,5.436,5.443,5.423,6.84,6.387,5.356,5.396,5.168,5.342,5.255,5.228,5.43,5.436,5.436,5.376,5.282,5.409,5.007,5.255,5.295,5.255,5.295,5.309,5.275,5.275,5.758,5.329,5.289,5.275,5.275,5.255,5.188,5.396,5.43,5.389,5.45,5.389,5.416,5.456,5.443,6.336,4.859,5.289,5.362,7.584,5.792,5.329,5.792,5.329,5.792,5.101,9.262,5.747,5.3,5.32,10.067,10.187,7.133,6.947,7.026,10.273,10.894,7.273,10.273,10.273,8,7.987,10.273,7.073,7.86,7.827,7.773,7.213,10.273,7.213,8.16,7.98,7.98,7.5,6.94,8.167,9.587,6.373],[3.806,4.29,4.134,4.742,5.86,6.409,6.263,5.629,4.823,6.409,5.839,6.876,5.269,5.586,5.57,6.011,5.081,5.753,6.78,5.376,5.769,6.339,6.038,5.591,6.36,8.5,5.737,5.672,5.344,5.419,5.774,6.07,4.349,8.29,9.242,6.339,5.484,7.108,6.349,4.704,5.296,4.79,4.796,6.183,6.328,6.048,7.14,6.011,6.011,6.011,6.011,6.011,6.419,6.634,5.538,5.651,6.005,6.962,7.871,5.387,0,7.559,5.011,6.796,5.349,1,2,5.828,6.554,6.081,7.07,6.425,5.048,6.263,6.828,6.704,4.758,5.715,9.677,9.677,9.677,9.677,6.317,8.559,6.758,5.113,6.145,6.694,5.871,6.769,6.237,5.93,5.758,7.242,6.177,6.285,9.661,9.28,6.806,5.172,8.769,5.823,0,0,5.317,6.301,7.016,0,0,6.14,6.129,6.28,5.323,6.355,6.333,5.247,6.167,6.156,6.263,5.269,6.065,5.726,7.075,7.075,7.075,6.478,5.113,6.978,5.113,6.478,5.5,5.5,6.478,7.102,6.108,6.333,7.032,7.102,7.102,6.489,6.823,7.102,7.022,7.102,7.102,7.102,6.495,10.054,8.22,6.269,6.269,6.269,6.269,6.269,8.457,7.435,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[3.166,3.786,3.444,4.171,4.599,4.695,5.54,4.064,4.497,4.342,4.476,4.529,4.92,5.262,4.738,4.893,4.31,4.305,6.07,4.604,4.54,5.556,4.658,5.262,5.77,5.193,4.893,4.626,4.594,4.455,4.69,4.861,3.802,6.283,4.84,5.54,4.176,5.492,5.401,4.182,4.695,4.38,4.342,4.209,5.171,4.642,6.011,4.973,4.973,4.973,4.973,4.973,5.406,4.791,5.08,4.561,4.818,5.893,6.005,4.176,4.706,6.492,4.508,4.979,4.749,7.652,7.802,5.021,5.139,4.77,4.834,4.845,4.492,4.668,4.765,4.834,4.048,4.524,8.364,8.364,8.364,8.364,4.941,4.984,5.444,4.604,5.021,5.995,4.781,5.016,5.005,5.583,5.075,5.674,4.866,5.16,7.872,6.845,4.797,4.551,7.829,5.38,5.727,5.727,4.765,4.909,5,5.727,5.61,4.684,4.898,4.775,4.61,4.824,4.834,4.733,4.952,4.979,4.898,4.658,4.941,4.754,4.861,4.872,4.861,4.845,4.273,4.856,4.251,4.845,4.679,4.663,4.872,4.829,4.658,4.802,4.989,4.973,4.925,4.93,4.973,4.92,5.021,5.016,6.353,4.556,5,4.904,6.497,4.963,4.888,4.963,4.888,4.963,4.84,5.62,4.957,4.882,4.888,7.203,8.572,5.968,5.674,5.754,7.893,8.032,6.182,7.893,7.893,6.872,6.861,7.893,6.037,6.775,6.701,6.471,6.267,7.893,6.267,6.353,6.743,6.743,6.545,5.69,6.856,7.717,6.412]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var20_shortpath_df, by=list(var20_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var20_shortpath_df, by=list(var20_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-b88af6f22f7c51d374f8" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-b88af6f22f7c51d374f8">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.416\" data-max=\"8.254\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.1\" data-max=\"2.388\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.902\" data-max=\"6.406\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.004\" data-max=\"3.857\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.444\" data-max=\"6.583\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.027\" data-max=\"1.373\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[5.957,8.254,5.054,5.46,5.046,3.416,5.742,5.102,6.446,6.609,6.622,5.383,4.611],[1.42,1.523,null,0.822,0.125,null,0.1,0.116,2.388,1.724,0.277,0.15,null],[5.955,2.902,4.29,4.749,6.145,4.134,4.793,6,6.116,6.406,4.127,6.037,5.753],[2.018,3.857,null,2.343,0.656,null,0.004,1.116,1.756,1.014,3.064,1.529,null],[4.998,6.583,3.786,4.875,4.595,3.444,4.361,4.545,5.412,5.515,5.287,4.815,4.305],[1.373,1.121,null,1.082,0.056,null,0.027,0.172,1.117,0.978,0.355,0.183,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var20, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-98fd3618d2485aaad6b1" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-98fd3618d2485aaad6b1">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["7214","1557","14090","8463","3152","305","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

var20_dist.from.CAPSAD <- distances(var20, v=V(var20)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(var20), weights=E(var20)$var20)

#Saving distance on igraph object 
V(var20)$var20_dist.from.CAPSAD<-var20_dist.from.CAPSAD

var20_dist.from.CAPSAD[var20_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(var20_dist.from.CAPSAD)+1)
col <- col[var20_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(var20)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(var20, es=E(var20), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var20))
maxC <- rep(Inf, vcount(var20))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var20, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var20)$var20)

#Plotting distance from CAPSAD 
plot(var20, 
     layout=co,
     edge.color=V(var20)$col[edge.start],
     edge.arrow.size=(betweenness(var20, weights = E(var20)$var20)+1)/100000,
     edge.width=E(var20)$var20/10*mean(E(var20)$var20),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(var20))*5,
     vertex.label=var20_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(var20))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var20)$var20_dist.from.CAPSAD
b<-V(var20)$col
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
  title("Distance from CAPS AD - 22_CONFIANÇA E) Confia nas informações recebas deste serviço (var20).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(var20, directed=T, unconnected = T)
     )
             )
```

![](22_CONFIANÇA_E_Confia_nas_informações_recebidas_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_var20.RData") 
```

