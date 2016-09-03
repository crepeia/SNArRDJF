# SNA Reciprocity Distance Path 26_CONFIANÇA I) Espera que o relacionamento com este serviço se fortaleça nos próximos anos. (var24)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 26_CONFIANÇA I) Espera que o relacionamento com este serviço se fortaleça nos próximos anos. (var24)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_var24.RData")
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
#var24<-simplify(var24) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var24, directed=T, unconnected = T)
```

```
## [1] 2.526671
```
##Shortest Paths

```r
#Shortest Paths
var24_sp_in <- shortest.paths(var24, mode='in', weights=E(var24)$var24) #in

var24_sp_out <- shortest.paths(var24, mode='out', weights=E(var24)$var24) # out

var24_sp_all <- shortest.paths(var24, mode='all', weights=E(var24)$var24) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var24_sp_in[which(var24_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   6.000   6.000   7.302   9.000  18.000
```

```r
sd(var24_sp_in[which(var24_sp_in != Inf)])
```

```
## [1] 2.434005
```
##Descriptive  Shortest Paths - OUT

```r
summary(var24_sp_out[which(var24_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   6.000   6.000   7.302   9.000  18.000
```

```r
sd(var24_sp_out[which(var24_sp_out != Inf)])
```

```
## [1] 2.434005
```

##Descriptive  Shortest Paths - ALL

```r
summary(var24_sp_all[which(var24_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   5.000   6.000   6.341   8.000  12.000
```

```r
sd(var24_sp_all[which(var24_sp_all != Inf)])
```

```
## [1] 1.944866
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var24<-distances(var24, mode="all", weights=E(var24)$var24)
#distances_sp_all_var24

distances_dist_all_var24[distances_dist_all_var24=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var24_vec <- vector()
for (i in 1:vcount(var24)) {
    distances_sp_all_var24_vec[i] <- 
    mean(distances_dist_all_var24[i,],na.rm=T)
}
#Adding to igraph object
V(var24)$sp_all<-distances_sp_all_var24_vec
```

#In shortest paths 

```r
distances_dist_in_var24<-distances(var24, mode="in",weights=E(var24)$var24)
#distances_sp_in_var24

distances_dist_in_var24[distances_dist_in_var24=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var24_vec <- vector()
for (i in 1:vcount(var24)) {
    distances_sp_in_var24_vec[i] <- mean(distances_dist_in_var24[i,], na.rm=T)
}

#Adding to igraph object
V(var24)$sp_in<-distances_sp_in_var24_vec
```

#Out shortest paths 

```r
distances_dist_out_var24<-distances(var24, mode="out", weights=E(var24)$var24)

distances_dist_out_var24[distances_dist_out_var24=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var24_vec <- vector()
for (i in 1:vcount(var24)) {
    distances_sp_out_var24_vec[i] <- 
    mean(distances_dist_out_var24[i,], na.rm = T)
}

#Adding to igraph object
V(var24)$sp_out<-distances_sp_out_var24_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var24_shortpath_df <- data.frame(distances_sp_in_var24_vec, distances_sp_out_var24_vec, distances_sp_all_var24_vec) %>% round(3)

#Adding type
var24_shortpath_df <-cbind(var24_shortpath_df, V(var24)$LABEL_COR)

#Adding names
names(var24_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var24_shortpath_df<-var24_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var24_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-d3c6acf9e929d1dc2dcc" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-d3c6acf9e929d1dc2dcc">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"11.805\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"10.694\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.626\" data-max=\"10.364\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[4.706,5.418,3.915,6.059,5.922,6.039,6.464,5.379,8.059,5.556,5.954,5.83,8.654,7.935,6.386,6.209,5.817,5.444,8.418,6.562,5.869,7.967,5.843,10.686,7.987,6.817,9.02,5.804,6.078,6.118,6,6.333,5.915,9.02,6.078,7.778,5.608,6.68,6.752,6.19,6.02,6.307,6.464,5.523,6.758,5.882,11.007,6.614,6.614,6.614,6.614,6.614,10.654,7.19,8.882,6.078,6.235,8.961,8.418,5.556,6.305,9.529,6.314,6.333,7.693,0,11.805,6.392,7.569,6.425,6.288,6.255,6.248,6.209,6.19,6.333,6.275,5.784,10.412,10.412,10.412,10.412,6.275,7.725,7.51,6.248,6.856,11.458,6.235,6.412,6.314,10.876,9.353,6.608,6.255,7.601,11.458,8.163,6.209,6.072,10.118,7.673,6.732,6.732,6.333,6.333,6.314,6.725,6.647,6.275,6.314,6.163,6.255,6.248,6.248,6.327,6.333,6.333,6.294,6.157,6.333,6.451,6.255,6.307,6.255,6.268,6.307,6.288,6.268,7.157,6.327,6.288,6.275,6.288,6.255,6.131,6.333,6.333,6.255,6.353,6.255,6.353,6.373,6.353,6.837,5.484,5.974,6.307,8.333,7.235,6.327,7.235,6.327,7.235,6.49,10.15,7.149,6.305,6.325,11.351,11,8.961,8.613,8.826,11.032,11.748,9.13,11.032,11.032,8.994,8.961,11.032,8.994,8.857,8.513,8.435,9.039,11.032,9.039,9.019,8.825,8.825,8.377,8.747,9.039,10.623,6.87],[4.871,5.591,4.392,5.946,6.452,6.688,6.43,6.28,6.371,7.237,7,7.151,5.93,6.022,5.833,7.21,5.29,6.242,7.016,7.29,6.699,7.323,6.995,5.839,7.414,9.328,7.651,6.124,5.785,6.344,6.597,6.86,6.435,10.134,9.898,7.323,6.613,7.978,6.914,6.124,5.941,6.188,6.188,7.032,6.898,6.823,9.753,7.183,7.183,7.183,7.183,7.183,7.435,6.892,5.828,6.274,6.688,7.194,8.172,6.14,0,9.946,6.962,6.769,6.866,1.5,2,6.64,6.731,7.328,7.328,7.059,6.995,7.011,7.145,7.134,7.151,6.317,9.866,9.866,9.866,9.866,7.167,8.806,6.946,6.962,7.032,7.726,6.785,7.247,7.108,6.742,7.167,8.925,6.898,7.177,10.694,10.258,7.28,4.64,9.683,6.323,10.366,10.366,7.07,6.806,7.097,9.677,9.677,6.93,6.925,7.118,7.21,7.247,7.199,5.048,6.946,6.93,7.14,7.14,7.027,6.548,7.328,7.328,7.328,7.14,7.36,7.263,7.36,7.14,7.156,7.156,7.124,7.36,7.36,6.935,7.29,7.36,7.36,7.156,7.344,7.36,7.285,7.36,7.36,7.36,7.156,10.312,8.72,8.559,8.559,8.559,8.559,8.559,9.113,8.091,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[3.84,4.38,3.626,5.572,5.583,5.679,5.824,4.856,6.091,5.364,5.872,5.529,5.599,5.845,5.396,5.091,4.973,4.679,6.246,6.08,4.834,6.412,4.829,5.455,6.401,6.128,6.032,4.797,5.604,5.743,5.791,6.134,5.497,7.877,5.984,6.299,5.182,5.043,5.086,5.679,5.658,5.775,5.722,5.267,6.214,5.813,7.668,5.086,5.086,5.086,5.086,5.086,6.668,6.128,5.283,5.567,6.128,5.952,6.241,4.529,5.813,7.695,6.203,6.171,6.155,10.364,8.203,5.973,5.941,6.396,6.348,6.316,5.968,5.941,6.262,6.23,5.845,5.701,8.69,8.69,8.69,8.69,6.289,6.203,5.77,6.374,5.636,6.701,6.096,6.353,6.332,6.246,6.471,6.444,6.246,5.84,8.551,7.492,6.358,4.155,8.321,5.989,5.112,5.112,6.31,6.219,6.267,5.118,5.118,5.845,6.31,6.31,5.807,6.353,6.364,4.385,6.262,6.241,6.305,6.225,6.31,6,6.353,6.412,6.353,6.316,6.412,6.369,6.316,6.316,6.428,6.396,6.358,6.337,6.353,6.283,6.353,6.289,6.182,6.294,6.299,6.358,6.38,6.364,6.535,4.888,5.84,5.893,7.69,6.722,6.428,6.722,6.428,6.722,5.749,6.337,6.695,6.412,6.428,7.695,8.636,7.818,7.396,7.567,8.102,8.235,7.968,8.102,8.102,7.952,7.898,8.102,7.936,7.829,6.807,6.765,7.872,8.102,7.872,7.834,7.433,7.433,7.358,7.417,7.759,9.123,6.594]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var24_shortpath_df, by=list(var24_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var24_shortpath_df, by=list(var24_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-d2004779ddb4d6f37a14" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-d2004779ddb4d6f37a14">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.915\" data-max=\"9.443\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.111\" data-max=\"2.619\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.153\" data-max=\"8.176\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"4.178\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.626\" data-max=\"7.409\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.021\" data-max=\"1.324\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[6.756,9.443,5.418,6.12,5.75,3.915,6.386,5.972,7.643,7.306,6.629,6.372,5.444],[1.367,1.356,null,0.757,0.179,null,0.111,0.115,2.619,1.221,0.143,0.267,null],[7.032,3.153,5.591,5.471,6.795,4.392,6.188,6.659,7.154,7.081,8.175,6.916,6.242],[1.483,4.178,null,2.816,0.522,null,0,1.097,1.929,0.954,1.401,1.641,null],[6.282,7.409,4.38,5.352,4.837,3.626,5.748,5.647,6.447,6.285,5.093,6.254,4.679],[1.222,1.004,null,1.146,0.038,null,0.037,0.244,1.324,0.72,0.021,0.313,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var24, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-db6b6404d8af02e3258a" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-db6b6404d8af02e3258a">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["6474","1572","14196","8966","3208","365","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

var24_dist.from.CAPSAD <- distances(var24, v=V(var24)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(var24), weights=E(var24)$var24)

#Saving distance on igraph object 
V(var24)$var24_dist.from.CAPSAD<-var24_dist.from.CAPSAD

var24_dist.from.CAPSAD[var24_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(var24_dist.from.CAPSAD)+1)
col <- col[var24_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(var24)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(var24, es=E(var24), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var24))
maxC <- rep(Inf, vcount(var24))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var24, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var24)$var24)

#Plotting distance from CAPSAD 
plot(var24, 
     layout=co,
     edge.color=V(var24)$col[edge.start],
     edge.arrow.size=(betweenness(var24, weights = E(var24)$var24)+1)/100000,
     edge.width=E(var24)$var24/10*mean(E(var24)$var24),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(var24))*5,
     vertex.label=var24_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(var24))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var24)$var24_dist.from.CAPSAD
b<-V(var24)$col
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
  title("Distance from CAPS AD - 26_CONFIANÇA I) Espera que o relacionamento com este serviço se fortaleça nos próximos anos. (var24)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(var24, directed=T, unconnected = T)
     )
             )
```

![](26_CONFIANÇA_I_Relacionamento_se_fortaleça_nos_próximos_anos_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_var24.RData") 
```

