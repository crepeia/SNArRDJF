# SNA Reciprocity Distance Path 20_CONFIANÇA C) Confia nas ações realizadas por este serviço. (var18)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 20_CONFIANÇA C) Confia nas ações realizadas por este serviço. (var18)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_var18.RData")
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
#var18<-simplify(var18) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var18, directed=T, unconnected = T)
```

```
## [1] 2.512914
```
##Shortest Paths

```r
#Shortest Paths
var18_sp_in <- shortest.paths(var18, mode='in', weights=E(var18)$var18) #in

var18_sp_out <- shortest.paths(var18, mode='out', weights=E(var18)$var18) # out

var18_sp_all <- shortest.paths(var18, mode='all', weights=E(var18)$var18) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var18_sp_in[which(var18_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   5.000   6.000   6.171   8.000  15.000
```

```r
sd(var18_sp_in[which(var18_sp_in != Inf)])
```

```
## [1] 2.267985
```
##Descriptive  Shortest Paths - OUT

```r
summary(var18_sp_out[which(var18_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   5.000   6.000   6.171   8.000  15.000
```

```r
sd(var18_sp_out[which(var18_sp_out != Inf)])
```

```
## [1] 2.267985
```

##Descriptive  Shortest Paths - ALL

```r
summary(var18_sp_all[which(var18_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   4.000   5.000   5.061   6.000  12.000
```

```r
sd(var18_sp_all[which(var18_sp_all != Inf)])
```

```
## [1] 1.840908
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var18<-distances(var18, mode="all", weights=E(var18)$var18)
#distances_sp_all_var18

distances_dist_all_var18[distances_dist_all_var18=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var18_vec <- vector()
for (i in 1:vcount(var18)) {
    distances_sp_all_var18_vec[i] <- 
    mean(distances_dist_all_var18[i,],na.rm=T)
}
#Adding to igraph object
V(var18)$sp_all<-distances_sp_all_var18_vec
```

#In shortest paths 

```r
distances_dist_in_var18<-distances(var18, mode="in",weights=E(var18)$var18)
#distances_sp_in_var18

distances_dist_in_var18[distances_dist_in_var18=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var18_vec <- vector()
for (i in 1:vcount(var18)) {
    distances_sp_in_var18_vec[i] <- mean(distances_dist_in_var18[i,], na.rm=T)
}

#Adding to igraph object
V(var18)$sp_in<-distances_sp_in_var18_vec
```

#Out shortest paths 

```r
distances_dist_out_var18<-distances(var18, mode="out", weights=E(var18)$var18)

distances_dist_out_var18[distances_dist_out_var18=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var18_vec <- vector()
for (i in 1:vcount(var18)) {
    distances_sp_out_var18_vec[i] <- 
    mean(distances_dist_out_var18[i,], na.rm = T)
}

#Adding to igraph object
V(var18)$sp_out<-distances_sp_out_var18_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var18_shortpath_df <- data.frame(distances_sp_in_var18_vec, distances_sp_out_var18_vec, distances_sp_all_var18_vec) %>% round(3)

#Adding type
var18_shortpath_df <-cbind(var18_shortpath_df, V(var18)$LABEL_COR)

#Adding names
names(var18_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var18_shortpath_df<-var18_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var18_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-996098a0c333c7377fad" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-996098a0c333c7377fad">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"10.187\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"9.349\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.93\" data-max=\"8.006\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[3.872,4.617,3.342,4.477,4.389,4.443,5.799,4.685,7.221,5.047,4.403,4.342,6.933,6.282,5.275,5.148,4.349,4.121,7.154,5.262,5.114,6.02,5.007,9.57,9.919,5.268,7.866,5.054,4.497,4.47,4.409,5.235,4.148,7.866,4.483,6.966,4.839,7.02,7.161,5.228,5.101,5.49,5.463,4.611,6,4.423,9.228,6.087,6.087,6.087,6.087,6.087,7.06,5.557,7.255,4.483,5.047,8.302,7.617,4.745,4.94,8.517,5.235,5.228,6.087,0,10.187,5.597,7.282,5.309,5.208,5.007,5.188,5.101,5.081,5.221,5.154,4.309,9.832,9.832,9.832,9.832,5.134,6.611,6.933,5.141,5.342,9.799,5.242,5.302,5.208,9.221,8.356,5.584,4.906,6.477,9.799,7.497,5.121,4.919,9.336,6.04,7.153,7.153,5.235,5.027,5.215,7.153,6.587,5.195,5.248,5.094,5.161,5.208,5.134,5.228,5.242,5.235,4.973,4.832,5.228,5.148,5.161,5.235,5.161,5.188,5.228,5.215,5.195,5.591,5.255,5.215,5.195,5.208,5.174,4.953,5.094,5.208,5.148,5.262,5.074,5.255,5.248,5.262,6.275,4.946,5.195,5.114,6.329,5.651,5.248,5.651,5.248,5.651,5.013,8.631,5.627,5.233,5.253,9.727,9.873,7.533,7.219,7.291,9.233,10.139,7.98,9.233,9.233,7.88,7.687,9.233,7,7.72,6.82,6.793,7.9,9.233,7.9,7.873,7.233,7.233,6.973,7.173,7.913,9.027,6.3],[3.473,4.844,4.349,5.285,4.823,6.376,6.167,5.457,4.075,6.226,5.597,7.118,5.247,5.409,5.785,5.93,5.027,4.618,5.973,4.339,4.898,5.247,4.323,5.72,6.172,8.317,5.398,5.935,4.823,4.849,4.86,5.941,4.177,8.188,8.102,5.172,5.339,6.575,6.608,3.651,4.683,4.64,4.645,5.973,6.032,4.898,6.231,5.892,5.892,5.892,5.892,5.892,6.22,6.726,5.527,5.559,5.769,7.043,6.414,5.29,0,8.188,4.059,6.505,5.376,1,2,5.651,6.677,7.285,7.285,6.667,5.059,6.247,6.694,6.629,6.167,4.833,7.801,7.801,7.801,7.801,6.215,8.688,4.882,5.129,6.059,6.382,5.866,6.866,4.247,5.801,5.398,7.048,5.882,6.215,9.349,8.118,6.075,4.962,8.505,5.753,0,0,6.145,6.167,6.828,0,0,6.065,6.065,5.36,6.194,5.253,6.199,4.14,5.075,6.059,4.425,5.151,6.054,4.145,6.306,6.306,6.306,6.823,7.317,5.591,7.317,6.823,5.978,5.978,6.796,7.317,7.317,5.93,7.242,7.317,7.317,6.973,6.946,7.317,7.118,7.317,7.317,7.317,6.973,9.274,7.565,7.812,7.812,7.812,7.812,7.812,8.935,7.909,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[2.93,3.759,3.412,3.738,3.551,3.85,5.433,4.529,3.642,5.08,3.722,3.663,4.807,5.048,4.519,5.091,3.813,3.519,5.219,3.711,4.07,4.69,3.567,5.214,5.652,5.144,4.668,4.086,3.754,3.588,3.711,4.631,3.556,5.866,3.872,4.599,4.636,5.481,5.952,3.321,4.075,4.219,4.203,4.471,5.358,3.567,5.021,5.337,5.337,5.337,5.337,5.337,5.364,4.658,5.048,3.733,4.519,5.604,5.364,3.984,4.717,6.765,3.535,4.663,4.84,8.005,7.963,4.235,5.877,4.722,4.679,4.513,4.396,4.572,4.61,4.658,4.561,3.631,7.053,7.053,7.053,7.053,4.615,5.535,4.134,4.615,4.604,5.781,4.631,4.786,3.636,5.364,4.898,4.251,4.358,5.043,7.759,6.984,4.679,4.075,6.984,5.086,6.626,6.626,4.77,4.535,4.775,6.626,5.92,4.599,4.791,4.668,4.647,4.583,4.749,3.604,4.556,4.765,3.786,4.214,4.738,3.519,4.599,4.604,4.599,4.786,4.791,4.497,4.733,4.786,4.572,4.561,4.791,4.743,4.775,4.38,4.727,4.749,4.679,4.781,4.615,4.77,4.759,4.791,6.337,4.914,4.037,4.706,5.561,4.872,4.791,4.872,4.791,4.872,4.824,4.684,4.872,4.791,4.791,7.096,7.834,5.701,6.027,6.193,7.16,8,6.529,7.16,7.16,6.513,6.38,7.16,5.706,6.422,5.412,5.594,6.519,7.16,6.519,6.503,5.84,5.84,5.727,5.668,6.476,7.807,6.38]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var18_shortpath_df, by=list(var18_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var18_shortpath_df, by=list(var18_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-590ae81785d35a8c2c44" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-590ae81785d35a8c2c44">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.342\" data-max=\"8.106\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.019\" data-max=\"2.2\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.658\" data-max=\"6.123\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.004\" data-max=\"3.515\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.412\" data-max=\"6.162\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.011\" data-max=\"1.182\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[5.193,8.106,4.617,5.212,5.03,3.342,5.476,4.421,6.397,6.652,6.484,5.214,4.121],[1.093,1.344,null,0.793,0.071,null,0.019,0.061,2.2,1.643,0.652,0.158,null],[5.475,2.658,4.844,4.468,5.618,4.349,4.643,5.55,5.813,6.077,4.048,6.123,4.618],[1.828,3.515,null,2.289,1.314,null,0.004,1.039,1.707,1.361,3,1.648,null],[4.397,6.162,3.759,4.395,4.159,3.412,4.211,3.707,5.344,5.057,5.751,4.609,3.519],[1.041,0.977,null,1.149,0.558,null,0.011,0.104,1.182,1.166,0.582,0.279,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var18, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-9ba1a6ea20c9169b92a2" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-9ba1a6ea20c9169b92a2">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["7214","1555","14084","8470","3153","305","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

var18_dist.from.CAPSAD <- distances(var18, v=V(var18)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(var18), weights=E(var18)$var18)

#Saving distance on igraph object 
V(var18)$var18_dist.from.CAPSAD<-var18_dist.from.CAPSAD

var18_dist.from.CAPSAD[var18_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(var18_dist.from.CAPSAD)+1)
col <- col[var18_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(var18)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(var18, es=E(var18), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var18))
maxC <- rep(Inf, vcount(var18))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var18, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var18)$var18)

#Plotting distance from CAPSAD 
plot(var18, 
     layout=co,
     edge.color=V(var18)$col[edge.start],
     edge.arrow.size=(betweenness(var18, weights = E(var18)$var18)+1)/100000,
     edge.width=E(var18)$var18/10*mean(E(var18)$var18),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(var18))*5,
     vertex.label=var18_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(var18))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var18)$var18_dist.from.CAPSAD
b<-V(var18)$col
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
  title("Distance from CAPS AD - 20_CONFIANÇA C) Confia nas ações realizadas por este serviço. (var18)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(var18, directed=T, unconnected = T)
     )
             )
```

![](20_CONFIANÇA_C_Confia_nas_ações_realizadas_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_var18.RData") 
```

