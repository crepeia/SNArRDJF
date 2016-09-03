# SNA Reciprocity Distance Path 31_CONFIANÇA
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 31_CONFIANÇA

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_confianca.RData")
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
#confianca<-simplify(confianca) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(confianca, directed=T, unconnected = T)
```

```
## [1] 2.526318
```
##Shortest Paths

```r
#Shortest Paths
confianca_sp_in <- shortest.paths(confianca, mode='in', weights=E(confianca)$confianca) #in

confianca_sp_out <- shortest.paths(confianca, mode='out', weights=E(confianca)$confianca) # out

confianca_sp_all <- shortest.paths(confianca, mode='all', weights=E(confianca)$confianca) # all
```
##Descriptive Shortest Paths - IN

```r
summary(confianca_sp_in[which(confianca_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00   40.00   46.00   50.99   63.00  119.00
```

```r
sd(confianca_sp_in[which(confianca_sp_in != Inf)])
```

```
## [1] 17.38769
```
##Descriptive  Shortest Paths - OUT

```r
summary(confianca_sp_out[which(confianca_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00   40.00   46.00   50.99   63.00  119.00
```

```r
sd(confianca_sp_out[which(confianca_sp_out != Inf)])
```

```
## [1] 17.38769
```

##Descriptive  Shortest Paths - ALL

```r
summary(confianca_sp_all[which(confianca_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00   35.00   40.00   42.88   54.00   92.00
```

```r
sd(confianca_sp_all[which(confianca_sp_all != Inf)])
```

```
## [1] 14.0836
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_confianca<-distances(confianca, mode="all", weights=E(confianca)$confianca)
#distances_sp_all_confianca

distances_dist_all_confianca[distances_dist_all_confianca=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_confianca_vec <- vector()
for (i in 1:vcount(confianca)) {
    distances_sp_all_confianca_vec[i] <- 
    mean(distances_dist_all_confianca[i,],na.rm=T)
}
#Adding to igraph object
V(confianca)$sp_all<-distances_sp_all_confianca_vec
```

#In shortest paths 

```r
distances_dist_in_confianca<-distances(confianca, mode="in",weights=E(confianca)$confianca)
#distances_sp_in_confianca

distances_dist_in_confianca[distances_dist_in_confianca=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_confianca_vec <- vector()
for (i in 1:vcount(confianca)) {
    distances_sp_in_confianca_vec[i] <- mean(distances_dist_in_confianca[i,], na.rm=T)
}

#Adding to igraph object
V(confianca)$sp_in<-distances_sp_in_confianca_vec
```

#Out shortest paths 

```r
distances_dist_out_confianca<-distances(confianca, mode="out", weights=E(confianca)$confianca)

distances_dist_out_confianca[distances_dist_out_confianca=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_confianca_vec <- vector()
for (i in 1:vcount(confianca)) {
    distances_sp_out_confianca_vec[i] <- 
    mean(distances_dist_out_confianca[i,], na.rm = T)
}

#Adding to igraph object
V(confianca)$sp_out<-distances_sp_out_confianca_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
confianca_shortpath_df <- data.frame(distances_sp_in_confianca_vec, distances_sp_out_confianca_vec, distances_sp_all_confianca_vec) %>% round(3)

#Adding type
confianca_shortpath_df <-cbind(confianca_shortpath_df, V(confianca)$LABEL_COR)

#Adding names
names(confianca_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
confianca_shortpath_df<-confianca_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(confianca_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-d528b0017fb7745d1945" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-d528b0017fb7745d1945">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"87.318\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"77.844\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"26.364\" data-max=\"69.733\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[32.876,39.399,27.549,41.261,40.523,41.078,47.569,36.654,53.66,35.922,40.503,38.856,60.105,54.111,44.314,43.32,38.843,37.588,59.144,46.634,42.137,51,41.699,77.484,39.353,43.974,64.758,41.641,41.49,41.549,40.791,44.052,39.954,65.725,41.575,51.503,37.889,42.033,56.085,43.15,42.464,45.974,46.673,37.052,46.993,40.353,81.654,53.418,53.418,53.418,53.418,53.418,62.673,48.078,63.889,41.444,43.209,56.235,62.248,39.216,41.143,71.346,43.732,43.987,50.902,0,87.318,43.928,35.033,44.693,43.399,43.039,43.248,42.758,42.523,44,43.556,39.261,69.66,69.66,69.66,69.66,43.542,53.797,48.431,42.974,46.601,81.863,43.111,44.523,44.092,77.641,68.673,48.013,43.046,51.32,81.863,54.412,42.699,42.405,69.569,52.752,56.065,56.065,44.02,43.791,43.902,55.922,54.601,43.542,43.882,42.085,43.431,43.242,42.948,43.954,44.033,44.026,43.412,42.529,43.948,44.124,43.216,43.627,43.216,43.431,43.627,43.458,43.373,47.725,43.83,43.503,43.366,43.497,43.209,41.699,43.771,43.935,43.078,44.144,43.523,44.105,44.261,44.163,50.98,39.098,43.556,43.699,53.856,48.196,43.81,48.196,43.81,48.196,42.02,73.529,47.812,43.643,43.799,81.351,76.481,62.156,59.658,60.819,81.773,86.903,62.753,81.773,81.773,62.5,64.857,81.773,61.63,62.396,58.838,58.39,61.117,81.773,61.117,65.682,62.662,62.662,58.695,60.201,65.864,72.429,51.214],[31.398,38.715,33.473,41.457,41.726,48.677,49.833,40.425,40.742,49.968,47.376,54.387,41.925,44.667,43.306,50.344,38.108,42.392,50.86,46.269,45.296,46.124,45.22,43.538,50.565,67.833,48.511,46.505,42.753,43.054,43.887,48.957,38.591,68.737,52.855,44.5,44.661,56.005,51.129,38.634,41.774,41.263,41.28,47.274,44.312,44.161,59.473,50.113,50.113,50.113,50.113,50.113,52.511,52.156,42.774,44.376,47.204,53.661,60.247,44.102,0,68.978,44.849,48.844,45.172,10,15.333,45.801,51.984,52.011,53.989,52.903,45.527,49.64,52.167,50.984,47.543,43.651,71.005,71.005,71.005,71.005,49.968,67.28,47.64,44.753,49.978,52.505,47.823,54.844,47.134,48.296,48.667,60.737,46,49.737,76.247,67.903,53.866,40.21,69.532,44.952,53.312,53.312,47.419,48.425,51.527,54.613,54.613,48.301,48.059,48.538,47.866,45.978,50.005,39.995,47.038,48.366,47.559,44.984,47.769,41.935,55.022,55.022,55.022,52.844,55.226,54.21,55.226,53.839,48.28,50.269,52.677,54.231,53.237,50.043,56.715,57.215,56.22,51.247,55.581,57.215,55.984,55.226,56.22,57.215,53.247,77.844,63.161,59.489,59.489,59.489,59.489,59.489,68.946,60.763,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[26.364,33.251,27.583,34.556,30.963,31.176,43.626,33.497,38.096,36.048,35.995,36.642,38.882,42.503,36.193,41.979,30.636,34.545,43.385,38.936,38.449,40.984,38.011,40.583,36.722,42.39,40.973,37.754,36.668,35.353,36.781,40.225,33.166,49.69,35.973,39.316,35.417,36.16,44.775,30.674,38.048,37.877,37.503,36.856,32.529,34.61,44.69,43.791,43.791,43.791,43.791,43.791,46.663,39.813,38.701,35.182,39.765,43.62,48.241,35.112,40.102,58.599,39.246,36.877,41.155,69.733,61.963,41.027,30.166,41.08,40.561,41.176,38.497,38.963,39.968,39.898,38.487,36.401,60.535,60.535,60.535,60.535,37.182,44.209,39.123,40.695,41.246,47.358,39.743,41,40.128,45.123,42.909,44.578,32.299,41.267,61.519,50.925,40.594,35.182,55.556,32.668,39.476,39.476,40.225,32.487,32.556,33.048,33.027,38.481,40.519,40.717,39.209,40.123,40.802,35.134,40.219,40.545,40.08,38.652,40.583,37.112,40.781,41.727,40.781,40.684,41.294,41.289,40.813,40.85,40.936,40.781,41.321,40.743,40.722,39.856,41.193,41.011,39.754,40.471,40.508,40.898,41.059,40.973,50.059,38.952,41.235,40.144,48.241,43.663,42.198,43.663,42.198,43.663,39.027,46.118,42.866,41.647,42.198,56.15,63.267,54.497,49.947,51.241,60.834,62.299,53.989,60.834,60.834,54.102,56.027,60.834,53.08,54.08,50.733,50.096,52.417,60.834,52.417,55.604,52.572,52.572,50.738,50.251,55.358,62.92,51.326]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(confianca_shortpath_df, by=list(confianca_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(confianca_shortpath_df, by=list(confianca_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-42d765fdd37cc99335ce" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-42d765fdd37cc99335ce">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"27.549\" data-max=\"66.446\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.494\" data-max=\"18.68\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"22.545\" data-max=\"51.991\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.012\" data-max=\"29.832\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"27.583\" data-max=\"52.287\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.264\" data-max=\"9.612\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[45.425,66.446,39.399,43.99,41.144,27.549,46.323,40.579,51.976,48.053,52.598,43.944,37.588],[7.409,11.344,null,6.285,1.382,null,0.494,1.002,18.68,9.694,4.789,1.398,null],[47.842,22.545,38.715,38.936,48.559,33.473,41.272,45.113,48.525,49.822,51.991,49.435,42.392],[13.362,29.832,null,19.36,5.801,null,0.012,4.588,13.577,8.397,2.225,12.078,null],[39.818,52.287,33.251,39.303,38.291,27.583,37.69,34.687,44.367,39.194,40.575,40.051,34.545],[7.692,7.462,null,8.095,0.526,null,0.264,2.268,9.612,8.139,4.337,2.237,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(confianca, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-51f2f597147992899b92" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-51f2f597147992899b92">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["6474","1574","14201","8960","3207","365","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

confianca_dist.from.CAPSAD <- distances(confianca, v=V(confianca)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(confianca), weights=E(confianca)$confianca)

#Saving distance on igraph object 
V(confianca)$confianca_dist.from.CAPSAD<-confianca_dist.from.CAPSAD

confianca_dist.from.CAPSAD[confianca_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(confianca_dist.from.CAPSAD)+1)
col <- col[confianca_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(confianca)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(confianca, es=E(confianca), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(confianca))
maxC <- rep(Inf, vcount(confianca))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(confianca, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(confianca)$confianca)

#Plotting distance from CAPSAD 
plot(confianca, 
     layout=co,
     edge.color=V(confianca)$col[edge.start],
     edge.arrow.size=(betweenness(confianca, weights = E(confianca)$confianca)+1)/100000,
     edge.width=E(confianca)$confianca/10*mean(E(confianca)$confianca),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(confianca))*5,
     vertex.label=confianca_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(confianca))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(confianca)$confianca_dist.from.CAPSAD
b<-V(confianca)$col
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
  title("Distance from CAPS AD - 31_CONFIANÇA", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(confianca, directed=T, unconnected = T)
     )
             )
```

![](31_CONFIANÇA_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_confianca.RData") 
```

