# SNA Reciprocity Distance Path 29_ACESSO_POSITIVO
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 29_ACESSO_POSITIVO

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_acesso_positivo.RData")
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
#acesso_positivo<-simplify(acesso_positivo) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(acesso_positivo, directed=T, unconnected = T)
```

```
## [1] 2.504135
```
##Shortest Paths

```r
#Shortest Paths
acesso_positivo_sp_in <- shortest.paths(acesso_positivo, mode='in', weights=E(acesso_positivo)$acesso_positivo) #in

acesso_positivo_sp_out <- shortest.paths(acesso_positivo, mode='out', weights=E(acesso_positivo)$acesso_positivo) # out

acesso_positivo_sp_all <- shortest.paths(acesso_positivo, mode='all', weights=E(acesso_positivo)$acesso_positivo) # all
```
##Descriptive Shortest Paths - IN

```r
summary(acesso_positivo_sp_in[which(acesso_positivo_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00   30.00   36.00   39.02   48.00   97.00
```

```r
sd(acesso_positivo_sp_in[which(acesso_positivo_sp_in != Inf)])
```

```
## [1] 13.84502
```
##Descriptive  Shortest Paths - OUT

```r
summary(acesso_positivo_sp_out[which(acesso_positivo_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00   30.00   36.00   39.02   48.00   97.00
```

```r
sd(acesso_positivo_sp_out[which(acesso_positivo_sp_out != Inf)])
```

```
## [1] 13.84502
```

##Descriptive  Shortest Paths - ALL

```r
summary(acesso_positivo_sp_all[which(acesso_positivo_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00   26.00   32.00   32.89   38.00   75.00
```

```r
sd(acesso_positivo_sp_all[which(acesso_positivo_sp_all != Inf)])
```

```
## [1] 10.59552
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_acesso_positivo<-distances(acesso_positivo, mode="all", weights=E(acesso_positivo)$acesso_positivo)
#distances_sp_all_acesso_positivo

distances_dist_all_acesso_positivo[distances_dist_all_acesso_positivo=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_acesso_positivo_vec <- vector()
for (i in 1:vcount(acesso_positivo)) {
    distances_sp_all_acesso_positivo_vec[i] <- 
    mean(distances_dist_all_acesso_positivo[i,],na.rm=T)
}
#Adding to igraph object
V(acesso_positivo)$sp_all<-distances_sp_all_acesso_positivo_vec
```

#In shortest paths 

```r
distances_dist_in_acesso_positivo<-distances(acesso_positivo, mode="in",weights=E(acesso_positivo)$acesso_positivo)
#distances_sp_in_acesso_positivo

distances_dist_in_acesso_positivo[distances_dist_in_acesso_positivo=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_acesso_positivo_vec <- vector()
for (i in 1:vcount(acesso_positivo)) {
    distances_sp_in_acesso_positivo_vec[i] <- mean(distances_dist_in_acesso_positivo[i,], na.rm=T)
}

#Adding to igraph object
V(acesso_positivo)$sp_in<-distances_sp_in_acesso_positivo_vec
```

#Out shortest paths 

```r
distances_dist_out_acesso_positivo<-distances(acesso_positivo, mode="out", weights=E(acesso_positivo)$acesso_positivo)

distances_dist_out_acesso_positivo[distances_dist_out_acesso_positivo=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_acesso_positivo_vec <- vector()
for (i in 1:vcount(acesso_positivo)) {
    distances_sp_out_acesso_positivo_vec[i] <- 
    mean(distances_dist_out_acesso_positivo[i,], na.rm = T)
}

#Adding to igraph object
V(acesso_positivo)$sp_out<-distances_sp_out_acesso_positivo_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
acesso_positivo_shortpath_df <- data.frame(distances_sp_in_acesso_positivo_vec, distances_sp_out_acesso_positivo_vec, distances_sp_all_acesso_positivo_vec) %>% round(3)

#Adding type
acesso_positivo_shortpath_df <-cbind(acesso_positivo_shortpath_df, V(acesso_positivo)$LABEL_COR)

#Adding names
names(acesso_positivo_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
acesso_positivo_shortpath_df<-acesso_positivo_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(acesso_positivo_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-44703fc8a9cda47d6696" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-44703fc8a9cda47d6696">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"69.007\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"61.183\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"20.412\" data-max=\"54.92\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[23.973,29.57,20.215,32.49,30.564,31.738,36.242,32.181,39.953,35.362,29.557,31.114,44.899,32.906,34.322,37.087,31.262,25.678,45.114,32.121,31.134,39.517,31.53,56.309,33.752,36.671,48.315,31.195,32.852,30.785,30.181,34.416,30.55,49.604,32.966,38.711,33.523,32.094,44.859,31.785,30.624,35.248,35.644,31.946,36.168,31.846,63.302,41.765,41.765,41.765,41.765,41.765,53.477,31.819,50.47,32.732,30.537,44.758,48.309,28.107,34.4,57.087,34.181,34.396,38.705,0,69.007,35.295,30.604,34.872,32.685,33.295,33.208,33.174,32.154,34.201,33.846,29.738,59.772,59.772,59.772,59.772,33.993,37.557,39.423,32.671,34.866,62.826,33.96,34.725,34.228,53.557,52.638,36.799,30.57,38.617,62.826,40.389,32.255,30.43,54.758,41.537,44.773,44.773,34.456,34.242,34.275,44.773,43.893,33.826,34.067,31.993,33.53,32.772,32.711,34.228,34.215,34.181,33.94,30.544,34.477,31.06,33.268,33.671,33.268,33.564,33.416,33.268,33.074,37.268,33.879,33.329,33.302,33.497,33.027,32.128,30.631,34.154,32.779,34.45,33.705,33.906,34.503,34.456,38.691,29.765,33.745,29.819,40.208,37.584,33.832,37.584,33.832,37.584,34.497,52.161,36.46,33.427,33.833,60.54,59.293,41.753,44.093,45.43,63.393,68.675,45.5,63.393,63.393,42.613,50.573,63.393,45.107,42.793,39.967,39.787,42,63.393,41.98,50.393,43.607,43.607,43.087,41.14,51.6,52.353,38.953],[26.145,29.129,26.828,29.5,36.624,35.468,38.71,27.237,36.425,40.489,40.247,40.059,30.441,36.849,35.29,38.194,30.043,28.903,40.242,36.462,37.059,36.097,33.806,33.581,37.091,48.473,39,33.629,28.634,33.538,31.398,36.548,29.312,56.688,39.64,36.382,37.71,40.134,37.812,30.984,31.28,30.774,30.785,33.672,36.043,33.817,47.925,37.957,37.957,37.957,37.957,37.957,38.909,40.823,32.758,32.527,35.177,35.306,46.296,36.253,0,55.559,31.156,40.505,32.844,9.5,12.667,32.048,39.317,39.484,40.473,41.242,32.296,36.435,41.651,36.22,38.344,31.935,55.919,55.919,55.919,55.919,40.866,52.382,37.441,34.71,31.941,42.387,36.247,42.801,35.317,34.113,38.715,48.973,34.038,34.575,61.183,54.452,39.575,33.038,53.339,34.054,0,0,34.505,37.124,46.237,0,0,34.231,30.602,35.011,39.366,35.022,35.527,31.812,34.349,37.796,33.419,33.097,31.452,34.048,46.473,46.473,46.473,40.253,42.64,41.72,42.64,41.247,35.349,36.93,37.14,42.64,40.651,39.914,44.892,45.624,36.672,34.129,42.909,45.624,36.758,43.634,44.629,47.613,41.887,59.333,48.382,46.946,46.946,46.946,46.946,46.946,56.618,48.527,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[20.412,23.979,20.642,25.647,25.866,25.722,33.743,23.369,30.706,33.385,27.642,29.818,28.385,26.749,29.273,32.492,25.225,22.824,33.679,28.251,28.594,29.465,27.631,30.968,29.187,34.471,32.096,27.401,26.791,27.198,26.412,31.316,24.957,43.444,30.369,29.342,29.299,26.406,33.583,27.059,26.957,27.995,27.909,27.171,31.128,28.209,38.695,32.476,32.476,32.476,32.476,32.476,34.497,29.289,28.904,26.947,29.102,28.947,37.444,25.872,32.439,45.091,26.465,32.738,28.064,54.92,47.262,28.668,25.321,32.144,29.636,32.807,27.166,29.936,30.123,30.15,31.898,26.845,48.775,48.775,48.775,48.775,33.866,31.738,30.251,29.909,25.406,36.92,31.246,34.203,30.112,30.011,33.203,34.797,28.171,26.786,47.08,32.128,29.668,26.979,43.182,31.053,41.471,41.471,28.759,31.246,34.064,41.471,40.684,28.909,26.139,27.914,32.048,27.754,28.733,26.294,29.775,31.615,28.257,27.326,26.888,27.027,33.652,34.075,33.652,33.027,32.487,31.802,32.011,33.267,30.102,30.69,30.396,32.417,31.572,30.422,29.727,33.791,28.572,28.663,33.465,33.267,30.171,33.754,38.187,30.316,33.011,28.733,35.417,35.321,34.176,35.321,34.176,35.321,32.193,37.182,33.84,33.091,34.176,41.481,44.973,33.492,36.123,37.695,46.037,47.599,40.455,46.037,46.037,37.487,44.096,46.037,37.428,37.957,35.032,34.508,36.733,46.037,36.642,44.096,35.85,35.85,37.717,32.893,43.332,41.914,39.439]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(acesso_positivo_shortpath_df, by=list(acesso_positivo_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(acesso_positivo_shortpath_df, by=list(acesso_positivo_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-3b5693407b63d4044396" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-3b5693407b63d4044396">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"20.215\" data-max=\"50.224\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.28\" data-max=\"14.074\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"17.51\" data-max=\"38.517\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.008\" data-max=\"23.289\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"20.642\" data-max=\"39.015\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.061\" data-max=\"8.147\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[33.794,50.224,29.57,32.375,30.906,20.215,35.446,31.371,40.549,37.501,41.756,33.664,25.678],[5.555,9.666,null,5.33,0.78,null,0.28,1.178,14.074,7.28,3.774,1.509,null],[36.325,17.51,29.129,31.369,38.027,26.828,30.779,34.11,37.651,38.517,25.494,37.666,28.903],[10.488,23.289,null,15.425,6.582,null,0.008,4.027,11.131,6.47,18.838,9.756,null],[29.11,39.015,23.979,29.928,28.486,20.642,27.952,27.13,32.735,31.591,34.996,31.046,22.824],[5.553,6.16,null,6.419,1.325,null,0.061,1.563,8.147,5.289,4.972,2.472,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(acesso_positivo, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-73e3fdb319ed9f7ea806" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-73e3fdb319ed9f7ea806">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["7214","1564","14157","8528","3024","294","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

acesso_positivo_dist.from.CAPSAD <- distances(acesso_positivo, v=V(acesso_positivo)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(acesso_positivo), weights=E(acesso_positivo)$acesso_positivo)

#Saving distance on igraph object 
V(acesso_positivo)$acesso_positivo_dist.from.CAPSAD<-acesso_positivo_dist.from.CAPSAD

acesso_positivo_dist.from.CAPSAD[acesso_positivo_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(acesso_positivo_dist.from.CAPSAD)+1)
col <- col[acesso_positivo_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(acesso_positivo)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(acesso_positivo, es=E(acesso_positivo), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acesso_positivo))
maxC <- rep(Inf, vcount(acesso_positivo))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acesso_positivo, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acesso_positivo)$acesso_positivo)

#Plotting distance from CAPSAD 
plot(acesso_positivo, 
     layout=co,
     edge.color=V(acesso_positivo)$col[edge.start],
     edge.arrow.size=(betweenness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo)+1)/100000,
     edge.width=E(acesso_positivo)$acesso_positivo/10*mean(E(acesso_positivo)$acesso_positivo),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(acesso_positivo))*5,
     vertex.label=acesso_positivo_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(acesso_positivo))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(acesso_positivo)$acesso_positivo_dist.from.CAPSAD
b<-V(acesso_positivo)$col
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
  title("Distance from CAPS AD - 29_ACESSO_POSITIVO", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(acesso_positivo, directed=T, unconnected = T)
     )
             )
```

![](29_ACESSO_POSITIVO_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_acesso_positivo.RData") 
```

