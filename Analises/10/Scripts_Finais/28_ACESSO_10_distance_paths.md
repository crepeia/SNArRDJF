# SNA Reciprocity Distance Path 28_ACESSO
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 28_ACESSO

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_acesso.RData")
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
#acesso<-simplify(acesso) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(acesso, directed=T, unconnected = T)
```

```
## [1] 2.504135
```
##Shortest Paths

```r
#Shortest Paths
acesso_sp_in <- shortest.paths(acesso, mode='in', weights=E(acesso)$acesso) #in

acesso_sp_out <- shortest.paths(acesso, mode='out', weights=E(acesso)$acesso) # out

acesso_sp_all <- shortest.paths(acesso, mode='all', weights=E(acesso)$acesso) # all
```
##Descriptive Shortest Paths - IN

```r
summary(acesso_sp_in[which(acesso_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0    42.0    48.0    53.1    65.0   132.0
```

```r
sd(acesso_sp_in[which(acesso_sp_in != Inf)])
```

```
## [1] 18.0845
```
##Descriptive  Shortest Paths - OUT

```r
summary(acesso_sp_out[which(acesso_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0    42.0    48.0    53.1    65.0   132.0
```

```r
sd(acesso_sp_out[which(acesso_sp_out != Inf)])
```

```
## [1] 18.0845
```

##Descriptive  Shortest Paths - ALL

```r
summary(acesso_sp_all[which(acesso_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00   38.00   44.00   45.63   55.00   99.00
```

```r
sd(acesso_sp_all[which(acesso_sp_all != Inf)])
```

```
## [1] 14.16621
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_acesso<-distances(acesso, mode="all", weights=E(acesso)$acesso)
#distances_sp_all_acesso

distances_dist_all_acesso[distances_dist_all_acesso=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_acesso_vec <- vector()
for (i in 1:vcount(acesso)) {
    distances_sp_all_acesso_vec[i] <- 
    mean(distances_dist_all_acesso[i,],na.rm=T)
}
#Adding to igraph object
V(acesso)$sp_all<-distances_sp_all_acesso_vec
```

#In shortest paths 

```r
distances_dist_in_acesso<-distances(acesso, mode="in",weights=E(acesso)$acesso)
#distances_sp_in_acesso

distances_dist_in_acesso[distances_dist_in_acesso=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_acesso_vec <- vector()
for (i in 1:vcount(acesso)) {
    distances_sp_in_acesso_vec[i] <- mean(distances_dist_in_acesso[i,], na.rm=T)
}

#Adding to igraph object
V(acesso)$sp_in<-distances_sp_in_acesso_vec
```

#Out shortest paths 

```r
distances_dist_out_acesso<-distances(acesso, mode="out", weights=E(acesso)$acesso)

distances_dist_out_acesso[distances_dist_out_acesso=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_acesso_vec <- vector()
for (i in 1:vcount(acesso)) {
    distances_sp_out_acesso_vec[i] <- 
    mean(distances_dist_out_acesso[i,], na.rm = T)
}

#Adding to igraph object
V(acesso)$sp_out<-distances_sp_out_acesso_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
acesso_shortpath_df <- data.frame(distances_sp_in_acesso_vec, distances_sp_out_acesso_vec, distances_sp_all_acesso_vec) %>% round(3)

#Adding type
acesso_shortpath_df <-cbind(acesso_shortpath_df, V(acesso)$LABEL_COR)

#Adding names
names(acesso_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
acesso_shortpath_df<-acesso_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(acesso_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-a810eadb83249b4e13d4" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-a810eadb83249b4e13d4">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"89.827\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"80.758\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"28.422\" data-max=\"76.524\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[33.02,40.215,28.054,43.732,41.49,42.913,48.074,43.859,57.134,47.228,40.664,41.98,61.403,43.638,47.087,48.664,42.141,36.188,60.134,44.141,42.295,56.463,42.503,75.49,44.443,48.342,69.013,42.087,44.161,42.121,41.403,47.174,41.57,66.718,44.289,55.362,45.497,42.919,59.336,43.201,41.745,46.812,47.409,43.51,48.389,42.946,83.577,56.101,56.101,56.101,56.101,56.087,75.55,45.329,66.43,44.013,42.919,59.154,65.651,38.779,45.7,75.617,46.846,47.094,53.859,0,89.827,47.101,40.597,47.852,45.262,45.664,45.483,45.423,44.369,46.839,46.349,40.564,78.503,78.503,78.503,78.503,46.51,51.805,53.322,45.154,48.624,85.926,46.517,47.617,46.846,69.181,71.443,48.423,42.893,53.879,85.926,57.53,44.638,41.758,73.483,56.101,59.22,59.22,47.195,46.913,46.966,59.22,58.173,46.423,46.758,44.262,46.007,45.154,45.228,46.812,46.799,46.779,46.389,42.732,47.255,43.154,45.638,46.195,45.638,46.034,45.859,45.691,45.302,50.987,46.45,45.738,45.738,45.987,45.349,44.423,43.215,46.792,45.436,47.221,46.268,46.577,47.221,47.242,50.436,40.43,44.557,42.678,54.711,51.443,46.409,51.443,46.409,51.443,45.96,67.591,50.013,45.887,46.407,83.06,78.027,58.733,61.06,62.245,83.747,89.384,62.887,83.747,83.747,59.6,67.373,83.747,61.927,62.04,57.293,57.12,60.967,83.747,60.947,70.853,60.42,60.42,59.767,57.787,69.627,76.347,50.713],[35.699,38.892,34.43,39.737,49.065,47.382,50.828,39.667,48.274,53.952,55.065,52.446,42.72,47.737,46.484,52.188,39.129,40.231,51.64,49.833,48.194,48.925,46.075,44.07,51.328,68.317,56.43,49.097,40.14,45.613,42.323,49.995,41.118,73.909,53.683,48.156,53.048,55.484,50.177,41.043,42.258,40.565,40.591,49.333,49.269,45.414,69.075,50.484,50.484,50.484,50.484,51.871,52.672,56.925,44.586,42.312,51.204,49.839,61.898,48.758,0,73.78,43.263,57.027,47.376,13,15.333,45.328,53.344,51.968,54.935,53.086,46.371,52.274,57.57,51.156,54.769,45.355,74.726,74.726,74.726,74.726,54.134,67.817,51.382,50.376,45.919,56.645,48.134,57.215,48.344,46.366,52.269,67.038,49.613,48.016,80.387,71.199,55.892,44.054,70.828,47.726,0,0,51.29,48.634,62.866,0,0,50.108,44.699,49.812,53.505,51.134,50.065,46.688,48.742,49.968,49.108,49.898,43.656,45.134,60.978,60.978,60.978,55.371,59.161,53.93,59.161,57.36,50.699,52.108,49.72,55.183,54.188,53.14,61.661,58.167,50.21,50.683,57.43,60.156,51.161,58.167,56.188,64.145,54.086,80.758,64.785,63.645,63.645,63.645,63.645,63.645,74.737,63.634,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[28.588,33.08,28.422,34.968,34.203,34.037,44.679,34.209,42.802,44.973,39.492,40.647,40.658,35.888,39.861,44.043,33.112,32.872,44.861,39.578,39.251,41.374,38.235,41.273,39.936,45.701,45.588,39.508,38.198,38.636,36.663,43.802,35.396,57.267,40.93,40.642,41.583,35.583,45.561,36.668,37.674,37.615,37.487,39.166,43.348,38.567,53.802,43.93,43.93,43.93,43.93,44.615,47.465,41.567,40.193,36.086,41.684,41.786,50.599,36.444,42.813,59.872,37.701,46.449,40.84,76.524,63.225,41.241,33.412,44.449,42.289,44.941,39.401,42.246,42.444,43.417,43.888,38.251,65.583,65.583,65.583,65.583,46.321,43.225,42.193,43.422,37.166,49.882,42.535,47.011,41.503,41.674,46.604,46.684,41.465,38.471,66.23,48.551,42.818,37.096,57.984,44.086,54.834,54.834,44.561,42.722,46.989,54.834,54.021,42.652,39.556,40.856,44.545,41.508,41.957,39.914,43.219,43.289,42.551,41.278,38.567,38.23,46.102,46.743,46.102,45.62,45.428,43.198,44.524,45.957,44.802,44.401,42.326,44.439,43.807,42.925,42.733,46.16,41.385,43.471,46.144,45.62,43.861,47.032,49.834,41.38,44,41.615,48.572,48.674,46.872,48.674,46.872,48.674,43.139,49.209,46.428,45.428,46.872,57.786,63.46,47.62,50.802,51.733,61.497,63.791,54.818,61.497,61.497,52.203,58.337,61.497,52.599,54.524,50.882,50.727,53.578,61.497,53.481,59.898,50.535,50.535,51.684,46.353,58.059,63.594,51.176]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(acesso_shortpath_df, by=list(acesso_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(acesso_shortpath_df, by=list(acesso_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-8a2b0fb7fa1b5c14a3c1" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-8a2b0fb7fa1b5c14a3c1">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"28.054\" data-max=\"68.009\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.422\" data-max=\"19.254\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"23.615\" data-max=\"51.878\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.018\" data-max=\"31.366\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"28.422\" data-max=\"53.663\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.091\" data-max=\"10.843\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[46.009,68.009,40.215,43.558,41.829,28.054,47.111,42.494,55.918,50.145,55.604,46.309,36.188],[7.537,11.752,null,6.228,0.948,null,0.422,1.292,19.254,9.718,4.955,1.801,null],[49.387,23.615,38.892,42.316,51.878,34.43,40.578,45.974,51.214,51.447,34.305,51.798,40.231],[13.347,31.366,null,20.893,8.276,null,0.018,5.345,14.065,7.828,25.376,12.878,null],[40.547,53.663,33.08,40.606,39.593,28.422,37.551,37.215,45.942,43.055,47.004,43.825,32.872],[7.042,7.842,null,7.532,1.312,null,0.091,2.57,10.843,6.665,6.164,2.488,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(acesso, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-abb21888ae3024a2adde" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-abb21888ae3024a2adde">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["7214","1564","14157","8528","3024","294","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

acesso_dist.from.CAPSAD <- distances(acesso, v=V(acesso)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(acesso), weights=E(acesso)$acesso)

#Saving distance on igraph object 
V(acesso)$acesso_dist.from.CAPSAD<-acesso_dist.from.CAPSAD

acesso_dist.from.CAPSAD[acesso_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(acesso_dist.from.CAPSAD)+1)
col <- col[acesso_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(acesso)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(acesso, es=E(acesso), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acesso))
maxC <- rep(Inf, vcount(acesso))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acesso, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acesso)$acesso)

#Plotting distance from CAPSAD 
plot(acesso, 
     layout=co,
     edge.color=V(acesso)$col[edge.start],
     edge.arrow.size=(betweenness(acesso, weights = E(acesso)$acesso)+1)/100000,
     edge.width=E(acesso)$acesso/10*mean(E(acesso)$acesso),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(acesso))*5,
     vertex.label=acesso_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(acesso))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(acesso)$acesso_dist.from.CAPSAD
b<-V(acesso)$col
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
  title("Distance from CAPS AD - 28_ACESSO", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(acesso, directed=T, unconnected = T)
     )
             )
```

![](28_ACESSO_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_acesso.RData") 
```

