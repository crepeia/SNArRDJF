# SNA Reciprocity Distance Path 27_ATIVIDADE
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 27_ATIVIDADE

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_atividade.RData")
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
#atividade<-simplify(atividade) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(atividade, directed=T, unconnected = T)
```

```
## [1] 2.605711
```
##Shortest Paths

```r
#Shortest Paths
atividade_sp_in <- shortest.paths(atividade, mode='in', weights=E(atividade)$atividade) #in

atividade_sp_out <- shortest.paths(atividade, mode='out', weights=E(atividade)$atividade) # out

atividade_sp_all <- shortest.paths(atividade, mode='all', weights=E(atividade)$atividade) # all
```
##Descriptive Shortest Paths - IN

```r
summary(atividade_sp_in[which(atividade_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   3.000   5.000   4.938   6.000  16.000
```

```r
sd(atividade_sp_in[which(atividade_sp_in != Inf)])
```

```
## [1] 2.279815
```
##Descriptive  Shortest Paths - OUT

```r
summary(atividade_sp_out[which(atividade_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   3.000   5.000   4.938   6.000  16.000
```

```r
sd(atividade_sp_out[which(atividade_sp_out != Inf)])
```

```
## [1] 2.279815
```

##Descriptive  Shortest Paths - ALL

```r
summary(atividade_sp_all[which(atividade_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   3.000   3.000   3.666   4.000  14.000
```

```r
sd(atividade_sp_all[which(atividade_sp_all != Inf)])
```

```
## [1] 1.762059
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_atividade<-distances(atividade, mode="all", weights=E(atividade)$atividade)
#distances_sp_all_atividade

distances_dist_all_atividade[distances_dist_all_atividade=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_atividade_vec <- vector()
for (i in 1:vcount(atividade)) {
    distances_sp_all_atividade_vec[i] <- 
    mean(distances_dist_all_atividade[i,],na.rm=T)
}
#Adding to igraph object
V(atividade)$sp_all<-distances_sp_all_atividade_vec
```

#In shortest paths 

```r
distances_dist_in_atividade<-distances(atividade, mode="in",weights=E(atividade)$atividade)
#distances_sp_in_atividade

distances_dist_in_atividade[distances_dist_in_atividade=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_atividade_vec <- vector()
for (i in 1:vcount(atividade)) {
    distances_sp_in_atividade_vec[i] <- mean(distances_dist_in_atividade[i,], na.rm=T)
}

#Adding to igraph object
V(atividade)$sp_in<-distances_sp_in_atividade_vec
```

#Out shortest paths 

```r
distances_dist_out_atividade<-distances(atividade, mode="out", weights=E(atividade)$atividade)

distances_dist_out_atividade[distances_dist_out_atividade=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_atividade_vec <- vector()
for (i in 1:vcount(atividade)) {
    distances_sp_out_atividade_vec[i] <- 
    mean(distances_dist_out_atividade[i,], na.rm = T)
}

#Adding to igraph object
V(atividade)$sp_out<-distances_sp_out_atividade_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
atividade_shortpath_df <- data.frame(distances_sp_in_atividade_vec, distances_sp_out_atividade_vec, distances_sp_all_atividade_vec) %>% round(3)

#Adding type
atividade_shortpath_df <-cbind(atividade_shortpath_df, V(atividade)$LABEL_COR)

#Adding names
names(atividade_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
atividade_shortpath_df<-atividade_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(atividade_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-8fc24e2d00dd337bf293" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-8fc24e2d00dd337bf293">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"9.638\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"11.173\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"9.161\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[3.04,3.638,2.564,3.584,3.497,3.591,4.584,3.946,6.832,4.49,3.443,3.349,5.356,5.383,4.295,4.49,3.443,3.181,5.396,3.973,4.356,5.181,4.047,5.772,8.154,3.523,4.826,4.832,3.658,3.523,3.497,4.47,3.336,6.114,3.678,4.336,3.758,6.02,6.06,3.128,3.846,4.121,4.06,3.859,4.376,3.577,6.617,6.081,6.081,6.081,6.081,6.081,6.02,4.114,4.57,3.638,3.866,4.322,5.685,3.765,3.307,9.631,4.617,4.45,4.047,0,8.513,3.45,4.517,4.852,4.275,4.154,4.503,4.483,4.201,4.443,4.342,3.537,9.195,9.195,9.195,9.195,4.383,3.805,4.376,4.268,4.208,9.221,4.859,4.993,4.711,6.356,9.638,4.732,3.812,4.423,9.221,6.846,4.208,4.128,6.248,4.221,6.06,6.06,4.47,4.443,4.436,6.06,6.047,4.664,4.812,4.383,4.168,4.262,4.262,4.45,4.846,4.852,4.45,3.933,4.638,3.611,4.081,4.611,4.081,4.369,4.503,4.503,4.477,4.369,4.644,4.51,4.161,4.47,4.141,4.255,4.047,4.416,4.134,4.403,4.852,4.51,4.523,4.839,5.329,3.953,4.101,4.114,6.195,4.779,4.644,4.779,4.644,4.779,3.322,6.007,4.653,4.52,4.633,6.253,5.18,4.88,5.318,4.543,6.613,8.477,5.593,6.613,6.613,5.187,5.04,6.613,6.58,5.887,5.56,5.547,5.32,6.613,5.32,6.06,4.573,4.573,4.36,4.46,5.353,6.007,0],[2.876,3.443,4.238,3.676,3.557,4.038,7.308,3.989,4.465,3.708,4.892,4.178,3.395,3.427,6.195,6.357,4.649,3.043,4.076,4.346,4.168,4.622,4.703,4.811,4.211,5.146,3.746,3.908,4.189,4.259,3.481,3.751,3.503,4.827,5.741,4.632,3.238,4.584,5.341,4.53,3.735,4.795,4.795,5.216,3.778,3.238,4.395,6.373,6.373,6.373,6.373,6.373,3.746,5.973,4.168,4.53,3.265,4.427,4.492,3.076,0,9.914,3.378,6.162,5.259,2,2,3.816,4.573,5.184,5.184,4.946,3.643,4,5.573,4.141,3.681,3.989,7.659,8.638,8.638,8.638,3.843,6.784,3.789,3.681,3.654,3.789,3.546,5.13,4.838,3.492,8.919,4.995,3.481,3.795,7.746,8.47,5.114,3.292,7.065,4.616,0,0,3.676,4.427,5.941,0,0,3.405,3.578,3.541,3.778,3.8,3.541,3.546,3.605,3.708,5.524,3.551,3.714,4.162,5.2,5.195,5.2,4.162,7.2,4.022,6.205,4.162,4.222,4.222,3.989,7.2,5.216,4.054,7.611,6.205,6.211,4.216,6.151,7.2,5.222,6.205,8.2,7.205,5.222,8.162,4.941,4.427,4.427,4.427,4.427,4.427,11.173,11.151,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[2.253,2.597,2.344,2.742,2.699,2.892,3.602,2.909,3.645,3.032,2.833,2.806,2.962,2.812,3.774,4.118,2.935,2.441,3.124,3.312,3.29,3.844,3.43,4.167,3.435,3.323,3.188,3.22,2.892,2.79,2.742,2.855,2.672,3.656,3.108,3.608,2.763,3.656,4.516,2.909,3.124,3.618,3.489,2.898,3.177,2.72,3.478,4.769,4.769,4.769,4.769,4.769,3.172,3.425,3.382,2.914,2.731,3.129,3.21,2.522,2.866,9.161,2.694,3.495,3.516,8.565,6.495,3,3.118,3.183,3.022,3.355,2.876,3.382,3.414,3.306,2.828,2.93,6.817,7.796,7.796,7.796,3.027,2.968,2.941,3.14,2.763,3.231,3.059,3.167,3.93,3,8.172,4.242,2.849,3.075,6.909,5.855,3.102,2.79,4.79,3.403,4.758,4.758,2.93,3.554,3.5,4.758,4.758,2.769,2.919,2.892,3.016,3.151,2.86,2.898,2.957,2.973,3.5,2.952,2.941,2.898,3.151,3.242,3.151,3.054,4.043,3.414,3.72,3.054,3.113,3.108,3.323,3.817,3.21,3.016,3.36,3.618,3.344,3.038,3.941,3.704,3.683,3.962,4.113,3.392,3.366,3.409,3.86,3.522,3.522,3.522,3.522,3.522,3.065,3.473,4.054,4.054,4.091,3.946,3.984,3.376,4.608,3.71,4.194,7.301,3.726,4.194,4.194,3.656,3.892,4.194,4.188,4.124,3.801,4.14,3.882,4.194,3.882,4.048,3.565,3.565,3.522,3.247,3.5,5.495,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(atividade_shortpath_df, by=list(atividade_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(atividade_shortpath_df, by=list(atividade_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-1d8690ad4fae81cc6dca" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-1d8690ad4fae81cc6dca">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.564\" data-max=\"5.934\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.043\" data-max=\"2.488\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.347\" data-max=\"4.996\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"3.394\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.344\" data-max=\"4.597\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.091\" data-max=\"2.184\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[4.459,5.805,3.638,3.46,4.297,2.564,4.091,3.54,5.6,4.776,5.934,4.445,3.181],[1.525,1.534,null,1.624,0.396,null,0.043,0.095,2.488,1.452,0.455,0.264,null],[4.06,2.347,3.443,3.699,4.996,4.238,4.795,4.186,4.947,4.932,4.012,4.532,3.043],[0.772,3.394,null,2.237,1.509,null,0,0.669,2.184,1.419,3.013,1.535,null],[3.219,4.189,2.597,2.674,3.333,2.344,3.553,2.846,4.363,3.394,4.597,3.294,2.441],[0.6,1.338,null,1.36,0.096,null,0.091,0.114,2.184,0.575,0.353,0.369,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(atividade, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-72f073c4037d80078d8a" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-72f073c4037d80078d8a">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["7363","1403","12821","8837","3919","420","19"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

atividade_dist.from.CAPSAD <- distances(atividade, v=V(atividade)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(atividade), weights=E(atividade)$atividade)

#Saving distance on igraph object 
V(atividade)$atividade_dist.from.CAPSAD<-atividade_dist.from.CAPSAD

atividade_dist.from.CAPSAD[atividade_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(atividade_dist.from.CAPSAD)+1)
col <- col[atividade_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(atividade)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(atividade, es=E(atividade), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(atividade))
maxC <- rep(Inf, vcount(atividade))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(atividade, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(atividade)$atividade)

#Plotting distance from CAPSAD 
plot(atividade, 
     layout=co,
     edge.color=V(atividade)$col[edge.start],
     edge.arrow.size=(betweenness(atividade, weights = E(atividade)$atividade)+1)/100000,
     edge.width=E(atividade)$atividade/10*mean(E(atividade)$atividade),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(atividade))*5,
     vertex.label=atividade_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(atividade))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
```

```
## Warning in mean.default(E(atividade)$atividade): argument is not numeric or
## logical: returning NA
```

```r
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(atividade)$atividade_dist.from.CAPSAD
b<-V(atividade)$col
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
  title("Distance from CAPS AD - 27_ATIVIDADE", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(atividade, directed=T, unconnected = T)
     )
             )
```

![](27_ATIVIDADE_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_atividade.RData") 
```

