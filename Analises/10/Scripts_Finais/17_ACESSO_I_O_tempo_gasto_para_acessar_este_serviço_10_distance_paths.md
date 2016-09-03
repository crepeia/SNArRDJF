# SNA Reciprocity Distance Path 17_ACESSO I) O tempo gasto para acessar este serviço impede a realização ativades em conjunto. (var15)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 17_ACESSO I) O tempo gasto para acessar este serviço impede a realização ativades em conjunto. (var15)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_var15.RData")
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
#var15<-simplify(var15) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var15, directed=T, unconnected = T)
```

```
## [1] 2.512914
```
##Shortest Paths

```r
#Shortest Paths
var15_sp_in <- shortest.paths(var15, mode='in', weights=E(var15)$var15) #in

var15_sp_out <- shortest.paths(var15, mode='out', weights=E(var15)$var15) # out

var15_sp_all <- shortest.paths(var15, mode='all', weights=E(var15)$var15) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var15_sp_in[which(var15_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   4.000   3.529   4.000   9.000
```

```r
sd(var15_sp_in[which(var15_sp_in != Inf)])
```

```
## [1] 1.419038
```
##Descriptive  Shortest Paths - OUT

```r
summary(var15_sp_out[which(var15_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   4.000   3.529   4.000   9.000
```

```r
sd(var15_sp_out[which(var15_sp_out != Inf)])
```

```
## [1] 1.419038
```

##Descriptive  Shortest Paths - ALL

```r
summary(var15_sp_all[which(var15_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   2.000   2.536   3.000   7.000
```

```r
sd(var15_sp_all[which(var15_sp_all != Inf)])
```

```
## [1] 0.8576831
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var15<-distances(var15, mode="all", weights=E(var15)$var15)
#distances_sp_all_var15

distances_dist_all_var15[distances_dist_all_var15=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var15_vec <- vector()
for (i in 1:vcount(var15)) {
    distances_sp_all_var15_vec[i] <- 
    mean(distances_dist_all_var15[i,],na.rm=T)
}
#Adding to igraph object
V(var15)$sp_all<-distances_sp_all_var15_vec
```

#In shortest paths 

```r
distances_dist_in_var15<-distances(var15, mode="in",weights=E(var15)$var15)
#distances_sp_in_var15

distances_dist_in_var15[distances_dist_in_var15=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var15_vec <- vector()
for (i in 1:vcount(var15)) {
    distances_sp_in_var15_vec[i] <- mean(distances_dist_in_var15[i,], na.rm=T)
}

#Adding to igraph object
V(var15)$sp_in<-distances_sp_in_var15_vec
```

#Out shortest paths 

```r
distances_dist_out_var15<-distances(var15, mode="out", weights=E(var15)$var15)

distances_dist_out_var15[distances_dist_out_var15=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var15_vec <- vector()
for (i in 1:vcount(var15)) {
    distances_sp_out_var15_vec[i] <- 
    mean(distances_dist_out_var15[i,], na.rm = T)
}

#Adding to igraph object
V(var15)$sp_out<-distances_sp_out_var15_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var15_shortpath_df <- data.frame(distances_sp_in_var15_vec, distances_sp_out_var15_vec, distances_sp_all_var15_vec) %>% round(3)

#Adding type
var15_shortpath_df <-cbind(var15_shortpath_df, V(var15)$LABEL_COR)

#Adding names
names(var15_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var15_shortpath_df<-var15_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var15_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-1ed0b9f7ea1d33a79efb" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-1ed0b9f7ea1d33a79efb">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"6.631\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"5.651\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.476\" data-max=\"5.856\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[2.403,2.792,2.161,2.913,2.826,2.879,3.027,2.96,4.899,3.047,2.872,2.799,3.987,3.725,3.013,3.034,2.846,2.718,3.604,3.007,2.839,5.523,2.846,4.55,4.47,3.107,4.691,2.826,2.94,2.913,2.899,3.02,2.805,3.846,2.906,4.691,3.04,3.564,3.577,2.899,2.879,2.966,2.993,3.02,3.101,2.839,4.698,3.517,3.517,3.517,3.517,3.503,6.631,3.342,3.839,2.926,2.926,3.879,4.691,2.705,2.887,5.953,3.013,3.007,3.658,0,4.807,3.047,3.799,3.074,3.02,3,2.946,2.946,2.96,3.013,2.973,2.765,4.47,4.47,4.47,4.47,2.946,3.564,3.503,3.007,3.443,5.893,2.987,3.047,3.007,4.705,5.893,3.027,2.953,3.591,5.893,5.557,2.973,2.852,4.302,3.557,3.567,3.567,3.007,3.027,3,3.567,3.567,2.966,2.96,2.98,2.906,2.966,3.02,2.993,3,3.013,2.98,2.893,3.027,2.98,2.98,3.013,2.98,3,3.007,3.007,2.966,3.503,3.027,2.993,3,3.013,2.987,3.013,2.987,3.02,2.966,3.013,2.919,2.98,3.027,3.027,3.094,2.705,2.886,2.993,3.725,3.604,3.034,3.604,3.034,3.604,2.94,4.497,3.473,3.013,3.033,5.427,4.533,3.847,4.669,4.715,4.713,4.781,3.907,4.713,4.713,3.867,3.847,4.713,3.82,3.793,4.687,4.733,4.673,4.713,4.673,4.693,4.673,4.673,3.573,3.873,3.88,6.613,3.14],[2.844,2.038,1.699,2.446,2.478,2.522,2.441,3.269,3.409,3.242,3.28,2.645,3.07,2.301,2.629,4.376,2.043,2.414,2.559,3.145,2.468,3.618,2.575,2.124,3.941,4.21,4.801,4.312,2.656,2.355,2.441,2.538,2.462,4.597,4.586,3.618,4.425,3.349,2.5,2.28,2.387,2.312,2.312,2.634,2.892,3.548,5.565,3.22,3.22,3.22,3.22,4.376,3.532,4.28,3.366,2.484,4.522,4.527,3.511,4.237,0,3.565,2.511,4.597,4.505,0.5,0.667,2.828,2.844,2.677,2.677,2.548,4.511,4.516,4.602,3.634,4.629,2.457,5.43,5.43,5.43,5.43,3.608,3.113,4.457,4.522,2.64,3.774,2.446,2.64,3.376,2.516,2.591,5.462,3.634,3.817,4.763,4.597,4.613,2.468,3.543,3.317,0,0,4.618,2.5,4.667,0,0,3.398,3.457,2.634,2.667,4.672,2.661,4.661,4.591,2.597,4.651,4.651,2.559,2.645,4.667,4.667,4.667,4.559,4.677,2.656,4.677,4.559,4.565,4.565,2.677,2.688,2.688,2.527,4.672,2.688,3.683,4.565,4.672,4.677,2.683,2.688,2.688,4.677,2.575,5.651,3.199,4.022,4.022,4.022,4.022,4.022,4.554,3.548,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[2,1.834,1.476,2.182,2.102,2.16,2.176,2.182,3.193,2.321,2.332,2.187,2.968,2.091,2.396,2.358,1.856,2.08,2.267,2.214,2.15,2.711,2.267,1.973,3.118,2.257,2.845,2.294,2.273,2.166,2.198,2.257,2.139,2.807,2.369,2.679,2.433,2.668,2.294,2.08,2.102,2.123,2.107,2.278,2.332,2.316,2.85,2.465,2.465,2.465,2.465,2.717,2.925,2.46,2.364,2.032,2.401,2.754,2.556,2.048,2.043,2.973,2.225,2.422,2.888,4.262,3.337,2.267,2.524,2.401,2.316,2.299,2.144,2.15,2.267,2.412,2.235,2.123,3.487,3.487,3.487,3.487,2.31,2.316,2.513,2.369,2.23,2.973,2.209,2.422,2.353,2.299,2.39,2.396,2.428,2.492,3.941,3.636,2.332,2.144,3.016,2.663,2.759,2.759,2.422,2.316,2.406,2.759,2.759,2.348,2.342,2.294,2.283,2.326,2.316,2.422,2.428,2.337,2.433,2.374,2.283,2.262,2.353,2.364,2.353,2.342,2.369,2.348,2.326,2.722,2.374,2.358,2.348,2.364,2.348,2.337,2.433,2.433,2.364,2.417,2.39,2.401,2.396,2.439,2.417,2.235,2.273,2.406,2.898,2.824,2.374,2.824,2.374,2.824,2.15,2.615,2.765,2.369,2.374,2.947,3.251,2.722,3.273,3.289,2.866,3.348,2.85,2.866,2.866,2.786,2.775,2.866,2.786,2.775,2.866,3.337,2.861,2.866,2.861,2.866,2.856,2.856,2.588,2.738,2.754,5.856,2.465]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var15_shortpath_df, by=list(var15_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var15_shortpath_df, by=list(var15_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-64f7794fd5989ca4f983" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-64f7794fd5989ca4f983">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.161\" data-max=\"4.339\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.019\" data-max=\"1.608\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.593\" data-max=\"3.606\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.173\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.476\" data-max=\"2.896\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.011\" data-max=\"0.602\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[3.136,4.339,2.792,2.88,2.804,2.161,2.979,2.871,4.284,3.431,3.501,3.041,2.718],[0.511,0.655,null,0.254,0.067,null,0.019,0.053,1.608,0.563,0.15,0.158,null],[2.683,1.593,2.038,2.942,3.508,1.699,2.312,2.765,3.362,2.919,2.29,3.606,2.414],[0.449,2.173,null,1.694,1.15,null,0,0.673,0.988,0.489,1.766,1.199,null],[2.38,2.896,1.834,2.206,2.236,1.476,2.115,2.177,2.819,2.474,2.578,2.386,2.08],[0.449,0.583,null,0.171,0.063,null,0.011,0.136,0.602,0.331,0.175,0.133,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var15, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-5e2dee5653e068fa5f9d" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-5e2dee5653e068fa5f9d">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["7214","1555","14084","8470","3153","305","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

var15_dist.from.CAPSAD <- distances(var15, v=V(var15)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(var15), weights=E(var15)$var15)

#Saving distance on igraph object 
V(var15)$var15_dist.from.CAPSAD<-var15_dist.from.CAPSAD

var15_dist.from.CAPSAD[var15_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(var15_dist.from.CAPSAD)+1)
col <- col[var15_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(var15)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(var15, es=E(var15), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var15))
maxC <- rep(Inf, vcount(var15))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var15, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var15)$var15)

#Plotting distance from CAPSAD 
plot(var15, 
     layout=co,
     edge.color=V(var15)$col[edge.start],
     edge.arrow.size=(betweenness(var15, weights = E(var15)$var15)+1)/100000,
     edge.width=E(var15)$var15/10*mean(E(var15)$var15),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(var15))*5,
     vertex.label=var15_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(var15))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var15)$var15_dist.from.CAPSAD
b<-V(var15)$col
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
  title("Distance from CAPS AD - 17_ACESSO I) O tempo gasto para acessar este serviço impede a realização ativades em conjunto. (var15)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(var15, directed=T, unconnected = T)
     )
             )
```

![](17_ACESSO_I_O_tempo_gasto_para_acessar_este_serviço_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_var15.RData") 
```

