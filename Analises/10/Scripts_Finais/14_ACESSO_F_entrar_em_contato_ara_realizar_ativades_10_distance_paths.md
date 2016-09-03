# SNA Reciprocity Distance Path 14_ACESSO F) Seu serviço tem facilade de entrar em contato com este serviço para realizar ativades em conjunto (var12).
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 14_ACESSO F) Seu serviço tem facilade de entrar em contato com este serviço para realizar ativades em conjunto (var12).

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_var12.RData")
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
#var12<-simplify(var12) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var12, directed=T, unconnected = T)
```

```
## [1] 2.512914
```
##Shortest Paths

```r
#Shortest Paths
var12_sp_in <- shortest.paths(var12, mode='in', weights=E(var12)$var12) #in

var12_sp_out <- shortest.paths(var12, mode='out', weights=E(var12)$var12) # out

var12_sp_all <- shortest.paths(var12, mode='all', weights=E(var12)$var12) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var12_sp_in[which(var12_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   4.000   6.000   5.906   7.000  15.000
```

```r
sd(var12_sp_in[which(var12_sp_in != Inf)])
```

```
## [1] 2.27411
```
##Descriptive  Shortest Paths - OUT

```r
summary(var12_sp_out[which(var12_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   4.000   6.000   5.906   7.000  15.000
```

```r
sd(var12_sp_out[which(var12_sp_out != Inf)])
```

```
## [1] 2.27411
```

##Descriptive  Shortest Paths - ALL

```r
summary(var12_sp_all[which(var12_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   3.000   4.000   4.345   5.000  12.000
```

```r
sd(var12_sp_all[which(var12_sp_all != Inf)])
```

```
## [1] 1.631598
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var12<-distances(var12, mode="all", weights=E(var12)$var12)
#distances_sp_all_var12

distances_dist_all_var12[distances_dist_all_var12=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var12_vec <- vector()
for (i in 1:vcount(var12)) {
    distances_sp_all_var12_vec[i] <- 
    mean(distances_dist_all_var12[i,],na.rm=T)
}
#Adding to igraph object
V(var12)$sp_all<-distances_sp_all_var12_vec
```

#In shortest paths 

```r
distances_dist_in_var12<-distances(var12, mode="in",weights=E(var12)$var12)
#distances_sp_in_var12

distances_dist_in_var12[distances_dist_in_var12=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var12_vec <- vector()
for (i in 1:vcount(var12)) {
    distances_sp_in_var12_vec[i] <- mean(distances_dist_in_var12[i,], na.rm=T)
}

#Adding to igraph object
V(var12)$sp_in<-distances_sp_in_var12_vec
```

#Out shortest paths 

```r
distances_dist_out_var12<-distances(var12, mode="out", weights=E(var12)$var12)

distances_dist_out_var12[distances_dist_out_var12=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var12_vec <- vector()
for (i in 1:vcount(var12)) {
    distances_sp_out_var12_vec[i] <- 
    mean(distances_dist_out_var12[i,], na.rm = T)
}

#Adding to igraph object
V(var12)$sp_out<-distances_sp_out_var12_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var12_shortpath_df <- data.frame(distances_sp_in_var12_vec, distances_sp_out_var12_vec, distances_sp_all_var12_vec) %>% round(3)

#Adding type
var12_shortpath_df <-cbind(var12_shortpath_df, V(var12)$LABEL_COR)

#Adding names
names(var12_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var12_shortpath_df<-var12_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var12_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-4564e4d2b970747a2049" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-4564e4d2b970747a2049">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"10.673\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"10.392\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.658\" data-max=\"8.08\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[3.423,4.128,2.866,4.98,4.604,4.671,5.02,4.826,5.711,5.376,4.591,4.611,5.926,7.295,5.302,5.591,4.94,3.55,5.852,4.322,4.49,5.342,4.537,8.06,8.591,5.772,7.812,4.517,4.98,4.745,4.866,5.389,4.698,7.691,4.987,5.329,4.933,6.732,6.852,4.43,4.839,5.45,5.383,4.517,5.584,4.839,9.765,6.591,6.591,6.591,6.591,6.591,8.53,5.544,7.745,4.919,5,5.96,7.262,4.356,5.013,7.946,5.255,5.383,5.57,0,10.673,5.047,7.745,5.537,4.718,4.785,4.826,5,4.644,5.148,5.074,4.678,8.711,8.711,8.711,8.711,4.899,5.101,5.812,4.832,4.738,8.765,5.329,5.43,5.336,10.235,7.698,5.013,5.168,5.53,8.765,5.597,4.664,4.678,6.752,6.148,6.847,6.847,5.383,5.383,5.369,6.847,6.067,5.087,5.114,4.671,5.027,4.859,4.826,5.195,5.221,5.215,5.362,5.148,5.295,4.43,5.034,5.134,5.034,5.141,4.832,4.819,4.812,5.577,5.174,4.839,4.792,4.832,4.765,4.812,5.248,5.114,4.926,5.403,5.228,5.121,5.403,5.396,5.785,4.47,4.805,4.349,5.832,5.604,5.168,5.604,5.168,5.604,5.262,9.483,5.067,4.84,5.16,6.813,8.713,6.073,6.728,6.834,9.787,10.623,8.267,9.787,9.787,6.573,7.567,9.787,7.633,7.413,5.38,5.4,6.007,9.787,6.007,6.933,5.84,5.84,6.907,5.98,8.133,8.513,5.827],[4.538,4.177,4.215,4.134,6.048,4.876,5.898,3.914,5.215,6.36,5.608,5.925,4.016,5.285,5.253,5.376,4.441,4.462,6.624,5.758,5.172,7.011,5.349,5.312,5.608,8.161,7.387,4.86,3.817,5.258,4.011,5.215,4.398,10.016,8.129,5.296,4.866,5.059,6.328,3.812,4.952,3.349,3.355,6.14,5.231,5.833,7.532,5.36,5.36,5.36,5.36,5.36,7.022,6.306,4.097,5.145,5.645,6.043,6.608,5.226,0,8.984,4.129,6.753,5.624,1.5,2,4,5.323,7.151,6.161,6.382,4.376,5.86,5.516,4.387,5.349,4.935,7.763,7.763,7.763,7.763,7.048,8.28,4.844,5.177,5.065,7.425,5.919,6.199,5.414,4.704,6.382,7.032,5.473,7.059,10.392,8.242,5.145,5.527,8.301,5.328,0,0,6.919,5.892,6.968,0,0,6.43,4.688,4.43,6.29,6.108,4.866,4.806,5.113,6.177,4.688,4.688,4.478,4.785,7.151,7.151,7.151,5.28,7.183,6.667,7.183,5.28,4.602,4.602,5.392,7.183,7.183,6.258,6.022,7.183,5.194,4.597,6.242,7.183,5.086,7.183,7.183,7.183,6.586,10.134,7.57,6.156,6.156,6.156,6.156,6.156,8.543,7.522,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[2.807,2.979,2.658,3.369,3.529,3.299,3.888,2.92,3.872,4.241,3.439,3.465,3.332,4.807,4.075,3.968,3.444,2.984,4.086,3.524,3.278,3.893,3.283,4.717,4.406,5.476,5.455,3.38,3,3.604,3.027,3.909,3.193,6.112,3.749,3.455,3.583,4.348,5.235,3.021,3.658,2.813,2.807,3.369,3.947,3.711,5.888,3.968,3.968,3.968,3.968,3.968,5.62,4.61,3.241,3.658,3.898,4.102,5.358,3.294,4.481,5.449,3.096,4.952,3.797,8.08,6.166,3.139,4.39,5.433,3.674,4.08,3.171,4.519,3.658,3.39,3.791,3.439,6.449,6.449,6.449,6.449,4.433,3.936,3.529,3.84,3.118,5.701,4.572,4.984,3.781,3.374,5.241,3.936,4.128,3.781,6.283,3.84,3.321,3.497,4.497,4.674,5.834,5.834,4.979,4.824,4.973,5.834,5.102,4.636,3.369,3.075,4.428,3.834,3.332,3.305,3.845,3.93,3.267,3.267,3.209,3.246,4.631,4.749,4.631,4.283,4.134,4.134,4.075,4.283,3.374,3.299,3.829,4.134,4.091,3.829,4.406,3.92,3.497,3.364,4.941,4.754,3.615,4.989,5.599,3.556,3.963,3.904,3.973,4.893,4.759,4.893,4.759,4.893,4.096,4.813,4.134,4.134,4.759,4.262,7.027,4.053,5.112,5.15,6.16,6.193,6.139,6.16,6.16,5.422,5.989,6.16,5.84,5.85,4.278,4.278,4.358,6.16,4.358,5.524,3.968,3.968,5.652,4.064,6.011,6.765,5.626]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var12_shortpath_df, by=list(var12_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var12_shortpath_df, by=list(var12_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-a0b028cc328afa6a142b" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-a0b028cc328afa6a142b">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.866\" data-max=\"7.6\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.029\" data-max=\"2.03\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.654\" data-max=\"6.095\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.004\" data-max=\"3.553\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.658\" data-max=\"5.252\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.004\" data-max=\"1.321\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[5.123,7.6,4.128,4.663,4.503,2.866,5.417,4.801,5.915,5.924,6.562,5.111,3.55],[0.618,1.656,null,0.748,0.029,null,0.047,0.156,2.03,1.334,0.375,0.276,null],[5.64,2.654,4.177,4.854,5.641,4.215,3.352,5.243,6.095,5.707,3.63,5.661,4.462],[1.694,3.553,null,2.269,1.048,null,0.004,1.14,1.986,1.357,2.698,1.615,null],[3.608,5.252,2.979,3.796,3.374,2.658,2.81,3.441,4.441,4.088,4.666,4.115,2.984],[0.392,1.031,null,0.892,0.13,null,0.004,0.231,1.321,0.76,0.833,0.605,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var12, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-3007475d461e21df6c19" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-3007475d461e21df6c19">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["7214","1555","14084","8470","3153","305","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

var12_dist.from.CAPSAD <- distances(var12, v=V(var12)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(var12), weights=E(var12)$var12)

#Saving distance on igraph object 
V(var12)$var12_dist.from.CAPSAD<-var12_dist.from.CAPSAD

var12_dist.from.CAPSAD[var12_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(var12_dist.from.CAPSAD)+1)
col <- col[var12_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(var12)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(var12, es=E(var12), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var12))
maxC <- rep(Inf, vcount(var12))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var12, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var12)$var12)

#Plotting distance from CAPSAD 
plot(var12, 
     layout=co,
     edge.color=V(var12)$col[edge.start],
     edge.arrow.size=(betweenness(var12, weights = E(var12)$var12)+1)/100000,
     edge.width=E(var12)$var12/10*mean(E(var12)$var12),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(var12))*5,
     vertex.label=var12_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(var12))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var12)$var12_dist.from.CAPSAD
b<-V(var12)$col
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
  title("Distance from CAPS AD - 14_ACESSO F) Seu serviço tem facilade de entrar em contato com este serviço para realizar ativades em conjunto (var12).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(var12, directed=T, unconnected = T)
     )
             )
```

![](14_ACESSO_F_entrar_em_contato_ara_realizar_ativades_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_var12.RData") 
```

