# SNA Reciprocity Distance Path 25_CONFIANÇA H) Espera que o relacionamento com este serviço continue nos próximos anos (var23).
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 25_CONFIANÇA H) Espera que o relacionamento com este serviço continue nos próximos anos (var23).

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_var23.RData")
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
#var23<-simplify(var23) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var23, directed=T, unconnected = T)
```

```
## [1] 2.512914
```
##Shortest Paths

```r
#Shortest Paths
var23_sp_in <- shortest.paths(var23, mode='in', weights=E(var23)$var23) #in

var23_sp_out <- shortest.paths(var23, mode='out', weights=E(var23)$var23) # out

var23_sp_all <- shortest.paths(var23, mode='all', weights=E(var23)$var23) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var23_sp_in[which(var23_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   6.000   6.000   7.235   9.000  18.000
```

```r
sd(var23_sp_in[which(var23_sp_in != Inf)])
```

```
## [1] 2.41371
```
##Descriptive  Shortest Paths - OUT

```r
summary(var23_sp_out[which(var23_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   6.000   6.000   7.235   9.000  18.000
```

```r
sd(var23_sp_out[which(var23_sp_out != Inf)])
```

```
## [1] 2.41371
```

##Descriptive  Shortest Paths - ALL

```r
summary(var23_sp_all[which(var23_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   5.000   6.000   6.312   8.000  12.000
```

```r
sd(var23_sp_all[which(var23_sp_all != Inf)])
```

```
## [1] 1.944106
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var23<-distances(var23, mode="all", weights=E(var23)$var23)
#distances_sp_all_var23

distances_dist_all_var23[distances_dist_all_var23=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var23_vec <- vector()
for (i in 1:vcount(var23)) {
    distances_sp_all_var23_vec[i] <- 
    mean(distances_dist_all_var23[i,],na.rm=T)
}
#Adding to igraph object
V(var23)$sp_all<-distances_sp_all_var23_vec
```

#In shortest paths 

```r
distances_dist_in_var23<-distances(var23, mode="in",weights=E(var23)$var23)
#distances_sp_in_var23

distances_dist_in_var23[distances_dist_in_var23=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var23_vec <- vector()
for (i in 1:vcount(var23)) {
    distances_sp_in_var23_vec[i] <- mean(distances_dist_in_var23[i,], na.rm=T)
}

#Adding to igraph object
V(var23)$sp_in<-distances_sp_in_var23_vec
```

#Out shortest paths 

```r
distances_dist_out_var23<-distances(var23, mode="out", weights=E(var23)$var23)

distances_dist_out_var23[distances_dist_out_var23=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var23_vec <- vector()
for (i in 1:vcount(var23)) {
    distances_sp_out_var23_vec[i] <- 
    mean(distances_dist_out_var23[i,], na.rm = T)
}

#Adding to igraph object
V(var23)$sp_out<-distances_sp_out_var23_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var23_shortpath_df <- data.frame(distances_sp_in_var23_vec, distances_sp_out_var23_vec, distances_sp_all_var23_vec) %>% round(3)

#Adding type
var23_shortpath_df <-cbind(var23_shortpath_df, V(var23)$LABEL_COR)

#Adding names
names(var23_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var23_shortpath_df<-var23_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var23_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-9c7b718bb1777b24aef4" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-9c7b718bb1777b24aef4">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"11.7\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"10.699\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.604\" data-max=\"10.278\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[4.644,5.362,3.805,5.913,5.819,5.94,6.389,5.255,7.966,5.503,5.852,5.765,8.644,7.879,6.302,6.128,5.752,5.336,8.383,6.443,5.772,7.859,5.738,10.617,11.081,6.705,8.933,5.732,5.98,6.02,5.899,6.255,5.792,8.926,5.98,7.732,5.49,6.597,6.671,6.081,5.94,6.221,6.282,5.443,6.644,5.785,10.906,6.537,6.537,6.537,6.537,6.537,10.55,7.128,8.779,5.98,6.154,9.047,8.336,5.477,6.24,9.409,6.235,6.255,7.591,0,11.7,6.356,8.55,6.356,6.201,6.168,6.161,6.121,6.101,6.255,6.195,5.698,10.45,10.45,10.45,10.45,6.188,7.658,7.55,6.161,6.812,11.443,6.201,6.329,6.228,10.819,9.242,6.49,6.168,7.497,11.443,8.034,6.121,5.98,10.134,7.584,6.667,6.667,6.255,6.248,6.235,6.667,6.587,6.188,6.228,6.094,6.168,6.161,6.161,6.235,6.255,6.255,6.208,6.067,6.255,6.329,6.168,6.221,6.168,6.181,6.221,6.201,6.181,7.081,6.242,6.201,6.188,6.201,6.174,6.101,6.255,6.255,6.161,6.268,6.161,6.268,6.289,6.275,6.725,5.403,5.886,6.221,8.423,7.161,6.242,7.161,6.242,7.161,6.369,10.081,7.073,6.22,6.24,11.327,11.053,8.873,8.497,8.675,10.933,11.642,7.16,10.933,10.933,8.9,8.867,10.933,8.9,8.76,8.433,8.353,8.953,10.933,8.953,8.933,8.72,8.72,8.28,8.593,8.953,10.52,6.76],[4.876,5.581,4.398,5.941,6.742,6.871,6.64,6.258,6.339,7.231,7.022,7.156,5.935,6.027,5.849,7.231,5.317,6.263,7.005,7.28,6.72,7.328,7.016,5.855,7.403,9.333,7.656,6.108,5.79,6.349,6.602,6.866,6.425,10.172,10.124,7.328,6.306,7.344,6.952,6.151,5.946,6.194,6.194,7.038,7.156,6.817,9.828,7.22,7.22,7.22,7.22,7.22,7.441,6.882,5.828,6.285,6.694,7.199,8.204,6.156,0,9.973,6.968,6.806,6.871,1.5,2,6.634,6.79,5.355,7.333,7.081,7.016,7.016,7.167,7.075,7.172,6.306,9.855,9.855,9.855,9.855,7.204,8.823,6.935,6.968,7.038,7.731,6.806,7.269,7.129,6.747,7.194,8.946,6.973,7.183,10.699,10.263,7.285,4.64,9.672,6.489,0,0,7.108,6.925,7.28,0,0,6.935,6.93,7.14,7.215,7.253,7.22,7.043,6.952,6.935,7.161,7.161,7.048,6.57,7.333,7.333,7.333,7.145,7.366,7.269,7.366,7.145,7.161,7.161,7.129,7.366,6.371,6.957,7.296,7.366,7.366,7.161,7.349,7.366,7.306,7.366,7.366,7.366,7.161,10.317,8.726,8.548,8.548,8.548,8.548,8.548,9.118,8.097,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[3.85,4.364,3.604,5.455,5.588,5.684,5.877,4.845,6.053,5.348,5.85,5.503,5.578,5.824,5.364,5.043,4.941,4.652,6.235,6.059,4.791,6.422,4.807,5.455,6.503,6.123,6.048,4.754,5.572,5.722,5.77,6.112,5.433,7.893,5.995,6.31,5.128,4.995,5.043,5.668,5.642,5.754,5.599,5.246,6.214,5.813,7.695,5.037,5.037,5.037,5.037,5.037,6.679,6.107,5.283,5.561,6.107,5.947,6.241,4.519,5.807,8.604,6.182,6.182,6.166,10.278,8.203,5.936,6.134,4.567,6.326,6.294,5.963,5.936,6.241,6.193,5.84,5.684,8.674,8.674,8.674,8.674,6.299,6.198,5.754,6.353,5.62,6.711,6.075,6.332,6.31,6.225,6.449,6.422,6.246,5.834,8.529,7.481,6.337,4.107,8.39,5.989,5.086,5.086,6.294,6.235,6.267,5.086,5.086,5.84,6.289,6.289,5.781,6.332,6.342,6.23,6.241,6.219,6.283,6.203,6.289,5.979,6.332,6.39,6.332,6.294,6.39,6.348,6.294,6.294,6.406,6.374,6.337,6.316,5.465,6.262,6.332,6.267,6.16,6.257,6.278,6.337,6.358,6.342,6.513,4.824,5.829,5.866,7.733,6.733,6.406,6.733,6.406,6.733,5.727,6.337,6.717,6.39,6.406,7.695,8.631,7.818,7.31,7.481,8.102,8.235,5.225,8.102,8.102,7.952,7.898,8.102,7.936,7.829,6.807,6.765,7.861,8.102,7.861,7.834,7.422,7.422,7.364,7.342,7.759,9.134,6.572]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var23_shortpath_df, by=list(var23_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var23_shortpath_df, by=list(var23_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-743dd42f9f1418ad7d2f" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-743dd42f9f1418ad7d2f">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.805\" data-max=\"9.33\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.043\" data-max=\"2.616\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.157\" data-max=\"7.141\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"4.183\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.604\" data-max=\"7.342\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.029\" data-max=\"1.379\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[6.718,9.33,5.362,6.026,5.661,3.805,6.252,5.876,7.568,7.614,6.556,6.291,5.336],[1.478,1.404,null,0.739,0.173,null,0.043,0.104,2.616,1.736,0.147,0.27,null],[7.032,3.157,5.581,5.505,6.803,4.398,6.194,6.717,7.141,7.127,4.802,6.915,6.263],[1.486,4.183,null,2.833,0.533,null,0,1.149,1.939,0.93,3.548,1.636,null],[6.269,7.342,4.364,5.344,4.794,3.604,5.676,5.626,6.479,6.308,5.051,6.225,4.652],[1.272,1.051,null,1.148,0.03,null,0.11,0.255,1.379,0.738,0.029,0.312,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var23, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-5e52de8d1f5a0569ae8d" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-5e52de8d1f5a0569ae8d">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["7214","1555","14084","8470","3153","305","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

var23_dist.from.CAPSAD <- distances(var23, v=V(var23)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(var23), weights=E(var23)$var23)

#Saving distance on igraph object 
V(var23)$var23_dist.from.CAPSAD<-var23_dist.from.CAPSAD

var23_dist.from.CAPSAD[var23_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(var23_dist.from.CAPSAD)+1)
col <- col[var23_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(var23)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(var23, es=E(var23), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var23))
maxC <- rep(Inf, vcount(var23))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var23, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var23)$var23)

#Plotting distance from CAPSAD 
plot(var23, 
     layout=co,
     edge.color=V(var23)$col[edge.start],
     edge.arrow.size=(betweenness(var23, weights = E(var23)$var23)+1)/100000,
     edge.width=E(var23)$var23/10*mean(E(var23)$var23),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(var23))*5,
     vertex.label=var23_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(var23))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var23)$var23_dist.from.CAPSAD
b<-V(var23)$col
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
  title("Distance from CAPS AD - 25_CONFIANÇA H) Espera que o relacionamento com este serviço continue nos próximos anos (var23).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(var23, directed=T, unconnected = T)
     )
             )
```

![](25_CONFIANÇA_H_Relacionamento_continue_nos_próximos_anos_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_var23.RData") 
```

