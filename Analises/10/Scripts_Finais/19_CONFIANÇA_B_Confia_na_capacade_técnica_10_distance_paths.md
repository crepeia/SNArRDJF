# SNA Reciprocity Distance Path 19_CONFIANÇA B) Confia na capacade técnica da equipe deste serviço. (var17)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 19_CONFIANÇA B) Confia na capacade técnica da equipe deste serviço. (var17)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_var17.RData")
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
#var17<-simplify(var17) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var17, directed=T, unconnected = T)
```

```
## [1] 2.512914
```
##Shortest Paths

```r
#Shortest Paths
var17_sp_in <- shortest.paths(var17, mode='in', weights=E(var17)$var17) #in

var17_sp_out <- shortest.paths(var17, mode='out', weights=E(var17)$var17) # out

var17_sp_all <- shortest.paths(var17, mode='all', weights=E(var17)$var17) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var17_sp_in[which(var17_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    5.00    6.00    6.23    8.00   15.00
```

```r
sd(var17_sp_in[which(var17_sp_in != Inf)])
```

```
## [1] 2.248261
```
##Descriptive  Shortest Paths - OUT

```r
summary(var17_sp_out[which(var17_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    5.00    6.00    6.23    8.00   15.00
```

```r
sd(var17_sp_out[which(var17_sp_out != Inf)])
```

```
## [1] 2.248261
```

##Descriptive  Shortest Paths - ALL

```r
summary(var17_sp_all[which(var17_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   4.000   5.000   5.072   6.000  12.000
```

```r
sd(var17_sp_all[which(var17_sp_all != Inf)])
```

```
## [1] 1.811478
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var17<-distances(var17, mode="all", weights=E(var17)$var17)
#distances_sp_all_var17

distances_dist_all_var17[distances_dist_all_var17=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var17_vec <- vector()
for (i in 1:vcount(var17)) {
    distances_sp_all_var17_vec[i] <- 
    mean(distances_dist_all_var17[i,],na.rm=T)
}
#Adding to igraph object
V(var17)$sp_all<-distances_sp_all_var17_vec
```

#In shortest paths 

```r
distances_dist_in_var17<-distances(var17, mode="in",weights=E(var17)$var17)
#distances_sp_in_var17

distances_dist_in_var17[distances_dist_in_var17=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var17_vec <- vector()
for (i in 1:vcount(var17)) {
    distances_sp_in_var17_vec[i] <- mean(distances_dist_in_var17[i,], na.rm=T)
}

#Adding to igraph object
V(var17)$sp_in<-distances_sp_in_var17_vec
```

#Out shortest paths 

```r
distances_dist_out_var17<-distances(var17, mode="out", weights=E(var17)$var17)

distances_dist_out_var17[distances_dist_out_var17=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var17_vec <- vector()
for (i in 1:vcount(var17)) {
    distances_sp_out_var17_vec[i] <- 
    mean(distances_dist_out_var17[i,], na.rm = T)
}

#Adding to igraph object
V(var17)$sp_out<-distances_sp_out_var17_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var17_shortpath_df <- data.frame(distances_sp_in_var17_vec, distances_sp_out_var17_vec, distances_sp_all_var17_vec) %>% round(3)

#Adding type
var17_shortpath_df <-cbind(var17_shortpath_df, V(var17)$LABEL_COR)

#Adding names
names(var17_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var17_shortpath_df<-var17_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var17_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-5356724b76ed0e24a128" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-5356724b76ed0e24a128">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"10.407\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"10.081\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.85\" data-max=\"8.941\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[4.067,4.799,3.463,4.651,4.617,4.631,6.007,4.973,6.785,5.195,4.597,4.523,6.846,6.503,5.43,5.322,4.537,4.725,7.242,5.557,5.275,6.128,5.221,9.436,9.987,5.389,8.107,5.255,4.671,4.691,4.624,5.403,4.617,8.087,4.664,6.577,5.013,6.06,6.107,5.517,5.329,5.859,5.631,5.02,5.859,4.591,9.953,6.255,6.255,6.255,6.255,6.255,6.966,5.832,7.477,4.664,5.322,8.181,7.644,4.779,5.013,8.718,5.349,5.389,5.993,0,10.407,5.57,7.409,5.497,5.389,5.342,5.369,5.295,5.268,5.376,5.315,4.53,7.208,7.208,7.208,7.208,5.295,6.477,6.295,5.329,5.926,9.711,5.383,5.45,5.409,9.443,8.376,5.805,5.262,6.443,9.711,6.799,5.302,5.128,8.987,6.215,6.107,6.107,5.396,5.369,5.383,6.107,5.973,5.322,5.383,5.275,5.322,5.376,5.322,5.383,5.383,5.383,5.289,5.221,5.383,5.463,5.349,5.416,5.349,5.376,5.416,5.396,5.383,5.772,5.443,5.403,5.383,5.396,5.362,5.268,5.356,5.362,5.315,5.376,5.302,5.403,5.409,5.403,6.403,5.134,5.403,5.376,6.463,5.832,5.436,5.832,5.436,5.832,5.094,9.081,5.813,5.42,5.44,9.64,9,7.68,7.371,7.45,9.967,10.351,8.22,9.967,9.967,8.127,8.067,9.967,8.133,7.06,6.073,6.073,8.1,9.967,8.1,8.08,7.447,7.447,6.327,7.353,8.127,8.933,6.42],[3.349,4.957,4.161,5.118,4.785,6.253,6.038,4.398,4.511,5.952,5.737,6.93,5.172,5.317,5.414,5.801,4.618,3.952,5.833,5.866,4.769,5.07,4.183,5.532,6.07,8.204,6.29,5.817,5.156,4.731,4.806,5.774,4.081,8.022,7.941,4.188,4.522,7.527,6.747,3.532,5.156,4.559,4.565,5.844,4.29,4.71,7.591,5.763,5.763,5.763,5.763,5.763,6.145,6.581,5.425,5.462,5.694,6.812,7.849,5.392,0,8.22,5.909,6.602,5.495,1.5,1.333,5.511,6.269,7.097,7.097,6.618,5.978,6.215,6.554,6.274,6.005,4.817,9.661,9.661,9.661,9.661,6.048,8.5,6.731,4.887,5.855,6.258,5.624,6.731,5.973,5.726,5.339,7.108,5.71,6.011,9.226,7.118,5.866,4.93,8.376,5.71,0,0,5.989,6.242,6.699,0,0,5.871,5.898,5.188,6.038,5.086,6.016,4.011,5.866,5.892,5.957,5.957,5.984,5.027,7.102,7.102,7.102,6.806,7.129,6.608,7.129,6.806,5.946,5.946,6.618,7.129,7.129,5.817,7.065,7.129,7.129,6.941,6.806,7.129,6.93,7.129,6.134,7.129,6.941,10.081,7.581,7.925,7.925,7.925,7.925,7.925,8.903,7.876,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[2.85,3.845,3.412,3.663,3.54,3.781,5.444,4.096,4.316,5.059,3.684,3.572,4.904,5.112,4.278,5.182,3.508,3.529,5.027,4.155,4.059,4.551,3.599,5.209,5.535,5.134,5.38,4.048,3.722,3.599,3.738,4.642,3.524,6.053,3.791,3.727,4.171,5.374,5.337,3.283,4.155,4.299,4.246,4.856,3.829,3.54,5.92,5.38,5.38,5.38,5.38,5.38,5.444,4.62,5.021,3.706,4.647,5.583,5.904,4.053,4.893,6.973,4.684,4.717,4.995,8.941,7.476,4.193,5.374,4.743,4.674,4.749,4.594,4.545,4.615,4.604,4.519,3.599,6.219,6.219,6.219,6.219,4.668,5.508,5.246,4.556,5,5.717,4.551,4.733,4.69,5.417,4.984,4.251,4.551,5.075,7.856,5.995,4.604,4.118,7.513,5.118,5.513,5.513,4.717,4.743,4.738,5.513,5.337,4.519,4.706,4.631,4.583,4.556,4.684,3.615,4.711,4.711,4.626,4.599,4.717,4.251,4.759,4.765,4.759,4.775,4.781,4.765,4.727,4.775,4.61,4.599,4.781,4.738,4.765,4.529,4.722,4.706,4.647,4.679,4.604,4.69,4.711,4.738,5.369,4.84,4.155,4.695,5.465,4.824,4.781,4.824,4.781,4.824,4.807,4.872,4.824,4.781,4.781,7.155,6.866,6.674,5.973,6.102,7.807,7.952,6.781,7.807,7.807,6.759,6.786,7.807,6.786,6.011,4.797,4.781,6.754,7.807,6.754,6.727,6.053,6.053,5.187,6.043,6.711,7.963,6.38]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var17_shortpath_df, by=list(var17_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var17_shortpath_df, by=list(var17_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-b7257947f6b76e61b73f" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-b7257947f6b76e61b73f">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.463\" data-max=\"7.998\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.056\" data-max=\"2.119\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.901\" data-max=\"6.204\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.004\" data-max=\"3.877\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.412\" data-max=\"6.261\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.037\" data-max=\"1.306\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[5.47,7.998,4.799,5.395,5.221,3.463,5.745,4.615,6.404,6.732,6.088,5.407,4.725],[0.931,1.413,null,0.799,0.062,null,0.161,0.056,2.119,1.541,0.26,0.14,null],[5.606,2.901,4.957,4.669,5.474,4.161,4.562,5.466,5.586,5.79,4.074,6.204,3.952],[1.793,3.877,null,2.359,1.294,null,0.004,1.014,1.61,1.283,3.056,1.616,null],[4.381,6.261,3.845,4.464,4.136,3.412,4.273,3.649,5.356,4.877,5.389,4.665,3.529],[0.99,0.966,null,1.13,0.516,null,0.037,0.096,1.306,1.139,0.093,0.162,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var17, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-b35ec8aba94a6cddb64e" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-b35ec8aba94a6cddb64e">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["7214","1555","14084","8470","3153","305","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

var17_dist.from.CAPSAD <- distances(var17, v=V(var17)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(var17), weights=E(var17)$var17)

#Saving distance on igraph object 
V(var17)$var17_dist.from.CAPSAD<-var17_dist.from.CAPSAD

var17_dist.from.CAPSAD[var17_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(var17_dist.from.CAPSAD)+1)
col <- col[var17_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(var17)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(var17, es=E(var17), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var17))
maxC <- rep(Inf, vcount(var17))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var17, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var17)$var17)

#Plotting distance from CAPSAD 
plot(var17, 
     layout=co,
     edge.color=V(var17)$col[edge.start],
     edge.arrow.size=(betweenness(var17, weights = E(var17)$var17)+1)/100000,
     edge.width=E(var17)$var17/10*mean(E(var17)$var17),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(var17))*5,
     vertex.label=var17_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(var17))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var17)$var17_dist.from.CAPSAD
b<-V(var17)$col
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
  title("Distance from CAPS AD - 19_CONFIANÇA B) Confia na capacade técnica da equipe deste serviço. (var17)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(var17, directed=T, unconnected = T)
     )
             )
```

![](19_CONFIANÇA_B_Confia_na_capacade_técnica_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_var17.RData") 
```

