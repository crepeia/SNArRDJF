# SNA Reciprocity Distance Path 15_ACESSO G) O acesso geográfico a este serviço impede a realização de ativades em conjunto. (var13) 
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 15_ACESSO G) O acesso geográfico a este serviço impede a realização de ativades em conjunto. (var13) 

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_var13.RData")
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
#var13<-simplify(var13) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var13, directed=T, unconnected = T)
```

```
## [1] 2.512914
```
##Shortest Paths

```r
#Shortest Paths
var13_sp_in <- shortest.paths(var13, mode='in', weights=E(var13)$var13) #in

var13_sp_out <- shortest.paths(var13, mode='out', weights=E(var13)$var13) # out

var13_sp_all <- shortest.paths(var13, mode='all', weights=E(var13)$var13) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var13_sp_in[which(var13_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   3.000   3.328   4.000  10.000
```

```r
sd(var13_sp_in[which(var13_sp_in != Inf)])
```

```
## [1] 1.482151
```
##Descriptive  Shortest Paths - OUT

```r
summary(var13_sp_out[which(var13_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   3.000   3.328   4.000  10.000
```

```r
sd(var13_sp_out[which(var13_sp_out != Inf)])
```

```
## [1] 1.482151
```

##Descriptive  Shortest Paths - ALL

```r
summary(var13_sp_all[which(var13_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   2.000   2.421   3.000   7.000
```

```r
sd(var13_sp_all[which(var13_sp_all != Inf)])
```

```
## [1] 0.7997755
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var13<-distances(var13, mode="all", weights=E(var13)$var13)
#distances_sp_all_var13

distances_dist_all_var13[distances_dist_all_var13=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var13_vec <- vector()
for (i in 1:vcount(var13)) {
    distances_sp_all_var13_vec[i] <- 
    mean(distances_dist_all_var13[i,],na.rm=T)
}
#Adding to igraph object
V(var13)$sp_all<-distances_sp_all_var13_vec
```

#In shortest paths 

```r
distances_dist_in_var13<-distances(var13, mode="in",weights=E(var13)$var13)
#distances_sp_in_var13

distances_dist_in_var13[distances_dist_in_var13=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var13_vec <- vector()
for (i in 1:vcount(var13)) {
    distances_sp_in_var13_vec[i] <- mean(distances_dist_in_var13[i,], na.rm=T)
}

#Adding to igraph object
V(var13)$sp_in<-distances_sp_in_var13_vec
```

#Out shortest paths 

```r
distances_dist_out_var13<-distances(var13, mode="out", weights=E(var13)$var13)

distances_dist_out_var13[distances_dist_out_var13=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var13_vec <- vector()
for (i in 1:vcount(var13)) {
    distances_sp_out_var13_vec[i] <- 
    mean(distances_dist_out_var13[i,], na.rm = T)
}

#Adding to igraph object
V(var13)$sp_out<-distances_sp_out_var13_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var13_shortpath_df <- data.frame(distances_sp_in_var13_vec, distances_sp_out_var13_vec, distances_sp_all_var13_vec) %>% round(3)

#Adding type
var13_shortpath_df <-cbind(var13_shortpath_df, V(var13)$LABEL_COR)

#Adding names
names(var13_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var13_shortpath_df<-var13_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var13_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-26e4e9d78c43cd111447" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-26e4e9d78c43cd111447">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"6.477\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"6.629\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.444\" data-max=\"5.251\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[2.255,2.544,1.993,2.705,2.664,2.691,2.893,2.779,3.752,2.919,2.671,2.631,3.624,3.51,2.826,2.879,2.651,2.537,3.423,2.846,2.678,3.698,2.651,4.215,4.315,2.94,4.51,2.638,2.711,2.752,2.678,2.812,2.718,3.685,2.718,3.631,2.839,3.315,3.389,2.752,2.738,2.792,2.839,2.886,2.94,2.651,4.523,3.235,3.235,3.235,3.235,3.235,6.45,3.067,3.651,2.718,2.765,3.718,4.537,2.591,2.753,5.812,2.819,2.799,3.483,0,4.62,2.872,3.591,2.826,2.812,2.819,2.805,2.765,2.765,2.799,2.779,2.597,6.336,6.336,6.336,6.336,2.779,3.228,3.383,2.779,3.43,4.557,2.772,2.832,2.826,6.477,5.577,2.852,2.765,3.389,4.557,3.698,2.765,2.752,4.174,3.289,3.38,3.38,2.799,2.819,2.792,3.38,3.38,2.772,2.792,2.765,2.738,2.799,2.792,2.812,2.805,2.819,2.785,2.745,2.826,2.826,2.812,2.826,2.812,2.819,2.819,2.826,2.805,3.181,2.832,2.812,2.819,2.826,2.772,2.745,2.799,2.812,2.785,2.805,2.785,2.812,2.826,2.826,2.933,2.497,2.745,2.792,3.503,3.221,2.826,3.221,2.826,3.221,2.805,4.477,3.167,2.827,2.84,4.493,4.3,3.653,4.517,4.523,4.527,4.596,3.693,4.527,4.527,3.68,3.687,4.527,3.693,3.613,3.513,3.547,3.667,4.527,3.667,4.513,4.527,4.527,3.427,3.687,3.687,6.44,2.973],[1.828,2.269,1.683,2.43,2.462,2.511,2.43,2.371,2.505,2.656,4.339,2.629,2.118,4.242,3.113,2.645,2.194,2.36,2.543,2.608,2.452,2.651,2.559,2.091,2.656,5.113,2.806,2.36,2.301,2.285,2.398,2.495,2.339,3.608,3.591,2.651,3.403,3.113,2.516,2.263,2.301,2.274,2.274,2.597,2.527,2.522,3.538,2.64,2.64,2.64,2.64,2.64,2.742,4.29,2.758,2.613,4.414,2.613,3.409,4.177,0,4.522,2.505,4.565,2.548,0.5,0.667,2.387,3.425,4.64,4.64,2.565,2.527,4.161,4.591,4.312,3.376,2.387,5.274,5.274,5.274,5.274,2.618,3.081,4.312,4.473,2.565,2.78,2.468,4.634,3.548,2.478,3.575,6.059,3.511,2.602,3.769,3.629,4.624,2.441,3.398,2.339,0,0,4.565,2.516,4.629,0,0,3.237,2.478,4.591,2.624,4.624,3.5,2.602,4.527,2.608,3.618,2.586,2.532,2.349,4.651,4.651,4.651,4.581,4.661,2.645,4.661,4.575,4.586,4.586,2.586,2.672,2.672,2.522,4.634,2.672,2.672,4.586,2.667,2.672,2.634,2.672,2.672,4.661,2.597,6.629,3.253,5.242,5.242,5.242,5.242,5.242,4.575,3.57,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[1.492,1.947,1.444,2.07,2.064,2.102,2.102,1.968,2.283,2.267,2.209,2.123,2.011,2.861,2.299,2.241,2.011,2,2.262,2.16,2.091,2.283,2.166,1.936,2.31,2.23,2.15,2.032,2.118,2.08,2.128,2.176,2.107,2.743,2.246,2.262,2.294,2.652,2.289,2.048,2.112,2.08,2.08,2.31,2.214,2.182,2.679,2.283,2.283,2.283,2.283,2.283,2.46,2.235,2.294,1.989,2.278,2.128,2.524,2.005,2.048,3.005,2.203,2.294,2.283,4.193,3.267,2.16,2.738,2.273,2.257,2.225,2.096,2.128,2.251,2.219,2.102,2.064,3.615,3.615,3.615,3.615,2.262,2.396,2.626,2.305,2.198,2.439,2.182,2.326,2.31,2.273,3.289,2.38,2.262,2.091,2.995,2.765,2.299,2.139,2.893,2.171,2.904,2.904,2.294,2.241,2.278,2.904,2.904,2.075,2.235,2.294,2.235,2.299,2.305,2.289,2.283,2.283,2.267,2.219,2.262,2.144,2.316,2.321,2.316,2.294,2.321,2.299,2.294,2.444,2.326,2.316,2.273,2.294,2.273,2.235,2.294,2.273,2.246,2.294,2.289,2.278,2.316,2.299,2.385,2.144,2.246,2.31,2.743,2.481,2.326,2.481,2.326,2.481,2.112,2.717,2.471,2.321,2.326,2.631,3.021,2.69,3.203,3.219,2.818,3.278,2.754,2.818,2.818,2.754,2.711,2.818,2.722,2.738,2.572,2.626,2.727,2.818,2.727,2.818,2.818,2.818,2.551,2.706,2.684,5.251,2.433]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var13_shortpath_df, by=list(var13_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var13_shortpath_df, by=list(var13_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-7edbb56a8a24811c26e5" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-7edbb56a8a24811c26e5">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.993\" data-max=\"4.286\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.033\" data-max=\"1.371\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.429\" data-max=\"3.613\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1.976\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.444\" data-max=\"2.798\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"0.549\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[2.986,4.286,2.544,2.737,2.616,1.993,2.816,2.68,3.715,3.26,3.273,2.835,2.537],[0.448,0.97,null,0.245,0.081,null,0.033,0.042,1.371,0.562,0.143,0.113,null],[2.631,1.429,2.269,2.792,3.008,1.683,2.274,2.666,2.865,2.651,1.79,3.613,2.36],[0.539,1.976,null,1.898,1.105,null,0,0.609,0.877,0.4,1.329,1.301,null],[2.321,2.798,1.947,2.102,2.108,1.444,2.08,2.107,2.54,2.325,2.518,2.283,2],[0.366,0.549,null,0.309,0.06,null,0,0.074,0.541,0.261,0.304,0.077,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var13, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-43c057a236b8c7f5f577" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-43c057a236b8c7f5f577">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["7214","1555","14084","8470","3153","305","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

var13_dist.from.CAPSAD <- distances(var13, v=V(var13)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(var13), weights=E(var13)$var13)

#Saving distance on igraph object 
V(var13)$var13_dist.from.CAPSAD<-var13_dist.from.CAPSAD

var13_dist.from.CAPSAD[var13_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(var13_dist.from.CAPSAD)+1)
col <- col[var13_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(var13)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(var13, es=E(var13), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var13))
maxC <- rep(Inf, vcount(var13))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var13, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var13)$var13)

#Plotting distance from CAPSAD 
plot(var13, 
     layout=co,
     edge.color=V(var13)$col[edge.start],
     edge.arrow.size=(betweenness(var13, weights = E(var13)$var13)+1)/100000,
     edge.width=E(var13)$var13/10*mean(E(var13)$var13),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(var13))*5,
     vertex.label=var13_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(var13))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var13)$var13_dist.from.CAPSAD
b<-V(var13)$col
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
  title("Distance from CAPS AD - 15_ACESSO G) O acesso geográfico a este serviço impede a realização de ativades em conjunto. (var13) ", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(var13, directed=T, unconnected = T)
     )
             )
```

![](15_ACESSO_G_O_acesso_geográfico_impede_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_var13.RData") 
```

