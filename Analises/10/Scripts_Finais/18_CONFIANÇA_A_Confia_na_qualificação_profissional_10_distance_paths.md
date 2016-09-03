# SNA Reciprocity Distance Path 18_CONFIANÇA A) Confia na qualificação profissional da equipe deste serviço.(var16)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 18_CONFIANÇA A) Confia na qualificação profissional da equipe deste serviço.(var16)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_var16.RData")
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
#var16<-simplify(var16) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var16, directed=T, unconnected = T)
```

```
## [1] 2.512914
```
##Shortest Paths

```r
#Shortest Paths
var16_sp_in <- shortest.paths(var16, mode='in', weights=E(var16)$var16) #in

var16_sp_out <- shortest.paths(var16, mode='out', weights=E(var16)$var16) # out

var16_sp_all <- shortest.paths(var16, mode='all', weights=E(var16)$var16) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var16_sp_in[which(var16_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   5.000   5.000   5.994   8.000  14.000
```

```r
sd(var16_sp_in[which(var16_sp_in != Inf)])
```

```
## [1] 2.218671
```
##Descriptive  Shortest Paths - OUT

```r
summary(var16_sp_out[which(var16_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   5.000   5.000   5.994   8.000  14.000
```

```r
sd(var16_sp_out[which(var16_sp_out != Inf)])
```

```
## [1] 2.218671
```

##Descriptive  Shortest Paths - ALL

```r
summary(var16_sp_all[which(var16_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   4.000   5.000   4.969   6.000  12.000
```

```r
sd(var16_sp_all[which(var16_sp_all != Inf)])
```

```
## [1] 1.714123
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var16<-distances(var16, mode="all", weights=E(var16)$var16)
#distances_sp_all_var16

distances_dist_all_var16[distances_dist_all_var16=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var16_vec <- vector()
for (i in 1:vcount(var16)) {
    distances_sp_all_var16_vec[i] <- 
    mean(distances_dist_all_var16[i,],na.rm=T)
}
#Adding to igraph object
V(var16)$sp_all<-distances_sp_all_var16_vec
```

#In shortest paths 

```r
distances_dist_in_var16<-distances(var16, mode="in",weights=E(var16)$var16)
#distances_sp_in_var16

distances_dist_in_var16[distances_dist_in_var16=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var16_vec <- vector()
for (i in 1:vcount(var16)) {
    distances_sp_in_var16_vec[i] <- mean(distances_dist_in_var16[i,], na.rm=T)
}

#Adding to igraph object
V(var16)$sp_in<-distances_sp_in_var16_vec
```

#Out shortest paths 

```r
distances_dist_out_var16<-distances(var16, mode="out", weights=E(var16)$var16)

distances_dist_out_var16[distances_dist_out_var16=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var16_vec <- vector()
for (i in 1:vcount(var16)) {
    distances_sp_out_var16_vec[i] <- 
    mean(distances_dist_out_var16[i,], na.rm = T)
}

#Adding to igraph object
V(var16)$sp_out<-distances_sp_out_var16_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var16_shortpath_df <- data.frame(distances_sp_in_var16_vec, distances_sp_out_var16_vec, distances_sp_all_var16_vec) %>% round(3)

#Adding type
var16_shortpath_df <-cbind(var16_shortpath_df, V(var16)$LABEL_COR)

#Adding names
names(var16_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var16_shortpath_df<-var16_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var16_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-9d1bad008e8ab9563734" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-9d1bad008e8ab9563734">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"10.58\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"9.473\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.898\" data-max=\"8.947\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[3.832,4.369,3.362,4.826,4.591,4.792,5.121,4.993,6.812,5.168,4.664,4.604,7.087,6.53,5.027,5.221,4.664,4.289,7.349,4.993,4.644,6.584,4.584,9.215,10.067,5.289,7.638,4.577,4.839,4.705,4.685,5.007,4.584,7.718,4.826,6.631,5.087,5.799,5.819,4.96,4.685,5.107,5.128,4.993,5.275,4.678,10.027,6.309,6.309,6.309,6.309,6.309,7.195,5.51,7.651,4.826,4.826,7.846,7.369,4.503,4.98,8.034,4.973,4.993,6.221,0,10.58,5.087,7.55,5.054,4.96,4.819,4.933,4.899,4.886,4.993,4.953,4.503,7.148,7.148,7.148,7.148,4.926,6.255,6.235,4.933,5.879,9.94,4.987,5.047,5.007,9.47,7.805,5.101,4.685,6.443,9.94,6.819,4.906,4.664,9.02,6.188,5.82,5.82,4.993,4.839,4.993,5.82,5.693,4.96,4.987,4.886,4.953,4.94,4.926,4.993,4.993,4.993,4.812,4.671,4.993,4.946,4.94,4.973,4.94,4.946,4.973,4.96,4.946,5.604,4.993,4.966,4.953,4.96,4.94,4.772,4.725,4.993,4.946,5.007,4.94,5.007,5.013,5.013,5.322,4.544,4.819,4.879,6.497,5.644,4.98,5.644,4.98,5.644,5.054,9.067,5.627,4.973,4.987,9.86,9.147,7.68,7.51,7.629,10.033,10.523,7.807,10.033,10.033,7.713,7.573,10.033,7.713,6.687,5.44,5.487,7.747,10.033,7.747,7.733,7.62,7.62,7.18,7.56,7.76,9.16,5.327],[3.608,5.048,3.323,5.091,5.866,5.892,5.683,4.505,4.586,6.161,5.065,6.183,5.124,5.339,5.473,5.548,4.892,4.597,5.043,6.097,4.876,5.253,4.097,5.054,6.312,7.769,6.516,5.602,4.78,4.677,4.833,5.898,4.29,8.156,8.118,4.366,5.237,6.769,6.075,3.704,4.613,4.516,4.516,5.995,6.102,4.909,6.398,5.522,5.522,5.522,5.522,5.522,6.328,5.946,5.005,5.554,5.812,6.086,7.403,5.339,0,7.78,6.048,6.097,4.231,1.5,1.333,5.634,5.86,6.269,6.269,6.113,6.07,5.833,6.086,6.038,6.161,5.602,8.887,8.887,8.887,8.887,6.183,8.022,5.957,4.968,6.022,6.505,5.785,6.237,5.172,4.995,4.941,7.091,5.898,6.151,9.473,7.306,6.081,4.441,7.898,5.505,0,0,6.134,5.828,6.156,0,0,5.989,5.984,6.134,6.194,5.226,6.199,4.14,6.038,6.027,4.591,6.124,6.108,4.866,6.28,6.28,6.28,6.199,6.296,5.57,6.296,6.194,5.215,5.215,6.188,6.296,6.296,6.054,6.258,6.296,6.296,6.21,6.285,6.296,6.22,6.296,5.301,6.296,6.21,9.253,7.543,8.016,8.016,8.016,8.016,8.016,8.172,7.151,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[2.92,3.775,2.898,4.225,4.021,4.385,4.62,4.107,4.316,4.663,4.289,4.053,4.439,5.059,4.406,4.471,4.139,3.845,4.679,4.112,4.021,4.642,3.422,4.909,5.594,4.695,5.422,3.989,4.198,4.016,4.225,4.508,3.46,6.027,4.487,3.834,4.342,5.198,5.332,3.31,3.968,4.187,4.123,4.513,4.701,4.037,5.251,4.802,4.802,4.802,4.802,4.802,4.722,4.588,4.684,4.305,4.455,5.294,5.904,4.118,4.524,6.914,4.529,4.62,3.749,8.947,7.535,4.166,5.267,4.636,4.599,4.39,4.481,4.417,4.551,4.54,4.449,4.021,5.93,5.93,5.93,5.93,4.61,5.422,4.957,4.594,5.032,5.829,4.417,4.642,4.599,4.636,4.545,4.209,4.294,5.112,7.39,6.011,4.594,3.594,7.214,5.139,5.385,5.385,4.615,4.342,4.626,5.385,5.235,4.433,4.626,4.551,4.567,4.604,4.572,3.636,4.556,4.578,3.775,4.321,4.61,4.096,4.647,4.658,4.647,4.636,4.658,4.353,4.631,4.807,4.647,4.636,4.62,4.642,4.647,4.348,4.401,4.62,4.572,4.636,4.62,4.604,4.615,4.642,4.856,4.348,4.155,4.54,5.893,4.893,4.663,4.893,4.663,4.893,4.428,4.861,4.888,4.658,4.663,6.765,6.572,6.647,5.979,6.545,7.524,7.615,6.754,7.524,7.524,6.733,6.545,7.524,6.738,5.941,4.738,4.733,6.722,7.524,6.722,6.717,6.449,6.449,6.235,6.465,6.647,6.717,4.877]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var16_shortpath_df, by=list(var16_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var16_shortpath_df, by=list(var16_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-670bbee7d94aa12dc2d2" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-670bbee7d94aa12dc2d2">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.362\" data-max=\"7.914\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.015\" data-max=\"2.104\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.686\" data-max=\"5.902\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"3.594\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.898\" data-max=\"6.116\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.045\" data-max=\"1.33\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[5.255,7.914,4.369,4.792,4.587,3.362,5.117,4.708,6.43,6.417,5.961,5.002,4.289],[1.077,1.492,null,0.509,0.042,null,0.015,0.107,2.104,1.828,0.348,0.208,null],[5.482,2.686,5.048,4.608,5.218,3.323,4.516,5.497,5.621,5.676,3.833,5.902,4.597],[1.792,3.594,null,2.321,0.946,null,0,0.928,1.639,1.047,2.854,1.506,null],[4.44,6.116,3.775,4.064,3.945,2.898,4.155,4.185,5.167,4.834,5.033,4.558,3.845],[1.283,0.957,null,0.649,0.385,null,0.045,0.154,1.33,1.017,0.318,0.198,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var16, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-ccc5baa269696f66bba2" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-ccc5baa269696f66bba2">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["7214","1555","14084","8470","3153","305","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

var16_dist.from.CAPSAD <- distances(var16, v=V(var16)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(var16), weights=E(var16)$var16)

#Saving distance on igraph object 
V(var16)$var16_dist.from.CAPSAD<-var16_dist.from.CAPSAD

var16_dist.from.CAPSAD[var16_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(var16_dist.from.CAPSAD)+1)
col <- col[var16_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(var16)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(var16, es=E(var16), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var16))
maxC <- rep(Inf, vcount(var16))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var16, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var16)$var16)

#Plotting distance from CAPSAD 
plot(var16, 
     layout=co,
     edge.color=V(var16)$col[edge.start],
     edge.arrow.size=(betweenness(var16, weights = E(var16)$var16)+1)/100000,
     edge.width=E(var16)$var16/10*mean(E(var16)$var16),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(var16))*5,
     vertex.label=var16_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(var16))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var16)$var16_dist.from.CAPSAD
b<-V(var16)$col
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
  title("Distance from CAPS AD - 18_CONFIANÇA A) Confia na qualificação profissional da equipe deste serviço.(var16)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(var16, directed=T, unconnected = T)
     )
             )
```

![](18_CONFIANÇA_A_Confia_na_qualificação_profissional_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_var16.RData") 
```

