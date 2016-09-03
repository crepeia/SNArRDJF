# SNA Reciprocity Distance Path 12_ACESSO D) O seu serviço consegue acessar facilmente informações deste serviço. (var10)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 12_ACESSO D) O seu serviço consegue acessar facilmente informações deste serviço. (var10)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_var10.RData")
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
#var10<-simplify(var10) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var10, directed=T, unconnected = T)
```

```
## [1] 2.50526
```
##Shortest Paths

```r
#Shortest Paths
var10_sp_in <- shortest.paths(var10, mode='in', weights=E(var10)$var10) #in

var10_sp_out <- shortest.paths(var10, mode='out', weights=E(var10)$var10) # out

var10_sp_all <- shortest.paths(var10, mode='all', weights=E(var10)$var10) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var10_sp_in[which(var10_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   4.000   5.000   5.343   7.000  15.000
```

```r
sd(var10_sp_in[which(var10_sp_in != Inf)])
```

```
## [1] 2.212684
```
##Descriptive  Shortest Paths - OUT

```r
summary(var10_sp_out[which(var10_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   4.000   5.000   5.343   7.000  15.000
```

```r
sd(var10_sp_out[which(var10_sp_out != Inf)])
```

```
## [1] 2.212684
```

##Descriptive  Shortest Paths - ALL

```r
summary(var10_sp_all[which(var10_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   3.000   4.000   4.052   5.000  11.000
```

```r
sd(var10_sp_all[which(var10_sp_all != Inf)])
```

```
## [1] 1.579992
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var10<-distances(var10, mode="all", weights=E(var10)$var10)
#distances_sp_all_var10

distances_dist_all_var10[distances_dist_all_var10=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var10_vec <- vector()
for (i in 1:vcount(var10)) {
    distances_sp_all_var10_vec[i] <- 
    mean(distances_dist_all_var10[i,],na.rm=T)
}
#Adding to igraph object
V(var10)$sp_all<-distances_sp_all_var10_vec
```

#In shortest paths 

```r
distances_dist_in_var10<-distances(var10, mode="in",weights=E(var10)$var10)
#distances_sp_in_var10

distances_dist_in_var10[distances_dist_in_var10=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var10_vec <- vector()
for (i in 1:vcount(var10)) {
    distances_sp_in_var10_vec[i] <- mean(distances_dist_in_var10[i,], na.rm=T)
}

#Adding to igraph object
V(var10)$sp_in<-distances_sp_in_var10_vec
```

#Out shortest paths 

```r
distances_dist_out_var10<-distances(var10, mode="out", weights=E(var10)$var10)

distances_dist_out_var10[distances_dist_out_var10=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var10_vec <- vector()
for (i in 1:vcount(var10)) {
    distances_sp_out_var10_vec[i] <- 
    mean(distances_dist_out_var10[i,], na.rm = T)
}

#Adding to igraph object
V(var10)$sp_out<-distances_sp_out_var10_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var10_shortpath_df <- data.frame(distances_sp_in_var10_vec, distances_sp_out_var10_vec, distances_sp_all_var10_vec) %>% round(3)

#Adding type
var10_shortpath_df <-cbind(var10_shortpath_df, V(var10)$LABEL_COR)

#Adding names
names(var10_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var10_shortpath_df<-var10_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var10_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-4427a12c5352fa6e53ca" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-4427a12c5352fa6e53ca">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"10.22\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"9.387\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.476\" data-max=\"7.882\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[3.161,3.933,2.584,4.443,3.846,4.221,5.02,4.107,4.839,5.295,3.792,4.107,6.396,5.591,4.631,5.362,4.275,3.389,6.403,4.081,4.04,5.013,4.101,7.745,6.96,5.497,5.819,4.121,4.477,3.919,3.893,4.644,3.812,7.349,4.477,4.758,4.564,5.168,6.389,4.107,3.966,4.96,4.866,4.289,5.148,4.242,9.114,6.268,6.268,6.268,6.268,6.268,6.705,4.094,7.289,4.463,3.933,7.349,6.544,3.644,4.767,7.651,4.624,4.644,4.732,0,10.22,4.732,6.47,4.671,4.322,4.43,4.463,4.409,4.262,4.644,4.611,3.805,7.671,7.671,7.671,7.671,4.55,4.779,4.772,4.309,4.517,9.235,4.584,4.658,4.658,8.53,6.691,4.752,3.933,4.987,9.235,5.027,4.262,3.879,7.658,6.268,6.373,6.373,4.651,4.597,4.624,6.373,6.24,4.369,4.383,4.188,4.315,4.322,4.302,4.611,4.624,4.624,4.544,3.953,4.651,4.081,4.456,4.497,4.456,4.51,4.389,4.356,4.383,4.886,4.537,4.376,4.45,4.47,4.423,4.215,3.94,4.651,4.221,4.638,4.315,4.376,4.651,4.631,5.51,3.98,4.577,3.94,5.342,4.906,4.53,4.906,4.53,4.906,4.765,8.215,4.58,4.387,4.527,9.12,9.173,5.453,5.927,6.305,9.12,10.172,5.533,9.12,9.12,5.213,7.213,9.12,5.367,5.3,4.987,4.967,5.507,9.12,5.507,7.407,7.26,7.26,5.867,5.407,7.4,6.687,5.547],[3.532,3.726,4.22,3.978,5.608,4.059,5.774,3.489,5.237,5.909,5.339,6.892,4.172,5.129,5.452,4.403,4.699,3.43,5.839,4.011,5.715,4.21,5.129,4.177,4.005,6.661,5.376,4.215,4.769,4.855,3.876,4.323,3.839,8.312,6.892,4.296,5.489,6.382,4.613,3.457,4.516,4.269,4.274,3.672,4.392,5.608,5.796,4.403,4.403,4.403,4.403,4.403,5.258,4.656,4.005,5.371,4.876,4.457,6.478,5.183,0,6.134,3.677,4.608,4.774,1.5,2,4.312,5.43,5.177,6.167,6.054,3.753,3.946,5.516,6.07,5.333,4.527,8.785,8.785,8.785,8.785,6.339,7.151,5.866,4.425,4.167,6.419,5.355,6.387,4.484,5.844,4.194,6.591,4.79,4.339,9.387,7.118,5.935,4.656,6.806,4.414,0,0,4.366,4.382,6.737,0,0,3.806,4.07,4.511,5.446,4.398,3.952,4.167,3.78,4.392,4.312,3.903,3.919,4.839,7.161,7.161,7.161,5.274,5.199,6.269,5.199,5.274,4.403,4.403,5.097,7.188,7.188,5.962,6.79,7.188,5.199,4.398,5.86,7.188,5.07,7.188,7.188,7.188,5.898,8.151,7.226,5.704,5.704,5.704,5.704,5.704,7.86,6.839,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[2.476,2.925,2.556,3.348,3.401,3.016,3.861,2.733,3.529,4.936,3.198,3.722,3.396,4.385,3.952,3.567,3.722,2.578,4.337,2.963,3.305,3.171,3.406,3.652,3.294,4.658,4.15,3.289,3.695,3.139,3.005,3.524,2.882,5.39,4.278,3.187,3.615,4.305,3.898,2.845,3.235,3.69,3.604,2.957,3.567,3.909,4.663,3.567,3.567,3.567,3.567,3.567,4.401,3.471,3.166,3.989,3.46,3.38,5.176,3.059,3.84,4.947,2.941,3.332,3.572,7.882,6.091,3.765,4.337,3.471,3.588,3.834,2.957,2.888,3.599,4.053,4.112,3.155,6.679,6.679,6.679,6.679,4.316,3.668,3.759,3.604,3.011,5.412,4.15,4.406,3.465,5.048,3.123,3.802,3.508,3.032,6.348,3.722,3.583,3.385,6.053,3.856,5.807,5.807,3.225,3.561,4.38,5.807,5.69,2.968,3.118,3.134,3.877,3.059,2.989,3.16,2.947,3.214,3.406,3.16,3.112,3.508,4.316,4.326,4.316,4.107,3.348,3.813,3.342,4.107,3.321,3.214,3.69,3.85,3.84,3.545,3.524,4.406,3.439,3.321,3.888,3.941,3.46,4.401,5.487,3.535,3.674,3.439,4.631,4.358,4.332,4.358,4.332,4.358,3.85,4.38,3.818,3.818,4.332,5.439,6.278,3.738,4.914,5.155,5.572,6.118,4.225,5.572,5.572,4.321,5.508,5.572,3.834,3.759,4.053,4.155,4.738,5.572,4.738,5.567,5.455,5.455,4.845,3.743,5.433,5.551,5.524]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var10_shortpath_df, by=list(var10_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var10_shortpath_df, by=list(var10_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-0062b1c81aefcb710fb9" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-0062b1c81aefcb710fb9">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.584\" data-max=\"7.002\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.064\" data-max=\"2.028\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.521\" data-max=\"5.562\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.004\" data-max=\"3.399\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.556\" data-max=\"4.923\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.061\" data-max=\"1.339\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[4.373,7.002,3.933,4.298,4.061,2.584,4.913,4.151,5.454,5.629,6.135,4.465,3.389],[0.842,1.625,null,0.84,0.064,null,0.066,0.272,2.028,1.203,0.412,0.228,null],[5.194,2.521,3.726,4.25,5.562,4.22,4.271,5.113,4.994,5.144,3.118,5.104,3.43],[1.792,3.399,null,2.14,1.248,null,0.004,0.982,1.636,1.187,2.368,1.56,null],[3.583,4.923,2.925,3.581,3.384,2.556,3.647,3.506,4.072,4.066,4.393,3.674,2.578],[0.925,0.973,null,0.985,0.113,null,0.061,0.408,1.339,0.951,1.046,0.469,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var10, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-658ba6ae6bd406fc1353" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-658ba6ae6bd406fc1353">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["7214","1562","14145","8527","3039","294","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

var10_dist.from.CAPSAD <- distances(var10, v=V(var10)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(var10), weights=E(var10)$var10)

#Saving distance on igraph object 
V(var10)$var10_dist.from.CAPSAD<-var10_dist.from.CAPSAD

var10_dist.from.CAPSAD[var10_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(var10_dist.from.CAPSAD)+1)
col <- col[var10_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(var10)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(var10, es=E(var10), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var10))
maxC <- rep(Inf, vcount(var10))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var10, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var10)$var10)

#Plotting distance from CAPSAD 
plot(var10, 
     layout=co,
     edge.color=V(var10)$col[edge.start],
     edge.arrow.size=(betweenness(var10, weights = E(var10)$var10)+1)/100000,
     edge.width=E(var10)$var10/10*mean(E(var10)$var10),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(var10))*5,
     vertex.label=var10_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(var10))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var10)$var10_dist.from.CAPSAD
b<-V(var10)$col
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
  title("Distance from CAPS AD - 12_ACESSO D) O seu serviço consegue acessar facilmente informações deste serviço. (var10)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(var10, directed=T, unconnected = T)
     )
             )
```

![](12_ACESSO_D_acessar_facilmente_informações_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_var10.RData") 
```

