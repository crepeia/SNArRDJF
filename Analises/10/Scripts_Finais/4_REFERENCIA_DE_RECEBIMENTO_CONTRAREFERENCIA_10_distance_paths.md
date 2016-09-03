# SNA Reciprocity Distance Path 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_var2.RData")
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
#var2<-simplify(var2) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var2, directed=T, unconnected = T)
```

```
## [1] 3.372042
```
##Shortest Paths

```r
#Shortest Paths
var2_sp_in <- shortest.paths(var2, mode='in', weights=E(var2)$var2) #in

var2_sp_out <- shortest.paths(var2, mode='out', weights=E(var2)$var2) # out

var2_sp_all <- shortest.paths(var2, mode='all', weights=E(var2)$var2) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var2_sp_in[which(var2_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   3.000   3.338   4.000   9.000
```

```r
sd(var2_sp_in[which(var2_sp_in != Inf)])
```

```
## [1] 1.763743
```
##Descriptive  Shortest Paths - OUT

```r
summary(var2_sp_out[which(var2_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   3.000   3.338   4.000   9.000
```

```r
sd(var2_sp_out[which(var2_sp_out != Inf)])
```

```
## [1] 1.763743
```

##Descriptive  Shortest Paths - ALL

```r
summary(var2_sp_all[which(var2_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   2.000   2.552   3.000   6.000
```

```r
sd(var2_sp_all[which(var2_sp_all != Inf)])
```

```
## [1] 0.8393559
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var2<-distances(var2, mode="all", weights=E(var2)$var2)
#distances_sp_all_var2

distances_dist_all_var2[distances_dist_all_var2=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var2_vec <- vector()
for (i in 1:vcount(var2)) {
    distances_sp_all_var2_vec[i] <- 
    mean(distances_dist_all_var2[i,],na.rm=T)
}
#Adding to igraph object
V(var2)$sp_all<-distances_sp_all_var2_vec
```

#In shortest paths 

```r
distances_dist_in_var2<-distances(var2, mode="in",weights=E(var2)$var2)
#distances_sp_in_var2

distances_dist_in_var2[distances_dist_in_var2=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var2_vec <- vector()
for (i in 1:vcount(var2)) {
    distances_sp_in_var2_vec[i] <- mean(distances_dist_in_var2[i,], na.rm=T)
}

#Adding to igraph object
V(var2)$sp_in<-distances_sp_in_var2_vec
```

#Out shortest paths 

```r
distances_dist_out_var2<-distances(var2, mode="out", weights=E(var2)$var2)

distances_dist_out_var2[distances_dist_out_var2=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var2_vec <- vector()
for (i in 1:vcount(var2)) {
    distances_sp_out_var2_vec[i] <- 
    mean(distances_dist_out_var2[i,], na.rm = T)
}

#Adding to igraph object
V(var2)$sp_out<-distances_sp_out_var2_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var2_shortpath_df <- data.frame(distances_sp_in_var2_vec, distances_sp_out_var2_vec, distances_sp_all_var2_vec) %>% round(3)

#Adding type
var2_shortpath_df <-cbind(var2_shortpath_df, V(var2)$LABEL_COR)

#Adding names
names(var2_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var2_shortpath_df<-var2_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var2_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-f080a7840fbbb0b53a5b" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-f080a7840fbbb0b53a5b">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"6.895\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"5.128\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"4.652\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[1.885,2.154,1.548,2.173,2.096,2.142,2.404,2.76,3.721,3.654,2.106,2.114,4.644,3,2.305,2.452,2.106,2.181,5.225,2.375,2.135,3.058,2.173,4.99,0,5.923,5.904,2.087,2.144,2.173,2.077,2.269,2.067,5.914,2.144,3.683,2.846,3.019,3.26,2.654,2.163,2.192,2.308,2.846,3.135,2.076,5.914,3.635,3.635,3.635,3.635,3.635,0,2.567,5.962,2.163,2.2,6.895,5.914,2.019,3.619,3.769,2.276,2.269,3.683,0,6.895,2.817,3.077,2.324,2.305,2.298,2.238,2.24,2.212,2.288,2.231,1.923,4.266,4.266,4.266,4.266,2.257,4.01,3.312,2.26,2.952,5.59,2.288,2.317,2.327,0,3.769,2.798,2.317,3.471,5.547,3.657,2.25,2.181,4.273,2.75,3.257,3.257,2.267,2.298,2.269,3.257,3.048,2.229,2.231,2.202,2.26,2.298,2.257,2.269,2.25,2.25,2.298,2.25,2.267,2.337,2.276,2.276,2.276,2.288,2.308,2.279,2.24,2.635,2.305,2.276,2.308,2.288,2.276,2.25,2.288,2.26,2.26,2.305,2.212,2.279,2.327,2.269,0,2.077,2.231,2.314,2.952,2.667,2.305,2.667,2.305,2.667,6.885,5.904,2.667,2.305,2.305,5.933,0,5.857,6.895,6.84,5.914,6.84,5.914,5.914,5.914,5.914,5.914,5.914,5.914,5.914,5.914,6.895,5.914,5.914,5.914,5.914,5.914,5.914,3.448,5.914,5.914,0,0],[2.492,2.609,2.307,2.609,3.207,0,3.017,2.704,3.173,3.659,3.497,0,2.95,3.235,0,3.095,2.458,0,0,4,3.123,3.324,3.156,2.034,3.472,4.408,3.447,2.927,2.715,2.994,3.134,3.235,3.374,0,5.073,3.38,3.168,3.81,3.229,2.648,2.48,3.123,3.123,3.291,2.581,0,0,3.095,3.095,3.095,3.095,3.095,0,3.05,2.838,3.274,0,0,0,3.212,0,5.128,0,3.279,3.536,0,0.667,2.916,4.196,0,0,3.24,0,3.598,3.201,3.279,4.106,2.771,1.286,1.286,1.286,1.286,0,3.022,1,3.246,4.179,0.5,3,4.089,3.19,0,5.128,4.145,3.212,3.201,0,0,4.307,0,0.5,2.989,0,0,0,3.106,3.916,0,0,0,4.112,3.821,4.112,3.296,0.5,3.916,3.447,3.318,3.196,3.291,0,3.201,0,0,0,4.117,3.296,3.274,3.296,4.112,0,0,3.246,3.296,0,3.201,3.285,3.296,3.296,0,4.112,3.296,3.291,3.296,3.289,3.296,3.246,0,3.374,0,0,0,0,0,4.201,4.207,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[1.635,2,1.514,2.138,2.287,2.315,2.215,2.055,2.365,3.011,2.249,2.271,2.414,2.68,2.381,2.326,1.95,2.304,4.652,2.348,2.232,2.343,2.276,1.923,2.624,2.729,2.188,2.177,2.215,2.232,2.271,2.331,2.227,2.823,2.304,2.42,2.309,2.934,2.376,2.116,2.138,2.32,2.337,2.37,2.337,2.254,2.823,2.343,2.343,2.343,2.343,2.343,0,2.182,2.508,2.249,2.304,3.497,2.823,2.055,2.707,3.575,2.359,2.365,2.912,0,3.486,2.464,2.762,2.403,2.381,2.37,2.166,2.171,2.326,2.37,2.116,2.116,3.657,3.657,3.657,3.657,2.376,2.729,2.685,2.365,2.713,3.398,2.249,2.359,2.37,0,3.575,2.591,2.354,2.149,3.398,3.022,2.343,2.326,3.663,2.271,3.238,3.238,2.37,2.326,2.37,3.238,3.116,2.309,2.309,2.331,2.365,2.376,2.359,2.37,2.365,2.359,2.359,2.343,2.37,2.304,2.376,2.376,2.376,2.365,2.381,2.359,2.337,2.53,2.381,2.376,2.381,2.354,2.376,2.343,2.376,2.331,2.326,2.376,2.343,2.37,2.381,2.37,2.503,2.232,2.287,2.381,2.873,2.552,2.381,2.552,2.381,2.552,2.801,2.724,2.552,2.381,2.381,2.912,0,2.807,3.497,3.492,2.823,3.492,2.823,2.823,2.823,2.823,2.823,2.823,2.823,2.823,2.823,3.497,2.823,2.823,2.823,2.823,2.823,2.823,2.713,2.823,2.823,0,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var2_shortpath_df, by=list(var2_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var2_shortpath_df, by=list(var2_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-80f3751271755e6bf804" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-80f3751271755e6bf804">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.548\" data-max=\"5.31\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.045\" data-max=\"1.687\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"3.126\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1.74\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.514\" data-max=\"2.746\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.012\" data-max=\"0.989\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[2.394,5.31,2.154,1.952,2.118,1.548,2.25,2.111,3.297,2.591,3.31,2.308,2.181],[0.486,1.687,null,0.91,0.045,null,0.082,0.066,1.415,1.549,0.36,0.111,null],[3.076,0.673,2.609,2.409,3.126,2.307,3.123,2.441,2.661,2.64,2.134,2.086,0],[0.516,1.293,null,1.74,0.152,null,0,1.533,1.697,1.264,1.589,1.738,null],[2.413,2.746,2,1.881,2.229,1.514,2.328,2.219,2.458,2.726,2.682,2.361,2.304],[0.401,0.823,null,0.881,0.041,null,0.012,0.1,0.989,0.763,0.424,0.076,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var2, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-efff2fdf94d9e087ddb5" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-efff2fdf94d9e087ddb5">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10"],["unconnected","one","two","three","four","five","six","unconnected","one","two"],["16233","934","6914","4286","2083","1047","1956","1035","266","28"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

var2_dist.from.CAPSAD <- distances(var2, v=V(var2)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(var2), weights=E(var2)$var2)

#Saving distance on igraph object 
V(var2)$var2_dist.from.CAPSAD<-var2_dist.from.CAPSAD

var2_dist.from.CAPSAD[var2_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(var2_dist.from.CAPSAD)+1)
col <- col[var2_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(var2)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(var2, es=E(var2), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2))
maxC <- rep(Inf, vcount(var2))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2)$var2)

#Plotting distance from CAPSAD 
plot(var2, 
     layout=co,
     edge.color=V(var2)$col[edge.start],
     edge.arrow.size=(betweenness(var2, weights = E(var2)$var2)+1)/100000,
     edge.width=E(var2)$var2/10*mean(E(var2)$var2),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(var2))*5,
     vertex.label=var2_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(var2))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var2)$var2_dist.from.CAPSAD
b<-V(var2)$col
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
  title("Distance from CAPS AD - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(var2, directed=T, unconnected = T)
     )
             )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_var2.RData") 
```

