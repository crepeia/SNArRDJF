# SNA Reciprocity Distance Path 6_PROGRAMAS EM CONJUNTO (var4)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 6_PROGRAMAS EM CONJUNTO (var4)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_var4.RData")
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
#var4<-simplify(var4) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var4, directed=T, unconnected = T)
```

```
## [1] 2.632896
```
##Shortest Paths

```r
#Shortest Paths
var4_sp_in <- shortest.paths(var4, mode='in', weights=E(var4)$var4) #in

var4_sp_out <- shortest.paths(var4, mode='out', weights=E(var4)$var4) # out

var4_sp_all <- shortest.paths(var4, mode='all', weights=E(var4)$var4) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var4_sp_in[which(var4_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   3.000   2.494   3.000   8.000
```

```r
sd(var4_sp_in[which(var4_sp_in != Inf)])
```

```
## [1] 1.136085
```
##Descriptive  Shortest Paths - OUT

```r
summary(var4_sp_out[which(var4_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   3.000   2.494   3.000   8.000
```

```r
sd(var4_sp_out[which(var4_sp_out != Inf)])
```

```
## [1] 1.136085
```

##Descriptive  Shortest Paths - ALL

```r
summary(var4_sp_all[which(var4_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   3.000   3.259   4.000   8.000
```

```r
sd(var4_sp_all[which(var4_sp_all != Inf)])
```

```
## [1] 1.218018
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var4<-distances(var4, mode="all", weights=E(var4)$var4)
#distances_sp_all_var4

distances_dist_all_var4[distances_dist_all_var4=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var4_vec <- vector()
for (i in 1:vcount(var4)) {
    distances_sp_all_var4_vec[i] <- 
    mean(distances_dist_all_var4[i,],na.rm=T)
}
#Adding to igraph object
V(var4)$sp_all<-distances_sp_all_var4_vec
```

#In shortest paths 

```r
distances_dist_in_var4<-distances(var4, mode="in",weights=E(var4)$var4)
#distances_sp_in_var4

distances_dist_in_var4[distances_dist_in_var4=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var4_vec <- vector()
for (i in 1:vcount(var4)) {
    distances_sp_in_var4_vec[i] <- mean(distances_dist_in_var4[i,], na.rm=T)
}

#Adding to igraph object
V(var4)$sp_in<-distances_sp_in_var4_vec
```

#Out shortest paths 

```r
distances_dist_out_var4<-distances(var4, mode="out", weights=E(var4)$var4)

distances_dist_out_var4[distances_dist_out_var4=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var4_vec <- vector()
for (i in 1:vcount(var4)) {
    distances_sp_out_var4_vec[i] <- 
    mean(distances_dist_out_var4[i,], na.rm = T)
}

#Adding to igraph object
V(var4)$sp_out<-distances_sp_out_var4_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var4_shortpath_df <- data.frame(distances_sp_in_var4_vec, distances_sp_out_var4_vec, distances_sp_all_var4_vec) %>% round(3)

#Adding type
var4_shortpath_df <-cbind(var4_shortpath_df, V(var4)$LABEL_COR)

#Adding names
names(var4_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var4_shortpath_df<-var4_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var4_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-ca0fed78c97e9554789f" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-ca0fed78c97e9554789f">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"5.22\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"3.625\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"5.375\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[1.972,1.846,1.667,0,0,3.25,3.056,0.667,0,0.667,3.111,2.308,0.5,0,2.432,1.833,0,1.846,0.5,3.946,2.5,0.667,2.568,1.25,0.5,0,0.5,2.5,0,0,0,2.583,1.625,0.5,0,0.667,0,0,0,1,1.562,1.667,1.583,3.5,0,1.462,0.5,1.833,1.833,1.833,1.833,1.833,0.5,4,0,4.865,2.432,0.5,2.919,2.111,5.22,0.667,2.595,2.611,0,0,0.5,1.692,0.667,2.432,2.595,2.595,2.528,2.432,2.297,2.595,2.595,1.625,1.556,1.556,1.556,1.556,2.333,0.667,1.111,2.459,1,1,2.444,2.595,2.595,0,0.667,0.667,2.5,0,1,0.667,2.459,2.189,0,4,0,0,2.595,2.595,2.595,0,3.405,2.351,2.432,2.333,2.432,2.595,2.459,2.432,2.432,2.444,2.342,2.079,2.595,4,2.432,2.432,2.432,2.595,2.595,2.432,2.541,0,2.595,2.432,2.595,2.595,2.432,2.472,2.595,2.595,2.432,2.595,2.054,2.595,2.595,2.432,2.611,2.405,2.119,2.541,0,0,2.595,0,2.595,0,1,0.5,0,2.595,2.595,0,0,0.5,0.75,0.667,0.5,0.667,0.5,0.5,0.5,0.5,0.5,0.5,3.324,0.5,0.5,0.667,0.5,0.5,0.5,0.5,0.5,0.5,2.389,0.5,0.5,0.5,0],[2.131,3.094,1.31,0,0,2.226,1.762,2.244,0,3.221,2.048,0,2.819,2.59,0,2.814,2.165,0,0,0,3.083,1,0,0,0,0,0,0,0,0,0,2.726,1,3.247,0,1,2.282,2.143,2.143,1.971,1,2.941,2.941,0,1,0,0.75,2.824,2.824,2.824,2.824,2.824,0.5,2.607,3.082,0.5,0.5,0,0,2.262,0,0.667,0,2.286,3.625,0.5,0.667,0,1.5,0,0,0,2.976,0,0,0,0,0.75,0.8,0.8,0.8,0.8,2.286,0.5,0.8,0,0,0.5,2.738,0,0.5,0,0.667,0.667,0,0,0.5,1.5,0,0,0.667,0,0,0,0,0,0,0,0,0,0,2.536,0,0,0,0,0,2.738,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3.024,0,0,0,0,0,0,0,0,2.286,0,0,0,1.6,0,0,0,0,0,0.5,0.5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[2.638,3.481,1.969,0,0,2.875,2.425,2.862,0,3.65,2.85,3.919,3.05,3.087,2.931,3.106,2.831,3.987,1,3.413,3.388,3.65,3.45,4.475,1,0,3.513,3.444,0,0,0,2.931,3.156,2.987,0,3.65,2.956,5.375,5.375,2.513,3.15,3.388,3.319,3.931,4.475,3.4,2.875,3.087,3.087,3.087,3.087,3.087,4.35,3.194,2.525,3.556,2.919,3.513,2.837,2.525,3.425,0.667,2.956,2.956,3.375,4.281,3.5,3.406,4.388,2.931,2.956,2.956,2.65,2.913,2.888,2.956,2.956,2.894,4.394,4.394,4.394,4.394,2.669,3.487,3.425,2.931,5.338,4.031,2.931,2.956,2.944,0,0.667,0.667,2.694,0,4.031,4.631,2.931,2.75,0.667,3.837,0,0,2.956,2.956,2.956,0,4.375,2.35,2.931,2.888,2.931,2.956,2.931,2.931,2.931,2.931,2.669,2.581,2.956,3.837,2.931,2.931,2.931,2.956,2.956,2.931,2.931,0,2.956,2.931,2.956,2.956,2.931,2.931,2.956,2.956,2.931,2.956,2.719,2.956,2.956,2.931,2.956,3.344,2.481,2.913,4.138,0,2.956,0,2.956,0,2.731,2.894,0,2.956,2.956,0,0,3.513,3.294,3.506,3.513,3.506,3.513,3.513,3.513,3.513,3.513,3.513,2.962,3.513,3.513,3.306,3.513,3.513,3.513,3.513,3.513,3.513,3.2,3.513,3.513,4.362,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var4_shortpath_df, by=list(var4_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var4_shortpath_df, by=list(var4_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-f2c201dcd6606e5c2129" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-f2c201dcd6606e5c2129">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.64\" data-max=\"2.493\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.059\" data-max=\"1.652\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"3.094\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1.542\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.717\" data-max=\"3.987\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.049\" data-max=\"1.901\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[1.062,0.856,1.846,1.992,2.493,1.667,1.625,1.279,0.64,1.554,1.2,2.332,1.846],[0.921,0.954,null,1.337,0.067,null,0.059,1.652,0.784,1.469,1.147,0.701,null],[1.2,0.288,3.094,0.975,0.771,1.31,2.941,0.591,1.243,0.675,1.768,0.372,0],[0.346,0.686,null,1.046,1.542,null,0,0.917,1.183,0.883,1.33,0.916,null],[3.481,3.199,3.481,2.06,3.406,1.969,3.353,1.717,3.064,2.778,2.805,2.691,3.987],[0.569,1.087,null,1.237,0.05,null,0.049,1.683,1.578,1.381,1.901,0.778,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var4, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-ccd4299ca23392cc8e39" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-ccd4299ca23392cc8e39">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9"],["unconnected","one","two","three","four","five","six","unconnected","one"],["31426","344","1192","1376","308","85","35","15","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

var4_dist.from.CAPSAD <- distances(var4, v=V(var4)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(var4), weights=E(var4)$var4)

#Saving distance on igraph object 
V(var4)$var4_dist.from.CAPSAD<-var4_dist.from.CAPSAD

var4_dist.from.CAPSAD[var4_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(var4_dist.from.CAPSAD)+1)
col <- col[var4_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(var4)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(var4, es=E(var4), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var4))
maxC <- rep(Inf, vcount(var4))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var4, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var4)$var4)

#Plotting distance from CAPSAD 
plot(var4, 
     layout=co,
     edge.color=V(var4)$col[edge.start],
     edge.arrow.size=(betweenness(var4, weights = E(var4)$var4)+1)/100000,
     edge.width=E(var4)$var4/10*mean(E(var4)$var4),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(var4))*5,
     vertex.label=var4_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(var4))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var4)$var4_dist.from.CAPSAD
b<-V(var4)$col
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
  title("Distance from CAPS AD - 6_PROGRAMAS EM CONJUNTO (var4)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(var4, directed=T, unconnected = T)
     )
             )
```

![](6_PROGRAMAS_EM_CONJUNTO_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_var4.RData") 
```

