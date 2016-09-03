# SNA Reciprocity Distance Path 32_ACESSO E CONFIANÇA
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 32_ACESSO E CONFIANÇA

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/9_dyad_triad_acessoeconfianca.RData")
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
#acessoeconfianca<-simplify(acessoeconfianca) #Simplify
```

#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(acessoeconfianca, directed=T, unconnected = T)
```

```
## [1] 2.525223
```
##Shortest Paths

```r
#Shortest Paths
acessoeconfianca_sp_in <- shortest.paths(acessoeconfianca, mode='in', weights=E(acessoeconfianca)$acessoeconfianca) #in

acessoeconfianca_sp_out <- shortest.paths(acessoeconfianca, mode='out', weights=E(acessoeconfianca)$acessoeconfianca) # out

acessoeconfianca_sp_all <- shortest.paths(acessoeconfianca, mode='all', weights=E(acessoeconfianca)$acessoeconfianca) # all
```
##Descriptive Shortest Paths - IN

```r
summary(acessoeconfianca_sp_in[which(acessoeconfianca_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0      82      94     104     128     255
```

```r
sd(acessoeconfianca_sp_in[which(acessoeconfianca_sp_in != Inf)])
```

```
## [1] 35.2897
```
##Descriptive  Shortest Paths - OUT

```r
summary(acessoeconfianca_sp_out[which(acessoeconfianca_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0      82      94     104     128     255
```

```r
sd(acessoeconfianca_sp_out[which(acessoeconfianca_sp_out != Inf)])
```

```
## [1] 35.2897
```

##Descriptive  Shortest Paths - ALL

```r
summary(acessoeconfianca_sp_all[which(acessoeconfianca_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00   75.00   82.00   87.64  108.00  191.00
```

```r
sd(acessoeconfianca_sp_all[which(acessoeconfianca_sp_all != Inf)])
```

```
## [1] 28.8056
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_acessoeconfianca<-distances(acessoeconfianca, mode="all", weights=E(acessoeconfianca)$acessoeconfianca)
#distances_sp_all_acessoeconfianca

distances_dist_all_acessoeconfianca[distances_dist_all_acessoeconfianca=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_acessoeconfianca_vec <- vector()
for (i in 1:vcount(acessoeconfianca)) {
    distances_sp_all_acessoeconfianca_vec[i] <- 
    mean(distances_dist_all_acessoeconfianca[i,],na.rm=T)
}
#Adding to igraph object
V(acessoeconfianca)$sp_all<-distances_sp_all_acessoeconfianca_vec
```

#In shortest paths 

```r
distances_dist_in_acessoeconfianca<-distances(acessoeconfianca, mode="in",weights=E(acessoeconfianca)$acessoeconfianca)
#distances_sp_in_acessoeconfianca

distances_dist_in_acessoeconfianca[distances_dist_in_acessoeconfianca=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_acessoeconfianca_vec <- vector()
for (i in 1:vcount(acessoeconfianca)) {
    distances_sp_in_acessoeconfianca_vec[i] <- mean(distances_dist_in_acessoeconfianca[i,], na.rm=T)
}

#Adding to igraph object
V(acessoeconfianca)$sp_in<-distances_sp_in_acessoeconfianca_vec
```

#Out shortest paths 

```r
distances_dist_out_acessoeconfianca<-distances(acessoeconfianca, mode="out", weights=E(acessoeconfianca)$acessoeconfianca)

distances_dist_out_acessoeconfianca[distances_dist_out_acessoeconfianca=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_acessoeconfianca_vec <- vector()
for (i in 1:vcount(acessoeconfianca)) {
    distances_sp_out_acessoeconfianca_vec[i] <- 
    mean(distances_dist_out_acessoeconfianca[i,], na.rm = T)
}

#Adding to igraph object
V(acessoeconfianca)$sp_out<-distances_sp_out_acessoeconfianca_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
acessoeconfianca_shortpath_df <- data.frame(distances_sp_in_acessoeconfianca_vec, distances_sp_out_acessoeconfianca_vec, distances_sp_all_acessoeconfianca_vec) %>% round(3)

#Adding type
acessoeconfianca_shortpath_df <-cbind(acessoeconfianca_shortpath_df, V(acessoeconfianca)$LABEL_COR)

#Adding names
names(acessoeconfianca_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
acessoeconfianca_shortpath_df<-acessoeconfianca_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(acessoeconfianca_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-4ea3153d5866ff0a746a" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-4ea3153d5866ff0a746a">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"177.416\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"158.876\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"55.021\" data-max=\"145.85\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[66.059,79.281,55.66,85.17,82.667,84.033,95.771,81.725,111.804,81.634,81.941,80.078,120.922,83.549,91.451,91.935,80.235,74.19,118.902,91.007,84.641,108.667,84.438,153.386,80.17,92.392,134.745,83.889,85.778,84.549,83.078,91.235,81.595,132.569,86.013,106.719,84.686,85.294,115.399,86.412,84.059,92.908,94.601,81.497,95.66,83.451,166.144,109.542,109.542,109.542,109.542,109.529,139.203,95.307,130.575,85.641,87.065,111.941,128.065,78,87.539,147.327,90.601,91.157,105.732,0,177.416,90.281,70.392,92.399,88.902,88.948,88.582,87.83,86.902,90.948,90.033,80.281,148.869,148.869,148.869,148.869,90.098,106.007,102.438,88.34,93.752,167.19,88.908,92.118,90.993,132.686,140.281,96.804,86.863,106.026,167.19,113.17,87.686,84.19,143.869,111.013,115.379,115.379,91.242,90.797,90.902,115.046,112.673,90.026,90.693,86.712,89.608,88.131,88.294,90.856,90.948,90.915,90.033,86.163,91.229,88.431,88.869,89.804,88.869,89.458,89.752,89.418,88.634,99.111,90.255,89.51,89.255,89.706,88.778,85.51,88,90.856,88.562,91.359,89.922,91.092,91.654,91.392,101.49,79.013,88.196,87.359,105.765,100.013,90.196,100.013,90.196,100.013,88.235,129.68,99.091,89.792,90.182,164.097,152.195,122.799,121.058,123.277,166.442,176.568,125.545,166.442,166.442,122.37,132.513,166.442,123.487,124.383,116.338,115.734,122.214,166.442,122.195,136.942,123.253,123.253,120.273,120.435,135.727,149.753,101.994],[67.14,77.989,68.177,79.075,90.925,96.414,100.565,80.645,89.355,104.694,102.683,107.011,84.435,93.215,89.715,102.457,77.651,83.113,102.602,96.769,92.801,94.935,90.522,87.742,102.054,136.919,104.812,95.892,82.892,88.737,86.382,99.242,79.855,139.473,106.737,92.844,97.484,110.022,101.511,79.78,84.048,82.554,82.591,97.672,92.21,89.903,127.559,100.339,100.339,100.339,100.339,101.876,105.366,108.731,87.355,82.231,98.876,103.914,121.828,93.29,0,142.457,88.194,99.565,93.516,23,30.667,91.075,106.113,104.253,109.199,106.339,92.591,102.398,109.973,102.634,103.038,89.194,145.785,145.785,145.785,145.785,100.93,135.231,99.075,95.876,96.425,109.151,95.409,112.296,95.672,95.14,100.484,128.366,93.538,97.968,156.634,139.29,110.124,83.887,140.489,90.742,104.575,104.575,98.538,95.091,107.312,108.36,108.36,98.446,92.446,98.731,101.446,97.247,100.532,86.882,96.349,98.387,97.124,95.5,91.532,87.07,116.258,116.258,116.258,108.737,114.661,108.419,114.661,111.72,99.296,102.688,102.645,109.688,107.699,104.113,118.645,115.656,106.704,102.28,113.22,117.645,107.387,113.667,112.683,121.634,107.64,158.876,128.317,123.516,123.516,123.516,123.516,123.516,143.989,124.763,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[55.021,65.898,56.064,68.118,62.722,62.193,87.684,69.182,81.31,79.952,76.84,77.551,78.963,68.995,74.487,86.257,61.326,68.631,87.064,78.316,77.781,83.219,75.781,81.749,76.449,88.701,86.444,76.652,75.786,74.316,75.754,85.064,68.503,101.877,75.786,80.85,77.053,71.251,90.54,60.209,75.337,75.23,75.064,78.08,62.529,73.31,91.091,86.957,86.957,86.957,86.957,87.711,95.107,82.38,78.567,69.417,83.465,84.519,98.818,71.909,83.936,118.535,78.187,76.893,83.23,145.85,124.952,81.722,59.866,87,83.786,86.444,78.631,81.278,82.722,83.668,82.102,76.187,127.048,127.048,127.048,127.048,76.738,85.802,80.332,84.765,78.412,97.385,82.348,86.797,82.262,86.658,88.422,89.321,62.332,79.786,127.171,100.524,84.262,72.182,112.513,62.209,79.348,79.348,85.807,62.412,62.583,62.802,62.802,81.021,82.299,83.455,83.904,81.412,84.519,74.941,84.283,84.722,83.15,81.08,81.733,75.257,87.588,89.62,87.588,86.401,87.332,85.909,85.503,86.824,87.048,86.07,85.904,85.979,85.684,83.032,85.476,86.679,82.561,84.909,85.663,85.663,85.358,87.209,99.973,80.406,83.567,83.503,95.824,93.225,89.877,93.225,89.877,93.225,81.182,93.321,89.909,87.615,89.877,113.321,125.588,103.048,100.342,102.481,122.155,125.765,109.636,122.155,122.155,106.556,114.743,122.155,106.471,109.037,101.011,100.353,105.807,122.155,105.668,116.059,103.829,103.829,102.914,97.722,114.326,127.749,102.561]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(acessoeconfianca_shortpath_df, by=list(acessoeconfianca_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(acessoeconfianca_shortpath_df, by=list(acessoeconfianca_shortpath_df$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-694e92ca1f7ac62bc34c" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-694e92ca1f7ac62bc34c">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"55.66\" data-max=\"134.259\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.197\" data-max=\"37.917\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"46.122\" data-max=\"103.591\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.026\" data-max=\"61.097\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"56.064\" data-max=\"105.721\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.117\" data-max=\"20.283\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[90.473,134.259,79.281,87.689,82.995,55.66,93.755,83.301,107.425,97.711,108.234,90.441,74.19],[13.3,22.81,null,12.503,2.674,null,1.197,2.156,37.917,20.466,9.629,3.1,null],[97.407,46.122,77.989,81.431,100.212,68.177,82.572,90.757,99.977,101.133,103.591,101.178,83.113],[26.851,61.097,null,40.386,14.45,null,0.026,9.845,27.428,16.416,3.563,24.813,null],[79.888,105.721,65.898,79.571,77.655,56.064,75.147,71.485,90.107,78.305,80.657,83.691,68.631],[14.218,15.406,null,15.313,2.008,null,0.117,6.028,20.283,17.158,9.849,5.979,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(SD)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(SD)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(acessoeconfianca, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-5d5d86234ed6c1980cac" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-5d5d86234ed6c1980cac">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7"],["unconnected","one","two","three","four","five","six"],["6474","1576","14213","8961","3192","365","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Distance from CAPSAD - just as an example for futher explortion

```r
#Setting distance from an vertex

acessoeconfianca_dist.from.CAPSAD <- distances(acessoeconfianca, v=V(acessoeconfianca)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"], to=V(acessoeconfianca), weights=E(acessoeconfianca)$acessoeconfianca)

#Saving distance on igraph object 
V(acessoeconfianca)$acessoeconfianca_dist.from.CAPSAD<-acessoeconfianca_dist.from.CAPSAD

acessoeconfianca_dist.from.CAPSAD[acessoeconfianca_dist.from.CAPSAD=="Inf"]<-10

set.seed(123)
# Set colors to plot distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(acessoeconfianca_dist.from.CAPSAD)+1)
col <- col[acessoeconfianca_dist.from.CAPSAD+1]

#Saving as Vertex properties
V(acessoeconfianca)$col<-col

#Plotting based only on degree measures 
edge.start <- ends(acessoeconfianca, es=E(acessoeconfianca), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acessoeconfianca))
maxC <- rep(Inf, vcount(acessoeconfianca))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acessoeconfianca, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acessoeconfianca)$acessoeconfianca)

#Plotting distance from CAPSAD 
plot(acessoeconfianca, 
     layout=co,
     edge.color=V(acessoeconfianca)$col[edge.start],
     edge.arrow.size=(betweenness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca)+1)/100000,
     edge.width=E(acessoeconfianca)$acessoeconfianca/10*mean(E(acessoeconfianca)$acessoeconfianca),
     edge.curved = TRUE,
     vertex.color=col,
     vertex.size=sqrt(degree(acessoeconfianca))*5,
     vertex.label=acessoeconfianca_dist.from.CAPSAD,
     vertex.label.color="white",
     vertex.frame.color="#ffffff",
     vertex.label.cex=log(degree(acessoeconfianca))/5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(acessoeconfianca)$acessoeconfianca_dist.from.CAPSAD
b<-V(acessoeconfianca)$col
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
  title("Distance from CAPS AD - 32_ACESSO E CONFIANÇA", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Avarege path length: %.2f",
     mean_distance(acessoeconfianca, directed=T, unconnected = T)
     )
             )
```

![](32_ACESSO_E_CONFIANÇA_10_distance_paths_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/10_distance_paths_acessoeconfianca.RData") 
```

