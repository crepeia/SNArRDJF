# SNA Reciprocity (Triads and Diads) and Transitivity 32_ACESSO E CONFIANÇA
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
load("~/SNArRDJF/Robject/8_spanning_tree_acessoeconfianca.RData")
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

#Reciprocity Default
Reciprocity Default - Proportion of mutual connections - probability that hte opposite counterpart of a directed graph is also included

```r
reciprocity(acessoeconfianca, mode="default")
```

```
## [1] 0.4124365
```

#Reciprocity Ratio
Reciprocity Ratio - Probability  of mutual connections between a vertex pair - if we know - probability that hte opposite counterpart of a directed graph is also included in the 

```r
reciprocity(acessoeconfianca, mode="ratio")
```

```
## [1] 0.2597922
```

#Dyad Census
A dyad consists of an unordered pair of actors and links that exist between two actors of the pair classified by mutal non-mutual and no connection in a directed graphs

Dyads are 2-subgraphs where a subgraph is a subset of actors taken from the complete set of network actors and all links
between them. See more here <http://file.scirp.org/pdf/SN_2013012915270187.pdf>

##Dyad Census 
Number of pairs with mutual connections "mut" and number of pairs with non-mutual connections "asym"

```r
dyad.census_acessoeconfianca<-dyad.census(acessoeconfianca)
```
##Mutual connections.

```r
dyad.census_acessoeconfianca_mut<-dyad.census_acessoeconfianca$mut
dyad.census_acessoeconfianca_mut
```

```
## [1] 325
```
##Non-mutual connections.

```r
dyad.census_acessoeconfianca_asym<-dyad.census_acessoeconfianca$asym
dyad.census_acessoeconfianca_asym
```

```
## [1] 926
```
##No connection between them.

```r
dyad.census_acessoeconfianca_null<-dyad.census_acessoeconfianca$null
dyad.census_acessoeconfianca_null
```

```
## [1] 16140
```
#Triad Census - Check this out in order to understand triad lables

The studies about transitivity in social networks led Holland and Leinhardt (1975) to propose that the local structure in social networks can be expressed by the triad census or triad count, the numbers of triads of any kinds.

You can see more here:
<http://www.stats.ox.ac.uk/~snijders/Trans_Triads_ha.pdf>


```r
#Triad Census 
tc_acessoeconfianca <- triad.census(acessoeconfianca)

#Triad Census Label 
census_labels = c('T.003',
                  'T.012',
                  'T.102',
                  'T.021D',
                  'T.021U',
                  'T.021C',
                  'T.111D',
                  'T.111U',
                  'T.030T',
                  'T.030C',
                  'T.201',
                  'T.120D',
                  'T.120U',
                  'T.120C',
                  'T.210',
                  'T.300')

ordering = c('1',
                  '2',
                  '3',
                  '4',
                  '5',
                  '6',
                  '7',
                  '8',
                  '13',
                  '10',
                  '9',
                  '14',
                  '15',
                  '11',
                  '12',
                  '16')

#Saving in a dataframe for further studies
triad_df_acessoeconfianca <- data.frame(census_labels,tc_acessoeconfianca)
write.csv(triad_df_acessoeconfianca, "~/SNArRDJF/Banco Redes R/acessoeconfianca_complet_triads.csv")
```
##Triad Census Types 
The following labels gives the 16 different triads for directed graphs. The coding refers to the numbers of mutual, asymmetric, and null dyads, with a further identifying letter: Up, Down, Cyclical, Transitive.

E.g., 1-2-0-D has *1* mutual, *2* asymmetric, *0* null dyads, and the *Down* orientation.

###Describing triads


```r
triad_df_acessoeconfianca
```

```
##    census_labels tc_acessoeconfianca
## 1          T.003              878877
## 2          T.012              123065
## 3          T.102               36095
## 4         T.021D                7384
## 5         T.021U                2749
## 6         T.021C                5247
## 7         T.111D                4187
## 8         T.111U                6634
## 9         T.030T                1000
## 10        T.030C                  85
## 11         T.201                4748
## 12        T.120D                 380
## 13        T.120U                 524
## 14        T.120C                 392
## 15         T.210                 817
## 16         T.300                 261
```

###Triads Tables Recoding

```r
#Recoding different types of triads 
triad_df_acessoeconfianca$type[triad_df_acessoeconfianca$census_labels=="T.003"]<-"Vacuously Transitive"
triad_df_acessoeconfianca$type[triad_df_acessoeconfianca$census_labels=="T.012"]<-"Vacuously Transitive"
triad_df_acessoeconfianca$type[triad_df_acessoeconfianca$census_labels=="T.102"]<-"Vacuously Transitive"
triad_df_acessoeconfianca$type[triad_df_acessoeconfianca$census_labels=="T.021D"]<-"Vacuously Transitive"
triad_df_acessoeconfianca$type[triad_df_acessoeconfianca$census_labels=="T.021U"]<-"Vacuously Transitive"
triad_df_acessoeconfianca$type[triad_df_acessoeconfianca$census_labels=="T.021C"]<-"Intransitive"
triad_df_acessoeconfianca$type[triad_df_acessoeconfianca$census_labels=="T.111D"]<-"Intransitive"
triad_df_acessoeconfianca$type[triad_df_acessoeconfianca$census_labels=="T.111U"]<-"Intransitive"
triad_df_acessoeconfianca$type[triad_df_acessoeconfianca$census_labels=="T.030T"]<-"Transitive"
triad_df_acessoeconfianca$type[triad_df_acessoeconfianca$census_labels=="T.030C"]<-"Intransitive"
triad_df_acessoeconfianca$type[triad_df_acessoeconfianca$census_labels=="T.201"]<-"Intransitive"
triad_df_acessoeconfianca$type[triad_df_acessoeconfianca$census_labels=="T.120D"]<-"Transitive"
triad_df_acessoeconfianca$type[triad_df_acessoeconfianca$census_labels=="T.120U"]<-"Transitive"
triad_df_acessoeconfianca$type[triad_df_acessoeconfianca$census_labels=="T.120C"]<-"Intransitive"
triad_df_acessoeconfianca$type[triad_df_acessoeconfianca$census_labels=="T.210"]<-"Intransitive"
triad_df_acessoeconfianca$type[triad_df_acessoeconfianca$census_labels=="T.300"]<-"Transitive"
```

Graphical Ilustration 

![<http://knoesis.wright.edu/sites/default/files/icnc15.pdf>](figures/img.png)

###Triads Tables

```r
datatable(triad_df_acessoeconfianca)
```

<!--html_preserve--><div id="htmlwidget-9e0d92f468c970b35670" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-9e0d92f468c970b35670">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"],["T.003","T.012","T.102","T.021D","T.021U","T.021C","T.111D","T.111U","T.030T","T.030C","T.201","T.120D","T.120U","T.120C","T.210","T.300"],[878877,123065,36095,7384,2749,5247,4187,6634,1000,85,4748,380,524,392,817,261],["Vacuously Transitive","Vacuously Transitive","Vacuously Transitive","Vacuously Transitive","Vacuously Transitive","Intransitive","Intransitive","Intransitive","Transitive","Intransitive","Intransitive","Transitive","Transitive","Intransitive","Intransitive","Transitive"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>census_labels\u003c/th>\n      <th>tc_acessoeconfianca\u003c/th>\n      <th>type\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":2},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Transitivity - clustering coefficient

##Transitivity Global
Socialnetwork is treated as an undirected network global - ratio of triangles (direction disregarded) to connected triples.


```r
transitivity(acessoeconfianca, type="global")
```

```
## [1] 0.251101
```

# Transitivity Local
Ratio of triangles to connected triples each vertex is part of.

```r
V(acessoeconfianca)$transitivity_local<-transitivity(acessoeconfianca, type="local") 
acessoeconfianca_transitivity_local<-transitivity(acessoeconfianca, type="local") #local - ratio of triangles to connected triples each vertex is part of.
```
#Descriptive Statistics for Local Transitivity by Vertex 

```r
summary(acessoeconfianca_transitivity_local[which(acessoeconfianca_transitivity_local != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0000  0.2314  0.3260  0.3606  0.4649  1.0000
```

```r
sd(acessoeconfianca_transitivity_local[which(acessoeconfianca_transitivity_local != Inf)])
```

```
## [1] 0.2015972
```

#Barrat's Weighted Transitivity by Edges (Barrat's)

```r
V(acessoeconfianca)$transitivity_barrat<-transitivity(acessoeconfianca, weights=E(acessoeconfianca)$weight, type="barrat")

acessoeconfianca_transitivity_barrat<-transitivity(acessoeconfianca, weights=E(acessoeconfianca)$weight, type="barrat")
```

#Descriptive Statistics for Barrat Weighted Transitivity by Vertex 

```r
summary(acessoeconfianca_transitivity_barrat[which(acessoeconfianca_transitivity_barrat != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0000  0.4448  0.6177  0.6368  0.7944  1.6870
```

```r
sd(acessoeconfianca_transitivity_barrat[which(acessoeconfianca_transitivity_barrat != Inf)])
```

```
## [1] 0.2994596
```

#Transitivity  Measures Dinamic Table

```r
#Getting  Measures

acessoeconfianca_transitivity_local<-transitivity(acessoeconfianca, type="local") %>% round(3)

acessoeconfianca_transitivity_barrat<-transitivity(acessoeconfianca, weights=E(acessoeconfianca)$weight, type="barrat") %>% round(3)

#Creating a datagrame of measures
acessoeconfianca_transitivity_df <- data.frame(acessoeconfianca_transitivity_local,acessoeconfianca_transitivity_barrat) %>% round(3)

#Adding type
acessoeconfianca_transitivity_df <-cbind(acessoeconfianca_transitivity_df, V(acessoeconfianca)$LABEL_COR)

#Adding names
names(acessoeconfianca_transitivity_df) <- c("Local", "Barrat's Weighted","Type")

#Ordering Variables
acessoeconfianca_transitivity_df<-acessoeconfianca_transitivity_df[c("Type", "Local", "Barrat's Weighted")]
```
## General tabel - DT 

```r
datatable(acessoeconfianca_transitivity_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-31ad3bef34994ef014ad" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-31ad3bef34994ef014ad">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1.687\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Assistência Hospitalar","Ambulatório de Saúde Mental","CAPSAD","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","Assistência Hospitalar","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","CRAS/CREAS","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Residência Terapeutica","CRAS/CREAS","Urgência/Emergência","Entidades Socioassistenciais","Assistência Hospitalar","CAPS","Entidades Assistênciais e Dependencia Química e CT","CAPS","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS/CREAS","CRAS/CREAS","CRAS/CREAS","UAPS","Acolhimento Institucional","Ajuda Mútua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Consultório na Rua","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","CRAS/CREAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Entidades Assistênciais e Dependencia Química e CT","UAPS","Ajuda Mútua","CRAS/CREAS","UAPS","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","CRAS/CREAS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","UAPS","UAPS","Ajuda Mútua","Entidades Assistênciais e Dependencia Química e CT","Assistência Hospitalar","UAPS","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","Entidades Assistênciais e Dependencia Química e CT","UAPS","Assistência Hospitalar","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","Residência Terapeutica","Residência Terapeutica","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","UAPS","Entidades Socioassistenciais","CAPS","Entidades Socioassistenciais","UAPS","Acolhimento Institucional","UAPS","UAPS","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","UAPS","UAPS","UAPS","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar"],[0.063,0.074,0.038,0.198,0.24,0.153,0.055,0.184,0.363,0.41,0.193,0.313,0.283,0.305,0.345,0.561,0.133,0.168,0.197,0.317,0.242,0.236,0.297,0.098,0.231,0.4,0.2,0.136,0.22,0.392,0.35,0.232,0.281,0.3,0.503,0.242,0.277,0.222,0.429,0.187,0.25,0.402,0.401,0.288,0.513,0.155,0.2,0.424,0.424,0.424,0.424,0.424,0.4,0.358,0.084,0.313,0.238,0.286,0.131,0.232,0.255,0.1,0.253,0.379,0.235,null,0.667,0.247,0.178,0.2,0.381,0.182,0.197,0.308,0.242,0.242,0.297,0.287,0.214,0.214,0.214,0.214,0.182,0.194,0.118,0.2,0.4,0.333,0.209,0.464,0.227,0.644,0.306,0.25,0.181,0.564,0.333,0.267,0.361,0.244,0.333,0.286,0.667,0.667,0.472,0.363,0.491,0.333,0.4,0.308,0.282,0.219,0.297,0.417,0.333,0.4,0.22,0.231,0.242,0.234,0.418,0.345,0.357,0.267,0.357,0.536,0.5,0.429,0.571,0.714,0.3,0.286,0.393,0.6,0.429,0.319,0.5,0.667,0.489,0.619,0.473,0.571,0.333,0.533,0.333,0.269,0.208,0.762,0.286,0.333,0.5,0.333,0.5,0.333,0.236,0.3,0.667,0.833,1,0,0,0.4,0,0.333,1,1,0.5,1,1,0.5,0.5,1,0.667,0.4,0.5,0,0.5,1,0.5,0.667,0.5,0.5,0.467,0.4,0.5,null,null],[0.123,0.132,0.085,0.375,0.429,0.264,0.147,0.34,0.526,0.665,0.359,0.521,0.398,0.482,0.786,0.855,0.265,0.325,0.281,0.569,0.442,0.385,0.549,0.109,0.381,0.576,0.394,0.295,0.439,0.65,0.641,0.514,0.554,0.416,0.784,0.331,0.544,0.45,0.772,0.338,0.464,0.655,0.637,0.485,0.906,0.32,0.288,0.694,0.694,0.694,0.694,0.691,0.493,0.651,0.103,0.54,0.563,0.362,0.168,0.398,0.364,0.429,0.606,0.817,0.302,null,0.671,0.552,0.292,0.633,0.785,0.511,0.465,0.575,0.509,0.66,0.618,0.557,0.857,0.857,0.857,0.857,0.446,0.276,0.254,0.553,0.542,0.421,0.433,0.864,0.575,0.756,0.511,0.53,0.497,0.787,0.502,0.521,0.69,0.431,0.5,0.493,0.892,0.892,1.023,0.822,1.028,0.657,0.617,0.674,0.717,0.482,0.639,0.824,0.758,1,0.682,0.69,0.651,0.499,0.978,0.641,0.776,0.724,0.776,1.101,1.103,0.895,0.897,1.233,0.879,0.752,0.883,1.038,0.914,0.694,1.14,1.244,0.894,1.294,0.896,1.121,0.823,1.205,0.502,0.41,0.421,1.168,0.568,0.488,1.092,0.488,1.092,0.488,0.338,0.451,0.661,1.36,1.687,0,0,0.523,0,0.348,1.333,1,0.706,1.333,1.333,0.702,0.663,1.333,0.858,0.504,0.706,0,0.712,1.333,0.71,0.822,0.687,0.687,0.602,0.607,0.66,null,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Local\u003c/th>\n      <th>Barrat's Weighted\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(acessoeconfianca_transitivity_df, by=list(acessoeconfianca_transitivity_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Local(M)", "Barrat's Weighted(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(acessoeconfianca_transitivity_df, by=list(acessoeconfianca_transitivity_df$Type), FUN=sd, na.rm=TRUE) 

names(aggdata_sd) <- c("Group","Type","Local(SD)", "Barrat's Weighted(SD)")

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
total_table<-total_table[c("Group","Local(M)","Local(SD)", "Barrat's Weighted(M)","Barrat's Weighted(SD)")]
```
##Final table with round - Transitivity

```r
datatable(total_table, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-e46cf97e18dbe5198b93" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-e46cf97e18dbe5198b93">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS/CREAS&quot;,&quot;Entidades Assistênciais e Dependencia Química e CT&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Residência Terapeutica&quot;,&quot;UAPS&quot;,&quot;Urgência/Emergência&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.038\" data-max=\"0.45\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001\" data-max=\"0.296\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.085\" data-max=\"0.806\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.013\" data-max=\"0.373\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório de Saúde Mental","Assistência Hospitalar","CAPS","CAPSAD","Consultório na Rua","CRAS/CREAS","Entidades Assistênciais e Dependencia Química e CT","Entidades Socioassistenciais","Residência Terapeutica","UAPS","Urgência/Emergência"],[0.272,0.418,0.074,0.194,0.236,0.038,0.402,0.265,0.307,0.278,0.45,0.39,0.168],[0.02,0.296,null,0.108,0.07,null,0.001,0.108,0.101,0.099,0.127,0.17,null],[0.529,0.603,0.132,0.366,0.424,0.085,0.646,0.473,0.481,0.482,0.717,0.806,0.325],[0.056,0.373,null,0.19,0.105,null,0.013,0.16,0.118,0.179,0.125,0.264,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Local(M)\u003c/th>\n      <th>Local(SD)\u003c/th>\n      <th>Barrat's Weighted(M)\u003c/th>\n      <th>Barrat's Weighted(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/9_dyad_triad_acessoeconfianca.RData") 
```


