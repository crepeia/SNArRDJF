# SNA Cliques 3_REFERENCIA DE ENVIO (var1)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 3_REFERENCIA DE ENVIO (var1)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/10_distance_paths_var1.RData")
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
#var1_u<-simplify(var1) #Simplify

var1_u<-as.undirected(var1, mode="collapse",edge.attr.comb=list(weight="mean","ignore"))
```

#Find cliques (complete subgraphs of an undirected graph)
Graph clique is a subset of vertices of a graph such that every two vertices in the clique are adjacent. - ***To check****

##Number of cliques - subgraphs

```r
cliques_var1_u<-cliques(var1_u) # list of cliques 
length(cliques_var1_u)
```

```
## [1] 7062
```
##Number of cliques by cliques size

```r
cliques_var1_u_size<-sapply(cliques(var1_u), length) 
cliques_var1_u_size_t<-table(cliques_var1_u_size)
cliques_var1_u_size_t
```

```
## cliques_var1_u_size
##    1    2    3    4    5    6    7    8 
##  187  950 1789 1966 1379  612  160   19
```

##Cliques Bar Plot Sizes Frequency

```r
barplot(cliques_var1_u_size_t)
title(main = "Cliques Sizes Frequency - Bar Plot 3_REFERENCIA DE ENVIO (var1)", font.main = 4)
```

![](3_REFERENCIA_DE_ENVIO_12_cliques_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

##Size of largest clique 
A maximum clique is a clique that cannot be extended by including one more adjacent vertex (not included in larger one). 

```r
clique_num(var1_u)
```

```
## [1] 8
```
##Number of maximal cliques

```r
count_max_cliques(var1_u)
```

```
## [1] 521
```
##Finding of largest cliques

```r
largest_cliques<-largest_cliques(var1_u) # cliques with max number of nodes
length(largest_cliques)
```

```
## [1] 19
```

##Plotting the largest cliques - important to consider connectivite 

```r
#Coloring largest clique as gold and others one as gray
vcol <- rep("grey80", vcount(var1_u))
vcol[unlist(largest_cliques(var1_u))] <- "gold"

#Saving gray and gold as igraph attribute
V(var1_u)$vcol<-vcol

#Saving labels to display as legend
V(var1_u)$vcollabel[V(var1_u)$vcol=="gold"]<-"Largets Clique"
V(var1_u)$vcollabel [V(var1_u)$vcol=="grey80"]<-"Others"
```
##Plotting Clique Size

```r
set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var1_u, es=E(var1_u), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var1_u))
maxC <- rep(Inf, vcount(var1_u))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var1_u, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights=E(var1_u)$var1)

#Plotting
plot(var1_u, 
     layout=co,
     edge.color=V(var1_u)$vcol[edge.start],
     #edge.arrow.size=E(var1_u)$var1/2000*mean(E(var1_u)$var1),
     #edge.width=E(var1_u)$var1/20*mean(E(var1_u)$var1),
     edge.curved = TRUE,
     vertex.color=vcol,
     vertex.size=log(degree(var1_u)+2)*10,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var1_u,"LABEL_COR"),
     vertex.label.cex=log(degree(var1_u)+2)/10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var1_u)$vcollabel
b<-V(var1_u)$vcol
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
  title("Network Vertex Degree Sized - classfied by largest clique vs. others", sub = "Source: from authors ")  
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels =       sprintf("Size of largest clique: %.1f\nNumber of maximal cliques: %.1f",
     clique_num(var1_u), 
     count_max_cliques(var1_u)
             )
       )
```

![](3_REFERENCIA_DE_ENVIO_12_cliques_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/12_cliques_var1.RData") 
```


