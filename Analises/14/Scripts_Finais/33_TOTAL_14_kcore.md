# SNA K-Core 33_TOTAL
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 33_TOTAL

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/12_cliques_total.RData")
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
#total<-simplify(total) #Simplify
```


#K-core
k-core is a maximal subset of vertices such that each is connected to at least k others in the subset.R has a function wich calculates the coreness for each vertex.The coreness of a vertex is k if it belongs to the k-core but not to the (k+1)-core.

##Finding maximum k-core and pick out it on graph

```r
coreness <- graph.coreness(total)
V(total)$coreness<-graph.coreness(total)
max_cor <- max(coreness)
max_cor <- max(coreness)
```
##Max Coreness

```r
max_cor
```

```
## [1] 17
```
##Heat map for coreness

```r
#Defining Heat Color
color_bar <- heat.colors(max_cor) 
V(total)$color_bar<-color_bar[coreness]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(total, es=E(total), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(total))
maxC <- rep(Inf, vcount(total))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(total, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(total)$weight)

#Plotting
plot(total, 
     layout=co,
     edge.color=V(total)$color_bar[edge.start],
     edge.arrow.size=degree(total)/10000,
     edge.width=E(total)$weight/10*mean(E(total)$weight),
     edge.curved = TRUE,
     vertex.color = color_bar[coreness],
     vertex.size=(degree(total)+10)/5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(total,"LABEL_COR"),
     vertex.label.cex=(degree(total)+0.01)/400,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(total)$coreness
b<-V(total)$color_bar
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
  title("Coreness Colored Graph - 33_TOTAL", sub = "Source: from authors ", cex = .5)
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels =     sprintf("Mean Coreness: %.2f\nMax Coreness: %.2f",
     mean(graph.coreness(total)), 
     max(graph.coreness(total))
             )
       )
```

![](33_TOTAL_14_kcore_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/14_kcore_total.RData") 
```
