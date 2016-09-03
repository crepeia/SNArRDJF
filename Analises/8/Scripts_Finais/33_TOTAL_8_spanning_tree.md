# SNA Minimum Spanning Tree 33_TOTAL
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
load("~/SNArRDJF/Robject/7_hits_total.RData")
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

#Minimum spanning tree non-normalized that connects all the vertices together with the minimal total edges

```r
set.seed(123)
#Creating Minimum spanning tree
msp_total<-mst(total)

#Plotting based only on degree measures 
edge.start <- ends(msp_total, es=E(msp_total), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(msp_total))
maxC <- rep(Inf, vcount(msp_total))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(msp_total, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights=E(msp_total)$total)

#Plotting
plot(msp_total, 
     layout=co,
     edge.color=V(msp_total)$color[edge.start],
     edge.arrow.size=(degree(msp_total)+1)/500,
     edge.width=E(msp_total)$total/10*mean(E(msp_total)$total),
     edge.curved = TRUE,
     vertex.color=V(msp_total)$color,
     vertex.size=(degree(msp_total)+1)*50,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(msp_total,"LABEL_COR"),
     vertex.label.cex=(degree(msp_total)+1)/20,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(msp_total)$LABEL_COR
b<-V(msp_total)$color
c<-table(a,b)
d<-as.data.frame(c)
e<-subset(d, d$Freq>0)
e<-e[order(e$a,decreasing=T),] 
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
  title("Minimum Spanning Tree Non-normalized - from 33_TOTAL", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels =    sprintf("Median In Degree: %.2f\nMedian Out Degree: %.2f",
     median(degree(msp_total, mode="in")), 
     median(degree(msp_total, mode="out"))
             )
       )
```

![](33_TOTAL_8_spanning_tree_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
#Minimum weighted spanning tree that connects all the vertices together with the minimal total weighting for its edges. 

Atention: Weighted edges are considered here as barries as we are trying to find the easiest path with minimal weight - so here we are considering the inverse of weight (1/weight) as a proxy of "unacess" and/or "untrust" - I'm not sure if it will work but it would be cool to think in something like this  

```r
set.seed(123)
#Creating Minimum weighted spanning tree
msp_total_w<-minimum.spanning.tree(total, weights=1/(edge_betweenness(total, weights=E(total)$total)+1))

#Plotting based only on degree measures 
edge.start <- ends(msp_total_w, es=E(msp_total_w), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(msp_total_w))
maxC <- rep(Inf, vcount(msp_total_w))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(msp_total_w, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights =E(msp_total_w)$total)

#Plotting based only on degree measures 
plot(msp_total_w, 
     layout=co,
     edge.color=V(msp_total_w)$color[edge.start],
     edge.arrow.size=(degree(msp_total_w)+1)/500,
     edge.width=0.2+1/(E(msp_total_w)$total+1),
     edge.curved = TRUE,
     vertex.color=V(msp_total_w)$color,
     vertex.size=(degree(msp_total_w)+1)*5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(msp_total_w,"LABEL_COR"),
     vertex.label.cex=(degree(msp_total_w)+1)/50,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(msp_total_w)$LABEL_COR
b<-V(msp_total_w)$color
c<-table(a,b)
d<-as.data.frame(c)
e<-subset(d, d$Freq>0)
e<-e[order(e$a,decreasing=T),] 
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
  title("Minimum Weighted Spanning Tree (Edge Betweenness) - 33_TOTAL", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
   sprintf("Median In Degree: %.2f\nMedian Out Degree: %.2f",
     median(degree(msp_total_w, mode="in")), 
     median(degree(msp_total_w, mode="out"))
             ))
```

![](33_TOTAL_8_spanning_tree_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/8_spanning_tree_total.RData") 
```

