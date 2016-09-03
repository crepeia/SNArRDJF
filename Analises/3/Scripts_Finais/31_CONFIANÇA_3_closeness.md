# SNA Closeness 31_CONFIANÇA
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 31_CONFIANÇA

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/confianca_data.RData")
```
##Reload packages

```r
suppressMessages(library(RColorBrewer))
#suppressMessages(library(car))
#suppressMessages(library(xtable))
suppressMessages(library(igraph))
#suppressMessages(library(miniCRAN))
#suppressMessages(library(magrittr))
#suppressMessages(library(keyplayer))
suppressMessages(library(dplyr))
#suppressMessages(library(feather))
#suppressMessages(library(visNetwork))
#suppressMessages(library(knitr))
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
#confianca<-simplify(confianca) #Simplify
```

#Closeness - centrality based on distance to others in the graph 

*How close an actor to all the other actors in network?*

High closeness centrality - short communication path to others, minimal number of steps to reach others.

Answers the “Kevin Bacon” question:

*How many steps are required to access every other vertex from a given vertex?*

One practical implication of this metric: it helps you gauge how information might spread within your network, and who might be the best people to leverage if you need to make sure information gets around. Link here: <http://www.tc.umn.edu/~alink/R-social-network-analysis.html>

Closeness centrality can be defined as a measure of how far other nodes are from the node in question. Nodes with high closeness centrality are likely to be relatively efficient in receiving or transmitting information to/from distant parts of the social network.

Scores may be interpreted as arising from a reciprocal process in which the centrality of each actor is proportional to the sum of the centralities of those actors to whom he or she is connected. 

In general, vertices with high eigenvector centralities are those which are connected to many other vertices which are, in turn, connected to many others (and so on). (The perceptive may realize that this implies that the largest values will be obtained by individuals in large cliques (or high-density substructures)

##Closeness Non-normalized

###Saving to Igraph object

```r
V(confianca)$incloseness <- closeness(confianca, mode = "in", weights = E(confianca)$confianca) %>% round(6)
V(confianca)$outcloseness <- closeness(confianca, mode = "out", weights = E(confianca)$confianca) %>% round(6)
V(confianca)$totalcloseness <- closeness(confianca, mode = "total", weights = E(confianca)$confianca) %>% round(4)
```

###Saving to Environment

```r
confianca_incloseness<- closeness(confianca, mode = "in", weights = E(confianca)$confianca) %>% round(6)
confianca_outcloseness<- closeness(confianca, mode = "out", weights = E(confianca)$confianca) %>% round(6)
confianca_totalcloseness<- closeness(confianca, mode = "total", weights = E(confianca)$confianca) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(confianca_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 2.900e-05 6.600e-05 7.600e-05 7.165e-05 7.700e-05 9.500e-05
```

```r
sd(confianca_incloseness)
```

```
## [1] 9.053724e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(confianca)$incloseness<-closeness(confianca, weights = E(confianca)$confianca, mode="in")

#Get Variable
V(confianca)$confianca_color_degree<-round(V(confianca)$incloseness,6)

#Creating brewer pallette
vertex_confianca_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(confianca)$confianca_color_degree)), "RdBu"))(
            length(unique(V(confianca)$confianca_color_degree)))

#Saving as Vertex properties 
V(confianca)$vertex_confianca_color_degree<-
  vertex_confianca_color_degree[as.numeric(
  cut(V(confianca)$confianca_color_degree,
      breaks=length(unique(V(confianca)$confianca_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(confianca, es=E(confianca), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(confianca))
maxC <- rep(Inf, vcount(confianca))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(confianca, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(confianca)$weight)


#PLotting
plot(confianca, 
     layout=co,
     edge.color=V(confianca)$vertex_confianca_color_degree[edge.start],
     edge.arrow.size=closeness(confianca, weights = E(confianca)$confianca, mode="in"),
     edge.width=E(confianca)$weight/mean(E(confianca)$weight),
     edge.curved = TRUE,
     vertex.color=V(confianca)$vertex_confianca_color_degree,
     vertex.size=closeness(confianca, weights = E(confianca)$confianca, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(confianca,"LABEL_COR"),
     vertex.label.cex=(closeness(confianca, weights = E(confianca)$confianca, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(confianca)$confianca_color_degree
b<-V(confianca)$vertex_confianca_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 31_CONFIANÇA", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(confianca, mode="in", weights = E(confianca)$confianca)), 
             sd(closeness(confianca, mode="in", weights = E(confianca)$confianca))
             )
       )
```

![](31_CONFIANÇA_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(confianca_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 2.900e-05 8.150e-05 1.010e-04 9.175e-05 1.120e-04 1.660e-04
```

```r
sd(confianca_outcloseness)
```

```
## [1] 3.29688e-05
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(confianca)$outcloseness<-closeness(confianca, weights = E(confianca)$confianca, mode="out")

#Get Variable
V(confianca)$confianca_color_degree<-round(V(confianca)$outcloseness,6)

#Creating brewer pallette
vertex_confianca_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(confianca)$confianca_color_degree)), "RdBu"))(
            length(unique(V(confianca)$confianca_color_degree)))

#Saving as Vertex properties 
V(confianca)$vertex_confianca_color_degree<-
  vertex_confianca_color_degree[as.numeric(
  cut(V(confianca)$confianca_color_degree,
      breaks=length(unique(V(confianca)$confianca_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(confianca, es=E(confianca), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(confianca))
maxC <- rep(Inf, vcount(confianca))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(confianca, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(confianca)$weight)


#PLotting
plot(confianca, 
     layout=co,
     edge.color=V(confianca)$vertex_confianca_color_degree[edge.start],
     edge.arrow.size=closeness(confianca, weights = E(confianca)$confianca, mode="out"),
     edge.width=E(confianca)$weight/2*mean(E(confianca)$weight),
     edge.curved = TRUE,
     vertex.color=V(confianca)$vertex_confianca_color_degree,
     vertex.size=closeness(confianca, weights = E(confianca)$confianca, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(confianca,"LABEL_COR"),
     vertex.label.cex=closeness(confianca, weights = E(confianca)$confianca, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(confianca)$confianca_color_degree
b<-V(confianca)$vertex_confianca_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 31_CONFIANÇA", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(confianca, mode="out", weights = E(confianca)$confianca)), 
             sd(closeness(confianca, mode="out", weights = E(confianca)$confianca))
             )
       )
```

![](31_CONFIANÇA_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(confianca_totalcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000770 0.0001195 0.0001310 0.0001288 0.0001390 0.0002030
```

```r
sd(confianca_totalcloseness)
```

```
## [1] 2.230934e-05
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(confianca)$allcloseness<-closeness(confianca, weights = E(confianca)$confianca, mode="all")

#Get Variable
V(confianca)$confianca_color_degree<-round(V(confianca)$allcloseness,6)

#Creating brewer pallette
vertex_confianca_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(confianca)$confianca_color_degree)), "RdBu"))(
            length(unique(V(confianca)$confianca_color_degree)))

#Saving as Vertex properties 
V(confianca)$vertex_confianca_color_degree<-
  vertex_confianca_color_degree[as.numeric(
  cut(V(confianca)$confianca_color_degree,
      breaks=length(unique(V(confianca)$confianca_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(confianca, es=E(confianca), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(confianca))
maxC <- rep(Inf, vcount(confianca))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(confianca, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(confianca)$weight)


#PLotting
plot(confianca, 
     layout=co,
     edge.color=V(confianca)$vertex_confianca_color_degree[edge.start],
     edge.arrow.size=closeness(confianca, weights = E(confianca)$confianca, mode="all"),
     edge.width=E(confianca)$weight/2*mean(E(confianca)$weight),
     edge.curved = TRUE,
     vertex.color=V(confianca)$vertex_confianca_color_degree,
     vertex.size=closeness(confianca, weights = E(confianca)$confianca, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(confianca,"LABEL_COR"),
     vertex.label.cex=(closeness(confianca, weights = E(confianca)$confianca, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(confianca)$confianca_color_degree
b<-V(confianca)$vertex_confianca_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 31_CONFIANÇA", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(confianca, mode="all", weights = E(confianca)$confianca)), 
             sd(closeness(confianca, mode="all", weights = E(confianca)$confianca))
             )
       )
```

![](31_CONFIANÇA_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(confianca)$incloseness_n <- closeness(confianca, mode = "in",, weights = E(confianca)$confianca, normalized = T) %>% round(10)
V(confianca)$outcloseness_n <- closeness(confianca, mode = "out", normalized = T, weights = E(confianca)$confianca) %>% round(6)
V(confianca)$totalcloseness_n <- closeness(confianca, mode = "total", normalized = T, weights = E(confianca)$confianca) %>% round(6)
```

###Saving to Environment

```r
confianca_incloseness_n<- closeness(confianca, mode = "in", normalized = T, weights = E(confianca)$confianca) %>% round(6)
confianca_outcloseness_n<- closeness(confianca, mode = "out", normalized = T, weights = E(confianca)$confianca) %>% round(6)
confianca_totalcloseness_n<- closeness(confianca, mode = "total", normalized = T, weights = E(confianca)$confianca) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(confianca_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.012220 0.014190 0.013320 0.014360 0.017590
```

```r
sd(confianca_incloseness_n)
```

```
## [1] 0.001680356
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(confianca)$incloseness_n<-closeness(confianca, weights = E(confianca)$confianca, mode="in", normalized = T)

#Get Variable
V(confianca)$confianca_color_degree<-round(V(confianca)$incloseness_n,6)

#Creating brewer pallette
vertex_confianca_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(confianca)$confianca_color_degree)), "RdBu"))(
            length(unique(V(confianca)$confianca_color_degree)))

#Saving as Vertex properties 
V(confianca)$vertex_confianca_color_degree<-
  vertex_confianca_color_degree[as.numeric(
  cut(V(confianca)$confianca_color_degree,
      breaks=length(unique(V(confianca)$confianca_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(confianca, es=E(confianca), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(confianca))
maxC <- rep(Inf, vcount(confianca))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(confianca, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(confianca)$weight)


#PLotting
plot(confianca, 
     layout=co,
     edge.color=V(confianca)$vertex_confianca_color_degree[edge.start],
     edge.arrow.size=closeness(confianca, weights = E(confianca)$confianca, mode="in",normalized = T),
     edge.width=E(confianca)$weight/10*mean(E(confianca)$weight),
     edge.curved = TRUE,
     vertex.color=V(confianca)$vertex_confianca_color_degree,
     vertex.size=(closeness(confianca, weights = E(confianca)$confianca, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(confianca,"LABEL_COR"),
     vertex.label.cex=closeness(confianca, weights = E(confianca)$confianca, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(confianca)$confianca_color_degree
b<-V(confianca)$vertex_confianca_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 31_CONFIANÇA", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(confianca, mode="in", weights = E(confianca)$confianca, normalized = T)), 
             sd(closeness(confianca, mode="in", weights = E(confianca)$confianca, normalized = T))
             )
       )
```

![](31_CONFIANÇA_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(confianca_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.015110 0.018810 0.017060 0.020790 0.030860
```

```r
sd(confianca_outcloseness_n)
```

```
## [1] 0.0061496
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(confianca)$outcloseness_n<-closeness(confianca, weights = E(confianca)$confianca, mode="out", normalized = T)

#Get Variable
V(confianca)$confianca_color_degree<-round(V(confianca)$outcloseness_n,6)

#Creating brewer pallette
vertex_confianca_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(confianca)$confianca_color_degree)), "RdBu"))(
            length(unique(V(confianca)$confianca_color_degree)))

#Saving as Vertex properties 
V(confianca)$vertex_confianca_color_degree<-
  vertex_confianca_color_degree[as.numeric(
  cut(V(confianca)$confianca_color_degree,
      breaks=length(unique(V(confianca)$confianca_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(confianca, es=E(confianca), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(confianca))
maxC <- rep(Inf, vcount(confianca))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(confianca, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(confianca)$weight)


#PLotting
plot(confianca, 
     layout=co,
     edge.color=V(confianca)$vertex_confianca_color_degree[edge.start],
     edge.arrow.size=closeness(confianca, weights = E(confianca)$confianca, mode="out",normalized = T),
     edge.width=E(confianca)$weight/10*mean(E(confianca)$weight),
     edge.curved = TRUE,
     vertex.color=V(confianca)$vertex_confianca_color_degree,
     vertex.size=(closeness(confianca, weights = E(confianca)$confianca, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(confianca,"LABEL_COR"),
     vertex.label.cex=closeness(confianca, weights = E(confianca)$confianca, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(confianca)$confianca_color_degree
b<-V(confianca)$vertex_confianca_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 31_CONFIANÇA", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(confianca, mode="out", weights = E(confianca)$confianca, normalized = T)), 
             sd(closeness(confianca, mode="out", weights = E(confianca)$confianca, normalized = T))
             )
       )
```

![](31_CONFIANÇA_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(confianca_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.01426 0.02224 0.02439 0.02395 0.02586 0.03773
```

```r
sd(confianca_totalcloseness_n)
```

```
## [1] 0.004143783
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(confianca)$allcloseness_n<-closeness(confianca, weights = E(confianca)$confianca, mode="all", normalized = T)

#Get Variable
V(confianca)$confianca_color_degree<-round(V(confianca)$allcloseness_n,6)

#Creating brewer pallette
vertex_confianca_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(confianca)$confianca_color_degree)), "RdBu"))(
            length(unique(V(confianca)$confianca_color_degree)))

#Saving as Vertex properties 
V(confianca)$vertex_confianca_color_degree<-
  vertex_confianca_color_degree[as.numeric(
  cut(V(confianca)$confianca_color_degree,
      breaks=length(unique(V(confianca)$confianca_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(confianca, es=E(confianca), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(confianca))
maxC <- rep(Inf, vcount(confianca))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(confianca, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(confianca)$weight)


#PLotting
plot(confianca, 
     layout=co,
     edge.color=V(confianca)$vertex_confianca_color_degree[edge.start],
     edge.arrow.size=closeness(confianca, weights = E(confianca)$confianca, mode="all",normalized = T),
     edge.width=E(confianca)$weight/10*mean(E(confianca)$weight),
     edge.curved = TRUE,
     vertex.color=V(confianca)$vertex_confianca_color_degree,
     vertex.size=(closeness(confianca, weights = E(confianca)$confianca, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(confianca,"LABEL_COR"),
     vertex.label.cex=closeness(confianca, weights = E(confianca)$confianca, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(confianca)$confianca_color_degree
b<-V(confianca)$vertex_confianca_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 31_CONFIANÇA", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(confianca, mode="all", weights = E(confianca)$confianca, normalized = T)), 
             sd(closeness(confianca, mode="all", weights = E(confianca)$confianca, normalized = T))
             )
       )
```

![](31_CONFIANÇA_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(confianca)$incloseness_n <- closeness(confianca, weights = E(confianca)$confianca, mode = "in", normalized = T) %>% round(6)
V(confianca)$outcloseness_n <- closeness(confianca, weights = E(confianca)$confianca, mode = "out", normalized = T) %>% round(6)
V(confianca)$totalcloseness_n <- closeness(confianca, weights = E(confianca)$confianca, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(confianca)$confianca_centr_closeness<- centralization.closeness(confianca)$res
confianca_centr_closeness<- centralization.closeness(confianca)$res
confianca_centr_closeness_all<- centralization.closeness(confianca)
```

###Centralization

```r
confianca_centr_closeness_all$centralization
```

```
## [1] 0.1578551
```

###Theoretical Max

```r
confianca_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(confianca)$confianca_centr_closeness<- centralization.closeness(confianca)$res

#Get Variable
V(confianca)$confianca_color_degree<-round(V(confianca)$confianca_centr_closeness,6)

#Creating brewer pallette
vertex_confianca_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(confianca)$confianca_color_degree)), "Spectral"))(
            length(unique(V(confianca)$confianca_color_degree)))

#Saving as Vertex properties 
V(confianca)$vertex_confianca_color_degree<-
  vertex_confianca_color_degree[as.numeric(
  cut(V(confianca)$confianca_color_degree,
      breaks=length(unique(V(confianca)$confianca_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(confianca, es=E(confianca), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(confianca))
maxC <- rep(Inf, vcount(confianca))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(confianca, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(confianca)$weight)


#PLotting
plot(confianca, 
     layout=co,
     edge.color=V(confianca)$vertex_confianca_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(confianca)$res,
     edge.width=E(confianca)$weight/10*mean(E(confianca)$weight),
     edge.curved = TRUE,
     vertex.color=V(confianca)$vertex_confianca_color_degree,
     vertex.size=centralization.closeness(confianca)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(confianca,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(confianca)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(confianca)$confianca_color_degree
b<-V(confianca)$vertex_confianca_color_degree
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
  title("Network Centralization Closeness - 31_CONFIANÇA", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(confianca)$res), 
             sd(centralization.closeness(confianca)$res)
             )
       )
```

![](31_CONFIANÇA_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
confianca_incloseness<- closeness(confianca, weights = E(confianca)$confianca, mode = "in") %>% round(6)
confianca_outcloseness<- closeness(confianca, weights = E(confianca)$confianca, mode = "out") %>% round(6)
confianca_totalcloseness<- closeness(confianca, weights = E(confianca)$confianca, mode = "total") %>% round(6)
confianca_incloseness_n<- closeness(confianca,weights = E(confianca)$confianca, mode = "in", normalized = T) %>% round(6)
confianca_outcloseness_n<- closeness(confianca,weights = E(confianca)$confianca, mode = "out", normalized = T) %>% round(6)
confianca_totalcloseness_n<- closeness(confianca,weights = E(confianca)$confianca, mode = "total", normalized = T) %>% round(6)
confianca_centr_closeness <- centralization.closeness(confianca)$res %>% round(6)
```

##Creating a datagrame of measures

```r
confianca_df_closseness <- data.frame(
confianca_incloseness,
confianca_outcloseness,
confianca_totalcloseness,
confianca_incloseness_n,
confianca_outcloseness_n,
confianca_totalcloseness_n,
confianca_centr_closeness) %>% round(6)

#Adding type
confianca_df_closseness <-cbind(confianca_df_closseness, V(confianca)$LABEL_COR)

#Adding names
names(confianca_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
confianca_df_closseness<-confianca_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(confianca_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-49c2554bddb591c70332" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-49c2554bddb591c70332">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"9.5e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000166\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7.7e-05\" data-max=\"0.000203\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.017592\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.030861\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.014264\" data-max=\"0.037728\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[8.8e-05,8.1e-05,9.5e-05,7.9e-05,8e-05,7.9e-05,7.3e-05,8.4e-05,6.9e-05,8.4e-05,8e-05,8.1e-05,6.4e-05,6.8e-05,7.6e-05,7.7e-05,8.1e-05,8.3e-05,6.5e-05,7.4e-05,7.8e-05,7.1e-05,7.9e-05,5.5e-05,8.1e-05,7.6e-05,6.1e-05,7.9e-05,7.9e-05,7.9e-05,7.9e-05,7.6e-05,8e-05,6.1e-05,7.9e-05,7e-05,8.2e-05,7.8e-05,6.7e-05,7.7e-05,7.8e-05,7.5e-05,7.4e-05,8.3e-05,7.4e-05,8e-05,5.3e-05,6.9e-05,6.9e-05,6.9e-05,6.9e-05,6.9e-05,6.3e-05,7.3e-05,6.2e-05,7.9e-05,7.7e-05,6.7e-05,6.3e-05,8.1e-05,8e-05,5.8e-05,7.7e-05,7.6e-05,7.1e-05,2.9e-05,5.1e-05,7.6e-05,8.5e-05,7.6e-05,7.7e-05,7.7e-05,7.7e-05,7.8e-05,7.8e-05,7.6e-05,7.7e-05,8.1e-05,5.9e-05,5.9e-05,5.9e-05,5.9e-05,7.7e-05,6.9e-05,7.3e-05,7.7e-05,7.4e-05,5.3e-05,7.7e-05,7.6e-05,7.6e-05,5.5e-05,5.9e-05,7.3e-05,7.7e-05,7e-05,5.3e-05,6.8e-05,7.8e-05,7.8e-05,5.9e-05,6.9e-05,6.7e-05,6.7e-05,7.6e-05,7.7e-05,7.6e-05,6.7e-05,6.8e-05,7.7e-05,7.6e-05,7.8e-05,7.7e-05,7.7e-05,7.7e-05,7.6e-05,7.6e-05,7.6e-05,7.7e-05,7.8e-05,7.6e-05,7.6e-05,7.7e-05,7.7e-05,7.7e-05,7.7e-05,7.7e-05,7.7e-05,7.7e-05,7.3e-05,7.7e-05,7.7e-05,7.7e-05,7.7e-05,7.7e-05,7.9e-05,7.7e-05,7.6e-05,7.7e-05,7.6e-05,7.7e-05,7.6e-05,7.6e-05,7.6e-05,7.1e-05,8.1e-05,7.7e-05,7.7e-05,6.9e-05,7.3e-05,7.7e-05,7.3e-05,7.7e-05,7.3e-05,7.8e-05,5.7e-05,7.4e-05,7.8e-05,7.7e-05,5.3e-05,5.6e-05,6.4e-05,6.6e-05,6.5e-05,5.3e-05,5.1e-05,6.3e-05,5.3e-05,5.3e-05,6.3e-05,6.2e-05,5.3e-05,6.4e-05,6.3e-05,6.6e-05,6.6e-05,6.4e-05,5.3e-05,6.4e-05,6.1e-05,6.3e-05,6.3e-05,6.6e-05,6.5e-05,6.1e-05,5.8e-05,7.1e-05],[0.000166,0.000135,0.000156,0.000127,0.000126,0.000108,0.000106,0.00013,0.000129,0.000105,0.000111,9.7e-05,0.000125,0.000118,0.000121,0.000105,0.000137,0.000124,0.000104,0.000114,0.000116,0.000114,0.000116,0.000121,0.000104,7.8e-05,0.000109,0.000113,0.000123,0.000122,0.00012,0.000108,0.000136,7.7e-05,0.0001,0.000118,0.000118,9.4e-05,0.000103,0.000136,0.000126,0.000127,0.000127,0.000111,0.000119,0.000119,8.9e-05,0.000105,0.000105,0.000105,0.000105,0.000105,0.0001,0.000101,0.000123,0.000118,0.000112,9.8e-05,8.8e-05,0.000119,2.9e-05,7.7e-05,0.000117,0.000108,0.000116,2.9e-05,2.9e-05,0.000115,0.000101,0.000101,9.8e-05,0.0001,0.000116,0.000106,0.000101,0.000103,0.000111,0.00012,7.5e-05,7.5e-05,7.5e-05,7.5e-05,0.000105,7.9e-05,0.000111,0.000117,0.000105,0.0001,0.00011,9.6e-05,0.000112,0.000109,0.000108,8.7e-05,0.000114,0.000106,7e-05,7.8e-05,9.8e-05,0.00013,7.6e-05,0.000117,9.9e-05,9.9e-05,0.000111,0.000109,0.000102,9.7e-05,9.7e-05,0.000109,0.00011,0.000109,0.00011,0.000114,0.000105,0.000131,0.000112,0.000109,0.000111,0.000117,0.00011,0.000125,9.6e-05,9.6e-05,9.6e-05,0.0001,9.6e-05,9.7e-05,9.6e-05,9.8e-05,0.000109,0.000105,0.0001,9.7e-05,9.9e-05,0.000105,9.3e-05,9.2e-05,9.4e-05,0.000103,9.5e-05,9.2e-05,9.4e-05,9.6e-05,9.4e-05,9.2e-05,9.9e-05,6.8e-05,8.4e-05,8.9e-05,8.9e-05,8.9e-05,8.9e-05,8.9e-05,7.7e-05,8.7e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000203,0.000161,0.000194,0.000155,0.000173,0.000172,0.000123,0.00016,0.00014,0.000148,0.000149,0.000146,0.000138,0.000126,0.000148,0.000127,0.000175,0.000155,0.000123,0.000137,0.000139,0.00013,0.000141,0.000132,0.000146,0.000126,0.000131,0.000142,0.000146,0.000151,0.000145,0.000133,0.000161,0.000108,0.000149,0.000136,0.000151,0.000148,0.000119,0.000174,0.000141,0.000141,0.000143,0.000145,0.000164,0.000155,0.00012,0.000122,0.000122,0.000122,0.000122,0.000122,0.000115,0.000134,0.000138,0.000152,0.000134,0.000123,0.000111,0.000152,0.000133,9.1e-05,0.000136,0.000145,0.00013,7.7e-05,8.6e-05,0.00013,0.000177,0.00013,0.000132,0.00013,0.000139,0.000137,0.000134,0.000134,0.000139,0.000147,8.8e-05,8.8e-05,8.8e-05,8.8e-05,0.000144,0.000121,0.000137,0.000131,0.00013,0.000113,0.000135,0.00013,0.000133,0.000119,0.000125,0.00012,0.000166,0.00013,8.7e-05,0.000105,0.000132,0.000152,9.6e-05,0.000164,0.000135,0.000135,0.000133,0.000165,0.000164,0.000162,0.000162,0.000139,0.000132,0.000131,0.000136,0.000133,0.000131,0.000152,0.000133,0.000132,0.000133,0.000138,0.000132,0.000144,0.000131,0.000128,0.000131,0.000131,0.00013,0.00013,0.000131,0.000131,0.000131,0.000131,0.000129,0.000131,0.000131,0.000134,0.00013,0.00013,0.000135,0.000132,0.000132,0.000131,0.00013,0.000131,0.000107,0.000137,0.00013,0.000133,0.000111,0.000122,0.000127,0.000122,0.000127,0.000122,0.000137,0.000116,0.000125,0.000128,0.000127,9.5e-05,8.5e-05,9.8e-05,0.000107,0.000104,8.8e-05,8.6e-05,9.9e-05,8.8e-05,8.8e-05,9.9e-05,9.5e-05,8.8e-05,0.000101,9.9e-05,0.000105,0.000107,0.000102,8.8e-05,0.000102,9.6e-05,0.000102,0.000102,0.000105,0.000106,9.7e-05,8.5e-05,0.000104],[0.016333,0.015017,0.017592,0.014679,0.014811,0.014712,0.01364,0.015544,0.012768,0.015691,0.014815,0.015118,0.011958,0.012708,0.014157,0.014323,0.015121,0.01536,0.012072,0.013785,0.014526,0.013135,0.014602,0.010212,0.015025,0.014214,0.011435,0.014612,0.014639,0.014628,0.014763,0.014201,0.014915,0.011332,0.014624,0.013064,0.015302,0.014544,0.012451,0.014352,0.014469,0.013889,0.013779,0.015465,0.013729,0.014842,0.009867,0.0128,0.0128,0.0128,0.0128,0.0128,0.011664,0.013563,0.011529,0.014647,0.014342,0.012431,0.011711,0.015051,0.014872,0.010768,0.014254,0.014211,0.013149,0.005348,0.009481,0.014221,0.015873,0.014095,0.01431,0.014371,0.014335,0.014419,0.014459,0.014209,0.014284,0.015042,0.010931,0.010931,0.010931,0.010931,0.014286,0.012749,0.01351,0.014382,0.01379,0.00985,0.014358,0.014123,0.014194,0.010199,0.011029,0.013573,0.01437,0.013089,0.00985,0.012668,0.014429,0.014479,0.01094,0.012891,0.012453,0.012453,0.014206,0.014244,0.014226,0.012472,0.012643,0.014286,0.014229,0.014535,0.014304,0.014336,0.014386,0.014217,0.014204,0.014205,0.014308,0.014458,0.014218,0.014189,0.014341,0.014271,0.014341,0.014304,0.014271,0.0143,0.014314,0.013616,0.014238,0.014292,0.014315,0.014293,0.014342,0.014602,0.014247,0.01422,0.014364,0.014185,0.014289,0.014192,0.014166,0.014182,0.013137,0.015073,0.014284,0.014259,0.012741,0.013545,0.014241,0.013545,0.014241,0.013545,0.014546,0.010563,0.013743,0.014428,0.014401,0.009947,0.010363,0.011815,0.012212,0.012069,0.009913,0.009561,0.011746,0.009913,0.009913,0.011775,0.011511,0.009913,0.011876,0.011787,0.012211,0.012267,0.011936,0.009913,0.011936,0.011421,0.011757,0.011757,0.012229,0.012045,0.011401,0.010736,0.013231],[0.030861,0.025176,0.029004,0.02355,0.023402,0.020128,0.01967,0.024137,0.023954,0.019618,0.020669,0.018053,0.023294,0.021895,0.022567,0.019474,0.025567,0.023043,0.019281,0.021153,0.021598,0.021218,0.021633,0.02245,0.019391,0.014527,0.020195,0.021048,0.022853,0.022697,0.022275,0.020015,0.025255,0.014339,0.018567,0.021975,0.021898,0.017541,0.019181,0.025227,0.023376,0.023658,0.023649,0.020713,0.022067,0.02214,0.016535,0.019562,0.019562,0.019562,0.019562,0.019562,0.018686,0.018811,0.022842,0.022035,0.020743,0.018293,0.016326,0.022169,0.005348,0.014289,0.021808,0.02006,0.021656,0.005373,0.005399,0.021365,0.018872,0.018862,0.018184,0.01855,0.02149,0.019745,0.018807,0.019235,0.020598,0.022393,0.013887,0.013887,0.013887,0.013887,0.019618,0.014645,0.020557,0.021854,0.019614,0.018688,0.02048,0.017905,0.020773,0.020284,0.020132,0.016196,0.021274,0.019708,0.012945,0.014512,0.018225,0.024263,0.014177,0.021759,0.01841,0.01841,0.020651,0.020231,0.019036,0.01798,0.01798,0.020281,0.020381,0.020184,0.020462,0.021284,0.019604,0.02439,0.020815,0.020255,0.020591,0.021744,0.020503,0.023288,0.017849,0.017849,0.017849,0.01857,0.017784,0.018111,0.017784,0.018234,0.02029,0.019503,0.018628,0.018104,0.018436,0.019589,0.017325,0.017176,0.017475,0.019138,0.017672,0.017176,0.017547,0.017784,0.017475,0.017176,0.018432,0.012682,0.015584,0.01653,0.01653,0.01653,0.01653,0.01653,0.014296,0.016189,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.037728,0.029913,0.03606,0.028784,0.032124,0.031904,0.0228,0.029693,0.026109,0.027592,0.027633,0.027145,0.025581,0.023402,0.027482,0.023694,0.032466,0.028793,0.022926,0.025546,0.025869,0.024269,0.026168,0.024509,0.027086,0.023464,0.024276,0.026346,0.027126,0.028135,0.027043,0.024727,0.02999,0.020017,0.02765,0.025299,0.028084,0.027507,0.022214,0.032427,0.026142,0.02626,0.026522,0.026988,0.030577,0.028739,0.022257,0.022713,0.022713,0.022713,0.022713,0.022713,0.021316,0.024983,0.025701,0.028272,0.025013,0.022803,0.020619,0.028328,0.024803,0.016974,0.025344,0.026972,0.024168,0.014264,0.016052,0.024244,0.032973,0.024212,0.024522,0.024156,0.025837,0.025528,0.024886,0.02493,0.025844,0.027325,0.016431,0.016431,0.016431,0.016431,0.026751,0.022499,0.025424,0.024442,0.024115,0.021003,0.025027,0.02426,0.024787,0.022043,0.02318,0.022313,0.030795,0.024103,0.016168,0.019532,0.024503,0.028272,0.017904,0.030447,0.025196,0.025196,0.024727,0.030617,0.030552,0.030097,0.030117,0.025848,0.024548,0.024429,0.025368,0.02479,0.024377,0.028311,0.024731,0.024532,0.024817,0.025733,0.024509,0.026801,0.02439,0.023837,0.02439,0.024448,0.024087,0.02409,0.024371,0.024349,0.024298,0.02439,0.024071,0.024413,0.024425,0.024956,0.024146,0.024253,0.02502,0.024577,0.024554,0.02432,0.024225,0.024276,0.01987,0.025535,0.024121,0.024777,0.020619,0.02278,0.023571,0.02278,0.023571,0.02278,0.025486,0.021568,0.023204,0.023883,0.023571,0.017714,0.015721,0.018251,0.019914,0.019411,0.01635,0.015966,0.018423,0.01635,0.01635,0.018385,0.017753,0.01635,0.018739,0.018392,0.019606,0.019855,0.018976,0.01635,0.018976,0.017888,0.01892,0.01892,0.019604,0.019794,0.017968,0.015808,0.019379],[0.372,0.343808,0.392405,0.334532,0.30897,0.301948,0.304918,0.319039,0.311558,0.288372,0.294304,0.286154,0.333932,0.322917,0.287481,0.29108,0.352273,0.315254,0.292913,0.286154,0.303426,0.285714,0.296178,0.333333,0.285714,0.240621,0.277198,0.319039,0.333333,0.316327,0.310518,0.2976,0.315789,0.224096,0.230198,0.285714,0.307438,0.267626,0.298555,0.32069,0.334532,0.317406,0.317406,0.293839,0.299035,0.300485,0.23192,0.291994,0.291994,0.291994,0.291994,0.291994,0.282675,0.2976,0.331551,0.319039,0.304419,0.288372,0.261972,0.324607,0.005348,0.228221,0.297125,0.291536,0.299517,0.005376,0.005405,0.307438,0.298555,0.283537,0.283537,0.294304,0.296651,0.296178,0.289269,0.290172,0.288372,0.314721,0.229346,0.229346,0.229346,0.229346,0.287926,0.250674,0.295238,0.299035,0.292453,0.274742,0.302932,0.286154,0.291536,0.302439,0.289269,0.24933,0.2976,0.288372,0.216028,0.223289,0.285714,0.307438,0.234552,0.318493,0.223022,0.223022,0.290625,0.301948,0.290172,0.231056,0.231056,0.296178,0.296651,0.28972,0.287037,0.285714,0.287037,0.291994,0.29477,0.295238,0.290172,0.290172,0.295238,0.311037,0.283537,0.283537,0.283537,0.291994,0.282675,0.285714,0.282675,0.291994,0.291536,0.291536,0.292453,0.282675,0.282675,0.298555,0.284839,0.282675,0.282675,0.291536,0.283105,0.282675,0.286154,0.282675,0.282675,0.282675,0.291536,0.221165,0.252374,0.256552,0.256552,0.256552,0.256552,0.256552,0.226553,0.227106,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(confianca_df_closseness, by=list(confianca_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(confianca_df_closseness, by=list(confianca_df_closseness$Type), FUN=sd, na.rm=TRUE) 

names(aggdata_sd) <- c("Group","Type","In Closeness(SD)", "Out Closeness(SD)", "Total Closeness(SD)","In Closeness Normalized(SD)", "Out Closeness Normalized(SD)", "Total Closeness Normalized(SD)", "Centralization Closeness(SD)")

#Removing Type variable
aggdata_sd<-aggdata_sd[,-c(2)]

#Merging mean and standart deviation
total_table <- merge(aggdata_mean,aggdata_sd,by="Group")

#Rounding
Group<-total_table[,c(1)] #Keeping group
total_table<-total_table[,-c(1)] %>% round(6) #Rouding
total_table<-cbind(Group,total_table) #Binding toghter

#Organizing Variabels
total_table<-total_table[c("Group","In Closeness(M)", "In Closeness(SD)", "Out Closeness(M)", "Out Closeness(SD)", "Total Closeness(M)","Total Closeness(SD)","In Closeness Normalized(M)", "In Closeness Normalized(SD)", "Out Closeness Normalized(M)", "Out Closeness Normalized(SD)", "Total Closeness Normalized(M)","Total Closeness Normalized(SD)", "Centralization Closeness(M)","Centralization Closeness(SD)")]
```

##Plotting final table with round for Closseness

```r
datatable(total_table, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-64e942d3c14ea28a5555" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-64e942d3c14ea28a5555">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6.2e-05\" data-max=\"9.5e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000166\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4e-06\" data-max=\"3.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000104\" data-max=\"0.000203\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2e-06\" data-max=\"2.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.011441\" data-max=\"0.017592\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000143\" data-max=\"0.002493\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.030861\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000776\" data-max=\"0.005946\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.019379\" data-max=\"0.037728\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000283\" data-max=\"0.004953\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003897\" data-max=\"0.126\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[7.6e-05,6.2e-05,7.4e-05,8.1e-05,7.3e-05,8.1e-05,7.9e-05,9.5e-05,7.9e-05,7.7e-05,8.1e-05,6.7e-05,7.2e-05,7.9e-05,8e-05,7.3e-05,7.1e-05,7.8e-05,8.8e-05,7e-05,8.3e-05,7.6e-05,7.7e-05],[6e-06,7e-06,null,null,null,null,2e-06,null,null,null,null,1.3e-05,5e-06,1e-06,1e-06,7e-06,null,null,null,4e-06,null,2e-06,1e-06],[0.000115,5.2e-05,0.000114,0.000135,0.000106,0.000119,0.000107,0.000156,0.000116,9.9e-05,0.00012,0.000101,0.000122,0.000115,0.000124,0.000111,2.9e-05,0.00013,0.000166,0.000102,0.000124,8.6e-05,0.000105],[2.8e-05,3.2e-05,null,null,null,null,1.3e-05,null,null,null,null,2.5e-05,6e-06,1e-05,1.5e-05,1.7e-05,null,null,null,4e-06,null,2.6e-05,1e-05],[0.000138,0.000104,0.000137,0.000161,0.000123,0.000152,0.000139,0.000194,0.000141,0.00013,0.000147,0.000125,0.000134,0.00015,0.000173,0.000143,0.000104,0.000152,0.000203,0.000133,0.000155,0.000128,0.000136],[2.5e-05,1.6e-05,null,null,null,null,3e-06,null,null,null,null,2.2e-05,1.1e-05,4e-06,2e-06,2.7e-05,null,null,null,1.6e-05,null,3e-06,9e-06],[0.014042,0.011441,0.013785,0.015017,0.01364,0.015051,0.014737,0.017592,0.014602,0.014284,0.015042,0.012511,0.013299,0.014751,0.014881,0.013655,0.013231,0.014479,0.016333,0.012945,0.01536,0.014132,0.014267],[0.001148,0.001257,null,null,null,null,0.000294,null,null,null,null,0.002493,0.000835,0.000161,0.000213,0.001365,null,null,null,0.000714,null,0.000338,0.000143],[0.021405,0.009612,0.021153,0.025176,0.01967,0.022169,0.019941,0.029004,0.021633,0.018432,0.022393,0.018874,0.022776,0.021427,0.023032,0.020596,0.005348,0.024263,0.030861,0.018899,0.023043,0.015932,0.019553],[0.005128,0.005946,null,null,null,null,0.00241,null,null,null,null,0.004574,0.001247,0.001933,0.002738,0.00315,null,null,null,0.000776,null,0.004845,0.00186],[0.025584,0.019434,0.025546,0.029913,0.0228,0.028328,0.025917,0.03606,0.026168,0.024121,0.027325,0.023197,0.024831,0.027836,0.032165,0.026525,0.019379,0.028272,0.037728,0.024799,0.028793,0.023854,0.025343],[0.00471,0.002977,null,null,null,null,0.000408,null,null,null,null,0.004167,0.002021,0.000678,0.000283,0.004953,null,null,null,0.002918,null,0.0006,0.001641],[0.300898,0.099209,0.286154,0.343808,0.304918,0.324607,0.301713,0.392405,0.296178,0.291536,0.314721,0.267099,0.320162,0.302766,0.321064,0.297137,0.005348,0.307438,0.372,0.268782,0.315254,0.234593,0.289489],[0.043056,0.126,null,null,null,null,0.018242,null,null,null,null,0.070876,0.003897,0.031763,0.027255,0.024497,null,null,null,0.031785,null,0.102905,0.01174]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
confianca_df_closseness <- data.frame(
confianca_incloseness,
confianca_outcloseness,
confianca_totalcloseness,
confianca_incloseness_n,
confianca_outcloseness_n,
confianca_totalcloseness_n,
confianca_centr_closeness) %>% round(6)

#Adding type
confianca_df_closseness <-cbind(confianca_df_closseness, V(confianca)$TIPO1)

#Adding names
names(confianca_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
confianca_df_closseness<-confianca_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(confianca_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-5cc9cc2a615a7056f790" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-5cc9cc2a615a7056f790">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"9.5e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000166\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7.7e-05\" data-max=\"0.000203\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.017592\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.030861\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.014264\" data-max=\"0.037728\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[8.8e-05,8.1e-05,9.5e-05,7.9e-05,8e-05,7.9e-05,7.3e-05,8.4e-05,6.9e-05,8.4e-05,8e-05,8.1e-05,6.4e-05,6.8e-05,7.6e-05,7.7e-05,8.1e-05,8.3e-05,6.5e-05,7.4e-05,7.8e-05,7.1e-05,7.9e-05,5.5e-05,8.1e-05,7.6e-05,6.1e-05,7.9e-05,7.9e-05,7.9e-05,7.9e-05,7.6e-05,8e-05,6.1e-05,7.9e-05,7e-05,8.2e-05,7.8e-05,6.7e-05,7.7e-05,7.8e-05,7.5e-05,7.4e-05,8.3e-05,7.4e-05,8e-05,5.3e-05,6.9e-05,6.9e-05,6.9e-05,6.9e-05,6.9e-05,6.3e-05,7.3e-05,6.2e-05,7.9e-05,7.7e-05,6.7e-05,6.3e-05,8.1e-05,8e-05,5.8e-05,7.7e-05,7.6e-05,7.1e-05,2.9e-05,5.1e-05,7.6e-05,8.5e-05,7.6e-05,7.7e-05,7.7e-05,7.7e-05,7.8e-05,7.8e-05,7.6e-05,7.7e-05,8.1e-05,5.9e-05,5.9e-05,5.9e-05,5.9e-05,7.7e-05,6.9e-05,7.3e-05,7.7e-05,7.4e-05,5.3e-05,7.7e-05,7.6e-05,7.6e-05,5.5e-05,5.9e-05,7.3e-05,7.7e-05,7e-05,5.3e-05,6.8e-05,7.8e-05,7.8e-05,5.9e-05,6.9e-05,6.7e-05,6.7e-05,7.6e-05,7.7e-05,7.6e-05,6.7e-05,6.8e-05,7.7e-05,7.6e-05,7.8e-05,7.7e-05,7.7e-05,7.7e-05,7.6e-05,7.6e-05,7.6e-05,7.7e-05,7.8e-05,7.6e-05,7.6e-05,7.7e-05,7.7e-05,7.7e-05,7.7e-05,7.7e-05,7.7e-05,7.7e-05,7.3e-05,7.7e-05,7.7e-05,7.7e-05,7.7e-05,7.7e-05,7.9e-05,7.7e-05,7.6e-05,7.7e-05,7.6e-05,7.7e-05,7.6e-05,7.6e-05,7.6e-05,7.1e-05,8.1e-05,7.7e-05,7.7e-05,6.9e-05,7.3e-05,7.7e-05,7.3e-05,7.7e-05,7.3e-05,7.8e-05,5.7e-05,7.4e-05,7.8e-05,7.7e-05,5.3e-05,5.6e-05,6.4e-05,6.6e-05,6.5e-05,5.3e-05,5.1e-05,6.3e-05,5.3e-05,5.3e-05,6.3e-05,6.2e-05,5.3e-05,6.4e-05,6.3e-05,6.6e-05,6.6e-05,6.4e-05,5.3e-05,6.4e-05,6.1e-05,6.3e-05,6.3e-05,6.6e-05,6.5e-05,6.1e-05,5.8e-05,7.1e-05],[0.000166,0.000135,0.000156,0.000127,0.000126,0.000108,0.000106,0.00013,0.000129,0.000105,0.000111,9.7e-05,0.000125,0.000118,0.000121,0.000105,0.000137,0.000124,0.000104,0.000114,0.000116,0.000114,0.000116,0.000121,0.000104,7.8e-05,0.000109,0.000113,0.000123,0.000122,0.00012,0.000108,0.000136,7.7e-05,0.0001,0.000118,0.000118,9.4e-05,0.000103,0.000136,0.000126,0.000127,0.000127,0.000111,0.000119,0.000119,8.9e-05,0.000105,0.000105,0.000105,0.000105,0.000105,0.0001,0.000101,0.000123,0.000118,0.000112,9.8e-05,8.8e-05,0.000119,2.9e-05,7.7e-05,0.000117,0.000108,0.000116,2.9e-05,2.9e-05,0.000115,0.000101,0.000101,9.8e-05,0.0001,0.000116,0.000106,0.000101,0.000103,0.000111,0.00012,7.5e-05,7.5e-05,7.5e-05,7.5e-05,0.000105,7.9e-05,0.000111,0.000117,0.000105,0.0001,0.00011,9.6e-05,0.000112,0.000109,0.000108,8.7e-05,0.000114,0.000106,7e-05,7.8e-05,9.8e-05,0.00013,7.6e-05,0.000117,9.9e-05,9.9e-05,0.000111,0.000109,0.000102,9.7e-05,9.7e-05,0.000109,0.00011,0.000109,0.00011,0.000114,0.000105,0.000131,0.000112,0.000109,0.000111,0.000117,0.00011,0.000125,9.6e-05,9.6e-05,9.6e-05,0.0001,9.6e-05,9.7e-05,9.6e-05,9.8e-05,0.000109,0.000105,0.0001,9.7e-05,9.9e-05,0.000105,9.3e-05,9.2e-05,9.4e-05,0.000103,9.5e-05,9.2e-05,9.4e-05,9.6e-05,9.4e-05,9.2e-05,9.9e-05,6.8e-05,8.4e-05,8.9e-05,8.9e-05,8.9e-05,8.9e-05,8.9e-05,7.7e-05,8.7e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000203,0.000161,0.000194,0.000155,0.000173,0.000172,0.000123,0.00016,0.00014,0.000148,0.000149,0.000146,0.000138,0.000126,0.000148,0.000127,0.000175,0.000155,0.000123,0.000137,0.000139,0.00013,0.000141,0.000132,0.000146,0.000126,0.000131,0.000142,0.000146,0.000151,0.000145,0.000133,0.000161,0.000108,0.000149,0.000136,0.000151,0.000148,0.000119,0.000174,0.000141,0.000141,0.000143,0.000145,0.000164,0.000155,0.00012,0.000122,0.000122,0.000122,0.000122,0.000122,0.000115,0.000134,0.000138,0.000152,0.000134,0.000123,0.000111,0.000152,0.000133,9.1e-05,0.000136,0.000145,0.00013,7.7e-05,8.6e-05,0.00013,0.000177,0.00013,0.000132,0.00013,0.000139,0.000137,0.000134,0.000134,0.000139,0.000147,8.8e-05,8.8e-05,8.8e-05,8.8e-05,0.000144,0.000121,0.000137,0.000131,0.00013,0.000113,0.000135,0.00013,0.000133,0.000119,0.000125,0.00012,0.000166,0.00013,8.7e-05,0.000105,0.000132,0.000152,9.6e-05,0.000164,0.000135,0.000135,0.000133,0.000165,0.000164,0.000162,0.000162,0.000139,0.000132,0.000131,0.000136,0.000133,0.000131,0.000152,0.000133,0.000132,0.000133,0.000138,0.000132,0.000144,0.000131,0.000128,0.000131,0.000131,0.00013,0.00013,0.000131,0.000131,0.000131,0.000131,0.000129,0.000131,0.000131,0.000134,0.00013,0.00013,0.000135,0.000132,0.000132,0.000131,0.00013,0.000131,0.000107,0.000137,0.00013,0.000133,0.000111,0.000122,0.000127,0.000122,0.000127,0.000122,0.000137,0.000116,0.000125,0.000128,0.000127,9.5e-05,8.5e-05,9.8e-05,0.000107,0.000104,8.8e-05,8.6e-05,9.9e-05,8.8e-05,8.8e-05,9.9e-05,9.5e-05,8.8e-05,0.000101,9.9e-05,0.000105,0.000107,0.000102,8.8e-05,0.000102,9.6e-05,0.000102,0.000102,0.000105,0.000106,9.7e-05,8.5e-05,0.000104],[0.016333,0.015017,0.017592,0.014679,0.014811,0.014712,0.01364,0.015544,0.012768,0.015691,0.014815,0.015118,0.011958,0.012708,0.014157,0.014323,0.015121,0.01536,0.012072,0.013785,0.014526,0.013135,0.014602,0.010212,0.015025,0.014214,0.011435,0.014612,0.014639,0.014628,0.014763,0.014201,0.014915,0.011332,0.014624,0.013064,0.015302,0.014544,0.012451,0.014352,0.014469,0.013889,0.013779,0.015465,0.013729,0.014842,0.009867,0.0128,0.0128,0.0128,0.0128,0.0128,0.011664,0.013563,0.011529,0.014647,0.014342,0.012431,0.011711,0.015051,0.014872,0.010768,0.014254,0.014211,0.013149,0.005348,0.009481,0.014221,0.015873,0.014095,0.01431,0.014371,0.014335,0.014419,0.014459,0.014209,0.014284,0.015042,0.010931,0.010931,0.010931,0.010931,0.014286,0.012749,0.01351,0.014382,0.01379,0.00985,0.014358,0.014123,0.014194,0.010199,0.011029,0.013573,0.01437,0.013089,0.00985,0.012668,0.014429,0.014479,0.01094,0.012891,0.012453,0.012453,0.014206,0.014244,0.014226,0.012472,0.012643,0.014286,0.014229,0.014535,0.014304,0.014336,0.014386,0.014217,0.014204,0.014205,0.014308,0.014458,0.014218,0.014189,0.014341,0.014271,0.014341,0.014304,0.014271,0.0143,0.014314,0.013616,0.014238,0.014292,0.014315,0.014293,0.014342,0.014602,0.014247,0.01422,0.014364,0.014185,0.014289,0.014192,0.014166,0.014182,0.013137,0.015073,0.014284,0.014259,0.012741,0.013545,0.014241,0.013545,0.014241,0.013545,0.014546,0.010563,0.013743,0.014428,0.014401,0.009947,0.010363,0.011815,0.012212,0.012069,0.009913,0.009561,0.011746,0.009913,0.009913,0.011775,0.011511,0.009913,0.011876,0.011787,0.012211,0.012267,0.011936,0.009913,0.011936,0.011421,0.011757,0.011757,0.012229,0.012045,0.011401,0.010736,0.013231],[0.030861,0.025176,0.029004,0.02355,0.023402,0.020128,0.01967,0.024137,0.023954,0.019618,0.020669,0.018053,0.023294,0.021895,0.022567,0.019474,0.025567,0.023043,0.019281,0.021153,0.021598,0.021218,0.021633,0.02245,0.019391,0.014527,0.020195,0.021048,0.022853,0.022697,0.022275,0.020015,0.025255,0.014339,0.018567,0.021975,0.021898,0.017541,0.019181,0.025227,0.023376,0.023658,0.023649,0.020713,0.022067,0.02214,0.016535,0.019562,0.019562,0.019562,0.019562,0.019562,0.018686,0.018811,0.022842,0.022035,0.020743,0.018293,0.016326,0.022169,0.005348,0.014289,0.021808,0.02006,0.021656,0.005373,0.005399,0.021365,0.018872,0.018862,0.018184,0.01855,0.02149,0.019745,0.018807,0.019235,0.020598,0.022393,0.013887,0.013887,0.013887,0.013887,0.019618,0.014645,0.020557,0.021854,0.019614,0.018688,0.02048,0.017905,0.020773,0.020284,0.020132,0.016196,0.021274,0.019708,0.012945,0.014512,0.018225,0.024263,0.014177,0.021759,0.01841,0.01841,0.020651,0.020231,0.019036,0.01798,0.01798,0.020281,0.020381,0.020184,0.020462,0.021284,0.019604,0.02439,0.020815,0.020255,0.020591,0.021744,0.020503,0.023288,0.017849,0.017849,0.017849,0.01857,0.017784,0.018111,0.017784,0.018234,0.02029,0.019503,0.018628,0.018104,0.018436,0.019589,0.017325,0.017176,0.017475,0.019138,0.017672,0.017176,0.017547,0.017784,0.017475,0.017176,0.018432,0.012682,0.015584,0.01653,0.01653,0.01653,0.01653,0.01653,0.014296,0.016189,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.037728,0.029913,0.03606,0.028784,0.032124,0.031904,0.0228,0.029693,0.026109,0.027592,0.027633,0.027145,0.025581,0.023402,0.027482,0.023694,0.032466,0.028793,0.022926,0.025546,0.025869,0.024269,0.026168,0.024509,0.027086,0.023464,0.024276,0.026346,0.027126,0.028135,0.027043,0.024727,0.02999,0.020017,0.02765,0.025299,0.028084,0.027507,0.022214,0.032427,0.026142,0.02626,0.026522,0.026988,0.030577,0.028739,0.022257,0.022713,0.022713,0.022713,0.022713,0.022713,0.021316,0.024983,0.025701,0.028272,0.025013,0.022803,0.020619,0.028328,0.024803,0.016974,0.025344,0.026972,0.024168,0.014264,0.016052,0.024244,0.032973,0.024212,0.024522,0.024156,0.025837,0.025528,0.024886,0.02493,0.025844,0.027325,0.016431,0.016431,0.016431,0.016431,0.026751,0.022499,0.025424,0.024442,0.024115,0.021003,0.025027,0.02426,0.024787,0.022043,0.02318,0.022313,0.030795,0.024103,0.016168,0.019532,0.024503,0.028272,0.017904,0.030447,0.025196,0.025196,0.024727,0.030617,0.030552,0.030097,0.030117,0.025848,0.024548,0.024429,0.025368,0.02479,0.024377,0.028311,0.024731,0.024532,0.024817,0.025733,0.024509,0.026801,0.02439,0.023837,0.02439,0.024448,0.024087,0.02409,0.024371,0.024349,0.024298,0.02439,0.024071,0.024413,0.024425,0.024956,0.024146,0.024253,0.02502,0.024577,0.024554,0.02432,0.024225,0.024276,0.01987,0.025535,0.024121,0.024777,0.020619,0.02278,0.023571,0.02278,0.023571,0.02278,0.025486,0.021568,0.023204,0.023883,0.023571,0.017714,0.015721,0.018251,0.019914,0.019411,0.01635,0.015966,0.018423,0.01635,0.01635,0.018385,0.017753,0.01635,0.018739,0.018392,0.019606,0.019855,0.018976,0.01635,0.018976,0.017888,0.01892,0.01892,0.019604,0.019794,0.017968,0.015808,0.019379],[0.372,0.343808,0.392405,0.334532,0.30897,0.301948,0.304918,0.319039,0.311558,0.288372,0.294304,0.286154,0.333932,0.322917,0.287481,0.29108,0.352273,0.315254,0.292913,0.286154,0.303426,0.285714,0.296178,0.333333,0.285714,0.240621,0.277198,0.319039,0.333333,0.316327,0.310518,0.2976,0.315789,0.224096,0.230198,0.285714,0.307438,0.267626,0.298555,0.32069,0.334532,0.317406,0.317406,0.293839,0.299035,0.300485,0.23192,0.291994,0.291994,0.291994,0.291994,0.291994,0.282675,0.2976,0.331551,0.319039,0.304419,0.288372,0.261972,0.324607,0.005348,0.228221,0.297125,0.291536,0.299517,0.005376,0.005405,0.307438,0.298555,0.283537,0.283537,0.294304,0.296651,0.296178,0.289269,0.290172,0.288372,0.314721,0.229346,0.229346,0.229346,0.229346,0.287926,0.250674,0.295238,0.299035,0.292453,0.274742,0.302932,0.286154,0.291536,0.302439,0.289269,0.24933,0.2976,0.288372,0.216028,0.223289,0.285714,0.307438,0.234552,0.318493,0.223022,0.223022,0.290625,0.301948,0.290172,0.231056,0.231056,0.296178,0.296651,0.28972,0.287037,0.285714,0.287037,0.291994,0.29477,0.295238,0.290172,0.290172,0.295238,0.311037,0.283537,0.283537,0.283537,0.291994,0.282675,0.285714,0.282675,0.291994,0.291536,0.291536,0.292453,0.282675,0.282675,0.298555,0.284839,0.282675,0.282675,0.291536,0.283105,0.282675,0.286154,0.282675,0.282675,0.282675,0.291536,0.221165,0.252374,0.256552,0.256552,0.256552,0.256552,0.256552,0.226553,0.227106,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(confianca_df_closseness, by=list(confianca_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(confianca_df_closseness, by=list(confianca_df_closseness$Type), FUN=sd, na.rm=TRUE) 

names(aggdata_sd) <- c("Group","Type","In Closeness(SD)", "Out Closeness(SD)", "Total Closeness(SD)","In Closeness Normalized(SD)", "Out Closeness Normalized(SD)", "Total Closeness Normalized(SD)", "Centralization Closeness(SD)")

#Removing Type variable
aggdata_sd<-aggdata_sd[,-c(2)]

#Merging mean and standart deviation
total_table <- merge(aggdata_mean,aggdata_sd,by="Group")

#Rounding
Group<-total_table[,c(1)] #Keeping group
total_table<-total_table[,-c(1)] %>% round(6) #Rouding
total_table<-cbind(Group,total_table) #Binding toghter

#Organizing Variabels
total_table<-total_table[c("Group","In Closeness(M)", "In Closeness(SD)", "Out Closeness(M)", "Out Closeness(SD)", "Total Closeness(M)","Total Closeness(SD)","In Closeness Normalized(M)", "In Closeness Normalized(SD)", "Out Closeness Normalized(M)", "Out Closeness Normalized(SD)", "Total Closeness Normalized(M)","Total Closeness Normalized(SD)", "Centralization Closeness(M)","Centralization Closeness(SD)")]
```

##Plotting final table with round for Closseness

```r
datatable(total_table, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-9604c987410552b74b80" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-9604c987410552b74b80">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6.5e-05\" data-max=\"7.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4e-06\" data-max=\"1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7.4e-05\" data-max=\"0.000105\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2e-05\" data-max=\"3.8e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000115\" data-max=\"0.000139\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.5e-05\" data-max=\"2.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.012084\" data-max=\"0.014231\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000718\" data-max=\"0.001825\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.01372\" data-max=\"0.019502\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003789\" data-max=\"0.007137\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.021473\" data-max=\"0.025769\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.002793\" data-max=\"0.004411\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.174226\" data-max=\"0.281591\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.060164\" data-max=\"0.135826\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[7.7e-05,6.5e-05],[4e-06,1e-05],[0.000105,7.4e-05],[2e-05,3.8e-05],[0.000139,0.000115],[1.5e-05,2.4e-05],[0.014231,0.012084],[0.000718,0.001825],[0.019502,0.01372],[0.003789,0.007137],[0.025769,0.021473],[0.002793,0.004411],[0.281591,0.174226],[0.060164,0.135826]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
confianca_df_closseness <- data.frame(
confianca_incloseness,
confianca_outcloseness,
confianca_totalcloseness,
confianca_incloseness_n,
confianca_outcloseness_n,
confianca_totalcloseness_n,
confianca_centr_closeness) %>% round(6)

#Adding type
confianca_df_closseness <-cbind(confianca_df_closseness, V(confianca)$TIPO2)

#Adding names
names(confianca_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
confianca_df_closseness<-confianca_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(confianca_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-e266f377db574c856351" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-e266f377db574c856351">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"9.5e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000166\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7.7e-05\" data-max=\"0.000203\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.017592\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.030861\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.014264\" data-max=\"0.037728\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[8.8e-05,8.1e-05,9.5e-05,7.9e-05,8e-05,7.9e-05,7.3e-05,8.4e-05,6.9e-05,8.4e-05,8e-05,8.1e-05,6.4e-05,6.8e-05,7.6e-05,7.7e-05,8.1e-05,8.3e-05,6.5e-05,7.4e-05,7.8e-05,7.1e-05,7.9e-05,5.5e-05,8.1e-05,7.6e-05,6.1e-05,7.9e-05,7.9e-05,7.9e-05,7.9e-05,7.6e-05,8e-05,6.1e-05,7.9e-05,7e-05,8.2e-05,7.8e-05,6.7e-05,7.7e-05,7.8e-05,7.5e-05,7.4e-05,8.3e-05,7.4e-05,8e-05,5.3e-05,6.9e-05,6.9e-05,6.9e-05,6.9e-05,6.9e-05,6.3e-05,7.3e-05,6.2e-05,7.9e-05,7.7e-05,6.7e-05,6.3e-05,8.1e-05,8e-05,5.8e-05,7.7e-05,7.6e-05,7.1e-05,2.9e-05,5.1e-05,7.6e-05,8.5e-05,7.6e-05,7.7e-05,7.7e-05,7.7e-05,7.8e-05,7.8e-05,7.6e-05,7.7e-05,8.1e-05,5.9e-05,5.9e-05,5.9e-05,5.9e-05,7.7e-05,6.9e-05,7.3e-05,7.7e-05,7.4e-05,5.3e-05,7.7e-05,7.6e-05,7.6e-05,5.5e-05,5.9e-05,7.3e-05,7.7e-05,7e-05,5.3e-05,6.8e-05,7.8e-05,7.8e-05,5.9e-05,6.9e-05,6.7e-05,6.7e-05,7.6e-05,7.7e-05,7.6e-05,6.7e-05,6.8e-05,7.7e-05,7.6e-05,7.8e-05,7.7e-05,7.7e-05,7.7e-05,7.6e-05,7.6e-05,7.6e-05,7.7e-05,7.8e-05,7.6e-05,7.6e-05,7.7e-05,7.7e-05,7.7e-05,7.7e-05,7.7e-05,7.7e-05,7.7e-05,7.3e-05,7.7e-05,7.7e-05,7.7e-05,7.7e-05,7.7e-05,7.9e-05,7.7e-05,7.6e-05,7.7e-05,7.6e-05,7.7e-05,7.6e-05,7.6e-05,7.6e-05,7.1e-05,8.1e-05,7.7e-05,7.7e-05,6.9e-05,7.3e-05,7.7e-05,7.3e-05,7.7e-05,7.3e-05,7.8e-05,5.7e-05,7.4e-05,7.8e-05,7.7e-05,5.3e-05,5.6e-05,6.4e-05,6.6e-05,6.5e-05,5.3e-05,5.1e-05,6.3e-05,5.3e-05,5.3e-05,6.3e-05,6.2e-05,5.3e-05,6.4e-05,6.3e-05,6.6e-05,6.6e-05,6.4e-05,5.3e-05,6.4e-05,6.1e-05,6.3e-05,6.3e-05,6.6e-05,6.5e-05,6.1e-05,5.8e-05,7.1e-05],[0.000166,0.000135,0.000156,0.000127,0.000126,0.000108,0.000106,0.00013,0.000129,0.000105,0.000111,9.7e-05,0.000125,0.000118,0.000121,0.000105,0.000137,0.000124,0.000104,0.000114,0.000116,0.000114,0.000116,0.000121,0.000104,7.8e-05,0.000109,0.000113,0.000123,0.000122,0.00012,0.000108,0.000136,7.7e-05,0.0001,0.000118,0.000118,9.4e-05,0.000103,0.000136,0.000126,0.000127,0.000127,0.000111,0.000119,0.000119,8.9e-05,0.000105,0.000105,0.000105,0.000105,0.000105,0.0001,0.000101,0.000123,0.000118,0.000112,9.8e-05,8.8e-05,0.000119,2.9e-05,7.7e-05,0.000117,0.000108,0.000116,2.9e-05,2.9e-05,0.000115,0.000101,0.000101,9.8e-05,0.0001,0.000116,0.000106,0.000101,0.000103,0.000111,0.00012,7.5e-05,7.5e-05,7.5e-05,7.5e-05,0.000105,7.9e-05,0.000111,0.000117,0.000105,0.0001,0.00011,9.6e-05,0.000112,0.000109,0.000108,8.7e-05,0.000114,0.000106,7e-05,7.8e-05,9.8e-05,0.00013,7.6e-05,0.000117,9.9e-05,9.9e-05,0.000111,0.000109,0.000102,9.7e-05,9.7e-05,0.000109,0.00011,0.000109,0.00011,0.000114,0.000105,0.000131,0.000112,0.000109,0.000111,0.000117,0.00011,0.000125,9.6e-05,9.6e-05,9.6e-05,0.0001,9.6e-05,9.7e-05,9.6e-05,9.8e-05,0.000109,0.000105,0.0001,9.7e-05,9.9e-05,0.000105,9.3e-05,9.2e-05,9.4e-05,0.000103,9.5e-05,9.2e-05,9.4e-05,9.6e-05,9.4e-05,9.2e-05,9.9e-05,6.8e-05,8.4e-05,8.9e-05,8.9e-05,8.9e-05,8.9e-05,8.9e-05,7.7e-05,8.7e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000203,0.000161,0.000194,0.000155,0.000173,0.000172,0.000123,0.00016,0.00014,0.000148,0.000149,0.000146,0.000138,0.000126,0.000148,0.000127,0.000175,0.000155,0.000123,0.000137,0.000139,0.00013,0.000141,0.000132,0.000146,0.000126,0.000131,0.000142,0.000146,0.000151,0.000145,0.000133,0.000161,0.000108,0.000149,0.000136,0.000151,0.000148,0.000119,0.000174,0.000141,0.000141,0.000143,0.000145,0.000164,0.000155,0.00012,0.000122,0.000122,0.000122,0.000122,0.000122,0.000115,0.000134,0.000138,0.000152,0.000134,0.000123,0.000111,0.000152,0.000133,9.1e-05,0.000136,0.000145,0.00013,7.7e-05,8.6e-05,0.00013,0.000177,0.00013,0.000132,0.00013,0.000139,0.000137,0.000134,0.000134,0.000139,0.000147,8.8e-05,8.8e-05,8.8e-05,8.8e-05,0.000144,0.000121,0.000137,0.000131,0.00013,0.000113,0.000135,0.00013,0.000133,0.000119,0.000125,0.00012,0.000166,0.00013,8.7e-05,0.000105,0.000132,0.000152,9.6e-05,0.000164,0.000135,0.000135,0.000133,0.000165,0.000164,0.000162,0.000162,0.000139,0.000132,0.000131,0.000136,0.000133,0.000131,0.000152,0.000133,0.000132,0.000133,0.000138,0.000132,0.000144,0.000131,0.000128,0.000131,0.000131,0.00013,0.00013,0.000131,0.000131,0.000131,0.000131,0.000129,0.000131,0.000131,0.000134,0.00013,0.00013,0.000135,0.000132,0.000132,0.000131,0.00013,0.000131,0.000107,0.000137,0.00013,0.000133,0.000111,0.000122,0.000127,0.000122,0.000127,0.000122,0.000137,0.000116,0.000125,0.000128,0.000127,9.5e-05,8.5e-05,9.8e-05,0.000107,0.000104,8.8e-05,8.6e-05,9.9e-05,8.8e-05,8.8e-05,9.9e-05,9.5e-05,8.8e-05,0.000101,9.9e-05,0.000105,0.000107,0.000102,8.8e-05,0.000102,9.6e-05,0.000102,0.000102,0.000105,0.000106,9.7e-05,8.5e-05,0.000104],[0.016333,0.015017,0.017592,0.014679,0.014811,0.014712,0.01364,0.015544,0.012768,0.015691,0.014815,0.015118,0.011958,0.012708,0.014157,0.014323,0.015121,0.01536,0.012072,0.013785,0.014526,0.013135,0.014602,0.010212,0.015025,0.014214,0.011435,0.014612,0.014639,0.014628,0.014763,0.014201,0.014915,0.011332,0.014624,0.013064,0.015302,0.014544,0.012451,0.014352,0.014469,0.013889,0.013779,0.015465,0.013729,0.014842,0.009867,0.0128,0.0128,0.0128,0.0128,0.0128,0.011664,0.013563,0.011529,0.014647,0.014342,0.012431,0.011711,0.015051,0.014872,0.010768,0.014254,0.014211,0.013149,0.005348,0.009481,0.014221,0.015873,0.014095,0.01431,0.014371,0.014335,0.014419,0.014459,0.014209,0.014284,0.015042,0.010931,0.010931,0.010931,0.010931,0.014286,0.012749,0.01351,0.014382,0.01379,0.00985,0.014358,0.014123,0.014194,0.010199,0.011029,0.013573,0.01437,0.013089,0.00985,0.012668,0.014429,0.014479,0.01094,0.012891,0.012453,0.012453,0.014206,0.014244,0.014226,0.012472,0.012643,0.014286,0.014229,0.014535,0.014304,0.014336,0.014386,0.014217,0.014204,0.014205,0.014308,0.014458,0.014218,0.014189,0.014341,0.014271,0.014341,0.014304,0.014271,0.0143,0.014314,0.013616,0.014238,0.014292,0.014315,0.014293,0.014342,0.014602,0.014247,0.01422,0.014364,0.014185,0.014289,0.014192,0.014166,0.014182,0.013137,0.015073,0.014284,0.014259,0.012741,0.013545,0.014241,0.013545,0.014241,0.013545,0.014546,0.010563,0.013743,0.014428,0.014401,0.009947,0.010363,0.011815,0.012212,0.012069,0.009913,0.009561,0.011746,0.009913,0.009913,0.011775,0.011511,0.009913,0.011876,0.011787,0.012211,0.012267,0.011936,0.009913,0.011936,0.011421,0.011757,0.011757,0.012229,0.012045,0.011401,0.010736,0.013231],[0.030861,0.025176,0.029004,0.02355,0.023402,0.020128,0.01967,0.024137,0.023954,0.019618,0.020669,0.018053,0.023294,0.021895,0.022567,0.019474,0.025567,0.023043,0.019281,0.021153,0.021598,0.021218,0.021633,0.02245,0.019391,0.014527,0.020195,0.021048,0.022853,0.022697,0.022275,0.020015,0.025255,0.014339,0.018567,0.021975,0.021898,0.017541,0.019181,0.025227,0.023376,0.023658,0.023649,0.020713,0.022067,0.02214,0.016535,0.019562,0.019562,0.019562,0.019562,0.019562,0.018686,0.018811,0.022842,0.022035,0.020743,0.018293,0.016326,0.022169,0.005348,0.014289,0.021808,0.02006,0.021656,0.005373,0.005399,0.021365,0.018872,0.018862,0.018184,0.01855,0.02149,0.019745,0.018807,0.019235,0.020598,0.022393,0.013887,0.013887,0.013887,0.013887,0.019618,0.014645,0.020557,0.021854,0.019614,0.018688,0.02048,0.017905,0.020773,0.020284,0.020132,0.016196,0.021274,0.019708,0.012945,0.014512,0.018225,0.024263,0.014177,0.021759,0.01841,0.01841,0.020651,0.020231,0.019036,0.01798,0.01798,0.020281,0.020381,0.020184,0.020462,0.021284,0.019604,0.02439,0.020815,0.020255,0.020591,0.021744,0.020503,0.023288,0.017849,0.017849,0.017849,0.01857,0.017784,0.018111,0.017784,0.018234,0.02029,0.019503,0.018628,0.018104,0.018436,0.019589,0.017325,0.017176,0.017475,0.019138,0.017672,0.017176,0.017547,0.017784,0.017475,0.017176,0.018432,0.012682,0.015584,0.01653,0.01653,0.01653,0.01653,0.01653,0.014296,0.016189,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.037728,0.029913,0.03606,0.028784,0.032124,0.031904,0.0228,0.029693,0.026109,0.027592,0.027633,0.027145,0.025581,0.023402,0.027482,0.023694,0.032466,0.028793,0.022926,0.025546,0.025869,0.024269,0.026168,0.024509,0.027086,0.023464,0.024276,0.026346,0.027126,0.028135,0.027043,0.024727,0.02999,0.020017,0.02765,0.025299,0.028084,0.027507,0.022214,0.032427,0.026142,0.02626,0.026522,0.026988,0.030577,0.028739,0.022257,0.022713,0.022713,0.022713,0.022713,0.022713,0.021316,0.024983,0.025701,0.028272,0.025013,0.022803,0.020619,0.028328,0.024803,0.016974,0.025344,0.026972,0.024168,0.014264,0.016052,0.024244,0.032973,0.024212,0.024522,0.024156,0.025837,0.025528,0.024886,0.02493,0.025844,0.027325,0.016431,0.016431,0.016431,0.016431,0.026751,0.022499,0.025424,0.024442,0.024115,0.021003,0.025027,0.02426,0.024787,0.022043,0.02318,0.022313,0.030795,0.024103,0.016168,0.019532,0.024503,0.028272,0.017904,0.030447,0.025196,0.025196,0.024727,0.030617,0.030552,0.030097,0.030117,0.025848,0.024548,0.024429,0.025368,0.02479,0.024377,0.028311,0.024731,0.024532,0.024817,0.025733,0.024509,0.026801,0.02439,0.023837,0.02439,0.024448,0.024087,0.02409,0.024371,0.024349,0.024298,0.02439,0.024071,0.024413,0.024425,0.024956,0.024146,0.024253,0.02502,0.024577,0.024554,0.02432,0.024225,0.024276,0.01987,0.025535,0.024121,0.024777,0.020619,0.02278,0.023571,0.02278,0.023571,0.02278,0.025486,0.021568,0.023204,0.023883,0.023571,0.017714,0.015721,0.018251,0.019914,0.019411,0.01635,0.015966,0.018423,0.01635,0.01635,0.018385,0.017753,0.01635,0.018739,0.018392,0.019606,0.019855,0.018976,0.01635,0.018976,0.017888,0.01892,0.01892,0.019604,0.019794,0.017968,0.015808,0.019379],[0.372,0.343808,0.392405,0.334532,0.30897,0.301948,0.304918,0.319039,0.311558,0.288372,0.294304,0.286154,0.333932,0.322917,0.287481,0.29108,0.352273,0.315254,0.292913,0.286154,0.303426,0.285714,0.296178,0.333333,0.285714,0.240621,0.277198,0.319039,0.333333,0.316327,0.310518,0.2976,0.315789,0.224096,0.230198,0.285714,0.307438,0.267626,0.298555,0.32069,0.334532,0.317406,0.317406,0.293839,0.299035,0.300485,0.23192,0.291994,0.291994,0.291994,0.291994,0.291994,0.282675,0.2976,0.331551,0.319039,0.304419,0.288372,0.261972,0.324607,0.005348,0.228221,0.297125,0.291536,0.299517,0.005376,0.005405,0.307438,0.298555,0.283537,0.283537,0.294304,0.296651,0.296178,0.289269,0.290172,0.288372,0.314721,0.229346,0.229346,0.229346,0.229346,0.287926,0.250674,0.295238,0.299035,0.292453,0.274742,0.302932,0.286154,0.291536,0.302439,0.289269,0.24933,0.2976,0.288372,0.216028,0.223289,0.285714,0.307438,0.234552,0.318493,0.223022,0.223022,0.290625,0.301948,0.290172,0.231056,0.231056,0.296178,0.296651,0.28972,0.287037,0.285714,0.287037,0.291994,0.29477,0.295238,0.290172,0.290172,0.295238,0.311037,0.283537,0.283537,0.283537,0.291994,0.282675,0.285714,0.282675,0.291994,0.291536,0.291536,0.292453,0.282675,0.282675,0.298555,0.284839,0.282675,0.282675,0.291536,0.283105,0.282675,0.286154,0.282675,0.282675,0.282675,0.291536,0.221165,0.252374,0.256552,0.256552,0.256552,0.256552,0.256552,0.226553,0.227106,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(confianca_df_closseness, by=list(confianca_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(confianca_df_closseness, by=list(confianca_df_closseness$Type), FUN=sd, na.rm=TRUE) 

names(aggdata_sd) <- c("Group","Type","In Closeness(SD)", "Out Closeness(SD)", "Total Closeness(SD)","In Closeness Normalized(SD)", "Out Closeness Normalized(SD)", "Total Closeness Normalized(SD)", "Centralization Closeness(SD)")

#Removing Type variable
aggdata_sd<-aggdata_sd[,-c(2)]

#Merging mean and standart deviation
total_table <- merge(aggdata_mean,aggdata_sd,by="Group")

#Rounding
Group<-total_table[,c(1)] #Keeping group
total_table<-total_table[,-c(1)] %>% round(6) #Rouding
total_table<-cbind(Group,total_table) #Binding toghter

#Organizing Variabels
total_table<-total_table[c("Group","In Closeness(M)", "In Closeness(SD)", "Out Closeness(M)", "Out Closeness(SD)", "Total Closeness(M)","Total Closeness(SD)","In Closeness Normalized(M)", "In Closeness Normalized(SD)", "Out Closeness Normalized(M)", "Out Closeness Normalized(SD)", "Total Closeness Normalized(M)","Total Closeness Normalized(SD)", "Centralization Closeness(M)","Centralization Closeness(SD)")]
```

##Plotting final table with round for Closseness

```r
datatable(total_table, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-f30e4b3f0620008ed553" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-f30e4b3f0620008ed553">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6.5e-05\" data-max=\"8e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7.4e-05\" data-max=\"0.000119\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.1e-05\" data-max=\"3.8e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000115\" data-max=\"0.000154\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.1e-05\" data-max=\"2.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.012047\" data-max=\"0.014788\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000193\" data-max=\"0.001809\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.013651\" data-max=\"0.022197\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.002123\" data-max=\"0.007151\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.021417\" data-max=\"0.028699\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.002012\" data-max=\"0.004398\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.17285\" data-max=\"0.310208\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.028097\" data-max=\"0.136174\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[8e-05,7.6e-05,6.5e-05],[1e-06,4e-06,1e-05],[0.000119,0.000102,7.4e-05],[1.1e-05,2e-05,3.8e-05],[0.000154,0.000136,0.000115],[1.1e-05,1.4e-05,2.4e-05],[0.014788,0.014151,0.012047],[0.000193,0.000727,0.001809],[0.022197,0.019067,0.013651],[0.002123,0.003814,0.007151],[0.028699,0.025302,0.021417],[0.002012,0.002617,0.004398],[0.310208,0.277025,0.17285],[0.028097,0.062346,0.136174]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/confianca_data.RData")
```

