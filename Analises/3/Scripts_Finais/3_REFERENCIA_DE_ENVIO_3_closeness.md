# SNA Closeness 3_REFERENCIA DE ENVIO (var1)
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
load("~/SNArRDJF/Robject/var1_data.RData")
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
#var1<-simplify(var1) #Simplify
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
V(var1)$incloseness <- closeness(var1, mode = "in", weights = E(var1)$var1) %>% round(6)
V(var1)$outcloseness <- closeness(var1, mode = "out", weights = E(var1)$var1) %>% round(6)
V(var1)$totalcloseness <- closeness(var1, mode = "total", weights = E(var1)$var1) %>% round(4)
```

###Saving to Environment

```r
var1_incloseness<- closeness(var1, mode = "in", weights = E(var1)$var1) %>% round(6)
var1_outcloseness<- closeness(var1, mode = "out", weights = E(var1)$var1) %>% round(6)
var1_totalcloseness<- closeness(var1, mode = "total", weights = E(var1)$var1) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var1_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 2.900e-05 9.700e-05 9.800e-05 9.332e-05 9.800e-05 1.080e-04
```

```r
sd(var1_incloseness)
```

```
## [1] 1.76904e-05
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var1)$incloseness<-closeness(var1, weights = E(var1)$var1, mode="in")

#Get Variable
V(var1)$var1_color_degree<-round(V(var1)$incloseness,6)

#Creating brewer pallette
vertex_var1_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var1)$var1_color_degree)), "RdBu"))(
            length(unique(V(var1)$var1_color_degree)))

#Saving as Vertex properties 
V(var1)$vertex_var1_color_degree<-
  vertex_var1_color_degree[as.numeric(
  cut(V(var1)$var1_color_degree,
      breaks=length(unique(V(var1)$var1_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var1, es=E(var1), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var1))
maxC <- rep(Inf, vcount(var1))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var1, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var1)$weight)


#PLotting
plot(var1, 
     layout=co,
     edge.color=V(var1)$vertex_var1_color_degree[edge.start],
     edge.arrow.size=closeness(var1, weights = E(var1)$var1, mode="in"),
     edge.width=E(var1)$weight/mean(E(var1)$weight),
     edge.curved = TRUE,
     vertex.color=V(var1)$vertex_var1_color_degree,
     vertex.size=closeness(var1, weights = E(var1)$var1, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var1,"LABEL_COR"),
     vertex.label.cex=(closeness(var1, weights = E(var1)$var1, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var1)$var1_color_degree
b<-V(var1)$vertex_var1_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 3_REFERENCIA DE ENVIO (var1)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var1, mode="in", weights = E(var1)$var1)), 
             sd(closeness(var1, mode="in", weights = E(var1)$var1))
             )
       )
```

![](3_REFERENCIA_DE_ENVIO_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var1_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0000290 0.0003460 0.0002587 0.0003490 0.0007550
```

```r
sd(var1_outcloseness)
```

```
## [1] 0.0001480164
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var1)$outcloseness<-closeness(var1, weights = E(var1)$var1, mode="out")

#Get Variable
V(var1)$var1_color_degree<-round(V(var1)$outcloseness,6)

#Creating brewer pallette
vertex_var1_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var1)$var1_color_degree)), "RdBu"))(
            length(unique(V(var1)$var1_color_degree)))

#Saving as Vertex properties 
V(var1)$vertex_var1_color_degree<-
  vertex_var1_color_degree[as.numeric(
  cut(V(var1)$var1_color_degree,
      breaks=length(unique(V(var1)$var1_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var1, es=E(var1), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var1))
maxC <- rep(Inf, vcount(var1))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var1, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var1)$weight)


#PLotting
plot(var1, 
     layout=co,
     edge.color=V(var1)$vertex_var1_color_degree[edge.start],
     edge.arrow.size=closeness(var1, weights = E(var1)$var1, mode="out"),
     edge.width=E(var1)$weight/2*mean(E(var1)$weight),
     edge.curved = TRUE,
     vertex.color=V(var1)$vertex_var1_color_degree,
     vertex.size=closeness(var1, weights = E(var1)$var1, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var1,"LABEL_COR"),
     vertex.label.cex=closeness(var1, weights = E(var1)$var1, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var1)$var1_color_degree
b<-V(var1)$vertex_var1_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 3_REFERENCIA DE ENVIO (var1)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var1, mode="out", weights = E(var1)$var1)), 
             sd(closeness(var1, mode="out", weights = E(var1)$var1))
             )
       )
```

![](3_REFERENCIA_DE_ENVIO_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var1_totalcloseness)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000029 0.001443 0.001637 0.001569 0.001665 0.002198
```

```r
sd(var1_totalcloseness)
```

```
## [1] 0.0002095795
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var1)$allcloseness<-closeness(var1, weights = E(var1)$var1, mode="all")

#Get Variable
V(var1)$var1_color_degree<-round(V(var1)$allcloseness,6)

#Creating brewer pallette
vertex_var1_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var1)$var1_color_degree)), "RdBu"))(
            length(unique(V(var1)$var1_color_degree)))

#Saving as Vertex properties 
V(var1)$vertex_var1_color_degree<-
  vertex_var1_color_degree[as.numeric(
  cut(V(var1)$var1_color_degree,
      breaks=length(unique(V(var1)$var1_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var1, es=E(var1), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var1))
maxC <- rep(Inf, vcount(var1))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var1, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var1)$weight)


#PLotting
plot(var1, 
     layout=co,
     edge.color=V(var1)$vertex_var1_color_degree[edge.start],
     edge.arrow.size=closeness(var1, weights = E(var1)$var1, mode="all"),
     edge.width=E(var1)$weight/2*mean(E(var1)$weight),
     edge.curved = TRUE,
     vertex.color=V(var1)$vertex_var1_color_degree,
     vertex.size=closeness(var1, weights = E(var1)$var1, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var1,"LABEL_COR"),
     vertex.label.cex=(closeness(var1, weights = E(var1)$var1, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var1)$var1_color_degree
b<-V(var1)$vertex_var1_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 3_REFERENCIA DE ENVIO (var1)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var1, mode="all", weights = E(var1)$var1)), 
             sd(closeness(var1, mode="all", weights = E(var1)$var1))
             )
       )
```

![](3_REFERENCIA_DE_ENVIO_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var1)$incloseness_n <- closeness(var1, mode = "in",, weights = E(var1)$var1, normalized = T) %>% round(10)
V(var1)$outcloseness_n <- closeness(var1, mode = "out", normalized = T, weights = E(var1)$var1) %>% round(6)
V(var1)$totalcloseness_n <- closeness(var1, mode = "total", normalized = T, weights = E(var1)$var1) %>% round(6)
```

###Saving to Environment

```r
var1_incloseness_n<- closeness(var1, mode = "in", normalized = T, weights = E(var1)$var1) %>% round(6)
var1_outcloseness_n<- closeness(var1, mode = "out", normalized = T, weights = E(var1)$var1) %>% round(6)
var1_totalcloseness_n<- closeness(var1, mode = "total", normalized = T, weights = E(var1)$var1) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var1_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.018040 0.018250 0.017370 0.018270 0.020060
```

```r
sd(var1_incloseness_n)
```

```
## [1] 0.00329762
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var1)$incloseness_n<-closeness(var1, weights = E(var1)$var1, mode="in", normalized = T)

#Get Variable
V(var1)$var1_color_degree<-round(V(var1)$incloseness_n,6)

#Creating brewer pallette
vertex_var1_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var1)$var1_color_degree)), "RdBu"))(
            length(unique(V(var1)$var1_color_degree)))

#Saving as Vertex properties 
V(var1)$vertex_var1_color_degree<-
  vertex_var1_color_degree[as.numeric(
  cut(V(var1)$var1_color_degree,
      breaks=length(unique(V(var1)$var1_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var1, es=E(var1), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var1))
maxC <- rep(Inf, vcount(var1))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var1, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var1)$weight)


#PLotting
plot(var1, 
     layout=co,
     edge.color=V(var1)$vertex_var1_color_degree[edge.start],
     edge.arrow.size=closeness(var1, weights = E(var1)$var1, mode="in",normalized = T),
     edge.width=E(var1)$weight/10*mean(E(var1)$weight),
     edge.curved = TRUE,
     vertex.color=V(var1)$vertex_var1_color_degree,
     vertex.size=(closeness(var1, weights = E(var1)$var1, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var1,"LABEL_COR"),
     vertex.label.cex=closeness(var1, weights = E(var1)$var1, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var1)$var1_color_degree
b<-V(var1)$vertex_var1_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 3_REFERENCIA DE ENVIO (var1)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var1, mode="in", weights = E(var1)$var1, normalized = T)), 
             sd(closeness(var1, mode="in", weights = E(var1)$var1, normalized = T))
             )
       )
```

![](3_REFERENCIA_DE_ENVIO_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var1_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.005362 0.064400 0.048100 0.064920 0.140400
```

```r
sd(var1_outcloseness_n)
```

```
## [1] 0.02754878
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var1)$outcloseness_n<-closeness(var1, weights = E(var1)$var1, mode="out", normalized = T)

#Get Variable
V(var1)$var1_color_degree<-round(V(var1)$outcloseness_n,6)

#Creating brewer pallette
vertex_var1_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var1)$var1_color_degree)), "RdBu"))(
            length(unique(V(var1)$var1_color_degree)))

#Saving as Vertex properties 
V(var1)$vertex_var1_color_degree<-
  vertex_var1_color_degree[as.numeric(
  cut(V(var1)$var1_color_degree,
      breaks=length(unique(V(var1)$var1_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var1, es=E(var1), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var1))
maxC <- rep(Inf, vcount(var1))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var1, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var1)$weight)


#PLotting
plot(var1, 
     layout=co,
     edge.color=V(var1)$vertex_var1_color_degree[edge.start],
     edge.arrow.size=closeness(var1, weights = E(var1)$var1, mode="out",normalized = T),
     edge.width=E(var1)$weight/10*mean(E(var1)$weight),
     edge.curved = TRUE,
     vertex.color=V(var1)$vertex_var1_color_degree,
     vertex.size=(closeness(var1, weights = E(var1)$var1, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var1,"LABEL_COR"),
     vertex.label.cex=closeness(var1, weights = E(var1)$var1, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var1)$var1_color_degree
b<-V(var1)$vertex_var1_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 3_REFERENCIA DE ENVIO (var1)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var1, mode="out", weights = E(var1)$var1, normalized = T)), 
             sd(closeness(var1, mode="out", weights = E(var1)$var1, normalized = T))
             )
       )
```

![](3_REFERENCIA_DE_ENVIO_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var1_totalcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.268400 0.304400 0.291900 0.309700 0.408800
```

```r
sd(var1_totalcloseness_n)
```

```
## [1] 0.0389912
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var1)$allcloseness_n<-closeness(var1, weights = E(var1)$var1, mode="all", normalized = T)

#Get Variable
V(var1)$var1_color_degree<-round(V(var1)$allcloseness_n,6)

#Creating brewer pallette
vertex_var1_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var1)$var1_color_degree)), "RdBu"))(
            length(unique(V(var1)$var1_color_degree)))

#Saving as Vertex properties 
V(var1)$vertex_var1_color_degree<-
  vertex_var1_color_degree[as.numeric(
  cut(V(var1)$var1_color_degree,
      breaks=length(unique(V(var1)$var1_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var1, es=E(var1), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var1))
maxC <- rep(Inf, vcount(var1))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var1, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var1)$weight)


#PLotting
plot(var1, 
     layout=co,
     edge.color=V(var1)$vertex_var1_color_degree[edge.start],
     edge.arrow.size=closeness(var1, weights = E(var1)$var1, mode="all",normalized = T),
     edge.width=E(var1)$weight/10*mean(E(var1)$weight),
     edge.curved = TRUE,
     vertex.color=V(var1)$vertex_var1_color_degree,
     vertex.size=(closeness(var1, weights = E(var1)$var1, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var1,"LABEL_COR"),
     vertex.label.cex=closeness(var1, weights = E(var1)$var1, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var1)$var1_color_degree
b<-V(var1)$vertex_var1_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 3_REFERENCIA DE ENVIO (var1)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var1, mode="all", weights = E(var1)$var1, normalized = T)), 
             sd(closeness(var1, mode="all", weights = E(var1)$var1, normalized = T))
             )
       )
```

![](3_REFERENCIA_DE_ENVIO_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var1)$incloseness_n <- closeness(var1, weights = E(var1)$var1, mode = "in", normalized = T) %>% round(6)
V(var1)$outcloseness_n <- closeness(var1, weights = E(var1)$var1, mode = "out", normalized = T) %>% round(6)
V(var1)$totalcloseness_n <- closeness(var1, weights = E(var1)$var1, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var1)$var1_centr_closeness<- centralization.closeness(var1)$res
var1_centr_closeness<- centralization.closeness(var1)$res
var1_centr_closeness_all<- centralization.closeness(var1)
```

###Centralization

```r
var1_centr_closeness_all$centralization
```

```
## [1] 0.09327074
```

###Theoretical Max

```r
var1_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var1)$var1_centr_closeness<- centralization.closeness(var1)$res

#Get Variable
V(var1)$var1_color_degree<-round(V(var1)$var1_centr_closeness,6)

#Creating brewer pallette
vertex_var1_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var1)$var1_color_degree)), "Spectral"))(
            length(unique(V(var1)$var1_color_degree)))

#Saving as Vertex properties 
V(var1)$vertex_var1_color_degree<-
  vertex_var1_color_degree[as.numeric(
  cut(V(var1)$var1_color_degree,
      breaks=length(unique(V(var1)$var1_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var1, es=E(var1), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var1))
maxC <- rep(Inf, vcount(var1))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var1, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var1)$weight)


#PLotting
plot(var1, 
     layout=co,
     edge.color=V(var1)$vertex_var1_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var1)$res,
     edge.width=E(var1)$weight/10*mean(E(var1)$weight),
     edge.curved = TRUE,
     vertex.color=V(var1)$vertex_var1_color_degree,
     vertex.size=centralization.closeness(var1)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var1,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var1)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var1)$var1_color_degree
b<-V(var1)$vertex_var1_color_degree
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
  title("Network Centralization Closeness - 3_REFERENCIA DE ENVIO (var1)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var1)$res), 
             sd(centralization.closeness(var1)$res)
             )
       )
```

![](3_REFERENCIA_DE_ENVIO_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var1_incloseness<- closeness(var1, weights = E(var1)$var1, mode = "in") %>% round(6)
var1_outcloseness<- closeness(var1, weights = E(var1)$var1, mode = "out") %>% round(6)
var1_totalcloseness<- closeness(var1, weights = E(var1)$var1, mode = "total") %>% round(6)
var1_incloseness_n<- closeness(var1,weights = E(var1)$var1, mode = "in", normalized = T) %>% round(6)
var1_outcloseness_n<- closeness(var1,weights = E(var1)$var1, mode = "out", normalized = T) %>% round(6)
var1_totalcloseness_n<- closeness(var1,weights = E(var1)$var1, mode = "total", normalized = T) %>% round(6)
var1_centr_closeness <- centralization.closeness(var1)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var1_df_closseness <- data.frame(
var1_incloseness,
var1_outcloseness,
var1_totalcloseness,
var1_incloseness_n,
var1_outcloseness_n,
var1_totalcloseness_n,
var1_centr_closeness) %>% round(6)

#Adding type
var1_df_closseness <-cbind(var1_df_closseness, V(var1)$LABEL_COR)

#Adding names
names(var1_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var1_df_closseness<-var1_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var1_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-d4868f3c0b73d70424d0" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-d4868f3c0b73d70424d0">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000108\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000755\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.002198\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.020063\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.140377\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.408791\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.140377\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[9.9e-05,9.8e-05,9.9e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.7e-05,9.6e-05,9.7e-05,9.8e-05,9.8e-05,9.6e-05,9.7e-05,9.8e-05,9.7e-05,9.8e-05,9.8e-05,9.7e-05,9.7e-05,9.8e-05,9.6e-05,9.8e-05,2.9e-05,9.6e-05,0.0001,9.9e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,0.0001,9.8e-05,9.6e-05,9.7e-05,9.7e-05,9.7e-05,9.7e-05,9.8e-05,9.8e-05,9.8e-05,9.9e-05,9.7e-05,9.8e-05,2.9e-05,9.7e-05,9.7e-05,9.7e-05,9.7e-05,9.7e-05,2.9e-05,9.8e-05,0.0001,9.8e-05,9.8e-05,9.6e-05,2.9e-05,9.8e-05,0.000102,9.5e-05,9.8e-05,9.8e-05,9.9e-05,2.9e-05,2.9e-05,9.7e-05,9.7e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.6e-05,9.6e-05,9.6e-05,9.6e-05,9.8e-05,0.000101,9.7e-05,9.8e-05,9.7e-05,9.5e-05,9.8e-05,9.8e-05,9.8e-05,9.6e-05,9.5e-05,9.6e-05,9.8e-05,9.7e-05,9.5e-05,9.6e-05,9.8e-05,9.8e-05,9.6e-05,9.7e-05,9.9e-05,9.9e-05,9.8e-05,9.8e-05,9.8e-05,9.9e-05,9.9e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.9e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,0.0001,9.8e-05,0.000108,9.8e-05,9.9e-05,9.7e-05,9.8e-05,9.7e-05,9.8e-05,9.7e-05,0.000102,9.9e-05,9.9e-05,0.0001,0.0001,9.6e-05,9.7e-05,0.0001,0.000102,0.0001,2.9e-05,2.9e-05,0.0001,2.9e-05,2.9e-05,0.0001,0.0001,2.9e-05,0.0001,0.000101,0.000101,0.000101,0.0001,2.9e-05,0.0001,0.0001,0.0001,0.0001,0.000101,0.000101,0.0001,9.9e-05,2.9e-05],[0.000363,0.000349,0.000368,0.000359,0.000352,0.000337,0.000353,0.000356,0.000348,0.000343,0.000342,0.000347,0.000356,0.000357,0.000346,0.000349,0.000364,0.000355,0.00035,0.000344,0.000351,0.000344,0.000347,0.000755,0.000343,2.9e-05,2.9e-05,0.000355,0.000359,0.000352,0.000351,0.000351,0.000351,2.9e-05,0.000329,0.000344,0.000352,0.000344,0.000349,0.000357,0.000356,0.000351,0.000351,2.9e-05,0.000346,0.000351,2.9e-05,0.000349,0.000349,0.000349,0.000349,0.000349,0.000365,0.000351,2.9e-05,0.000349,0.000354,0.000348,3.4e-05,0.000346,2.9e-05,0.000311,0.000351,0.000347,3e-05,2.9e-05,2.9e-05,0.000354,0.00035,0.000347,0.000347,0.000347,0.000349,0.000349,0.000348,0.00035,0.000349,0.000355,0.00033,0.00033,0.00033,0.00033,0.000348,2.9e-05,0.000349,0.000351,0.000349,0.000342,0.00035,0.000347,0.000349,0.000344,0.000311,0.000328,0.000349,0.000347,0.000324,0.000325,0.000346,0.000353,0.000324,0.000355,2.9e-05,2.9e-05,0.000349,0.00035,0.000347,2.9e-05,2.9e-05,0.000351,0.00035,0.000349,0.000348,0.000348,0.000349,0.000348,0.000352,0.000352,0.000349,0.000349,0.000348,2.9e-05,0.000346,0.000346,0.000346,0.000347,0.000346,0.000348,0.000346,0.000347,0.000347,0.000347,0.000348,0.000346,0.000346,0.000348,0.000347,0.000346,0.000346,0.000347,0.000346,0.000346,0.000347,0.000346,2.9e-05,0.000346,2.9e-05,0.000327,2.9e-05,0.000329,0.000329,0.000329,0.000329,0.000329,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.002141,0.001805,0.002198,0.001795,0.001779,0.001684,0.001698,0.001855,0.001639,0.001642,0.001701,0.001701,0.001678,0.001721,0.001634,0.001642,0.001905,0.001795,0.001629,0.001645,0.001724,0.001623,0.001724,0.001799,0.001587,0.001639,0.001401,0.00177,0.00177,0.00173,0.001733,0.001672,0.001709,0.001385,0.001637,0.001631,0.001718,0.001471,0.001629,0.001789,0.001779,0.001656,0.001661,0.001504,0.001567,0.001736,0.001353,0.001637,0.001637,0.001637,0.001637,0.001637,0.001538,0.001672,0.001397,0.001675,0.001689,0.0016,0.001471,0.001808,0.001689,0.001129,0.001675,0.001653,0.001458,0.001094,0.001088,0.001686,0.00161,0.001631,0.001645,0.001642,0.001721,0.001721,0.001667,0.001656,0.00173,0.001786,0.001276,0.001276,0.001276,0.001276,0.001642,0.001558,0.001658,0.001647,0.001684,0.001558,0.001669,0.001634,0.001637,0.001595,0.001129,0.001422,0.001645,0.001692,0.001284,0.001395,0.001645,0.001715,0.001314,0.001727,0.001304,0.001304,0.00165,0.001667,0.00165,0.001304,0.001304,0.00165,0.001647,0.00165,0.00165,0.001645,0.001645,0.001664,0.001656,0.001656,0.001639,0.001656,0.001645,0.00157,0.001631,0.001623,0.001631,0.001658,0.001623,0.001634,0.001631,0.001658,0.001626,0.001631,0.001637,0.001623,0.001629,0.00165,0.001642,0.001639,0.001661,0.001658,0.001647,0.001631,0.001631,0.001639,0.001575,0.001724,0.001656,0.001634,0.00137,0.00155,0.001626,0.00155,0.001626,0.00155,0.001681,0.001445,0.001543,0.001623,0.001621,0.00141,0.001267,0.001401,0.00137,0.001357,0.001353,0.000907,0.001377,0.001353,0.001353,0.001377,0.001383,0.001353,0.001383,0.001391,0.001443,0.001364,0.001389,0.001353,0.001389,0.001391,0.001443,0.001443,0.001433,0.001466,0.001401,0.001149,2.9e-05],[0.018369,0.018294,0.018438,0.01823,0.01825,0.018232,0.018226,0.018099,0.017878,0.018027,0.018251,0.018244,0.017804,0.01803,0.018246,0.018041,0.018232,0.018293,0.018001,0.018071,0.018268,0.01789,0.018266,0.005348,0.017814,0.018537,0.018352,0.018271,0.018226,0.018239,0.018246,0.01825,0.018237,0.018692,0.018225,0.017905,0.018088,0.018042,0.018016,0.018104,0.018244,0.018225,0.018223,0.018399,0.018023,0.018255,0.005405,0.018041,0.018041,0.018041,0.018041,0.018041,0.005348,0.018166,0.018671,0.01823,0.018253,0.017847,0.005376,0.018294,0.018926,0.017672,0.01825,0.01825,0.018381,0.005348,0.005348,0.018032,0.018027,0.018239,0.018244,0.018246,0.018246,0.01825,0.018253,0.018248,0.018251,0.018253,0.017797,0.017797,0.017797,0.017797,0.01825,0.018773,0.01802,0.01825,0.018062,0.017599,0.018248,0.018242,0.018244,0.017808,0.017674,0.017897,0.018251,0.018018,0.01759,0.017874,0.018253,0.018248,0.017878,0.018118,0.018343,0.018343,0.01825,0.01825,0.01825,0.018343,0.018343,0.01825,0.01825,0.018253,0.018251,0.01825,0.01825,0.018251,0.01825,0.01825,0.018248,0.01826,0.018248,0.018465,0.018246,0.018239,0.018246,0.018246,0.018241,0.018242,0.018244,0.018148,0.018237,0.018242,0.018244,0.018241,0.018246,0.018251,0.01825,0.018248,0.018251,0.018248,0.018253,0.018246,0.018246,0.018248,0.018537,0.018293,0.020063,0.018248,0.018372,0.018134,0.018239,0.018134,0.018239,0.018134,0.018914,0.018462,0.018474,0.018578,0.018574,0.017902,0.018123,0.018692,0.01904,0.018667,0.005405,0.005376,0.01868,0.005405,0.005405,0.018686,0.018688,0.005405,0.018686,0.018695,0.018714,0.018722,0.01869,0.005405,0.01869,0.018692,0.018667,0.018667,0.01871,0.01871,0.018692,0.018476,0.005348],[0.067489,0.064899,0.068483,0.066834,0.065447,0.062732,0.065585,0.066192,0.064741,0.06383,0.063546,0.064583,0.066216,0.066429,0.064427,0.064854,0.067612,0.066098,0.065058,0.063918,0.065195,0.063961,0.064606,0.140377,0.06383,0.005348,0.005348,0.066028,0.066762,0.06547,0.065355,0.065217,0.065195,0.005348,0.061124,0.063939,0.065562,0.063896,0.064921,0.066476,0.066192,0.065378,0.065378,0.005348,0.064382,0.065217,0.005348,0.064876,0.064876,0.064876,0.064876,0.064876,0.067858,0.065217,0.005348,0.064967,0.065911,0.064651,0.00625,0.064404,0.005348,0.0578,0.065195,0.064561,0.005494,0.005376,0.005405,0.065794,0.065126,0.064449,0.064449,0.064583,0.064921,0.064831,0.064673,0.065035,0.064921,0.066004,0.061325,0.061325,0.061325,0.061325,0.064741,0.005348,0.064944,0.06524,0.064831,0.063611,0.065149,0.064628,0.064831,0.064072,0.057782,0.061044,0.064899,0.064516,0.060175,0.060468,0.064404,0.065655,0.060331,0.066051,0.005348,0.005348,0.064921,0.065172,0.064539,0.005348,0.005348,0.065195,0.06508,0.06499,0.064741,0.064673,0.064831,0.064808,0.065493,0.065424,0.064899,0.064899,0.064673,0.005348,0.064404,0.064404,0.064404,0.064583,0.064404,0.064786,0.064404,0.064606,0.064449,0.064449,0.064718,0.064404,0.064404,0.064741,0.064516,0.064404,0.064404,0.064449,0.064404,0.064404,0.064606,0.064404,0.005348,0.064404,0.005348,0.060804,0.005348,0.061245,0.061245,0.061245,0.061245,0.061245,0.005376,0.005376,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.398287,0.33574,0.408791,0.333932,0.330961,0.313131,0.315789,0.345083,0.304918,0.305419,0.316327,0.316327,0.312081,0.320138,0.303922,0.305419,0.354286,0.333932,0.302932,0.305921,0.32069,0.301948,0.32069,0.334532,0.295238,0.304918,0.260504,0.329204,0.329204,0.321799,0.322357,0.311037,0.317949,0.257618,0.304419,0.303426,0.319588,0.273529,0.302932,0.332737,0.330961,0.307947,0.30897,0.279699,0.291536,0.322917,0.251691,0.304419,0.304419,0.304419,0.304419,0.304419,0.286154,0.311037,0.259777,0.311558,0.314189,0.2976,0.273529,0.336347,0.314189,0.209932,0.311558,0.307438,0.271137,0.203501,0.202394,0.313659,0.299517,0.303426,0.305921,0.305419,0.320138,0.320138,0.31,0.307947,0.321799,0.332143,0.237245,0.237245,0.237245,0.237245,0.305419,0.28972,0.308458,0.306425,0.313131,0.28972,0.310518,0.303922,0.304419,0.296651,0.209932,0.26458,0.305921,0.314721,0.238768,0.259414,0.305921,0.319039,0.244415,0.321244,0.242503,0.242503,0.306931,0.31,0.306931,0.242503,0.242503,0.306931,0.306425,0.306931,0.306931,0.305921,0.305921,0.309484,0.307947,0.307947,0.304918,0.307947,0.305921,0.291994,0.303426,0.301948,0.303426,0.308458,0.301948,0.303922,0.303426,0.308458,0.302439,0.303426,0.304419,0.301948,0.302932,0.306931,0.305419,0.304918,0.30897,0.308458,0.306425,0.303426,0.303426,0.304918,0.292913,0.32069,0.307947,0.303922,0.254795,0.288372,0.302439,0.288372,0.302439,0.288372,0.312605,0.268786,0.287037,0.301948,0.301459,0.262341,0.235741,0.260504,0.254795,0.252374,0.251691,0.168631,0.256198,0.251691,0.251691,0.256198,0.257261,0.251691,0.257261,0.258693,0.268398,0.253752,0.258333,0.251691,0.258333,0.258693,0.268398,0.268398,0.266476,0.272727,0.260504,0.213793,0.005348],[0.067489,0.064899,0.068483,0.066834,0.065447,0.062732,0.065585,0.066192,0.064741,0.06383,0.063546,0.064583,0.066216,0.066429,0.064427,0.064854,0.067612,0.066098,0.065058,0.063918,0.065195,0.063961,0.064606,0.140377,0.06383,0.005348,0.005348,0.066028,0.066762,0.06547,0.065355,0.065217,0.065195,0.005348,0.061124,0.063939,0.065562,0.063896,0.064921,0.066476,0.066192,0.065378,0.065378,0.005348,0.064382,0.065217,0.005348,0.064876,0.064876,0.064876,0.064876,0.064876,0.067858,0.065217,0.005348,0.064967,0.065911,0.064651,0.00625,0.064404,0.005348,0.0578,0.065195,0.064561,0.005494,0.005376,0.005405,0.065794,0.065126,0.064449,0.064449,0.064583,0.064921,0.064831,0.064673,0.065035,0.064921,0.066004,0.061325,0.061325,0.061325,0.061325,0.064741,0.005348,0.064944,0.06524,0.064831,0.063611,0.065149,0.064628,0.064831,0.064072,0.057782,0.061044,0.064899,0.064516,0.060175,0.060468,0.064404,0.065655,0.060331,0.066051,0.005348,0.005348,0.064921,0.065172,0.064539,0.005348,0.005348,0.065195,0.06508,0.06499,0.064741,0.064673,0.064831,0.064808,0.065493,0.065424,0.064899,0.064899,0.064673,0.005348,0.064404,0.064404,0.064404,0.064583,0.064404,0.064786,0.064404,0.064606,0.064449,0.064449,0.064718,0.064404,0.064404,0.064741,0.064516,0.064404,0.064404,0.064449,0.064404,0.064404,0.064606,0.064404,0.005348,0.064404,0.005348,0.060804,0.005348,0.061245,0.061245,0.061245,0.061245,0.061245,0.005376,0.005376,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var1_df_closseness, by=list(var1_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var1_df_closseness, by=list(var1_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-1cfe43307e407496de8d" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-1cfe43307e407496de8d">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000108\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.9e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000368\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4e-06\" data-max=\"0.000187\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.002198\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.3e-05\" data-max=\"0.000219\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.020063\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-05\" data-max=\"0.005472\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.068483\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000672\" data-max=\"0.034844\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.408791\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.004256\" data-max=\"0.040741\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.068483\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000672\" data-max=\"0.034844\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[9.8e-05,8.4e-05,9.7e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.9e-05,9.8e-05,0.000108,9.8e-05,8.9e-05,9.8e-05,9.8e-05,9.8e-05,9.7e-05,2.9e-05,9.8e-05,9.9e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05],[1e-06,2.9e-05,null,null,null,null,0,null,null,null,null,2.1e-05,1e-06,0,0,1e-06,null,null,null,1e-06,null,1e-06,0],[0.000245,9.2e-05,0.000344,0.000349,0.000353,0.000346,0.000351,0.000368,0.000347,2.9e-05,0.000355,0.000291,0.000354,0.000349,0.000351,0.00029,2.9e-05,0.000353,0.000363,0.000242,0.000355,0.000292,0.000348],[0.000187,0.00015,null,null,null,null,5e-06,null,null,null,null,0.000117,4e-06,9e-06,1.4e-05,0.000129,null,null,null,0.000157,null,0.000117,4e-06],[0.001619,0.001404,0.001645,0.001805,0.001698,0.001808,0.001739,0.002198,0.001724,0.001656,0.001786,0.001509,0.001688,0.00172,0.001789,0.00161,2.9e-05,0.001715,0.002141,0.001512,0.001795,0.001615,0.001654],[0.000219,0.000154,null,null,null,null,2.7e-05,null,null,null,null,0.000218,4.6e-05,4.8e-05,0.000111,0.000121,null,null,null,0.000161,null,3.7e-05,2.3e-05],[0.018284,0.015638,0.018071,0.018294,0.018226,0.018294,0.018277,0.018438,0.018266,0.020063,0.018253,0.016608,0.018128,0.018238,0.018238,0.018111,0.005348,0.018248,0.018369,0.01814,0.018293,0.018268,0.018247],[7.6e-05,0.005472,null,null,null,null,1.4e-05,null,null,null,null,0.003974,0.000138,1.1e-05,1e-05,0.000223,null,null,null,0.00015,null,0.000131,1.2e-05],[0.045578,0.017034,0.063918,0.064899,0.065585,0.064404,0.065209,0.068483,0.064606,0.005348,0.066004,0.054144,0.065904,0.064873,0.065264,0.05392,0.005348,0.065655,0.067489,0.044954,0.066098,0.054324,0.064714],[0.034844,0.027889,null,null,null,null,0.000812,null,null,null,null,0.021853,0.000743,0.001733,0.002445,0.024071,null,null,null,0.029252,null,0.021836,0.000672],[0.301235,0.261048,0.305921,0.33574,0.315789,0.336347,0.323528,0.408791,0.32069,0.307947,0.332143,0.280692,0.314042,0.319871,0.332793,0.29956,0.005348,0.319039,0.398287,0.281166,0.333932,0.300434,0.307558],[0.040741,0.028657,null,null,null,null,0.004916,null,null,null,null,0.040598,0.00862,0.008923,0.020639,0.022572,null,null,null,0.029853,null,0.006892,0.004256],[0.045578,0.017034,0.063918,0.064899,0.065585,0.064404,0.065209,0.068483,0.064606,0.005348,0.066004,0.054144,0.065904,0.064873,0.065264,0.05392,0.005348,0.065655,0.067489,0.044954,0.066098,0.054324,0.064714],[0.034844,0.027889,null,null,null,null,0.000812,null,null,null,null,0.021853,0.000743,0.001733,0.002445,0.024071,null,null,null,0.029252,null,0.021836,0.000672]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var1_df_closseness <- data.frame(
var1_incloseness,
var1_outcloseness,
var1_totalcloseness,
var1_incloseness_n,
var1_outcloseness_n,
var1_totalcloseness_n,
var1_centr_closeness) %>% round(6)

#Adding type
var1_df_closseness <-cbind(var1_df_closseness, V(var1)$TIPO1)

#Adding names
names(var1_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var1_df_closseness<-var1_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var1_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-29c568638bdd98d70db1" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-29c568638bdd98d70db1">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000108\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000755\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.002198\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.020063\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.140377\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.408791\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.140377\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[9.9e-05,9.8e-05,9.9e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.7e-05,9.6e-05,9.7e-05,9.8e-05,9.8e-05,9.6e-05,9.7e-05,9.8e-05,9.7e-05,9.8e-05,9.8e-05,9.7e-05,9.7e-05,9.8e-05,9.6e-05,9.8e-05,2.9e-05,9.6e-05,0.0001,9.9e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,0.0001,9.8e-05,9.6e-05,9.7e-05,9.7e-05,9.7e-05,9.7e-05,9.8e-05,9.8e-05,9.8e-05,9.9e-05,9.7e-05,9.8e-05,2.9e-05,9.7e-05,9.7e-05,9.7e-05,9.7e-05,9.7e-05,2.9e-05,9.8e-05,0.0001,9.8e-05,9.8e-05,9.6e-05,2.9e-05,9.8e-05,0.000102,9.5e-05,9.8e-05,9.8e-05,9.9e-05,2.9e-05,2.9e-05,9.7e-05,9.7e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.6e-05,9.6e-05,9.6e-05,9.6e-05,9.8e-05,0.000101,9.7e-05,9.8e-05,9.7e-05,9.5e-05,9.8e-05,9.8e-05,9.8e-05,9.6e-05,9.5e-05,9.6e-05,9.8e-05,9.7e-05,9.5e-05,9.6e-05,9.8e-05,9.8e-05,9.6e-05,9.7e-05,9.9e-05,9.9e-05,9.8e-05,9.8e-05,9.8e-05,9.9e-05,9.9e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.9e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,0.0001,9.8e-05,0.000108,9.8e-05,9.9e-05,9.7e-05,9.8e-05,9.7e-05,9.8e-05,9.7e-05,0.000102,9.9e-05,9.9e-05,0.0001,0.0001,9.6e-05,9.7e-05,0.0001,0.000102,0.0001,2.9e-05,2.9e-05,0.0001,2.9e-05,2.9e-05,0.0001,0.0001,2.9e-05,0.0001,0.000101,0.000101,0.000101,0.0001,2.9e-05,0.0001,0.0001,0.0001,0.0001,0.000101,0.000101,0.0001,9.9e-05,2.9e-05],[0.000363,0.000349,0.000368,0.000359,0.000352,0.000337,0.000353,0.000356,0.000348,0.000343,0.000342,0.000347,0.000356,0.000357,0.000346,0.000349,0.000364,0.000355,0.00035,0.000344,0.000351,0.000344,0.000347,0.000755,0.000343,2.9e-05,2.9e-05,0.000355,0.000359,0.000352,0.000351,0.000351,0.000351,2.9e-05,0.000329,0.000344,0.000352,0.000344,0.000349,0.000357,0.000356,0.000351,0.000351,2.9e-05,0.000346,0.000351,2.9e-05,0.000349,0.000349,0.000349,0.000349,0.000349,0.000365,0.000351,2.9e-05,0.000349,0.000354,0.000348,3.4e-05,0.000346,2.9e-05,0.000311,0.000351,0.000347,3e-05,2.9e-05,2.9e-05,0.000354,0.00035,0.000347,0.000347,0.000347,0.000349,0.000349,0.000348,0.00035,0.000349,0.000355,0.00033,0.00033,0.00033,0.00033,0.000348,2.9e-05,0.000349,0.000351,0.000349,0.000342,0.00035,0.000347,0.000349,0.000344,0.000311,0.000328,0.000349,0.000347,0.000324,0.000325,0.000346,0.000353,0.000324,0.000355,2.9e-05,2.9e-05,0.000349,0.00035,0.000347,2.9e-05,2.9e-05,0.000351,0.00035,0.000349,0.000348,0.000348,0.000349,0.000348,0.000352,0.000352,0.000349,0.000349,0.000348,2.9e-05,0.000346,0.000346,0.000346,0.000347,0.000346,0.000348,0.000346,0.000347,0.000347,0.000347,0.000348,0.000346,0.000346,0.000348,0.000347,0.000346,0.000346,0.000347,0.000346,0.000346,0.000347,0.000346,2.9e-05,0.000346,2.9e-05,0.000327,2.9e-05,0.000329,0.000329,0.000329,0.000329,0.000329,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.002141,0.001805,0.002198,0.001795,0.001779,0.001684,0.001698,0.001855,0.001639,0.001642,0.001701,0.001701,0.001678,0.001721,0.001634,0.001642,0.001905,0.001795,0.001629,0.001645,0.001724,0.001623,0.001724,0.001799,0.001587,0.001639,0.001401,0.00177,0.00177,0.00173,0.001733,0.001672,0.001709,0.001385,0.001637,0.001631,0.001718,0.001471,0.001629,0.001789,0.001779,0.001656,0.001661,0.001504,0.001567,0.001736,0.001353,0.001637,0.001637,0.001637,0.001637,0.001637,0.001538,0.001672,0.001397,0.001675,0.001689,0.0016,0.001471,0.001808,0.001689,0.001129,0.001675,0.001653,0.001458,0.001094,0.001088,0.001686,0.00161,0.001631,0.001645,0.001642,0.001721,0.001721,0.001667,0.001656,0.00173,0.001786,0.001276,0.001276,0.001276,0.001276,0.001642,0.001558,0.001658,0.001647,0.001684,0.001558,0.001669,0.001634,0.001637,0.001595,0.001129,0.001422,0.001645,0.001692,0.001284,0.001395,0.001645,0.001715,0.001314,0.001727,0.001304,0.001304,0.00165,0.001667,0.00165,0.001304,0.001304,0.00165,0.001647,0.00165,0.00165,0.001645,0.001645,0.001664,0.001656,0.001656,0.001639,0.001656,0.001645,0.00157,0.001631,0.001623,0.001631,0.001658,0.001623,0.001634,0.001631,0.001658,0.001626,0.001631,0.001637,0.001623,0.001629,0.00165,0.001642,0.001639,0.001661,0.001658,0.001647,0.001631,0.001631,0.001639,0.001575,0.001724,0.001656,0.001634,0.00137,0.00155,0.001626,0.00155,0.001626,0.00155,0.001681,0.001445,0.001543,0.001623,0.001621,0.00141,0.001267,0.001401,0.00137,0.001357,0.001353,0.000907,0.001377,0.001353,0.001353,0.001377,0.001383,0.001353,0.001383,0.001391,0.001443,0.001364,0.001389,0.001353,0.001389,0.001391,0.001443,0.001443,0.001433,0.001466,0.001401,0.001149,2.9e-05],[0.018369,0.018294,0.018438,0.01823,0.01825,0.018232,0.018226,0.018099,0.017878,0.018027,0.018251,0.018244,0.017804,0.01803,0.018246,0.018041,0.018232,0.018293,0.018001,0.018071,0.018268,0.01789,0.018266,0.005348,0.017814,0.018537,0.018352,0.018271,0.018226,0.018239,0.018246,0.01825,0.018237,0.018692,0.018225,0.017905,0.018088,0.018042,0.018016,0.018104,0.018244,0.018225,0.018223,0.018399,0.018023,0.018255,0.005405,0.018041,0.018041,0.018041,0.018041,0.018041,0.005348,0.018166,0.018671,0.01823,0.018253,0.017847,0.005376,0.018294,0.018926,0.017672,0.01825,0.01825,0.018381,0.005348,0.005348,0.018032,0.018027,0.018239,0.018244,0.018246,0.018246,0.01825,0.018253,0.018248,0.018251,0.018253,0.017797,0.017797,0.017797,0.017797,0.01825,0.018773,0.01802,0.01825,0.018062,0.017599,0.018248,0.018242,0.018244,0.017808,0.017674,0.017897,0.018251,0.018018,0.01759,0.017874,0.018253,0.018248,0.017878,0.018118,0.018343,0.018343,0.01825,0.01825,0.01825,0.018343,0.018343,0.01825,0.01825,0.018253,0.018251,0.01825,0.01825,0.018251,0.01825,0.01825,0.018248,0.01826,0.018248,0.018465,0.018246,0.018239,0.018246,0.018246,0.018241,0.018242,0.018244,0.018148,0.018237,0.018242,0.018244,0.018241,0.018246,0.018251,0.01825,0.018248,0.018251,0.018248,0.018253,0.018246,0.018246,0.018248,0.018537,0.018293,0.020063,0.018248,0.018372,0.018134,0.018239,0.018134,0.018239,0.018134,0.018914,0.018462,0.018474,0.018578,0.018574,0.017902,0.018123,0.018692,0.01904,0.018667,0.005405,0.005376,0.01868,0.005405,0.005405,0.018686,0.018688,0.005405,0.018686,0.018695,0.018714,0.018722,0.01869,0.005405,0.01869,0.018692,0.018667,0.018667,0.01871,0.01871,0.018692,0.018476,0.005348],[0.067489,0.064899,0.068483,0.066834,0.065447,0.062732,0.065585,0.066192,0.064741,0.06383,0.063546,0.064583,0.066216,0.066429,0.064427,0.064854,0.067612,0.066098,0.065058,0.063918,0.065195,0.063961,0.064606,0.140377,0.06383,0.005348,0.005348,0.066028,0.066762,0.06547,0.065355,0.065217,0.065195,0.005348,0.061124,0.063939,0.065562,0.063896,0.064921,0.066476,0.066192,0.065378,0.065378,0.005348,0.064382,0.065217,0.005348,0.064876,0.064876,0.064876,0.064876,0.064876,0.067858,0.065217,0.005348,0.064967,0.065911,0.064651,0.00625,0.064404,0.005348,0.0578,0.065195,0.064561,0.005494,0.005376,0.005405,0.065794,0.065126,0.064449,0.064449,0.064583,0.064921,0.064831,0.064673,0.065035,0.064921,0.066004,0.061325,0.061325,0.061325,0.061325,0.064741,0.005348,0.064944,0.06524,0.064831,0.063611,0.065149,0.064628,0.064831,0.064072,0.057782,0.061044,0.064899,0.064516,0.060175,0.060468,0.064404,0.065655,0.060331,0.066051,0.005348,0.005348,0.064921,0.065172,0.064539,0.005348,0.005348,0.065195,0.06508,0.06499,0.064741,0.064673,0.064831,0.064808,0.065493,0.065424,0.064899,0.064899,0.064673,0.005348,0.064404,0.064404,0.064404,0.064583,0.064404,0.064786,0.064404,0.064606,0.064449,0.064449,0.064718,0.064404,0.064404,0.064741,0.064516,0.064404,0.064404,0.064449,0.064404,0.064404,0.064606,0.064404,0.005348,0.064404,0.005348,0.060804,0.005348,0.061245,0.061245,0.061245,0.061245,0.061245,0.005376,0.005376,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.398287,0.33574,0.408791,0.333932,0.330961,0.313131,0.315789,0.345083,0.304918,0.305419,0.316327,0.316327,0.312081,0.320138,0.303922,0.305419,0.354286,0.333932,0.302932,0.305921,0.32069,0.301948,0.32069,0.334532,0.295238,0.304918,0.260504,0.329204,0.329204,0.321799,0.322357,0.311037,0.317949,0.257618,0.304419,0.303426,0.319588,0.273529,0.302932,0.332737,0.330961,0.307947,0.30897,0.279699,0.291536,0.322917,0.251691,0.304419,0.304419,0.304419,0.304419,0.304419,0.286154,0.311037,0.259777,0.311558,0.314189,0.2976,0.273529,0.336347,0.314189,0.209932,0.311558,0.307438,0.271137,0.203501,0.202394,0.313659,0.299517,0.303426,0.305921,0.305419,0.320138,0.320138,0.31,0.307947,0.321799,0.332143,0.237245,0.237245,0.237245,0.237245,0.305419,0.28972,0.308458,0.306425,0.313131,0.28972,0.310518,0.303922,0.304419,0.296651,0.209932,0.26458,0.305921,0.314721,0.238768,0.259414,0.305921,0.319039,0.244415,0.321244,0.242503,0.242503,0.306931,0.31,0.306931,0.242503,0.242503,0.306931,0.306425,0.306931,0.306931,0.305921,0.305921,0.309484,0.307947,0.307947,0.304918,0.307947,0.305921,0.291994,0.303426,0.301948,0.303426,0.308458,0.301948,0.303922,0.303426,0.308458,0.302439,0.303426,0.304419,0.301948,0.302932,0.306931,0.305419,0.304918,0.30897,0.308458,0.306425,0.303426,0.303426,0.304918,0.292913,0.32069,0.307947,0.303922,0.254795,0.288372,0.302439,0.288372,0.302439,0.288372,0.312605,0.268786,0.287037,0.301948,0.301459,0.262341,0.235741,0.260504,0.254795,0.252374,0.251691,0.168631,0.256198,0.251691,0.251691,0.256198,0.257261,0.251691,0.257261,0.258693,0.268398,0.253752,0.258333,0.251691,0.258333,0.258693,0.268398,0.268398,0.266476,0.272727,0.260504,0.213793,0.005348],[0.067489,0.064899,0.068483,0.066834,0.065447,0.062732,0.065585,0.066192,0.064741,0.06383,0.063546,0.064583,0.066216,0.066429,0.064427,0.064854,0.067612,0.066098,0.065058,0.063918,0.065195,0.063961,0.064606,0.140377,0.06383,0.005348,0.005348,0.066028,0.066762,0.06547,0.065355,0.065217,0.065195,0.005348,0.061124,0.063939,0.065562,0.063896,0.064921,0.066476,0.066192,0.065378,0.065378,0.005348,0.064382,0.065217,0.005348,0.064876,0.064876,0.064876,0.064876,0.064876,0.067858,0.065217,0.005348,0.064967,0.065911,0.064651,0.00625,0.064404,0.005348,0.0578,0.065195,0.064561,0.005494,0.005376,0.005405,0.065794,0.065126,0.064449,0.064449,0.064583,0.064921,0.064831,0.064673,0.065035,0.064921,0.066004,0.061325,0.061325,0.061325,0.061325,0.064741,0.005348,0.064944,0.06524,0.064831,0.063611,0.065149,0.064628,0.064831,0.064072,0.057782,0.061044,0.064899,0.064516,0.060175,0.060468,0.064404,0.065655,0.060331,0.066051,0.005348,0.005348,0.064921,0.065172,0.064539,0.005348,0.005348,0.065195,0.06508,0.06499,0.064741,0.064673,0.064831,0.064808,0.065493,0.065424,0.064899,0.064899,0.064673,0.005348,0.064404,0.064404,0.064404,0.064583,0.064404,0.064786,0.064404,0.064606,0.064449,0.064449,0.064718,0.064404,0.064404,0.064741,0.064516,0.064404,0.064404,0.064449,0.064404,0.064404,0.064606,0.064404,0.005348,0.064404,0.005348,0.060804,0.005348,0.061245,0.061245,0.061245,0.061245,0.061245,0.005376,0.005376,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var1_df_closseness, by=list(var1_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var1_df_closseness, by=list(var1_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-a2ff9bbe0bc7edcfca72" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-a2ff9bbe0bc7edcfca72">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8.8e-05\" data-max=\"9.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7e-06\" data-max=\"2.5e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000169\" data-max=\"0.000324\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8.4e-05\" data-max=\"0.000169\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001465\" data-max=\"0.001645\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000184\" data-max=\"0.000194\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.016337\" data-max=\"0.018121\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001244\" data-max=\"0.004685\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.031427\" data-max=\"0.060298\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.01566\" data-max=\"0.031399\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.272525\" data-max=\"0.306034\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.034298\" data-max=\"0.036145\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.031427\" data-max=\"0.060298\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.01566\" data-max=\"0.031399\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[9.7e-05,8.8e-05],[7e-06,2.5e-05],[0.000324,0.000169],[8.4e-05,0.000169],[0.001645,0.001465],[0.000194,0.000184],[0.018121,0.016337],[0.001244,0.004685],[0.060298,0.031428],[0.01566,0.031398],[0.306034,0.272525],[0.036145,0.034298],[0.060298,0.031428],[0.01566,0.031398]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var1_df_closseness <- data.frame(
var1_incloseness,
var1_outcloseness,
var1_totalcloseness,
var1_incloseness_n,
var1_outcloseness_n,
var1_totalcloseness_n,
var1_centr_closeness) %>% round(6)

#Adding type
var1_df_closseness <-cbind(var1_df_closseness, V(var1)$TIPO2)

#Adding names
names(var1_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var1_df_closseness<-var1_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var1_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-54f557116f1d0d973b33" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-54f557116f1d0d973b33">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000108\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000755\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.002198\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.020063\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.140377\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.408791\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.140377\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[9.9e-05,9.8e-05,9.9e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.7e-05,9.6e-05,9.7e-05,9.8e-05,9.8e-05,9.6e-05,9.7e-05,9.8e-05,9.7e-05,9.8e-05,9.8e-05,9.7e-05,9.7e-05,9.8e-05,9.6e-05,9.8e-05,2.9e-05,9.6e-05,0.0001,9.9e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,0.0001,9.8e-05,9.6e-05,9.7e-05,9.7e-05,9.7e-05,9.7e-05,9.8e-05,9.8e-05,9.8e-05,9.9e-05,9.7e-05,9.8e-05,2.9e-05,9.7e-05,9.7e-05,9.7e-05,9.7e-05,9.7e-05,2.9e-05,9.8e-05,0.0001,9.8e-05,9.8e-05,9.6e-05,2.9e-05,9.8e-05,0.000102,9.5e-05,9.8e-05,9.8e-05,9.9e-05,2.9e-05,2.9e-05,9.7e-05,9.7e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.6e-05,9.6e-05,9.6e-05,9.6e-05,9.8e-05,0.000101,9.7e-05,9.8e-05,9.7e-05,9.5e-05,9.8e-05,9.8e-05,9.8e-05,9.6e-05,9.5e-05,9.6e-05,9.8e-05,9.7e-05,9.5e-05,9.6e-05,9.8e-05,9.8e-05,9.6e-05,9.7e-05,9.9e-05,9.9e-05,9.8e-05,9.8e-05,9.8e-05,9.9e-05,9.9e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.9e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,9.8e-05,0.0001,9.8e-05,0.000108,9.8e-05,9.9e-05,9.7e-05,9.8e-05,9.7e-05,9.8e-05,9.7e-05,0.000102,9.9e-05,9.9e-05,0.0001,0.0001,9.6e-05,9.7e-05,0.0001,0.000102,0.0001,2.9e-05,2.9e-05,0.0001,2.9e-05,2.9e-05,0.0001,0.0001,2.9e-05,0.0001,0.000101,0.000101,0.000101,0.0001,2.9e-05,0.0001,0.0001,0.0001,0.0001,0.000101,0.000101,0.0001,9.9e-05,2.9e-05],[0.000363,0.000349,0.000368,0.000359,0.000352,0.000337,0.000353,0.000356,0.000348,0.000343,0.000342,0.000347,0.000356,0.000357,0.000346,0.000349,0.000364,0.000355,0.00035,0.000344,0.000351,0.000344,0.000347,0.000755,0.000343,2.9e-05,2.9e-05,0.000355,0.000359,0.000352,0.000351,0.000351,0.000351,2.9e-05,0.000329,0.000344,0.000352,0.000344,0.000349,0.000357,0.000356,0.000351,0.000351,2.9e-05,0.000346,0.000351,2.9e-05,0.000349,0.000349,0.000349,0.000349,0.000349,0.000365,0.000351,2.9e-05,0.000349,0.000354,0.000348,3.4e-05,0.000346,2.9e-05,0.000311,0.000351,0.000347,3e-05,2.9e-05,2.9e-05,0.000354,0.00035,0.000347,0.000347,0.000347,0.000349,0.000349,0.000348,0.00035,0.000349,0.000355,0.00033,0.00033,0.00033,0.00033,0.000348,2.9e-05,0.000349,0.000351,0.000349,0.000342,0.00035,0.000347,0.000349,0.000344,0.000311,0.000328,0.000349,0.000347,0.000324,0.000325,0.000346,0.000353,0.000324,0.000355,2.9e-05,2.9e-05,0.000349,0.00035,0.000347,2.9e-05,2.9e-05,0.000351,0.00035,0.000349,0.000348,0.000348,0.000349,0.000348,0.000352,0.000352,0.000349,0.000349,0.000348,2.9e-05,0.000346,0.000346,0.000346,0.000347,0.000346,0.000348,0.000346,0.000347,0.000347,0.000347,0.000348,0.000346,0.000346,0.000348,0.000347,0.000346,0.000346,0.000347,0.000346,0.000346,0.000347,0.000346,2.9e-05,0.000346,2.9e-05,0.000327,2.9e-05,0.000329,0.000329,0.000329,0.000329,0.000329,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.002141,0.001805,0.002198,0.001795,0.001779,0.001684,0.001698,0.001855,0.001639,0.001642,0.001701,0.001701,0.001678,0.001721,0.001634,0.001642,0.001905,0.001795,0.001629,0.001645,0.001724,0.001623,0.001724,0.001799,0.001587,0.001639,0.001401,0.00177,0.00177,0.00173,0.001733,0.001672,0.001709,0.001385,0.001637,0.001631,0.001718,0.001471,0.001629,0.001789,0.001779,0.001656,0.001661,0.001504,0.001567,0.001736,0.001353,0.001637,0.001637,0.001637,0.001637,0.001637,0.001538,0.001672,0.001397,0.001675,0.001689,0.0016,0.001471,0.001808,0.001689,0.001129,0.001675,0.001653,0.001458,0.001094,0.001088,0.001686,0.00161,0.001631,0.001645,0.001642,0.001721,0.001721,0.001667,0.001656,0.00173,0.001786,0.001276,0.001276,0.001276,0.001276,0.001642,0.001558,0.001658,0.001647,0.001684,0.001558,0.001669,0.001634,0.001637,0.001595,0.001129,0.001422,0.001645,0.001692,0.001284,0.001395,0.001645,0.001715,0.001314,0.001727,0.001304,0.001304,0.00165,0.001667,0.00165,0.001304,0.001304,0.00165,0.001647,0.00165,0.00165,0.001645,0.001645,0.001664,0.001656,0.001656,0.001639,0.001656,0.001645,0.00157,0.001631,0.001623,0.001631,0.001658,0.001623,0.001634,0.001631,0.001658,0.001626,0.001631,0.001637,0.001623,0.001629,0.00165,0.001642,0.001639,0.001661,0.001658,0.001647,0.001631,0.001631,0.001639,0.001575,0.001724,0.001656,0.001634,0.00137,0.00155,0.001626,0.00155,0.001626,0.00155,0.001681,0.001445,0.001543,0.001623,0.001621,0.00141,0.001267,0.001401,0.00137,0.001357,0.001353,0.000907,0.001377,0.001353,0.001353,0.001377,0.001383,0.001353,0.001383,0.001391,0.001443,0.001364,0.001389,0.001353,0.001389,0.001391,0.001443,0.001443,0.001433,0.001466,0.001401,0.001149,2.9e-05],[0.018369,0.018294,0.018438,0.01823,0.01825,0.018232,0.018226,0.018099,0.017878,0.018027,0.018251,0.018244,0.017804,0.01803,0.018246,0.018041,0.018232,0.018293,0.018001,0.018071,0.018268,0.01789,0.018266,0.005348,0.017814,0.018537,0.018352,0.018271,0.018226,0.018239,0.018246,0.01825,0.018237,0.018692,0.018225,0.017905,0.018088,0.018042,0.018016,0.018104,0.018244,0.018225,0.018223,0.018399,0.018023,0.018255,0.005405,0.018041,0.018041,0.018041,0.018041,0.018041,0.005348,0.018166,0.018671,0.01823,0.018253,0.017847,0.005376,0.018294,0.018926,0.017672,0.01825,0.01825,0.018381,0.005348,0.005348,0.018032,0.018027,0.018239,0.018244,0.018246,0.018246,0.01825,0.018253,0.018248,0.018251,0.018253,0.017797,0.017797,0.017797,0.017797,0.01825,0.018773,0.01802,0.01825,0.018062,0.017599,0.018248,0.018242,0.018244,0.017808,0.017674,0.017897,0.018251,0.018018,0.01759,0.017874,0.018253,0.018248,0.017878,0.018118,0.018343,0.018343,0.01825,0.01825,0.01825,0.018343,0.018343,0.01825,0.01825,0.018253,0.018251,0.01825,0.01825,0.018251,0.01825,0.01825,0.018248,0.01826,0.018248,0.018465,0.018246,0.018239,0.018246,0.018246,0.018241,0.018242,0.018244,0.018148,0.018237,0.018242,0.018244,0.018241,0.018246,0.018251,0.01825,0.018248,0.018251,0.018248,0.018253,0.018246,0.018246,0.018248,0.018537,0.018293,0.020063,0.018248,0.018372,0.018134,0.018239,0.018134,0.018239,0.018134,0.018914,0.018462,0.018474,0.018578,0.018574,0.017902,0.018123,0.018692,0.01904,0.018667,0.005405,0.005376,0.01868,0.005405,0.005405,0.018686,0.018688,0.005405,0.018686,0.018695,0.018714,0.018722,0.01869,0.005405,0.01869,0.018692,0.018667,0.018667,0.01871,0.01871,0.018692,0.018476,0.005348],[0.067489,0.064899,0.068483,0.066834,0.065447,0.062732,0.065585,0.066192,0.064741,0.06383,0.063546,0.064583,0.066216,0.066429,0.064427,0.064854,0.067612,0.066098,0.065058,0.063918,0.065195,0.063961,0.064606,0.140377,0.06383,0.005348,0.005348,0.066028,0.066762,0.06547,0.065355,0.065217,0.065195,0.005348,0.061124,0.063939,0.065562,0.063896,0.064921,0.066476,0.066192,0.065378,0.065378,0.005348,0.064382,0.065217,0.005348,0.064876,0.064876,0.064876,0.064876,0.064876,0.067858,0.065217,0.005348,0.064967,0.065911,0.064651,0.00625,0.064404,0.005348,0.0578,0.065195,0.064561,0.005494,0.005376,0.005405,0.065794,0.065126,0.064449,0.064449,0.064583,0.064921,0.064831,0.064673,0.065035,0.064921,0.066004,0.061325,0.061325,0.061325,0.061325,0.064741,0.005348,0.064944,0.06524,0.064831,0.063611,0.065149,0.064628,0.064831,0.064072,0.057782,0.061044,0.064899,0.064516,0.060175,0.060468,0.064404,0.065655,0.060331,0.066051,0.005348,0.005348,0.064921,0.065172,0.064539,0.005348,0.005348,0.065195,0.06508,0.06499,0.064741,0.064673,0.064831,0.064808,0.065493,0.065424,0.064899,0.064899,0.064673,0.005348,0.064404,0.064404,0.064404,0.064583,0.064404,0.064786,0.064404,0.064606,0.064449,0.064449,0.064718,0.064404,0.064404,0.064741,0.064516,0.064404,0.064404,0.064449,0.064404,0.064404,0.064606,0.064404,0.005348,0.064404,0.005348,0.060804,0.005348,0.061245,0.061245,0.061245,0.061245,0.061245,0.005376,0.005376,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.398287,0.33574,0.408791,0.333932,0.330961,0.313131,0.315789,0.345083,0.304918,0.305419,0.316327,0.316327,0.312081,0.320138,0.303922,0.305419,0.354286,0.333932,0.302932,0.305921,0.32069,0.301948,0.32069,0.334532,0.295238,0.304918,0.260504,0.329204,0.329204,0.321799,0.322357,0.311037,0.317949,0.257618,0.304419,0.303426,0.319588,0.273529,0.302932,0.332737,0.330961,0.307947,0.30897,0.279699,0.291536,0.322917,0.251691,0.304419,0.304419,0.304419,0.304419,0.304419,0.286154,0.311037,0.259777,0.311558,0.314189,0.2976,0.273529,0.336347,0.314189,0.209932,0.311558,0.307438,0.271137,0.203501,0.202394,0.313659,0.299517,0.303426,0.305921,0.305419,0.320138,0.320138,0.31,0.307947,0.321799,0.332143,0.237245,0.237245,0.237245,0.237245,0.305419,0.28972,0.308458,0.306425,0.313131,0.28972,0.310518,0.303922,0.304419,0.296651,0.209932,0.26458,0.305921,0.314721,0.238768,0.259414,0.305921,0.319039,0.244415,0.321244,0.242503,0.242503,0.306931,0.31,0.306931,0.242503,0.242503,0.306931,0.306425,0.306931,0.306931,0.305921,0.305921,0.309484,0.307947,0.307947,0.304918,0.307947,0.305921,0.291994,0.303426,0.301948,0.303426,0.308458,0.301948,0.303922,0.303426,0.308458,0.302439,0.303426,0.304419,0.301948,0.302932,0.306931,0.305419,0.304918,0.30897,0.308458,0.306425,0.303426,0.303426,0.304918,0.292913,0.32069,0.307947,0.303922,0.254795,0.288372,0.302439,0.288372,0.302439,0.288372,0.312605,0.268786,0.287037,0.301948,0.301459,0.262341,0.235741,0.260504,0.254795,0.252374,0.251691,0.168631,0.256198,0.251691,0.251691,0.256198,0.257261,0.251691,0.257261,0.258693,0.268398,0.253752,0.258333,0.251691,0.258333,0.258693,0.268398,0.268398,0.266476,0.272727,0.260504,0.213793,0.005348],[0.067489,0.064899,0.068483,0.066834,0.065447,0.062732,0.065585,0.066192,0.064741,0.06383,0.063546,0.064583,0.066216,0.066429,0.064427,0.064854,0.067612,0.066098,0.065058,0.063918,0.065195,0.063961,0.064606,0.140377,0.06383,0.005348,0.005348,0.066028,0.066762,0.06547,0.065355,0.065217,0.065195,0.005348,0.061124,0.063939,0.065562,0.063896,0.064921,0.066476,0.066192,0.065378,0.065378,0.005348,0.064382,0.065217,0.005348,0.064876,0.064876,0.064876,0.064876,0.064876,0.067858,0.065217,0.005348,0.064967,0.065911,0.064651,0.00625,0.064404,0.005348,0.0578,0.065195,0.064561,0.005494,0.005376,0.005405,0.065794,0.065126,0.064449,0.064449,0.064583,0.064921,0.064831,0.064673,0.065035,0.064921,0.066004,0.061325,0.061325,0.061325,0.061325,0.064741,0.005348,0.064944,0.06524,0.064831,0.063611,0.065149,0.064628,0.064831,0.064072,0.057782,0.061044,0.064899,0.064516,0.060175,0.060468,0.064404,0.065655,0.060331,0.066051,0.005348,0.005348,0.064921,0.065172,0.064539,0.005348,0.005348,0.065195,0.06508,0.06499,0.064741,0.064673,0.064831,0.064808,0.065493,0.065424,0.064899,0.064899,0.064673,0.005348,0.064404,0.064404,0.064404,0.064583,0.064404,0.064786,0.064404,0.064606,0.064449,0.064449,0.064718,0.064404,0.064404,0.064741,0.064516,0.064404,0.064404,0.064449,0.064404,0.064404,0.064606,0.064404,0.005348,0.064404,0.005348,0.060804,0.005348,0.061245,0.061245,0.061245,0.061245,0.061245,0.005376,0.005376,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var1_df_closseness, by=list(var1_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var1_df_closseness, by=list(var1_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-c3637597e24fc5ded45d" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-c3637597e24fc5ded45d">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8.8e-05\" data-max=\"9.8e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.5e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000171\" data-max=\"0.00035\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"9e-06\" data-max=\"0.000169\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001462\" data-max=\"0.001741\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6.5e-05\" data-max=\"0.000203\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.016289\" data-max=\"0.01824\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-05\" data-max=\"0.004696\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.031759\" data-max=\"0.065136\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001659\" data-max=\"0.031456\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.271942\" data-max=\"0.323885\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.012028\" data-max=\"0.037739\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.031759\" data-max=\"0.065136\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001659\" data-max=\"0.031456\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[9.8e-05,9.7e-05,8.8e-05],[0,7e-06,2.5e-05],[0.00035,0.000317,0.000171],[9e-06,9.4e-05,0.000169],[0.001741,0.001631,0.001462],[6.5e-05,0.000203,0.000183],[0.01824,0.018123,0.016289],[1e-05,0.001348,0.004696],[0.065136,0.058944,0.031759],[0.001659,0.017567,0.031456],[0.323885,0.303313,0.271942],[0.012028,0.037739,0.034119],[0.065136,0.058944,0.031759],[0.001659,0.017567,0.031456]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var1_data.RData")
```

