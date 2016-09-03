# SNA Closeness 2_RELACIONAMENTO FORMAL (var0)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 2_RELACIONAMENTO FORMAL (var0)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var0_data.RData")
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
#var0<-simplify(var0) #Simplify
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
V(var0)$incloseness <- closeness(var0, mode = "in", weights = E(var0)$var0) %>% round(6)
V(var0)$outcloseness <- closeness(var0, mode = "out", weights = E(var0)$var0) %>% round(6)
V(var0)$totalcloseness <- closeness(var0, mode = "total", weights = E(var0)$var0) %>% round(4)
```

###Saving to Environment

```r
var0_incloseness<- closeness(var0, mode = "in", weights = E(var0)$var0) %>% round(6)
var0_outcloseness<- closeness(var0, mode = "out", weights = E(var0)$var0) %>% round(6)
var0_totalcloseness<- closeness(var0, mode = "total", weights = E(var0)$var0) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var0_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 2.900e-05 7.200e-05 7.200e-05 6.492e-05 7.200e-05 7.600e-05
```

```r
sd(var0_incloseness)
```

```
## [1] 1.638413e-05
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var0)$incloseness<-closeness(var0, weights = E(var0)$var0, mode="in")

#Get Variable
V(var0)$var0_color_degree<-round(V(var0)$incloseness,6)

#Creating brewer pallette
vertex_var0_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var0)$var0_color_degree)), "RdBu"))(
            length(unique(V(var0)$var0_color_degree)))

#Saving as Vertex properties 
V(var0)$vertex_var0_color_degree<-
  vertex_var0_color_degree[as.numeric(
  cut(V(var0)$var0_color_degree,
      breaks=length(unique(V(var0)$var0_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var0, es=E(var0), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var0))
maxC <- rep(Inf, vcount(var0))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var0, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var0)$weight)


#PLotting
plot(var0, 
     layout=co,
     edge.color=V(var0)$vertex_var0_color_degree[edge.start],
     edge.arrow.size=closeness(var0, weights = E(var0)$var0, mode="in"),
     edge.width=E(var0)$weight/mean(E(var0)$weight),
     edge.curved = TRUE,
     vertex.color=V(var0)$vertex_var0_color_degree,
     vertex.size=closeness(var0, weights = E(var0)$var0, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var0,"LABEL_COR"),
     vertex.label.cex=(closeness(var0, weights = E(var0)$var0, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var0)$var0_color_degree
b<-V(var0)$vertex_var0_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 2_RELACIONAMENTO FORMAL (var0)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var0, mode="in", weights = E(var0)$var0)), 
             sd(closeness(var0, mode="in", weights = E(var0)$var0))
             )
       )
```

![](2_RELACIONAMENTO_FORMAL_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var0_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0000290 0.0001570 0.0001091 0.0001580 0.0004730
```

```r
sd(var0_outcloseness)
```

```
## [1] 6.830992e-05
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var0)$outcloseness<-closeness(var0, weights = E(var0)$var0, mode="out")

#Get Variable
V(var0)$var0_color_degree<-round(V(var0)$outcloseness,6)

#Creating brewer pallette
vertex_var0_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var0)$var0_color_degree)), "RdBu"))(
            length(unique(V(var0)$var0_color_degree)))

#Saving as Vertex properties 
V(var0)$vertex_var0_color_degree<-
  vertex_var0_color_degree[as.numeric(
  cut(V(var0)$var0_color_degree,
      breaks=length(unique(V(var0)$var0_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var0, es=E(var0), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var0))
maxC <- rep(Inf, vcount(var0))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var0, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var0)$weight)


#PLotting
plot(var0, 
     layout=co,
     edge.color=V(var0)$vertex_var0_color_degree[edge.start],
     edge.arrow.size=closeness(var0, weights = E(var0)$var0, mode="out"),
     edge.width=E(var0)$weight/2*mean(E(var0)$weight),
     edge.curved = TRUE,
     vertex.color=V(var0)$vertex_var0_color_degree,
     vertex.size=closeness(var0, weights = E(var0)$var0, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var0,"LABEL_COR"),
     vertex.label.cex=closeness(var0, weights = E(var0)$var0, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var0)$var0_color_degree
b<-V(var0)$vertex_var0_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 2_RELACIONAMENTO FORMAL (var0)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var0, mode="out", weights = E(var0)$var0)), 
             sd(closeness(var0, mode="out", weights = E(var0)$var0))
             )
       )
```

![](2_RELACIONAMENTO_FORMAL_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var0_totalcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0006740 0.0007260 0.0006898 0.0007320 0.0008220
```

```r
sd(var0_totalcloseness)
```

```
## [1] 0.0001176304
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var0)$allcloseness<-closeness(var0, weights = E(var0)$var0, mode="all")

#Get Variable
V(var0)$var0_color_degree<-round(V(var0)$allcloseness,6)

#Creating brewer pallette
vertex_var0_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var0)$var0_color_degree)), "RdBu"))(
            length(unique(V(var0)$var0_color_degree)))

#Saving as Vertex properties 
V(var0)$vertex_var0_color_degree<-
  vertex_var0_color_degree[as.numeric(
  cut(V(var0)$var0_color_degree,
      breaks=length(unique(V(var0)$var0_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var0, es=E(var0), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var0))
maxC <- rep(Inf, vcount(var0))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var0, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var0)$weight)


#PLotting
plot(var0, 
     layout=co,
     edge.color=V(var0)$vertex_var0_color_degree[edge.start],
     edge.arrow.size=closeness(var0, weights = E(var0)$var0, mode="all"),
     edge.width=E(var0)$weight/2*mean(E(var0)$weight),
     edge.curved = TRUE,
     vertex.color=V(var0)$vertex_var0_color_degree,
     vertex.size=closeness(var0, weights = E(var0)$var0, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var0,"LABEL_COR"),
     vertex.label.cex=(closeness(var0, weights = E(var0)$var0, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var0)$var0_color_degree
b<-V(var0)$vertex_var0_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 2_RELACIONAMENTO FORMAL (var0)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var0, mode="all", weights = E(var0)$var0)), 
             sd(closeness(var0, mode="all", weights = E(var0)$var0))
             )
       )
```

![](2_RELACIONAMENTO_FORMAL_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var0)$incloseness_n <- closeness(var0, mode = "in",, weights = E(var0)$var0, normalized = T) %>% round(10)
V(var0)$outcloseness_n <- closeness(var0, mode = "out", normalized = T, weights = E(var0)$var0) %>% round(6)
V(var0)$totalcloseness_n <- closeness(var0, mode = "total", normalized = T, weights = E(var0)$var0) %>% round(6)
```

###Saving to Environment

```r
var0_incloseness_n<- closeness(var0, mode = "in", normalized = T, weights = E(var0)$var0) %>% round(6)
var0_outcloseness_n<- closeness(var0, mode = "out", normalized = T, weights = E(var0)$var0) %>% round(6)
var0_totalcloseness_n<- closeness(var0, mode = "total", normalized = T, weights = E(var0)$var0) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var0_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.013340 0.013390 0.012070 0.013420 0.014210
```

```r
sd(var0_incloseness_n)
```

```
## [1] 0.003055024
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var0)$incloseness_n<-closeness(var0, weights = E(var0)$var0, mode="in", normalized = T)

#Get Variable
V(var0)$var0_color_degree<-round(V(var0)$incloseness_n,6)

#Creating brewer pallette
vertex_var0_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var0)$var0_color_degree)), "RdBu"))(
            length(unique(V(var0)$var0_color_degree)))

#Saving as Vertex properties 
V(var0)$vertex_var0_color_degree<-
  vertex_var0_color_degree[as.numeric(
  cut(V(var0)$var0_color_degree,
      breaks=length(unique(V(var0)$var0_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var0, es=E(var0), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var0))
maxC <- rep(Inf, vcount(var0))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var0, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var0)$weight)


#PLotting
plot(var0, 
     layout=co,
     edge.color=V(var0)$vertex_var0_color_degree[edge.start],
     edge.arrow.size=closeness(var0, weights = E(var0)$var0, mode="in",normalized = T),
     edge.width=E(var0)$weight/10*mean(E(var0)$weight),
     edge.curved = TRUE,
     vertex.color=V(var0)$vertex_var0_color_degree,
     vertex.size=(closeness(var0, weights = E(var0)$var0, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var0,"LABEL_COR"),
     vertex.label.cex=closeness(var0, weights = E(var0)$var0, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var0)$var0_color_degree
b<-V(var0)$vertex_var0_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 2_RELACIONAMENTO FORMAL (var0)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var0, mode="in", weights = E(var0)$var0, normalized = T)), 
             sd(closeness(var0, mode="in", weights = E(var0)$var0, normalized = T))
             )
       )
```

![](2_RELACIONAMENTO_FORMAL_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var0_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.005348 0.029260 0.020280 0.029340 0.087900
```

```r
sd(var0_outcloseness_n)
```

```
## [1] 0.01271714
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var0)$outcloseness_n<-closeness(var0, weights = E(var0)$var0, mode="out", normalized = T)

#Get Variable
V(var0)$var0_color_degree<-round(V(var0)$outcloseness_n,6)

#Creating brewer pallette
vertex_var0_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var0)$var0_color_degree)), "RdBu"))(
            length(unique(V(var0)$var0_color_degree)))

#Saving as Vertex properties 
V(var0)$vertex_var0_color_degree<-
  vertex_var0_color_degree[as.numeric(
  cut(V(var0)$var0_color_degree,
      breaks=length(unique(V(var0)$var0_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var0, es=E(var0), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var0))
maxC <- rep(Inf, vcount(var0))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var0, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var0)$weight)


#PLotting
plot(var0, 
     layout=co,
     edge.color=V(var0)$vertex_var0_color_degree[edge.start],
     edge.arrow.size=closeness(var0, weights = E(var0)$var0, mode="out",normalized = T),
     edge.width=E(var0)$weight/10*mean(E(var0)$weight),
     edge.curved = TRUE,
     vertex.color=V(var0)$vertex_var0_color_degree,
     vertex.size=(closeness(var0, weights = E(var0)$var0, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var0,"LABEL_COR"),
     vertex.label.cex=closeness(var0, weights = E(var0)$var0, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var0)$var0_color_degree
b<-V(var0)$vertex_var0_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 2_RELACIONAMENTO FORMAL (var0)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var0, mode="out", weights = E(var0)$var0, normalized = T)), 
             sd(closeness(var0, mode="out", weights = E(var0)$var0, normalized = T))
             )
       )
```

![](2_RELACIONAMENTO_FORMAL_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var0_totalcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.125400 0.135100 0.128300 0.136100 0.153000
```

```r
sd(var0_totalcloseness_n)
```

```
## [1] 0.02188651
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var0)$allcloseness_n<-closeness(var0, weights = E(var0)$var0, mode="all", normalized = T)

#Get Variable
V(var0)$var0_color_degree<-round(V(var0)$allcloseness_n,6)

#Creating brewer pallette
vertex_var0_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var0)$var0_color_degree)), "RdBu"))(
            length(unique(V(var0)$var0_color_degree)))

#Saving as Vertex properties 
V(var0)$vertex_var0_color_degree<-
  vertex_var0_color_degree[as.numeric(
  cut(V(var0)$var0_color_degree,
      breaks=length(unique(V(var0)$var0_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var0, es=E(var0), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var0))
maxC <- rep(Inf, vcount(var0))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var0, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var0)$weight)


#PLotting
plot(var0, 
     layout=co,
     edge.color=V(var0)$vertex_var0_color_degree[edge.start],
     edge.arrow.size=closeness(var0, weights = E(var0)$var0, mode="all",normalized = T),
     edge.width=E(var0)$weight/10*mean(E(var0)$weight),
     edge.curved = TRUE,
     vertex.color=V(var0)$vertex_var0_color_degree,
     vertex.size=(closeness(var0, weights = E(var0)$var0, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var0,"LABEL_COR"),
     vertex.label.cex=closeness(var0, weights = E(var0)$var0, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var0)$var0_color_degree
b<-V(var0)$vertex_var0_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 2_RELACIONAMENTO FORMAL (var0)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var0, mode="all", weights = E(var0)$var0, normalized = T)), 
             sd(closeness(var0, mode="all", weights = E(var0)$var0, normalized = T))
             )
       )
```

![](2_RELACIONAMENTO_FORMAL_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var0)$incloseness_n <- closeness(var0, weights = E(var0)$var0, mode = "in", normalized = T) %>% round(6)
V(var0)$outcloseness_n <- closeness(var0, weights = E(var0)$var0, mode = "out", normalized = T) %>% round(6)
V(var0)$totalcloseness_n <- closeness(var0, weights = E(var0)$var0, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var0)$var0_centr_closeness<- centralization.closeness(var0)$res
var0_centr_closeness<- centralization.closeness(var0)$res
var0_centr_closeness_all<- centralization.closeness(var0)
```

###Centralization

```r
var0_centr_closeness_all$centralization
```

```
## [1] 0.06834656
```

###Theoretical Max

```r
var0_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var0)$var0_centr_closeness<- centralization.closeness(var0)$res

#Get Variable
V(var0)$var0_color_degree<-round(V(var0)$var0_centr_closeness,6)

#Creating brewer pallette
vertex_var0_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var0)$var0_color_degree)), "Spectral"))(
            length(unique(V(var0)$var0_color_degree)))

#Saving as Vertex properties 
V(var0)$vertex_var0_color_degree<-
  vertex_var0_color_degree[as.numeric(
  cut(V(var0)$var0_color_degree,
      breaks=length(unique(V(var0)$var0_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var0, es=E(var0), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var0))
maxC <- rep(Inf, vcount(var0))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var0, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var0)$weight)


#PLotting
plot(var0, 
     layout=co,
     edge.color=V(var0)$vertex_var0_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var0)$res,
     edge.width=E(var0)$weight/10*mean(E(var0)$weight),
     edge.curved = TRUE,
     vertex.color=V(var0)$vertex_var0_color_degree,
     vertex.size=centralization.closeness(var0)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var0,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var0)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var0)$var0_color_degree
b<-V(var0)$vertex_var0_color_degree
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
  title("Network Centralization Closeness - 2_RELACIONAMENTO FORMAL (var0)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var0)$res), 
             sd(centralization.closeness(var0)$res)
             )
       )
```

![](2_RELACIONAMENTO_FORMAL_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var0_incloseness<- closeness(var0, weights = E(var0)$var0, mode = "in") %>% round(6)
var0_outcloseness<- closeness(var0, weights = E(var0)$var0, mode = "out") %>% round(6)
var0_totalcloseness<- closeness(var0, weights = E(var0)$var0, mode = "total") %>% round(6)
var0_incloseness_n<- closeness(var0,weights = E(var0)$var0, mode = "in", normalized = T) %>% round(6)
var0_outcloseness_n<- closeness(var0,weights = E(var0)$var0, mode = "out", normalized = T) %>% round(6)
var0_totalcloseness_n<- closeness(var0,weights = E(var0)$var0, mode = "total", normalized = T) %>% round(6)
var0_centr_closeness <- centralization.closeness(var0)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var0_df_closseness <- data.frame(
var0_incloseness,
var0_outcloseness,
var0_totalcloseness,
var0_incloseness_n,
var0_outcloseness_n,
var0_totalcloseness_n,
var0_centr_closeness) %>% round(6)

#Adding type
var0_df_closseness <-cbind(var0_df_closseness, V(var0)$LABEL_COR)

#Adding names
names(var0_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var0_df_closseness<-var0_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var0_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-072495e4189ede06d828" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-072495e4189ede06d828">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"7.6e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000473\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000822\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.014214\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.087902\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.152961\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.087902\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.1e-05,7.2e-05,7.2e-05,7.3e-05,2.9e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.3e-05,7.3e-05,7.1e-05,7.3e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.4e-05,7.3e-05,7.2e-05,7.2e-05,7.2e-05,7.4e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.2e-05,2.9e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,2.9e-05,7.2e-05,2.9e-05,7.2e-05,7.2e-05,2.9e-05,7.2e-05,7.2e-05,7.3e-05,7.4e-05,7.2e-05,7.2e-05,7.2e-05,2.9e-05,2.9e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.4e-05,7.6e-05,7.6e-05,7.6e-05,7.6e-05,7.2e-05,7.2e-05,7.6e-05,7.2e-05,7.3e-05,2.9e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.4e-05,7.5e-05,7.2e-05,7.3e-05,2.9e-05,7.3e-05,7.2e-05,7.2e-05,7.1e-05,7.2e-05,7.3e-05,7.3e-05,7.2e-05,7.2e-05,7.3e-05,7.3e-05,7.3e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.1e-05,7.3e-05,7.3e-05,7.3e-05,2.9e-05,2.9e-05,2.9e-05,7.2e-05,7.2e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,7.2e-05,7.2e-05,2.9e-05,2.9e-05,7.3e-05],[0.000161,0.000159,0.000161,0.00016,0.000158,0.000158,0.000159,0.000159,0.000155,0.000156,0.000158,2.9e-05,0.000169,0.000159,0.000158,0.000157,0.00016,0.000158,0.000158,0.000157,0.000158,2.9e-05,2.9e-05,2.9e-05,0.000157,2.9e-05,2.9e-05,0.000159,0.000159,0.000158,2.9e-05,0.000154,0.000158,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000156,0.000158,0.000158,2.9e-05,0.000158,0.000158,0.000157,2.9e-05,0.000156,2.9e-05,0.000157,0.000157,0.000157,0.000157,0.000157,2.9e-05,0.000158,0.000473,0.000158,0.000158,2.9e-05,2.9e-05,0.000158,2.9e-05,2.9e-05,0.000158,0.000158,2.9e-05,2.9e-05,2.9e-05,0.000155,0.000157,0.000157,0.000157,0.000158,0.000158,0.000158,0.000158,0.000157,0.000158,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000158,2.9e-05,2.9e-05,0.000158,2.9e-05,0.000167,0.000158,0.000158,0.000157,2.9e-05,2.9e-05,2.9e-05,0.000158,2.9e-05,0.000162,2.9e-05,0.000157,0.000158,0.000154,0.000159,2.9e-05,2.9e-05,0.000158,0.000158,2.9e-05,2.9e-05,2.9e-05,0.000158,0.000158,0.000158,0.000158,0.000158,0.000158,0.000158,0.000158,0.000158,0.000158,0.000158,0.000158,0.000159,0.000158,0.000158,0.000158,0.000158,0.000157,0.000158,0.000157,2.9e-05,0.000158,0.000158,0.000158,0.000157,0.000157,0.000158,0.000157,0.000157,0.000157,0.000155,0.000157,0.000157,0.000157,0.000157,0.000157,0.000157,0.000158,0.000154,2.9e-05,0.000155,0.000155,0.000155,0.000155,0.000155,0.000154,0.000154,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000803,0.000758,0.000822,0.000772,0.000747,0.000746,0.000741,0.000749,0.000662,0.000726,0.000743,0.000741,0.000675,0.000764,0.000728,0.000733,0.000769,0.000756,0.000722,0.000732,0.000745,0.000662,0.000737,0.000629,0.000704,0.000716,0.000696,0.000745,0.000746,0.000736,0.000734,0.000727,0.000744,0.000688,0.000732,0.000661,0.000729,0.000697,0.00073,0.000749,0.000745,0.000731,0.000736,0.000729,0.000716,0.000739,0.000627,0.000726,0.000726,0.000726,0.000726,0.000726,2.9e-05,0.00075,0.000707,0.000742,0.00075,0.000627,0.000698,0.000755,0.00075,0.000637,0.000733,0.000743,0.00066,2.9e-05,0.000627,0.00073,0.000722,0.000724,0.000724,0.000728,0.000733,0.000734,0.000732,0.000726,0.000728,0.000747,0.000635,0.000635,0.000635,0.000635,0.00074,0.000709,0.000716,0.000728,0.000682,0.000704,0.000738,0.000727,0.000726,0.000672,0.000637,0.000718,0.000729,0.00066,0.000629,0.000661,0.000727,0.000743,0.000644,0.000739,0.000673,0.000673,0.000726,0.000733,0.000726,0.000673,0.000682,0.000732,0.000731,0.00073,0.000728,0.000733,0.000725,0.000728,0.000728,0.000729,0.000727,0.000729,0.000729,0.000739,0.000725,0.000724,0.000725,0.000732,0.000724,0.000725,0.000729,0.00072,0.000727,0.000728,0.000728,0.000724,0.000725,0.00073,0.000726,0.000726,0.000729,0.000731,0.000727,0.000727,0.000725,0.000726,0.000718,0.000739,0.000742,0.000727,0.000679,0.000706,0.000727,0.000706,0.000727,0.000706,0.000756,0.000709,0.000702,0.000724,0.000724,2.9e-05,2.9e-05,0.000627,0.000708,0.000708,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000692,0.000708,0.000627,2.9e-05,0.000716],[0.013442,0.013421,0.01348,0.013399,0.013404,0.0134,0.013385,0.013383,0.013276,0.013378,0.013403,0.013592,0.005348,0.013301,0.013388,0.013388,0.013405,0.01342,0.013305,0.013384,0.013401,0.013652,0.013589,0.013544,0.013202,0.013552,0.013469,0.01341,0.013398,0.013395,0.013582,0.013392,0.013403,0.013465,0.013577,0.013827,0.013571,0.013334,0.013326,0.013398,0.013771,0.013392,0.013387,0.01338,0.013552,0.013405,0.005376,0.013335,0.013335,0.013335,0.013335,0.013335,0.005348,0.013356,0.005348,0.013398,0.013394,0.005376,0.013481,0.013406,0.013555,0.013812,0.013392,0.013391,0.013454,0.005348,0.005376,0.013378,0.013305,0.013384,0.013386,0.013386,0.013388,0.013389,0.013391,0.013389,0.013391,0.013784,0.014091,0.014091,0.014091,0.014091,0.01339,0.013473,0.014214,0.013387,0.01364,0.005405,0.013389,0.013387,0.013389,0.013371,0.013812,0.013929,0.013391,0.013637,0.005405,0.013638,0.013387,0.013402,0.013199,0.013342,0.013505,0.013505,0.013391,0.01339,0.013572,0.013505,0.013514,0.013392,0.013392,0.013391,0.013393,0.01339,0.013387,0.013393,0.013391,0.013391,0.01339,0.013396,0.01339,0.01339,0.013388,0.013384,0.013388,0.013388,0.013386,0.013387,0.013388,0.013522,0.013383,0.013387,0.013386,0.013386,0.013387,0.013387,0.01339,0.013389,0.013393,0.013389,0.013393,0.013391,0.013389,0.01339,0.013375,0.013411,0.013399,0.013391,0.013375,0.013335,0.013384,0.013335,0.013384,0.013335,0.013379,0.013203,0.01352,0.013567,0.013564,0.005348,0.005348,0.005376,0.013473,0.013473,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.013432,0.013473,0.005376,0.005348,0.013552],[0.029899,0.029519,0.029985,0.02967,0.029472,0.029384,0.029486,0.029491,0.02881,0.028927,0.029393,0.005348,0.031509,0.029585,0.029338,0.029282,0.029741,0.029472,0.029305,0.029241,0.029416,0.005405,0.005348,0.005348,0.02919,0.005348,0.005348,0.02951,0.029519,0.029333,0.005348,0.028646,0.029416,0.005348,0.005348,0.005348,0.005348,0.028945,0.029393,0.029472,0.005348,0.029361,0.029361,0.029264,0.005348,0.028963,0.005348,0.029291,0.029291,0.029291,0.029291,0.029291,0.005348,0.029384,0.087902,0.02937,0.029444,0.005348,0.005348,0.029333,0.005348,0.005405,0.029426,0.029338,0.005348,0.005348,0.005348,0.028895,0.029117,0.029264,0.029264,0.029324,0.029407,0.029384,0.029347,0.029282,0.029338,0.005348,0.005464,0.005464,0.005464,0.005464,0.029338,0.005376,0.005464,0.029379,0.005376,0.030985,0.029472,0.029314,0.029268,0.005348,0.005405,0.005405,0.029365,0.005376,0.030205,0.005405,0.029282,0.029472,0.028677,0.029486,0.005348,0.005348,0.029324,0.02943,0.005348,0.005348,0.005348,0.029338,0.029342,0.029361,0.029333,0.029324,0.029333,0.029347,0.029375,0.029375,0.029342,0.029342,0.029393,0.029486,0.029301,0.029301,0.029301,0.029324,0.029264,0.02931,0.029264,0.005348,0.029319,0.029319,0.029356,0.029264,0.029264,0.029384,0.029278,0.029264,0.029264,0.028824,0.029268,0.029264,0.029273,0.029264,0.029264,0.029264,0.029319,0.02858,0.005405,0.028824,0.028824,0.028824,0.028824,0.028824,0.028633,0.028642,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.149278,0.141016,0.152961,0.143519,0.139013,0.138806,0.13788,0.139222,0.123179,0.134978,0.13829,0.13788,0.125591,0.142093,0.135371,0.136364,0.142967,0.140696,0.134296,0.136064,0.138599,0.123097,0.137067,0.116981,0.130986,0.133238,0.129526,0.138496,0.138702,0.136966,0.136564,0.135174,0.138393,0.128011,0.136064,0.122935,0.135667,0.129617,0.135766,0.139326,0.138496,0.135965,0.136865,0.135667,0.133238,0.137371,0.116614,0.135076,0.135076,0.135076,0.135076,0.135076,0.005348,0.139535,0.131449,0.138085,0.13943,0.116614,0.129888,0.140483,0.139535,0.118396,0.136364,0.138187,0.122691,0.005348,0.116614,0.135766,0.134296,0.134685,0.134685,0.135371,0.136264,0.136464,0.136164,0.135076,0.135371,0.13891,0.11817,0.11817,0.11817,0.11817,0.137676,0.131915,0.133142,0.135371,0.126789,0.130894,0.137269,0.135174,0.134978,0.124916,0.118396,0.133621,0.135667,0.122772,0.116908,0.122935,0.135174,0.13829,0.119845,0.137472,0.125253,0.125253,0.135076,0.136264,0.135076,0.125253,0.126789,0.136064,0.135965,0.135766,0.135371,0.136264,0.13488,0.13547,0.135371,0.135569,0.135174,0.135667,0.135667,0.137472,0.134783,0.134685,0.134783,0.136064,0.134685,0.13488,0.135569,0.133909,0.135174,0.135371,0.135371,0.134685,0.134783,0.135766,0.135076,0.134978,0.135667,0.135965,0.135273,0.135174,0.13488,0.135076,0.133525,0.137472,0.138085,0.135174,0.126273,0.131356,0.135174,0.131356,0.135174,0.131356,0.14059,0.131821,0.130618,0.134685,0.134588,0.005348,0.005348,0.116614,0.131728,0.131728,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.12872,0.131728,0.116614,0.005348,0.133238],[0.029899,0.029519,0.029985,0.02967,0.029472,0.029384,0.029486,0.029491,0.02881,0.028927,0.029393,0.005348,0.031509,0.029585,0.029338,0.029282,0.029741,0.029472,0.029305,0.029241,0.029416,0.005405,0.005348,0.005348,0.02919,0.005348,0.005348,0.02951,0.029519,0.029333,0.005348,0.028646,0.029416,0.005348,0.005348,0.005348,0.005348,0.028945,0.029393,0.029472,0.005348,0.029361,0.029361,0.029264,0.005348,0.028963,0.005348,0.029291,0.029291,0.029291,0.029291,0.029291,0.005348,0.029384,0.087902,0.02937,0.029444,0.005348,0.005348,0.029333,0.005348,0.005405,0.029426,0.029338,0.005348,0.005348,0.005348,0.028895,0.029117,0.029264,0.029264,0.029324,0.029407,0.029384,0.029347,0.029282,0.029338,0.005348,0.005464,0.005464,0.005464,0.005464,0.029338,0.005376,0.005464,0.029379,0.005376,0.030985,0.029472,0.029314,0.029268,0.005348,0.005405,0.005405,0.029365,0.005376,0.030205,0.005405,0.029282,0.029472,0.028677,0.029486,0.005348,0.005348,0.029324,0.02943,0.005348,0.005348,0.005348,0.029338,0.029342,0.029361,0.029333,0.029324,0.029333,0.029347,0.029375,0.029375,0.029342,0.029342,0.029393,0.029486,0.029301,0.029301,0.029301,0.029324,0.029264,0.02931,0.029264,0.005348,0.029319,0.029319,0.029356,0.029264,0.029264,0.029384,0.029278,0.029264,0.029264,0.028824,0.029268,0.029264,0.029273,0.029264,0.029264,0.029264,0.029319,0.02858,0.005405,0.028824,0.028824,0.028824,0.028824,0.028824,0.028633,0.028642,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var0_df_closseness, by=list(var0_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var0_df_closseness, by=list(var0_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-806d60ae4f2dab339cac" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-806d60ae4f2dab339cac">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.7e-05\" data-max=\"7.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000161\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"7.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000613\" data-max=\"0.000822\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3e-06\" data-max=\"0.000209\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.008782\" data-max=\"0.013784\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3e-06\" data-max=\"0.004114\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.029985\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000124\" data-max=\"0.013879\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.113918\" data-max=\"0.152961\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000623\" data-max=\"0.038816\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.029985\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000124\" data-max=\"0.013879\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[7.3e-05,4.7e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.2e-05,7.4e-05,6.1e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05],[1e-06,2.2e-05,null,null,null,null,0,null,null,null,null,2e-05,0,0,0,1e-06,null,null,null,0,null,0,0],[7.2e-05,4.4e-05,0.000157,0.000159,0.000159,0.000158,0.000158,0.000161,2.9e-05,0.000158,2.9e-05,7.8e-05,0.000158,0.000115,0.000159,0.000146,2.9e-05,0.000158,0.000161,0.000114,0.000158,0.00013,0.000155],[7.4e-05,7e-05,null,null,null,null,1e-06,null,null,null,null,6.5e-05,1e-06,6.5e-05,1e-06,3.9e-05,null,null,null,6.3e-05,null,5.4e-05,1.9e-05],[0.000723,0.000615,0.000732,0.000758,0.000741,0.000755,0.000743,0.000822,0.000737,0.000742,0.000747,0.000613,0.000748,0.000743,0.000754,0.00072,0.000716,0.000743,0.000803,0.000708,0.000756,0.000722,0.00073],[3.8e-05,0.000162,null,null,null,null,3e-06,null,null,null,null,0.000209,2.3e-05,1.2e-05,1.3e-05,2.8e-05,null,null,null,2.6e-05,null,9e-06,6e-06],[0.013516,0.008782,0.013384,0.013421,0.013385,0.013406,0.013407,0.01348,0.013589,0.013399,0.013784,0.011434,0.013346,0.013461,0.013403,0.013348,0.013552,0.013402,0.013442,0.013396,0.01342,0.013411,0.013393],[0.000221,0.004114,null,null,null,null,6e-06,null,null,null,null,0.003727,6.4e-05,9.2e-05,3e-06,9.8e-05,null,null,null,8.4e-05,null,7.3e-05,2.7e-05],[0.01339,0.008168,0.029241,0.029519,0.029486,0.029333,0.029397,0.029985,0.005348,0.029319,0.005348,0.014406,0.029473,0.021366,0.029532,0.027055,0.005348,0.029472,0.029899,0.021289,0.029472,0.024136,0.028794],[0.013879,0.012939,null,null,null,null,0.000124,null,null,null,null,0.012161,0.000158,0.012015,0.000186,0.007204,null,null,null,0.011774,null,0.00997,0.00346],[0.134387,0.114473,0.136064,0.141016,0.13788,0.140483,0.138189,0.152961,0.137067,0.138085,0.13891,0.113918,0.139029,0.13816,0.140262,0.133917,0.133238,0.13829,0.149278,0.13164,0.140696,0.134209,0.135733],[0.007027,0.030055,null,null,null,null,0.000623,null,null,null,null,0.038816,0.004333,0.002182,0.002345,0.005236,null,null,null,0.004752,null,0.001686,0.001077],[0.01339,0.008168,0.029241,0.029519,0.029486,0.029333,0.029397,0.029985,0.005348,0.029319,0.005348,0.014406,0.029473,0.021366,0.029532,0.027055,0.005348,0.029472,0.029899,0.021289,0.029472,0.024136,0.028794],[0.013879,0.012939,null,null,null,null,0.000124,null,null,null,null,0.012161,0.000158,0.012015,0.000186,0.007204,null,null,null,0.011774,null,0.00997,0.00346]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var0_df_closseness <- data.frame(
var0_incloseness,
var0_outcloseness,
var0_totalcloseness,
var0_incloseness_n,
var0_outcloseness_n,
var0_totalcloseness_n,
var0_centr_closeness) %>% round(6)

#Adding type
var0_df_closseness <-cbind(var0_df_closseness, V(var0)$TIPO1)

#Adding names
names(var0_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var0_df_closseness<-var0_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var0_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-d480ecfc2977003dc23d" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-d480ecfc2977003dc23d">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"7.6e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000473\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000822\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.014214\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.087902\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.152961\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.087902\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.1e-05,7.2e-05,7.2e-05,7.3e-05,2.9e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.3e-05,7.3e-05,7.1e-05,7.3e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.4e-05,7.3e-05,7.2e-05,7.2e-05,7.2e-05,7.4e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.2e-05,2.9e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,2.9e-05,7.2e-05,2.9e-05,7.2e-05,7.2e-05,2.9e-05,7.2e-05,7.2e-05,7.3e-05,7.4e-05,7.2e-05,7.2e-05,7.2e-05,2.9e-05,2.9e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.4e-05,7.6e-05,7.6e-05,7.6e-05,7.6e-05,7.2e-05,7.2e-05,7.6e-05,7.2e-05,7.3e-05,2.9e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.4e-05,7.5e-05,7.2e-05,7.3e-05,2.9e-05,7.3e-05,7.2e-05,7.2e-05,7.1e-05,7.2e-05,7.3e-05,7.3e-05,7.2e-05,7.2e-05,7.3e-05,7.3e-05,7.3e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.1e-05,7.3e-05,7.3e-05,7.3e-05,2.9e-05,2.9e-05,2.9e-05,7.2e-05,7.2e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,7.2e-05,7.2e-05,2.9e-05,2.9e-05,7.3e-05],[0.000161,0.000159,0.000161,0.00016,0.000158,0.000158,0.000159,0.000159,0.000155,0.000156,0.000158,2.9e-05,0.000169,0.000159,0.000158,0.000157,0.00016,0.000158,0.000158,0.000157,0.000158,2.9e-05,2.9e-05,2.9e-05,0.000157,2.9e-05,2.9e-05,0.000159,0.000159,0.000158,2.9e-05,0.000154,0.000158,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000156,0.000158,0.000158,2.9e-05,0.000158,0.000158,0.000157,2.9e-05,0.000156,2.9e-05,0.000157,0.000157,0.000157,0.000157,0.000157,2.9e-05,0.000158,0.000473,0.000158,0.000158,2.9e-05,2.9e-05,0.000158,2.9e-05,2.9e-05,0.000158,0.000158,2.9e-05,2.9e-05,2.9e-05,0.000155,0.000157,0.000157,0.000157,0.000158,0.000158,0.000158,0.000158,0.000157,0.000158,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000158,2.9e-05,2.9e-05,0.000158,2.9e-05,0.000167,0.000158,0.000158,0.000157,2.9e-05,2.9e-05,2.9e-05,0.000158,2.9e-05,0.000162,2.9e-05,0.000157,0.000158,0.000154,0.000159,2.9e-05,2.9e-05,0.000158,0.000158,2.9e-05,2.9e-05,2.9e-05,0.000158,0.000158,0.000158,0.000158,0.000158,0.000158,0.000158,0.000158,0.000158,0.000158,0.000158,0.000158,0.000159,0.000158,0.000158,0.000158,0.000158,0.000157,0.000158,0.000157,2.9e-05,0.000158,0.000158,0.000158,0.000157,0.000157,0.000158,0.000157,0.000157,0.000157,0.000155,0.000157,0.000157,0.000157,0.000157,0.000157,0.000157,0.000158,0.000154,2.9e-05,0.000155,0.000155,0.000155,0.000155,0.000155,0.000154,0.000154,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000803,0.000758,0.000822,0.000772,0.000747,0.000746,0.000741,0.000749,0.000662,0.000726,0.000743,0.000741,0.000675,0.000764,0.000728,0.000733,0.000769,0.000756,0.000722,0.000732,0.000745,0.000662,0.000737,0.000629,0.000704,0.000716,0.000696,0.000745,0.000746,0.000736,0.000734,0.000727,0.000744,0.000688,0.000732,0.000661,0.000729,0.000697,0.00073,0.000749,0.000745,0.000731,0.000736,0.000729,0.000716,0.000739,0.000627,0.000726,0.000726,0.000726,0.000726,0.000726,2.9e-05,0.00075,0.000707,0.000742,0.00075,0.000627,0.000698,0.000755,0.00075,0.000637,0.000733,0.000743,0.00066,2.9e-05,0.000627,0.00073,0.000722,0.000724,0.000724,0.000728,0.000733,0.000734,0.000732,0.000726,0.000728,0.000747,0.000635,0.000635,0.000635,0.000635,0.00074,0.000709,0.000716,0.000728,0.000682,0.000704,0.000738,0.000727,0.000726,0.000672,0.000637,0.000718,0.000729,0.00066,0.000629,0.000661,0.000727,0.000743,0.000644,0.000739,0.000673,0.000673,0.000726,0.000733,0.000726,0.000673,0.000682,0.000732,0.000731,0.00073,0.000728,0.000733,0.000725,0.000728,0.000728,0.000729,0.000727,0.000729,0.000729,0.000739,0.000725,0.000724,0.000725,0.000732,0.000724,0.000725,0.000729,0.00072,0.000727,0.000728,0.000728,0.000724,0.000725,0.00073,0.000726,0.000726,0.000729,0.000731,0.000727,0.000727,0.000725,0.000726,0.000718,0.000739,0.000742,0.000727,0.000679,0.000706,0.000727,0.000706,0.000727,0.000706,0.000756,0.000709,0.000702,0.000724,0.000724,2.9e-05,2.9e-05,0.000627,0.000708,0.000708,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000692,0.000708,0.000627,2.9e-05,0.000716],[0.013442,0.013421,0.01348,0.013399,0.013404,0.0134,0.013385,0.013383,0.013276,0.013378,0.013403,0.013592,0.005348,0.013301,0.013388,0.013388,0.013405,0.01342,0.013305,0.013384,0.013401,0.013652,0.013589,0.013544,0.013202,0.013552,0.013469,0.01341,0.013398,0.013395,0.013582,0.013392,0.013403,0.013465,0.013577,0.013827,0.013571,0.013334,0.013326,0.013398,0.013771,0.013392,0.013387,0.01338,0.013552,0.013405,0.005376,0.013335,0.013335,0.013335,0.013335,0.013335,0.005348,0.013356,0.005348,0.013398,0.013394,0.005376,0.013481,0.013406,0.013555,0.013812,0.013392,0.013391,0.013454,0.005348,0.005376,0.013378,0.013305,0.013384,0.013386,0.013386,0.013388,0.013389,0.013391,0.013389,0.013391,0.013784,0.014091,0.014091,0.014091,0.014091,0.01339,0.013473,0.014214,0.013387,0.01364,0.005405,0.013389,0.013387,0.013389,0.013371,0.013812,0.013929,0.013391,0.013637,0.005405,0.013638,0.013387,0.013402,0.013199,0.013342,0.013505,0.013505,0.013391,0.01339,0.013572,0.013505,0.013514,0.013392,0.013392,0.013391,0.013393,0.01339,0.013387,0.013393,0.013391,0.013391,0.01339,0.013396,0.01339,0.01339,0.013388,0.013384,0.013388,0.013388,0.013386,0.013387,0.013388,0.013522,0.013383,0.013387,0.013386,0.013386,0.013387,0.013387,0.01339,0.013389,0.013393,0.013389,0.013393,0.013391,0.013389,0.01339,0.013375,0.013411,0.013399,0.013391,0.013375,0.013335,0.013384,0.013335,0.013384,0.013335,0.013379,0.013203,0.01352,0.013567,0.013564,0.005348,0.005348,0.005376,0.013473,0.013473,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.013432,0.013473,0.005376,0.005348,0.013552],[0.029899,0.029519,0.029985,0.02967,0.029472,0.029384,0.029486,0.029491,0.02881,0.028927,0.029393,0.005348,0.031509,0.029585,0.029338,0.029282,0.029741,0.029472,0.029305,0.029241,0.029416,0.005405,0.005348,0.005348,0.02919,0.005348,0.005348,0.02951,0.029519,0.029333,0.005348,0.028646,0.029416,0.005348,0.005348,0.005348,0.005348,0.028945,0.029393,0.029472,0.005348,0.029361,0.029361,0.029264,0.005348,0.028963,0.005348,0.029291,0.029291,0.029291,0.029291,0.029291,0.005348,0.029384,0.087902,0.02937,0.029444,0.005348,0.005348,0.029333,0.005348,0.005405,0.029426,0.029338,0.005348,0.005348,0.005348,0.028895,0.029117,0.029264,0.029264,0.029324,0.029407,0.029384,0.029347,0.029282,0.029338,0.005348,0.005464,0.005464,0.005464,0.005464,0.029338,0.005376,0.005464,0.029379,0.005376,0.030985,0.029472,0.029314,0.029268,0.005348,0.005405,0.005405,0.029365,0.005376,0.030205,0.005405,0.029282,0.029472,0.028677,0.029486,0.005348,0.005348,0.029324,0.02943,0.005348,0.005348,0.005348,0.029338,0.029342,0.029361,0.029333,0.029324,0.029333,0.029347,0.029375,0.029375,0.029342,0.029342,0.029393,0.029486,0.029301,0.029301,0.029301,0.029324,0.029264,0.02931,0.029264,0.005348,0.029319,0.029319,0.029356,0.029264,0.029264,0.029384,0.029278,0.029264,0.029264,0.028824,0.029268,0.029264,0.029273,0.029264,0.029264,0.029264,0.029319,0.02858,0.005405,0.028824,0.028824,0.028824,0.028824,0.028824,0.028633,0.028642,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.149278,0.141016,0.152961,0.143519,0.139013,0.138806,0.13788,0.139222,0.123179,0.134978,0.13829,0.13788,0.125591,0.142093,0.135371,0.136364,0.142967,0.140696,0.134296,0.136064,0.138599,0.123097,0.137067,0.116981,0.130986,0.133238,0.129526,0.138496,0.138702,0.136966,0.136564,0.135174,0.138393,0.128011,0.136064,0.122935,0.135667,0.129617,0.135766,0.139326,0.138496,0.135965,0.136865,0.135667,0.133238,0.137371,0.116614,0.135076,0.135076,0.135076,0.135076,0.135076,0.005348,0.139535,0.131449,0.138085,0.13943,0.116614,0.129888,0.140483,0.139535,0.118396,0.136364,0.138187,0.122691,0.005348,0.116614,0.135766,0.134296,0.134685,0.134685,0.135371,0.136264,0.136464,0.136164,0.135076,0.135371,0.13891,0.11817,0.11817,0.11817,0.11817,0.137676,0.131915,0.133142,0.135371,0.126789,0.130894,0.137269,0.135174,0.134978,0.124916,0.118396,0.133621,0.135667,0.122772,0.116908,0.122935,0.135174,0.13829,0.119845,0.137472,0.125253,0.125253,0.135076,0.136264,0.135076,0.125253,0.126789,0.136064,0.135965,0.135766,0.135371,0.136264,0.13488,0.13547,0.135371,0.135569,0.135174,0.135667,0.135667,0.137472,0.134783,0.134685,0.134783,0.136064,0.134685,0.13488,0.135569,0.133909,0.135174,0.135371,0.135371,0.134685,0.134783,0.135766,0.135076,0.134978,0.135667,0.135965,0.135273,0.135174,0.13488,0.135076,0.133525,0.137472,0.138085,0.135174,0.126273,0.131356,0.135174,0.131356,0.135174,0.131356,0.14059,0.131821,0.130618,0.134685,0.134588,0.005348,0.005348,0.116614,0.131728,0.131728,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.12872,0.131728,0.116614,0.005348,0.133238],[0.029899,0.029519,0.029985,0.02967,0.029472,0.029384,0.029486,0.029491,0.02881,0.028927,0.029393,0.005348,0.031509,0.029585,0.029338,0.029282,0.029741,0.029472,0.029305,0.029241,0.029416,0.005405,0.005348,0.005348,0.02919,0.005348,0.005348,0.02951,0.029519,0.029333,0.005348,0.028646,0.029416,0.005348,0.005348,0.005348,0.005348,0.028945,0.029393,0.029472,0.005348,0.029361,0.029361,0.029264,0.005348,0.028963,0.005348,0.029291,0.029291,0.029291,0.029291,0.029291,0.005348,0.029384,0.087902,0.02937,0.029444,0.005348,0.005348,0.029333,0.005348,0.005405,0.029426,0.029338,0.005348,0.005348,0.005348,0.028895,0.029117,0.029264,0.029264,0.029324,0.029407,0.029384,0.029347,0.029282,0.029338,0.005348,0.005464,0.005464,0.005464,0.005464,0.029338,0.005376,0.005464,0.029379,0.005376,0.030985,0.029472,0.029314,0.029268,0.005348,0.005405,0.005405,0.029365,0.005376,0.030205,0.005405,0.029282,0.029472,0.028677,0.029486,0.005348,0.005348,0.029324,0.02943,0.005348,0.005348,0.005348,0.029338,0.029342,0.029361,0.029333,0.029324,0.029333,0.029347,0.029375,0.029375,0.029342,0.029342,0.029393,0.029486,0.029301,0.029301,0.029301,0.029324,0.029264,0.02931,0.029264,0.005348,0.029319,0.029319,0.029356,0.029264,0.029264,0.029384,0.029278,0.029264,0.029264,0.028824,0.029268,0.029264,0.029273,0.029264,0.029264,0.029264,0.029319,0.02858,0.005405,0.028824,0.028824,0.028824,0.028824,0.028824,0.028633,0.028642,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var0_df_closseness, by=list(var0_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var0_df_closseness, by=list(var0_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-743f50118c3da57b1f3b" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-743f50118c3da57b1f3b">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.5e-05\" data-max=\"7.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6.9e-05\" data-max=\"0.000139\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.6e-05\" data-max=\"7.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000634\" data-max=\"0.000731\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.9e-05\" data-max=\"0.000164\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.010229\" data-max=\"0.013415\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7.8e-05\" data-max=\"0.004039\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.012804\" data-max=\"0.025756\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.008553\" data-max=\"0.013681\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.117863\" data-max=\"0.135935\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00358\" data-max=\"0.030556\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.012804\" data-max=\"0.025756\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.008553\" data-max=\"0.013681\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[7.2e-05,5.5e-05],[0,2.2e-05],[0.000139,6.9e-05],[4.6e-05,7.4e-05],[0.000731,0.000634],[1.9e-05,0.000164],[0.013415,0.010229],[7.8e-05,0.004039],[0.025756,0.012804],[0.008553,0.013681],[0.135935,0.117863],[0.00358,0.030556],[0.025756,0.012804],[0.008553,0.013681]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var0_df_closseness <- data.frame(
var0_incloseness,
var0_outcloseness,
var0_totalcloseness,
var0_incloseness_n,
var0_outcloseness_n,
var0_totalcloseness_n,
var0_centr_closeness) %>% round(6)

#Adding type
var0_df_closseness <-cbind(var0_df_closseness, V(var0)$TIPO2)

#Adding names
names(var0_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var0_df_closseness<-var0_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var0_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-2f32f6e7e9db0cd87e2e" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-2f32f6e7e9db0cd87e2e">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"7.6e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000473\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000822\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.014214\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.087902\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.152961\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.087902\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.1e-05,7.2e-05,7.2e-05,7.3e-05,2.9e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.3e-05,7.3e-05,7.1e-05,7.3e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.4e-05,7.3e-05,7.2e-05,7.2e-05,7.2e-05,7.4e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.2e-05,2.9e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,2.9e-05,7.2e-05,2.9e-05,7.2e-05,7.2e-05,2.9e-05,7.2e-05,7.2e-05,7.3e-05,7.4e-05,7.2e-05,7.2e-05,7.2e-05,2.9e-05,2.9e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.4e-05,7.6e-05,7.6e-05,7.6e-05,7.6e-05,7.2e-05,7.2e-05,7.6e-05,7.2e-05,7.3e-05,2.9e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.4e-05,7.5e-05,7.2e-05,7.3e-05,2.9e-05,7.3e-05,7.2e-05,7.2e-05,7.1e-05,7.2e-05,7.3e-05,7.3e-05,7.2e-05,7.2e-05,7.3e-05,7.3e-05,7.3e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.1e-05,7.3e-05,7.3e-05,7.3e-05,2.9e-05,2.9e-05,2.9e-05,7.2e-05,7.2e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,7.2e-05,7.2e-05,2.9e-05,2.9e-05,7.3e-05],[0.000161,0.000159,0.000161,0.00016,0.000158,0.000158,0.000159,0.000159,0.000155,0.000156,0.000158,2.9e-05,0.000169,0.000159,0.000158,0.000157,0.00016,0.000158,0.000158,0.000157,0.000158,2.9e-05,2.9e-05,2.9e-05,0.000157,2.9e-05,2.9e-05,0.000159,0.000159,0.000158,2.9e-05,0.000154,0.000158,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000156,0.000158,0.000158,2.9e-05,0.000158,0.000158,0.000157,2.9e-05,0.000156,2.9e-05,0.000157,0.000157,0.000157,0.000157,0.000157,2.9e-05,0.000158,0.000473,0.000158,0.000158,2.9e-05,2.9e-05,0.000158,2.9e-05,2.9e-05,0.000158,0.000158,2.9e-05,2.9e-05,2.9e-05,0.000155,0.000157,0.000157,0.000157,0.000158,0.000158,0.000158,0.000158,0.000157,0.000158,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000158,2.9e-05,2.9e-05,0.000158,2.9e-05,0.000167,0.000158,0.000158,0.000157,2.9e-05,2.9e-05,2.9e-05,0.000158,2.9e-05,0.000162,2.9e-05,0.000157,0.000158,0.000154,0.000159,2.9e-05,2.9e-05,0.000158,0.000158,2.9e-05,2.9e-05,2.9e-05,0.000158,0.000158,0.000158,0.000158,0.000158,0.000158,0.000158,0.000158,0.000158,0.000158,0.000158,0.000158,0.000159,0.000158,0.000158,0.000158,0.000158,0.000157,0.000158,0.000157,2.9e-05,0.000158,0.000158,0.000158,0.000157,0.000157,0.000158,0.000157,0.000157,0.000157,0.000155,0.000157,0.000157,0.000157,0.000157,0.000157,0.000157,0.000158,0.000154,2.9e-05,0.000155,0.000155,0.000155,0.000155,0.000155,0.000154,0.000154,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000803,0.000758,0.000822,0.000772,0.000747,0.000746,0.000741,0.000749,0.000662,0.000726,0.000743,0.000741,0.000675,0.000764,0.000728,0.000733,0.000769,0.000756,0.000722,0.000732,0.000745,0.000662,0.000737,0.000629,0.000704,0.000716,0.000696,0.000745,0.000746,0.000736,0.000734,0.000727,0.000744,0.000688,0.000732,0.000661,0.000729,0.000697,0.00073,0.000749,0.000745,0.000731,0.000736,0.000729,0.000716,0.000739,0.000627,0.000726,0.000726,0.000726,0.000726,0.000726,2.9e-05,0.00075,0.000707,0.000742,0.00075,0.000627,0.000698,0.000755,0.00075,0.000637,0.000733,0.000743,0.00066,2.9e-05,0.000627,0.00073,0.000722,0.000724,0.000724,0.000728,0.000733,0.000734,0.000732,0.000726,0.000728,0.000747,0.000635,0.000635,0.000635,0.000635,0.00074,0.000709,0.000716,0.000728,0.000682,0.000704,0.000738,0.000727,0.000726,0.000672,0.000637,0.000718,0.000729,0.00066,0.000629,0.000661,0.000727,0.000743,0.000644,0.000739,0.000673,0.000673,0.000726,0.000733,0.000726,0.000673,0.000682,0.000732,0.000731,0.00073,0.000728,0.000733,0.000725,0.000728,0.000728,0.000729,0.000727,0.000729,0.000729,0.000739,0.000725,0.000724,0.000725,0.000732,0.000724,0.000725,0.000729,0.00072,0.000727,0.000728,0.000728,0.000724,0.000725,0.00073,0.000726,0.000726,0.000729,0.000731,0.000727,0.000727,0.000725,0.000726,0.000718,0.000739,0.000742,0.000727,0.000679,0.000706,0.000727,0.000706,0.000727,0.000706,0.000756,0.000709,0.000702,0.000724,0.000724,2.9e-05,2.9e-05,0.000627,0.000708,0.000708,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000627,0.000692,0.000708,0.000627,2.9e-05,0.000716],[0.013442,0.013421,0.01348,0.013399,0.013404,0.0134,0.013385,0.013383,0.013276,0.013378,0.013403,0.013592,0.005348,0.013301,0.013388,0.013388,0.013405,0.01342,0.013305,0.013384,0.013401,0.013652,0.013589,0.013544,0.013202,0.013552,0.013469,0.01341,0.013398,0.013395,0.013582,0.013392,0.013403,0.013465,0.013577,0.013827,0.013571,0.013334,0.013326,0.013398,0.013771,0.013392,0.013387,0.01338,0.013552,0.013405,0.005376,0.013335,0.013335,0.013335,0.013335,0.013335,0.005348,0.013356,0.005348,0.013398,0.013394,0.005376,0.013481,0.013406,0.013555,0.013812,0.013392,0.013391,0.013454,0.005348,0.005376,0.013378,0.013305,0.013384,0.013386,0.013386,0.013388,0.013389,0.013391,0.013389,0.013391,0.013784,0.014091,0.014091,0.014091,0.014091,0.01339,0.013473,0.014214,0.013387,0.01364,0.005405,0.013389,0.013387,0.013389,0.013371,0.013812,0.013929,0.013391,0.013637,0.005405,0.013638,0.013387,0.013402,0.013199,0.013342,0.013505,0.013505,0.013391,0.01339,0.013572,0.013505,0.013514,0.013392,0.013392,0.013391,0.013393,0.01339,0.013387,0.013393,0.013391,0.013391,0.01339,0.013396,0.01339,0.01339,0.013388,0.013384,0.013388,0.013388,0.013386,0.013387,0.013388,0.013522,0.013383,0.013387,0.013386,0.013386,0.013387,0.013387,0.01339,0.013389,0.013393,0.013389,0.013393,0.013391,0.013389,0.01339,0.013375,0.013411,0.013399,0.013391,0.013375,0.013335,0.013384,0.013335,0.013384,0.013335,0.013379,0.013203,0.01352,0.013567,0.013564,0.005348,0.005348,0.005376,0.013473,0.013473,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.013432,0.013473,0.005376,0.005348,0.013552],[0.029899,0.029519,0.029985,0.02967,0.029472,0.029384,0.029486,0.029491,0.02881,0.028927,0.029393,0.005348,0.031509,0.029585,0.029338,0.029282,0.029741,0.029472,0.029305,0.029241,0.029416,0.005405,0.005348,0.005348,0.02919,0.005348,0.005348,0.02951,0.029519,0.029333,0.005348,0.028646,0.029416,0.005348,0.005348,0.005348,0.005348,0.028945,0.029393,0.029472,0.005348,0.029361,0.029361,0.029264,0.005348,0.028963,0.005348,0.029291,0.029291,0.029291,0.029291,0.029291,0.005348,0.029384,0.087902,0.02937,0.029444,0.005348,0.005348,0.029333,0.005348,0.005405,0.029426,0.029338,0.005348,0.005348,0.005348,0.028895,0.029117,0.029264,0.029264,0.029324,0.029407,0.029384,0.029347,0.029282,0.029338,0.005348,0.005464,0.005464,0.005464,0.005464,0.029338,0.005376,0.005464,0.029379,0.005376,0.030985,0.029472,0.029314,0.029268,0.005348,0.005405,0.005405,0.029365,0.005376,0.030205,0.005405,0.029282,0.029472,0.028677,0.029486,0.005348,0.005348,0.029324,0.02943,0.005348,0.005348,0.005348,0.029338,0.029342,0.029361,0.029333,0.029324,0.029333,0.029347,0.029375,0.029375,0.029342,0.029342,0.029393,0.029486,0.029301,0.029301,0.029301,0.029324,0.029264,0.02931,0.029264,0.005348,0.029319,0.029319,0.029356,0.029264,0.029264,0.029384,0.029278,0.029264,0.029264,0.028824,0.029268,0.029264,0.029273,0.029264,0.029264,0.029264,0.029319,0.02858,0.005405,0.028824,0.028824,0.028824,0.028824,0.028824,0.028633,0.028642,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.149278,0.141016,0.152961,0.143519,0.139013,0.138806,0.13788,0.139222,0.123179,0.134978,0.13829,0.13788,0.125591,0.142093,0.135371,0.136364,0.142967,0.140696,0.134296,0.136064,0.138599,0.123097,0.137067,0.116981,0.130986,0.133238,0.129526,0.138496,0.138702,0.136966,0.136564,0.135174,0.138393,0.128011,0.136064,0.122935,0.135667,0.129617,0.135766,0.139326,0.138496,0.135965,0.136865,0.135667,0.133238,0.137371,0.116614,0.135076,0.135076,0.135076,0.135076,0.135076,0.005348,0.139535,0.131449,0.138085,0.13943,0.116614,0.129888,0.140483,0.139535,0.118396,0.136364,0.138187,0.122691,0.005348,0.116614,0.135766,0.134296,0.134685,0.134685,0.135371,0.136264,0.136464,0.136164,0.135076,0.135371,0.13891,0.11817,0.11817,0.11817,0.11817,0.137676,0.131915,0.133142,0.135371,0.126789,0.130894,0.137269,0.135174,0.134978,0.124916,0.118396,0.133621,0.135667,0.122772,0.116908,0.122935,0.135174,0.13829,0.119845,0.137472,0.125253,0.125253,0.135076,0.136264,0.135076,0.125253,0.126789,0.136064,0.135965,0.135766,0.135371,0.136264,0.13488,0.13547,0.135371,0.135569,0.135174,0.135667,0.135667,0.137472,0.134783,0.134685,0.134783,0.136064,0.134685,0.13488,0.135569,0.133909,0.135174,0.135371,0.135371,0.134685,0.134783,0.135766,0.135076,0.134978,0.135667,0.135965,0.135273,0.135174,0.13488,0.135076,0.133525,0.137472,0.138085,0.135174,0.126273,0.131356,0.135174,0.131356,0.135174,0.131356,0.14059,0.131821,0.130618,0.134685,0.134588,0.005348,0.005348,0.116614,0.131728,0.131728,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.116614,0.12872,0.131728,0.116614,0.005348,0.133238],[0.029899,0.029519,0.029985,0.02967,0.029472,0.029384,0.029486,0.029491,0.02881,0.028927,0.029393,0.005348,0.031509,0.029585,0.029338,0.029282,0.029741,0.029472,0.029305,0.029241,0.029416,0.005405,0.005348,0.005348,0.02919,0.005348,0.005348,0.02951,0.029519,0.029333,0.005348,0.028646,0.029416,0.005348,0.005348,0.005348,0.005348,0.028945,0.029393,0.029472,0.005348,0.029361,0.029361,0.029264,0.005348,0.028963,0.005348,0.029291,0.029291,0.029291,0.029291,0.029291,0.005348,0.029384,0.087902,0.02937,0.029444,0.005348,0.005348,0.029333,0.005348,0.005405,0.029426,0.029338,0.005348,0.005348,0.005348,0.028895,0.029117,0.029264,0.029264,0.029324,0.029407,0.029384,0.029347,0.029282,0.029338,0.005348,0.005464,0.005464,0.005464,0.005464,0.029338,0.005376,0.005464,0.029379,0.005376,0.030985,0.029472,0.029314,0.029268,0.005348,0.005405,0.005405,0.029365,0.005376,0.030205,0.005405,0.029282,0.029472,0.028677,0.029486,0.005348,0.005348,0.029324,0.02943,0.005348,0.005348,0.005348,0.029338,0.029342,0.029361,0.029333,0.029324,0.029333,0.029347,0.029375,0.029375,0.029342,0.029342,0.029393,0.029486,0.029301,0.029301,0.029301,0.029324,0.029264,0.02931,0.029264,0.005348,0.029319,0.029319,0.029356,0.029264,0.029264,0.029384,0.029278,0.029264,0.029264,0.028824,0.029268,0.029264,0.029273,0.029264,0.029264,0.029264,0.029319,0.02858,0.005405,0.028824,0.028824,0.028824,0.028824,0.028824,0.028633,0.028642,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var0_df_closseness, by=list(var0_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var0_df_closseness, by=list(var0_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-339f91d086bf6c3d5266" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-339f91d086bf6c3d5266">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.5e-05\" data-max=\"7.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6.8e-05\" data-max=\"0.000142\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.2e-05\" data-max=\"7.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000632\" data-max=\"0.000746\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.1e-05\" data-max=\"0.000165\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.010189\" data-max=\"0.013488\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.5e-05\" data-max=\"0.004049\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.01259\" data-max=\"0.026491\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.007741\" data-max=\"0.013637\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.117585\" data-max=\"0.138668\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.002056\" data-max=\"0.030654\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.01259\" data-max=\"0.026491\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.007741\" data-max=\"0.013637\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[7.2e-05,7.2e-05,5.5e-05],[1e-06,0,2.2e-05],[0.000115,0.000142,6.8e-05],[6.3e-05,4.2e-05,7.3e-05],[0.000746,0.000729,0.000632],[1.1e-05,1.9e-05,0.000165],[0.013488,0.013403,0.010189],[0.000139,5.5e-05,0.004049],[0.0214,0.026491,0.01259],[0.01175,0.007741,0.013637],[0.138668,0.135537,0.117585],[0.002056,0.003584,0.030654],[0.0214,0.026491,0.01259],[0.01175,0.007741,0.013637]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var0_data.RData")
```

