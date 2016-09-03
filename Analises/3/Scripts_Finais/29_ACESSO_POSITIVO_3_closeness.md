# SNA Closeness 29_ACESSO_POSITIVO
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 29_ACESSO_POSITIVO

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/acesso_positivo_data.RData")
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
#acesso_positivo<-simplify(acesso_positivo) #Simplify
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
V(acesso_positivo)$incloseness <- closeness(acesso_positivo, mode = "in", weights = E(acesso_positivo)$acesso_positivo) %>% round(6)
V(acesso_positivo)$outcloseness <- closeness(acesso_positivo, mode = "out", weights = E(acesso_positivo)$acesso_positivo) %>% round(6)
V(acesso_positivo)$totalcloseness <- closeness(acesso_positivo, mode = "total", weights = E(acesso_positivo)$acesso_positivo) %>% round(4)
```

###Saving to Environment

```r
acesso_positivo_incloseness<- closeness(acesso_positivo, mode = "in", weights = E(acesso_positivo)$acesso_positivo) %>% round(6)
acesso_positivo_outcloseness<- closeness(acesso_positivo, mode = "out", weights = E(acesso_positivo)$acesso_positivo) %>% round(6)
acesso_positivo_totalcloseness<- closeness(acesso_positivo, mode = "total", weights = E(acesso_positivo)$acesso_positivo) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(acesso_positivo_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 2.900e-05 7.500e-05 8.200e-05 7.816e-05 8.300e-05 9.900e-05
```

```r
sd(acesso_positivo_incloseness)
```

```
## [1] 8.474028e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(acesso_positivo)$incloseness<-closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="in")

#Get Variable
V(acesso_positivo)$acesso_positivo_color_degree<-round(V(acesso_positivo)$incloseness,6)

#Creating brewer pallette
vertex_acesso_positivo_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(acesso_positivo)$acesso_positivo_color_degree)), "RdBu"))(
            length(unique(V(acesso_positivo)$acesso_positivo_color_degree)))

#Saving as Vertex properties 
V(acesso_positivo)$vertex_acesso_positivo_color_degree<-
  vertex_acesso_positivo_color_degree[as.numeric(
  cut(V(acesso_positivo)$acesso_positivo_color_degree,
      breaks=length(unique(V(acesso_positivo)$acesso_positivo_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(acesso_positivo, es=E(acesso_positivo), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acesso_positivo))
maxC <- rep(Inf, vcount(acesso_positivo))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acesso_positivo, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acesso_positivo)$weight)


#PLotting
plot(acesso_positivo, 
     layout=co,
     edge.color=V(acesso_positivo)$vertex_acesso_positivo_color_degree[edge.start],
     edge.arrow.size=closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="in"),
     edge.width=E(acesso_positivo)$weight/mean(E(acesso_positivo)$weight),
     edge.curved = TRUE,
     vertex.color=V(acesso_positivo)$vertex_acesso_positivo_color_degree,
     vertex.size=closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(acesso_positivo,"LABEL_COR"),
     vertex.label.cex=(closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(acesso_positivo)$acesso_positivo_color_degree
b<-V(acesso_positivo)$vertex_acesso_positivo_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 29_ACESSO_POSITIVO", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(acesso_positivo, mode="in", weights = E(acesso_positivo)$acesso_positivo)), 
             sd(closeness(acesso_positivo, mode="in", weights = E(acesso_positivo)$acesso_positivo))
             )
       )
```

![](29_ACESSO_POSITIVO_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(acesso_positivo_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0000960 0.0001310 0.0001159 0.0001490 0.0001980
```

```r
sd(acesso_positivo_outcloseness)
```

```
## [1] 4.851395e-05
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(acesso_positivo)$outcloseness<-closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="out")

#Get Variable
V(acesso_positivo)$acesso_positivo_color_degree<-round(V(acesso_positivo)$outcloseness,6)

#Creating brewer pallette
vertex_acesso_positivo_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(acesso_positivo)$acesso_positivo_color_degree)), "RdBu"))(
            length(unique(V(acesso_positivo)$acesso_positivo_color_degree)))

#Saving as Vertex properties 
V(acesso_positivo)$vertex_acesso_positivo_color_degree<-
  vertex_acesso_positivo_color_degree[as.numeric(
  cut(V(acesso_positivo)$acesso_positivo_color_degree,
      breaks=length(unique(V(acesso_positivo)$acesso_positivo_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(acesso_positivo, es=E(acesso_positivo), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acesso_positivo))
maxC <- rep(Inf, vcount(acesso_positivo))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acesso_positivo, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acesso_positivo)$weight)


#PLotting
plot(acesso_positivo, 
     layout=co,
     edge.color=V(acesso_positivo)$vertex_acesso_positivo_color_degree[edge.start],
     edge.arrow.size=closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="out"),
     edge.width=E(acesso_positivo)$weight/2*mean(E(acesso_positivo)$weight),
     edge.curved = TRUE,
     vertex.color=V(acesso_positivo)$vertex_acesso_positivo_color_degree,
     vertex.size=closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(acesso_positivo,"LABEL_COR"),
     vertex.label.cex=closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(acesso_positivo)$acesso_positivo_color_degree
b<-V(acesso_positivo)$vertex_acesso_positivo_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 29_ACESSO_POSITIVO", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(acesso_positivo, mode="out", weights = E(acesso_positivo)$acesso_positivo)), 
             sd(closeness(acesso_positivo, mode="out", weights = E(acesso_positivo)$acesso_positivo))
             )
       )
```

![](29_ACESSO_POSITIVO_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(acesso_positivo_totalcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000970 0.0001520 0.0001670 0.0001679 0.0001870 0.0002620
```

```r
sd(acesso_positivo_totalcloseness)
```

```
## [1] 2.895156e-05
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(acesso_positivo)$allcloseness<-closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="all")

#Get Variable
V(acesso_positivo)$acesso_positivo_color_degree<-round(V(acesso_positivo)$allcloseness,6)

#Creating brewer pallette
vertex_acesso_positivo_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(acesso_positivo)$acesso_positivo_color_degree)), "RdBu"))(
            length(unique(V(acesso_positivo)$acesso_positivo_color_degree)))

#Saving as Vertex properties 
V(acesso_positivo)$vertex_acesso_positivo_color_degree<-
  vertex_acesso_positivo_color_degree[as.numeric(
  cut(V(acesso_positivo)$acesso_positivo_color_degree,
      breaks=length(unique(V(acesso_positivo)$acesso_positivo_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(acesso_positivo, es=E(acesso_positivo), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acesso_positivo))
maxC <- rep(Inf, vcount(acesso_positivo))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acesso_positivo, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acesso_positivo)$weight)


#PLotting
plot(acesso_positivo, 
     layout=co,
     edge.color=V(acesso_positivo)$vertex_acesso_positivo_color_degree[edge.start],
     edge.arrow.size=closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="all"),
     edge.width=E(acesso_positivo)$weight/2*mean(E(acesso_positivo)$weight),
     edge.curved = TRUE,
     vertex.color=V(acesso_positivo)$vertex_acesso_positivo_color_degree,
     vertex.size=closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(acesso_positivo,"LABEL_COR"),
     vertex.label.cex=(closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(acesso_positivo)$acesso_positivo_color_degree
b<-V(acesso_positivo)$vertex_acesso_positivo_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 29_ACESSO_POSITIVO", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(acesso_positivo, mode="all", weights = E(acesso_positivo)$acesso_positivo)), 
             sd(closeness(acesso_positivo, mode="all", weights = E(acesso_positivo)$acesso_positivo))
             )
       )
```

![](29_ACESSO_POSITIVO_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(acesso_positivo)$incloseness_n <- closeness(acesso_positivo, mode = "in",, weights = E(acesso_positivo)$acesso_positivo, normalized = T) %>% round(10)
V(acesso_positivo)$outcloseness_n <- closeness(acesso_positivo, mode = "out", normalized = T, weights = E(acesso_positivo)$acesso_positivo) %>% round(6)
V(acesso_positivo)$totalcloseness_n <- closeness(acesso_positivo, mode = "total", normalized = T, weights = E(acesso_positivo)$acesso_positivo) %>% round(6)
```

###Saving to Environment

```r
acesso_positivo_incloseness_n<- closeness(acesso_positivo, mode = "in", normalized = T, weights = E(acesso_positivo)$acesso_positivo) %>% round(6)
acesso_positivo_outcloseness_n<- closeness(acesso_positivo, mode = "out", normalized = T, weights = E(acesso_positivo)$acesso_positivo) %>% round(6)
acesso_positivo_totalcloseness_n<- closeness(acesso_positivo, mode = "total", normalized = T, weights = E(acesso_positivo)$acesso_positivo) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(acesso_positivo_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.013920 0.015200 0.014540 0.015500 0.018380
```

```r
sd(acesso_positivo_incloseness_n)
```

```
## [1] 0.001570769
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(acesso_positivo)$incloseness_n<-closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="in", normalized = T)

#Get Variable
V(acesso_positivo)$acesso_positivo_color_degree<-round(V(acesso_positivo)$incloseness_n,6)

#Creating brewer pallette
vertex_acesso_positivo_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(acesso_positivo)$acesso_positivo_color_degree)), "RdBu"))(
            length(unique(V(acesso_positivo)$acesso_positivo_color_degree)))

#Saving as Vertex properties 
V(acesso_positivo)$vertex_acesso_positivo_color_degree<-
  vertex_acesso_positivo_color_degree[as.numeric(
  cut(V(acesso_positivo)$acesso_positivo_color_degree,
      breaks=length(unique(V(acesso_positivo)$acesso_positivo_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(acesso_positivo, es=E(acesso_positivo), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acesso_positivo))
maxC <- rep(Inf, vcount(acesso_positivo))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acesso_positivo, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acesso_positivo)$weight)


#PLotting
plot(acesso_positivo, 
     layout=co,
     edge.color=V(acesso_positivo)$vertex_acesso_positivo_color_degree[edge.start],
     edge.arrow.size=closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="in",normalized = T),
     edge.width=E(acesso_positivo)$weight/10*mean(E(acesso_positivo)$weight),
     edge.curved = TRUE,
     vertex.color=V(acesso_positivo)$vertex_acesso_positivo_color_degree,
     vertex.size=(closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(acesso_positivo,"LABEL_COR"),
     vertex.label.cex=closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(acesso_positivo)$acesso_positivo_color_degree
b<-V(acesso_positivo)$vertex_acesso_positivo_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 29_ACESSO_POSITIVO", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(acesso_positivo, mode="in", weights = E(acesso_positivo)$acesso_positivo, normalized = T)), 
             sd(closeness(acesso_positivo, mode="in", weights = E(acesso_positivo)$acesso_positivo, normalized = T))
             )
       )
```

![](29_ACESSO_POSITIVO_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(acesso_positivo_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.017860 0.024440 0.021560 0.027700 0.036830
```

```r
sd(acesso_positivo_outcloseness_n)
```

```
## [1] 0.009044043
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(acesso_positivo)$outcloseness_n<-closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="out", normalized = T)

#Get Variable
V(acesso_positivo)$acesso_positivo_color_degree<-round(V(acesso_positivo)$outcloseness_n,6)

#Creating brewer pallette
vertex_acesso_positivo_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(acesso_positivo)$acesso_positivo_color_degree)), "RdBu"))(
            length(unique(V(acesso_positivo)$acesso_positivo_color_degree)))

#Saving as Vertex properties 
V(acesso_positivo)$vertex_acesso_positivo_color_degree<-
  vertex_acesso_positivo_color_degree[as.numeric(
  cut(V(acesso_positivo)$acesso_positivo_color_degree,
      breaks=length(unique(V(acesso_positivo)$acesso_positivo_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(acesso_positivo, es=E(acesso_positivo), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acesso_positivo))
maxC <- rep(Inf, vcount(acesso_positivo))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acesso_positivo, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acesso_positivo)$weight)


#PLotting
plot(acesso_positivo, 
     layout=co,
     edge.color=V(acesso_positivo)$vertex_acesso_positivo_color_degree[edge.start],
     edge.arrow.size=closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="out",normalized = T),
     edge.width=E(acesso_positivo)$weight/10*mean(E(acesso_positivo)$weight),
     edge.curved = TRUE,
     vertex.color=V(acesso_positivo)$vertex_acesso_positivo_color_degree,
     vertex.size=(closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(acesso_positivo,"LABEL_COR"),
     vertex.label.cex=closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(acesso_positivo)$acesso_positivo_color_degree
b<-V(acesso_positivo)$vertex_acesso_positivo_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 29_ACESSO_POSITIVO", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(acesso_positivo, mode="out", weights = E(acesso_positivo)$acesso_positivo, normalized = T)), 
             sd(closeness(acesso_positivo, mode="out", weights = E(acesso_positivo)$acesso_positivo, normalized = T))
             )
       )
```

![](29_ACESSO_POSITIVO_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(acesso_positivo_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.01811 0.02828 0.03107 0.03122 0.03474 0.04873
```

```r
sd(acesso_positivo_totalcloseness_n)
```

```
## [1] 0.005382911
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(acesso_positivo)$allcloseness_n<-closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="all", normalized = T)

#Get Variable
V(acesso_positivo)$acesso_positivo_color_degree<-round(V(acesso_positivo)$allcloseness_n,6)

#Creating brewer pallette
vertex_acesso_positivo_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(acesso_positivo)$acesso_positivo_color_degree)), "RdBu"))(
            length(unique(V(acesso_positivo)$acesso_positivo_color_degree)))

#Saving as Vertex properties 
V(acesso_positivo)$vertex_acesso_positivo_color_degree<-
  vertex_acesso_positivo_color_degree[as.numeric(
  cut(V(acesso_positivo)$acesso_positivo_color_degree,
      breaks=length(unique(V(acesso_positivo)$acesso_positivo_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(acesso_positivo, es=E(acesso_positivo), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acesso_positivo))
maxC <- rep(Inf, vcount(acesso_positivo))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acesso_positivo, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acesso_positivo)$weight)


#PLotting
plot(acesso_positivo, 
     layout=co,
     edge.color=V(acesso_positivo)$vertex_acesso_positivo_color_degree[edge.start],
     edge.arrow.size=closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="all",normalized = T),
     edge.width=E(acesso_positivo)$weight/10*mean(E(acesso_positivo)$weight),
     edge.curved = TRUE,
     vertex.color=V(acesso_positivo)$vertex_acesso_positivo_color_degree,
     vertex.size=(closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(acesso_positivo,"LABEL_COR"),
     vertex.label.cex=closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(acesso_positivo)$acesso_positivo_color_degree
b<-V(acesso_positivo)$vertex_acesso_positivo_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 29_ACESSO_POSITIVO", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(acesso_positivo, mode="all", weights = E(acesso_positivo)$acesso_positivo, normalized = T)), 
             sd(closeness(acesso_positivo, mode="all", weights = E(acesso_positivo)$acesso_positivo, normalized = T))
             )
       )
```

![](29_ACESSO_POSITIVO_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(acesso_positivo)$incloseness_n <- closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode = "in", normalized = T) %>% round(6)
V(acesso_positivo)$outcloseness_n <- closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode = "out", normalized = T) %>% round(6)
V(acesso_positivo)$totalcloseness_n <- closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(acesso_positivo)$acesso_positivo_centr_closeness<- centralization.closeness(acesso_positivo)$res
acesso_positivo_centr_closeness<- centralization.closeness(acesso_positivo)$res
acesso_positivo_centr_closeness_all<- centralization.closeness(acesso_positivo)
```

###Centralization

```r
acesso_positivo_centr_closeness_all$centralization
```

```
## [1] 0.1627615
```

###Theoretical Max

```r
acesso_positivo_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(acesso_positivo)$acesso_positivo_centr_closeness<- centralization.closeness(acesso_positivo)$res

#Get Variable
V(acesso_positivo)$acesso_positivo_color_degree<-round(V(acesso_positivo)$acesso_positivo_centr_closeness,6)

#Creating brewer pallette
vertex_acesso_positivo_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(acesso_positivo)$acesso_positivo_color_degree)), "Spectral"))(
            length(unique(V(acesso_positivo)$acesso_positivo_color_degree)))

#Saving as Vertex properties 
V(acesso_positivo)$vertex_acesso_positivo_color_degree<-
  vertex_acesso_positivo_color_degree[as.numeric(
  cut(V(acesso_positivo)$acesso_positivo_color_degree,
      breaks=length(unique(V(acesso_positivo)$acesso_positivo_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(acesso_positivo, es=E(acesso_positivo), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acesso_positivo))
maxC <- rep(Inf, vcount(acesso_positivo))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acesso_positivo, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acesso_positivo)$weight)


#PLotting
plot(acesso_positivo, 
     layout=co,
     edge.color=V(acesso_positivo)$vertex_acesso_positivo_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(acesso_positivo)$res,
     edge.width=E(acesso_positivo)$weight/10*mean(E(acesso_positivo)$weight),
     edge.curved = TRUE,
     vertex.color=V(acesso_positivo)$vertex_acesso_positivo_color_degree,
     vertex.size=centralization.closeness(acesso_positivo)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(acesso_positivo,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(acesso_positivo)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(acesso_positivo)$acesso_positivo_color_degree
b<-V(acesso_positivo)$vertex_acesso_positivo_color_degree
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
  title("Network Centralization Closeness - 29_ACESSO_POSITIVO", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(acesso_positivo)$res), 
             sd(centralization.closeness(acesso_positivo)$res)
             )
       )
```

![](29_ACESSO_POSITIVO_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
acesso_positivo_incloseness<- closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode = "in") %>% round(6)
acesso_positivo_outcloseness<- closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode = "out") %>% round(6)
acesso_positivo_totalcloseness<- closeness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo, mode = "total") %>% round(6)
acesso_positivo_incloseness_n<- closeness(acesso_positivo,weights = E(acesso_positivo)$acesso_positivo, mode = "in", normalized = T) %>% round(6)
acesso_positivo_outcloseness_n<- closeness(acesso_positivo,weights = E(acesso_positivo)$acesso_positivo, mode = "out", normalized = T) %>% round(6)
acesso_positivo_totalcloseness_n<- closeness(acesso_positivo,weights = E(acesso_positivo)$acesso_positivo, mode = "total", normalized = T) %>% round(6)
acesso_positivo_centr_closeness <- centralization.closeness(acesso_positivo)$res %>% round(6)
```

##Creating a datagrame of measures

```r
acesso_positivo_df_closseness <- data.frame(
acesso_positivo_incloseness,
acesso_positivo_outcloseness,
acesso_positivo_totalcloseness,
acesso_positivo_incloseness_n,
acesso_positivo_outcloseness_n,
acesso_positivo_totalcloseness_n,
acesso_positivo_centr_closeness) %>% round(6)

#Adding type
acesso_positivo_df_closseness <-cbind(acesso_positivo_df_closseness, V(acesso_positivo)$LABEL_COR)

#Adding names
names(acesso_positivo_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
acesso_positivo_df_closseness<-acesso_positivo_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(acesso_positivo_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-26ad6d1144e06cd26706" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-26ad6d1144e06cd26706">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"9.9e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000198\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"9.7e-05\" data-max=\"0.000262\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.018383\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.036832\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.018111\" data-max=\"0.048729\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[9.4e-05,8.7e-05,9.9e-05,8.4e-05,8.6e-05,8.4e-05,8e-05,8.4e-05,7.7e-05,8.1e-05,8.7e-05,8.5e-05,7.2e-05,8.3e-05,8.2e-05,7.9e-05,8.5e-05,9.1e-05,7.2e-05,8.4e-05,8.5e-05,7.7e-05,8.5e-05,6.5e-05,8.2e-05,8e-05,7e-05,8.5e-05,8.3e-05,8.6e-05,8.6e-05,8.2e-05,8.6e-05,6.9e-05,8.3e-05,7.8e-05,8.3e-05,8.4e-05,7.3e-05,8.4e-05,8.6e-05,8.1e-05,8.1e-05,8.4e-05,8e-05,8.4e-05,6e-05,7.5e-05,7.5e-05,7.5e-05,7.5e-05,7.5e-05,6.6e-05,8.4e-05,6.8e-05,8.3e-05,8.6e-05,7.3e-05,7e-05,8.9e-05,8.3e-05,6.4e-05,8.2e-05,8.2e-05,7.8e-05,2.9e-05,5.8e-05,8.1e-05,8.6e-05,8.1e-05,8.4e-05,8.3e-05,8.3e-05,8.3e-05,8.4e-05,8.2e-05,8.2e-05,8.7e-05,6.2e-05,6.2e-05,6.2e-05,6.2e-05,8.2e-05,7.9e-05,7.7e-05,8.4e-05,8.1e-05,6.1e-05,8.2e-05,8.1e-05,8.2e-05,6.6e-05,6.7e-05,7.9e-05,8.6e-05,7.8e-05,6.1e-05,7.6e-05,8.4e-05,8.6e-05,6.6e-05,7.5e-05,7.3e-05,7.3e-05,8.2e-05,8.2e-05,8.2e-05,7.3e-05,7.4e-05,8.2e-05,8.2e-05,8.4e-05,8.3e-05,8.3e-05,8.3e-05,8.2e-05,8.2e-05,8.2e-05,8.2e-05,8.6e-05,8.2e-05,8.5e-05,8.3e-05,8.2e-05,8.3e-05,8.3e-05,8.3e-05,8.3e-05,8.3e-05,7.9e-05,8.2e-05,8.3e-05,8.3e-05,8.3e-05,8.3e-05,8.4e-05,8.6e-05,8.2e-05,8.3e-05,8.2e-05,8.2e-05,8.2e-05,8.2e-05,8.2e-05,7.8e-05,8.7e-05,8.2e-05,8.7e-05,7.6e-05,7.9e-05,8.2e-05,7.9e-05,8.2e-05,7.9e-05,8.2e-05,6.7e-05,8.1e-05,8.4e-05,8.3e-05,6.2e-05,6.3e-05,7.6e-05,7.5e-05,7.4e-05,6.1e-05,5.8e-05,7.3e-05,6.1e-05,6.1e-05,7.5e-05,6.9e-05,6.1e-05,7.3e-05,7.5e-05,7.7e-05,7.8e-05,7.6e-05,6.1e-05,7.6e-05,6.9e-05,7.4e-05,7.4e-05,7.5e-05,7.6e-05,6.8e-05,6.8e-05,7.8e-05],[0.000198,0.000178,0.000193,0.000176,0.000143,0.000147,0.000135,0.00019,0.000144,0.00013,0.00013,0.000131,0.000171,0.000142,0.000148,0.000137,0.000173,0.00018,0.00013,0.000143,0.000141,0.000145,0.000154,0.000155,0.000141,0.000109,0.000134,0.000155,0.000181,0.000156,0.000166,0.000143,0.000177,9.3e-05,0.000132,0.000144,0.000139,0.000131,0.000139,0.000168,0.000167,0.000169,0.000169,0.000155,0.000145,0.000154,0.00011,0.000138,0.000138,0.000138,0.000138,0.000138,0.000135,0.000129,0.000159,0.00016,0.000149,0.000148,0.000114,0.000144,2.9e-05,9.5e-05,0.000167,0.00013,0.000159,2.9e-05,2.9e-05,0.000163,0.000133,0.000133,0.00013,0.000127,0.000161,0.000144,0.000126,0.000144,0.000137,0.000163,9.4e-05,9.4e-05,9.4e-05,9.4e-05,0.000128,0.000101,0.00014,0.000151,0.000163,0.000124,0.000144,0.000123,0.000148,0.000153,0.000135,0.000108,0.000153,0.000151,8.6e-05,9.7e-05,0.000132,0.000158,9.9e-05,0.000153,2.9e-05,2.9e-05,0.000151,0.000141,0.000114,2.9e-05,2.9e-05,0.000153,0.00017,0.000149,0.000133,0.000149,0.000147,0.000164,0.000152,0.000139,0.000156,0.000158,0.000166,0.000153,0.000113,0.000113,0.000113,0.00013,0.000123,0.000126,0.000123,0.000127,0.000148,0.000142,0.000141,0.000123,0.000129,0.000131,0.000117,0.000115,0.000143,0.000153,0.000122,0.000115,0.000142,0.00012,0.000118,0.000111,0.000125,8.9e-05,0.000109,0.000112,0.000112,0.000112,0.000112,0.000112,9.3e-05,0.000109,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000262,0.000223,0.000259,0.000209,0.000207,0.000208,0.000158,0.000229,0.000174,0.00016,0.000193,0.000179,0.000188,0.0002,0.000183,0.000165,0.000212,0.000234,0.000159,0.000189,0.000187,0.000181,0.000194,0.000173,0.000183,0.000155,0.000167,0.000195,0.0002,0.000197,0.000202,0.000171,0.000214,0.000123,0.000176,0.000182,0.000183,0.000203,0.000159,0.000198,0.000198,0.000191,0.000192,0.000197,0.000172,0.00019,0.000138,0.000165,0.000165,0.000165,0.000165,0.000165,0.000155,0.000183,0.000185,0.000198,0.000184,0.000185,0.000143,0.000207,0.000165,0.000119,0.000202,0.000163,0.000191,9.7e-05,0.000113,0.000187,0.000211,0.000166,0.00018,0.000163,0.000197,0.000179,0.000178,0.000177,0.000168,0.000199,0.00011,0.00011,0.00011,0.00011,0.000158,0.000168,0.000177,0.000179,0.00021,0.000145,0.000171,0.000156,0.000178,0.000178,0.000161,0.000154,0.00019,0.0002,0.000114,0.000166,0.00018,0.000198,0.000124,0.000172,0.000129,0.000129,0.000186,0.000171,0.000157,0.000129,0.000131,0.000185,0.000205,0.000192,0.000167,0.000193,0.000186,0.000203,0.00018,0.000169,0.000189,0.000196,0.000199,0.000198,0.000159,0.000157,0.000159,0.000162,0.000165,0.000168,0.000167,0.000161,0.000178,0.000174,0.000176,0.000165,0.000169,0.000176,0.00018,0.000158,0.000187,0.000187,0.00016,0.000161,0.000177,0.000158,0.00014,0.000176,0.000162,0.000186,0.000151,0.000151,0.000156,0.000151,0.000156,0.000151,0.000166,0.000144,0.000158,0.000162,0.000156,0.000129,0.000119,0.00016,0.000148,0.000142,0.000116,0.000112,0.000132,0.000116,0.000116,0.000143,0.000121,0.000116,0.000143,0.000141,0.000153,0.000155,0.000146,0.000116,0.000146,0.000121,0.000149,0.000149,0.000142,0.000163,0.000123,0.000128,0.000136],[0.017419,0.016157,0.018383,0.015569,0.015952,0.015716,0.014873,0.015629,0.014243,0.01503,0.01616,0.015841,0.013482,0.015488,0.015221,0.014725,0.015811,0.017014,0.013451,0.015641,0.015837,0.014314,0.015757,0.012003,0.015328,0.014797,0.013002,0.015824,0.015499,0.015907,0.01603,0.015204,0.015955,0.01283,0.015477,0.014448,0.015371,0.015646,0.013488,0.015707,0.01594,0.015051,0.014979,0.015675,0.014886,0.015695,0.011247,0.013955,0.013955,0.013955,0.013955,0.013955,0.012339,0.0157,0.012717,0.015522,0.015957,0.013503,0.013003,0.016469,0.015399,0.011914,0.015247,0.015207,0.014449,0.005348,0.01077,0.015042,0.015944,0.015119,0.015531,0.015414,0.015431,0.015437,0.015634,0.015243,0.01531,0.016122,0.011616,0.011616,0.011616,0.011616,0.015282,0.014643,0.01433,0.015534,0.015121,0.011295,0.015289,0.015147,0.015238,0.012329,0.012442,0.014775,0.015951,0.014463,0.011295,0.014173,0.015615,0.015979,0.012185,0.01399,0.013641,0.013641,0.015196,0.015236,0.01523,0.013641,0.013775,0.015314,0.015268,0.015666,0.015369,0.015514,0.015526,0.015238,0.015241,0.015247,0.015292,0.015956,0.015192,0.015851,0.015419,0.015343,0.015419,0.015363,0.015391,0.015419,0.015456,0.014693,0.015304,0.015408,0.015413,0.015376,0.015465,0.015639,0.015938,0.015252,0.015513,0.015197,0.015336,0.015299,0.015187,0.015196,0.014451,0.016116,0.015329,0.016105,0.014202,0.014639,0.015312,0.014639,0.015312,0.014639,0.015189,0.012502,0.015015,0.015587,0.015508,0.011625,0.011762,0.01411,0.013891,0.013685,0.011322,0.010876,0.013533,0.011322,0.011322,0.013973,0.012823,0.011322,0.013592,0.013945,0.014403,0.014433,0.014071,0.011322,0.014074,0.012847,0.013819,0.013819,0.013899,0.014209,0.012688,0.012591,0.014575],[0.036832,0.033185,0.035928,0.032781,0.026575,0.027417,0.025179,0.035408,0.026716,0.0241,0.024241,0.024352,0.0318,0.026417,0.027551,0.025511,0.032208,0.033435,0.024244,0.02669,0.026271,0.026953,0.028726,0.028913,0.026249,0.020211,0.024997,0.028873,0.033738,0.028949,0.030861,0.026628,0.032985,0.017333,0.024603,0.026747,0.02583,0.024307,0.025762,0.031261,0.030974,0.031467,0.031456,0.028837,0.026992,0.028717,0.020437,0.025666,0.025666,0.025666,0.025666,0.025666,0.025054,0.023907,0.029618,0.029822,0.027637,0.027539,0.021141,0.02684,0.005348,0.017679,0.031093,0.02409,0.029543,0.005374,0.0054,0.030254,0.0248,0.024698,0.024109,0.02367,0.030029,0.026709,0.023443,0.026863,0.025413,0.030357,0.017567,0.017567,0.017567,0.017567,0.023883,0.018731,0.02601,0.027999,0.030352,0.023045,0.026844,0.022828,0.027531,0.028475,0.025176,0.020009,0.028536,0.028105,0.01608,0.018032,0.024642,0.029375,0.018401,0.028523,0.005348,0.005348,0.02816,0.026227,0.021168,0.005348,0.005348,0.02838,0.031638,0.027765,0.02477,0.027757,0.027373,0.030472,0.028285,0.025772,0.029049,0.029324,0.03081,0.028528,0.021062,0.021062,0.021062,0.024238,0.022912,0.023405,0.022912,0.023667,0.027507,0.026361,0.026216,0.022912,0.024006,0.024438,0.021788,0.021446,0.026541,0.028462,0.022772,0.021446,0.026481,0.022402,0.021913,0.020568,0.023314,0.016573,0.020248,0.020854,0.020854,0.020854,0.020854,0.020854,0.017354,0.020189,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.048729,0.041481,0.048187,0.038782,0.038454,0.038669,0.029477,0.042563,0.032393,0.029793,0.035984,0.033357,0.035041,0.037185,0.033979,0.030612,0.039432,0.04358,0.029533,0.035207,0.034786,0.033757,0.035998,0.032119,0.034078,0.028855,0.03099,0.0363,0.037126,0.036571,0.037659,0.031762,0.039854,0.022895,0.032752,0.033898,0.033948,0.037667,0.029618,0.036759,0.036897,0.03553,0.035639,0.036607,0.031953,0.035261,0.025705,0.030627,0.030627,0.030627,0.030627,0.030627,0.028833,0.03396,0.034413,0.036912,0.034179,0.034362,0.026564,0.038446,0.030663,0.022059,0.037583,0.030382,0.035442,0.018111,0.021045,0.034695,0.039282,0.030943,0.033562,0.030318,0.036614,0.033226,0.03302,0.03299,0.031182,0.037052,0.020393,0.020393,0.020393,0.020393,0.02937,0.03134,0.03288,0.033256,0.03915,0.026941,0.031833,0.029081,0.033031,0.033143,0.029957,0.028585,0.035308,0.037133,0.021127,0.030959,0.033526,0.036868,0.023034,0.03203,0.023985,0.023985,0.034585,0.031833,0.029199,0.023985,0.024448,0.034406,0.038052,0.035632,0.031036,0.035838,0.034618,0.037828,0.033405,0.031461,0.035201,0.036399,0.036993,0.036803,0.029557,0.02919,0.029557,0.030117,0.030617,0.031276,0.031073,0.029899,0.033043,0.03241,0.032723,0.030683,0.031504,0.032695,0.033459,0.029435,0.034812,0.034701,0.029722,0.029899,0.032967,0.029468,0.026047,0.03281,0.030131,0.034618,0.028084,0.02816,0.029103,0.02816,0.029103,0.02816,0.030897,0.026751,0.029393,0.030058,0.029103,0.023978,0.022117,0.029698,0.027535,0.026387,0.021605,0.020897,0.024587,0.021605,0.021605,0.026534,0.022556,0.021605,0.026575,0.026205,0.028393,0.028824,0.027078,0.021605,0.027145,0.022556,0.027745,0.027745,0.026372,0.030239,0.022954,0.023731,0.02522],[0.372,0.343173,0.392405,0.336347,0.308458,0.301948,0.304419,0.319039,0.311037,0.288372,0.293839,0.286154,0.333932,0.322917,0.287481,0.290625,0.353612,0.315254,0.292913,0.286154,0.303922,0.285714,0.295707,0.333333,0.285714,0.240621,0.277198,0.318493,0.333333,0.316327,0.310518,0.2976,0.315789,0.223827,0.230198,0.285714,0.30897,0.268786,0.298555,0.320138,0.334532,0.317406,0.317406,0.293839,0.293375,0.300485,0.230769,0.291536,0.291536,0.291536,0.291536,0.291536,0.282675,0.298555,0.331551,0.32069,0.304419,0.288372,0.261236,0.324607,0.005348,0.227941,0.298555,0.290625,0.299517,0.005376,0.005405,0.306931,0.298555,0.283537,0.283537,0.294304,0.296651,0.296178,0.289269,0.290172,0.288372,0.314189,0.229346,0.229346,0.229346,0.229346,0.287481,0.250674,0.295238,0.299035,0.292453,0.274742,0.303922,0.286154,0.291536,0.302439,0.28882,0.24933,0.295707,0.288372,0.216028,0.223289,0.285714,0.306931,0.234552,0.313659,0.005348,0.005348,0.290625,0.299517,0.285276,0.005348,0.005348,0.297125,0.2976,0.28972,0.287037,0.285714,0.287037,0.291994,0.29477,0.295238,0.290172,0.290172,0.295238,0.311037,0.283537,0.283537,0.283537,0.291994,0.282675,0.285714,0.282675,0.291994,0.291536,0.291536,0.292453,0.282675,0.282675,0.298555,0.284839,0.282675,0.282675,0.291536,0.283105,0.282675,0.286154,0.282675,0.282675,0.282675,0.291536,0.221165,0.252374,0.256198,0.256198,0.256198,0.256198,0.256198,0.226553,0.227106,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(acesso_positivo_df_closseness, by=list(acesso_positivo_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(acesso_positivo_df_closseness, by=list(acesso_positivo_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-91d2b00bf0c5ad13ee42" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-91d2b00bf0c5ad13ee42">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7e-05\" data-max=\"9.9e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000198\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.6e-05\" data-max=\"5.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000136\" data-max=\"0.000262\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3e-06\" data-max=\"3.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.012957\" data-max=\"0.018383\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000119\" data-max=\"0.002414\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.036832\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003038\" data-max=\"0.009924\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.02522\" data-max=\"0.048729\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000514\" data-max=\"0.006246\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003897\" data-max=\"0.140058\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[8.3e-05,7e-05,8.4e-05,8.7e-05,8e-05,8.9e-05,8.6e-05,9.9e-05,8.5e-05,8.2e-05,8.7e-05,7.2e-05,8.2e-05,8.5e-05,8.5e-05,7.9e-05,7.8e-05,8.6e-05,9.4e-05,7.5e-05,9.1e-05,8.2e-05,8.3e-05],[6e-06,7e-06,null,null,null,null,1e-06,null,null,null,null,1.3e-05,1e-06,2e-06,1e-06,6e-06,null,null,null,3e-06,null,2e-06,1e-06],[0.000151,6.2e-05,0.000143,0.000178,0.000135,0.000144,0.000136,0.000193,0.000154,0.000125,0.000163,0.000132,0.000155,0.000154,0.000154,0.000143,2.9e-05,0.000158,0.000198,0.000101,0.00018,0.000108,0.000139],[3.7e-05,4.6e-05,null,null,null,null,2.2e-05,null,null,null,null,3.6e-05,1.9e-05,1.9e-05,1.6e-05,2.2e-05,null,null,null,5.3e-05,null,3.7e-05,1.6e-05],[0.000188,0.000141,0.000189,0.000223,0.000158,0.000207,0.000186,0.000259,0.000194,0.000162,0.000199,0.000169,0.000196,0.000194,0.000209,0.000176,0.000136,0.000198,0.000262,0.000156,0.000234,0.000162,0.000178],[3.3e-05,2.2e-05,null,null,null,null,1e-05,null,null,null,null,3.4e-05,6e-06,1.1e-05,3e-06,2.6e-05,null,null,null,2.2e-05,null,1e-05,1.3e-05],[0.015366,0.012957,0.015641,0.016157,0.014873,0.016469,0.015926,0.018383,0.015757,0.015329,0.016122,0.013463,0.01527,0.015744,0.015826,0.01471,0.014575,0.015979,0.017419,0.014028,0.017014,0.015239,0.015405],[0.001008,0.001275,null,null,null,null,0.000165,null,null,null,null,0.002414,0.000309,0.000251,0.000119,0.001138,null,null,null,0.000598,null,0.000337,0.000245],[0.028069,0.011443,0.02669,0.033185,0.025179,0.02684,0.025237,0.035928,0.028726,0.023314,0.030357,0.024465,0.028942,0.028674,0.028733,0.026602,0.005348,0.029375,0.036832,0.018775,0.033435,0.020119,0.025891],[0.006847,0.008509,null,null,null,null,0.004248,null,null,null,null,0.00679,0.003571,0.003599,0.003038,0.004054,null,null,null,0.009924,null,0.006944,0.003055],[0.034945,0.026132,0.035207,0.041481,0.029477,0.038446,0.034632,0.048187,0.035998,0.030131,0.037052,0.031384,0.036358,0.036045,0.038852,0.032714,0.02522,0.036868,0.048729,0.028953,0.04358,0.03019,0.033055],[0.006123,0.004171,null,null,null,null,0.00175,null,null,null,null,0.006246,0.00117,0.001968,0.000514,0.004898,null,null,null,0.004128,null,0.001875,0.002428],[0.300898,0.099162,0.286154,0.343173,0.304419,0.324607,0.301697,0.392405,0.295707,0.291536,0.314189,0.267114,0.320162,0.303099,0.321339,0.296087,0.005348,0.306931,0.372,0.194753,0.315254,0.234499,0.289379],[0.043056,0.125946,null,null,null,null,0.018012,null,null,null,null,0.070908,0.003897,0.032119,0.028138,0.024042,null,null,null,0.140058,null,0.102884,0.011767]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
acesso_positivo_df_closseness <- data.frame(
acesso_positivo_incloseness,
acesso_positivo_outcloseness,
acesso_positivo_totalcloseness,
acesso_positivo_incloseness_n,
acesso_positivo_outcloseness_n,
acesso_positivo_totalcloseness_n,
acesso_positivo_centr_closeness) %>% round(6)

#Adding type
acesso_positivo_df_closseness <-cbind(acesso_positivo_df_closseness, V(acesso_positivo)$TIPO1)

#Adding names
names(acesso_positivo_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
acesso_positivo_df_closseness<-acesso_positivo_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(acesso_positivo_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-dee057588612d803a6b9" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-dee057588612d803a6b9">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"9.9e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000198\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"9.7e-05\" data-max=\"0.000262\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.018383\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.036832\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.018111\" data-max=\"0.048729\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[9.4e-05,8.7e-05,9.9e-05,8.4e-05,8.6e-05,8.4e-05,8e-05,8.4e-05,7.7e-05,8.1e-05,8.7e-05,8.5e-05,7.2e-05,8.3e-05,8.2e-05,7.9e-05,8.5e-05,9.1e-05,7.2e-05,8.4e-05,8.5e-05,7.7e-05,8.5e-05,6.5e-05,8.2e-05,8e-05,7e-05,8.5e-05,8.3e-05,8.6e-05,8.6e-05,8.2e-05,8.6e-05,6.9e-05,8.3e-05,7.8e-05,8.3e-05,8.4e-05,7.3e-05,8.4e-05,8.6e-05,8.1e-05,8.1e-05,8.4e-05,8e-05,8.4e-05,6e-05,7.5e-05,7.5e-05,7.5e-05,7.5e-05,7.5e-05,6.6e-05,8.4e-05,6.8e-05,8.3e-05,8.6e-05,7.3e-05,7e-05,8.9e-05,8.3e-05,6.4e-05,8.2e-05,8.2e-05,7.8e-05,2.9e-05,5.8e-05,8.1e-05,8.6e-05,8.1e-05,8.4e-05,8.3e-05,8.3e-05,8.3e-05,8.4e-05,8.2e-05,8.2e-05,8.7e-05,6.2e-05,6.2e-05,6.2e-05,6.2e-05,8.2e-05,7.9e-05,7.7e-05,8.4e-05,8.1e-05,6.1e-05,8.2e-05,8.1e-05,8.2e-05,6.6e-05,6.7e-05,7.9e-05,8.6e-05,7.8e-05,6.1e-05,7.6e-05,8.4e-05,8.6e-05,6.6e-05,7.5e-05,7.3e-05,7.3e-05,8.2e-05,8.2e-05,8.2e-05,7.3e-05,7.4e-05,8.2e-05,8.2e-05,8.4e-05,8.3e-05,8.3e-05,8.3e-05,8.2e-05,8.2e-05,8.2e-05,8.2e-05,8.6e-05,8.2e-05,8.5e-05,8.3e-05,8.2e-05,8.3e-05,8.3e-05,8.3e-05,8.3e-05,8.3e-05,7.9e-05,8.2e-05,8.3e-05,8.3e-05,8.3e-05,8.3e-05,8.4e-05,8.6e-05,8.2e-05,8.3e-05,8.2e-05,8.2e-05,8.2e-05,8.2e-05,8.2e-05,7.8e-05,8.7e-05,8.2e-05,8.7e-05,7.6e-05,7.9e-05,8.2e-05,7.9e-05,8.2e-05,7.9e-05,8.2e-05,6.7e-05,8.1e-05,8.4e-05,8.3e-05,6.2e-05,6.3e-05,7.6e-05,7.5e-05,7.4e-05,6.1e-05,5.8e-05,7.3e-05,6.1e-05,6.1e-05,7.5e-05,6.9e-05,6.1e-05,7.3e-05,7.5e-05,7.7e-05,7.8e-05,7.6e-05,6.1e-05,7.6e-05,6.9e-05,7.4e-05,7.4e-05,7.5e-05,7.6e-05,6.8e-05,6.8e-05,7.8e-05],[0.000198,0.000178,0.000193,0.000176,0.000143,0.000147,0.000135,0.00019,0.000144,0.00013,0.00013,0.000131,0.000171,0.000142,0.000148,0.000137,0.000173,0.00018,0.00013,0.000143,0.000141,0.000145,0.000154,0.000155,0.000141,0.000109,0.000134,0.000155,0.000181,0.000156,0.000166,0.000143,0.000177,9.3e-05,0.000132,0.000144,0.000139,0.000131,0.000139,0.000168,0.000167,0.000169,0.000169,0.000155,0.000145,0.000154,0.00011,0.000138,0.000138,0.000138,0.000138,0.000138,0.000135,0.000129,0.000159,0.00016,0.000149,0.000148,0.000114,0.000144,2.9e-05,9.5e-05,0.000167,0.00013,0.000159,2.9e-05,2.9e-05,0.000163,0.000133,0.000133,0.00013,0.000127,0.000161,0.000144,0.000126,0.000144,0.000137,0.000163,9.4e-05,9.4e-05,9.4e-05,9.4e-05,0.000128,0.000101,0.00014,0.000151,0.000163,0.000124,0.000144,0.000123,0.000148,0.000153,0.000135,0.000108,0.000153,0.000151,8.6e-05,9.7e-05,0.000132,0.000158,9.9e-05,0.000153,2.9e-05,2.9e-05,0.000151,0.000141,0.000114,2.9e-05,2.9e-05,0.000153,0.00017,0.000149,0.000133,0.000149,0.000147,0.000164,0.000152,0.000139,0.000156,0.000158,0.000166,0.000153,0.000113,0.000113,0.000113,0.00013,0.000123,0.000126,0.000123,0.000127,0.000148,0.000142,0.000141,0.000123,0.000129,0.000131,0.000117,0.000115,0.000143,0.000153,0.000122,0.000115,0.000142,0.00012,0.000118,0.000111,0.000125,8.9e-05,0.000109,0.000112,0.000112,0.000112,0.000112,0.000112,9.3e-05,0.000109,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000262,0.000223,0.000259,0.000209,0.000207,0.000208,0.000158,0.000229,0.000174,0.00016,0.000193,0.000179,0.000188,0.0002,0.000183,0.000165,0.000212,0.000234,0.000159,0.000189,0.000187,0.000181,0.000194,0.000173,0.000183,0.000155,0.000167,0.000195,0.0002,0.000197,0.000202,0.000171,0.000214,0.000123,0.000176,0.000182,0.000183,0.000203,0.000159,0.000198,0.000198,0.000191,0.000192,0.000197,0.000172,0.00019,0.000138,0.000165,0.000165,0.000165,0.000165,0.000165,0.000155,0.000183,0.000185,0.000198,0.000184,0.000185,0.000143,0.000207,0.000165,0.000119,0.000202,0.000163,0.000191,9.7e-05,0.000113,0.000187,0.000211,0.000166,0.00018,0.000163,0.000197,0.000179,0.000178,0.000177,0.000168,0.000199,0.00011,0.00011,0.00011,0.00011,0.000158,0.000168,0.000177,0.000179,0.00021,0.000145,0.000171,0.000156,0.000178,0.000178,0.000161,0.000154,0.00019,0.0002,0.000114,0.000166,0.00018,0.000198,0.000124,0.000172,0.000129,0.000129,0.000186,0.000171,0.000157,0.000129,0.000131,0.000185,0.000205,0.000192,0.000167,0.000193,0.000186,0.000203,0.00018,0.000169,0.000189,0.000196,0.000199,0.000198,0.000159,0.000157,0.000159,0.000162,0.000165,0.000168,0.000167,0.000161,0.000178,0.000174,0.000176,0.000165,0.000169,0.000176,0.00018,0.000158,0.000187,0.000187,0.00016,0.000161,0.000177,0.000158,0.00014,0.000176,0.000162,0.000186,0.000151,0.000151,0.000156,0.000151,0.000156,0.000151,0.000166,0.000144,0.000158,0.000162,0.000156,0.000129,0.000119,0.00016,0.000148,0.000142,0.000116,0.000112,0.000132,0.000116,0.000116,0.000143,0.000121,0.000116,0.000143,0.000141,0.000153,0.000155,0.000146,0.000116,0.000146,0.000121,0.000149,0.000149,0.000142,0.000163,0.000123,0.000128,0.000136],[0.017419,0.016157,0.018383,0.015569,0.015952,0.015716,0.014873,0.015629,0.014243,0.01503,0.01616,0.015841,0.013482,0.015488,0.015221,0.014725,0.015811,0.017014,0.013451,0.015641,0.015837,0.014314,0.015757,0.012003,0.015328,0.014797,0.013002,0.015824,0.015499,0.015907,0.01603,0.015204,0.015955,0.01283,0.015477,0.014448,0.015371,0.015646,0.013488,0.015707,0.01594,0.015051,0.014979,0.015675,0.014886,0.015695,0.011247,0.013955,0.013955,0.013955,0.013955,0.013955,0.012339,0.0157,0.012717,0.015522,0.015957,0.013503,0.013003,0.016469,0.015399,0.011914,0.015247,0.015207,0.014449,0.005348,0.01077,0.015042,0.015944,0.015119,0.015531,0.015414,0.015431,0.015437,0.015634,0.015243,0.01531,0.016122,0.011616,0.011616,0.011616,0.011616,0.015282,0.014643,0.01433,0.015534,0.015121,0.011295,0.015289,0.015147,0.015238,0.012329,0.012442,0.014775,0.015951,0.014463,0.011295,0.014173,0.015615,0.015979,0.012185,0.01399,0.013641,0.013641,0.015196,0.015236,0.01523,0.013641,0.013775,0.015314,0.015268,0.015666,0.015369,0.015514,0.015526,0.015238,0.015241,0.015247,0.015292,0.015956,0.015192,0.015851,0.015419,0.015343,0.015419,0.015363,0.015391,0.015419,0.015456,0.014693,0.015304,0.015408,0.015413,0.015376,0.015465,0.015639,0.015938,0.015252,0.015513,0.015197,0.015336,0.015299,0.015187,0.015196,0.014451,0.016116,0.015329,0.016105,0.014202,0.014639,0.015312,0.014639,0.015312,0.014639,0.015189,0.012502,0.015015,0.015587,0.015508,0.011625,0.011762,0.01411,0.013891,0.013685,0.011322,0.010876,0.013533,0.011322,0.011322,0.013973,0.012823,0.011322,0.013592,0.013945,0.014403,0.014433,0.014071,0.011322,0.014074,0.012847,0.013819,0.013819,0.013899,0.014209,0.012688,0.012591,0.014575],[0.036832,0.033185,0.035928,0.032781,0.026575,0.027417,0.025179,0.035408,0.026716,0.0241,0.024241,0.024352,0.0318,0.026417,0.027551,0.025511,0.032208,0.033435,0.024244,0.02669,0.026271,0.026953,0.028726,0.028913,0.026249,0.020211,0.024997,0.028873,0.033738,0.028949,0.030861,0.026628,0.032985,0.017333,0.024603,0.026747,0.02583,0.024307,0.025762,0.031261,0.030974,0.031467,0.031456,0.028837,0.026992,0.028717,0.020437,0.025666,0.025666,0.025666,0.025666,0.025666,0.025054,0.023907,0.029618,0.029822,0.027637,0.027539,0.021141,0.02684,0.005348,0.017679,0.031093,0.02409,0.029543,0.005374,0.0054,0.030254,0.0248,0.024698,0.024109,0.02367,0.030029,0.026709,0.023443,0.026863,0.025413,0.030357,0.017567,0.017567,0.017567,0.017567,0.023883,0.018731,0.02601,0.027999,0.030352,0.023045,0.026844,0.022828,0.027531,0.028475,0.025176,0.020009,0.028536,0.028105,0.01608,0.018032,0.024642,0.029375,0.018401,0.028523,0.005348,0.005348,0.02816,0.026227,0.021168,0.005348,0.005348,0.02838,0.031638,0.027765,0.02477,0.027757,0.027373,0.030472,0.028285,0.025772,0.029049,0.029324,0.03081,0.028528,0.021062,0.021062,0.021062,0.024238,0.022912,0.023405,0.022912,0.023667,0.027507,0.026361,0.026216,0.022912,0.024006,0.024438,0.021788,0.021446,0.026541,0.028462,0.022772,0.021446,0.026481,0.022402,0.021913,0.020568,0.023314,0.016573,0.020248,0.020854,0.020854,0.020854,0.020854,0.020854,0.017354,0.020189,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.048729,0.041481,0.048187,0.038782,0.038454,0.038669,0.029477,0.042563,0.032393,0.029793,0.035984,0.033357,0.035041,0.037185,0.033979,0.030612,0.039432,0.04358,0.029533,0.035207,0.034786,0.033757,0.035998,0.032119,0.034078,0.028855,0.03099,0.0363,0.037126,0.036571,0.037659,0.031762,0.039854,0.022895,0.032752,0.033898,0.033948,0.037667,0.029618,0.036759,0.036897,0.03553,0.035639,0.036607,0.031953,0.035261,0.025705,0.030627,0.030627,0.030627,0.030627,0.030627,0.028833,0.03396,0.034413,0.036912,0.034179,0.034362,0.026564,0.038446,0.030663,0.022059,0.037583,0.030382,0.035442,0.018111,0.021045,0.034695,0.039282,0.030943,0.033562,0.030318,0.036614,0.033226,0.03302,0.03299,0.031182,0.037052,0.020393,0.020393,0.020393,0.020393,0.02937,0.03134,0.03288,0.033256,0.03915,0.026941,0.031833,0.029081,0.033031,0.033143,0.029957,0.028585,0.035308,0.037133,0.021127,0.030959,0.033526,0.036868,0.023034,0.03203,0.023985,0.023985,0.034585,0.031833,0.029199,0.023985,0.024448,0.034406,0.038052,0.035632,0.031036,0.035838,0.034618,0.037828,0.033405,0.031461,0.035201,0.036399,0.036993,0.036803,0.029557,0.02919,0.029557,0.030117,0.030617,0.031276,0.031073,0.029899,0.033043,0.03241,0.032723,0.030683,0.031504,0.032695,0.033459,0.029435,0.034812,0.034701,0.029722,0.029899,0.032967,0.029468,0.026047,0.03281,0.030131,0.034618,0.028084,0.02816,0.029103,0.02816,0.029103,0.02816,0.030897,0.026751,0.029393,0.030058,0.029103,0.023978,0.022117,0.029698,0.027535,0.026387,0.021605,0.020897,0.024587,0.021605,0.021605,0.026534,0.022556,0.021605,0.026575,0.026205,0.028393,0.028824,0.027078,0.021605,0.027145,0.022556,0.027745,0.027745,0.026372,0.030239,0.022954,0.023731,0.02522],[0.372,0.343173,0.392405,0.336347,0.308458,0.301948,0.304419,0.319039,0.311037,0.288372,0.293839,0.286154,0.333932,0.322917,0.287481,0.290625,0.353612,0.315254,0.292913,0.286154,0.303922,0.285714,0.295707,0.333333,0.285714,0.240621,0.277198,0.318493,0.333333,0.316327,0.310518,0.2976,0.315789,0.223827,0.230198,0.285714,0.30897,0.268786,0.298555,0.320138,0.334532,0.317406,0.317406,0.293839,0.293375,0.300485,0.230769,0.291536,0.291536,0.291536,0.291536,0.291536,0.282675,0.298555,0.331551,0.32069,0.304419,0.288372,0.261236,0.324607,0.005348,0.227941,0.298555,0.290625,0.299517,0.005376,0.005405,0.306931,0.298555,0.283537,0.283537,0.294304,0.296651,0.296178,0.289269,0.290172,0.288372,0.314189,0.229346,0.229346,0.229346,0.229346,0.287481,0.250674,0.295238,0.299035,0.292453,0.274742,0.303922,0.286154,0.291536,0.302439,0.28882,0.24933,0.295707,0.288372,0.216028,0.223289,0.285714,0.306931,0.234552,0.313659,0.005348,0.005348,0.290625,0.299517,0.285276,0.005348,0.005348,0.297125,0.2976,0.28972,0.287037,0.285714,0.287037,0.291994,0.29477,0.295238,0.290172,0.290172,0.295238,0.311037,0.283537,0.283537,0.283537,0.291994,0.282675,0.285714,0.282675,0.291994,0.291536,0.291536,0.292453,0.282675,0.282675,0.298555,0.284839,0.282675,0.282675,0.291536,0.283105,0.282675,0.286154,0.282675,0.282675,0.282675,0.291536,0.221165,0.252374,0.256198,0.256198,0.256198,0.256198,0.256198,0.226553,0.227106,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(acesso_positivo_df_closseness, by=list(acesso_positivo_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(acesso_positivo_df_closseness, by=list(acesso_positivo_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-c9664e6f2eae33816d38" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-c9664e6f2eae33816d38">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7.2e-05\" data-max=\"8.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4e-06\" data-max=\"9e-06\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"9.2e-05\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.5e-05\" data-max=\"5.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000153\" data-max=\"0.000178\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.3e-05\" data-max=\"3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.013413\" data-max=\"0.015357\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000714\" data-max=\"0.001724\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.01711\" data-max=\"0.024812\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.006613\" data-max=\"0.010018\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.028542\" data-max=\"0.033184\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.004294\" data-max=\"0.005586\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.17405\" data-max=\"0.273316\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.079334\" data-max=\"0.135668\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[8.3e-05,7.2e-05],[4e-06,9e-06],[0.000133,9.2e-05],[3.5e-05,5.4e-05],[0.000178,0.000153],[2.3e-05,3e-05],[0.015357,0.013413],[0.000714,0.001724],[0.024812,0.01711],[0.006613,0.010018],[0.033184,0.028542],[0.004294,0.005586],[0.273316,0.17405],[0.079334,0.135668]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
acesso_positivo_df_closseness <- data.frame(
acesso_positivo_incloseness,
acesso_positivo_outcloseness,
acesso_positivo_totalcloseness,
acesso_positivo_incloseness_n,
acesso_positivo_outcloseness_n,
acesso_positivo_totalcloseness_n,
acesso_positivo_centr_closeness) %>% round(6)

#Adding type
acesso_positivo_df_closseness <-cbind(acesso_positivo_df_closseness, V(acesso_positivo)$TIPO2)

#Adding names
names(acesso_positivo_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
acesso_positivo_df_closseness<-acesso_positivo_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(acesso_positivo_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-e34e4b8b34b3878cf3cd" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-e34e4b8b34b3878cf3cd">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"9.9e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000198\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"9.7e-05\" data-max=\"0.000262\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.018383\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.036832\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.018111\" data-max=\"0.048729\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[9.4e-05,8.7e-05,9.9e-05,8.4e-05,8.6e-05,8.4e-05,8e-05,8.4e-05,7.7e-05,8.1e-05,8.7e-05,8.5e-05,7.2e-05,8.3e-05,8.2e-05,7.9e-05,8.5e-05,9.1e-05,7.2e-05,8.4e-05,8.5e-05,7.7e-05,8.5e-05,6.5e-05,8.2e-05,8e-05,7e-05,8.5e-05,8.3e-05,8.6e-05,8.6e-05,8.2e-05,8.6e-05,6.9e-05,8.3e-05,7.8e-05,8.3e-05,8.4e-05,7.3e-05,8.4e-05,8.6e-05,8.1e-05,8.1e-05,8.4e-05,8e-05,8.4e-05,6e-05,7.5e-05,7.5e-05,7.5e-05,7.5e-05,7.5e-05,6.6e-05,8.4e-05,6.8e-05,8.3e-05,8.6e-05,7.3e-05,7e-05,8.9e-05,8.3e-05,6.4e-05,8.2e-05,8.2e-05,7.8e-05,2.9e-05,5.8e-05,8.1e-05,8.6e-05,8.1e-05,8.4e-05,8.3e-05,8.3e-05,8.3e-05,8.4e-05,8.2e-05,8.2e-05,8.7e-05,6.2e-05,6.2e-05,6.2e-05,6.2e-05,8.2e-05,7.9e-05,7.7e-05,8.4e-05,8.1e-05,6.1e-05,8.2e-05,8.1e-05,8.2e-05,6.6e-05,6.7e-05,7.9e-05,8.6e-05,7.8e-05,6.1e-05,7.6e-05,8.4e-05,8.6e-05,6.6e-05,7.5e-05,7.3e-05,7.3e-05,8.2e-05,8.2e-05,8.2e-05,7.3e-05,7.4e-05,8.2e-05,8.2e-05,8.4e-05,8.3e-05,8.3e-05,8.3e-05,8.2e-05,8.2e-05,8.2e-05,8.2e-05,8.6e-05,8.2e-05,8.5e-05,8.3e-05,8.2e-05,8.3e-05,8.3e-05,8.3e-05,8.3e-05,8.3e-05,7.9e-05,8.2e-05,8.3e-05,8.3e-05,8.3e-05,8.3e-05,8.4e-05,8.6e-05,8.2e-05,8.3e-05,8.2e-05,8.2e-05,8.2e-05,8.2e-05,8.2e-05,7.8e-05,8.7e-05,8.2e-05,8.7e-05,7.6e-05,7.9e-05,8.2e-05,7.9e-05,8.2e-05,7.9e-05,8.2e-05,6.7e-05,8.1e-05,8.4e-05,8.3e-05,6.2e-05,6.3e-05,7.6e-05,7.5e-05,7.4e-05,6.1e-05,5.8e-05,7.3e-05,6.1e-05,6.1e-05,7.5e-05,6.9e-05,6.1e-05,7.3e-05,7.5e-05,7.7e-05,7.8e-05,7.6e-05,6.1e-05,7.6e-05,6.9e-05,7.4e-05,7.4e-05,7.5e-05,7.6e-05,6.8e-05,6.8e-05,7.8e-05],[0.000198,0.000178,0.000193,0.000176,0.000143,0.000147,0.000135,0.00019,0.000144,0.00013,0.00013,0.000131,0.000171,0.000142,0.000148,0.000137,0.000173,0.00018,0.00013,0.000143,0.000141,0.000145,0.000154,0.000155,0.000141,0.000109,0.000134,0.000155,0.000181,0.000156,0.000166,0.000143,0.000177,9.3e-05,0.000132,0.000144,0.000139,0.000131,0.000139,0.000168,0.000167,0.000169,0.000169,0.000155,0.000145,0.000154,0.00011,0.000138,0.000138,0.000138,0.000138,0.000138,0.000135,0.000129,0.000159,0.00016,0.000149,0.000148,0.000114,0.000144,2.9e-05,9.5e-05,0.000167,0.00013,0.000159,2.9e-05,2.9e-05,0.000163,0.000133,0.000133,0.00013,0.000127,0.000161,0.000144,0.000126,0.000144,0.000137,0.000163,9.4e-05,9.4e-05,9.4e-05,9.4e-05,0.000128,0.000101,0.00014,0.000151,0.000163,0.000124,0.000144,0.000123,0.000148,0.000153,0.000135,0.000108,0.000153,0.000151,8.6e-05,9.7e-05,0.000132,0.000158,9.9e-05,0.000153,2.9e-05,2.9e-05,0.000151,0.000141,0.000114,2.9e-05,2.9e-05,0.000153,0.00017,0.000149,0.000133,0.000149,0.000147,0.000164,0.000152,0.000139,0.000156,0.000158,0.000166,0.000153,0.000113,0.000113,0.000113,0.00013,0.000123,0.000126,0.000123,0.000127,0.000148,0.000142,0.000141,0.000123,0.000129,0.000131,0.000117,0.000115,0.000143,0.000153,0.000122,0.000115,0.000142,0.00012,0.000118,0.000111,0.000125,8.9e-05,0.000109,0.000112,0.000112,0.000112,0.000112,0.000112,9.3e-05,0.000109,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000262,0.000223,0.000259,0.000209,0.000207,0.000208,0.000158,0.000229,0.000174,0.00016,0.000193,0.000179,0.000188,0.0002,0.000183,0.000165,0.000212,0.000234,0.000159,0.000189,0.000187,0.000181,0.000194,0.000173,0.000183,0.000155,0.000167,0.000195,0.0002,0.000197,0.000202,0.000171,0.000214,0.000123,0.000176,0.000182,0.000183,0.000203,0.000159,0.000198,0.000198,0.000191,0.000192,0.000197,0.000172,0.00019,0.000138,0.000165,0.000165,0.000165,0.000165,0.000165,0.000155,0.000183,0.000185,0.000198,0.000184,0.000185,0.000143,0.000207,0.000165,0.000119,0.000202,0.000163,0.000191,9.7e-05,0.000113,0.000187,0.000211,0.000166,0.00018,0.000163,0.000197,0.000179,0.000178,0.000177,0.000168,0.000199,0.00011,0.00011,0.00011,0.00011,0.000158,0.000168,0.000177,0.000179,0.00021,0.000145,0.000171,0.000156,0.000178,0.000178,0.000161,0.000154,0.00019,0.0002,0.000114,0.000166,0.00018,0.000198,0.000124,0.000172,0.000129,0.000129,0.000186,0.000171,0.000157,0.000129,0.000131,0.000185,0.000205,0.000192,0.000167,0.000193,0.000186,0.000203,0.00018,0.000169,0.000189,0.000196,0.000199,0.000198,0.000159,0.000157,0.000159,0.000162,0.000165,0.000168,0.000167,0.000161,0.000178,0.000174,0.000176,0.000165,0.000169,0.000176,0.00018,0.000158,0.000187,0.000187,0.00016,0.000161,0.000177,0.000158,0.00014,0.000176,0.000162,0.000186,0.000151,0.000151,0.000156,0.000151,0.000156,0.000151,0.000166,0.000144,0.000158,0.000162,0.000156,0.000129,0.000119,0.00016,0.000148,0.000142,0.000116,0.000112,0.000132,0.000116,0.000116,0.000143,0.000121,0.000116,0.000143,0.000141,0.000153,0.000155,0.000146,0.000116,0.000146,0.000121,0.000149,0.000149,0.000142,0.000163,0.000123,0.000128,0.000136],[0.017419,0.016157,0.018383,0.015569,0.015952,0.015716,0.014873,0.015629,0.014243,0.01503,0.01616,0.015841,0.013482,0.015488,0.015221,0.014725,0.015811,0.017014,0.013451,0.015641,0.015837,0.014314,0.015757,0.012003,0.015328,0.014797,0.013002,0.015824,0.015499,0.015907,0.01603,0.015204,0.015955,0.01283,0.015477,0.014448,0.015371,0.015646,0.013488,0.015707,0.01594,0.015051,0.014979,0.015675,0.014886,0.015695,0.011247,0.013955,0.013955,0.013955,0.013955,0.013955,0.012339,0.0157,0.012717,0.015522,0.015957,0.013503,0.013003,0.016469,0.015399,0.011914,0.015247,0.015207,0.014449,0.005348,0.01077,0.015042,0.015944,0.015119,0.015531,0.015414,0.015431,0.015437,0.015634,0.015243,0.01531,0.016122,0.011616,0.011616,0.011616,0.011616,0.015282,0.014643,0.01433,0.015534,0.015121,0.011295,0.015289,0.015147,0.015238,0.012329,0.012442,0.014775,0.015951,0.014463,0.011295,0.014173,0.015615,0.015979,0.012185,0.01399,0.013641,0.013641,0.015196,0.015236,0.01523,0.013641,0.013775,0.015314,0.015268,0.015666,0.015369,0.015514,0.015526,0.015238,0.015241,0.015247,0.015292,0.015956,0.015192,0.015851,0.015419,0.015343,0.015419,0.015363,0.015391,0.015419,0.015456,0.014693,0.015304,0.015408,0.015413,0.015376,0.015465,0.015639,0.015938,0.015252,0.015513,0.015197,0.015336,0.015299,0.015187,0.015196,0.014451,0.016116,0.015329,0.016105,0.014202,0.014639,0.015312,0.014639,0.015312,0.014639,0.015189,0.012502,0.015015,0.015587,0.015508,0.011625,0.011762,0.01411,0.013891,0.013685,0.011322,0.010876,0.013533,0.011322,0.011322,0.013973,0.012823,0.011322,0.013592,0.013945,0.014403,0.014433,0.014071,0.011322,0.014074,0.012847,0.013819,0.013819,0.013899,0.014209,0.012688,0.012591,0.014575],[0.036832,0.033185,0.035928,0.032781,0.026575,0.027417,0.025179,0.035408,0.026716,0.0241,0.024241,0.024352,0.0318,0.026417,0.027551,0.025511,0.032208,0.033435,0.024244,0.02669,0.026271,0.026953,0.028726,0.028913,0.026249,0.020211,0.024997,0.028873,0.033738,0.028949,0.030861,0.026628,0.032985,0.017333,0.024603,0.026747,0.02583,0.024307,0.025762,0.031261,0.030974,0.031467,0.031456,0.028837,0.026992,0.028717,0.020437,0.025666,0.025666,0.025666,0.025666,0.025666,0.025054,0.023907,0.029618,0.029822,0.027637,0.027539,0.021141,0.02684,0.005348,0.017679,0.031093,0.02409,0.029543,0.005374,0.0054,0.030254,0.0248,0.024698,0.024109,0.02367,0.030029,0.026709,0.023443,0.026863,0.025413,0.030357,0.017567,0.017567,0.017567,0.017567,0.023883,0.018731,0.02601,0.027999,0.030352,0.023045,0.026844,0.022828,0.027531,0.028475,0.025176,0.020009,0.028536,0.028105,0.01608,0.018032,0.024642,0.029375,0.018401,0.028523,0.005348,0.005348,0.02816,0.026227,0.021168,0.005348,0.005348,0.02838,0.031638,0.027765,0.02477,0.027757,0.027373,0.030472,0.028285,0.025772,0.029049,0.029324,0.03081,0.028528,0.021062,0.021062,0.021062,0.024238,0.022912,0.023405,0.022912,0.023667,0.027507,0.026361,0.026216,0.022912,0.024006,0.024438,0.021788,0.021446,0.026541,0.028462,0.022772,0.021446,0.026481,0.022402,0.021913,0.020568,0.023314,0.016573,0.020248,0.020854,0.020854,0.020854,0.020854,0.020854,0.017354,0.020189,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.048729,0.041481,0.048187,0.038782,0.038454,0.038669,0.029477,0.042563,0.032393,0.029793,0.035984,0.033357,0.035041,0.037185,0.033979,0.030612,0.039432,0.04358,0.029533,0.035207,0.034786,0.033757,0.035998,0.032119,0.034078,0.028855,0.03099,0.0363,0.037126,0.036571,0.037659,0.031762,0.039854,0.022895,0.032752,0.033898,0.033948,0.037667,0.029618,0.036759,0.036897,0.03553,0.035639,0.036607,0.031953,0.035261,0.025705,0.030627,0.030627,0.030627,0.030627,0.030627,0.028833,0.03396,0.034413,0.036912,0.034179,0.034362,0.026564,0.038446,0.030663,0.022059,0.037583,0.030382,0.035442,0.018111,0.021045,0.034695,0.039282,0.030943,0.033562,0.030318,0.036614,0.033226,0.03302,0.03299,0.031182,0.037052,0.020393,0.020393,0.020393,0.020393,0.02937,0.03134,0.03288,0.033256,0.03915,0.026941,0.031833,0.029081,0.033031,0.033143,0.029957,0.028585,0.035308,0.037133,0.021127,0.030959,0.033526,0.036868,0.023034,0.03203,0.023985,0.023985,0.034585,0.031833,0.029199,0.023985,0.024448,0.034406,0.038052,0.035632,0.031036,0.035838,0.034618,0.037828,0.033405,0.031461,0.035201,0.036399,0.036993,0.036803,0.029557,0.02919,0.029557,0.030117,0.030617,0.031276,0.031073,0.029899,0.033043,0.03241,0.032723,0.030683,0.031504,0.032695,0.033459,0.029435,0.034812,0.034701,0.029722,0.029899,0.032967,0.029468,0.026047,0.03281,0.030131,0.034618,0.028084,0.02816,0.029103,0.02816,0.029103,0.02816,0.030897,0.026751,0.029393,0.030058,0.029103,0.023978,0.022117,0.029698,0.027535,0.026387,0.021605,0.020897,0.024587,0.021605,0.021605,0.026534,0.022556,0.021605,0.026575,0.026205,0.028393,0.028824,0.027078,0.021605,0.027145,0.022556,0.027745,0.027745,0.026372,0.030239,0.022954,0.023731,0.02522],[0.372,0.343173,0.392405,0.336347,0.308458,0.301948,0.304419,0.319039,0.311037,0.288372,0.293839,0.286154,0.333932,0.322917,0.287481,0.290625,0.353612,0.315254,0.292913,0.286154,0.303922,0.285714,0.295707,0.333333,0.285714,0.240621,0.277198,0.318493,0.333333,0.316327,0.310518,0.2976,0.315789,0.223827,0.230198,0.285714,0.30897,0.268786,0.298555,0.320138,0.334532,0.317406,0.317406,0.293839,0.293375,0.300485,0.230769,0.291536,0.291536,0.291536,0.291536,0.291536,0.282675,0.298555,0.331551,0.32069,0.304419,0.288372,0.261236,0.324607,0.005348,0.227941,0.298555,0.290625,0.299517,0.005376,0.005405,0.306931,0.298555,0.283537,0.283537,0.294304,0.296651,0.296178,0.289269,0.290172,0.288372,0.314189,0.229346,0.229346,0.229346,0.229346,0.287481,0.250674,0.295238,0.299035,0.292453,0.274742,0.303922,0.286154,0.291536,0.302439,0.28882,0.24933,0.295707,0.288372,0.216028,0.223289,0.285714,0.306931,0.234552,0.313659,0.005348,0.005348,0.290625,0.299517,0.285276,0.005348,0.005348,0.297125,0.2976,0.28972,0.287037,0.285714,0.287037,0.291994,0.29477,0.295238,0.290172,0.290172,0.295238,0.311037,0.283537,0.283537,0.283537,0.291994,0.282675,0.285714,0.282675,0.291994,0.291536,0.291536,0.292453,0.282675,0.282675,0.298555,0.284839,0.282675,0.282675,0.291536,0.283105,0.282675,0.286154,0.282675,0.282675,0.282675,0.291536,0.221165,0.252374,0.256198,0.256198,0.256198,0.256198,0.256198,0.226553,0.227106,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(acesso_positivo_df_closseness, by=list(acesso_positivo_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(acesso_positivo_df_closseness, by=list(acesso_positivo_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-5e8e8aed01a53e8d1535" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-5e8e8aed01a53e8d1535">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7.2e-05\" data-max=\"8.5e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"9e-06\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"9.2e-05\" data-max=\"0.000157\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.7e-05\" data-max=\"5.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000153\" data-max=\"0.000199\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.1e-05\" data-max=\"3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.013376\" data-max=\"0.015813\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000225\" data-max=\"0.001705\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.017058\" data-max=\"0.029239\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003207\" data-max=\"0.010094\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.028506\" data-max=\"0.036984\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.002037\" data-max=\"0.005597\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172678\" data-max=\"0.310428\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.028412\" data-max=\"0.13602\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[8.5e-05,8.2e-05,7.2e-05],[1e-06,4e-06,9e-06],[0.000157,0.000129,9.2e-05],[1.7e-05,3.6e-05,5.4e-05],[0.000199,0.000175,0.000153],[1.1e-05,2.3e-05,3e-05],[0.015813,0.015295,0.013376],[0.000225,0.000738,0.001705],[0.029239,0.024068,0.017058],[0.003207,0.006694,0.010094],[0.036984,0.032558,0.028506],[0.002038,0.004248,0.005597],[0.310428,0.267476,0.172678],[0.028412,0.082813,0.13602]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/acesso_positivo_data.RData")
```

