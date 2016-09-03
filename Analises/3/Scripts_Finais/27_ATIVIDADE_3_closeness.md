# SNA Closeness 27_ATIVIDADE
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 27_ATIVIDADE

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/atividade_data.RData")
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
#atividade<-simplify(atividade) #Simplify
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
V(atividade)$incloseness <- closeness(atividade, mode = "in", weights = E(atividade)$atividade) %>% round(6)
V(atividade)$outcloseness <- closeness(atividade, mode = "out", weights = E(atividade)$atividade) %>% round(6)
V(atividade)$totalcloseness <- closeness(atividade, mode = "total", weights = E(atividade)$atividade) %>% round(4)
```

###Saving to Environment

```r
atividade_incloseness<- closeness(atividade, mode = "in", weights = E(atividade)$atividade) %>% round(6)
atividade_outcloseness<- closeness(atividade, mode = "out", weights = E(atividade)$atividade) %>% round(6)
atividade_totalcloseness<- closeness(atividade, mode = "total", weights = E(atividade)$atividade) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(atividade_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0001280 0.0001290 0.0001272 0.0001300 0.0001350
```

```r
sd(atividade_incloseness)
```

```
## [1] 1.070926e-05
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(atividade)$incloseness<-closeness(atividade, weights = E(atividade)$atividade, mode="in")

#Get Variable
V(atividade)$atividade_color_degree<-round(V(atividade)$incloseness,6)

#Creating brewer pallette
vertex_atividade_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(atividade)$atividade_color_degree)), "RdBu"))(
            length(unique(V(atividade)$atividade_color_degree)))

#Saving as Vertex properties 
V(atividade)$vertex_atividade_color_degree<-
  vertex_atividade_color_degree[as.numeric(
  cut(V(atividade)$atividade_color_degree,
      breaks=length(unique(V(atividade)$atividade_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(atividade, es=E(atividade), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(atividade))
maxC <- rep(Inf, vcount(atividade))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(atividade, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(atividade)$weight)


#PLotting
plot(atividade, 
     layout=co,
     edge.color=V(atividade)$vertex_atividade_color_degree[edge.start],
     edge.arrow.size=closeness(atividade, weights = E(atividade)$atividade, mode="in"),
     edge.width=E(atividade)$weight/mean(E(atividade)$weight),
     edge.curved = TRUE,
     vertex.color=V(atividade)$vertex_atividade_color_degree,
     vertex.size=closeness(atividade, weights = E(atividade)$atividade, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(atividade,"LABEL_COR"),
     vertex.label.cex=(closeness(atividade, weights = E(atividade)$atividade, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(atividade)$atividade_color_degree
b<-V(atividade)$vertex_atividade_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 27_ATIVIDADE", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(atividade, mode="in", weights = E(atividade)$atividade)), 
             sd(closeness(atividade, mode="in", weights = E(atividade)$atividade))
             )
       )
```

![](27_ATIVIDADE_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(atividade_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0005300 0.0007820 0.0006511 0.0008990 0.0011040
```

```r
sd(atividade_outcloseness)
```

```
## [1] 0.0003428904
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(atividade)$outcloseness<-closeness(atividade, weights = E(atividade)$atividade, mode="out")

#Get Variable
V(atividade)$atividade_color_degree<-round(V(atividade)$outcloseness,6)

#Creating brewer pallette
vertex_atividade_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(atividade)$atividade_color_degree)), "RdBu"))(
            length(unique(V(atividade)$atividade_color_degree)))

#Saving as Vertex properties 
V(atividade)$vertex_atividade_color_degree<-
  vertex_atividade_color_degree[as.numeric(
  cut(V(atividade)$atividade_color_degree,
      breaks=length(unique(V(atividade)$atividade_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(atividade, es=E(atividade), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(atividade))
maxC <- rep(Inf, vcount(atividade))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(atividade, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(atividade)$weight)


#PLotting
plot(atividade, 
     layout=co,
     edge.color=V(atividade)$vertex_atividade_color_degree[edge.start],
     edge.arrow.size=closeness(atividade, weights = E(atividade)$atividade, mode="out"),
     edge.width=E(atividade)$weight/2*mean(E(atividade)$weight),
     edge.curved = TRUE,
     vertex.color=V(atividade)$vertex_atividade_color_degree,
     vertex.size=closeness(atividade, weights = E(atividade)$atividade, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(atividade,"LABEL_COR"),
     vertex.label.cex=closeness(atividade, weights = E(atividade)$atividade, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(atividade)$atividade_color_degree
b<-V(atividade)$vertex_atividade_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 27_ATIVIDADE", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(atividade, mode="out", weights = E(atividade)$atividade)), 
             sd(closeness(atividade, mode="out", weights = E(atividade)$atividade))
             )
       )
```

![](27_ATIVIDADE_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(atividade_totalcloseness)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000029 0.001093 0.001225 0.001192 0.001342 0.001650
```

```r
sd(atividade_totalcloseness)
```

```
## [1] 0.0002215581
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(atividade)$allcloseness<-closeness(atividade, weights = E(atividade)$atividade, mode="all")

#Get Variable
V(atividade)$atividade_color_degree<-round(V(atividade)$allcloseness,6)

#Creating brewer pallette
vertex_atividade_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(atividade)$atividade_color_degree)), "RdBu"))(
            length(unique(V(atividade)$atividade_color_degree)))

#Saving as Vertex properties 
V(atividade)$vertex_atividade_color_degree<-
  vertex_atividade_color_degree[as.numeric(
  cut(V(atividade)$atividade_color_degree,
      breaks=length(unique(V(atividade)$atividade_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(atividade, es=E(atividade), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(atividade))
maxC <- rep(Inf, vcount(atividade))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(atividade, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(atividade)$weight)


#PLotting
plot(atividade, 
     layout=co,
     edge.color=V(atividade)$vertex_atividade_color_degree[edge.start],
     edge.arrow.size=closeness(atividade, weights = E(atividade)$atividade, mode="all"),
     edge.width=E(atividade)$weight/2*mean(E(atividade)$weight),
     edge.curved = TRUE,
     vertex.color=V(atividade)$vertex_atividade_color_degree,
     vertex.size=closeness(atividade, weights = E(atividade)$atividade, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(atividade,"LABEL_COR"),
     vertex.label.cex=(closeness(atividade, weights = E(atividade)$atividade, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(atividade)$atividade_color_degree
b<-V(atividade)$vertex_atividade_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 27_ATIVIDADE", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(atividade, mode="all", weights = E(atividade)$atividade)), 
             sd(closeness(atividade, mode="all", weights = E(atividade)$atividade))
             )
       )
```

![](27_ATIVIDADE_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(atividade)$incloseness_n <- closeness(atividade, mode = "in",, weights = E(atividade)$atividade, normalized = T) %>% round(10)
V(atividade)$outcloseness_n <- closeness(atividade, mode = "out", normalized = T, weights = E(atividade)$atividade) %>% round(6)
V(atividade)$totalcloseness_n <- closeness(atividade, mode = "total", normalized = T, weights = E(atividade)$atividade) %>% round(6)
```

###Saving to Environment

```r
atividade_incloseness_n<- closeness(atividade, mode = "in", normalized = T, weights = E(atividade)$atividade) %>% round(6)
atividade_outcloseness_n<- closeness(atividade, mode = "out", normalized = T, weights = E(atividade)$atividade) %>% round(6)
atividade_totalcloseness_n<- closeness(atividade, mode = "total", normalized = T, weights = E(atividade)$atividade) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(atividade_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.023760 0.023950 0.023650 0.024160 0.025080
```

```r
sd(atividade_incloseness_n)
```

```
## [1] 0.001994514
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(atividade)$incloseness_n<-closeness(atividade, weights = E(atividade)$atividade, mode="in", normalized = T)

#Get Variable
V(atividade)$atividade_color_degree<-round(V(atividade)$incloseness_n,6)

#Creating brewer pallette
vertex_atividade_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(atividade)$atividade_color_degree)), "RdBu"))(
            length(unique(V(atividade)$atividade_color_degree)))

#Saving as Vertex properties 
V(atividade)$vertex_atividade_color_degree<-
  vertex_atividade_color_degree[as.numeric(
  cut(V(atividade)$atividade_color_degree,
      breaks=length(unique(V(atividade)$atividade_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(atividade, es=E(atividade), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(atividade))
maxC <- rep(Inf, vcount(atividade))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(atividade, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(atividade)$weight)


#PLotting
plot(atividade, 
     layout=co,
     edge.color=V(atividade)$vertex_atividade_color_degree[edge.start],
     edge.arrow.size=closeness(atividade, weights = E(atividade)$atividade, mode="in",normalized = T),
     edge.width=E(atividade)$weight/10*mean(E(atividade)$weight),
     edge.curved = TRUE,
     vertex.color=V(atividade)$vertex_atividade_color_degree,
     vertex.size=(closeness(atividade, weights = E(atividade)$atividade, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(atividade,"LABEL_COR"),
     vertex.label.cex=closeness(atividade, weights = E(atividade)$atividade, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(atividade)$atividade_color_degree
b<-V(atividade)$vertex_atividade_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 27_ATIVIDADE", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(atividade, mode="in", weights = E(atividade)$atividade, normalized = T)), 
             sd(closeness(atividade, mode="in", weights = E(atividade)$atividade, normalized = T))
             )
       )
```

![](27_ATIVIDADE_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(atividade_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.098540 0.145400 0.121100 0.167300 0.205300
```

```r
sd(atividade_outcloseness_n)
```

```
## [1] 0.06379751
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(atividade)$outcloseness_n<-closeness(atividade, weights = E(atividade)$atividade, mode="out", normalized = T)

#Get Variable
V(atividade)$atividade_color_degree<-round(V(atividade)$outcloseness_n,6)

#Creating brewer pallette
vertex_atividade_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(atividade)$atividade_color_degree)), "RdBu"))(
            length(unique(V(atividade)$atividade_color_degree)))

#Saving as Vertex properties 
V(atividade)$vertex_atividade_color_degree<-
  vertex_atividade_color_degree[as.numeric(
  cut(V(atividade)$atividade_color_degree,
      breaks=length(unique(V(atividade)$atividade_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(atividade, es=E(atividade), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(atividade))
maxC <- rep(Inf, vcount(atividade))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(atividade, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(atividade)$weight)


#PLotting
plot(atividade, 
     layout=co,
     edge.color=V(atividade)$vertex_atividade_color_degree[edge.start],
     edge.arrow.size=closeness(atividade, weights = E(atividade)$atividade, mode="out",normalized = T),
     edge.width=E(atividade)$weight/10*mean(E(atividade)$weight),
     edge.curved = TRUE,
     vertex.color=V(atividade)$vertex_atividade_color_degree,
     vertex.size=(closeness(atividade, weights = E(atividade)$atividade, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(atividade,"LABEL_COR"),
     vertex.label.cex=closeness(atividade, weights = E(atividade)$atividade, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(atividade)$atividade_color_degree
b<-V(atividade)$vertex_atividade_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 27_ATIVIDADE", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(atividade, mode="out", weights = E(atividade)$atividade, normalized = T)), 
             sd(closeness(atividade, mode="out", weights = E(atividade)$atividade, normalized = T))
             )
       )
```

![](27_ATIVIDADE_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(atividade_totalcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.203400 0.227900 0.221700 0.249700 0.306900
```

```r
sd(atividade_totalcloseness_n)
```

```
## [1] 0.04121422
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(atividade)$allcloseness_n<-closeness(atividade, weights = E(atividade)$atividade, mode="all", normalized = T)

#Get Variable
V(atividade)$atividade_color_degree<-round(V(atividade)$allcloseness_n,6)

#Creating brewer pallette
vertex_atividade_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(atividade)$atividade_color_degree)), "RdBu"))(
            length(unique(V(atividade)$atividade_color_degree)))

#Saving as Vertex properties 
V(atividade)$vertex_atividade_color_degree<-
  vertex_atividade_color_degree[as.numeric(
  cut(V(atividade)$atividade_color_degree,
      breaks=length(unique(V(atividade)$atividade_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(atividade, es=E(atividade), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(atividade))
maxC <- rep(Inf, vcount(atividade))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(atividade, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(atividade)$weight)


#PLotting
plot(atividade, 
     layout=co,
     edge.color=V(atividade)$vertex_atividade_color_degree[edge.start],
     edge.arrow.size=closeness(atividade, weights = E(atividade)$atividade, mode="all",normalized = T),
     edge.width=E(atividade)$weight/10*mean(E(atividade)$weight),
     edge.curved = TRUE,
     vertex.color=V(atividade)$vertex_atividade_color_degree,
     vertex.size=(closeness(atividade, weights = E(atividade)$atividade, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(atividade,"LABEL_COR"),
     vertex.label.cex=closeness(atividade, weights = E(atividade)$atividade, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(atividade)$atividade_color_degree
b<-V(atividade)$vertex_atividade_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 27_ATIVIDADE", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(atividade, mode="all", weights = E(atividade)$atividade, normalized = T)), 
             sd(closeness(atividade, mode="all", weights = E(atividade)$atividade, normalized = T))
             )
       )
```

![](27_ATIVIDADE_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(atividade)$incloseness_n <- closeness(atividade, weights = E(atividade)$atividade, mode = "in", normalized = T) %>% round(6)
V(atividade)$outcloseness_n <- closeness(atividade, weights = E(atividade)$atividade, mode = "out", normalized = T) %>% round(6)
V(atividade)$totalcloseness_n <- closeness(atividade, weights = E(atividade)$atividade, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(atividade)$atividade_centr_closeness<- centralization.closeness(atividade)$res
atividade_centr_closeness<- centralization.closeness(atividade)$res
atividade_centr_closeness_all<- centralization.closeness(atividade)
```

###Centralization

```r
atividade_centr_closeness_all$centralization
```

```
## [1] 0.1023336
```

###Theoretical Max

```r
atividade_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(atividade)$atividade_centr_closeness<- centralization.closeness(atividade)$res

#Get Variable
V(atividade)$atividade_color_degree<-round(V(atividade)$atividade_centr_closeness,6)

#Creating brewer pallette
vertex_atividade_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(atividade)$atividade_color_degree)), "Spectral"))(
            length(unique(V(atividade)$atividade_color_degree)))

#Saving as Vertex properties 
V(atividade)$vertex_atividade_color_degree<-
  vertex_atividade_color_degree[as.numeric(
  cut(V(atividade)$atividade_color_degree,
      breaks=length(unique(V(atividade)$atividade_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(atividade, es=E(atividade), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(atividade))
maxC <- rep(Inf, vcount(atividade))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(atividade, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(atividade)$weight)


#PLotting
plot(atividade, 
     layout=co,
     edge.color=V(atividade)$vertex_atividade_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(atividade)$res,
     edge.width=E(atividade)$weight/10*mean(E(atividade)$weight),
     edge.curved = TRUE,
     vertex.color=V(atividade)$vertex_atividade_color_degree,
     vertex.size=centralization.closeness(atividade)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(atividade,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(atividade)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(atividade)$atividade_color_degree
b<-V(atividade)$vertex_atividade_color_degree
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
  title("Network Centralization Closeness - 27_ATIVIDADE", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(atividade)$res), 
             sd(centralization.closeness(atividade)$res)
             )
       )
```

![](27_ATIVIDADE_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
atividade_incloseness<- closeness(atividade, weights = E(atividade)$atividade, mode = "in") %>% round(6)
atividade_outcloseness<- closeness(atividade, weights = E(atividade)$atividade, mode = "out") %>% round(6)
atividade_totalcloseness<- closeness(atividade, weights = E(atividade)$atividade, mode = "total") %>% round(6)
atividade_incloseness_n<- closeness(atividade,weights = E(atividade)$atividade, mode = "in", normalized = T) %>% round(6)
atividade_outcloseness_n<- closeness(atividade,weights = E(atividade)$atividade, mode = "out", normalized = T) %>% round(6)
atividade_totalcloseness_n<- closeness(atividade,weights = E(atividade)$atividade, mode = "total", normalized = T) %>% round(6)
atividade_centr_closeness <- centralization.closeness(atividade)$res %>% round(6)
```

##Creating a datagrame of measures

```r
atividade_df_closseness <- data.frame(
atividade_incloseness,
atividade_outcloseness,
atividade_totalcloseness,
atividade_incloseness_n,
atividade_outcloseness_n,
atividade_totalcloseness_n,
atividade_centr_closeness) %>% round(6)

#Adding type
atividade_df_closseness <-cbind(atividade_df_closseness, V(atividade)$LABEL_COR)

#Adding names
names(atividade_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
atividade_df_closseness<-atividade_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(atividade_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-a3ca2526d14dd5af00c6" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-a3ca2526d14dd5af00c6">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000135\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001104\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.00165\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.025084\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.205298\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.306931\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.277198\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[0.000132,0.000131,0.000134,0.000131,0.000131,0.000131,0.000128,0.00013,0.000123,0.000129,0.000131,0.000131,0.000127,0.000126,0.000129,0.000129,0.000131,0.000132,0.000126,0.00013,0.000129,0.000127,0.00013,0.000126,0.00012,0.000131,0.000128,0.000128,0.000131,0.000131,0.000131,0.000129,0.000132,0.000125,0.000131,0.000129,0.00013,0.000125,0.000125,0.000132,0.00013,0.00013,0.00013,0.00013,0.000129,0.000131,0.000124,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.00013,0.000128,0.000131,0.00013,0.000129,0.000126,0.00013,0.000135,0.000117,0.000128,0.000129,0.00013,2.9e-05,0.000122,0.000131,0.000129,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000131,0.000118,0.000118,0.000118,0.000118,0.000129,0.00013,0.000129,0.000129,0.000129,0.000118,0.000128,0.000127,0.000128,0.000124,0.000117,0.000128,0.00013,0.000129,0.000118,0.000123,0.000129,0.00013,0.000124,0.000129,0.000128,0.000128,0.000129,0.000129,0.000129,0.000128,0.000128,0.000128,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.000128,0.000128,0.000129,0.00013,0.000128,0.000131,0.00013,0.000128,0.00013,0.000129,0.000129,0.000129,0.000129,0.000129,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.00013,0.000129,0.00013,0.000129,0.000128,0.000129,0.000129,0.000128,0.000127,0.00013,0.00013,0.00013,0.000125,0.000128,0.000128,0.000128,0.000128,0.000128,0.000132,0.000125,0.000131,0.000132,0.000131,0.000127,0.00013,0.000131,0.000133,0.000135,0.000126,0.000125,0.000129,0.000126,0.000126,0.00013,0.00013,0.000126,0.000126,0.000128,0.000129,0.000129,0.00013,0.000126,0.00013,0.000128,0.000131,0.000131,0.000132,0.000132,0.00013,0.000128,2.9e-05],[0.001104,0.000989,0.000864,0.000949,0.000969,0.000892,0.000579,0.000899,0.000833,0.000943,0.000782,0.000872,0.000998,0.000992,0.000658,0.000645,0.00081,0.001067,0.000887,0.000849,0.000873,0.000814,0.000804,0.000791,0.000867,0.000754,0.000937,0.000912,0.00087,0.000861,0.000982,0.000936,0.000978,0.000789,0.000696,0.000812,0.001028,0.000818,0.000734,0.000825,0.000939,0.000793,0.000793,0.000747,0.000932,0.001028,0.000842,0.000644,0.000644,0.000644,0.000644,0.000644,0.000937,0.000676,0.000873,0.000825,0.001022,0.000838,0.00083,0.00106,2.9e-05,0.000453,0.001001,0.000661,0.000742,2.9e-05,2.9e-05,0.000926,0.00082,0.00075,0.00075,0.000776,0.000954,0.000898,0.000712,0.000877,0.000948,0.000899,0.000558,0.000507,0.000507,0.000507,0.000922,0.000614,0.00093,0.000948,0.000952,0.00093,0.000971,0.000756,0.000788,0.00098,0.000494,0.00077,0.000982,0.000929,0.000553,0.000515,0.000758,0.001017,0.000595,0.000814,2.9e-05,2.9e-05,0.000949,0.000838,0.000679,2.9e-05,2.9e-05,0.000996,0.000965,0.000972,0.000932,0.000929,0.000972,0.000971,0.000961,0.000943,0.000716,0.00097,0.000943,0.000874,0.000749,0.000749,0.000749,0.000874,0.000586,0.000894,0.000657,0.000874,0.000866,0.000866,0.000899,0.000586,0.000747,0.00089,0.000561,0.000657,0.000657,0.000867,0.000661,0.000586,0.000746,0.000657,0.000529,0.000586,0.000746,0.000531,0.000776,0.000838,0.000838,0.000838,0.000838,0.000838,0.00041,0.00041,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.00165,0.001493,0.001605,0.001435,0.001451,0.001379,0.001167,0.001374,0.001156,0.001332,0.001401,0.00141,0.001355,0.001408,0.001125,0.001049,0.001364,0.00156,0.001302,0.001245,0.001252,0.001109,0.001212,0.00104,0.001211,0.001242,0.001282,0.001272,0.001379,0.001416,0.001435,0.001393,0.001462,0.001153,0.001307,0.001166,0.001427,0.001153,0.000974,0.001374,0.001302,0.001163,0.001196,0.001377,0.001285,0.001443,0.001199,0.000931,0.000931,0.000931,0.000931,0.000931,0.001287,0.001214,0.001225,0.001372,0.001439,0.0013,0.001276,0.001524,0.001389,0.000529,0.001453,0.001195,0.001189,0.000562,0.000717,0.001342,0.001304,0.001284,0.001335,0.001233,0.001385,0.001225,0.001217,0.001247,0.001403,0.001366,0.000687,0.000611,0.000611,0.000611,0.001333,0.001353,0.001362,0.001297,0.001427,0.001269,0.001323,0.001289,0.001089,0.001342,0.000586,0.001025,0.001395,0.001318,0.000679,0.000784,0.001309,0.001416,0.000928,0.00122,0.000933,0.000933,0.001366,0.001179,0.001193,0.000933,0.000933,0.001425,0.00137,0.001379,0.001337,0.001294,0.001391,0.001377,0.001357,0.001351,0.001193,0.001359,0.001362,0.001377,0.001294,0.001266,0.001294,0.001325,0.001065,0.001217,0.001138,0.001325,0.001305,0.001307,0.001242,0.001115,0.001276,0.001337,0.001232,0.001163,0.001236,0.00133,0.001087,0.001142,0.001147,0.001082,0.00105,0.001222,0.00123,0.001218,0.001105,0.001188,0.001188,0.001188,0.001188,0.001188,0.001321,0.0012,0.001063,0.001063,0.001055,0.001086,0.001078,0.001227,0.000958,0.00114,0.001034,0.000647,0.001136,0.001034,0.001034,0.001153,0.001098,0.001034,0.001035,0.001048,0.001119,0.001045,0.0011,0.001034,0.0011,0.001064,0.001176,0.001176,0.001188,0.001264,0.001193,0.000827,2.9e-05],[0.024606,0.02432,0.02484,0.024346,0.024387,0.024342,0.02388,0.024175,0.022895,0.023923,0.024413,0.024458,0.023532,0.02352,0.024012,0.023923,0.024413,0.024538,0.023515,0.024162,0.023985,0.02361,0.024128,0.023349,0.022353,0.024374,0.02377,0.023767,0.024311,0.024374,0.024387,0.023932,0.024464,0.023201,0.024301,0.023994,0.024263,0.023241,0.023224,0.024564,0.024222,0.024093,0.024121,0.024216,0.023975,0.024349,0.022986,0.023215,0.023215,0.023215,0.023215,0.023215,0.023241,0.024096,0.023886,0.02432,0.024212,0.024,0.023387,0.02426,0.025084,0.021777,0.023865,0.023941,0.024128,0.005348,0.022694,0.024409,0.023911,0.023758,0.024022,0.024078,0.023917,0.023926,0.024056,0.023944,0.023991,0.024368,0.021944,0.021944,0.021944,0.021944,0.023972,0.024241,0.023975,0.024025,0.024053,0.021934,0.023755,0.023694,0.023822,0.023097,0.021775,0.023813,0.024238,0.023954,0.021934,0.022889,0.024053,0.02409,0.023143,0.024047,0.023761,0.023761,0.023932,0.023944,0.023947,0.023761,0.023767,0.023843,0.023776,0.023972,0.024071,0.024028,0.024028,0.023941,0.023761,0.023758,0.023941,0.024181,0.023855,0.024333,0.024112,0.023868,0.024112,0.023978,0.023917,0.023917,0.023929,0.023978,0.023852,0.023914,0.024075,0.023932,0.024084,0.024031,0.024128,0.023957,0.024087,0.023963,0.023758,0.023914,0.023907,0.023764,0.023544,0.024172,0.024103,0.024096,0.023166,0.023791,0.023852,0.023791,0.023852,0.023791,0.02447,0.023247,0.024419,0.024483,0.024429,0.023673,0.024168,0.024311,0.024685,0.025074,0.023512,0.023215,0.023975,0.023512,0.023512,0.024165,0.024235,0.023512,0.023526,0.02384,0.023991,0.023997,0.024103,0.023512,0.024103,0.023761,0.024458,0.024458,0.024561,0.024512,0.024087,0.023785,0.005348],[0.205298,0.183976,0.160622,0.176471,0.180233,0.165923,0.107764,0.167266,0.155,0.175472,0.145426,0.162162,0.185629,0.184524,0.122368,0.12,0.150729,0.198506,0.164894,0.157895,0.162445,0.151343,0.149518,0.147152,0.161318,0.140271,0.174321,0.169553,0.16188,0.160069,0.182711,0.174157,0.181996,0.146803,0.129526,0.151097,0.191161,0.152209,0.136564,0.153465,0.174648,0.147502,0.147502,0.13891,0.173346,0.191161,0.156698,0.119768,0.119768,0.119768,0.119768,0.119768,0.174321,0.125761,0.162445,0.153465,0.190184,0.155909,0.154357,0.197243,0.005348,0.084239,0.186186,0.122853,0.138085,0.005376,0.005405,0.172222,0.152459,0.139535,0.139535,0.144298,0.177481,0.166966,0.132384,0.163158,0.176303,0.167266,0.103853,0.09432,0.09432,0.09432,0.171429,0.11418,0.173023,0.176303,0.177143,0.173023,0.180583,0.14059,0.146572,0.182353,0.091897,0.143297,0.182711,0.172862,0.102933,0.095827,0.140909,0.189217,0.110648,0.151466,0.005348,0.005348,0.176471,0.155909,0.126273,0.005348,0.005348,0.185259,0.179537,0.180758,0.173346,0.172702,0.180758,0.180583,0.178674,0.175472,0.133238,0.180407,0.175306,0.162587,0.139222,0.139326,0.139222,0.162587,0.109027,0.166369,0.122208,0.162587,0.161039,0.161039,0.167266,0.109027,0.13891,0.16548,0.104377,0.122208,0.122127,0.161179,0.123016,0.109027,0.138806,0.122208,0.098361,0.108963,0.138806,0.098726,0.14441,0.155909,0.155909,0.155909,0.155909,0.155909,0.076198,0.076323,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.306931,0.277612,0.298555,0.266858,0.269956,0.256552,0.217036,0.255495,0.215029,0.24767,0.260504,0.262341,0.252033,0.261972,0.209224,0.195173,0.253752,0.290172,0.242188,0.231631,0.232791,0.206208,0.225455,0.193347,0.225182,0.231056,0.238462,0.236641,0.256552,0.263456,0.266858,0.259053,0.27193,0.214533,0.243137,0.216783,0.265335,0.214533,0.18111,0.255495,0.242188,0.216279,0.222488,0.256198,0.239075,0.268398,0.223022,0.173184,0.173184,0.173184,0.173184,0.173184,0.239382,0.225728,0.227941,0.255144,0.267626,0.241873,0.237245,0.283537,0.258333,0.098361,0.270349,0.222222,0.221165,0.104494,0.133333,0.249664,0.242503,0.238768,0.248331,0.229346,0.257618,0.227941,0.226277,0.23192,0.26087,0.254098,0.127835,0.113622,0.113622,0.113622,0.248,0.251691,0.253406,0.241245,0.265335,0.236041,0.246032,0.239691,0.202614,0.249664,0.108963,0.190574,0.259414,0.245059,0.126359,0.145768,0.243455,0.263456,0.172542,0.226829,0.173507,0.173507,0.254098,0.21934,0.221957,0.173507,0.173507,0.264957,0.254795,0.256552,0.248663,0.240621,0.258693,0.256198,0.252374,0.251351,0.221957,0.252717,0.253406,0.256198,0.240621,0.235443,0.240621,0.246358,0.198083,0.226277,0.211604,0.246358,0.24282,0.243137,0.231056,0.207358,0.237245,0.248663,0.229064,0.216279,0.229913,0.24734,0.202174,0.212329,0.213303,0.201299,0.195378,0.227384,0.228782,0.226553,0.205525,0.220903,0.220903,0.220903,0.220903,0.220903,0.245707,0.223289,0.197662,0.197662,0.196203,0.201954,0.200431,0.228221,0.178161,0.212087,0.192347,0.120388,0.211364,0.192347,0.192347,0.214533,0.204171,0.192347,0.192547,0.194969,0.208054,0.194357,0.20462,0.192347,0.20462,0.197872,0.218824,0.218824,0.220903,0.235145,0.221957,0.153846,0.005348],[0.271137,0.24767,0.277198,0.248996,0.228501,0.227384,0.231631,0.24031,0.223558,0.223827,0.225728,0.219858,0.237548,0.237852,0.218054,0.225455,0.258693,0.236041,0.224096,0.223022,0.230198,0.222222,0.223022,0.248663,0.217544,0.193347,0.216279,0.241873,0.246032,0.237852,0.230483,0.227662,0.228501,0.18111,0.181641,0.221957,0.233668,0.211124,0.224638,0.240933,0.243775,0.224367,0.224367,0.218054,0.222754,0.224638,0.184707,0.225728,0.225728,0.225728,0.225728,0.225728,0.217036,0.227941,0.214781,0.234848,0.23221,0.220903,0.186747,0.24282,0.005348,0.16548,0.224638,0.219599,0.20462,0.005376,0.005405,0.230769,0.227941,0.21831,0.21831,0.221692,0.223022,0.222222,0.220903,0.223289,0.221692,0.237852,0.185814,0.185814,0.185814,0.185814,0.220379,0.199785,0.226553,0.226002,0.224367,0.215777,0.229064,0.21934,0.221429,0.226553,0.16548,0.197452,0.223827,0.221165,0.17799,0.182532,0.219858,0.233375,0.186186,0.236641,0.005348,0.005348,0.222488,0.226277,0.21934,0.005348,0.005348,0.226277,0.226553,0.221692,0.220379,0.219599,0.220118,0.224367,0.226553,0.227384,0.220903,0.220903,0.219599,0.23192,0.21831,0.21831,0.21831,0.221429,0.217799,0.219599,0.217799,0.221429,0.220379,0.220379,0.220903,0.217799,0.217799,0.223022,0.219081,0.217799,0.217799,0.220379,0.218054,0.217799,0.218566,0.217799,0.217799,0.217799,0.220379,0.179537,0.200215,0.199143,0.199143,0.199143,0.199143,0.199143,0.181287,0.181641,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(atividade_df_closseness, by=list(atividade_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(atividade_df_closseness, by=list(atividade_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-8f6a3ab34173dd0e9ad6" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-8f6a3ab34173dd0e9ad6">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000134\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001104\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8e-05\" data-max=\"0.000352\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.00165\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.5e-05\" data-max=\"0.000314\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.02484\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.6e-05\" data-max=\"0.004222\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.205298\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.014754\" data-max=\"0.065504\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.306931\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00465\" data-max=\"0.058373\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.277198\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.007077\" data-max=\"0.107602\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[0.000129,0.000128,0.00013,0.000131,0.000128,0.00013,0.000129,0.000134,0.00013,0.00013,0.000131,0.00012,0.000128,0.000131,0.000131,0.000128,2.9e-05,0.00013,0.000132,0.000126,0.000132,0.000129,0.000129],[4e-06,4e-06,null,null,null,null,1e-06,null,null,null,null,2.3e-05,3e-06,0,0,4e-06,null,null,null,2e-06,null,1e-06,1e-06],[0.000898,0.000281,0.000849,0.000989,0.000579,0.00106,0.00079,0.000864,0.000804,0.000746,0.000899,0.000757,0.000893,0.000874,0.00089,0.000806,2.9e-05,0.001017,0.001104,0.000461,0.001067,0.00069,0.000824],[0.000107,0.000352,null,null,null,null,0.000178,null,null,null,null,0.00025,0.000141,0.000102,8e-05,0.000129,null,null,null,0.000323,null,0.000307,0.000142],[0.00129,0.001086,0.001245,0.001493,0.001167,0.001524,0.001249,0.001605,0.001212,0.00123,0.001366,0.001103,0.001286,0.0014,0.001398,0.001235,2.9e-05,0.001416,0.00165,0.000964,0.00156,0.001212,0.001278],[0.000179,0.000207,null,null,null,null,2.5e-05,null,null,null,null,0.000314,0.000173,4.3e-05,4.7e-05,0.000139,null,null,null,6.9e-05,null,0.000102,0.000101],[0.023951,0.023734,0.024162,0.02432,0.02388,0.02426,0.023975,0.02484,0.024128,0.024103,0.024368,0.022392,0.023806,0.024362,0.024381,0.02381,0.005348,0.02409,0.024606,0.023459,0.024538,0.023998,0.023959],[0.00069,0.00076,null,null,null,null,0.000203,null,null,null,null,0.004222,0.000405,5.1e-05,3.6e-05,0.000642,null,null,null,0.000299,null,0.000219,0.000129],[0.167018,0.052217,0.157895,0.183976,0.107764,0.197243,0.146987,0.160622,0.149518,0.138806,0.167266,0.140783,0.166013,0.162541,0.165628,0.149843,0.005348,0.189217,0.205298,0.08575,0.198506,0.128367,0.153262],[0.019921,0.065503,null,null,null,null,0.033121,null,null,null,null,0.046417,0.026179,0.019013,0.014754,0.024059,null,null,null,0.060163,null,0.057071,0.026335],[0.239881,0.201982,0.231631,0.277612,0.217036,0.283537,0.232272,0.298555,0.225455,0.228782,0.254098,0.205066,0.239126,0.260361,0.260087,0.229777,0.005348,0.263456,0.306931,0.17923,0.290172,0.225469,0.237696],[0.033263,0.038488,null,null,null,null,0.00465,null,null,null,null,0.058373,0.03231,0.007913,0.008661,0.025883,null,null,null,0.012855,null,0.018946,0.018771],[0.224164,0.077408,0.223022,0.24767,0.231631,0.24282,0.229957,0.277198,0.223022,0.220379,0.237852,0.199917,0.23111,0.227786,0.238193,0.223723,0.005348,0.233375,0.271137,0.150937,0.236041,0.180379,0.220969],[0.022102,0.095968,null,null,null,null,0.012039,null,null,null,null,0.052309,0.009535,0.019836,0.017763,0.014427,null,null,null,0.107602,null,0.078386,0.007077]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
atividade_df_closseness <- data.frame(
atividade_incloseness,
atividade_outcloseness,
atividade_totalcloseness,
atividade_incloseness_n,
atividade_outcloseness_n,
atividade_totalcloseness_n,
atividade_centr_closeness) %>% round(6)

#Adding type
atividade_df_closseness <-cbind(atividade_df_closseness, V(atividade)$TIPO1)

#Adding names
names(atividade_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
atividade_df_closseness<-atividade_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(atividade_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-222191cab354d86f761b" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-222191cab354d86f761b">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000135\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001104\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.00165\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.025084\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.205298\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.306931\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.277198\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[0.000132,0.000131,0.000134,0.000131,0.000131,0.000131,0.000128,0.00013,0.000123,0.000129,0.000131,0.000131,0.000127,0.000126,0.000129,0.000129,0.000131,0.000132,0.000126,0.00013,0.000129,0.000127,0.00013,0.000126,0.00012,0.000131,0.000128,0.000128,0.000131,0.000131,0.000131,0.000129,0.000132,0.000125,0.000131,0.000129,0.00013,0.000125,0.000125,0.000132,0.00013,0.00013,0.00013,0.00013,0.000129,0.000131,0.000124,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.00013,0.000128,0.000131,0.00013,0.000129,0.000126,0.00013,0.000135,0.000117,0.000128,0.000129,0.00013,2.9e-05,0.000122,0.000131,0.000129,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000131,0.000118,0.000118,0.000118,0.000118,0.000129,0.00013,0.000129,0.000129,0.000129,0.000118,0.000128,0.000127,0.000128,0.000124,0.000117,0.000128,0.00013,0.000129,0.000118,0.000123,0.000129,0.00013,0.000124,0.000129,0.000128,0.000128,0.000129,0.000129,0.000129,0.000128,0.000128,0.000128,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.000128,0.000128,0.000129,0.00013,0.000128,0.000131,0.00013,0.000128,0.00013,0.000129,0.000129,0.000129,0.000129,0.000129,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.00013,0.000129,0.00013,0.000129,0.000128,0.000129,0.000129,0.000128,0.000127,0.00013,0.00013,0.00013,0.000125,0.000128,0.000128,0.000128,0.000128,0.000128,0.000132,0.000125,0.000131,0.000132,0.000131,0.000127,0.00013,0.000131,0.000133,0.000135,0.000126,0.000125,0.000129,0.000126,0.000126,0.00013,0.00013,0.000126,0.000126,0.000128,0.000129,0.000129,0.00013,0.000126,0.00013,0.000128,0.000131,0.000131,0.000132,0.000132,0.00013,0.000128,2.9e-05],[0.001104,0.000989,0.000864,0.000949,0.000969,0.000892,0.000579,0.000899,0.000833,0.000943,0.000782,0.000872,0.000998,0.000992,0.000658,0.000645,0.00081,0.001067,0.000887,0.000849,0.000873,0.000814,0.000804,0.000791,0.000867,0.000754,0.000937,0.000912,0.00087,0.000861,0.000982,0.000936,0.000978,0.000789,0.000696,0.000812,0.001028,0.000818,0.000734,0.000825,0.000939,0.000793,0.000793,0.000747,0.000932,0.001028,0.000842,0.000644,0.000644,0.000644,0.000644,0.000644,0.000937,0.000676,0.000873,0.000825,0.001022,0.000838,0.00083,0.00106,2.9e-05,0.000453,0.001001,0.000661,0.000742,2.9e-05,2.9e-05,0.000926,0.00082,0.00075,0.00075,0.000776,0.000954,0.000898,0.000712,0.000877,0.000948,0.000899,0.000558,0.000507,0.000507,0.000507,0.000922,0.000614,0.00093,0.000948,0.000952,0.00093,0.000971,0.000756,0.000788,0.00098,0.000494,0.00077,0.000982,0.000929,0.000553,0.000515,0.000758,0.001017,0.000595,0.000814,2.9e-05,2.9e-05,0.000949,0.000838,0.000679,2.9e-05,2.9e-05,0.000996,0.000965,0.000972,0.000932,0.000929,0.000972,0.000971,0.000961,0.000943,0.000716,0.00097,0.000943,0.000874,0.000749,0.000749,0.000749,0.000874,0.000586,0.000894,0.000657,0.000874,0.000866,0.000866,0.000899,0.000586,0.000747,0.00089,0.000561,0.000657,0.000657,0.000867,0.000661,0.000586,0.000746,0.000657,0.000529,0.000586,0.000746,0.000531,0.000776,0.000838,0.000838,0.000838,0.000838,0.000838,0.00041,0.00041,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.00165,0.001493,0.001605,0.001435,0.001451,0.001379,0.001167,0.001374,0.001156,0.001332,0.001401,0.00141,0.001355,0.001408,0.001125,0.001049,0.001364,0.00156,0.001302,0.001245,0.001252,0.001109,0.001212,0.00104,0.001211,0.001242,0.001282,0.001272,0.001379,0.001416,0.001435,0.001393,0.001462,0.001153,0.001307,0.001166,0.001427,0.001153,0.000974,0.001374,0.001302,0.001163,0.001196,0.001377,0.001285,0.001443,0.001199,0.000931,0.000931,0.000931,0.000931,0.000931,0.001287,0.001214,0.001225,0.001372,0.001439,0.0013,0.001276,0.001524,0.001389,0.000529,0.001453,0.001195,0.001189,0.000562,0.000717,0.001342,0.001304,0.001284,0.001335,0.001233,0.001385,0.001225,0.001217,0.001247,0.001403,0.001366,0.000687,0.000611,0.000611,0.000611,0.001333,0.001353,0.001362,0.001297,0.001427,0.001269,0.001323,0.001289,0.001089,0.001342,0.000586,0.001025,0.001395,0.001318,0.000679,0.000784,0.001309,0.001416,0.000928,0.00122,0.000933,0.000933,0.001366,0.001179,0.001193,0.000933,0.000933,0.001425,0.00137,0.001379,0.001337,0.001294,0.001391,0.001377,0.001357,0.001351,0.001193,0.001359,0.001362,0.001377,0.001294,0.001266,0.001294,0.001325,0.001065,0.001217,0.001138,0.001325,0.001305,0.001307,0.001242,0.001115,0.001276,0.001337,0.001232,0.001163,0.001236,0.00133,0.001087,0.001142,0.001147,0.001082,0.00105,0.001222,0.00123,0.001218,0.001105,0.001188,0.001188,0.001188,0.001188,0.001188,0.001321,0.0012,0.001063,0.001063,0.001055,0.001086,0.001078,0.001227,0.000958,0.00114,0.001034,0.000647,0.001136,0.001034,0.001034,0.001153,0.001098,0.001034,0.001035,0.001048,0.001119,0.001045,0.0011,0.001034,0.0011,0.001064,0.001176,0.001176,0.001188,0.001264,0.001193,0.000827,2.9e-05],[0.024606,0.02432,0.02484,0.024346,0.024387,0.024342,0.02388,0.024175,0.022895,0.023923,0.024413,0.024458,0.023532,0.02352,0.024012,0.023923,0.024413,0.024538,0.023515,0.024162,0.023985,0.02361,0.024128,0.023349,0.022353,0.024374,0.02377,0.023767,0.024311,0.024374,0.024387,0.023932,0.024464,0.023201,0.024301,0.023994,0.024263,0.023241,0.023224,0.024564,0.024222,0.024093,0.024121,0.024216,0.023975,0.024349,0.022986,0.023215,0.023215,0.023215,0.023215,0.023215,0.023241,0.024096,0.023886,0.02432,0.024212,0.024,0.023387,0.02426,0.025084,0.021777,0.023865,0.023941,0.024128,0.005348,0.022694,0.024409,0.023911,0.023758,0.024022,0.024078,0.023917,0.023926,0.024056,0.023944,0.023991,0.024368,0.021944,0.021944,0.021944,0.021944,0.023972,0.024241,0.023975,0.024025,0.024053,0.021934,0.023755,0.023694,0.023822,0.023097,0.021775,0.023813,0.024238,0.023954,0.021934,0.022889,0.024053,0.02409,0.023143,0.024047,0.023761,0.023761,0.023932,0.023944,0.023947,0.023761,0.023767,0.023843,0.023776,0.023972,0.024071,0.024028,0.024028,0.023941,0.023761,0.023758,0.023941,0.024181,0.023855,0.024333,0.024112,0.023868,0.024112,0.023978,0.023917,0.023917,0.023929,0.023978,0.023852,0.023914,0.024075,0.023932,0.024084,0.024031,0.024128,0.023957,0.024087,0.023963,0.023758,0.023914,0.023907,0.023764,0.023544,0.024172,0.024103,0.024096,0.023166,0.023791,0.023852,0.023791,0.023852,0.023791,0.02447,0.023247,0.024419,0.024483,0.024429,0.023673,0.024168,0.024311,0.024685,0.025074,0.023512,0.023215,0.023975,0.023512,0.023512,0.024165,0.024235,0.023512,0.023526,0.02384,0.023991,0.023997,0.024103,0.023512,0.024103,0.023761,0.024458,0.024458,0.024561,0.024512,0.024087,0.023785,0.005348],[0.205298,0.183976,0.160622,0.176471,0.180233,0.165923,0.107764,0.167266,0.155,0.175472,0.145426,0.162162,0.185629,0.184524,0.122368,0.12,0.150729,0.198506,0.164894,0.157895,0.162445,0.151343,0.149518,0.147152,0.161318,0.140271,0.174321,0.169553,0.16188,0.160069,0.182711,0.174157,0.181996,0.146803,0.129526,0.151097,0.191161,0.152209,0.136564,0.153465,0.174648,0.147502,0.147502,0.13891,0.173346,0.191161,0.156698,0.119768,0.119768,0.119768,0.119768,0.119768,0.174321,0.125761,0.162445,0.153465,0.190184,0.155909,0.154357,0.197243,0.005348,0.084239,0.186186,0.122853,0.138085,0.005376,0.005405,0.172222,0.152459,0.139535,0.139535,0.144298,0.177481,0.166966,0.132384,0.163158,0.176303,0.167266,0.103853,0.09432,0.09432,0.09432,0.171429,0.11418,0.173023,0.176303,0.177143,0.173023,0.180583,0.14059,0.146572,0.182353,0.091897,0.143297,0.182711,0.172862,0.102933,0.095827,0.140909,0.189217,0.110648,0.151466,0.005348,0.005348,0.176471,0.155909,0.126273,0.005348,0.005348,0.185259,0.179537,0.180758,0.173346,0.172702,0.180758,0.180583,0.178674,0.175472,0.133238,0.180407,0.175306,0.162587,0.139222,0.139326,0.139222,0.162587,0.109027,0.166369,0.122208,0.162587,0.161039,0.161039,0.167266,0.109027,0.13891,0.16548,0.104377,0.122208,0.122127,0.161179,0.123016,0.109027,0.138806,0.122208,0.098361,0.108963,0.138806,0.098726,0.14441,0.155909,0.155909,0.155909,0.155909,0.155909,0.076198,0.076323,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.306931,0.277612,0.298555,0.266858,0.269956,0.256552,0.217036,0.255495,0.215029,0.24767,0.260504,0.262341,0.252033,0.261972,0.209224,0.195173,0.253752,0.290172,0.242188,0.231631,0.232791,0.206208,0.225455,0.193347,0.225182,0.231056,0.238462,0.236641,0.256552,0.263456,0.266858,0.259053,0.27193,0.214533,0.243137,0.216783,0.265335,0.214533,0.18111,0.255495,0.242188,0.216279,0.222488,0.256198,0.239075,0.268398,0.223022,0.173184,0.173184,0.173184,0.173184,0.173184,0.239382,0.225728,0.227941,0.255144,0.267626,0.241873,0.237245,0.283537,0.258333,0.098361,0.270349,0.222222,0.221165,0.104494,0.133333,0.249664,0.242503,0.238768,0.248331,0.229346,0.257618,0.227941,0.226277,0.23192,0.26087,0.254098,0.127835,0.113622,0.113622,0.113622,0.248,0.251691,0.253406,0.241245,0.265335,0.236041,0.246032,0.239691,0.202614,0.249664,0.108963,0.190574,0.259414,0.245059,0.126359,0.145768,0.243455,0.263456,0.172542,0.226829,0.173507,0.173507,0.254098,0.21934,0.221957,0.173507,0.173507,0.264957,0.254795,0.256552,0.248663,0.240621,0.258693,0.256198,0.252374,0.251351,0.221957,0.252717,0.253406,0.256198,0.240621,0.235443,0.240621,0.246358,0.198083,0.226277,0.211604,0.246358,0.24282,0.243137,0.231056,0.207358,0.237245,0.248663,0.229064,0.216279,0.229913,0.24734,0.202174,0.212329,0.213303,0.201299,0.195378,0.227384,0.228782,0.226553,0.205525,0.220903,0.220903,0.220903,0.220903,0.220903,0.245707,0.223289,0.197662,0.197662,0.196203,0.201954,0.200431,0.228221,0.178161,0.212087,0.192347,0.120388,0.211364,0.192347,0.192347,0.214533,0.204171,0.192347,0.192547,0.194969,0.208054,0.194357,0.20462,0.192347,0.20462,0.197872,0.218824,0.218824,0.220903,0.235145,0.221957,0.153846,0.005348],[0.271137,0.24767,0.277198,0.248996,0.228501,0.227384,0.231631,0.24031,0.223558,0.223827,0.225728,0.219858,0.237548,0.237852,0.218054,0.225455,0.258693,0.236041,0.224096,0.223022,0.230198,0.222222,0.223022,0.248663,0.217544,0.193347,0.216279,0.241873,0.246032,0.237852,0.230483,0.227662,0.228501,0.18111,0.181641,0.221957,0.233668,0.211124,0.224638,0.240933,0.243775,0.224367,0.224367,0.218054,0.222754,0.224638,0.184707,0.225728,0.225728,0.225728,0.225728,0.225728,0.217036,0.227941,0.214781,0.234848,0.23221,0.220903,0.186747,0.24282,0.005348,0.16548,0.224638,0.219599,0.20462,0.005376,0.005405,0.230769,0.227941,0.21831,0.21831,0.221692,0.223022,0.222222,0.220903,0.223289,0.221692,0.237852,0.185814,0.185814,0.185814,0.185814,0.220379,0.199785,0.226553,0.226002,0.224367,0.215777,0.229064,0.21934,0.221429,0.226553,0.16548,0.197452,0.223827,0.221165,0.17799,0.182532,0.219858,0.233375,0.186186,0.236641,0.005348,0.005348,0.222488,0.226277,0.21934,0.005348,0.005348,0.226277,0.226553,0.221692,0.220379,0.219599,0.220118,0.224367,0.226553,0.227384,0.220903,0.220903,0.219599,0.23192,0.21831,0.21831,0.21831,0.221429,0.217799,0.219599,0.217799,0.221429,0.220379,0.220379,0.220903,0.217799,0.217799,0.223022,0.219081,0.217799,0.217799,0.220379,0.218054,0.217799,0.218566,0.217799,0.217799,0.217799,0.220379,0.179537,0.200215,0.199143,0.199143,0.199143,0.199143,0.199143,0.181287,0.181641,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(atividade_df_closseness, by=list(atividade_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(atividade_df_closseness, by=list(atividade_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-9153d79c19a058e96b95" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-9153d79c19a058e96b95">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000126\" data-max=\"0.000128\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-05\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000492\" data-max=\"0.000767\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000247\" data-max=\"0.000391\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00112\" data-max=\"0.001245\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000197\" data-max=\"0.000234\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.023417\" data-max=\"0.023826\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00182\" data-max=\"0.002201\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.091577\" data-max=\"0.142688\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.04591\" data-max=\"0.072707\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.208256\" data-max=\"0.231553\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.036587\" data-max=\"0.043552\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.132754\" data-max=\"0.207558\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.058904\" data-max=\"0.101806\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[0.000128,0.000126],[1e-05,1.2e-05],[0.000767,0.000492],[0.000247,0.000391],[0.001245,0.00112],[0.000197,0.000234],[0.023826,0.023417],[0.00182,0.002201],[0.142688,0.091577],[0.04591,0.072707],[0.231553,0.208256],[0.036587,0.043552],[0.207558,0.132754],[0.058904,0.101806]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
atividade_df_closseness <- data.frame(
atividade_incloseness,
atividade_outcloseness,
atividade_totalcloseness,
atividade_incloseness_n,
atividade_outcloseness_n,
atividade_totalcloseness_n,
atividade_centr_closeness) %>% round(6)

#Adding type
atividade_df_closseness <-cbind(atividade_df_closseness, V(atividade)$TIPO2)

#Adding names
names(atividade_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
atividade_df_closseness<-atividade_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(atividade_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-63d7125179e43e7afbac" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-63d7125179e43e7afbac">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000135\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001104\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.00165\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.025084\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.205298\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.306931\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.277198\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[0.000132,0.000131,0.000134,0.000131,0.000131,0.000131,0.000128,0.00013,0.000123,0.000129,0.000131,0.000131,0.000127,0.000126,0.000129,0.000129,0.000131,0.000132,0.000126,0.00013,0.000129,0.000127,0.00013,0.000126,0.00012,0.000131,0.000128,0.000128,0.000131,0.000131,0.000131,0.000129,0.000132,0.000125,0.000131,0.000129,0.00013,0.000125,0.000125,0.000132,0.00013,0.00013,0.00013,0.00013,0.000129,0.000131,0.000124,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.00013,0.000128,0.000131,0.00013,0.000129,0.000126,0.00013,0.000135,0.000117,0.000128,0.000129,0.00013,2.9e-05,0.000122,0.000131,0.000129,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000131,0.000118,0.000118,0.000118,0.000118,0.000129,0.00013,0.000129,0.000129,0.000129,0.000118,0.000128,0.000127,0.000128,0.000124,0.000117,0.000128,0.00013,0.000129,0.000118,0.000123,0.000129,0.00013,0.000124,0.000129,0.000128,0.000128,0.000129,0.000129,0.000129,0.000128,0.000128,0.000128,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.000128,0.000128,0.000129,0.00013,0.000128,0.000131,0.00013,0.000128,0.00013,0.000129,0.000129,0.000129,0.000129,0.000129,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.00013,0.000129,0.00013,0.000129,0.000128,0.000129,0.000129,0.000128,0.000127,0.00013,0.00013,0.00013,0.000125,0.000128,0.000128,0.000128,0.000128,0.000128,0.000132,0.000125,0.000131,0.000132,0.000131,0.000127,0.00013,0.000131,0.000133,0.000135,0.000126,0.000125,0.000129,0.000126,0.000126,0.00013,0.00013,0.000126,0.000126,0.000128,0.000129,0.000129,0.00013,0.000126,0.00013,0.000128,0.000131,0.000131,0.000132,0.000132,0.00013,0.000128,2.9e-05],[0.001104,0.000989,0.000864,0.000949,0.000969,0.000892,0.000579,0.000899,0.000833,0.000943,0.000782,0.000872,0.000998,0.000992,0.000658,0.000645,0.00081,0.001067,0.000887,0.000849,0.000873,0.000814,0.000804,0.000791,0.000867,0.000754,0.000937,0.000912,0.00087,0.000861,0.000982,0.000936,0.000978,0.000789,0.000696,0.000812,0.001028,0.000818,0.000734,0.000825,0.000939,0.000793,0.000793,0.000747,0.000932,0.001028,0.000842,0.000644,0.000644,0.000644,0.000644,0.000644,0.000937,0.000676,0.000873,0.000825,0.001022,0.000838,0.00083,0.00106,2.9e-05,0.000453,0.001001,0.000661,0.000742,2.9e-05,2.9e-05,0.000926,0.00082,0.00075,0.00075,0.000776,0.000954,0.000898,0.000712,0.000877,0.000948,0.000899,0.000558,0.000507,0.000507,0.000507,0.000922,0.000614,0.00093,0.000948,0.000952,0.00093,0.000971,0.000756,0.000788,0.00098,0.000494,0.00077,0.000982,0.000929,0.000553,0.000515,0.000758,0.001017,0.000595,0.000814,2.9e-05,2.9e-05,0.000949,0.000838,0.000679,2.9e-05,2.9e-05,0.000996,0.000965,0.000972,0.000932,0.000929,0.000972,0.000971,0.000961,0.000943,0.000716,0.00097,0.000943,0.000874,0.000749,0.000749,0.000749,0.000874,0.000586,0.000894,0.000657,0.000874,0.000866,0.000866,0.000899,0.000586,0.000747,0.00089,0.000561,0.000657,0.000657,0.000867,0.000661,0.000586,0.000746,0.000657,0.000529,0.000586,0.000746,0.000531,0.000776,0.000838,0.000838,0.000838,0.000838,0.000838,0.00041,0.00041,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.00165,0.001493,0.001605,0.001435,0.001451,0.001379,0.001167,0.001374,0.001156,0.001332,0.001401,0.00141,0.001355,0.001408,0.001125,0.001049,0.001364,0.00156,0.001302,0.001245,0.001252,0.001109,0.001212,0.00104,0.001211,0.001242,0.001282,0.001272,0.001379,0.001416,0.001435,0.001393,0.001462,0.001153,0.001307,0.001166,0.001427,0.001153,0.000974,0.001374,0.001302,0.001163,0.001196,0.001377,0.001285,0.001443,0.001199,0.000931,0.000931,0.000931,0.000931,0.000931,0.001287,0.001214,0.001225,0.001372,0.001439,0.0013,0.001276,0.001524,0.001389,0.000529,0.001453,0.001195,0.001189,0.000562,0.000717,0.001342,0.001304,0.001284,0.001335,0.001233,0.001385,0.001225,0.001217,0.001247,0.001403,0.001366,0.000687,0.000611,0.000611,0.000611,0.001333,0.001353,0.001362,0.001297,0.001427,0.001269,0.001323,0.001289,0.001089,0.001342,0.000586,0.001025,0.001395,0.001318,0.000679,0.000784,0.001309,0.001416,0.000928,0.00122,0.000933,0.000933,0.001366,0.001179,0.001193,0.000933,0.000933,0.001425,0.00137,0.001379,0.001337,0.001294,0.001391,0.001377,0.001357,0.001351,0.001193,0.001359,0.001362,0.001377,0.001294,0.001266,0.001294,0.001325,0.001065,0.001217,0.001138,0.001325,0.001305,0.001307,0.001242,0.001115,0.001276,0.001337,0.001232,0.001163,0.001236,0.00133,0.001087,0.001142,0.001147,0.001082,0.00105,0.001222,0.00123,0.001218,0.001105,0.001188,0.001188,0.001188,0.001188,0.001188,0.001321,0.0012,0.001063,0.001063,0.001055,0.001086,0.001078,0.001227,0.000958,0.00114,0.001034,0.000647,0.001136,0.001034,0.001034,0.001153,0.001098,0.001034,0.001035,0.001048,0.001119,0.001045,0.0011,0.001034,0.0011,0.001064,0.001176,0.001176,0.001188,0.001264,0.001193,0.000827,2.9e-05],[0.024606,0.02432,0.02484,0.024346,0.024387,0.024342,0.02388,0.024175,0.022895,0.023923,0.024413,0.024458,0.023532,0.02352,0.024012,0.023923,0.024413,0.024538,0.023515,0.024162,0.023985,0.02361,0.024128,0.023349,0.022353,0.024374,0.02377,0.023767,0.024311,0.024374,0.024387,0.023932,0.024464,0.023201,0.024301,0.023994,0.024263,0.023241,0.023224,0.024564,0.024222,0.024093,0.024121,0.024216,0.023975,0.024349,0.022986,0.023215,0.023215,0.023215,0.023215,0.023215,0.023241,0.024096,0.023886,0.02432,0.024212,0.024,0.023387,0.02426,0.025084,0.021777,0.023865,0.023941,0.024128,0.005348,0.022694,0.024409,0.023911,0.023758,0.024022,0.024078,0.023917,0.023926,0.024056,0.023944,0.023991,0.024368,0.021944,0.021944,0.021944,0.021944,0.023972,0.024241,0.023975,0.024025,0.024053,0.021934,0.023755,0.023694,0.023822,0.023097,0.021775,0.023813,0.024238,0.023954,0.021934,0.022889,0.024053,0.02409,0.023143,0.024047,0.023761,0.023761,0.023932,0.023944,0.023947,0.023761,0.023767,0.023843,0.023776,0.023972,0.024071,0.024028,0.024028,0.023941,0.023761,0.023758,0.023941,0.024181,0.023855,0.024333,0.024112,0.023868,0.024112,0.023978,0.023917,0.023917,0.023929,0.023978,0.023852,0.023914,0.024075,0.023932,0.024084,0.024031,0.024128,0.023957,0.024087,0.023963,0.023758,0.023914,0.023907,0.023764,0.023544,0.024172,0.024103,0.024096,0.023166,0.023791,0.023852,0.023791,0.023852,0.023791,0.02447,0.023247,0.024419,0.024483,0.024429,0.023673,0.024168,0.024311,0.024685,0.025074,0.023512,0.023215,0.023975,0.023512,0.023512,0.024165,0.024235,0.023512,0.023526,0.02384,0.023991,0.023997,0.024103,0.023512,0.024103,0.023761,0.024458,0.024458,0.024561,0.024512,0.024087,0.023785,0.005348],[0.205298,0.183976,0.160622,0.176471,0.180233,0.165923,0.107764,0.167266,0.155,0.175472,0.145426,0.162162,0.185629,0.184524,0.122368,0.12,0.150729,0.198506,0.164894,0.157895,0.162445,0.151343,0.149518,0.147152,0.161318,0.140271,0.174321,0.169553,0.16188,0.160069,0.182711,0.174157,0.181996,0.146803,0.129526,0.151097,0.191161,0.152209,0.136564,0.153465,0.174648,0.147502,0.147502,0.13891,0.173346,0.191161,0.156698,0.119768,0.119768,0.119768,0.119768,0.119768,0.174321,0.125761,0.162445,0.153465,0.190184,0.155909,0.154357,0.197243,0.005348,0.084239,0.186186,0.122853,0.138085,0.005376,0.005405,0.172222,0.152459,0.139535,0.139535,0.144298,0.177481,0.166966,0.132384,0.163158,0.176303,0.167266,0.103853,0.09432,0.09432,0.09432,0.171429,0.11418,0.173023,0.176303,0.177143,0.173023,0.180583,0.14059,0.146572,0.182353,0.091897,0.143297,0.182711,0.172862,0.102933,0.095827,0.140909,0.189217,0.110648,0.151466,0.005348,0.005348,0.176471,0.155909,0.126273,0.005348,0.005348,0.185259,0.179537,0.180758,0.173346,0.172702,0.180758,0.180583,0.178674,0.175472,0.133238,0.180407,0.175306,0.162587,0.139222,0.139326,0.139222,0.162587,0.109027,0.166369,0.122208,0.162587,0.161039,0.161039,0.167266,0.109027,0.13891,0.16548,0.104377,0.122208,0.122127,0.161179,0.123016,0.109027,0.138806,0.122208,0.098361,0.108963,0.138806,0.098726,0.14441,0.155909,0.155909,0.155909,0.155909,0.155909,0.076198,0.076323,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.306931,0.277612,0.298555,0.266858,0.269956,0.256552,0.217036,0.255495,0.215029,0.24767,0.260504,0.262341,0.252033,0.261972,0.209224,0.195173,0.253752,0.290172,0.242188,0.231631,0.232791,0.206208,0.225455,0.193347,0.225182,0.231056,0.238462,0.236641,0.256552,0.263456,0.266858,0.259053,0.27193,0.214533,0.243137,0.216783,0.265335,0.214533,0.18111,0.255495,0.242188,0.216279,0.222488,0.256198,0.239075,0.268398,0.223022,0.173184,0.173184,0.173184,0.173184,0.173184,0.239382,0.225728,0.227941,0.255144,0.267626,0.241873,0.237245,0.283537,0.258333,0.098361,0.270349,0.222222,0.221165,0.104494,0.133333,0.249664,0.242503,0.238768,0.248331,0.229346,0.257618,0.227941,0.226277,0.23192,0.26087,0.254098,0.127835,0.113622,0.113622,0.113622,0.248,0.251691,0.253406,0.241245,0.265335,0.236041,0.246032,0.239691,0.202614,0.249664,0.108963,0.190574,0.259414,0.245059,0.126359,0.145768,0.243455,0.263456,0.172542,0.226829,0.173507,0.173507,0.254098,0.21934,0.221957,0.173507,0.173507,0.264957,0.254795,0.256552,0.248663,0.240621,0.258693,0.256198,0.252374,0.251351,0.221957,0.252717,0.253406,0.256198,0.240621,0.235443,0.240621,0.246358,0.198083,0.226277,0.211604,0.246358,0.24282,0.243137,0.231056,0.207358,0.237245,0.248663,0.229064,0.216279,0.229913,0.24734,0.202174,0.212329,0.213303,0.201299,0.195378,0.227384,0.228782,0.226553,0.205525,0.220903,0.220903,0.220903,0.220903,0.220903,0.245707,0.223289,0.197662,0.197662,0.196203,0.201954,0.200431,0.228221,0.178161,0.212087,0.192347,0.120388,0.211364,0.192347,0.192347,0.214533,0.204171,0.192347,0.192547,0.194969,0.208054,0.194357,0.20462,0.192347,0.20462,0.197872,0.218824,0.218824,0.220903,0.235145,0.221957,0.153846,0.005348],[0.271137,0.24767,0.277198,0.248996,0.228501,0.227384,0.231631,0.24031,0.223558,0.223827,0.225728,0.219858,0.237548,0.237852,0.218054,0.225455,0.258693,0.236041,0.224096,0.223022,0.230198,0.222222,0.223022,0.248663,0.217544,0.193347,0.216279,0.241873,0.246032,0.237852,0.230483,0.227662,0.228501,0.18111,0.181641,0.221957,0.233668,0.211124,0.224638,0.240933,0.243775,0.224367,0.224367,0.218054,0.222754,0.224638,0.184707,0.225728,0.225728,0.225728,0.225728,0.225728,0.217036,0.227941,0.214781,0.234848,0.23221,0.220903,0.186747,0.24282,0.005348,0.16548,0.224638,0.219599,0.20462,0.005376,0.005405,0.230769,0.227941,0.21831,0.21831,0.221692,0.223022,0.222222,0.220903,0.223289,0.221692,0.237852,0.185814,0.185814,0.185814,0.185814,0.220379,0.199785,0.226553,0.226002,0.224367,0.215777,0.229064,0.21934,0.221429,0.226553,0.16548,0.197452,0.223827,0.221165,0.17799,0.182532,0.219858,0.233375,0.186186,0.236641,0.005348,0.005348,0.222488,0.226277,0.21934,0.005348,0.005348,0.226277,0.226553,0.221692,0.220379,0.219599,0.220118,0.224367,0.226553,0.227384,0.220903,0.220903,0.219599,0.23192,0.21831,0.21831,0.21831,0.221429,0.217799,0.219599,0.217799,0.221429,0.220379,0.220379,0.220903,0.217799,0.217799,0.223022,0.219081,0.217799,0.217799,0.220379,0.218054,0.217799,0.218566,0.217799,0.217799,0.217799,0.220379,0.179537,0.200215,0.199143,0.199143,0.199143,0.199143,0.199143,0.181287,0.181641,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(atividade_df_closseness, by=list(atividade_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(atividade_df_closseness, by=list(atividade_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-349a2ba715b7090f3ea0" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-349a2ba715b7090f3ea0">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000126\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000486\" data-max=\"0.00089\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8.8e-05\" data-max=\"0.000389\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001115\" data-max=\"0.001395\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.9e-05\" data-max=\"0.000233\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.023408\" data-max=\"0.024364\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6.3e-05\" data-max=\"0.002214\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.090437\" data-max=\"0.165578\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.016399\" data-max=\"0.072398\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.207468\" data-max=\"0.259448\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.009032\" data-max=\"0.043341\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.131515\" data-max=\"0.231652\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.017396\" data-max=\"0.101859\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[0.000131,0.000128,0.000126],[0,1e-05,1.2e-05],[0.00089,0.00075,0.000486],[8.8e-05,0.000258,0.000389],[0.001395,0.001223,0.001115],[4.9e-05,0.000201,0.000233],[0.024364,0.023742,0.023408],[6.3e-05,0.001939,0.002214],[0.165578,0.139438,0.090437],[0.016399,0.048089,0.072398],[0.259448,0.227508,0.207468],[0.009032,0.037362,0.043341],[0.231652,0.203945,0.131515],[0.017396,0.062013,0.101859]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/atividade_data.RData")
```

