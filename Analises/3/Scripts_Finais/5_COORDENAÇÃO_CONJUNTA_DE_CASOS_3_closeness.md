# SNA Closeness 5_COORDENAÇÃO CONJUNTA DE CASOS (var3)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 5_COORDENAÇÃO CONJUNTA DE CASOS (var3)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var3_data.RData")
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
#var3<-simplify(var3) #Simplify
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
V(var3)$incloseness <- closeness(var3, mode = "in", weights = E(var3)$var3) %>% round(6)
V(var3)$outcloseness <- closeness(var3, mode = "out", weights = E(var3)$var3) %>% round(6)
V(var3)$totalcloseness <- closeness(var3, mode = "total", weights = E(var3)$var3) %>% round(4)
```

###Saving to Environment

```r
var3_incloseness<- closeness(var3, mode = "in", weights = E(var3)$var3) %>% round(6)
var3_outcloseness<- closeness(var3, mode = "out", weights = E(var3)$var3) %>% round(6)
var3_totalcloseness<- closeness(var3, mode = "total", weights = E(var3)$var3) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var3_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 2.900e-05 4.700e-05 4.700e-05 4.306e-05 4.700e-05 4.900e-05
```

```r
sd(var3_incloseness)
```

```
## [1] 7.599247e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var3)$incloseness<-closeness(var3, weights = E(var3)$var3, mode="in")

#Get Variable
V(var3)$var3_color_degree<-round(V(var3)$incloseness,6)

#Creating brewer pallette
vertex_var3_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var3)$var3_color_degree)), "RdBu"))(
            length(unique(V(var3)$var3_color_degree)))

#Saving as Vertex properties 
V(var3)$vertex_var3_color_degree<-
  vertex_var3_color_degree[as.numeric(
  cut(V(var3)$var3_color_degree,
      breaks=length(unique(V(var3)$var3_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var3, es=E(var3), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var3))
maxC <- rep(Inf, vcount(var3))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var3, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var3)$weight)


#PLotting
plot(var3, 
     layout=co,
     edge.color=V(var3)$vertex_var3_color_degree[edge.start],
     edge.arrow.size=closeness(var3, weights = E(var3)$var3, mode="in"),
     edge.width=E(var3)$weight/mean(E(var3)$weight),
     edge.curved = TRUE,
     vertex.color=V(var3)$vertex_var3_color_degree,
     vertex.size=closeness(var3, weights = E(var3)$var3, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var3,"LABEL_COR"),
     vertex.label.cex=(closeness(var3, weights = E(var3)$var3, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var3)$var3_color_degree
b<-V(var3)$vertex_var3_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 5_COORDENAÇÃO CONJUNTA DE CASOS (var3)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var3, mode="in", weights = E(var3)$var3)), 
             sd(closeness(var3, mode="in", weights = E(var3)$var3))
             )
       )
```

![](5_COORDENAÇÃO_CONJUNTA_DE_CASOS_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var3_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 2.900e-05 2.900e-05 2.900e-05 6.563e-05 1.210e-04 1.240e-04
```

```r
sd(var3_outcloseness)
```

```
## [1] 4.539122e-05
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var3)$outcloseness<-closeness(var3, weights = E(var3)$var3, mode="out")

#Get Variable
V(var3)$var3_color_degree<-round(V(var3)$outcloseness,6)

#Creating brewer pallette
vertex_var3_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var3)$var3_color_degree)), "RdBu"))(
            length(unique(V(var3)$var3_color_degree)))

#Saving as Vertex properties 
V(var3)$vertex_var3_color_degree<-
  vertex_var3_color_degree[as.numeric(
  cut(V(var3)$var3_color_degree,
      breaks=length(unique(V(var3)$var3_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var3, es=E(var3), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var3))
maxC <- rep(Inf, vcount(var3))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var3, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var3)$weight)


#PLotting
plot(var3, 
     layout=co,
     edge.color=V(var3)$vertex_var3_color_degree[edge.start],
     edge.arrow.size=closeness(var3, weights = E(var3)$var3, mode="out"),
     edge.width=E(var3)$weight/2*mean(E(var3)$weight),
     edge.curved = TRUE,
     vertex.color=V(var3)$vertex_var3_color_degree,
     vertex.size=closeness(var3, weights = E(var3)$var3, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var3,"LABEL_COR"),
     vertex.label.cex=closeness(var3, weights = E(var3)$var3, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var3)$var3_color_degree
b<-V(var3)$vertex_var3_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 5_COORDENAÇÃO CONJUNTA DE CASOS (var3)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var3, mode="out", weights = E(var3)$var3)), 
             sd(closeness(var3, mode="out", weights = E(var3)$var3))
             )
       )
```

![](5_COORDENAÇÃO_CONJUNTA_DE_CASOS_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var3_totalcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0001280 0.0001310 0.0001094 0.0001310 0.0001330
```

```r
sd(var3_totalcloseness)
```

```
## [1] 4.138477e-05
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var3)$allcloseness<-closeness(var3, weights = E(var3)$var3, mode="all")

#Get Variable
V(var3)$var3_color_degree<-round(V(var3)$allcloseness,6)

#Creating brewer pallette
vertex_var3_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var3)$var3_color_degree)), "RdBu"))(
            length(unique(V(var3)$var3_color_degree)))

#Saving as Vertex properties 
V(var3)$vertex_var3_color_degree<-
  vertex_var3_color_degree[as.numeric(
  cut(V(var3)$var3_color_degree,
      breaks=length(unique(V(var3)$var3_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var3, es=E(var3), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var3))
maxC <- rep(Inf, vcount(var3))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var3, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var3)$weight)


#PLotting
plot(var3, 
     layout=co,
     edge.color=V(var3)$vertex_var3_color_degree[edge.start],
     edge.arrow.size=closeness(var3, weights = E(var3)$var3, mode="all"),
     edge.width=E(var3)$weight/2*mean(E(var3)$weight),
     edge.curved = TRUE,
     vertex.color=V(var3)$vertex_var3_color_degree,
     vertex.size=closeness(var3, weights = E(var3)$var3, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var3,"LABEL_COR"),
     vertex.label.cex=(closeness(var3, weights = E(var3)$var3, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var3)$var3_color_degree
b<-V(var3)$vertex_var3_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 5_COORDENAÇÃO CONJUNTA DE CASOS (var3)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var3, mode="all", weights = E(var3)$var3)), 
             sd(closeness(var3, mode="all", weights = E(var3)$var3))
             )
       )
```

![](5_COORDENAÇÃO_CONJUNTA_DE_CASOS_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var3)$incloseness_n <- closeness(var3, mode = "in",, weights = E(var3)$var3, normalized = T) %>% round(10)
V(var3)$outcloseness_n <- closeness(var3, mode = "out", normalized = T, weights = E(var3)$var3) %>% round(6)
V(var3)$totalcloseness_n <- closeness(var3, mode = "total", normalized = T, weights = E(var3)$var3) %>% round(6)
```

###Saving to Environment

```r
var3_incloseness_n<- closeness(var3, mode = "in", normalized = T, weights = E(var3)$var3) %>% round(6)
var3_outcloseness_n<- closeness(var3, mode = "out", normalized = T, weights = E(var3)$var3) %>% round(6)
var3_totalcloseness_n<- closeness(var3, mode = "total", normalized = T, weights = E(var3)$var3) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var3_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.008710 0.008736 0.008011 0.008807 0.009090
```

```r
sd(var3_incloseness_n)
```

```
## [1] 0.001438137
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var3)$incloseness_n<-closeness(var3, weights = E(var3)$var3, mode="in", normalized = T)

#Get Variable
V(var3)$var3_color_degree<-round(V(var3)$incloseness_n,6)

#Creating brewer pallette
vertex_var3_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var3)$var3_color_degree)), "RdBu"))(
            length(unique(V(var3)$var3_color_degree)))

#Saving as Vertex properties 
V(var3)$vertex_var3_color_degree<-
  vertex_var3_color_degree[as.numeric(
  cut(V(var3)$var3_color_degree,
      breaks=length(unique(V(var3)$var3_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var3, es=E(var3), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var3))
maxC <- rep(Inf, vcount(var3))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var3, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var3)$weight)


#PLotting
plot(var3, 
     layout=co,
     edge.color=V(var3)$vertex_var3_color_degree[edge.start],
     edge.arrow.size=closeness(var3, weights = E(var3)$var3, mode="in",normalized = T),
     edge.width=E(var3)$weight/10*mean(E(var3)$weight),
     edge.curved = TRUE,
     vertex.color=V(var3)$vertex_var3_color_degree,
     vertex.size=(closeness(var3, weights = E(var3)$var3, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var3,"LABEL_COR"),
     vertex.label.cex=closeness(var3, weights = E(var3)$var3, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var3)$var3_color_degree
b<-V(var3)$vertex_var3_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 5_COORDENAÇÃO CONJUNTA DE CASOS (var3)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var3, mode="in", weights = E(var3)$var3, normalized = T)), 
             sd(closeness(var3, mode="in", weights = E(var3)$var3, normalized = T))
             )
       )
```

![](5_COORDENAÇÃO_CONJUNTA_DE_CASOS_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var3_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.005348 0.005348 0.012190 0.022590 0.023050
```

```r
sd(var3_outcloseness_n)
```

```
## [1] 0.008460018
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var3)$outcloseness_n<-closeness(var3, weights = E(var3)$var3, mode="out", normalized = T)

#Get Variable
V(var3)$var3_color_degree<-round(V(var3)$outcloseness_n,6)

#Creating brewer pallette
vertex_var3_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var3)$var3_color_degree)), "RdBu"))(
            length(unique(V(var3)$var3_color_degree)))

#Saving as Vertex properties 
V(var3)$vertex_var3_color_degree<-
  vertex_var3_color_degree[as.numeric(
  cut(V(var3)$var3_color_degree,
      breaks=length(unique(V(var3)$var3_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var3, es=E(var3), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var3))
maxC <- rep(Inf, vcount(var3))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var3, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var3)$weight)


#PLotting
plot(var3, 
     layout=co,
     edge.color=V(var3)$vertex_var3_color_degree[edge.start],
     edge.arrow.size=closeness(var3, weights = E(var3)$var3, mode="out",normalized = T),
     edge.width=E(var3)$weight/10*mean(E(var3)$weight),
     edge.curved = TRUE,
     vertex.color=V(var3)$vertex_var3_color_degree,
     vertex.size=(closeness(var3, weights = E(var3)$var3, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var3,"LABEL_COR"),
     vertex.label.cex=closeness(var3, weights = E(var3)$var3, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var3)$var3_color_degree
b<-V(var3)$vertex_var3_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 5_COORDENAÇÃO CONJUNTA DE CASOS (var3)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var3, mode="out", weights = E(var3)$var3, normalized = T)), 
             sd(closeness(var3, mode="out", weights = E(var3)$var3, normalized = T))
             )
       )
```

![](5_COORDENAÇÃO_CONJUNTA_DE_CASOS_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var3_totalcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.023860 0.024370 0.020340 0.024400 0.024810
```

```r
sd(var3_totalcloseness_n)
```

```
## [1] 0.007718569
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var3)$allcloseness_n<-closeness(var3, weights = E(var3)$var3, mode="all", normalized = T)

#Get Variable
V(var3)$var3_color_degree<-round(V(var3)$allcloseness_n,6)

#Creating brewer pallette
vertex_var3_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var3)$var3_color_degree)), "RdBu"))(
            length(unique(V(var3)$var3_color_degree)))

#Saving as Vertex properties 
V(var3)$vertex_var3_color_degree<-
  vertex_var3_color_degree[as.numeric(
  cut(V(var3)$var3_color_degree,
      breaks=length(unique(V(var3)$var3_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var3, es=E(var3), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var3))
maxC <- rep(Inf, vcount(var3))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var3, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var3)$weight)


#PLotting
plot(var3, 
     layout=co,
     edge.color=V(var3)$vertex_var3_color_degree[edge.start],
     edge.arrow.size=closeness(var3, weights = E(var3)$var3, mode="all",normalized = T),
     edge.width=E(var3)$weight/10*mean(E(var3)$weight),
     edge.curved = TRUE,
     vertex.color=V(var3)$vertex_var3_color_degree,
     vertex.size=(closeness(var3, weights = E(var3)$var3, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var3,"LABEL_COR"),
     vertex.label.cex=closeness(var3, weights = E(var3)$var3, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var3)$var3_color_degree
b<-V(var3)$vertex_var3_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 5_COORDENAÇÃO CONJUNTA DE CASOS (var3)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var3, mode="all", weights = E(var3)$var3, normalized = T)), 
             sd(closeness(var3, mode="all", weights = E(var3)$var3, normalized = T))
             )
       )
```

![](5_COORDENAÇÃO_CONJUNTA_DE_CASOS_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var3)$incloseness_n <- closeness(var3, weights = E(var3)$var3, mode = "in", normalized = T) %>% round(6)
V(var3)$outcloseness_n <- closeness(var3, weights = E(var3)$var3, mode = "out", normalized = T) %>% round(6)
V(var3)$totalcloseness_n <- closeness(var3, weights = E(var3)$var3, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var3)$var3_centr_closeness<- centralization.closeness(var3)$res
var3_centr_closeness<- centralization.closeness(var3)$res
var3_centr_closeness_all<- centralization.closeness(var3)
```

###Centralization

```r
var3_centr_closeness_all$centralization
```

```
## [1] 0.01098041
```

###Theoretical Max

```r
var3_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var3)$var3_centr_closeness<- centralization.closeness(var3)$res

#Get Variable
V(var3)$var3_color_degree<-round(V(var3)$var3_centr_closeness,6)

#Creating brewer pallette
vertex_var3_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var3)$var3_color_degree)), "Spectral"))(
            length(unique(V(var3)$var3_color_degree)))

#Saving as Vertex properties 
V(var3)$vertex_var3_color_degree<-
  vertex_var3_color_degree[as.numeric(
  cut(V(var3)$var3_color_degree,
      breaks=length(unique(V(var3)$var3_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var3, es=E(var3), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var3))
maxC <- rep(Inf, vcount(var3))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var3, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var3)$weight)


#PLotting
plot(var3, 
     layout=co,
     edge.color=V(var3)$vertex_var3_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var3)$res,
     edge.width=E(var3)$weight/10*mean(E(var3)$weight),
     edge.curved = TRUE,
     vertex.color=V(var3)$vertex_var3_color_degree,
     vertex.size=centralization.closeness(var3)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var3,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var3)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var3)$var3_color_degree
b<-V(var3)$vertex_var3_color_degree
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
  title("Network Centralization Closeness - 5_COORDENAÇÃO CONJUNTA DE CASOS (var3)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var3)$res), 
             sd(centralization.closeness(var3)$res)
             )
       )
```

![](5_COORDENAÇÃO_CONJUNTA_DE_CASOS_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var3_incloseness<- closeness(var3, weights = E(var3)$var3, mode = "in") %>% round(6)
var3_outcloseness<- closeness(var3, weights = E(var3)$var3, mode = "out") %>% round(6)
var3_totalcloseness<- closeness(var3, weights = E(var3)$var3, mode = "total") %>% round(6)
var3_incloseness_n<- closeness(var3,weights = E(var3)$var3, mode = "in", normalized = T) %>% round(6)
var3_outcloseness_n<- closeness(var3,weights = E(var3)$var3, mode = "out", normalized = T) %>% round(6)
var3_totalcloseness_n<- closeness(var3,weights = E(var3)$var3, mode = "total", normalized = T) %>% round(6)
var3_centr_closeness <- centralization.closeness(var3)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var3_df_closseness <- data.frame(
var3_incloseness,
var3_outcloseness,
var3_totalcloseness,
var3_incloseness_n,
var3_outcloseness_n,
var3_totalcloseness_n,
var3_centr_closeness) %>% round(6)

#Adding type
var3_df_closseness <-cbind(var3_df_closseness, V(var3)$LABEL_COR)

#Adding names
names(var3_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var3_df_closseness<-var3_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var3_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-8b56e0269e14537e5054" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-8b56e0269e14537e5054">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"4.9e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000124\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.00909\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.023048\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024813\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.023048\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.8e-05,4.7e-05,4.7e-05,4.8e-05,4.7e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.8e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,2.9e-05,4.7e-05,2.9e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,2.9e-05,4.7e-05,2.9e-05,4.7e-05,4.7e-05,2.9e-05,4.7e-05,4.7e-05,2.9e-05,4.8e-05,4.7e-05,4.7e-05,4.7e-05,2.9e-05,2.9e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.9e-05,4.9e-05,4.9e-05,4.9e-05,4.7e-05,2.9e-05,4.9e-05,4.7e-05,4.8e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,2.9e-05,4.8e-05,4.8e-05,4.7e-05,4.7e-05,4.7e-05,4.8e-05,4.7e-05,4.7e-05,2.9e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,2.9e-05,2.9e-05,4.7e-05,4.7e-05,4.7e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,4.8e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,4.8e-05,2.9e-05,2.9e-05,4.7e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000122,0.000123,0.000124,0.000122,2.9e-05,0.000122,0.000122,0.000122,0.000122,0.00012,0.00012,2.9e-05,0.000121,0.000122,0.000122,0.000121,0.000123,2.9e-05,2.9e-05,0.000121,0.000121,2.9e-05,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000123,0.000123,0.000121,2.9e-05,0.000121,0.000121,2.9e-05,2.9e-05,2.9e-05,0.000122,0.000121,0.000122,0.000122,0.000122,0.000122,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000121,0.000121,0.000121,0.000121,0.000121,2.9e-05,0.000122,2.9e-05,0.000121,0.000121,2.9e-05,2.9e-05,0.000123,2.9e-05,2.9e-05,2.9e-05,0.000121,2.9e-05,2.9e-05,2.9e-05,0.00012,0.000121,2.9e-05,2.9e-05,0.000122,2.9e-05,0.000122,2.9e-05,0.000122,2.9e-05,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000122,2.9e-05,2.9e-05,0.000121,2.9e-05,2.9e-05,0.00012,0.00012,0.000122,2.9e-05,2.9e-05,2.9e-05,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000122,2.9e-05,0.000121,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000122,2.9e-05,2.9e-05,2.9e-05,0.000119,2.9e-05,0.000122,2.9e-05,2.9e-05,0.000121,0.000121,0.000121,0.000122,0.000122,2.9e-05,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000122,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000118,0.000122,2.9e-05,2.9e-05,2.9e-05,0.000122,0.000122,2.9e-05,2.9e-05,0.000122,0.000122,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000122,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000132,0.000132,0.000133,0.000132,0.000131,0.000132,0.000132,0.000131,0.000131,0.00013,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000133,0.000131,0.00013,0.000131,0.000132,0.000129,0.000131,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000132,0.000132,0.000131,0.000131,0.000131,0.000131,0.000129,0.000131,0.000129,0.000131,0.00013,0.000131,0.000132,0.000132,0.000131,0.000131,0.00013,2.9e-05,0.000131,2.9e-05,0.00013,0.00013,0.00013,0.00013,0.00013,2.9e-05,0.000131,2.9e-05,0.000131,0.000131,2.9e-05,0.000129,0.000132,2.9e-05,0.000128,0.000131,0.000131,0.000129,2.9e-05,2.9e-05,0.000131,0.00013,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000132,0.000127,0.000127,0.000127,0.000127,0.000131,2.9e-05,0.000129,0.000131,0.000129,0.000128,0.000131,0.000131,0.000131,2.9e-05,0.000128,0.00013,0.000131,0.000129,0.000128,0.000129,0.000131,0.000132,0.000128,0.000131,0.000129,0.000129,0.000131,0.000131,0.000131,0.000129,0.00013,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.00013,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000132,0.000131,0.000131,0.00013,0.00013,0.000131,0.00013,0.000131,0.00013,0.000129,0.000129,0.00013,0.000131,0.000131,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000129,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000129,2.9e-05,2.9e-05,0.000129,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.008744,0.008744,0.008758,0.008736,0.008818,0.008736,0.008734,0.008714,0.008686,0.008716,0.008737,0.008817,0.008701,0.008688,0.008731,0.008733,0.008741,0.008812,0.008866,0.008732,0.00874,0.008917,0.00874,0.005348,0.005348,0.005348,0.005348,0.008745,0.008739,0.008736,0.008816,0.008734,0.008737,0.008778,0.008816,0.008917,0.008707,0.008718,0.008713,0.008741,0.00874,0.008741,0.008737,0.008786,0.005348,0.008818,0.005348,0.008718,0.008718,0.008718,0.008718,0.008718,0.005348,0.008721,0.005348,0.008739,0.008734,0.005348,0.008788,0.008741,0.005348,0.008914,0.00881,0.008734,0.00876,0.005348,0.005348,0.008731,0.008691,0.008805,0.008807,0.008731,0.008808,0.008733,0.00881,0.008734,0.00881,0.008747,0.009057,0.009057,0.009057,0.009057,0.008731,0.005348,0.00909,0.008731,0.008838,0.008825,0.008732,0.008731,0.008731,0.005348,0.008914,0.008945,0.008731,0.00876,0.008825,0.008917,0.008807,0.00874,0.005348,0.008717,0.008789,0.008789,0.008811,0.008808,0.008734,0.008789,0.008793,0.008811,0.008735,0.008808,0.008734,0.008808,0.008807,0.008734,0.008734,0.008734,0.008732,0.008736,0.008812,0.008734,0.008806,0.008806,0.008806,0.008807,0.008731,0.008731,0.008808,0.008793,0.008806,0.008808,0.008807,0.008807,0.008808,0.008733,0.008733,0.00881,0.008808,0.008808,0.008736,0.008733,0.008807,0.00881,0.008728,0.008745,0.008738,0.008808,0.00879,0.008791,0.008806,0.008791,0.008806,0.008791,0.005348,0.005348,0.008793,0.008807,0.008806,0.005348,0.005348,0.005348,0.005376,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.008857,0.005348,0.005348,0.005348,0.005348,0.005348,0.008857,0.005348,0.005348,0.008768,0.005348,0.005348,0.005348,0.005348],[0.022747,0.022808,0.023048,0.02275,0.005348,0.022719,0.022783,0.02275,0.022677,0.022404,0.02235,0.005348,0.022556,0.022617,0.02265,0.022515,0.022943,0.005348,0.005348,0.022426,0.022496,0.005405,0.022669,0.005348,0.005348,0.005348,0.005348,0.022833,0.0228,0.022592,0.005348,0.022581,0.022434,0.005348,0.005348,0.005405,0.022653,0.022477,0.022708,0.022752,0.022775,0.022672,0.022672,0.005348,0.005348,0.005348,0.005348,0.022515,0.022515,0.022515,0.022515,0.022515,0.005348,0.022719,0.005348,0.022597,0.022439,0.005348,0.005348,0.022791,0.005348,0.005405,0.005348,0.022439,0.005348,0.005376,0.005348,0.022364,0.022584,0.005348,0.005348,0.022666,0.005348,0.022664,0.005376,0.022658,0.005376,0.022719,0.005464,0.005464,0.005464,0.005464,0.022655,0.005348,0.005464,0.022415,0.005348,0.005376,0.022372,0.022291,0.02268,0.005348,0.005405,0.005405,0.022688,0.005376,0.005376,0.005405,0.005348,0.022747,0.005376,0.022573,0.005348,0.005348,0.005348,0.005348,0.022658,0.005348,0.005348,0.005348,0.022114,0.005348,0.022672,0.005348,0.005348,0.022439,0.022439,0.022597,0.022658,0.02265,0.005348,0.022724,0.005348,0.005348,0.005348,0.005348,0.022647,0.022653,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.021973,0.022655,0.005348,0.005348,0.005348,0.02265,0.022647,0.005348,0.005348,0.022647,0.022647,0.022664,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.02278,0.02278,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.024499,0.024577,0.024813,0.024483,0.024442,0.024483,0.024493,0.024448,0.024409,0.024156,0.024397,0.024454,0.024311,0.024291,0.024387,0.024429,0.024646,0.024397,0.024184,0.024413,0.024487,0.023994,0.024458,0.005348,0.005348,0.005348,0.005348,0.024535,0.024499,0.024425,0.024409,0.02439,0.024458,0.02392,0.024429,0.023994,0.024387,0.024175,0.024438,0.024474,0.024516,0.024429,0.024422,0.024109,0.005348,0.024451,0.005348,0.024209,0.024209,0.024209,0.024209,0.024209,0.005348,0.024442,0.005348,0.024454,0.024381,0.005348,0.024043,0.024506,0.005348,0.023743,0.024377,0.024374,0.023988,0.005376,0.005348,0.024371,0.024253,0.024342,0.024377,0.024384,0.024387,0.02439,0.024403,0.024377,0.024377,0.024538,0.023592,0.023592,0.023592,0.023592,0.024377,0.005348,0.024028,0.024377,0.023991,0.023858,0.024419,0.024381,0.0244,0.005348,0.023743,0.02419,0.024403,0.023991,0.023858,0.023994,0.024377,0.024487,0.023734,0.024355,0.024043,0.024043,0.024374,0.024374,0.024381,0.024043,0.024109,0.024403,0.024403,0.024409,0.024377,0.024381,0.024377,0.024381,0.024381,0.0244,0.0244,0.024406,0.024381,0.024458,0.024371,0.024371,0.024371,0.024374,0.024377,0.024384,0.024387,0.024168,0.024368,0.024381,0.024377,0.024377,0.024381,0.024384,0.024371,0.024371,0.024374,0.024371,0.024406,0.024374,0.024365,0.024374,0.024339,0.02448,0.024422,0.024368,0.024106,0.024112,0.024368,0.024112,0.024368,0.024112,0.023963,0.023963,0.02414,0.024377,0.024368,0.005348,0.005348,0.005348,0.005376,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.023944,0.005348,0.005348,0.005348,0.005348,0.005348,0.02392,0.005348,0.005348,0.023981,0.005348,0.005348,0.005348,0.005348],[0.022747,0.022808,0.023048,0.02275,0.005348,0.022719,0.022783,0.02275,0.022677,0.022404,0.02235,0.005348,0.022556,0.022617,0.02265,0.022515,0.022943,0.005348,0.005348,0.022426,0.022496,0.005405,0.022669,0.005348,0.005348,0.005348,0.005348,0.022833,0.0228,0.022592,0.005348,0.022581,0.022434,0.005348,0.005348,0.005405,0.022653,0.022477,0.022708,0.022752,0.022775,0.022672,0.022672,0.005348,0.005348,0.005348,0.005348,0.022515,0.022515,0.022515,0.022515,0.022515,0.005348,0.022719,0.005348,0.022597,0.022439,0.005348,0.005348,0.022791,0.005348,0.005405,0.005348,0.022439,0.005348,0.005376,0.005348,0.022364,0.022584,0.005348,0.005348,0.022666,0.005348,0.022664,0.005376,0.022658,0.005376,0.022719,0.005464,0.005464,0.005464,0.005464,0.022655,0.005348,0.005464,0.022415,0.005348,0.005376,0.022372,0.022291,0.02268,0.005348,0.005405,0.005405,0.022688,0.005376,0.005376,0.005405,0.005348,0.022747,0.005376,0.022573,0.005348,0.005348,0.005348,0.005348,0.022658,0.005348,0.005348,0.005348,0.022114,0.005348,0.022672,0.005348,0.005348,0.022439,0.022439,0.022597,0.022658,0.02265,0.005348,0.022724,0.005348,0.005348,0.005348,0.005348,0.022647,0.022653,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.021973,0.022655,0.005348,0.005348,0.005348,0.02265,0.022647,0.005348,0.005348,0.022647,0.022647,0.022664,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.02278,0.02278,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var3_df_closseness, by=list(var3_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var3_df_closseness, by=list(var3_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-fb03be063e4abc673e97" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-fb03be063e4abc673e97">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"4.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"8e-06\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000124\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"5.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"4.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.008812\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3e-06\" data-max=\"0.001586\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.023048\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.9e-05\" data-max=\"0.010094\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024813\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.6e-05\" data-max=\"0.008212\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.023048\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.9e-05\" data-max=\"0.010094\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[4.7e-05,3.3e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.5e-05,4.7e-05,4.7e-05,4.7e-05,4.2e-05,2.9e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05],[0,8e-06,null,null,null,null,0,null,null,null,null,6e-06,0,0,0,8e-06,null,null,null,0,null,0,0],[9.1e-05,3.3e-05,0.000121,0.000123,0.000122,0.000123,0.000122,0.000124,0.000122,0.000122,0.000122,5.3e-05,0.000122,8e-05,9.1e-05,8.8e-05,2.9e-05,0.000122,0.000122,9e-05,2.9e-05,3.9e-05,7.9e-05],[5.3e-05,1.9e-05,null,null,null,null,1e-06,null,null,null,null,4.2e-05,0,4.9e-05,5.4e-05,4.7e-05,null,null,null,4.5e-05,null,2.9e-05,4.6e-05],[0.000131,5.5e-05,0.000131,0.000132,0.000132,0.000132,0.000132,0.000133,0.000131,0.000131,0.000132,0.000119,0.000131,0.000131,0.000132,0.000112,2.9e-05,0.000132,0.000132,0.00013,0.000131,0.000131,0.000131],[1e-06,4.4e-05,null,null,null,null,0,null,null,null,null,3.2e-05,0,0,1e-06,4.1e-05,null,null,null,1e-06,null,0,0],[0.008756,0.006129,0.008732,0.008744,0.008734,0.008741,0.008743,0.008758,0.00874,0.008738,0.008747,0.008449,0.008714,0.008773,0.008765,0.007817,0.005348,0.00874,0.008744,0.008743,0.008812,0.008795,0.008767],[3e-05,0.001497,null,null,null,null,3e-06,null,null,null,null,0.001096,3.7e-05,4.2e-05,4.6e-05,0.001586,null,null,null,3.5e-05,null,2.3e-05,3.8e-05],[0.016852,0.006119,0.022426,0.022808,0.022783,0.022791,0.022659,0.023048,0.022669,0.022664,0.022719,0.009914,0.022644,0.014942,0.017003,0.01634,0.005348,0.022747,0.022747,0.016806,0.005348,0.007169,0.014662],[0.009965,0.003592,null,null,null,null,0.000169,null,null,null,null,0.007794,3.9e-05,0.009103,0.010094,0.008709,null,null,null,0.008462,null,0.005455,0.008657],[0.02436,0.010174,0.024413,0.024577,0.024493,0.024506,0.024501,0.024813,0.024458,0.024422,0.024538,0.022099,0.02436,0.024445,0.024524,0.020844,0.005348,0.024487,0.024499,0.024194,0.024397,0.024311,0.024384],[0.000222,0.008212,null,null,null,null,3e-05,null,null,null,null,0.005902,9.8e-05,3.3e-05,0.000108,0.007664,null,null,null,0.000132,null,0.000113,1.6e-05],[0.016852,0.006119,0.022426,0.022808,0.022783,0.022791,0.022659,0.023048,0.022669,0.022664,0.022719,0.009914,0.022644,0.014942,0.017003,0.01634,0.005348,0.022747,0.022747,0.016806,0.005348,0.007169,0.014662],[0.009965,0.003592,null,null,null,null,0.000169,null,null,null,null,0.007794,3.9e-05,0.009103,0.010094,0.008709,null,null,null,0.008462,null,0.005455,0.008657]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var3_df_closseness <- data.frame(
var3_incloseness,
var3_outcloseness,
var3_totalcloseness,
var3_incloseness_n,
var3_outcloseness_n,
var3_totalcloseness_n,
var3_centr_closeness) %>% round(6)

#Adding type
var3_df_closseness <-cbind(var3_df_closseness, V(var3)$TIPO1)

#Adding names
names(var3_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var3_df_closseness<-var3_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var3_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-88c225bb29da3b4aacef" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-88c225bb29da3b4aacef">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"4.9e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000124\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.00909\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.023048\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024813\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.023048\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.8e-05,4.7e-05,4.7e-05,4.8e-05,4.7e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.8e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,2.9e-05,4.7e-05,2.9e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,2.9e-05,4.7e-05,2.9e-05,4.7e-05,4.7e-05,2.9e-05,4.7e-05,4.7e-05,2.9e-05,4.8e-05,4.7e-05,4.7e-05,4.7e-05,2.9e-05,2.9e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.9e-05,4.9e-05,4.9e-05,4.9e-05,4.7e-05,2.9e-05,4.9e-05,4.7e-05,4.8e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,2.9e-05,4.8e-05,4.8e-05,4.7e-05,4.7e-05,4.7e-05,4.8e-05,4.7e-05,4.7e-05,2.9e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,2.9e-05,2.9e-05,4.7e-05,4.7e-05,4.7e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,4.8e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,4.8e-05,2.9e-05,2.9e-05,4.7e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000122,0.000123,0.000124,0.000122,2.9e-05,0.000122,0.000122,0.000122,0.000122,0.00012,0.00012,2.9e-05,0.000121,0.000122,0.000122,0.000121,0.000123,2.9e-05,2.9e-05,0.000121,0.000121,2.9e-05,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000123,0.000123,0.000121,2.9e-05,0.000121,0.000121,2.9e-05,2.9e-05,2.9e-05,0.000122,0.000121,0.000122,0.000122,0.000122,0.000122,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000121,0.000121,0.000121,0.000121,0.000121,2.9e-05,0.000122,2.9e-05,0.000121,0.000121,2.9e-05,2.9e-05,0.000123,2.9e-05,2.9e-05,2.9e-05,0.000121,2.9e-05,2.9e-05,2.9e-05,0.00012,0.000121,2.9e-05,2.9e-05,0.000122,2.9e-05,0.000122,2.9e-05,0.000122,2.9e-05,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000122,2.9e-05,2.9e-05,0.000121,2.9e-05,2.9e-05,0.00012,0.00012,0.000122,2.9e-05,2.9e-05,2.9e-05,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000122,2.9e-05,0.000121,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000122,2.9e-05,2.9e-05,2.9e-05,0.000119,2.9e-05,0.000122,2.9e-05,2.9e-05,0.000121,0.000121,0.000121,0.000122,0.000122,2.9e-05,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000122,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000118,0.000122,2.9e-05,2.9e-05,2.9e-05,0.000122,0.000122,2.9e-05,2.9e-05,0.000122,0.000122,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000122,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000132,0.000132,0.000133,0.000132,0.000131,0.000132,0.000132,0.000131,0.000131,0.00013,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000133,0.000131,0.00013,0.000131,0.000132,0.000129,0.000131,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000132,0.000132,0.000131,0.000131,0.000131,0.000131,0.000129,0.000131,0.000129,0.000131,0.00013,0.000131,0.000132,0.000132,0.000131,0.000131,0.00013,2.9e-05,0.000131,2.9e-05,0.00013,0.00013,0.00013,0.00013,0.00013,2.9e-05,0.000131,2.9e-05,0.000131,0.000131,2.9e-05,0.000129,0.000132,2.9e-05,0.000128,0.000131,0.000131,0.000129,2.9e-05,2.9e-05,0.000131,0.00013,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000132,0.000127,0.000127,0.000127,0.000127,0.000131,2.9e-05,0.000129,0.000131,0.000129,0.000128,0.000131,0.000131,0.000131,2.9e-05,0.000128,0.00013,0.000131,0.000129,0.000128,0.000129,0.000131,0.000132,0.000128,0.000131,0.000129,0.000129,0.000131,0.000131,0.000131,0.000129,0.00013,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.00013,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000132,0.000131,0.000131,0.00013,0.00013,0.000131,0.00013,0.000131,0.00013,0.000129,0.000129,0.00013,0.000131,0.000131,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000129,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000129,2.9e-05,2.9e-05,0.000129,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.008744,0.008744,0.008758,0.008736,0.008818,0.008736,0.008734,0.008714,0.008686,0.008716,0.008737,0.008817,0.008701,0.008688,0.008731,0.008733,0.008741,0.008812,0.008866,0.008732,0.00874,0.008917,0.00874,0.005348,0.005348,0.005348,0.005348,0.008745,0.008739,0.008736,0.008816,0.008734,0.008737,0.008778,0.008816,0.008917,0.008707,0.008718,0.008713,0.008741,0.00874,0.008741,0.008737,0.008786,0.005348,0.008818,0.005348,0.008718,0.008718,0.008718,0.008718,0.008718,0.005348,0.008721,0.005348,0.008739,0.008734,0.005348,0.008788,0.008741,0.005348,0.008914,0.00881,0.008734,0.00876,0.005348,0.005348,0.008731,0.008691,0.008805,0.008807,0.008731,0.008808,0.008733,0.00881,0.008734,0.00881,0.008747,0.009057,0.009057,0.009057,0.009057,0.008731,0.005348,0.00909,0.008731,0.008838,0.008825,0.008732,0.008731,0.008731,0.005348,0.008914,0.008945,0.008731,0.00876,0.008825,0.008917,0.008807,0.00874,0.005348,0.008717,0.008789,0.008789,0.008811,0.008808,0.008734,0.008789,0.008793,0.008811,0.008735,0.008808,0.008734,0.008808,0.008807,0.008734,0.008734,0.008734,0.008732,0.008736,0.008812,0.008734,0.008806,0.008806,0.008806,0.008807,0.008731,0.008731,0.008808,0.008793,0.008806,0.008808,0.008807,0.008807,0.008808,0.008733,0.008733,0.00881,0.008808,0.008808,0.008736,0.008733,0.008807,0.00881,0.008728,0.008745,0.008738,0.008808,0.00879,0.008791,0.008806,0.008791,0.008806,0.008791,0.005348,0.005348,0.008793,0.008807,0.008806,0.005348,0.005348,0.005348,0.005376,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.008857,0.005348,0.005348,0.005348,0.005348,0.005348,0.008857,0.005348,0.005348,0.008768,0.005348,0.005348,0.005348,0.005348],[0.022747,0.022808,0.023048,0.02275,0.005348,0.022719,0.022783,0.02275,0.022677,0.022404,0.02235,0.005348,0.022556,0.022617,0.02265,0.022515,0.022943,0.005348,0.005348,0.022426,0.022496,0.005405,0.022669,0.005348,0.005348,0.005348,0.005348,0.022833,0.0228,0.022592,0.005348,0.022581,0.022434,0.005348,0.005348,0.005405,0.022653,0.022477,0.022708,0.022752,0.022775,0.022672,0.022672,0.005348,0.005348,0.005348,0.005348,0.022515,0.022515,0.022515,0.022515,0.022515,0.005348,0.022719,0.005348,0.022597,0.022439,0.005348,0.005348,0.022791,0.005348,0.005405,0.005348,0.022439,0.005348,0.005376,0.005348,0.022364,0.022584,0.005348,0.005348,0.022666,0.005348,0.022664,0.005376,0.022658,0.005376,0.022719,0.005464,0.005464,0.005464,0.005464,0.022655,0.005348,0.005464,0.022415,0.005348,0.005376,0.022372,0.022291,0.02268,0.005348,0.005405,0.005405,0.022688,0.005376,0.005376,0.005405,0.005348,0.022747,0.005376,0.022573,0.005348,0.005348,0.005348,0.005348,0.022658,0.005348,0.005348,0.005348,0.022114,0.005348,0.022672,0.005348,0.005348,0.022439,0.022439,0.022597,0.022658,0.02265,0.005348,0.022724,0.005348,0.005348,0.005348,0.005348,0.022647,0.022653,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.021973,0.022655,0.005348,0.005348,0.005348,0.02265,0.022647,0.005348,0.005348,0.022647,0.022647,0.022664,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.02278,0.02278,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.024499,0.024577,0.024813,0.024483,0.024442,0.024483,0.024493,0.024448,0.024409,0.024156,0.024397,0.024454,0.024311,0.024291,0.024387,0.024429,0.024646,0.024397,0.024184,0.024413,0.024487,0.023994,0.024458,0.005348,0.005348,0.005348,0.005348,0.024535,0.024499,0.024425,0.024409,0.02439,0.024458,0.02392,0.024429,0.023994,0.024387,0.024175,0.024438,0.024474,0.024516,0.024429,0.024422,0.024109,0.005348,0.024451,0.005348,0.024209,0.024209,0.024209,0.024209,0.024209,0.005348,0.024442,0.005348,0.024454,0.024381,0.005348,0.024043,0.024506,0.005348,0.023743,0.024377,0.024374,0.023988,0.005376,0.005348,0.024371,0.024253,0.024342,0.024377,0.024384,0.024387,0.02439,0.024403,0.024377,0.024377,0.024538,0.023592,0.023592,0.023592,0.023592,0.024377,0.005348,0.024028,0.024377,0.023991,0.023858,0.024419,0.024381,0.0244,0.005348,0.023743,0.02419,0.024403,0.023991,0.023858,0.023994,0.024377,0.024487,0.023734,0.024355,0.024043,0.024043,0.024374,0.024374,0.024381,0.024043,0.024109,0.024403,0.024403,0.024409,0.024377,0.024381,0.024377,0.024381,0.024381,0.0244,0.0244,0.024406,0.024381,0.024458,0.024371,0.024371,0.024371,0.024374,0.024377,0.024384,0.024387,0.024168,0.024368,0.024381,0.024377,0.024377,0.024381,0.024384,0.024371,0.024371,0.024374,0.024371,0.024406,0.024374,0.024365,0.024374,0.024339,0.02448,0.024422,0.024368,0.024106,0.024112,0.024368,0.024112,0.024368,0.024112,0.023963,0.023963,0.02414,0.024377,0.024368,0.005348,0.005348,0.005348,0.005376,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.023944,0.005348,0.005348,0.005348,0.005348,0.005348,0.02392,0.005348,0.005348,0.023981,0.005348,0.005348,0.005348,0.005348],[0.022747,0.022808,0.023048,0.02275,0.005348,0.022719,0.022783,0.02275,0.022677,0.022404,0.02235,0.005348,0.022556,0.022617,0.02265,0.022515,0.022943,0.005348,0.005348,0.022426,0.022496,0.005405,0.022669,0.005348,0.005348,0.005348,0.005348,0.022833,0.0228,0.022592,0.005348,0.022581,0.022434,0.005348,0.005348,0.005405,0.022653,0.022477,0.022708,0.022752,0.022775,0.022672,0.022672,0.005348,0.005348,0.005348,0.005348,0.022515,0.022515,0.022515,0.022515,0.022515,0.005348,0.022719,0.005348,0.022597,0.022439,0.005348,0.005348,0.022791,0.005348,0.005405,0.005348,0.022439,0.005348,0.005376,0.005348,0.022364,0.022584,0.005348,0.005348,0.022666,0.005348,0.022664,0.005376,0.022658,0.005376,0.022719,0.005464,0.005464,0.005464,0.005464,0.022655,0.005348,0.005464,0.022415,0.005348,0.005376,0.022372,0.022291,0.02268,0.005348,0.005405,0.005405,0.022688,0.005376,0.005376,0.005405,0.005348,0.022747,0.005376,0.022573,0.005348,0.005348,0.005348,0.005348,0.022658,0.005348,0.005348,0.005348,0.022114,0.005348,0.022672,0.005348,0.005348,0.022439,0.022439,0.022597,0.022658,0.02265,0.005348,0.022724,0.005348,0.005348,0.005348,0.005348,0.022647,0.022653,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.021973,0.022655,0.005348,0.005348,0.005348,0.02265,0.022647,0.005348,0.005348,0.022647,0.022647,0.022664,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.02278,0.02278,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var3_df_closseness, by=list(var3_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var3_df_closseness, by=list(var3_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-576e4f57d813bcbf1f65" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-576e4f57d813bcbf1f65">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.8e-05\" data-max=\"4.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2e-06\" data-max=\"9e-06\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.8e-05\" data-max=\"7.9e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.7e-05\" data-max=\"4.6e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8.1e-05\" data-max=\"0.00013\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-05\" data-max=\"5.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.007021\" data-max=\"0.008735\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000331\" data-max=\"0.00175\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.008864\" data-max=\"0.014615\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.006988\" data-max=\"0.008644\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.015064\" data-max=\"0.0242\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001835\" data-max=\"0.009414\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.008864\" data-max=\"0.014615\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.006988\" data-max=\"0.008644\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[4.7e-05,3.8e-05],[2e-06,9e-06],[7.9e-05,4.8e-05],[4.6e-05,3.7e-05],[0.00013,8.1e-05],[1e-05,5.1e-05],[0.008735,0.007021],[0.000331,0.00175],[0.014615,0.008864],[0.008644,0.006988],[0.0242,0.015064],[0.001835,0.009414],[0.014615,0.008864],[0.008644,0.006988]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var3_df_closseness <- data.frame(
var3_incloseness,
var3_outcloseness,
var3_totalcloseness,
var3_incloseness_n,
var3_outcloseness_n,
var3_totalcloseness_n,
var3_centr_closeness) %>% round(6)

#Adding type
var3_df_closseness <-cbind(var3_df_closseness, V(var3)$TIPO2)

#Adding names
names(var3_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var3_df_closseness<-var3_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var3_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-5e823af4da274dccb0ba" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-5e823af4da274dccb0ba">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"4.9e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000124\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.00909\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.023048\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024813\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.023048\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.8e-05,4.7e-05,4.7e-05,4.8e-05,4.7e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.8e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,2.9e-05,4.7e-05,2.9e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,2.9e-05,4.7e-05,2.9e-05,4.7e-05,4.7e-05,2.9e-05,4.7e-05,4.7e-05,2.9e-05,4.8e-05,4.7e-05,4.7e-05,4.7e-05,2.9e-05,2.9e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.9e-05,4.9e-05,4.9e-05,4.9e-05,4.7e-05,2.9e-05,4.9e-05,4.7e-05,4.8e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,2.9e-05,4.8e-05,4.8e-05,4.7e-05,4.7e-05,4.7e-05,4.8e-05,4.7e-05,4.7e-05,2.9e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,4.7e-05,2.9e-05,2.9e-05,4.7e-05,4.7e-05,4.7e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,4.8e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,4.8e-05,2.9e-05,2.9e-05,4.7e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000122,0.000123,0.000124,0.000122,2.9e-05,0.000122,0.000122,0.000122,0.000122,0.00012,0.00012,2.9e-05,0.000121,0.000122,0.000122,0.000121,0.000123,2.9e-05,2.9e-05,0.000121,0.000121,2.9e-05,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000123,0.000123,0.000121,2.9e-05,0.000121,0.000121,2.9e-05,2.9e-05,2.9e-05,0.000122,0.000121,0.000122,0.000122,0.000122,0.000122,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000121,0.000121,0.000121,0.000121,0.000121,2.9e-05,0.000122,2.9e-05,0.000121,0.000121,2.9e-05,2.9e-05,0.000123,2.9e-05,2.9e-05,2.9e-05,0.000121,2.9e-05,2.9e-05,2.9e-05,0.00012,0.000121,2.9e-05,2.9e-05,0.000122,2.9e-05,0.000122,2.9e-05,0.000122,2.9e-05,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000122,2.9e-05,2.9e-05,0.000121,2.9e-05,2.9e-05,0.00012,0.00012,0.000122,2.9e-05,2.9e-05,2.9e-05,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000122,2.9e-05,0.000121,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000122,2.9e-05,2.9e-05,2.9e-05,0.000119,2.9e-05,0.000122,2.9e-05,2.9e-05,0.000121,0.000121,0.000121,0.000122,0.000122,2.9e-05,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000122,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000118,0.000122,2.9e-05,2.9e-05,2.9e-05,0.000122,0.000122,2.9e-05,2.9e-05,0.000122,0.000122,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000122,0.000122,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000132,0.000132,0.000133,0.000132,0.000131,0.000132,0.000132,0.000131,0.000131,0.00013,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000133,0.000131,0.00013,0.000131,0.000132,0.000129,0.000131,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000132,0.000132,0.000131,0.000131,0.000131,0.000131,0.000129,0.000131,0.000129,0.000131,0.00013,0.000131,0.000132,0.000132,0.000131,0.000131,0.00013,2.9e-05,0.000131,2.9e-05,0.00013,0.00013,0.00013,0.00013,0.00013,2.9e-05,0.000131,2.9e-05,0.000131,0.000131,2.9e-05,0.000129,0.000132,2.9e-05,0.000128,0.000131,0.000131,0.000129,2.9e-05,2.9e-05,0.000131,0.00013,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000132,0.000127,0.000127,0.000127,0.000127,0.000131,2.9e-05,0.000129,0.000131,0.000129,0.000128,0.000131,0.000131,0.000131,2.9e-05,0.000128,0.00013,0.000131,0.000129,0.000128,0.000129,0.000131,0.000132,0.000128,0.000131,0.000129,0.000129,0.000131,0.000131,0.000131,0.000129,0.00013,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.00013,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000132,0.000131,0.000131,0.00013,0.00013,0.000131,0.00013,0.000131,0.00013,0.000129,0.000129,0.00013,0.000131,0.000131,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000129,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000129,2.9e-05,2.9e-05,0.000129,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.008744,0.008744,0.008758,0.008736,0.008818,0.008736,0.008734,0.008714,0.008686,0.008716,0.008737,0.008817,0.008701,0.008688,0.008731,0.008733,0.008741,0.008812,0.008866,0.008732,0.00874,0.008917,0.00874,0.005348,0.005348,0.005348,0.005348,0.008745,0.008739,0.008736,0.008816,0.008734,0.008737,0.008778,0.008816,0.008917,0.008707,0.008718,0.008713,0.008741,0.00874,0.008741,0.008737,0.008786,0.005348,0.008818,0.005348,0.008718,0.008718,0.008718,0.008718,0.008718,0.005348,0.008721,0.005348,0.008739,0.008734,0.005348,0.008788,0.008741,0.005348,0.008914,0.00881,0.008734,0.00876,0.005348,0.005348,0.008731,0.008691,0.008805,0.008807,0.008731,0.008808,0.008733,0.00881,0.008734,0.00881,0.008747,0.009057,0.009057,0.009057,0.009057,0.008731,0.005348,0.00909,0.008731,0.008838,0.008825,0.008732,0.008731,0.008731,0.005348,0.008914,0.008945,0.008731,0.00876,0.008825,0.008917,0.008807,0.00874,0.005348,0.008717,0.008789,0.008789,0.008811,0.008808,0.008734,0.008789,0.008793,0.008811,0.008735,0.008808,0.008734,0.008808,0.008807,0.008734,0.008734,0.008734,0.008732,0.008736,0.008812,0.008734,0.008806,0.008806,0.008806,0.008807,0.008731,0.008731,0.008808,0.008793,0.008806,0.008808,0.008807,0.008807,0.008808,0.008733,0.008733,0.00881,0.008808,0.008808,0.008736,0.008733,0.008807,0.00881,0.008728,0.008745,0.008738,0.008808,0.00879,0.008791,0.008806,0.008791,0.008806,0.008791,0.005348,0.005348,0.008793,0.008807,0.008806,0.005348,0.005348,0.005348,0.005376,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.008857,0.005348,0.005348,0.005348,0.005348,0.005348,0.008857,0.005348,0.005348,0.008768,0.005348,0.005348,0.005348,0.005348],[0.022747,0.022808,0.023048,0.02275,0.005348,0.022719,0.022783,0.02275,0.022677,0.022404,0.02235,0.005348,0.022556,0.022617,0.02265,0.022515,0.022943,0.005348,0.005348,0.022426,0.022496,0.005405,0.022669,0.005348,0.005348,0.005348,0.005348,0.022833,0.0228,0.022592,0.005348,0.022581,0.022434,0.005348,0.005348,0.005405,0.022653,0.022477,0.022708,0.022752,0.022775,0.022672,0.022672,0.005348,0.005348,0.005348,0.005348,0.022515,0.022515,0.022515,0.022515,0.022515,0.005348,0.022719,0.005348,0.022597,0.022439,0.005348,0.005348,0.022791,0.005348,0.005405,0.005348,0.022439,0.005348,0.005376,0.005348,0.022364,0.022584,0.005348,0.005348,0.022666,0.005348,0.022664,0.005376,0.022658,0.005376,0.022719,0.005464,0.005464,0.005464,0.005464,0.022655,0.005348,0.005464,0.022415,0.005348,0.005376,0.022372,0.022291,0.02268,0.005348,0.005405,0.005405,0.022688,0.005376,0.005376,0.005405,0.005348,0.022747,0.005376,0.022573,0.005348,0.005348,0.005348,0.005348,0.022658,0.005348,0.005348,0.005348,0.022114,0.005348,0.022672,0.005348,0.005348,0.022439,0.022439,0.022597,0.022658,0.02265,0.005348,0.022724,0.005348,0.005348,0.005348,0.005348,0.022647,0.022653,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.021973,0.022655,0.005348,0.005348,0.005348,0.02265,0.022647,0.005348,0.005348,0.022647,0.022647,0.022664,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.02278,0.02278,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.024499,0.024577,0.024813,0.024483,0.024442,0.024483,0.024493,0.024448,0.024409,0.024156,0.024397,0.024454,0.024311,0.024291,0.024387,0.024429,0.024646,0.024397,0.024184,0.024413,0.024487,0.023994,0.024458,0.005348,0.005348,0.005348,0.005348,0.024535,0.024499,0.024425,0.024409,0.02439,0.024458,0.02392,0.024429,0.023994,0.024387,0.024175,0.024438,0.024474,0.024516,0.024429,0.024422,0.024109,0.005348,0.024451,0.005348,0.024209,0.024209,0.024209,0.024209,0.024209,0.005348,0.024442,0.005348,0.024454,0.024381,0.005348,0.024043,0.024506,0.005348,0.023743,0.024377,0.024374,0.023988,0.005376,0.005348,0.024371,0.024253,0.024342,0.024377,0.024384,0.024387,0.02439,0.024403,0.024377,0.024377,0.024538,0.023592,0.023592,0.023592,0.023592,0.024377,0.005348,0.024028,0.024377,0.023991,0.023858,0.024419,0.024381,0.0244,0.005348,0.023743,0.02419,0.024403,0.023991,0.023858,0.023994,0.024377,0.024487,0.023734,0.024355,0.024043,0.024043,0.024374,0.024374,0.024381,0.024043,0.024109,0.024403,0.024403,0.024409,0.024377,0.024381,0.024377,0.024381,0.024381,0.0244,0.0244,0.024406,0.024381,0.024458,0.024371,0.024371,0.024371,0.024374,0.024377,0.024384,0.024387,0.024168,0.024368,0.024381,0.024377,0.024377,0.024381,0.024384,0.024371,0.024371,0.024374,0.024371,0.024406,0.024374,0.024365,0.024374,0.024339,0.02448,0.024422,0.024368,0.024106,0.024112,0.024368,0.024112,0.024368,0.024112,0.023963,0.023963,0.02414,0.024377,0.024368,0.005348,0.005348,0.005348,0.005376,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.023944,0.005348,0.005348,0.005348,0.005348,0.005348,0.02392,0.005348,0.005348,0.023981,0.005348,0.005348,0.005348,0.005348],[0.022747,0.022808,0.023048,0.02275,0.005348,0.022719,0.022783,0.02275,0.022677,0.022404,0.02235,0.005348,0.022556,0.022617,0.02265,0.022515,0.022943,0.005348,0.005348,0.022426,0.022496,0.005405,0.022669,0.005348,0.005348,0.005348,0.005348,0.022833,0.0228,0.022592,0.005348,0.022581,0.022434,0.005348,0.005348,0.005405,0.022653,0.022477,0.022708,0.022752,0.022775,0.022672,0.022672,0.005348,0.005348,0.005348,0.005348,0.022515,0.022515,0.022515,0.022515,0.022515,0.005348,0.022719,0.005348,0.022597,0.022439,0.005348,0.005348,0.022791,0.005348,0.005405,0.005348,0.022439,0.005348,0.005376,0.005348,0.022364,0.022584,0.005348,0.005348,0.022666,0.005348,0.022664,0.005376,0.022658,0.005376,0.022719,0.005464,0.005464,0.005464,0.005464,0.022655,0.005348,0.005464,0.022415,0.005348,0.005376,0.022372,0.022291,0.02268,0.005348,0.005405,0.005405,0.022688,0.005376,0.005376,0.005405,0.005348,0.022747,0.005376,0.022573,0.005348,0.005348,0.005348,0.005348,0.022658,0.005348,0.005348,0.005348,0.022114,0.005348,0.022672,0.005348,0.005348,0.022439,0.022439,0.022597,0.022658,0.02265,0.005348,0.022724,0.005348,0.005348,0.005348,0.005348,0.022647,0.022653,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.021973,0.022655,0.005348,0.005348,0.005348,0.02265,0.022647,0.005348,0.005348,0.022647,0.022647,0.022664,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.02278,0.02278,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var3_df_closseness, by=list(var3_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var3_df_closseness, by=list(var3_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-dec723193b132aa61584" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-dec723193b132aa61584">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.8e-05\" data-max=\"4.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"9e-06\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.7e-05\" data-max=\"9.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.7e-05\" data-max=\"4.6e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8e-05\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"5.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.006999\" data-max=\"0.008765\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.8e-05\" data-max=\"0.00175\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.008686\" data-max=\"0.016895\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.006851\" data-max=\"0.00866\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.014943\" data-max=\"0.024472\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6.2e-05\" data-max=\"0.009413\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.008686\" data-max=\"0.016895\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.006851\" data-max=\"0.00866\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[4.7e-05,4.7e-05,3.8e-05],[0,2e-06,9e-06],[9.1e-05,7.7e-05,4.7e-05],[4.5e-05,4.6e-05,3.7e-05],[0.000131,0.00013,8e-05],[1e-06,1.1e-05,5.1e-05],[0.008765,0.00873,0.006999],[3.8e-05,0.000355,0.00175],[0.016895,0.014337,0.008686],[0.008452,0.00866,0.006851],[0.024472,0.02416,0.014943],[6.2e-05,0.001965,0.009413],[0.016895,0.014337,0.008686],[0.008452,0.00866,0.006851]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var3_data.RData")
```

