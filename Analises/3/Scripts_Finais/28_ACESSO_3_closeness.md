# SNA Closeness 28_ACESSO
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 28_ACESSO

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/acesso_data.RData")
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
#acesso<-simplify(acesso) #Simplify
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
V(acesso)$incloseness <- closeness(acesso, mode = "in", weights = E(acesso)$acesso) %>% round(6)
V(acesso)$outcloseness <- closeness(acesso, mode = "out", weights = E(acesso)$acesso) %>% round(6)
V(acesso)$totalcloseness <- closeness(acesso, mode = "total", weights = E(acesso)$acesso) %>% round(4)
```

###Saving to Environment

```r
acesso_incloseness<- closeness(acesso, mode = "in", weights = E(acesso)$acesso) %>% round(6)
acesso_outcloseness<- closeness(acesso, mode = "out", weights = E(acesso)$acesso) %>% round(6)
acesso_totalcloseness<- closeness(acesso, mode = "total", weights = E(acesso)$acesso) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(acesso_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 2.900e-05 6.300e-05 7.100e-05 6.741e-05 7.200e-05 8.900e-05
```

```r
sd(acesso_incloseness)
```

```
## [1] 7.979514e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(acesso)$incloseness<-closeness(acesso, weights = E(acesso)$acesso, mode="in")

#Get Variable
V(acesso)$acesso_color_degree<-round(V(acesso)$incloseness,6)

#Creating brewer pallette
vertex_acesso_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(acesso)$acesso_color_degree)), "RdBu"))(
            length(unique(V(acesso)$acesso_color_degree)))

#Saving as Vertex properties 
V(acesso)$vertex_acesso_color_degree<-
  vertex_acesso_color_degree[as.numeric(
  cut(V(acesso)$acesso_color_degree,
      breaks=length(unique(V(acesso)$acesso_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(acesso, es=E(acesso), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acesso))
maxC <- rep(Inf, vcount(acesso))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acesso, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acesso)$weight)


#PLotting
plot(acesso, 
     layout=co,
     edge.color=V(acesso)$vertex_acesso_color_degree[edge.start],
     edge.arrow.size=closeness(acesso, weights = E(acesso)$acesso, mode="in"),
     edge.width=E(acesso)$weight/mean(E(acesso)$weight),
     edge.curved = TRUE,
     vertex.color=V(acesso)$vertex_acesso_color_degree,
     vertex.size=closeness(acesso, weights = E(acesso)$acesso, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(acesso,"LABEL_COR"),
     vertex.label.cex=(closeness(acesso, weights = E(acesso)$acesso, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(acesso)$acesso_color_degree
b<-V(acesso)$vertex_acesso_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 28_ACESSO", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(acesso, mode="in", weights = E(acesso)$acesso)), 
             sd(closeness(acesso, mode="in", weights = E(acesso)$acesso))
             )
       )
```

![](28_ACESSO_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(acesso_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 2.900e-05 7.300e-05 9.800e-05 8.704e-05 1.075e-04 1.520e-04
```

```r
sd(acesso_outcloseness)
```

```
## [1] 3.274697e-05
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(acesso)$outcloseness<-closeness(acesso, weights = E(acesso)$acesso, mode="out")

#Get Variable
V(acesso)$acesso_color_degree<-round(V(acesso)$outcloseness,6)

#Creating brewer pallette
vertex_acesso_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(acesso)$acesso_color_degree)), "RdBu"))(
            length(unique(V(acesso)$acesso_color_degree)))

#Saving as Vertex properties 
V(acesso)$vertex_acesso_color_degree<-
  vertex_acesso_color_degree[as.numeric(
  cut(V(acesso)$acesso_color_degree,
      breaks=length(unique(V(acesso)$acesso_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(acesso, es=E(acesso), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acesso))
maxC <- rep(Inf, vcount(acesso))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acesso, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acesso)$weight)


#PLotting
plot(acesso, 
     layout=co,
     edge.color=V(acesso)$vertex_acesso_color_degree[edge.start],
     edge.arrow.size=closeness(acesso, weights = E(acesso)$acesso, mode="out"),
     edge.width=E(acesso)$weight/2*mean(E(acesso)$weight),
     edge.curved = TRUE,
     vertex.color=V(acesso)$vertex_acesso_color_degree,
     vertex.size=closeness(acesso, weights = E(acesso)$acesso, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(acesso,"LABEL_COR"),
     vertex.label.cex=closeness(acesso, weights = E(acesso)$acesso, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(acesso)$acesso_color_degree
b<-V(acesso)$vertex_acesso_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 28_ACESSO", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(acesso, mode="out", weights = E(acesso)$acesso)), 
             sd(closeness(acesso, mode="out", weights = E(acesso)$acesso))
             )
       )
```

![](28_ACESSO_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(acesso_totalcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000700 0.0001100 0.0001220 0.0001206 0.0001310 0.0001880
```

```r
sd(acesso_totalcloseness)
```

```
## [1] 1.972632e-05
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(acesso)$allcloseness<-closeness(acesso, weights = E(acesso)$acesso, mode="all")

#Get Variable
V(acesso)$acesso_color_degree<-round(V(acesso)$allcloseness,6)

#Creating brewer pallette
vertex_acesso_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(acesso)$acesso_color_degree)), "RdBu"))(
            length(unique(V(acesso)$acesso_color_degree)))

#Saving as Vertex properties 
V(acesso)$vertex_acesso_color_degree<-
  vertex_acesso_color_degree[as.numeric(
  cut(V(acesso)$acesso_color_degree,
      breaks=length(unique(V(acesso)$acesso_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(acesso, es=E(acesso), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acesso))
maxC <- rep(Inf, vcount(acesso))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acesso, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acesso)$weight)


#PLotting
plot(acesso, 
     layout=co,
     edge.color=V(acesso)$vertex_acesso_color_degree[edge.start],
     edge.arrow.size=closeness(acesso, weights = E(acesso)$acesso, mode="all"),
     edge.width=E(acesso)$weight/2*mean(E(acesso)$weight),
     edge.curved = TRUE,
     vertex.color=V(acesso)$vertex_acesso_color_degree,
     vertex.size=closeness(acesso, weights = E(acesso)$acesso, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(acesso,"LABEL_COR"),
     vertex.label.cex=(closeness(acesso, weights = E(acesso)$acesso, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(acesso)$acesso_color_degree
b<-V(acesso)$vertex_acesso_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 28_ACESSO", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(acesso, mode="all", weights = E(acesso)$acesso)), 
             sd(closeness(acesso, mode="all", weights = E(acesso)$acesso))
             )
       )
```

![](28_ACESSO_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(acesso)$incloseness_n <- closeness(acesso, mode = "in",, weights = E(acesso)$acesso, normalized = T) %>% round(10)
V(acesso)$outcloseness_n <- closeness(acesso, mode = "out", normalized = T, weights = E(acesso)$acesso) %>% round(6)
V(acesso)$totalcloseness_n <- closeness(acesso, mode = "total", normalized = T, weights = E(acesso)$acesso) %>% round(6)
```

###Saving to Environment

```r
acesso_incloseness_n<- closeness(acesso, mode = "in", normalized = T, weights = E(acesso)$acesso) %>% round(6)
acesso_outcloseness_n<- closeness(acesso, mode = "out", normalized = T, weights = E(acesso)$acesso) %>% round(6)
acesso_totalcloseness_n<- closeness(acesso, mode = "total", normalized = T, weights = E(acesso)$acesso) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(acesso_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.011750 0.013170 0.012540 0.013440 0.016480
```

```r
sd(acesso_incloseness_n)
```

```
## [1] 0.001477409
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(acesso)$incloseness_n<-closeness(acesso, weights = E(acesso)$acesso, mode="in", normalized = T)

#Get Variable
V(acesso)$acesso_color_degree<-round(V(acesso)$incloseness_n,6)

#Creating brewer pallette
vertex_acesso_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(acesso)$acesso_color_degree)), "RdBu"))(
            length(unique(V(acesso)$acesso_color_degree)))

#Saving as Vertex properties 
V(acesso)$vertex_acesso_color_degree<-
  vertex_acesso_color_degree[as.numeric(
  cut(V(acesso)$acesso_color_degree,
      breaks=length(unique(V(acesso)$acesso_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(acesso, es=E(acesso), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acesso))
maxC <- rep(Inf, vcount(acesso))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acesso, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acesso)$weight)


#PLotting
plot(acesso, 
     layout=co,
     edge.color=V(acesso)$vertex_acesso_color_degree[edge.start],
     edge.arrow.size=closeness(acesso, weights = E(acesso)$acesso, mode="in",normalized = T),
     edge.width=E(acesso)$weight/10*mean(E(acesso)$weight),
     edge.curved = TRUE,
     vertex.color=V(acesso)$vertex_acesso_color_degree,
     vertex.size=(closeness(acesso, weights = E(acesso)$acesso, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(acesso,"LABEL_COR"),
     vertex.label.cex=closeness(acesso, weights = E(acesso)$acesso, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(acesso)$acesso_color_degree
b<-V(acesso)$vertex_acesso_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 28_ACESSO", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(acesso, mode="in", weights = E(acesso)$acesso, normalized = T)), 
             sd(closeness(acesso, mode="in", weights = E(acesso)$acesso, normalized = T))
             )
       )
```

![](28_ACESSO_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(acesso_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.013610 0.018280 0.016190 0.020000 0.028220
```

```r
sd(acesso_outcloseness_n)
```

```
## [1] 0.006111667
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(acesso)$outcloseness_n<-closeness(acesso, weights = E(acesso)$acesso, mode="out", normalized = T)

#Get Variable
V(acesso)$acesso_color_degree<-round(V(acesso)$outcloseness_n,6)

#Creating brewer pallette
vertex_acesso_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(acesso)$acesso_color_degree)), "RdBu"))(
            length(unique(V(acesso)$acesso_color_degree)))

#Saving as Vertex properties 
V(acesso)$vertex_acesso_color_degree<-
  vertex_acesso_color_degree[as.numeric(
  cut(V(acesso)$acesso_color_degree,
      breaks=length(unique(V(acesso)$acesso_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(acesso, es=E(acesso), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acesso))
maxC <- rep(Inf, vcount(acesso))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acesso, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acesso)$weight)


#PLotting
plot(acesso, 
     layout=co,
     edge.color=V(acesso)$vertex_acesso_color_degree[edge.start],
     edge.arrow.size=closeness(acesso, weights = E(acesso)$acesso, mode="out",normalized = T),
     edge.width=E(acesso)$weight/10*mean(E(acesso)$weight),
     edge.curved = TRUE,
     vertex.color=V(acesso)$vertex_acesso_color_degree,
     vertex.size=(closeness(acesso, weights = E(acesso)$acesso, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(acesso,"LABEL_COR"),
     vertex.label.cex=closeness(acesso, weights = E(acesso)$acesso, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(acesso)$acesso_color_degree
b<-V(acesso)$vertex_acesso_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 28_ACESSO", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(acesso, mode="out", weights = E(acesso)$acesso, normalized = T)), 
             sd(closeness(acesso, mode="out", weights = E(acesso)$acesso, normalized = T))
             )
       )
```

![](28_ACESSO_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(acesso_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.01300 0.02043 0.02264 0.02242 0.02432 0.03500
```

```r
sd(acesso_totalcloseness_n)
```

```
## [1] 0.003672963
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(acesso)$allcloseness_n<-closeness(acesso, weights = E(acesso)$acesso, mode="all", normalized = T)

#Get Variable
V(acesso)$acesso_color_degree<-round(V(acesso)$allcloseness_n,6)

#Creating brewer pallette
vertex_acesso_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(acesso)$acesso_color_degree)), "RdBu"))(
            length(unique(V(acesso)$acesso_color_degree)))

#Saving as Vertex properties 
V(acesso)$vertex_acesso_color_degree<-
  vertex_acesso_color_degree[as.numeric(
  cut(V(acesso)$acesso_color_degree,
      breaks=length(unique(V(acesso)$acesso_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(acesso, es=E(acesso), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acesso))
maxC <- rep(Inf, vcount(acesso))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acesso, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acesso)$weight)


#PLotting
plot(acesso, 
     layout=co,
     edge.color=V(acesso)$vertex_acesso_color_degree[edge.start],
     edge.arrow.size=closeness(acesso, weights = E(acesso)$acesso, mode="all",normalized = T),
     edge.width=E(acesso)$weight/10*mean(E(acesso)$weight),
     edge.curved = TRUE,
     vertex.color=V(acesso)$vertex_acesso_color_degree,
     vertex.size=(closeness(acesso, weights = E(acesso)$acesso, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(acesso,"LABEL_COR"),
     vertex.label.cex=closeness(acesso, weights = E(acesso)$acesso, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(acesso)$acesso_color_degree
b<-V(acesso)$vertex_acesso_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 28_ACESSO", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(acesso, mode="all", weights = E(acesso)$acesso, normalized = T)), 
             sd(closeness(acesso, mode="all", weights = E(acesso)$acesso, normalized = T))
             )
       )
```

![](28_ACESSO_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(acesso)$incloseness_n <- closeness(acesso, weights = E(acesso)$acesso, mode = "in", normalized = T) %>% round(6)
V(acesso)$outcloseness_n <- closeness(acesso, weights = E(acesso)$acesso, mode = "out", normalized = T) %>% round(6)
V(acesso)$totalcloseness_n <- closeness(acesso, weights = E(acesso)$acesso, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(acesso)$acesso_centr_closeness<- centralization.closeness(acesso)$res
acesso_centr_closeness<- centralization.closeness(acesso)$res
acesso_centr_closeness_all<- centralization.closeness(acesso)
```

###Centralization

```r
acesso_centr_closeness_all$centralization
```

```
## [1] 0.1627615
```

###Theoretical Max

```r
acesso_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(acesso)$acesso_centr_closeness<- centralization.closeness(acesso)$res

#Get Variable
V(acesso)$acesso_color_degree<-round(V(acesso)$acesso_centr_closeness,6)

#Creating brewer pallette
vertex_acesso_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(acesso)$acesso_color_degree)), "Spectral"))(
            length(unique(V(acesso)$acesso_color_degree)))

#Saving as Vertex properties 
V(acesso)$vertex_acesso_color_degree<-
  vertex_acesso_color_degree[as.numeric(
  cut(V(acesso)$acesso_color_degree,
      breaks=length(unique(V(acesso)$acesso_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(acesso, es=E(acesso), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acesso))
maxC <- rep(Inf, vcount(acesso))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acesso, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acesso)$weight)


#PLotting
plot(acesso, 
     layout=co,
     edge.color=V(acesso)$vertex_acesso_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(acesso)$res,
     edge.width=E(acesso)$weight/10*mean(E(acesso)$weight),
     edge.curved = TRUE,
     vertex.color=V(acesso)$vertex_acesso_color_degree,
     vertex.size=centralization.closeness(acesso)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(acesso,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(acesso)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(acesso)$acesso_color_degree
b<-V(acesso)$vertex_acesso_color_degree
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
  title("Network Centralization Closeness - 28_ACESSO", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(acesso)$res), 
             sd(centralization.closeness(acesso)$res)
             )
       )
```

![](28_ACESSO_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
acesso_incloseness<- closeness(acesso, weights = E(acesso)$acesso, mode = "in") %>% round(6)
acesso_outcloseness<- closeness(acesso, weights = E(acesso)$acesso, mode = "out") %>% round(6)
acesso_totalcloseness<- closeness(acesso, weights = E(acesso)$acesso, mode = "total") %>% round(6)
acesso_incloseness_n<- closeness(acesso,weights = E(acesso)$acesso, mode = "in", normalized = T) %>% round(6)
acesso_outcloseness_n<- closeness(acesso,weights = E(acesso)$acesso, mode = "out", normalized = T) %>% round(6)
acesso_totalcloseness_n<- closeness(acesso,weights = E(acesso)$acesso, mode = "total", normalized = T) %>% round(6)
acesso_centr_closeness <- centralization.closeness(acesso)$res %>% round(6)
```

##Creating a datagrame of measures

```r
acesso_df_closseness <- data.frame(
acesso_incloseness,
acesso_outcloseness,
acesso_totalcloseness,
acesso_incloseness_n,
acesso_outcloseness_n,
acesso_totalcloseness_n,
acesso_centr_closeness) %>% round(6)

#Adding type
acesso_df_closseness <-cbind(acesso_df_closseness, V(acesso)$LABEL_COR)

#Adding names
names(acesso_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
acesso_df_closseness<-acesso_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(acesso_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-14211c91f40542ae345e" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-14211c91f40542ae345e">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"8.9e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000152\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7e-05\" data-max=\"0.000188\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.016481\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.02822\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.012998\" data-max=\"0.034995\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[8.3e-05,7.6e-05,8.9e-05,7.3e-05,7.5e-05,7.4e-05,7e-05,7.3e-05,6.4e-05,7.1e-05,7.6e-05,7.5e-05,6.2e-05,7.3e-05,7.1e-05,7e-05,7.5e-05,8e-05,6.2e-05,7.3e-05,7.5e-05,6.4e-05,7.4e-05,5.4e-05,7.3e-05,7e-05,5.8e-05,7.5e-05,7.3e-05,7.5e-05,7.5e-05,7.1e-05,7.5e-05,5.9e-05,7.3e-05,6.5e-05,7.2e-05,7.4e-05,6.3e-05,7.4e-05,7.5e-05,7.1e-05,7.1e-05,7.4e-05,7e-05,7.4e-05,5.1e-05,6.5e-05,6.5e-05,6.5e-05,6.5e-05,6.5e-05,5.4e-05,7.2e-05,5.9e-05,7.3e-05,7.4e-05,6.3e-05,5.9e-05,7.8e-05,7.3e-05,5.4e-05,7.1e-05,7.1e-05,6.6e-05,2.9e-05,4.9e-05,7.1e-05,7.6e-05,7e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.1e-05,7.1e-05,7.6e-05,5.3e-05,5.3e-05,5.3e-05,5.3e-05,7.1e-05,6.7e-05,6.6e-05,7.2e-05,7e-05,5e-05,7.1e-05,7e-05,7.1e-05,5.7e-05,5.6e-05,7e-05,7.4e-05,6.6e-05,5e-05,6.4e-05,7.3e-05,7.5e-05,5.5e-05,6.5e-05,6.3e-05,6.3e-05,7.1e-05,7.1e-05,7.1e-05,6.3e-05,6.4e-05,7.1e-05,7.1e-05,7.3e-05,7.2e-05,7.2e-05,7.2e-05,7.1e-05,7.1e-05,7.1e-05,7.1e-05,7.4e-05,7.1e-05,7.4e-05,7.2e-05,7.1e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,6.8e-05,7.1e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.4e-05,7.1e-05,7.2e-05,7.1e-05,7.1e-05,7.1e-05,7.1e-05,7.1e-05,6.8e-05,7.6e-05,7.3e-05,7.4e-05,6.6e-05,6.8e-05,7.1e-05,6.8e-05,7.1e-05,6.8e-05,7.2e-05,5.8e-05,6.9e-05,7.2e-05,7.2e-05,5.2e-05,5.4e-05,6.4e-05,6.3e-05,6.2e-05,5.1e-05,4.9e-05,6.1e-05,5.1e-05,5.1e-05,6.3e-05,5.9e-05,5.1e-05,6.2e-05,6.2e-05,6.4e-05,6.5e-05,6.2e-05,5.1e-05,6.2e-05,5.7e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,5.8e-05,5.4e-05,6.9e-05],[0.000146,0.000135,0.000152,0.000132,0.000107,0.000111,0.000104,0.000132,0.000109,9.8e-05,9.6e-05,0.000101,0.000123,0.00011,0.000113,0.000101,0.000134,0.00013,0.000102,0.000106,0.000109,0.000108,0.000114,0.000119,0.000103,7.8e-05,9.4e-05,0.000107,0.000131,0.000115,0.000124,0.000105,0.000128,7.2e-05,9.8e-05,0.000109,9.9e-05,9.5e-05,0.000105,0.000128,0.000124,0.000129,0.000129,0.000107,0.000107,0.000116,7.7e-05,0.000104,0.000104,0.000104,0.000104,0.000102,0.0001,9.3e-05,0.000118,0.000124,0.000103,0.000106,8.5e-05,0.000108,2.9e-05,7.2e-05,0.000121,9.3e-05,0.000111,2.9e-05,2.9e-05,0.000116,9.9e-05,0.000101,9.6e-05,9.9e-05,0.000113,0.000101,9.2e-05,0.000103,9.6e-05,0.000116,7.1e-05,7.1e-05,7.1e-05,7.1e-05,9.8e-05,7.8e-05,0.000103,0.000105,0.000115,9.3e-05,0.000109,9.2e-05,0.000109,0.000113,0.000101,7.9e-05,0.000106,0.00011,6.6e-05,7.4e-05,9.4e-05,0.000119,7.5e-05,0.00011,2.9e-05,2.9e-05,0.000103,0.000108,8.4e-05,2.9e-05,2.9e-05,0.000105,0.000118,0.000106,9.9e-05,0.000103,0.000105,0.000113,0.000108,0.000105,0.000107,0.000106,0.00012,0.000117,8.7e-05,8.7e-05,8.7e-05,9.5e-05,8.9e-05,9.8e-05,8.9e-05,9.2e-05,0.000104,0.000101,0.000106,9.6e-05,9.7e-05,9.9e-05,8.6e-05,9.1e-05,0.000105,0.000104,9.2e-05,8.8e-05,0.000103,9.1e-05,9.4e-05,8.3e-05,9.8e-05,6.6e-05,8.2e-05,8.3e-05,8.3e-05,8.3e-05,8.3e-05,8.3e-05,7.1e-05,8.3e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000187,0.000162,0.000188,0.000153,0.000156,0.000157,0.00012,0.000156,0.000125,0.000119,0.000135,0.000132,0.000132,0.000149,0.000134,0.000121,0.000161,0.000163,0.000119,0.000135,0.000136,0.000129,0.00014,0.00013,0.000134,0.000117,0.000117,0.000135,0.00014,0.000138,0.000146,0.000122,0.000151,9.3e-05,0.000131,0.000132,0.000129,0.00015,0.000117,0.000146,0.000142,0.000142,0.000143,0.000137,0.000123,0.000139,9.9e-05,0.000122,0.000122,0.000122,0.000122,0.00012,0.000113,0.000129,0.000133,0.000148,0.000128,0.000128,0.000106,0.000147,0.000125,8.9e-05,0.000142,0.000115,0.000131,7e-05,8.5e-05,0.00013,0.00016,0.00012,0.000126,0.000119,0.000136,0.000127,0.000126,0.000123,0.000122,0.00014,8.2e-05,8.2e-05,8.2e-05,8.2e-05,0.000115,0.000124,0.000127,0.000123,0.000144,0.000107,0.000126,0.000114,0.000129,0.000128,0.000115,0.000115,0.000129,0.000139,8.1e-05,0.00011,0.000125,0.000144,9.2e-05,0.000121,9.8e-05,9.8e-05,0.00012,0.000125,0.000114,9.8e-05,9.9e-05,0.000125,0.000135,0.000131,0.00012,0.000129,0.000127,0.000134,0.000124,0.000124,0.000126,0.00013,0.000139,0.00014,0.000116,0.000114,0.000116,0.000117,0.000118,0.000124,0.00012,0.000116,0.000119,0.00012,0.000126,0.00012,0.000122,0.000125,0.000125,0.000116,0.000129,0.000123,0.000116,0.000117,0.000122,0.000114,0.000107,0.000129,0.000122,0.000129,0.00011,0.00011,0.000114,0.00011,0.000114,0.00011,0.000124,0.000109,0.000115,0.000118,0.000114,9.3e-05,8.4e-05,0.000112,0.000105,0.000103,8.7e-05,8.4e-05,9.8e-05,8.7e-05,8.7e-05,0.000102,9.2e-05,8.7e-05,0.000102,9.8e-05,0.000105,0.000105,0.0001,8.7e-05,0.0001,8.9e-05,0.000106,0.000106,0.000103,0.000115,9.2e-05,8.4e-05,0.000104],[0.015466,0.014201,0.016481,0.013654,0.013998,0.013778,0.013035,0.013635,0.011909,0.013151,0.014128,0.013921,0.011443,0.013668,0.013171,0.012955,0.013896,0.014882,0.011577,0.013594,0.013872,0.011985,0.01384,0.010134,0.013549,0.012999,0.010696,0.013904,0.013591,0.013899,0.014011,0.013159,0.013985,0.010911,0.013572,0.012113,0.013396,0.013777,0.011664,0.013734,0.013958,0.013209,0.013126,0.013688,0.012992,0.013773,0.00951,0.012027,0.012027,0.012027,0.012027,0.012029,0.010129,0.01342,0.010939,0.013612,0.013777,0.011683,0.011014,0.014437,0.013504,0.010124,0.013205,0.01317,0.012293,0.005348,0.009121,0.013169,0.014139,0.013065,0.01343,0.013372,0.013398,0.013406,0.01356,0.013206,0.013274,0.014144,0.009892,0.009892,0.009892,0.009892,0.013252,0.012546,0.012358,0.013445,0.012961,0.009343,0.013251,0.013098,0.013205,0.010681,0.010478,0.012988,0.013781,0.01229,0.009343,0.011864,0.01352,0.013956,0.010302,0.012027,0.011771,0.011771,0.013156,0.013195,0.013188,0.011771,0.011889,0.013264,0.013217,0.013576,0.013323,0.013445,0.013434,0.013209,0.013211,0.013214,0.013269,0.013805,0.013148,0.013741,0.013376,0.013296,0.013376,0.013319,0.013344,0.013368,0.013424,0.01265,0.01326,0.013361,0.013361,0.013326,0.013417,0.013552,0.013732,0.013212,0.013404,0.013152,0.013286,0.013242,0.013152,0.01315,0.012721,0.014166,0.013532,0.013814,0.01219,0.012592,0.013266,0.012592,0.013266,0.012592,0.01333,0.010828,0.012898,0.013476,0.013401,0.009599,0.009988,0.011825,0.01166,0.011531,0.009548,0.009195,0.011375,0.009548,0.009548,0.011728,0.010925,0.009548,0.011476,0.011464,0.01199,0.01201,0.011579,0.009548,0.011581,0.0106,0.011638,0.011638,0.01171,0.011933,0.010712,0.010125,0.012805],[0.027245,0.025064,0.02822,0.024545,0.019972,0.020667,0.019293,0.024587,0.020292,0.018196,0.017835,0.018709,0.02287,0.020516,0.021057,0.018799,0.024916,0.02425,0.018995,0.01967,0.020326,0.020028,0.02124,0.022185,0.019108,0.014425,0.017411,0.019959,0.024304,0.021451,0.02308,0.019608,0.02374,0.013349,0.018285,0.020341,0.0185,0.017702,0.019538,0.023782,0.023114,0.024056,0.02404,0.019865,0.019891,0.021543,0.014269,0.019422,0.019422,0.019422,0.019422,0.018912,0.01863,0.017262,0.021934,0.023086,0.019154,0.019668,0.015897,0.020095,0.005348,0.013372,0.022589,0.017232,0.020669,0.005372,0.005399,0.021583,0.018399,0.018877,0.017876,0.018487,0.021108,0.018769,0.017072,0.019171,0.017929,0.02157,0.013205,0.013205,0.013205,0.013205,0.018136,0.01453,0.019089,0.019462,0.021311,0.017346,0.02035,0.017176,0.020264,0.02111,0.018771,0.014697,0.019756,0.020399,0.012286,0.01385,0.017575,0.022193,0.013921,0.020521,0.005348,0.005348,0.019122,0.020145,0.015657,0.005348,0.005348,0.019565,0.02188,0.019678,0.018345,0.019179,0.019581,0.020967,0.020102,0.019618,0.019955,0.019645,0.022391,0.021673,0.016133,0.016133,0.016133,0.017738,0.01662,0.018203,0.01662,0.017133,0.019341,0.018828,0.019714,0.017797,0.018118,0.018469,0.015957,0.0169,0.019526,0.019347,0.017113,0.01635,0.019169,0.0169,0.017484,0.015349,0.018152,0.01223,0.0152,0.015468,0.015468,0.015468,0.015468,0.015468,0.013203,0.01547,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.034792,0.030068,0.034995,0.028445,0.029081,0.029222,0.022262,0.029076,0.023238,0.022117,0.025186,0.02447,0.024464,0.027716,0.024953,0.022584,0.030039,0.030259,0.022172,0.025132,0.025341,0.02404,0.026014,0.0241,0.024906,0.021765,0.021818,0.025176,0.026039,0.025744,0.02713,0.022708,0.028101,0.017369,0.024301,0.024474,0.02392,0.027953,0.021831,0.027126,0.026402,0.026443,0.026534,0.025396,0.022946,0.02579,0.018487,0.022642,0.022642,0.022642,0.022642,0.022294,0.020955,0.023929,0.024747,0.027564,0.023861,0.023803,0.019658,0.027293,0.023233,0.016613,0.026383,0.021414,0.024355,0.012998,0.015732,0.024118,0.02977,0.022377,0.02352,0.022132,0.025244,0.023544,0.023435,0.022909,0.022664,0.026003,0.015166,0.015166,0.015166,0.015166,0.021473,0.023011,0.023574,0.022906,0.026763,0.01994,0.023384,0.021158,0.023966,0.023868,0.021343,0.021306,0.023988,0.025855,0.015018,0.020487,0.02323,0.026813,0.017154,0.022562,0.018139,0.018139,0.022321,0.023282,0.021168,0.018139,0.018412,0.02332,0.025145,0.024346,0.022329,0.023963,0.023706,0.02492,0.023014,0.022977,0.023376,0.024096,0.02579,0.026018,0.021575,0.021279,0.021575,0.021803,0.021895,0.023026,0.02234,0.021643,0.022201,0.022402,0.0235,0.022383,0.022705,0.023172,0.023276,0.021548,0.024034,0.022881,0.021555,0.021803,0.022677,0.021148,0.019959,0.024037,0.022606,0.023901,0.020478,0.020435,0.021221,0.020435,0.021221,0.020435,0.023057,0.020213,0.021424,0.021895,0.021221,0.017213,0.015674,0.020887,0.019579,0.019227,0.016174,0.015592,0.018145,0.016174,0.016174,0.019053,0.01705,0.016174,0.01891,0.018242,0.019548,0.019608,0.018565,0.016174,0.018598,0.016606,0.019683,0.019683,0.019245,0.021458,0.017132,0.015641,0.019436],[0.372,0.343173,0.392405,0.336347,0.308458,0.301948,0.304419,0.319039,0.311037,0.288372,0.293839,0.286154,0.333932,0.322917,0.287481,0.290625,0.353612,0.315254,0.292913,0.286154,0.303922,0.285714,0.295707,0.333333,0.285714,0.240621,0.277198,0.318493,0.333333,0.316327,0.310518,0.2976,0.315789,0.223827,0.230198,0.285714,0.30897,0.268786,0.298555,0.320138,0.334532,0.317406,0.317406,0.293839,0.293375,0.300485,0.230769,0.291536,0.291536,0.291536,0.291536,0.291536,0.282675,0.298555,0.331551,0.32069,0.304419,0.288372,0.261236,0.324607,0.005348,0.227941,0.298555,0.290625,0.299517,0.005376,0.005405,0.306931,0.298555,0.283537,0.283537,0.294304,0.296651,0.296178,0.289269,0.290172,0.288372,0.314189,0.229346,0.229346,0.229346,0.229346,0.287481,0.250674,0.295238,0.299035,0.292453,0.274742,0.303922,0.286154,0.291536,0.302439,0.28882,0.24933,0.295707,0.288372,0.216028,0.223289,0.285714,0.306931,0.234552,0.313659,0.005348,0.005348,0.290625,0.299517,0.285276,0.005348,0.005348,0.297125,0.2976,0.28972,0.287037,0.285714,0.287037,0.291994,0.29477,0.295238,0.290172,0.290172,0.295238,0.311037,0.283537,0.283537,0.283537,0.291994,0.282675,0.285714,0.282675,0.291994,0.291536,0.291536,0.292453,0.282675,0.282675,0.298555,0.284839,0.282675,0.282675,0.291536,0.283105,0.282675,0.286154,0.282675,0.282675,0.282675,0.291536,0.221165,0.252374,0.256198,0.256198,0.256198,0.256198,0.256198,0.226553,0.227106,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(acesso_df_closseness, by=list(acesso_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(acesso_df_closseness, by=list(acesso_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-511587798981314270d8" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-511587798981314270d8">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.9e-05\" data-max=\"8.9e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000152\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-05\" data-max=\"3.6e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000102\" data-max=\"0.000188\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3e-06\" data-max=\"2.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.010953\" data-max=\"0.016481\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00011\" data-max=\"0.002031\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.02822\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001852\" data-max=\"0.006779\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.018941\" data-max=\"0.034995\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000517\" data-max=\"0.004032\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003897\" data-max=\"0.140058\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[7.2e-05,5.9e-05,7.3e-05,7.6e-05,7e-05,7.8e-05,7.5e-05,8.9e-05,7.4e-05,7.3e-05,7.6e-05,6.2e-05,7.2e-05,7.4e-05,7.5e-05,6.9e-05,6.9e-05,7.5e-05,8.3e-05,6.5e-05,8e-05,7.1e-05,7.2e-05],[5e-06,6e-06,null,null,null,null,1e-06,null,null,null,null,1.1e-05,1e-06,1e-06,1e-06,6e-06,null,null,null,3e-06,null,2e-06,1e-06],[0.000111,5e-05,0.000106,0.000135,0.000104,0.000108,0.0001,0.000152,0.000114,9.8e-05,0.000116,9.7e-05,0.00012,0.000115,0.000117,0.000107,2.9e-05,0.000119,0.000146,7.8e-05,0.00013,8.1e-05,0.000101],[2.5e-05,3e-05,null,null,null,null,1.4e-05,null,null,null,null,2.4e-05,1.3e-05,1.4e-05,1.5e-05,1.6e-05,null,null,null,3.6e-05,null,2.4e-05,1e-05],[0.000134,0.000102,0.000135,0.000162,0.00012,0.000147,0.000133,0.000188,0.00014,0.000122,0.00014,0.00012,0.000146,0.00014,0.000158,0.000129,0.000104,0.000144,0.000187,0.000116,0.000163,0.000117,0.000125],[2.2e-05,1.5e-05,null,null,null,null,4e-06,null,null,null,null,2.2e-05,5e-06,7e-06,3e-06,1.9e-05,null,null,null,1.5e-05,null,5e-06,6e-06],[0.013378,0.010953,0.013594,0.014201,0.013035,0.014437,0.013981,0.016481,0.01384,0.013532,0.014144,0.011499,0.013438,0.013796,0.013891,0.012825,0.012805,0.013956,0.015466,0.012145,0.014882,0.013181,0.013334],[0.001029,0.001127,null,null,null,null,0.000161,null,null,null,null,0.00203,0.000325,0.000203,0.00011,0.001128,null,null,null,0.000611,null,0.000331,0.000195],[0.020685,0.009359,0.01967,0.025064,0.019293,0.020095,0.018545,0.02822,0.02124,0.018152,0.02157,0.017967,0.022286,0.021426,0.021852,0.019945,0.005348,0.022193,0.027245,0.014503,0.02425,0.01505,0.018805],[0.00476,0.005655,null,null,null,null,0.002774,null,null,null,null,0.004426,0.002503,0.00259,0.002676,0.002895,null,null,null,0.006779,null,0.004517,0.001852],[0.024994,0.018941,0.025132,0.030068,0.022262,0.027293,0.024851,0.034995,0.026014,0.022606,0.026003,0.022229,0.02708,0.026074,0.029447,0.023933,0.019436,0.026813,0.034792,0.021505,0.030259,0.021704,0.023191],[0.004002,0.002869,null,null,null,null,0.00071,null,null,null,null,0.004032,0.0009,0.001399,0.000517,0.003522,null,null,null,0.002895,null,0.000936,0.001201],[0.300898,0.099162,0.286154,0.343173,0.304419,0.324607,0.301697,0.392405,0.295707,0.291536,0.314189,0.267114,0.320162,0.303099,0.321339,0.296087,0.005348,0.306931,0.372,0.194753,0.315254,0.234499,0.289379],[0.043056,0.125946,null,null,null,null,0.018012,null,null,null,null,0.070908,0.003897,0.032119,0.028138,0.024042,null,null,null,0.140058,null,0.102884,0.011767]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
acesso_df_closseness <- data.frame(
acesso_incloseness,
acesso_outcloseness,
acesso_totalcloseness,
acesso_incloseness_n,
acesso_outcloseness_n,
acesso_totalcloseness_n,
acesso_centr_closeness) %>% round(6)

#Adding type
acesso_df_closseness <-cbind(acesso_df_closseness, V(acesso)$TIPO1)

#Adding names
names(acesso_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
acesso_df_closseness<-acesso_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(acesso_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-875fdd95a527abad64d8" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-875fdd95a527abad64d8">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"8.9e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000152\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7e-05\" data-max=\"0.000188\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.016481\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.02822\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.012998\" data-max=\"0.034995\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[8.3e-05,7.6e-05,8.9e-05,7.3e-05,7.5e-05,7.4e-05,7e-05,7.3e-05,6.4e-05,7.1e-05,7.6e-05,7.5e-05,6.2e-05,7.3e-05,7.1e-05,7e-05,7.5e-05,8e-05,6.2e-05,7.3e-05,7.5e-05,6.4e-05,7.4e-05,5.4e-05,7.3e-05,7e-05,5.8e-05,7.5e-05,7.3e-05,7.5e-05,7.5e-05,7.1e-05,7.5e-05,5.9e-05,7.3e-05,6.5e-05,7.2e-05,7.4e-05,6.3e-05,7.4e-05,7.5e-05,7.1e-05,7.1e-05,7.4e-05,7e-05,7.4e-05,5.1e-05,6.5e-05,6.5e-05,6.5e-05,6.5e-05,6.5e-05,5.4e-05,7.2e-05,5.9e-05,7.3e-05,7.4e-05,6.3e-05,5.9e-05,7.8e-05,7.3e-05,5.4e-05,7.1e-05,7.1e-05,6.6e-05,2.9e-05,4.9e-05,7.1e-05,7.6e-05,7e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.1e-05,7.1e-05,7.6e-05,5.3e-05,5.3e-05,5.3e-05,5.3e-05,7.1e-05,6.7e-05,6.6e-05,7.2e-05,7e-05,5e-05,7.1e-05,7e-05,7.1e-05,5.7e-05,5.6e-05,7e-05,7.4e-05,6.6e-05,5e-05,6.4e-05,7.3e-05,7.5e-05,5.5e-05,6.5e-05,6.3e-05,6.3e-05,7.1e-05,7.1e-05,7.1e-05,6.3e-05,6.4e-05,7.1e-05,7.1e-05,7.3e-05,7.2e-05,7.2e-05,7.2e-05,7.1e-05,7.1e-05,7.1e-05,7.1e-05,7.4e-05,7.1e-05,7.4e-05,7.2e-05,7.1e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,6.8e-05,7.1e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.4e-05,7.1e-05,7.2e-05,7.1e-05,7.1e-05,7.1e-05,7.1e-05,7.1e-05,6.8e-05,7.6e-05,7.3e-05,7.4e-05,6.6e-05,6.8e-05,7.1e-05,6.8e-05,7.1e-05,6.8e-05,7.2e-05,5.8e-05,6.9e-05,7.2e-05,7.2e-05,5.2e-05,5.4e-05,6.4e-05,6.3e-05,6.2e-05,5.1e-05,4.9e-05,6.1e-05,5.1e-05,5.1e-05,6.3e-05,5.9e-05,5.1e-05,6.2e-05,6.2e-05,6.4e-05,6.5e-05,6.2e-05,5.1e-05,6.2e-05,5.7e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,5.8e-05,5.4e-05,6.9e-05],[0.000146,0.000135,0.000152,0.000132,0.000107,0.000111,0.000104,0.000132,0.000109,9.8e-05,9.6e-05,0.000101,0.000123,0.00011,0.000113,0.000101,0.000134,0.00013,0.000102,0.000106,0.000109,0.000108,0.000114,0.000119,0.000103,7.8e-05,9.4e-05,0.000107,0.000131,0.000115,0.000124,0.000105,0.000128,7.2e-05,9.8e-05,0.000109,9.9e-05,9.5e-05,0.000105,0.000128,0.000124,0.000129,0.000129,0.000107,0.000107,0.000116,7.7e-05,0.000104,0.000104,0.000104,0.000104,0.000102,0.0001,9.3e-05,0.000118,0.000124,0.000103,0.000106,8.5e-05,0.000108,2.9e-05,7.2e-05,0.000121,9.3e-05,0.000111,2.9e-05,2.9e-05,0.000116,9.9e-05,0.000101,9.6e-05,9.9e-05,0.000113,0.000101,9.2e-05,0.000103,9.6e-05,0.000116,7.1e-05,7.1e-05,7.1e-05,7.1e-05,9.8e-05,7.8e-05,0.000103,0.000105,0.000115,9.3e-05,0.000109,9.2e-05,0.000109,0.000113,0.000101,7.9e-05,0.000106,0.00011,6.6e-05,7.4e-05,9.4e-05,0.000119,7.5e-05,0.00011,2.9e-05,2.9e-05,0.000103,0.000108,8.4e-05,2.9e-05,2.9e-05,0.000105,0.000118,0.000106,9.9e-05,0.000103,0.000105,0.000113,0.000108,0.000105,0.000107,0.000106,0.00012,0.000117,8.7e-05,8.7e-05,8.7e-05,9.5e-05,8.9e-05,9.8e-05,8.9e-05,9.2e-05,0.000104,0.000101,0.000106,9.6e-05,9.7e-05,9.9e-05,8.6e-05,9.1e-05,0.000105,0.000104,9.2e-05,8.8e-05,0.000103,9.1e-05,9.4e-05,8.3e-05,9.8e-05,6.6e-05,8.2e-05,8.3e-05,8.3e-05,8.3e-05,8.3e-05,8.3e-05,7.1e-05,8.3e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000187,0.000162,0.000188,0.000153,0.000156,0.000157,0.00012,0.000156,0.000125,0.000119,0.000135,0.000132,0.000132,0.000149,0.000134,0.000121,0.000161,0.000163,0.000119,0.000135,0.000136,0.000129,0.00014,0.00013,0.000134,0.000117,0.000117,0.000135,0.00014,0.000138,0.000146,0.000122,0.000151,9.3e-05,0.000131,0.000132,0.000129,0.00015,0.000117,0.000146,0.000142,0.000142,0.000143,0.000137,0.000123,0.000139,9.9e-05,0.000122,0.000122,0.000122,0.000122,0.00012,0.000113,0.000129,0.000133,0.000148,0.000128,0.000128,0.000106,0.000147,0.000125,8.9e-05,0.000142,0.000115,0.000131,7e-05,8.5e-05,0.00013,0.00016,0.00012,0.000126,0.000119,0.000136,0.000127,0.000126,0.000123,0.000122,0.00014,8.2e-05,8.2e-05,8.2e-05,8.2e-05,0.000115,0.000124,0.000127,0.000123,0.000144,0.000107,0.000126,0.000114,0.000129,0.000128,0.000115,0.000115,0.000129,0.000139,8.1e-05,0.00011,0.000125,0.000144,9.2e-05,0.000121,9.8e-05,9.8e-05,0.00012,0.000125,0.000114,9.8e-05,9.9e-05,0.000125,0.000135,0.000131,0.00012,0.000129,0.000127,0.000134,0.000124,0.000124,0.000126,0.00013,0.000139,0.00014,0.000116,0.000114,0.000116,0.000117,0.000118,0.000124,0.00012,0.000116,0.000119,0.00012,0.000126,0.00012,0.000122,0.000125,0.000125,0.000116,0.000129,0.000123,0.000116,0.000117,0.000122,0.000114,0.000107,0.000129,0.000122,0.000129,0.00011,0.00011,0.000114,0.00011,0.000114,0.00011,0.000124,0.000109,0.000115,0.000118,0.000114,9.3e-05,8.4e-05,0.000112,0.000105,0.000103,8.7e-05,8.4e-05,9.8e-05,8.7e-05,8.7e-05,0.000102,9.2e-05,8.7e-05,0.000102,9.8e-05,0.000105,0.000105,0.0001,8.7e-05,0.0001,8.9e-05,0.000106,0.000106,0.000103,0.000115,9.2e-05,8.4e-05,0.000104],[0.015466,0.014201,0.016481,0.013654,0.013998,0.013778,0.013035,0.013635,0.011909,0.013151,0.014128,0.013921,0.011443,0.013668,0.013171,0.012955,0.013896,0.014882,0.011577,0.013594,0.013872,0.011985,0.01384,0.010134,0.013549,0.012999,0.010696,0.013904,0.013591,0.013899,0.014011,0.013159,0.013985,0.010911,0.013572,0.012113,0.013396,0.013777,0.011664,0.013734,0.013958,0.013209,0.013126,0.013688,0.012992,0.013773,0.00951,0.012027,0.012027,0.012027,0.012027,0.012029,0.010129,0.01342,0.010939,0.013612,0.013777,0.011683,0.011014,0.014437,0.013504,0.010124,0.013205,0.01317,0.012293,0.005348,0.009121,0.013169,0.014139,0.013065,0.01343,0.013372,0.013398,0.013406,0.01356,0.013206,0.013274,0.014144,0.009892,0.009892,0.009892,0.009892,0.013252,0.012546,0.012358,0.013445,0.012961,0.009343,0.013251,0.013098,0.013205,0.010681,0.010478,0.012988,0.013781,0.01229,0.009343,0.011864,0.01352,0.013956,0.010302,0.012027,0.011771,0.011771,0.013156,0.013195,0.013188,0.011771,0.011889,0.013264,0.013217,0.013576,0.013323,0.013445,0.013434,0.013209,0.013211,0.013214,0.013269,0.013805,0.013148,0.013741,0.013376,0.013296,0.013376,0.013319,0.013344,0.013368,0.013424,0.01265,0.01326,0.013361,0.013361,0.013326,0.013417,0.013552,0.013732,0.013212,0.013404,0.013152,0.013286,0.013242,0.013152,0.01315,0.012721,0.014166,0.013532,0.013814,0.01219,0.012592,0.013266,0.012592,0.013266,0.012592,0.01333,0.010828,0.012898,0.013476,0.013401,0.009599,0.009988,0.011825,0.01166,0.011531,0.009548,0.009195,0.011375,0.009548,0.009548,0.011728,0.010925,0.009548,0.011476,0.011464,0.01199,0.01201,0.011579,0.009548,0.011581,0.0106,0.011638,0.011638,0.01171,0.011933,0.010712,0.010125,0.012805],[0.027245,0.025064,0.02822,0.024545,0.019972,0.020667,0.019293,0.024587,0.020292,0.018196,0.017835,0.018709,0.02287,0.020516,0.021057,0.018799,0.024916,0.02425,0.018995,0.01967,0.020326,0.020028,0.02124,0.022185,0.019108,0.014425,0.017411,0.019959,0.024304,0.021451,0.02308,0.019608,0.02374,0.013349,0.018285,0.020341,0.0185,0.017702,0.019538,0.023782,0.023114,0.024056,0.02404,0.019865,0.019891,0.021543,0.014269,0.019422,0.019422,0.019422,0.019422,0.018912,0.01863,0.017262,0.021934,0.023086,0.019154,0.019668,0.015897,0.020095,0.005348,0.013372,0.022589,0.017232,0.020669,0.005372,0.005399,0.021583,0.018399,0.018877,0.017876,0.018487,0.021108,0.018769,0.017072,0.019171,0.017929,0.02157,0.013205,0.013205,0.013205,0.013205,0.018136,0.01453,0.019089,0.019462,0.021311,0.017346,0.02035,0.017176,0.020264,0.02111,0.018771,0.014697,0.019756,0.020399,0.012286,0.01385,0.017575,0.022193,0.013921,0.020521,0.005348,0.005348,0.019122,0.020145,0.015657,0.005348,0.005348,0.019565,0.02188,0.019678,0.018345,0.019179,0.019581,0.020967,0.020102,0.019618,0.019955,0.019645,0.022391,0.021673,0.016133,0.016133,0.016133,0.017738,0.01662,0.018203,0.01662,0.017133,0.019341,0.018828,0.019714,0.017797,0.018118,0.018469,0.015957,0.0169,0.019526,0.019347,0.017113,0.01635,0.019169,0.0169,0.017484,0.015349,0.018152,0.01223,0.0152,0.015468,0.015468,0.015468,0.015468,0.015468,0.013203,0.01547,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.034792,0.030068,0.034995,0.028445,0.029081,0.029222,0.022262,0.029076,0.023238,0.022117,0.025186,0.02447,0.024464,0.027716,0.024953,0.022584,0.030039,0.030259,0.022172,0.025132,0.025341,0.02404,0.026014,0.0241,0.024906,0.021765,0.021818,0.025176,0.026039,0.025744,0.02713,0.022708,0.028101,0.017369,0.024301,0.024474,0.02392,0.027953,0.021831,0.027126,0.026402,0.026443,0.026534,0.025396,0.022946,0.02579,0.018487,0.022642,0.022642,0.022642,0.022642,0.022294,0.020955,0.023929,0.024747,0.027564,0.023861,0.023803,0.019658,0.027293,0.023233,0.016613,0.026383,0.021414,0.024355,0.012998,0.015732,0.024118,0.02977,0.022377,0.02352,0.022132,0.025244,0.023544,0.023435,0.022909,0.022664,0.026003,0.015166,0.015166,0.015166,0.015166,0.021473,0.023011,0.023574,0.022906,0.026763,0.01994,0.023384,0.021158,0.023966,0.023868,0.021343,0.021306,0.023988,0.025855,0.015018,0.020487,0.02323,0.026813,0.017154,0.022562,0.018139,0.018139,0.022321,0.023282,0.021168,0.018139,0.018412,0.02332,0.025145,0.024346,0.022329,0.023963,0.023706,0.02492,0.023014,0.022977,0.023376,0.024096,0.02579,0.026018,0.021575,0.021279,0.021575,0.021803,0.021895,0.023026,0.02234,0.021643,0.022201,0.022402,0.0235,0.022383,0.022705,0.023172,0.023276,0.021548,0.024034,0.022881,0.021555,0.021803,0.022677,0.021148,0.019959,0.024037,0.022606,0.023901,0.020478,0.020435,0.021221,0.020435,0.021221,0.020435,0.023057,0.020213,0.021424,0.021895,0.021221,0.017213,0.015674,0.020887,0.019579,0.019227,0.016174,0.015592,0.018145,0.016174,0.016174,0.019053,0.01705,0.016174,0.01891,0.018242,0.019548,0.019608,0.018565,0.016174,0.018598,0.016606,0.019683,0.019683,0.019245,0.021458,0.017132,0.015641,0.019436],[0.372,0.343173,0.392405,0.336347,0.308458,0.301948,0.304419,0.319039,0.311037,0.288372,0.293839,0.286154,0.333932,0.322917,0.287481,0.290625,0.353612,0.315254,0.292913,0.286154,0.303922,0.285714,0.295707,0.333333,0.285714,0.240621,0.277198,0.318493,0.333333,0.316327,0.310518,0.2976,0.315789,0.223827,0.230198,0.285714,0.30897,0.268786,0.298555,0.320138,0.334532,0.317406,0.317406,0.293839,0.293375,0.300485,0.230769,0.291536,0.291536,0.291536,0.291536,0.291536,0.282675,0.298555,0.331551,0.32069,0.304419,0.288372,0.261236,0.324607,0.005348,0.227941,0.298555,0.290625,0.299517,0.005376,0.005405,0.306931,0.298555,0.283537,0.283537,0.294304,0.296651,0.296178,0.289269,0.290172,0.288372,0.314189,0.229346,0.229346,0.229346,0.229346,0.287481,0.250674,0.295238,0.299035,0.292453,0.274742,0.303922,0.286154,0.291536,0.302439,0.28882,0.24933,0.295707,0.288372,0.216028,0.223289,0.285714,0.306931,0.234552,0.313659,0.005348,0.005348,0.290625,0.299517,0.285276,0.005348,0.005348,0.297125,0.2976,0.28972,0.287037,0.285714,0.287037,0.291994,0.29477,0.295238,0.290172,0.290172,0.295238,0.311037,0.283537,0.283537,0.283537,0.291994,0.282675,0.285714,0.282675,0.291994,0.291536,0.291536,0.292453,0.282675,0.282675,0.298555,0.284839,0.282675,0.282675,0.291536,0.283105,0.282675,0.286154,0.282675,0.282675,0.282675,0.291536,0.221165,0.252374,0.256198,0.256198,0.256198,0.256198,0.256198,0.226553,0.227106,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(acesso_df_closseness, by=list(acesso_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(acesso_df_closseness, by=list(acesso_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-00c93ed815b7b395f0b4" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-00c93ed815b7b395f0b4">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6.1e-05\" data-max=\"7.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4e-06\" data-max=\"8e-06\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7.1e-05\" data-max=\"9.9e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.4e-05\" data-max=\"3.6e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000111\" data-max=\"0.000128\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.6e-05\" data-max=\"2.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.011438\" data-max=\"0.013344\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000689\" data-max=\"0.001559\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.013206\" data-max=\"0.018367\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.004535\" data-max=\"0.006729\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.020608\" data-max=\"0.023749\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.002924\" data-max=\"0.00383\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.17405\" data-max=\"0.273316\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.079334\" data-max=\"0.135668\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[7.2e-05,6.1e-05],[4e-06,8e-06],[9.9e-05,7.1e-05],[2.4e-05,3.6e-05],[0.000128,0.000111],[1.6e-05,2.1e-05],[0.013344,0.011438],[0.000689,0.001559],[0.018367,0.013206],[0.004535,0.006729],[0.023749,0.020608],[0.002924,0.00383],[0.273316,0.17405],[0.079334,0.135668]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
acesso_df_closseness <- data.frame(
acesso_incloseness,
acesso_outcloseness,
acesso_totalcloseness,
acesso_incloseness_n,
acesso_outcloseness_n,
acesso_totalcloseness_n,
acesso_centr_closeness) %>% round(6)

#Adding type
acesso_df_closseness <-cbind(acesso_df_closseness, V(acesso)$TIPO2)

#Adding names
names(acesso_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
acesso_df_closseness<-acesso_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(acesso_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-dca47c7bf94c0b565ec8" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-dca47c7bf94c0b565ec8">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"8.9e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000152\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7e-05\" data-max=\"0.000188\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.016481\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.02822\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.012998\" data-max=\"0.034995\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[8.3e-05,7.6e-05,8.9e-05,7.3e-05,7.5e-05,7.4e-05,7e-05,7.3e-05,6.4e-05,7.1e-05,7.6e-05,7.5e-05,6.2e-05,7.3e-05,7.1e-05,7e-05,7.5e-05,8e-05,6.2e-05,7.3e-05,7.5e-05,6.4e-05,7.4e-05,5.4e-05,7.3e-05,7e-05,5.8e-05,7.5e-05,7.3e-05,7.5e-05,7.5e-05,7.1e-05,7.5e-05,5.9e-05,7.3e-05,6.5e-05,7.2e-05,7.4e-05,6.3e-05,7.4e-05,7.5e-05,7.1e-05,7.1e-05,7.4e-05,7e-05,7.4e-05,5.1e-05,6.5e-05,6.5e-05,6.5e-05,6.5e-05,6.5e-05,5.4e-05,7.2e-05,5.9e-05,7.3e-05,7.4e-05,6.3e-05,5.9e-05,7.8e-05,7.3e-05,5.4e-05,7.1e-05,7.1e-05,6.6e-05,2.9e-05,4.9e-05,7.1e-05,7.6e-05,7e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.1e-05,7.1e-05,7.6e-05,5.3e-05,5.3e-05,5.3e-05,5.3e-05,7.1e-05,6.7e-05,6.6e-05,7.2e-05,7e-05,5e-05,7.1e-05,7e-05,7.1e-05,5.7e-05,5.6e-05,7e-05,7.4e-05,6.6e-05,5e-05,6.4e-05,7.3e-05,7.5e-05,5.5e-05,6.5e-05,6.3e-05,6.3e-05,7.1e-05,7.1e-05,7.1e-05,6.3e-05,6.4e-05,7.1e-05,7.1e-05,7.3e-05,7.2e-05,7.2e-05,7.2e-05,7.1e-05,7.1e-05,7.1e-05,7.1e-05,7.4e-05,7.1e-05,7.4e-05,7.2e-05,7.1e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,6.8e-05,7.1e-05,7.2e-05,7.2e-05,7.2e-05,7.2e-05,7.3e-05,7.4e-05,7.1e-05,7.2e-05,7.1e-05,7.1e-05,7.1e-05,7.1e-05,7.1e-05,6.8e-05,7.6e-05,7.3e-05,7.4e-05,6.6e-05,6.8e-05,7.1e-05,6.8e-05,7.1e-05,6.8e-05,7.2e-05,5.8e-05,6.9e-05,7.2e-05,7.2e-05,5.2e-05,5.4e-05,6.4e-05,6.3e-05,6.2e-05,5.1e-05,4.9e-05,6.1e-05,5.1e-05,5.1e-05,6.3e-05,5.9e-05,5.1e-05,6.2e-05,6.2e-05,6.4e-05,6.5e-05,6.2e-05,5.1e-05,6.2e-05,5.7e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,5.8e-05,5.4e-05,6.9e-05],[0.000146,0.000135,0.000152,0.000132,0.000107,0.000111,0.000104,0.000132,0.000109,9.8e-05,9.6e-05,0.000101,0.000123,0.00011,0.000113,0.000101,0.000134,0.00013,0.000102,0.000106,0.000109,0.000108,0.000114,0.000119,0.000103,7.8e-05,9.4e-05,0.000107,0.000131,0.000115,0.000124,0.000105,0.000128,7.2e-05,9.8e-05,0.000109,9.9e-05,9.5e-05,0.000105,0.000128,0.000124,0.000129,0.000129,0.000107,0.000107,0.000116,7.7e-05,0.000104,0.000104,0.000104,0.000104,0.000102,0.0001,9.3e-05,0.000118,0.000124,0.000103,0.000106,8.5e-05,0.000108,2.9e-05,7.2e-05,0.000121,9.3e-05,0.000111,2.9e-05,2.9e-05,0.000116,9.9e-05,0.000101,9.6e-05,9.9e-05,0.000113,0.000101,9.2e-05,0.000103,9.6e-05,0.000116,7.1e-05,7.1e-05,7.1e-05,7.1e-05,9.8e-05,7.8e-05,0.000103,0.000105,0.000115,9.3e-05,0.000109,9.2e-05,0.000109,0.000113,0.000101,7.9e-05,0.000106,0.00011,6.6e-05,7.4e-05,9.4e-05,0.000119,7.5e-05,0.00011,2.9e-05,2.9e-05,0.000103,0.000108,8.4e-05,2.9e-05,2.9e-05,0.000105,0.000118,0.000106,9.9e-05,0.000103,0.000105,0.000113,0.000108,0.000105,0.000107,0.000106,0.00012,0.000117,8.7e-05,8.7e-05,8.7e-05,9.5e-05,8.9e-05,9.8e-05,8.9e-05,9.2e-05,0.000104,0.000101,0.000106,9.6e-05,9.7e-05,9.9e-05,8.6e-05,9.1e-05,0.000105,0.000104,9.2e-05,8.8e-05,0.000103,9.1e-05,9.4e-05,8.3e-05,9.8e-05,6.6e-05,8.2e-05,8.3e-05,8.3e-05,8.3e-05,8.3e-05,8.3e-05,7.1e-05,8.3e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000187,0.000162,0.000188,0.000153,0.000156,0.000157,0.00012,0.000156,0.000125,0.000119,0.000135,0.000132,0.000132,0.000149,0.000134,0.000121,0.000161,0.000163,0.000119,0.000135,0.000136,0.000129,0.00014,0.00013,0.000134,0.000117,0.000117,0.000135,0.00014,0.000138,0.000146,0.000122,0.000151,9.3e-05,0.000131,0.000132,0.000129,0.00015,0.000117,0.000146,0.000142,0.000142,0.000143,0.000137,0.000123,0.000139,9.9e-05,0.000122,0.000122,0.000122,0.000122,0.00012,0.000113,0.000129,0.000133,0.000148,0.000128,0.000128,0.000106,0.000147,0.000125,8.9e-05,0.000142,0.000115,0.000131,7e-05,8.5e-05,0.00013,0.00016,0.00012,0.000126,0.000119,0.000136,0.000127,0.000126,0.000123,0.000122,0.00014,8.2e-05,8.2e-05,8.2e-05,8.2e-05,0.000115,0.000124,0.000127,0.000123,0.000144,0.000107,0.000126,0.000114,0.000129,0.000128,0.000115,0.000115,0.000129,0.000139,8.1e-05,0.00011,0.000125,0.000144,9.2e-05,0.000121,9.8e-05,9.8e-05,0.00012,0.000125,0.000114,9.8e-05,9.9e-05,0.000125,0.000135,0.000131,0.00012,0.000129,0.000127,0.000134,0.000124,0.000124,0.000126,0.00013,0.000139,0.00014,0.000116,0.000114,0.000116,0.000117,0.000118,0.000124,0.00012,0.000116,0.000119,0.00012,0.000126,0.00012,0.000122,0.000125,0.000125,0.000116,0.000129,0.000123,0.000116,0.000117,0.000122,0.000114,0.000107,0.000129,0.000122,0.000129,0.00011,0.00011,0.000114,0.00011,0.000114,0.00011,0.000124,0.000109,0.000115,0.000118,0.000114,9.3e-05,8.4e-05,0.000112,0.000105,0.000103,8.7e-05,8.4e-05,9.8e-05,8.7e-05,8.7e-05,0.000102,9.2e-05,8.7e-05,0.000102,9.8e-05,0.000105,0.000105,0.0001,8.7e-05,0.0001,8.9e-05,0.000106,0.000106,0.000103,0.000115,9.2e-05,8.4e-05,0.000104],[0.015466,0.014201,0.016481,0.013654,0.013998,0.013778,0.013035,0.013635,0.011909,0.013151,0.014128,0.013921,0.011443,0.013668,0.013171,0.012955,0.013896,0.014882,0.011577,0.013594,0.013872,0.011985,0.01384,0.010134,0.013549,0.012999,0.010696,0.013904,0.013591,0.013899,0.014011,0.013159,0.013985,0.010911,0.013572,0.012113,0.013396,0.013777,0.011664,0.013734,0.013958,0.013209,0.013126,0.013688,0.012992,0.013773,0.00951,0.012027,0.012027,0.012027,0.012027,0.012029,0.010129,0.01342,0.010939,0.013612,0.013777,0.011683,0.011014,0.014437,0.013504,0.010124,0.013205,0.01317,0.012293,0.005348,0.009121,0.013169,0.014139,0.013065,0.01343,0.013372,0.013398,0.013406,0.01356,0.013206,0.013274,0.014144,0.009892,0.009892,0.009892,0.009892,0.013252,0.012546,0.012358,0.013445,0.012961,0.009343,0.013251,0.013098,0.013205,0.010681,0.010478,0.012988,0.013781,0.01229,0.009343,0.011864,0.01352,0.013956,0.010302,0.012027,0.011771,0.011771,0.013156,0.013195,0.013188,0.011771,0.011889,0.013264,0.013217,0.013576,0.013323,0.013445,0.013434,0.013209,0.013211,0.013214,0.013269,0.013805,0.013148,0.013741,0.013376,0.013296,0.013376,0.013319,0.013344,0.013368,0.013424,0.01265,0.01326,0.013361,0.013361,0.013326,0.013417,0.013552,0.013732,0.013212,0.013404,0.013152,0.013286,0.013242,0.013152,0.01315,0.012721,0.014166,0.013532,0.013814,0.01219,0.012592,0.013266,0.012592,0.013266,0.012592,0.01333,0.010828,0.012898,0.013476,0.013401,0.009599,0.009988,0.011825,0.01166,0.011531,0.009548,0.009195,0.011375,0.009548,0.009548,0.011728,0.010925,0.009548,0.011476,0.011464,0.01199,0.01201,0.011579,0.009548,0.011581,0.0106,0.011638,0.011638,0.01171,0.011933,0.010712,0.010125,0.012805],[0.027245,0.025064,0.02822,0.024545,0.019972,0.020667,0.019293,0.024587,0.020292,0.018196,0.017835,0.018709,0.02287,0.020516,0.021057,0.018799,0.024916,0.02425,0.018995,0.01967,0.020326,0.020028,0.02124,0.022185,0.019108,0.014425,0.017411,0.019959,0.024304,0.021451,0.02308,0.019608,0.02374,0.013349,0.018285,0.020341,0.0185,0.017702,0.019538,0.023782,0.023114,0.024056,0.02404,0.019865,0.019891,0.021543,0.014269,0.019422,0.019422,0.019422,0.019422,0.018912,0.01863,0.017262,0.021934,0.023086,0.019154,0.019668,0.015897,0.020095,0.005348,0.013372,0.022589,0.017232,0.020669,0.005372,0.005399,0.021583,0.018399,0.018877,0.017876,0.018487,0.021108,0.018769,0.017072,0.019171,0.017929,0.02157,0.013205,0.013205,0.013205,0.013205,0.018136,0.01453,0.019089,0.019462,0.021311,0.017346,0.02035,0.017176,0.020264,0.02111,0.018771,0.014697,0.019756,0.020399,0.012286,0.01385,0.017575,0.022193,0.013921,0.020521,0.005348,0.005348,0.019122,0.020145,0.015657,0.005348,0.005348,0.019565,0.02188,0.019678,0.018345,0.019179,0.019581,0.020967,0.020102,0.019618,0.019955,0.019645,0.022391,0.021673,0.016133,0.016133,0.016133,0.017738,0.01662,0.018203,0.01662,0.017133,0.019341,0.018828,0.019714,0.017797,0.018118,0.018469,0.015957,0.0169,0.019526,0.019347,0.017113,0.01635,0.019169,0.0169,0.017484,0.015349,0.018152,0.01223,0.0152,0.015468,0.015468,0.015468,0.015468,0.015468,0.013203,0.01547,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.034792,0.030068,0.034995,0.028445,0.029081,0.029222,0.022262,0.029076,0.023238,0.022117,0.025186,0.02447,0.024464,0.027716,0.024953,0.022584,0.030039,0.030259,0.022172,0.025132,0.025341,0.02404,0.026014,0.0241,0.024906,0.021765,0.021818,0.025176,0.026039,0.025744,0.02713,0.022708,0.028101,0.017369,0.024301,0.024474,0.02392,0.027953,0.021831,0.027126,0.026402,0.026443,0.026534,0.025396,0.022946,0.02579,0.018487,0.022642,0.022642,0.022642,0.022642,0.022294,0.020955,0.023929,0.024747,0.027564,0.023861,0.023803,0.019658,0.027293,0.023233,0.016613,0.026383,0.021414,0.024355,0.012998,0.015732,0.024118,0.02977,0.022377,0.02352,0.022132,0.025244,0.023544,0.023435,0.022909,0.022664,0.026003,0.015166,0.015166,0.015166,0.015166,0.021473,0.023011,0.023574,0.022906,0.026763,0.01994,0.023384,0.021158,0.023966,0.023868,0.021343,0.021306,0.023988,0.025855,0.015018,0.020487,0.02323,0.026813,0.017154,0.022562,0.018139,0.018139,0.022321,0.023282,0.021168,0.018139,0.018412,0.02332,0.025145,0.024346,0.022329,0.023963,0.023706,0.02492,0.023014,0.022977,0.023376,0.024096,0.02579,0.026018,0.021575,0.021279,0.021575,0.021803,0.021895,0.023026,0.02234,0.021643,0.022201,0.022402,0.0235,0.022383,0.022705,0.023172,0.023276,0.021548,0.024034,0.022881,0.021555,0.021803,0.022677,0.021148,0.019959,0.024037,0.022606,0.023901,0.020478,0.020435,0.021221,0.020435,0.021221,0.020435,0.023057,0.020213,0.021424,0.021895,0.021221,0.017213,0.015674,0.020887,0.019579,0.019227,0.016174,0.015592,0.018145,0.016174,0.016174,0.019053,0.01705,0.016174,0.01891,0.018242,0.019548,0.019608,0.018565,0.016174,0.018598,0.016606,0.019683,0.019683,0.019245,0.021458,0.017132,0.015641,0.019436],[0.372,0.343173,0.392405,0.336347,0.308458,0.301948,0.304419,0.319039,0.311037,0.288372,0.293839,0.286154,0.333932,0.322917,0.287481,0.290625,0.353612,0.315254,0.292913,0.286154,0.303922,0.285714,0.295707,0.333333,0.285714,0.240621,0.277198,0.318493,0.333333,0.316327,0.310518,0.2976,0.315789,0.223827,0.230198,0.285714,0.30897,0.268786,0.298555,0.320138,0.334532,0.317406,0.317406,0.293839,0.293375,0.300485,0.230769,0.291536,0.291536,0.291536,0.291536,0.291536,0.282675,0.298555,0.331551,0.32069,0.304419,0.288372,0.261236,0.324607,0.005348,0.227941,0.298555,0.290625,0.299517,0.005376,0.005405,0.306931,0.298555,0.283537,0.283537,0.294304,0.296651,0.296178,0.289269,0.290172,0.288372,0.314189,0.229346,0.229346,0.229346,0.229346,0.287481,0.250674,0.295238,0.299035,0.292453,0.274742,0.303922,0.286154,0.291536,0.302439,0.28882,0.24933,0.295707,0.288372,0.216028,0.223289,0.285714,0.306931,0.234552,0.313659,0.005348,0.005348,0.290625,0.299517,0.285276,0.005348,0.005348,0.297125,0.2976,0.28972,0.287037,0.285714,0.287037,0.291994,0.29477,0.295238,0.290172,0.290172,0.295238,0.311037,0.283537,0.283537,0.283537,0.291994,0.282675,0.285714,0.282675,0.291994,0.291536,0.291536,0.292453,0.282675,0.282675,0.298555,0.284839,0.282675,0.282675,0.291536,0.283105,0.282675,0.286154,0.282675,0.282675,0.282675,0.291536,0.221165,0.252374,0.256198,0.256198,0.256198,0.256198,0.256198,0.226553,0.227106,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(acesso_df_closseness, by=list(acesso_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(acesso_df_closseness, by=list(acesso_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-b8cf2bae8c6569817a51" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-b8cf2bae8c6569817a51">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6.1e-05\" data-max=\"7.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"8e-06\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7.1e-05\" data-max=\"0.000117\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.2e-05\" data-max=\"3.6e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000111\" data-max=\"0.000145\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"9e-06\" data-max=\"2.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.011401\" data-max=\"0.013861\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000189\" data-max=\"0.001535\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.013167\" data-max=\"0.021788\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.002311\" data-max=\"0.006784\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.020579\" data-max=\"0.026901\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001779\" data-max=\"0.003842\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172678\" data-max=\"0.310428\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.028412\" data-max=\"0.13602\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[7.4e-05,7.1e-05,6.1e-05],[1e-06,4e-06,8e-06],[0.000117,9.6e-05,7.1e-05],[1.2e-05,2.4e-05,3.6e-05],[0.000145,0.000125,0.000111],[9e-06,1.5e-05,2.1e-05],[0.013861,0.013272,0.011401],[0.000189,0.000705,0.001535],[0.021788,0.017799,0.013167],[0.002311,0.004523,0.006784],[0.026901,0.023237,0.020579],[0.001779,0.002743,0.003842],[0.310428,0.267476,0.172678],[0.028412,0.082813,0.13602]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/acesso_data.RData")
```

