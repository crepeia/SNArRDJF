# SNA Closeness 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var2_data.RData")
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
#var2<-simplify(var2) #Simplify
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
V(var2)$incloseness <- closeness(var2, mode = "in", weights = E(var2)$var2) %>% round(6)
V(var2)$outcloseness <- closeness(var2, mode = "out", weights = E(var2)$var2) %>% round(6)
V(var2)$totalcloseness <- closeness(var2, mode = "total", weights = E(var2)$var2) %>% round(4)
```

###Saving to Environment

```r
var2_incloseness<- closeness(var2, mode = "in", weights = E(var2)$var2) %>% round(6)
var2_outcloseness<- closeness(var2, mode = "out", weights = E(var2)$var2) %>% round(6)
var2_totalcloseness<- closeness(var2, mode = "total", weights = E(var2)$var2) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var2_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 2.900e-05 6.300e-05 6.300e-05 6.195e-05 6.400e-05 6.800e-05
```

```r
sd(var2_incloseness)
```

```
## [1] 7.035801e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var2)$incloseness<-closeness(var2, weights = E(var2)$var2, mode="in")

#Get Variable
V(var2)$var2_color_degree<-round(V(var2)$incloseness,6)

#Creating brewer pallette
vertex_var2_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var2)$var2_color_degree)), "RdBu"))(
            length(unique(V(var2)$var2_color_degree)))

#Saving as Vertex properties 
V(var2)$vertex_var2_color_degree<-
  vertex_var2_color_degree[as.numeric(
  cut(V(var2)$var2_color_degree,
      breaks=length(unique(V(var2)$var2_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var2, es=E(var2), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2))
maxC <- rep(Inf, vcount(var2))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2)$weight)


#PLotting
plot(var2, 
     layout=co,
     edge.color=V(var2)$vertex_var2_color_degree[edge.start],
     edge.arrow.size=closeness(var2, weights = E(var2)$var2, mode="in"),
     edge.width=E(var2)$weight/mean(E(var2)$weight),
     edge.curved = TRUE,
     vertex.color=V(var2)$vertex_var2_color_degree,
     vertex.size=closeness(var2, weights = E(var2)$var2, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2,"LABEL_COR"),
     vertex.label.cex=(closeness(var2, weights = E(var2)$var2, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var2)$var2_color_degree
b<-V(var2)$vertex_var2_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var2, mode="in", weights = E(var2)$var2)), 
             sd(closeness(var2, mode="in", weights = E(var2)$var2))
             )
       )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var2_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0000290 0.0004480 0.0002792 0.0004825 0.0005380
```

```r
sd(var2_outcloseness)
```

```
## [1] 0.0002246621
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var2)$outcloseness<-closeness(var2, weights = E(var2)$var2, mode="out")

#Get Variable
V(var2)$var2_color_degree<-round(V(var2)$outcloseness,6)

#Creating brewer pallette
vertex_var2_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var2)$var2_color_degree)), "RdBu"))(
            length(unique(V(var2)$var2_color_degree)))

#Saving as Vertex properties 
V(var2)$vertex_var2_color_degree<-
  vertex_var2_color_degree[as.numeric(
  cut(V(var2)$var2_color_degree,
      breaks=length(unique(V(var2)$var2_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var2, es=E(var2), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2))
maxC <- rep(Inf, vcount(var2))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2)$weight)


#PLotting
plot(var2, 
     layout=co,
     edge.color=V(var2)$vertex_var2_color_degree[edge.start],
     edge.arrow.size=closeness(var2, weights = E(var2)$var2, mode="out"),
     edge.width=E(var2)$weight/2*mean(E(var2)$weight),
     edge.curved = TRUE,
     vertex.color=V(var2)$vertex_var2_color_degree,
     vertex.size=closeness(var2, weights = E(var2)$var2, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2,"LABEL_COR"),
     vertex.label.cex=closeness(var2, weights = E(var2)$var2, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var2)$var2_color_degree
b<-V(var2)$vertex_var2_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var2, mode="out", weights = E(var2)$var2)), 
             sd(closeness(var2, mode="out", weights = E(var2)$var2))
             )
       )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var2_totalcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0006120 0.0006440 0.0006133 0.0006480 0.0007160
```

```r
sd(var2_totalcloseness)
```

```
## [1] 0.0001105428
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var2)$allcloseness<-closeness(var2, weights = E(var2)$var2, mode="all")

#Get Variable
V(var2)$var2_color_degree<-round(V(var2)$allcloseness,6)

#Creating brewer pallette
vertex_var2_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var2)$var2_color_degree)), "RdBu"))(
            length(unique(V(var2)$var2_color_degree)))

#Saving as Vertex properties 
V(var2)$vertex_var2_color_degree<-
  vertex_var2_color_degree[as.numeric(
  cut(V(var2)$var2_color_degree,
      breaks=length(unique(V(var2)$var2_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var2, es=E(var2), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2))
maxC <- rep(Inf, vcount(var2))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2)$weight)


#PLotting
plot(var2, 
     layout=co,
     edge.color=V(var2)$vertex_var2_color_degree[edge.start],
     edge.arrow.size=closeness(var2, weights = E(var2)$var2, mode="all"),
     edge.width=E(var2)$weight/2*mean(E(var2)$weight),
     edge.curved = TRUE,
     vertex.color=V(var2)$vertex_var2_color_degree,
     vertex.size=closeness(var2, weights = E(var2)$var2, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2,"LABEL_COR"),
     vertex.label.cex=(closeness(var2, weights = E(var2)$var2, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var2)$var2_color_degree
b<-V(var2)$vertex_var2_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var2, mode="all", weights = E(var2)$var2)), 
             sd(closeness(var2, mode="all", weights = E(var2)$var2))
             )
       )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var2)$incloseness_n <- closeness(var2, mode = "in",, weights = E(var2)$var2, normalized = T) %>% round(10)
V(var2)$outcloseness_n <- closeness(var2, mode = "out", normalized = T, weights = E(var2)$var2) %>% round(6)
V(var2)$totalcloseness_n <- closeness(var2, mode = "total", normalized = T, weights = E(var2)$var2) %>% round(6)
```

###Saving to Environment

```r
var2_incloseness_n<- closeness(var2, mode = "in", normalized = T, weights = E(var2)$var2) %>% round(6)
var2_outcloseness_n<- closeness(var2, mode = "out", normalized = T, weights = E(var2)$var2) %>% round(6)
var2_totalcloseness_n<- closeness(var2, mode = "total", normalized = T, weights = E(var2)$var2) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var2_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.011690 0.011800 0.011530 0.011830 0.012570
```

```r
sd(var2_incloseness_n)
```

```
## [1] 0.001320909
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var2)$incloseness_n<-closeness(var2, weights = E(var2)$var2, mode="in", normalized = T)

#Get Variable
V(var2)$var2_color_degree<-round(V(var2)$incloseness_n,6)

#Creating brewer pallette
vertex_var2_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var2)$var2_color_degree)), "RdBu"))(
            length(unique(V(var2)$var2_color_degree)))

#Saving as Vertex properties 
V(var2)$vertex_var2_color_degree<-
  vertex_var2_color_degree[as.numeric(
  cut(V(var2)$var2_color_degree,
      breaks=length(unique(V(var2)$var2_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var2, es=E(var2), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2))
maxC <- rep(Inf, vcount(var2))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2)$weight)


#PLotting
plot(var2, 
     layout=co,
     edge.color=V(var2)$vertex_var2_color_degree[edge.start],
     edge.arrow.size=closeness(var2, weights = E(var2)$var2, mode="in",normalized = T),
     edge.width=E(var2)$weight/10*mean(E(var2)$weight),
     edge.curved = TRUE,
     vertex.color=V(var2)$vertex_var2_color_degree,
     vertex.size=(closeness(var2, weights = E(var2)$var2, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2,"LABEL_COR"),
     vertex.label.cex=closeness(var2, weights = E(var2)$var2, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var2)$var2_color_degree
b<-V(var2)$vertex_var2_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var2, mode="in", weights = E(var2)$var2, normalized = T)), 
             sd(closeness(var2, mode="in", weights = E(var2)$var2, normalized = T))
             )
       )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var2_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.005348 0.083300 0.051910 0.089750 0.100000
```

```r
sd(var2_outcloseness_n)
```

```
## [1] 0.04181229
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var2)$outcloseness_n<-closeness(var2, weights = E(var2)$var2, mode="out", normalized = T)

#Get Variable
V(var2)$var2_color_degree<-round(V(var2)$outcloseness_n,6)

#Creating brewer pallette
vertex_var2_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var2)$var2_color_degree)), "RdBu"))(
            length(unique(V(var2)$var2_color_degree)))

#Saving as Vertex properties 
V(var2)$vertex_var2_color_degree<-
  vertex_var2_color_degree[as.numeric(
  cut(V(var2)$var2_color_degree,
      breaks=length(unique(V(var2)$var2_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var2, es=E(var2), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2))
maxC <- rep(Inf, vcount(var2))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2)$weight)


#PLotting
plot(var2, 
     layout=co,
     edge.color=V(var2)$vertex_var2_color_degree[edge.start],
     edge.arrow.size=closeness(var2, weights = E(var2)$var2, mode="out",normalized = T),
     edge.width=E(var2)$weight/10*mean(E(var2)$weight),
     edge.curved = TRUE,
     vertex.color=V(var2)$vertex_var2_color_degree,
     vertex.size=(closeness(var2, weights = E(var2)$var2, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2,"LABEL_COR"),
     vertex.label.cex=closeness(var2, weights = E(var2)$var2, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var2)$var2_color_degree
b<-V(var2)$vertex_var2_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var2, mode="out", weights = E(var2)$var2, normalized = T)), 
             sd(closeness(var2, mode="out", weights = E(var2)$var2, normalized = T))
             )
       )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var2_totalcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.113900 0.119800 0.114100 0.120500 0.133200
```

```r
sd(var2_totalcloseness_n)
```

```
## [1] 0.02056888
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var2)$allcloseness_n<-closeness(var2, weights = E(var2)$var2, mode="all", normalized = T)

#Get Variable
V(var2)$var2_color_degree<-round(V(var2)$allcloseness_n,6)

#Creating brewer pallette
vertex_var2_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var2)$var2_color_degree)), "RdBu"))(
            length(unique(V(var2)$var2_color_degree)))

#Saving as Vertex properties 
V(var2)$vertex_var2_color_degree<-
  vertex_var2_color_degree[as.numeric(
  cut(V(var2)$var2_color_degree,
      breaks=length(unique(V(var2)$var2_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var2, es=E(var2), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2))
maxC <- rep(Inf, vcount(var2))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2)$weight)


#PLotting
plot(var2, 
     layout=co,
     edge.color=V(var2)$vertex_var2_color_degree[edge.start],
     edge.arrow.size=closeness(var2, weights = E(var2)$var2, mode="all",normalized = T),
     edge.width=E(var2)$weight/10*mean(E(var2)$weight),
     edge.curved = TRUE,
     vertex.color=V(var2)$vertex_var2_color_degree,
     vertex.size=(closeness(var2, weights = E(var2)$var2, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2,"LABEL_COR"),
     vertex.label.cex=closeness(var2, weights = E(var2)$var2, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var2)$var2_color_degree
b<-V(var2)$vertex_var2_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var2, mode="all", weights = E(var2)$var2, normalized = T)), 
             sd(closeness(var2, mode="all", weights = E(var2)$var2, normalized = T))
             )
       )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var2)$incloseness_n <- closeness(var2, weights = E(var2)$var2, mode = "in", normalized = T) %>% round(6)
V(var2)$outcloseness_n <- closeness(var2, weights = E(var2)$var2, mode = "out", normalized = T) %>% round(6)
V(var2)$totalcloseness_n <- closeness(var2, weights = E(var2)$var2, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var2)$var2_centr_closeness<- centralization.closeness(var2)$res
var2_centr_closeness<- centralization.closeness(var2)$res
var2_centr_closeness_all<- centralization.closeness(var2)
```

###Centralization

```r
var2_centr_closeness_all$centralization
```

```
## [1] 0.04861136
```

###Theoretical Max

```r
var2_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var2)$var2_centr_closeness<- centralization.closeness(var2)$res

#Get Variable
V(var2)$var2_color_degree<-round(V(var2)$var2_centr_closeness,6)

#Creating brewer pallette
vertex_var2_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var2)$var2_color_degree)), "Spectral"))(
            length(unique(V(var2)$var2_color_degree)))

#Saving as Vertex properties 
V(var2)$vertex_var2_color_degree<-
  vertex_var2_color_degree[as.numeric(
  cut(V(var2)$var2_color_degree,
      breaks=length(unique(V(var2)$var2_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var2, es=E(var2), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2))
maxC <- rep(Inf, vcount(var2))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2)$weight)


#PLotting
plot(var2, 
     layout=co,
     edge.color=V(var2)$vertex_var2_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var2)$res,
     edge.width=E(var2)$weight/10*mean(E(var2)$weight),
     edge.curved = TRUE,
     vertex.color=V(var2)$vertex_var2_color_degree,
     vertex.size=centralization.closeness(var2)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var2)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var2)$var2_color_degree
b<-V(var2)$vertex_var2_color_degree
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
  title("Network Centralization Closeness - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var2)$res), 
             sd(centralization.closeness(var2)$res)
             )
       )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var2_incloseness<- closeness(var2, weights = E(var2)$var2, mode = "in") %>% round(6)
var2_outcloseness<- closeness(var2, weights = E(var2)$var2, mode = "out") %>% round(6)
var2_totalcloseness<- closeness(var2, weights = E(var2)$var2, mode = "total") %>% round(6)
var2_incloseness_n<- closeness(var2,weights = E(var2)$var2, mode = "in", normalized = T) %>% round(6)
var2_outcloseness_n<- closeness(var2,weights = E(var2)$var2, mode = "out", normalized = T) %>% round(6)
var2_totalcloseness_n<- closeness(var2,weights = E(var2)$var2, mode = "total", normalized = T) %>% round(6)
var2_centr_closeness <- centralization.closeness(var2)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var2_df_closseness <- data.frame(
var2_incloseness,
var2_outcloseness,
var2_totalcloseness,
var2_incloseness_n,
var2_outcloseness_n,
var2_totalcloseness_n,
var2_centr_closeness) %>% round(6)

#Adding type
var2_df_closseness <-cbind(var2_df_closseness, V(var2)$LABEL_COR)

#Adding names
names(var2_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var2_df_closseness<-var2_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var2_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-05f98203998d070af601" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-05f98203998d070af601">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"6.8e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000538\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000716\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.012574\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.1\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.133238\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.1\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.5e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.2e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.8e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.2e-05,2.9e-05,6.2e-05,6.2e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,2.9e-05,6.3e-05,6.2e-05,6.4e-05,6.4e-05,6.2e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,2.9e-05,6.2e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.6e-05,6.6e-05,6.6e-05,6.6e-05,6.4e-05,6.3e-05,6.7e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,2.9e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.7e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,2.9e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.2e-05,6.2e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,2.9e-05,6.3e-05,6.2e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.2e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,2.9e-05,2.9e-05],[0.000515,0.000509,0.000524,0.000509,0.000483,2.9e-05,0.000491,0.000505,0.000484,0.000465,0.000471,2.9e-05,0.000494,0.000482,2.9e-05,0.000488,0.000517,2.9e-05,2.9e-05,0.000452,0.000487,0.000478,0.000485,0.000538,0.000517,0.000438,0.000473,0.000495,0.000505,0.000492,0.000486,0.000482,0.000476,2.9e-05,0.000416,0.000476,0.000485,0.000459,0.000482,0.000508,0.000515,0.000487,0.000487,0.00048,0.000511,2.9e-05,2.9e-05,0.000488,0.000488,0.000488,0.000488,0.000488,2.9e-05,0.00049,0.000499,0.00048,2.9e-05,2.9e-05,2.9e-05,0.000483,2.9e-05,0.000414,2.9e-05,0.00048,0.00047,2.9e-05,2.9e-05,0.000496,0.000445,2.9e-05,2.9e-05,0.000482,2.9e-05,0.000467,0.000483,0.00048,0.000448,0.000502,3e-05,3e-05,3e-05,3e-05,2.9e-05,0.000491,3e-05,0.000481,0.000446,2.9e-05,0.000492,0.000449,0.000484,2.9e-05,0.000414,0.000447,0.000483,0.000483,2.9e-05,2.9e-05,0.000441,2.9e-05,2.9e-05,0.000492,2.9e-05,2.9e-05,2.9e-05,0.000487,0.000455,2.9e-05,2.9e-05,2.9e-05,0.000448,0.000459,0.000448,0.000479,2.9e-05,0.000455,0.000473,0.000478,0.000484,0.00048,2.9e-05,0.000483,2.9e-05,2.9e-05,2.9e-05,0.000448,0.000479,0.00048,0.000479,0.000448,2.9e-05,2.9e-05,0.000481,0.000479,2.9e-05,0.000483,0.00048,0.000479,0.000479,2.9e-05,0.000448,0.000479,0.00048,0.000479,0.000526,0.000479,0.000481,2.9e-05,0.000476,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000445,0.000445,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000705,0.000674,0.000716,0.000663,0.000651,0.000649,0.000657,0.000669,0.000645,0.0006,0.000654,0.000652,0.000641,0.000622,0.000644,0.000648,0.000678,0.00065,0.000509,0.000646,0.000655,0.000647,0.000652,0.00068,0.000626,0.000619,0.000659,0.00066,0.000657,0.000655,0.000652,0.000648,0.000656,0.000612,0.00065,0.000641,0.000649,0.000605,0.000644,0.000664,0.000663,0.000649,0.000647,0.000645,0.000647,0.000654,0.000612,0.000647,0.000647,0.000647,0.000647,0.000647,2.9e-05,0.000659,0.000635,0.000654,0.00065,0.00057,0.000612,0.000669,0.00062,0.000565,0.000646,0.000645,0.000606,2.9e-05,0.00057,0.000638,0.000617,0.000642,0.000644,0.000645,0.000661,0.00066,0.000648,0.000645,0.000664,0.000664,0.000561,0.000561,0.000561,0.000561,0.000644,0.000619,0.000622,0.000645,0.00062,0.000576,0.000654,0.000646,0.000645,2.9e-05,0.000565,0.000629,0.000646,0.000662,0.000576,0.000599,0.000647,0.000648,0.00056,0.000652,0.000585,0.000585,0.000645,0.000648,0.000645,0.000585,0.000593,0.000649,0.000649,0.000648,0.000645,0.000644,0.000646,0.000645,0.000645,0.000646,0.000646,0.000647,0.000645,0.00065,0.000644,0.000644,0.000644,0.000645,0.000644,0.000646,0.000647,0.000633,0.000644,0.000644,0.000644,0.000646,0.000644,0.000647,0.000644,0.000648,0.000648,0.000644,0.000647,0.000645,0.000644,0.000645,0.000635,0.000655,0.000651,0.000644,0.000609,0.000631,0.000644,0.000631,0.000644,0.000631,0.000614,0.000619,0.000631,0.000644,0.000644,0.000606,2.9e-05,0.000613,0.00057,0.00057,0.000612,0.00057,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.00057,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.00062,0.000612,0.000612,2.9e-05,2.9e-05],[0.011834,0.011813,0.011861,0.011812,0.011818,0.012098,0.011794,0.011766,0.011692,0.011697,0.011817,0.011957,0.011622,0.011748,0.011941,0.01179,0.011817,0.011951,0.012574,0.011796,0.011815,0.011743,0.011812,0.011596,0.005348,0.011526,0.011528,0.011819,0.011814,0.011812,0.011819,0.011804,0.01182,0.011658,0.011814,0.011695,0.011759,0.011746,0.011728,0.011774,0.011813,0.01181,0.011801,0.011759,0.011737,0.01196,0.011658,0.011699,0.011699,0.011699,0.011699,0.011699,0.005348,0.011781,0.011523,0.011813,0.01195,0.011583,0.011658,0.011824,0.011837,0.011689,0.011944,0.011804,0.011695,0.005348,0.011583,0.011762,0.011742,0.01194,0.011941,0.011802,0.011947,0.011807,0.011809,0.011803,0.011807,0.011831,0.012358,0.012358,0.012358,0.012358,0.011945,0.01167,0.012444,0.011805,0.011751,0.011683,0.011803,0.011801,0.0118,0.005348,0.011689,0.011763,0.011801,0.011711,0.011821,0.011834,0.011806,0.011951,0.012509,0.011767,0.011865,0.011865,0.011945,0.011802,0.011804,0.011865,0.011882,0.011948,0.011807,0.01181,0.011805,0.011802,0.011945,0.011804,0.011806,0.011806,0.011802,0.011806,0.011945,0.011799,0.011944,0.011944,0.011944,0.011803,0.011801,0.011804,0.011807,0.011776,0.011941,0.011944,0.011801,0.011803,0.011944,0.011806,0.011803,0.011805,0.011805,0.011941,0.011809,0.011804,0.0118,0.011804,0.005348,0.011819,0.011807,0.011941,0.011751,0.011912,0.011941,0.011912,0.011941,0.011912,0.011455,0.011528,0.011912,0.011941,0.011941,0.011656,0.005348,0.011662,0.011583,0.011719,0.011658,0.011719,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011583,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.01185,0.011658,0.011658,0.005348,0.005348],[0.095778,0.094753,0.097433,0.094753,0.089855,0.005348,0.091356,0.093939,0.090116,0.086471,0.087653,0.005348,0.091897,0.089639,0.005348,0.090732,0.096074,0.005348,0.005348,0.084087,0.090511,0.088953,0.090247,0.1,0.096174,0.0814,0.088027,0.092079,0.093845,0.091535,0.090423,0.089639,0.088571,0.005348,0.077371,0.088529,0.09016,0.085399,0.089682,0.094416,0.095876,0.090511,0.090511,0.089209,0.094995,0.005348,0.005348,0.090732,0.090732,0.090732,0.090732,0.090732,0.005348,0.091087,0.092814,0.089337,0.005348,0.005348,0.005348,0.089812,0.005348,0.077051,0.005348,0.089294,0.087365,0.005348,0.005405,0.09217,0.082777,0.005348,0.005348,0.089595,0.005348,0.086916,0.089899,0.089294,0.083371,0.093373,0.005524,0.005524,0.005524,0.005524,0.005348,0.091311,0.005525,0.089552,0.082888,0.005376,0.09149,0.083483,0.089985,0.005348,0.077051,0.08311,0.089812,0.089899,0.005348,0.005348,0.082047,0.005348,0.005376,0.091581,0.005348,0.005348,0.005348,0.090643,0.084661,0.005348,0.005348,0.005348,0.083333,0.085321,0.083333,0.089166,0.005376,0.084661,0.088027,0.088995,0.089942,0.089209,0.005348,0.089899,0.005348,0.005348,0.005348,0.083296,0.089166,0.089337,0.089166,0.083333,0.005348,0.005348,0.089552,0.089166,0.005348,0.089899,0.089251,0.089166,0.089166,0.005348,0.083333,0.089166,0.089209,0.089166,0.097843,0.089166,0.089552,0.005348,0.088571,0.005348,0.005348,0.005348,0.005348,0.005348,0.08274,0.082703,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.131171,0.125337,0.133238,0.12326,0.121094,0.120701,0.122127,0.124498,0.12,0.111578,0.121648,0.121331,0.119307,0.115744,0.119768,0.120544,0.126102,0.120858,0.094705,0.120233,0.121887,0.12031,0.121252,0.126531,0.116468,0.115099,0.12253,0.122691,0.122127,0.121887,0.121331,0.120466,0.121967,0.113901,0.120858,0.119231,0.120779,0.112523,0.119845,0.123588,0.12326,0.120623,0.120388,0.119923,0.120388,0.121569,0.113901,0.12031,0.12031,0.12031,0.12031,0.12031,0.005348,0.12261,0.11802,0.121648,0.120858,0.105983,0.113901,0.124498,0.115385,0.105144,0.120077,0.12,0.112796,0.005348,0.106104,0.118622,0.114673,0.119461,0.119768,0.119923,0.122853,0.122772,0.120544,0.119923,0.123588,0.123588,0.10426,0.10426,0.10426,0.10426,0.119845,0.115099,0.115672,0.12,0.115313,0.107081,0.121648,0.120077,0.119923,0.005348,0.105144,0.116908,0.120155,0.123097,0.107081,0.111444,0.12031,0.120544,0.104202,0.121331,0.108899,0.108899,0.119923,0.120544,0.119923,0.108899,0.11032,0.120779,0.120779,0.120466,0.12,0.119845,0.120077,0.119923,0.12,0.120077,0.120077,0.12031,0.119923,0.120858,0.119845,0.119845,0.119845,0.12,0.119768,0.120077,0.120388,0.117722,0.119768,0.119845,0.119768,0.120155,0.119845,0.12031,0.119845,0.120466,0.120544,0.119845,0.12031,0.119923,0.119768,0.119923,0.118095,0.121887,0.121094,0.119768,0.113276,0.117424,0.119768,0.117424,0.119768,0.117424,0.11418,0.11517,0.117424,0.119768,0.119768,0.112796,0.005348,0.11411,0.105983,0.106043,0.113901,0.106043,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.105983,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.115313,0.113901,0.113901,0.005348,0.005348],[0.095778,0.094753,0.097433,0.094753,0.089855,0.005348,0.091356,0.093939,0.090116,0.086471,0.087653,0.005348,0.091897,0.089639,0.005348,0.090732,0.096074,0.005348,0.005348,0.084087,0.090511,0.088953,0.090247,0.1,0.096174,0.0814,0.088027,0.092079,0.093845,0.091535,0.090423,0.089639,0.088571,0.005348,0.077371,0.088529,0.09016,0.085399,0.089682,0.094416,0.095876,0.090511,0.090511,0.089209,0.094995,0.005348,0.005348,0.090732,0.090732,0.090732,0.090732,0.090732,0.005348,0.091087,0.092814,0.089337,0.005348,0.005348,0.005348,0.089812,0.005348,0.077051,0.005348,0.089294,0.087365,0.005348,0.005405,0.09217,0.082777,0.005348,0.005348,0.089595,0.005348,0.086916,0.089899,0.089294,0.083371,0.093373,0.005524,0.005524,0.005524,0.005524,0.005348,0.091311,0.005525,0.089552,0.082888,0.005376,0.09149,0.083483,0.089985,0.005348,0.077051,0.08311,0.089812,0.089899,0.005348,0.005348,0.082047,0.005348,0.005376,0.091581,0.005348,0.005348,0.005348,0.090643,0.084661,0.005348,0.005348,0.005348,0.083333,0.085321,0.083333,0.089166,0.005376,0.084661,0.088027,0.088995,0.089942,0.089209,0.005348,0.089899,0.005348,0.005348,0.005348,0.083296,0.089166,0.089337,0.089166,0.083333,0.005348,0.005348,0.089552,0.089166,0.005348,0.089899,0.089251,0.089166,0.089166,0.005348,0.083333,0.089166,0.089209,0.089166,0.097843,0.089166,0.089552,0.005348,0.088571,0.005348,0.005348,0.005348,0.005348,0.005348,0.08274,0.082703,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var2_df_closseness, by=list(var2_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var2_df_closseness, by=list(var2_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-0a5993888e323b8e76bc" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-0a5993888e323b8e76bc">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"6.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000524\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4e-06\" data-max=\"0.000272\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000716\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3e-06\" data-max=\"0.000189\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.011951\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2e-06\" data-max=\"0.002684\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.097433\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000617\" data-max=\"0.050681\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.133238\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000464\" data-max=\"0.03518\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.097433\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000617\" data-max=\"0.050681\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[6.4e-05,6.1e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,5.9e-05,6.3e-05,6.4e-05,6.4e-05,5.8e-05,2.9e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05],[1e-06,9e-06,null,null,null,null,0,null,null,null,null,1.1e-05,0,0,1e-06,1.4e-05,null,null,null,0,null,0,0],[0.000489,9.7e-05,0.000452,0.000509,0.000491,0.000483,0.000487,0.000524,0.000485,0.000481,0.000502,0.000352,0.000484,0.00038,0.000343,0.000411,2.9e-05,2.9e-05,0.000515,0.000332,2.9e-05,0.000167,0.000344],[2.3e-05,0.000163,null,null,null,null,8e-06,null,null,null,null,0.0002,4e-06,0.000201,0.000272,0.00019,null,null,null,0.000224,null,0.000209,0.000204],[0.000643,0.000568,0.000646,0.000674,0.000657,0.000669,0.000657,0.000716,0.000652,0.000651,0.000664,0.000558,0.000636,0.000655,0.000659,0.000622,2.9e-05,0.000648,0.000705,0.000623,0.00065,0.000641,0.000647],[2.9e-05,0.000146,null,null,null,null,3e-06,null,null,null,null,0.000189,1.9e-05,4e-06,1.6e-05,4.7e-05,null,null,null,2.9e-05,null,6e-06,5e-06],[0.011795,0.011312,0.011796,0.011813,0.011794,0.011824,0.011818,0.011861,0.011812,0.011807,0.011831,0.011056,0.011779,0.011846,0.011911,0.010742,0.005348,0.011951,0.011834,0.01177,0.011951,0.011891,0.011844],[3.8e-05,0.001611,null,null,null,null,2e-06,null,null,null,null,0.002012,4.4e-05,6.4e-05,0.000162,0.002684,null,null,null,7.8e-05,null,6.5e-05,6.5e-05],[0.091006,0.018011,0.084087,0.094753,0.091356,0.089812,0.090585,0.097433,0.090247,0.089552,0.093373,0.065442,0.090075,0.070624,0.063759,0.076463,0.005348,0.005348,0.095778,0.061739,0.005348,0.031008,0.063946],[0.004218,0.030264,null,null,null,null,0.001458,null,null,null,null,0.03715,0.000617,0.037347,0.050681,0.035377,null,null,null,0.041674,null,0.038836,0.038065],[0.119501,0.105675,0.120233,0.125337,0.122127,0.124498,0.122155,0.133238,0.121252,0.121094,0.123588,0.103702,0.118184,0.12174,0.122632,0.115756,0.005348,0.120544,0.131171,0.115957,0.120858,0.119281,0.120359],[0.00543,0.027181,null,null,null,null,0.000464,null,null,null,null,0.03518,0.00345,0.000675,0.003011,0.00868,null,null,null,0.005426,null,0.001123,0.000881],[0.091006,0.018011,0.084087,0.094753,0.091356,0.089812,0.090585,0.097433,0.090247,0.089552,0.093373,0.065442,0.090075,0.070624,0.063759,0.076463,0.005348,0.005348,0.095778,0.061739,0.005348,0.031008,0.063946],[0.004218,0.030264,null,null,null,null,0.001458,null,null,null,null,0.03715,0.000617,0.037347,0.050681,0.035377,null,null,null,0.041674,null,0.038836,0.038065]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var2_df_closseness <- data.frame(
var2_incloseness,
var2_outcloseness,
var2_totalcloseness,
var2_incloseness_n,
var2_outcloseness_n,
var2_totalcloseness_n,
var2_centr_closeness) %>% round(6)

#Adding type
var2_df_closseness <-cbind(var2_df_closseness, V(var2)$TIPO1)

#Adding names
names(var2_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var2_df_closseness<-var2_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var2_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-0bc77049157f2380e9f9" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-0bc77049157f2380e9f9">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"6.8e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000538\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000716\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.012574\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.1\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.133238\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.1\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.5e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.2e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.8e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.2e-05,2.9e-05,6.2e-05,6.2e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,2.9e-05,6.3e-05,6.2e-05,6.4e-05,6.4e-05,6.2e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,2.9e-05,6.2e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.6e-05,6.6e-05,6.6e-05,6.6e-05,6.4e-05,6.3e-05,6.7e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,2.9e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.7e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,2.9e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.2e-05,6.2e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,2.9e-05,6.3e-05,6.2e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.2e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,2.9e-05,2.9e-05],[0.000515,0.000509,0.000524,0.000509,0.000483,2.9e-05,0.000491,0.000505,0.000484,0.000465,0.000471,2.9e-05,0.000494,0.000482,2.9e-05,0.000488,0.000517,2.9e-05,2.9e-05,0.000452,0.000487,0.000478,0.000485,0.000538,0.000517,0.000438,0.000473,0.000495,0.000505,0.000492,0.000486,0.000482,0.000476,2.9e-05,0.000416,0.000476,0.000485,0.000459,0.000482,0.000508,0.000515,0.000487,0.000487,0.00048,0.000511,2.9e-05,2.9e-05,0.000488,0.000488,0.000488,0.000488,0.000488,2.9e-05,0.00049,0.000499,0.00048,2.9e-05,2.9e-05,2.9e-05,0.000483,2.9e-05,0.000414,2.9e-05,0.00048,0.00047,2.9e-05,2.9e-05,0.000496,0.000445,2.9e-05,2.9e-05,0.000482,2.9e-05,0.000467,0.000483,0.00048,0.000448,0.000502,3e-05,3e-05,3e-05,3e-05,2.9e-05,0.000491,3e-05,0.000481,0.000446,2.9e-05,0.000492,0.000449,0.000484,2.9e-05,0.000414,0.000447,0.000483,0.000483,2.9e-05,2.9e-05,0.000441,2.9e-05,2.9e-05,0.000492,2.9e-05,2.9e-05,2.9e-05,0.000487,0.000455,2.9e-05,2.9e-05,2.9e-05,0.000448,0.000459,0.000448,0.000479,2.9e-05,0.000455,0.000473,0.000478,0.000484,0.00048,2.9e-05,0.000483,2.9e-05,2.9e-05,2.9e-05,0.000448,0.000479,0.00048,0.000479,0.000448,2.9e-05,2.9e-05,0.000481,0.000479,2.9e-05,0.000483,0.00048,0.000479,0.000479,2.9e-05,0.000448,0.000479,0.00048,0.000479,0.000526,0.000479,0.000481,2.9e-05,0.000476,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000445,0.000445,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000705,0.000674,0.000716,0.000663,0.000651,0.000649,0.000657,0.000669,0.000645,0.0006,0.000654,0.000652,0.000641,0.000622,0.000644,0.000648,0.000678,0.00065,0.000509,0.000646,0.000655,0.000647,0.000652,0.00068,0.000626,0.000619,0.000659,0.00066,0.000657,0.000655,0.000652,0.000648,0.000656,0.000612,0.00065,0.000641,0.000649,0.000605,0.000644,0.000664,0.000663,0.000649,0.000647,0.000645,0.000647,0.000654,0.000612,0.000647,0.000647,0.000647,0.000647,0.000647,2.9e-05,0.000659,0.000635,0.000654,0.00065,0.00057,0.000612,0.000669,0.00062,0.000565,0.000646,0.000645,0.000606,2.9e-05,0.00057,0.000638,0.000617,0.000642,0.000644,0.000645,0.000661,0.00066,0.000648,0.000645,0.000664,0.000664,0.000561,0.000561,0.000561,0.000561,0.000644,0.000619,0.000622,0.000645,0.00062,0.000576,0.000654,0.000646,0.000645,2.9e-05,0.000565,0.000629,0.000646,0.000662,0.000576,0.000599,0.000647,0.000648,0.00056,0.000652,0.000585,0.000585,0.000645,0.000648,0.000645,0.000585,0.000593,0.000649,0.000649,0.000648,0.000645,0.000644,0.000646,0.000645,0.000645,0.000646,0.000646,0.000647,0.000645,0.00065,0.000644,0.000644,0.000644,0.000645,0.000644,0.000646,0.000647,0.000633,0.000644,0.000644,0.000644,0.000646,0.000644,0.000647,0.000644,0.000648,0.000648,0.000644,0.000647,0.000645,0.000644,0.000645,0.000635,0.000655,0.000651,0.000644,0.000609,0.000631,0.000644,0.000631,0.000644,0.000631,0.000614,0.000619,0.000631,0.000644,0.000644,0.000606,2.9e-05,0.000613,0.00057,0.00057,0.000612,0.00057,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.00057,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.00062,0.000612,0.000612,2.9e-05,2.9e-05],[0.011834,0.011813,0.011861,0.011812,0.011818,0.012098,0.011794,0.011766,0.011692,0.011697,0.011817,0.011957,0.011622,0.011748,0.011941,0.01179,0.011817,0.011951,0.012574,0.011796,0.011815,0.011743,0.011812,0.011596,0.005348,0.011526,0.011528,0.011819,0.011814,0.011812,0.011819,0.011804,0.01182,0.011658,0.011814,0.011695,0.011759,0.011746,0.011728,0.011774,0.011813,0.01181,0.011801,0.011759,0.011737,0.01196,0.011658,0.011699,0.011699,0.011699,0.011699,0.011699,0.005348,0.011781,0.011523,0.011813,0.01195,0.011583,0.011658,0.011824,0.011837,0.011689,0.011944,0.011804,0.011695,0.005348,0.011583,0.011762,0.011742,0.01194,0.011941,0.011802,0.011947,0.011807,0.011809,0.011803,0.011807,0.011831,0.012358,0.012358,0.012358,0.012358,0.011945,0.01167,0.012444,0.011805,0.011751,0.011683,0.011803,0.011801,0.0118,0.005348,0.011689,0.011763,0.011801,0.011711,0.011821,0.011834,0.011806,0.011951,0.012509,0.011767,0.011865,0.011865,0.011945,0.011802,0.011804,0.011865,0.011882,0.011948,0.011807,0.01181,0.011805,0.011802,0.011945,0.011804,0.011806,0.011806,0.011802,0.011806,0.011945,0.011799,0.011944,0.011944,0.011944,0.011803,0.011801,0.011804,0.011807,0.011776,0.011941,0.011944,0.011801,0.011803,0.011944,0.011806,0.011803,0.011805,0.011805,0.011941,0.011809,0.011804,0.0118,0.011804,0.005348,0.011819,0.011807,0.011941,0.011751,0.011912,0.011941,0.011912,0.011941,0.011912,0.011455,0.011528,0.011912,0.011941,0.011941,0.011656,0.005348,0.011662,0.011583,0.011719,0.011658,0.011719,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011583,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.01185,0.011658,0.011658,0.005348,0.005348],[0.095778,0.094753,0.097433,0.094753,0.089855,0.005348,0.091356,0.093939,0.090116,0.086471,0.087653,0.005348,0.091897,0.089639,0.005348,0.090732,0.096074,0.005348,0.005348,0.084087,0.090511,0.088953,0.090247,0.1,0.096174,0.0814,0.088027,0.092079,0.093845,0.091535,0.090423,0.089639,0.088571,0.005348,0.077371,0.088529,0.09016,0.085399,0.089682,0.094416,0.095876,0.090511,0.090511,0.089209,0.094995,0.005348,0.005348,0.090732,0.090732,0.090732,0.090732,0.090732,0.005348,0.091087,0.092814,0.089337,0.005348,0.005348,0.005348,0.089812,0.005348,0.077051,0.005348,0.089294,0.087365,0.005348,0.005405,0.09217,0.082777,0.005348,0.005348,0.089595,0.005348,0.086916,0.089899,0.089294,0.083371,0.093373,0.005524,0.005524,0.005524,0.005524,0.005348,0.091311,0.005525,0.089552,0.082888,0.005376,0.09149,0.083483,0.089985,0.005348,0.077051,0.08311,0.089812,0.089899,0.005348,0.005348,0.082047,0.005348,0.005376,0.091581,0.005348,0.005348,0.005348,0.090643,0.084661,0.005348,0.005348,0.005348,0.083333,0.085321,0.083333,0.089166,0.005376,0.084661,0.088027,0.088995,0.089942,0.089209,0.005348,0.089899,0.005348,0.005348,0.005348,0.083296,0.089166,0.089337,0.089166,0.083333,0.005348,0.005348,0.089552,0.089166,0.005348,0.089899,0.089251,0.089166,0.089166,0.005348,0.083333,0.089166,0.089209,0.089166,0.097843,0.089166,0.089552,0.005348,0.088571,0.005348,0.005348,0.005348,0.005348,0.005348,0.08274,0.082703,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.131171,0.125337,0.133238,0.12326,0.121094,0.120701,0.122127,0.124498,0.12,0.111578,0.121648,0.121331,0.119307,0.115744,0.119768,0.120544,0.126102,0.120858,0.094705,0.120233,0.121887,0.12031,0.121252,0.126531,0.116468,0.115099,0.12253,0.122691,0.122127,0.121887,0.121331,0.120466,0.121967,0.113901,0.120858,0.119231,0.120779,0.112523,0.119845,0.123588,0.12326,0.120623,0.120388,0.119923,0.120388,0.121569,0.113901,0.12031,0.12031,0.12031,0.12031,0.12031,0.005348,0.12261,0.11802,0.121648,0.120858,0.105983,0.113901,0.124498,0.115385,0.105144,0.120077,0.12,0.112796,0.005348,0.106104,0.118622,0.114673,0.119461,0.119768,0.119923,0.122853,0.122772,0.120544,0.119923,0.123588,0.123588,0.10426,0.10426,0.10426,0.10426,0.119845,0.115099,0.115672,0.12,0.115313,0.107081,0.121648,0.120077,0.119923,0.005348,0.105144,0.116908,0.120155,0.123097,0.107081,0.111444,0.12031,0.120544,0.104202,0.121331,0.108899,0.108899,0.119923,0.120544,0.119923,0.108899,0.11032,0.120779,0.120779,0.120466,0.12,0.119845,0.120077,0.119923,0.12,0.120077,0.120077,0.12031,0.119923,0.120858,0.119845,0.119845,0.119845,0.12,0.119768,0.120077,0.120388,0.117722,0.119768,0.119845,0.119768,0.120155,0.119845,0.12031,0.119845,0.120466,0.120544,0.119845,0.12031,0.119923,0.119768,0.119923,0.118095,0.121887,0.121094,0.119768,0.113276,0.117424,0.119768,0.117424,0.119768,0.117424,0.11418,0.11517,0.117424,0.119768,0.119768,0.112796,0.005348,0.11411,0.105983,0.106043,0.113901,0.106043,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.105983,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.115313,0.113901,0.113901,0.005348,0.005348],[0.095778,0.094753,0.097433,0.094753,0.089855,0.005348,0.091356,0.093939,0.090116,0.086471,0.087653,0.005348,0.091897,0.089639,0.005348,0.090732,0.096074,0.005348,0.005348,0.084087,0.090511,0.088953,0.090247,0.1,0.096174,0.0814,0.088027,0.092079,0.093845,0.091535,0.090423,0.089639,0.088571,0.005348,0.077371,0.088529,0.09016,0.085399,0.089682,0.094416,0.095876,0.090511,0.090511,0.089209,0.094995,0.005348,0.005348,0.090732,0.090732,0.090732,0.090732,0.090732,0.005348,0.091087,0.092814,0.089337,0.005348,0.005348,0.005348,0.089812,0.005348,0.077051,0.005348,0.089294,0.087365,0.005348,0.005405,0.09217,0.082777,0.005348,0.005348,0.089595,0.005348,0.086916,0.089899,0.089294,0.083371,0.093373,0.005524,0.005524,0.005524,0.005524,0.005348,0.091311,0.005525,0.089552,0.082888,0.005376,0.09149,0.083483,0.089985,0.005348,0.077051,0.08311,0.089812,0.089899,0.005348,0.005348,0.082047,0.005348,0.005376,0.091581,0.005348,0.005348,0.005348,0.090643,0.084661,0.005348,0.005348,0.005348,0.083333,0.085321,0.083333,0.089166,0.005376,0.084661,0.088027,0.088995,0.089942,0.089209,0.005348,0.089899,0.005348,0.005348,0.005348,0.083296,0.089166,0.089337,0.089166,0.083333,0.005348,0.005348,0.089552,0.089166,0.005348,0.089899,0.089251,0.089166,0.089166,0.005348,0.083333,0.089166,0.089209,0.089166,0.097843,0.089166,0.089552,0.005348,0.088571,0.005348,0.005348,0.005348,0.005348,0.005348,0.08274,0.082703,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var2_df_closseness, by=list(var2_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var2_df_closseness, by=list(var2_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-3e1e4f54aa1299303115" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-3e1e4f54aa1299303115">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6e-05\" data-max=\"6.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3e-06\" data-max=\"1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000211\" data-max=\"0.000329\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000214\" data-max=\"0.000222\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000575\" data-max=\"0.000641\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6.2e-05\" data-max=\"0.000146\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.01119\" data-max=\"0.011783\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000629\" data-max=\"0.001847\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.039185\" data-max=\"0.061214\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.039792\" data-max=\"0.041389\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.107025\" data-max=\"0.119249\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.011499\" data-max=\"0.027205\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.039185\" data-max=\"0.061214\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.039792\" data-max=\"0.041389\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[6.3e-05,6e-05],[3e-06,1e-05],[0.000329,0.000211],[0.000214,0.000222],[0.000641,0.000575],[6.2e-05,0.000146],[0.011783,0.01119],[0.000629,0.001847],[0.061214,0.039185],[0.039792,0.041389],[0.119249,0.107025],[0.011499,0.027205],[0.061214,0.039185],[0.039792,0.041389]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var2_df_closseness <- data.frame(
var2_incloseness,
var2_outcloseness,
var2_totalcloseness,
var2_incloseness_n,
var2_outcloseness_n,
var2_totalcloseness_n,
var2_centr_closeness) %>% round(6)

#Adding type
var2_df_closseness <-cbind(var2_df_closseness, V(var2)$TIPO2)

#Adding names
names(var2_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var2_df_closseness<-var2_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var2_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-3bb3469f7cd736e9e6b8" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-3bb3469f7cd736e9e6b8">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"6.8e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000538\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000716\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.012574\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.1\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.133238\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.1\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.5e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.2e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.8e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.2e-05,2.9e-05,6.2e-05,6.2e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,2.9e-05,6.3e-05,6.2e-05,6.4e-05,6.4e-05,6.2e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,2.9e-05,6.2e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.6e-05,6.6e-05,6.6e-05,6.6e-05,6.4e-05,6.3e-05,6.7e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,2.9e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.7e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,2.9e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.2e-05,6.2e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,2.9e-05,6.3e-05,6.2e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.2e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,2.9e-05,2.9e-05],[0.000515,0.000509,0.000524,0.000509,0.000483,2.9e-05,0.000491,0.000505,0.000484,0.000465,0.000471,2.9e-05,0.000494,0.000482,2.9e-05,0.000488,0.000517,2.9e-05,2.9e-05,0.000452,0.000487,0.000478,0.000485,0.000538,0.000517,0.000438,0.000473,0.000495,0.000505,0.000492,0.000486,0.000482,0.000476,2.9e-05,0.000416,0.000476,0.000485,0.000459,0.000482,0.000508,0.000515,0.000487,0.000487,0.00048,0.000511,2.9e-05,2.9e-05,0.000488,0.000488,0.000488,0.000488,0.000488,2.9e-05,0.00049,0.000499,0.00048,2.9e-05,2.9e-05,2.9e-05,0.000483,2.9e-05,0.000414,2.9e-05,0.00048,0.00047,2.9e-05,2.9e-05,0.000496,0.000445,2.9e-05,2.9e-05,0.000482,2.9e-05,0.000467,0.000483,0.00048,0.000448,0.000502,3e-05,3e-05,3e-05,3e-05,2.9e-05,0.000491,3e-05,0.000481,0.000446,2.9e-05,0.000492,0.000449,0.000484,2.9e-05,0.000414,0.000447,0.000483,0.000483,2.9e-05,2.9e-05,0.000441,2.9e-05,2.9e-05,0.000492,2.9e-05,2.9e-05,2.9e-05,0.000487,0.000455,2.9e-05,2.9e-05,2.9e-05,0.000448,0.000459,0.000448,0.000479,2.9e-05,0.000455,0.000473,0.000478,0.000484,0.00048,2.9e-05,0.000483,2.9e-05,2.9e-05,2.9e-05,0.000448,0.000479,0.00048,0.000479,0.000448,2.9e-05,2.9e-05,0.000481,0.000479,2.9e-05,0.000483,0.00048,0.000479,0.000479,2.9e-05,0.000448,0.000479,0.00048,0.000479,0.000526,0.000479,0.000481,2.9e-05,0.000476,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000445,0.000445,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000705,0.000674,0.000716,0.000663,0.000651,0.000649,0.000657,0.000669,0.000645,0.0006,0.000654,0.000652,0.000641,0.000622,0.000644,0.000648,0.000678,0.00065,0.000509,0.000646,0.000655,0.000647,0.000652,0.00068,0.000626,0.000619,0.000659,0.00066,0.000657,0.000655,0.000652,0.000648,0.000656,0.000612,0.00065,0.000641,0.000649,0.000605,0.000644,0.000664,0.000663,0.000649,0.000647,0.000645,0.000647,0.000654,0.000612,0.000647,0.000647,0.000647,0.000647,0.000647,2.9e-05,0.000659,0.000635,0.000654,0.00065,0.00057,0.000612,0.000669,0.00062,0.000565,0.000646,0.000645,0.000606,2.9e-05,0.00057,0.000638,0.000617,0.000642,0.000644,0.000645,0.000661,0.00066,0.000648,0.000645,0.000664,0.000664,0.000561,0.000561,0.000561,0.000561,0.000644,0.000619,0.000622,0.000645,0.00062,0.000576,0.000654,0.000646,0.000645,2.9e-05,0.000565,0.000629,0.000646,0.000662,0.000576,0.000599,0.000647,0.000648,0.00056,0.000652,0.000585,0.000585,0.000645,0.000648,0.000645,0.000585,0.000593,0.000649,0.000649,0.000648,0.000645,0.000644,0.000646,0.000645,0.000645,0.000646,0.000646,0.000647,0.000645,0.00065,0.000644,0.000644,0.000644,0.000645,0.000644,0.000646,0.000647,0.000633,0.000644,0.000644,0.000644,0.000646,0.000644,0.000647,0.000644,0.000648,0.000648,0.000644,0.000647,0.000645,0.000644,0.000645,0.000635,0.000655,0.000651,0.000644,0.000609,0.000631,0.000644,0.000631,0.000644,0.000631,0.000614,0.000619,0.000631,0.000644,0.000644,0.000606,2.9e-05,0.000613,0.00057,0.00057,0.000612,0.00057,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.00057,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.00062,0.000612,0.000612,2.9e-05,2.9e-05],[0.011834,0.011813,0.011861,0.011812,0.011818,0.012098,0.011794,0.011766,0.011692,0.011697,0.011817,0.011957,0.011622,0.011748,0.011941,0.01179,0.011817,0.011951,0.012574,0.011796,0.011815,0.011743,0.011812,0.011596,0.005348,0.011526,0.011528,0.011819,0.011814,0.011812,0.011819,0.011804,0.01182,0.011658,0.011814,0.011695,0.011759,0.011746,0.011728,0.011774,0.011813,0.01181,0.011801,0.011759,0.011737,0.01196,0.011658,0.011699,0.011699,0.011699,0.011699,0.011699,0.005348,0.011781,0.011523,0.011813,0.01195,0.011583,0.011658,0.011824,0.011837,0.011689,0.011944,0.011804,0.011695,0.005348,0.011583,0.011762,0.011742,0.01194,0.011941,0.011802,0.011947,0.011807,0.011809,0.011803,0.011807,0.011831,0.012358,0.012358,0.012358,0.012358,0.011945,0.01167,0.012444,0.011805,0.011751,0.011683,0.011803,0.011801,0.0118,0.005348,0.011689,0.011763,0.011801,0.011711,0.011821,0.011834,0.011806,0.011951,0.012509,0.011767,0.011865,0.011865,0.011945,0.011802,0.011804,0.011865,0.011882,0.011948,0.011807,0.01181,0.011805,0.011802,0.011945,0.011804,0.011806,0.011806,0.011802,0.011806,0.011945,0.011799,0.011944,0.011944,0.011944,0.011803,0.011801,0.011804,0.011807,0.011776,0.011941,0.011944,0.011801,0.011803,0.011944,0.011806,0.011803,0.011805,0.011805,0.011941,0.011809,0.011804,0.0118,0.011804,0.005348,0.011819,0.011807,0.011941,0.011751,0.011912,0.011941,0.011912,0.011941,0.011912,0.011455,0.011528,0.011912,0.011941,0.011941,0.011656,0.005348,0.011662,0.011583,0.011719,0.011658,0.011719,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011583,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.01185,0.011658,0.011658,0.005348,0.005348],[0.095778,0.094753,0.097433,0.094753,0.089855,0.005348,0.091356,0.093939,0.090116,0.086471,0.087653,0.005348,0.091897,0.089639,0.005348,0.090732,0.096074,0.005348,0.005348,0.084087,0.090511,0.088953,0.090247,0.1,0.096174,0.0814,0.088027,0.092079,0.093845,0.091535,0.090423,0.089639,0.088571,0.005348,0.077371,0.088529,0.09016,0.085399,0.089682,0.094416,0.095876,0.090511,0.090511,0.089209,0.094995,0.005348,0.005348,0.090732,0.090732,0.090732,0.090732,0.090732,0.005348,0.091087,0.092814,0.089337,0.005348,0.005348,0.005348,0.089812,0.005348,0.077051,0.005348,0.089294,0.087365,0.005348,0.005405,0.09217,0.082777,0.005348,0.005348,0.089595,0.005348,0.086916,0.089899,0.089294,0.083371,0.093373,0.005524,0.005524,0.005524,0.005524,0.005348,0.091311,0.005525,0.089552,0.082888,0.005376,0.09149,0.083483,0.089985,0.005348,0.077051,0.08311,0.089812,0.089899,0.005348,0.005348,0.082047,0.005348,0.005376,0.091581,0.005348,0.005348,0.005348,0.090643,0.084661,0.005348,0.005348,0.005348,0.083333,0.085321,0.083333,0.089166,0.005376,0.084661,0.088027,0.088995,0.089942,0.089209,0.005348,0.089899,0.005348,0.005348,0.005348,0.083296,0.089166,0.089337,0.089166,0.083333,0.005348,0.005348,0.089552,0.089166,0.005348,0.089899,0.089251,0.089166,0.089166,0.005348,0.083333,0.089166,0.089209,0.089166,0.097843,0.089166,0.089552,0.005348,0.088571,0.005348,0.005348,0.005348,0.005348,0.005348,0.08274,0.082703,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.131171,0.125337,0.133238,0.12326,0.121094,0.120701,0.122127,0.124498,0.12,0.111578,0.121648,0.121331,0.119307,0.115744,0.119768,0.120544,0.126102,0.120858,0.094705,0.120233,0.121887,0.12031,0.121252,0.126531,0.116468,0.115099,0.12253,0.122691,0.122127,0.121887,0.121331,0.120466,0.121967,0.113901,0.120858,0.119231,0.120779,0.112523,0.119845,0.123588,0.12326,0.120623,0.120388,0.119923,0.120388,0.121569,0.113901,0.12031,0.12031,0.12031,0.12031,0.12031,0.005348,0.12261,0.11802,0.121648,0.120858,0.105983,0.113901,0.124498,0.115385,0.105144,0.120077,0.12,0.112796,0.005348,0.106104,0.118622,0.114673,0.119461,0.119768,0.119923,0.122853,0.122772,0.120544,0.119923,0.123588,0.123588,0.10426,0.10426,0.10426,0.10426,0.119845,0.115099,0.115672,0.12,0.115313,0.107081,0.121648,0.120077,0.119923,0.005348,0.105144,0.116908,0.120155,0.123097,0.107081,0.111444,0.12031,0.120544,0.104202,0.121331,0.108899,0.108899,0.119923,0.120544,0.119923,0.108899,0.11032,0.120779,0.120779,0.120466,0.12,0.119845,0.120077,0.119923,0.12,0.120077,0.120077,0.12031,0.119923,0.120858,0.119845,0.119845,0.119845,0.12,0.119768,0.120077,0.120388,0.117722,0.119768,0.119845,0.119768,0.120155,0.119845,0.12031,0.119845,0.120466,0.120544,0.119845,0.12031,0.119923,0.119768,0.119923,0.118095,0.121887,0.121094,0.119768,0.113276,0.117424,0.119768,0.117424,0.119768,0.117424,0.11418,0.11517,0.117424,0.119768,0.119768,0.112796,0.005348,0.11411,0.105983,0.106043,0.113901,0.106043,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.105983,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.115313,0.113901,0.113901,0.005348,0.005348],[0.095778,0.094753,0.097433,0.094753,0.089855,0.005348,0.091356,0.093939,0.090116,0.086471,0.087653,0.005348,0.091897,0.089639,0.005348,0.090732,0.096074,0.005348,0.005348,0.084087,0.090511,0.088953,0.090247,0.1,0.096174,0.0814,0.088027,0.092079,0.093845,0.091535,0.090423,0.089639,0.088571,0.005348,0.077371,0.088529,0.09016,0.085399,0.089682,0.094416,0.095876,0.090511,0.090511,0.089209,0.094995,0.005348,0.005348,0.090732,0.090732,0.090732,0.090732,0.090732,0.005348,0.091087,0.092814,0.089337,0.005348,0.005348,0.005348,0.089812,0.005348,0.077051,0.005348,0.089294,0.087365,0.005348,0.005405,0.09217,0.082777,0.005348,0.005348,0.089595,0.005348,0.086916,0.089899,0.089294,0.083371,0.093373,0.005524,0.005524,0.005524,0.005524,0.005348,0.091311,0.005525,0.089552,0.082888,0.005376,0.09149,0.083483,0.089985,0.005348,0.077051,0.08311,0.089812,0.089899,0.005348,0.005348,0.082047,0.005348,0.005376,0.091581,0.005348,0.005348,0.005348,0.090643,0.084661,0.005348,0.005348,0.005348,0.083333,0.085321,0.083333,0.089166,0.005376,0.084661,0.088027,0.088995,0.089942,0.089209,0.005348,0.089899,0.005348,0.005348,0.005348,0.083296,0.089166,0.089337,0.089166,0.083333,0.005348,0.005348,0.089552,0.089166,0.005348,0.089899,0.089251,0.089166,0.089166,0.005348,0.083333,0.089166,0.089209,0.089166,0.097843,0.089166,0.089552,0.005348,0.088571,0.005348,0.005348,0.005348,0.005348,0.005348,0.08274,0.082703,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var2_df_closseness, by=list(var2_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var2_df_closseness, by=list(var2_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-6177e97791827bfa667b" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-6177e97791827bfa667b">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6e-05\" data-max=\"6.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000213\" data-max=\"0.000396\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000191\" data-max=\"0.000223\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000574\" data-max=\"0.000657\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8e-06\" data-max=\"0.000147\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.01118\" data-max=\"0.011854\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8.4e-05\" data-max=\"0.001857\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.039631\" data-max=\"0.073647\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.035634\" data-max=\"0.04149\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.106843\" data-max=\"0.122158\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001397\" data-max=\"0.027332\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.039631\" data-max=\"0.073647\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.035634\" data-max=\"0.04149\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[6.4e-05,6.3e-05,6e-05],[0,4e-06,1e-05],[0.000396,0.000315,0.000213],[0.000191,0.000217,0.000223],[0.000657,0.000639,0.000574],[8e-06,6.6e-05,0.000147],[0.011854,0.011774,0.01118],[8.4e-05,0.000674,0.001857],[0.073647,0.058625,0.039631],[0.035634,0.040396,0.04149],[0.122158,0.118807,0.106843],[0.001397,0.01226,0.027332],[0.073647,0.058625,0.039631],[0.035634,0.040396,0.04149]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var2_data.RData")
```

