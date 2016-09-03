# SNA Closeness 6_PROGRAMAS EM CONJUNTO (var4)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 6_PROGRAMAS EM CONJUNTO (var4)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var4_data.RData")
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
#var4<-simplify(var4) #Simplify
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
V(var4)$incloseness <- closeness(var4, mode = "in", weights = E(var4)$var4) %>% round(6)
V(var4)$outcloseness <- closeness(var4, mode = "out", weights = E(var4)$var4) %>% round(6)
V(var4)$totalcloseness <- closeness(var4, mode = "total", weights = E(var4)$var4) %>% round(4)
```

###Saving to Environment

```r
var4_incloseness<- closeness(var4, mode = "in", weights = E(var4)$var4) %>% round(6)
var4_outcloseness<- closeness(var4, mode = "out", weights = E(var4)$var4) %>% round(6)
var4_totalcloseness<- closeness(var4, mode = "total", weights = E(var4)$var4) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var4_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 2.900e-05 2.900e-05 3.100e-05 3.226e-05 3.600e-05 3.700e-05
```

```r
sd(var4_incloseness)
```

```
## [1] 3.222634e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var4)$incloseness<-closeness(var4, weights = E(var4)$var4, mode="in")

#Get Variable
V(var4)$var4_color_degree<-round(V(var4)$incloseness,6)

#Creating brewer pallette
vertex_var4_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var4)$var4_color_degree)), "RdBu"))(
            length(unique(V(var4)$var4_color_degree)))

#Saving as Vertex properties 
V(var4)$vertex_var4_color_degree<-
  vertex_var4_color_degree[as.numeric(
  cut(V(var4)$var4_color_degree,
      breaks=length(unique(V(var4)$var4_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var4, es=E(var4), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var4))
maxC <- rep(Inf, vcount(var4))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var4, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var4)$weight)


#PLotting
plot(var4, 
     layout=co,
     edge.color=V(var4)$vertex_var4_color_degree[edge.start],
     edge.arrow.size=closeness(var4, weights = E(var4)$var4, mode="in"),
     edge.width=E(var4)$weight/mean(E(var4)$weight),
     edge.curved = TRUE,
     vertex.color=V(var4)$vertex_var4_color_degree,
     vertex.size=closeness(var4, weights = E(var4)$var4, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var4,"LABEL_COR"),
     vertex.label.cex=(closeness(var4, weights = E(var4)$var4, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var4)$var4_color_degree
b<-V(var4)$vertex_var4_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 6_PROGRAMAS EM CONJUNTO (var4)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var4, mode="in", weights = E(var4)$var4)), 
             sd(closeness(var4, mode="in", weights = E(var4)$var4))
             )
       )
```

![](6_PROGRAMAS_EM_CONJUNTO_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var4_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 2.900e-05 2.900e-05 2.900e-05 3.423e-05 2.900e-05 8.000e-05
```

```r
sd(var4_outcloseness)
```

```
## [1] 1.108014e-05
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var4)$outcloseness<-closeness(var4, weights = E(var4)$var4, mode="out")

#Get Variable
V(var4)$var4_color_degree<-round(V(var4)$outcloseness,6)

#Creating brewer pallette
vertex_var4_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var4)$var4_color_degree)), "RdBu"))(
            length(unique(V(var4)$var4_color_degree)))

#Saving as Vertex properties 
V(var4)$vertex_var4_color_degree<-
  vertex_var4_color_degree[as.numeric(
  cut(V(var4)$var4_color_degree,
      breaks=length(unique(V(var4)$var4_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var4, es=E(var4), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var4))
maxC <- rep(Inf, vcount(var4))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var4, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var4)$weight)


#PLotting
plot(var4, 
     layout=co,
     edge.color=V(var4)$vertex_var4_color_degree[edge.start],
     edge.arrow.size=closeness(var4, weights = E(var4)$var4, mode="out"),
     edge.width=E(var4)$weight/2*mean(E(var4)$weight),
     edge.curved = TRUE,
     vertex.color=V(var4)$vertex_var4_color_degree,
     vertex.size=closeness(var4, weights = E(var4)$var4, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var4,"LABEL_COR"),
     vertex.label.cex=closeness(var4, weights = E(var4)$var4, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var4)$var4_color_degree
b<-V(var4)$vertex_var4_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 6_PROGRAMAS EM CONJUNTO (var4)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var4, mode="out", weights = E(var4)$var4)), 
             sd(closeness(var4, mode="out", weights = E(var4)$var4))
             )
       )
```

![](6_PROGRAMAS_EM_CONJUNTO_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var4_totalcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0001760 0.0001800 0.0001577 0.0001810 0.0001860
```

```r
sd(var4_totalcloseness)
```

```
## [1] 5.308773e-05
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var4)$allcloseness<-closeness(var4, weights = E(var4)$var4, mode="all")

#Get Variable
V(var4)$var4_color_degree<-round(V(var4)$allcloseness,6)

#Creating brewer pallette
vertex_var4_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var4)$var4_color_degree)), "RdBu"))(
            length(unique(V(var4)$var4_color_degree)))

#Saving as Vertex properties 
V(var4)$vertex_var4_color_degree<-
  vertex_var4_color_degree[as.numeric(
  cut(V(var4)$var4_color_degree,
      breaks=length(unique(V(var4)$var4_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var4, es=E(var4), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var4))
maxC <- rep(Inf, vcount(var4))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var4, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var4)$weight)


#PLotting
plot(var4, 
     layout=co,
     edge.color=V(var4)$vertex_var4_color_degree[edge.start],
     edge.arrow.size=closeness(var4, weights = E(var4)$var4, mode="all"),
     edge.width=E(var4)$weight/2*mean(E(var4)$weight),
     edge.curved = TRUE,
     vertex.color=V(var4)$vertex_var4_color_degree,
     vertex.size=closeness(var4, weights = E(var4)$var4, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var4,"LABEL_COR"),
     vertex.label.cex=(closeness(var4, weights = E(var4)$var4, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var4)$var4_color_degree
b<-V(var4)$vertex_var4_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 6_PROGRAMAS EM CONJUNTO (var4)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var4, mode="all", weights = E(var4)$var4)), 
             sd(closeness(var4, mode="all", weights = E(var4)$var4))
             )
       )
```

![](6_PROGRAMAS_EM_CONJUNTO_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var4)$incloseness_n <- closeness(var4, mode = "in",, weights = E(var4)$var4, normalized = T) %>% round(10)
V(var4)$outcloseness_n <- closeness(var4, mode = "out", normalized = T, weights = E(var4)$var4) %>% round(6)
V(var4)$totalcloseness_n <- closeness(var4, mode = "total", normalized = T, weights = E(var4)$var4) %>% round(6)
```

###Saving to Environment

```r
var4_incloseness_n<- closeness(var4, mode = "in", normalized = T, weights = E(var4)$var4) %>% round(6)
var4_outcloseness_n<- closeness(var4, mode = "out", normalized = T, weights = E(var4)$var4) %>% round(6)
var4_totalcloseness_n<- closeness(var4, mode = "total", normalized = T, weights = E(var4)$var4) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var4_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.005376 0.005682 0.005967 0.006608 0.006837
```

```r
sd(var4_incloseness_n)
```

```
## [1] 0.0005888838
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var4)$incloseness_n<-closeness(var4, weights = E(var4)$var4, mode="in", normalized = T)

#Get Variable
V(var4)$var4_color_degree<-round(V(var4)$incloseness_n,6)

#Creating brewer pallette
vertex_var4_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var4)$var4_color_degree)), "RdBu"))(
            length(unique(V(var4)$var4_color_degree)))

#Saving as Vertex properties 
V(var4)$vertex_var4_color_degree<-
  vertex_var4_color_degree[as.numeric(
  cut(V(var4)$var4_color_degree,
      breaks=length(unique(V(var4)$var4_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var4, es=E(var4), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var4))
maxC <- rep(Inf, vcount(var4))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var4, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var4)$weight)


#PLotting
plot(var4, 
     layout=co,
     edge.color=V(var4)$vertex_var4_color_degree[edge.start],
     edge.arrow.size=closeness(var4, weights = E(var4)$var4, mode="in",normalized = T),
     edge.width=E(var4)$weight/10*mean(E(var4)$weight),
     edge.curved = TRUE,
     vertex.color=V(var4)$vertex_var4_color_degree,
     vertex.size=(closeness(var4, weights = E(var4)$var4, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var4,"LABEL_COR"),
     vertex.label.cex=closeness(var4, weights = E(var4)$var4, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var4)$var4_color_degree
b<-V(var4)$vertex_var4_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 6_PROGRAMAS EM CONJUNTO (var4)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var4, mode="in", weights = E(var4)$var4, normalized = T)), 
             sd(closeness(var4, mode="in", weights = E(var4)$var4, normalized = T))
             )
       )
```

![](6_PROGRAMAS_EM_CONJUNTO_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var4_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.005348 0.005348 0.006340 0.005464 0.014840
```

```r
sd(var4_outcloseness_n)
```

```
## [1] 0.002077426
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var4)$outcloseness_n<-closeness(var4, weights = E(var4)$var4, mode="out", normalized = T)

#Get Variable
V(var4)$var4_color_degree<-round(V(var4)$outcloseness_n,6)

#Creating brewer pallette
vertex_var4_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var4)$var4_color_degree)), "RdBu"))(
            length(unique(V(var4)$var4_color_degree)))

#Saving as Vertex properties 
V(var4)$vertex_var4_color_degree<-
  vertex_var4_color_degree[as.numeric(
  cut(V(var4)$var4_color_degree,
      breaks=length(unique(V(var4)$var4_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var4, es=E(var4), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var4))
maxC <- rep(Inf, vcount(var4))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var4, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var4)$weight)


#PLotting
plot(var4, 
     layout=co,
     edge.color=V(var4)$vertex_var4_color_degree[edge.start],
     edge.arrow.size=closeness(var4, weights = E(var4)$var4, mode="out",normalized = T),
     edge.width=E(var4)$weight/10*mean(E(var4)$weight),
     edge.curved = TRUE,
     vertex.color=V(var4)$vertex_var4_color_degree,
     vertex.size=(closeness(var4, weights = E(var4)$var4, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var4,"LABEL_COR"),
     vertex.label.cex=closeness(var4, weights = E(var4)$var4, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var4)$var4_color_degree
b<-V(var4)$vertex_var4_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 6_PROGRAMAS EM CONJUNTO (var4)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var4, mode="out", weights = E(var4)$var4, normalized = T)), 
             sd(closeness(var4, mode="out", weights = E(var4)$var4, normalized = T))
             )
       )
```

![](6_PROGRAMAS_EM_CONJUNTO_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var4_totalcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.032760 0.033450 0.029350 0.033710 0.034680
```

```r
sd(var4_totalcloseness_n)
```

```
## [1] 0.009892183
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var4)$allcloseness_n<-closeness(var4, weights = E(var4)$var4, mode="all", normalized = T)

#Get Variable
V(var4)$var4_color_degree<-round(V(var4)$allcloseness_n,6)

#Creating brewer pallette
vertex_var4_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var4)$var4_color_degree)), "RdBu"))(
            length(unique(V(var4)$var4_color_degree)))

#Saving as Vertex properties 
V(var4)$vertex_var4_color_degree<-
  vertex_var4_color_degree[as.numeric(
  cut(V(var4)$var4_color_degree,
      breaks=length(unique(V(var4)$var4_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var4, es=E(var4), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var4))
maxC <- rep(Inf, vcount(var4))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var4, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var4)$weight)


#PLotting
plot(var4, 
     layout=co,
     edge.color=V(var4)$vertex_var4_color_degree[edge.start],
     edge.arrow.size=closeness(var4, weights = E(var4)$var4, mode="all",normalized = T),
     edge.width=E(var4)$weight/10*mean(E(var4)$weight),
     edge.curved = TRUE,
     vertex.color=V(var4)$vertex_var4_color_degree,
     vertex.size=(closeness(var4, weights = E(var4)$var4, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var4,"LABEL_COR"),
     vertex.label.cex=closeness(var4, weights = E(var4)$var4, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var4)$var4_color_degree
b<-V(var4)$vertex_var4_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 6_PROGRAMAS EM CONJUNTO (var4)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var4, mode="all", weights = E(var4)$var4, normalized = T)), 
             sd(closeness(var4, mode="all", weights = E(var4)$var4, normalized = T))
             )
       )
```

![](6_PROGRAMAS_EM_CONJUNTO_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var4)$incloseness_n <- closeness(var4, weights = E(var4)$var4, mode = "in", normalized = T) %>% round(6)
V(var4)$outcloseness_n <- closeness(var4, weights = E(var4)$var4, mode = "out", normalized = T) %>% round(6)
V(var4)$totalcloseness_n <- closeness(var4, weights = E(var4)$var4, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var4)$var4_centr_closeness<- centralization.closeness(var4)$res
var4_centr_closeness<- centralization.closeness(var4)$res
var4_centr_closeness_all<- centralization.closeness(var4)
```

###Centralization

```r
var4_centr_closeness_all$centralization
```

```
## [1] 0.008594759
```

###Theoretical Max

```r
var4_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var4)$var4_centr_closeness<- centralization.closeness(var4)$res

#Get Variable
V(var4)$var4_color_degree<-round(V(var4)$var4_centr_closeness,6)

#Creating brewer pallette
vertex_var4_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var4)$var4_color_degree)), "Spectral"))(
            length(unique(V(var4)$var4_color_degree)))

#Saving as Vertex properties 
V(var4)$vertex_var4_color_degree<-
  vertex_var4_color_degree[as.numeric(
  cut(V(var4)$var4_color_degree,
      breaks=length(unique(V(var4)$var4_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var4, es=E(var4), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var4))
maxC <- rep(Inf, vcount(var4))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var4, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var4)$weight)


#PLotting
plot(var4, 
     layout=co,
     edge.color=V(var4)$vertex_var4_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var4)$res,
     edge.width=E(var4)$weight/10*mean(E(var4)$weight),
     edge.curved = TRUE,
     vertex.color=V(var4)$vertex_var4_color_degree,
     vertex.size=centralization.closeness(var4)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var4,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var4)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var4)$var4_color_degree
b<-V(var4)$vertex_var4_color_degree
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
  title("Network Centralization Closeness - 6_PROGRAMAS EM CONJUNTO (var4)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var4)$res), 
             sd(centralization.closeness(var4)$res)
             )
       )
```

![](6_PROGRAMAS_EM_CONJUNTO_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var4_incloseness<- closeness(var4, weights = E(var4)$var4, mode = "in") %>% round(6)
var4_outcloseness<- closeness(var4, weights = E(var4)$var4, mode = "out") %>% round(6)
var4_totalcloseness<- closeness(var4, weights = E(var4)$var4, mode = "total") %>% round(6)
var4_incloseness_n<- closeness(var4,weights = E(var4)$var4, mode = "in", normalized = T) %>% round(6)
var4_outcloseness_n<- closeness(var4,weights = E(var4)$var4, mode = "out", normalized = T) %>% round(6)
var4_totalcloseness_n<- closeness(var4,weights = E(var4)$var4, mode = "total", normalized = T) %>% round(6)
var4_centr_closeness <- centralization.closeness(var4)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var4_df_closseness <- data.frame(
var4_incloseness,
var4_outcloseness,
var4_totalcloseness,
var4_incloseness_n,
var4_outcloseness_n,
var4_totalcloseness_n,
var4_centr_closeness) %>% round(6)

#Adding type
var4_df_closseness <-cbind(var4_df_closseness, V(var4)$LABEL_COR)

#Adding names
names(var4_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var4_df_closseness<-var4_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var4_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-839716330907a024e60e" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-839716330907a024e60e">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"3.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"8e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000186\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.006837\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.014843\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.034676\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.014843\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[3.5e-05,3.1e-05,3.5e-05,2.9e-05,2.9e-05,3.5e-05,3.5e-05,2.9e-05,2.9e-05,2.9e-05,3.5e-05,3.1e-05,2.9e-05,2.9e-05,3.6e-05,3.1e-05,2.9e-05,3.1e-05,2.9e-05,3.5e-05,3.5e-05,2.9e-05,3.6e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,3.6e-05,2.9e-05,2.9e-05,2.9e-05,3.5e-05,3.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,3.1e-05,3.1e-05,3.1e-05,3.1e-05,3.6e-05,2.9e-05,3.1e-05,2.9e-05,3.1e-05,3.1e-05,3.1e-05,3.1e-05,3.1e-05,2.9e-05,3.5e-05,2.9e-05,3.5e-05,3.6e-05,2.9e-05,3.6e-05,3.5e-05,3.6e-05,2.9e-05,3.6e-05,3.5e-05,2.9e-05,2.9e-05,2.9e-05,3.1e-05,2.9e-05,3.6e-05,3.6e-05,3.6e-05,3.5e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.1e-05,3e-05,3e-05,3e-05,3e-05,3.5e-05,2.9e-05,3e-05,3.6e-05,2.9e-05,2.9e-05,3.5e-05,3.6e-05,3.6e-05,2.9e-05,2.9e-05,2.9e-05,3.6e-05,2.9e-05,2.9e-05,2.9e-05,3.6e-05,3.6e-05,2.9e-05,3.5e-05,2.9e-05,2.9e-05,3.6e-05,3.6e-05,3.6e-05,2.9e-05,3.5e-05,3.6e-05,3.6e-05,3.5e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.5e-05,3.6e-05,3.6e-05,3.6e-05,3.5e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,2.9e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.5e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.5e-05,3.6e-05,3.7e-05,3.6e-05,2.9e-05,2.9e-05,3.6e-05,2.9e-05,3.6e-05,2.9e-05,3e-05,2.9e-05,2.9e-05,3.6e-05,3.6e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,3.5e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,3.2e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[5.1e-05,5.2e-05,5.2e-05,2.9e-05,2.9e-05,5.1e-05,5.2e-05,5.2e-05,2.9e-05,5.2e-05,5.1e-05,2.9e-05,6.4e-05,6.4e-05,2.9e-05,6.2e-05,5.2e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,3e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,5.2e-05,2.9e-05,3e-05,5.2e-05,3e-05,3e-05,6.2e-05,2.9e-05,6.2e-05,6.2e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,6.2e-05,6.2e-05,6.2e-05,6.2e-05,6.2e-05,2.9e-05,5.1e-05,8e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,6.9e-05,2.9e-05,2.9e-05,2.9e-05,3e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,3e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000183,0.000178,0.000186,2.9e-05,2.9e-05,0.000182,0.000184,0.000182,2.9e-05,0.000178,0.000182,0.000176,0.000181,0.00018,0.000181,0.00018,0.000182,0.000176,2.9e-05,0.000179,0.000179,0.000178,0.000179,0.000173,2.9e-05,2.9e-05,0.000178,0.000179,2.9e-05,2.9e-05,2.9e-05,0.000181,0.00018,0.000181,2.9e-05,0.000178,0.000181,0.000169,0.000169,0.000183,0.00018,0.000179,0.000179,0.000176,0.000173,0.000179,0.000182,0.00018,0.00018,0.00018,0.00018,0.00018,0.000174,0.00018,0.000183,0.000178,0.000181,0.000178,0.000182,0.000183,0.000179,2.9e-05,0.000181,0.000181,0.000179,0.000174,0.000178,0.000179,0.000174,0.000181,0.000181,0.000181,0.000183,0.000181,0.000181,0.000181,0.000181,0.000181,0.000174,0.000174,0.000174,0.000174,0.000183,0.000178,0.000179,0.000181,0.000169,0.000176,0.000181,0.000181,0.000181,2.9e-05,2.9e-05,2.9e-05,0.000182,2.9e-05,0.000176,0.000173,0.000181,0.000182,2.9e-05,0.000177,2.9e-05,2.9e-05,0.000181,0.000181,0.000181,2.9e-05,0.000174,0.000184,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000183,0.000183,0.000181,0.000177,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,2.9e-05,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000182,0.000181,0.000181,0.000181,0.000181,0.000179,0.000184,0.000181,0.000175,2.9e-05,0.000181,2.9e-05,0.000181,2.9e-05,0.000182,0.000181,2.9e-05,0.000181,0.000181,2.9e-05,2.9e-05,0.000178,0.000179,0.000178,0.000178,0.000178,0.000178,0.000178,0.000178,0.000178,0.000178,0.000178,0.000181,0.000178,0.000178,0.000179,0.000178,0.000178,0.000178,0.000178,0.000178,0.000178,0.00018,0.000178,0.000178,0.000174,2.9e-05],[0.006571,0.005712,0.006573,0.005348,0.005348,0.00656,0.006562,0.005405,0.005348,0.005405,0.006561,0.005711,0.005376,0.005348,0.00661,0.00568,0.005348,0.005712,0.005376,0.006597,0.006566,0.005405,0.006609,0.005434,0.005376,0.005348,0.005376,0.006653,0.005348,0.005348,0.005348,0.006565,0.005812,0.005376,0.005348,0.005405,0.005348,0.005348,0.005348,0.005682,0.005812,0.00568,0.00568,0.006644,0.005348,0.005713,0.005376,0.00568,0.00568,0.00568,0.00568,0.00568,0.005376,0.006554,0.005348,0.006589,0.00661,0.005376,0.006606,0.006569,0.00676,0.005405,0.006608,0.006565,0.005348,0.005348,0.005376,0.005713,0.005405,0.00661,0.006608,0.006608,0.006566,0.00661,0.006611,0.006608,0.006608,0.005812,0.005586,0.005586,0.005586,0.005586,0.006568,0.005405,0.005586,0.00661,0.005405,0.005435,0.006567,0.006608,0.006608,0.005348,0.005405,0.005405,0.006653,0.005348,0.005435,0.005405,0.00661,0.006612,0.005348,0.006596,0.005348,0.005348,0.006608,0.006608,0.006608,0.005348,0.006601,0.006611,0.00661,0.006568,0.00661,0.006608,0.00661,0.00661,0.00661,0.006567,0.006654,0.006657,0.006608,0.006596,0.00661,0.00661,0.00661,0.006608,0.006608,0.00661,0.006609,0.005348,0.006608,0.00661,0.006608,0.006608,0.00661,0.006566,0.006608,0.006608,0.00661,0.006608,0.006613,0.006608,0.006608,0.00661,0.006565,0.00661,0.006837,0.006609,0.005348,0.005348,0.006608,0.005348,0.006608,0.005348,0.005494,0.005376,0.005348,0.006608,0.006608,0.005348,0.005348,0.005376,0.005435,0.005405,0.005376,0.005405,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.006602,0.005376,0.005376,0.005405,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005878,0.005376,0.005376,0.005376,0.005348],[0.009568,0.009619,0.009602,0.005348,0.005348,0.009564,0.009583,0.009748,0.005348,0.009706,0.009571,0.005348,0.0119,0.011918,0.005348,0.011494,0.009658,0.005348,0.005348,0.005348,0.009529,0.005494,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009543,0.005435,0.009612,0.005348,0.005494,0.009653,0.005523,0.005523,0.011556,0.005435,0.011485,0.011485,0.005348,0.005405,0.005348,0.005435,0.011494,0.011494,0.011494,0.011494,0.011494,0.005376,0.009548,0.014843,0.005376,0.005376,0.005348,0.005348,0.009562,0.005348,0.005405,0.005348,0.009562,0.012889,0.005376,0.005405,0.005348,0.005494,0.005348,0.005348,0.005348,0.009533,0.005348,0.005348,0.005348,0.005348,0.005435,0.005464,0.005464,0.005464,0.005464,0.009562,0.005376,0.005464,0.005348,0.005348,0.005376,0.009543,0.005348,0.005376,0.005348,0.005405,0.005405,0.005348,0.005348,0.005376,0.005494,0.005348,0.005348,0.005405,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009551,0.005348,0.005348,0.005348,0.005348,0.005348,0.009543,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009531,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009562,0.005348,0.005348,0.005348,0.005464,0.005348,0.005348,0.005348,0.005348,0.005348,0.005376,0.005376,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.033997,0.033179,0.034676,0.005348,0.005348,0.033763,0.03421,0.033775,0.005348,0.03302,0.033787,0.03277,0.033592,0.033556,0.033708,0.033538,0.033806,0.032706,0.005405,0.033244,0.033268,0.03302,0.033208,0.032264,0.005405,0.005348,0.033149,0.033214,0.005348,0.005348,0.005348,0.033708,0.033489,0.033653,0.005348,0.03302,0.033683,0.031477,0.031477,0.034122,0.033495,0.033268,0.033333,0.032758,0.032264,0.033256,0.033763,0.033556,0.033556,0.033556,0.033556,0.033556,0.032376,0.033453,0.03411,0.033108,0.03372,0.033149,0.0338,0.03411,0.033232,0.005405,0.033683,0.033683,0.03328,0.032438,0.033161,0.03325,0.032342,0.033708,0.033683,0.033683,0.033985,0.033726,0.033751,0.033683,0.033683,0.033745,0.032337,0.032337,0.032337,0.032337,0.033966,0.033173,0.033232,0.033708,0.031509,0.032666,0.033708,0.033683,0.033696,0.005348,0.005405,0.005405,0.033942,0.005348,0.032666,0.032124,0.033708,0.033886,0.005405,0.032845,0.005348,0.005348,0.033683,0.033683,0.033683,0.005348,0.032353,0.034286,0.033708,0.033751,0.033708,0.033683,0.033708,0.033708,0.033708,0.033708,0.033966,0.034053,0.033683,0.032845,0.033708,0.033708,0.033708,0.033683,0.033683,0.033708,0.033708,0.005348,0.033683,0.033708,0.033683,0.033683,0.033708,0.033708,0.033683,0.033683,0.033708,0.033683,0.033917,0.033683,0.033683,0.033708,0.033683,0.033309,0.034154,0.033726,0.032569,0.005348,0.033683,0.005348,0.033683,0.005348,0.033904,0.033745,0.005348,0.033683,0.033683,0.005348,0.005348,0.033149,0.033357,0.033155,0.033149,0.033155,0.033149,0.033149,0.033149,0.033149,0.033149,0.033149,0.033677,0.033149,0.033149,0.033345,0.033149,0.033149,0.033149,0.033149,0.033149,0.033149,0.033447,0.033149,0.033149,0.032365,0.005348],[0.009568,0.009619,0.009602,0.005348,0.005348,0.009564,0.009583,0.009748,0.005348,0.009706,0.009571,0.005348,0.0119,0.011918,0.005348,0.011494,0.009658,0.005348,0.005348,0.005348,0.009529,0.005494,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009543,0.005435,0.009612,0.005348,0.005494,0.009653,0.005523,0.005523,0.011556,0.005435,0.011485,0.011485,0.005348,0.005405,0.005348,0.005435,0.011494,0.011494,0.011494,0.011494,0.011494,0.005376,0.009548,0.014843,0.005376,0.005376,0.005348,0.005348,0.009562,0.005348,0.005405,0.005348,0.009562,0.012889,0.005376,0.005405,0.005348,0.005494,0.005348,0.005348,0.005348,0.009533,0.005348,0.005348,0.005348,0.005348,0.005435,0.005464,0.005464,0.005464,0.005464,0.009562,0.005376,0.005464,0.005348,0.005348,0.005376,0.009543,0.005348,0.005376,0.005348,0.005405,0.005405,0.005348,0.005348,0.005376,0.005494,0.005348,0.005348,0.005405,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009551,0.005348,0.005348,0.005348,0.005348,0.005348,0.009543,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009531,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009562,0.005348,0.005348,0.005348,0.005464,0.005348,0.005348,0.005348,0.005348,0.005348,0.005376,0.005376,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var4_df_closseness, by=list(var4_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var4_df_closseness, by=list(var4_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-69f4db40b959d6f5a1bd" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-69f4db40b959d6f5a1bd">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"3.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"3e-06\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"6.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000186\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"8.8e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.006837\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.1e-05\" data-max=\"7e-04\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.011702\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.7e-05\" data-max=\"0.00318\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.034676\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.8e-05\" data-max=\"0.016418\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.011702\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.7e-05\" data-max=\"0.00318\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[3e-05,3e-05,3.5e-05,3.1e-05,3.5e-05,3.5e-05,3.6e-05,3.5e-05,3.6e-05,3.7e-05,3.1e-05,2.9e-05,3e-05,3.1e-05,3.1e-05,3.1e-05,2.9e-05,3.6e-05,3.5e-05,3e-05,3.1e-05,3.4e-05,3.6e-05],[1e-06,2e-06,null,null,null,null,1e-06,null,null,null,null,2e-06,1e-06,3e-06,3e-06,3e-06,null,null,null,2e-06,null,3e-06,0],[2.9e-05,3.1e-05,2.9e-05,5.2e-05,5.2e-05,5.1e-05,3.6e-05,5.2e-05,2.9e-05,2.9e-05,2.9e-05,3.7e-05,6.3e-05,3.1e-05,4.4e-05,3.7e-05,2.9e-05,2.9e-05,5.1e-05,4.6e-05,2.9e-05,3e-05,3.3e-05],[0,8e-06,null,null,null,null,1.3e-05,null,null,null,null,1.3e-05,1e-06,7e-06,1.3e-05,1.4e-05,null,null,null,1.7e-05,null,5e-06,8e-06],[0.000178,0.000165,0.000179,0.000178,0.000184,0.000183,0.000179,0.000186,0.000179,0.000184,0.000181,0.000138,0.00018,9.6e-05,0.000131,0.000137,2.9e-05,0.000182,0.000183,0.00014,0.000176,0.000141,0.000181],[3e-06,4.3e-05,null,null,null,null,0,null,null,null,null,6.7e-05,1e-06,7.9e-05,8.8e-05,7e-05,null,null,null,6.7e-05,null,6.9e-05,1e-06],[0.005657,0.005498,0.006597,0.005712,0.006562,0.006569,0.00661,0.006573,0.006609,0.006837,0.005812,0.005455,0.005514,0.005702,0.005752,0.00579,0.005348,0.006612,0.006571,0.005618,0.005712,0.006275,0.006605],[0.000268,0.000325,null,null,null,null,4.4e-05,null,null,null,null,0.000289,0.000235,0.000519,0.0007,0.00053,null,null,null,0.000351,null,0.000569,2.1e-05],[0.005445,0.005665,0.005348,0.009619,0.009583,0.009562,0.006742,0.009602,0.005348,0.005348,0.005435,0.006815,0.011702,0.00582,0.00819,0.006877,0.005348,0.005348,0.009568,0.00845,0.005348,0.005569,0.006049],[1.7e-05,0.001519,null,null,null,null,0.002414,null,null,null,null,0.002537,0.000306,0.001407,0.002462,0.002613,null,null,null,0.00318,null,0.000964,0.00158],[0.033184,0.030745,0.033244,0.033179,0.03421,0.03411,0.033264,0.034676,0.033208,0.034154,0.033745,0.025623,0.033412,0.01774,0.024306,0.025536,0.005348,0.033886,0.033997,0.026056,0.032706,0.026238,0.033739],[0.000533,0.007935,null,null,null,null,4.8e-05,null,null,null,null,0.012439,0.000204,0.014698,0.016418,0.012941,null,null,null,0.012512,null,0.012826,0.00013],[0.005445,0.005665,0.005348,0.009619,0.009583,0.009562,0.006742,0.009602,0.005348,0.005348,0.005435,0.006815,0.011702,0.00582,0.00819,0.006877,0.005348,0.005348,0.009568,0.00845,0.005348,0.005569,0.006049],[1.7e-05,0.001519,null,null,null,null,0.002414,null,null,null,null,0.002537,0.000306,0.001407,0.002462,0.002613,null,null,null,0.00318,null,0.000964,0.00158]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var4_df_closseness <- data.frame(
var4_incloseness,
var4_outcloseness,
var4_totalcloseness,
var4_incloseness_n,
var4_outcloseness_n,
var4_totalcloseness_n,
var4_centr_closeness) %>% round(6)

#Adding type
var4_df_closseness <-cbind(var4_df_closseness, V(var4)$TIPO1)

#Adding names
names(var4_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var4_df_closseness<-var4_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var4_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-96dbe99220649a64bf9f" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-96dbe99220649a64bf9f">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"3.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"8e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000186\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.006837\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.014843\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.034676\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.014843\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[3.5e-05,3.1e-05,3.5e-05,2.9e-05,2.9e-05,3.5e-05,3.5e-05,2.9e-05,2.9e-05,2.9e-05,3.5e-05,3.1e-05,2.9e-05,2.9e-05,3.6e-05,3.1e-05,2.9e-05,3.1e-05,2.9e-05,3.5e-05,3.5e-05,2.9e-05,3.6e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,3.6e-05,2.9e-05,2.9e-05,2.9e-05,3.5e-05,3.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,3.1e-05,3.1e-05,3.1e-05,3.1e-05,3.6e-05,2.9e-05,3.1e-05,2.9e-05,3.1e-05,3.1e-05,3.1e-05,3.1e-05,3.1e-05,2.9e-05,3.5e-05,2.9e-05,3.5e-05,3.6e-05,2.9e-05,3.6e-05,3.5e-05,3.6e-05,2.9e-05,3.6e-05,3.5e-05,2.9e-05,2.9e-05,2.9e-05,3.1e-05,2.9e-05,3.6e-05,3.6e-05,3.6e-05,3.5e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.1e-05,3e-05,3e-05,3e-05,3e-05,3.5e-05,2.9e-05,3e-05,3.6e-05,2.9e-05,2.9e-05,3.5e-05,3.6e-05,3.6e-05,2.9e-05,2.9e-05,2.9e-05,3.6e-05,2.9e-05,2.9e-05,2.9e-05,3.6e-05,3.6e-05,2.9e-05,3.5e-05,2.9e-05,2.9e-05,3.6e-05,3.6e-05,3.6e-05,2.9e-05,3.5e-05,3.6e-05,3.6e-05,3.5e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.5e-05,3.6e-05,3.6e-05,3.6e-05,3.5e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,2.9e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.5e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.5e-05,3.6e-05,3.7e-05,3.6e-05,2.9e-05,2.9e-05,3.6e-05,2.9e-05,3.6e-05,2.9e-05,3e-05,2.9e-05,2.9e-05,3.6e-05,3.6e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,3.5e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,3.2e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[5.1e-05,5.2e-05,5.2e-05,2.9e-05,2.9e-05,5.1e-05,5.2e-05,5.2e-05,2.9e-05,5.2e-05,5.1e-05,2.9e-05,6.4e-05,6.4e-05,2.9e-05,6.2e-05,5.2e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,3e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,5.2e-05,2.9e-05,3e-05,5.2e-05,3e-05,3e-05,6.2e-05,2.9e-05,6.2e-05,6.2e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,6.2e-05,6.2e-05,6.2e-05,6.2e-05,6.2e-05,2.9e-05,5.1e-05,8e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,6.9e-05,2.9e-05,2.9e-05,2.9e-05,3e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,3e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000183,0.000178,0.000186,2.9e-05,2.9e-05,0.000182,0.000184,0.000182,2.9e-05,0.000178,0.000182,0.000176,0.000181,0.00018,0.000181,0.00018,0.000182,0.000176,2.9e-05,0.000179,0.000179,0.000178,0.000179,0.000173,2.9e-05,2.9e-05,0.000178,0.000179,2.9e-05,2.9e-05,2.9e-05,0.000181,0.00018,0.000181,2.9e-05,0.000178,0.000181,0.000169,0.000169,0.000183,0.00018,0.000179,0.000179,0.000176,0.000173,0.000179,0.000182,0.00018,0.00018,0.00018,0.00018,0.00018,0.000174,0.00018,0.000183,0.000178,0.000181,0.000178,0.000182,0.000183,0.000179,2.9e-05,0.000181,0.000181,0.000179,0.000174,0.000178,0.000179,0.000174,0.000181,0.000181,0.000181,0.000183,0.000181,0.000181,0.000181,0.000181,0.000181,0.000174,0.000174,0.000174,0.000174,0.000183,0.000178,0.000179,0.000181,0.000169,0.000176,0.000181,0.000181,0.000181,2.9e-05,2.9e-05,2.9e-05,0.000182,2.9e-05,0.000176,0.000173,0.000181,0.000182,2.9e-05,0.000177,2.9e-05,2.9e-05,0.000181,0.000181,0.000181,2.9e-05,0.000174,0.000184,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000183,0.000183,0.000181,0.000177,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,2.9e-05,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000182,0.000181,0.000181,0.000181,0.000181,0.000179,0.000184,0.000181,0.000175,2.9e-05,0.000181,2.9e-05,0.000181,2.9e-05,0.000182,0.000181,2.9e-05,0.000181,0.000181,2.9e-05,2.9e-05,0.000178,0.000179,0.000178,0.000178,0.000178,0.000178,0.000178,0.000178,0.000178,0.000178,0.000178,0.000181,0.000178,0.000178,0.000179,0.000178,0.000178,0.000178,0.000178,0.000178,0.000178,0.00018,0.000178,0.000178,0.000174,2.9e-05],[0.006571,0.005712,0.006573,0.005348,0.005348,0.00656,0.006562,0.005405,0.005348,0.005405,0.006561,0.005711,0.005376,0.005348,0.00661,0.00568,0.005348,0.005712,0.005376,0.006597,0.006566,0.005405,0.006609,0.005434,0.005376,0.005348,0.005376,0.006653,0.005348,0.005348,0.005348,0.006565,0.005812,0.005376,0.005348,0.005405,0.005348,0.005348,0.005348,0.005682,0.005812,0.00568,0.00568,0.006644,0.005348,0.005713,0.005376,0.00568,0.00568,0.00568,0.00568,0.00568,0.005376,0.006554,0.005348,0.006589,0.00661,0.005376,0.006606,0.006569,0.00676,0.005405,0.006608,0.006565,0.005348,0.005348,0.005376,0.005713,0.005405,0.00661,0.006608,0.006608,0.006566,0.00661,0.006611,0.006608,0.006608,0.005812,0.005586,0.005586,0.005586,0.005586,0.006568,0.005405,0.005586,0.00661,0.005405,0.005435,0.006567,0.006608,0.006608,0.005348,0.005405,0.005405,0.006653,0.005348,0.005435,0.005405,0.00661,0.006612,0.005348,0.006596,0.005348,0.005348,0.006608,0.006608,0.006608,0.005348,0.006601,0.006611,0.00661,0.006568,0.00661,0.006608,0.00661,0.00661,0.00661,0.006567,0.006654,0.006657,0.006608,0.006596,0.00661,0.00661,0.00661,0.006608,0.006608,0.00661,0.006609,0.005348,0.006608,0.00661,0.006608,0.006608,0.00661,0.006566,0.006608,0.006608,0.00661,0.006608,0.006613,0.006608,0.006608,0.00661,0.006565,0.00661,0.006837,0.006609,0.005348,0.005348,0.006608,0.005348,0.006608,0.005348,0.005494,0.005376,0.005348,0.006608,0.006608,0.005348,0.005348,0.005376,0.005435,0.005405,0.005376,0.005405,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.006602,0.005376,0.005376,0.005405,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005878,0.005376,0.005376,0.005376,0.005348],[0.009568,0.009619,0.009602,0.005348,0.005348,0.009564,0.009583,0.009748,0.005348,0.009706,0.009571,0.005348,0.0119,0.011918,0.005348,0.011494,0.009658,0.005348,0.005348,0.005348,0.009529,0.005494,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009543,0.005435,0.009612,0.005348,0.005494,0.009653,0.005523,0.005523,0.011556,0.005435,0.011485,0.011485,0.005348,0.005405,0.005348,0.005435,0.011494,0.011494,0.011494,0.011494,0.011494,0.005376,0.009548,0.014843,0.005376,0.005376,0.005348,0.005348,0.009562,0.005348,0.005405,0.005348,0.009562,0.012889,0.005376,0.005405,0.005348,0.005494,0.005348,0.005348,0.005348,0.009533,0.005348,0.005348,0.005348,0.005348,0.005435,0.005464,0.005464,0.005464,0.005464,0.009562,0.005376,0.005464,0.005348,0.005348,0.005376,0.009543,0.005348,0.005376,0.005348,0.005405,0.005405,0.005348,0.005348,0.005376,0.005494,0.005348,0.005348,0.005405,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009551,0.005348,0.005348,0.005348,0.005348,0.005348,0.009543,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009531,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009562,0.005348,0.005348,0.005348,0.005464,0.005348,0.005348,0.005348,0.005348,0.005348,0.005376,0.005376,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.033997,0.033179,0.034676,0.005348,0.005348,0.033763,0.03421,0.033775,0.005348,0.03302,0.033787,0.03277,0.033592,0.033556,0.033708,0.033538,0.033806,0.032706,0.005405,0.033244,0.033268,0.03302,0.033208,0.032264,0.005405,0.005348,0.033149,0.033214,0.005348,0.005348,0.005348,0.033708,0.033489,0.033653,0.005348,0.03302,0.033683,0.031477,0.031477,0.034122,0.033495,0.033268,0.033333,0.032758,0.032264,0.033256,0.033763,0.033556,0.033556,0.033556,0.033556,0.033556,0.032376,0.033453,0.03411,0.033108,0.03372,0.033149,0.0338,0.03411,0.033232,0.005405,0.033683,0.033683,0.03328,0.032438,0.033161,0.03325,0.032342,0.033708,0.033683,0.033683,0.033985,0.033726,0.033751,0.033683,0.033683,0.033745,0.032337,0.032337,0.032337,0.032337,0.033966,0.033173,0.033232,0.033708,0.031509,0.032666,0.033708,0.033683,0.033696,0.005348,0.005405,0.005405,0.033942,0.005348,0.032666,0.032124,0.033708,0.033886,0.005405,0.032845,0.005348,0.005348,0.033683,0.033683,0.033683,0.005348,0.032353,0.034286,0.033708,0.033751,0.033708,0.033683,0.033708,0.033708,0.033708,0.033708,0.033966,0.034053,0.033683,0.032845,0.033708,0.033708,0.033708,0.033683,0.033683,0.033708,0.033708,0.005348,0.033683,0.033708,0.033683,0.033683,0.033708,0.033708,0.033683,0.033683,0.033708,0.033683,0.033917,0.033683,0.033683,0.033708,0.033683,0.033309,0.034154,0.033726,0.032569,0.005348,0.033683,0.005348,0.033683,0.005348,0.033904,0.033745,0.005348,0.033683,0.033683,0.005348,0.005348,0.033149,0.033357,0.033155,0.033149,0.033155,0.033149,0.033149,0.033149,0.033149,0.033149,0.033149,0.033677,0.033149,0.033149,0.033345,0.033149,0.033149,0.033149,0.033149,0.033149,0.033149,0.033447,0.033149,0.033149,0.032365,0.005348],[0.009568,0.009619,0.009602,0.005348,0.005348,0.009564,0.009583,0.009748,0.005348,0.009706,0.009571,0.005348,0.0119,0.011918,0.005348,0.011494,0.009658,0.005348,0.005348,0.005348,0.009529,0.005494,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009543,0.005435,0.009612,0.005348,0.005494,0.009653,0.005523,0.005523,0.011556,0.005435,0.011485,0.011485,0.005348,0.005405,0.005348,0.005435,0.011494,0.011494,0.011494,0.011494,0.011494,0.005376,0.009548,0.014843,0.005376,0.005376,0.005348,0.005348,0.009562,0.005348,0.005405,0.005348,0.009562,0.012889,0.005376,0.005405,0.005348,0.005494,0.005348,0.005348,0.005348,0.009533,0.005348,0.005348,0.005348,0.005348,0.005435,0.005464,0.005464,0.005464,0.005464,0.009562,0.005376,0.005464,0.005348,0.005348,0.005376,0.009543,0.005348,0.005376,0.005348,0.005405,0.005405,0.005348,0.005348,0.005376,0.005494,0.005348,0.005348,0.005405,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009551,0.005348,0.005348,0.005348,0.005348,0.005348,0.009543,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009531,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009562,0.005348,0.005348,0.005348,0.005464,0.005348,0.005348,0.005348,0.005348,0.005348,0.005376,0.005376,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var4_df_closseness, by=list(var4_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var4_df_closseness, by=list(var4_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-eab52e4573b6052e30e5" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-eab52e4573b6052e30e5">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3e-05\" data-max=\"3.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2e-06\" data-max=\"3e-06\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.3e-05\" data-max=\"3.5e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"disabled\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000155\" data-max=\"0.000159\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.3e-05\" data-max=\"5.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005553\" data-max=\"0.00627\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000406\" data-max=\"0.000514\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.006105\" data-max=\"0.006512\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.002024\" data-max=\"0.002109\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.028897\" data-max=\"0.029676\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.009824\" data-max=\"0.010029\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.006105\" data-max=\"0.006512\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.002024\" data-max=\"0.002109\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[3.4e-05,3e-05],[3e-06,2e-06],[3.5e-05,3.3e-05],[1.1e-05,1.1e-05],[0.000159,0.000155],[5.3e-05,5.4e-05],[0.00627,0.005553],[0.000514,0.000406],[0.006512,0.006105],[0.002109,0.002024],[0.029676,0.028897],[0.009824,0.010029],[0.006512,0.006105],[0.002109,0.002024]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var4_df_closseness <- data.frame(
var4_incloseness,
var4_outcloseness,
var4_totalcloseness,
var4_incloseness_n,
var4_outcloseness_n,
var4_totalcloseness_n,
var4_centr_closeness) %>% round(6)

#Adding type
var4_df_closseness <-cbind(var4_df_closseness, V(var4)$TIPO2)

#Adding names
names(var4_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var4_df_closseness<-var4_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var4_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-112fabeb91b0789102d1" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-112fabeb91b0789102d1">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"3.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"8e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000186\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.006837\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.014843\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.034676\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.014843\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[3.5e-05,3.1e-05,3.5e-05,2.9e-05,2.9e-05,3.5e-05,3.5e-05,2.9e-05,2.9e-05,2.9e-05,3.5e-05,3.1e-05,2.9e-05,2.9e-05,3.6e-05,3.1e-05,2.9e-05,3.1e-05,2.9e-05,3.5e-05,3.5e-05,2.9e-05,3.6e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,3.6e-05,2.9e-05,2.9e-05,2.9e-05,3.5e-05,3.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,3.1e-05,3.1e-05,3.1e-05,3.1e-05,3.6e-05,2.9e-05,3.1e-05,2.9e-05,3.1e-05,3.1e-05,3.1e-05,3.1e-05,3.1e-05,2.9e-05,3.5e-05,2.9e-05,3.5e-05,3.6e-05,2.9e-05,3.6e-05,3.5e-05,3.6e-05,2.9e-05,3.6e-05,3.5e-05,2.9e-05,2.9e-05,2.9e-05,3.1e-05,2.9e-05,3.6e-05,3.6e-05,3.6e-05,3.5e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.1e-05,3e-05,3e-05,3e-05,3e-05,3.5e-05,2.9e-05,3e-05,3.6e-05,2.9e-05,2.9e-05,3.5e-05,3.6e-05,3.6e-05,2.9e-05,2.9e-05,2.9e-05,3.6e-05,2.9e-05,2.9e-05,2.9e-05,3.6e-05,3.6e-05,2.9e-05,3.5e-05,2.9e-05,2.9e-05,3.6e-05,3.6e-05,3.6e-05,2.9e-05,3.5e-05,3.6e-05,3.6e-05,3.5e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.5e-05,3.6e-05,3.6e-05,3.6e-05,3.5e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,2.9e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.5e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.6e-05,3.5e-05,3.6e-05,3.7e-05,3.6e-05,2.9e-05,2.9e-05,3.6e-05,2.9e-05,3.6e-05,2.9e-05,3e-05,2.9e-05,2.9e-05,3.6e-05,3.6e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,3.5e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,3.2e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[5.1e-05,5.2e-05,5.2e-05,2.9e-05,2.9e-05,5.1e-05,5.2e-05,5.2e-05,2.9e-05,5.2e-05,5.1e-05,2.9e-05,6.4e-05,6.4e-05,2.9e-05,6.2e-05,5.2e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,3e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,5.2e-05,2.9e-05,3e-05,5.2e-05,3e-05,3e-05,6.2e-05,2.9e-05,6.2e-05,6.2e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,6.2e-05,6.2e-05,6.2e-05,6.2e-05,6.2e-05,2.9e-05,5.1e-05,8e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,6.9e-05,2.9e-05,2.9e-05,2.9e-05,3e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,3e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,5.1e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000183,0.000178,0.000186,2.9e-05,2.9e-05,0.000182,0.000184,0.000182,2.9e-05,0.000178,0.000182,0.000176,0.000181,0.00018,0.000181,0.00018,0.000182,0.000176,2.9e-05,0.000179,0.000179,0.000178,0.000179,0.000173,2.9e-05,2.9e-05,0.000178,0.000179,2.9e-05,2.9e-05,2.9e-05,0.000181,0.00018,0.000181,2.9e-05,0.000178,0.000181,0.000169,0.000169,0.000183,0.00018,0.000179,0.000179,0.000176,0.000173,0.000179,0.000182,0.00018,0.00018,0.00018,0.00018,0.00018,0.000174,0.00018,0.000183,0.000178,0.000181,0.000178,0.000182,0.000183,0.000179,2.9e-05,0.000181,0.000181,0.000179,0.000174,0.000178,0.000179,0.000174,0.000181,0.000181,0.000181,0.000183,0.000181,0.000181,0.000181,0.000181,0.000181,0.000174,0.000174,0.000174,0.000174,0.000183,0.000178,0.000179,0.000181,0.000169,0.000176,0.000181,0.000181,0.000181,2.9e-05,2.9e-05,2.9e-05,0.000182,2.9e-05,0.000176,0.000173,0.000181,0.000182,2.9e-05,0.000177,2.9e-05,2.9e-05,0.000181,0.000181,0.000181,2.9e-05,0.000174,0.000184,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000183,0.000183,0.000181,0.000177,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,2.9e-05,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000181,0.000182,0.000181,0.000181,0.000181,0.000181,0.000179,0.000184,0.000181,0.000175,2.9e-05,0.000181,2.9e-05,0.000181,2.9e-05,0.000182,0.000181,2.9e-05,0.000181,0.000181,2.9e-05,2.9e-05,0.000178,0.000179,0.000178,0.000178,0.000178,0.000178,0.000178,0.000178,0.000178,0.000178,0.000178,0.000181,0.000178,0.000178,0.000179,0.000178,0.000178,0.000178,0.000178,0.000178,0.000178,0.00018,0.000178,0.000178,0.000174,2.9e-05],[0.006571,0.005712,0.006573,0.005348,0.005348,0.00656,0.006562,0.005405,0.005348,0.005405,0.006561,0.005711,0.005376,0.005348,0.00661,0.00568,0.005348,0.005712,0.005376,0.006597,0.006566,0.005405,0.006609,0.005434,0.005376,0.005348,0.005376,0.006653,0.005348,0.005348,0.005348,0.006565,0.005812,0.005376,0.005348,0.005405,0.005348,0.005348,0.005348,0.005682,0.005812,0.00568,0.00568,0.006644,0.005348,0.005713,0.005376,0.00568,0.00568,0.00568,0.00568,0.00568,0.005376,0.006554,0.005348,0.006589,0.00661,0.005376,0.006606,0.006569,0.00676,0.005405,0.006608,0.006565,0.005348,0.005348,0.005376,0.005713,0.005405,0.00661,0.006608,0.006608,0.006566,0.00661,0.006611,0.006608,0.006608,0.005812,0.005586,0.005586,0.005586,0.005586,0.006568,0.005405,0.005586,0.00661,0.005405,0.005435,0.006567,0.006608,0.006608,0.005348,0.005405,0.005405,0.006653,0.005348,0.005435,0.005405,0.00661,0.006612,0.005348,0.006596,0.005348,0.005348,0.006608,0.006608,0.006608,0.005348,0.006601,0.006611,0.00661,0.006568,0.00661,0.006608,0.00661,0.00661,0.00661,0.006567,0.006654,0.006657,0.006608,0.006596,0.00661,0.00661,0.00661,0.006608,0.006608,0.00661,0.006609,0.005348,0.006608,0.00661,0.006608,0.006608,0.00661,0.006566,0.006608,0.006608,0.00661,0.006608,0.006613,0.006608,0.006608,0.00661,0.006565,0.00661,0.006837,0.006609,0.005348,0.005348,0.006608,0.005348,0.006608,0.005348,0.005494,0.005376,0.005348,0.006608,0.006608,0.005348,0.005348,0.005376,0.005435,0.005405,0.005376,0.005405,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.006602,0.005376,0.005376,0.005405,0.005376,0.005376,0.005376,0.005376,0.005376,0.005376,0.005878,0.005376,0.005376,0.005376,0.005348],[0.009568,0.009619,0.009602,0.005348,0.005348,0.009564,0.009583,0.009748,0.005348,0.009706,0.009571,0.005348,0.0119,0.011918,0.005348,0.011494,0.009658,0.005348,0.005348,0.005348,0.009529,0.005494,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009543,0.005435,0.009612,0.005348,0.005494,0.009653,0.005523,0.005523,0.011556,0.005435,0.011485,0.011485,0.005348,0.005405,0.005348,0.005435,0.011494,0.011494,0.011494,0.011494,0.011494,0.005376,0.009548,0.014843,0.005376,0.005376,0.005348,0.005348,0.009562,0.005348,0.005405,0.005348,0.009562,0.012889,0.005376,0.005405,0.005348,0.005494,0.005348,0.005348,0.005348,0.009533,0.005348,0.005348,0.005348,0.005348,0.005435,0.005464,0.005464,0.005464,0.005464,0.009562,0.005376,0.005464,0.005348,0.005348,0.005376,0.009543,0.005348,0.005376,0.005348,0.005405,0.005405,0.005348,0.005348,0.005376,0.005494,0.005348,0.005348,0.005405,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009551,0.005348,0.005348,0.005348,0.005348,0.005348,0.009543,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009531,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009562,0.005348,0.005348,0.005348,0.005464,0.005348,0.005348,0.005348,0.005348,0.005348,0.005376,0.005376,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.033997,0.033179,0.034676,0.005348,0.005348,0.033763,0.03421,0.033775,0.005348,0.03302,0.033787,0.03277,0.033592,0.033556,0.033708,0.033538,0.033806,0.032706,0.005405,0.033244,0.033268,0.03302,0.033208,0.032264,0.005405,0.005348,0.033149,0.033214,0.005348,0.005348,0.005348,0.033708,0.033489,0.033653,0.005348,0.03302,0.033683,0.031477,0.031477,0.034122,0.033495,0.033268,0.033333,0.032758,0.032264,0.033256,0.033763,0.033556,0.033556,0.033556,0.033556,0.033556,0.032376,0.033453,0.03411,0.033108,0.03372,0.033149,0.0338,0.03411,0.033232,0.005405,0.033683,0.033683,0.03328,0.032438,0.033161,0.03325,0.032342,0.033708,0.033683,0.033683,0.033985,0.033726,0.033751,0.033683,0.033683,0.033745,0.032337,0.032337,0.032337,0.032337,0.033966,0.033173,0.033232,0.033708,0.031509,0.032666,0.033708,0.033683,0.033696,0.005348,0.005405,0.005405,0.033942,0.005348,0.032666,0.032124,0.033708,0.033886,0.005405,0.032845,0.005348,0.005348,0.033683,0.033683,0.033683,0.005348,0.032353,0.034286,0.033708,0.033751,0.033708,0.033683,0.033708,0.033708,0.033708,0.033708,0.033966,0.034053,0.033683,0.032845,0.033708,0.033708,0.033708,0.033683,0.033683,0.033708,0.033708,0.005348,0.033683,0.033708,0.033683,0.033683,0.033708,0.033708,0.033683,0.033683,0.033708,0.033683,0.033917,0.033683,0.033683,0.033708,0.033683,0.033309,0.034154,0.033726,0.032569,0.005348,0.033683,0.005348,0.033683,0.005348,0.033904,0.033745,0.005348,0.033683,0.033683,0.005348,0.005348,0.033149,0.033357,0.033155,0.033149,0.033155,0.033149,0.033149,0.033149,0.033149,0.033149,0.033149,0.033677,0.033149,0.033149,0.033345,0.033149,0.033149,0.033149,0.033149,0.033149,0.033149,0.033447,0.033149,0.033149,0.032365,0.005348],[0.009568,0.009619,0.009602,0.005348,0.005348,0.009564,0.009583,0.009748,0.005348,0.009706,0.009571,0.005348,0.0119,0.011918,0.005348,0.011494,0.009658,0.005348,0.005348,0.005348,0.009529,0.005494,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009543,0.005435,0.009612,0.005348,0.005494,0.009653,0.005523,0.005523,0.011556,0.005435,0.011485,0.011485,0.005348,0.005405,0.005348,0.005435,0.011494,0.011494,0.011494,0.011494,0.011494,0.005376,0.009548,0.014843,0.005376,0.005376,0.005348,0.005348,0.009562,0.005348,0.005405,0.005348,0.009562,0.012889,0.005376,0.005405,0.005348,0.005494,0.005348,0.005348,0.005348,0.009533,0.005348,0.005348,0.005348,0.005348,0.005435,0.005464,0.005464,0.005464,0.005464,0.009562,0.005376,0.005464,0.005348,0.005348,0.005376,0.009543,0.005348,0.005376,0.005348,0.005405,0.005405,0.005348,0.005348,0.005376,0.005494,0.005348,0.005348,0.005405,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009551,0.005348,0.005348,0.005348,0.005348,0.005348,0.009543,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009531,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.009562,0.005348,0.005348,0.005348,0.005464,0.005348,0.005348,0.005348,0.005348,0.005348,0.005376,0.005376,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var4_df_closseness, by=list(var4_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var4_df_closseness, by=list(var4_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-9b6307a905918f6b8c70" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-9b6307a905918f6b8c70">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3e-05\" data-max=\"3.5e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2e-06\" data-max=\"3e-06\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.3e-05\" data-max=\"3.5e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"9e-06\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00012\" data-max=\"0.000166\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.5e-05\" data-max=\"7.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005525\" data-max=\"0.006371\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000361\" data-max=\"0.000475\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.006194\" data-max=\"0.006482\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00175\" data-max=\"0.002122\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.02222\" data-max=\"0.03092\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.008376\" data-max=\"0.014262\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.006194\" data-max=\"0.006482\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00175\" data-max=\"0.002122\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[3.1e-05,3.5e-05,3e-05],[2e-06,3e-06,2e-06],[3.3e-05,3.5e-05,3.3e-05],[9e-06,1.1e-05,1.1e-05],[0.00012,0.000166,0.000155],[7.7e-05,4.5e-05,5.4e-05],[0.005734,0.006371,0.005525],[0.000475,0.000463,0.000361],[0.006217,0.006482,0.006194],[0.00175,0.002098,0.002122],[0.02222,0.03092,0.028822],[0.014262,0.008376,0.010073],[0.006217,0.006482,0.006194],[0.00175,0.002098,0.002122]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var4_data.RData")
```

