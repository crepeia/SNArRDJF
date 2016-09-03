# SNA Closeness 10_ACESSO B) O horário de funcionamento deste serviço possibilita a realização de ações em conjunto. (var8)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 10_ACESSO B) O horário de funcionamento deste serviço possibilita a realização de ações em conjunto. (var8)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var8_data.RData")
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
#var8<-simplify(var8) #Simplify
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
V(var8)$incloseness <- closeness(var8, mode = "in", weights = E(var8)$var8) %>% round(6)
V(var8)$outcloseness <- closeness(var8, mode = "out", weights = E(var8)$var8) %>% round(6)
V(var8)$totalcloseness <- closeness(var8, mode = "total", weights = E(var8)$var8) %>% round(4)
```

###Saving to Environment

```r
var8_incloseness<- closeness(var8, mode = "in", weights = E(var8)$var8) %>% round(6)
var8_outcloseness<- closeness(var8, mode = "out", weights = E(var8)$var8) %>% round(6)
var8_totalcloseness<- closeness(var8, mode = "total", weights = E(var8)$var8) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var8_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0001220 0.0001250 0.0001232 0.0001260 0.0001310
```

```r
sd(var8_incloseness)
```

```
## [1] 7.692576e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var8)$incloseness<-closeness(var8, weights = E(var8)$var8, mode="in")

#Get Variable
V(var8)$var8_color_degree<-round(V(var8)$incloseness,6)

#Creating brewer pallette
vertex_var8_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var8)$var8_color_degree)), "RdBu"))(
            length(unique(V(var8)$var8_color_degree)))

#Saving as Vertex properties 
V(var8)$vertex_var8_color_degree<-
  vertex_var8_color_degree[as.numeric(
  cut(V(var8)$var8_color_degree,
      breaks=length(unique(V(var8)$var8_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var8, es=E(var8), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var8))
maxC <- rep(Inf, vcount(var8))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var8, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var8)$weight)


#PLotting
plot(var8, 
     layout=co,
     edge.color=V(var8)$vertex_var8_color_degree[edge.start],
     edge.arrow.size=closeness(var8, weights = E(var8)$var8, mode="in"),
     edge.width=E(var8)$weight/mean(E(var8)$weight),
     edge.curved = TRUE,
     vertex.color=V(var8)$vertex_var8_color_degree,
     vertex.size=closeness(var8, weights = E(var8)$var8, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var8,"LABEL_COR"),
     vertex.label.cex=(closeness(var8, weights = E(var8)$var8, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var8)$var8_color_degree
b<-V(var8)$vertex_var8_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 10_ACESSO B) O horário de funcionamento deste serviço possibilita a realização de ações em conjunto. (var8)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var8, mode="in", weights = E(var8)$var8)), 
             sd(closeness(var8, mode="in", weights = E(var8)$var8))
             )
       )
```

![](10_ACESSO_B_realização_de_ações_em_conjunto_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var8_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0005410 0.0006710 0.0005599 0.0007190 0.0009820
```

```r
sd(var8_outcloseness)
```

```
## [1] 0.0002813041
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var8)$outcloseness<-closeness(var8, weights = E(var8)$var8, mode="out")

#Get Variable
V(var8)$var8_color_degree<-round(V(var8)$outcloseness,6)

#Creating brewer pallette
vertex_var8_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var8)$var8_color_degree)), "RdBu"))(
            length(unique(V(var8)$var8_color_degree)))

#Saving as Vertex properties 
V(var8)$vertex_var8_color_degree<-
  vertex_var8_color_degree[as.numeric(
  cut(V(var8)$var8_color_degree,
      breaks=length(unique(V(var8)$var8_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var8, es=E(var8), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var8))
maxC <- rep(Inf, vcount(var8))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var8, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var8)$weight)


#PLotting
plot(var8, 
     layout=co,
     edge.color=V(var8)$vertex_var8_color_degree[edge.start],
     edge.arrow.size=closeness(var8, weights = E(var8)$var8, mode="out"),
     edge.width=E(var8)$weight/2*mean(E(var8)$weight),
     edge.curved = TRUE,
     vertex.color=V(var8)$vertex_var8_color_degree,
     vertex.size=closeness(var8, weights = E(var8)$var8, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var8,"LABEL_COR"),
     vertex.label.cex=closeness(var8, weights = E(var8)$var8, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var8)$var8_color_degree
b<-V(var8)$vertex_var8_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 10_ACESSO B) O horário de funcionamento deste serviço possibilita a realização de ações em conjunto. (var8)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var8, mode="out", weights = E(var8)$var8)), 
             sd(closeness(var8, mode="out", weights = E(var8)$var8))
             )
       )
```

![](10_ACESSO_B_realização_de_ações_em_conjunto_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var8_totalcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0005800 0.0008750 0.0009400 0.0009478 0.0010310 0.0015500
```

```r
sd(var8_totalcloseness)
```

```
## [1] 0.0001580186
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var8)$allcloseness<-closeness(var8, weights = E(var8)$var8, mode="all")

#Get Variable
V(var8)$var8_color_degree<-round(V(var8)$allcloseness,6)

#Creating brewer pallette
vertex_var8_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var8)$var8_color_degree)), "RdBu"))(
            length(unique(V(var8)$var8_color_degree)))

#Saving as Vertex properties 
V(var8)$vertex_var8_color_degree<-
  vertex_var8_color_degree[as.numeric(
  cut(V(var8)$var8_color_degree,
      breaks=length(unique(V(var8)$var8_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var8, es=E(var8), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var8))
maxC <- rep(Inf, vcount(var8))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var8, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var8)$weight)


#PLotting
plot(var8, 
     layout=co,
     edge.color=V(var8)$vertex_var8_color_degree[edge.start],
     edge.arrow.size=closeness(var8, weights = E(var8)$var8, mode="all"),
     edge.width=E(var8)$weight/2*mean(E(var8)$weight),
     edge.curved = TRUE,
     vertex.color=V(var8)$vertex_var8_color_degree,
     vertex.size=closeness(var8, weights = E(var8)$var8, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var8,"LABEL_COR"),
     vertex.label.cex=(closeness(var8, weights = E(var8)$var8, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var8)$var8_color_degree
b<-V(var8)$vertex_var8_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 10_ACESSO B) O horário de funcionamento deste serviço possibilita a realização de ações em conjunto. (var8)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var8, mode="all", weights = E(var8)$var8)), 
             sd(closeness(var8, mode="all", weights = E(var8)$var8))
             )
       )
```

![](10_ACESSO_B_realização_de_ações_em_conjunto_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var8)$incloseness_n <- closeness(var8, mode = "in",, weights = E(var8)$var8, normalized = T) %>% round(10)
V(var8)$outcloseness_n <- closeness(var8, mode = "out", normalized = T, weights = E(var8)$var8) %>% round(6)
V(var8)$totalcloseness_n <- closeness(var8, mode = "total", normalized = T, weights = E(var8)$var8) %>% round(6)
```

###Saving to Environment

```r
var8_incloseness_n<- closeness(var8, mode = "in", normalized = T, weights = E(var8)$var8) %>% round(6)
var8_outcloseness_n<- closeness(var8, mode = "out", normalized = T, weights = E(var8)$var8) %>% round(6)
var8_totalcloseness_n<- closeness(var8, mode = "total", normalized = T, weights = E(var8)$var8) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var8_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.022660 0.023240 0.022920 0.023390 0.024350
```

```r
sd(var8_incloseness_n)
```

```
## [1] 0.001432067
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var8)$incloseness_n<-closeness(var8, weights = E(var8)$var8, mode="in", normalized = T)

#Get Variable
V(var8)$var8_color_degree<-round(V(var8)$incloseness_n,6)

#Creating brewer pallette
vertex_var8_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var8)$var8_color_degree)), "RdBu"))(
            length(unique(V(var8)$var8_color_degree)))

#Saving as Vertex properties 
V(var8)$vertex_var8_color_degree<-
  vertex_var8_color_degree[as.numeric(
  cut(V(var8)$var8_color_degree,
      breaks=length(unique(V(var8)$var8_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var8, es=E(var8), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var8))
maxC <- rep(Inf, vcount(var8))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var8, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var8)$weight)


#PLotting
plot(var8, 
     layout=co,
     edge.color=V(var8)$vertex_var8_color_degree[edge.start],
     edge.arrow.size=closeness(var8, weights = E(var8)$var8, mode="in",normalized = T),
     edge.width=E(var8)$weight/10*mean(E(var8)$weight),
     edge.curved = TRUE,
     vertex.color=V(var8)$vertex_var8_color_degree,
     vertex.size=(closeness(var8, weights = E(var8)$var8, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var8,"LABEL_COR"),
     vertex.label.cex=closeness(var8, weights = E(var8)$var8, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var8)$var8_color_degree
b<-V(var8)$vertex_var8_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 10_ACESSO B) O horário de funcionamento deste serviço possibilita a realização de ações em conjunto. (var8)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var8, mode="in", weights = E(var8)$var8, normalized = T)), 
             sd(closeness(var8, mode="in", weights = E(var8)$var8, normalized = T))
             )
       )
```

![](10_ACESSO_B_realização_de_ações_em_conjunto_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var8_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.100700 0.124800 0.104100 0.133800 0.182700
```

```r
sd(var8_outcloseness_n)
```

```
## [1] 0.05233834
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var8)$outcloseness_n<-closeness(var8, weights = E(var8)$var8, mode="out", normalized = T)

#Get Variable
V(var8)$var8_color_degree<-round(V(var8)$outcloseness_n,6)

#Creating brewer pallette
vertex_var8_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var8)$var8_color_degree)), "RdBu"))(
            length(unique(V(var8)$var8_color_degree)))

#Saving as Vertex properties 
V(var8)$vertex_var8_color_degree<-
  vertex_var8_color_degree[as.numeric(
  cut(V(var8)$var8_color_degree,
      breaks=length(unique(V(var8)$var8_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var8, es=E(var8), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var8))
maxC <- rep(Inf, vcount(var8))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var8, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var8)$weight)


#PLotting
plot(var8, 
     layout=co,
     edge.color=V(var8)$vertex_var8_color_degree[edge.start],
     edge.arrow.size=closeness(var8, weights = E(var8)$var8, mode="out",normalized = T),
     edge.width=E(var8)$weight/10*mean(E(var8)$weight),
     edge.curved = TRUE,
     vertex.color=V(var8)$vertex_var8_color_degree,
     vertex.size=(closeness(var8, weights = E(var8)$var8, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var8,"LABEL_COR"),
     vertex.label.cex=closeness(var8, weights = E(var8)$var8, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var8)$var8_color_degree
b<-V(var8)$vertex_var8_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 10_ACESSO B) O horário de funcionamento deste serviço possibilita a realização de ações em conjunto. (var8)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var8, mode="out", weights = E(var8)$var8, normalized = T)), 
             sd(closeness(var8, mode="out", weights = E(var8)$var8, normalized = T))
             )
       )
```

![](10_ACESSO_B_realização_de_ações_em_conjunto_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var8_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1078  0.1628  0.1748  0.1763  0.1917  0.2884
```

```r
sd(var8_totalcloseness_n)
```

```
## [1] 0.02939541
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var8)$allcloseness_n<-closeness(var8, weights = E(var8)$var8, mode="all", normalized = T)

#Get Variable
V(var8)$var8_color_degree<-round(V(var8)$allcloseness_n,6)

#Creating brewer pallette
vertex_var8_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var8)$var8_color_degree)), "RdBu"))(
            length(unique(V(var8)$var8_color_degree)))

#Saving as Vertex properties 
V(var8)$vertex_var8_color_degree<-
  vertex_var8_color_degree[as.numeric(
  cut(V(var8)$var8_color_degree,
      breaks=length(unique(V(var8)$var8_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var8, es=E(var8), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var8))
maxC <- rep(Inf, vcount(var8))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var8, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var8)$weight)


#PLotting
plot(var8, 
     layout=co,
     edge.color=V(var8)$vertex_var8_color_degree[edge.start],
     edge.arrow.size=closeness(var8, weights = E(var8)$var8, mode="all",normalized = T),
     edge.width=E(var8)$weight/10*mean(E(var8)$weight),
     edge.curved = TRUE,
     vertex.color=V(var8)$vertex_var8_color_degree,
     vertex.size=(closeness(var8, weights = E(var8)$var8, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var8,"LABEL_COR"),
     vertex.label.cex=closeness(var8, weights = E(var8)$var8, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var8)$var8_color_degree
b<-V(var8)$vertex_var8_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 10_ACESSO B) O horário de funcionamento deste serviço possibilita a realização de ações em conjunto. (var8)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var8, mode="all", weights = E(var8)$var8, normalized = T)), 
             sd(closeness(var8, mode="all", weights = E(var8)$var8, normalized = T))
             )
       )
```

![](10_ACESSO_B_realização_de_ações_em_conjunto_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var8)$incloseness_n <- closeness(var8, weights = E(var8)$var8, mode = "in", normalized = T) %>% round(6)
V(var8)$outcloseness_n <- closeness(var8, weights = E(var8)$var8, mode = "out", normalized = T) %>% round(6)
V(var8)$totalcloseness_n <- closeness(var8, weights = E(var8)$var8, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var8)$var8_centr_closeness<- centralization.closeness(var8)$res
var8_centr_closeness<- centralization.closeness(var8)$res
var8_centr_closeness_all<- centralization.closeness(var8)
```

###Centralization

```r
var8_centr_closeness_all$centralization
```

```
## [1] 0.1625197
```

###Theoretical Max

```r
var8_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var8)$var8_centr_closeness<- centralization.closeness(var8)$res

#Get Variable
V(var8)$var8_color_degree<-round(V(var8)$var8_centr_closeness,6)

#Creating brewer pallette
vertex_var8_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var8)$var8_color_degree)), "Spectral"))(
            length(unique(V(var8)$var8_color_degree)))

#Saving as Vertex properties 
V(var8)$vertex_var8_color_degree<-
  vertex_var8_color_degree[as.numeric(
  cut(V(var8)$var8_color_degree,
      breaks=length(unique(V(var8)$var8_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var8, es=E(var8), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var8))
maxC <- rep(Inf, vcount(var8))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var8, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var8)$weight)


#PLotting
plot(var8, 
     layout=co,
     edge.color=V(var8)$vertex_var8_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var8)$res,
     edge.width=E(var8)$weight/10*mean(E(var8)$weight),
     edge.curved = TRUE,
     vertex.color=V(var8)$vertex_var8_color_degree,
     vertex.size=centralization.closeness(var8)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var8,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var8)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var8)$var8_color_degree
b<-V(var8)$vertex_var8_color_degree
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
  title("Network Centralization Closeness - 10_ACESSO B) O horário de funcionamento deste serviço possibilita a realização de ações em conjunto. (var8)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var8)$res), 
             sd(centralization.closeness(var8)$res)
             )
       )
```

![](10_ACESSO_B_realização_de_ações_em_conjunto_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var8_incloseness<- closeness(var8, weights = E(var8)$var8, mode = "in") %>% round(6)
var8_outcloseness<- closeness(var8, weights = E(var8)$var8, mode = "out") %>% round(6)
var8_totalcloseness<- closeness(var8, weights = E(var8)$var8, mode = "total") %>% round(6)
var8_incloseness_n<- closeness(var8,weights = E(var8)$var8, mode = "in", normalized = T) %>% round(6)
var8_outcloseness_n<- closeness(var8,weights = E(var8)$var8, mode = "out", normalized = T) %>% round(6)
var8_totalcloseness_n<- closeness(var8,weights = E(var8)$var8, mode = "total", normalized = T) %>% round(6)
var8_centr_closeness <- centralization.closeness(var8)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var8_df_closseness <- data.frame(
var8_incloseness,
var8_outcloseness,
var8_totalcloseness,
var8_incloseness_n,
var8_outcloseness_n,
var8_totalcloseness_n,
var8_centr_closeness) %>% round(6)

#Adding type
var8_df_closseness <-cbind(var8_df_closseness, V(var8)$LABEL_COR)

#Adding names
names(var8_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var8_df_closseness<-var8_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var8_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-4bd1719d5aeb02ee3cc7" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-4bd1719d5aeb02ee3cc7">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000982\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00058\" data-max=\"0.00155\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024346\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.182711\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.107826\" data-max=\"0.288372\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[0.000129,0.000127,0.000131,0.000126,0.000126,0.000126,0.000125,0.000126,0.00012,0.000125,0.000127,0.000126,0.00012,0.000121,0.000125,0.000125,0.000126,0.000128,0.00012,0.000125,0.000126,0.00012,0.000126,0.000117,0.000115,0.000124,0.00012,0.000126,0.000126,0.000127,0.000127,0.000125,0.000126,0.000123,0.000126,0.000121,0.000125,0.000122,0.000121,0.000126,0.000126,0.000126,0.000125,0.000125,0.000125,0.000126,0.000115,0.000122,0.000122,0.000122,0.000122,0.000122,0.000116,0.000126,0.000119,0.000126,0.000127,0.000119,0.000121,0.000127,0.000128,0.000118,0.000125,0.000125,0.000122,2.9e-05,0.000116,0.000125,0.000123,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.000126,0.000117,0.000117,0.000117,0.000117,0.000125,0.000123,0.000123,0.000125,0.000124,0.000114,0.000125,0.000125,0.000125,0.000115,0.000119,0.000124,0.000127,0.000121,0.000114,0.00012,0.000125,0.000126,0.000119,0.000122,0.000124,0.000124,0.000125,0.000125,0.000125,0.000124,0.000124,0.000125,0.000125,0.000126,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.000127,0.000125,0.000127,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000124,0.000126,0.000126,0.000125,0.000125,0.000125,0.000125,0.000127,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.000124,0.000127,0.000126,0.000127,0.00012,0.000124,0.000126,0.000124,0.000126,0.000124,0.000125,0.000117,0.000127,0.000129,0.000129,0.000117,0.000118,0.000126,0.000128,0.000127,0.000118,0.000118,0.000126,0.000118,0.000118,0.000122,0.000122,0.000118,0.000122,0.000124,0.000123,0.000123,0.000126,0.000118,0.000126,0.000122,0.000127,0.000127,0.000125,0.000127,0.000122,0.000118,0.000127],[0.000929,0.000908,0.000982,0.000834,0.000728,0.000696,0.00071,0.000748,0.000758,0.000657,0.000754,0.000652,0.000802,0.000778,0.000783,0.000687,0.000855,0.00088,0.000671,0.000661,0.000693,0.000651,0.000701,0.000781,0.000669,0.000701,0.000708,0.000852,0.000803,0.000755,0.000735,0.000707,0.000765,0.000524,0.00048,0.000651,0.000735,0.000746,0.000705,0.000772,0.000859,0.000755,0.000756,0.000698,0.00071,0.000703,0.0006,0.000688,0.000688,0.000688,0.000688,0.000688,0.000718,0.000695,0.00078,0.000739,0.000719,0.000853,0.000593,0.000756,2.9e-05,0.000515,0.000719,0.000691,0.000694,2.9e-05,2.9e-05,0.000715,0.000718,0.000639,0.000639,0.000686,0.000691,0.000711,0.000656,0.000887,0.000652,0.000806,0.000541,0.000541,0.000541,0.000541,0.000649,0.000546,0.000767,0.000801,0.000664,0.000622,0.000733,0.000675,0.000661,0.000945,0.000686,0.000605,0.00087,0.000653,0.000463,0.00048,0.000646,0.000747,0.000514,0.000749,2.9e-05,2.9e-05,0.000882,0.00072,0.00066,2.9e-05,2.9e-05,0.000693,0.000672,0.00068,0.000649,0.000848,0.000677,0.000676,0.000698,0.000693,0.000758,0.000682,0.000699,0.00073,0.000641,0.000641,0.000641,0.00068,0.000637,0.000672,0.000637,0.00068,0.000678,0.000678,0.000704,0.000722,0.000637,0.000701,0.000658,0.000637,0.000637,0.000907,0.000638,0.000637,0.000646,0.000637,0.000722,0.000637,0.000679,0.000572,0.000601,0.000605,0.000605,0.000605,0.000605,0.000605,0.000544,0.000607,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001437,0.001325,0.00155,0.0011,0.001029,0.000982,0.000962,0.00102,0.001085,0.001078,0.001193,0.001037,0.001031,0.00096,0.00106,0.000929,0.001133,0.001316,0.000874,0.000978,0.000978,0.000846,0.00098,0.001009,0.000864,0.001015,0.001016,0.001221,0.001052,0.001131,0.001185,0.000916,0.00105,0.000822,0.000934,0.001067,0.001082,0.000968,0.000892,0.00102,0.001127,0.001079,0.000985,0.001126,0.000943,0.000978,0.00083,0.000914,0.000914,0.000914,0.000914,0.000914,0.000946,0.001115,0.00103,0.000997,0.001122,0.001232,0.000893,0.001247,0.000951,0.00066,0.00097,0.000917,0.001081,0.00058,0.000672,0.00094,0.001072,0.000894,0.000892,0.000897,0.00094,0.000966,0.000917,0.001264,0.000935,0.001109,0.000684,0.000684,0.000684,0.000684,0.000905,0.000931,0.001091,0.001073,0.00111,0.000811,0.000954,0.0009,0.000897,0.001304,0.000903,0.000921,0.001233,0.000931,0.000657,0.000735,0.000898,0.00105,0.000786,0.000959,0.000774,0.000774,0.001266,0.00093,0.000887,0.000774,0.000784,0.000946,0.000882,0.001001,0.000962,0.001255,0.000914,0.000894,0.000904,0.000894,0.001034,0.001087,0.000906,0.001095,0.000968,0.000962,0.000968,0.000974,0.000962,0.000982,0.000973,0.000974,0.000959,0.000964,0.000913,0.001017,0.00089,0.00091,0.001101,0.000886,0.000897,0.00129,0.00088,0.000871,0.000865,0.000876,0.000988,0.001048,0.000966,0.001112,0.000771,0.000933,0.000959,0.000933,0.000959,0.000933,0.00098,0.000873,0.000935,0.000962,0.000959,0.000723,0.00085,0.000869,0.000855,0.000838,0.000669,0.000657,0.000813,0.000669,0.000669,0.000693,0.000698,0.000669,0.000696,0.000779,0.000802,0.000816,0.001065,0.000669,0.001065,0.000693,0.000922,0.000922,0.000867,0.00091,0.000702,0.000676,0.000833],[0.023997,0.02364,0.024346,0.023423,0.02344,0.023405,0.023235,0.023373,0.022337,0.023212,0.023691,0.023464,0.022259,0.022526,0.023218,0.02316,0.023461,0.023779,0.022388,0.02334,0.023408,0.022308,0.023432,0.021734,0.021431,0.023034,0.022407,0.023452,0.023379,0.023619,0.023661,0.023238,0.023429,0.022904,0.023387,0.022461,0.023282,0.022619,0.022545,0.023343,0.023376,0.023411,0.023192,0.023335,0.023192,0.023476,0.021476,0.022666,0.022666,0.022666,0.022666,0.022666,0.021508,0.0235,0.022222,0.023387,0.023604,0.022169,0.02248,0.023703,0.023782,0.021957,0.023244,0.023238,0.022664,0.005348,0.021553,0.023265,0.022918,0.023201,0.023265,0.023279,0.023282,0.0233,0.023308,0.023233,0.023265,0.023488,0.021765,0.021765,0.021765,0.021765,0.023279,0.022915,0.022923,0.023279,0.022994,0.021197,0.023262,0.023206,0.023317,0.021387,0.022103,0.023134,0.023571,0.022545,0.021197,0.02223,0.0233,0.0235,0.022156,0.022711,0.023051,0.023051,0.023233,0.023241,0.023247,0.023051,0.023111,0.023273,0.023256,0.02344,0.023276,0.023276,0.023282,0.023244,0.023235,0.023235,0.02325,0.023595,0.023238,0.023556,0.023399,0.023379,0.023399,0.023384,0.023379,0.023387,0.023393,0.023126,0.023367,0.023384,0.02327,0.023262,0.023279,0.023308,0.023589,0.023238,0.023282,0.023227,0.023279,0.023233,0.023221,0.023227,0.023031,0.023661,0.02339,0.02358,0.022332,0.023106,0.023367,0.023106,0.023367,0.023106,0.023265,0.021671,0.023661,0.023923,0.023914,0.021701,0.022041,0.023432,0.023746,0.023688,0.021916,0.022022,0.023376,0.021916,0.021916,0.022642,0.022655,0.021916,0.022642,0.023077,0.022814,0.022845,0.023408,0.021916,0.023408,0.022642,0.02355,0.02355,0.023338,0.023529,0.022633,0.021968,0.023544],[0.172862,0.168937,0.182711,0.155129,0.13547,0.129526,0.132009,0.139117,0.140909,0.122208,0.140166,0.121331,0.149158,0.144747,0.145654,0.127747,0.158974,0.163588,0.124832,0.122853,0.128988,0.121015,0.130343,0.145312,0.124415,0.130435,0.131635,0.158433,0.149398,0.140483,0.136664,0.131542,0.142202,0.097433,0.089294,0.121015,0.136765,0.138702,0.131171,0.143519,0.159794,0.140377,0.14059,0.129888,0.132009,0.13071,0.111645,0.127923,0.127923,0.127923,0.127923,0.127923,0.133621,0.129346,0.145086,0.137472,0.133813,0.158703,0.11032,0.14059,0.005348,0.095876,0.133717,0.128542,0.129077,0.005376,0.005405,0.133047,0.133525,0.118926,0.118926,0.127572,0.128542,0.13229,0.121967,0.164894,0.121252,0.149879,0.100704,0.100704,0.100704,0.100704,0.120779,0.101528,0.142638,0.148919,0.123506,0.1156,0.136364,0.125506,0.123016,0.175803,0.12766,0.112591,0.161739,0.12141,0.086071,0.089294,0.120077,0.139013,0.09563,0.139222,0.005348,0.005348,0.164021,0.134006,0.122772,0.005348,0.005348,0.128809,0.125,0.126531,0.120779,0.157761,0.125846,0.125761,0.129888,0.128898,0.141016,0.126876,0.129979,0.135866,0.119307,0.119307,0.119307,0.126445,0.118471,0.125,0.118471,0.126445,0.126187,0.126187,0.130986,0.134296,0.118471,0.130343,0.122368,0.118471,0.118471,0.168631,0.118698,0.118471,0.120155,0.118471,0.134296,0.118471,0.126273,0.106346,0.111712,0.112455,0.112455,0.112455,0.112455,0.112455,0.101252,0.112933,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.267241,0.246358,0.288372,0.20462,0.191358,0.182711,0.179018,0.189796,0.201735,0.200431,0.221957,0.192946,0.191753,0.178503,0.197243,0.172862,0.210646,0.244737,0.162587,0.181818,0.181996,0.15736,0.182353,0.187689,0.160761,0.188832,0.189024,0.227106,0.195584,0.210407,0.220379,0.17033,0.195378,0.152961,0.173669,0.198506,0.201299,0.180058,0.165923,0.189796,0.209696,0.200647,0.183251,0.209459,0.175306,0.181818,0.154357,0.170018,0.170018,0.170018,0.170018,0.170018,0.17597,0.207358,0.191555,0.185444,0.208754,0.229064,0.166071,0.23192,0.176806,0.122691,0.180407,0.170642,0.201081,0.107826,0.125,0.174812,0.199357,0.16622,0.165923,0.166816,0.174812,0.17971,0.170642,0.235145,0.173994,0.206208,0.127136,0.127136,0.127136,0.127136,0.168326,0.173184,0.202835,0.199571,0.206437,0.150852,0.177481,0.167417,0.166816,0.242503,0.168022,0.171271,0.229346,0.173184,0.122208,0.136765,0.167116,0.195378,0.146112,0.178332,0.143963,0.143963,0.235443,0.173023,0.164894,0.143963,0.145768,0.17597,0.164021,0.186186,0.178846,0.233375,0.170018,0.166369,0.168174,0.166369,0.192347,0.202174,0.168478,0.203724,0.180058,0.178846,0.180058,0.18111,0.178846,0.182711,0.180934,0.18111,0.178332,0.179364,0.169863,0.189217,0.165628,0.169245,0.204846,0.164748,0.166816,0.24,0.163732,0.162021,0.1609,0.163015,0.183794,0.194969,0.17971,0.206897,0.143408,0.173507,0.178332,0.173507,0.178332,0.173507,0.182353,0.162304,0.173994,0.178846,0.178332,0.13449,0.158163,0.161599,0.158974,0.155909,0.124415,0.122127,0.15122,0.124415,0.124415,0.128898,0.129888,0.124415,0.129526,0.14486,0.149158,0.151713,0.198083,0.124415,0.198083,0.128988,0.171429,0.171429,0.161318,0.169245,0.130526,0.125676,0.155],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var8_df_closseness, by=list(var8_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var8_df_closseness, by=list(var8_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-42fc347b0ace1f87443d" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-42fc347b0ace1f87443d">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000116\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000982\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.6e-05\" data-max=\"0.000329\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000833\" data-max=\"0.00155\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.8e-05\" data-max=\"0.000187\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.02155\" data-max=\"0.024346\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.8e-05\" data-max=\"0.003981\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.182711\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00309\" data-max=\"0.061281\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.155\" data-max=\"0.288372\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003304\" data-max=\"0.034881\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003883\" data-max=\"0.139638\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[0.000124,0.000121,0.000125,0.000127,0.000125,0.000127,0.000126,0.000131,0.000126,0.000126,0.000126,0.000116,0.000124,0.000126,0.000126,0.000123,0.000127,0.000126,0.000129,0.000123,0.000128,0.000126,0.000125],[3e-06,4e-06,null,null,null,null,1e-06,null,null,null,null,2.1e-05,4e-06,0,0,4e-06,null,null,null,1e-06,null,1e-06,1e-06],[0.000742,0.00026,0.000661,0.000908,0.00071,0.000756,0.000727,0.000982,0.000701,0.000679,0.000806,0.000623,0.000766,0.000717,0.00076,0.000702,2.9e-05,0.000747,0.000929,0.000474,0.00088,0.000546,0.000701],[0.000131,0.000315,null,null,null,null,0.000112,null,null,null,null,0.00017,1.6e-05,0.000103,8.4e-05,7e-05,null,null,null,0.000329,null,0.000232,7.4e-05],[0.000983,0.00084,0.000978,0.001325,0.000962,0.001247,0.001082,0.00155,0.00098,0.000966,0.001109,0.00093,0.00102,0.001067,0.001048,0.000957,0.000833,0.00105,0.001437,0.000872,0.001316,0.000961,0.000978],[0.000187,0.000162,null,null,null,null,0.000125,null,null,null,null,0.000172,8.4e-05,9.1e-05,7.7e-05,9.1e-05,null,null,null,7.3e-05,null,1.8e-05,0.000122],[0.023046,0.022549,0.02334,0.02364,0.023235,0.023703,0.023507,0.024346,0.023432,0.02339,0.023488,0.02155,0.022969,0.023499,0.023435,0.022835,0.023544,0.0235,0.023997,0.022826,0.023779,0.023399,0.023297],[0.000619,0.000725,null,null,null,null,0.000135,null,null,null,null,0.003981,0.000626,0.000125,2.8e-05,0.000625,null,null,null,0.000232,null,0.000229,0.000109],[0.137903,0.048319,0.122853,0.168937,0.132009,0.14059,0.135297,0.182711,0.130343,0.126273,0.149879,0.115798,0.142562,0.133405,0.141323,0.130632,0.005348,0.139013,0.172862,0.088219,0.163588,0.101578,0.130395],[0.024328,0.058573,null,null,null,null,0.020715,null,null,null,null,0.031564,0.00309,0.019201,0.015572,0.013013,null,null,null,0.061281,null,0.043144,0.013715],[0.182827,0.156227,0.181818,0.246358,0.179018,0.23192,0.201357,0.288372,0.182353,0.17971,0.206208,0.172981,0.189575,0.198536,0.194905,0.177985,0.155,0.195378,0.267241,0.162216,0.244737,0.178732,0.181885],[0.034881,0.030115,null,null,null,null,0.023224,null,null,null,null,0.031922,0.015658,0.017003,0.014301,0.016992,null,null,null,0.013551,null,0.003304,0.022623],[0.300585,0.099063,0.286154,0.343173,0.303922,0.323478,0.300896,0.391579,0.295238,0.29108,0.313659,0.266552,0.319611,0.301223,0.316849,0.295625,0.005348,0.306425,0.371257,0.194167,0.314189,0.234219,0.288627],[0.043014,0.125815,null,null,null,null,0.018146,null,null,null,null,0.070696,0.003883,0.033229,0.029748,0.023781,null,null,null,0.139638,null,0.102732,0.011577]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var8_df_closseness <- data.frame(
var8_incloseness,
var8_outcloseness,
var8_totalcloseness,
var8_incloseness_n,
var8_outcloseness_n,
var8_totalcloseness_n,
var8_centr_closeness) %>% round(6)

#Adding type
var8_df_closseness <-cbind(var8_df_closseness, V(var8)$TIPO1)

#Adding names
names(var8_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var8_df_closseness<-var8_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var8_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-8658a0c20997b02591ac" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-8658a0c20997b02591ac">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000982\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00058\" data-max=\"0.00155\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024346\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.182711\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.107826\" data-max=\"0.288372\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[0.000129,0.000127,0.000131,0.000126,0.000126,0.000126,0.000125,0.000126,0.00012,0.000125,0.000127,0.000126,0.00012,0.000121,0.000125,0.000125,0.000126,0.000128,0.00012,0.000125,0.000126,0.00012,0.000126,0.000117,0.000115,0.000124,0.00012,0.000126,0.000126,0.000127,0.000127,0.000125,0.000126,0.000123,0.000126,0.000121,0.000125,0.000122,0.000121,0.000126,0.000126,0.000126,0.000125,0.000125,0.000125,0.000126,0.000115,0.000122,0.000122,0.000122,0.000122,0.000122,0.000116,0.000126,0.000119,0.000126,0.000127,0.000119,0.000121,0.000127,0.000128,0.000118,0.000125,0.000125,0.000122,2.9e-05,0.000116,0.000125,0.000123,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.000126,0.000117,0.000117,0.000117,0.000117,0.000125,0.000123,0.000123,0.000125,0.000124,0.000114,0.000125,0.000125,0.000125,0.000115,0.000119,0.000124,0.000127,0.000121,0.000114,0.00012,0.000125,0.000126,0.000119,0.000122,0.000124,0.000124,0.000125,0.000125,0.000125,0.000124,0.000124,0.000125,0.000125,0.000126,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.000127,0.000125,0.000127,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000124,0.000126,0.000126,0.000125,0.000125,0.000125,0.000125,0.000127,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.000124,0.000127,0.000126,0.000127,0.00012,0.000124,0.000126,0.000124,0.000126,0.000124,0.000125,0.000117,0.000127,0.000129,0.000129,0.000117,0.000118,0.000126,0.000128,0.000127,0.000118,0.000118,0.000126,0.000118,0.000118,0.000122,0.000122,0.000118,0.000122,0.000124,0.000123,0.000123,0.000126,0.000118,0.000126,0.000122,0.000127,0.000127,0.000125,0.000127,0.000122,0.000118,0.000127],[0.000929,0.000908,0.000982,0.000834,0.000728,0.000696,0.00071,0.000748,0.000758,0.000657,0.000754,0.000652,0.000802,0.000778,0.000783,0.000687,0.000855,0.00088,0.000671,0.000661,0.000693,0.000651,0.000701,0.000781,0.000669,0.000701,0.000708,0.000852,0.000803,0.000755,0.000735,0.000707,0.000765,0.000524,0.00048,0.000651,0.000735,0.000746,0.000705,0.000772,0.000859,0.000755,0.000756,0.000698,0.00071,0.000703,0.0006,0.000688,0.000688,0.000688,0.000688,0.000688,0.000718,0.000695,0.00078,0.000739,0.000719,0.000853,0.000593,0.000756,2.9e-05,0.000515,0.000719,0.000691,0.000694,2.9e-05,2.9e-05,0.000715,0.000718,0.000639,0.000639,0.000686,0.000691,0.000711,0.000656,0.000887,0.000652,0.000806,0.000541,0.000541,0.000541,0.000541,0.000649,0.000546,0.000767,0.000801,0.000664,0.000622,0.000733,0.000675,0.000661,0.000945,0.000686,0.000605,0.00087,0.000653,0.000463,0.00048,0.000646,0.000747,0.000514,0.000749,2.9e-05,2.9e-05,0.000882,0.00072,0.00066,2.9e-05,2.9e-05,0.000693,0.000672,0.00068,0.000649,0.000848,0.000677,0.000676,0.000698,0.000693,0.000758,0.000682,0.000699,0.00073,0.000641,0.000641,0.000641,0.00068,0.000637,0.000672,0.000637,0.00068,0.000678,0.000678,0.000704,0.000722,0.000637,0.000701,0.000658,0.000637,0.000637,0.000907,0.000638,0.000637,0.000646,0.000637,0.000722,0.000637,0.000679,0.000572,0.000601,0.000605,0.000605,0.000605,0.000605,0.000605,0.000544,0.000607,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001437,0.001325,0.00155,0.0011,0.001029,0.000982,0.000962,0.00102,0.001085,0.001078,0.001193,0.001037,0.001031,0.00096,0.00106,0.000929,0.001133,0.001316,0.000874,0.000978,0.000978,0.000846,0.00098,0.001009,0.000864,0.001015,0.001016,0.001221,0.001052,0.001131,0.001185,0.000916,0.00105,0.000822,0.000934,0.001067,0.001082,0.000968,0.000892,0.00102,0.001127,0.001079,0.000985,0.001126,0.000943,0.000978,0.00083,0.000914,0.000914,0.000914,0.000914,0.000914,0.000946,0.001115,0.00103,0.000997,0.001122,0.001232,0.000893,0.001247,0.000951,0.00066,0.00097,0.000917,0.001081,0.00058,0.000672,0.00094,0.001072,0.000894,0.000892,0.000897,0.00094,0.000966,0.000917,0.001264,0.000935,0.001109,0.000684,0.000684,0.000684,0.000684,0.000905,0.000931,0.001091,0.001073,0.00111,0.000811,0.000954,0.0009,0.000897,0.001304,0.000903,0.000921,0.001233,0.000931,0.000657,0.000735,0.000898,0.00105,0.000786,0.000959,0.000774,0.000774,0.001266,0.00093,0.000887,0.000774,0.000784,0.000946,0.000882,0.001001,0.000962,0.001255,0.000914,0.000894,0.000904,0.000894,0.001034,0.001087,0.000906,0.001095,0.000968,0.000962,0.000968,0.000974,0.000962,0.000982,0.000973,0.000974,0.000959,0.000964,0.000913,0.001017,0.00089,0.00091,0.001101,0.000886,0.000897,0.00129,0.00088,0.000871,0.000865,0.000876,0.000988,0.001048,0.000966,0.001112,0.000771,0.000933,0.000959,0.000933,0.000959,0.000933,0.00098,0.000873,0.000935,0.000962,0.000959,0.000723,0.00085,0.000869,0.000855,0.000838,0.000669,0.000657,0.000813,0.000669,0.000669,0.000693,0.000698,0.000669,0.000696,0.000779,0.000802,0.000816,0.001065,0.000669,0.001065,0.000693,0.000922,0.000922,0.000867,0.00091,0.000702,0.000676,0.000833],[0.023997,0.02364,0.024346,0.023423,0.02344,0.023405,0.023235,0.023373,0.022337,0.023212,0.023691,0.023464,0.022259,0.022526,0.023218,0.02316,0.023461,0.023779,0.022388,0.02334,0.023408,0.022308,0.023432,0.021734,0.021431,0.023034,0.022407,0.023452,0.023379,0.023619,0.023661,0.023238,0.023429,0.022904,0.023387,0.022461,0.023282,0.022619,0.022545,0.023343,0.023376,0.023411,0.023192,0.023335,0.023192,0.023476,0.021476,0.022666,0.022666,0.022666,0.022666,0.022666,0.021508,0.0235,0.022222,0.023387,0.023604,0.022169,0.02248,0.023703,0.023782,0.021957,0.023244,0.023238,0.022664,0.005348,0.021553,0.023265,0.022918,0.023201,0.023265,0.023279,0.023282,0.0233,0.023308,0.023233,0.023265,0.023488,0.021765,0.021765,0.021765,0.021765,0.023279,0.022915,0.022923,0.023279,0.022994,0.021197,0.023262,0.023206,0.023317,0.021387,0.022103,0.023134,0.023571,0.022545,0.021197,0.02223,0.0233,0.0235,0.022156,0.022711,0.023051,0.023051,0.023233,0.023241,0.023247,0.023051,0.023111,0.023273,0.023256,0.02344,0.023276,0.023276,0.023282,0.023244,0.023235,0.023235,0.02325,0.023595,0.023238,0.023556,0.023399,0.023379,0.023399,0.023384,0.023379,0.023387,0.023393,0.023126,0.023367,0.023384,0.02327,0.023262,0.023279,0.023308,0.023589,0.023238,0.023282,0.023227,0.023279,0.023233,0.023221,0.023227,0.023031,0.023661,0.02339,0.02358,0.022332,0.023106,0.023367,0.023106,0.023367,0.023106,0.023265,0.021671,0.023661,0.023923,0.023914,0.021701,0.022041,0.023432,0.023746,0.023688,0.021916,0.022022,0.023376,0.021916,0.021916,0.022642,0.022655,0.021916,0.022642,0.023077,0.022814,0.022845,0.023408,0.021916,0.023408,0.022642,0.02355,0.02355,0.023338,0.023529,0.022633,0.021968,0.023544],[0.172862,0.168937,0.182711,0.155129,0.13547,0.129526,0.132009,0.139117,0.140909,0.122208,0.140166,0.121331,0.149158,0.144747,0.145654,0.127747,0.158974,0.163588,0.124832,0.122853,0.128988,0.121015,0.130343,0.145312,0.124415,0.130435,0.131635,0.158433,0.149398,0.140483,0.136664,0.131542,0.142202,0.097433,0.089294,0.121015,0.136765,0.138702,0.131171,0.143519,0.159794,0.140377,0.14059,0.129888,0.132009,0.13071,0.111645,0.127923,0.127923,0.127923,0.127923,0.127923,0.133621,0.129346,0.145086,0.137472,0.133813,0.158703,0.11032,0.14059,0.005348,0.095876,0.133717,0.128542,0.129077,0.005376,0.005405,0.133047,0.133525,0.118926,0.118926,0.127572,0.128542,0.13229,0.121967,0.164894,0.121252,0.149879,0.100704,0.100704,0.100704,0.100704,0.120779,0.101528,0.142638,0.148919,0.123506,0.1156,0.136364,0.125506,0.123016,0.175803,0.12766,0.112591,0.161739,0.12141,0.086071,0.089294,0.120077,0.139013,0.09563,0.139222,0.005348,0.005348,0.164021,0.134006,0.122772,0.005348,0.005348,0.128809,0.125,0.126531,0.120779,0.157761,0.125846,0.125761,0.129888,0.128898,0.141016,0.126876,0.129979,0.135866,0.119307,0.119307,0.119307,0.126445,0.118471,0.125,0.118471,0.126445,0.126187,0.126187,0.130986,0.134296,0.118471,0.130343,0.122368,0.118471,0.118471,0.168631,0.118698,0.118471,0.120155,0.118471,0.134296,0.118471,0.126273,0.106346,0.111712,0.112455,0.112455,0.112455,0.112455,0.112455,0.101252,0.112933,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.267241,0.246358,0.288372,0.20462,0.191358,0.182711,0.179018,0.189796,0.201735,0.200431,0.221957,0.192946,0.191753,0.178503,0.197243,0.172862,0.210646,0.244737,0.162587,0.181818,0.181996,0.15736,0.182353,0.187689,0.160761,0.188832,0.189024,0.227106,0.195584,0.210407,0.220379,0.17033,0.195378,0.152961,0.173669,0.198506,0.201299,0.180058,0.165923,0.189796,0.209696,0.200647,0.183251,0.209459,0.175306,0.181818,0.154357,0.170018,0.170018,0.170018,0.170018,0.170018,0.17597,0.207358,0.191555,0.185444,0.208754,0.229064,0.166071,0.23192,0.176806,0.122691,0.180407,0.170642,0.201081,0.107826,0.125,0.174812,0.199357,0.16622,0.165923,0.166816,0.174812,0.17971,0.170642,0.235145,0.173994,0.206208,0.127136,0.127136,0.127136,0.127136,0.168326,0.173184,0.202835,0.199571,0.206437,0.150852,0.177481,0.167417,0.166816,0.242503,0.168022,0.171271,0.229346,0.173184,0.122208,0.136765,0.167116,0.195378,0.146112,0.178332,0.143963,0.143963,0.235443,0.173023,0.164894,0.143963,0.145768,0.17597,0.164021,0.186186,0.178846,0.233375,0.170018,0.166369,0.168174,0.166369,0.192347,0.202174,0.168478,0.203724,0.180058,0.178846,0.180058,0.18111,0.178846,0.182711,0.180934,0.18111,0.178332,0.179364,0.169863,0.189217,0.165628,0.169245,0.204846,0.164748,0.166816,0.24,0.163732,0.162021,0.1609,0.163015,0.183794,0.194969,0.17971,0.206897,0.143408,0.173507,0.178332,0.173507,0.178332,0.173507,0.182353,0.162304,0.173994,0.178846,0.178332,0.13449,0.158163,0.161599,0.158974,0.155909,0.124415,0.122127,0.15122,0.124415,0.124415,0.128898,0.129888,0.124415,0.129526,0.14486,0.149158,0.151713,0.198083,0.124415,0.198083,0.128988,0.171429,0.171429,0.161318,0.169245,0.130526,0.125676,0.155],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var8_df_closseness, by=list(var8_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var8_df_closseness, by=list(var8_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-c392f071d2ba05cbfa8c" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-c392f071d2ba05cbfa8c">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00012\" data-max=\"0.000125\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000425\" data-max=\"0.000659\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000197\" data-max=\"0.000322\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000881\" data-max=\"0.000997\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000137\" data-max=\"0.000161\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.02236\" data-max=\"0.023324\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000271\" data-max=\"0.002061\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.078999\" data-max=\"0.122502\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.036605\" data-max=\"0.059893\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.163857\" data-max=\"0.185376\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.025503\" data-max=\"0.029957\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.173776\" data-max=\"0.2725\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.079091\" data-max=\"0.135432\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[0.000125,0.00012],[1e-06,1.1e-05],[0.000659,0.000425],[0.000197,0.000322],[0.000997,0.000881],[0.000137,0.000161],[0.023324,0.02236],[0.000271,0.002061],[0.122502,0.078999],[0.036605,0.059893],[0.185376,0.163857],[0.025503,0.029957],[0.2725,0.173776],[0.079091,0.135432]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var8_df_closseness <- data.frame(
var8_incloseness,
var8_outcloseness,
var8_totalcloseness,
var8_incloseness_n,
var8_outcloseness_n,
var8_totalcloseness_n,
var8_centr_closeness) %>% round(6)

#Adding type
var8_df_closseness <-cbind(var8_df_closseness, V(var8)$TIPO2)

#Adding names
names(var8_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var8_df_closseness<-var8_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var8_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-a14e4a053d78acfcddc8" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-a14e4a053d78acfcddc8">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000982\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00058\" data-max=\"0.00155\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024346\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.182711\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.107826\" data-max=\"0.288372\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[0.000129,0.000127,0.000131,0.000126,0.000126,0.000126,0.000125,0.000126,0.00012,0.000125,0.000127,0.000126,0.00012,0.000121,0.000125,0.000125,0.000126,0.000128,0.00012,0.000125,0.000126,0.00012,0.000126,0.000117,0.000115,0.000124,0.00012,0.000126,0.000126,0.000127,0.000127,0.000125,0.000126,0.000123,0.000126,0.000121,0.000125,0.000122,0.000121,0.000126,0.000126,0.000126,0.000125,0.000125,0.000125,0.000126,0.000115,0.000122,0.000122,0.000122,0.000122,0.000122,0.000116,0.000126,0.000119,0.000126,0.000127,0.000119,0.000121,0.000127,0.000128,0.000118,0.000125,0.000125,0.000122,2.9e-05,0.000116,0.000125,0.000123,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.000126,0.000117,0.000117,0.000117,0.000117,0.000125,0.000123,0.000123,0.000125,0.000124,0.000114,0.000125,0.000125,0.000125,0.000115,0.000119,0.000124,0.000127,0.000121,0.000114,0.00012,0.000125,0.000126,0.000119,0.000122,0.000124,0.000124,0.000125,0.000125,0.000125,0.000124,0.000124,0.000125,0.000125,0.000126,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.000127,0.000125,0.000127,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000124,0.000126,0.000126,0.000125,0.000125,0.000125,0.000125,0.000127,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.000125,0.000124,0.000127,0.000126,0.000127,0.00012,0.000124,0.000126,0.000124,0.000126,0.000124,0.000125,0.000117,0.000127,0.000129,0.000129,0.000117,0.000118,0.000126,0.000128,0.000127,0.000118,0.000118,0.000126,0.000118,0.000118,0.000122,0.000122,0.000118,0.000122,0.000124,0.000123,0.000123,0.000126,0.000118,0.000126,0.000122,0.000127,0.000127,0.000125,0.000127,0.000122,0.000118,0.000127],[0.000929,0.000908,0.000982,0.000834,0.000728,0.000696,0.00071,0.000748,0.000758,0.000657,0.000754,0.000652,0.000802,0.000778,0.000783,0.000687,0.000855,0.00088,0.000671,0.000661,0.000693,0.000651,0.000701,0.000781,0.000669,0.000701,0.000708,0.000852,0.000803,0.000755,0.000735,0.000707,0.000765,0.000524,0.00048,0.000651,0.000735,0.000746,0.000705,0.000772,0.000859,0.000755,0.000756,0.000698,0.00071,0.000703,0.0006,0.000688,0.000688,0.000688,0.000688,0.000688,0.000718,0.000695,0.00078,0.000739,0.000719,0.000853,0.000593,0.000756,2.9e-05,0.000515,0.000719,0.000691,0.000694,2.9e-05,2.9e-05,0.000715,0.000718,0.000639,0.000639,0.000686,0.000691,0.000711,0.000656,0.000887,0.000652,0.000806,0.000541,0.000541,0.000541,0.000541,0.000649,0.000546,0.000767,0.000801,0.000664,0.000622,0.000733,0.000675,0.000661,0.000945,0.000686,0.000605,0.00087,0.000653,0.000463,0.00048,0.000646,0.000747,0.000514,0.000749,2.9e-05,2.9e-05,0.000882,0.00072,0.00066,2.9e-05,2.9e-05,0.000693,0.000672,0.00068,0.000649,0.000848,0.000677,0.000676,0.000698,0.000693,0.000758,0.000682,0.000699,0.00073,0.000641,0.000641,0.000641,0.00068,0.000637,0.000672,0.000637,0.00068,0.000678,0.000678,0.000704,0.000722,0.000637,0.000701,0.000658,0.000637,0.000637,0.000907,0.000638,0.000637,0.000646,0.000637,0.000722,0.000637,0.000679,0.000572,0.000601,0.000605,0.000605,0.000605,0.000605,0.000605,0.000544,0.000607,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001437,0.001325,0.00155,0.0011,0.001029,0.000982,0.000962,0.00102,0.001085,0.001078,0.001193,0.001037,0.001031,0.00096,0.00106,0.000929,0.001133,0.001316,0.000874,0.000978,0.000978,0.000846,0.00098,0.001009,0.000864,0.001015,0.001016,0.001221,0.001052,0.001131,0.001185,0.000916,0.00105,0.000822,0.000934,0.001067,0.001082,0.000968,0.000892,0.00102,0.001127,0.001079,0.000985,0.001126,0.000943,0.000978,0.00083,0.000914,0.000914,0.000914,0.000914,0.000914,0.000946,0.001115,0.00103,0.000997,0.001122,0.001232,0.000893,0.001247,0.000951,0.00066,0.00097,0.000917,0.001081,0.00058,0.000672,0.00094,0.001072,0.000894,0.000892,0.000897,0.00094,0.000966,0.000917,0.001264,0.000935,0.001109,0.000684,0.000684,0.000684,0.000684,0.000905,0.000931,0.001091,0.001073,0.00111,0.000811,0.000954,0.0009,0.000897,0.001304,0.000903,0.000921,0.001233,0.000931,0.000657,0.000735,0.000898,0.00105,0.000786,0.000959,0.000774,0.000774,0.001266,0.00093,0.000887,0.000774,0.000784,0.000946,0.000882,0.001001,0.000962,0.001255,0.000914,0.000894,0.000904,0.000894,0.001034,0.001087,0.000906,0.001095,0.000968,0.000962,0.000968,0.000974,0.000962,0.000982,0.000973,0.000974,0.000959,0.000964,0.000913,0.001017,0.00089,0.00091,0.001101,0.000886,0.000897,0.00129,0.00088,0.000871,0.000865,0.000876,0.000988,0.001048,0.000966,0.001112,0.000771,0.000933,0.000959,0.000933,0.000959,0.000933,0.00098,0.000873,0.000935,0.000962,0.000959,0.000723,0.00085,0.000869,0.000855,0.000838,0.000669,0.000657,0.000813,0.000669,0.000669,0.000693,0.000698,0.000669,0.000696,0.000779,0.000802,0.000816,0.001065,0.000669,0.001065,0.000693,0.000922,0.000922,0.000867,0.00091,0.000702,0.000676,0.000833],[0.023997,0.02364,0.024346,0.023423,0.02344,0.023405,0.023235,0.023373,0.022337,0.023212,0.023691,0.023464,0.022259,0.022526,0.023218,0.02316,0.023461,0.023779,0.022388,0.02334,0.023408,0.022308,0.023432,0.021734,0.021431,0.023034,0.022407,0.023452,0.023379,0.023619,0.023661,0.023238,0.023429,0.022904,0.023387,0.022461,0.023282,0.022619,0.022545,0.023343,0.023376,0.023411,0.023192,0.023335,0.023192,0.023476,0.021476,0.022666,0.022666,0.022666,0.022666,0.022666,0.021508,0.0235,0.022222,0.023387,0.023604,0.022169,0.02248,0.023703,0.023782,0.021957,0.023244,0.023238,0.022664,0.005348,0.021553,0.023265,0.022918,0.023201,0.023265,0.023279,0.023282,0.0233,0.023308,0.023233,0.023265,0.023488,0.021765,0.021765,0.021765,0.021765,0.023279,0.022915,0.022923,0.023279,0.022994,0.021197,0.023262,0.023206,0.023317,0.021387,0.022103,0.023134,0.023571,0.022545,0.021197,0.02223,0.0233,0.0235,0.022156,0.022711,0.023051,0.023051,0.023233,0.023241,0.023247,0.023051,0.023111,0.023273,0.023256,0.02344,0.023276,0.023276,0.023282,0.023244,0.023235,0.023235,0.02325,0.023595,0.023238,0.023556,0.023399,0.023379,0.023399,0.023384,0.023379,0.023387,0.023393,0.023126,0.023367,0.023384,0.02327,0.023262,0.023279,0.023308,0.023589,0.023238,0.023282,0.023227,0.023279,0.023233,0.023221,0.023227,0.023031,0.023661,0.02339,0.02358,0.022332,0.023106,0.023367,0.023106,0.023367,0.023106,0.023265,0.021671,0.023661,0.023923,0.023914,0.021701,0.022041,0.023432,0.023746,0.023688,0.021916,0.022022,0.023376,0.021916,0.021916,0.022642,0.022655,0.021916,0.022642,0.023077,0.022814,0.022845,0.023408,0.021916,0.023408,0.022642,0.02355,0.02355,0.023338,0.023529,0.022633,0.021968,0.023544],[0.172862,0.168937,0.182711,0.155129,0.13547,0.129526,0.132009,0.139117,0.140909,0.122208,0.140166,0.121331,0.149158,0.144747,0.145654,0.127747,0.158974,0.163588,0.124832,0.122853,0.128988,0.121015,0.130343,0.145312,0.124415,0.130435,0.131635,0.158433,0.149398,0.140483,0.136664,0.131542,0.142202,0.097433,0.089294,0.121015,0.136765,0.138702,0.131171,0.143519,0.159794,0.140377,0.14059,0.129888,0.132009,0.13071,0.111645,0.127923,0.127923,0.127923,0.127923,0.127923,0.133621,0.129346,0.145086,0.137472,0.133813,0.158703,0.11032,0.14059,0.005348,0.095876,0.133717,0.128542,0.129077,0.005376,0.005405,0.133047,0.133525,0.118926,0.118926,0.127572,0.128542,0.13229,0.121967,0.164894,0.121252,0.149879,0.100704,0.100704,0.100704,0.100704,0.120779,0.101528,0.142638,0.148919,0.123506,0.1156,0.136364,0.125506,0.123016,0.175803,0.12766,0.112591,0.161739,0.12141,0.086071,0.089294,0.120077,0.139013,0.09563,0.139222,0.005348,0.005348,0.164021,0.134006,0.122772,0.005348,0.005348,0.128809,0.125,0.126531,0.120779,0.157761,0.125846,0.125761,0.129888,0.128898,0.141016,0.126876,0.129979,0.135866,0.119307,0.119307,0.119307,0.126445,0.118471,0.125,0.118471,0.126445,0.126187,0.126187,0.130986,0.134296,0.118471,0.130343,0.122368,0.118471,0.118471,0.168631,0.118698,0.118471,0.120155,0.118471,0.134296,0.118471,0.126273,0.106346,0.111712,0.112455,0.112455,0.112455,0.112455,0.112455,0.101252,0.112933,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.267241,0.246358,0.288372,0.20462,0.191358,0.182711,0.179018,0.189796,0.201735,0.200431,0.221957,0.192946,0.191753,0.178503,0.197243,0.172862,0.210646,0.244737,0.162587,0.181818,0.181996,0.15736,0.182353,0.187689,0.160761,0.188832,0.189024,0.227106,0.195584,0.210407,0.220379,0.17033,0.195378,0.152961,0.173669,0.198506,0.201299,0.180058,0.165923,0.189796,0.209696,0.200647,0.183251,0.209459,0.175306,0.181818,0.154357,0.170018,0.170018,0.170018,0.170018,0.170018,0.17597,0.207358,0.191555,0.185444,0.208754,0.229064,0.166071,0.23192,0.176806,0.122691,0.180407,0.170642,0.201081,0.107826,0.125,0.174812,0.199357,0.16622,0.165923,0.166816,0.174812,0.17971,0.170642,0.235145,0.173994,0.206208,0.127136,0.127136,0.127136,0.127136,0.168326,0.173184,0.202835,0.199571,0.206437,0.150852,0.177481,0.167417,0.166816,0.242503,0.168022,0.171271,0.229346,0.173184,0.122208,0.136765,0.167116,0.195378,0.146112,0.178332,0.143963,0.143963,0.235443,0.173023,0.164894,0.143963,0.145768,0.17597,0.164021,0.186186,0.178846,0.233375,0.170018,0.166369,0.168174,0.166369,0.192347,0.202174,0.168478,0.203724,0.180058,0.178846,0.180058,0.18111,0.178846,0.182711,0.180934,0.18111,0.178332,0.179364,0.169863,0.189217,0.165628,0.169245,0.204846,0.164748,0.166816,0.24,0.163732,0.162021,0.1609,0.163015,0.183794,0.194969,0.17971,0.206897,0.143408,0.173507,0.178332,0.173507,0.178332,0.173507,0.182353,0.162304,0.173994,0.178846,0.178332,0.13449,0.158163,0.161599,0.158974,0.155909,0.124415,0.122127,0.15122,0.124415,0.124415,0.128898,0.129888,0.124415,0.129526,0.14486,0.149158,0.151713,0.198083,0.124415,0.198083,0.128988,0.171429,0.171429,0.161318,0.169245,0.130526,0.125676,0.155],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var8_df_closseness, by=list(var8_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var8_df_closseness, by=list(var8_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-93a7b6a2d98d232e5f32" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-93a7b6a2d98d232e5f32">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00012\" data-max=\"0.000126\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000422\" data-max=\"0.000744\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"9.4e-05\" data-max=\"0.000323\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000879\" data-max=\"0.001069\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7.8e-05\" data-max=\"0.000161\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022343\" data-max=\"0.023472\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000103\" data-max=\"0.002069\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.078414\" data-max=\"0.138433\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.017502\" data-max=\"0.060057\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.163498\" data-max=\"0.198855\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.014524\" data-max=\"0.029961\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172406\" data-max=\"0.308329\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.029295\" data-max=\"0.135782\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[0.000126,0.000125,0.00012],[0,2e-06,1.1e-05],[0.000744,0.000645,0.000422],[9.4e-05,0.000205,0.000323],[0.001069,0.000985,0.000879],[7.8e-05,0.000141,0.000161],[0.023472,0.023304,0.022343],[0.000103,0.000282,0.002069],[0.138433,0.119983,0.078414],[0.017502,0.038049,0.060057],[0.198855,0.183294,0.163498],[0.014524,0.026159,0.029961],[0.308329,0.266869,0.172406],[0.029295,0.082605,0.135782]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var8_data.RData")
```

