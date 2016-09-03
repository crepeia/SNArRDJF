# SNA Closeness 32_ACESSO E CONFIANÇA
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 32_ACESSO E CONFIANÇA

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/acessoeconfianca_data.RData")
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
#acessoeconfianca<-simplify(acessoeconfianca) #Simplify
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
V(acessoeconfianca)$incloseness <- closeness(acessoeconfianca, mode = "in", weights = E(acessoeconfianca)$acessoeconfianca) %>% round(6)
V(acessoeconfianca)$outcloseness <- closeness(acessoeconfianca, mode = "out", weights = E(acessoeconfianca)$acessoeconfianca) %>% round(6)
V(acessoeconfianca)$totalcloseness <- closeness(acessoeconfianca, mode = "total", weights = E(acessoeconfianca)$acessoeconfianca) %>% round(4)
```

###Saving to Environment

```r
acessoeconfianca_incloseness<- closeness(acessoeconfianca, mode = "in", weights = E(acessoeconfianca)$acessoeconfianca) %>% round(6)
acessoeconfianca_outcloseness<- closeness(acessoeconfianca, mode = "out", weights = E(acessoeconfianca)$acessoeconfianca) %>% round(6)
acessoeconfianca_totalcloseness<- closeness(acessoeconfianca, mode = "total", weights = E(acessoeconfianca)$acessoeconfianca) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(acessoeconfianca_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 2.900e-05 4.150e-05 4.900e-05 4.594e-05 5.000e-05 6.700e-05
```

```r
sd(acessoeconfianca_incloseness)
```

```
## [1] 6.929892e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(acessoeconfianca)$incloseness<-closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="in")

#Get Variable
V(acessoeconfianca)$acessoeconfianca_color_degree<-round(V(acessoeconfianca)$incloseness,6)

#Creating brewer pallette
vertex_acessoeconfianca_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(acessoeconfianca)$acessoeconfianca_color_degree)), "RdBu"))(
            length(unique(V(acessoeconfianca)$acessoeconfianca_color_degree)))

#Saving as Vertex properties 
V(acessoeconfianca)$vertex_acessoeconfianca_color_degree<-
  vertex_acessoeconfianca_color_degree[as.numeric(
  cut(V(acessoeconfianca)$acessoeconfianca_color_degree,
      breaks=length(unique(V(acessoeconfianca)$acessoeconfianca_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(acessoeconfianca, es=E(acessoeconfianca), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acessoeconfianca))
maxC <- rep(Inf, vcount(acessoeconfianca))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acessoeconfianca, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acessoeconfianca)$weight)


#PLotting
plot(acessoeconfianca, 
     layout=co,
     edge.color=V(acessoeconfianca)$vertex_acessoeconfianca_color_degree[edge.start],
     edge.arrow.size=closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="in"),
     edge.width=E(acessoeconfianca)$weight/mean(E(acessoeconfianca)$weight),
     edge.curved = TRUE,
     vertex.color=V(acessoeconfianca)$vertex_acessoeconfianca_color_degree,
     vertex.size=closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(acessoeconfianca,"LABEL_COR"),
     vertex.label.cex=(closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(acessoeconfianca)$acessoeconfianca_color_degree
b<-V(acessoeconfianca)$vertex_acessoeconfianca_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 32_ACESSO E CONFIANÇA", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(acessoeconfianca, mode="in", weights = E(acessoeconfianca)$acessoeconfianca)), 
             sd(closeness(acessoeconfianca, mode="in", weights = E(acessoeconfianca)$acessoeconfianca))
             )
       )
```

![](32_ACESSO_E_CONFIANÇA_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(acessoeconfianca_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 2.900e-05 4.050e-05 5.100e-05 4.811e-05 5.550e-05 7.900e-05
```

```r
sd(acessoeconfianca_outcloseness)
```

```
## [1] 1.157273e-05
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(acessoeconfianca)$outcloseness<-closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="out")

#Get Variable
V(acessoeconfianca)$acessoeconfianca_color_degree<-round(V(acessoeconfianca)$outcloseness,6)

#Creating brewer pallette
vertex_acessoeconfianca_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(acessoeconfianca)$acessoeconfianca_color_degree)), "RdBu"))(
            length(unique(V(acessoeconfianca)$acessoeconfianca_color_degree)))

#Saving as Vertex properties 
V(acessoeconfianca)$vertex_acessoeconfianca_color_degree<-
  vertex_acessoeconfianca_color_degree[as.numeric(
  cut(V(acessoeconfianca)$acessoeconfianca_color_degree,
      breaks=length(unique(V(acessoeconfianca)$acessoeconfianca_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(acessoeconfianca, es=E(acessoeconfianca), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acessoeconfianca))
maxC <- rep(Inf, vcount(acessoeconfianca))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acessoeconfianca, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acessoeconfianca)$weight)


#PLotting
plot(acessoeconfianca, 
     layout=co,
     edge.color=V(acessoeconfianca)$vertex_acessoeconfianca_color_degree[edge.start],
     edge.arrow.size=closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="out"),
     edge.width=E(acessoeconfianca)$weight/2*mean(E(acessoeconfianca)$weight),
     edge.curved = TRUE,
     vertex.color=V(acessoeconfianca)$vertex_acessoeconfianca_color_degree,
     vertex.size=closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(acessoeconfianca,"LABEL_COR"),
     vertex.label.cex=closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(acessoeconfianca)$acessoeconfianca_color_degree
b<-V(acessoeconfianca)$vertex_acessoeconfianca_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 32_ACESSO E CONFIANÇA", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(acessoeconfianca, mode="out", weights = E(acessoeconfianca)$acessoeconfianca)), 
             sd(closeness(acessoeconfianca, mode="out", weights = E(acessoeconfianca)$acessoeconfianca))
             )
       )
```

![](32_ACESSO_E_CONFIANÇA_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(acessoeconfianca_totalcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 3.700e-05 5.700e-05 6.300e-05 6.302e-05 6.800e-05 9.700e-05
```

```r
sd(acessoeconfianca_totalcloseness)
```

```
## [1] 1.119282e-05
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(acessoeconfianca)$allcloseness<-closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="all")

#Get Variable
V(acessoeconfianca)$acessoeconfianca_color_degree<-round(V(acessoeconfianca)$allcloseness,6)

#Creating brewer pallette
vertex_acessoeconfianca_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(acessoeconfianca)$acessoeconfianca_color_degree)), "RdBu"))(
            length(unique(V(acessoeconfianca)$acessoeconfianca_color_degree)))

#Saving as Vertex properties 
V(acessoeconfianca)$vertex_acessoeconfianca_color_degree<-
  vertex_acessoeconfianca_color_degree[as.numeric(
  cut(V(acessoeconfianca)$acessoeconfianca_color_degree,
      breaks=length(unique(V(acessoeconfianca)$acessoeconfianca_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(acessoeconfianca, es=E(acessoeconfianca), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acessoeconfianca))
maxC <- rep(Inf, vcount(acessoeconfianca))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acessoeconfianca, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acessoeconfianca)$weight)


#PLotting
plot(acessoeconfianca, 
     layout=co,
     edge.color=V(acessoeconfianca)$vertex_acessoeconfianca_color_degree[edge.start],
     edge.arrow.size=closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="all"),
     edge.width=E(acessoeconfianca)$weight/2*mean(E(acessoeconfianca)$weight),
     edge.curved = TRUE,
     vertex.color=V(acessoeconfianca)$vertex_acessoeconfianca_color_degree,
     vertex.size=closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(acessoeconfianca,"LABEL_COR"),
     vertex.label.cex=(closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(acessoeconfianca)$acessoeconfianca_color_degree
b<-V(acessoeconfianca)$vertex_acessoeconfianca_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 32_ACESSO E CONFIANÇA", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(acessoeconfianca, mode="all", weights = E(acessoeconfianca)$acessoeconfianca)), 
             sd(closeness(acessoeconfianca, mode="all", weights = E(acessoeconfianca)$acessoeconfianca))
             )
       )
```

![](32_ACESSO_E_CONFIANÇA_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(acessoeconfianca)$incloseness_n <- closeness(acessoeconfianca, mode = "in",, weights = E(acessoeconfianca)$acessoeconfianca, normalized = T) %>% round(10)
V(acessoeconfianca)$outcloseness_n <- closeness(acessoeconfianca, mode = "out", normalized = T, weights = E(acessoeconfianca)$acessoeconfianca) %>% round(6)
V(acessoeconfianca)$totalcloseness_n <- closeness(acessoeconfianca, mode = "total", normalized = T, weights = E(acessoeconfianca)$acessoeconfianca) %>% round(6)
```

###Saving to Environment

```r
acessoeconfianca_incloseness_n<- closeness(acessoeconfianca, mode = "in", normalized = T, weights = E(acessoeconfianca)$acessoeconfianca) %>% round(6)
acessoeconfianca_outcloseness_n<- closeness(acessoeconfianca, mode = "out", normalized = T, weights = E(acessoeconfianca)$acessoeconfianca) %>% round(6)
acessoeconfianca_totalcloseness_n<- closeness(acessoeconfianca, mode = "total", normalized = T, weights = E(acessoeconfianca)$acessoeconfianca) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(acessoeconfianca_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.007649 0.009155 0.008550 0.009368 0.012500
```

```r
sd(acessoeconfianca_incloseness_n)
```

```
## [1] 0.00128527
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(acessoeconfianca)$incloseness_n<-closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="in", normalized = T)

#Get Variable
V(acessoeconfianca)$acessoeconfianca_color_degree<-round(V(acessoeconfianca)$incloseness_n,6)

#Creating brewer pallette
vertex_acessoeconfianca_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(acessoeconfianca)$acessoeconfianca_color_degree)), "RdBu"))(
            length(unique(V(acessoeconfianca)$acessoeconfianca_color_degree)))

#Saving as Vertex properties 
V(acessoeconfianca)$vertex_acessoeconfianca_color_degree<-
  vertex_acessoeconfianca_color_degree[as.numeric(
  cut(V(acessoeconfianca)$acessoeconfianca_color_degree,
      breaks=length(unique(V(acessoeconfianca)$acessoeconfianca_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(acessoeconfianca, es=E(acessoeconfianca), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acessoeconfianca))
maxC <- rep(Inf, vcount(acessoeconfianca))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acessoeconfianca, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acessoeconfianca)$weight)


#PLotting
plot(acessoeconfianca, 
     layout=co,
     edge.color=V(acessoeconfianca)$vertex_acessoeconfianca_color_degree[edge.start],
     edge.arrow.size=closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="in",normalized = T),
     edge.width=E(acessoeconfianca)$weight/10*mean(E(acessoeconfianca)$weight),
     edge.curved = TRUE,
     vertex.color=V(acessoeconfianca)$vertex_acessoeconfianca_color_degree,
     vertex.size=(closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(acessoeconfianca,"LABEL_COR"),
     vertex.label.cex=closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(acessoeconfianca)$acessoeconfianca_color_degree
b<-V(acessoeconfianca)$vertex_acessoeconfianca_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 32_ACESSO E CONFIANÇA", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(acessoeconfianca, mode="in", weights = E(acessoeconfianca)$acessoeconfianca, normalized = T)), 
             sd(closeness(acessoeconfianca, mode="in", weights = E(acessoeconfianca)$acessoeconfianca, normalized = T))
             )
       )
```

![](32_ACESSO_E_CONFIANÇA_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(acessoeconfianca_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.007535 0.009461 0.008942 0.010330 0.014680
```

```r
sd(acessoeconfianca_outcloseness_n)
```

```
## [1] 0.002166893
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(acessoeconfianca)$outcloseness_n<-closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="out", normalized = T)

#Get Variable
V(acessoeconfianca)$acessoeconfianca_color_degree<-round(V(acessoeconfianca)$outcloseness_n,6)

#Creating brewer pallette
vertex_acessoeconfianca_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(acessoeconfianca)$acessoeconfianca_color_degree)), "RdBu"))(
            length(unique(V(acessoeconfianca)$acessoeconfianca_color_degree)))

#Saving as Vertex properties 
V(acessoeconfianca)$vertex_acessoeconfianca_color_degree<-
  vertex_acessoeconfianca_color_degree[as.numeric(
  cut(V(acessoeconfianca)$acessoeconfianca_color_degree,
      breaks=length(unique(V(acessoeconfianca)$acessoeconfianca_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(acessoeconfianca, es=E(acessoeconfianca), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acessoeconfianca))
maxC <- rep(Inf, vcount(acessoeconfianca))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acessoeconfianca, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acessoeconfianca)$weight)


#PLotting
plot(acessoeconfianca, 
     layout=co,
     edge.color=V(acessoeconfianca)$vertex_acessoeconfianca_color_degree[edge.start],
     edge.arrow.size=closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="out",normalized = T),
     edge.width=E(acessoeconfianca)$weight/10*mean(E(acessoeconfianca)$weight),
     edge.curved = TRUE,
     vertex.color=V(acessoeconfianca)$vertex_acessoeconfianca_color_degree,
     vertex.size=(closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(acessoeconfianca,"LABEL_COR"),
     vertex.label.cex=closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(acessoeconfianca)$acessoeconfianca_color_degree
b<-V(acessoeconfianca)$vertex_acessoeconfianca_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 32_ACESSO E CONFIANÇA", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(acessoeconfianca, mode="out", weights = E(acessoeconfianca)$acessoeconfianca, normalized = T)), 
             sd(closeness(acessoeconfianca, mode="out", weights = E(acessoeconfianca)$acessoeconfianca, normalized = T))
             )
       )
```

![](32_ACESSO_E_CONFIANÇA_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(acessoeconfianca_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.00682 0.01067 0.01171 0.01173 0.01271 0.01808
```

```r
sd(acessoeconfianca_totalcloseness_n)
```

```
## [1] 0.002086136
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(acessoeconfianca)$allcloseness_n<-closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="all", normalized = T)

#Get Variable
V(acessoeconfianca)$acessoeconfianca_color_degree<-round(V(acessoeconfianca)$allcloseness_n,6)

#Creating brewer pallette
vertex_acessoeconfianca_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(acessoeconfianca)$acessoeconfianca_color_degree)), "RdBu"))(
            length(unique(V(acessoeconfianca)$acessoeconfianca_color_degree)))

#Saving as Vertex properties 
V(acessoeconfianca)$vertex_acessoeconfianca_color_degree<-
  vertex_acessoeconfianca_color_degree[as.numeric(
  cut(V(acessoeconfianca)$acessoeconfianca_color_degree,
      breaks=length(unique(V(acessoeconfianca)$acessoeconfianca_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(acessoeconfianca, es=E(acessoeconfianca), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acessoeconfianca))
maxC <- rep(Inf, vcount(acessoeconfianca))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acessoeconfianca, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acessoeconfianca)$weight)


#PLotting
plot(acessoeconfianca, 
     layout=co,
     edge.color=V(acessoeconfianca)$vertex_acessoeconfianca_color_degree[edge.start],
     edge.arrow.size=closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="all",normalized = T),
     edge.width=E(acessoeconfianca)$weight/10*mean(E(acessoeconfianca)$weight),
     edge.curved = TRUE,
     vertex.color=V(acessoeconfianca)$vertex_acessoeconfianca_color_degree,
     vertex.size=(closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(acessoeconfianca,"LABEL_COR"),
     vertex.label.cex=closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(acessoeconfianca)$acessoeconfianca_color_degree
b<-V(acessoeconfianca)$vertex_acessoeconfianca_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 32_ACESSO E CONFIANÇA", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(acessoeconfianca, mode="all", weights = E(acessoeconfianca)$acessoeconfianca, normalized = T)), 
             sd(closeness(acessoeconfianca, mode="all", weights = E(acessoeconfianca)$acessoeconfianca, normalized = T))
             )
       )
```

![](32_ACESSO_E_CONFIANÇA_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(acessoeconfianca)$incloseness_n <- closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode = "in", normalized = T) %>% round(6)
V(acessoeconfianca)$outcloseness_n <- closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode = "out", normalized = T) %>% round(6)
V(acessoeconfianca)$totalcloseness_n <- closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(acessoeconfianca)$acessoeconfianca_centr_closeness<- centralization.closeness(acessoeconfianca)$res
acessoeconfianca_centr_closeness<- centralization.closeness(acessoeconfianca)$res
acessoeconfianca_centr_closeness_all<- centralization.closeness(acessoeconfianca)
```

###Centralization

```r
acessoeconfianca_centr_closeness_all$centralization
```

```
## [1] 0.15777
```

###Theoretical Max

```r
acessoeconfianca_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(acessoeconfianca)$acessoeconfianca_centr_closeness<- centralization.closeness(acessoeconfianca)$res

#Get Variable
V(acessoeconfianca)$acessoeconfianca_color_degree<-round(V(acessoeconfianca)$acessoeconfianca_centr_closeness,6)

#Creating brewer pallette
vertex_acessoeconfianca_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(acessoeconfianca)$acessoeconfianca_color_degree)), "Spectral"))(
            length(unique(V(acessoeconfianca)$acessoeconfianca_color_degree)))

#Saving as Vertex properties 
V(acessoeconfianca)$vertex_acessoeconfianca_color_degree<-
  vertex_acessoeconfianca_color_degree[as.numeric(
  cut(V(acessoeconfianca)$acessoeconfianca_color_degree,
      breaks=length(unique(V(acessoeconfianca)$acessoeconfianca_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(acessoeconfianca, es=E(acessoeconfianca), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acessoeconfianca))
maxC <- rep(Inf, vcount(acessoeconfianca))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acessoeconfianca, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acessoeconfianca)$weight)


#PLotting
plot(acessoeconfianca, 
     layout=co,
     edge.color=V(acessoeconfianca)$vertex_acessoeconfianca_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(acessoeconfianca)$res,
     edge.width=E(acessoeconfianca)$weight/10*mean(E(acessoeconfianca)$weight),
     edge.curved = TRUE,
     vertex.color=V(acessoeconfianca)$vertex_acessoeconfianca_color_degree,
     vertex.size=centralization.closeness(acessoeconfianca)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(acessoeconfianca,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(acessoeconfianca)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(acessoeconfianca)$acessoeconfianca_color_degree
b<-V(acessoeconfianca)$vertex_acessoeconfianca_color_degree
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
  title("Network Centralization Closeness - 32_ACESSO E CONFIANÇA", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(acessoeconfianca)$res), 
             sd(centralization.closeness(acessoeconfianca)$res)
             )
       )
```

![](32_ACESSO_E_CONFIANÇA_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
acessoeconfianca_incloseness<- closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode = "in") %>% round(6)
acessoeconfianca_outcloseness<- closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode = "out") %>% round(6)
acessoeconfianca_totalcloseness<- closeness(acessoeconfianca, weights = E(acessoeconfianca)$acessoeconfianca, mode = "total") %>% round(6)
acessoeconfianca_incloseness_n<- closeness(acessoeconfianca,weights = E(acessoeconfianca)$acessoeconfianca, mode = "in", normalized = T) %>% round(6)
acessoeconfianca_outcloseness_n<- closeness(acessoeconfianca,weights = E(acessoeconfianca)$acessoeconfianca, mode = "out", normalized = T) %>% round(6)
acessoeconfianca_totalcloseness_n<- closeness(acessoeconfianca,weights = E(acessoeconfianca)$acessoeconfianca, mode = "total", normalized = T) %>% round(6)
acessoeconfianca_centr_closeness <- centralization.closeness(acessoeconfianca)$res %>% round(6)
```

##Creating a datagrame of measures

```r
acessoeconfianca_df_closseness <- data.frame(
acessoeconfianca_incloseness,
acessoeconfianca_outcloseness,
acessoeconfianca_totalcloseness,
acessoeconfianca_incloseness_n,
acessoeconfianca_outcloseness_n,
acessoeconfianca_totalcloseness_n,
acessoeconfianca_centr_closeness) %>% round(6)

#Adding type
acessoeconfianca_df_closseness <-cbind(acessoeconfianca_df_closseness, V(acessoeconfianca)$LABEL_COR)

#Adding names
names(acessoeconfianca_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
acessoeconfianca_df_closseness<-acessoeconfianca_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(acessoeconfianca_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-3a34e62c916bd74a9328" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-3a34e62c916bd74a9328">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"6.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"7.9e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.7e-05\" data-max=\"9.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.012505\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.014675\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00682\" data-max=\"0.018078\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[6.1e-05,5.4e-05,6.7e-05,5.2e-05,5.3e-05,5.2e-05,4.8e-05,5.3e-05,4.3e-05,5.3e-05,5.3e-05,5.4e-05,4e-05,5.2e-05,4.9e-05,4.9e-05,5.4e-05,5.6e-05,4.1e-05,4.9e-05,5.2e-05,4.4e-05,5.2e-05,3.4e-05,5.4e-05,4.9e-05,3.7e-05,5.2e-05,5.1e-05,5.2e-05,5.2e-05,4.9e-05,5.3e-05,3.8e-05,5.1e-05,4.4e-05,5.2e-05,5.2e-05,4.2e-05,5.1e-05,5.2e-05,4.9e-05,4.8e-05,5.3e-05,4.8e-05,5.2e-05,3.1e-05,4.3e-05,4.3e-05,4.3e-05,4.3e-05,4.3e-05,3.6e-05,4.8e-05,3.8e-05,5.1e-05,5.1e-05,4.3e-05,3.9e-05,5.5e-05,5.1e-05,3.5e-05,4.9e-05,4.9e-05,4.4e-05,2.9e-05,3e-05,5e-05,5.8e-05,4.9e-05,5e-05,5e-05,5e-05,5.1e-05,5.1e-05,4.9e-05,5e-05,5.4e-05,3.4e-05,3.4e-05,3.4e-05,3.4e-05,5e-05,4.4e-05,4.5e-05,5e-05,4.8e-05,3.1e-05,5e-05,4.9e-05,4.9e-05,3.8e-05,3.6e-05,4.7e-05,5.1e-05,4.4e-05,3.1e-05,4.2e-05,5.1e-05,5.2e-05,3.5e-05,4.3e-05,4.2e-05,4.2e-05,4.9e-05,4.9e-05,4.9e-05,4.2e-05,4.2e-05,5e-05,4.9e-05,5.1e-05,5e-05,5e-05,5e-05,4.9e-05,4.9e-05,4.9e-05,5e-05,5.1e-05,4.9e-05,5e-05,5e-05,5e-05,5e-05,5e-05,5e-05,5e-05,5e-05,4.6e-05,5e-05,5e-05,5e-05,5e-05,5e-05,5.1e-05,5e-05,4.9e-05,5e-05,4.9e-05,5e-05,4.9e-05,4.9e-05,4.9e-05,4.6e-05,5.4e-05,5e-05,5.1e-05,4.4e-05,4.6e-05,5e-05,4.6e-05,5e-05,4.6e-05,5e-05,3.8e-05,4.7e-05,5e-05,5e-05,3.2e-05,3.4e-05,4e-05,4e-05,4e-05,3.1e-05,3e-05,3.9e-05,3.1e-05,3.1e-05,4e-05,3.8e-05,3.1e-05,4e-05,3.9e-05,4.2e-05,4.2e-05,4e-05,3.1e-05,4e-05,3.7e-05,4e-05,4e-05,4e-05,4e-05,3.7e-05,3.4e-05,4.6e-05],[7.9e-05,6.8e-05,7.8e-05,6.7e-05,5.8e-05,5.5e-05,5.3e-05,6.6e-05,5.9e-05,5.1e-05,5.2e-05,5e-05,6.3e-05,5.7e-05,5.9e-05,5.2e-05,6.8e-05,6.4e-05,5.2e-05,5.5e-05,5.7e-05,5.6e-05,5.9e-05,6.1e-05,5.2e-05,3.9e-05,5.1e-05,5.5e-05,6.4e-05,6e-05,6.2e-05,5.4e-05,6.6e-05,3.8e-05,5e-05,5.7e-05,5.5e-05,4.8e-05,5.2e-05,6.7e-05,6.3e-05,6.4e-05,6.4e-05,5.4e-05,5.8e-05,5.9e-05,4.2e-05,5.3e-05,5.3e-05,5.3e-05,5.3e-05,5.2e-05,5.1e-05,4.9e-05,6.1e-05,6.5e-05,5.4e-05,5.1e-05,4.4e-05,5.7e-05,2.9e-05,3.7e-05,6e-05,5.3e-05,5.7e-05,2.9e-05,2.9e-05,5.8e-05,5e-05,5.1e-05,4.9e-05,5e-05,5.7e-05,5.2e-05,4.8e-05,5.2e-05,5.2e-05,6e-05,3.7e-05,3.7e-05,3.7e-05,3.7e-05,5.3e-05,3.9e-05,5.4e-05,5.5e-05,5.5e-05,4.9e-05,5.6e-05,4.7e-05,5.6e-05,5.6e-05,5.3e-05,4.2e-05,5.7e-05,5.4e-05,3.4e-05,3.8e-05,4.8e-05,6.3e-05,3.8e-05,5.9e-05,5.1e-05,5.1e-05,5.4e-05,5.6e-05,5e-05,4.9e-05,4.9e-05,5.4e-05,5.8e-05,5.4e-05,5.2e-05,5.5e-05,5.3e-05,6.1e-05,5.5e-05,5.4e-05,5.5e-05,5.6e-05,5.8e-05,6.1e-05,4.6e-05,4.6e-05,4.6e-05,4.9e-05,4.6e-05,4.9e-05,4.6e-05,4.8e-05,5.4e-05,5.2e-05,5.2e-05,4.9e-05,4.9e-05,5.1e-05,4.5e-05,4.6e-05,5e-05,5.2e-05,4.7e-05,4.5e-05,5e-05,4.7e-05,4.7e-05,4.4e-05,4.9e-05,3.4e-05,4.2e-05,4.3e-05,4.3e-05,4.3e-05,4.3e-05,4.3e-05,3.7e-05,4.3e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[9.7e-05,8.1e-05,9.5e-05,7.9e-05,8.5e-05,8.6e-05,6.1e-05,7.7e-05,6.6e-05,6.7e-05,7e-05,6.9e-05,6.8e-05,7.8e-05,7.2e-05,6.2e-05,8.7e-05,7.8e-05,6.1e-05,6.8e-05,6.9e-05,6.4e-05,7.1e-05,6.5e-05,7e-05,6e-05,6.2e-05,7e-05,7.1e-05,7.2e-05,7.1e-05,6.3e-05,7.8e-05,5.2e-05,7.1e-05,6.6e-05,6.9e-05,7.5e-05,5.9e-05,8.9e-05,7.1e-05,7.1e-05,7.1e-05,6.8e-05,8.6e-05,7.3e-05,5.9e-05,6.1e-05,6.1e-05,6.1e-05,6.1e-05,6.1e-05,5.6e-05,6.5e-05,6.8e-05,7.7e-05,6.4e-05,6.3e-05,5.4e-05,7.4e-05,6.4e-05,4.5e-05,6.8e-05,7e-05,6.4e-05,3.7e-05,4.3e-05,6.5e-05,8.9e-05,6.1e-05,6.4e-05,6.2e-05,6.8e-05,6.6e-05,6.5e-05,6.4e-05,6.5e-05,7e-05,4.2e-05,4.2e-05,4.2e-05,4.2e-05,7e-05,6.2e-05,6.7e-05,6.3e-05,6.8e-05,5.5e-05,6.5e-05,6.2e-05,6.5e-05,6.2e-05,6e-05,6e-05,8.6e-05,6.7e-05,4.2e-05,5.3e-05,6.3e-05,7.4e-05,4.8e-05,8.6e-05,6.7e-05,6.7e-05,6.2e-05,8.6e-05,8.5e-05,8.5e-05,8.5e-05,6.6e-05,6.5e-05,6.4e-05,6.4e-05,6.6e-05,6.3e-05,7.1e-05,6.3e-05,6.3e-05,6.4e-05,6.6e-05,6.5e-05,7.1e-05,6.1e-05,6e-05,6.1e-05,6.2e-05,6.1e-05,6.2e-05,6.3e-05,6.2e-05,6.1e-05,6.2e-05,6.2e-05,6.2e-05,6.2e-05,6.4e-05,6.3e-05,6.2e-05,6.5e-05,6.3e-05,6.2e-05,6.2e-05,6.3e-05,6.1e-05,5.3e-05,6.7e-05,6.4e-05,6.4e-05,5.6e-05,5.7e-05,5.9e-05,5.7e-05,5.9e-05,5.7e-05,6.6e-05,5.7e-05,5.9e-05,6.1e-05,5.9e-05,4.7e-05,4.3e-05,5.2e-05,5.3e-05,5.2e-05,4.4e-05,4.3e-05,4.9e-05,4.4e-05,4.4e-05,5e-05,4.7e-05,4.4e-05,5e-05,4.9e-05,5.3e-05,5.3e-05,5.1e-05,4.4e-05,5.1e-05,4.6e-05,5.2e-05,5.2e-05,5.2e-05,5.5e-05,4.7e-05,4.2e-05,5.2e-05],[0.011297,0.010061,0.012505,0.009593,0.009786,0.00968,0.008853,0.009861,0.007927,0.009868,0.009844,0.009995,0.007482,0.009717,0.00914,0.009107,0.009982,0.010503,0.007576,0.009171,0.009633,0.008093,0.009649,0.006236,0.009987,0.009076,0.006896,0.009691,0.009547,0.00964,0.009754,0.009155,0.009872,0.006982,0.00953,0.008199,0.00963,0.009584,0.007745,0.0095,0.009678,0.009041,0.008929,0.009879,0.00886,0.009725,0.005853,0.008046,0.008046,0.008046,0.008046,0.008046,0.006725,0.008883,0.007063,0.009558,0.009452,0.00792,0.007167,0.010168,0.009465,0.006436,0.009199,0.00916,0.008254,0.005348,0.005553,0.009221,0.010859,0.009075,0.009319,0.009315,0.009342,0.009396,0.009464,0.009175,0.009239,0.009978,0.006384,0.006384,0.006384,0.006384,0.009234,0.008238,0.008443,0.009359,0.008985,0.005824,0.009318,0.009094,0.009172,0.006977,0.006686,0.008786,0.009467,0.008237,0.005824,0.007857,0.009406,0.009668,0.006556,0.007968,0.007746,0.007746,0.009154,0.009185,0.009178,0.007763,0.007882,0.009239,0.009192,0.009478,0.009268,0.009374,0.009362,0.009181,0.009175,0.009177,0.009239,0.009518,0.009155,0.009352,0.009321,0.009255,0.009321,0.009279,0.009258,0.009282,0.009338,0.008642,0.009223,0.009275,0.009293,0.009262,0.009328,0.009567,0.009384,0.009181,0.009343,0.009146,0.009246,0.009165,0.009126,0.009144,0.008499,0.010083,0.009369,0.00943,0.008252,0.008587,0.009227,0.008587,0.009227,0.008587,0.009367,0.0071,0.008679,0.0093,0.009273,0.005916,0.006282,0.007416,0.007516,0.007413,0.005849,0.005577,0.007293,0.005849,0.005849,0.007435,0.006998,0.005849,0.007384,0.007344,0.007722,0.007752,0.007442,0.005849,0.007443,0.006823,0.007395,0.007395,0.007532,0.007525,0.00687,0.006363,0.008502],[0.014675,0.012659,0.014454,0.012487,0.010878,0.010265,0.009845,0.012247,0.011067,0.009461,0.009644,0.009258,0.011704,0.010613,0.011023,0.009665,0.012714,0.011888,0.009652,0.010228,0.01066,0.010423,0.010926,0.011268,0.009703,0.00725,0.00945,0.01032,0.011919,0.011143,0.011443,0.009975,0.012367,0.007119,0.009281,0.010655,0.010153,0.009007,0.009755,0.012379,0.011757,0.011968,0.011962,0.010134,0.010728,0.011,0.007778,0.009867,0.009867,0.009867,0.009867,0.00972,0.009401,0.009113,0.011317,0.012014,0.010012,0.009531,0.008141,0.010605,0.005348,0.00697,0.011211,0.009943,0.01058,0.005369,0.005391,0.01086,0.009335,0.0095,0.009074,0.009316,0.010684,0.009671,0.009011,0.009649,0.009611,0.011087,0.006812,0.006812,0.006812,0.006812,0.00981,0.00734,0.009992,0.010322,0.010264,0.009078,0.010372,0.008826,0.010344,0.010401,0.009853,0.00773,0.010577,0.010104,0.006344,0.007128,0.008999,0.01178,0.007067,0.0109,0.009471,0.009471,0.010046,0.010406,0.009232,0.009144,0.009144,0.010055,0.010701,0.010026,0.009761,0.010178,0.009849,0.011378,0.010272,0.010061,0.010191,0.010362,0.010806,0.011354,0.008528,0.008528,0.008528,0.009112,0.008646,0.009139,0.008646,0.008871,0.00997,0.009644,0.009648,0.009034,0.009199,0.009513,0.008358,0.008572,0.009284,0.009682,0.008755,0.008428,0.009226,0.008721,0.008796,0.008154,0.009204,0.006255,0.007733,0.008031,0.008031,0.008031,0.008031,0.008031,0.006897,0.007951,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.018078,0.015094,0.017741,0.014602,0.015858,0.015993,0.011344,0.014377,0.012233,0.012441,0.012945,0.012826,0.012597,0.014416,0.013353,0.011531,0.016219,0.014493,0.011424,0.012701,0.012788,0.011952,0.013125,0.012167,0.013011,0.011214,0.011506,0.012976,0.013124,0.013384,0.01313,0.011693,0.01452,0.009763,0.013124,0.012302,0.012909,0.01396,0.010986,0.01652,0.013203,0.013221,0.013251,0.012739,0.015907,0.013568,0.010919,0.011438,0.011438,0.011438,0.011438,0.01134,0.010458,0.012074,0.01266,0.014329,0.011917,0.011768,0.010065,0.013832,0.01185,0.008391,0.012721,0.012936,0.011951,0.00682,0.00796,0.012171,0.016615,0.011433,0.011871,0.011506,0.01265,0.012238,0.012024,0.011888,0.012115,0.013055,0.007829,0.007829,0.007829,0.007829,0.012962,0.011592,0.012382,0.011734,0.012685,0.010214,0.012079,0.01146,0.012091,0.011478,0.011249,0.011136,0.015957,0.012466,0.007821,0.009895,0.011804,0.01378,0.00884,0.015989,0.012535,0.012535,0.011592,0.015937,0.015893,0.015838,0.015838,0.012276,0.012086,0.011918,0.011855,0.012218,0.011768,0.013272,0.011801,0.01174,0.011962,0.012268,0.01217,0.013217,0.011356,0.011099,0.011356,0.011512,0.011389,0.011578,0.011633,0.011456,0.011426,0.011556,0.011579,0.011569,0.011608,0.011979,0.011637,0.011475,0.012047,0.011714,0.011611,0.011611,0.011653,0.011405,0.009949,0.01237,0.011902,0.011912,0.01038,0.010669,0.011067,0.010669,0.011067,0.010669,0.012252,0.010658,0.011063,0.011353,0.011067,0.008777,0.00792,0.009652,0.009913,0.009706,0.008143,0.007909,0.009072,0.008143,0.008143,0.009335,0.008668,0.008143,0.009342,0.009122,0.009847,0.009912,0.009401,0.008143,0.009413,0.00857,0.00958,0.00958,0.009665,0.010178,0.0087,0.007786,0.009698],[0.372,0.343808,0.392405,0.336347,0.30897,0.301948,0.304918,0.319039,0.311558,0.288372,0.294304,0.286154,0.333932,0.322917,0.287481,0.29108,0.353612,0.315254,0.292913,0.286154,0.304419,0.285714,0.296178,0.333333,0.285714,0.240621,0.277198,0.319039,0.333333,0.316327,0.310518,0.2976,0.315789,0.224096,0.230198,0.285714,0.30897,0.268786,0.298555,0.32069,0.334532,0.317406,0.317406,0.293839,0.299035,0.300485,0.23192,0.291994,0.291994,0.291994,0.291994,0.291994,0.282675,0.298555,0.331551,0.32069,0.304419,0.288372,0.261972,0.324607,0.005348,0.228221,0.298555,0.291536,0.299517,0.005376,0.005405,0.307438,0.298555,0.283537,0.283537,0.294304,0.296651,0.296178,0.289269,0.290172,0.288372,0.314721,0.229346,0.229346,0.229346,0.229346,0.287926,0.250674,0.295238,0.299035,0.292453,0.274742,0.304419,0.286154,0.291536,0.302439,0.289269,0.24933,0.2976,0.288372,0.216028,0.223289,0.285714,0.307438,0.234552,0.318493,0.223022,0.223022,0.290625,0.303426,0.290172,0.231056,0.231056,0.297125,0.2976,0.28972,0.287037,0.285714,0.287037,0.291994,0.29477,0.295238,0.290172,0.290172,0.295238,0.311037,0.283537,0.283537,0.283537,0.291994,0.282675,0.285714,0.282675,0.291994,0.291536,0.291536,0.292453,0.282675,0.282675,0.298555,0.284839,0.282675,0.282675,0.291536,0.283105,0.282675,0.286154,0.282675,0.282675,0.282675,0.291536,0.221165,0.252374,0.256552,0.256552,0.256552,0.256552,0.256552,0.226553,0.227106,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(acessoeconfianca_df_closseness, by=list(acessoeconfianca_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(acessoeconfianca_df_closseness, by=list(acessoeconfianca_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-3d7a11cfa5690fd61662" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-3d7a11cfa5690fd61662">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.8e-05\" data-max=\"6.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"8e-06\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"7.9e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2e-06\" data-max=\"1.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.2e-05\" data-max=\"9.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.007042\" data-max=\"0.012505\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000131\" data-max=\"0.001439\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.014675\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000319\" data-max=\"0.002518\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.009615\" data-max=\"0.018078\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000182\" data-max=\"0.002671\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003897\" data-max=\"0.126\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[5e-05,3.8e-05,4.9e-05,5.4e-05,4.8e-05,5.5e-05,5.3e-05,6.7e-05,5.2e-05,5e-05,5.4e-05,4.2e-05,5e-05,5.2e-05,5.3e-05,4.8e-05,4.6e-05,5.2e-05,6.1e-05,4.4e-05,5.6e-05,4.9e-05,5e-05],[5e-06,5e-06,null,null,null,null,1e-06,null,null,null,null,8e-06,2e-06,1e-06,1e-06,6e-06,null,null,null,3e-06,null,2e-06,1e-06],[5.7e-05,3.5e-05,5.5e-05,6.8e-05,5.3e-05,5.7e-05,5.2e-05,7.8e-05,5.9e-05,4.9e-05,6e-05,5.1e-05,6e-05,5.9e-05,6e-05,5.5e-05,2.9e-05,6.3e-05,7.9e-05,5.1e-05,6.4e-05,4.4e-05,5.2e-05],[1.3e-05,9e-06,null,null,null,null,7e-06,null,null,null,null,1e-05,5e-06,7e-06,7e-06,8e-06,null,null,null,2e-06,null,8e-06,5e-06],[6.8e-05,5.2e-05,6.8e-05,8.1e-05,6.1e-05,7.4e-05,6.9e-05,9.5e-05,7.1e-05,6.4e-05,7e-05,6.1e-05,7.4e-05,7.3e-05,8.6e-05,7.2e-05,5.2e-05,7.4e-05,9.7e-05,6.7e-05,7.8e-05,6e-05,6.6e-05],[1.1e-05,8e-06,null,null,null,null,2e-06,null,null,null,null,1e-05,5e-06,3e-06,1e-06,1.4e-05,null,null,null,9e-06,null,2e-06,6e-06],[0.009267,0.007042,0.009171,0.010061,0.008853,0.010168,0.009802,0.012505,0.009649,0.009369,0.009978,0.00789,0.009379,0.009687,0.009816,0.008846,0.008502,0.009668,0.011297,0.00815,0.010503,0.009113,0.009258],[0.000885,0.000945,null,null,null,null,0.000245,null,null,null,null,0.001439,0.000478,0.000157,0.000153,0.00118,null,null,null,0.000583,null,0.00031,0.000131],[0.010619,0.006453,0.010228,0.012659,0.009845,0.010605,0.009711,0.014454,0.010926,0.009204,0.011087,0.009403,0.01129,0.01091,0.011286,0.010249,0.005348,0.01178,0.014675,0.00957,0.011888,0.008202,0.009688],[0.002518,0.001752,null,null,null,null,0.001359,null,null,null,null,0.001861,0.000958,0.001228,0.001274,0.001531,null,null,null,0.000319,null,0.001414,0.000886],[0.012701,0.009615,0.012701,0.015094,0.011344,0.013832,0.012711,0.017741,0.013125,0.011902,0.013055,0.011297,0.013819,0.013448,0.016023,0.013354,0.009698,0.01378,0.018078,0.012526,0.014493,0.011258,0.012232],[0.002115,0.001467,null,null,null,null,0.00031,null,null,null,null,0.001939,0.000845,0.00062,0.000182,0.002671,null,null,null,0.001745,null,0.000346,0.001067],[0.300898,0.099209,0.286154,0.343808,0.304918,0.324607,0.302044,0.392405,0.296178,0.291536,0.314721,0.267179,0.320162,0.303151,0.32151,0.297137,0.005348,0.307438,0.372,0.268879,0.315254,0.234593,0.28964],[0.043056,0.126,null,null,null,null,0.018298,null,null,null,null,0.070925,0.003897,0.032103,0.028022,0.024497,null,null,null,0.031783,null,0.102905,0.011874]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
acessoeconfianca_df_closseness <- data.frame(
acessoeconfianca_incloseness,
acessoeconfianca_outcloseness,
acessoeconfianca_totalcloseness,
acessoeconfianca_incloseness_n,
acessoeconfianca_outcloseness_n,
acessoeconfianca_totalcloseness_n,
acessoeconfianca_centr_closeness) %>% round(6)

#Adding type
acessoeconfianca_df_closseness <-cbind(acessoeconfianca_df_closseness, V(acessoeconfianca)$TIPO1)

#Adding names
names(acessoeconfianca_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
acessoeconfianca_df_closseness<-acessoeconfianca_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(acessoeconfianca_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-4f14283900684f2c2806" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-4f14283900684f2c2806">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"6.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"7.9e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.7e-05\" data-max=\"9.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.012505\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.014675\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00682\" data-max=\"0.018078\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[6.1e-05,5.4e-05,6.7e-05,5.2e-05,5.3e-05,5.2e-05,4.8e-05,5.3e-05,4.3e-05,5.3e-05,5.3e-05,5.4e-05,4e-05,5.2e-05,4.9e-05,4.9e-05,5.4e-05,5.6e-05,4.1e-05,4.9e-05,5.2e-05,4.4e-05,5.2e-05,3.4e-05,5.4e-05,4.9e-05,3.7e-05,5.2e-05,5.1e-05,5.2e-05,5.2e-05,4.9e-05,5.3e-05,3.8e-05,5.1e-05,4.4e-05,5.2e-05,5.2e-05,4.2e-05,5.1e-05,5.2e-05,4.9e-05,4.8e-05,5.3e-05,4.8e-05,5.2e-05,3.1e-05,4.3e-05,4.3e-05,4.3e-05,4.3e-05,4.3e-05,3.6e-05,4.8e-05,3.8e-05,5.1e-05,5.1e-05,4.3e-05,3.9e-05,5.5e-05,5.1e-05,3.5e-05,4.9e-05,4.9e-05,4.4e-05,2.9e-05,3e-05,5e-05,5.8e-05,4.9e-05,5e-05,5e-05,5e-05,5.1e-05,5.1e-05,4.9e-05,5e-05,5.4e-05,3.4e-05,3.4e-05,3.4e-05,3.4e-05,5e-05,4.4e-05,4.5e-05,5e-05,4.8e-05,3.1e-05,5e-05,4.9e-05,4.9e-05,3.8e-05,3.6e-05,4.7e-05,5.1e-05,4.4e-05,3.1e-05,4.2e-05,5.1e-05,5.2e-05,3.5e-05,4.3e-05,4.2e-05,4.2e-05,4.9e-05,4.9e-05,4.9e-05,4.2e-05,4.2e-05,5e-05,4.9e-05,5.1e-05,5e-05,5e-05,5e-05,4.9e-05,4.9e-05,4.9e-05,5e-05,5.1e-05,4.9e-05,5e-05,5e-05,5e-05,5e-05,5e-05,5e-05,5e-05,5e-05,4.6e-05,5e-05,5e-05,5e-05,5e-05,5e-05,5.1e-05,5e-05,4.9e-05,5e-05,4.9e-05,5e-05,4.9e-05,4.9e-05,4.9e-05,4.6e-05,5.4e-05,5e-05,5.1e-05,4.4e-05,4.6e-05,5e-05,4.6e-05,5e-05,4.6e-05,5e-05,3.8e-05,4.7e-05,5e-05,5e-05,3.2e-05,3.4e-05,4e-05,4e-05,4e-05,3.1e-05,3e-05,3.9e-05,3.1e-05,3.1e-05,4e-05,3.8e-05,3.1e-05,4e-05,3.9e-05,4.2e-05,4.2e-05,4e-05,3.1e-05,4e-05,3.7e-05,4e-05,4e-05,4e-05,4e-05,3.7e-05,3.4e-05,4.6e-05],[7.9e-05,6.8e-05,7.8e-05,6.7e-05,5.8e-05,5.5e-05,5.3e-05,6.6e-05,5.9e-05,5.1e-05,5.2e-05,5e-05,6.3e-05,5.7e-05,5.9e-05,5.2e-05,6.8e-05,6.4e-05,5.2e-05,5.5e-05,5.7e-05,5.6e-05,5.9e-05,6.1e-05,5.2e-05,3.9e-05,5.1e-05,5.5e-05,6.4e-05,6e-05,6.2e-05,5.4e-05,6.6e-05,3.8e-05,5e-05,5.7e-05,5.5e-05,4.8e-05,5.2e-05,6.7e-05,6.3e-05,6.4e-05,6.4e-05,5.4e-05,5.8e-05,5.9e-05,4.2e-05,5.3e-05,5.3e-05,5.3e-05,5.3e-05,5.2e-05,5.1e-05,4.9e-05,6.1e-05,6.5e-05,5.4e-05,5.1e-05,4.4e-05,5.7e-05,2.9e-05,3.7e-05,6e-05,5.3e-05,5.7e-05,2.9e-05,2.9e-05,5.8e-05,5e-05,5.1e-05,4.9e-05,5e-05,5.7e-05,5.2e-05,4.8e-05,5.2e-05,5.2e-05,6e-05,3.7e-05,3.7e-05,3.7e-05,3.7e-05,5.3e-05,3.9e-05,5.4e-05,5.5e-05,5.5e-05,4.9e-05,5.6e-05,4.7e-05,5.6e-05,5.6e-05,5.3e-05,4.2e-05,5.7e-05,5.4e-05,3.4e-05,3.8e-05,4.8e-05,6.3e-05,3.8e-05,5.9e-05,5.1e-05,5.1e-05,5.4e-05,5.6e-05,5e-05,4.9e-05,4.9e-05,5.4e-05,5.8e-05,5.4e-05,5.2e-05,5.5e-05,5.3e-05,6.1e-05,5.5e-05,5.4e-05,5.5e-05,5.6e-05,5.8e-05,6.1e-05,4.6e-05,4.6e-05,4.6e-05,4.9e-05,4.6e-05,4.9e-05,4.6e-05,4.8e-05,5.4e-05,5.2e-05,5.2e-05,4.9e-05,4.9e-05,5.1e-05,4.5e-05,4.6e-05,5e-05,5.2e-05,4.7e-05,4.5e-05,5e-05,4.7e-05,4.7e-05,4.4e-05,4.9e-05,3.4e-05,4.2e-05,4.3e-05,4.3e-05,4.3e-05,4.3e-05,4.3e-05,3.7e-05,4.3e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[9.7e-05,8.1e-05,9.5e-05,7.9e-05,8.5e-05,8.6e-05,6.1e-05,7.7e-05,6.6e-05,6.7e-05,7e-05,6.9e-05,6.8e-05,7.8e-05,7.2e-05,6.2e-05,8.7e-05,7.8e-05,6.1e-05,6.8e-05,6.9e-05,6.4e-05,7.1e-05,6.5e-05,7e-05,6e-05,6.2e-05,7e-05,7.1e-05,7.2e-05,7.1e-05,6.3e-05,7.8e-05,5.2e-05,7.1e-05,6.6e-05,6.9e-05,7.5e-05,5.9e-05,8.9e-05,7.1e-05,7.1e-05,7.1e-05,6.8e-05,8.6e-05,7.3e-05,5.9e-05,6.1e-05,6.1e-05,6.1e-05,6.1e-05,6.1e-05,5.6e-05,6.5e-05,6.8e-05,7.7e-05,6.4e-05,6.3e-05,5.4e-05,7.4e-05,6.4e-05,4.5e-05,6.8e-05,7e-05,6.4e-05,3.7e-05,4.3e-05,6.5e-05,8.9e-05,6.1e-05,6.4e-05,6.2e-05,6.8e-05,6.6e-05,6.5e-05,6.4e-05,6.5e-05,7e-05,4.2e-05,4.2e-05,4.2e-05,4.2e-05,7e-05,6.2e-05,6.7e-05,6.3e-05,6.8e-05,5.5e-05,6.5e-05,6.2e-05,6.5e-05,6.2e-05,6e-05,6e-05,8.6e-05,6.7e-05,4.2e-05,5.3e-05,6.3e-05,7.4e-05,4.8e-05,8.6e-05,6.7e-05,6.7e-05,6.2e-05,8.6e-05,8.5e-05,8.5e-05,8.5e-05,6.6e-05,6.5e-05,6.4e-05,6.4e-05,6.6e-05,6.3e-05,7.1e-05,6.3e-05,6.3e-05,6.4e-05,6.6e-05,6.5e-05,7.1e-05,6.1e-05,6e-05,6.1e-05,6.2e-05,6.1e-05,6.2e-05,6.3e-05,6.2e-05,6.1e-05,6.2e-05,6.2e-05,6.2e-05,6.2e-05,6.4e-05,6.3e-05,6.2e-05,6.5e-05,6.3e-05,6.2e-05,6.2e-05,6.3e-05,6.1e-05,5.3e-05,6.7e-05,6.4e-05,6.4e-05,5.6e-05,5.7e-05,5.9e-05,5.7e-05,5.9e-05,5.7e-05,6.6e-05,5.7e-05,5.9e-05,6.1e-05,5.9e-05,4.7e-05,4.3e-05,5.2e-05,5.3e-05,5.2e-05,4.4e-05,4.3e-05,4.9e-05,4.4e-05,4.4e-05,5e-05,4.7e-05,4.4e-05,5e-05,4.9e-05,5.3e-05,5.3e-05,5.1e-05,4.4e-05,5.1e-05,4.6e-05,5.2e-05,5.2e-05,5.2e-05,5.5e-05,4.7e-05,4.2e-05,5.2e-05],[0.011297,0.010061,0.012505,0.009593,0.009786,0.00968,0.008853,0.009861,0.007927,0.009868,0.009844,0.009995,0.007482,0.009717,0.00914,0.009107,0.009982,0.010503,0.007576,0.009171,0.009633,0.008093,0.009649,0.006236,0.009987,0.009076,0.006896,0.009691,0.009547,0.00964,0.009754,0.009155,0.009872,0.006982,0.00953,0.008199,0.00963,0.009584,0.007745,0.0095,0.009678,0.009041,0.008929,0.009879,0.00886,0.009725,0.005853,0.008046,0.008046,0.008046,0.008046,0.008046,0.006725,0.008883,0.007063,0.009558,0.009452,0.00792,0.007167,0.010168,0.009465,0.006436,0.009199,0.00916,0.008254,0.005348,0.005553,0.009221,0.010859,0.009075,0.009319,0.009315,0.009342,0.009396,0.009464,0.009175,0.009239,0.009978,0.006384,0.006384,0.006384,0.006384,0.009234,0.008238,0.008443,0.009359,0.008985,0.005824,0.009318,0.009094,0.009172,0.006977,0.006686,0.008786,0.009467,0.008237,0.005824,0.007857,0.009406,0.009668,0.006556,0.007968,0.007746,0.007746,0.009154,0.009185,0.009178,0.007763,0.007882,0.009239,0.009192,0.009478,0.009268,0.009374,0.009362,0.009181,0.009175,0.009177,0.009239,0.009518,0.009155,0.009352,0.009321,0.009255,0.009321,0.009279,0.009258,0.009282,0.009338,0.008642,0.009223,0.009275,0.009293,0.009262,0.009328,0.009567,0.009384,0.009181,0.009343,0.009146,0.009246,0.009165,0.009126,0.009144,0.008499,0.010083,0.009369,0.00943,0.008252,0.008587,0.009227,0.008587,0.009227,0.008587,0.009367,0.0071,0.008679,0.0093,0.009273,0.005916,0.006282,0.007416,0.007516,0.007413,0.005849,0.005577,0.007293,0.005849,0.005849,0.007435,0.006998,0.005849,0.007384,0.007344,0.007722,0.007752,0.007442,0.005849,0.007443,0.006823,0.007395,0.007395,0.007532,0.007525,0.00687,0.006363,0.008502],[0.014675,0.012659,0.014454,0.012487,0.010878,0.010265,0.009845,0.012247,0.011067,0.009461,0.009644,0.009258,0.011704,0.010613,0.011023,0.009665,0.012714,0.011888,0.009652,0.010228,0.01066,0.010423,0.010926,0.011268,0.009703,0.00725,0.00945,0.01032,0.011919,0.011143,0.011443,0.009975,0.012367,0.007119,0.009281,0.010655,0.010153,0.009007,0.009755,0.012379,0.011757,0.011968,0.011962,0.010134,0.010728,0.011,0.007778,0.009867,0.009867,0.009867,0.009867,0.00972,0.009401,0.009113,0.011317,0.012014,0.010012,0.009531,0.008141,0.010605,0.005348,0.00697,0.011211,0.009943,0.01058,0.005369,0.005391,0.01086,0.009335,0.0095,0.009074,0.009316,0.010684,0.009671,0.009011,0.009649,0.009611,0.011087,0.006812,0.006812,0.006812,0.006812,0.00981,0.00734,0.009992,0.010322,0.010264,0.009078,0.010372,0.008826,0.010344,0.010401,0.009853,0.00773,0.010577,0.010104,0.006344,0.007128,0.008999,0.01178,0.007067,0.0109,0.009471,0.009471,0.010046,0.010406,0.009232,0.009144,0.009144,0.010055,0.010701,0.010026,0.009761,0.010178,0.009849,0.011378,0.010272,0.010061,0.010191,0.010362,0.010806,0.011354,0.008528,0.008528,0.008528,0.009112,0.008646,0.009139,0.008646,0.008871,0.00997,0.009644,0.009648,0.009034,0.009199,0.009513,0.008358,0.008572,0.009284,0.009682,0.008755,0.008428,0.009226,0.008721,0.008796,0.008154,0.009204,0.006255,0.007733,0.008031,0.008031,0.008031,0.008031,0.008031,0.006897,0.007951,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.018078,0.015094,0.017741,0.014602,0.015858,0.015993,0.011344,0.014377,0.012233,0.012441,0.012945,0.012826,0.012597,0.014416,0.013353,0.011531,0.016219,0.014493,0.011424,0.012701,0.012788,0.011952,0.013125,0.012167,0.013011,0.011214,0.011506,0.012976,0.013124,0.013384,0.01313,0.011693,0.01452,0.009763,0.013124,0.012302,0.012909,0.01396,0.010986,0.01652,0.013203,0.013221,0.013251,0.012739,0.015907,0.013568,0.010919,0.011438,0.011438,0.011438,0.011438,0.01134,0.010458,0.012074,0.01266,0.014329,0.011917,0.011768,0.010065,0.013832,0.01185,0.008391,0.012721,0.012936,0.011951,0.00682,0.00796,0.012171,0.016615,0.011433,0.011871,0.011506,0.01265,0.012238,0.012024,0.011888,0.012115,0.013055,0.007829,0.007829,0.007829,0.007829,0.012962,0.011592,0.012382,0.011734,0.012685,0.010214,0.012079,0.01146,0.012091,0.011478,0.011249,0.011136,0.015957,0.012466,0.007821,0.009895,0.011804,0.01378,0.00884,0.015989,0.012535,0.012535,0.011592,0.015937,0.015893,0.015838,0.015838,0.012276,0.012086,0.011918,0.011855,0.012218,0.011768,0.013272,0.011801,0.01174,0.011962,0.012268,0.01217,0.013217,0.011356,0.011099,0.011356,0.011512,0.011389,0.011578,0.011633,0.011456,0.011426,0.011556,0.011579,0.011569,0.011608,0.011979,0.011637,0.011475,0.012047,0.011714,0.011611,0.011611,0.011653,0.011405,0.009949,0.01237,0.011902,0.011912,0.01038,0.010669,0.011067,0.010669,0.011067,0.010669,0.012252,0.010658,0.011063,0.011353,0.011067,0.008777,0.00792,0.009652,0.009913,0.009706,0.008143,0.007909,0.009072,0.008143,0.008143,0.009335,0.008668,0.008143,0.009342,0.009122,0.009847,0.009912,0.009401,0.008143,0.009413,0.00857,0.00958,0.00958,0.009665,0.010178,0.0087,0.007786,0.009698],[0.372,0.343808,0.392405,0.336347,0.30897,0.301948,0.304918,0.319039,0.311558,0.288372,0.294304,0.286154,0.333932,0.322917,0.287481,0.29108,0.353612,0.315254,0.292913,0.286154,0.304419,0.285714,0.296178,0.333333,0.285714,0.240621,0.277198,0.319039,0.333333,0.316327,0.310518,0.2976,0.315789,0.224096,0.230198,0.285714,0.30897,0.268786,0.298555,0.32069,0.334532,0.317406,0.317406,0.293839,0.299035,0.300485,0.23192,0.291994,0.291994,0.291994,0.291994,0.291994,0.282675,0.298555,0.331551,0.32069,0.304419,0.288372,0.261972,0.324607,0.005348,0.228221,0.298555,0.291536,0.299517,0.005376,0.005405,0.307438,0.298555,0.283537,0.283537,0.294304,0.296651,0.296178,0.289269,0.290172,0.288372,0.314721,0.229346,0.229346,0.229346,0.229346,0.287926,0.250674,0.295238,0.299035,0.292453,0.274742,0.304419,0.286154,0.291536,0.302439,0.289269,0.24933,0.2976,0.288372,0.216028,0.223289,0.285714,0.307438,0.234552,0.318493,0.223022,0.223022,0.290625,0.303426,0.290172,0.231056,0.231056,0.297125,0.2976,0.28972,0.287037,0.285714,0.287037,0.291994,0.29477,0.295238,0.290172,0.290172,0.295238,0.311037,0.283537,0.283537,0.283537,0.291994,0.282675,0.285714,0.282675,0.291994,0.291536,0.291536,0.292453,0.282675,0.282675,0.298555,0.284839,0.282675,0.282675,0.291536,0.283105,0.282675,0.286154,0.282675,0.282675,0.282675,0.291536,0.221165,0.252374,0.256552,0.256552,0.256552,0.256552,0.256552,0.226553,0.227106,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(acessoeconfianca_df_closseness, by=list(acessoeconfianca_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(acessoeconfianca_df_closseness, by=list(acessoeconfianca_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-a8f2a43873bfb8ed0f73" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-a8f2a43873bfb8ed0f73">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.1e-05\" data-max=\"5e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4e-06\" data-max=\"7e-06\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.2e-05\" data-max=\"5.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8e-06\" data-max=\"1.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.7e-05\" data-max=\"6.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8e-06\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.007585\" data-max=\"0.009255\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000649\" data-max=\"0.001315\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.007792\" data-max=\"0.009784\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001564\" data-max=\"0.00235\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.010646\" data-max=\"0.012518\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001551\" data-max=\"0.002243\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.174245\" data-max=\"0.281723\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.060225\" data-max=\"0.135845\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[5e-05,4.1e-05],[4e-06,7e-06],[5.3e-05,4.2e-05],[8e-06,1.3e-05],[6.7e-05,5.7e-05],[8e-06,1.2e-05],[0.009255,0.007585],[0.000649,0.001315],[0.009784,0.007792],[0.001564,0.00235],[0.012518,0.010646],[0.001551,0.002243],[0.281723,0.174245],[0.060225,0.135845]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
acessoeconfianca_df_closseness <- data.frame(
acessoeconfianca_incloseness,
acessoeconfianca_outcloseness,
acessoeconfianca_totalcloseness,
acessoeconfianca_incloseness_n,
acessoeconfianca_outcloseness_n,
acessoeconfianca_totalcloseness_n,
acessoeconfianca_centr_closeness) %>% round(6)

#Adding type
acessoeconfianca_df_closseness <-cbind(acessoeconfianca_df_closseness, V(acessoeconfianca)$TIPO2)

#Adding names
names(acessoeconfianca_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
acessoeconfianca_df_closseness<-acessoeconfianca_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(acessoeconfianca_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-198611e6a9ac077c147c" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-198611e6a9ac077c147c">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"6.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"7.9e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.7e-05\" data-max=\"9.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.012505\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.014675\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00682\" data-max=\"0.018078\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[6.1e-05,5.4e-05,6.7e-05,5.2e-05,5.3e-05,5.2e-05,4.8e-05,5.3e-05,4.3e-05,5.3e-05,5.3e-05,5.4e-05,4e-05,5.2e-05,4.9e-05,4.9e-05,5.4e-05,5.6e-05,4.1e-05,4.9e-05,5.2e-05,4.4e-05,5.2e-05,3.4e-05,5.4e-05,4.9e-05,3.7e-05,5.2e-05,5.1e-05,5.2e-05,5.2e-05,4.9e-05,5.3e-05,3.8e-05,5.1e-05,4.4e-05,5.2e-05,5.2e-05,4.2e-05,5.1e-05,5.2e-05,4.9e-05,4.8e-05,5.3e-05,4.8e-05,5.2e-05,3.1e-05,4.3e-05,4.3e-05,4.3e-05,4.3e-05,4.3e-05,3.6e-05,4.8e-05,3.8e-05,5.1e-05,5.1e-05,4.3e-05,3.9e-05,5.5e-05,5.1e-05,3.5e-05,4.9e-05,4.9e-05,4.4e-05,2.9e-05,3e-05,5e-05,5.8e-05,4.9e-05,5e-05,5e-05,5e-05,5.1e-05,5.1e-05,4.9e-05,5e-05,5.4e-05,3.4e-05,3.4e-05,3.4e-05,3.4e-05,5e-05,4.4e-05,4.5e-05,5e-05,4.8e-05,3.1e-05,5e-05,4.9e-05,4.9e-05,3.8e-05,3.6e-05,4.7e-05,5.1e-05,4.4e-05,3.1e-05,4.2e-05,5.1e-05,5.2e-05,3.5e-05,4.3e-05,4.2e-05,4.2e-05,4.9e-05,4.9e-05,4.9e-05,4.2e-05,4.2e-05,5e-05,4.9e-05,5.1e-05,5e-05,5e-05,5e-05,4.9e-05,4.9e-05,4.9e-05,5e-05,5.1e-05,4.9e-05,5e-05,5e-05,5e-05,5e-05,5e-05,5e-05,5e-05,5e-05,4.6e-05,5e-05,5e-05,5e-05,5e-05,5e-05,5.1e-05,5e-05,4.9e-05,5e-05,4.9e-05,5e-05,4.9e-05,4.9e-05,4.9e-05,4.6e-05,5.4e-05,5e-05,5.1e-05,4.4e-05,4.6e-05,5e-05,4.6e-05,5e-05,4.6e-05,5e-05,3.8e-05,4.7e-05,5e-05,5e-05,3.2e-05,3.4e-05,4e-05,4e-05,4e-05,3.1e-05,3e-05,3.9e-05,3.1e-05,3.1e-05,4e-05,3.8e-05,3.1e-05,4e-05,3.9e-05,4.2e-05,4.2e-05,4e-05,3.1e-05,4e-05,3.7e-05,4e-05,4e-05,4e-05,4e-05,3.7e-05,3.4e-05,4.6e-05],[7.9e-05,6.8e-05,7.8e-05,6.7e-05,5.8e-05,5.5e-05,5.3e-05,6.6e-05,5.9e-05,5.1e-05,5.2e-05,5e-05,6.3e-05,5.7e-05,5.9e-05,5.2e-05,6.8e-05,6.4e-05,5.2e-05,5.5e-05,5.7e-05,5.6e-05,5.9e-05,6.1e-05,5.2e-05,3.9e-05,5.1e-05,5.5e-05,6.4e-05,6e-05,6.2e-05,5.4e-05,6.6e-05,3.8e-05,5e-05,5.7e-05,5.5e-05,4.8e-05,5.2e-05,6.7e-05,6.3e-05,6.4e-05,6.4e-05,5.4e-05,5.8e-05,5.9e-05,4.2e-05,5.3e-05,5.3e-05,5.3e-05,5.3e-05,5.2e-05,5.1e-05,4.9e-05,6.1e-05,6.5e-05,5.4e-05,5.1e-05,4.4e-05,5.7e-05,2.9e-05,3.7e-05,6e-05,5.3e-05,5.7e-05,2.9e-05,2.9e-05,5.8e-05,5e-05,5.1e-05,4.9e-05,5e-05,5.7e-05,5.2e-05,4.8e-05,5.2e-05,5.2e-05,6e-05,3.7e-05,3.7e-05,3.7e-05,3.7e-05,5.3e-05,3.9e-05,5.4e-05,5.5e-05,5.5e-05,4.9e-05,5.6e-05,4.7e-05,5.6e-05,5.6e-05,5.3e-05,4.2e-05,5.7e-05,5.4e-05,3.4e-05,3.8e-05,4.8e-05,6.3e-05,3.8e-05,5.9e-05,5.1e-05,5.1e-05,5.4e-05,5.6e-05,5e-05,4.9e-05,4.9e-05,5.4e-05,5.8e-05,5.4e-05,5.2e-05,5.5e-05,5.3e-05,6.1e-05,5.5e-05,5.4e-05,5.5e-05,5.6e-05,5.8e-05,6.1e-05,4.6e-05,4.6e-05,4.6e-05,4.9e-05,4.6e-05,4.9e-05,4.6e-05,4.8e-05,5.4e-05,5.2e-05,5.2e-05,4.9e-05,4.9e-05,5.1e-05,4.5e-05,4.6e-05,5e-05,5.2e-05,4.7e-05,4.5e-05,5e-05,4.7e-05,4.7e-05,4.4e-05,4.9e-05,3.4e-05,4.2e-05,4.3e-05,4.3e-05,4.3e-05,4.3e-05,4.3e-05,3.7e-05,4.3e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[9.7e-05,8.1e-05,9.5e-05,7.9e-05,8.5e-05,8.6e-05,6.1e-05,7.7e-05,6.6e-05,6.7e-05,7e-05,6.9e-05,6.8e-05,7.8e-05,7.2e-05,6.2e-05,8.7e-05,7.8e-05,6.1e-05,6.8e-05,6.9e-05,6.4e-05,7.1e-05,6.5e-05,7e-05,6e-05,6.2e-05,7e-05,7.1e-05,7.2e-05,7.1e-05,6.3e-05,7.8e-05,5.2e-05,7.1e-05,6.6e-05,6.9e-05,7.5e-05,5.9e-05,8.9e-05,7.1e-05,7.1e-05,7.1e-05,6.8e-05,8.6e-05,7.3e-05,5.9e-05,6.1e-05,6.1e-05,6.1e-05,6.1e-05,6.1e-05,5.6e-05,6.5e-05,6.8e-05,7.7e-05,6.4e-05,6.3e-05,5.4e-05,7.4e-05,6.4e-05,4.5e-05,6.8e-05,7e-05,6.4e-05,3.7e-05,4.3e-05,6.5e-05,8.9e-05,6.1e-05,6.4e-05,6.2e-05,6.8e-05,6.6e-05,6.5e-05,6.4e-05,6.5e-05,7e-05,4.2e-05,4.2e-05,4.2e-05,4.2e-05,7e-05,6.2e-05,6.7e-05,6.3e-05,6.8e-05,5.5e-05,6.5e-05,6.2e-05,6.5e-05,6.2e-05,6e-05,6e-05,8.6e-05,6.7e-05,4.2e-05,5.3e-05,6.3e-05,7.4e-05,4.8e-05,8.6e-05,6.7e-05,6.7e-05,6.2e-05,8.6e-05,8.5e-05,8.5e-05,8.5e-05,6.6e-05,6.5e-05,6.4e-05,6.4e-05,6.6e-05,6.3e-05,7.1e-05,6.3e-05,6.3e-05,6.4e-05,6.6e-05,6.5e-05,7.1e-05,6.1e-05,6e-05,6.1e-05,6.2e-05,6.1e-05,6.2e-05,6.3e-05,6.2e-05,6.1e-05,6.2e-05,6.2e-05,6.2e-05,6.2e-05,6.4e-05,6.3e-05,6.2e-05,6.5e-05,6.3e-05,6.2e-05,6.2e-05,6.3e-05,6.1e-05,5.3e-05,6.7e-05,6.4e-05,6.4e-05,5.6e-05,5.7e-05,5.9e-05,5.7e-05,5.9e-05,5.7e-05,6.6e-05,5.7e-05,5.9e-05,6.1e-05,5.9e-05,4.7e-05,4.3e-05,5.2e-05,5.3e-05,5.2e-05,4.4e-05,4.3e-05,4.9e-05,4.4e-05,4.4e-05,5e-05,4.7e-05,4.4e-05,5e-05,4.9e-05,5.3e-05,5.3e-05,5.1e-05,4.4e-05,5.1e-05,4.6e-05,5.2e-05,5.2e-05,5.2e-05,5.5e-05,4.7e-05,4.2e-05,5.2e-05],[0.011297,0.010061,0.012505,0.009593,0.009786,0.00968,0.008853,0.009861,0.007927,0.009868,0.009844,0.009995,0.007482,0.009717,0.00914,0.009107,0.009982,0.010503,0.007576,0.009171,0.009633,0.008093,0.009649,0.006236,0.009987,0.009076,0.006896,0.009691,0.009547,0.00964,0.009754,0.009155,0.009872,0.006982,0.00953,0.008199,0.00963,0.009584,0.007745,0.0095,0.009678,0.009041,0.008929,0.009879,0.00886,0.009725,0.005853,0.008046,0.008046,0.008046,0.008046,0.008046,0.006725,0.008883,0.007063,0.009558,0.009452,0.00792,0.007167,0.010168,0.009465,0.006436,0.009199,0.00916,0.008254,0.005348,0.005553,0.009221,0.010859,0.009075,0.009319,0.009315,0.009342,0.009396,0.009464,0.009175,0.009239,0.009978,0.006384,0.006384,0.006384,0.006384,0.009234,0.008238,0.008443,0.009359,0.008985,0.005824,0.009318,0.009094,0.009172,0.006977,0.006686,0.008786,0.009467,0.008237,0.005824,0.007857,0.009406,0.009668,0.006556,0.007968,0.007746,0.007746,0.009154,0.009185,0.009178,0.007763,0.007882,0.009239,0.009192,0.009478,0.009268,0.009374,0.009362,0.009181,0.009175,0.009177,0.009239,0.009518,0.009155,0.009352,0.009321,0.009255,0.009321,0.009279,0.009258,0.009282,0.009338,0.008642,0.009223,0.009275,0.009293,0.009262,0.009328,0.009567,0.009384,0.009181,0.009343,0.009146,0.009246,0.009165,0.009126,0.009144,0.008499,0.010083,0.009369,0.00943,0.008252,0.008587,0.009227,0.008587,0.009227,0.008587,0.009367,0.0071,0.008679,0.0093,0.009273,0.005916,0.006282,0.007416,0.007516,0.007413,0.005849,0.005577,0.007293,0.005849,0.005849,0.007435,0.006998,0.005849,0.007384,0.007344,0.007722,0.007752,0.007442,0.005849,0.007443,0.006823,0.007395,0.007395,0.007532,0.007525,0.00687,0.006363,0.008502],[0.014675,0.012659,0.014454,0.012487,0.010878,0.010265,0.009845,0.012247,0.011067,0.009461,0.009644,0.009258,0.011704,0.010613,0.011023,0.009665,0.012714,0.011888,0.009652,0.010228,0.01066,0.010423,0.010926,0.011268,0.009703,0.00725,0.00945,0.01032,0.011919,0.011143,0.011443,0.009975,0.012367,0.007119,0.009281,0.010655,0.010153,0.009007,0.009755,0.012379,0.011757,0.011968,0.011962,0.010134,0.010728,0.011,0.007778,0.009867,0.009867,0.009867,0.009867,0.00972,0.009401,0.009113,0.011317,0.012014,0.010012,0.009531,0.008141,0.010605,0.005348,0.00697,0.011211,0.009943,0.01058,0.005369,0.005391,0.01086,0.009335,0.0095,0.009074,0.009316,0.010684,0.009671,0.009011,0.009649,0.009611,0.011087,0.006812,0.006812,0.006812,0.006812,0.00981,0.00734,0.009992,0.010322,0.010264,0.009078,0.010372,0.008826,0.010344,0.010401,0.009853,0.00773,0.010577,0.010104,0.006344,0.007128,0.008999,0.01178,0.007067,0.0109,0.009471,0.009471,0.010046,0.010406,0.009232,0.009144,0.009144,0.010055,0.010701,0.010026,0.009761,0.010178,0.009849,0.011378,0.010272,0.010061,0.010191,0.010362,0.010806,0.011354,0.008528,0.008528,0.008528,0.009112,0.008646,0.009139,0.008646,0.008871,0.00997,0.009644,0.009648,0.009034,0.009199,0.009513,0.008358,0.008572,0.009284,0.009682,0.008755,0.008428,0.009226,0.008721,0.008796,0.008154,0.009204,0.006255,0.007733,0.008031,0.008031,0.008031,0.008031,0.008031,0.006897,0.007951,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.018078,0.015094,0.017741,0.014602,0.015858,0.015993,0.011344,0.014377,0.012233,0.012441,0.012945,0.012826,0.012597,0.014416,0.013353,0.011531,0.016219,0.014493,0.011424,0.012701,0.012788,0.011952,0.013125,0.012167,0.013011,0.011214,0.011506,0.012976,0.013124,0.013384,0.01313,0.011693,0.01452,0.009763,0.013124,0.012302,0.012909,0.01396,0.010986,0.01652,0.013203,0.013221,0.013251,0.012739,0.015907,0.013568,0.010919,0.011438,0.011438,0.011438,0.011438,0.01134,0.010458,0.012074,0.01266,0.014329,0.011917,0.011768,0.010065,0.013832,0.01185,0.008391,0.012721,0.012936,0.011951,0.00682,0.00796,0.012171,0.016615,0.011433,0.011871,0.011506,0.01265,0.012238,0.012024,0.011888,0.012115,0.013055,0.007829,0.007829,0.007829,0.007829,0.012962,0.011592,0.012382,0.011734,0.012685,0.010214,0.012079,0.01146,0.012091,0.011478,0.011249,0.011136,0.015957,0.012466,0.007821,0.009895,0.011804,0.01378,0.00884,0.015989,0.012535,0.012535,0.011592,0.015937,0.015893,0.015838,0.015838,0.012276,0.012086,0.011918,0.011855,0.012218,0.011768,0.013272,0.011801,0.01174,0.011962,0.012268,0.01217,0.013217,0.011356,0.011099,0.011356,0.011512,0.011389,0.011578,0.011633,0.011456,0.011426,0.011556,0.011579,0.011569,0.011608,0.011979,0.011637,0.011475,0.012047,0.011714,0.011611,0.011611,0.011653,0.011405,0.009949,0.01237,0.011902,0.011912,0.01038,0.010669,0.011067,0.010669,0.011067,0.010669,0.012252,0.010658,0.011063,0.011353,0.011067,0.008777,0.00792,0.009652,0.009913,0.009706,0.008143,0.007909,0.009072,0.008143,0.008143,0.009335,0.008668,0.008143,0.009342,0.009122,0.009847,0.009912,0.009401,0.008143,0.009413,0.00857,0.00958,0.00958,0.009665,0.010178,0.0087,0.007786,0.009698],[0.372,0.343808,0.392405,0.336347,0.30897,0.301948,0.304918,0.319039,0.311558,0.288372,0.294304,0.286154,0.333932,0.322917,0.287481,0.29108,0.353612,0.315254,0.292913,0.286154,0.304419,0.285714,0.296178,0.333333,0.285714,0.240621,0.277198,0.319039,0.333333,0.316327,0.310518,0.2976,0.315789,0.224096,0.230198,0.285714,0.30897,0.268786,0.298555,0.32069,0.334532,0.317406,0.317406,0.293839,0.299035,0.300485,0.23192,0.291994,0.291994,0.291994,0.291994,0.291994,0.282675,0.298555,0.331551,0.32069,0.304419,0.288372,0.261972,0.324607,0.005348,0.228221,0.298555,0.291536,0.299517,0.005376,0.005405,0.307438,0.298555,0.283537,0.283537,0.294304,0.296651,0.296178,0.289269,0.290172,0.288372,0.314721,0.229346,0.229346,0.229346,0.229346,0.287926,0.250674,0.295238,0.299035,0.292453,0.274742,0.304419,0.286154,0.291536,0.302439,0.289269,0.24933,0.2976,0.288372,0.216028,0.223289,0.285714,0.307438,0.234552,0.318493,0.223022,0.223022,0.290625,0.303426,0.290172,0.231056,0.231056,0.297125,0.2976,0.28972,0.287037,0.285714,0.287037,0.291994,0.29477,0.295238,0.290172,0.290172,0.295238,0.311037,0.283537,0.283537,0.283537,0.291994,0.282675,0.285714,0.282675,0.291994,0.291536,0.291536,0.292453,0.282675,0.282675,0.298555,0.284839,0.282675,0.282675,0.291536,0.283105,0.282675,0.286154,0.282675,0.282675,0.282675,0.291536,0.221165,0.252374,0.256552,0.256552,0.256552,0.256552,0.256552,0.226553,0.227106,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(acessoeconfianca_df_closseness, by=list(acessoeconfianca_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(acessoeconfianca_df_closseness, by=list(acessoeconfianca_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-df30665349d00bce9f96" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-df30665349d00bce9f96">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.1e-05\" data-max=\"5.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"7e-06\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.2e-05\" data-max=\"6e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6e-06\" data-max=\"1.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.7e-05\" data-max=\"7.5e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6e-06\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.007553\" data-max=\"0.009744\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000161\" data-max=\"0.001295\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.007776\" data-max=\"0.01115\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001125\" data-max=\"0.002363\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.010624\" data-max=\"0.013992\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001194\" data-max=\"0.002244\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172869\" data-max=\"0.310529\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.028396\" data-max=\"0.136194\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[5.2e-05,4.9e-05,4.1e-05],[1e-06,4e-06,7e-06],[6e-05,5.1e-05,4.2e-05],[6e-06,8e-06,1.3e-05],[7.5e-05,6.6e-05,5.7e-05],[6e-06,8e-06,1.2e-05],[0.009744,0.009186,0.007553],[0.000161,0.000662,0.001295],[0.01115,0.009558,0.007776],[0.001125,0.001505,0.002363],[0.013992,0.012282,0.010624],[0.001194,0.00147,0.002244],[0.310529,0.277125,0.172869],[0.028396,0.062378,0.136194]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/acessoeconfianca_data.RData")
```

