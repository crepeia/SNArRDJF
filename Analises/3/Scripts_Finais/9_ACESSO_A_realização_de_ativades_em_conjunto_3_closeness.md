# SNA Closeness 9_ACESSO A) Este serviço mostra-se disponível para realização de ativades em conjunto. (var7)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 9_ACESSO A) Este serviço mostra-se disponível para realização de ativades em conjunto. (var7)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var7_data.RData")
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
#var7<-simplify(var7) #Simplify
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
V(var7)$incloseness <- closeness(var7, mode = "in", weights = E(var7)$var7) %>% round(6)
V(var7)$outcloseness <- closeness(var7, mode = "out", weights = E(var7)$var7) %>% round(6)
V(var7)$totalcloseness <- closeness(var7, mode = "total", weights = E(var7)$var7) %>% round(4)
```

###Saving to Environment

```r
var7_incloseness<- closeness(var7, mode = "in", weights = E(var7)$var7) %>% round(6)
var7_outcloseness<- closeness(var7, mode = "out", weights = E(var7)$var7) %>% round(6)
var7_totalcloseness<- closeness(var7, mode = "total", weights = E(var7)$var7) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var7_incloseness)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000029 0.000124 0.000126 0.000125 0.000127 0.000132
```

```r
sd(var7_incloseness)
```

```
## [1] 7.753794e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var7)$incloseness<-closeness(var7, weights = E(var7)$var7, mode="in")

#Get Variable
V(var7)$var7_color_degree<-round(V(var7)$incloseness,6)

#Creating brewer pallette
vertex_var7_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var7)$var7_color_degree)), "RdBu"))(
            length(unique(V(var7)$var7_color_degree)))

#Saving as Vertex properties 
V(var7)$vertex_var7_color_degree<-
  vertex_var7_color_degree[as.numeric(
  cut(V(var7)$var7_color_degree,
      breaks=length(unique(V(var7)$var7_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var7, es=E(var7), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var7))
maxC <- rep(Inf, vcount(var7))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var7, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var7)$weight)


#PLotting
plot(var7, 
     layout=co,
     edge.color=V(var7)$vertex_var7_color_degree[edge.start],
     edge.arrow.size=closeness(var7, weights = E(var7)$var7, mode="in"),
     edge.width=E(var7)$weight/mean(E(var7)$weight),
     edge.curved = TRUE,
     vertex.color=V(var7)$vertex_var7_color_degree,
     vertex.size=closeness(var7, weights = E(var7)$var7, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var7,"LABEL_COR"),
     vertex.label.cex=(closeness(var7, weights = E(var7)$var7, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var7)$var7_color_degree
b<-V(var7)$vertex_var7_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 9_ACESSO A) Este serviço mostra-se disponível para realização de ativades em conjunto. (var7)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var7, mode="in", weights = E(var7)$var7)), 
             sd(closeness(var7, mode="in", weights = E(var7)$var7))
             )
       )
```

![](9_ACESSO_A_realização_de_ativades_em_conjunto_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var7_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0005645 0.0007410 0.0006308 0.0008650 0.0010760
```

```r
sd(var7_outcloseness)
```

```
## [1] 0.0003259981
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var7)$outcloseness<-closeness(var7, weights = E(var7)$var7, mode="out")

#Get Variable
V(var7)$var7_color_degree<-round(V(var7)$outcloseness,6)

#Creating brewer pallette
vertex_var7_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var7)$var7_color_degree)), "RdBu"))(
            length(unique(V(var7)$var7_color_degree)))

#Saving as Vertex properties 
V(var7)$vertex_var7_color_degree<-
  vertex_var7_color_degree[as.numeric(
  cut(V(var7)$var7_color_degree,
      breaks=length(unique(V(var7)$var7_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var7, es=E(var7), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var7))
maxC <- rep(Inf, vcount(var7))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var7, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var7)$weight)


#PLotting
plot(var7, 
     layout=co,
     edge.color=V(var7)$vertex_var7_color_degree[edge.start],
     edge.arrow.size=closeness(var7, weights = E(var7)$var7, mode="out"),
     edge.width=E(var7)$weight/2*mean(E(var7)$weight),
     edge.curved = TRUE,
     vertex.color=V(var7)$vertex_var7_color_degree,
     vertex.size=closeness(var7, weights = E(var7)$var7, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var7,"LABEL_COR"),
     vertex.label.cex=closeness(var7, weights = E(var7)$var7, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var7)$var7_color_degree
b<-V(var7)$vertex_var7_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 9_ACESSO A) Este serviço mostra-se disponível para realização de ativades em conjunto. (var7)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var7, mode="out", weights = E(var7)$var7)), 
             sd(closeness(var7, mode="out", weights = E(var7)$var7))
             )
       )
```

![](9_ACESSO_A_realização_de_ativades_em_conjunto_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var7_totalcloseness)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000670 0.000967 0.001131 0.001150 0.001338 0.001805
```

```r
sd(var7_totalcloseness)
```

```
## [1] 0.0002561501
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var7)$allcloseness<-closeness(var7, weights = E(var7)$var7, mode="all")

#Get Variable
V(var7)$var7_color_degree<-round(V(var7)$allcloseness,6)

#Creating brewer pallette
vertex_var7_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var7)$var7_color_degree)), "RdBu"))(
            length(unique(V(var7)$var7_color_degree)))

#Saving as Vertex properties 
V(var7)$vertex_var7_color_degree<-
  vertex_var7_color_degree[as.numeric(
  cut(V(var7)$var7_color_degree,
      breaks=length(unique(V(var7)$var7_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var7, es=E(var7), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var7))
maxC <- rep(Inf, vcount(var7))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var7, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var7)$weight)


#PLotting
plot(var7, 
     layout=co,
     edge.color=V(var7)$vertex_var7_color_degree[edge.start],
     edge.arrow.size=closeness(var7, weights = E(var7)$var7, mode="all"),
     edge.width=E(var7)$weight/2*mean(E(var7)$weight),
     edge.curved = TRUE,
     vertex.color=V(var7)$vertex_var7_color_degree,
     vertex.size=closeness(var7, weights = E(var7)$var7, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var7,"LABEL_COR"),
     vertex.label.cex=(closeness(var7, weights = E(var7)$var7, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var7)$var7_color_degree
b<-V(var7)$vertex_var7_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 9_ACESSO A) Este serviço mostra-se disponível para realização de ativades em conjunto. (var7)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var7, mode="all", weights = E(var7)$var7)), 
             sd(closeness(var7, mode="all", weights = E(var7)$var7))
             )
       )
```

![](9_ACESSO_A_realização_de_ativades_em_conjunto_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var7)$incloseness_n <- closeness(var7, mode = "in",, weights = E(var7)$var7, normalized = T) %>% round(10)
V(var7)$outcloseness_n <- closeness(var7, mode = "out", normalized = T, weights = E(var7)$var7) %>% round(6)
V(var7)$totalcloseness_n <- closeness(var7, mode = "total", normalized = T, weights = E(var7)$var7) %>% round(6)
```

###Saving to Environment

```r
var7_incloseness_n<- closeness(var7, mode = "in", normalized = T, weights = E(var7)$var7) %>% round(6)
var7_outcloseness_n<- closeness(var7, mode = "out", normalized = T, weights = E(var7)$var7) %>% round(6)
var7_totalcloseness_n<- closeness(var7, mode = "total", normalized = T, weights = E(var7)$var7) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var7_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.023090 0.023520 0.023260 0.023680 0.024590
```

```r
sd(var7_incloseness_n)
```

```
## [1] 0.001445448
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var7)$incloseness_n<-closeness(var7, weights = E(var7)$var7, mode="in", normalized = T)

#Get Variable
V(var7)$var7_color_degree<-round(V(var7)$incloseness_n,6)

#Creating brewer pallette
vertex_var7_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var7)$var7_color_degree)), "RdBu"))(
            length(unique(V(var7)$var7_color_degree)))

#Saving as Vertex properties 
V(var7)$vertex_var7_color_degree<-
  vertex_var7_color_degree[as.numeric(
  cut(V(var7)$var7_color_degree,
      breaks=length(unique(V(var7)$var7_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var7, es=E(var7), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var7))
maxC <- rep(Inf, vcount(var7))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var7, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var7)$weight)


#PLotting
plot(var7, 
     layout=co,
     edge.color=V(var7)$vertex_var7_color_degree[edge.start],
     edge.arrow.size=closeness(var7, weights = E(var7)$var7, mode="in",normalized = T),
     edge.width=E(var7)$weight/10*mean(E(var7)$weight),
     edge.curved = TRUE,
     vertex.color=V(var7)$vertex_var7_color_degree,
     vertex.size=(closeness(var7, weights = E(var7)$var7, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var7,"LABEL_COR"),
     vertex.label.cex=closeness(var7, weights = E(var7)$var7, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var7)$var7_color_degree
b<-V(var7)$vertex_var7_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 9_ACESSO A) Este serviço mostra-se disponível para realização de ativades em conjunto. (var7)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var7, mode="in", weights = E(var7)$var7, normalized = T)), 
             sd(closeness(var7, mode="in", weights = E(var7)$var7, normalized = T))
             )
       )
```

![](9_ACESSO_A_realização_de_ativades_em_conjunto_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var7_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.104900 0.137900 0.117300 0.160800 0.200200
```

```r
sd(var7_outcloseness_n)
```

```
## [1] 0.06065538
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var7)$outcloseness_n<-closeness(var7, weights = E(var7)$var7, mode="out", normalized = T)

#Get Variable
V(var7)$var7_color_degree<-round(V(var7)$outcloseness_n,6)

#Creating brewer pallette
vertex_var7_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var7)$var7_color_degree)), "RdBu"))(
            length(unique(V(var7)$var7_color_degree)))

#Saving as Vertex properties 
V(var7)$vertex_var7_color_degree<-
  vertex_var7_color_degree[as.numeric(
  cut(V(var7)$var7_color_degree,
      breaks=length(unique(V(var7)$var7_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var7, es=E(var7), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var7))
maxC <- rep(Inf, vcount(var7))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var7, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var7)$weight)


#PLotting
plot(var7, 
     layout=co,
     edge.color=V(var7)$vertex_var7_color_degree[edge.start],
     edge.arrow.size=closeness(var7, weights = E(var7)$var7, mode="out",normalized = T),
     edge.width=E(var7)$weight/10*mean(E(var7)$weight),
     edge.curved = TRUE,
     vertex.color=V(var7)$vertex_var7_color_degree,
     vertex.size=(closeness(var7, weights = E(var7)$var7, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var7,"LABEL_COR"),
     vertex.label.cex=closeness(var7, weights = E(var7)$var7, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var7)$var7_color_degree
b<-V(var7)$vertex_var7_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 9_ACESSO A) Este serviço mostra-se disponível para realização de ativades em conjunto. (var7)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var7, mode="out", weights = E(var7)$var7, normalized = T)), 
             sd(closeness(var7, mode="out", weights = E(var7)$var7, normalized = T))
             )
       )
```

![](9_ACESSO_A_realização_de_ativades_em_conjunto_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var7_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1246  0.1799  0.2104  0.2140  0.2488  0.3357
```

```r
sd(var7_totalcloseness_n)
```

```
## [1] 0.04764298
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var7)$allcloseness_n<-closeness(var7, weights = E(var7)$var7, mode="all", normalized = T)

#Get Variable
V(var7)$var7_color_degree<-round(V(var7)$allcloseness_n,6)

#Creating brewer pallette
vertex_var7_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var7)$var7_color_degree)), "RdBu"))(
            length(unique(V(var7)$var7_color_degree)))

#Saving as Vertex properties 
V(var7)$vertex_var7_color_degree<-
  vertex_var7_color_degree[as.numeric(
  cut(V(var7)$var7_color_degree,
      breaks=length(unique(V(var7)$var7_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var7, es=E(var7), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var7))
maxC <- rep(Inf, vcount(var7))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var7, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var7)$weight)


#PLotting
plot(var7, 
     layout=co,
     edge.color=V(var7)$vertex_var7_color_degree[edge.start],
     edge.arrow.size=closeness(var7, weights = E(var7)$var7, mode="all",normalized = T),
     edge.width=E(var7)$weight/10*mean(E(var7)$weight),
     edge.curved = TRUE,
     vertex.color=V(var7)$vertex_var7_color_degree,
     vertex.size=(closeness(var7, weights = E(var7)$var7, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var7,"LABEL_COR"),
     vertex.label.cex=closeness(var7, weights = E(var7)$var7, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var7)$var7_color_degree
b<-V(var7)$vertex_var7_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 9_ACESSO A) Este serviço mostra-se disponível para realização de ativades em conjunto. (var7)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var7, mode="all", weights = E(var7)$var7, normalized = T)), 
             sd(closeness(var7, mode="all", weights = E(var7)$var7, normalized = T))
             )
       )
```

![](9_ACESSO_A_realização_de_ativades_em_conjunto_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var7)$incloseness_n <- closeness(var7, weights = E(var7)$var7, mode = "in", normalized = T) %>% round(6)
V(var7)$outcloseness_n <- closeness(var7, weights = E(var7)$var7, mode = "out", normalized = T) %>% round(6)
V(var7)$totalcloseness_n <- closeness(var7, weights = E(var7)$var7, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var7)$var7_centr_closeness<- centralization.closeness(var7)$res
var7_centr_closeness<- centralization.closeness(var7)$res
var7_centr_closeness_all<- centralization.closeness(var7)
```

###Centralization

```r
var7_centr_closeness_all$centralization
```

```
## [1] 0.1625197
```

###Theoretical Max

```r
var7_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var7)$var7_centr_closeness<- centralization.closeness(var7)$res

#Get Variable
V(var7)$var7_color_degree<-round(V(var7)$var7_centr_closeness,6)

#Creating brewer pallette
vertex_var7_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var7)$var7_color_degree)), "Spectral"))(
            length(unique(V(var7)$var7_color_degree)))

#Saving as Vertex properties 
V(var7)$vertex_var7_color_degree<-
  vertex_var7_color_degree[as.numeric(
  cut(V(var7)$var7_color_degree,
      breaks=length(unique(V(var7)$var7_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var7, es=E(var7), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var7))
maxC <- rep(Inf, vcount(var7))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var7, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var7)$weight)


#PLotting
plot(var7, 
     layout=co,
     edge.color=V(var7)$vertex_var7_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var7)$res,
     edge.width=E(var7)$weight/10*mean(E(var7)$weight),
     edge.curved = TRUE,
     vertex.color=V(var7)$vertex_var7_color_degree,
     vertex.size=centralization.closeness(var7)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var7,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var7)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var7)$var7_color_degree
b<-V(var7)$vertex_var7_color_degree
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
  title("Network Centralization Closeness - 9_ACESSO A) Este serviço mostra-se disponível para realização de ativades em conjunto. (var7)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var7)$res), 
             sd(centralization.closeness(var7)$res)
             )
       )
```

![](9_ACESSO_A_realização_de_ativades_em_conjunto_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var7_incloseness<- closeness(var7, weights = E(var7)$var7, mode = "in") %>% round(6)
var7_outcloseness<- closeness(var7, weights = E(var7)$var7, mode = "out") %>% round(6)
var7_totalcloseness<- closeness(var7, weights = E(var7)$var7, mode = "total") %>% round(6)
var7_incloseness_n<- closeness(var7,weights = E(var7)$var7, mode = "in", normalized = T) %>% round(6)
var7_outcloseness_n<- closeness(var7,weights = E(var7)$var7, mode = "out", normalized = T) %>% round(6)
var7_totalcloseness_n<- closeness(var7,weights = E(var7)$var7, mode = "total", normalized = T) %>% round(6)
var7_centr_closeness <- centralization.closeness(var7)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var7_df_closseness <- data.frame(
var7_incloseness,
var7_outcloseness,
var7_totalcloseness,
var7_incloseness_n,
var7_outcloseness_n,
var7_totalcloseness_n,
var7_centr_closeness) %>% round(6)

#Adding type
var7_df_closseness <-cbind(var7_df_closseness, V(var7)$LABEL_COR)

#Adding names
names(var7_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var7_df_closseness<-var7_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var7_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-1ed0fb03801f064a76e4" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-1ed0fb03801f064a76e4">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000132\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001076\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00067\" data-max=\"0.001805\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024587\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.200215\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.124581\" data-max=\"0.33574\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[0.000132,0.000129,0.000132,0.000127,0.000128,0.000127,0.000126,0.000128,0.000126,0.000128,0.000128,0.000127,0.000123,0.000122,0.000126,0.000126,0.000127,0.000132,0.000123,0.000127,0.000128,0.000127,0.000127,0.000118,0.000117,0.00013,0.000123,0.000128,0.000127,0.000128,0.000128,0.000126,0.000128,0.00012,0.000127,0.000127,0.000128,0.000124,0.000124,0.000127,0.00013,0.000126,0.000126,0.000129,0.000126,0.000127,0.000117,0.000124,0.000124,0.000124,0.000124,0.000124,0.000122,0.000129,0.000121,0.000127,0.000129,0.00012,0.000123,0.00013,0.000129,0.000119,0.000126,0.000126,0.000127,2.9e-05,0.000117,0.000126,0.000122,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000126,0.000128,0.000119,0.000119,0.000119,0.000119,0.000126,0.000125,0.000126,0.000127,0.000128,0.000117,0.000126,0.000126,0.000127,0.000116,0.000122,0.000126,0.00013,0.000127,0.000117,0.000126,0.000127,0.00013,0.000119,0.000124,0.000127,0.000127,0.000126,0.000127,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000126,0.000126,0.000127,0.00013,0.000126,0.000127,0.000127,0.000126,0.000127,0.000126,0.000126,0.000126,0.000127,0.000125,0.000126,0.000126,0.000127,0.000126,0.000127,0.000128,0.000129,0.000126,0.000127,0.000126,0.000127,0.000127,0.000126,0.000126,0.000125,0.000128,0.000127,0.000129,0.000124,0.000125,0.000126,0.000125,0.000126,0.000125,0.00013,0.000118,0.000128,0.000129,0.000129,0.00012,0.00012,0.000129,0.000129,0.000129,0.00012,0.00012,0.000123,0.00012,0.00012,0.000126,0.000124,0.00012,0.000123,0.000126,0.000126,0.000127,0.000125,0.00012,0.000125,0.000123,0.000126,0.000126,0.000128,0.000128,0.000123,0.000127,0.000128],[0.000953,0.000939,0.001076,0.000976,0.000739,0.000741,0.000757,0.001032,0.000813,0.000714,0.000793,0.000792,0.000903,0.00081,0.000824,0.000747,0.000926,0.000999,0.000706,0.000701,0.000754,0.000883,0.000924,0.000838,0.000674,0.000636,0.000739,0.000818,0.000884,0.000931,0.000771,0.000936,0.000984,0.000568,0.000569,0.000705,0.000765,0.000837,0.000894,0.000907,0.00091,0.000916,0.000915,0.00085,0.000978,0.000816,0.000615,0.000744,0.000744,0.000744,0.000744,0.000744,0.000739,0.000815,0.000818,0.000864,0.000917,0.000785,0.000606,0.00081,2.9e-05,0.000529,0.000996,0.000756,0.000925,2.9e-05,2.9e-05,0.000945,0.000808,0.000903,0.000775,0.00071,0.000706,0.00085,0.000706,0.000922,0.000786,0.000857,0.000561,0.000561,0.000561,0.000561,0.000826,0.000573,0.000806,0.000848,0.000915,0.000631,0.000747,0.000936,0.000912,0.000956,0.000721,0.000633,0.000922,0.000911,0.000468,0.000597,0.000769,0.000847,0.000524,0.000776,2.9e-05,2.9e-05,0.000917,0.000875,0.000699,2.9e-05,2.9e-05,0.000959,0.000945,0.000931,0.000737,0.00091,0.000955,0.000928,0.000991,0.000801,0.000862,0.000918,0.000951,0.000924,0.000677,0.000677,0.000677,0.000799,0.000675,0.000743,0.000675,0.000696,0.000842,0.000695,0.000866,0.0009,0.000772,0.000786,0.000681,0.000675,0.0009,0.000843,0.00069,0.000675,0.000684,0.000675,0.000675,0.000675,0.000729,0.000542,0.000612,0.000618,0.000618,0.000618,0.000618,0.000618,0.00052,0.000647,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001779,0.001495,0.001805,0.001412,0.001295,0.001309,0.001274,0.00161,0.001267,0.001131,0.001326,0.001195,0.001239,0.001059,0.001144,0.001072,0.00133,0.001724,0.001031,0.00123,0.00129,0.001425,0.001449,0.0011,0.000941,0.001376,0.001124,0.001295,0.001305,0.001372,0.001224,0.001456,0.001522,0.00085,0.00103,0.001253,0.001316,0.001126,0.001307,0.001292,0.001451,0.001314,0.001287,0.001499,0.001486,0.001205,0.000884,0.00107,0.00107,0.00107,0.00107,0.00107,0.001095,0.00134,0.001119,0.001264,0.001447,0.001163,0.000948,0.001493,0.001022,0.000696,0.00159,0.001241,0.001502,0.00067,0.000715,0.001435,0.001131,0.001357,0.001271,0.001178,0.001015,0.001238,0.00125,0.001383,0.0012,0.001318,0.000719,0.000719,0.000719,0.000719,0.001344,0.001037,0.001185,0.001282,0.001555,0.000903,0.001053,0.001429,0.001477,0.001499,0.000994,0.001049,0.001466,0.001541,0.000736,0.001245,0.001244,0.001364,0.000739,0.001047,0.000844,0.000844,0.001473,0.001342,0.000979,0.000844,0.000867,0.001555,0.001488,0.001486,0.001139,0.001515,0.001567,0.001484,0.001577,0.001175,0.001374,0.001466,0.001524,0.001391,0.00099,0.000969,0.00099,0.001143,0.000969,0.001063,0.000978,0.00098,0.001247,0.000971,0.001325,0.001355,0.001121,0.001332,0.001335,0.000973,0.001397,0.001252,0.001026,0.001022,0.000964,0.000966,0.000908,0.001107,0.001083,0.001328,0.000906,0.000917,0.000967,0.000917,0.000967,0.000917,0.001484,0.000948,0.000918,0.000969,0.000967,0.000792,0.00082,0.001267,0.001066,0.000981,0.000708,0.000692,0.000837,0.000708,0.000708,0.000918,0.000798,0.000708,0.000727,0.000913,0.000936,0.001042,0.000968,0.000708,0.000968,0.000787,0.000943,0.000943,0.000972,0.001266,0.00088,0.001175,0.000902],[0.024538,0.02396,0.024587,0.023667,0.023779,0.0237,0.023497,0.023892,0.023488,0.023749,0.023822,0.023706,0.022889,0.022769,0.023509,0.02339,0.0237,0.024461,0.022845,0.023592,0.023788,0.023562,0.023697,0.021965,0.021754,0.024134,0.022839,0.023743,0.023616,0.023721,0.023773,0.023482,0.023791,0.022364,0.023601,0.023541,0.02384,0.023037,0.023003,0.023676,0.024153,0.023515,0.023482,0.023997,0.023405,0.023679,0.021846,0.023043,0.023043,0.023043,0.023043,0.023043,0.022719,0.024065,0.022439,0.023628,0.024081,0.022375,0.022957,0.024241,0.024022,0.022182,0.023526,0.023488,0.023562,0.005348,0.021757,0.02352,0.022775,0.023458,0.023628,0.023637,0.023544,0.023553,0.023661,0.023482,0.023515,0.023855,0.022156,0.022156,0.022156,0.022156,0.023512,0.023172,0.023358,0.023631,0.023813,0.021762,0.023509,0.023461,0.023544,0.02161,0.022711,0.023382,0.02409,0.023547,0.021762,0.023491,0.023661,0.02409,0.022188,0.023131,0.023535,0.023535,0.023482,0.023586,0.023491,0.023535,0.023592,0.023595,0.023544,0.023652,0.023562,0.023631,0.023634,0.023497,0.023488,0.023491,0.02361,0.024087,0.023482,0.023679,0.023574,0.023518,0.023574,0.023526,0.023518,0.023526,0.023532,0.023305,0.023503,0.023526,0.023562,0.023518,0.023571,0.023724,0.024075,0.023491,0.023664,0.023485,0.023586,0.023604,0.023479,0.023485,0.02325,0.02384,0.023541,0.024068,0.022997,0.023285,0.023509,0.023285,0.023509,0.023285,0.024216,0.021929,0.02384,0.024068,0.024059,0.022243,0.022233,0.023981,0.024053,0.023935,0.022308,0.022236,0.022819,0.022308,0.022308,0.023355,0.02298,0.022308,0.022884,0.023346,0.023494,0.023571,0.023238,0.022308,0.023238,0.022856,0.023358,0.023358,0.023828,0.023767,0.022842,0.023676,0.02377],[0.177312,0.174648,0.200215,0.181463,0.137371,0.13788,0.140802,0.19195,0.15122,0.132857,0.147502,0.147268,0.168022,0.150729,0.153339,0.13891,0.172222,0.185814,0.131263,0.130435,0.140271,0.164166,0.171904,0.155909,0.125337,0.118321,0.137472,0.152085,0.164456,0.173184,0.143408,0.174157,0.183071,0.105562,0.105923,0.131078,0.142202,0.155649,0.166369,0.168631,0.169245,0.17033,0.170174,0.158163,0.181996,0.151837,0.114391,0.138393,0.138393,0.138393,0.138393,0.138393,0.137472,0.151589,0.152209,0.160622,0.170486,0.145997,0.112796,0.150729,0.005348,0.098361,0.185259,0.14059,0.172063,0.005376,0.005405,0.175803,0.150364,0.168022,0.144074,0.132009,0.131263,0.158163,0.131356,0.171429,0.146226,0.159383,0.10426,0.10426,0.10426,0.10426,0.153719,0.10659,0.149879,0.157761,0.170174,0.117424,0.13891,0.174157,0.169708,0.17782,0.134102,0.117796,0.171429,0.169399,0.087079,0.111045,0.143077,0.157627,0.097535,0.14441,0.005348,0.005348,0.170642,0.16273,0.129979,0.005348,0.005348,0.178332,0.175803,0.173184,0.137067,0.169245,0.17765,0.172542,0.184341,0.148919,0.160345,0.170799,0.176806,0.171904,0.125846,0.125846,0.125846,0.148681,0.125591,0.138187,0.125591,0.129526,0.156698,0.129256,0.161039,0.167417,0.143519,0.146112,0.126617,0.125591,0.167417,0.15683,0.128364,0.125591,0.127136,0.125591,0.125591,0.125591,0.135569,0.100758,0.113831,0.115028,0.115028,0.115028,0.115028,0.115028,0.096724,0.12031,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.330961,0.278027,0.33574,0.262712,0.240933,0.243455,0.236943,0.299517,0.235741,0.210407,0.246684,0.222222,0.230483,0.197034,0.212815,0.199357,0.24734,0.32069,0.191753,0.228782,0.24,0.264957,0.269565,0.20462,0.174976,0.255846,0.208989,0.240933,0.24282,0.255144,0.227662,0.270742,0.283105,0.158029,0.191555,0.233083,0.244737,0.209459,0.243137,0.24031,0.269956,0.244415,0.239382,0.278861,0.276374,0.224096,0.164456,0.19893,0.19893,0.19893,0.19893,0.19893,0.203724,0.24933,0.208054,0.235145,0.269175,0.216279,0.176303,0.277612,0.190184,0.129526,0.295707,0.230769,0.279279,0.124581,0.132952,0.266858,0.210407,0.252374,0.236341,0.219081,0.188832,0.230198,0.2325,0.257261,0.223289,0.245059,0.133813,0.133813,0.133813,0.133813,0.25,0.192946,0.220379,0.238462,0.289269,0.168022,0.195789,0.265714,0.274742,0.278861,0.184891,0.195173,0.272727,0.286595,0.136865,0.231631,0.231343,0.253752,0.137371,0.194764,0.156962,0.156962,0.273932,0.249664,0.182174,0.156962,0.161179,0.289269,0.276786,0.276374,0.211845,0.281818,0.291536,0.275964,0.293375,0.218566,0.255495,0.272727,0.283537,0.258693,0.184158,0.180233,0.184158,0.212571,0.180233,0.197662,0.181996,0.182353,0.23192,0.180583,0.246358,0.252033,0.20852,0.24767,0.248331,0.180934,0.259777,0.232791,0.190769,0.190184,0.179364,0.17971,0.168937,0.20598,0.201517,0.247012,0.168478,0.170486,0.179884,0.170486,0.179884,0.170486,0.275964,0.176303,0.170799,0.180233,0.179884,0.147385,0.152584,0.235741,0.198294,0.182532,0.131728,0.128631,0.155649,0.131728,0.131728,0.170799,0.148444,0.131728,0.135273,0.169863,0.174157,0.19375,0.180058,0.131728,0.180058,0.146341,0.175306,0.175306,0.180758,0.235443,0.163588,0.218566,0.167719],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var7_df_closseness, by=list(var7_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var7_df_closseness, by=list(var7_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-34171982c4af4dc4bf39" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-34171982c4af4dc4bf39">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00012\" data-max=\"0.000132\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001076\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7.2e-05\" data-max=\"0.00037\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000902\" data-max=\"0.001805\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.8e-05\" data-max=\"0.000337\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022276\" data-max=\"0.024587\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.6e-05\" data-max=\"0.004157\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.200215\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.013273\" data-max=\"0.068865\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.167719\" data-max=\"0.33574\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003228\" data-max=\"0.06273\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003883\" data-max=\"0.139638\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[0.000127,0.000123,0.000127,0.000129,0.000126,0.00013,0.000128,0.000132,0.000127,0.000127,0.000128,0.00012,0.000124,0.000127,0.000127,0.000124,0.000128,0.00013,0.000132,0.000125,0.000132,0.000126,0.000127],[3e-06,4e-06,null,null,null,null,0,null,null,null,null,2.2e-05,3e-06,0,1e-06,3e-06,null,null,null,1e-06,null,1e-06,1e-06],[0.000835,0.000265,0.000701,0.000939,0.000757,0.00081,0.000749,0.001076,0.000924,0.000729,0.000857,0.000724,0.000863,0.000822,0.000802,0.000803,2.9e-05,0.000847,0.000953,0.000526,0.000999,0.000593,0.000827],[0.000197,0.000321,null,null,null,null,7.2e-05,null,null,null,null,0.000225,7.5e-05,0.000117,0.000107,0.000145,null,null,null,0.00037,null,0.000264,0.000107],[0.001293,0.000947,0.00123,0.001495,0.001274,0.001493,0.001231,0.001805,0.001449,0.001083,0.001318,0.001196,0.001186,0.001259,0.001311,0.001153,0.000902,0.001364,0.001779,0.001021,0.001724,0.001017,0.001301],[0.000337,0.000213,null,null,null,null,0.000107,null,null,null,null,0.000297,0.00018,0.000114,1.8e-05,0.000242,null,null,null,0.000143,null,0.00014,0.000181],[0.023647,0.022879,0.023592,0.02396,0.023497,0.024241,0.02379,0.024587,0.023697,0.023541,0.023855,0.022276,0.023142,0.02369,0.023726,0.023064,0.02377,0.02409,0.024538,0.023237,0.024461,0.023558,0.023615],[0.000591,0.000736,null,null,null,null,4.9e-05,null,null,null,null,0.004157,0.000528,7.4e-05,4.6e-05,0.000625,null,null,null,0.000252,null,0.000224,0.000188],[0.155382,0.049265,0.130435,0.174648,0.140802,0.150729,0.139316,0.200215,0.171904,0.135569,0.159383,0.134734,0.16053,0.152851,0.149158,0.149364,0.005348,0.157627,0.177312,0.097857,0.185814,0.110286,0.153831],[0.036643,0.059737,null,null,null,null,0.013273,null,null,null,null,0.041817,0.01386,0.021735,0.019976,0.026941,null,null,null,0.068865,null,0.049123,0.019856],[0.240513,0.176143,0.228782,0.278027,0.236943,0.277612,0.228971,0.33574,0.269565,0.201517,0.245059,0.222492,0.220724,0.234227,0.243909,0.21453,0.167719,0.253752,0.330961,0.189889,0.32069,0.189178,0.242028],[0.06273,0.039598,null,null,null,null,0.019916,null,null,null,null,0.055188,0.033503,0.021182,0.003228,0.045076,null,null,null,0.026618,null,0.025988,0.033743],[0.300585,0.099063,0.286154,0.343173,0.303922,0.323478,0.300896,0.391579,0.295238,0.29108,0.313659,0.266552,0.319611,0.301223,0.316849,0.295625,0.005348,0.306425,0.371257,0.194167,0.314189,0.234219,0.288627],[0.043014,0.125815,null,null,null,null,0.018146,null,null,null,null,0.070696,0.003883,0.033229,0.029748,0.023781,null,null,null,0.139638,null,0.102732,0.011577]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var7_df_closseness <- data.frame(
var7_incloseness,
var7_outcloseness,
var7_totalcloseness,
var7_incloseness_n,
var7_outcloseness_n,
var7_totalcloseness_n,
var7_centr_closeness) %>% round(6)

#Adding type
var7_df_closseness <-cbind(var7_df_closseness, V(var7)$TIPO1)

#Adding names
names(var7_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var7_df_closseness<-var7_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var7_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-f5090db1445c278b2cf9" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-f5090db1445c278b2cf9">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000132\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001076\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00067\" data-max=\"0.001805\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024587\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.200215\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.124581\" data-max=\"0.33574\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[0.000132,0.000129,0.000132,0.000127,0.000128,0.000127,0.000126,0.000128,0.000126,0.000128,0.000128,0.000127,0.000123,0.000122,0.000126,0.000126,0.000127,0.000132,0.000123,0.000127,0.000128,0.000127,0.000127,0.000118,0.000117,0.00013,0.000123,0.000128,0.000127,0.000128,0.000128,0.000126,0.000128,0.00012,0.000127,0.000127,0.000128,0.000124,0.000124,0.000127,0.00013,0.000126,0.000126,0.000129,0.000126,0.000127,0.000117,0.000124,0.000124,0.000124,0.000124,0.000124,0.000122,0.000129,0.000121,0.000127,0.000129,0.00012,0.000123,0.00013,0.000129,0.000119,0.000126,0.000126,0.000127,2.9e-05,0.000117,0.000126,0.000122,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000126,0.000128,0.000119,0.000119,0.000119,0.000119,0.000126,0.000125,0.000126,0.000127,0.000128,0.000117,0.000126,0.000126,0.000127,0.000116,0.000122,0.000126,0.00013,0.000127,0.000117,0.000126,0.000127,0.00013,0.000119,0.000124,0.000127,0.000127,0.000126,0.000127,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000126,0.000126,0.000127,0.00013,0.000126,0.000127,0.000127,0.000126,0.000127,0.000126,0.000126,0.000126,0.000127,0.000125,0.000126,0.000126,0.000127,0.000126,0.000127,0.000128,0.000129,0.000126,0.000127,0.000126,0.000127,0.000127,0.000126,0.000126,0.000125,0.000128,0.000127,0.000129,0.000124,0.000125,0.000126,0.000125,0.000126,0.000125,0.00013,0.000118,0.000128,0.000129,0.000129,0.00012,0.00012,0.000129,0.000129,0.000129,0.00012,0.00012,0.000123,0.00012,0.00012,0.000126,0.000124,0.00012,0.000123,0.000126,0.000126,0.000127,0.000125,0.00012,0.000125,0.000123,0.000126,0.000126,0.000128,0.000128,0.000123,0.000127,0.000128],[0.000953,0.000939,0.001076,0.000976,0.000739,0.000741,0.000757,0.001032,0.000813,0.000714,0.000793,0.000792,0.000903,0.00081,0.000824,0.000747,0.000926,0.000999,0.000706,0.000701,0.000754,0.000883,0.000924,0.000838,0.000674,0.000636,0.000739,0.000818,0.000884,0.000931,0.000771,0.000936,0.000984,0.000568,0.000569,0.000705,0.000765,0.000837,0.000894,0.000907,0.00091,0.000916,0.000915,0.00085,0.000978,0.000816,0.000615,0.000744,0.000744,0.000744,0.000744,0.000744,0.000739,0.000815,0.000818,0.000864,0.000917,0.000785,0.000606,0.00081,2.9e-05,0.000529,0.000996,0.000756,0.000925,2.9e-05,2.9e-05,0.000945,0.000808,0.000903,0.000775,0.00071,0.000706,0.00085,0.000706,0.000922,0.000786,0.000857,0.000561,0.000561,0.000561,0.000561,0.000826,0.000573,0.000806,0.000848,0.000915,0.000631,0.000747,0.000936,0.000912,0.000956,0.000721,0.000633,0.000922,0.000911,0.000468,0.000597,0.000769,0.000847,0.000524,0.000776,2.9e-05,2.9e-05,0.000917,0.000875,0.000699,2.9e-05,2.9e-05,0.000959,0.000945,0.000931,0.000737,0.00091,0.000955,0.000928,0.000991,0.000801,0.000862,0.000918,0.000951,0.000924,0.000677,0.000677,0.000677,0.000799,0.000675,0.000743,0.000675,0.000696,0.000842,0.000695,0.000866,0.0009,0.000772,0.000786,0.000681,0.000675,0.0009,0.000843,0.00069,0.000675,0.000684,0.000675,0.000675,0.000675,0.000729,0.000542,0.000612,0.000618,0.000618,0.000618,0.000618,0.000618,0.00052,0.000647,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001779,0.001495,0.001805,0.001412,0.001295,0.001309,0.001274,0.00161,0.001267,0.001131,0.001326,0.001195,0.001239,0.001059,0.001144,0.001072,0.00133,0.001724,0.001031,0.00123,0.00129,0.001425,0.001449,0.0011,0.000941,0.001376,0.001124,0.001295,0.001305,0.001372,0.001224,0.001456,0.001522,0.00085,0.00103,0.001253,0.001316,0.001126,0.001307,0.001292,0.001451,0.001314,0.001287,0.001499,0.001486,0.001205,0.000884,0.00107,0.00107,0.00107,0.00107,0.00107,0.001095,0.00134,0.001119,0.001264,0.001447,0.001163,0.000948,0.001493,0.001022,0.000696,0.00159,0.001241,0.001502,0.00067,0.000715,0.001435,0.001131,0.001357,0.001271,0.001178,0.001015,0.001238,0.00125,0.001383,0.0012,0.001318,0.000719,0.000719,0.000719,0.000719,0.001344,0.001037,0.001185,0.001282,0.001555,0.000903,0.001053,0.001429,0.001477,0.001499,0.000994,0.001049,0.001466,0.001541,0.000736,0.001245,0.001244,0.001364,0.000739,0.001047,0.000844,0.000844,0.001473,0.001342,0.000979,0.000844,0.000867,0.001555,0.001488,0.001486,0.001139,0.001515,0.001567,0.001484,0.001577,0.001175,0.001374,0.001466,0.001524,0.001391,0.00099,0.000969,0.00099,0.001143,0.000969,0.001063,0.000978,0.00098,0.001247,0.000971,0.001325,0.001355,0.001121,0.001332,0.001335,0.000973,0.001397,0.001252,0.001026,0.001022,0.000964,0.000966,0.000908,0.001107,0.001083,0.001328,0.000906,0.000917,0.000967,0.000917,0.000967,0.000917,0.001484,0.000948,0.000918,0.000969,0.000967,0.000792,0.00082,0.001267,0.001066,0.000981,0.000708,0.000692,0.000837,0.000708,0.000708,0.000918,0.000798,0.000708,0.000727,0.000913,0.000936,0.001042,0.000968,0.000708,0.000968,0.000787,0.000943,0.000943,0.000972,0.001266,0.00088,0.001175,0.000902],[0.024538,0.02396,0.024587,0.023667,0.023779,0.0237,0.023497,0.023892,0.023488,0.023749,0.023822,0.023706,0.022889,0.022769,0.023509,0.02339,0.0237,0.024461,0.022845,0.023592,0.023788,0.023562,0.023697,0.021965,0.021754,0.024134,0.022839,0.023743,0.023616,0.023721,0.023773,0.023482,0.023791,0.022364,0.023601,0.023541,0.02384,0.023037,0.023003,0.023676,0.024153,0.023515,0.023482,0.023997,0.023405,0.023679,0.021846,0.023043,0.023043,0.023043,0.023043,0.023043,0.022719,0.024065,0.022439,0.023628,0.024081,0.022375,0.022957,0.024241,0.024022,0.022182,0.023526,0.023488,0.023562,0.005348,0.021757,0.02352,0.022775,0.023458,0.023628,0.023637,0.023544,0.023553,0.023661,0.023482,0.023515,0.023855,0.022156,0.022156,0.022156,0.022156,0.023512,0.023172,0.023358,0.023631,0.023813,0.021762,0.023509,0.023461,0.023544,0.02161,0.022711,0.023382,0.02409,0.023547,0.021762,0.023491,0.023661,0.02409,0.022188,0.023131,0.023535,0.023535,0.023482,0.023586,0.023491,0.023535,0.023592,0.023595,0.023544,0.023652,0.023562,0.023631,0.023634,0.023497,0.023488,0.023491,0.02361,0.024087,0.023482,0.023679,0.023574,0.023518,0.023574,0.023526,0.023518,0.023526,0.023532,0.023305,0.023503,0.023526,0.023562,0.023518,0.023571,0.023724,0.024075,0.023491,0.023664,0.023485,0.023586,0.023604,0.023479,0.023485,0.02325,0.02384,0.023541,0.024068,0.022997,0.023285,0.023509,0.023285,0.023509,0.023285,0.024216,0.021929,0.02384,0.024068,0.024059,0.022243,0.022233,0.023981,0.024053,0.023935,0.022308,0.022236,0.022819,0.022308,0.022308,0.023355,0.02298,0.022308,0.022884,0.023346,0.023494,0.023571,0.023238,0.022308,0.023238,0.022856,0.023358,0.023358,0.023828,0.023767,0.022842,0.023676,0.02377],[0.177312,0.174648,0.200215,0.181463,0.137371,0.13788,0.140802,0.19195,0.15122,0.132857,0.147502,0.147268,0.168022,0.150729,0.153339,0.13891,0.172222,0.185814,0.131263,0.130435,0.140271,0.164166,0.171904,0.155909,0.125337,0.118321,0.137472,0.152085,0.164456,0.173184,0.143408,0.174157,0.183071,0.105562,0.105923,0.131078,0.142202,0.155649,0.166369,0.168631,0.169245,0.17033,0.170174,0.158163,0.181996,0.151837,0.114391,0.138393,0.138393,0.138393,0.138393,0.138393,0.137472,0.151589,0.152209,0.160622,0.170486,0.145997,0.112796,0.150729,0.005348,0.098361,0.185259,0.14059,0.172063,0.005376,0.005405,0.175803,0.150364,0.168022,0.144074,0.132009,0.131263,0.158163,0.131356,0.171429,0.146226,0.159383,0.10426,0.10426,0.10426,0.10426,0.153719,0.10659,0.149879,0.157761,0.170174,0.117424,0.13891,0.174157,0.169708,0.17782,0.134102,0.117796,0.171429,0.169399,0.087079,0.111045,0.143077,0.157627,0.097535,0.14441,0.005348,0.005348,0.170642,0.16273,0.129979,0.005348,0.005348,0.178332,0.175803,0.173184,0.137067,0.169245,0.17765,0.172542,0.184341,0.148919,0.160345,0.170799,0.176806,0.171904,0.125846,0.125846,0.125846,0.148681,0.125591,0.138187,0.125591,0.129526,0.156698,0.129256,0.161039,0.167417,0.143519,0.146112,0.126617,0.125591,0.167417,0.15683,0.128364,0.125591,0.127136,0.125591,0.125591,0.125591,0.135569,0.100758,0.113831,0.115028,0.115028,0.115028,0.115028,0.115028,0.096724,0.12031,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.330961,0.278027,0.33574,0.262712,0.240933,0.243455,0.236943,0.299517,0.235741,0.210407,0.246684,0.222222,0.230483,0.197034,0.212815,0.199357,0.24734,0.32069,0.191753,0.228782,0.24,0.264957,0.269565,0.20462,0.174976,0.255846,0.208989,0.240933,0.24282,0.255144,0.227662,0.270742,0.283105,0.158029,0.191555,0.233083,0.244737,0.209459,0.243137,0.24031,0.269956,0.244415,0.239382,0.278861,0.276374,0.224096,0.164456,0.19893,0.19893,0.19893,0.19893,0.19893,0.203724,0.24933,0.208054,0.235145,0.269175,0.216279,0.176303,0.277612,0.190184,0.129526,0.295707,0.230769,0.279279,0.124581,0.132952,0.266858,0.210407,0.252374,0.236341,0.219081,0.188832,0.230198,0.2325,0.257261,0.223289,0.245059,0.133813,0.133813,0.133813,0.133813,0.25,0.192946,0.220379,0.238462,0.289269,0.168022,0.195789,0.265714,0.274742,0.278861,0.184891,0.195173,0.272727,0.286595,0.136865,0.231631,0.231343,0.253752,0.137371,0.194764,0.156962,0.156962,0.273932,0.249664,0.182174,0.156962,0.161179,0.289269,0.276786,0.276374,0.211845,0.281818,0.291536,0.275964,0.293375,0.218566,0.255495,0.272727,0.283537,0.258693,0.184158,0.180233,0.184158,0.212571,0.180233,0.197662,0.181996,0.182353,0.23192,0.180583,0.246358,0.252033,0.20852,0.24767,0.248331,0.180934,0.259777,0.232791,0.190769,0.190184,0.179364,0.17971,0.168937,0.20598,0.201517,0.247012,0.168478,0.170486,0.179884,0.170486,0.179884,0.170486,0.275964,0.176303,0.170799,0.180233,0.179884,0.147385,0.152584,0.235741,0.198294,0.182532,0.131728,0.128631,0.155649,0.131728,0.131728,0.170799,0.148444,0.131728,0.135273,0.169863,0.174157,0.19375,0.180058,0.131728,0.180058,0.146341,0.175306,0.175306,0.180758,0.235443,0.163588,0.218566,0.167719],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var7_df_closseness, by=list(var7_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var7_df_closseness, by=list(var7_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-d4e3cc46a1d1258d7a46" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-d4e3cc46a1d1258d7a46">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000122\" data-max=\"0.000127\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2e-06\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000467\" data-max=\"0.000751\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000233\" data-max=\"0.000363\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001039\" data-max=\"0.001232\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000221\" data-max=\"0.00026\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022776\" data-max=\"0.023619\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000286\" data-max=\"0.00211\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.086794\" data-max=\"0.139668\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.043304\" data-max=\"0.067568\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.193306\" data-max=\"0.229121\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.041134\" data-max=\"0.048387\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.173776\" data-max=\"0.2725\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.079091\" data-max=\"0.135432\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[0.000127,0.000122],[2e-06,1.1e-05],[0.000751,0.000467],[0.000233,0.000363],[0.001232,0.001039],[0.000221,0.00026],[0.023619,0.022776],[0.000286,0.00211],[0.139668,0.086794],[0.043304,0.067568],[0.229121,0.193306],[0.041134,0.048387],[0.2725,0.173776],[0.079091,0.135432]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var7_df_closseness <- data.frame(
var7_incloseness,
var7_outcloseness,
var7_totalcloseness,
var7_incloseness_n,
var7_outcloseness_n,
var7_totalcloseness_n,
var7_centr_closeness) %>% round(6)

#Adding type
var7_df_closseness <-cbind(var7_df_closseness, V(var7)$TIPO2)

#Adding names
names(var7_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var7_df_closseness<-var7_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var7_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-15f7788a53adf475f62c" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-15f7788a53adf475f62c">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000132\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001076\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00067\" data-max=\"0.001805\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024587\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.200215\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.124581\" data-max=\"0.33574\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[0.000132,0.000129,0.000132,0.000127,0.000128,0.000127,0.000126,0.000128,0.000126,0.000128,0.000128,0.000127,0.000123,0.000122,0.000126,0.000126,0.000127,0.000132,0.000123,0.000127,0.000128,0.000127,0.000127,0.000118,0.000117,0.00013,0.000123,0.000128,0.000127,0.000128,0.000128,0.000126,0.000128,0.00012,0.000127,0.000127,0.000128,0.000124,0.000124,0.000127,0.00013,0.000126,0.000126,0.000129,0.000126,0.000127,0.000117,0.000124,0.000124,0.000124,0.000124,0.000124,0.000122,0.000129,0.000121,0.000127,0.000129,0.00012,0.000123,0.00013,0.000129,0.000119,0.000126,0.000126,0.000127,2.9e-05,0.000117,0.000126,0.000122,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000126,0.000128,0.000119,0.000119,0.000119,0.000119,0.000126,0.000125,0.000126,0.000127,0.000128,0.000117,0.000126,0.000126,0.000127,0.000116,0.000122,0.000126,0.00013,0.000127,0.000117,0.000126,0.000127,0.00013,0.000119,0.000124,0.000127,0.000127,0.000126,0.000127,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000126,0.000126,0.000127,0.00013,0.000126,0.000127,0.000127,0.000126,0.000127,0.000126,0.000126,0.000126,0.000127,0.000125,0.000126,0.000126,0.000127,0.000126,0.000127,0.000128,0.000129,0.000126,0.000127,0.000126,0.000127,0.000127,0.000126,0.000126,0.000125,0.000128,0.000127,0.000129,0.000124,0.000125,0.000126,0.000125,0.000126,0.000125,0.00013,0.000118,0.000128,0.000129,0.000129,0.00012,0.00012,0.000129,0.000129,0.000129,0.00012,0.00012,0.000123,0.00012,0.00012,0.000126,0.000124,0.00012,0.000123,0.000126,0.000126,0.000127,0.000125,0.00012,0.000125,0.000123,0.000126,0.000126,0.000128,0.000128,0.000123,0.000127,0.000128],[0.000953,0.000939,0.001076,0.000976,0.000739,0.000741,0.000757,0.001032,0.000813,0.000714,0.000793,0.000792,0.000903,0.00081,0.000824,0.000747,0.000926,0.000999,0.000706,0.000701,0.000754,0.000883,0.000924,0.000838,0.000674,0.000636,0.000739,0.000818,0.000884,0.000931,0.000771,0.000936,0.000984,0.000568,0.000569,0.000705,0.000765,0.000837,0.000894,0.000907,0.00091,0.000916,0.000915,0.00085,0.000978,0.000816,0.000615,0.000744,0.000744,0.000744,0.000744,0.000744,0.000739,0.000815,0.000818,0.000864,0.000917,0.000785,0.000606,0.00081,2.9e-05,0.000529,0.000996,0.000756,0.000925,2.9e-05,2.9e-05,0.000945,0.000808,0.000903,0.000775,0.00071,0.000706,0.00085,0.000706,0.000922,0.000786,0.000857,0.000561,0.000561,0.000561,0.000561,0.000826,0.000573,0.000806,0.000848,0.000915,0.000631,0.000747,0.000936,0.000912,0.000956,0.000721,0.000633,0.000922,0.000911,0.000468,0.000597,0.000769,0.000847,0.000524,0.000776,2.9e-05,2.9e-05,0.000917,0.000875,0.000699,2.9e-05,2.9e-05,0.000959,0.000945,0.000931,0.000737,0.00091,0.000955,0.000928,0.000991,0.000801,0.000862,0.000918,0.000951,0.000924,0.000677,0.000677,0.000677,0.000799,0.000675,0.000743,0.000675,0.000696,0.000842,0.000695,0.000866,0.0009,0.000772,0.000786,0.000681,0.000675,0.0009,0.000843,0.00069,0.000675,0.000684,0.000675,0.000675,0.000675,0.000729,0.000542,0.000612,0.000618,0.000618,0.000618,0.000618,0.000618,0.00052,0.000647,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001779,0.001495,0.001805,0.001412,0.001295,0.001309,0.001274,0.00161,0.001267,0.001131,0.001326,0.001195,0.001239,0.001059,0.001144,0.001072,0.00133,0.001724,0.001031,0.00123,0.00129,0.001425,0.001449,0.0011,0.000941,0.001376,0.001124,0.001295,0.001305,0.001372,0.001224,0.001456,0.001522,0.00085,0.00103,0.001253,0.001316,0.001126,0.001307,0.001292,0.001451,0.001314,0.001287,0.001499,0.001486,0.001205,0.000884,0.00107,0.00107,0.00107,0.00107,0.00107,0.001095,0.00134,0.001119,0.001264,0.001447,0.001163,0.000948,0.001493,0.001022,0.000696,0.00159,0.001241,0.001502,0.00067,0.000715,0.001435,0.001131,0.001357,0.001271,0.001178,0.001015,0.001238,0.00125,0.001383,0.0012,0.001318,0.000719,0.000719,0.000719,0.000719,0.001344,0.001037,0.001185,0.001282,0.001555,0.000903,0.001053,0.001429,0.001477,0.001499,0.000994,0.001049,0.001466,0.001541,0.000736,0.001245,0.001244,0.001364,0.000739,0.001047,0.000844,0.000844,0.001473,0.001342,0.000979,0.000844,0.000867,0.001555,0.001488,0.001486,0.001139,0.001515,0.001567,0.001484,0.001577,0.001175,0.001374,0.001466,0.001524,0.001391,0.00099,0.000969,0.00099,0.001143,0.000969,0.001063,0.000978,0.00098,0.001247,0.000971,0.001325,0.001355,0.001121,0.001332,0.001335,0.000973,0.001397,0.001252,0.001026,0.001022,0.000964,0.000966,0.000908,0.001107,0.001083,0.001328,0.000906,0.000917,0.000967,0.000917,0.000967,0.000917,0.001484,0.000948,0.000918,0.000969,0.000967,0.000792,0.00082,0.001267,0.001066,0.000981,0.000708,0.000692,0.000837,0.000708,0.000708,0.000918,0.000798,0.000708,0.000727,0.000913,0.000936,0.001042,0.000968,0.000708,0.000968,0.000787,0.000943,0.000943,0.000972,0.001266,0.00088,0.001175,0.000902],[0.024538,0.02396,0.024587,0.023667,0.023779,0.0237,0.023497,0.023892,0.023488,0.023749,0.023822,0.023706,0.022889,0.022769,0.023509,0.02339,0.0237,0.024461,0.022845,0.023592,0.023788,0.023562,0.023697,0.021965,0.021754,0.024134,0.022839,0.023743,0.023616,0.023721,0.023773,0.023482,0.023791,0.022364,0.023601,0.023541,0.02384,0.023037,0.023003,0.023676,0.024153,0.023515,0.023482,0.023997,0.023405,0.023679,0.021846,0.023043,0.023043,0.023043,0.023043,0.023043,0.022719,0.024065,0.022439,0.023628,0.024081,0.022375,0.022957,0.024241,0.024022,0.022182,0.023526,0.023488,0.023562,0.005348,0.021757,0.02352,0.022775,0.023458,0.023628,0.023637,0.023544,0.023553,0.023661,0.023482,0.023515,0.023855,0.022156,0.022156,0.022156,0.022156,0.023512,0.023172,0.023358,0.023631,0.023813,0.021762,0.023509,0.023461,0.023544,0.02161,0.022711,0.023382,0.02409,0.023547,0.021762,0.023491,0.023661,0.02409,0.022188,0.023131,0.023535,0.023535,0.023482,0.023586,0.023491,0.023535,0.023592,0.023595,0.023544,0.023652,0.023562,0.023631,0.023634,0.023497,0.023488,0.023491,0.02361,0.024087,0.023482,0.023679,0.023574,0.023518,0.023574,0.023526,0.023518,0.023526,0.023532,0.023305,0.023503,0.023526,0.023562,0.023518,0.023571,0.023724,0.024075,0.023491,0.023664,0.023485,0.023586,0.023604,0.023479,0.023485,0.02325,0.02384,0.023541,0.024068,0.022997,0.023285,0.023509,0.023285,0.023509,0.023285,0.024216,0.021929,0.02384,0.024068,0.024059,0.022243,0.022233,0.023981,0.024053,0.023935,0.022308,0.022236,0.022819,0.022308,0.022308,0.023355,0.02298,0.022308,0.022884,0.023346,0.023494,0.023571,0.023238,0.022308,0.023238,0.022856,0.023358,0.023358,0.023828,0.023767,0.022842,0.023676,0.02377],[0.177312,0.174648,0.200215,0.181463,0.137371,0.13788,0.140802,0.19195,0.15122,0.132857,0.147502,0.147268,0.168022,0.150729,0.153339,0.13891,0.172222,0.185814,0.131263,0.130435,0.140271,0.164166,0.171904,0.155909,0.125337,0.118321,0.137472,0.152085,0.164456,0.173184,0.143408,0.174157,0.183071,0.105562,0.105923,0.131078,0.142202,0.155649,0.166369,0.168631,0.169245,0.17033,0.170174,0.158163,0.181996,0.151837,0.114391,0.138393,0.138393,0.138393,0.138393,0.138393,0.137472,0.151589,0.152209,0.160622,0.170486,0.145997,0.112796,0.150729,0.005348,0.098361,0.185259,0.14059,0.172063,0.005376,0.005405,0.175803,0.150364,0.168022,0.144074,0.132009,0.131263,0.158163,0.131356,0.171429,0.146226,0.159383,0.10426,0.10426,0.10426,0.10426,0.153719,0.10659,0.149879,0.157761,0.170174,0.117424,0.13891,0.174157,0.169708,0.17782,0.134102,0.117796,0.171429,0.169399,0.087079,0.111045,0.143077,0.157627,0.097535,0.14441,0.005348,0.005348,0.170642,0.16273,0.129979,0.005348,0.005348,0.178332,0.175803,0.173184,0.137067,0.169245,0.17765,0.172542,0.184341,0.148919,0.160345,0.170799,0.176806,0.171904,0.125846,0.125846,0.125846,0.148681,0.125591,0.138187,0.125591,0.129526,0.156698,0.129256,0.161039,0.167417,0.143519,0.146112,0.126617,0.125591,0.167417,0.15683,0.128364,0.125591,0.127136,0.125591,0.125591,0.125591,0.135569,0.100758,0.113831,0.115028,0.115028,0.115028,0.115028,0.115028,0.096724,0.12031,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.330961,0.278027,0.33574,0.262712,0.240933,0.243455,0.236943,0.299517,0.235741,0.210407,0.246684,0.222222,0.230483,0.197034,0.212815,0.199357,0.24734,0.32069,0.191753,0.228782,0.24,0.264957,0.269565,0.20462,0.174976,0.255846,0.208989,0.240933,0.24282,0.255144,0.227662,0.270742,0.283105,0.158029,0.191555,0.233083,0.244737,0.209459,0.243137,0.24031,0.269956,0.244415,0.239382,0.278861,0.276374,0.224096,0.164456,0.19893,0.19893,0.19893,0.19893,0.19893,0.203724,0.24933,0.208054,0.235145,0.269175,0.216279,0.176303,0.277612,0.190184,0.129526,0.295707,0.230769,0.279279,0.124581,0.132952,0.266858,0.210407,0.252374,0.236341,0.219081,0.188832,0.230198,0.2325,0.257261,0.223289,0.245059,0.133813,0.133813,0.133813,0.133813,0.25,0.192946,0.220379,0.238462,0.289269,0.168022,0.195789,0.265714,0.274742,0.278861,0.184891,0.195173,0.272727,0.286595,0.136865,0.231631,0.231343,0.253752,0.137371,0.194764,0.156962,0.156962,0.273932,0.249664,0.182174,0.156962,0.161179,0.289269,0.276786,0.276374,0.211845,0.281818,0.291536,0.275964,0.293375,0.218566,0.255495,0.272727,0.283537,0.258693,0.184158,0.180233,0.184158,0.212571,0.180233,0.197662,0.181996,0.182353,0.23192,0.180583,0.246358,0.252033,0.20852,0.24767,0.248331,0.180934,0.259777,0.232791,0.190769,0.190184,0.179364,0.17971,0.168937,0.20598,0.201517,0.247012,0.168478,0.170486,0.179884,0.170486,0.179884,0.170486,0.275964,0.176303,0.170799,0.180233,0.179884,0.147385,0.152584,0.235741,0.198294,0.182532,0.131728,0.128631,0.155649,0.131728,0.131728,0.170799,0.148444,0.131728,0.135273,0.169863,0.174157,0.19375,0.180058,0.131728,0.180058,0.146341,0.175306,0.175306,0.180758,0.235443,0.163588,0.218566,0.167719],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var7_df_closseness, by=list(var7_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var7_df_closseness, by=list(var7_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-ffa899ed319bceb55c41" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-ffa899ed319bceb55c41">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000122\" data-max=\"0.000128\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000464\" data-max=\"0.000837\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000109\" data-max=\"0.000365\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001038\" data-max=\"0.001304\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000117\" data-max=\"0.000261\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022758\" data-max=\"0.023746\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000135\" data-max=\"0.002119\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.086329\" data-max=\"0.155656\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.020222\" data-max=\"0.067961\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.193016\" data-max=\"0.242526\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.021781\" data-max=\"0.048486\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172406\" data-max=\"0.308329\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.029295\" data-max=\"0.135782\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[0.000128,0.000127,0.000122],[1e-06,2e-06,1.1e-05],[0.000837,0.000736,0.000464],[0.000109,0.000243,0.000365],[0.001304,0.00122,0.001038],[0.000117,0.000231,0.000261],[0.023746,0.023605,0.022758],[0.000135,0.000301,0.002119],[0.155656,0.136939,0.086329],[0.020222,0.045176,0.067961],[0.242526,0.226841,0.193016],[0.021781,0.043066,0.048486],[0.308329,0.266869,0.172406],[0.029295,0.082605,0.135782]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var7_data.RData")
```

