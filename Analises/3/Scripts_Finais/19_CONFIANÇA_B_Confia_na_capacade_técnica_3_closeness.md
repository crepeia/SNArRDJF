# SNA Closeness 19_CONFIANÇA B) Confia na capacade técnica da equipe deste serviço. (var17)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 19_CONFIANÇA B) Confia na capacade técnica da equipe deste serviço. (var17)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var17_data.RData")
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
#var17<-simplify(var17) #Simplify
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
V(var17)$incloseness <- closeness(var17, mode = "in", weights = E(var17)$var17) %>% round(6)
V(var17)$outcloseness <- closeness(var17, mode = "out", weights = E(var17)$var17) %>% round(6)
V(var17)$totalcloseness <- closeness(var17, mode = "total", weights = E(var17)$var17) %>% round(4)
```

###Saving to Environment

```r
var17_incloseness<- closeness(var17, mode = "in", weights = E(var17)$var17) %>% round(6)
var17_outcloseness<- closeness(var17, mode = "out", weights = E(var17)$var17) %>% round(6)
var17_totalcloseness<- closeness(var17, mode = "total", weights = E(var17)$var17) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var17_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0001240 0.0001260 0.0001246 0.0001270 0.0001310
```

```r
sd(var17_incloseness)
```

```
## [1] 7.632008e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var17)$incloseness<-closeness(var17, weights = E(var17)$var17, mode="in")

#Get Variable
V(var17)$var17_color_degree<-round(V(var17)$incloseness,6)

#Creating brewer pallette
vertex_var17_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var17)$var17_color_degree)), "RdBu"))(
            length(unique(V(var17)$var17_color_degree)))

#Saving as Vertex properties 
V(var17)$vertex_var17_color_degree<-
  vertex_var17_color_degree[as.numeric(
  cut(V(var17)$var17_color_degree,
      breaks=length(unique(V(var17)$var17_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var17, es=E(var17), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var17))
maxC <- rep(Inf, vcount(var17))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var17, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var17)$weight)


#PLotting
plot(var17, 
     layout=co,
     edge.color=V(var17)$vertex_var17_color_degree[edge.start],
     edge.arrow.size=closeness(var17, weights = E(var17)$var17, mode="in"),
     edge.width=E(var17)$weight/mean(E(var17)$weight),
     edge.curved = TRUE,
     vertex.color=V(var17)$vertex_var17_color_degree,
     vertex.size=closeness(var17, weights = E(var17)$var17, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var17,"LABEL_COR"),
     vertex.label.cex=(closeness(var17, weights = E(var17)$var17, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var17)$var17_color_degree
b<-V(var17)$vertex_var17_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 19_CONFIANÇA B) Confia na capacade técnica da equipe deste serviço. (var17)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var17, mode="in", weights = E(var17)$var17)), 
             sd(closeness(var17, mode="in", weights = E(var17)$var17))
             )
       )
```

![](19_CONFIANÇA_B_Confia_na_capacade_técnica_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var17_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0005780 0.0007070 0.0006160 0.0007955 0.0012350
```

```r
sd(var17_outcloseness)
```

```
## [1] 0.0003214478
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var17)$outcloseness<-closeness(var17, weights = E(var17)$var17, mode="out")

#Get Variable
V(var17)$var17_color_degree<-round(V(var17)$outcloseness,6)

#Creating brewer pallette
vertex_var17_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var17)$var17_color_degree)), "RdBu"))(
            length(unique(V(var17)$var17_color_degree)))

#Saving as Vertex properties 
V(var17)$vertex_var17_color_degree<-
  vertex_var17_color_degree[as.numeric(
  cut(V(var17)$var17_color_degree,
      breaks=length(unique(V(var17)$var17_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var17, es=E(var17), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var17))
maxC <- rep(Inf, vcount(var17))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var17, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var17)$weight)


#PLotting
plot(var17, 
     layout=co,
     edge.color=V(var17)$vertex_var17_color_degree[edge.start],
     edge.arrow.size=closeness(var17, weights = E(var17)$var17, mode="out"),
     edge.width=E(var17)$weight/2*mean(E(var17)$weight),
     edge.curved = TRUE,
     vertex.color=V(var17)$vertex_var17_color_degree,
     vertex.size=closeness(var17, weights = E(var17)$var17, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var17,"LABEL_COR"),
     vertex.label.cex=closeness(var17, weights = E(var17)$var17, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var17)$var17_color_degree
b<-V(var17)$vertex_var17_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 19_CONFIANÇA B) Confia na capacade técnica da equipe deste serviço. (var17)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var17, mode="out", weights = E(var17)$var17)), 
             sd(closeness(var17, mode="out", weights = E(var17)$var17))
             )
       )
```

![](19_CONFIANÇA_B_Confia_na_capacade_técnica_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var17_totalcloseness)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000598 0.000982 0.001120 0.001099 0.001174 0.001876
```

```r
sd(var17_totalcloseness)
```

```
## [1] 0.000216211
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var17)$allcloseness<-closeness(var17, weights = E(var17)$var17, mode="all")

#Get Variable
V(var17)$var17_color_degree<-round(V(var17)$allcloseness,6)

#Creating brewer pallette
vertex_var17_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var17)$var17_color_degree)), "RdBu"))(
            length(unique(V(var17)$var17_color_degree)))

#Saving as Vertex properties 
V(var17)$vertex_var17_color_degree<-
  vertex_var17_color_degree[as.numeric(
  cut(V(var17)$var17_color_degree,
      breaks=length(unique(V(var17)$var17_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var17, es=E(var17), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var17))
maxC <- rep(Inf, vcount(var17))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var17, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var17)$weight)


#PLotting
plot(var17, 
     layout=co,
     edge.color=V(var17)$vertex_var17_color_degree[edge.start],
     edge.arrow.size=closeness(var17, weights = E(var17)$var17, mode="all"),
     edge.width=E(var17)$weight/2*mean(E(var17)$weight),
     edge.curved = TRUE,
     vertex.color=V(var17)$vertex_var17_color_degree,
     vertex.size=closeness(var17, weights = E(var17)$var17, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var17,"LABEL_COR"),
     vertex.label.cex=(closeness(var17, weights = E(var17)$var17, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var17)$var17_color_degree
b<-V(var17)$vertex_var17_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 19_CONFIANÇA B) Confia na capacade técnica da equipe deste serviço. (var17)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var17, mode="all", weights = E(var17)$var17)), 
             sd(closeness(var17, mode="all", weights = E(var17)$var17))
             )
       )
```

![](19_CONFIANÇA_B_Confia_na_capacade_técnica_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var17)$incloseness_n <- closeness(var17, mode = "in",, weights = E(var17)$var17, normalized = T) %>% round(10)
V(var17)$outcloseness_n <- closeness(var17, mode = "out", normalized = T, weights = E(var17)$var17) %>% round(6)
V(var17)$totalcloseness_n <- closeness(var17, mode = "total", normalized = T, weights = E(var17)$var17) %>% round(6)
```

###Saving to Environment

```r
var17_incloseness_n<- closeness(var17, mode = "in", normalized = T, weights = E(var17)$var17) %>% round(6)
var17_outcloseness_n<- closeness(var17, mode = "out", normalized = T, weights = E(var17)$var17) %>% round(6)
var17_totalcloseness_n<- closeness(var17, mode = "total", normalized = T, weights = E(var17)$var17) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var17_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.023040 0.023510 0.023180 0.023570 0.024400
```

```r
sd(var17_incloseness_n)
```

```
## [1] 0.001422608
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var17)$incloseness_n<-closeness(var17, weights = E(var17)$var17, mode="in", normalized = T)

#Get Variable
V(var17)$var17_color_degree<-round(V(var17)$incloseness_n,6)

#Creating brewer pallette
vertex_var17_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var17)$var17_color_degree)), "RdBu"))(
            length(unique(V(var17)$var17_color_degree)))

#Saving as Vertex properties 
V(var17)$vertex_var17_color_degree<-
  vertex_var17_color_degree[as.numeric(
  cut(V(var17)$var17_color_degree,
      breaks=length(unique(V(var17)$var17_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var17, es=E(var17), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var17))
maxC <- rep(Inf, vcount(var17))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var17, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var17)$weight)


#PLotting
plot(var17, 
     layout=co,
     edge.color=V(var17)$vertex_var17_color_degree[edge.start],
     edge.arrow.size=closeness(var17, weights = E(var17)$var17, mode="in",normalized = T),
     edge.width=E(var17)$weight/10*mean(E(var17)$weight),
     edge.curved = TRUE,
     vertex.color=V(var17)$vertex_var17_color_degree,
     vertex.size=(closeness(var17, weights = E(var17)$var17, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var17,"LABEL_COR"),
     vertex.label.cex=closeness(var17, weights = E(var17)$var17, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var17)$var17_color_degree
b<-V(var17)$vertex_var17_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 19_CONFIANÇA B) Confia na capacade técnica da equipe deste serviço. (var17)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var17, mode="in", weights = E(var17)$var17, normalized = T)), 
             sd(closeness(var17, mode="in", weights = E(var17)$var17, normalized = T))
             )
       )
```

![](19_CONFIANÇA_B_Confia_na_capacade_técnica_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var17_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.107500 0.131400 0.114600 0.148000 0.229600
```

```r
sd(var17_outcloseness_n)
```

```
## [1] 0.05980528
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var17)$outcloseness_n<-closeness(var17, weights = E(var17)$var17, mode="out", normalized = T)

#Get Variable
V(var17)$var17_color_degree<-round(V(var17)$outcloseness_n,6)

#Creating brewer pallette
vertex_var17_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var17)$var17_color_degree)), "RdBu"))(
            length(unique(V(var17)$var17_color_degree)))

#Saving as Vertex properties 
V(var17)$vertex_var17_color_degree<-
  vertex_var17_color_degree[as.numeric(
  cut(V(var17)$var17_color_degree,
      breaks=length(unique(V(var17)$var17_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var17, es=E(var17), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var17))
maxC <- rep(Inf, vcount(var17))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var17, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var17)$weight)


#PLotting
plot(var17, 
     layout=co,
     edge.color=V(var17)$vertex_var17_color_degree[edge.start],
     edge.arrow.size=closeness(var17, weights = E(var17)$var17, mode="out",normalized = T),
     edge.width=E(var17)$weight/10*mean(E(var17)$weight),
     edge.curved = TRUE,
     vertex.color=V(var17)$vertex_var17_color_degree,
     vertex.size=(closeness(var17, weights = E(var17)$var17, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var17,"LABEL_COR"),
     vertex.label.cex=closeness(var17, weights = E(var17)$var17, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var17)$var17_color_degree
b<-V(var17)$vertex_var17_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 19_CONFIANÇA B) Confia na capacade técnica da equipe deste serviço. (var17)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var17, mode="out", weights = E(var17)$var17, normalized = T)), 
             sd(closeness(var17, mode="out", weights = E(var17)$var17, normalized = T))
             )
       )
```

![](19_CONFIANÇA_B_Confia_na_capacade_técnica_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var17_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1112  0.1827  0.2083  0.2043  0.2183  0.3490
```

```r
sd(var17_totalcloseness_n)
```

```
## [1] 0.04021494
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var17)$allcloseness_n<-closeness(var17, weights = E(var17)$var17, mode="all", normalized = T)

#Get Variable
V(var17)$var17_color_degree<-round(V(var17)$allcloseness_n,6)

#Creating brewer pallette
vertex_var17_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var17)$var17_color_degree)), "RdBu"))(
            length(unique(V(var17)$var17_color_degree)))

#Saving as Vertex properties 
V(var17)$vertex_var17_color_degree<-
  vertex_var17_color_degree[as.numeric(
  cut(V(var17)$var17_color_degree,
      breaks=length(unique(V(var17)$var17_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var17, es=E(var17), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var17))
maxC <- rep(Inf, vcount(var17))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var17, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var17)$weight)


#PLotting
plot(var17, 
     layout=co,
     edge.color=V(var17)$vertex_var17_color_degree[edge.start],
     edge.arrow.size=closeness(var17, weights = E(var17)$var17, mode="all",normalized = T),
     edge.width=E(var17)$weight/10*mean(E(var17)$weight),
     edge.curved = TRUE,
     vertex.color=V(var17)$vertex_var17_color_degree,
     vertex.size=(closeness(var17, weights = E(var17)$var17, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var17,"LABEL_COR"),
     vertex.label.cex=closeness(var17, weights = E(var17)$var17, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var17)$var17_color_degree
b<-V(var17)$vertex_var17_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 19_CONFIANÇA B) Confia na capacade técnica da equipe deste serviço. (var17)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var17, mode="all", weights = E(var17)$var17, normalized = T)), 
             sd(closeness(var17, mode="all", weights = E(var17)$var17, normalized = T))
             )
       )
```

![](19_CONFIANÇA_B_Confia_na_capacade_técnica_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var17)$incloseness_n <- closeness(var17, weights = E(var17)$var17, mode = "in", normalized = T) %>% round(6)
V(var17)$outcloseness_n <- closeness(var17, weights = E(var17)$var17, mode = "out", normalized = T) %>% round(6)
V(var17)$totalcloseness_n <- closeness(var17, weights = E(var17)$var17, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var17)$var17_centr_closeness<- centralization.closeness(var17)$res
var17_centr_closeness<- centralization.closeness(var17)$res
var17_centr_closeness_all<- centralization.closeness(var17)
```

###Centralization

```r
var17_centr_closeness_all$centralization
```

```
## [1] 0.1625197
```

###Theoretical Max

```r
var17_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var17)$var17_centr_closeness<- centralization.closeness(var17)$res

#Get Variable
V(var17)$var17_color_degree<-round(V(var17)$var17_centr_closeness,6)

#Creating brewer pallette
vertex_var17_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var17)$var17_color_degree)), "Spectral"))(
            length(unique(V(var17)$var17_color_degree)))

#Saving as Vertex properties 
V(var17)$vertex_var17_color_degree<-
  vertex_var17_color_degree[as.numeric(
  cut(V(var17)$var17_color_degree,
      breaks=length(unique(V(var17)$var17_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var17, es=E(var17), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var17))
maxC <- rep(Inf, vcount(var17))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var17, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var17)$weight)


#PLotting
plot(var17, 
     layout=co,
     edge.color=V(var17)$vertex_var17_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var17)$res,
     edge.width=E(var17)$weight/10*mean(E(var17)$weight),
     edge.curved = TRUE,
     vertex.color=V(var17)$vertex_var17_color_degree,
     vertex.size=centralization.closeness(var17)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var17,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var17)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var17)$var17_color_degree
b<-V(var17)$vertex_var17_color_degree
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
  title("Network Centralization Closeness - 19_CONFIANÇA B) Confia na capacade técnica da equipe deste serviço. (var17)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var17)$res), 
             sd(centralization.closeness(var17)$res)
             )
       )
```

![](19_CONFIANÇA_B_Confia_na_capacade_técnica_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var17_incloseness<- closeness(var17, weights = E(var17)$var17, mode = "in") %>% round(6)
var17_outcloseness<- closeness(var17, weights = E(var17)$var17, mode = "out") %>% round(6)
var17_totalcloseness<- closeness(var17, weights = E(var17)$var17, mode = "total") %>% round(6)
var17_incloseness_n<- closeness(var17,weights = E(var17)$var17, mode = "in", normalized = T) %>% round(6)
var17_outcloseness_n<- closeness(var17,weights = E(var17)$var17, mode = "out", normalized = T) %>% round(6)
var17_totalcloseness_n<- closeness(var17,weights = E(var17)$var17, mode = "total", normalized = T) %>% round(6)
var17_centr_closeness <- centralization.closeness(var17)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var17_df_closseness <- data.frame(
var17_incloseness,
var17_outcloseness,
var17_totalcloseness,
var17_incloseness_n,
var17_outcloseness_n,
var17_totalcloseness_n,
var17_centr_closeness) %>% round(6)

#Adding type
var17_df_closseness <-cbind(var17_df_closseness, V(var17)$LABEL_COR)

#Adding names
names(var17_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var17_df_closseness<-var17_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var17_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-06c9b2a23eb3656b5300" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-06c9b2a23eb3656b5300">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001235\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000598\" data-max=\"0.001876\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024403\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.22963\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.111244\" data-max=\"0.348968\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[0.00013,0.000128,0.000131,0.000128,0.000128,0.000128,0.000125,0.000127,0.000123,0.000127,0.000128,0.000129,0.000123,0.000124,0.000126,0.000127,0.000129,0.000128,0.000122,0.000126,0.000127,0.000125,0.000127,0.000117,0.000116,0.000126,0.00012,0.000127,0.000128,0.000128,0.000128,0.000126,0.000128,0.00012,0.000128,0.000124,0.000127,0.000125,0.000125,0.000126,0.000127,0.000125,0.000126,0.000127,0.000125,0.000128,0.000116,0.000124,0.000124,0.000124,0.000124,0.000124,0.000123,0.000125,0.000122,0.000128,0.000127,0.00012,0.000121,0.000128,0.00013,0.000119,0.000127,0.000126,0.000125,2.9e-05,0.000118,0.000126,0.000122,0.000126,0.000126,0.000127,0.000126,0.000127,0.000127,0.000126,0.000127,0.000129,0.000122,0.000122,0.000122,0.000122,0.000127,0.000124,0.000124,0.000127,0.000125,0.000117,0.000126,0.000126,0.000126,0.000117,0.00012,0.000125,0.000127,0.000124,0.000117,0.000123,0.000127,0.000127,0.000118,0.000125,0.000128,0.000128,0.000126,0.000126,0.000126,0.000128,0.000128,0.000127,0.000126,0.000127,0.000127,0.000126,0.000127,0.000126,0.000126,0.000126,0.000127,0.000127,0.000126,0.000126,0.000127,0.000126,0.000127,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000127,0.000126,0.000126,0.000126,0.000124,0.000127,0.000126,0.000126,0.000124,0.000125,0.000126,0.000125,0.000126,0.000125,0.000127,0.000118,0.000128,0.000129,0.000129,0.00012,0.000121,0.000124,0.000127,0.000127,0.000119,0.000121,0.000123,0.000119,0.000119,0.000123,0.000123,0.000119,0.000123,0.000125,0.000128,0.000128,0.000123,0.000119,0.000123,0.000123,0.000124,0.000124,0.000127,0.000125,0.000123,0.000121,0.000127],[0.001235,0.000902,0.001041,0.000878,0.000929,0.000741,0.000763,0.000995,0.000975,0.000773,0.000797,0.000678,0.00087,0.00085,0.000838,0.00079,0.000956,0.001085,0.000786,0.000782,0.000931,0.000885,0.001036,0.000822,0.00076,0.000584,0.000737,0.000788,0.000873,0.000937,0.000925,0.000793,0.001057,0.000596,0.000601,0.001035,0.000973,0.00063,0.000693,0.001185,0.000873,0.000966,0.000965,0.000785,0.001015,0.000941,0.000625,0.000794,0.000794,0.000794,0.000794,0.000794,0.000752,0.000709,0.000836,0.000831,0.000803,0.000688,0.000607,0.00084,2.9e-05,0.000583,0.000778,0.000707,0.000827,2.9e-05,2.9e-05,0.000825,0.000739,0.000664,0.000664,0.000705,0.00077,0.000745,0.000711,0.000739,0.000767,0.000923,0.000504,0.000504,0.000504,0.000504,0.000762,0.000566,0.000695,0.000912,0.000784,0.00074,0.000811,0.000695,0.00077,0.000799,0.000847,0.000663,0.000801,0.000766,0.000525,0.000662,0.000782,0.000906,0.000573,0.000801,2.9e-05,2.9e-05,0.000769,0.000742,0.000698,2.9e-05,2.9e-05,0.000782,0.000779,0.000868,0.000763,0.000883,0.000766,0.001072,0.000782,0.000779,0.000772,0.000772,0.000769,0.000891,0.000663,0.000663,0.000663,0.000688,0.000661,0.000706,0.000661,0.000688,0.000773,0.000773,0.000705,0.000661,0.000661,0.000788,0.000666,0.000661,0.000661,0.000677,0.000688,0.000661,0.000678,0.000661,0.000753,0.000661,0.000677,0.000485,0.000626,0.000602,0.000602,0.000602,0.000602,0.000602,0.000543,0.000605,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001876,0.001391,0.001567,0.00146,0.001511,0.001414,0.000982,0.001305,0.001239,0.001057,0.001451,0.001497,0.001091,0.001046,0.00125,0.001032,0.001524,0.001515,0.001064,0.001287,0.001318,0.001175,0.001486,0.001027,0.000966,0.001042,0.000994,0.001321,0.001437,0.001486,0.001431,0.001152,0.001517,0.000883,0.00141,0.001435,0.001282,0.000995,0.001002,0.001629,0.001287,0.001244,0.001259,0.001101,0.001397,0.001511,0.000903,0.000994,0.000994,0.000994,0.000994,0.000994,0.000982,0.001157,0.001065,0.001443,0.001151,0.000958,0.000906,0.001319,0.001093,0.000767,0.001142,0.001134,0.001071,0.000598,0.000715,0.001276,0.000995,0.001127,0.001144,0.001126,0.001164,0.001176,0.001159,0.001161,0.001183,0.001486,0.00086,0.00086,0.00086,0.00086,0.001145,0.000971,0.001019,0.001174,0.00107,0.000935,0.001175,0.00113,0.00114,0.000987,0.001073,0.001258,0.001175,0.001054,0.000681,0.000892,0.001161,0.001299,0.000712,0.001045,0.00097,0.00097,0.001134,0.001127,0.001129,0.00097,0.001002,0.001183,0.001136,0.001155,0.001167,0.001174,0.001142,0.001479,0.001135,0.001135,0.001156,0.001163,0.001134,0.001258,0.001124,0.001122,0.001124,0.00112,0.001119,0.001122,0.001131,0.00112,0.00116,0.001163,0.001119,0.001129,0.001122,0.001181,0.001133,0.001136,0.001151,0.001143,0.001161,0.00114,0.001135,0.001129,0.000996,0.001105,0.001287,0.001139,0.000978,0.001109,0.001119,0.001109,0.001119,0.001109,0.001112,0.001098,0.001109,0.001119,0.001119,0.000747,0.000779,0.000801,0.000895,0.000876,0.000685,0.000672,0.000789,0.000685,0.000685,0.000791,0.000788,0.000685,0.000788,0.00089,0.001115,0.001119,0.000792,0.000685,0.000792,0.000795,0.000883,0.000883,0.001031,0.000885,0.000797,0.000672,0.000838],[0.024118,0.023782,0.024403,0.023849,0.023865,0.023858,0.023247,0.023703,0.022915,0.023604,0.023874,0.023907,0.022889,0.023034,0.0235,0.023547,0.023901,0.023816,0.022724,0.023443,0.023568,0.023195,0.023592,0.021852,0.021643,0.023518,0.022372,0.023577,0.02384,0.023831,0.023861,0.023512,0.023865,0.02238,0.023843,0.023003,0.023685,0.023224,0.023204,0.023461,0.023544,0.023311,0.023411,0.023682,0.023311,0.023877,0.021656,0.02314,0.02314,0.02314,0.02314,0.02314,0.022839,0.023323,0.022628,0.023843,0.023547,0.022342,0.022559,0.023791,0.024247,0.02213,0.023535,0.023518,0.023253,0.005348,0.021934,0.023438,0.022655,0.02347,0.023518,0.023538,0.023526,0.023559,0.023571,0.023523,0.02355,0.023904,0.022738,0.022738,0.022738,0.022738,0.023559,0.023045,0.023123,0.023544,0.023282,0.021747,0.02352,0.023491,0.023509,0.021849,0.022265,0.023335,0.023574,0.02306,0.021747,0.022909,0.023556,0.023634,0.022025,0.023157,0.02374,0.02374,0.023515,0.023526,0.02352,0.02374,0.0238,0.023547,0.02352,0.023568,0.023547,0.023523,0.023547,0.02352,0.02352,0.02352,0.023562,0.023592,0.02352,0.023485,0.023535,0.023506,0.023535,0.023523,0.023506,0.023515,0.02352,0.023349,0.023494,0.023512,0.02352,0.023515,0.023529,0.023571,0.023532,0.023529,0.02355,0.023523,0.023556,0.023512,0.023509,0.023512,0.023077,0.023631,0.023512,0.023523,0.023051,0.023323,0.023497,0.023323,0.023497,0.023323,0.023649,0.021988,0.023874,0.024056,0.024047,0.022236,0.022494,0.023045,0.023709,0.023673,0.022106,0.022423,0.022816,0.022106,0.022106,0.022856,0.022881,0.022106,0.022853,0.023314,0.023755,0.023755,0.022867,0.022106,0.022867,0.022875,0.023146,0.023146,0.02364,0.023186,0.022856,0.022521,0.023598],[0.22963,0.167719,0.193548,0.163301,0.172702,0.137778,0.141985,0.185075,0.181287,0.14374,0.148325,0.126016,0.16188,0.158163,0.155779,0.146919,0.17782,0.201735,0.146226,0.14554,0.173184,0.164602,0.192746,0.152961,0.141337,0.108581,0.137067,0.146572,0.162304,0.174321,0.172063,0.147502,0.196617,0.11078,0.111779,0.192547,0.180934,0.117202,0.128988,0.220379,0.162304,0.17971,0.179537,0.145997,0.188832,0.174976,0.116323,0.147736,0.147736,0.147736,0.147736,0.147736,0.13985,0.131821,0.155518,0.154613,0.149278,0.127923,0.112933,0.156303,0.005348,0.108392,0.144635,0.131449,0.153846,0.005376,0.005405,0.153465,0.137472,0.123424,0.123424,0.131171,0.143187,0.138496,0.13229,0.137371,0.142638,0.171745,0.09375,0.09375,0.09375,0.09375,0.141768,0.105204,0.129256,0.169708,0.145768,0.137676,0.150852,0.129256,0.143297,0.148562,0.157627,0.12326,0.148919,0.142529,0.09774,0.123097,0.14554,0.168478,0.10659,0.148919,0.005348,0.005348,0.142967,0.137982,0.129798,0.005348,0.005348,0.145426,0.14486,0.161458,0.141985,0.164166,0.14242,0.199357,0.14554,0.144973,0.143629,0.143629,0.143077,0.165775,0.123342,0.123342,0.123342,0.128011,0.122935,0.131356,0.122935,0.128011,0.143852,0.143852,0.131171,0.122935,0.122935,0.146572,0.123917,0.122935,0.122935,0.125846,0.128011,0.122935,0.126016,0.122935,0.14006,0.122935,0.125846,0.090204,0.116468,0.111981,0.111981,0.111981,0.111981,0.111981,0.100922,0.112591,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.348968,0.258693,0.291536,0.271533,0.280967,0.263083,0.182711,0.24282,0.230483,0.196617,0.269956,0.278443,0.202835,0.194561,0.2325,0.19195,0.283537,0.281818,0.197872,0.239382,0.245059,0.218566,0.276374,0.190965,0.17971,0.19375,0.184891,0.245707,0.267241,0.276374,0.266094,0.214286,0.282246,0.164311,0.262341,0.266858,0.238462,0.185075,0.186373,0.302932,0.239382,0.231343,0.234257,0.204846,0.259777,0.280967,0.168022,0.184891,0.184891,0.184891,0.184891,0.184891,0.182711,0.215278,0.198083,0.268398,0.214039,0.178161,0.168478,0.245383,0.203279,0.142638,0.212329,0.210884,0.199143,0.111244,0.133047,0.237245,0.185075,0.209696,0.212815,0.209459,0.216531,0.218824,0.215527,0.216028,0.220118,0.276374,0.159931,0.159931,0.159931,0.159931,0.213058,0.180583,0.189602,0.21831,0.19893,0.173994,0.218566,0.210169,0.212087,0.183613,0.199571,0.233962,0.218566,0.195996,0.126617,0.165923,0.216028,0.241558,0.132384,0.194357,0.180407,0.180407,0.210884,0.209696,0.209932,0.180407,0.186373,0.220118,0.211364,0.214781,0.217036,0.21831,0.212329,0.275148,0.211124,0.211124,0.215029,0.216279,0.210884,0.233962,0.208989,0.208754,0.208989,0.208287,0.208054,0.208754,0.210407,0.208287,0.215777,0.216279,0.208054,0.209932,0.208754,0.219599,0.210646,0.211364,0.214039,0.212571,0.216028,0.212087,0.211124,0.209932,0.185259,0.205525,0.239382,0.211845,0.181996,0.206208,0.208054,0.206208,0.208054,0.206208,0.206897,0.204171,0.206208,0.208054,0.208054,0.139013,0.14486,0.149038,0.166517,0.163015,0.127397,0.125084,0.146688,0.127397,0.127397,0.147152,0.146572,0.127397,0.146572,0.16548,0.207358,0.208054,0.147268,0.127397,0.147268,0.147854,0.164311,0.164311,0.191753,0.164602,0.148207,0.124916,0.155909],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var17_df_closseness, by=list(var17_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var17_df_closseness, by=list(var17_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-454d7fca5a2381451ae1" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-454d7fca5a2381451ae1">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000118\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001235\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8.2e-05\" data-max=\"0.000364\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000838\" data-max=\"0.001876\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.6e-05\" data-max=\"0.00027\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022031\" data-max=\"0.024403\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.3e-05\" data-max=\"0.004085\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.22963\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.015236\" data-max=\"0.067679\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.155909\" data-max=\"0.348968\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003078\" data-max=\"0.0503\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003883\" data-max=\"0.139638\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[0.000126,0.000122,0.000126,0.000128,0.000125,0.000128,0.000127,0.000131,0.000127,0.000126,0.000129,0.000118,0.000124,0.000128,0.000128,0.000123,0.000127,0.000127,0.00013,0.000126,0.000128,0.000126,0.000126],[2e-06,3e-06,null,null,null,null,0,null,null,null,null,2.2e-05,1e-06,0,1e-06,3e-06,null,null,null,2e-06,null,1e-06,1e-06],[0.000852,0.000251,0.000782,0.000902,0.000763,0.00084,0.000793,0.001041,0.001036,0.000677,0.000923,0.000762,0.000908,0.000829,0.000875,0.000845,2.9e-05,0.000906,0.001235,0.000517,0.001085,0.000574,0.000744],[0.000216,0.000301,null,null,null,null,0.000135,null,null,null,null,0.000224,8.2e-05,0.000119,0.000117,0.000164,null,null,null,0.000364,null,0.000252,8.6e-05],[0.001261,0.000874,0.001287,0.001391,0.000982,0.001319,0.001248,0.001567,0.001486,0.001287,0.001486,0.001056,0.001145,0.001458,0.001483,0.001145,0.000838,0.001299,0.001876,0.000993,0.001515,0.001125,0.001157],[0.00027,0.000135,null,null,null,null,0.000124,null,null,null,null,0.000215,0.00014,3.3e-05,6e-05,0.000249,null,null,null,1.7e-05,null,1.6e-05,5.3e-05],[0.023487,0.022773,0.023443,0.023782,0.023247,0.023791,0.023592,0.024403,0.023592,0.023512,0.023904,0.022031,0.023172,0.023858,0.023875,0.022944,0.023598,0.023634,0.024118,0.023391,0.023816,0.023553,0.023527],[0.00041,0.000607,null,null,null,null,3.4e-05,null,null,null,null,0.004085,0.000196,2.4e-05,2.3e-05,0.000624,null,null,null,0.000292,null,0.000214,3.8e-05],[0.158463,0.046712,0.14554,0.167719,0.141985,0.156303,0.147564,0.193548,0.192746,0.125846,0.171745,0.141643,0.168936,0.154189,0.162767,0.157145,0.005348,0.168478,0.22963,0.096098,0.201735,0.106757,0.138353],[0.040212,0.056056,null,null,null,null,0.025139,null,null,null,null,0.041685,0.015236,0.022172,0.021792,0.030436,null,null,null,0.067679,null,0.046898,0.015912],[0.234541,0.162619,0.239382,0.258693,0.182711,0.245383,0.232097,0.291536,0.276374,0.239382,0.276374,0.196432,0.212952,0.271261,0.275862,0.212985,0.155909,0.241558,0.348968,0.184621,0.281818,0.209179,0.215215],[0.0503,0.025172,null,null,null,null,0.023014,null,null,null,null,0.039944,0.026009,0.006168,0.011142,0.046355,null,null,null,0.003215,null,0.003078,0.009831],[0.300585,0.099063,0.286154,0.343173,0.303922,0.323478,0.300896,0.391579,0.295238,0.29108,0.313659,0.266552,0.319611,0.301223,0.316849,0.295625,0.005348,0.306425,0.371257,0.194167,0.314189,0.234219,0.288627],[0.043014,0.125815,null,null,null,null,0.018146,null,null,null,null,0.070696,0.003883,0.033229,0.029748,0.023781,null,null,null,0.139638,null,0.102732,0.011577]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var17_df_closseness <- data.frame(
var17_incloseness,
var17_outcloseness,
var17_totalcloseness,
var17_incloseness_n,
var17_outcloseness_n,
var17_totalcloseness_n,
var17_centr_closeness) %>% round(6)

#Adding type
var17_df_closseness <-cbind(var17_df_closseness, V(var17)$TIPO1)

#Adding names
names(var17_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var17_df_closseness<-var17_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var17_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-84698bb9966ea3a9c52a" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-84698bb9966ea3a9c52a">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001235\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000598\" data-max=\"0.001876\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024403\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.22963\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.111244\" data-max=\"0.348968\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[0.00013,0.000128,0.000131,0.000128,0.000128,0.000128,0.000125,0.000127,0.000123,0.000127,0.000128,0.000129,0.000123,0.000124,0.000126,0.000127,0.000129,0.000128,0.000122,0.000126,0.000127,0.000125,0.000127,0.000117,0.000116,0.000126,0.00012,0.000127,0.000128,0.000128,0.000128,0.000126,0.000128,0.00012,0.000128,0.000124,0.000127,0.000125,0.000125,0.000126,0.000127,0.000125,0.000126,0.000127,0.000125,0.000128,0.000116,0.000124,0.000124,0.000124,0.000124,0.000124,0.000123,0.000125,0.000122,0.000128,0.000127,0.00012,0.000121,0.000128,0.00013,0.000119,0.000127,0.000126,0.000125,2.9e-05,0.000118,0.000126,0.000122,0.000126,0.000126,0.000127,0.000126,0.000127,0.000127,0.000126,0.000127,0.000129,0.000122,0.000122,0.000122,0.000122,0.000127,0.000124,0.000124,0.000127,0.000125,0.000117,0.000126,0.000126,0.000126,0.000117,0.00012,0.000125,0.000127,0.000124,0.000117,0.000123,0.000127,0.000127,0.000118,0.000125,0.000128,0.000128,0.000126,0.000126,0.000126,0.000128,0.000128,0.000127,0.000126,0.000127,0.000127,0.000126,0.000127,0.000126,0.000126,0.000126,0.000127,0.000127,0.000126,0.000126,0.000127,0.000126,0.000127,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000127,0.000126,0.000126,0.000126,0.000124,0.000127,0.000126,0.000126,0.000124,0.000125,0.000126,0.000125,0.000126,0.000125,0.000127,0.000118,0.000128,0.000129,0.000129,0.00012,0.000121,0.000124,0.000127,0.000127,0.000119,0.000121,0.000123,0.000119,0.000119,0.000123,0.000123,0.000119,0.000123,0.000125,0.000128,0.000128,0.000123,0.000119,0.000123,0.000123,0.000124,0.000124,0.000127,0.000125,0.000123,0.000121,0.000127],[0.001235,0.000902,0.001041,0.000878,0.000929,0.000741,0.000763,0.000995,0.000975,0.000773,0.000797,0.000678,0.00087,0.00085,0.000838,0.00079,0.000956,0.001085,0.000786,0.000782,0.000931,0.000885,0.001036,0.000822,0.00076,0.000584,0.000737,0.000788,0.000873,0.000937,0.000925,0.000793,0.001057,0.000596,0.000601,0.001035,0.000973,0.00063,0.000693,0.001185,0.000873,0.000966,0.000965,0.000785,0.001015,0.000941,0.000625,0.000794,0.000794,0.000794,0.000794,0.000794,0.000752,0.000709,0.000836,0.000831,0.000803,0.000688,0.000607,0.00084,2.9e-05,0.000583,0.000778,0.000707,0.000827,2.9e-05,2.9e-05,0.000825,0.000739,0.000664,0.000664,0.000705,0.00077,0.000745,0.000711,0.000739,0.000767,0.000923,0.000504,0.000504,0.000504,0.000504,0.000762,0.000566,0.000695,0.000912,0.000784,0.00074,0.000811,0.000695,0.00077,0.000799,0.000847,0.000663,0.000801,0.000766,0.000525,0.000662,0.000782,0.000906,0.000573,0.000801,2.9e-05,2.9e-05,0.000769,0.000742,0.000698,2.9e-05,2.9e-05,0.000782,0.000779,0.000868,0.000763,0.000883,0.000766,0.001072,0.000782,0.000779,0.000772,0.000772,0.000769,0.000891,0.000663,0.000663,0.000663,0.000688,0.000661,0.000706,0.000661,0.000688,0.000773,0.000773,0.000705,0.000661,0.000661,0.000788,0.000666,0.000661,0.000661,0.000677,0.000688,0.000661,0.000678,0.000661,0.000753,0.000661,0.000677,0.000485,0.000626,0.000602,0.000602,0.000602,0.000602,0.000602,0.000543,0.000605,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001876,0.001391,0.001567,0.00146,0.001511,0.001414,0.000982,0.001305,0.001239,0.001057,0.001451,0.001497,0.001091,0.001046,0.00125,0.001032,0.001524,0.001515,0.001064,0.001287,0.001318,0.001175,0.001486,0.001027,0.000966,0.001042,0.000994,0.001321,0.001437,0.001486,0.001431,0.001152,0.001517,0.000883,0.00141,0.001435,0.001282,0.000995,0.001002,0.001629,0.001287,0.001244,0.001259,0.001101,0.001397,0.001511,0.000903,0.000994,0.000994,0.000994,0.000994,0.000994,0.000982,0.001157,0.001065,0.001443,0.001151,0.000958,0.000906,0.001319,0.001093,0.000767,0.001142,0.001134,0.001071,0.000598,0.000715,0.001276,0.000995,0.001127,0.001144,0.001126,0.001164,0.001176,0.001159,0.001161,0.001183,0.001486,0.00086,0.00086,0.00086,0.00086,0.001145,0.000971,0.001019,0.001174,0.00107,0.000935,0.001175,0.00113,0.00114,0.000987,0.001073,0.001258,0.001175,0.001054,0.000681,0.000892,0.001161,0.001299,0.000712,0.001045,0.00097,0.00097,0.001134,0.001127,0.001129,0.00097,0.001002,0.001183,0.001136,0.001155,0.001167,0.001174,0.001142,0.001479,0.001135,0.001135,0.001156,0.001163,0.001134,0.001258,0.001124,0.001122,0.001124,0.00112,0.001119,0.001122,0.001131,0.00112,0.00116,0.001163,0.001119,0.001129,0.001122,0.001181,0.001133,0.001136,0.001151,0.001143,0.001161,0.00114,0.001135,0.001129,0.000996,0.001105,0.001287,0.001139,0.000978,0.001109,0.001119,0.001109,0.001119,0.001109,0.001112,0.001098,0.001109,0.001119,0.001119,0.000747,0.000779,0.000801,0.000895,0.000876,0.000685,0.000672,0.000789,0.000685,0.000685,0.000791,0.000788,0.000685,0.000788,0.00089,0.001115,0.001119,0.000792,0.000685,0.000792,0.000795,0.000883,0.000883,0.001031,0.000885,0.000797,0.000672,0.000838],[0.024118,0.023782,0.024403,0.023849,0.023865,0.023858,0.023247,0.023703,0.022915,0.023604,0.023874,0.023907,0.022889,0.023034,0.0235,0.023547,0.023901,0.023816,0.022724,0.023443,0.023568,0.023195,0.023592,0.021852,0.021643,0.023518,0.022372,0.023577,0.02384,0.023831,0.023861,0.023512,0.023865,0.02238,0.023843,0.023003,0.023685,0.023224,0.023204,0.023461,0.023544,0.023311,0.023411,0.023682,0.023311,0.023877,0.021656,0.02314,0.02314,0.02314,0.02314,0.02314,0.022839,0.023323,0.022628,0.023843,0.023547,0.022342,0.022559,0.023791,0.024247,0.02213,0.023535,0.023518,0.023253,0.005348,0.021934,0.023438,0.022655,0.02347,0.023518,0.023538,0.023526,0.023559,0.023571,0.023523,0.02355,0.023904,0.022738,0.022738,0.022738,0.022738,0.023559,0.023045,0.023123,0.023544,0.023282,0.021747,0.02352,0.023491,0.023509,0.021849,0.022265,0.023335,0.023574,0.02306,0.021747,0.022909,0.023556,0.023634,0.022025,0.023157,0.02374,0.02374,0.023515,0.023526,0.02352,0.02374,0.0238,0.023547,0.02352,0.023568,0.023547,0.023523,0.023547,0.02352,0.02352,0.02352,0.023562,0.023592,0.02352,0.023485,0.023535,0.023506,0.023535,0.023523,0.023506,0.023515,0.02352,0.023349,0.023494,0.023512,0.02352,0.023515,0.023529,0.023571,0.023532,0.023529,0.02355,0.023523,0.023556,0.023512,0.023509,0.023512,0.023077,0.023631,0.023512,0.023523,0.023051,0.023323,0.023497,0.023323,0.023497,0.023323,0.023649,0.021988,0.023874,0.024056,0.024047,0.022236,0.022494,0.023045,0.023709,0.023673,0.022106,0.022423,0.022816,0.022106,0.022106,0.022856,0.022881,0.022106,0.022853,0.023314,0.023755,0.023755,0.022867,0.022106,0.022867,0.022875,0.023146,0.023146,0.02364,0.023186,0.022856,0.022521,0.023598],[0.22963,0.167719,0.193548,0.163301,0.172702,0.137778,0.141985,0.185075,0.181287,0.14374,0.148325,0.126016,0.16188,0.158163,0.155779,0.146919,0.17782,0.201735,0.146226,0.14554,0.173184,0.164602,0.192746,0.152961,0.141337,0.108581,0.137067,0.146572,0.162304,0.174321,0.172063,0.147502,0.196617,0.11078,0.111779,0.192547,0.180934,0.117202,0.128988,0.220379,0.162304,0.17971,0.179537,0.145997,0.188832,0.174976,0.116323,0.147736,0.147736,0.147736,0.147736,0.147736,0.13985,0.131821,0.155518,0.154613,0.149278,0.127923,0.112933,0.156303,0.005348,0.108392,0.144635,0.131449,0.153846,0.005376,0.005405,0.153465,0.137472,0.123424,0.123424,0.131171,0.143187,0.138496,0.13229,0.137371,0.142638,0.171745,0.09375,0.09375,0.09375,0.09375,0.141768,0.105204,0.129256,0.169708,0.145768,0.137676,0.150852,0.129256,0.143297,0.148562,0.157627,0.12326,0.148919,0.142529,0.09774,0.123097,0.14554,0.168478,0.10659,0.148919,0.005348,0.005348,0.142967,0.137982,0.129798,0.005348,0.005348,0.145426,0.14486,0.161458,0.141985,0.164166,0.14242,0.199357,0.14554,0.144973,0.143629,0.143629,0.143077,0.165775,0.123342,0.123342,0.123342,0.128011,0.122935,0.131356,0.122935,0.128011,0.143852,0.143852,0.131171,0.122935,0.122935,0.146572,0.123917,0.122935,0.122935,0.125846,0.128011,0.122935,0.126016,0.122935,0.14006,0.122935,0.125846,0.090204,0.116468,0.111981,0.111981,0.111981,0.111981,0.111981,0.100922,0.112591,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.348968,0.258693,0.291536,0.271533,0.280967,0.263083,0.182711,0.24282,0.230483,0.196617,0.269956,0.278443,0.202835,0.194561,0.2325,0.19195,0.283537,0.281818,0.197872,0.239382,0.245059,0.218566,0.276374,0.190965,0.17971,0.19375,0.184891,0.245707,0.267241,0.276374,0.266094,0.214286,0.282246,0.164311,0.262341,0.266858,0.238462,0.185075,0.186373,0.302932,0.239382,0.231343,0.234257,0.204846,0.259777,0.280967,0.168022,0.184891,0.184891,0.184891,0.184891,0.184891,0.182711,0.215278,0.198083,0.268398,0.214039,0.178161,0.168478,0.245383,0.203279,0.142638,0.212329,0.210884,0.199143,0.111244,0.133047,0.237245,0.185075,0.209696,0.212815,0.209459,0.216531,0.218824,0.215527,0.216028,0.220118,0.276374,0.159931,0.159931,0.159931,0.159931,0.213058,0.180583,0.189602,0.21831,0.19893,0.173994,0.218566,0.210169,0.212087,0.183613,0.199571,0.233962,0.218566,0.195996,0.126617,0.165923,0.216028,0.241558,0.132384,0.194357,0.180407,0.180407,0.210884,0.209696,0.209932,0.180407,0.186373,0.220118,0.211364,0.214781,0.217036,0.21831,0.212329,0.275148,0.211124,0.211124,0.215029,0.216279,0.210884,0.233962,0.208989,0.208754,0.208989,0.208287,0.208054,0.208754,0.210407,0.208287,0.215777,0.216279,0.208054,0.209932,0.208754,0.219599,0.210646,0.211364,0.214039,0.212571,0.216028,0.212087,0.211124,0.209932,0.185259,0.205525,0.239382,0.211845,0.181996,0.206208,0.208054,0.206208,0.208054,0.206208,0.206897,0.204171,0.206208,0.208054,0.208054,0.139013,0.14486,0.149038,0.166517,0.163015,0.127397,0.125084,0.146688,0.127397,0.127397,0.147152,0.146572,0.127397,0.146572,0.16548,0.207358,0.208054,0.147268,0.127397,0.147268,0.147854,0.164311,0.164311,0.191753,0.164602,0.148207,0.124916,0.155909],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var17_df_closseness, by=list(var17_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var17_df_closseness, by=list(var17_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-3af2e33ef06aa280b90d" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-3af2e33ef06aa280b90d">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000122\" data-max=\"0.000127\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000474\" data-max=\"0.00072\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00023\" data-max=\"0.000372\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000965\" data-max=\"0.001196\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000166\" data-max=\"0.000206\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022637\" data-max=\"0.023578\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000214\" data-max=\"0.00206\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.088076\" data-max=\"0.133942\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.042842\" data-max=\"0.069142\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.179478\" data-max=\"0.222498\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.030888\" data-max=\"0.038277\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.173776\" data-max=\"0.2725\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.079091\" data-max=\"0.135432\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[0.000127,0.000122],[1e-06,1.1e-05],[0.00072,0.000474],[0.00023,0.000372],[0.001196,0.000965],[0.000166,0.000206],[0.023578,0.022637],[0.000214,0.00206],[0.133942,0.088076],[0.042842,0.069142],[0.222498,0.179478],[0.030888,0.038277],[0.2725,0.173776],[0.079091,0.135432]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var17_df_closseness <- data.frame(
var17_incloseness,
var17_outcloseness,
var17_totalcloseness,
var17_incloseness_n,
var17_outcloseness_n,
var17_totalcloseness_n,
var17_centr_closeness) %>% round(6)

#Adding type
var17_df_closseness <-cbind(var17_df_closseness, V(var17)$TIPO2)

#Adding names
names(var17_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var17_df_closseness<-var17_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var17_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-eb560ac9f173bf13dd11" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-eb560ac9f173bf13dd11">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001235\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000598\" data-max=\"0.001876\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024403\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.22963\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.111244\" data-max=\"0.348968\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[0.00013,0.000128,0.000131,0.000128,0.000128,0.000128,0.000125,0.000127,0.000123,0.000127,0.000128,0.000129,0.000123,0.000124,0.000126,0.000127,0.000129,0.000128,0.000122,0.000126,0.000127,0.000125,0.000127,0.000117,0.000116,0.000126,0.00012,0.000127,0.000128,0.000128,0.000128,0.000126,0.000128,0.00012,0.000128,0.000124,0.000127,0.000125,0.000125,0.000126,0.000127,0.000125,0.000126,0.000127,0.000125,0.000128,0.000116,0.000124,0.000124,0.000124,0.000124,0.000124,0.000123,0.000125,0.000122,0.000128,0.000127,0.00012,0.000121,0.000128,0.00013,0.000119,0.000127,0.000126,0.000125,2.9e-05,0.000118,0.000126,0.000122,0.000126,0.000126,0.000127,0.000126,0.000127,0.000127,0.000126,0.000127,0.000129,0.000122,0.000122,0.000122,0.000122,0.000127,0.000124,0.000124,0.000127,0.000125,0.000117,0.000126,0.000126,0.000126,0.000117,0.00012,0.000125,0.000127,0.000124,0.000117,0.000123,0.000127,0.000127,0.000118,0.000125,0.000128,0.000128,0.000126,0.000126,0.000126,0.000128,0.000128,0.000127,0.000126,0.000127,0.000127,0.000126,0.000127,0.000126,0.000126,0.000126,0.000127,0.000127,0.000126,0.000126,0.000127,0.000126,0.000127,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000127,0.000126,0.000126,0.000126,0.000124,0.000127,0.000126,0.000126,0.000124,0.000125,0.000126,0.000125,0.000126,0.000125,0.000127,0.000118,0.000128,0.000129,0.000129,0.00012,0.000121,0.000124,0.000127,0.000127,0.000119,0.000121,0.000123,0.000119,0.000119,0.000123,0.000123,0.000119,0.000123,0.000125,0.000128,0.000128,0.000123,0.000119,0.000123,0.000123,0.000124,0.000124,0.000127,0.000125,0.000123,0.000121,0.000127],[0.001235,0.000902,0.001041,0.000878,0.000929,0.000741,0.000763,0.000995,0.000975,0.000773,0.000797,0.000678,0.00087,0.00085,0.000838,0.00079,0.000956,0.001085,0.000786,0.000782,0.000931,0.000885,0.001036,0.000822,0.00076,0.000584,0.000737,0.000788,0.000873,0.000937,0.000925,0.000793,0.001057,0.000596,0.000601,0.001035,0.000973,0.00063,0.000693,0.001185,0.000873,0.000966,0.000965,0.000785,0.001015,0.000941,0.000625,0.000794,0.000794,0.000794,0.000794,0.000794,0.000752,0.000709,0.000836,0.000831,0.000803,0.000688,0.000607,0.00084,2.9e-05,0.000583,0.000778,0.000707,0.000827,2.9e-05,2.9e-05,0.000825,0.000739,0.000664,0.000664,0.000705,0.00077,0.000745,0.000711,0.000739,0.000767,0.000923,0.000504,0.000504,0.000504,0.000504,0.000762,0.000566,0.000695,0.000912,0.000784,0.00074,0.000811,0.000695,0.00077,0.000799,0.000847,0.000663,0.000801,0.000766,0.000525,0.000662,0.000782,0.000906,0.000573,0.000801,2.9e-05,2.9e-05,0.000769,0.000742,0.000698,2.9e-05,2.9e-05,0.000782,0.000779,0.000868,0.000763,0.000883,0.000766,0.001072,0.000782,0.000779,0.000772,0.000772,0.000769,0.000891,0.000663,0.000663,0.000663,0.000688,0.000661,0.000706,0.000661,0.000688,0.000773,0.000773,0.000705,0.000661,0.000661,0.000788,0.000666,0.000661,0.000661,0.000677,0.000688,0.000661,0.000678,0.000661,0.000753,0.000661,0.000677,0.000485,0.000626,0.000602,0.000602,0.000602,0.000602,0.000602,0.000543,0.000605,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001876,0.001391,0.001567,0.00146,0.001511,0.001414,0.000982,0.001305,0.001239,0.001057,0.001451,0.001497,0.001091,0.001046,0.00125,0.001032,0.001524,0.001515,0.001064,0.001287,0.001318,0.001175,0.001486,0.001027,0.000966,0.001042,0.000994,0.001321,0.001437,0.001486,0.001431,0.001152,0.001517,0.000883,0.00141,0.001435,0.001282,0.000995,0.001002,0.001629,0.001287,0.001244,0.001259,0.001101,0.001397,0.001511,0.000903,0.000994,0.000994,0.000994,0.000994,0.000994,0.000982,0.001157,0.001065,0.001443,0.001151,0.000958,0.000906,0.001319,0.001093,0.000767,0.001142,0.001134,0.001071,0.000598,0.000715,0.001276,0.000995,0.001127,0.001144,0.001126,0.001164,0.001176,0.001159,0.001161,0.001183,0.001486,0.00086,0.00086,0.00086,0.00086,0.001145,0.000971,0.001019,0.001174,0.00107,0.000935,0.001175,0.00113,0.00114,0.000987,0.001073,0.001258,0.001175,0.001054,0.000681,0.000892,0.001161,0.001299,0.000712,0.001045,0.00097,0.00097,0.001134,0.001127,0.001129,0.00097,0.001002,0.001183,0.001136,0.001155,0.001167,0.001174,0.001142,0.001479,0.001135,0.001135,0.001156,0.001163,0.001134,0.001258,0.001124,0.001122,0.001124,0.00112,0.001119,0.001122,0.001131,0.00112,0.00116,0.001163,0.001119,0.001129,0.001122,0.001181,0.001133,0.001136,0.001151,0.001143,0.001161,0.00114,0.001135,0.001129,0.000996,0.001105,0.001287,0.001139,0.000978,0.001109,0.001119,0.001109,0.001119,0.001109,0.001112,0.001098,0.001109,0.001119,0.001119,0.000747,0.000779,0.000801,0.000895,0.000876,0.000685,0.000672,0.000789,0.000685,0.000685,0.000791,0.000788,0.000685,0.000788,0.00089,0.001115,0.001119,0.000792,0.000685,0.000792,0.000795,0.000883,0.000883,0.001031,0.000885,0.000797,0.000672,0.000838],[0.024118,0.023782,0.024403,0.023849,0.023865,0.023858,0.023247,0.023703,0.022915,0.023604,0.023874,0.023907,0.022889,0.023034,0.0235,0.023547,0.023901,0.023816,0.022724,0.023443,0.023568,0.023195,0.023592,0.021852,0.021643,0.023518,0.022372,0.023577,0.02384,0.023831,0.023861,0.023512,0.023865,0.02238,0.023843,0.023003,0.023685,0.023224,0.023204,0.023461,0.023544,0.023311,0.023411,0.023682,0.023311,0.023877,0.021656,0.02314,0.02314,0.02314,0.02314,0.02314,0.022839,0.023323,0.022628,0.023843,0.023547,0.022342,0.022559,0.023791,0.024247,0.02213,0.023535,0.023518,0.023253,0.005348,0.021934,0.023438,0.022655,0.02347,0.023518,0.023538,0.023526,0.023559,0.023571,0.023523,0.02355,0.023904,0.022738,0.022738,0.022738,0.022738,0.023559,0.023045,0.023123,0.023544,0.023282,0.021747,0.02352,0.023491,0.023509,0.021849,0.022265,0.023335,0.023574,0.02306,0.021747,0.022909,0.023556,0.023634,0.022025,0.023157,0.02374,0.02374,0.023515,0.023526,0.02352,0.02374,0.0238,0.023547,0.02352,0.023568,0.023547,0.023523,0.023547,0.02352,0.02352,0.02352,0.023562,0.023592,0.02352,0.023485,0.023535,0.023506,0.023535,0.023523,0.023506,0.023515,0.02352,0.023349,0.023494,0.023512,0.02352,0.023515,0.023529,0.023571,0.023532,0.023529,0.02355,0.023523,0.023556,0.023512,0.023509,0.023512,0.023077,0.023631,0.023512,0.023523,0.023051,0.023323,0.023497,0.023323,0.023497,0.023323,0.023649,0.021988,0.023874,0.024056,0.024047,0.022236,0.022494,0.023045,0.023709,0.023673,0.022106,0.022423,0.022816,0.022106,0.022106,0.022856,0.022881,0.022106,0.022853,0.023314,0.023755,0.023755,0.022867,0.022106,0.022867,0.022875,0.023146,0.023146,0.02364,0.023186,0.022856,0.022521,0.023598],[0.22963,0.167719,0.193548,0.163301,0.172702,0.137778,0.141985,0.185075,0.181287,0.14374,0.148325,0.126016,0.16188,0.158163,0.155779,0.146919,0.17782,0.201735,0.146226,0.14554,0.173184,0.164602,0.192746,0.152961,0.141337,0.108581,0.137067,0.146572,0.162304,0.174321,0.172063,0.147502,0.196617,0.11078,0.111779,0.192547,0.180934,0.117202,0.128988,0.220379,0.162304,0.17971,0.179537,0.145997,0.188832,0.174976,0.116323,0.147736,0.147736,0.147736,0.147736,0.147736,0.13985,0.131821,0.155518,0.154613,0.149278,0.127923,0.112933,0.156303,0.005348,0.108392,0.144635,0.131449,0.153846,0.005376,0.005405,0.153465,0.137472,0.123424,0.123424,0.131171,0.143187,0.138496,0.13229,0.137371,0.142638,0.171745,0.09375,0.09375,0.09375,0.09375,0.141768,0.105204,0.129256,0.169708,0.145768,0.137676,0.150852,0.129256,0.143297,0.148562,0.157627,0.12326,0.148919,0.142529,0.09774,0.123097,0.14554,0.168478,0.10659,0.148919,0.005348,0.005348,0.142967,0.137982,0.129798,0.005348,0.005348,0.145426,0.14486,0.161458,0.141985,0.164166,0.14242,0.199357,0.14554,0.144973,0.143629,0.143629,0.143077,0.165775,0.123342,0.123342,0.123342,0.128011,0.122935,0.131356,0.122935,0.128011,0.143852,0.143852,0.131171,0.122935,0.122935,0.146572,0.123917,0.122935,0.122935,0.125846,0.128011,0.122935,0.126016,0.122935,0.14006,0.122935,0.125846,0.090204,0.116468,0.111981,0.111981,0.111981,0.111981,0.111981,0.100922,0.112591,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.348968,0.258693,0.291536,0.271533,0.280967,0.263083,0.182711,0.24282,0.230483,0.196617,0.269956,0.278443,0.202835,0.194561,0.2325,0.19195,0.283537,0.281818,0.197872,0.239382,0.245059,0.218566,0.276374,0.190965,0.17971,0.19375,0.184891,0.245707,0.267241,0.276374,0.266094,0.214286,0.282246,0.164311,0.262341,0.266858,0.238462,0.185075,0.186373,0.302932,0.239382,0.231343,0.234257,0.204846,0.259777,0.280967,0.168022,0.184891,0.184891,0.184891,0.184891,0.184891,0.182711,0.215278,0.198083,0.268398,0.214039,0.178161,0.168478,0.245383,0.203279,0.142638,0.212329,0.210884,0.199143,0.111244,0.133047,0.237245,0.185075,0.209696,0.212815,0.209459,0.216531,0.218824,0.215527,0.216028,0.220118,0.276374,0.159931,0.159931,0.159931,0.159931,0.213058,0.180583,0.189602,0.21831,0.19893,0.173994,0.218566,0.210169,0.212087,0.183613,0.199571,0.233962,0.218566,0.195996,0.126617,0.165923,0.216028,0.241558,0.132384,0.194357,0.180407,0.180407,0.210884,0.209696,0.209932,0.180407,0.186373,0.220118,0.211364,0.214781,0.217036,0.21831,0.212329,0.275148,0.211124,0.211124,0.215029,0.216279,0.210884,0.233962,0.208989,0.208754,0.208989,0.208287,0.208054,0.208754,0.210407,0.208287,0.215777,0.216279,0.208054,0.209932,0.208754,0.219599,0.210646,0.211364,0.214039,0.212571,0.216028,0.212087,0.211124,0.209932,0.185259,0.205525,0.239382,0.211845,0.181996,0.206208,0.208054,0.206208,0.208054,0.206208,0.206897,0.204171,0.206208,0.208054,0.208054,0.139013,0.14486,0.149038,0.166517,0.163015,0.127397,0.125084,0.146688,0.127397,0.127397,0.147152,0.146572,0.127397,0.146572,0.16548,0.207358,0.208054,0.147268,0.127397,0.147268,0.147854,0.164311,0.164311,0.191753,0.164602,0.148207,0.124916,0.155909],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var17_df_closseness, by=list(var17_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var17_df_closseness, by=list(var17_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-d29fc44fc07db30d940d" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-d29fc44fc07db30d940d">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000122\" data-max=\"0.000128\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000472\" data-max=\"0.000863\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000117\" data-max=\"0.000374\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00096\" data-max=\"0.001458\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6.1e-05\" data-max=\"0.000203\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022623\" data-max=\"0.023844\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8.6e-05\" data-max=\"0.00207\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.087734\" data-max=\"0.160444\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.021861\" data-max=\"0.069633\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.178616\" data-max=\"0.271129\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.011272\" data-max=\"0.037759\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172406\" data-max=\"0.308329\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.029295\" data-max=\"0.135782\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[0.000128,0.000126,0.000122],[1e-06,1e-06,1.1e-05],[0.000863,0.000696,0.000472],[0.000117,0.000234,0.000374],[0.001458,0.001156,0.00096],[6.1e-05,0.000137,0.000203],[0.023844,0.023537,0.022623],[8.6e-05,0.000195,0.00207],[0.160444,0.129509,0.087734],[0.021861,0.043564,0.069633],[0.271129,0.214995,0.178616],[0.011272,0.02541,0.037759],[0.308329,0.266869,0.172406],[0.029295,0.082605,0.135782]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var17_data.RData")
```

