# SNA Closeness 22_CONFIANÇA E) Confia nas informações recebas deste serviço (var20).
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 22_CONFIANÇA E) Confia nas informações recebas deste serviço (var20).

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var20_data.RData")
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
#var20<-simplify(var20) #Simplify
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
V(var20)$incloseness <- closeness(var20, mode = "in", weights = E(var20)$var20) %>% round(6)
V(var20)$outcloseness <- closeness(var20, mode = "out", weights = E(var20)$var20) %>% round(6)
V(var20)$totalcloseness <- closeness(var20, mode = "total", weights = E(var20)$var20) %>% round(4)
```

###Saving to Environment

```r
var20_incloseness<- closeness(var20, mode = "in", weights = E(var20)$var20) %>% round(6)
var20_outcloseness<- closeness(var20, mode = "out", weights = E(var20)$var20) %>% round(6)
var20_totalcloseness<- closeness(var20, mode = "total", weights = E(var20)$var20) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var20_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0001235 0.0001260 0.0001243 0.0001270 0.0001310
```

```r
sd(var20_incloseness)
```

```
## [1] 7.730128e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var20)$incloseness<-closeness(var20, weights = E(var20)$var20, mode="in")

#Get Variable
V(var20)$var20_color_degree<-round(V(var20)$incloseness,6)

#Creating brewer pallette
vertex_var20_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var20)$var20_color_degree)), "RdBu"))(
            length(unique(V(var20)$var20_color_degree)))

#Saving as Vertex properties 
V(var20)$vertex_var20_color_degree<-
  vertex_var20_color_degree[as.numeric(
  cut(V(var20)$var20_color_degree,
      breaks=length(unique(V(var20)$var20_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var20, es=E(var20), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var20))
maxC <- rep(Inf, vcount(var20))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var20, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var20)$weight)


#PLotting
plot(var20, 
     layout=co,
     edge.color=V(var20)$vertex_var20_color_degree[edge.start],
     edge.arrow.size=closeness(var20, weights = E(var20)$var20, mode="in"),
     edge.width=E(var20)$weight/mean(E(var20)$weight),
     edge.curved = TRUE,
     vertex.color=V(var20)$vertex_var20_color_degree,
     vertex.size=closeness(var20, weights = E(var20)$var20, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var20,"LABEL_COR"),
     vertex.label.cex=(closeness(var20, weights = E(var20)$var20, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var20)$var20_color_degree
b<-V(var20)$vertex_var20_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 22_CONFIANÇA E) Confia nas informações recebas deste serviço (var20).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var20, mode="in", weights = E(var20)$var20)), 
             sd(closeness(var20, mode="in", weights = E(var20)$var20))
             )
       )
```

![](22_CONFIANÇA_E_Confia_nas_informações_recebidas_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var20_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0005560 0.0007250 0.0006002 0.0007865 0.0011170
```

```r
sd(var20_outcloseness)
```

```
## [1] 0.0003052081
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var20)$outcloseness<-closeness(var20, weights = E(var20)$var20, mode="out")

#Get Variable
V(var20)$var20_color_degree<-round(V(var20)$outcloseness,6)

#Creating brewer pallette
vertex_var20_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var20)$var20_color_degree)), "RdBu"))(
            length(unique(V(var20)$var20_color_degree)))

#Saving as Vertex properties 
V(var20)$vertex_var20_color_degree<-
  vertex_var20_color_degree[as.numeric(
  cut(V(var20)$var20_color_degree,
      breaks=length(unique(V(var20)$var20_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var20, es=E(var20), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var20))
maxC <- rep(Inf, vcount(var20))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var20, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var20)$weight)


#PLotting
plot(var20, 
     layout=co,
     edge.color=V(var20)$vertex_var20_color_degree[edge.start],
     edge.arrow.size=closeness(var20, weights = E(var20)$var20, mode="out"),
     edge.width=E(var20)$weight/2*mean(E(var20)$weight),
     edge.curved = TRUE,
     vertex.color=V(var20)$vertex_var20_color_degree,
     vertex.size=closeness(var20, weights = E(var20)$var20, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var20,"LABEL_COR"),
     vertex.label.cex=closeness(var20, weights = E(var20)$var20, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var20)$var20_color_degree
b<-V(var20)$vertex_var20_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 22_CONFIANÇA E) Confia nas informações recebas deste serviço (var20).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var20, mode="out", weights = E(var20)$var20)), 
             sd(closeness(var20, mode="out", weights = E(var20)$var20))
             )
       )
```

![](22_CONFIANÇA_E_Confia_nas_informações_recebidas_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var20_totalcloseness)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000624 0.000934 0.001080 0.001035 0.001133 0.001689
```

```r
sd(var20_totalcloseness)
```

```
## [1] 0.0001783619
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var20)$allcloseness<-closeness(var20, weights = E(var20)$var20, mode="all")

#Get Variable
V(var20)$var20_color_degree<-round(V(var20)$allcloseness,6)

#Creating brewer pallette
vertex_var20_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var20)$var20_color_degree)), "RdBu"))(
            length(unique(V(var20)$var20_color_degree)))

#Saving as Vertex properties 
V(var20)$vertex_var20_color_degree<-
  vertex_var20_color_degree[as.numeric(
  cut(V(var20)$var20_color_degree,
      breaks=length(unique(V(var20)$var20_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var20, es=E(var20), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var20))
maxC <- rep(Inf, vcount(var20))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var20, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var20)$weight)


#PLotting
plot(var20, 
     layout=co,
     edge.color=V(var20)$vertex_var20_color_degree[edge.start],
     edge.arrow.size=closeness(var20, weights = E(var20)$var20, mode="all"),
     edge.width=E(var20)$weight/2*mean(E(var20)$weight),
     edge.curved = TRUE,
     vertex.color=V(var20)$vertex_var20_color_degree,
     vertex.size=closeness(var20, weights = E(var20)$var20, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var20,"LABEL_COR"),
     vertex.label.cex=(closeness(var20, weights = E(var20)$var20, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var20)$var20_color_degree
b<-V(var20)$vertex_var20_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 22_CONFIANÇA E) Confia nas informações recebas deste serviço (var20).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var20, mode="all", weights = E(var20)$var20)), 
             sd(closeness(var20, mode="all", weights = E(var20)$var20))
             )
       )
```

![](22_CONFIANÇA_E_Confia_nas_informações_recebidas_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var20)$incloseness_n <- closeness(var20, mode = "in",, weights = E(var20)$var20, normalized = T) %>% round(10)
V(var20)$outcloseness_n <- closeness(var20, mode = "out", normalized = T, weights = E(var20)$var20) %>% round(6)
V(var20)$totalcloseness_n <- closeness(var20, mode = "total", normalized = T, weights = E(var20)$var20) %>% round(6)
```

###Saving to Environment

```r
var20_incloseness_n<- closeness(var20, mode = "in", normalized = T, weights = E(var20)$var20) %>% round(6)
var20_outcloseness_n<- closeness(var20, mode = "out", normalized = T, weights = E(var20)$var20) %>% round(6)
var20_totalcloseness_n<- closeness(var20, mode = "total", normalized = T, weights = E(var20)$var20) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var20_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.022970 0.023500 0.023130 0.023580 0.024420
```

```r
sd(var20_incloseness_n)
```

```
## [1] 0.001439842
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var20)$incloseness_n<-closeness(var20, weights = E(var20)$var20, mode="in", normalized = T)

#Get Variable
V(var20)$var20_color_degree<-round(V(var20)$incloseness_n,6)

#Creating brewer pallette
vertex_var20_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var20)$var20_color_degree)), "RdBu"))(
            length(unique(V(var20)$var20_color_degree)))

#Saving as Vertex properties 
V(var20)$vertex_var20_color_degree<-
  vertex_var20_color_degree[as.numeric(
  cut(V(var20)$var20_color_degree,
      breaks=length(unique(V(var20)$var20_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var20, es=E(var20), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var20))
maxC <- rep(Inf, vcount(var20))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var20, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var20)$weight)


#PLotting
plot(var20, 
     layout=co,
     edge.color=V(var20)$vertex_var20_color_degree[edge.start],
     edge.arrow.size=closeness(var20, weights = E(var20)$var20, mode="in",normalized = T),
     edge.width=E(var20)$weight/10*mean(E(var20)$weight),
     edge.curved = TRUE,
     vertex.color=V(var20)$vertex_var20_color_degree,
     vertex.size=(closeness(var20, weights = E(var20)$var20, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var20,"LABEL_COR"),
     vertex.label.cex=closeness(var20, weights = E(var20)$var20, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var20)$var20_color_degree
b<-V(var20)$vertex_var20_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 22_CONFIANÇA E) Confia nas informações recebas deste serviço (var20).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var20, mode="in", weights = E(var20)$var20, normalized = T)), 
             sd(closeness(var20, mode="in", weights = E(var20)$var20, normalized = T))
             )
       )
```

![](22_CONFIANÇA_E_Confia_nas_informações_recebidas_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var20_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.103400 0.134900 0.111600 0.146200 0.207800
```

```r
sd(var20_outcloseness_n)
```

```
## [1] 0.0567851
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var20)$outcloseness_n<-closeness(var20, weights = E(var20)$var20, mode="out", normalized = T)

#Get Variable
V(var20)$var20_color_degree<-round(V(var20)$outcloseness_n,6)

#Creating brewer pallette
vertex_var20_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var20)$var20_color_degree)), "RdBu"))(
            length(unique(V(var20)$var20_color_degree)))

#Saving as Vertex properties 
V(var20)$vertex_var20_color_degree<-
  vertex_var20_color_degree[as.numeric(
  cut(V(var20)$var20_color_degree,
      breaks=length(unique(V(var20)$var20_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var20, es=E(var20), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var20))
maxC <- rep(Inf, vcount(var20))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var20, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var20)$weight)


#PLotting
plot(var20, 
     layout=co,
     edge.color=V(var20)$vertex_var20_color_degree[edge.start],
     edge.arrow.size=closeness(var20, weights = E(var20)$var20, mode="out",normalized = T),
     edge.width=E(var20)$weight/10*mean(E(var20)$weight),
     edge.curved = TRUE,
     vertex.color=V(var20)$vertex_var20_color_degree,
     vertex.size=(closeness(var20, weights = E(var20)$var20, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var20,"LABEL_COR"),
     vertex.label.cex=closeness(var20, weights = E(var20)$var20, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var20)$var20_color_degree
b<-V(var20)$vertex_var20_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 22_CONFIANÇA E) Confia nas informações recebas deste serviço (var20).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var20, mode="out", weights = E(var20)$var20, normalized = T)), 
             sd(closeness(var20, mode="out", weights = E(var20)$var20, normalized = T))
             )
       )
```

![](22_CONFIANÇA_E_Confia_nas_informações_recebidas_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var20_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1160  0.1737  0.2009  0.1926  0.2108  0.3142
```

```r
sd(var20_totalcloseness_n)
```

```
## [1] 0.03317454
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var20)$allcloseness_n<-closeness(var20, weights = E(var20)$var20, mode="all", normalized = T)

#Get Variable
V(var20)$var20_color_degree<-round(V(var20)$allcloseness_n,6)

#Creating brewer pallette
vertex_var20_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var20)$var20_color_degree)), "RdBu"))(
            length(unique(V(var20)$var20_color_degree)))

#Saving as Vertex properties 
V(var20)$vertex_var20_color_degree<-
  vertex_var20_color_degree[as.numeric(
  cut(V(var20)$var20_color_degree,
      breaks=length(unique(V(var20)$var20_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var20, es=E(var20), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var20))
maxC <- rep(Inf, vcount(var20))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var20, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var20)$weight)


#PLotting
plot(var20, 
     layout=co,
     edge.color=V(var20)$vertex_var20_color_degree[edge.start],
     edge.arrow.size=closeness(var20, weights = E(var20)$var20, mode="all",normalized = T),
     edge.width=E(var20)$weight/10*mean(E(var20)$weight),
     edge.curved = TRUE,
     vertex.color=V(var20)$vertex_var20_color_degree,
     vertex.size=(closeness(var20, weights = E(var20)$var20, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var20,"LABEL_COR"),
     vertex.label.cex=closeness(var20, weights = E(var20)$var20, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var20)$var20_color_degree
b<-V(var20)$vertex_var20_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 22_CONFIANÇA E) Confia nas informações recebas deste serviço (var20).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var20, mode="all", weights = E(var20)$var20, normalized = T)), 
             sd(closeness(var20, mode="all", weights = E(var20)$var20, normalized = T))
             )
       )
```

![](22_CONFIANÇA_E_Confia_nas_informações_recebidas_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var20)$incloseness_n <- closeness(var20, weights = E(var20)$var20, mode = "in", normalized = T) %>% round(6)
V(var20)$outcloseness_n <- closeness(var20, weights = E(var20)$var20, mode = "out", normalized = T) %>% round(6)
V(var20)$totalcloseness_n <- closeness(var20, weights = E(var20)$var20, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var20)$var20_centr_closeness<- centralization.closeness(var20)$res
var20_centr_closeness<- centralization.closeness(var20)$res
var20_centr_closeness_all<- centralization.closeness(var20)
```

###Centralization

```r
var20_centr_closeness_all$centralization
```

```
## [1] 0.1624897
```

###Theoretical Max

```r
var20_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var20)$var20_centr_closeness<- centralization.closeness(var20)$res

#Get Variable
V(var20)$var20_color_degree<-round(V(var20)$var20_centr_closeness,6)

#Creating brewer pallette
vertex_var20_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var20)$var20_color_degree)), "Spectral"))(
            length(unique(V(var20)$var20_color_degree)))

#Saving as Vertex properties 
V(var20)$vertex_var20_color_degree<-
  vertex_var20_color_degree[as.numeric(
  cut(V(var20)$var20_color_degree,
      breaks=length(unique(V(var20)$var20_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var20, es=E(var20), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var20))
maxC <- rep(Inf, vcount(var20))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var20, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var20)$weight)


#PLotting
plot(var20, 
     layout=co,
     edge.color=V(var20)$vertex_var20_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var20)$res,
     edge.width=E(var20)$weight/10*mean(E(var20)$weight),
     edge.curved = TRUE,
     vertex.color=V(var20)$vertex_var20_color_degree,
     vertex.size=centralization.closeness(var20)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var20,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var20)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var20)$var20_color_degree
b<-V(var20)$vertex_var20_color_degree
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
  title("Network Centralization Closeness - 22_CONFIANÇA E) Confia nas informações recebas deste serviço (var20).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var20)$res), 
             sd(centralization.closeness(var20)$res)
             )
       )
```

![](22_CONFIANÇA_E_Confia_nas_informações_recebidas_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var20_incloseness<- closeness(var20, weights = E(var20)$var20, mode = "in") %>% round(6)
var20_outcloseness<- closeness(var20, weights = E(var20)$var20, mode = "out") %>% round(6)
var20_totalcloseness<- closeness(var20, weights = E(var20)$var20, mode = "total") %>% round(6)
var20_incloseness_n<- closeness(var20,weights = E(var20)$var20, mode = "in", normalized = T) %>% round(6)
var20_outcloseness_n<- closeness(var20,weights = E(var20)$var20, mode = "out", normalized = T) %>% round(6)
var20_totalcloseness_n<- closeness(var20,weights = E(var20)$var20, mode = "total", normalized = T) %>% round(6)
var20_centr_closeness <- centralization.closeness(var20)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var20_df_closseness <- data.frame(
var20_incloseness,
var20_outcloseness,
var20_totalcloseness,
var20_incloseness_n,
var20_outcloseness_n,
var20_totalcloseness_n,
var20_centr_closeness) %>% round(6)

#Adding type
var20_df_closseness <-cbind(var20_df_closseness, V(var20)$LABEL_COR)

#Adding names
names(var20_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var20_df_closseness<-var20_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var20_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-b80c56778e83aa160d7b" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-b80c56778e83aa160d7b">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001117\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000624\" data-max=\"0.001689\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024425\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.207821\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.116032\" data-max=\"0.314189\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[0.00013,0.000127,0.000131,0.000127,0.000127,0.000127,0.000125,0.000129,0.000123,0.000129,0.000127,0.000127,0.000122,0.000123,0.000126,0.000125,0.000127,0.000128,0.000122,0.000125,0.000127,0.000125,0.000127,0.000118,0.000116,0.000127,0.00012,0.000127,0.000127,0.000127,0.000127,0.000126,0.000127,0.00012,0.000127,0.000123,0.000129,0.000124,0.000123,0.000127,0.000127,0.000126,0.000125,0.000129,0.000127,0.000127,0.000116,0.000124,0.000124,0.000124,0.000124,0.000124,0.000121,0.000126,0.00012,0.000127,0.000127,0.000121,0.000121,0.000128,0.00013,0.000119,0.000126,0.000126,0.000124,2.9e-05,0.000117,0.000127,0.000121,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000126,0.000128,0.000119,0.000119,0.000119,0.000119,0.000127,0.000125,0.000125,0.000127,0.000125,0.000116,0.000126,0.000126,0.000126,0.000117,0.000119,0.000125,0.000127,0.000124,0.000116,0.000123,0.000127,0.000127,0.000119,0.000124,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000127,0.000127,0.000126,0.000127,0.000127,0.000127,0.000127,0.000126,0.000126,0.000126,0.000126,0.000127,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000124,0.000128,0.000127,0.000127,0.000121,0.000125,0.000127,0.000125,0.000127,0.000125,0.000127,0.000118,0.000129,0.00013,0.00013,0.000119,0.000118,0.000125,0.000129,0.000128,0.000118,0.000119,0.000125,0.000118,0.000118,0.000123,0.000123,0.000118,0.000125,0.000123,0.000124,0.000124,0.000125,0.000118,0.000125,0.000123,0.000123,0.000123,0.000124,0.000126,0.000123,0.00012,0.000127],[0.001117,0.001015,0.001046,0.000935,0.000783,0.000725,0.00074,0.00081,0.000923,0.000725,0.000786,0.000682,0.000857,0.000816,0.000818,0.000766,0.000883,0.000796,0.000691,0.000842,0.000794,0.000732,0.000763,0.000815,0.00073,0.000566,0.000797,0.000805,0.000847,0.000837,0.000793,0.00076,0.001004,0.000578,0.000525,0.000732,0.000829,0.000663,0.000731,0.000942,0.000853,0.000928,0.000927,0.000748,0.000733,0.000762,0.00066,0.000766,0.000766,0.000766,0.000766,0.000766,0.000724,0.000704,0.000822,0.000808,0.000767,0.000675,0.000606,0.000841,2.9e-05,0.000628,0.000894,0.000689,0.000846,2.9e-05,2.9e-05,0.000787,0.000711,0.000759,0.000666,0.000724,0.000888,0.00074,0.000686,0.000697,0.000933,0.0008,0.000503,0.000503,0.000503,0.000503,0.000734,0.000562,0.000693,0.000879,0.000752,0.000698,0.000782,0.000692,0.000742,0.000775,0.000795,0.000652,0.000749,0.000737,0.000504,0.000523,0.000688,0.00087,0.00055,0.000787,2.9e-05,2.9e-05,0.00085,0.000736,0.00067,2.9e-05,2.9e-05,0.000752,0.000754,0.000738,0.00085,0.00073,0.000733,0.00086,0.00075,0.000751,0.00074,0.000857,0.00076,0.000799,0.000665,0.000665,0.000665,0.000718,0.000879,0.000673,0.000879,0.000718,0.000826,0.000826,0.000718,0.000663,0.000756,0.000733,0.000669,0.000663,0.000663,0.000717,0.000687,0.000663,0.00067,0.000663,0.000663,0.000663,0.000717,0.000486,0.000583,0.000739,0.000739,0.000739,0.000739,0.000739,0.000568,0.000637,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001689,0.001412,0.001553,0.001282,0.001163,0.001139,0.000965,0.001316,0.001189,0.001232,0.001195,0.001181,0.001087,0.001016,0.001129,0.001093,0.001241,0.001242,0.000881,0.001161,0.001178,0.000962,0.001148,0.001016,0.000927,0.00103,0.001093,0.001156,0.001164,0.0012,0.00114,0.0011,0.001406,0.000851,0.001105,0.000965,0.00128,0.000974,0.00099,0.001279,0.001139,0.001221,0.001232,0.001271,0.001034,0.001152,0.00089,0.001075,0.001075,0.001075,0.001075,0.001075,0.000989,0.001116,0.001053,0.001172,0.00111,0.000907,0.00089,0.00128,0.001136,0.000824,0.001186,0.001074,0.001126,0.000699,0.000685,0.001065,0.001041,0.001121,0.001106,0.001104,0.00119,0.001145,0.001122,0.001106,0.001321,0.001182,0.000639,0.000639,0.000639,0.000639,0.001082,0.001073,0.000982,0.001161,0.001065,0.000892,0.001119,0.001066,0.001068,0.000958,0.001054,0.000943,0.001099,0.001036,0.000679,0.000781,0.001115,0.001175,0.000683,0.000994,0.000934,0.000934,0.001122,0.001089,0.00107,0.000934,0.000953,0.001142,0.001092,0.00112,0.00116,0.001109,0.001106,0.00113,0.00108,0.001074,0.001092,0.001148,0.001082,0.001125,0.0011,0.001098,0.0011,0.001104,0.001252,0.001101,0.001258,0.001104,0.001143,0.001147,0.001098,0.001107,0.001148,0.001114,0.001072,0.001075,0.001086,0.001085,0.001075,0.001087,0.001065,0.001066,0.000842,0.001174,0.00107,0.001091,0.000823,0.001078,0.001094,0.001078,0.001094,0.001078,0.001105,0.000951,0.001079,0.001095,0.001094,0.000742,0.000624,0.000896,0.000943,0.000929,0.000678,0.000666,0.000865,0.000678,0.000678,0.000778,0.000779,0.000678,0.000886,0.000789,0.000798,0.000826,0.000853,0.000678,0.000853,0.000842,0.000793,0.000793,0.000817,0.00094,0.00078,0.000693,0.000834],[0.0241,0.023667,0.024425,0.023601,0.023664,0.023619,0.023265,0.024084,0.02285,0.024015,0.023667,0.023712,0.022691,0.022895,0.023485,0.023314,0.023706,0.023868,0.022639,0.023288,0.023634,0.023212,0.023649,0.021952,0.021573,0.023538,0.022369,0.023646,0.023589,0.023601,0.023634,0.023494,0.023706,0.022358,0.023595,0.022929,0.024012,0.02298,0.022887,0.0237,0.023547,0.023393,0.023332,0.02405,0.02355,0.023667,0.02154,0.022974,0.022974,0.022974,0.022974,0.022974,0.022567,0.023346,0.022407,0.023592,0.023559,0.022431,0.022562,0.023794,0.02426,0.022048,0.023512,0.023503,0.022971,0.005348,0.021726,0.023607,0.022475,0.02347,0.023568,0.023574,0.023577,0.023604,0.023616,0.023497,0.023515,0.023743,0.022138,0.022138,0.022138,0.022138,0.023535,0.023157,0.023338,0.023586,0.023218,0.021578,0.0235,0.023473,0.023488,0.021724,0.022177,0.023244,0.023553,0.023043,0.021578,0.022856,0.023604,0.023658,0.022169,0.02304,0.023411,0.023411,0.023497,0.023494,0.023503,0.023411,0.023613,0.023532,0.023515,0.023616,0.023538,0.023577,0.023589,0.0235,0.023497,0.023497,0.023523,0.023565,0.023509,0.023688,0.023577,0.023559,0.023577,0.023559,0.023553,0.023568,0.023568,0.023355,0.023544,0.023562,0.023568,0.023568,0.023577,0.023607,0.023515,0.0235,0.023518,0.023491,0.023518,0.023506,0.023488,0.023494,0.023106,0.023755,0.023562,0.023529,0.022584,0.02334,0.023544,0.02334,0.023544,0.02334,0.023646,0.021918,0.023904,0.024112,0.024103,0.022067,0.02202,0.023282,0.023904,0.023868,0.021986,0.022204,0.023221,0.021986,0.021986,0.022909,0.022915,0.021986,0.023308,0.022969,0.022983,0.023006,0.023247,0.021986,0.023247,0.022842,0.022918,0.022918,0.023123,0.023367,0.022839,0.022257,0.023619],[0.207821,0.188832,0.194561,0.173994,0.145654,0.13488,0.137574,0.150729,0.171587,0.13488,0.146112,0.126876,0.159383,0.151713,0.152085,0.142529,0.164311,0.147971,0.128453,0.156698,0.147619,0.136164,0.141985,0.151589,0.135766,0.105204,0.148325,0.149758,0.157494,0.155649,0.147502,0.141337,0.186747,0.107577,0.097587,0.136164,0.154101,0.12326,0.135965,0.175141,0.158703,0.172542,0.172382,0.139117,0.136364,0.141768,0.122772,0.142529,0.142529,0.142529,0.142529,0.142529,0.134685,0.130894,0.152835,0.150242,0.142638,0.125506,0.112659,0.156434,0.005348,0.116761,0.16622,0.128187,0.15736,0.005376,0.005405,0.146341,0.13229,0.141123,0.123835,0.134588,0.165187,0.137574,0.12766,0.129707,0.173507,0.1488,0.093608,0.093608,0.093608,0.093608,0.136564,0.104553,0.128809,0.163445,0.13985,0.129888,0.145426,0.128631,0.138085,0.144186,0.147854,0.121252,0.139222,0.137168,0.09375,0.097229,0.128011,0.16188,0.10231,0.146457,0.005348,0.005348,0.158163,0.136865,0.124665,0.005348,0.005348,0.139955,0.140166,0.137269,0.158029,0.135866,0.136264,0.159931,0.13943,0.13964,0.137574,0.159383,0.141445,0.148562,0.123752,0.123752,0.123752,0.133621,0.163445,0.125253,0.163445,0.133621,0.153719,0.153719,0.133621,0.123342,0.14059,0.136264,0.124415,0.123342,0.123342,0.133429,0.127747,0.123342,0.124581,0.123342,0.123342,0.123342,0.133333,0.090423,0.108392,0.137472,0.137472,0.137472,0.137472,0.137472,0.105682,0.118471,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.314189,0.262712,0.28882,0.238462,0.216279,0.211845,0.179537,0.244737,0.221165,0.229064,0.222222,0.219599,0.202174,0.189024,0.209932,0.203279,0.230769,0.231056,0.163877,0.216028,0.219081,0.179018,0.213548,0.189024,0.172382,0.191555,0.203279,0.215029,0.216531,0.223289,0.212087,0.20462,0.261603,0.158298,0.205525,0.179537,0.238156,0.18111,0.184158,0.237852,0.211845,0.227106,0.229064,0.236341,0.192347,0.214286,0.16548,0.2,0.2,0.2,0.2,0.2,0.183976,0.207589,0.195789,0.218054,0.206437,0.168784,0.165628,0.238156,0.211364,0.153213,0.220641,0.199785,0.209459,0.129979,0.127485,0.198083,0.193548,0.20852,0.205752,0.205298,0.221429,0.213058,0.208754,0.205752,0.245707,0.219858,0.118926,0.118926,0.118926,0.118926,0.201299,0.199571,0.182711,0.216028,0.198083,0.165923,0.208054,0.198294,0.198718,0.178161,0.195996,0.175306,0.204396,0.192746,0.126359,0.145312,0.207358,0.218566,0.127049,0.184891,0.173669,0.173669,0.208754,0.202614,0.19893,0.173669,0.177312,0.212329,0.203057,0.208287,0.215777,0.206208,0.205752,0.210169,0.200864,0.199785,0.203057,0.213548,0.201299,0.209224,0.20462,0.204171,0.20462,0.205298,0.232791,0.204846,0.233962,0.205298,0.212571,0.213303,0.204171,0.20598,0.213548,0.207127,0.199357,0.2,0.201954,0.201735,0.2,0.202174,0.198083,0.198294,0.156566,0.21831,0.19893,0.202835,0.153086,0.200431,0.203501,0.200431,0.203501,0.200431,0.205525,0.176974,0.200647,0.203724,0.203501,0.138085,0.116032,0.166667,0.175306,0.172862,0.126016,0.123835,0.1609,0.126016,0.126016,0.144747,0.144973,0.126016,0.164748,0.146803,0.148444,0.153719,0.158703,0.126016,0.158703,0.156566,0.147502,0.147502,0.151961,0.174812,0.145086,0.128898,0.155129],[0.371257,0.343173,0.391579,0.333932,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.302439,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306931,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.2976,0.331551,0.319039,0.303922,0.287926,0.261236,0.324042,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248996,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295707,0.296178,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var20_df_closseness, by=list(var20_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var20_df_closseness, by=list(var20_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-c6d852e549765cb70bd2" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-c6d852e549765cb70bd2">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000118\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001117\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7.9e-05\" data-max=\"0.000356\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000834\" data-max=\"0.001689\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.2e-05\" data-max=\"0.000292\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022025\" data-max=\"0.024425\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.4e-05\" data-max=\"0.004109\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.207821\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.014673\" data-max=\"0.066181\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.155129\" data-max=\"0.314189\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.002152\" data-max=\"0.054321\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003883\" data-max=\"0.139638\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[0.000125,0.000122,0.000125,0.000127,0.000125,0.000128,0.000127,0.000131,0.000127,0.000127,0.000128,0.000118,0.000124,0.000127,0.000127,0.000124,0.000127,0.000127,0.00013,0.000125,0.000128,0.000127,0.000126],[3e-06,4e-06,null,null,null,null,1e-06,null,null,null,null,2.2e-05,2e-06,0,0,4e-06,null,null,null,1e-06,null,1e-06,0],[0.000813,0.000252,0.000842,0.001015,0.00074,0.000841,0.000754,0.001046,0.000763,0.000717,0.0008,0.000697,0.000872,0.000775,0.000797,0.000756,2.9e-05,0.00087,0.001117,0.000509,0.000796,0.000633,0.00074],[0.000213,0.000303,null,null,null,null,7.9e-05,null,null,null,null,0.000193,7.9e-05,0.000116,8e-05,0.000112,null,null,null,0.000356,null,0.000277,8e-05],[0.001123,0.000836,0.001161,0.001412,0.000965,0.00128,0.001169,0.001553,0.001148,0.00107,0.001182,0.001021,0.001119,0.001177,0.001181,0.001009,0.000834,0.001175,0.001689,0.001016,0.001242,0.001117,0.001111],[0.000292,0.000143,null,null,null,null,1.2e-05,null,null,null,null,0.000191,0.000145,4.9e-05,5.3e-05,0.000173,null,null,null,6.7e-05,null,5.2e-05,4.4e-05],[0.023279,0.02267,0.023288,0.023667,0.023265,0.023794,0.023678,0.024425,0.023649,0.023562,0.023743,0.022025,0.023144,0.023629,0.023663,0.022989,0.023619,0.023658,0.0241,0.023158,0.023868,0.023593,0.023527],[0.000607,0.000667,null,null,null,null,6.7e-05,null,null,null,null,0.004109,0.000352,4.4e-05,4.4e-05,0.000699,null,null,null,0.000252,null,0.000222,4.8e-05],[0.151281,0.046908,0.156698,0.188832,0.137574,0.156434,0.14024,0.194561,0.141985,0.133333,0.1488,0.129647,0.162127,0.144136,0.148282,0.140673,0.005348,0.16188,0.207821,0.094649,0.147971,0.117829,0.137689],[0.039701,0.056308,null,null,null,null,0.014673,null,null,null,null,0.035895,0.014728,0.021579,0.01489,0.02087,null,null,null,0.066181,null,0.051457,0.014787],[0.208845,0.155484,0.216028,0.262712,0.179537,0.238156,0.217473,0.28882,0.213548,0.19893,0.219858,0.189818,0.208065,0.218895,0.219631,0.187717,0.155129,0.218566,0.314189,0.188906,0.231056,0.207681,0.206559],[0.054321,0.026659,null,null,null,null,0.002152,null,null,null,null,0.035481,0.026928,0.009127,0.009897,0.032111,null,null,null,0.012563,null,0.009726,0.008152],[0.300585,0.099063,0.286154,0.343173,0.303922,0.324042,0.301059,0.391579,0.295238,0.29108,0.313659,0.266596,0.319611,0.301471,0.316849,0.295625,0.005348,0.306425,0.371257,0.194167,0.314189,0.234219,0.288657],[0.043014,0.125815,null,null,null,null,0.018163,null,null,null,null,0.070707,0.003883,0.033404,0.029748,0.023781,null,null,null,0.139638,null,0.102732,0.011596]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var20_df_closseness <- data.frame(
var20_incloseness,
var20_outcloseness,
var20_totalcloseness,
var20_incloseness_n,
var20_outcloseness_n,
var20_totalcloseness_n,
var20_centr_closeness) %>% round(6)

#Adding type
var20_df_closseness <-cbind(var20_df_closseness, V(var20)$TIPO1)

#Adding names
names(var20_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var20_df_closseness<-var20_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var20_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-955f0351bd28b715eb3c" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-955f0351bd28b715eb3c">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001117\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000624\" data-max=\"0.001689\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024425\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.207821\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.116032\" data-max=\"0.314189\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[0.00013,0.000127,0.000131,0.000127,0.000127,0.000127,0.000125,0.000129,0.000123,0.000129,0.000127,0.000127,0.000122,0.000123,0.000126,0.000125,0.000127,0.000128,0.000122,0.000125,0.000127,0.000125,0.000127,0.000118,0.000116,0.000127,0.00012,0.000127,0.000127,0.000127,0.000127,0.000126,0.000127,0.00012,0.000127,0.000123,0.000129,0.000124,0.000123,0.000127,0.000127,0.000126,0.000125,0.000129,0.000127,0.000127,0.000116,0.000124,0.000124,0.000124,0.000124,0.000124,0.000121,0.000126,0.00012,0.000127,0.000127,0.000121,0.000121,0.000128,0.00013,0.000119,0.000126,0.000126,0.000124,2.9e-05,0.000117,0.000127,0.000121,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000126,0.000128,0.000119,0.000119,0.000119,0.000119,0.000127,0.000125,0.000125,0.000127,0.000125,0.000116,0.000126,0.000126,0.000126,0.000117,0.000119,0.000125,0.000127,0.000124,0.000116,0.000123,0.000127,0.000127,0.000119,0.000124,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000127,0.000127,0.000126,0.000127,0.000127,0.000127,0.000127,0.000126,0.000126,0.000126,0.000126,0.000127,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000124,0.000128,0.000127,0.000127,0.000121,0.000125,0.000127,0.000125,0.000127,0.000125,0.000127,0.000118,0.000129,0.00013,0.00013,0.000119,0.000118,0.000125,0.000129,0.000128,0.000118,0.000119,0.000125,0.000118,0.000118,0.000123,0.000123,0.000118,0.000125,0.000123,0.000124,0.000124,0.000125,0.000118,0.000125,0.000123,0.000123,0.000123,0.000124,0.000126,0.000123,0.00012,0.000127],[0.001117,0.001015,0.001046,0.000935,0.000783,0.000725,0.00074,0.00081,0.000923,0.000725,0.000786,0.000682,0.000857,0.000816,0.000818,0.000766,0.000883,0.000796,0.000691,0.000842,0.000794,0.000732,0.000763,0.000815,0.00073,0.000566,0.000797,0.000805,0.000847,0.000837,0.000793,0.00076,0.001004,0.000578,0.000525,0.000732,0.000829,0.000663,0.000731,0.000942,0.000853,0.000928,0.000927,0.000748,0.000733,0.000762,0.00066,0.000766,0.000766,0.000766,0.000766,0.000766,0.000724,0.000704,0.000822,0.000808,0.000767,0.000675,0.000606,0.000841,2.9e-05,0.000628,0.000894,0.000689,0.000846,2.9e-05,2.9e-05,0.000787,0.000711,0.000759,0.000666,0.000724,0.000888,0.00074,0.000686,0.000697,0.000933,0.0008,0.000503,0.000503,0.000503,0.000503,0.000734,0.000562,0.000693,0.000879,0.000752,0.000698,0.000782,0.000692,0.000742,0.000775,0.000795,0.000652,0.000749,0.000737,0.000504,0.000523,0.000688,0.00087,0.00055,0.000787,2.9e-05,2.9e-05,0.00085,0.000736,0.00067,2.9e-05,2.9e-05,0.000752,0.000754,0.000738,0.00085,0.00073,0.000733,0.00086,0.00075,0.000751,0.00074,0.000857,0.00076,0.000799,0.000665,0.000665,0.000665,0.000718,0.000879,0.000673,0.000879,0.000718,0.000826,0.000826,0.000718,0.000663,0.000756,0.000733,0.000669,0.000663,0.000663,0.000717,0.000687,0.000663,0.00067,0.000663,0.000663,0.000663,0.000717,0.000486,0.000583,0.000739,0.000739,0.000739,0.000739,0.000739,0.000568,0.000637,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001689,0.001412,0.001553,0.001282,0.001163,0.001139,0.000965,0.001316,0.001189,0.001232,0.001195,0.001181,0.001087,0.001016,0.001129,0.001093,0.001241,0.001242,0.000881,0.001161,0.001178,0.000962,0.001148,0.001016,0.000927,0.00103,0.001093,0.001156,0.001164,0.0012,0.00114,0.0011,0.001406,0.000851,0.001105,0.000965,0.00128,0.000974,0.00099,0.001279,0.001139,0.001221,0.001232,0.001271,0.001034,0.001152,0.00089,0.001075,0.001075,0.001075,0.001075,0.001075,0.000989,0.001116,0.001053,0.001172,0.00111,0.000907,0.00089,0.00128,0.001136,0.000824,0.001186,0.001074,0.001126,0.000699,0.000685,0.001065,0.001041,0.001121,0.001106,0.001104,0.00119,0.001145,0.001122,0.001106,0.001321,0.001182,0.000639,0.000639,0.000639,0.000639,0.001082,0.001073,0.000982,0.001161,0.001065,0.000892,0.001119,0.001066,0.001068,0.000958,0.001054,0.000943,0.001099,0.001036,0.000679,0.000781,0.001115,0.001175,0.000683,0.000994,0.000934,0.000934,0.001122,0.001089,0.00107,0.000934,0.000953,0.001142,0.001092,0.00112,0.00116,0.001109,0.001106,0.00113,0.00108,0.001074,0.001092,0.001148,0.001082,0.001125,0.0011,0.001098,0.0011,0.001104,0.001252,0.001101,0.001258,0.001104,0.001143,0.001147,0.001098,0.001107,0.001148,0.001114,0.001072,0.001075,0.001086,0.001085,0.001075,0.001087,0.001065,0.001066,0.000842,0.001174,0.00107,0.001091,0.000823,0.001078,0.001094,0.001078,0.001094,0.001078,0.001105,0.000951,0.001079,0.001095,0.001094,0.000742,0.000624,0.000896,0.000943,0.000929,0.000678,0.000666,0.000865,0.000678,0.000678,0.000778,0.000779,0.000678,0.000886,0.000789,0.000798,0.000826,0.000853,0.000678,0.000853,0.000842,0.000793,0.000793,0.000817,0.00094,0.00078,0.000693,0.000834],[0.0241,0.023667,0.024425,0.023601,0.023664,0.023619,0.023265,0.024084,0.02285,0.024015,0.023667,0.023712,0.022691,0.022895,0.023485,0.023314,0.023706,0.023868,0.022639,0.023288,0.023634,0.023212,0.023649,0.021952,0.021573,0.023538,0.022369,0.023646,0.023589,0.023601,0.023634,0.023494,0.023706,0.022358,0.023595,0.022929,0.024012,0.02298,0.022887,0.0237,0.023547,0.023393,0.023332,0.02405,0.02355,0.023667,0.02154,0.022974,0.022974,0.022974,0.022974,0.022974,0.022567,0.023346,0.022407,0.023592,0.023559,0.022431,0.022562,0.023794,0.02426,0.022048,0.023512,0.023503,0.022971,0.005348,0.021726,0.023607,0.022475,0.02347,0.023568,0.023574,0.023577,0.023604,0.023616,0.023497,0.023515,0.023743,0.022138,0.022138,0.022138,0.022138,0.023535,0.023157,0.023338,0.023586,0.023218,0.021578,0.0235,0.023473,0.023488,0.021724,0.022177,0.023244,0.023553,0.023043,0.021578,0.022856,0.023604,0.023658,0.022169,0.02304,0.023411,0.023411,0.023497,0.023494,0.023503,0.023411,0.023613,0.023532,0.023515,0.023616,0.023538,0.023577,0.023589,0.0235,0.023497,0.023497,0.023523,0.023565,0.023509,0.023688,0.023577,0.023559,0.023577,0.023559,0.023553,0.023568,0.023568,0.023355,0.023544,0.023562,0.023568,0.023568,0.023577,0.023607,0.023515,0.0235,0.023518,0.023491,0.023518,0.023506,0.023488,0.023494,0.023106,0.023755,0.023562,0.023529,0.022584,0.02334,0.023544,0.02334,0.023544,0.02334,0.023646,0.021918,0.023904,0.024112,0.024103,0.022067,0.02202,0.023282,0.023904,0.023868,0.021986,0.022204,0.023221,0.021986,0.021986,0.022909,0.022915,0.021986,0.023308,0.022969,0.022983,0.023006,0.023247,0.021986,0.023247,0.022842,0.022918,0.022918,0.023123,0.023367,0.022839,0.022257,0.023619],[0.207821,0.188832,0.194561,0.173994,0.145654,0.13488,0.137574,0.150729,0.171587,0.13488,0.146112,0.126876,0.159383,0.151713,0.152085,0.142529,0.164311,0.147971,0.128453,0.156698,0.147619,0.136164,0.141985,0.151589,0.135766,0.105204,0.148325,0.149758,0.157494,0.155649,0.147502,0.141337,0.186747,0.107577,0.097587,0.136164,0.154101,0.12326,0.135965,0.175141,0.158703,0.172542,0.172382,0.139117,0.136364,0.141768,0.122772,0.142529,0.142529,0.142529,0.142529,0.142529,0.134685,0.130894,0.152835,0.150242,0.142638,0.125506,0.112659,0.156434,0.005348,0.116761,0.16622,0.128187,0.15736,0.005376,0.005405,0.146341,0.13229,0.141123,0.123835,0.134588,0.165187,0.137574,0.12766,0.129707,0.173507,0.1488,0.093608,0.093608,0.093608,0.093608,0.136564,0.104553,0.128809,0.163445,0.13985,0.129888,0.145426,0.128631,0.138085,0.144186,0.147854,0.121252,0.139222,0.137168,0.09375,0.097229,0.128011,0.16188,0.10231,0.146457,0.005348,0.005348,0.158163,0.136865,0.124665,0.005348,0.005348,0.139955,0.140166,0.137269,0.158029,0.135866,0.136264,0.159931,0.13943,0.13964,0.137574,0.159383,0.141445,0.148562,0.123752,0.123752,0.123752,0.133621,0.163445,0.125253,0.163445,0.133621,0.153719,0.153719,0.133621,0.123342,0.14059,0.136264,0.124415,0.123342,0.123342,0.133429,0.127747,0.123342,0.124581,0.123342,0.123342,0.123342,0.133333,0.090423,0.108392,0.137472,0.137472,0.137472,0.137472,0.137472,0.105682,0.118471,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.314189,0.262712,0.28882,0.238462,0.216279,0.211845,0.179537,0.244737,0.221165,0.229064,0.222222,0.219599,0.202174,0.189024,0.209932,0.203279,0.230769,0.231056,0.163877,0.216028,0.219081,0.179018,0.213548,0.189024,0.172382,0.191555,0.203279,0.215029,0.216531,0.223289,0.212087,0.20462,0.261603,0.158298,0.205525,0.179537,0.238156,0.18111,0.184158,0.237852,0.211845,0.227106,0.229064,0.236341,0.192347,0.214286,0.16548,0.2,0.2,0.2,0.2,0.2,0.183976,0.207589,0.195789,0.218054,0.206437,0.168784,0.165628,0.238156,0.211364,0.153213,0.220641,0.199785,0.209459,0.129979,0.127485,0.198083,0.193548,0.20852,0.205752,0.205298,0.221429,0.213058,0.208754,0.205752,0.245707,0.219858,0.118926,0.118926,0.118926,0.118926,0.201299,0.199571,0.182711,0.216028,0.198083,0.165923,0.208054,0.198294,0.198718,0.178161,0.195996,0.175306,0.204396,0.192746,0.126359,0.145312,0.207358,0.218566,0.127049,0.184891,0.173669,0.173669,0.208754,0.202614,0.19893,0.173669,0.177312,0.212329,0.203057,0.208287,0.215777,0.206208,0.205752,0.210169,0.200864,0.199785,0.203057,0.213548,0.201299,0.209224,0.20462,0.204171,0.20462,0.205298,0.232791,0.204846,0.233962,0.205298,0.212571,0.213303,0.204171,0.20598,0.213548,0.207127,0.199357,0.2,0.201954,0.201735,0.2,0.202174,0.198083,0.198294,0.156566,0.21831,0.19893,0.202835,0.153086,0.200431,0.203501,0.200431,0.203501,0.200431,0.205525,0.176974,0.200647,0.203724,0.203501,0.138085,0.116032,0.166667,0.175306,0.172862,0.126016,0.123835,0.1609,0.126016,0.126016,0.144747,0.144973,0.126016,0.164748,0.146803,0.148444,0.153719,0.158703,0.126016,0.158703,0.156566,0.147502,0.147502,0.151961,0.174812,0.145086,0.128898,0.155129],[0.371257,0.343173,0.391579,0.333932,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.302439,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306931,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.2976,0.331551,0.319039,0.303922,0.287926,0.261236,0.324042,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248996,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295707,0.296178,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var20_df_closseness, by=list(var20_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var20_df_closseness, by=list(var20_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-f3c038eedb9822a0ce09" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-f3c038eedb9822a0ce09">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000121\" data-max=\"0.000127\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000446\" data-max=\"0.000713\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000216\" data-max=\"0.000341\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000909\" data-max=\"0.001128\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000108\" data-max=\"0.000178\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022576\" data-max=\"0.023533\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000222\" data-max=\"0.002084\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.082929\" data-max=\"0.132619\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.040198\" data-max=\"0.063469\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.169041\" data-max=\"0.209793\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.020067\" data-max=\"0.033175\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.173787\" data-max=\"0.272543\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.079113\" data-max=\"0.135441\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[0.000127,0.000121],[1e-06,1.1e-05],[0.000713,0.000446],[0.000216,0.000341],[0.001128,0.000909],[0.000108,0.000178],[0.023533,0.022576],[0.000222,0.002084],[0.132619,0.082929],[0.040198,0.063469],[0.209793,0.169041],[0.020067,0.033175],[0.272543,0.173787],[0.079113,0.135441]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var20_df_closseness <- data.frame(
var20_incloseness,
var20_outcloseness,
var20_totalcloseness,
var20_incloseness_n,
var20_outcloseness_n,
var20_totalcloseness_n,
var20_centr_closeness) %>% round(6)

#Adding type
var20_df_closseness <-cbind(var20_df_closseness, V(var20)$TIPO2)

#Adding names
names(var20_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var20_df_closseness<-var20_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var20_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-6cf9a705967939c982b8" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-6cf9a705967939c982b8">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001117\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000624\" data-max=\"0.001689\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024425\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.207821\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.116032\" data-max=\"0.314189\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[0.00013,0.000127,0.000131,0.000127,0.000127,0.000127,0.000125,0.000129,0.000123,0.000129,0.000127,0.000127,0.000122,0.000123,0.000126,0.000125,0.000127,0.000128,0.000122,0.000125,0.000127,0.000125,0.000127,0.000118,0.000116,0.000127,0.00012,0.000127,0.000127,0.000127,0.000127,0.000126,0.000127,0.00012,0.000127,0.000123,0.000129,0.000124,0.000123,0.000127,0.000127,0.000126,0.000125,0.000129,0.000127,0.000127,0.000116,0.000124,0.000124,0.000124,0.000124,0.000124,0.000121,0.000126,0.00012,0.000127,0.000127,0.000121,0.000121,0.000128,0.00013,0.000119,0.000126,0.000126,0.000124,2.9e-05,0.000117,0.000127,0.000121,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000126,0.000128,0.000119,0.000119,0.000119,0.000119,0.000127,0.000125,0.000125,0.000127,0.000125,0.000116,0.000126,0.000126,0.000126,0.000117,0.000119,0.000125,0.000127,0.000124,0.000116,0.000123,0.000127,0.000127,0.000119,0.000124,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000127,0.000127,0.000126,0.000127,0.000127,0.000127,0.000127,0.000126,0.000126,0.000126,0.000126,0.000127,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000124,0.000128,0.000127,0.000127,0.000121,0.000125,0.000127,0.000125,0.000127,0.000125,0.000127,0.000118,0.000129,0.00013,0.00013,0.000119,0.000118,0.000125,0.000129,0.000128,0.000118,0.000119,0.000125,0.000118,0.000118,0.000123,0.000123,0.000118,0.000125,0.000123,0.000124,0.000124,0.000125,0.000118,0.000125,0.000123,0.000123,0.000123,0.000124,0.000126,0.000123,0.00012,0.000127],[0.001117,0.001015,0.001046,0.000935,0.000783,0.000725,0.00074,0.00081,0.000923,0.000725,0.000786,0.000682,0.000857,0.000816,0.000818,0.000766,0.000883,0.000796,0.000691,0.000842,0.000794,0.000732,0.000763,0.000815,0.00073,0.000566,0.000797,0.000805,0.000847,0.000837,0.000793,0.00076,0.001004,0.000578,0.000525,0.000732,0.000829,0.000663,0.000731,0.000942,0.000853,0.000928,0.000927,0.000748,0.000733,0.000762,0.00066,0.000766,0.000766,0.000766,0.000766,0.000766,0.000724,0.000704,0.000822,0.000808,0.000767,0.000675,0.000606,0.000841,2.9e-05,0.000628,0.000894,0.000689,0.000846,2.9e-05,2.9e-05,0.000787,0.000711,0.000759,0.000666,0.000724,0.000888,0.00074,0.000686,0.000697,0.000933,0.0008,0.000503,0.000503,0.000503,0.000503,0.000734,0.000562,0.000693,0.000879,0.000752,0.000698,0.000782,0.000692,0.000742,0.000775,0.000795,0.000652,0.000749,0.000737,0.000504,0.000523,0.000688,0.00087,0.00055,0.000787,2.9e-05,2.9e-05,0.00085,0.000736,0.00067,2.9e-05,2.9e-05,0.000752,0.000754,0.000738,0.00085,0.00073,0.000733,0.00086,0.00075,0.000751,0.00074,0.000857,0.00076,0.000799,0.000665,0.000665,0.000665,0.000718,0.000879,0.000673,0.000879,0.000718,0.000826,0.000826,0.000718,0.000663,0.000756,0.000733,0.000669,0.000663,0.000663,0.000717,0.000687,0.000663,0.00067,0.000663,0.000663,0.000663,0.000717,0.000486,0.000583,0.000739,0.000739,0.000739,0.000739,0.000739,0.000568,0.000637,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001689,0.001412,0.001553,0.001282,0.001163,0.001139,0.000965,0.001316,0.001189,0.001232,0.001195,0.001181,0.001087,0.001016,0.001129,0.001093,0.001241,0.001242,0.000881,0.001161,0.001178,0.000962,0.001148,0.001016,0.000927,0.00103,0.001093,0.001156,0.001164,0.0012,0.00114,0.0011,0.001406,0.000851,0.001105,0.000965,0.00128,0.000974,0.00099,0.001279,0.001139,0.001221,0.001232,0.001271,0.001034,0.001152,0.00089,0.001075,0.001075,0.001075,0.001075,0.001075,0.000989,0.001116,0.001053,0.001172,0.00111,0.000907,0.00089,0.00128,0.001136,0.000824,0.001186,0.001074,0.001126,0.000699,0.000685,0.001065,0.001041,0.001121,0.001106,0.001104,0.00119,0.001145,0.001122,0.001106,0.001321,0.001182,0.000639,0.000639,0.000639,0.000639,0.001082,0.001073,0.000982,0.001161,0.001065,0.000892,0.001119,0.001066,0.001068,0.000958,0.001054,0.000943,0.001099,0.001036,0.000679,0.000781,0.001115,0.001175,0.000683,0.000994,0.000934,0.000934,0.001122,0.001089,0.00107,0.000934,0.000953,0.001142,0.001092,0.00112,0.00116,0.001109,0.001106,0.00113,0.00108,0.001074,0.001092,0.001148,0.001082,0.001125,0.0011,0.001098,0.0011,0.001104,0.001252,0.001101,0.001258,0.001104,0.001143,0.001147,0.001098,0.001107,0.001148,0.001114,0.001072,0.001075,0.001086,0.001085,0.001075,0.001087,0.001065,0.001066,0.000842,0.001174,0.00107,0.001091,0.000823,0.001078,0.001094,0.001078,0.001094,0.001078,0.001105,0.000951,0.001079,0.001095,0.001094,0.000742,0.000624,0.000896,0.000943,0.000929,0.000678,0.000666,0.000865,0.000678,0.000678,0.000778,0.000779,0.000678,0.000886,0.000789,0.000798,0.000826,0.000853,0.000678,0.000853,0.000842,0.000793,0.000793,0.000817,0.00094,0.00078,0.000693,0.000834],[0.0241,0.023667,0.024425,0.023601,0.023664,0.023619,0.023265,0.024084,0.02285,0.024015,0.023667,0.023712,0.022691,0.022895,0.023485,0.023314,0.023706,0.023868,0.022639,0.023288,0.023634,0.023212,0.023649,0.021952,0.021573,0.023538,0.022369,0.023646,0.023589,0.023601,0.023634,0.023494,0.023706,0.022358,0.023595,0.022929,0.024012,0.02298,0.022887,0.0237,0.023547,0.023393,0.023332,0.02405,0.02355,0.023667,0.02154,0.022974,0.022974,0.022974,0.022974,0.022974,0.022567,0.023346,0.022407,0.023592,0.023559,0.022431,0.022562,0.023794,0.02426,0.022048,0.023512,0.023503,0.022971,0.005348,0.021726,0.023607,0.022475,0.02347,0.023568,0.023574,0.023577,0.023604,0.023616,0.023497,0.023515,0.023743,0.022138,0.022138,0.022138,0.022138,0.023535,0.023157,0.023338,0.023586,0.023218,0.021578,0.0235,0.023473,0.023488,0.021724,0.022177,0.023244,0.023553,0.023043,0.021578,0.022856,0.023604,0.023658,0.022169,0.02304,0.023411,0.023411,0.023497,0.023494,0.023503,0.023411,0.023613,0.023532,0.023515,0.023616,0.023538,0.023577,0.023589,0.0235,0.023497,0.023497,0.023523,0.023565,0.023509,0.023688,0.023577,0.023559,0.023577,0.023559,0.023553,0.023568,0.023568,0.023355,0.023544,0.023562,0.023568,0.023568,0.023577,0.023607,0.023515,0.0235,0.023518,0.023491,0.023518,0.023506,0.023488,0.023494,0.023106,0.023755,0.023562,0.023529,0.022584,0.02334,0.023544,0.02334,0.023544,0.02334,0.023646,0.021918,0.023904,0.024112,0.024103,0.022067,0.02202,0.023282,0.023904,0.023868,0.021986,0.022204,0.023221,0.021986,0.021986,0.022909,0.022915,0.021986,0.023308,0.022969,0.022983,0.023006,0.023247,0.021986,0.023247,0.022842,0.022918,0.022918,0.023123,0.023367,0.022839,0.022257,0.023619],[0.207821,0.188832,0.194561,0.173994,0.145654,0.13488,0.137574,0.150729,0.171587,0.13488,0.146112,0.126876,0.159383,0.151713,0.152085,0.142529,0.164311,0.147971,0.128453,0.156698,0.147619,0.136164,0.141985,0.151589,0.135766,0.105204,0.148325,0.149758,0.157494,0.155649,0.147502,0.141337,0.186747,0.107577,0.097587,0.136164,0.154101,0.12326,0.135965,0.175141,0.158703,0.172542,0.172382,0.139117,0.136364,0.141768,0.122772,0.142529,0.142529,0.142529,0.142529,0.142529,0.134685,0.130894,0.152835,0.150242,0.142638,0.125506,0.112659,0.156434,0.005348,0.116761,0.16622,0.128187,0.15736,0.005376,0.005405,0.146341,0.13229,0.141123,0.123835,0.134588,0.165187,0.137574,0.12766,0.129707,0.173507,0.1488,0.093608,0.093608,0.093608,0.093608,0.136564,0.104553,0.128809,0.163445,0.13985,0.129888,0.145426,0.128631,0.138085,0.144186,0.147854,0.121252,0.139222,0.137168,0.09375,0.097229,0.128011,0.16188,0.10231,0.146457,0.005348,0.005348,0.158163,0.136865,0.124665,0.005348,0.005348,0.139955,0.140166,0.137269,0.158029,0.135866,0.136264,0.159931,0.13943,0.13964,0.137574,0.159383,0.141445,0.148562,0.123752,0.123752,0.123752,0.133621,0.163445,0.125253,0.163445,0.133621,0.153719,0.153719,0.133621,0.123342,0.14059,0.136264,0.124415,0.123342,0.123342,0.133429,0.127747,0.123342,0.124581,0.123342,0.123342,0.123342,0.133333,0.090423,0.108392,0.137472,0.137472,0.137472,0.137472,0.137472,0.105682,0.118471,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.314189,0.262712,0.28882,0.238462,0.216279,0.211845,0.179537,0.244737,0.221165,0.229064,0.222222,0.219599,0.202174,0.189024,0.209932,0.203279,0.230769,0.231056,0.163877,0.216028,0.219081,0.179018,0.213548,0.189024,0.172382,0.191555,0.203279,0.215029,0.216531,0.223289,0.212087,0.20462,0.261603,0.158298,0.205525,0.179537,0.238156,0.18111,0.184158,0.237852,0.211845,0.227106,0.229064,0.236341,0.192347,0.214286,0.16548,0.2,0.2,0.2,0.2,0.2,0.183976,0.207589,0.195789,0.218054,0.206437,0.168784,0.165628,0.238156,0.211364,0.153213,0.220641,0.199785,0.209459,0.129979,0.127485,0.198083,0.193548,0.20852,0.205752,0.205298,0.221429,0.213058,0.208754,0.205752,0.245707,0.219858,0.118926,0.118926,0.118926,0.118926,0.201299,0.199571,0.182711,0.216028,0.198083,0.165923,0.208054,0.198294,0.198718,0.178161,0.195996,0.175306,0.204396,0.192746,0.126359,0.145312,0.207358,0.218566,0.127049,0.184891,0.173669,0.173669,0.208754,0.202614,0.19893,0.173669,0.177312,0.212329,0.203057,0.208287,0.215777,0.206208,0.205752,0.210169,0.200864,0.199785,0.203057,0.213548,0.201299,0.209224,0.20462,0.204171,0.20462,0.205298,0.232791,0.204846,0.233962,0.205298,0.212571,0.213303,0.204171,0.20598,0.213548,0.207127,0.199357,0.2,0.201954,0.201735,0.2,0.202174,0.198083,0.198294,0.156566,0.21831,0.19893,0.202835,0.153086,0.200431,0.203501,0.200431,0.203501,0.200431,0.205525,0.176974,0.200647,0.203724,0.203501,0.138085,0.116032,0.166667,0.175306,0.172862,0.126016,0.123835,0.1609,0.126016,0.126016,0.144747,0.144973,0.126016,0.164748,0.146803,0.148444,0.153719,0.158703,0.126016,0.158703,0.156566,0.147502,0.147502,0.151961,0.174812,0.145086,0.128898,0.155129],[0.371257,0.343173,0.391579,0.333932,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.302439,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306931,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.2976,0.331551,0.319039,0.303922,0.287926,0.261236,0.324042,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248996,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295707,0.296178,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var20_df_closseness, by=list(var20_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var20_df_closseness, by=list(var20_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-4a323ba91b88ef1808cd" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-4a323ba91b88ef1808cd">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000121\" data-max=\"0.000127\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000443\" data-max=\"0.000802\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00011\" data-max=\"0.000343\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000907\" data-max=\"0.001191\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7.4e-05\" data-max=\"0.00018\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.02256\" data-max=\"0.023643\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.7e-05\" data-max=\"0.002092\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.082417\" data-max=\"0.149088\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.020532\" data-max=\"0.0638\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.168792\" data-max=\"0.221484\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.013727\" data-max=\"0.033438\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172417\" data-max=\"0.308478\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.029371\" data-max=\"0.135791\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[0.000127,0.000126,0.000121],[0,1e-06,1.1e-05],[0.000802,0.000698,0.000443],[0.00011,0.000224,0.000343],[0.001191,0.001117,0.000907],[7.4e-05,0.000108,0.00018],[0.023643,0.023519,0.02256],[5.7e-05,0.000232,0.002092],[0.149088,0.129886,0.082417],[0.020532,0.041673,0.0638],[0.221484,0.2077,0.168792],[0.013727,0.020164,0.033438],[0.308478,0.266895,0.172417],[0.029371,0.082617,0.135791]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var20_data.RData")
```

