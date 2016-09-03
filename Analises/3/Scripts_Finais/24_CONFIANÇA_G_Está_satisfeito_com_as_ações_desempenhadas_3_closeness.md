# SNA Closeness 24_CONFIANÇA G) Está satisfeito com as ações desempenhadas por este serviço (var22).
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 24_CONFIANÇA G) Está satisfeito com as ações desempenhadas por este serviço (var22).

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var22_data.RData")
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
#var22<-simplify(var22) #Simplify
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
V(var22)$incloseness <- closeness(var22, mode = "in", weights = E(var22)$var22) %>% round(6)
V(var22)$outcloseness <- closeness(var22, mode = "out", weights = E(var22)$var22) %>% round(6)
V(var22)$totalcloseness <- closeness(var22, mode = "total", weights = E(var22)$var22) %>% round(4)
```

###Saving to Environment

```r
var22_incloseness<- closeness(var22, mode = "in", weights = E(var22)$var22) %>% round(6)
var22_outcloseness<- closeness(var22, mode = "out", weights = E(var22)$var22) %>% round(6)
var22_totalcloseness<- closeness(var22, mode = "total", weights = E(var22)$var22) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var22_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0001270 0.0001290 0.0001269 0.0001290 0.0001330
```

```r
sd(var22_incloseness)
```

```
## [1] 7.858902e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var22)$incloseness<-closeness(var22, weights = E(var22)$var22, mode="in")

#Get Variable
V(var22)$var22_color_degree<-round(V(var22)$incloseness,6)

#Creating brewer pallette
vertex_var22_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var22)$var22_color_degree)), "RdBu"))(
            length(unique(V(var22)$var22_color_degree)))

#Saving as Vertex properties 
V(var22)$vertex_var22_color_degree<-
  vertex_var22_color_degree[as.numeric(
  cut(V(var22)$var22_color_degree,
      breaks=length(unique(V(var22)$var22_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var22, es=E(var22), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var22))
maxC <- rep(Inf, vcount(var22))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var22, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var22)$weight)


#PLotting
plot(var22, 
     layout=co,
     edge.color=V(var22)$vertex_var22_color_degree[edge.start],
     edge.arrow.size=closeness(var22, weights = E(var22)$var22, mode="in"),
     edge.width=E(var22)$weight/mean(E(var22)$weight),
     edge.curved = TRUE,
     vertex.color=V(var22)$vertex_var22_color_degree,
     vertex.size=closeness(var22, weights = E(var22)$var22, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var22,"LABEL_COR"),
     vertex.label.cex=(closeness(var22, weights = E(var22)$var22, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var22)$var22_color_degree
b<-V(var22)$vertex_var22_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 24_CONFIANÇA G) Está satisfeito com as ações desempenhadas por este serviço (var22).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var22, mode="in", weights = E(var22)$var22)), 
             sd(closeness(var22, mode="in", weights = E(var22)$var22))
             )
       )
```

![](24_CONFIANÇA_G_Está_satisfeito_com_as_ações_desempenhadas_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var22_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0006430 0.0008380 0.0007090 0.0009775 0.0012170
```

```r
sd(var22_outcloseness)
```

```
## [1] 0.0003699683
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var22)$outcloseness<-closeness(var22, weights = E(var22)$var22, mode="out")

#Get Variable
V(var22)$var22_color_degree<-round(V(var22)$outcloseness,6)

#Creating brewer pallette
vertex_var22_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var22)$var22_color_degree)), "RdBu"))(
            length(unique(V(var22)$var22_color_degree)))

#Saving as Vertex properties 
V(var22)$vertex_var22_color_degree<-
  vertex_var22_color_degree[as.numeric(
  cut(V(var22)$var22_color_degree,
      breaks=length(unique(V(var22)$var22_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var22, es=E(var22), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var22))
maxC <- rep(Inf, vcount(var22))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var22, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var22)$weight)


#PLotting
plot(var22, 
     layout=co,
     edge.color=V(var22)$vertex_var22_color_degree[edge.start],
     edge.arrow.size=closeness(var22, weights = E(var22)$var22, mode="out"),
     edge.width=E(var22)$weight/2*mean(E(var22)$weight),
     edge.curved = TRUE,
     vertex.color=V(var22)$vertex_var22_color_degree,
     vertex.size=closeness(var22, weights = E(var22)$var22, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var22,"LABEL_COR"),
     vertex.label.cex=closeness(var22, weights = E(var22)$var22, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var22)$var22_color_degree
b<-V(var22)$vertex_var22_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 24_CONFIANÇA G) Está satisfeito com as ações desempenhadas por este serviço (var22).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var22, mode="out", weights = E(var22)$var22)), 
             sd(closeness(var22, mode="out", weights = E(var22)$var22))
             )
       )
```

![](24_CONFIANÇA_G_Está_satisfeito_com_as_ações_desempenhadas_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var22_totalcloseness)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000629 0.001149 0.001321 0.001309 0.001538 0.002105
```

```r
sd(var22_totalcloseness)
```

```
## [1] 0.0002857168
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var22)$allcloseness<-closeness(var22, weights = E(var22)$var22, mode="all")

#Get Variable
V(var22)$var22_color_degree<-round(V(var22)$allcloseness,6)

#Creating brewer pallette
vertex_var22_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var22)$var22_color_degree)), "RdBu"))(
            length(unique(V(var22)$var22_color_degree)))

#Saving as Vertex properties 
V(var22)$vertex_var22_color_degree<-
  vertex_var22_color_degree[as.numeric(
  cut(V(var22)$var22_color_degree,
      breaks=length(unique(V(var22)$var22_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var22, es=E(var22), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var22))
maxC <- rep(Inf, vcount(var22))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var22, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var22)$weight)


#PLotting
plot(var22, 
     layout=co,
     edge.color=V(var22)$vertex_var22_color_degree[edge.start],
     edge.arrow.size=closeness(var22, weights = E(var22)$var22, mode="all"),
     edge.width=E(var22)$weight/2*mean(E(var22)$weight),
     edge.curved = TRUE,
     vertex.color=V(var22)$vertex_var22_color_degree,
     vertex.size=closeness(var22, weights = E(var22)$var22, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var22,"LABEL_COR"),
     vertex.label.cex=(closeness(var22, weights = E(var22)$var22, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var22)$var22_color_degree
b<-V(var22)$vertex_var22_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 24_CONFIANÇA G) Está satisfeito com as ações desempenhadas por este serviço (var22).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var22, mode="all", weights = E(var22)$var22)), 
             sd(closeness(var22, mode="all", weights = E(var22)$var22))
             )
       )
```

![](24_CONFIANÇA_G_Está_satisfeito_com_as_ações_desempenhadas_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var22)$incloseness_n <- closeness(var22, mode = "in",, weights = E(var22)$var22, normalized = T) %>% round(10)
V(var22)$outcloseness_n <- closeness(var22, mode = "out", normalized = T, weights = E(var22)$var22) %>% round(6)
V(var22)$totalcloseness_n <- closeness(var22, mode = "total", normalized = T, weights = E(var22)$var22) %>% round(6)
```

###Saving to Environment

```r
var22_incloseness_n<- closeness(var22, mode = "in", normalized = T, weights = E(var22)$var22) %>% round(6)
var22_outcloseness_n<- closeness(var22, mode = "out", normalized = T, weights = E(var22)$var22) %>% round(6)
var22_totalcloseness_n<- closeness(var22, mode = "total", normalized = T, weights = E(var22)$var22) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var22_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.023550 0.023900 0.023600 0.024030 0.024820
```

```r
sd(var22_incloseness_n)
```

```
## [1] 0.001459996
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var22)$incloseness_n<-closeness(var22, weights = E(var22)$var22, mode="in", normalized = T)

#Get Variable
V(var22)$var22_color_degree<-round(V(var22)$incloseness_n,6)

#Creating brewer pallette
vertex_var22_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var22)$var22_color_degree)), "RdBu"))(
            length(unique(V(var22)$var22_color_degree)))

#Saving as Vertex properties 
V(var22)$vertex_var22_color_degree<-
  vertex_var22_color_degree[as.numeric(
  cut(V(var22)$var22_color_degree,
      breaks=length(unique(V(var22)$var22_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var22, es=E(var22), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var22))
maxC <- rep(Inf, vcount(var22))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var22, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var22)$weight)


#PLotting
plot(var22, 
     layout=co,
     edge.color=V(var22)$vertex_var22_color_degree[edge.start],
     edge.arrow.size=closeness(var22, weights = E(var22)$var22, mode="in",normalized = T),
     edge.width=E(var22)$weight/10*mean(E(var22)$weight),
     edge.curved = TRUE,
     vertex.color=V(var22)$vertex_var22_color_degree,
     vertex.size=(closeness(var22, weights = E(var22)$var22, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var22,"LABEL_COR"),
     vertex.label.cex=closeness(var22, weights = E(var22)$var22, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var22)$var22_color_degree
b<-V(var22)$vertex_var22_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 24_CONFIANÇA G) Está satisfeito com as ações desempenhadas por este serviço (var22).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var22, mode="in", weights = E(var22)$var22, normalized = T)), 
             sd(closeness(var22, mode="in", weights = E(var22)$var22, normalized = T))
             )
       )
```

![](24_CONFIANÇA_G_Está_satisfeito_com_as_ações_desempenhadas_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var22_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.119500 0.155900 0.131900 0.181800 0.226300
```

```r
sd(var22_outcloseness_n)
```

```
## [1] 0.06883115
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var22)$outcloseness_n<-closeness(var22, weights = E(var22)$var22, mode="out", normalized = T)

#Get Variable
V(var22)$var22_color_degree<-round(V(var22)$outcloseness_n,6)

#Creating brewer pallette
vertex_var22_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var22)$var22_color_degree)), "RdBu"))(
            length(unique(V(var22)$var22_color_degree)))

#Saving as Vertex properties 
V(var22)$vertex_var22_color_degree<-
  vertex_var22_color_degree[as.numeric(
  cut(V(var22)$var22_color_degree,
      breaks=length(unique(V(var22)$var22_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var22, es=E(var22), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var22))
maxC <- rep(Inf, vcount(var22))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var22, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var22)$weight)


#PLotting
plot(var22, 
     layout=co,
     edge.color=V(var22)$vertex_var22_color_degree[edge.start],
     edge.arrow.size=closeness(var22, weights = E(var22)$var22, mode="out",normalized = T),
     edge.width=E(var22)$weight/10*mean(E(var22)$weight),
     edge.curved = TRUE,
     vertex.color=V(var22)$vertex_var22_color_degree,
     vertex.size=(closeness(var22, weights = E(var22)$var22, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var22,"LABEL_COR"),
     vertex.label.cex=closeness(var22, weights = E(var22)$var22, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var22)$var22_color_degree
b<-V(var22)$vertex_var22_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 24_CONFIANÇA G) Está satisfeito com as ações desempenhadas por este serviço (var22).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var22, mode="out", weights = E(var22)$var22, normalized = T)), 
             sd(closeness(var22, mode="out", weights = E(var22)$var22, normalized = T))
             )
       )
```

![](24_CONFIANÇA_G_Está_satisfeito_com_as_ações_desempenhadas_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var22_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1170  0.2138  0.2457  0.2435  0.2862  0.3916
```

```r
sd(var22_totalcloseness_n)
```

```
## [1] 0.05314797
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var22)$allcloseness_n<-closeness(var22, weights = E(var22)$var22, mode="all", normalized = T)

#Get Variable
V(var22)$var22_color_degree<-round(V(var22)$allcloseness_n,6)

#Creating brewer pallette
vertex_var22_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var22)$var22_color_degree)), "RdBu"))(
            length(unique(V(var22)$var22_color_degree)))

#Saving as Vertex properties 
V(var22)$vertex_var22_color_degree<-
  vertex_var22_color_degree[as.numeric(
  cut(V(var22)$var22_color_degree,
      breaks=length(unique(V(var22)$var22_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var22, es=E(var22), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var22))
maxC <- rep(Inf, vcount(var22))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var22, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var22)$weight)


#PLotting
plot(var22, 
     layout=co,
     edge.color=V(var22)$vertex_var22_color_degree[edge.start],
     edge.arrow.size=closeness(var22, weights = E(var22)$var22, mode="all",normalized = T),
     edge.width=E(var22)$weight/10*mean(E(var22)$weight),
     edge.curved = TRUE,
     vertex.color=V(var22)$vertex_var22_color_degree,
     vertex.size=(closeness(var22, weights = E(var22)$var22, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var22,"LABEL_COR"),
     vertex.label.cex=closeness(var22, weights = E(var22)$var22, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var22)$var22_color_degree
b<-V(var22)$vertex_var22_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 24_CONFIANÇA G) Está satisfeito com as ações desempenhadas por este serviço (var22).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var22, mode="all", weights = E(var22)$var22, normalized = T)), 
             sd(closeness(var22, mode="all", weights = E(var22)$var22, normalized = T))
             )
       )
```

![](24_CONFIANÇA_G_Está_satisfeito_com_as_ações_desempenhadas_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var22)$incloseness_n <- closeness(var22, weights = E(var22)$var22, mode = "in", normalized = T) %>% round(6)
V(var22)$outcloseness_n <- closeness(var22, weights = E(var22)$var22, mode = "out", normalized = T) %>% round(6)
V(var22)$totalcloseness_n <- closeness(var22, weights = E(var22)$var22, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var22)$var22_centr_closeness<- centralization.closeness(var22)$res
var22_centr_closeness<- centralization.closeness(var22)$res
var22_centr_closeness_all<- centralization.closeness(var22)
```

###Centralization

```r
var22_centr_closeness_all$centralization
```

```
## [1] 0.1625197
```

###Theoretical Max

```r
var22_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var22)$var22_centr_closeness<- centralization.closeness(var22)$res

#Get Variable
V(var22)$var22_color_degree<-round(V(var22)$var22_centr_closeness,6)

#Creating brewer pallette
vertex_var22_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var22)$var22_color_degree)), "Spectral"))(
            length(unique(V(var22)$var22_color_degree)))

#Saving as Vertex properties 
V(var22)$vertex_var22_color_degree<-
  vertex_var22_color_degree[as.numeric(
  cut(V(var22)$var22_color_degree,
      breaks=length(unique(V(var22)$var22_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var22, es=E(var22), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var22))
maxC <- rep(Inf, vcount(var22))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var22, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var22)$weight)


#PLotting
plot(var22, 
     layout=co,
     edge.color=V(var22)$vertex_var22_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var22)$res,
     edge.width=E(var22)$weight/10*mean(E(var22)$weight),
     edge.curved = TRUE,
     vertex.color=V(var22)$vertex_var22_color_degree,
     vertex.size=centralization.closeness(var22)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var22,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var22)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var22)$var22_color_degree
b<-V(var22)$vertex_var22_color_degree
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
  title("Network Centralization Closeness - 24_CONFIANÇA G) Está satisfeito com as ações desempenhadas por este serviço (var22).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var22)$res), 
             sd(centralization.closeness(var22)$res)
             )
       )
```

![](24_CONFIANÇA_G_Está_satisfeito_com_as_ações_desempenhadas_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var22_incloseness<- closeness(var22, weights = E(var22)$var22, mode = "in") %>% round(6)
var22_outcloseness<- closeness(var22, weights = E(var22)$var22, mode = "out") %>% round(6)
var22_totalcloseness<- closeness(var22, weights = E(var22)$var22, mode = "total") %>% round(6)
var22_incloseness_n<- closeness(var22,weights = E(var22)$var22, mode = "in", normalized = T) %>% round(6)
var22_outcloseness_n<- closeness(var22,weights = E(var22)$var22, mode = "out", normalized = T) %>% round(6)
var22_totalcloseness_n<- closeness(var22,weights = E(var22)$var22, mode = "total", normalized = T) %>% round(6)
var22_centr_closeness <- centralization.closeness(var22)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var22_df_closseness <- data.frame(
var22_incloseness,
var22_outcloseness,
var22_totalcloseness,
var22_incloseness_n,
var22_outcloseness_n,
var22_totalcloseness_n,
var22_centr_closeness) %>% round(6)

#Adding type
var22_df_closseness <-cbind(var22_df_closseness, V(var22)$LABEL_COR)

#Adding names
names(var22_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var22_df_closseness<-var22_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var22_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-9c5654b933ae55a770e0" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-9c5654b933ae55a770e0">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001217\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000629\" data-max=\"0.002105\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024823\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.226277\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.116981\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[0.000132,0.00013,0.000133,0.000129,0.000129,0.00013,0.000128,0.000131,0.000129,0.000131,0.00013,0.000129,0.000127,0.000127,0.000128,0.000129,0.000129,0.000131,0.000127,0.000128,0.00013,0.000129,0.00013,0.000118,0.00012,0.000129,0.000125,0.000129,0.000129,0.000129,0.000129,0.000129,0.00013,0.000122,0.000129,0.000129,0.000131,0.000125,0.000125,0.000129,0.00013,0.000128,0.000128,0.000132,0.000129,0.000129,0.000118,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000129,0.000122,0.000129,0.000129,0.000122,0.000124,0.000131,0.000132,0.00012,0.000129,0.000129,0.000129,2.9e-05,0.000118,0.000129,0.000126,0.000128,0.00013,0.000129,0.000129,0.000129,0.00013,0.000129,0.000129,0.000131,0.00012,0.00012,0.00012,0.00012,0.000128,0.000125,0.000127,0.00013,0.00013,0.000123,0.000128,0.000128,0.000128,0.00012,0.000123,0.000127,0.000129,0.000129,0.000123,0.000129,0.00013,0.000129,0.000121,0.000126,0.000128,0.000128,0.000129,0.000128,0.000129,0.000128,0.000129,0.000129,0.000129,0.00013,0.000129,0.00013,0.00013,0.000128,0.000128,0.000128,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000128,0.000128,0.000129,0.000129,0.000129,0.000129,0.00013,0.000129,0.000129,0.000129,0.000128,0.000128,0.000129,0.000128,0.000128,0.000126,0.00013,0.000128,0.000129,0.000125,0.000128,0.000128,0.000128,0.000128,0.000128,0.000129,0.000121,0.000131,0.000132,0.000132,0.000125,0.000123,0.000126,0.000129,0.000128,0.00012,0.000121,0.000127,0.00012,0.00012,0.00013,0.000125,0.00012,0.000129,0.000127,0.000126,0.000126,0.00013,0.00012,0.00013,0.000125,0.000125,0.000125,0.000129,0.000125,0.000125,0.000125,0.000129],[0.001188,0.001024,0.001152,0.001049,0.00096,0.000766,0.000805,0.001217,0.000925,0.000785,0.000975,0.000832,0.001092,0.000861,0.001041,0.000841,0.001067,0.000988,0.000841,0.00106,0.000984,0.000894,0.00092,0.000874,0.001016,0.000655,0.000843,0.000853,0.000993,0.001096,0.001017,0.000845,0.001059,0.000539,0.000602,0.001019,0.00088,0.00089,0.000748,0.00107,0.000978,0.000996,0.000995,0.000922,0.00078,0.001089,0.000784,0.000844,0.000844,0.000844,0.000844,0.000844,0.000761,0.000854,0.000967,0.000895,0.000972,0.000959,0.000678,0.000898,2.9e-05,0.00059,0.001093,0.001004,0.001142,2.9e-05,2.9e-05,0.001136,0.000826,0.000956,0.000954,0.000745,0.001096,0.001057,0.001005,0.000997,0.001085,0.000969,0.000643,0.000643,0.000643,0.000643,0.000768,0.00059,0.000987,0.001056,0.000781,0.000985,0.000804,0.000734,0.001071,0.000949,0.000858,0.000684,0.000884,0.000979,0.000638,0.000654,0.000823,0.000974,0.000678,0.000846,2.9e-05,2.9e-05,0.001073,0.00085,0.000716,2.9e-05,2.9e-05,0.001068,0.001103,0.000861,0.000904,0.001065,0.000977,0.001083,0.00111,0.001066,0.001086,0.001086,0.00091,0.001072,0.000811,0.000811,0.000811,0.000748,0.000704,0.000838,0.000704,0.000748,0.000851,0.000851,0.000861,0.000952,0.000952,0.000824,0.00071,0.000704,0.00081,0.001011,0.000723,0.000704,0.000718,0.000952,0.000704,0.000704,0.000852,0.000561,0.000746,0.000743,0.000743,0.000743,0.000743,0.000743,0.000649,0.00074,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.002,0.001634,0.002105,0.001669,0.001481,0.001466,0.001364,0.001838,0.001397,0.001536,0.001653,0.001425,0.001495,0.001153,0.0016,0.001344,0.001543,0.001621,0.001318,0.001538,0.001531,0.001383,0.001515,0.001163,0.001529,0.001211,0.001198,0.001439,0.001401,0.001689,0.001631,0.00135,0.001664,0.000817,0.001242,0.001585,0.001639,0.001307,0.001055,0.001555,0.001511,0.001359,0.001435,0.001684,0.001225,0.001727,0.001094,0.001406,0.001406,0.001406,0.001406,0.001406,0.001269,0.001377,0.001389,0.001372,0.001377,0.001522,0.00097,0.001577,0.001244,0.000764,0.001718,0.001618,0.001656,0.000629,0.000788,0.001706,0.001304,0.001541,0.00161,0.00122,0.001678,0.001658,0.001656,0.001585,0.001675,0.001527,0.000844,0.000844,0.000844,0.000844,0.001193,0.001014,0.001565,0.001587,0.00149,0.001471,0.001256,0.001186,0.001642,0.001271,0.001235,0.001143,0.001241,0.001597,0.000963,0.001377,0.001387,0.001416,0.000891,0.001145,0.000973,0.000973,0.001669,0.001333,0.001335,0.000973,0.001035,0.001686,0.001667,0.001383,0.001381,0.001681,0.001642,0.001675,0.001724,0.001658,0.001681,0.001681,0.001397,0.001667,0.001274,0.001266,0.001274,0.001217,0.001192,0.001332,0.001208,0.001217,0.001264,0.001267,0.001287,0.001546,0.001538,0.001397,0.001362,0.001312,0.001364,0.0016,0.001192,0.001289,0.001179,0.001536,0.000975,0.001427,0.00129,0.00129,0.001046,0.001174,0.00119,0.001174,0.00119,0.001174,0.001321,0.001089,0.001175,0.001192,0.00119,0.001002,0.000842,0.000942,0.000966,0.000917,0.000791,0.000786,0.001028,0.000791,0.000791,0.001269,0.00082,0.000791,0.001309,0.001083,0.000867,0.000946,0.001252,0.000791,0.001252,0.000927,0.000851,0.000851,0.00104,0.000957,0.000841,0.000863,0.000971],[0.024567,0.024134,0.024823,0.023929,0.02405,0.024184,0.023776,0.024458,0.024031,0.024377,0.024235,0.024075,0.023625,0.023646,0.023852,0.023954,0.024047,0.024346,0.023562,0.02384,0.024165,0.024062,0.024093,0.021991,0.022345,0.023904,0.02316,0.02405,0.023929,0.023991,0.02404,0.023911,0.024087,0.022688,0.023929,0.024071,0.024413,0.023332,0.0233,0.024084,0.024197,0.023728,0.023755,0.02447,0.023938,0.024025,0.021903,0.023676,0.023676,0.023676,0.023676,0.023676,0.023607,0.023991,0.022711,0.023935,0.023988,0.022761,0.023008,0.024304,0.024603,0.022407,0.023917,0.023911,0.02405,0.005348,0.022012,0.023947,0.023503,0.0238,0.024106,0.023947,0.02392,0.023935,0.024156,0.023923,0.023944,0.024279,0.02238,0.02238,0.02238,0.02238,0.023889,0.023201,0.023607,0.024112,0.02419,0.022808,0.023868,0.023831,0.023834,0.022393,0.0228,0.023646,0.023978,0.024047,0.022808,0.024019,0.024125,0.024009,0.022426,0.023494,0.02384,0.02384,0.023907,0.02388,0.02392,0.02384,0.024031,0.023957,0.023941,0.024134,0.023972,0.024115,0.024112,0.023889,0.023877,0.02388,0.023892,0.023969,0.023957,0.024062,0.023926,0.023901,0.023926,0.023914,0.023904,0.023911,0.023917,0.023813,0.023889,0.023907,0.023917,0.023904,0.023926,0.024134,0.024028,0.023935,0.023978,0.023846,0.023883,0.023941,0.023837,0.02384,0.023455,0.024146,0.023822,0.023932,0.023215,0.023782,0.023889,0.023782,0.023889,0.023782,0.024022,0.02248,0.024365,0.024474,0.024464,0.023323,0.022833,0.023393,0.023981,0.023752,0.022367,0.022502,0.023538,0.022367,0.022367,0.02426,0.023262,0.022367,0.024071,0.023673,0.023352,0.023512,0.024087,0.022367,0.024087,0.023235,0.023233,0.023233,0.024,0.023282,0.023221,0.023267,0.023991],[0.220903,0.190379,0.214286,0.195173,0.178503,0.14242,0.149758,0.226277,0.172063,0.145997,0.181287,0.154742,0.203057,0.160069,0.193548,0.156434,0.198506,0.183794,0.156434,0.197243,0.183071,0.16622,0.171113,0.162587,0.189024,0.121887,0.15683,0.158568,0.184707,0.203947,0.189217,0.157095,0.197034,0.100324,0.111981,0.189602,0.163588,0.165628,0.139117,0.19893,0.181996,0.185259,0.185075,0.171429,0.145086,0.202614,0.145768,0.156962,0.156962,0.156962,0.156962,0.156962,0.141553,0.158839,0.179884,0.166517,0.180758,0.178332,0.126187,0.167116,0.005348,0.109799,0.203279,0.186747,0.212329,0.005376,0.005405,0.211364,0.153719,0.17782,0.177481,0.138599,0.203947,0.196617,0.186935,0.185444,0.201735,0.180233,0.119537,0.119537,0.119537,0.119537,0.142857,0.10967,0.183613,0.19641,0.145312,0.183251,0.149518,0.136564,0.199143,0.176471,0.159657,0.12731,0.164456,0.182174,0.118698,0.121648,0.153086,0.18111,0.126016,0.15736,0.005348,0.005348,0.199571,0.158029,0.133142,0.005348,0.005348,0.198718,0.205072,0.160069,0.168174,0.198083,0.181641,0.201517,0.206437,0.198294,0.201954,0.201954,0.169245,0.199357,0.150852,0.150852,0.150852,0.139117,0.130986,0.155909,0.130986,0.139117,0.158298,0.158298,0.160207,0.177143,0.177143,0.153339,0.132009,0.130986,0.150607,0.188069,0.13449,0.130986,0.133525,0.177143,0.130986,0.130986,0.158433,0.10426,0.138806,0.13829,0.13829,0.13829,0.13829,0.13829,0.120779,0.137676,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.372,0.303922,0.391579,0.310518,0.275556,0.272727,0.253752,0.341912,0.259777,0.285714,0.307438,0.264957,0.278027,0.214533,0.2976,0.25,0.287037,0.301459,0.245059,0.286154,0.284839,0.257261,0.281818,0.216279,0.284404,0.225182,0.222754,0.267626,0.260504,0.314189,0.303426,0.251012,0.309484,0.151961,0.231056,0.29477,0.304918,0.243137,0.196203,0.289269,0.280967,0.252717,0.266858,0.313131,0.227941,0.321244,0.203501,0.261603,0.261603,0.261603,0.261603,0.261603,0.236041,0.256198,0.258333,0.255144,0.256198,0.283105,0.180407,0.293375,0.231343,0.142093,0.319588,0.300971,0.307947,0.116981,0.146572,0.317406,0.242503,0.286595,0.299517,0.226829,0.312081,0.308458,0.307947,0.29477,0.311558,0.283969,0.156962,0.156962,0.156962,0.156962,0.221957,0.188641,0.29108,0.295238,0.277198,0.273529,0.233668,0.220641,0.305419,0.236341,0.22963,0.212571,0.230769,0.297125,0.179191,0.256198,0.257975,0.263456,0.165775,0.213058,0.180934,0.180934,0.310518,0.248,0.248331,0.180934,0.192547,0.313659,0.31,0.257261,0.256906,0.312605,0.305419,0.311558,0.32069,0.308458,0.312605,0.312605,0.259777,0.31,0.236943,0.235443,0.236943,0.226277,0.221692,0.24767,0.224638,0.226277,0.235145,0.235741,0.239382,0.287481,0.286154,0.259777,0.253406,0.244094,0.253752,0.2976,0.221692,0.239691,0.21934,0.285714,0.181287,0.265335,0.24,0.24,0.194561,0.21831,0.221429,0.21831,0.221429,0.21831,0.245707,0.202614,0.218566,0.221692,0.221429,0.186373,0.156698,0.175141,0.17971,0.170486,0.147152,0.146226,0.191161,0.147152,0.147152,0.236041,0.152584,0.147152,0.243455,0.201517,0.161179,0.17597,0.232791,0.147152,0.232791,0.172382,0.158298,0.158298,0.193347,0.17799,0.156434,0.160483,0.180583],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var22_df_closseness, by=list(var22_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var22_df_closseness, by=list(var22_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-49d37008b1fc7f79553d" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-49d37008b1fc7f79553d">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000123\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001188\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"9.5e-05\" data-max=\"0.000399\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000971\" data-max=\"0.002105\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.1e-05\" data-max=\"0.000322\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022802\" data-max=\"0.024823\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.8e-05\" data-max=\"0.004273\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.220903\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.017812\" data-max=\"0.074286\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.180583\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.007578\" data-max=\"0.059842\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003883\" data-max=\"0.139638\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[0.000128,0.000124,0.000128,0.00013,0.000128,0.000131,0.00013,0.000133,0.00013,0.000128,0.000131,0.000123,0.000128,0.000129,0.000129,0.000126,0.000129,0.000129,0.000132,0.000127,0.000131,0.000129,0.000129],[3e-06,4e-06,null,null,null,null,1e-06,null,null,null,null,2.3e-05,1e-06,0,1e-06,3e-06,null,null,null,1e-06,null,1e-06,1e-06],[0.000928,0.000296,0.00106,0.001024,0.000805,0.000898,0.000847,0.001152,0.00092,0.000852,0.000969,0.000833,0.000928,0.00095,0.000931,0.000906,2.9e-05,0.000974,0.001188,0.000568,0.000988,0.00066,0.000925],[0.000162,0.000364,null,null,null,null,0.00014,null,null,null,null,0.00026,9.5e-05,0.000156,0.000153,0.000158,null,null,null,0.000399,null,0.000285,0.000144],[0.001407,0.001013,0.001538,0.001634,0.001364,0.001577,0.001466,0.002105,0.001515,0.00129,0.001527,0.001376,0.001256,0.001534,0.001497,0.001341,0.000971,0.001416,0.002,0.001224,0.001621,0.001229,0.001485],[0.000322,0.000212,null,null,null,null,5.7e-05,null,null,null,null,0.000316,0.000146,0.000175,4.1e-05,0.000268,null,null,null,0.0002,null,5.9e-05,0.000179],[0.023833,0.023106,0.02384,0.024134,0.023776,0.024304,0.02412,0.024823,0.024093,0.023822,0.024279,0.022802,0.023687,0.02401,0.024094,0.023506,0.023991,0.024009,0.024567,0.02371,0.024346,0.023977,0.023944],[0.000538,0.000694,null,null,null,null,6.2e-05,null,null,null,null,0.004273,5.8e-05,0.000101,7.8e-05,0.000601,null,null,null,0.00022,null,0.000218,8.8e-05],[0.172612,0.055072,0.197243,0.190379,0.149758,0.167116,0.157542,0.214286,0.171113,0.158433,0.180233,0.155018,0.172664,0.176687,0.173143,0.168486,0.005348,0.18111,0.220903,0.105615,0.183794,0.122781,0.172138],[0.030227,0.067689,null,null,null,null,0.026058,null,null,null,null,0.048406,0.017812,0.029093,0.028425,0.029428,null,null,null,0.074286,null,0.053029,0.026863],[0.261671,0.188408,0.286154,0.303922,0.253752,0.293375,0.2726,0.391579,0.281818,0.24,0.283969,0.256001,0.233625,0.285386,0.27844,0.249415,0.180583,0.263456,0.372,0.227725,0.301459,0.228606,0.276129],[0.059842,0.039478,null,null,null,null,0.010661,null,null,null,null,0.058808,0.027,0.032522,0.007578,0.049897,null,null,null,0.03726,null,0.010953,0.033326],[0.300585,0.099063,0.286154,0.343173,0.303922,0.323478,0.300896,0.391579,0.295238,0.29108,0.313659,0.266552,0.319611,0.301223,0.316849,0.295625,0.005348,0.306425,0.371257,0.194167,0.314189,0.234219,0.288627],[0.043014,0.125815,null,null,null,null,0.018146,null,null,null,null,0.070696,0.003883,0.033229,0.029748,0.023781,null,null,null,0.139638,null,0.102732,0.011577]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var22_df_closseness <- data.frame(
var22_incloseness,
var22_outcloseness,
var22_totalcloseness,
var22_incloseness_n,
var22_outcloseness_n,
var22_totalcloseness_n,
var22_centr_closeness) %>% round(6)

#Adding type
var22_df_closseness <-cbind(var22_df_closseness, V(var22)$TIPO1)

#Adding names
names(var22_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var22_df_closseness<-var22_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var22_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-5b681f5fd3b3add54d56" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-5b681f5fd3b3add54d56">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001217\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000629\" data-max=\"0.002105\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024823\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.226277\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.116981\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[0.000132,0.00013,0.000133,0.000129,0.000129,0.00013,0.000128,0.000131,0.000129,0.000131,0.00013,0.000129,0.000127,0.000127,0.000128,0.000129,0.000129,0.000131,0.000127,0.000128,0.00013,0.000129,0.00013,0.000118,0.00012,0.000129,0.000125,0.000129,0.000129,0.000129,0.000129,0.000129,0.00013,0.000122,0.000129,0.000129,0.000131,0.000125,0.000125,0.000129,0.00013,0.000128,0.000128,0.000132,0.000129,0.000129,0.000118,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000129,0.000122,0.000129,0.000129,0.000122,0.000124,0.000131,0.000132,0.00012,0.000129,0.000129,0.000129,2.9e-05,0.000118,0.000129,0.000126,0.000128,0.00013,0.000129,0.000129,0.000129,0.00013,0.000129,0.000129,0.000131,0.00012,0.00012,0.00012,0.00012,0.000128,0.000125,0.000127,0.00013,0.00013,0.000123,0.000128,0.000128,0.000128,0.00012,0.000123,0.000127,0.000129,0.000129,0.000123,0.000129,0.00013,0.000129,0.000121,0.000126,0.000128,0.000128,0.000129,0.000128,0.000129,0.000128,0.000129,0.000129,0.000129,0.00013,0.000129,0.00013,0.00013,0.000128,0.000128,0.000128,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000128,0.000128,0.000129,0.000129,0.000129,0.000129,0.00013,0.000129,0.000129,0.000129,0.000128,0.000128,0.000129,0.000128,0.000128,0.000126,0.00013,0.000128,0.000129,0.000125,0.000128,0.000128,0.000128,0.000128,0.000128,0.000129,0.000121,0.000131,0.000132,0.000132,0.000125,0.000123,0.000126,0.000129,0.000128,0.00012,0.000121,0.000127,0.00012,0.00012,0.00013,0.000125,0.00012,0.000129,0.000127,0.000126,0.000126,0.00013,0.00012,0.00013,0.000125,0.000125,0.000125,0.000129,0.000125,0.000125,0.000125,0.000129],[0.001188,0.001024,0.001152,0.001049,0.00096,0.000766,0.000805,0.001217,0.000925,0.000785,0.000975,0.000832,0.001092,0.000861,0.001041,0.000841,0.001067,0.000988,0.000841,0.00106,0.000984,0.000894,0.00092,0.000874,0.001016,0.000655,0.000843,0.000853,0.000993,0.001096,0.001017,0.000845,0.001059,0.000539,0.000602,0.001019,0.00088,0.00089,0.000748,0.00107,0.000978,0.000996,0.000995,0.000922,0.00078,0.001089,0.000784,0.000844,0.000844,0.000844,0.000844,0.000844,0.000761,0.000854,0.000967,0.000895,0.000972,0.000959,0.000678,0.000898,2.9e-05,0.00059,0.001093,0.001004,0.001142,2.9e-05,2.9e-05,0.001136,0.000826,0.000956,0.000954,0.000745,0.001096,0.001057,0.001005,0.000997,0.001085,0.000969,0.000643,0.000643,0.000643,0.000643,0.000768,0.00059,0.000987,0.001056,0.000781,0.000985,0.000804,0.000734,0.001071,0.000949,0.000858,0.000684,0.000884,0.000979,0.000638,0.000654,0.000823,0.000974,0.000678,0.000846,2.9e-05,2.9e-05,0.001073,0.00085,0.000716,2.9e-05,2.9e-05,0.001068,0.001103,0.000861,0.000904,0.001065,0.000977,0.001083,0.00111,0.001066,0.001086,0.001086,0.00091,0.001072,0.000811,0.000811,0.000811,0.000748,0.000704,0.000838,0.000704,0.000748,0.000851,0.000851,0.000861,0.000952,0.000952,0.000824,0.00071,0.000704,0.00081,0.001011,0.000723,0.000704,0.000718,0.000952,0.000704,0.000704,0.000852,0.000561,0.000746,0.000743,0.000743,0.000743,0.000743,0.000743,0.000649,0.00074,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.002,0.001634,0.002105,0.001669,0.001481,0.001466,0.001364,0.001838,0.001397,0.001536,0.001653,0.001425,0.001495,0.001153,0.0016,0.001344,0.001543,0.001621,0.001318,0.001538,0.001531,0.001383,0.001515,0.001163,0.001529,0.001211,0.001198,0.001439,0.001401,0.001689,0.001631,0.00135,0.001664,0.000817,0.001242,0.001585,0.001639,0.001307,0.001055,0.001555,0.001511,0.001359,0.001435,0.001684,0.001225,0.001727,0.001094,0.001406,0.001406,0.001406,0.001406,0.001406,0.001269,0.001377,0.001389,0.001372,0.001377,0.001522,0.00097,0.001577,0.001244,0.000764,0.001718,0.001618,0.001656,0.000629,0.000788,0.001706,0.001304,0.001541,0.00161,0.00122,0.001678,0.001658,0.001656,0.001585,0.001675,0.001527,0.000844,0.000844,0.000844,0.000844,0.001193,0.001014,0.001565,0.001587,0.00149,0.001471,0.001256,0.001186,0.001642,0.001271,0.001235,0.001143,0.001241,0.001597,0.000963,0.001377,0.001387,0.001416,0.000891,0.001145,0.000973,0.000973,0.001669,0.001333,0.001335,0.000973,0.001035,0.001686,0.001667,0.001383,0.001381,0.001681,0.001642,0.001675,0.001724,0.001658,0.001681,0.001681,0.001397,0.001667,0.001274,0.001266,0.001274,0.001217,0.001192,0.001332,0.001208,0.001217,0.001264,0.001267,0.001287,0.001546,0.001538,0.001397,0.001362,0.001312,0.001364,0.0016,0.001192,0.001289,0.001179,0.001536,0.000975,0.001427,0.00129,0.00129,0.001046,0.001174,0.00119,0.001174,0.00119,0.001174,0.001321,0.001089,0.001175,0.001192,0.00119,0.001002,0.000842,0.000942,0.000966,0.000917,0.000791,0.000786,0.001028,0.000791,0.000791,0.001269,0.00082,0.000791,0.001309,0.001083,0.000867,0.000946,0.001252,0.000791,0.001252,0.000927,0.000851,0.000851,0.00104,0.000957,0.000841,0.000863,0.000971],[0.024567,0.024134,0.024823,0.023929,0.02405,0.024184,0.023776,0.024458,0.024031,0.024377,0.024235,0.024075,0.023625,0.023646,0.023852,0.023954,0.024047,0.024346,0.023562,0.02384,0.024165,0.024062,0.024093,0.021991,0.022345,0.023904,0.02316,0.02405,0.023929,0.023991,0.02404,0.023911,0.024087,0.022688,0.023929,0.024071,0.024413,0.023332,0.0233,0.024084,0.024197,0.023728,0.023755,0.02447,0.023938,0.024025,0.021903,0.023676,0.023676,0.023676,0.023676,0.023676,0.023607,0.023991,0.022711,0.023935,0.023988,0.022761,0.023008,0.024304,0.024603,0.022407,0.023917,0.023911,0.02405,0.005348,0.022012,0.023947,0.023503,0.0238,0.024106,0.023947,0.02392,0.023935,0.024156,0.023923,0.023944,0.024279,0.02238,0.02238,0.02238,0.02238,0.023889,0.023201,0.023607,0.024112,0.02419,0.022808,0.023868,0.023831,0.023834,0.022393,0.0228,0.023646,0.023978,0.024047,0.022808,0.024019,0.024125,0.024009,0.022426,0.023494,0.02384,0.02384,0.023907,0.02388,0.02392,0.02384,0.024031,0.023957,0.023941,0.024134,0.023972,0.024115,0.024112,0.023889,0.023877,0.02388,0.023892,0.023969,0.023957,0.024062,0.023926,0.023901,0.023926,0.023914,0.023904,0.023911,0.023917,0.023813,0.023889,0.023907,0.023917,0.023904,0.023926,0.024134,0.024028,0.023935,0.023978,0.023846,0.023883,0.023941,0.023837,0.02384,0.023455,0.024146,0.023822,0.023932,0.023215,0.023782,0.023889,0.023782,0.023889,0.023782,0.024022,0.02248,0.024365,0.024474,0.024464,0.023323,0.022833,0.023393,0.023981,0.023752,0.022367,0.022502,0.023538,0.022367,0.022367,0.02426,0.023262,0.022367,0.024071,0.023673,0.023352,0.023512,0.024087,0.022367,0.024087,0.023235,0.023233,0.023233,0.024,0.023282,0.023221,0.023267,0.023991],[0.220903,0.190379,0.214286,0.195173,0.178503,0.14242,0.149758,0.226277,0.172063,0.145997,0.181287,0.154742,0.203057,0.160069,0.193548,0.156434,0.198506,0.183794,0.156434,0.197243,0.183071,0.16622,0.171113,0.162587,0.189024,0.121887,0.15683,0.158568,0.184707,0.203947,0.189217,0.157095,0.197034,0.100324,0.111981,0.189602,0.163588,0.165628,0.139117,0.19893,0.181996,0.185259,0.185075,0.171429,0.145086,0.202614,0.145768,0.156962,0.156962,0.156962,0.156962,0.156962,0.141553,0.158839,0.179884,0.166517,0.180758,0.178332,0.126187,0.167116,0.005348,0.109799,0.203279,0.186747,0.212329,0.005376,0.005405,0.211364,0.153719,0.17782,0.177481,0.138599,0.203947,0.196617,0.186935,0.185444,0.201735,0.180233,0.119537,0.119537,0.119537,0.119537,0.142857,0.10967,0.183613,0.19641,0.145312,0.183251,0.149518,0.136564,0.199143,0.176471,0.159657,0.12731,0.164456,0.182174,0.118698,0.121648,0.153086,0.18111,0.126016,0.15736,0.005348,0.005348,0.199571,0.158029,0.133142,0.005348,0.005348,0.198718,0.205072,0.160069,0.168174,0.198083,0.181641,0.201517,0.206437,0.198294,0.201954,0.201954,0.169245,0.199357,0.150852,0.150852,0.150852,0.139117,0.130986,0.155909,0.130986,0.139117,0.158298,0.158298,0.160207,0.177143,0.177143,0.153339,0.132009,0.130986,0.150607,0.188069,0.13449,0.130986,0.133525,0.177143,0.130986,0.130986,0.158433,0.10426,0.138806,0.13829,0.13829,0.13829,0.13829,0.13829,0.120779,0.137676,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.372,0.303922,0.391579,0.310518,0.275556,0.272727,0.253752,0.341912,0.259777,0.285714,0.307438,0.264957,0.278027,0.214533,0.2976,0.25,0.287037,0.301459,0.245059,0.286154,0.284839,0.257261,0.281818,0.216279,0.284404,0.225182,0.222754,0.267626,0.260504,0.314189,0.303426,0.251012,0.309484,0.151961,0.231056,0.29477,0.304918,0.243137,0.196203,0.289269,0.280967,0.252717,0.266858,0.313131,0.227941,0.321244,0.203501,0.261603,0.261603,0.261603,0.261603,0.261603,0.236041,0.256198,0.258333,0.255144,0.256198,0.283105,0.180407,0.293375,0.231343,0.142093,0.319588,0.300971,0.307947,0.116981,0.146572,0.317406,0.242503,0.286595,0.299517,0.226829,0.312081,0.308458,0.307947,0.29477,0.311558,0.283969,0.156962,0.156962,0.156962,0.156962,0.221957,0.188641,0.29108,0.295238,0.277198,0.273529,0.233668,0.220641,0.305419,0.236341,0.22963,0.212571,0.230769,0.297125,0.179191,0.256198,0.257975,0.263456,0.165775,0.213058,0.180934,0.180934,0.310518,0.248,0.248331,0.180934,0.192547,0.313659,0.31,0.257261,0.256906,0.312605,0.305419,0.311558,0.32069,0.308458,0.312605,0.312605,0.259777,0.31,0.236943,0.235443,0.236943,0.226277,0.221692,0.24767,0.224638,0.226277,0.235145,0.235741,0.239382,0.287481,0.286154,0.259777,0.253406,0.244094,0.253752,0.2976,0.221692,0.239691,0.21934,0.285714,0.181287,0.265335,0.24,0.24,0.194561,0.21831,0.221429,0.21831,0.221429,0.21831,0.245707,0.202614,0.218566,0.221692,0.221429,0.186373,0.156698,0.175141,0.17971,0.170486,0.147152,0.146226,0.191161,0.147152,0.147152,0.236041,0.152584,0.147152,0.243455,0.201517,0.161179,0.17597,0.232791,0.147152,0.232791,0.172382,0.158298,0.158298,0.193347,0.17799,0.156434,0.160483,0.180583],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var22_df_closseness, by=list(var22_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var22_df_closseness, by=list(var22_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-949db05b8534d220ca24" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-949db05b8534d220ca24">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000124\" data-max=\"0.000129\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00053\" data-max=\"0.00084\" data-scale=\"5\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000266\" data-max=\"0.000416\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001152\" data-max=\"0.001424\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000216\" data-max=\"0.000295\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.023109\" data-max=\"0.023963\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000206\" data-max=\"0.002144\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.098549\" data-max=\"0.156246\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.049544\" data-max=\"0.07735\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.214178\" data-max=\"0.264913\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.040154\" data-max=\"0.054939\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.173776\" data-max=\"0.2725\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.079091\" data-max=\"0.135432\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[0.000129,0.000124],[1e-06,1.2e-05],[0.00084,0.00053],[0.000266,0.000416],[0.001424,0.001152],[0.000216,0.000295],[0.023963,0.023109],[0.000206,0.002144],[0.156246,0.098549],[0.049544,0.07735],[0.264913,0.214178],[0.040154,0.054939],[0.2725,0.173776],[0.079091,0.135432]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var22_df_closseness <- data.frame(
var22_incloseness,
var22_outcloseness,
var22_totalcloseness,
var22_incloseness_n,
var22_outcloseness_n,
var22_totalcloseness_n,
var22_centr_closeness) %>% round(6)

#Adding type
var22_df_closseness <-cbind(var22_df_closseness, V(var22)$TIPO2)

#Adding names
names(var22_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var22_df_closseness<-var22_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var22_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-68d44f28048c482167c5" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-68d44f28048c482167c5">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001217\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000629\" data-max=\"0.002105\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024823\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.226277\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.116981\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[0.000132,0.00013,0.000133,0.000129,0.000129,0.00013,0.000128,0.000131,0.000129,0.000131,0.00013,0.000129,0.000127,0.000127,0.000128,0.000129,0.000129,0.000131,0.000127,0.000128,0.00013,0.000129,0.00013,0.000118,0.00012,0.000129,0.000125,0.000129,0.000129,0.000129,0.000129,0.000129,0.00013,0.000122,0.000129,0.000129,0.000131,0.000125,0.000125,0.000129,0.00013,0.000128,0.000128,0.000132,0.000129,0.000129,0.000118,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000129,0.000122,0.000129,0.000129,0.000122,0.000124,0.000131,0.000132,0.00012,0.000129,0.000129,0.000129,2.9e-05,0.000118,0.000129,0.000126,0.000128,0.00013,0.000129,0.000129,0.000129,0.00013,0.000129,0.000129,0.000131,0.00012,0.00012,0.00012,0.00012,0.000128,0.000125,0.000127,0.00013,0.00013,0.000123,0.000128,0.000128,0.000128,0.00012,0.000123,0.000127,0.000129,0.000129,0.000123,0.000129,0.00013,0.000129,0.000121,0.000126,0.000128,0.000128,0.000129,0.000128,0.000129,0.000128,0.000129,0.000129,0.000129,0.00013,0.000129,0.00013,0.00013,0.000128,0.000128,0.000128,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000128,0.000128,0.000129,0.000129,0.000129,0.000129,0.00013,0.000129,0.000129,0.000129,0.000128,0.000128,0.000129,0.000128,0.000128,0.000126,0.00013,0.000128,0.000129,0.000125,0.000128,0.000128,0.000128,0.000128,0.000128,0.000129,0.000121,0.000131,0.000132,0.000132,0.000125,0.000123,0.000126,0.000129,0.000128,0.00012,0.000121,0.000127,0.00012,0.00012,0.00013,0.000125,0.00012,0.000129,0.000127,0.000126,0.000126,0.00013,0.00012,0.00013,0.000125,0.000125,0.000125,0.000129,0.000125,0.000125,0.000125,0.000129],[0.001188,0.001024,0.001152,0.001049,0.00096,0.000766,0.000805,0.001217,0.000925,0.000785,0.000975,0.000832,0.001092,0.000861,0.001041,0.000841,0.001067,0.000988,0.000841,0.00106,0.000984,0.000894,0.00092,0.000874,0.001016,0.000655,0.000843,0.000853,0.000993,0.001096,0.001017,0.000845,0.001059,0.000539,0.000602,0.001019,0.00088,0.00089,0.000748,0.00107,0.000978,0.000996,0.000995,0.000922,0.00078,0.001089,0.000784,0.000844,0.000844,0.000844,0.000844,0.000844,0.000761,0.000854,0.000967,0.000895,0.000972,0.000959,0.000678,0.000898,2.9e-05,0.00059,0.001093,0.001004,0.001142,2.9e-05,2.9e-05,0.001136,0.000826,0.000956,0.000954,0.000745,0.001096,0.001057,0.001005,0.000997,0.001085,0.000969,0.000643,0.000643,0.000643,0.000643,0.000768,0.00059,0.000987,0.001056,0.000781,0.000985,0.000804,0.000734,0.001071,0.000949,0.000858,0.000684,0.000884,0.000979,0.000638,0.000654,0.000823,0.000974,0.000678,0.000846,2.9e-05,2.9e-05,0.001073,0.00085,0.000716,2.9e-05,2.9e-05,0.001068,0.001103,0.000861,0.000904,0.001065,0.000977,0.001083,0.00111,0.001066,0.001086,0.001086,0.00091,0.001072,0.000811,0.000811,0.000811,0.000748,0.000704,0.000838,0.000704,0.000748,0.000851,0.000851,0.000861,0.000952,0.000952,0.000824,0.00071,0.000704,0.00081,0.001011,0.000723,0.000704,0.000718,0.000952,0.000704,0.000704,0.000852,0.000561,0.000746,0.000743,0.000743,0.000743,0.000743,0.000743,0.000649,0.00074,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.002,0.001634,0.002105,0.001669,0.001481,0.001466,0.001364,0.001838,0.001397,0.001536,0.001653,0.001425,0.001495,0.001153,0.0016,0.001344,0.001543,0.001621,0.001318,0.001538,0.001531,0.001383,0.001515,0.001163,0.001529,0.001211,0.001198,0.001439,0.001401,0.001689,0.001631,0.00135,0.001664,0.000817,0.001242,0.001585,0.001639,0.001307,0.001055,0.001555,0.001511,0.001359,0.001435,0.001684,0.001225,0.001727,0.001094,0.001406,0.001406,0.001406,0.001406,0.001406,0.001269,0.001377,0.001389,0.001372,0.001377,0.001522,0.00097,0.001577,0.001244,0.000764,0.001718,0.001618,0.001656,0.000629,0.000788,0.001706,0.001304,0.001541,0.00161,0.00122,0.001678,0.001658,0.001656,0.001585,0.001675,0.001527,0.000844,0.000844,0.000844,0.000844,0.001193,0.001014,0.001565,0.001587,0.00149,0.001471,0.001256,0.001186,0.001642,0.001271,0.001235,0.001143,0.001241,0.001597,0.000963,0.001377,0.001387,0.001416,0.000891,0.001145,0.000973,0.000973,0.001669,0.001333,0.001335,0.000973,0.001035,0.001686,0.001667,0.001383,0.001381,0.001681,0.001642,0.001675,0.001724,0.001658,0.001681,0.001681,0.001397,0.001667,0.001274,0.001266,0.001274,0.001217,0.001192,0.001332,0.001208,0.001217,0.001264,0.001267,0.001287,0.001546,0.001538,0.001397,0.001362,0.001312,0.001364,0.0016,0.001192,0.001289,0.001179,0.001536,0.000975,0.001427,0.00129,0.00129,0.001046,0.001174,0.00119,0.001174,0.00119,0.001174,0.001321,0.001089,0.001175,0.001192,0.00119,0.001002,0.000842,0.000942,0.000966,0.000917,0.000791,0.000786,0.001028,0.000791,0.000791,0.001269,0.00082,0.000791,0.001309,0.001083,0.000867,0.000946,0.001252,0.000791,0.001252,0.000927,0.000851,0.000851,0.00104,0.000957,0.000841,0.000863,0.000971],[0.024567,0.024134,0.024823,0.023929,0.02405,0.024184,0.023776,0.024458,0.024031,0.024377,0.024235,0.024075,0.023625,0.023646,0.023852,0.023954,0.024047,0.024346,0.023562,0.02384,0.024165,0.024062,0.024093,0.021991,0.022345,0.023904,0.02316,0.02405,0.023929,0.023991,0.02404,0.023911,0.024087,0.022688,0.023929,0.024071,0.024413,0.023332,0.0233,0.024084,0.024197,0.023728,0.023755,0.02447,0.023938,0.024025,0.021903,0.023676,0.023676,0.023676,0.023676,0.023676,0.023607,0.023991,0.022711,0.023935,0.023988,0.022761,0.023008,0.024304,0.024603,0.022407,0.023917,0.023911,0.02405,0.005348,0.022012,0.023947,0.023503,0.0238,0.024106,0.023947,0.02392,0.023935,0.024156,0.023923,0.023944,0.024279,0.02238,0.02238,0.02238,0.02238,0.023889,0.023201,0.023607,0.024112,0.02419,0.022808,0.023868,0.023831,0.023834,0.022393,0.0228,0.023646,0.023978,0.024047,0.022808,0.024019,0.024125,0.024009,0.022426,0.023494,0.02384,0.02384,0.023907,0.02388,0.02392,0.02384,0.024031,0.023957,0.023941,0.024134,0.023972,0.024115,0.024112,0.023889,0.023877,0.02388,0.023892,0.023969,0.023957,0.024062,0.023926,0.023901,0.023926,0.023914,0.023904,0.023911,0.023917,0.023813,0.023889,0.023907,0.023917,0.023904,0.023926,0.024134,0.024028,0.023935,0.023978,0.023846,0.023883,0.023941,0.023837,0.02384,0.023455,0.024146,0.023822,0.023932,0.023215,0.023782,0.023889,0.023782,0.023889,0.023782,0.024022,0.02248,0.024365,0.024474,0.024464,0.023323,0.022833,0.023393,0.023981,0.023752,0.022367,0.022502,0.023538,0.022367,0.022367,0.02426,0.023262,0.022367,0.024071,0.023673,0.023352,0.023512,0.024087,0.022367,0.024087,0.023235,0.023233,0.023233,0.024,0.023282,0.023221,0.023267,0.023991],[0.220903,0.190379,0.214286,0.195173,0.178503,0.14242,0.149758,0.226277,0.172063,0.145997,0.181287,0.154742,0.203057,0.160069,0.193548,0.156434,0.198506,0.183794,0.156434,0.197243,0.183071,0.16622,0.171113,0.162587,0.189024,0.121887,0.15683,0.158568,0.184707,0.203947,0.189217,0.157095,0.197034,0.100324,0.111981,0.189602,0.163588,0.165628,0.139117,0.19893,0.181996,0.185259,0.185075,0.171429,0.145086,0.202614,0.145768,0.156962,0.156962,0.156962,0.156962,0.156962,0.141553,0.158839,0.179884,0.166517,0.180758,0.178332,0.126187,0.167116,0.005348,0.109799,0.203279,0.186747,0.212329,0.005376,0.005405,0.211364,0.153719,0.17782,0.177481,0.138599,0.203947,0.196617,0.186935,0.185444,0.201735,0.180233,0.119537,0.119537,0.119537,0.119537,0.142857,0.10967,0.183613,0.19641,0.145312,0.183251,0.149518,0.136564,0.199143,0.176471,0.159657,0.12731,0.164456,0.182174,0.118698,0.121648,0.153086,0.18111,0.126016,0.15736,0.005348,0.005348,0.199571,0.158029,0.133142,0.005348,0.005348,0.198718,0.205072,0.160069,0.168174,0.198083,0.181641,0.201517,0.206437,0.198294,0.201954,0.201954,0.169245,0.199357,0.150852,0.150852,0.150852,0.139117,0.130986,0.155909,0.130986,0.139117,0.158298,0.158298,0.160207,0.177143,0.177143,0.153339,0.132009,0.130986,0.150607,0.188069,0.13449,0.130986,0.133525,0.177143,0.130986,0.130986,0.158433,0.10426,0.138806,0.13829,0.13829,0.13829,0.13829,0.13829,0.120779,0.137676,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.372,0.303922,0.391579,0.310518,0.275556,0.272727,0.253752,0.341912,0.259777,0.285714,0.307438,0.264957,0.278027,0.214533,0.2976,0.25,0.287037,0.301459,0.245059,0.286154,0.284839,0.257261,0.281818,0.216279,0.284404,0.225182,0.222754,0.267626,0.260504,0.314189,0.303426,0.251012,0.309484,0.151961,0.231056,0.29477,0.304918,0.243137,0.196203,0.289269,0.280967,0.252717,0.266858,0.313131,0.227941,0.321244,0.203501,0.261603,0.261603,0.261603,0.261603,0.261603,0.236041,0.256198,0.258333,0.255144,0.256198,0.283105,0.180407,0.293375,0.231343,0.142093,0.319588,0.300971,0.307947,0.116981,0.146572,0.317406,0.242503,0.286595,0.299517,0.226829,0.312081,0.308458,0.307947,0.29477,0.311558,0.283969,0.156962,0.156962,0.156962,0.156962,0.221957,0.188641,0.29108,0.295238,0.277198,0.273529,0.233668,0.220641,0.305419,0.236341,0.22963,0.212571,0.230769,0.297125,0.179191,0.256198,0.257975,0.263456,0.165775,0.213058,0.180934,0.180934,0.310518,0.248,0.248331,0.180934,0.192547,0.313659,0.31,0.257261,0.256906,0.312605,0.305419,0.311558,0.32069,0.308458,0.312605,0.312605,0.259777,0.31,0.236943,0.235443,0.236943,0.226277,0.221692,0.24767,0.224638,0.226277,0.235145,0.235741,0.239382,0.287481,0.286154,0.259777,0.253406,0.244094,0.253752,0.2976,0.221692,0.239691,0.21934,0.285714,0.181287,0.265335,0.24,0.24,0.194561,0.21831,0.221429,0.21831,0.221429,0.21831,0.245707,0.202614,0.218566,0.221692,0.221429,0.186373,0.156698,0.175141,0.17971,0.170486,0.147152,0.146226,0.191161,0.147152,0.147152,0.236041,0.152584,0.147152,0.243455,0.201517,0.161179,0.17597,0.232791,0.147152,0.232791,0.172382,0.158298,0.158298,0.193347,0.17799,0.156434,0.160483,0.180583],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var22_df_closseness, by=list(var22_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var22_df_closseness, by=list(var22_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-12dfead0c382cf2bce60" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-12dfead0c382cf2bce60">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000124\" data-max=\"0.000129\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000526\" data-max=\"0.000956\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000135\" data-max=\"0.000417\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00115\" data-max=\"0.001533\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000139\" data-max=\"0.000297\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.023097\" data-max=\"0.024062\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000116\" data-max=\"0.002156\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.097832\" data-max=\"0.177925\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.025152\" data-max=\"0.077604\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.21389\" data-max=\"0.285214\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.025806\" data-max=\"0.055256\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172406\" data-max=\"0.308329\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.029295\" data-max=\"0.135782\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[0.000129,0.000129,0.000124],[1e-06,1e-06,1.2e-05],[0.000956,0.000821,0.000526],[0.000135,0.000276,0.000417],[0.001533,0.001405,0.00115],[0.000139,0.00022,0.000297],[0.024062,0.023949,0.023097],[0.000116,0.000211,0.002156],[0.177925,0.152768,0.097832],[0.025152,0.051368,0.077604],[0.285214,0.261373,0.21389],[0.025806,0.041017,0.055256],[0.308329,0.266869,0.172406],[0.029295,0.082605,0.135782]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var22_data.RData")
```

