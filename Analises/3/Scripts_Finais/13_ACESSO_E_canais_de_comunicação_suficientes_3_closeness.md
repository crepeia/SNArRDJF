# SNA Closeness 13_ACESSO E) Os canais de comunicação com este serviço são suficientes para realização de ações em conjunto. (var11)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 13_ACESSO E) Os canais de comunicação com este serviço são suficientes para realização de ações em conjunto. (var11)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var11_data.RData")
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
#var11<-simplify(var11) #Simplify
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
V(var11)$incloseness <- closeness(var11, mode = "in", weights = E(var11)$var11) %>% round(6)
V(var11)$outcloseness <- closeness(var11, mode = "out", weights = E(var11)$var11) %>% round(6)
V(var11)$totalcloseness <- closeness(var11, mode = "total", weights = E(var11)$var11) %>% round(4)
```

###Saving to Environment

```r
var11_incloseness<- closeness(var11, mode = "in", weights = E(var11)$var11) %>% round(6)
var11_outcloseness<- closeness(var11, mode = "out", weights = E(var11)$var11) %>% round(6)
var11_totalcloseness<- closeness(var11, mode = "total", weights = E(var11)$var11) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var11_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0001270 0.0001280 0.0001269 0.0001290 0.0001340
```

```r
sd(var11_incloseness)
```

```
## [1] 7.818822e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var11)$incloseness<-closeness(var11, weights = E(var11)$var11, mode="in")

#Get Variable
V(var11)$var11_color_degree<-round(V(var11)$incloseness,6)

#Creating brewer pallette
vertex_var11_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var11)$var11_color_degree)), "RdBu"))(
            length(unique(V(var11)$var11_color_degree)))

#Saving as Vertex properties 
V(var11)$vertex_var11_color_degree<-
  vertex_var11_color_degree[as.numeric(
  cut(V(var11)$var11_color_degree,
      breaks=length(unique(V(var11)$var11_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var11, es=E(var11), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var11))
maxC <- rep(Inf, vcount(var11))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var11, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var11)$weight)


#PLotting
plot(var11, 
     layout=co,
     edge.color=V(var11)$vertex_var11_color_degree[edge.start],
     edge.arrow.size=closeness(var11, weights = E(var11)$var11, mode="in"),
     edge.width=E(var11)$weight/mean(E(var11)$weight),
     edge.curved = TRUE,
     vertex.color=V(var11)$vertex_var11_color_degree,
     vertex.size=closeness(var11, weights = E(var11)$var11, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var11,"LABEL_COR"),
     vertex.label.cex=(closeness(var11, weights = E(var11)$var11, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var11)$var11_color_degree
b<-V(var11)$vertex_var11_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 13_ACESSO E) Os canais de comunicação com este serviço são suficientes para realização de ações em conjunto. (var11)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var11, mode="in", weights = E(var11)$var11)), 
             sd(closeness(var11, mode="in", weights = E(var11)$var11))
             )
       )
```

![](13_ACESSO_E_canais_de_comunicação_suficientes_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var11_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0006260 0.0008050 0.0007168 0.0010040 0.0013850
```

```r
sd(var11_outcloseness)
```

```
## [1] 0.0003835739
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var11)$outcloseness<-closeness(var11, weights = E(var11)$var11, mode="out")

#Get Variable
V(var11)$var11_color_degree<-round(V(var11)$outcloseness,6)

#Creating brewer pallette
vertex_var11_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var11)$var11_color_degree)), "RdBu"))(
            length(unique(V(var11)$var11_color_degree)))

#Saving as Vertex properties 
V(var11)$vertex_var11_color_degree<-
  vertex_var11_color_degree[as.numeric(
  cut(V(var11)$var11_color_degree,
      breaks=length(unique(V(var11)$var11_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var11, es=E(var11), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var11))
maxC <- rep(Inf, vcount(var11))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var11, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var11)$weight)


#PLotting
plot(var11, 
     layout=co,
     edge.color=V(var11)$vertex_var11_color_degree[edge.start],
     edge.arrow.size=closeness(var11, weights = E(var11)$var11, mode="out"),
     edge.width=E(var11)$weight/2*mean(E(var11)$weight),
     edge.curved = TRUE,
     vertex.color=V(var11)$vertex_var11_color_degree,
     vertex.size=closeness(var11, weights = E(var11)$var11, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var11,"LABEL_COR"),
     vertex.label.cex=closeness(var11, weights = E(var11)$var11, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var11)$var11_color_degree
b<-V(var11)$vertex_var11_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 13_ACESSO E) Os canais de comunicação com este serviço são suficientes para realização de ações em conjunto. (var11)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var11, mode="out", weights = E(var11)$var11)), 
             sd(closeness(var11, mode="out", weights = E(var11)$var11))
             )
       )
```

![](13_ACESSO_E_canais_de_comunicação_suficientes_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var11_totalcloseness)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000732 0.001228 0.001464 0.001426 0.001645 0.002179
```

```r
sd(var11_totalcloseness)
```

```
## [1] 0.0003151202
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var11)$allcloseness<-closeness(var11, weights = E(var11)$var11, mode="all")

#Get Variable
V(var11)$var11_color_degree<-round(V(var11)$allcloseness,6)

#Creating brewer pallette
vertex_var11_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var11)$var11_color_degree)), "RdBu"))(
            length(unique(V(var11)$var11_color_degree)))

#Saving as Vertex properties 
V(var11)$vertex_var11_color_degree<-
  vertex_var11_color_degree[as.numeric(
  cut(V(var11)$var11_color_degree,
      breaks=length(unique(V(var11)$var11_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var11, es=E(var11), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var11))
maxC <- rep(Inf, vcount(var11))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var11, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var11)$weight)


#PLotting
plot(var11, 
     layout=co,
     edge.color=V(var11)$vertex_var11_color_degree[edge.start],
     edge.arrow.size=closeness(var11, weights = E(var11)$var11, mode="all"),
     edge.width=E(var11)$weight/2*mean(E(var11)$weight),
     edge.curved = TRUE,
     vertex.color=V(var11)$vertex_var11_color_degree,
     vertex.size=closeness(var11, weights = E(var11)$var11, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var11,"LABEL_COR"),
     vertex.label.cex=(closeness(var11, weights = E(var11)$var11, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var11)$var11_color_degree
b<-V(var11)$vertex_var11_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 13_ACESSO E) Os canais de comunicação com este serviço são suficientes para realização de ações em conjunto. (var11)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var11, mode="all", weights = E(var11)$var11)), 
             sd(closeness(var11, mode="all", weights = E(var11)$var11))
             )
       )
```

![](13_ACESSO_E_canais_de_comunicação_suficientes_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var11)$incloseness_n <- closeness(var11, mode = "in",, weights = E(var11)$var11, normalized = T) %>% round(10)
V(var11)$outcloseness_n <- closeness(var11, mode = "out", normalized = T, weights = E(var11)$var11) %>% round(6)
V(var11)$totalcloseness_n <- closeness(var11, mode = "total", normalized = T, weights = E(var11)$var11) %>% round(6)
```

###Saving to Environment

```r
var11_incloseness_n<- closeness(var11, mode = "in", normalized = T, weights = E(var11)$var11) %>% round(6)
var11_outcloseness_n<- closeness(var11, mode = "out", normalized = T, weights = E(var11)$var11) %>% round(6)
var11_totalcloseness_n<- closeness(var11, mode = "total", normalized = T, weights = E(var11)$var11) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var11_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.023610 0.023880 0.023600 0.024020 0.024840
```

```r
sd(var11_incloseness_n)
```

```
## [1] 0.001454879
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var11)$incloseness_n<-closeness(var11, weights = E(var11)$var11, mode="in", normalized = T)

#Get Variable
V(var11)$var11_color_degree<-round(V(var11)$incloseness_n,6)

#Creating brewer pallette
vertex_var11_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var11)$var11_color_degree)), "RdBu"))(
            length(unique(V(var11)$var11_color_degree)))

#Saving as Vertex properties 
V(var11)$vertex_var11_color_degree<-
  vertex_var11_color_degree[as.numeric(
  cut(V(var11)$var11_color_degree,
      breaks=length(unique(V(var11)$var11_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var11, es=E(var11), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var11))
maxC <- rep(Inf, vcount(var11))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var11, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var11)$weight)


#PLotting
plot(var11, 
     layout=co,
     edge.color=V(var11)$vertex_var11_color_degree[edge.start],
     edge.arrow.size=closeness(var11, weights = E(var11)$var11, mode="in",normalized = T),
     edge.width=E(var11)$weight/10*mean(E(var11)$weight),
     edge.curved = TRUE,
     vertex.color=V(var11)$vertex_var11_color_degree,
     vertex.size=(closeness(var11, weights = E(var11)$var11, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var11,"LABEL_COR"),
     vertex.label.cex=closeness(var11, weights = E(var11)$var11, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var11)$var11_color_degree
b<-V(var11)$vertex_var11_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 13_ACESSO E) Os canais de comunicação com este serviço são suficientes para realização de ações em conjunto. (var11)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var11, mode="in", weights = E(var11)$var11, normalized = T)), 
             sd(closeness(var11, mode="in", weights = E(var11)$var11, normalized = T))
             )
       )
```

![](13_ACESSO_E_canais_de_comunicação_suficientes_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var11_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.116400 0.149800 0.133300 0.186700 0.257600
```

```r
sd(var11_outcloseness_n)
```

```
## [1] 0.07135868
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var11)$outcloseness_n<-closeness(var11, weights = E(var11)$var11, mode="out", normalized = T)

#Get Variable
V(var11)$var11_color_degree<-round(V(var11)$outcloseness_n,6)

#Creating brewer pallette
vertex_var11_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var11)$var11_color_degree)), "RdBu"))(
            length(unique(V(var11)$var11_color_degree)))

#Saving as Vertex properties 
V(var11)$vertex_var11_color_degree<-
  vertex_var11_color_degree[as.numeric(
  cut(V(var11)$var11_color_degree,
      breaks=length(unique(V(var11)$var11_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var11, es=E(var11), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var11))
maxC <- rep(Inf, vcount(var11))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var11, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var11)$weight)


#PLotting
plot(var11, 
     layout=co,
     edge.color=V(var11)$vertex_var11_color_degree[edge.start],
     edge.arrow.size=closeness(var11, weights = E(var11)$var11, mode="out",normalized = T),
     edge.width=E(var11)$weight/10*mean(E(var11)$weight),
     edge.curved = TRUE,
     vertex.color=V(var11)$vertex_var11_color_degree,
     vertex.size=(closeness(var11, weights = E(var11)$var11, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var11,"LABEL_COR"),
     vertex.label.cex=closeness(var11, weights = E(var11)$var11, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var11)$var11_color_degree
b<-V(var11)$vertex_var11_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 13_ACESSO E) Os canais de comunicação com este serviço são suficientes para realização de ações em conjunto. (var11)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var11, mode="out", weights = E(var11)$var11, normalized = T)), 
             sd(closeness(var11, mode="out", weights = E(var11)$var11, normalized = T))
             )
       )
```

![](13_ACESSO_E_canais_de_comunicação_suficientes_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var11_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1361  0.2284  0.2723  0.2652  0.3059  0.4052
```

```r
sd(var11_totalcloseness_n)
```

```
## [1] 0.0586147
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var11)$allcloseness_n<-closeness(var11, weights = E(var11)$var11, mode="all", normalized = T)

#Get Variable
V(var11)$var11_color_degree<-round(V(var11)$allcloseness_n,6)

#Creating brewer pallette
vertex_var11_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var11)$var11_color_degree)), "RdBu"))(
            length(unique(V(var11)$var11_color_degree)))

#Saving as Vertex properties 
V(var11)$vertex_var11_color_degree<-
  vertex_var11_color_degree[as.numeric(
  cut(V(var11)$var11_color_degree,
      breaks=length(unique(V(var11)$var11_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var11, es=E(var11), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var11))
maxC <- rep(Inf, vcount(var11))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var11, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var11)$weight)


#PLotting
plot(var11, 
     layout=co,
     edge.color=V(var11)$vertex_var11_color_degree[edge.start],
     edge.arrow.size=closeness(var11, weights = E(var11)$var11, mode="all",normalized = T),
     edge.width=E(var11)$weight/10*mean(E(var11)$weight),
     edge.curved = TRUE,
     vertex.color=V(var11)$vertex_var11_color_degree,
     vertex.size=(closeness(var11, weights = E(var11)$var11, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var11,"LABEL_COR"),
     vertex.label.cex=closeness(var11, weights = E(var11)$var11, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var11)$var11_color_degree
b<-V(var11)$vertex_var11_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 13_ACESSO E) Os canais de comunicação com este serviço são suficientes para realização de ações em conjunto. (var11)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var11, mode="all", weights = E(var11)$var11, normalized = T)), 
             sd(closeness(var11, mode="all", weights = E(var11)$var11, normalized = T))
             )
       )
```

![](13_ACESSO_E_canais_de_comunicação_suficientes_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var11)$incloseness_n <- closeness(var11, weights = E(var11)$var11, mode = "in", normalized = T) %>% round(6)
V(var11)$outcloseness_n <- closeness(var11, weights = E(var11)$var11, mode = "out", normalized = T) %>% round(6)
V(var11)$totalcloseness_n <- closeness(var11, weights = E(var11)$var11, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var11)$var11_centr_closeness<- centralization.closeness(var11)$res
var11_centr_closeness<- centralization.closeness(var11)$res
var11_centr_closeness_all<- centralization.closeness(var11)
```

###Centralization

```r
var11_centr_closeness_all$centralization
```

```
## [1] 0.1624353
```

###Theoretical Max

```r
var11_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var11)$var11_centr_closeness<- centralization.closeness(var11)$res

#Get Variable
V(var11)$var11_color_degree<-round(V(var11)$var11_centr_closeness,6)

#Creating brewer pallette
vertex_var11_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var11)$var11_color_degree)), "Spectral"))(
            length(unique(V(var11)$var11_color_degree)))

#Saving as Vertex properties 
V(var11)$vertex_var11_color_degree<-
  vertex_var11_color_degree[as.numeric(
  cut(V(var11)$var11_color_degree,
      breaks=length(unique(V(var11)$var11_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var11, es=E(var11), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var11))
maxC <- rep(Inf, vcount(var11))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var11, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var11)$weight)


#PLotting
plot(var11, 
     layout=co,
     edge.color=V(var11)$vertex_var11_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var11)$res,
     edge.width=E(var11)$weight/10*mean(E(var11)$weight),
     edge.curved = TRUE,
     vertex.color=V(var11)$vertex_var11_color_degree,
     vertex.size=centralization.closeness(var11)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var11,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var11)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var11)$var11_color_degree
b<-V(var11)$vertex_var11_color_degree
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
  title("Network Centralization Closeness - 13_ACESSO E) Os canais de comunicação com este serviço são suficientes para realização de ações em conjunto. (var11)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var11)$res), 
             sd(centralization.closeness(var11)$res)
             )
       )
```

![](13_ACESSO_E_canais_de_comunicação_suficientes_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var11_incloseness<- closeness(var11, weights = E(var11)$var11, mode = "in") %>% round(6)
var11_outcloseness<- closeness(var11, weights = E(var11)$var11, mode = "out") %>% round(6)
var11_totalcloseness<- closeness(var11, weights = E(var11)$var11, mode = "total") %>% round(6)
var11_incloseness_n<- closeness(var11,weights = E(var11)$var11, mode = "in", normalized = T) %>% round(6)
var11_outcloseness_n<- closeness(var11,weights = E(var11)$var11, mode = "out", normalized = T) %>% round(6)
var11_totalcloseness_n<- closeness(var11,weights = E(var11)$var11, mode = "total", normalized = T) %>% round(6)
var11_centr_closeness <- centralization.closeness(var11)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var11_df_closseness <- data.frame(
var11_incloseness,
var11_outcloseness,
var11_totalcloseness,
var11_incloseness_n,
var11_outcloseness_n,
var11_totalcloseness_n,
var11_centr_closeness) %>% round(6)

#Adding type
var11_df_closseness <-cbind(var11_df_closseness, V(var11)$LABEL_COR)

#Adding names
names(var11_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var11_df_closseness<-var11_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var11_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-7668a0423f1436fcbac4" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-7668a0423f1436fcbac4">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000134\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001385\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000732\" data-max=\"0.002179\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024836\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.257618\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.136064\" data-max=\"0.405229\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[0.000132,0.00013,0.000134,0.000129,0.00013,0.000129,0.000128,0.000129,0.000128,0.000127,0.00013,0.000129,0.000126,0.000126,0.000129,0.000126,0.000129,0.000131,0.000125,0.00013,0.000129,0.000128,0.000129,0.000121,0.000119,0.000126,0.000125,0.000129,0.000128,0.00013,0.00013,0.000128,0.00013,0.000122,0.000129,0.000128,0.000129,0.000124,0.000124,0.00013,0.00013,0.000127,0.000128,0.000129,0.000128,0.000129,0.00012,0.000127,0.000127,0.000127,0.000127,0.000127,0.000125,0.000129,0.000123,0.000129,0.00013,0.000125,0.000124,0.000131,0.000131,0.00012,0.000128,0.000128,0.000127,2.9e-05,0.000119,0.000127,0.000122,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.00013,0.000119,0.000119,0.000119,0.000119,0.000129,0.000128,0.000126,0.000129,0.000129,0.00012,0.000128,0.000128,0.000128,0.00012,0.000123,0.000126,0.00013,0.000128,0.00012,0.000127,0.000129,0.00013,0.00012,0.000127,0.000127,0.000127,0.000128,0.000128,0.000128,0.000127,0.000128,0.000128,0.000128,0.000129,0.000129,0.000129,0.000129,0.000128,0.000128,0.000128,0.000129,0.00013,0.000128,0.00013,0.000129,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.000128,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.00013,0.000129,0.000129,0.000128,0.000129,0.000129,0.000128,0.000128,0.000126,0.000129,0.000128,0.00013,0.000126,0.000128,0.000128,0.000128,0.000128,0.000128,0.000128,0.00012,0.000131,0.000132,0.000132,0.000122,0.000124,0.000129,0.000132,0.00013,0.000122,0.000122,0.000129,0.000122,0.000122,0.00013,0.000125,0.000122,0.00013,0.00013,0.00013,0.00013,0.000127,0.000122,0.000127,0.00013,0.000129,0.000129,0.000128,0.000129,0.000125,0.000128,0.000129],[0.001244,0.001159,0.001056,0.000959,0.000811,0.001029,0.000812,0.001242,0.000867,0.000801,0.000874,0.000925,0.000932,0.000878,0.000917,0.000995,0.001036,0.001209,0.000821,0.000778,0.000945,0.000786,0.001018,0.000876,0.001083,0.000668,0.000866,0.001034,0.001385,0.00093,0.001182,0.000806,0.001126,0.000532,0.000665,0.001045,0.00085,0.00075,0.000757,0.000874,0.000962,0.000897,0.000895,0.001164,0.000803,0.001149,0.000805,0.000993,0.000993,0.000993,0.000993,0.000993,0.000786,0.000876,0.000834,0.000858,0.001029,0.000838,0.000772,0.000974,2.9e-05,0.000543,0.001148,0.000979,0.001159,2.9e-05,2.9e-05,0.001175,0.000778,0.000889,0.000889,0.000758,0.001115,0.001082,0.000845,0.001027,0.001126,0.001193,0.000626,0.000626,0.000626,0.000626,0.000752,0.00059,0.000948,0.001152,0.001049,0.000743,0.001134,0.000729,0.001031,0.000805,0.000742,0.000724,0.000942,0.001037,0.000527,0.000664,0.000894,0.001074,0.000693,0.001055,2.9e-05,2.9e-05,0.001037,0.000799,0.000703,2.9e-05,2.9e-05,0.001091,0.001064,0.001091,0.000864,0.001034,0.001129,0.001072,0.001211,0.000831,0.001129,0.001129,0.001088,0.001052,0.000762,0.000762,0.000762,0.000763,0.000886,0.000741,0.000886,0.000763,0.001012,0.001012,0.000894,0.000667,0.000886,0.000762,0.000801,0.000667,0.000886,0.001013,0.000668,0.000667,0.000969,0.000667,0.000667,0.000667,0.000737,0.000537,0.000724,0.000706,0.000706,0.000706,0.000706,0.000706,0.000581,0.000653,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.002165,0.001916,0.002179,0.0016,0.001647,0.001637,0.001447,0.001988,0.001513,0.001534,0.001724,0.001773,0.001479,0.001221,0.001451,0.001473,0.001661,0.002008,0.001414,0.001623,0.00152,0.001462,0.001695,0.00117,0.001637,0.001018,0.001244,0.001742,0.002119,0.001629,0.001866,0.001247,0.001792,0.000939,0.001538,0.001675,0.00161,0.001054,0.001044,0.001585,0.001742,0.001218,0.001443,0.001739,0.001389,0.001887,0.001172,0.001508,0.001508,0.001508,0.001508,0.001508,0.001311,0.001499,0.001227,0.00156,0.001616,0.001414,0.001416,0.001848,0.001212,0.000753,0.001883,0.001667,0.001727,0.000773,0.000739,0.001876,0.001088,0.001572,0.001739,0.001468,0.001821,0.001761,0.001513,0.001642,0.001799,0.001866,0.000863,0.000863,0.000863,0.000863,0.001295,0.001534,0.001631,0.001767,0.001745,0.000992,0.001802,0.001229,0.0016,0.001065,0.001066,0.00112,0.001522,0.001842,0.000814,0.001453,0.001672,0.001818,0.000911,0.0016,0.000965,0.000965,0.001684,0.001235,0.001233,0.000965,0.00102,0.001672,0.001701,0.001721,0.001414,0.001715,0.001799,0.001792,0.001838,0.001522,0.00177,0.00177,0.001764,0.001828,0.001399,0.001297,0.001399,0.00125,0.001608,0.001355,0.00161,0.00125,0.001634,0.001664,0.001488,0.001361,0.001621,0.001481,0.001543,0.001397,0.001582,0.001637,0.001297,0.001414,0.001605,0.001227,0.001014,0.001397,0.001147,0.001486,0.001527,0.001232,0.001244,0.001232,0.001244,0.001232,0.001433,0.000998,0.001355,0.001355,0.001244,0.000854,0.000932,0.001412,0.001353,0.001034,0.000935,0.000732,0.001263,0.000935,0.000935,0.001416,0.000955,0.000935,0.001464,0.001401,0.001412,0.00146,0.001094,0.000935,0.001176,0.001453,0.001634,0.001634,0.001055,0.001499,0.000939,0.001309,0.000986],[0.024541,0.024172,0.024836,0.023926,0.024197,0.024019,0.023761,0.024084,0.023737,0.023697,0.024219,0.024028,0.023452,0.023458,0.023904,0.02352,0.024028,0.024403,0.0233,0.0241,0.023991,0.023749,0.024019,0.022464,0.022132,0.023479,0.02332,0.023991,0.023898,0.024153,0.024187,0.023849,0.024181,0.022733,0.023917,0.023791,0.023975,0.023111,0.023068,0.024106,0.02415,0.023694,0.023806,0.024,0.023846,0.024009,0.022262,0.023631,0.023631,0.023631,0.023631,0.023631,0.02325,0.024062,0.022845,0.02392,0.024159,0.023288,0.023031,0.024326,0.024301,0.022326,0.023858,0.023846,0.023679,0.005348,0.022138,0.02367,0.022708,0.023813,0.024012,0.023997,0.02396,0.023932,0.02404,0.023947,0.023963,0.024206,0.022175,0.022175,0.022175,0.022175,0.023911,0.023728,0.023379,0.024019,0.023901,0.022254,0.023855,0.023831,0.023889,0.02223,0.022935,0.023515,0.024153,0.023758,0.022254,0.02367,0.024031,0.024175,0.022345,0.023649,0.023598,0.023598,0.023843,0.023843,0.023858,0.023598,0.023849,0.023883,0.023865,0.024043,0.023941,0.023972,0.023981,0.02388,0.023871,0.023877,0.023911,0.02415,0.023846,0.024087,0.023969,0.023898,0.023969,0.023904,0.023951,0.023966,0.023957,0.023743,0.023883,0.023957,0.023997,0.02396,0.024003,0.024003,0.024153,0.023957,0.024006,0.02384,0.023907,0.023911,0.023889,0.023843,0.023479,0.024062,0.023743,0.02415,0.023503,0.023728,0.023889,0.023728,0.023889,0.023728,0.023791,0.02235,0.024442,0.024529,0.024461,0.02275,0.023011,0.024043,0.024506,0.0242,0.022747,0.022633,0.023981,0.022747,0.022747,0.024187,0.023329,0.022747,0.024128,0.024096,0.024178,0.024222,0.023679,0.022747,0.023709,0.024128,0.024075,0.024075,0.023813,0.024075,0.023195,0.023794,0.024003],[0.231343,0.215527,0.19641,0.178332,0.150852,0.191358,0.150974,0.231056,0.161179,0.149038,0.162587,0.172063,0.173346,0.163301,0.170486,0.185075,0.192746,0.224909,0.152709,0.144747,0.175803,0.146226,0.189409,0.162872,0.201517,0.124248,0.161039,0.192347,0.257618,0.173023,0.219858,0.149879,0.209459,0.098884,0.12367,0.194357,0.158029,0.13943,0.140802,0.162587,0.178846,0.166816,0.166517,0.216531,0.149398,0.213793,0.149758,0.184707,0.184707,0.184707,0.184707,0.184707,0.146112,0.163015,0.155129,0.15952,0.191358,0.155779,0.143629,0.18111,0.005348,0.100977,0.213548,0.182174,0.215527,0.005376,0.005405,0.218566,0.144635,0.165333,0.165333,0.140909,0.207358,0.201299,0.157095,0.190965,0.209459,0.221957,0.116395,0.116395,0.116395,0.116395,0.139955,0.109799,0.176303,0.214286,0.195173,0.13829,0.210884,0.135569,0.191753,0.149758,0.137982,0.134588,0.175141,0.192946,0.09805,0.123506,0.16622,0.199785,0.128898,0.196203,0.005348,0.005348,0.192946,0.148562,0.13071,0.005348,0.005348,0.202835,0.197872,0.202835,0.160622,0.192347,0.209932,0.199357,0.225182,0.154613,0.209932,0.209932,0.202394,0.195584,0.141768,0.141768,0.141768,0.141985,0.164748,0.137778,0.164748,0.141985,0.188259,0.188259,0.16622,0.124083,0.164748,0.14166,0.148919,0.124083,0.164748,0.18845,0.124332,0.124083,0.180233,0.124083,0.124083,0.124083,0.137067,0.099839,0.134685,0.131356,0.131356,0.131356,0.131356,0.131356,0.108077,0.12141,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.402597,0.356322,0.405229,0.2976,0.306425,0.304419,0.269175,0.369781,0.281392,0.285276,0.32069,0.329787,0.275148,0.227106,0.269956,0.273932,0.30897,0.373494,0.263083,0.301948,0.282675,0.27193,0.315254,0.217544,0.304419,0.189409,0.231343,0.324042,0.394068,0.302932,0.347015,0.23192,0.333333,0.174648,0.286154,0.311558,0.299517,0.195996,0.194154,0.29477,0.324042,0.226553,0.268398,0.323478,0.258333,0.350943,0.218054,0.280543,0.280543,0.280543,0.280543,0.280543,0.243775,0.278861,0.228221,0.290172,0.300485,0.263083,0.263456,0.343808,0.225455,0.14006,0.350282,0.31,0.321244,0.14374,0.137472,0.348968,0.202394,0.292453,0.323478,0.273128,0.338798,0.327465,0.281392,0.305419,0.334532,0.347015,0.160483,0.160483,0.160483,0.160483,0.240933,0.285276,0.303426,0.328622,0.324607,0.184524,0.335135,0.228501,0.2976,0.198083,0.198294,0.208287,0.283105,0.342541,0.151466,0.270349,0.311037,0.338182,0.169399,0.2976,0.179537,0.179537,0.313131,0.22963,0.229346,0.179537,0.189796,0.311037,0.316327,0.320138,0.263083,0.319039,0.334532,0.333333,0.341912,0.283105,0.329204,0.329204,0.328042,0.340037,0.26014,0.241245,0.26014,0.2325,0.299035,0.252033,0.299517,0.2325,0.303922,0.309484,0.276786,0.253061,0.301459,0.275556,0.287037,0.259777,0.294304,0.304419,0.241245,0.263083,0.298555,0.228221,0.188641,0.259777,0.213303,0.276374,0.283969,0.229064,0.231343,0.229064,0.231343,0.229064,0.266476,0.185629,0.252033,0.252033,0.231343,0.158839,0.173346,0.262712,0.251691,0.192347,0.173832,0.136064,0.234848,0.173832,0.173832,0.263456,0.17765,0.173832,0.272328,0.260504,0.262712,0.271533,0.203501,0.173832,0.218824,0.270349,0.303922,0.303922,0.196203,0.278861,0.174648,0.243455,0.183432],[0.371257,0.343173,0.391579,0.335135,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.352273,0.314189,0.292913,0.286154,0.302932,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.307947,0.268398,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.298077,0.331551,0.319039,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.298077,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.303426,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.299517,0.284839,0.005348,0.005348,0.296178,0.296651,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var11_df_closseness, by=list(var11_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var11_df_closseness, by=list(var11_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-4a03b23e3a97cd4524b7" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-4a03b23e3a97cd4524b7">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000121\" data-max=\"0.000134\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001244\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.3e-05\" data-max=\"0.000454\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000985\" data-max=\"0.002179\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2e-06\" data-max=\"0.000377\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022493\" data-max=\"0.024836\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.1e-05\" data-max=\"0.004193\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.231343\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.002485\" data-max=\"0.08447\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.183432\" data-max=\"0.405229\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000391\" data-max=\"0.070212\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003883\" data-max=\"0.139693\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[0.000129,0.000125,0.00013,0.00013,0.000128,0.000131,0.000129,0.000134,0.000129,0.000128,0.00013,0.000121,0.000126,0.000129,0.000129,0.000126,0.000129,0.00013,0.000132,0.000126,0.000131,0.000129,0.000129],[2e-06,4e-06,null,null,null,null,0,null,null,null,null,2.2e-05,1e-06,1e-06,1e-06,4e-06,null,null,null,1e-06,null,1e-06,1e-06],[0.000937,0.000285,0.000778,0.001159,0.000812,0.000974,0.000882,0.001056,0.001018,0.000737,0.001193,0.000826,0.000888,0.000992,0.000959,0.0009,2.9e-05,0.001074,0.001244,0.000632,0.001209,0.000687,0.000925],[0.000202,0.000346,null,null,null,null,0.000191,null,null,null,null,0.00028,1.3e-05,0.000213,0.000128,0.000169,null,null,null,0.000454,null,0.000315,0.000169],[0.001687,0.00117,0.001623,0.001916,0.001447,0.001848,0.001553,0.002179,0.001695,0.001147,0.001866,0.0014,0.00122,0.001744,0.001648,0.001435,0.000986,0.001818,0.002165,0.001252,0.002008,0.001386,0.001575],[0.000141,0.000261,null,null,null,null,0.000175,null,null,null,null,0.000377,2e-06,0.00019,1.2e-05,0.000318,null,null,null,0.000263,null,0.000172,0.000188],[0.023945,0.023295,0.0241,0.024172,0.023761,0.024326,0.024015,0.024836,0.024019,0.023743,0.024206,0.022493,0.023576,0.024029,0.024081,0.023375,0.024003,0.024175,0.024541,0.023541,0.024403,0.023981,0.023945],[0.000383,0.000745,null,null,null,null,4.1e-05,null,null,null,null,0.004193,0.000167,0.000127,0.0001,0.000684,null,null,null,0.000224,null,0.00024,9.6e-05],[0.17433,0.052987,0.144747,0.215527,0.150974,0.18111,0.164078,0.19641,0.189409,0.137067,0.221957,0.153594,0.165058,0.184496,0.178319,0.167336,0.005348,0.199785,0.231343,0.11752,0.224909,0.127828,0.171974],[0.037591,0.06445,null,null,null,null,0.03561,null,null,null,null,0.052122,0.002485,0.03966,0.023797,0.031379,null,null,null,0.08447,null,0.058597,0.031482],[0.313781,0.217617,0.301948,0.356322,0.269175,0.343808,0.288831,0.405229,0.315254,0.213303,0.347015,0.260367,0.22683,0.324373,0.306605,0.266913,0.183432,0.338182,0.402597,0.232934,0.373494,0.257681,0.292998],[0.026233,0.048589,null,null,null,null,0.032572,null,null,null,null,0.070212,0.000391,0.035224,0.002281,0.05924,null,null,null,0.048894,null,0.031944,0.034904],[0.300585,0.099063,0.286154,0.343173,0.303922,0.323478,0.301224,0.391579,0.295238,0.29108,0.313659,0.266632,0.319611,0.301604,0.317292,0.295625,0.005348,0.306425,0.371257,0.194264,0.314189,0.234219,0.288777],[0.043014,0.125815,null,null,null,null,0.018184,null,null,null,null,0.070744,0.003883,0.033553,0.03051,0.023781,null,null,null,0.139693,null,0.102732,0.011704]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var11_df_closseness <- data.frame(
var11_incloseness,
var11_outcloseness,
var11_totalcloseness,
var11_incloseness_n,
var11_outcloseness_n,
var11_totalcloseness_n,
var11_centr_closeness) %>% round(6)

#Adding type
var11_df_closseness <-cbind(var11_df_closseness, V(var11)$TIPO1)

#Adding names
names(var11_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var11_df_closseness<-var11_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var11_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-8296c6159510ac6ea8d8" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-8296c6159510ac6ea8d8">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000134\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001385\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000732\" data-max=\"0.002179\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024836\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.257618\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.136064\" data-max=\"0.405229\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[0.000132,0.00013,0.000134,0.000129,0.00013,0.000129,0.000128,0.000129,0.000128,0.000127,0.00013,0.000129,0.000126,0.000126,0.000129,0.000126,0.000129,0.000131,0.000125,0.00013,0.000129,0.000128,0.000129,0.000121,0.000119,0.000126,0.000125,0.000129,0.000128,0.00013,0.00013,0.000128,0.00013,0.000122,0.000129,0.000128,0.000129,0.000124,0.000124,0.00013,0.00013,0.000127,0.000128,0.000129,0.000128,0.000129,0.00012,0.000127,0.000127,0.000127,0.000127,0.000127,0.000125,0.000129,0.000123,0.000129,0.00013,0.000125,0.000124,0.000131,0.000131,0.00012,0.000128,0.000128,0.000127,2.9e-05,0.000119,0.000127,0.000122,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.00013,0.000119,0.000119,0.000119,0.000119,0.000129,0.000128,0.000126,0.000129,0.000129,0.00012,0.000128,0.000128,0.000128,0.00012,0.000123,0.000126,0.00013,0.000128,0.00012,0.000127,0.000129,0.00013,0.00012,0.000127,0.000127,0.000127,0.000128,0.000128,0.000128,0.000127,0.000128,0.000128,0.000128,0.000129,0.000129,0.000129,0.000129,0.000128,0.000128,0.000128,0.000129,0.00013,0.000128,0.00013,0.000129,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.000128,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.00013,0.000129,0.000129,0.000128,0.000129,0.000129,0.000128,0.000128,0.000126,0.000129,0.000128,0.00013,0.000126,0.000128,0.000128,0.000128,0.000128,0.000128,0.000128,0.00012,0.000131,0.000132,0.000132,0.000122,0.000124,0.000129,0.000132,0.00013,0.000122,0.000122,0.000129,0.000122,0.000122,0.00013,0.000125,0.000122,0.00013,0.00013,0.00013,0.00013,0.000127,0.000122,0.000127,0.00013,0.000129,0.000129,0.000128,0.000129,0.000125,0.000128,0.000129],[0.001244,0.001159,0.001056,0.000959,0.000811,0.001029,0.000812,0.001242,0.000867,0.000801,0.000874,0.000925,0.000932,0.000878,0.000917,0.000995,0.001036,0.001209,0.000821,0.000778,0.000945,0.000786,0.001018,0.000876,0.001083,0.000668,0.000866,0.001034,0.001385,0.00093,0.001182,0.000806,0.001126,0.000532,0.000665,0.001045,0.00085,0.00075,0.000757,0.000874,0.000962,0.000897,0.000895,0.001164,0.000803,0.001149,0.000805,0.000993,0.000993,0.000993,0.000993,0.000993,0.000786,0.000876,0.000834,0.000858,0.001029,0.000838,0.000772,0.000974,2.9e-05,0.000543,0.001148,0.000979,0.001159,2.9e-05,2.9e-05,0.001175,0.000778,0.000889,0.000889,0.000758,0.001115,0.001082,0.000845,0.001027,0.001126,0.001193,0.000626,0.000626,0.000626,0.000626,0.000752,0.00059,0.000948,0.001152,0.001049,0.000743,0.001134,0.000729,0.001031,0.000805,0.000742,0.000724,0.000942,0.001037,0.000527,0.000664,0.000894,0.001074,0.000693,0.001055,2.9e-05,2.9e-05,0.001037,0.000799,0.000703,2.9e-05,2.9e-05,0.001091,0.001064,0.001091,0.000864,0.001034,0.001129,0.001072,0.001211,0.000831,0.001129,0.001129,0.001088,0.001052,0.000762,0.000762,0.000762,0.000763,0.000886,0.000741,0.000886,0.000763,0.001012,0.001012,0.000894,0.000667,0.000886,0.000762,0.000801,0.000667,0.000886,0.001013,0.000668,0.000667,0.000969,0.000667,0.000667,0.000667,0.000737,0.000537,0.000724,0.000706,0.000706,0.000706,0.000706,0.000706,0.000581,0.000653,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.002165,0.001916,0.002179,0.0016,0.001647,0.001637,0.001447,0.001988,0.001513,0.001534,0.001724,0.001773,0.001479,0.001221,0.001451,0.001473,0.001661,0.002008,0.001414,0.001623,0.00152,0.001462,0.001695,0.00117,0.001637,0.001018,0.001244,0.001742,0.002119,0.001629,0.001866,0.001247,0.001792,0.000939,0.001538,0.001675,0.00161,0.001054,0.001044,0.001585,0.001742,0.001218,0.001443,0.001739,0.001389,0.001887,0.001172,0.001508,0.001508,0.001508,0.001508,0.001508,0.001311,0.001499,0.001227,0.00156,0.001616,0.001414,0.001416,0.001848,0.001212,0.000753,0.001883,0.001667,0.001727,0.000773,0.000739,0.001876,0.001088,0.001572,0.001739,0.001468,0.001821,0.001761,0.001513,0.001642,0.001799,0.001866,0.000863,0.000863,0.000863,0.000863,0.001295,0.001534,0.001631,0.001767,0.001745,0.000992,0.001802,0.001229,0.0016,0.001065,0.001066,0.00112,0.001522,0.001842,0.000814,0.001453,0.001672,0.001818,0.000911,0.0016,0.000965,0.000965,0.001684,0.001235,0.001233,0.000965,0.00102,0.001672,0.001701,0.001721,0.001414,0.001715,0.001799,0.001792,0.001838,0.001522,0.00177,0.00177,0.001764,0.001828,0.001399,0.001297,0.001399,0.00125,0.001608,0.001355,0.00161,0.00125,0.001634,0.001664,0.001488,0.001361,0.001621,0.001481,0.001543,0.001397,0.001582,0.001637,0.001297,0.001414,0.001605,0.001227,0.001014,0.001397,0.001147,0.001486,0.001527,0.001232,0.001244,0.001232,0.001244,0.001232,0.001433,0.000998,0.001355,0.001355,0.001244,0.000854,0.000932,0.001412,0.001353,0.001034,0.000935,0.000732,0.001263,0.000935,0.000935,0.001416,0.000955,0.000935,0.001464,0.001401,0.001412,0.00146,0.001094,0.000935,0.001176,0.001453,0.001634,0.001634,0.001055,0.001499,0.000939,0.001309,0.000986],[0.024541,0.024172,0.024836,0.023926,0.024197,0.024019,0.023761,0.024084,0.023737,0.023697,0.024219,0.024028,0.023452,0.023458,0.023904,0.02352,0.024028,0.024403,0.0233,0.0241,0.023991,0.023749,0.024019,0.022464,0.022132,0.023479,0.02332,0.023991,0.023898,0.024153,0.024187,0.023849,0.024181,0.022733,0.023917,0.023791,0.023975,0.023111,0.023068,0.024106,0.02415,0.023694,0.023806,0.024,0.023846,0.024009,0.022262,0.023631,0.023631,0.023631,0.023631,0.023631,0.02325,0.024062,0.022845,0.02392,0.024159,0.023288,0.023031,0.024326,0.024301,0.022326,0.023858,0.023846,0.023679,0.005348,0.022138,0.02367,0.022708,0.023813,0.024012,0.023997,0.02396,0.023932,0.02404,0.023947,0.023963,0.024206,0.022175,0.022175,0.022175,0.022175,0.023911,0.023728,0.023379,0.024019,0.023901,0.022254,0.023855,0.023831,0.023889,0.02223,0.022935,0.023515,0.024153,0.023758,0.022254,0.02367,0.024031,0.024175,0.022345,0.023649,0.023598,0.023598,0.023843,0.023843,0.023858,0.023598,0.023849,0.023883,0.023865,0.024043,0.023941,0.023972,0.023981,0.02388,0.023871,0.023877,0.023911,0.02415,0.023846,0.024087,0.023969,0.023898,0.023969,0.023904,0.023951,0.023966,0.023957,0.023743,0.023883,0.023957,0.023997,0.02396,0.024003,0.024003,0.024153,0.023957,0.024006,0.02384,0.023907,0.023911,0.023889,0.023843,0.023479,0.024062,0.023743,0.02415,0.023503,0.023728,0.023889,0.023728,0.023889,0.023728,0.023791,0.02235,0.024442,0.024529,0.024461,0.02275,0.023011,0.024043,0.024506,0.0242,0.022747,0.022633,0.023981,0.022747,0.022747,0.024187,0.023329,0.022747,0.024128,0.024096,0.024178,0.024222,0.023679,0.022747,0.023709,0.024128,0.024075,0.024075,0.023813,0.024075,0.023195,0.023794,0.024003],[0.231343,0.215527,0.19641,0.178332,0.150852,0.191358,0.150974,0.231056,0.161179,0.149038,0.162587,0.172063,0.173346,0.163301,0.170486,0.185075,0.192746,0.224909,0.152709,0.144747,0.175803,0.146226,0.189409,0.162872,0.201517,0.124248,0.161039,0.192347,0.257618,0.173023,0.219858,0.149879,0.209459,0.098884,0.12367,0.194357,0.158029,0.13943,0.140802,0.162587,0.178846,0.166816,0.166517,0.216531,0.149398,0.213793,0.149758,0.184707,0.184707,0.184707,0.184707,0.184707,0.146112,0.163015,0.155129,0.15952,0.191358,0.155779,0.143629,0.18111,0.005348,0.100977,0.213548,0.182174,0.215527,0.005376,0.005405,0.218566,0.144635,0.165333,0.165333,0.140909,0.207358,0.201299,0.157095,0.190965,0.209459,0.221957,0.116395,0.116395,0.116395,0.116395,0.139955,0.109799,0.176303,0.214286,0.195173,0.13829,0.210884,0.135569,0.191753,0.149758,0.137982,0.134588,0.175141,0.192946,0.09805,0.123506,0.16622,0.199785,0.128898,0.196203,0.005348,0.005348,0.192946,0.148562,0.13071,0.005348,0.005348,0.202835,0.197872,0.202835,0.160622,0.192347,0.209932,0.199357,0.225182,0.154613,0.209932,0.209932,0.202394,0.195584,0.141768,0.141768,0.141768,0.141985,0.164748,0.137778,0.164748,0.141985,0.188259,0.188259,0.16622,0.124083,0.164748,0.14166,0.148919,0.124083,0.164748,0.18845,0.124332,0.124083,0.180233,0.124083,0.124083,0.124083,0.137067,0.099839,0.134685,0.131356,0.131356,0.131356,0.131356,0.131356,0.108077,0.12141,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.402597,0.356322,0.405229,0.2976,0.306425,0.304419,0.269175,0.369781,0.281392,0.285276,0.32069,0.329787,0.275148,0.227106,0.269956,0.273932,0.30897,0.373494,0.263083,0.301948,0.282675,0.27193,0.315254,0.217544,0.304419,0.189409,0.231343,0.324042,0.394068,0.302932,0.347015,0.23192,0.333333,0.174648,0.286154,0.311558,0.299517,0.195996,0.194154,0.29477,0.324042,0.226553,0.268398,0.323478,0.258333,0.350943,0.218054,0.280543,0.280543,0.280543,0.280543,0.280543,0.243775,0.278861,0.228221,0.290172,0.300485,0.263083,0.263456,0.343808,0.225455,0.14006,0.350282,0.31,0.321244,0.14374,0.137472,0.348968,0.202394,0.292453,0.323478,0.273128,0.338798,0.327465,0.281392,0.305419,0.334532,0.347015,0.160483,0.160483,0.160483,0.160483,0.240933,0.285276,0.303426,0.328622,0.324607,0.184524,0.335135,0.228501,0.2976,0.198083,0.198294,0.208287,0.283105,0.342541,0.151466,0.270349,0.311037,0.338182,0.169399,0.2976,0.179537,0.179537,0.313131,0.22963,0.229346,0.179537,0.189796,0.311037,0.316327,0.320138,0.263083,0.319039,0.334532,0.333333,0.341912,0.283105,0.329204,0.329204,0.328042,0.340037,0.26014,0.241245,0.26014,0.2325,0.299035,0.252033,0.299517,0.2325,0.303922,0.309484,0.276786,0.253061,0.301459,0.275556,0.287037,0.259777,0.294304,0.304419,0.241245,0.263083,0.298555,0.228221,0.188641,0.259777,0.213303,0.276374,0.283969,0.229064,0.231343,0.229064,0.231343,0.229064,0.266476,0.185629,0.252033,0.252033,0.231343,0.158839,0.173346,0.262712,0.251691,0.192347,0.173832,0.136064,0.234848,0.173832,0.173832,0.263456,0.17765,0.173832,0.272328,0.260504,0.262712,0.271533,0.203501,0.173832,0.218824,0.270349,0.303922,0.303922,0.196203,0.278861,0.174648,0.243455,0.183432],[0.371257,0.343173,0.391579,0.335135,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.352273,0.314189,0.292913,0.286154,0.302932,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.307947,0.268398,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.298077,0.331551,0.319039,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.298077,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.303426,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.299517,0.284839,0.005348,0.005348,0.296178,0.296651,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var11_df_closseness, by=list(var11_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var11_df_closseness, by=list(var11_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-c0150f8195dfcc062ad6" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-c0150f8195dfcc062ad6">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000124\" data-max=\"0.000129\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000522\" data-max=\"0.00086\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000286\" data-max=\"0.000414\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001272\" data-max=\"0.001539\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000258\" data-max=\"0.000322\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.023129\" data-max=\"0.023947\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00024\" data-max=\"0.002139\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.097022\" data-max=\"0.159859\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.053282\" data-max=\"0.077062\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.236551\" data-max=\"0.286216\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.048029\" data-max=\"0.059879\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.173795\" data-max=\"0.27263\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.07915\" data-max=\"0.135451\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[0.000129,0.000124],[1e-06,1.1e-05],[0.00086,0.000522],[0.000286,0.000414],[0.001539,0.001272],[0.000258,0.000322],[0.023947,0.023129],[0.00024,0.002139],[0.159859,0.097022],[0.053282,0.077062],[0.286216,0.236551],[0.048029,0.059879],[0.27263,0.173795],[0.07915,0.135451]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var11_df_closseness <- data.frame(
var11_incloseness,
var11_outcloseness,
var11_totalcloseness,
var11_incloseness_n,
var11_outcloseness_n,
var11_totalcloseness_n,
var11_centr_closeness) %>% round(6)

#Adding type
var11_df_closseness <-cbind(var11_df_closseness, V(var11)$TIPO2)

#Adding names
names(var11_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var11_df_closseness<-var11_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var11_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-b474c2a9d8fe3b7baf64" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-b474c2a9d8fe3b7baf64">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000134\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001385\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000732\" data-max=\"0.002179\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024836\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.257618\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.136064\" data-max=\"0.405229\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[0.000132,0.00013,0.000134,0.000129,0.00013,0.000129,0.000128,0.000129,0.000128,0.000127,0.00013,0.000129,0.000126,0.000126,0.000129,0.000126,0.000129,0.000131,0.000125,0.00013,0.000129,0.000128,0.000129,0.000121,0.000119,0.000126,0.000125,0.000129,0.000128,0.00013,0.00013,0.000128,0.00013,0.000122,0.000129,0.000128,0.000129,0.000124,0.000124,0.00013,0.00013,0.000127,0.000128,0.000129,0.000128,0.000129,0.00012,0.000127,0.000127,0.000127,0.000127,0.000127,0.000125,0.000129,0.000123,0.000129,0.00013,0.000125,0.000124,0.000131,0.000131,0.00012,0.000128,0.000128,0.000127,2.9e-05,0.000119,0.000127,0.000122,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.00013,0.000119,0.000119,0.000119,0.000119,0.000129,0.000128,0.000126,0.000129,0.000129,0.00012,0.000128,0.000128,0.000128,0.00012,0.000123,0.000126,0.00013,0.000128,0.00012,0.000127,0.000129,0.00013,0.00012,0.000127,0.000127,0.000127,0.000128,0.000128,0.000128,0.000127,0.000128,0.000128,0.000128,0.000129,0.000129,0.000129,0.000129,0.000128,0.000128,0.000128,0.000129,0.00013,0.000128,0.00013,0.000129,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.000128,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.00013,0.000129,0.000129,0.000128,0.000129,0.000129,0.000128,0.000128,0.000126,0.000129,0.000128,0.00013,0.000126,0.000128,0.000128,0.000128,0.000128,0.000128,0.000128,0.00012,0.000131,0.000132,0.000132,0.000122,0.000124,0.000129,0.000132,0.00013,0.000122,0.000122,0.000129,0.000122,0.000122,0.00013,0.000125,0.000122,0.00013,0.00013,0.00013,0.00013,0.000127,0.000122,0.000127,0.00013,0.000129,0.000129,0.000128,0.000129,0.000125,0.000128,0.000129],[0.001244,0.001159,0.001056,0.000959,0.000811,0.001029,0.000812,0.001242,0.000867,0.000801,0.000874,0.000925,0.000932,0.000878,0.000917,0.000995,0.001036,0.001209,0.000821,0.000778,0.000945,0.000786,0.001018,0.000876,0.001083,0.000668,0.000866,0.001034,0.001385,0.00093,0.001182,0.000806,0.001126,0.000532,0.000665,0.001045,0.00085,0.00075,0.000757,0.000874,0.000962,0.000897,0.000895,0.001164,0.000803,0.001149,0.000805,0.000993,0.000993,0.000993,0.000993,0.000993,0.000786,0.000876,0.000834,0.000858,0.001029,0.000838,0.000772,0.000974,2.9e-05,0.000543,0.001148,0.000979,0.001159,2.9e-05,2.9e-05,0.001175,0.000778,0.000889,0.000889,0.000758,0.001115,0.001082,0.000845,0.001027,0.001126,0.001193,0.000626,0.000626,0.000626,0.000626,0.000752,0.00059,0.000948,0.001152,0.001049,0.000743,0.001134,0.000729,0.001031,0.000805,0.000742,0.000724,0.000942,0.001037,0.000527,0.000664,0.000894,0.001074,0.000693,0.001055,2.9e-05,2.9e-05,0.001037,0.000799,0.000703,2.9e-05,2.9e-05,0.001091,0.001064,0.001091,0.000864,0.001034,0.001129,0.001072,0.001211,0.000831,0.001129,0.001129,0.001088,0.001052,0.000762,0.000762,0.000762,0.000763,0.000886,0.000741,0.000886,0.000763,0.001012,0.001012,0.000894,0.000667,0.000886,0.000762,0.000801,0.000667,0.000886,0.001013,0.000668,0.000667,0.000969,0.000667,0.000667,0.000667,0.000737,0.000537,0.000724,0.000706,0.000706,0.000706,0.000706,0.000706,0.000581,0.000653,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.002165,0.001916,0.002179,0.0016,0.001647,0.001637,0.001447,0.001988,0.001513,0.001534,0.001724,0.001773,0.001479,0.001221,0.001451,0.001473,0.001661,0.002008,0.001414,0.001623,0.00152,0.001462,0.001695,0.00117,0.001637,0.001018,0.001244,0.001742,0.002119,0.001629,0.001866,0.001247,0.001792,0.000939,0.001538,0.001675,0.00161,0.001054,0.001044,0.001585,0.001742,0.001218,0.001443,0.001739,0.001389,0.001887,0.001172,0.001508,0.001508,0.001508,0.001508,0.001508,0.001311,0.001499,0.001227,0.00156,0.001616,0.001414,0.001416,0.001848,0.001212,0.000753,0.001883,0.001667,0.001727,0.000773,0.000739,0.001876,0.001088,0.001572,0.001739,0.001468,0.001821,0.001761,0.001513,0.001642,0.001799,0.001866,0.000863,0.000863,0.000863,0.000863,0.001295,0.001534,0.001631,0.001767,0.001745,0.000992,0.001802,0.001229,0.0016,0.001065,0.001066,0.00112,0.001522,0.001842,0.000814,0.001453,0.001672,0.001818,0.000911,0.0016,0.000965,0.000965,0.001684,0.001235,0.001233,0.000965,0.00102,0.001672,0.001701,0.001721,0.001414,0.001715,0.001799,0.001792,0.001838,0.001522,0.00177,0.00177,0.001764,0.001828,0.001399,0.001297,0.001399,0.00125,0.001608,0.001355,0.00161,0.00125,0.001634,0.001664,0.001488,0.001361,0.001621,0.001481,0.001543,0.001397,0.001582,0.001637,0.001297,0.001414,0.001605,0.001227,0.001014,0.001397,0.001147,0.001486,0.001527,0.001232,0.001244,0.001232,0.001244,0.001232,0.001433,0.000998,0.001355,0.001355,0.001244,0.000854,0.000932,0.001412,0.001353,0.001034,0.000935,0.000732,0.001263,0.000935,0.000935,0.001416,0.000955,0.000935,0.001464,0.001401,0.001412,0.00146,0.001094,0.000935,0.001176,0.001453,0.001634,0.001634,0.001055,0.001499,0.000939,0.001309,0.000986],[0.024541,0.024172,0.024836,0.023926,0.024197,0.024019,0.023761,0.024084,0.023737,0.023697,0.024219,0.024028,0.023452,0.023458,0.023904,0.02352,0.024028,0.024403,0.0233,0.0241,0.023991,0.023749,0.024019,0.022464,0.022132,0.023479,0.02332,0.023991,0.023898,0.024153,0.024187,0.023849,0.024181,0.022733,0.023917,0.023791,0.023975,0.023111,0.023068,0.024106,0.02415,0.023694,0.023806,0.024,0.023846,0.024009,0.022262,0.023631,0.023631,0.023631,0.023631,0.023631,0.02325,0.024062,0.022845,0.02392,0.024159,0.023288,0.023031,0.024326,0.024301,0.022326,0.023858,0.023846,0.023679,0.005348,0.022138,0.02367,0.022708,0.023813,0.024012,0.023997,0.02396,0.023932,0.02404,0.023947,0.023963,0.024206,0.022175,0.022175,0.022175,0.022175,0.023911,0.023728,0.023379,0.024019,0.023901,0.022254,0.023855,0.023831,0.023889,0.02223,0.022935,0.023515,0.024153,0.023758,0.022254,0.02367,0.024031,0.024175,0.022345,0.023649,0.023598,0.023598,0.023843,0.023843,0.023858,0.023598,0.023849,0.023883,0.023865,0.024043,0.023941,0.023972,0.023981,0.02388,0.023871,0.023877,0.023911,0.02415,0.023846,0.024087,0.023969,0.023898,0.023969,0.023904,0.023951,0.023966,0.023957,0.023743,0.023883,0.023957,0.023997,0.02396,0.024003,0.024003,0.024153,0.023957,0.024006,0.02384,0.023907,0.023911,0.023889,0.023843,0.023479,0.024062,0.023743,0.02415,0.023503,0.023728,0.023889,0.023728,0.023889,0.023728,0.023791,0.02235,0.024442,0.024529,0.024461,0.02275,0.023011,0.024043,0.024506,0.0242,0.022747,0.022633,0.023981,0.022747,0.022747,0.024187,0.023329,0.022747,0.024128,0.024096,0.024178,0.024222,0.023679,0.022747,0.023709,0.024128,0.024075,0.024075,0.023813,0.024075,0.023195,0.023794,0.024003],[0.231343,0.215527,0.19641,0.178332,0.150852,0.191358,0.150974,0.231056,0.161179,0.149038,0.162587,0.172063,0.173346,0.163301,0.170486,0.185075,0.192746,0.224909,0.152709,0.144747,0.175803,0.146226,0.189409,0.162872,0.201517,0.124248,0.161039,0.192347,0.257618,0.173023,0.219858,0.149879,0.209459,0.098884,0.12367,0.194357,0.158029,0.13943,0.140802,0.162587,0.178846,0.166816,0.166517,0.216531,0.149398,0.213793,0.149758,0.184707,0.184707,0.184707,0.184707,0.184707,0.146112,0.163015,0.155129,0.15952,0.191358,0.155779,0.143629,0.18111,0.005348,0.100977,0.213548,0.182174,0.215527,0.005376,0.005405,0.218566,0.144635,0.165333,0.165333,0.140909,0.207358,0.201299,0.157095,0.190965,0.209459,0.221957,0.116395,0.116395,0.116395,0.116395,0.139955,0.109799,0.176303,0.214286,0.195173,0.13829,0.210884,0.135569,0.191753,0.149758,0.137982,0.134588,0.175141,0.192946,0.09805,0.123506,0.16622,0.199785,0.128898,0.196203,0.005348,0.005348,0.192946,0.148562,0.13071,0.005348,0.005348,0.202835,0.197872,0.202835,0.160622,0.192347,0.209932,0.199357,0.225182,0.154613,0.209932,0.209932,0.202394,0.195584,0.141768,0.141768,0.141768,0.141985,0.164748,0.137778,0.164748,0.141985,0.188259,0.188259,0.16622,0.124083,0.164748,0.14166,0.148919,0.124083,0.164748,0.18845,0.124332,0.124083,0.180233,0.124083,0.124083,0.124083,0.137067,0.099839,0.134685,0.131356,0.131356,0.131356,0.131356,0.131356,0.108077,0.12141,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.402597,0.356322,0.405229,0.2976,0.306425,0.304419,0.269175,0.369781,0.281392,0.285276,0.32069,0.329787,0.275148,0.227106,0.269956,0.273932,0.30897,0.373494,0.263083,0.301948,0.282675,0.27193,0.315254,0.217544,0.304419,0.189409,0.231343,0.324042,0.394068,0.302932,0.347015,0.23192,0.333333,0.174648,0.286154,0.311558,0.299517,0.195996,0.194154,0.29477,0.324042,0.226553,0.268398,0.323478,0.258333,0.350943,0.218054,0.280543,0.280543,0.280543,0.280543,0.280543,0.243775,0.278861,0.228221,0.290172,0.300485,0.263083,0.263456,0.343808,0.225455,0.14006,0.350282,0.31,0.321244,0.14374,0.137472,0.348968,0.202394,0.292453,0.323478,0.273128,0.338798,0.327465,0.281392,0.305419,0.334532,0.347015,0.160483,0.160483,0.160483,0.160483,0.240933,0.285276,0.303426,0.328622,0.324607,0.184524,0.335135,0.228501,0.2976,0.198083,0.198294,0.208287,0.283105,0.342541,0.151466,0.270349,0.311037,0.338182,0.169399,0.2976,0.179537,0.179537,0.313131,0.22963,0.229346,0.179537,0.189796,0.311037,0.316327,0.320138,0.263083,0.319039,0.334532,0.333333,0.341912,0.283105,0.329204,0.329204,0.328042,0.340037,0.26014,0.241245,0.26014,0.2325,0.299035,0.252033,0.299517,0.2325,0.303922,0.309484,0.276786,0.253061,0.301459,0.275556,0.287037,0.259777,0.294304,0.304419,0.241245,0.263083,0.298555,0.228221,0.188641,0.259777,0.213303,0.276374,0.283969,0.229064,0.231343,0.229064,0.231343,0.229064,0.266476,0.185629,0.252033,0.252033,0.231343,0.158839,0.173346,0.262712,0.251691,0.192347,0.173832,0.136064,0.234848,0.173832,0.173832,0.263456,0.17765,0.173832,0.272328,0.260504,0.262712,0.271533,0.203501,0.173832,0.218824,0.270349,0.303922,0.303922,0.196203,0.278861,0.174648,0.243455,0.183432],[0.371257,0.343173,0.391579,0.335135,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.352273,0.314189,0.292913,0.286154,0.302932,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.307947,0.268398,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.298077,0.331551,0.319039,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.298077,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.303426,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.299517,0.284839,0.005348,0.005348,0.296178,0.296651,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var11_df_closseness, by=list(var11_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var11_df_closseness, by=list(var11_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-415ca5197b172e8d924b" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-415ca5197b172e8d924b">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000124\" data-max=\"0.000129\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000517\" data-max=\"0.001006\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00018\" data-max=\"0.000414\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001269\" data-max=\"0.001736\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000154\" data-max=\"0.000318\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.023116\" data-max=\"0.024069\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00012\" data-max=\"0.00215\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.096082\" data-max=\"0.187045\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.033585\" data-max=\"0.076938\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.235955\" data-max=\"0.322904\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.028591\" data-max=\"0.059192\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172425\" data-max=\"0.308646\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.029585\" data-max=\"0.135802\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[0.000129,0.000129,0.000124],[1e-06,1e-06,1.2e-05],[0.001006,0.000837,0.000517],[0.00018,0.000294,0.000414],[0.001736,0.001507,0.001269],[0.000154,0.000261,0.000318],[0.024069,0.023929,0.023116],[0.00012,0.000249,0.00215],[0.187045,0.155632,0.096082],[0.033585,0.054603,0.076938],[0.322904,0.280328,0.235955],[0.028591,0.048597,0.059192],[0.308646,0.266968,0.172425],[0.029585,0.082639,0.135802]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var11_data.RData")
```

