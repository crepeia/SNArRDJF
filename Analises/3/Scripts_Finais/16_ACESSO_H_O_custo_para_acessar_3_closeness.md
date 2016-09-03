# SNA Closeness 16_ACESSO H) O custo para acessar este serviço impede a realização de ativades em conjunto. (var14)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 16_ACESSO H) O custo para acessar este serviço impede a realização de ativades em conjunto. (var14)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var14_data.RData")
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
#var14<-simplify(var14) #Simplify
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
V(var14)$incloseness <- closeness(var14, mode = "in", weights = E(var14)$var14) %>% round(6)
V(var14)$outcloseness <- closeness(var14, mode = "out", weights = E(var14)$var14) %>% round(6)
V(var14)$totalcloseness <- closeness(var14, mode = "total", weights = E(var14)$var14) %>% round(4)
```

###Saving to Environment

```r
var14_incloseness<- closeness(var14, mode = "in", weights = E(var14)$var14) %>% round(6)
var14_outcloseness<- closeness(var14, mode = "out", weights = E(var14)$var14) %>% round(6)
var14_totalcloseness<- closeness(var14, mode = "total", weights = E(var14)$var14) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var14_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0001310 0.0001320 0.0001312 0.0001330 0.0001370
```

```r
sd(var14_incloseness)
```

```
## [1] 7.785366e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var14)$incloseness<-closeness(var14, weights = E(var14)$var14, mode="in")

#Get Variable
V(var14)$var14_color_degree<-round(V(var14)$incloseness,6)

#Creating brewer pallette
vertex_var14_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var14)$var14_color_degree)), "RdBu"))(
            length(unique(V(var14)$var14_color_degree)))

#Saving as Vertex properties 
V(var14)$vertex_var14_color_degree<-
  vertex_var14_color_degree[as.numeric(
  cut(V(var14)$var14_color_degree,
      breaks=length(unique(V(var14)$var14_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var14, es=E(var14), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var14))
maxC <- rep(Inf, vcount(var14))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var14, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var14)$weight)


#PLotting
plot(var14, 
     layout=co,
     edge.color=V(var14)$vertex_var14_color_degree[edge.start],
     edge.arrow.size=closeness(var14, weights = E(var14)$var14, mode="in"),
     edge.width=E(var14)$weight/mean(E(var14)$weight),
     edge.curved = TRUE,
     vertex.color=V(var14)$vertex_var14_color_degree,
     vertex.size=closeness(var14, weights = E(var14)$var14, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var14,"LABEL_COR"),
     vertex.label.cex=(closeness(var14, weights = E(var14)$var14, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var14)$var14_color_degree
b<-V(var14)$vertex_var14_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 16_ACESSO H) O custo para acessar este serviço impede a realização de ativades em conjunto. (var14)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var14, mode="in", weights = E(var14)$var14)), 
             sd(closeness(var14, mode="in", weights = E(var14)$var14))
             )
       )
```

![](16_ACESSO_H_O_custo_para_acessar_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var14_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0008960 0.0011270 0.0009891 0.0013800 0.0017830
```

```r
sd(var14_outcloseness)
```

```
## [1] 0.000536634
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var14)$outcloseness<-closeness(var14, weights = E(var14)$var14, mode="out")

#Get Variable
V(var14)$var14_color_degree<-round(V(var14)$outcloseness,6)

#Creating brewer pallette
vertex_var14_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var14)$var14_color_degree)), "RdBu"))(
            length(unique(V(var14)$var14_color_degree)))

#Saving as Vertex properties 
V(var14)$vertex_var14_color_degree<-
  vertex_var14_color_degree[as.numeric(
  cut(V(var14)$var14_color_degree,
      breaks=length(unique(V(var14)$var14_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var14, es=E(var14), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var14))
maxC <- rep(Inf, vcount(var14))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var14, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var14)$weight)


#PLotting
plot(var14, 
     layout=co,
     edge.color=V(var14)$vertex_var14_color_degree[edge.start],
     edge.arrow.size=closeness(var14, weights = E(var14)$var14, mode="out"),
     edge.width=E(var14)$weight/2*mean(E(var14)$weight),
     edge.curved = TRUE,
     vertex.color=V(var14)$vertex_var14_color_degree,
     vertex.size=closeness(var14, weights = E(var14)$var14, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var14,"LABEL_COR"),
     vertex.label.cex=closeness(var14, weights = E(var14)$var14, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var14)$var14_color_degree
b<-V(var14)$vertex_var14_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 16_ACESSO H) O custo para acessar este serviço impede a realização de ativades em conjunto. (var14)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var14, mode="out", weights = E(var14)$var14)), 
             sd(closeness(var14, mode="out", weights = E(var14)$var14))
             )
       )
```

![](16_ACESSO_H_O_custo_para_acessar_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var14_totalcloseness)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000933 0.001938 0.002092 0.002130 0.002288 0.003311
```

```r
sd(var14_totalcloseness)
```

```
## [1] 0.000318169
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var14)$allcloseness<-closeness(var14, weights = E(var14)$var14, mode="all")

#Get Variable
V(var14)$var14_color_degree<-round(V(var14)$allcloseness,6)

#Creating brewer pallette
vertex_var14_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var14)$var14_color_degree)), "RdBu"))(
            length(unique(V(var14)$var14_color_degree)))

#Saving as Vertex properties 
V(var14)$vertex_var14_color_degree<-
  vertex_var14_color_degree[as.numeric(
  cut(V(var14)$var14_color_degree,
      breaks=length(unique(V(var14)$var14_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var14, es=E(var14), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var14))
maxC <- rep(Inf, vcount(var14))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var14, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var14)$weight)


#PLotting
plot(var14, 
     layout=co,
     edge.color=V(var14)$vertex_var14_color_degree[edge.start],
     edge.arrow.size=closeness(var14, weights = E(var14)$var14, mode="all"),
     edge.width=E(var14)$weight/2*mean(E(var14)$weight),
     edge.curved = TRUE,
     vertex.color=V(var14)$vertex_var14_color_degree,
     vertex.size=closeness(var14, weights = E(var14)$var14, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var14,"LABEL_COR"),
     vertex.label.cex=(closeness(var14, weights = E(var14)$var14, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var14)$var14_color_degree
b<-V(var14)$vertex_var14_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 16_ACESSO H) O custo para acessar este serviço impede a realização de ativades em conjunto. (var14)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var14, mode="all", weights = E(var14)$var14)), 
             sd(closeness(var14, mode="all", weights = E(var14)$var14))
             )
       )
```

![](16_ACESSO_H_O_custo_para_acessar_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var14)$incloseness_n <- closeness(var14, mode = "in",, weights = E(var14)$var14, normalized = T) %>% round(10)
V(var14)$outcloseness_n <- closeness(var14, mode = "out", normalized = T, weights = E(var14)$var14) %>% round(6)
V(var14)$totalcloseness_n <- closeness(var14, mode = "total", normalized = T, weights = E(var14)$var14) %>% round(6)
```

###Saving to Environment

```r
var14_incloseness_n<- closeness(var14, mode = "in", normalized = T, weights = E(var14)$var14) %>% round(6)
var14_outcloseness_n<- closeness(var14, mode = "out", normalized = T, weights = E(var14)$var14) %>% round(6)
var14_totalcloseness_n<- closeness(var14, mode = "total", normalized = T, weights = E(var14)$var14) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var14_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.024380 0.024480 0.024390 0.024730 0.025530
```

```r
sd(var14_incloseness_n)
```

```
## [1] 0.00145175
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var14)$incloseness_n<-closeness(var14, weights = E(var14)$var14, mode="in", normalized = T)

#Get Variable
V(var14)$var14_color_degree<-round(V(var14)$incloseness_n,6)

#Creating brewer pallette
vertex_var14_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var14)$var14_color_degree)), "RdBu"))(
            length(unique(V(var14)$var14_color_degree)))

#Saving as Vertex properties 
V(var14)$vertex_var14_color_degree<-
  vertex_var14_color_degree[as.numeric(
  cut(V(var14)$var14_color_degree,
      breaks=length(unique(V(var14)$var14_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var14, es=E(var14), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var14))
maxC <- rep(Inf, vcount(var14))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var14, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var14)$weight)


#PLotting
plot(var14, 
     layout=co,
     edge.color=V(var14)$vertex_var14_color_degree[edge.start],
     edge.arrow.size=closeness(var14, weights = E(var14)$var14, mode="in",normalized = T),
     edge.width=E(var14)$weight/10*mean(E(var14)$weight),
     edge.curved = TRUE,
     vertex.color=V(var14)$vertex_var14_color_degree,
     vertex.size=(closeness(var14, weights = E(var14)$var14, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var14,"LABEL_COR"),
     vertex.label.cex=closeness(var14, weights = E(var14)$var14, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var14)$var14_color_degree
b<-V(var14)$vertex_var14_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 16_ACESSO H) O custo para acessar este serviço impede a realização de ativades em conjunto. (var14)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var14, mode="in", weights = E(var14)$var14, normalized = T)), 
             sd(closeness(var14, mode="in", weights = E(var14)$var14, normalized = T))
             )
       )
```

![](16_ACESSO_H_O_custo_para_acessar_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var14_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.166700 0.209700 0.184000 0.256700 0.331600
```

```r
sd(var14_outcloseness_n)
```

```
## [1] 0.09983089
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var14)$outcloseness_n<-closeness(var14, weights = E(var14)$var14, mode="out", normalized = T)

#Get Variable
V(var14)$var14_color_degree<-round(V(var14)$outcloseness_n,6)

#Creating brewer pallette
vertex_var14_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var14)$var14_color_degree)), "RdBu"))(
            length(unique(V(var14)$var14_color_degree)))

#Saving as Vertex properties 
V(var14)$vertex_var14_color_degree<-
  vertex_var14_color_degree[as.numeric(
  cut(V(var14)$var14_color_degree,
      breaks=length(unique(V(var14)$var14_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var14, es=E(var14), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var14))
maxC <- rep(Inf, vcount(var14))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var14, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var14)$weight)


#PLotting
plot(var14, 
     layout=co,
     edge.color=V(var14)$vertex_var14_color_degree[edge.start],
     edge.arrow.size=closeness(var14, weights = E(var14)$var14, mode="out",normalized = T),
     edge.width=E(var14)$weight/10*mean(E(var14)$weight),
     edge.curved = TRUE,
     vertex.color=V(var14)$vertex_var14_color_degree,
     vertex.size=(closeness(var14, weights = E(var14)$var14, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var14,"LABEL_COR"),
     vertex.label.cex=closeness(var14, weights = E(var14)$var14, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var14)$var14_color_degree
b<-V(var14)$vertex_var14_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 16_ACESSO H) O custo para acessar este serviço impede a realização de ativades em conjunto. (var14)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var14, mode="out", weights = E(var14)$var14, normalized = T)), 
             sd(closeness(var14, mode="out", weights = E(var14)$var14, normalized = T))
             )
       )
```

![](16_ACESSO_H_O_custo_para_acessar_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var14_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1735  0.3605  0.3891  0.3963  0.4256  0.6159
```

```r
sd(var14_totalcloseness_n)
```

```
## [1] 0.05917484
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var14)$allcloseness_n<-closeness(var14, weights = E(var14)$var14, mode="all", normalized = T)

#Get Variable
V(var14)$var14_color_degree<-round(V(var14)$allcloseness_n,6)

#Creating brewer pallette
vertex_var14_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var14)$var14_color_degree)), "RdBu"))(
            length(unique(V(var14)$var14_color_degree)))

#Saving as Vertex properties 
V(var14)$vertex_var14_color_degree<-
  vertex_var14_color_degree[as.numeric(
  cut(V(var14)$var14_color_degree,
      breaks=length(unique(V(var14)$var14_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var14, es=E(var14), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var14))
maxC <- rep(Inf, vcount(var14))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var14, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var14)$weight)


#PLotting
plot(var14, 
     layout=co,
     edge.color=V(var14)$vertex_var14_color_degree[edge.start],
     edge.arrow.size=closeness(var14, weights = E(var14)$var14, mode="all",normalized = T),
     edge.width=E(var14)$weight/10*mean(E(var14)$weight),
     edge.curved = TRUE,
     vertex.color=V(var14)$vertex_var14_color_degree,
     vertex.size=(closeness(var14, weights = E(var14)$var14, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var14,"LABEL_COR"),
     vertex.label.cex=closeness(var14, weights = E(var14)$var14, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var14)$var14_color_degree
b<-V(var14)$vertex_var14_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 16_ACESSO H) O custo para acessar este serviço impede a realização de ativades em conjunto. (var14)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var14, mode="all", weights = E(var14)$var14, normalized = T)), 
             sd(closeness(var14, mode="all", weights = E(var14)$var14, normalized = T))
             )
       )
```

![](16_ACESSO_H_O_custo_para_acessar_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var14)$incloseness_n <- closeness(var14, weights = E(var14)$var14, mode = "in", normalized = T) %>% round(6)
V(var14)$outcloseness_n <- closeness(var14, weights = E(var14)$var14, mode = "out", normalized = T) %>% round(6)
V(var14)$totalcloseness_n <- closeness(var14, weights = E(var14)$var14, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var14)$var14_centr_closeness<- centralization.closeness(var14)$res
var14_centr_closeness<- centralization.closeness(var14)$res
var14_centr_closeness_all<- centralization.closeness(var14)
```

###Centralization

```r
var14_centr_closeness_all$centralization
```

```
## [1] 0.1625197
```

###Theoretical Max

```r
var14_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var14)$var14_centr_closeness<- centralization.closeness(var14)$res

#Get Variable
V(var14)$var14_color_degree<-round(V(var14)$var14_centr_closeness,6)

#Creating brewer pallette
vertex_var14_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var14)$var14_color_degree)), "Spectral"))(
            length(unique(V(var14)$var14_color_degree)))

#Saving as Vertex properties 
V(var14)$vertex_var14_color_degree<-
  vertex_var14_color_degree[as.numeric(
  cut(V(var14)$var14_color_degree,
      breaks=length(unique(V(var14)$var14_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var14, es=E(var14), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var14))
maxC <- rep(Inf, vcount(var14))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var14, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var14)$weight)


#PLotting
plot(var14, 
     layout=co,
     edge.color=V(var14)$vertex_var14_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var14)$res,
     edge.width=E(var14)$weight/10*mean(E(var14)$weight),
     edge.curved = TRUE,
     vertex.color=V(var14)$vertex_var14_color_degree,
     vertex.size=centralization.closeness(var14)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var14,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var14)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var14)$var14_color_degree
b<-V(var14)$vertex_var14_color_degree
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
  title("Network Centralization Closeness - 16_ACESSO H) O custo para acessar este serviço impede a realização de ativades em conjunto. (var14)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var14)$res), 
             sd(centralization.closeness(var14)$res)
             )
       )
```

![](16_ACESSO_H_O_custo_para_acessar_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var14_incloseness<- closeness(var14, weights = E(var14)$var14, mode = "in") %>% round(6)
var14_outcloseness<- closeness(var14, weights = E(var14)$var14, mode = "out") %>% round(6)
var14_totalcloseness<- closeness(var14, weights = E(var14)$var14, mode = "total") %>% round(6)
var14_incloseness_n<- closeness(var14,weights = E(var14)$var14, mode = "in", normalized = T) %>% round(6)
var14_outcloseness_n<- closeness(var14,weights = E(var14)$var14, mode = "out", normalized = T) %>% round(6)
var14_totalcloseness_n<- closeness(var14,weights = E(var14)$var14, mode = "total", normalized = T) %>% round(6)
var14_centr_closeness <- centralization.closeness(var14)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var14_df_closseness <- data.frame(
var14_incloseness,
var14_outcloseness,
var14_totalcloseness,
var14_incloseness_n,
var14_outcloseness_n,
var14_totalcloseness_n,
var14_centr_closeness) %>% round(6)

#Adding type
var14_df_closseness <-cbind(var14_df_closseness, V(var14)$LABEL_COR)

#Adding names
names(var14_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var14_df_closseness<-var14_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var14_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-88ca73b1be3e7575c4da" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-88ca73b1be3e7575c4da">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000137\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001783\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000933\" data-max=\"0.003311\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.025528\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.331551\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.173507\" data-max=\"0.615894\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[0.000134,0.000133,0.000135,0.000133,0.000133,0.000133,0.000133,0.000133,0.00013,0.000133,0.000133,0.000133,0.000131,0.000131,0.000131,0.000133,0.000133,0.000134,0.000131,0.000133,0.000133,0.000131,0.000133,0.000129,0.000129,0.000133,0.000129,0.000133,0.000133,0.000133,0.000133,0.000131,0.000133,0.000129,0.000133,0.000131,0.000133,0.000131,0.000131,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000129,0.000132,0.000132,0.000132,0.000132,0.000132,0.000124,0.000131,0.000131,0.000133,0.000131,0.00013,0.000131,0.000133,0.000136,0.00013,0.000131,0.000131,0.000131,2.9e-05,0.000131,0.000133,0.000131,0.000131,0.000132,0.000132,0.000132,0.000132,0.000132,0.000131,0.000131,0.000133,0.000124,0.000124,0.000124,0.000124,0.000131,0.000132,0.000131,0.000132,0.000132,0.000126,0.000131,0.000131,0.000131,0.000128,0.00013,0.000133,0.000131,0.000131,0.000126,0.000131,0.000132,0.000133,0.000129,0.000131,0.000134,0.000134,0.000131,0.000131,0.000131,0.000134,0.000135,0.000131,0.000131,0.000132,0.000132,0.000132,0.000132,0.000131,0.000131,0.000131,0.000132,0.000132,0.000131,0.000133,0.000132,0.000131,0.000132,0.000131,0.000132,0.000132,0.000132,0.000131,0.000131,0.000132,0.000132,0.000132,0.000132,0.000132,0.000131,0.000131,0.000132,0.000131,0.000132,0.000131,0.000131,0.000131,0.000132,0.000134,0.000133,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000133,0.000129,0.000135,0.000135,0.000134,0.00013,0.000132,0.000134,0.000137,0.000137,0.000132,0.000135,0.000132,0.000132,0.000132,0.000133,0.000133,0.000132,0.000133,0.000133,0.000132,0.000131,0.000132,0.000132,0.000132,0.000132,0.000134,0.000134,0.000134,0.000134,0.000132,0.000127,0.000136],[0.001401,0.001776,0.001773,0.001745,0.001179,0.001451,0.001484,0.001558,0.001468,0.001319,0.001261,0.001368,0.001319,0.001567,0.001435,0.00134,0.001783,0.001192,0.001401,0.001299,0.001456,0.001157,0.001462,0.00161,0.00137,0.001036,0.000792,0.001027,0.001681,0.001029,0.001613,0.001166,0.001575,0.001081,0.001085,0.001199,0.000944,0.00119,0.001468,0.001233,0.001724,0.001634,0.001634,0.00134,0.001422,0.001395,0.0007,0.001351,0.001351,0.001351,0.001351,0.001351,0.00119,0.000957,0.001575,0.001592,0.000967,0.00134,0.001277,0.001672,2.9e-05,0.000882,0.001471,0.000913,0.00094,2.9e-05,2.9e-05,0.001414,0.0012,0.001344,0.000899,0.00146,0.001453,0.000946,0.000935,0.001087,0.000943,0.001493,0.001127,0.001127,0.001127,0.001127,0.00135,0.001242,0.001412,0.000951,0.001355,0.001127,0.001531,0.000923,0.001235,0.001425,0.0009,0.001046,0.000935,0.00135,0.000934,0.000984,0.000901,0.001541,0.001179,0.001012,2.9e-05,2.9e-05,0.000904,0.001546,0.000903,2.9e-05,2.9e-05,0.000929,0.001425,0.00094,0.001087,0.000896,0.001145,0.000913,0.001464,0.001466,0.000907,0.000907,0.001445,0.001608,0.001391,0.001391,0.001391,0.001142,0.000895,0.001362,0.000895,0.000943,0.000938,0.000938,0.001439,0.001339,0.001073,0.001364,0.000896,0.001339,0.001339,0.000939,0.00134,0.001339,0.000905,0.000895,0.001337,0.000894,0.001437,0.00077,0.001312,0.001073,0.001073,0.001073,0.001073,0.001073,0.00094,0.00114,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.002809,0.003021,0.003311,0.002717,0.00232,0.002513,0.002331,0.002653,0.002278,0.002217,0.002326,0.0025,0.001848,0.002404,0.002227,0.00232,0.002907,0.002451,0.002262,0.002252,0.002558,0.002053,0.002519,0.002532,0.002137,0.002237,0.001786,0.002421,0.002611,0.002304,0.002551,0.001916,0.00277,0.00189,0.002278,0.002079,0.002247,0.002024,0.002278,0.002242,0.002667,0.002584,0.002551,0.002331,0.002227,0.002387,0.001808,0.002273,0.002273,0.002273,0.002273,0.002273,0.001876,0.002092,0.002545,0.002591,0.00207,0.002237,0.002217,0.002882,0.0025,0.001786,0.002457,0.001901,0.001934,0.000958,0.001751,0.002217,0.001988,0.002128,0.00211,0.002273,0.002433,0.002227,0.002273,0.001919,0.002137,0.002591,0.001647,0.001647,0.001647,0.001647,0.002257,0.002404,0.00237,0.002088,0.002353,0.001859,0.002506,0.00189,0.002083,0.002203,0.001672,0.002174,0.001946,0.00237,0.001387,0.001805,0.002096,0.002532,0.001842,0.002008,0.001965,0.001965,0.00189,0.002398,0.001898,0.001965,0.002008,0.002252,0.002268,0.002203,0.002049,0.002128,0.002088,0.001976,0.002283,0.002288,0.002,0.002041,0.002392,0.002513,0.002278,0.002273,0.002278,0.002024,0.001961,0.002288,0.002004,0.002024,0.001938,0.002049,0.002242,0.002232,0.002066,0.002123,0.001905,0.002165,0.002262,0.001957,0.002208,0.002208,0.001942,0.001942,0.002075,0.002506,0.002299,0.001953,0.001873,0.001938,0.001938,0.001938,0.001938,0.001938,0.002309,0.001996,0.001961,0.001961,0.001938,0.001733,0.001595,0.001984,0.002045,0.002041,0.001789,0.001733,0.001825,0.001789,0.001789,0.001873,0.001894,0.001789,0.001894,0.001908,0.001789,0.001736,0.001795,0.001789,0.001795,0.001789,0.002028,0.002028,0.002041,0.002088,0.001799,0.000933,0.002053],[0.024997,0.02483,0.025122,0.024744,0.024767,0.024767,0.024685,0.024734,0.024266,0.024695,0.024774,0.024774,0.024276,0.024339,0.0244,0.024668,0.024774,0.024836,0.024346,0.024665,0.02475,0.024301,0.024747,0.024012,0.023941,0.024646,0.023901,0.02476,0.024734,0.024734,0.024747,0.024352,0.024754,0.024078,0.024734,0.024304,0.024698,0.024413,0.024406,0.024741,0.024731,0.024701,0.024708,0.024698,0.024652,0.02477,0.023981,0.024464,0.024464,0.024464,0.024464,0.024464,0.023063,0.024438,0.024288,0.024737,0.024425,0.024244,0.024368,0.02483,0.025358,0.024197,0.024413,0.024374,0.024403,0.005348,0.024409,0.024662,0.024323,0.024288,0.02449,0.024493,0.024483,0.024503,0.024567,0.024368,0.024393,0.024807,0.023023,0.023023,0.023023,0.023023,0.024419,0.024477,0.024349,0.024506,0.024467,0.023408,0.024451,0.024342,0.024432,0.023883,0.024222,0.024665,0.024454,0.0244,0.023408,0.024301,0.024525,0.024721,0.024015,0.024403,0.025013,0.025013,0.024355,0.024403,0.024365,0.025013,0.025051,0.024458,0.024432,0.024551,0.024467,0.024506,0.024499,0.024397,0.024381,0.024381,0.024477,0.024506,0.024387,0.024691,0.024493,0.024445,0.024493,0.024438,0.024464,0.024493,0.024477,0.024442,0.024409,0.024496,0.024477,0.024461,0.024519,0.024525,0.024374,0.024368,0.02447,0.024371,0.024461,0.024442,0.024409,0.024381,0.024639,0.02484,0.024734,0.024374,0.024358,0.024406,0.024406,0.024406,0.024406,0.024406,0.024698,0.024025,0.025064,0.025064,0.02501,0.024172,0.02447,0.024883,0.025528,0.025528,0.024548,0.02502,0.024629,0.024548,0.024548,0.024731,0.024744,0.024548,0.024724,0.0248,0.024558,0.024448,0.024561,0.024548,0.024561,0.024558,0.024896,0.024896,0.02499,0.02492,0.024564,0.023598,0.025251],[0.260504,0.330373,0.329787,0.324607,0.21934,0.269956,0.275964,0.28972,0.273128,0.245383,0.234552,0.254446,0.245383,0.291536,0.266858,0.24933,0.331551,0.221692,0.260504,0.241558,0.270742,0.215278,0.27193,0.299517,0.254795,0.192746,0.147268,0.190965,0.312605,0.191358,0.3,0.216783,0.292913,0.201081,0.201735,0.223022,0.175637,0.221429,0.273128,0.229346,0.32069,0.303922,0.303922,0.24933,0.26458,0.259414,0.130252,0.251351,0.251351,0.251351,0.251351,0.251351,0.221429,0.17799,0.292913,0.296178,0.179884,0.24933,0.237548,0.311037,0.005348,0.164021,0.273529,0.169863,0.174812,0.005376,0.005405,0.263083,0.223289,0.25,0.167266,0.271533,0.270349,0.17597,0.173832,0.202174,0.175306,0.277612,0.209696,0.209696,0.209696,0.209696,0.251012,0.231056,0.262712,0.176806,0.252033,0.209696,0.284839,0.171587,0.22963,0.264957,0.167417,0.194561,0.173832,0.251012,0.173669,0.183071,0.167568,0.286595,0.21934,0.188259,0.005348,0.005348,0.168174,0.287481,0.16787,0.005348,0.005348,0.172862,0.264957,0.174812,0.202174,0.166667,0.213058,0.169863,0.272328,0.272727,0.168784,0.168784,0.268786,0.299035,0.258693,0.258693,0.258693,0.212329,0.166517,0.253406,0.166517,0.175306,0.174484,0.174484,0.267626,0.248996,0.199571,0.253752,0.166667,0.248996,0.248996,0.174648,0.24933,0.248996,0.168326,0.166517,0.248663,0.166369,0.267241,0.143297,0.244094,0.199571,0.199571,0.199571,0.199571,0.199571,0.174812,0.212087,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.522472,0.561934,0.615894,0.505435,0.431555,0.467337,0.433566,0.493369,0.42369,0.412417,0.432558,0.465,0.343808,0.447115,0.414254,0.431555,0.540698,0.455882,0.420814,0.418919,0.475703,0.38193,0.468514,0.470886,0.397436,0.416107,0.332143,0.450363,0.48564,0.428571,0.47449,0.356322,0.515235,0.351607,0.42369,0.386694,0.417978,0.376518,0.42369,0.41704,0.496,0.48062,0.47449,0.433566,0.414254,0.443914,0.336347,0.422727,0.422727,0.422727,0.422727,0.422727,0.348968,0.389121,0.473282,0.481865,0.385093,0.416107,0.412417,0.536023,0.465,0.332143,0.457002,0.353612,0.359768,0.178161,0.325744,0.412417,0.369781,0.395745,0.392405,0.422727,0.452555,0.414254,0.422727,0.357006,0.397436,0.481865,0.306425,0.306425,0.306425,0.306425,0.419865,0.447115,0.440758,0.388309,0.437647,0.345725,0.466165,0.351607,0.3875,0.409692,0.311037,0.404348,0.361868,0.440758,0.257975,0.33574,0.389937,0.470886,0.342541,0.373494,0.365422,0.365422,0.351607,0.446043,0.352941,0.365422,0.373494,0.418919,0.421769,0.409692,0.381148,0.395745,0.388309,0.367589,0.424658,0.425629,0.372,0.379592,0.444976,0.467337,0.42369,0.422727,0.42369,0.376518,0.364706,0.425629,0.372745,0.376518,0.360465,0.381148,0.41704,0.415179,0.384298,0.394904,0.354286,0.402597,0.420814,0.363992,0.410596,0.410596,0.361165,0.361165,0.385892,0.466165,0.427586,0.363281,0.348315,0.360465,0.360465,0.360465,0.360465,0.360465,0.429561,0.371257,0.364706,0.364706,0.360465,0.322357,0.296651,0.369048,0.380368,0.379592,0.332737,0.322357,0.339416,0.332737,0.332737,0.348315,0.352273,0.332737,0.352273,0.354962,0.332737,0.322917,0.333932,0.332737,0.333932,0.332737,0.377282,0.377282,0.379592,0.388309,0.334532,0.173507,0.38193],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var14_df_closseness, by=list(var14_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var14_df_closseness, by=list(var14_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-c178b155ce1ef561464b" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-c178b155ce1ef561464b">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000125\" data-max=\"0.000136\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001776\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.7e-05\" data-max=\"0.00065\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001927\" data-max=\"0.003311\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6.9e-05\" data-max=\"0.000491\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.023255\" data-max=\"0.025251\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4e-06\" data-max=\"0.004362\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.330373\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.008758\" data-max=\"0.12101\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.358517\" data-max=\"0.615894\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.012798\" data-max=\"0.091326\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003883\" data-max=\"0.139638\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[0.000132,0.000131,0.000133,0.000133,0.000133,0.000133,0.000133,0.000135,0.000133,0.000133,0.000133,0.000125,0.000132,0.000133,0.000133,0.000132,0.000136,0.000133,0.000134,0.000133,0.000134,0.000132,0.000131],[1e-06,3e-06,null,null,null,null,1e-06,null,null,null,null,2.3e-05,1e-06,0,0,2e-06,null,null,null,1e-06,null,1e-06,0],[0.001537,0.000455,0.001299,0.001776,0.001484,0.001672,0.001126,0.001773,0.001462,0.001437,0.001493,0.001107,0.0016,0.001419,0.001471,0.001346,2.9e-05,0.001541,0.001401,0.000906,0.001192,0.00093,0.00114],[0.000209,0.000581,null,null,null,null,0.000294,null,null,null,null,0.000331,4.7e-05,0.000258,0.000302,0.000184,null,null,null,0.00065,null,0.000435,0.000242],[0.002437,0.001927,0.002252,0.003021,0.002331,0.002882,0.002495,0.003311,0.002519,0.002299,0.002591,0.001994,0.002494,0.002474,0.00258,0.002187,0.002053,0.002532,0.002809,0.002158,0.002451,0.002046,0.002129],[0.000491,0.000294,null,null,null,null,6.9e-05,null,null,null,null,0.00039,0.000127,0.000156,0.000299,0.000213,null,null,null,0.000153,null,0.000139,0.000166],[0.024614,0.02442,0.024665,0.02483,0.024685,0.02483,0.024783,0.025122,0.024747,0.024734,0.024807,0.023255,0.02452,0.02475,0.024769,0.024466,0.025251,0.024721,0.024997,0.024658,0.024836,0.024546,0.024433],[0.000222,0.000587,null,null,null,null,4.9e-05,null,null,null,null,0.004362,0.000256,1.8e-05,4e-06,0.000284,null,null,null,0.000277,null,0.000227,6e-05],[0.285899,0.08457,0.241558,0.330373,0.275964,0.311037,0.209359,0.329787,0.27193,0.267241,0.277612,0.205999,0.297729,0.263877,0.273616,0.250438,0.005348,0.286595,0.260504,0.168503,0.221692,0.173044,0.212018],[0.038777,0.108137,null,null,null,null,0.054564,null,null,null,null,0.061628,0.008758,0.048089,0.056195,0.034236,null,null,null,0.12101,null,0.080963,0.044944],[0.453183,0.358517,0.418919,0.561934,0.433566,0.536023,0.464077,0.615894,0.468514,0.427586,0.481865,0.370827,0.463868,0.460129,0.479863,0.406863,0.38193,0.470886,0.522472,0.401263,0.455882,0.380512,0.395966],[0.091326,0.054627,null,null,null,null,0.012798,null,null,null,null,0.072523,0.023692,0.029053,0.055639,0.03971,null,null,null,0.028542,null,0.025877,0.030849],[0.300585,0.099063,0.286154,0.343173,0.303922,0.323478,0.300896,0.391579,0.295238,0.29108,0.313659,0.266552,0.319611,0.301223,0.316849,0.295625,0.005348,0.306425,0.371257,0.194167,0.314189,0.234219,0.288627],[0.043014,0.125815,null,null,null,null,0.018146,null,null,null,null,0.070696,0.003883,0.033229,0.029748,0.023781,null,null,null,0.139638,null,0.102732,0.011577]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var14_df_closseness <- data.frame(
var14_incloseness,
var14_outcloseness,
var14_totalcloseness,
var14_incloseness_n,
var14_outcloseness_n,
var14_totalcloseness_n,
var14_centr_closeness) %>% round(6)

#Adding type
var14_df_closseness <-cbind(var14_df_closseness, V(var14)$TIPO1)

#Adding names
names(var14_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var14_df_closseness<-var14_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var14_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-19ac205cdf4434fdc6fd" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-19ac205cdf4434fdc6fd">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000137\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001783\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000933\" data-max=\"0.003311\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.025528\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.331551\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.173507\" data-max=\"0.615894\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[0.000134,0.000133,0.000135,0.000133,0.000133,0.000133,0.000133,0.000133,0.00013,0.000133,0.000133,0.000133,0.000131,0.000131,0.000131,0.000133,0.000133,0.000134,0.000131,0.000133,0.000133,0.000131,0.000133,0.000129,0.000129,0.000133,0.000129,0.000133,0.000133,0.000133,0.000133,0.000131,0.000133,0.000129,0.000133,0.000131,0.000133,0.000131,0.000131,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000129,0.000132,0.000132,0.000132,0.000132,0.000132,0.000124,0.000131,0.000131,0.000133,0.000131,0.00013,0.000131,0.000133,0.000136,0.00013,0.000131,0.000131,0.000131,2.9e-05,0.000131,0.000133,0.000131,0.000131,0.000132,0.000132,0.000132,0.000132,0.000132,0.000131,0.000131,0.000133,0.000124,0.000124,0.000124,0.000124,0.000131,0.000132,0.000131,0.000132,0.000132,0.000126,0.000131,0.000131,0.000131,0.000128,0.00013,0.000133,0.000131,0.000131,0.000126,0.000131,0.000132,0.000133,0.000129,0.000131,0.000134,0.000134,0.000131,0.000131,0.000131,0.000134,0.000135,0.000131,0.000131,0.000132,0.000132,0.000132,0.000132,0.000131,0.000131,0.000131,0.000132,0.000132,0.000131,0.000133,0.000132,0.000131,0.000132,0.000131,0.000132,0.000132,0.000132,0.000131,0.000131,0.000132,0.000132,0.000132,0.000132,0.000132,0.000131,0.000131,0.000132,0.000131,0.000132,0.000131,0.000131,0.000131,0.000132,0.000134,0.000133,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000133,0.000129,0.000135,0.000135,0.000134,0.00013,0.000132,0.000134,0.000137,0.000137,0.000132,0.000135,0.000132,0.000132,0.000132,0.000133,0.000133,0.000132,0.000133,0.000133,0.000132,0.000131,0.000132,0.000132,0.000132,0.000132,0.000134,0.000134,0.000134,0.000134,0.000132,0.000127,0.000136],[0.001401,0.001776,0.001773,0.001745,0.001179,0.001451,0.001484,0.001558,0.001468,0.001319,0.001261,0.001368,0.001319,0.001567,0.001435,0.00134,0.001783,0.001192,0.001401,0.001299,0.001456,0.001157,0.001462,0.00161,0.00137,0.001036,0.000792,0.001027,0.001681,0.001029,0.001613,0.001166,0.001575,0.001081,0.001085,0.001199,0.000944,0.00119,0.001468,0.001233,0.001724,0.001634,0.001634,0.00134,0.001422,0.001395,0.0007,0.001351,0.001351,0.001351,0.001351,0.001351,0.00119,0.000957,0.001575,0.001592,0.000967,0.00134,0.001277,0.001672,2.9e-05,0.000882,0.001471,0.000913,0.00094,2.9e-05,2.9e-05,0.001414,0.0012,0.001344,0.000899,0.00146,0.001453,0.000946,0.000935,0.001087,0.000943,0.001493,0.001127,0.001127,0.001127,0.001127,0.00135,0.001242,0.001412,0.000951,0.001355,0.001127,0.001531,0.000923,0.001235,0.001425,0.0009,0.001046,0.000935,0.00135,0.000934,0.000984,0.000901,0.001541,0.001179,0.001012,2.9e-05,2.9e-05,0.000904,0.001546,0.000903,2.9e-05,2.9e-05,0.000929,0.001425,0.00094,0.001087,0.000896,0.001145,0.000913,0.001464,0.001466,0.000907,0.000907,0.001445,0.001608,0.001391,0.001391,0.001391,0.001142,0.000895,0.001362,0.000895,0.000943,0.000938,0.000938,0.001439,0.001339,0.001073,0.001364,0.000896,0.001339,0.001339,0.000939,0.00134,0.001339,0.000905,0.000895,0.001337,0.000894,0.001437,0.00077,0.001312,0.001073,0.001073,0.001073,0.001073,0.001073,0.00094,0.00114,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.002809,0.003021,0.003311,0.002717,0.00232,0.002513,0.002331,0.002653,0.002278,0.002217,0.002326,0.0025,0.001848,0.002404,0.002227,0.00232,0.002907,0.002451,0.002262,0.002252,0.002558,0.002053,0.002519,0.002532,0.002137,0.002237,0.001786,0.002421,0.002611,0.002304,0.002551,0.001916,0.00277,0.00189,0.002278,0.002079,0.002247,0.002024,0.002278,0.002242,0.002667,0.002584,0.002551,0.002331,0.002227,0.002387,0.001808,0.002273,0.002273,0.002273,0.002273,0.002273,0.001876,0.002092,0.002545,0.002591,0.00207,0.002237,0.002217,0.002882,0.0025,0.001786,0.002457,0.001901,0.001934,0.000958,0.001751,0.002217,0.001988,0.002128,0.00211,0.002273,0.002433,0.002227,0.002273,0.001919,0.002137,0.002591,0.001647,0.001647,0.001647,0.001647,0.002257,0.002404,0.00237,0.002088,0.002353,0.001859,0.002506,0.00189,0.002083,0.002203,0.001672,0.002174,0.001946,0.00237,0.001387,0.001805,0.002096,0.002532,0.001842,0.002008,0.001965,0.001965,0.00189,0.002398,0.001898,0.001965,0.002008,0.002252,0.002268,0.002203,0.002049,0.002128,0.002088,0.001976,0.002283,0.002288,0.002,0.002041,0.002392,0.002513,0.002278,0.002273,0.002278,0.002024,0.001961,0.002288,0.002004,0.002024,0.001938,0.002049,0.002242,0.002232,0.002066,0.002123,0.001905,0.002165,0.002262,0.001957,0.002208,0.002208,0.001942,0.001942,0.002075,0.002506,0.002299,0.001953,0.001873,0.001938,0.001938,0.001938,0.001938,0.001938,0.002309,0.001996,0.001961,0.001961,0.001938,0.001733,0.001595,0.001984,0.002045,0.002041,0.001789,0.001733,0.001825,0.001789,0.001789,0.001873,0.001894,0.001789,0.001894,0.001908,0.001789,0.001736,0.001795,0.001789,0.001795,0.001789,0.002028,0.002028,0.002041,0.002088,0.001799,0.000933,0.002053],[0.024997,0.02483,0.025122,0.024744,0.024767,0.024767,0.024685,0.024734,0.024266,0.024695,0.024774,0.024774,0.024276,0.024339,0.0244,0.024668,0.024774,0.024836,0.024346,0.024665,0.02475,0.024301,0.024747,0.024012,0.023941,0.024646,0.023901,0.02476,0.024734,0.024734,0.024747,0.024352,0.024754,0.024078,0.024734,0.024304,0.024698,0.024413,0.024406,0.024741,0.024731,0.024701,0.024708,0.024698,0.024652,0.02477,0.023981,0.024464,0.024464,0.024464,0.024464,0.024464,0.023063,0.024438,0.024288,0.024737,0.024425,0.024244,0.024368,0.02483,0.025358,0.024197,0.024413,0.024374,0.024403,0.005348,0.024409,0.024662,0.024323,0.024288,0.02449,0.024493,0.024483,0.024503,0.024567,0.024368,0.024393,0.024807,0.023023,0.023023,0.023023,0.023023,0.024419,0.024477,0.024349,0.024506,0.024467,0.023408,0.024451,0.024342,0.024432,0.023883,0.024222,0.024665,0.024454,0.0244,0.023408,0.024301,0.024525,0.024721,0.024015,0.024403,0.025013,0.025013,0.024355,0.024403,0.024365,0.025013,0.025051,0.024458,0.024432,0.024551,0.024467,0.024506,0.024499,0.024397,0.024381,0.024381,0.024477,0.024506,0.024387,0.024691,0.024493,0.024445,0.024493,0.024438,0.024464,0.024493,0.024477,0.024442,0.024409,0.024496,0.024477,0.024461,0.024519,0.024525,0.024374,0.024368,0.02447,0.024371,0.024461,0.024442,0.024409,0.024381,0.024639,0.02484,0.024734,0.024374,0.024358,0.024406,0.024406,0.024406,0.024406,0.024406,0.024698,0.024025,0.025064,0.025064,0.02501,0.024172,0.02447,0.024883,0.025528,0.025528,0.024548,0.02502,0.024629,0.024548,0.024548,0.024731,0.024744,0.024548,0.024724,0.0248,0.024558,0.024448,0.024561,0.024548,0.024561,0.024558,0.024896,0.024896,0.02499,0.02492,0.024564,0.023598,0.025251],[0.260504,0.330373,0.329787,0.324607,0.21934,0.269956,0.275964,0.28972,0.273128,0.245383,0.234552,0.254446,0.245383,0.291536,0.266858,0.24933,0.331551,0.221692,0.260504,0.241558,0.270742,0.215278,0.27193,0.299517,0.254795,0.192746,0.147268,0.190965,0.312605,0.191358,0.3,0.216783,0.292913,0.201081,0.201735,0.223022,0.175637,0.221429,0.273128,0.229346,0.32069,0.303922,0.303922,0.24933,0.26458,0.259414,0.130252,0.251351,0.251351,0.251351,0.251351,0.251351,0.221429,0.17799,0.292913,0.296178,0.179884,0.24933,0.237548,0.311037,0.005348,0.164021,0.273529,0.169863,0.174812,0.005376,0.005405,0.263083,0.223289,0.25,0.167266,0.271533,0.270349,0.17597,0.173832,0.202174,0.175306,0.277612,0.209696,0.209696,0.209696,0.209696,0.251012,0.231056,0.262712,0.176806,0.252033,0.209696,0.284839,0.171587,0.22963,0.264957,0.167417,0.194561,0.173832,0.251012,0.173669,0.183071,0.167568,0.286595,0.21934,0.188259,0.005348,0.005348,0.168174,0.287481,0.16787,0.005348,0.005348,0.172862,0.264957,0.174812,0.202174,0.166667,0.213058,0.169863,0.272328,0.272727,0.168784,0.168784,0.268786,0.299035,0.258693,0.258693,0.258693,0.212329,0.166517,0.253406,0.166517,0.175306,0.174484,0.174484,0.267626,0.248996,0.199571,0.253752,0.166667,0.248996,0.248996,0.174648,0.24933,0.248996,0.168326,0.166517,0.248663,0.166369,0.267241,0.143297,0.244094,0.199571,0.199571,0.199571,0.199571,0.199571,0.174812,0.212087,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.522472,0.561934,0.615894,0.505435,0.431555,0.467337,0.433566,0.493369,0.42369,0.412417,0.432558,0.465,0.343808,0.447115,0.414254,0.431555,0.540698,0.455882,0.420814,0.418919,0.475703,0.38193,0.468514,0.470886,0.397436,0.416107,0.332143,0.450363,0.48564,0.428571,0.47449,0.356322,0.515235,0.351607,0.42369,0.386694,0.417978,0.376518,0.42369,0.41704,0.496,0.48062,0.47449,0.433566,0.414254,0.443914,0.336347,0.422727,0.422727,0.422727,0.422727,0.422727,0.348968,0.389121,0.473282,0.481865,0.385093,0.416107,0.412417,0.536023,0.465,0.332143,0.457002,0.353612,0.359768,0.178161,0.325744,0.412417,0.369781,0.395745,0.392405,0.422727,0.452555,0.414254,0.422727,0.357006,0.397436,0.481865,0.306425,0.306425,0.306425,0.306425,0.419865,0.447115,0.440758,0.388309,0.437647,0.345725,0.466165,0.351607,0.3875,0.409692,0.311037,0.404348,0.361868,0.440758,0.257975,0.33574,0.389937,0.470886,0.342541,0.373494,0.365422,0.365422,0.351607,0.446043,0.352941,0.365422,0.373494,0.418919,0.421769,0.409692,0.381148,0.395745,0.388309,0.367589,0.424658,0.425629,0.372,0.379592,0.444976,0.467337,0.42369,0.422727,0.42369,0.376518,0.364706,0.425629,0.372745,0.376518,0.360465,0.381148,0.41704,0.415179,0.384298,0.394904,0.354286,0.402597,0.420814,0.363992,0.410596,0.410596,0.361165,0.361165,0.385892,0.466165,0.427586,0.363281,0.348315,0.360465,0.360465,0.360465,0.360465,0.360465,0.429561,0.371257,0.364706,0.364706,0.360465,0.322357,0.296651,0.369048,0.380368,0.379592,0.332737,0.322357,0.339416,0.332737,0.332737,0.348315,0.352273,0.332737,0.352273,0.354962,0.332737,0.322917,0.333932,0.332737,0.333932,0.332737,0.377282,0.377282,0.379592,0.388309,0.334532,0.173507,0.38193],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var14_df_closseness, by=list(var14_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var14_df_closseness, by=list(var14_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-dea216173346a7ba5e97" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-dea216173346a7ba5e97">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00013\" data-max=\"0.000132\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000772\" data-max=\"0.001148\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00041\" data-max=\"0.000611\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001988\" data-max=\"0.002234\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000276\" data-max=\"0.000319\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.024149\" data-max=\"0.024575\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000213\" data-max=\"0.002204\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.143602\" data-max=\"0.213498\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.076285\" data-max=\"0.113688\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.37003\" data-max=\"0.41544\" data-scale=\"5\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.051381\" data-max=\"0.059349\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.173776\" data-max=\"0.2725\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.079091\" data-max=\"0.135432\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[0.000132,0.00013],[1e-06,1.2e-05],[0.001148,0.000772],[0.00041,0.000611],[0.002234,0.001989],[0.000276,0.000319],[0.024575,0.024149],[0.000213,0.002204],[0.213498,0.143602],[0.076285,0.113688],[0.41544,0.37003],[0.051381,0.059349],[0.2725,0.173776],[0.079091,0.135432]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var14_df_closseness <- data.frame(
var14_incloseness,
var14_outcloseness,
var14_totalcloseness,
var14_incloseness_n,
var14_outcloseness_n,
var14_totalcloseness_n,
var14_centr_closeness) %>% round(6)

#Adding type
var14_df_closseness <-cbind(var14_df_closseness, V(var14)$TIPO2)

#Adding names
names(var14_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var14_df_closseness<-var14_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var14_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-e416dd5c7b92f7cf8b89" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-e416dd5c7b92f7cf8b89">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000137\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001783\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000933\" data-max=\"0.003311\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.025528\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.331551\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.173507\" data-max=\"0.615894\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[0.000134,0.000133,0.000135,0.000133,0.000133,0.000133,0.000133,0.000133,0.00013,0.000133,0.000133,0.000133,0.000131,0.000131,0.000131,0.000133,0.000133,0.000134,0.000131,0.000133,0.000133,0.000131,0.000133,0.000129,0.000129,0.000133,0.000129,0.000133,0.000133,0.000133,0.000133,0.000131,0.000133,0.000129,0.000133,0.000131,0.000133,0.000131,0.000131,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000129,0.000132,0.000132,0.000132,0.000132,0.000132,0.000124,0.000131,0.000131,0.000133,0.000131,0.00013,0.000131,0.000133,0.000136,0.00013,0.000131,0.000131,0.000131,2.9e-05,0.000131,0.000133,0.000131,0.000131,0.000132,0.000132,0.000132,0.000132,0.000132,0.000131,0.000131,0.000133,0.000124,0.000124,0.000124,0.000124,0.000131,0.000132,0.000131,0.000132,0.000132,0.000126,0.000131,0.000131,0.000131,0.000128,0.00013,0.000133,0.000131,0.000131,0.000126,0.000131,0.000132,0.000133,0.000129,0.000131,0.000134,0.000134,0.000131,0.000131,0.000131,0.000134,0.000135,0.000131,0.000131,0.000132,0.000132,0.000132,0.000132,0.000131,0.000131,0.000131,0.000132,0.000132,0.000131,0.000133,0.000132,0.000131,0.000132,0.000131,0.000132,0.000132,0.000132,0.000131,0.000131,0.000132,0.000132,0.000132,0.000132,0.000132,0.000131,0.000131,0.000132,0.000131,0.000132,0.000131,0.000131,0.000131,0.000132,0.000134,0.000133,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000133,0.000129,0.000135,0.000135,0.000134,0.00013,0.000132,0.000134,0.000137,0.000137,0.000132,0.000135,0.000132,0.000132,0.000132,0.000133,0.000133,0.000132,0.000133,0.000133,0.000132,0.000131,0.000132,0.000132,0.000132,0.000132,0.000134,0.000134,0.000134,0.000134,0.000132,0.000127,0.000136],[0.001401,0.001776,0.001773,0.001745,0.001179,0.001451,0.001484,0.001558,0.001468,0.001319,0.001261,0.001368,0.001319,0.001567,0.001435,0.00134,0.001783,0.001192,0.001401,0.001299,0.001456,0.001157,0.001462,0.00161,0.00137,0.001036,0.000792,0.001027,0.001681,0.001029,0.001613,0.001166,0.001575,0.001081,0.001085,0.001199,0.000944,0.00119,0.001468,0.001233,0.001724,0.001634,0.001634,0.00134,0.001422,0.001395,0.0007,0.001351,0.001351,0.001351,0.001351,0.001351,0.00119,0.000957,0.001575,0.001592,0.000967,0.00134,0.001277,0.001672,2.9e-05,0.000882,0.001471,0.000913,0.00094,2.9e-05,2.9e-05,0.001414,0.0012,0.001344,0.000899,0.00146,0.001453,0.000946,0.000935,0.001087,0.000943,0.001493,0.001127,0.001127,0.001127,0.001127,0.00135,0.001242,0.001412,0.000951,0.001355,0.001127,0.001531,0.000923,0.001235,0.001425,0.0009,0.001046,0.000935,0.00135,0.000934,0.000984,0.000901,0.001541,0.001179,0.001012,2.9e-05,2.9e-05,0.000904,0.001546,0.000903,2.9e-05,2.9e-05,0.000929,0.001425,0.00094,0.001087,0.000896,0.001145,0.000913,0.001464,0.001466,0.000907,0.000907,0.001445,0.001608,0.001391,0.001391,0.001391,0.001142,0.000895,0.001362,0.000895,0.000943,0.000938,0.000938,0.001439,0.001339,0.001073,0.001364,0.000896,0.001339,0.001339,0.000939,0.00134,0.001339,0.000905,0.000895,0.001337,0.000894,0.001437,0.00077,0.001312,0.001073,0.001073,0.001073,0.001073,0.001073,0.00094,0.00114,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.002809,0.003021,0.003311,0.002717,0.00232,0.002513,0.002331,0.002653,0.002278,0.002217,0.002326,0.0025,0.001848,0.002404,0.002227,0.00232,0.002907,0.002451,0.002262,0.002252,0.002558,0.002053,0.002519,0.002532,0.002137,0.002237,0.001786,0.002421,0.002611,0.002304,0.002551,0.001916,0.00277,0.00189,0.002278,0.002079,0.002247,0.002024,0.002278,0.002242,0.002667,0.002584,0.002551,0.002331,0.002227,0.002387,0.001808,0.002273,0.002273,0.002273,0.002273,0.002273,0.001876,0.002092,0.002545,0.002591,0.00207,0.002237,0.002217,0.002882,0.0025,0.001786,0.002457,0.001901,0.001934,0.000958,0.001751,0.002217,0.001988,0.002128,0.00211,0.002273,0.002433,0.002227,0.002273,0.001919,0.002137,0.002591,0.001647,0.001647,0.001647,0.001647,0.002257,0.002404,0.00237,0.002088,0.002353,0.001859,0.002506,0.00189,0.002083,0.002203,0.001672,0.002174,0.001946,0.00237,0.001387,0.001805,0.002096,0.002532,0.001842,0.002008,0.001965,0.001965,0.00189,0.002398,0.001898,0.001965,0.002008,0.002252,0.002268,0.002203,0.002049,0.002128,0.002088,0.001976,0.002283,0.002288,0.002,0.002041,0.002392,0.002513,0.002278,0.002273,0.002278,0.002024,0.001961,0.002288,0.002004,0.002024,0.001938,0.002049,0.002242,0.002232,0.002066,0.002123,0.001905,0.002165,0.002262,0.001957,0.002208,0.002208,0.001942,0.001942,0.002075,0.002506,0.002299,0.001953,0.001873,0.001938,0.001938,0.001938,0.001938,0.001938,0.002309,0.001996,0.001961,0.001961,0.001938,0.001733,0.001595,0.001984,0.002045,0.002041,0.001789,0.001733,0.001825,0.001789,0.001789,0.001873,0.001894,0.001789,0.001894,0.001908,0.001789,0.001736,0.001795,0.001789,0.001795,0.001789,0.002028,0.002028,0.002041,0.002088,0.001799,0.000933,0.002053],[0.024997,0.02483,0.025122,0.024744,0.024767,0.024767,0.024685,0.024734,0.024266,0.024695,0.024774,0.024774,0.024276,0.024339,0.0244,0.024668,0.024774,0.024836,0.024346,0.024665,0.02475,0.024301,0.024747,0.024012,0.023941,0.024646,0.023901,0.02476,0.024734,0.024734,0.024747,0.024352,0.024754,0.024078,0.024734,0.024304,0.024698,0.024413,0.024406,0.024741,0.024731,0.024701,0.024708,0.024698,0.024652,0.02477,0.023981,0.024464,0.024464,0.024464,0.024464,0.024464,0.023063,0.024438,0.024288,0.024737,0.024425,0.024244,0.024368,0.02483,0.025358,0.024197,0.024413,0.024374,0.024403,0.005348,0.024409,0.024662,0.024323,0.024288,0.02449,0.024493,0.024483,0.024503,0.024567,0.024368,0.024393,0.024807,0.023023,0.023023,0.023023,0.023023,0.024419,0.024477,0.024349,0.024506,0.024467,0.023408,0.024451,0.024342,0.024432,0.023883,0.024222,0.024665,0.024454,0.0244,0.023408,0.024301,0.024525,0.024721,0.024015,0.024403,0.025013,0.025013,0.024355,0.024403,0.024365,0.025013,0.025051,0.024458,0.024432,0.024551,0.024467,0.024506,0.024499,0.024397,0.024381,0.024381,0.024477,0.024506,0.024387,0.024691,0.024493,0.024445,0.024493,0.024438,0.024464,0.024493,0.024477,0.024442,0.024409,0.024496,0.024477,0.024461,0.024519,0.024525,0.024374,0.024368,0.02447,0.024371,0.024461,0.024442,0.024409,0.024381,0.024639,0.02484,0.024734,0.024374,0.024358,0.024406,0.024406,0.024406,0.024406,0.024406,0.024698,0.024025,0.025064,0.025064,0.02501,0.024172,0.02447,0.024883,0.025528,0.025528,0.024548,0.02502,0.024629,0.024548,0.024548,0.024731,0.024744,0.024548,0.024724,0.0248,0.024558,0.024448,0.024561,0.024548,0.024561,0.024558,0.024896,0.024896,0.02499,0.02492,0.024564,0.023598,0.025251],[0.260504,0.330373,0.329787,0.324607,0.21934,0.269956,0.275964,0.28972,0.273128,0.245383,0.234552,0.254446,0.245383,0.291536,0.266858,0.24933,0.331551,0.221692,0.260504,0.241558,0.270742,0.215278,0.27193,0.299517,0.254795,0.192746,0.147268,0.190965,0.312605,0.191358,0.3,0.216783,0.292913,0.201081,0.201735,0.223022,0.175637,0.221429,0.273128,0.229346,0.32069,0.303922,0.303922,0.24933,0.26458,0.259414,0.130252,0.251351,0.251351,0.251351,0.251351,0.251351,0.221429,0.17799,0.292913,0.296178,0.179884,0.24933,0.237548,0.311037,0.005348,0.164021,0.273529,0.169863,0.174812,0.005376,0.005405,0.263083,0.223289,0.25,0.167266,0.271533,0.270349,0.17597,0.173832,0.202174,0.175306,0.277612,0.209696,0.209696,0.209696,0.209696,0.251012,0.231056,0.262712,0.176806,0.252033,0.209696,0.284839,0.171587,0.22963,0.264957,0.167417,0.194561,0.173832,0.251012,0.173669,0.183071,0.167568,0.286595,0.21934,0.188259,0.005348,0.005348,0.168174,0.287481,0.16787,0.005348,0.005348,0.172862,0.264957,0.174812,0.202174,0.166667,0.213058,0.169863,0.272328,0.272727,0.168784,0.168784,0.268786,0.299035,0.258693,0.258693,0.258693,0.212329,0.166517,0.253406,0.166517,0.175306,0.174484,0.174484,0.267626,0.248996,0.199571,0.253752,0.166667,0.248996,0.248996,0.174648,0.24933,0.248996,0.168326,0.166517,0.248663,0.166369,0.267241,0.143297,0.244094,0.199571,0.199571,0.199571,0.199571,0.199571,0.174812,0.212087,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.522472,0.561934,0.615894,0.505435,0.431555,0.467337,0.433566,0.493369,0.42369,0.412417,0.432558,0.465,0.343808,0.447115,0.414254,0.431555,0.540698,0.455882,0.420814,0.418919,0.475703,0.38193,0.468514,0.470886,0.397436,0.416107,0.332143,0.450363,0.48564,0.428571,0.47449,0.356322,0.515235,0.351607,0.42369,0.386694,0.417978,0.376518,0.42369,0.41704,0.496,0.48062,0.47449,0.433566,0.414254,0.443914,0.336347,0.422727,0.422727,0.422727,0.422727,0.422727,0.348968,0.389121,0.473282,0.481865,0.385093,0.416107,0.412417,0.536023,0.465,0.332143,0.457002,0.353612,0.359768,0.178161,0.325744,0.412417,0.369781,0.395745,0.392405,0.422727,0.452555,0.414254,0.422727,0.357006,0.397436,0.481865,0.306425,0.306425,0.306425,0.306425,0.419865,0.447115,0.440758,0.388309,0.437647,0.345725,0.466165,0.351607,0.3875,0.409692,0.311037,0.404348,0.361868,0.440758,0.257975,0.33574,0.389937,0.470886,0.342541,0.373494,0.365422,0.365422,0.351607,0.446043,0.352941,0.365422,0.373494,0.418919,0.421769,0.409692,0.381148,0.395745,0.388309,0.367589,0.424658,0.425629,0.372,0.379592,0.444976,0.467337,0.42369,0.422727,0.42369,0.376518,0.364706,0.425629,0.372745,0.376518,0.360465,0.381148,0.41704,0.415179,0.384298,0.394904,0.354286,0.402597,0.420814,0.363992,0.410596,0.410596,0.361165,0.361165,0.385892,0.466165,0.427586,0.363281,0.348315,0.360465,0.360465,0.360465,0.360465,0.360465,0.429561,0.371257,0.364706,0.364706,0.360465,0.322357,0.296651,0.369048,0.380368,0.379592,0.332737,0.322357,0.339416,0.332737,0.332737,0.348315,0.352273,0.332737,0.352273,0.354962,0.332737,0.322917,0.333932,0.332737,0.333932,0.332737,0.377282,0.377282,0.379592,0.388309,0.334532,0.173507,0.38193],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var14_df_closseness, by=list(var14_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var14_df_closseness, by=list(var14_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-2aaecf2ede1fb86648dc" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-2aaecf2ede1fb86648dc">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00013\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000765\" data-max=\"0.001465\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000242\" data-max=\"0.000612\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001986\" data-max=\"0.002536\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000187\" data-max=\"0.00032\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.024141\" data-max=\"0.024757\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.2e-05\" data-max=\"0.002217\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.142239\" data-max=\"0.272464\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.044924\" data-max=\"0.113845\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.369339\" data-max=\"0.47159\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.034783\" data-max=\"0.059449\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172406\" data-max=\"0.308329\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.029295\" data-max=\"0.135782\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[0.000133,0.000132,0.00013],[0,1e-06,1.2e-05],[0.001465,0.001099,0.000765],[0.000242,0.000408,0.000612],[0.002536,0.002186,0.001986],[0.000187,0.000256,0.00032],[0.024757,0.024548,0.024141],[2.2e-05,0.000216,0.002217],[0.272464,0.204476,0.142239],[0.044924,0.075823,0.113845],[0.47159,0.406571,0.369339],[0.034783,0.047586,0.059449],[0.308329,0.266869,0.172406],[0.029295,0.082605,0.135782]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var14_data.RData")
```

