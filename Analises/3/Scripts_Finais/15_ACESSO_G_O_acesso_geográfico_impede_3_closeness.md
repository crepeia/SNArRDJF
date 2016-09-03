# SNA Closeness 15_ACESSO G) O acesso geográfico a este serviço impede a realização de ativades em conjunto. (var13) 
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 15_ACESSO G) O acesso geográfico a este serviço impede a realização de ativades em conjunto. (var13) 

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var13_data.RData")
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
#var13<-simplify(var13) #Simplify
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
V(var13)$incloseness <- closeness(var13, mode = "in", weights = E(var13)$var13) %>% round(6)
V(var13)$outcloseness <- closeness(var13, mode = "out", weights = E(var13)$var13) %>% round(6)
V(var13)$totalcloseness <- closeness(var13, mode = "total", weights = E(var13)$var13) %>% round(4)
```

###Saving to Environment

```r
var13_incloseness<- closeness(var13, mode = "in", weights = E(var13)$var13) %>% round(6)
var13_outcloseness<- closeness(var13, mode = "out", weights = E(var13)$var13) %>% round(6)
var13_totalcloseness<- closeness(var13, mode = "total", weights = E(var13)$var13) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var13_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0001320 0.0001330 0.0001318 0.0001330 0.0001360
```

```r
sd(var13_incloseness)
```

```
## [1] 7.87147e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var13)$incloseness<-closeness(var13, weights = E(var13)$var13, mode="in")

#Get Variable
V(var13)$var13_color_degree<-round(V(var13)$incloseness,6)

#Creating brewer pallette
vertex_var13_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var13)$var13_color_degree)), "RdBu"))(
            length(unique(V(var13)$var13_color_degree)))

#Saving as Vertex properties 
V(var13)$vertex_var13_color_degree<-
  vertex_var13_color_degree[as.numeric(
  cut(V(var13)$var13_color_degree,
      breaks=length(unique(V(var13)$var13_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var13, es=E(var13), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var13))
maxC <- rep(Inf, vcount(var13))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var13, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var13)$weight)


#PLotting
plot(var13, 
     layout=co,
     edge.color=V(var13)$vertex_var13_color_degree[edge.start],
     edge.arrow.size=closeness(var13, weights = E(var13)$var13, mode="in"),
     edge.width=E(var13)$weight/mean(E(var13)$weight),
     edge.curved = TRUE,
     vertex.color=V(var13)$vertex_var13_color_degree,
     vertex.size=closeness(var13, weights = E(var13)$var13, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var13,"LABEL_COR"),
     vertex.label.cex=(closeness(var13, weights = E(var13)$var13, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var13)$var13_color_degree
b<-V(var13)$vertex_var13_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 15_ACESSO G) O acesso geográfico a este serviço impede a realização de ativades em conjunto. (var13) ", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var13, mode="in", weights = E(var13)$var13)), 
             sd(closeness(var13, mode="in", weights = E(var13)$var13))
             )
       )
```

![](15_ACESSO_G_O_acesso_geográfico_impede_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var13_outcloseness)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000029 0.000861 0.001190 0.001047 0.001495 0.002000
```

```r
sd(var13_outcloseness)
```

```
## [1] 0.0005734853
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var13)$outcloseness<-closeness(var13, weights = E(var13)$var13, mode="out")

#Get Variable
V(var13)$var13_color_degree<-round(V(var13)$outcloseness,6)

#Creating brewer pallette
vertex_var13_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var13)$var13_color_degree)), "RdBu"))(
            length(unique(V(var13)$var13_color_degree)))

#Saving as Vertex properties 
V(var13)$vertex_var13_color_degree<-
  vertex_var13_color_degree[as.numeric(
  cut(V(var13)$var13_color_degree,
      breaks=length(unique(V(var13)$var13_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var13, es=E(var13), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var13))
maxC <- rep(Inf, vcount(var13))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var13, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var13)$weight)


#PLotting
plot(var13, 
     layout=co,
     edge.color=V(var13)$vertex_var13_color_degree[edge.start],
     edge.arrow.size=closeness(var13, weights = E(var13)$var13, mode="out"),
     edge.width=E(var13)$weight/2*mean(E(var13)$weight),
     edge.curved = TRUE,
     vertex.color=V(var13)$vertex_var13_color_degree,
     vertex.size=closeness(var13, weights = E(var13)$var13, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var13,"LABEL_COR"),
     vertex.label.cex=closeness(var13, weights = E(var13)$var13, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var13)$var13_color_degree
b<-V(var13)$vertex_var13_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 15_ACESSO G) O acesso geográfico a este serviço impede a realização de ativades em conjunto. (var13) ", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var13, mode="out", weights = E(var13)$var13)), 
             sd(closeness(var13, mode="out", weights = E(var13)$var13))
             )
       )
```

![](15_ACESSO_G_O_acesso_geográfico_impede_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var13_totalcloseness)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.001018 0.002037 0.002331 0.002263 0.002424 0.003704
```

```r
sd(var13_totalcloseness)
```

```
## [1] 0.0003289734
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var13)$allcloseness<-closeness(var13, weights = E(var13)$var13, mode="all")

#Get Variable
V(var13)$var13_color_degree<-round(V(var13)$allcloseness,6)

#Creating brewer pallette
vertex_var13_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var13)$var13_color_degree)), "RdBu"))(
            length(unique(V(var13)$var13_color_degree)))

#Saving as Vertex properties 
V(var13)$vertex_var13_color_degree<-
  vertex_var13_color_degree[as.numeric(
  cut(V(var13)$var13_color_degree,
      breaks=length(unique(V(var13)$var13_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var13, es=E(var13), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var13))
maxC <- rep(Inf, vcount(var13))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var13, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var13)$weight)


#PLotting
plot(var13, 
     layout=co,
     edge.color=V(var13)$vertex_var13_color_degree[edge.start],
     edge.arrow.size=closeness(var13, weights = E(var13)$var13, mode="all"),
     edge.width=E(var13)$weight/2*mean(E(var13)$weight),
     edge.curved = TRUE,
     vertex.color=V(var13)$vertex_var13_color_degree,
     vertex.size=closeness(var13, weights = E(var13)$var13, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var13,"LABEL_COR"),
     vertex.label.cex=(closeness(var13, weights = E(var13)$var13, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var13)$var13_color_degree
b<-V(var13)$vertex_var13_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 15_ACESSO G) O acesso geográfico a este serviço impede a realização de ativades em conjunto. (var13) ", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var13, mode="all", weights = E(var13)$var13)), 
             sd(closeness(var13, mode="all", weights = E(var13)$var13))
             )
       )
```

![](15_ACESSO_G_O_acesso_geográfico_impede_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var13)$incloseness_n <- closeness(var13, mode = "in",, weights = E(var13)$var13, normalized = T) %>% round(10)
V(var13)$outcloseness_n <- closeness(var13, mode = "out", normalized = T, weights = E(var13)$var13) %>% round(6)
V(var13)$totalcloseness_n <- closeness(var13, mode = "total", normalized = T, weights = E(var13)$var13) %>% round(6)
```

###Saving to Environment

```r
var13_incloseness_n<- closeness(var13, mode = "in", normalized = T, weights = E(var13)$var13) %>% round(6)
var13_outcloseness_n<- closeness(var13, mode = "out", normalized = T, weights = E(var13)$var13) %>% round(6)
var13_totalcloseness_n<- closeness(var13, mode = "total", normalized = T, weights = E(var13)$var13) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var13_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.024490 0.024720 0.024500 0.024760 0.025370
```

```r
sd(var13_incloseness_n)
```

```
## [1] 0.001465054
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var13)$incloseness_n<-closeness(var13, weights = E(var13)$var13, mode="in", normalized = T)

#Get Variable
V(var13)$var13_color_degree<-round(V(var13)$incloseness_n,6)

#Creating brewer pallette
vertex_var13_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var13)$var13_color_degree)), "RdBu"))(
            length(unique(V(var13)$var13_color_degree)))

#Saving as Vertex properties 
V(var13)$vertex_var13_color_degree<-
  vertex_var13_color_degree[as.numeric(
  cut(V(var13)$var13_color_degree,
      breaks=length(unique(V(var13)$var13_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var13, es=E(var13), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var13))
maxC <- rep(Inf, vcount(var13))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var13, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var13)$weight)


#PLotting
plot(var13, 
     layout=co,
     edge.color=V(var13)$vertex_var13_color_degree[edge.start],
     edge.arrow.size=closeness(var13, weights = E(var13)$var13, mode="in",normalized = T),
     edge.width=E(var13)$weight/10*mean(E(var13)$weight),
     edge.curved = TRUE,
     vertex.color=V(var13)$vertex_var13_color_degree,
     vertex.size=(closeness(var13, weights = E(var13)$var13, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var13,"LABEL_COR"),
     vertex.label.cex=closeness(var13, weights = E(var13)$var13, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var13)$var13_color_degree
b<-V(var13)$vertex_var13_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 15_ACESSO G) O acesso geográfico a este serviço impede a realização de ativades em conjunto. (var13) ", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var13, mode="in", weights = E(var13)$var13, normalized = T)), 
             sd(closeness(var13, mode="in", weights = E(var13)$var13, normalized = T))
             )
       )
```

![](15_ACESSO_G_O_acesso_geográfico_impede_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var13_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.160100 0.221400 0.194700 0.278000 0.372000
```

```r
sd(var13_outcloseness_n)
```

```
## [1] 0.1066857
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var13)$outcloseness_n<-closeness(var13, weights = E(var13)$var13, mode="out", normalized = T)

#Get Variable
V(var13)$var13_color_degree<-round(V(var13)$outcloseness_n,6)

#Creating brewer pallette
vertex_var13_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var13)$var13_color_degree)), "RdBu"))(
            length(unique(V(var13)$var13_color_degree)))

#Saving as Vertex properties 
V(var13)$vertex_var13_color_degree<-
  vertex_var13_color_degree[as.numeric(
  cut(V(var13)$var13_color_degree,
      breaks=length(unique(V(var13)$var13_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var13, es=E(var13), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var13))
maxC <- rep(Inf, vcount(var13))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var13, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var13)$weight)


#PLotting
plot(var13, 
     layout=co,
     edge.color=V(var13)$vertex_var13_color_degree[edge.start],
     edge.arrow.size=closeness(var13, weights = E(var13)$var13, mode="out",normalized = T),
     edge.width=E(var13)$weight/10*mean(E(var13)$weight),
     edge.curved = TRUE,
     vertex.color=V(var13)$vertex_var13_color_degree,
     vertex.size=(closeness(var13, weights = E(var13)$var13, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var13,"LABEL_COR"),
     vertex.label.cex=closeness(var13, weights = E(var13)$var13, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var13)$var13_color_degree
b<-V(var13)$vertex_var13_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 15_ACESSO G) O acesso geográfico a este serviço impede a realização de ativades em conjunto. (var13) ", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var13, mode="out", weights = E(var13)$var13, normalized = T)), 
             sd(closeness(var13, mode="out", weights = E(var13)$var13, normalized = T))
             )
       )
```

![](15_ACESSO_G_O_acesso_geográfico_impede_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var13_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1894  0.3788  0.4336  0.4210  0.4509  0.6889
```

```r
sd(var13_totalcloseness_n)
```

```
## [1] 0.06118657
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var13)$allcloseness_n<-closeness(var13, weights = E(var13)$var13, mode="all", normalized = T)

#Get Variable
V(var13)$var13_color_degree<-round(V(var13)$allcloseness_n,6)

#Creating brewer pallette
vertex_var13_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var13)$var13_color_degree)), "RdBu"))(
            length(unique(V(var13)$var13_color_degree)))

#Saving as Vertex properties 
V(var13)$vertex_var13_color_degree<-
  vertex_var13_color_degree[as.numeric(
  cut(V(var13)$var13_color_degree,
      breaks=length(unique(V(var13)$var13_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var13, es=E(var13), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var13))
maxC <- rep(Inf, vcount(var13))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var13, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var13)$weight)


#PLotting
plot(var13, 
     layout=co,
     edge.color=V(var13)$vertex_var13_color_degree[edge.start],
     edge.arrow.size=closeness(var13, weights = E(var13)$var13, mode="all",normalized = T),
     edge.width=E(var13)$weight/10*mean(E(var13)$weight),
     edge.curved = TRUE,
     vertex.color=V(var13)$vertex_var13_color_degree,
     vertex.size=(closeness(var13, weights = E(var13)$var13, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var13,"LABEL_COR"),
     vertex.label.cex=closeness(var13, weights = E(var13)$var13, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var13)$var13_color_degree
b<-V(var13)$vertex_var13_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 15_ACESSO G) O acesso geográfico a este serviço impede a realização de ativades em conjunto. (var13) ", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var13, mode="all", weights = E(var13)$var13, normalized = T)), 
             sd(closeness(var13, mode="all", weights = E(var13)$var13, normalized = T))
             )
       )
```

![](15_ACESSO_G_O_acesso_geográfico_impede_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var13)$incloseness_n <- closeness(var13, weights = E(var13)$var13, mode = "in", normalized = T) %>% round(6)
V(var13)$outcloseness_n <- closeness(var13, weights = E(var13)$var13, mode = "out", normalized = T) %>% round(6)
V(var13)$totalcloseness_n <- closeness(var13, weights = E(var13)$var13, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var13)$var13_centr_closeness<- centralization.closeness(var13)$res
var13_centr_closeness<- centralization.closeness(var13)$res
var13_centr_closeness_all<- centralization.closeness(var13)
```

###Centralization

```r
var13_centr_closeness_all$centralization
```

```
## [1] 0.1625197
```

###Theoretical Max

```r
var13_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var13)$var13_centr_closeness<- centralization.closeness(var13)$res

#Get Variable
V(var13)$var13_color_degree<-round(V(var13)$var13_centr_closeness,6)

#Creating brewer pallette
vertex_var13_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var13)$var13_color_degree)), "Spectral"))(
            length(unique(V(var13)$var13_color_degree)))

#Saving as Vertex properties 
V(var13)$vertex_var13_color_degree<-
  vertex_var13_color_degree[as.numeric(
  cut(V(var13)$var13_color_degree,
      breaks=length(unique(V(var13)$var13_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var13, es=E(var13), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var13))
maxC <- rep(Inf, vcount(var13))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var13, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var13)$weight)


#PLotting
plot(var13, 
     layout=co,
     edge.color=V(var13)$vertex_var13_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var13)$res,
     edge.width=E(var13)$weight/10*mean(E(var13)$weight),
     edge.curved = TRUE,
     vertex.color=V(var13)$vertex_var13_color_degree,
     vertex.size=centralization.closeness(var13)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var13,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var13)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var13)$var13_color_degree
b<-V(var13)$vertex_var13_color_degree
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
  title("Network Centralization Closeness - 15_ACESSO G) O acesso geográfico a este serviço impede a realização de ativades em conjunto. (var13) ", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var13)$res), 
             sd(centralization.closeness(var13)$res)
             )
       )
```

![](15_ACESSO_G_O_acesso_geográfico_impede_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var13_incloseness<- closeness(var13, weights = E(var13)$var13, mode = "in") %>% round(6)
var13_outcloseness<- closeness(var13, weights = E(var13)$var13, mode = "out") %>% round(6)
var13_totalcloseness<- closeness(var13, weights = E(var13)$var13, mode = "total") %>% round(6)
var13_incloseness_n<- closeness(var13,weights = E(var13)$var13, mode = "in", normalized = T) %>% round(6)
var13_outcloseness_n<- closeness(var13,weights = E(var13)$var13, mode = "out", normalized = T) %>% round(6)
var13_totalcloseness_n<- closeness(var13,weights = E(var13)$var13, mode = "total", normalized = T) %>% round(6)
var13_centr_closeness <- centralization.closeness(var13)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var13_df_closseness <- data.frame(
var13_incloseness,
var13_outcloseness,
var13_totalcloseness,
var13_incloseness_n,
var13_outcloseness_n,
var13_totalcloseness_n,
var13_centr_closeness) %>% round(6)

#Adding type
var13_df_closseness <-cbind(var13_df_closseness, V(var13)$LABEL_COR)

#Adding names
names(var13_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var13_df_closseness<-var13_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var13_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-3c753a96cc907b6284bf" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-3c753a96cc907b6284bf">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000136\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.002\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001018\" data-max=\"0.003704\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.025368\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.372\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.189409\" data-max=\"0.688889\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[0.000134,0.000134,0.000135,0.000133,0.000133,0.000133,0.000133,0.000133,0.00013,0.000133,0.000133,0.000133,0.000131,0.000131,0.000133,0.000133,0.000133,0.000134,0.000131,0.000133,0.000133,0.000131,0.000133,0.000129,0.000129,0.000133,0.000129,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000131,0.000133,0.000131,0.000133,0.000132,0.000131,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000129,0.000132,0.000132,0.000132,0.000132,0.000132,0.000124,0.000132,0.000131,0.000133,0.000133,0.000131,0.000129,0.000133,0.000136,0.000125,0.000133,0.000133,0.000131,2.9e-05,0.000131,0.000133,0.000131,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000124,0.000124,0.000124,0.000124,0.000133,0.000132,0.000131,0.000133,0.000131,0.000128,0.000133,0.000133,0.000133,0.000124,0.000126,0.000133,0.000133,0.000131,0.000128,0.000131,0.000133,0.000133,0.000129,0.000132,0.000135,0.000135,0.000133,0.000133,0.000133,0.000135,0.000135,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000132,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000134,0.000133,0.000133,0.000131,0.000132,0.000133,0.000132,0.000133,0.000132,0.000133,0.000129,0.000135,0.000136,0.000136,0.000132,0.000132,0.000134,0.000135,0.000135,0.000132,0.000135,0.000134,0.000132,0.000132,0.000134,0.000134,0.000132,0.000134,0.000134,0.000134,0.000134,0.000134,0.000132,0.000134,0.000132,0.000132,0.000132,0.000135,0.000134,0.000134,0.000127,0.000136],[0.001898,0.001642,0.002,0.001565,0.00155,0.001529,0.001565,0.001592,0.001531,0.001468,0.001006,0.001479,0.001721,0.001025,0.001305,0.001473,0.001681,0.001597,0.001515,0.001488,0.001555,0.001471,0.001508,0.001736,0.001468,0.000879,0.00141,0.001597,0.001626,0.001634,0.00158,0.001536,0.001608,0.001166,0.00117,0.001471,0.00122,0.001305,0.001527,0.001645,0.001626,0.001639,0.001639,0.001493,0.001522,0.001524,0.001183,0.001475,0.001475,0.001475,0.001475,0.001475,0.001435,0.001015,0.001429,0.001486,0.000992,0.001486,0.001218,0.001037,2.9e-05,0.000973,0.001531,0.000965,0.001513,2.9e-05,2.9e-05,0.001585,0.001214,0.000952,0.000952,0.001506,0.001522,0.001041,0.000961,0.001011,0.001227,0.001585,0.000856,0.000856,0.000856,0.000856,0.001484,0.001316,0.001011,0.000981,0.001506,0.00142,0.001548,0.000953,0.001181,0.001543,0.001174,0.000761,0.00119,0.00149,0.001126,0.00116,0.000955,0.00156,0.001221,0.001608,2.9e-05,2.9e-05,0.000965,0.001527,0.000954,2.9e-05,2.9e-05,0.001267,0.001543,0.000961,0.001481,0.000955,0.001193,0.00149,0.000972,0.001488,0.001163,0.001497,0.00152,0.001603,0.000951,0.000951,0.000951,0.000962,0.000949,0.001473,0.000949,0.000963,0.000962,0.000962,0.001497,0.001462,0.001462,0.001524,0.000953,0.001462,0.001462,0.000962,0.001464,0.001462,0.001477,0.001462,0.001462,0.000949,0.001493,0.000704,0.001263,0.000861,0.000861,0.000861,0.000861,0.000861,0.000963,0.001175,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.003584,0.002747,0.003704,0.002584,0.002591,0.002545,0.002545,0.002717,0.002342,0.002358,0.002421,0.002519,0.00266,0.001869,0.002326,0.002387,0.00266,0.002674,0.002364,0.002475,0.002558,0.002342,0.002469,0.002762,0.002315,0.002398,0.002488,0.002632,0.002525,0.002571,0.002513,0.002457,0.002538,0.001949,0.002381,0.002364,0.002331,0.002016,0.002336,0.002611,0.002532,0.002571,0.002571,0.002315,0.002415,0.002451,0.001996,0.002342,0.002342,0.002342,0.002342,0.002342,0.002174,0.002392,0.002331,0.002688,0.002347,0.002513,0.002119,0.002667,0.002611,0.001779,0.002427,0.002331,0.002342,0.001276,0.001637,0.002475,0.001953,0.002353,0.00237,0.002404,0.002551,0.002513,0.002375,0.00241,0.002545,0.002591,0.001479,0.001479,0.001479,0.001479,0.002364,0.002232,0.002037,0.00232,0.002433,0.002193,0.002451,0.002299,0.002315,0.002353,0.001626,0.002247,0.002364,0.002558,0.001786,0.001934,0.002326,0.0025,0.001848,0.002463,0.001842,0.001842,0.002331,0.002387,0.002347,0.001842,0.001842,0.002577,0.002392,0.002331,0.002392,0.002326,0.00232,0.002336,0.002342,0.002342,0.002358,0.00241,0.002364,0.002494,0.002309,0.002304,0.002309,0.002331,0.002304,0.002326,0.002331,0.002188,0.002299,0.002309,0.002353,0.002331,0.002353,0.002392,0.002331,0.002353,0.002381,0.002331,0.002336,0.002347,0.002309,0.002326,0.002242,0.002494,0.002381,0.002315,0.001949,0.002155,0.002299,0.002155,0.002299,0.002155,0.002532,0.001969,0.002165,0.002304,0.002299,0.002033,0.00177,0.001988,0.001669,0.001661,0.001898,0.001631,0.001942,0.001898,0.001898,0.001942,0.001972,0.001898,0.001965,0.001953,0.002079,0.002037,0.001961,0.001898,0.001961,0.001898,0.001898,0.001898,0.002096,0.001976,0.001992,0.001018,0.002198],[0.024993,0.02485,0.025125,0.02477,0.02479,0.024777,0.024678,0.024734,0.024266,0.024665,0.024787,0.024807,0.024326,0.024381,0.024711,0.024685,0.024797,0.024853,0.024422,0.024701,0.024783,0.024291,0.024797,0.02405,0.024003,0.024655,0.023914,0.024803,0.024767,0.024747,0.024783,0.024718,0.024764,0.024298,0.024764,0.024323,0.024704,0.024474,0.024438,0.024747,0.024754,0.024727,0.024704,0.024682,0.024655,0.024797,0.023907,0.024512,0.024512,0.024512,0.024512,0.024512,0.023057,0.024593,0.024314,0.024764,0.024741,0.024282,0.023901,0.024826,0.025368,0.023332,0.024714,0.024724,0.024393,0.005348,0.024435,0.024688,0.024342,0.024711,0.024718,0.024714,0.024721,0.024741,0.024741,0.024724,0.024734,0.024823,0.023106,0.023106,0.023106,0.023106,0.024734,0.024516,0.024442,0.024734,0.024419,0.023892,0.024737,0.024708,0.024711,0.023045,0.023435,0.024698,0.024741,0.024438,0.023892,0.024291,0.024741,0.024747,0.024068,0.024487,0.025047,0.025047,0.024724,0.024714,0.024727,0.025047,0.025047,0.024737,0.024727,0.024741,0.024754,0.024724,0.024727,0.024718,0.024721,0.024714,0.024731,0.02475,0.024711,0.024711,0.024718,0.024711,0.024718,0.024714,0.024714,0.024711,0.024721,0.024538,0.024708,0.024718,0.024714,0.024711,0.024737,0.02475,0.024724,0.024718,0.024731,0.024721,0.024731,0.024718,0.024711,0.024711,0.024659,0.024873,0.02475,0.024727,0.024384,0.024519,0.024711,0.024519,0.024711,0.024519,0.024721,0.023929,0.025156,0.02533,0.025323,0.024496,0.02459,0.02491,0.025088,0.025084,0.02448,0.025047,0.02489,0.02448,0.02448,0.024896,0.024893,0.02448,0.02489,0.02493,0.02498,0.024963,0.024903,0.02448,0.024903,0.024487,0.02448,0.02448,0.025024,0.024893,0.024893,0.023589,0.025255],[0.352941,0.305419,0.372,0.29108,0.288372,0.284404,0.29108,0.296178,0.284839,0.273128,0.187123,0.275148,0.320138,0.190574,0.24282,0.273932,0.312605,0.297125,0.281818,0.276786,0.289269,0.273529,0.280543,0.322917,0.273128,0.163445,0.262341,0.297125,0.302439,0.303922,0.293839,0.285714,0.299035,0.216783,0.217544,0.273529,0.226829,0.24282,0.283969,0.305921,0.302439,0.304918,0.304918,0.277612,0.283105,0.283537,0.220118,0.274336,0.274336,0.274336,0.274336,0.274336,0.266858,0.188832,0.265714,0.276374,0.184524,0.276374,0.226553,0.192946,0.005348,0.180934,0.284839,0.179537,0.281392,0.005376,0.005405,0.29477,0.225728,0.177143,0.177143,0.28012,0.283105,0.193548,0.178674,0.188069,0.228221,0.29477,0.159247,0.159247,0.159247,0.159247,0.275964,0.244737,0.188069,0.182532,0.28012,0.264205,0.287926,0.177312,0.219599,0.287037,0.21831,0.141553,0.221429,0.277198,0.209459,0.215777,0.17765,0.290172,0.227106,0.299035,0.005348,0.005348,0.179537,0.283969,0.177481,0.005348,0.005348,0.235741,0.287037,0.178674,0.275556,0.17765,0.221957,0.277198,0.180758,0.276786,0.216279,0.278443,0.282675,0.298077,0.176806,0.176806,0.176806,0.179018,0.176471,0.273932,0.176471,0.179191,0.178846,0.178846,0.278443,0.27193,0.27193,0.283537,0.177312,0.27193,0.27193,0.178846,0.272328,0.27193,0.274742,0.27193,0.27193,0.176471,0.277612,0.130986,0.234848,0.160069,0.160069,0.160069,0.160069,0.160069,0.179191,0.218566,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.666667,0.510989,0.688889,0.48062,0.481865,0.473282,0.473282,0.505435,0.435597,0.438679,0.450363,0.468514,0.494681,0.347664,0.432558,0.443914,0.494681,0.497326,0.439716,0.460396,0.475703,0.435597,0.459259,0.513812,0.430556,0.446043,0.462687,0.489474,0.469697,0.478149,0.467337,0.457002,0.472081,0.362573,0.442857,0.439716,0.433566,0.375,0.434579,0.48564,0.470886,0.478149,0.478149,0.430556,0.449275,0.455882,0.371257,0.435597,0.435597,0.435597,0.435597,0.435597,0.404348,0.444976,0.433566,0.5,0.43662,0.467337,0.394068,0.496,0.48564,0.330961,0.451456,0.433566,0.435597,0.237245,0.304419,0.460396,0.363281,0.437647,0.440758,0.447115,0.47449,0.467337,0.441805,0.448193,0.473282,0.481865,0.275148,0.275148,0.275148,0.275148,0.439716,0.415179,0.378819,0.431555,0.452555,0.407895,0.455882,0.427586,0.430556,0.437647,0.302439,0.417978,0.439716,0.475703,0.332143,0.359768,0.432558,0.465,0.343808,0.458128,0.342541,0.342541,0.433566,0.443914,0.43662,0.342541,0.342541,0.479381,0.444976,0.433566,0.444976,0.432558,0.431555,0.434579,0.435597,0.435597,0.438679,0.448193,0.439716,0.46384,0.429561,0.428571,0.429561,0.433566,0.428571,0.432558,0.433566,0.407002,0.427586,0.429561,0.437647,0.433566,0.437647,0.444976,0.433566,0.437647,0.442857,0.433566,0.434579,0.43662,0.429561,0.432558,0.41704,0.46384,0.442857,0.430556,0.362573,0.400862,0.427586,0.400862,0.427586,0.400862,0.470886,0.366142,0.402597,0.428571,0.427586,0.378049,0.329204,0.369781,0.310518,0.30897,0.352941,0.303426,0.361165,0.352941,0.352941,0.361165,0.366864,0.352941,0.365422,0.363281,0.386694,0.378819,0.364706,0.352941,0.364706,0.352941,0.352941,0.352941,0.389937,0.367589,0.370518,0.189409,0.408791],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var13_df_closseness, by=list(var13_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var13_df_closseness, by=list(var13_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-6e4195b4d83862cfc386" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-6e4195b4d83862cfc386">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000125\" data-max=\"0.000136\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.002\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8.2e-05\" data-max=\"0.000706\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001971\" data-max=\"0.003704\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.8e-05\" data-max=\"0.000496\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.02322\" data-max=\"0.025255\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-05\" data-max=\"0.004355\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.372\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.015266\" data-max=\"0.131424\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.366618\" data-max=\"0.688889\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.010769\" data-max=\"0.092267\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003883\" data-max=\"0.139638\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[0.000132,0.000132,0.000133,0.000134,0.000133,0.000133,0.000133,0.000135,0.000133,0.000133,0.000133,0.000125,0.000132,0.000133,0.000133,0.000132,0.000136,0.000133,0.000134,0.000133,0.000134,0.000133,0.000133],[1e-06,3e-06,null,null,null,null,1e-06,null,null,null,null,2.3e-05,1e-06,0,0,2e-06,null,null,null,2e-06,null,1e-06,0],[0.001499,0.000452,0.001488,0.001642,0.001565,0.001037,0.001367,0.002,0.001508,0.001493,0.001585,0.001292,0.001332,0.001452,0.001587,0.001498,2.9e-05,0.00156,0.001898,0.000983,0.001597,0.000812,0.001254],[0.000205,0.000583,null,null,null,null,0.000363,null,null,null,null,0.000386,0.000434,0.000217,8.2e-05,0.000153,null,null,null,0.000706,null,0.000372,0.000255],[0.00234,0.001971,0.002475,0.002747,0.002545,0.002667,0.002561,0.003704,0.002469,0.002381,0.002591,0.002199,0.00222,0.002517,0.002599,0.002341,0.002198,0.0025,0.003584,0.002151,0.002674,0.002272,0.002373],[0.000338,0.000329,null,null,null,null,6.9e-05,null,null,null,null,0.000366,0.000496,9.3e-05,5.8e-05,0.000243,null,null,null,0.000247,null,6.8e-05,6.4e-05],[0.024634,0.024433,0.024701,0.02485,0.024678,0.024826,0.02482,0.025125,0.024797,0.02475,0.024823,0.02322,0.024554,0.024776,0.024788,0.024499,0.025255,0.024747,0.024993,0.024695,0.024853,0.024763,0.024723],[0.000217,0.000603,null,null,null,null,4.7e-05,null,null,null,null,0.004355,0.000245,1.9e-05,1e-05,0.000264,null,null,null,0.000266,null,0.000241,2.3e-05],[0.278774,0.084,0.276786,0.305419,0.29108,0.192946,0.254288,0.372,0.280543,0.277612,0.29477,0.240367,0.247746,0.270112,0.295127,0.278685,0.005348,0.290172,0.352941,0.182816,0.297125,0.150961,0.233199],[0.038079,0.108403,null,null,null,null,0.067506,null,null,null,null,0.071784,0.080853,0.040445,0.015266,0.028372,null,null,null,0.131424,null,0.069266,0.047366],[0.43518,0.366618,0.460396,0.510989,0.473282,0.496,0.476339,0.688889,0.459259,0.442857,0.481865,0.408972,0.412906,0.468158,0.483276,0.435439,0.408791,0.465,0.666667,0.400137,0.497326,0.422641,0.441451],[0.062882,0.061218,null,null,null,null,0.012829,null,null,null,null,0.068061,0.092267,0.017247,0.010769,0.045237,null,null,null,0.046015,null,0.012633,0.011892],[0.300585,0.099063,0.286154,0.343173,0.303922,0.323478,0.300896,0.391579,0.295238,0.29108,0.313659,0.266552,0.319611,0.301223,0.316849,0.295625,0.005348,0.306425,0.371257,0.194167,0.314189,0.234219,0.288627],[0.043014,0.125815,null,null,null,null,0.018146,null,null,null,null,0.070696,0.003883,0.033229,0.029748,0.023781,null,null,null,0.139638,null,0.102732,0.011577]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var13_df_closseness <- data.frame(
var13_incloseness,
var13_outcloseness,
var13_totalcloseness,
var13_incloseness_n,
var13_outcloseness_n,
var13_totalcloseness_n,
var13_centr_closeness) %>% round(6)

#Adding type
var13_df_closseness <-cbind(var13_df_closseness, V(var13)$TIPO1)

#Adding names
names(var13_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var13_df_closseness<-var13_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var13_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-54e9d133e5eee30b8116" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-54e9d133e5eee30b8116">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000136\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.002\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001018\" data-max=\"0.003704\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.025368\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.372\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.189409\" data-max=\"0.688889\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[0.000134,0.000134,0.000135,0.000133,0.000133,0.000133,0.000133,0.000133,0.00013,0.000133,0.000133,0.000133,0.000131,0.000131,0.000133,0.000133,0.000133,0.000134,0.000131,0.000133,0.000133,0.000131,0.000133,0.000129,0.000129,0.000133,0.000129,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000131,0.000133,0.000131,0.000133,0.000132,0.000131,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000129,0.000132,0.000132,0.000132,0.000132,0.000132,0.000124,0.000132,0.000131,0.000133,0.000133,0.000131,0.000129,0.000133,0.000136,0.000125,0.000133,0.000133,0.000131,2.9e-05,0.000131,0.000133,0.000131,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000124,0.000124,0.000124,0.000124,0.000133,0.000132,0.000131,0.000133,0.000131,0.000128,0.000133,0.000133,0.000133,0.000124,0.000126,0.000133,0.000133,0.000131,0.000128,0.000131,0.000133,0.000133,0.000129,0.000132,0.000135,0.000135,0.000133,0.000133,0.000133,0.000135,0.000135,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000132,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000134,0.000133,0.000133,0.000131,0.000132,0.000133,0.000132,0.000133,0.000132,0.000133,0.000129,0.000135,0.000136,0.000136,0.000132,0.000132,0.000134,0.000135,0.000135,0.000132,0.000135,0.000134,0.000132,0.000132,0.000134,0.000134,0.000132,0.000134,0.000134,0.000134,0.000134,0.000134,0.000132,0.000134,0.000132,0.000132,0.000132,0.000135,0.000134,0.000134,0.000127,0.000136],[0.001898,0.001642,0.002,0.001565,0.00155,0.001529,0.001565,0.001592,0.001531,0.001468,0.001006,0.001479,0.001721,0.001025,0.001305,0.001473,0.001681,0.001597,0.001515,0.001488,0.001555,0.001471,0.001508,0.001736,0.001468,0.000879,0.00141,0.001597,0.001626,0.001634,0.00158,0.001536,0.001608,0.001166,0.00117,0.001471,0.00122,0.001305,0.001527,0.001645,0.001626,0.001639,0.001639,0.001493,0.001522,0.001524,0.001183,0.001475,0.001475,0.001475,0.001475,0.001475,0.001435,0.001015,0.001429,0.001486,0.000992,0.001486,0.001218,0.001037,2.9e-05,0.000973,0.001531,0.000965,0.001513,2.9e-05,2.9e-05,0.001585,0.001214,0.000952,0.000952,0.001506,0.001522,0.001041,0.000961,0.001011,0.001227,0.001585,0.000856,0.000856,0.000856,0.000856,0.001484,0.001316,0.001011,0.000981,0.001506,0.00142,0.001548,0.000953,0.001181,0.001543,0.001174,0.000761,0.00119,0.00149,0.001126,0.00116,0.000955,0.00156,0.001221,0.001608,2.9e-05,2.9e-05,0.000965,0.001527,0.000954,2.9e-05,2.9e-05,0.001267,0.001543,0.000961,0.001481,0.000955,0.001193,0.00149,0.000972,0.001488,0.001163,0.001497,0.00152,0.001603,0.000951,0.000951,0.000951,0.000962,0.000949,0.001473,0.000949,0.000963,0.000962,0.000962,0.001497,0.001462,0.001462,0.001524,0.000953,0.001462,0.001462,0.000962,0.001464,0.001462,0.001477,0.001462,0.001462,0.000949,0.001493,0.000704,0.001263,0.000861,0.000861,0.000861,0.000861,0.000861,0.000963,0.001175,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.003584,0.002747,0.003704,0.002584,0.002591,0.002545,0.002545,0.002717,0.002342,0.002358,0.002421,0.002519,0.00266,0.001869,0.002326,0.002387,0.00266,0.002674,0.002364,0.002475,0.002558,0.002342,0.002469,0.002762,0.002315,0.002398,0.002488,0.002632,0.002525,0.002571,0.002513,0.002457,0.002538,0.001949,0.002381,0.002364,0.002331,0.002016,0.002336,0.002611,0.002532,0.002571,0.002571,0.002315,0.002415,0.002451,0.001996,0.002342,0.002342,0.002342,0.002342,0.002342,0.002174,0.002392,0.002331,0.002688,0.002347,0.002513,0.002119,0.002667,0.002611,0.001779,0.002427,0.002331,0.002342,0.001276,0.001637,0.002475,0.001953,0.002353,0.00237,0.002404,0.002551,0.002513,0.002375,0.00241,0.002545,0.002591,0.001479,0.001479,0.001479,0.001479,0.002364,0.002232,0.002037,0.00232,0.002433,0.002193,0.002451,0.002299,0.002315,0.002353,0.001626,0.002247,0.002364,0.002558,0.001786,0.001934,0.002326,0.0025,0.001848,0.002463,0.001842,0.001842,0.002331,0.002387,0.002347,0.001842,0.001842,0.002577,0.002392,0.002331,0.002392,0.002326,0.00232,0.002336,0.002342,0.002342,0.002358,0.00241,0.002364,0.002494,0.002309,0.002304,0.002309,0.002331,0.002304,0.002326,0.002331,0.002188,0.002299,0.002309,0.002353,0.002331,0.002353,0.002392,0.002331,0.002353,0.002381,0.002331,0.002336,0.002347,0.002309,0.002326,0.002242,0.002494,0.002381,0.002315,0.001949,0.002155,0.002299,0.002155,0.002299,0.002155,0.002532,0.001969,0.002165,0.002304,0.002299,0.002033,0.00177,0.001988,0.001669,0.001661,0.001898,0.001631,0.001942,0.001898,0.001898,0.001942,0.001972,0.001898,0.001965,0.001953,0.002079,0.002037,0.001961,0.001898,0.001961,0.001898,0.001898,0.001898,0.002096,0.001976,0.001992,0.001018,0.002198],[0.024993,0.02485,0.025125,0.02477,0.02479,0.024777,0.024678,0.024734,0.024266,0.024665,0.024787,0.024807,0.024326,0.024381,0.024711,0.024685,0.024797,0.024853,0.024422,0.024701,0.024783,0.024291,0.024797,0.02405,0.024003,0.024655,0.023914,0.024803,0.024767,0.024747,0.024783,0.024718,0.024764,0.024298,0.024764,0.024323,0.024704,0.024474,0.024438,0.024747,0.024754,0.024727,0.024704,0.024682,0.024655,0.024797,0.023907,0.024512,0.024512,0.024512,0.024512,0.024512,0.023057,0.024593,0.024314,0.024764,0.024741,0.024282,0.023901,0.024826,0.025368,0.023332,0.024714,0.024724,0.024393,0.005348,0.024435,0.024688,0.024342,0.024711,0.024718,0.024714,0.024721,0.024741,0.024741,0.024724,0.024734,0.024823,0.023106,0.023106,0.023106,0.023106,0.024734,0.024516,0.024442,0.024734,0.024419,0.023892,0.024737,0.024708,0.024711,0.023045,0.023435,0.024698,0.024741,0.024438,0.023892,0.024291,0.024741,0.024747,0.024068,0.024487,0.025047,0.025047,0.024724,0.024714,0.024727,0.025047,0.025047,0.024737,0.024727,0.024741,0.024754,0.024724,0.024727,0.024718,0.024721,0.024714,0.024731,0.02475,0.024711,0.024711,0.024718,0.024711,0.024718,0.024714,0.024714,0.024711,0.024721,0.024538,0.024708,0.024718,0.024714,0.024711,0.024737,0.02475,0.024724,0.024718,0.024731,0.024721,0.024731,0.024718,0.024711,0.024711,0.024659,0.024873,0.02475,0.024727,0.024384,0.024519,0.024711,0.024519,0.024711,0.024519,0.024721,0.023929,0.025156,0.02533,0.025323,0.024496,0.02459,0.02491,0.025088,0.025084,0.02448,0.025047,0.02489,0.02448,0.02448,0.024896,0.024893,0.02448,0.02489,0.02493,0.02498,0.024963,0.024903,0.02448,0.024903,0.024487,0.02448,0.02448,0.025024,0.024893,0.024893,0.023589,0.025255],[0.352941,0.305419,0.372,0.29108,0.288372,0.284404,0.29108,0.296178,0.284839,0.273128,0.187123,0.275148,0.320138,0.190574,0.24282,0.273932,0.312605,0.297125,0.281818,0.276786,0.289269,0.273529,0.280543,0.322917,0.273128,0.163445,0.262341,0.297125,0.302439,0.303922,0.293839,0.285714,0.299035,0.216783,0.217544,0.273529,0.226829,0.24282,0.283969,0.305921,0.302439,0.304918,0.304918,0.277612,0.283105,0.283537,0.220118,0.274336,0.274336,0.274336,0.274336,0.274336,0.266858,0.188832,0.265714,0.276374,0.184524,0.276374,0.226553,0.192946,0.005348,0.180934,0.284839,0.179537,0.281392,0.005376,0.005405,0.29477,0.225728,0.177143,0.177143,0.28012,0.283105,0.193548,0.178674,0.188069,0.228221,0.29477,0.159247,0.159247,0.159247,0.159247,0.275964,0.244737,0.188069,0.182532,0.28012,0.264205,0.287926,0.177312,0.219599,0.287037,0.21831,0.141553,0.221429,0.277198,0.209459,0.215777,0.17765,0.290172,0.227106,0.299035,0.005348,0.005348,0.179537,0.283969,0.177481,0.005348,0.005348,0.235741,0.287037,0.178674,0.275556,0.17765,0.221957,0.277198,0.180758,0.276786,0.216279,0.278443,0.282675,0.298077,0.176806,0.176806,0.176806,0.179018,0.176471,0.273932,0.176471,0.179191,0.178846,0.178846,0.278443,0.27193,0.27193,0.283537,0.177312,0.27193,0.27193,0.178846,0.272328,0.27193,0.274742,0.27193,0.27193,0.176471,0.277612,0.130986,0.234848,0.160069,0.160069,0.160069,0.160069,0.160069,0.179191,0.218566,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.666667,0.510989,0.688889,0.48062,0.481865,0.473282,0.473282,0.505435,0.435597,0.438679,0.450363,0.468514,0.494681,0.347664,0.432558,0.443914,0.494681,0.497326,0.439716,0.460396,0.475703,0.435597,0.459259,0.513812,0.430556,0.446043,0.462687,0.489474,0.469697,0.478149,0.467337,0.457002,0.472081,0.362573,0.442857,0.439716,0.433566,0.375,0.434579,0.48564,0.470886,0.478149,0.478149,0.430556,0.449275,0.455882,0.371257,0.435597,0.435597,0.435597,0.435597,0.435597,0.404348,0.444976,0.433566,0.5,0.43662,0.467337,0.394068,0.496,0.48564,0.330961,0.451456,0.433566,0.435597,0.237245,0.304419,0.460396,0.363281,0.437647,0.440758,0.447115,0.47449,0.467337,0.441805,0.448193,0.473282,0.481865,0.275148,0.275148,0.275148,0.275148,0.439716,0.415179,0.378819,0.431555,0.452555,0.407895,0.455882,0.427586,0.430556,0.437647,0.302439,0.417978,0.439716,0.475703,0.332143,0.359768,0.432558,0.465,0.343808,0.458128,0.342541,0.342541,0.433566,0.443914,0.43662,0.342541,0.342541,0.479381,0.444976,0.433566,0.444976,0.432558,0.431555,0.434579,0.435597,0.435597,0.438679,0.448193,0.439716,0.46384,0.429561,0.428571,0.429561,0.433566,0.428571,0.432558,0.433566,0.407002,0.427586,0.429561,0.437647,0.433566,0.437647,0.444976,0.433566,0.437647,0.442857,0.433566,0.434579,0.43662,0.429561,0.432558,0.41704,0.46384,0.442857,0.430556,0.362573,0.400862,0.427586,0.400862,0.427586,0.400862,0.470886,0.366142,0.402597,0.428571,0.427586,0.378049,0.329204,0.369781,0.310518,0.30897,0.352941,0.303426,0.361165,0.352941,0.352941,0.361165,0.366864,0.352941,0.365422,0.363281,0.386694,0.378819,0.364706,0.352941,0.364706,0.352941,0.352941,0.352941,0.389937,0.367589,0.370518,0.189409,0.408791],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var13_df_closseness, by=list(var13_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var13_df_closseness, by=list(var13_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-016c48903554e5a38a1d" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-016c48903554e5a38a1d">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00013\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000829\" data-max=\"0.001205\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000437\" data-max=\"0.000663\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00208\" data-max=\"0.002397\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000237\" data-max=\"0.00035\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.024154\" data-max=\"0.024752\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000151\" data-max=\"0.002209\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.154238\" data-max=\"0.224214\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.081283\" data-max=\"0.123343\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.386894\" data-max=\"0.445893\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.044081\" data-max=\"0.065007\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.173776\" data-max=\"0.2725\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.079091\" data-max=\"0.135432\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[0.000133,0.00013],[1e-06,1.2e-05],[0.001205,0.000829],[0.000437,0.000663],[0.002397,0.00208],[0.000237,0.00035],[0.024752,0.024154],[0.000151,0.002209],[0.224214,0.154238],[0.081283,0.123343],[0.445893,0.386894],[0.044081,0.065007],[0.2725,0.173776],[0.079091,0.135432]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var13_df_closseness <- data.frame(
var13_incloseness,
var13_outcloseness,
var13_totalcloseness,
var13_incloseness_n,
var13_outcloseness_n,
var13_totalcloseness_n,
var13_centr_closeness) %>% round(6)

#Adding type
var13_df_closseness <-cbind(var13_df_closseness, V(var13)$TIPO2)

#Adding names
names(var13_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var13_df_closseness<-var13_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var13_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-e6af22cc1e538ee03eaa" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-e6af22cc1e538ee03eaa">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000136\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.002\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001018\" data-max=\"0.003704\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.025368\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.372\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.189409\" data-max=\"0.688889\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[0.000134,0.000134,0.000135,0.000133,0.000133,0.000133,0.000133,0.000133,0.00013,0.000133,0.000133,0.000133,0.000131,0.000131,0.000133,0.000133,0.000133,0.000134,0.000131,0.000133,0.000133,0.000131,0.000133,0.000129,0.000129,0.000133,0.000129,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000131,0.000133,0.000131,0.000133,0.000132,0.000131,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000129,0.000132,0.000132,0.000132,0.000132,0.000132,0.000124,0.000132,0.000131,0.000133,0.000133,0.000131,0.000129,0.000133,0.000136,0.000125,0.000133,0.000133,0.000131,2.9e-05,0.000131,0.000133,0.000131,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000124,0.000124,0.000124,0.000124,0.000133,0.000132,0.000131,0.000133,0.000131,0.000128,0.000133,0.000133,0.000133,0.000124,0.000126,0.000133,0.000133,0.000131,0.000128,0.000131,0.000133,0.000133,0.000129,0.000132,0.000135,0.000135,0.000133,0.000133,0.000133,0.000135,0.000135,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000132,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000133,0.000134,0.000133,0.000133,0.000131,0.000132,0.000133,0.000132,0.000133,0.000132,0.000133,0.000129,0.000135,0.000136,0.000136,0.000132,0.000132,0.000134,0.000135,0.000135,0.000132,0.000135,0.000134,0.000132,0.000132,0.000134,0.000134,0.000132,0.000134,0.000134,0.000134,0.000134,0.000134,0.000132,0.000134,0.000132,0.000132,0.000132,0.000135,0.000134,0.000134,0.000127,0.000136],[0.001898,0.001642,0.002,0.001565,0.00155,0.001529,0.001565,0.001592,0.001531,0.001468,0.001006,0.001479,0.001721,0.001025,0.001305,0.001473,0.001681,0.001597,0.001515,0.001488,0.001555,0.001471,0.001508,0.001736,0.001468,0.000879,0.00141,0.001597,0.001626,0.001634,0.00158,0.001536,0.001608,0.001166,0.00117,0.001471,0.00122,0.001305,0.001527,0.001645,0.001626,0.001639,0.001639,0.001493,0.001522,0.001524,0.001183,0.001475,0.001475,0.001475,0.001475,0.001475,0.001435,0.001015,0.001429,0.001486,0.000992,0.001486,0.001218,0.001037,2.9e-05,0.000973,0.001531,0.000965,0.001513,2.9e-05,2.9e-05,0.001585,0.001214,0.000952,0.000952,0.001506,0.001522,0.001041,0.000961,0.001011,0.001227,0.001585,0.000856,0.000856,0.000856,0.000856,0.001484,0.001316,0.001011,0.000981,0.001506,0.00142,0.001548,0.000953,0.001181,0.001543,0.001174,0.000761,0.00119,0.00149,0.001126,0.00116,0.000955,0.00156,0.001221,0.001608,2.9e-05,2.9e-05,0.000965,0.001527,0.000954,2.9e-05,2.9e-05,0.001267,0.001543,0.000961,0.001481,0.000955,0.001193,0.00149,0.000972,0.001488,0.001163,0.001497,0.00152,0.001603,0.000951,0.000951,0.000951,0.000962,0.000949,0.001473,0.000949,0.000963,0.000962,0.000962,0.001497,0.001462,0.001462,0.001524,0.000953,0.001462,0.001462,0.000962,0.001464,0.001462,0.001477,0.001462,0.001462,0.000949,0.001493,0.000704,0.001263,0.000861,0.000861,0.000861,0.000861,0.000861,0.000963,0.001175,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.003584,0.002747,0.003704,0.002584,0.002591,0.002545,0.002545,0.002717,0.002342,0.002358,0.002421,0.002519,0.00266,0.001869,0.002326,0.002387,0.00266,0.002674,0.002364,0.002475,0.002558,0.002342,0.002469,0.002762,0.002315,0.002398,0.002488,0.002632,0.002525,0.002571,0.002513,0.002457,0.002538,0.001949,0.002381,0.002364,0.002331,0.002016,0.002336,0.002611,0.002532,0.002571,0.002571,0.002315,0.002415,0.002451,0.001996,0.002342,0.002342,0.002342,0.002342,0.002342,0.002174,0.002392,0.002331,0.002688,0.002347,0.002513,0.002119,0.002667,0.002611,0.001779,0.002427,0.002331,0.002342,0.001276,0.001637,0.002475,0.001953,0.002353,0.00237,0.002404,0.002551,0.002513,0.002375,0.00241,0.002545,0.002591,0.001479,0.001479,0.001479,0.001479,0.002364,0.002232,0.002037,0.00232,0.002433,0.002193,0.002451,0.002299,0.002315,0.002353,0.001626,0.002247,0.002364,0.002558,0.001786,0.001934,0.002326,0.0025,0.001848,0.002463,0.001842,0.001842,0.002331,0.002387,0.002347,0.001842,0.001842,0.002577,0.002392,0.002331,0.002392,0.002326,0.00232,0.002336,0.002342,0.002342,0.002358,0.00241,0.002364,0.002494,0.002309,0.002304,0.002309,0.002331,0.002304,0.002326,0.002331,0.002188,0.002299,0.002309,0.002353,0.002331,0.002353,0.002392,0.002331,0.002353,0.002381,0.002331,0.002336,0.002347,0.002309,0.002326,0.002242,0.002494,0.002381,0.002315,0.001949,0.002155,0.002299,0.002155,0.002299,0.002155,0.002532,0.001969,0.002165,0.002304,0.002299,0.002033,0.00177,0.001988,0.001669,0.001661,0.001898,0.001631,0.001942,0.001898,0.001898,0.001942,0.001972,0.001898,0.001965,0.001953,0.002079,0.002037,0.001961,0.001898,0.001961,0.001898,0.001898,0.001898,0.002096,0.001976,0.001992,0.001018,0.002198],[0.024993,0.02485,0.025125,0.02477,0.02479,0.024777,0.024678,0.024734,0.024266,0.024665,0.024787,0.024807,0.024326,0.024381,0.024711,0.024685,0.024797,0.024853,0.024422,0.024701,0.024783,0.024291,0.024797,0.02405,0.024003,0.024655,0.023914,0.024803,0.024767,0.024747,0.024783,0.024718,0.024764,0.024298,0.024764,0.024323,0.024704,0.024474,0.024438,0.024747,0.024754,0.024727,0.024704,0.024682,0.024655,0.024797,0.023907,0.024512,0.024512,0.024512,0.024512,0.024512,0.023057,0.024593,0.024314,0.024764,0.024741,0.024282,0.023901,0.024826,0.025368,0.023332,0.024714,0.024724,0.024393,0.005348,0.024435,0.024688,0.024342,0.024711,0.024718,0.024714,0.024721,0.024741,0.024741,0.024724,0.024734,0.024823,0.023106,0.023106,0.023106,0.023106,0.024734,0.024516,0.024442,0.024734,0.024419,0.023892,0.024737,0.024708,0.024711,0.023045,0.023435,0.024698,0.024741,0.024438,0.023892,0.024291,0.024741,0.024747,0.024068,0.024487,0.025047,0.025047,0.024724,0.024714,0.024727,0.025047,0.025047,0.024737,0.024727,0.024741,0.024754,0.024724,0.024727,0.024718,0.024721,0.024714,0.024731,0.02475,0.024711,0.024711,0.024718,0.024711,0.024718,0.024714,0.024714,0.024711,0.024721,0.024538,0.024708,0.024718,0.024714,0.024711,0.024737,0.02475,0.024724,0.024718,0.024731,0.024721,0.024731,0.024718,0.024711,0.024711,0.024659,0.024873,0.02475,0.024727,0.024384,0.024519,0.024711,0.024519,0.024711,0.024519,0.024721,0.023929,0.025156,0.02533,0.025323,0.024496,0.02459,0.02491,0.025088,0.025084,0.02448,0.025047,0.02489,0.02448,0.02448,0.024896,0.024893,0.02448,0.02489,0.02493,0.02498,0.024963,0.024903,0.02448,0.024903,0.024487,0.02448,0.02448,0.025024,0.024893,0.024893,0.023589,0.025255],[0.352941,0.305419,0.372,0.29108,0.288372,0.284404,0.29108,0.296178,0.284839,0.273128,0.187123,0.275148,0.320138,0.190574,0.24282,0.273932,0.312605,0.297125,0.281818,0.276786,0.289269,0.273529,0.280543,0.322917,0.273128,0.163445,0.262341,0.297125,0.302439,0.303922,0.293839,0.285714,0.299035,0.216783,0.217544,0.273529,0.226829,0.24282,0.283969,0.305921,0.302439,0.304918,0.304918,0.277612,0.283105,0.283537,0.220118,0.274336,0.274336,0.274336,0.274336,0.274336,0.266858,0.188832,0.265714,0.276374,0.184524,0.276374,0.226553,0.192946,0.005348,0.180934,0.284839,0.179537,0.281392,0.005376,0.005405,0.29477,0.225728,0.177143,0.177143,0.28012,0.283105,0.193548,0.178674,0.188069,0.228221,0.29477,0.159247,0.159247,0.159247,0.159247,0.275964,0.244737,0.188069,0.182532,0.28012,0.264205,0.287926,0.177312,0.219599,0.287037,0.21831,0.141553,0.221429,0.277198,0.209459,0.215777,0.17765,0.290172,0.227106,0.299035,0.005348,0.005348,0.179537,0.283969,0.177481,0.005348,0.005348,0.235741,0.287037,0.178674,0.275556,0.17765,0.221957,0.277198,0.180758,0.276786,0.216279,0.278443,0.282675,0.298077,0.176806,0.176806,0.176806,0.179018,0.176471,0.273932,0.176471,0.179191,0.178846,0.178846,0.278443,0.27193,0.27193,0.283537,0.177312,0.27193,0.27193,0.178846,0.272328,0.27193,0.274742,0.27193,0.27193,0.176471,0.277612,0.130986,0.234848,0.160069,0.160069,0.160069,0.160069,0.160069,0.179191,0.218566,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.666667,0.510989,0.688889,0.48062,0.481865,0.473282,0.473282,0.505435,0.435597,0.438679,0.450363,0.468514,0.494681,0.347664,0.432558,0.443914,0.494681,0.497326,0.439716,0.460396,0.475703,0.435597,0.459259,0.513812,0.430556,0.446043,0.462687,0.489474,0.469697,0.478149,0.467337,0.457002,0.472081,0.362573,0.442857,0.439716,0.433566,0.375,0.434579,0.48564,0.470886,0.478149,0.478149,0.430556,0.449275,0.455882,0.371257,0.435597,0.435597,0.435597,0.435597,0.435597,0.404348,0.444976,0.433566,0.5,0.43662,0.467337,0.394068,0.496,0.48564,0.330961,0.451456,0.433566,0.435597,0.237245,0.304419,0.460396,0.363281,0.437647,0.440758,0.447115,0.47449,0.467337,0.441805,0.448193,0.473282,0.481865,0.275148,0.275148,0.275148,0.275148,0.439716,0.415179,0.378819,0.431555,0.452555,0.407895,0.455882,0.427586,0.430556,0.437647,0.302439,0.417978,0.439716,0.475703,0.332143,0.359768,0.432558,0.465,0.343808,0.458128,0.342541,0.342541,0.433566,0.443914,0.43662,0.342541,0.342541,0.479381,0.444976,0.433566,0.444976,0.432558,0.431555,0.434579,0.435597,0.435597,0.438679,0.448193,0.439716,0.46384,0.429561,0.428571,0.429561,0.433566,0.428571,0.432558,0.433566,0.407002,0.427586,0.429561,0.437647,0.433566,0.437647,0.444976,0.433566,0.437647,0.442857,0.433566,0.434579,0.43662,0.429561,0.432558,0.41704,0.46384,0.442857,0.430556,0.362573,0.400862,0.427586,0.400862,0.427586,0.400862,0.470886,0.366142,0.402597,0.428571,0.427586,0.378049,0.329204,0.369781,0.310518,0.30897,0.352941,0.303426,0.361165,0.352941,0.352941,0.361165,0.366864,0.352941,0.365422,0.363281,0.386694,0.378819,0.364706,0.352941,0.364706,0.352941,0.352941,0.352941,0.389937,0.367589,0.370518,0.189409,0.408791],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var13_df_closseness, by=list(var13_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var13_df_closseness, by=list(var13_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-50db92a87a95be24f9dc" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-50db92a87a95be24f9dc">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00013\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000822\" data-max=\"0.00151\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000183\" data-max=\"0.000664\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.002077\" data-max=\"0.002541\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8.2e-05\" data-max=\"0.000351\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.024145\" data-max=\"0.024779\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.1e-05\" data-max=\"0.002222\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.152845\" data-max=\"0.280842\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.034008\" data-max=\"0.123557\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.386345\" data-max=\"0.472539\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.015166\" data-max=\"0.065336\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172406\" data-max=\"0.308329\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.029295\" data-max=\"0.135782\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[0.000133,0.000133,0.00013],[0,1e-06,1.2e-05],[0.00151,0.001159,0.000822],[0.000183,0.000445,0.000664],[0.002541,0.002374,0.002077],[8.2e-05,0.000244,0.000351],[0.024779,0.024748,0.024145],[2.1e-05,0.000162,0.002222],[0.280842,0.215589,0.152845],[0.034008,0.082719,0.123557],[0.472539,0.441469,0.386345],[0.015166,0.045382,0.065335],[0.308329,0.266869,0.172406],[0.029295,0.082605,0.135782]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var13_data.RData")
```

