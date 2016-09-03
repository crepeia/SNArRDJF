# SNA Closeness 33_TOTAL
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 33_TOTAL

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/total_data.RData")
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
#total<-simplify(total) #Simplify
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
V(total)$incloseness <- closeness(total, mode = "in", weights = E(total)$total) %>% round(6)
V(total)$outcloseness <- closeness(total, mode = "out", weights = E(total)$total) %>% round(6)
V(total)$totalcloseness <- closeness(total, mode = "total", weights = E(total)$total) %>% round(4)
```

###Saving to Environment

```r
total_incloseness<- closeness(total, mode = "in", weights = E(total)$total) %>% round(6)
total_outcloseness<- closeness(total, mode = "out", weights = E(total)$total) %>% round(6)
total_totalcloseness<- closeness(total, mode = "total", weights = E(total)$total) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(total_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 2.900e-05 4.000e-05 4.700e-05 4.452e-05 4.900e-05 6.500e-05
```

```r
sd(total_incloseness)
```

```
## [1] 6.732461e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(total)$incloseness<-closeness(total, weights = E(total)$total, mode="in")

#Get Variable
V(total)$total_color_degree<-round(V(total)$incloseness,6)

#Creating brewer pallette
vertex_total_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(total)$total_color_degree)), "RdBu"))(
            length(unique(V(total)$total_color_degree)))

#Saving as Vertex properties 
V(total)$vertex_total_color_degree<-
  vertex_total_color_degree[as.numeric(
  cut(V(total)$total_color_degree,
      breaks=length(unique(V(total)$total_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(total, es=E(total), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(total))
maxC <- rep(Inf, vcount(total))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(total, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(total)$weight)


#PLotting
plot(total, 
     layout=co,
     edge.color=V(total)$vertex_total_color_degree[edge.start],
     edge.arrow.size=closeness(total, weights = E(total)$total, mode="in"),
     edge.width=E(total)$weight/mean(E(total)$weight),
     edge.curved = TRUE,
     vertex.color=V(total)$vertex_total_color_degree,
     vertex.size=closeness(total, weights = E(total)$total, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(total,"LABEL_COR"),
     vertex.label.cex=(closeness(total, weights = E(total)$total, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(total)$total_color_degree
b<-V(total)$vertex_total_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 33_TOTAL", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(total, mode="in", weights = E(total)$total)), 
             sd(closeness(total, mode="in", weights = E(total)$total))
             )
       )
```

![](33_TOTAL_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(total_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 2.900e-05 3.850e-05 4.900e-05 4.629e-05 5.300e-05 7.600e-05
```

```r
sd(total_outcloseness)
```

```
## [1] 1.079646e-05
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(total)$outcloseness<-closeness(total, weights = E(total)$total, mode="out")

#Get Variable
V(total)$total_color_degree<-round(V(total)$outcloseness,6)

#Creating brewer pallette
vertex_total_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(total)$total_color_degree)), "RdBu"))(
            length(unique(V(total)$total_color_degree)))

#Saving as Vertex properties 
V(total)$vertex_total_color_degree<-
  vertex_total_color_degree[as.numeric(
  cut(V(total)$total_color_degree,
      breaks=length(unique(V(total)$total_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(total, es=E(total), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(total))
maxC <- rep(Inf, vcount(total))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(total, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(total)$weight)


#PLotting
plot(total, 
     layout=co,
     edge.color=V(total)$vertex_total_color_degree[edge.start],
     edge.arrow.size=closeness(total, weights = E(total)$total, mode="out"),
     edge.width=E(total)$weight/2*mean(E(total)$weight),
     edge.curved = TRUE,
     vertex.color=V(total)$vertex_total_color_degree,
     vertex.size=closeness(total, weights = E(total)$total, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(total,"LABEL_COR"),
     vertex.label.cex=closeness(total, weights = E(total)$total, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(total)$total_color_degree
b<-V(total)$vertex_total_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 33_TOTAL", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(total, mode="out", weights = E(total)$total)), 
             sd(closeness(total, mode="out", weights = E(total)$total))
             )
       )
```

![](33_TOTAL_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(total_totalcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 3.500e-05 5.500e-05 6.000e-05 6.036e-05 6.600e-05 9.300e-05
```

```r
sd(total_totalcloseness)
```

```
## [1] 1.077414e-05
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(total)$allcloseness<-closeness(total, weights = E(total)$total, mode="all")

#Get Variable
V(total)$total_color_degree<-round(V(total)$allcloseness,6)

#Creating brewer pallette
vertex_total_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(total)$total_color_degree)), "RdBu"))(
            length(unique(V(total)$total_color_degree)))

#Saving as Vertex properties 
V(total)$vertex_total_color_degree<-
  vertex_total_color_degree[as.numeric(
  cut(V(total)$total_color_degree,
      breaks=length(unique(V(total)$total_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(total, es=E(total), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(total))
maxC <- rep(Inf, vcount(total))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(total, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(total)$weight)


#PLotting
plot(total, 
     layout=co,
     edge.color=V(total)$vertex_total_color_degree[edge.start],
     edge.arrow.size=closeness(total, weights = E(total)$total, mode="all"),
     edge.width=E(total)$weight/2*mean(E(total)$weight),
     edge.curved = TRUE,
     vertex.color=V(total)$vertex_total_color_degree,
     vertex.size=closeness(total, weights = E(total)$total, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(total,"LABEL_COR"),
     vertex.label.cex=(closeness(total, weights = E(total)$total, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(total)$total_color_degree
b<-V(total)$vertex_total_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 33_TOTAL", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(total, mode="all", weights = E(total)$total)), 
             sd(closeness(total, mode="all", weights = E(total)$total))
             )
       )
```

![](33_TOTAL_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(total)$incloseness_n <- closeness(total, mode = "in",, weights = E(total)$total, normalized = T) %>% round(10)
V(total)$outcloseness_n <- closeness(total, mode = "out", normalized = T, weights = E(total)$total) %>% round(6)
V(total)$totalcloseness_n <- closeness(total, mode = "total", normalized = T, weights = E(total)$total) %>% round(6)
```

###Saving to Environment

```r
total_incloseness_n<- closeness(total, mode = "in", normalized = T, weights = E(total)$total) %>% round(6)
total_outcloseness_n<- closeness(total, mode = "out", normalized = T, weights = E(total)$total) %>% round(6)
total_totalcloseness_n<- closeness(total, mode = "total", normalized = T, weights = E(total)$total) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(total_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005344 0.007432 0.008831 0.008284 0.009068 0.012160
```

```r
sd(total_incloseness_n)
```

```
## [1] 0.001252253
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(total)$incloseness_n<-closeness(total, weights = E(total)$total, mode="in", normalized = T)

#Get Variable
V(total)$total_color_degree<-round(V(total)$incloseness_n,6)

#Creating brewer pallette
vertex_total_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(total)$total_color_degree)), "RdBu"))(
            length(unique(V(total)$total_color_degree)))

#Saving as Vertex properties 
V(total)$vertex_total_color_degree<-
  vertex_total_color_degree[as.numeric(
  cut(V(total)$total_color_degree,
      breaks=length(unique(V(total)$total_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(total, es=E(total), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(total))
maxC <- rep(Inf, vcount(total))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(total, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(total)$weight)


#PLotting
plot(total, 
     layout=co,
     edge.color=V(total)$vertex_total_color_degree[edge.start],
     edge.arrow.size=closeness(total, weights = E(total)$total, mode="in",normalized = T),
     edge.width=E(total)$weight/10*mean(E(total)$weight),
     edge.curved = TRUE,
     vertex.color=V(total)$vertex_total_color_degree,
     vertex.size=(closeness(total, weights = E(total)$total, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(total,"LABEL_COR"),
     vertex.label.cex=closeness(total, weights = E(total)$total, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(total)$total_color_degree
b<-V(total)$vertex_total_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 33_TOTAL", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(total, mode="in", weights = E(total)$total, normalized = T)), 
             sd(closeness(total, mode="in", weights = E(total)$total, normalized = T))
             )
       )
```

![](33_TOTAL_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(total_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.007153 0.009093 0.008606 0.009927 0.014060
```

```r
sd(total_outcloseness_n)
```

```
## [1] 0.002020159
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(total)$outcloseness_n<-closeness(total, weights = E(total)$total, mode="out", normalized = T)

#Get Variable
V(total)$total_color_degree<-round(V(total)$outcloseness_n,6)

#Creating brewer pallette
vertex_total_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(total)$total_color_degree)), "RdBu"))(
            length(unique(V(total)$total_color_degree)))

#Saving as Vertex properties 
V(total)$vertex_total_color_degree<-
  vertex_total_color_degree[as.numeric(
  cut(V(total)$total_color_degree,
      breaks=length(unique(V(total)$total_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(total, es=E(total), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(total))
maxC <- rep(Inf, vcount(total))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(total, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(total)$weight)


#PLotting
plot(total, 
     layout=co,
     edge.color=V(total)$vertex_total_color_degree[edge.start],
     edge.arrow.size=closeness(total, weights = E(total)$total, mode="out",normalized = T),
     edge.width=E(total)$weight/10*mean(E(total)$weight),
     edge.curved = TRUE,
     vertex.color=V(total)$vertex_total_color_degree,
     vertex.size=(closeness(total, weights = E(total)$total, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(total,"LABEL_COR"),
     vertex.label.cex=closeness(total, weights = E(total)$total, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(total)$total_color_degree
b<-V(total)$vertex_total_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 33_TOTAL", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(total, mode="out", weights = E(total)$total, normalized = T)), 
             sd(closeness(total, mode="out", weights = E(total)$total, normalized = T))
             )
       )
```

![](33_TOTAL_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(total_totalcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.006424 0.010200 0.011230 0.011230 0.012230 0.017280
```

```r
sd(total_totalcloseness_n)
```

```
## [1] 0.002001964
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(total)$allcloseness_n<-closeness(total, weights = E(total)$total, mode="all", normalized = T)

#Get Variable
V(total)$total_color_degree<-round(V(total)$allcloseness_n,6)

#Creating brewer pallette
vertex_total_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(total)$total_color_degree)), "RdBu"))(
            length(unique(V(total)$total_color_degree)))

#Saving as Vertex properties 
V(total)$vertex_total_color_degree<-
  vertex_total_color_degree[as.numeric(
  cut(V(total)$total_color_degree,
      breaks=length(unique(V(total)$total_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(total, es=E(total), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(total))
maxC <- rep(Inf, vcount(total))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(total, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(total)$weight)


#PLotting
plot(total, 
     layout=co,
     edge.color=V(total)$vertex_total_color_degree[edge.start],
     edge.arrow.size=closeness(total, weights = E(total)$total, mode="all",normalized = T),
     edge.width=E(total)$weight/10*mean(E(total)$weight),
     edge.curved = TRUE,
     vertex.color=V(total)$vertex_total_color_degree,
     vertex.size=(closeness(total, weights = E(total)$total, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(total,"LABEL_COR"),
     vertex.label.cex=closeness(total, weights = E(total)$total, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(total)$total_color_degree
b<-V(total)$vertex_total_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 33_TOTAL", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(total, mode="all", weights = E(total)$total, normalized = T)), 
             sd(closeness(total, mode="all", weights = E(total)$total, normalized = T))
             )
       )
```

![](33_TOTAL_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(total)$incloseness_n <- closeness(total, weights = E(total)$total, mode = "in", normalized = T) %>% round(6)
V(total)$outcloseness_n <- closeness(total, weights = E(total)$total, mode = "out", normalized = T) %>% round(6)
V(total)$totalcloseness_n <- closeness(total, weights = E(total)$total, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(total)$total_centr_closeness<- centralization.closeness(total)$res
total_centr_closeness<- centralization.closeness(total)$res
total_centr_closeness_all<- centralization.closeness(total)
```

###Centralization

```r
total_centr_closeness_all$centralization
```

```
## [1] 0.15777
```

###Theoretical Max

```r
total_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(total)$total_centr_closeness<- centralization.closeness(total)$res

#Get Variable
V(total)$total_color_degree<-round(V(total)$total_centr_closeness,6)

#Creating brewer pallette
vertex_total_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(total)$total_color_degree)), "Spectral"))(
            length(unique(V(total)$total_color_degree)))

#Saving as Vertex properties 
V(total)$vertex_total_color_degree<-
  vertex_total_color_degree[as.numeric(
  cut(V(total)$total_color_degree,
      breaks=length(unique(V(total)$total_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(total, es=E(total), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(total))
maxC <- rep(Inf, vcount(total))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(total, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(total)$weight)


#PLotting
plot(total, 
     layout=co,
     edge.color=V(total)$vertex_total_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(total)$res,
     edge.width=E(total)$weight/10*mean(E(total)$weight),
     edge.curved = TRUE,
     vertex.color=V(total)$vertex_total_color_degree,
     vertex.size=centralization.closeness(total)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(total,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(total)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(total)$total_color_degree
b<-V(total)$vertex_total_color_degree
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
  title("Network Centralization Closeness - 33_TOTAL", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(total)$res), 
             sd(centralization.closeness(total)$res)
             )
       )
```

![](33_TOTAL_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
total_incloseness<- closeness(total, weights = E(total)$total, mode = "in") %>% round(6)
total_outcloseness<- closeness(total, weights = E(total)$total, mode = "out") %>% round(6)
total_totalcloseness<- closeness(total, weights = E(total)$total, mode = "total") %>% round(6)
total_incloseness_n<- closeness(total,weights = E(total)$total, mode = "in", normalized = T) %>% round(6)
total_outcloseness_n<- closeness(total,weights = E(total)$total, mode = "out", normalized = T) %>% round(6)
total_totalcloseness_n<- closeness(total,weights = E(total)$total, mode = "total", normalized = T) %>% round(6)
total_centr_closeness <- centralization.closeness(total)$res %>% round(6)
```

##Creating a datagrame of measures

```r
total_df_closseness <- data.frame(
total_incloseness,
total_outcloseness,
total_totalcloseness,
total_incloseness_n,
total_outcloseness_n,
total_totalcloseness_n,
total_centr_closeness) %>% round(6)

#Adding type
total_df_closseness <-cbind(total_df_closseness, V(total)$LABEL_COR)

#Adding names
names(total_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
total_df_closseness<-total_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(total_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-de33cdc6668e33c80378" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-de33cdc6668e33c80378">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"6.5e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"7.6e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.5e-05\" data-max=\"9.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005344\" data-max=\"0.012162\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.014065\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.006424\" data-max=\"0.017281\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[5.9e-05,5.3e-05,6.5e-05,5e-05,5.1e-05,5.1e-05,4.6e-05,5.2e-05,4.1e-05,5.2e-05,5.1e-05,5.2e-05,3.9e-05,5.1e-05,4.7e-05,4.7e-05,5.2e-05,5.5e-05,4e-05,4.8e-05,5e-05,4.2e-05,5e-05,3.3e-05,5.2e-05,4.7e-05,3.6e-05,5e-05,5e-05,5e-05,5.1e-05,4.7e-05,5.2e-05,3.6e-05,5e-05,4.3e-05,5.1e-05,5e-05,4e-05,5e-05,5.1e-05,4.7e-05,4.6e-05,5.2e-05,4.7e-05,5.1e-05,3e-05,4.2e-05,4.2e-05,4.2e-05,4.2e-05,4.2e-05,3.5e-05,4.6e-05,3.7e-05,5e-05,4.9e-05,4.2e-05,3.7e-05,5.3e-05,5e-05,3.3e-05,4.8e-05,4.7e-05,4.3e-05,2.9e-05,2.9e-05,4.8e-05,5.7e-05,4.7e-05,4.8e-05,4.8e-05,4.9e-05,4.9e-05,4.9e-05,4.8e-05,4.8e-05,5.2e-05,3.3e-05,3.3e-05,3.3e-05,3.3e-05,4.8e-05,4.3e-05,4.4e-05,4.9e-05,4.7e-05,3e-05,4.8e-05,4.7e-05,4.8e-05,3.7e-05,3.5e-05,4.6e-05,4.9e-05,4.3e-05,3e-05,4.1e-05,4.9e-05,5e-05,3.4e-05,4.2e-05,4e-05,4e-05,4.7e-05,4.8e-05,4.8e-05,4.1e-05,4.1e-05,4.8e-05,4.8e-05,4.9e-05,4.8e-05,4.9e-05,4.9e-05,4.8e-05,4.8e-05,4.8e-05,4.8e-05,4.9e-05,4.7e-05,4.9e-05,4.8e-05,4.8e-05,4.8e-05,4.8e-05,4.8e-05,4.8e-05,4.8e-05,4.5e-05,4.8e-05,4.8e-05,4.8e-05,4.8e-05,4.9e-05,5e-05,4.9e-05,4.8e-05,4.9e-05,4.7e-05,4.8e-05,4.8e-05,4.7e-05,4.7e-05,4.4e-05,5.3e-05,4.9e-05,4.9e-05,4.3e-05,4.5e-05,4.8e-05,4.5e-05,4.8e-05,4.5e-05,4.9e-05,3.7e-05,4.5e-05,4.8e-05,4.8e-05,3.1e-05,3.3e-05,3.9e-05,3.9e-05,3.9e-05,3e-05,2.9e-05,3.8e-05,3e-05,3e-05,3.9e-05,3.7e-05,3e-05,3.8e-05,3.8e-05,4e-05,4e-05,3.9e-05,3e-05,3.9e-05,3.5e-05,3.9e-05,3.9e-05,3.9e-05,3.9e-05,3.6e-05,3.3e-05,4.5e-05],[7.6e-05,6.5e-05,7.4e-05,6.4e-05,5.6e-05,5.3e-05,4.9e-05,6.3e-05,5.7e-05,4.9e-05,5e-05,4.8e-05,6.1e-05,5.5e-05,5.7e-05,4.9e-05,6.5e-05,6.2e-05,5e-05,5.3e-05,5.4e-05,5.3e-05,5.6e-05,5.7e-05,5e-05,3.8e-05,4.9e-05,5.3e-05,6.1e-05,5.7e-05,5.9e-05,5.2e-05,6.4e-05,3.7e-05,4.8e-05,5.5e-05,5.3e-05,4.6e-05,5e-05,6.2e-05,6.1e-05,6.2e-05,6.2e-05,5.3e-05,5.6e-05,5.7e-05,4e-05,5e-05,5e-05,5e-05,5e-05,4.9e-05,4.9e-05,4.6e-05,5.9e-05,6.2e-05,5.2e-05,4.9e-05,4.2e-05,5.5e-05,2.9e-05,3.5e-05,5.8e-05,5.1e-05,5.5e-05,2.9e-05,2.9e-05,5.6e-05,4.8e-05,4.9e-05,4.7e-05,4.8e-05,5.5e-05,5e-05,4.6e-05,4.9e-05,5e-05,5.7e-05,3.5e-05,3.4e-05,3.4e-05,3.4e-05,5.1e-05,3.7e-05,5.1e-05,5.3e-05,5.3e-05,4.7e-05,5.4e-05,4.6e-05,5.3e-05,5.4e-05,5.1e-05,4e-05,5.5e-05,5.2e-05,3.2e-05,3.6e-05,4.6e-05,6.1e-05,3.6e-05,5.6e-05,4.9e-05,4.9e-05,5.2e-05,5.4e-05,4.8e-05,4.7e-05,4.7e-05,5.2e-05,5.6e-05,5.2e-05,5.1e-05,5.3e-05,5.1e-05,5.9e-05,5.3e-05,5.2e-05,5.2e-05,5.4e-05,5.6e-05,5.9e-05,4.4e-05,4.4e-05,4.4e-05,4.7e-05,4.4e-05,4.7e-05,4.4e-05,4.6e-05,5.2e-05,5e-05,5e-05,4.6e-05,4.7e-05,4.9e-05,4.2e-05,4.4e-05,4.7e-05,5e-05,4.5e-05,4.3e-05,4.7e-05,4.5e-05,4.4e-05,4.2e-05,4.7e-05,3.2e-05,4e-05,4.2e-05,4.2e-05,4.2e-05,4.2e-05,4.2e-05,3.5e-05,3.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[9.3e-05,7.8e-05,9.1e-05,7.5e-05,8.2e-05,8.2e-05,5.8e-05,7.4e-05,6.3e-05,6.5e-05,6.7e-05,6.7e-05,6.6e-05,7.4e-05,6.9e-05,5.9e-05,8.3e-05,7.5e-05,5.9e-05,6.6e-05,6.5e-05,6.1e-05,6.7e-05,6.2e-05,6.7e-05,5.8e-05,5.9e-05,6.6e-05,6.8e-05,6.9e-05,6.8e-05,6e-05,7.5e-05,5.1e-05,6.8e-05,6.3e-05,6.7e-05,7.2e-05,5.6e-05,8.4e-05,6.7e-05,6.9e-05,6.9e-05,6.6e-05,8.2e-05,7e-05,5.6e-05,5.8e-05,5.8e-05,5.8e-05,5.8e-05,5.8e-05,5.4e-05,6.2e-05,6.6e-05,7.4e-05,6.1e-05,6.1e-05,5.2e-05,7.1e-05,6.2e-05,4.4e-05,6.6e-05,6.7e-05,6.2e-05,3.5e-05,4.1e-05,6.2e-05,8.5e-05,5.9e-05,6.1e-05,5.9e-05,6.5e-05,6.3e-05,6.2e-05,6.1e-05,6.2e-05,6.7e-05,4e-05,3.9e-05,3.9e-05,3.9e-05,6.7e-05,6e-05,6.4e-05,6e-05,6.6e-05,5.3e-05,6.2e-05,5.9e-05,6.2e-05,6e-05,5.9e-05,5.7e-05,8.2e-05,6.4e-05,4e-05,5e-05,6.1e-05,7.1e-05,4.5e-05,8.2e-05,6.5e-05,6.5e-05,6e-05,8.2e-05,8.1e-05,8.1e-05,8.1e-05,6.3e-05,6.3e-05,6.1e-05,6.1e-05,6.3e-05,6.1e-05,6.9e-05,6.1e-05,6e-05,6.1e-05,6.4e-05,6.3e-05,6.9e-05,5.9e-05,5.7e-05,5.9e-05,5.9e-05,5.9e-05,6e-05,6e-05,5.9e-05,5.9e-05,6e-05,6e-05,5.9e-05,6e-05,6.2e-05,6e-05,5.9e-05,6.2e-05,6e-05,6e-05,5.9e-05,6e-05,5.9e-05,5e-05,6.3e-05,6e-05,6.2e-05,5.4e-05,5.5e-05,5.7e-05,5.5e-05,5.7e-05,5.5e-05,6.3e-05,5.3e-05,5.7e-05,5.8e-05,5.7e-05,4.5e-05,4.1e-05,5e-05,5.1e-05,5e-05,4.2e-05,4e-05,4.7e-05,4.2e-05,4.2e-05,4.8e-05,4.5e-05,4.2e-05,4.8e-05,4.7e-05,5e-05,5e-05,4.8e-05,4.2e-05,4.8e-05,4.4e-05,5e-05,5e-05,5e-05,5.3e-05,4.5e-05,4e-05,5.1e-05],[0.010975,0.009768,0.012162,0.00933,0.009515,0.009413,0.008547,0.009684,0.007686,0.00969,0.009565,0.009722,0.007284,0.009458,0.008829,0.008832,0.009711,0.010246,0.007399,0.008954,0.009313,0.007834,0.009321,0.006066,0.009699,0.008831,0.006695,0.00935,0.00928,0.009372,0.009487,0.008827,0.00961,0.006712,0.009261,0.007957,0.009428,0.009347,0.007524,0.009281,0.009395,0.008739,0.008645,0.009671,0.008686,0.009452,0.005659,0.007877,0.007877,0.007877,0.007877,0.007878,0.006499,0.008623,0.00688,0.009291,0.009144,0.007735,0.006928,0.00985,0.009228,0.006181,0.008877,0.008832,0.008019,0.005348,0.005344,0.008975,0.010629,0.008748,0.009016,0.009005,0.009023,0.009071,0.009152,0.008852,0.008917,0.009695,0.006075,0.006075,0.006075,0.006075,0.008886,0.008047,0.008166,0.009051,0.008732,0.005601,0.009007,0.008741,0.008863,0.006805,0.006457,0.008603,0.009168,0.007992,0.005601,0.007604,0.0091,0.009343,0.006336,0.007732,0.007525,0.007525,0.008825,0.00886,0.00885,0.007542,0.007641,0.008908,0.008861,0.009165,0.008954,0.009066,0.009053,0.008854,0.008843,0.008845,0.008916,0.009197,0.008829,0.009093,0.009013,0.008938,0.009013,0.00896,0.008947,0.00897,0.009019,0.008358,0.008906,0.008962,0.008989,0.008951,0.009022,0.009268,0.009076,0.008858,0.009028,0.008817,0.008905,0.008844,0.0088,0.008814,0.008178,0.009765,0.009027,0.00913,0.00804,0.008305,0.00891,0.008305,0.00891,0.008305,0.009127,0.006927,0.008396,0.008983,0.008951,0.005764,0.006135,0.0072,0.007268,0.00721,0.005654,0.005366,0.007044,0.005654,0.005654,0.0072,0.006789,0.005654,0.007065,0.007148,0.007464,0.007493,0.00719,0.005654,0.007191,0.006583,0.007193,0.007193,0.00732,0.00729,0.006654,0.006159,0.008342],[0.014065,0.012166,0.013682,0.011959,0.010481,0.009832,0.00917,0.011632,0.010656,0.009092,0.009216,0.008883,0.011324,0.010224,0.010511,0.009093,0.012002,0.011484,0.009258,0.009794,0.010119,0.009931,0.010414,0.01063,0.009317,0.006984,0.009124,0.009883,0.011393,0.010657,0.011049,0.009602,0.01196,0.006861,0.008935,0.010166,0.009833,0.008645,0.009233,0.011608,0.011254,0.01161,0.011604,0.009885,0.010339,0.010663,0.007521,0.009284,0.009284,0.009284,0.009284,0.009154,0.009095,0.008615,0.010896,0.011594,0.009688,0.009128,0.007877,0.010223,0.005348,0.006578,0.01085,0.009572,0.010243,0.005369,0.00539,0.010441,0.008942,0.009082,0.008691,0.008862,0.010308,0.009328,0.008622,0.009167,0.009294,0.010606,0.006439,0.006398,0.006398,0.006398,0.009415,0.006967,0.009548,0.009934,0.009923,0.00877,0.010012,0.00851,0.009842,0.010086,0.009516,0.007418,0.010224,0.009733,0.006044,0.00672,0.008621,0.011339,0.006724,0.010402,0.009104,0.009104,0.009693,0.009977,0.008855,0.008769,0.008769,0.009742,0.010325,0.009671,0.009414,0.009805,0.009525,0.010965,0.009885,0.009695,0.009667,0.010005,0.010455,0.010986,0.008188,0.008188,0.008188,0.008815,0.008161,0.008699,0.008228,0.008589,0.009604,0.009297,0.009258,0.008506,0.008804,0.009153,0.007847,0.008161,0.008804,0.009337,0.008397,0.007967,0.008719,0.008295,0.008226,0.007721,0.008809,0.005962,0.00745,0.007769,0.007769,0.007769,0.007769,0.007769,0.006418,0.007322,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.017281,0.014514,0.016982,0.014037,0.015185,0.015277,0.010703,0.013781,0.011712,0.012106,0.012436,0.012375,0.012218,0.013832,0.012807,0.010974,0.015415,0.014029,0.01104,0.012197,0.012151,0.011331,0.012487,0.01154,0.012549,0.010776,0.010992,0.012272,0.012615,0.012884,0.01271,0.01124,0.013935,0.009402,0.012666,0.011675,0.012514,0.013426,0.010442,0.01562,0.012545,0.012818,0.012853,0.01232,0.015186,0.013109,0.010508,0.01088,0.01088,0.01088,0.01088,0.010794,0.010084,0.011531,0.012241,0.013768,0.011437,0.011282,0.009726,0.013187,0.011458,0.00813,0.012269,0.012447,0.011527,0.006424,0.007563,0.011597,0.015767,0.01097,0.011385,0.011011,0.01217,0.011694,0.011492,0.011372,0.011614,0.012486,0.007381,0.007328,0.007328,0.007328,0.012475,0.011198,0.011818,0.011224,0.012202,0.009854,0.011564,0.010944,0.011548,0.011136,0.011001,0.010638,0.015242,0.011918,0.007447,0.009388,0.01131,0.013238,0.008446,0.015206,0.012105,0.012105,0.011203,0.015186,0.015144,0.015067,0.015067,0.011721,0.011647,0.011413,0.01138,0.011742,0.011288,0.012774,0.011342,0.011231,0.01134,0.011837,0.011807,0.012766,0.01091,0.010636,0.01091,0.011044,0.01089,0.011092,0.011129,0.01099,0.01101,0.011103,0.011107,0.011054,0.011156,0.011506,0.011107,0.010976,0.011541,0.011231,0.011081,0.011066,0.01112,0.010884,0.009357,0.011782,0.011241,0.011462,0.009972,0.010196,0.010557,0.010196,0.010557,0.010196,0.011661,0.009902,0.010572,0.01084,0.010557,0.008461,0.007648,0.009289,0.009439,0.009333,0.007791,0.007514,0.008707,0.007791,0.007791,0.008958,0.008329,0.007791,0.008902,0.008807,0.009381,0.009382,0.008999,0.007791,0.009011,0.008205,0.009222,0.009222,0.009297,0.009825,0.008362,0.007491,0.009467],[0.372,0.343808,0.392405,0.336347,0.30897,0.301948,0.304918,0.319039,0.311558,0.288372,0.294304,0.286154,0.333932,0.322917,0.287481,0.29108,0.353612,0.315254,0.292913,0.286154,0.304419,0.285714,0.296178,0.333333,0.285714,0.240621,0.277198,0.319039,0.333333,0.316327,0.310518,0.2976,0.315789,0.224096,0.230198,0.285714,0.30897,0.268786,0.298555,0.32069,0.334532,0.317406,0.317406,0.293839,0.299035,0.300485,0.23192,0.291994,0.291994,0.291994,0.291994,0.291994,0.282675,0.298555,0.331551,0.32069,0.304419,0.288372,0.261972,0.324607,0.005348,0.228221,0.298555,0.291536,0.299517,0.005376,0.005405,0.307438,0.298555,0.283537,0.283537,0.294304,0.296651,0.296178,0.289269,0.290172,0.288372,0.314721,0.229346,0.229346,0.229346,0.229346,0.287926,0.250674,0.295238,0.299035,0.292453,0.274742,0.304419,0.286154,0.291536,0.302439,0.289269,0.24933,0.2976,0.288372,0.216028,0.223289,0.285714,0.307438,0.234552,0.318493,0.223022,0.223022,0.290625,0.303426,0.290172,0.231056,0.231056,0.297125,0.2976,0.28972,0.287037,0.285714,0.287037,0.291994,0.29477,0.295238,0.290172,0.290172,0.295238,0.311037,0.283537,0.283537,0.283537,0.291994,0.282675,0.285714,0.282675,0.291994,0.291536,0.291536,0.292453,0.282675,0.282675,0.298555,0.284839,0.282675,0.282675,0.291536,0.283105,0.282675,0.286154,0.282675,0.282675,0.282675,0.291536,0.221165,0.252374,0.256552,0.256552,0.256552,0.256552,0.256552,0.226553,0.227106,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(total_df_closseness, by=list(total_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(total_df_closseness, by=list(total_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-6e5a92c0e2a46f04af1e" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-6e5a92c0e2a46f04af1e">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.7e-05\" data-max=\"6.5e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"8e-06\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"7.6e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.9e-05\" data-max=\"9.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.006818\" data-max=\"0.012162\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000136\" data-max=\"0.001428\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.014065\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000229\" data-max=\"0.002426\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.009202\" data-max=\"0.017281\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000116\" data-max=\"0.002523\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003897\" data-max=\"0.126\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[4.9e-05,3.7e-05,4.8e-05,5.3e-05,4.6e-05,5.3e-05,5.1e-05,6.5e-05,5e-05,4.9e-05,5.2e-05,4.1e-05,4.9e-05,5.1e-05,5.1e-05,4.6e-05,4.5e-05,5e-05,5.9e-05,4.2e-05,5.5e-05,4.7e-05,4.8e-05],[5e-06,5e-06,null,null,null,null,2e-06,null,null,null,null,8e-06,3e-06,1e-06,1e-06,6e-06,null,null,null,3e-06,null,1e-06,1e-06],[5.5e-05,3.4e-05,5.3e-05,6.5e-05,4.9e-05,5.5e-05,5e-05,7.4e-05,5.6e-05,4.7e-05,5.7e-05,4.9e-05,5.8e-05,5.6e-05,5.8e-05,5.3e-05,2.9e-05,6.1e-05,7.6e-05,4.9e-05,6.2e-05,4.3e-05,5e-05],[1.3e-05,8e-06,null,null,null,null,7e-06,null,null,null,null,1e-05,5e-06,6e-06,6e-06,8e-06,null,null,null,1e-06,null,7e-06,5e-06],[6.5e-05,4.9e-05,6.6e-05,7.8e-05,5.8e-05,7.1e-05,6.5e-05,9.1e-05,6.7e-05,6e-05,6.7e-05,5.8e-05,7.2e-05,7e-05,8.2e-05,6.9e-05,5.1e-05,7.1e-05,9.3e-05,6.4e-05,7.5e-05,5.8e-05,6.3e-05],[1.1e-05,8e-06,null,null,null,null,2e-06,null,null,null,null,1e-05,4e-06,3e-06,1e-06,1.4e-05,null,null,null,9e-06,null,2e-06,5e-06],[0.009015,0.006818,0.008954,0.009768,0.008547,0.00985,0.009476,0.012162,0.009321,0.009027,0.009695,0.007677,0.009098,0.009418,0.009546,0.008605,0.008342,0.009343,0.010975,0.007943,0.010246,0.008806,0.008939],[0.000851,0.000936,null,null,null,null,0.000251,null,null,null,null,0.001428,0.000508,0.000154,0.000151,0.00117,null,null,null,0.000569,null,0.000296,0.000136],[0.010221,0.006307,0.009794,0.012166,0.00917,0.010223,0.009241,0.013682,0.010414,0.008809,0.010606,0.009049,0.010917,0.010483,0.010772,0.009804,0.005348,0.011339,0.014065,0.009084,0.011484,0.007922,0.009291],[0.002426,0.001586,null,null,null,null,0.001322,null,null,null,null,0.001787,0.00098,0.001182,0.001114,0.00149,null,null,null,0.000229,null,0.001291,0.000889],[0.012151,0.009202,0.012197,0.014514,0.010703,0.013187,0.012068,0.016982,0.012487,0.011241,0.012486,0.010856,0.013325,0.012956,0.015292,0.012762,0.009467,0.013238,0.017281,0.011958,0.014029,0.010779,0.011722],[0.002011,0.001428,null,null,null,null,0.000255,null,null,null,null,0.001892,0.000717,0.000584,0.000116,0.002523,null,null,null,0.001673,null,0.000348,0.001014],[0.300898,0.099209,0.286154,0.343808,0.304918,0.324607,0.302044,0.392405,0.296178,0.291536,0.314721,0.267179,0.320162,0.303151,0.32151,0.297137,0.005348,0.307438,0.372,0.268879,0.315254,0.234593,0.28964],[0.043056,0.126,null,null,null,null,0.018298,null,null,null,null,0.070925,0.003897,0.032103,0.028022,0.024497,null,null,null,0.031783,null,0.102905,0.011874]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
total_df_closseness <- data.frame(
total_incloseness,
total_outcloseness,
total_totalcloseness,
total_incloseness_n,
total_outcloseness_n,
total_totalcloseness_n,
total_centr_closeness) %>% round(6)

#Adding type
total_df_closseness <-cbind(total_df_closseness, V(total)$TIPO1)

#Adding names
names(total_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
total_df_closseness<-total_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(total_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-fdbc4d2dc21b1aad0c46" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-fdbc4d2dc21b1aad0c46">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"6.5e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"7.6e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.5e-05\" data-max=\"9.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005344\" data-max=\"0.012162\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.014065\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.006424\" data-max=\"0.017281\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[5.9e-05,5.3e-05,6.5e-05,5e-05,5.1e-05,5.1e-05,4.6e-05,5.2e-05,4.1e-05,5.2e-05,5.1e-05,5.2e-05,3.9e-05,5.1e-05,4.7e-05,4.7e-05,5.2e-05,5.5e-05,4e-05,4.8e-05,5e-05,4.2e-05,5e-05,3.3e-05,5.2e-05,4.7e-05,3.6e-05,5e-05,5e-05,5e-05,5.1e-05,4.7e-05,5.2e-05,3.6e-05,5e-05,4.3e-05,5.1e-05,5e-05,4e-05,5e-05,5.1e-05,4.7e-05,4.6e-05,5.2e-05,4.7e-05,5.1e-05,3e-05,4.2e-05,4.2e-05,4.2e-05,4.2e-05,4.2e-05,3.5e-05,4.6e-05,3.7e-05,5e-05,4.9e-05,4.2e-05,3.7e-05,5.3e-05,5e-05,3.3e-05,4.8e-05,4.7e-05,4.3e-05,2.9e-05,2.9e-05,4.8e-05,5.7e-05,4.7e-05,4.8e-05,4.8e-05,4.9e-05,4.9e-05,4.9e-05,4.8e-05,4.8e-05,5.2e-05,3.3e-05,3.3e-05,3.3e-05,3.3e-05,4.8e-05,4.3e-05,4.4e-05,4.9e-05,4.7e-05,3e-05,4.8e-05,4.7e-05,4.8e-05,3.7e-05,3.5e-05,4.6e-05,4.9e-05,4.3e-05,3e-05,4.1e-05,4.9e-05,5e-05,3.4e-05,4.2e-05,4e-05,4e-05,4.7e-05,4.8e-05,4.8e-05,4.1e-05,4.1e-05,4.8e-05,4.8e-05,4.9e-05,4.8e-05,4.9e-05,4.9e-05,4.8e-05,4.8e-05,4.8e-05,4.8e-05,4.9e-05,4.7e-05,4.9e-05,4.8e-05,4.8e-05,4.8e-05,4.8e-05,4.8e-05,4.8e-05,4.8e-05,4.5e-05,4.8e-05,4.8e-05,4.8e-05,4.8e-05,4.9e-05,5e-05,4.9e-05,4.8e-05,4.9e-05,4.7e-05,4.8e-05,4.8e-05,4.7e-05,4.7e-05,4.4e-05,5.3e-05,4.9e-05,4.9e-05,4.3e-05,4.5e-05,4.8e-05,4.5e-05,4.8e-05,4.5e-05,4.9e-05,3.7e-05,4.5e-05,4.8e-05,4.8e-05,3.1e-05,3.3e-05,3.9e-05,3.9e-05,3.9e-05,3e-05,2.9e-05,3.8e-05,3e-05,3e-05,3.9e-05,3.7e-05,3e-05,3.8e-05,3.8e-05,4e-05,4e-05,3.9e-05,3e-05,3.9e-05,3.5e-05,3.9e-05,3.9e-05,3.9e-05,3.9e-05,3.6e-05,3.3e-05,4.5e-05],[7.6e-05,6.5e-05,7.4e-05,6.4e-05,5.6e-05,5.3e-05,4.9e-05,6.3e-05,5.7e-05,4.9e-05,5e-05,4.8e-05,6.1e-05,5.5e-05,5.7e-05,4.9e-05,6.5e-05,6.2e-05,5e-05,5.3e-05,5.4e-05,5.3e-05,5.6e-05,5.7e-05,5e-05,3.8e-05,4.9e-05,5.3e-05,6.1e-05,5.7e-05,5.9e-05,5.2e-05,6.4e-05,3.7e-05,4.8e-05,5.5e-05,5.3e-05,4.6e-05,5e-05,6.2e-05,6.1e-05,6.2e-05,6.2e-05,5.3e-05,5.6e-05,5.7e-05,4e-05,5e-05,5e-05,5e-05,5e-05,4.9e-05,4.9e-05,4.6e-05,5.9e-05,6.2e-05,5.2e-05,4.9e-05,4.2e-05,5.5e-05,2.9e-05,3.5e-05,5.8e-05,5.1e-05,5.5e-05,2.9e-05,2.9e-05,5.6e-05,4.8e-05,4.9e-05,4.7e-05,4.8e-05,5.5e-05,5e-05,4.6e-05,4.9e-05,5e-05,5.7e-05,3.5e-05,3.4e-05,3.4e-05,3.4e-05,5.1e-05,3.7e-05,5.1e-05,5.3e-05,5.3e-05,4.7e-05,5.4e-05,4.6e-05,5.3e-05,5.4e-05,5.1e-05,4e-05,5.5e-05,5.2e-05,3.2e-05,3.6e-05,4.6e-05,6.1e-05,3.6e-05,5.6e-05,4.9e-05,4.9e-05,5.2e-05,5.4e-05,4.8e-05,4.7e-05,4.7e-05,5.2e-05,5.6e-05,5.2e-05,5.1e-05,5.3e-05,5.1e-05,5.9e-05,5.3e-05,5.2e-05,5.2e-05,5.4e-05,5.6e-05,5.9e-05,4.4e-05,4.4e-05,4.4e-05,4.7e-05,4.4e-05,4.7e-05,4.4e-05,4.6e-05,5.2e-05,5e-05,5e-05,4.6e-05,4.7e-05,4.9e-05,4.2e-05,4.4e-05,4.7e-05,5e-05,4.5e-05,4.3e-05,4.7e-05,4.5e-05,4.4e-05,4.2e-05,4.7e-05,3.2e-05,4e-05,4.2e-05,4.2e-05,4.2e-05,4.2e-05,4.2e-05,3.5e-05,3.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[9.3e-05,7.8e-05,9.1e-05,7.5e-05,8.2e-05,8.2e-05,5.8e-05,7.4e-05,6.3e-05,6.5e-05,6.7e-05,6.7e-05,6.6e-05,7.4e-05,6.9e-05,5.9e-05,8.3e-05,7.5e-05,5.9e-05,6.6e-05,6.5e-05,6.1e-05,6.7e-05,6.2e-05,6.7e-05,5.8e-05,5.9e-05,6.6e-05,6.8e-05,6.9e-05,6.8e-05,6e-05,7.5e-05,5.1e-05,6.8e-05,6.3e-05,6.7e-05,7.2e-05,5.6e-05,8.4e-05,6.7e-05,6.9e-05,6.9e-05,6.6e-05,8.2e-05,7e-05,5.6e-05,5.8e-05,5.8e-05,5.8e-05,5.8e-05,5.8e-05,5.4e-05,6.2e-05,6.6e-05,7.4e-05,6.1e-05,6.1e-05,5.2e-05,7.1e-05,6.2e-05,4.4e-05,6.6e-05,6.7e-05,6.2e-05,3.5e-05,4.1e-05,6.2e-05,8.5e-05,5.9e-05,6.1e-05,5.9e-05,6.5e-05,6.3e-05,6.2e-05,6.1e-05,6.2e-05,6.7e-05,4e-05,3.9e-05,3.9e-05,3.9e-05,6.7e-05,6e-05,6.4e-05,6e-05,6.6e-05,5.3e-05,6.2e-05,5.9e-05,6.2e-05,6e-05,5.9e-05,5.7e-05,8.2e-05,6.4e-05,4e-05,5e-05,6.1e-05,7.1e-05,4.5e-05,8.2e-05,6.5e-05,6.5e-05,6e-05,8.2e-05,8.1e-05,8.1e-05,8.1e-05,6.3e-05,6.3e-05,6.1e-05,6.1e-05,6.3e-05,6.1e-05,6.9e-05,6.1e-05,6e-05,6.1e-05,6.4e-05,6.3e-05,6.9e-05,5.9e-05,5.7e-05,5.9e-05,5.9e-05,5.9e-05,6e-05,6e-05,5.9e-05,5.9e-05,6e-05,6e-05,5.9e-05,6e-05,6.2e-05,6e-05,5.9e-05,6.2e-05,6e-05,6e-05,5.9e-05,6e-05,5.9e-05,5e-05,6.3e-05,6e-05,6.2e-05,5.4e-05,5.5e-05,5.7e-05,5.5e-05,5.7e-05,5.5e-05,6.3e-05,5.3e-05,5.7e-05,5.8e-05,5.7e-05,4.5e-05,4.1e-05,5e-05,5.1e-05,5e-05,4.2e-05,4e-05,4.7e-05,4.2e-05,4.2e-05,4.8e-05,4.5e-05,4.2e-05,4.8e-05,4.7e-05,5e-05,5e-05,4.8e-05,4.2e-05,4.8e-05,4.4e-05,5e-05,5e-05,5e-05,5.3e-05,4.5e-05,4e-05,5.1e-05],[0.010975,0.009768,0.012162,0.00933,0.009515,0.009413,0.008547,0.009684,0.007686,0.00969,0.009565,0.009722,0.007284,0.009458,0.008829,0.008832,0.009711,0.010246,0.007399,0.008954,0.009313,0.007834,0.009321,0.006066,0.009699,0.008831,0.006695,0.00935,0.00928,0.009372,0.009487,0.008827,0.00961,0.006712,0.009261,0.007957,0.009428,0.009347,0.007524,0.009281,0.009395,0.008739,0.008645,0.009671,0.008686,0.009452,0.005659,0.007877,0.007877,0.007877,0.007877,0.007878,0.006499,0.008623,0.00688,0.009291,0.009144,0.007735,0.006928,0.00985,0.009228,0.006181,0.008877,0.008832,0.008019,0.005348,0.005344,0.008975,0.010629,0.008748,0.009016,0.009005,0.009023,0.009071,0.009152,0.008852,0.008917,0.009695,0.006075,0.006075,0.006075,0.006075,0.008886,0.008047,0.008166,0.009051,0.008732,0.005601,0.009007,0.008741,0.008863,0.006805,0.006457,0.008603,0.009168,0.007992,0.005601,0.007604,0.0091,0.009343,0.006336,0.007732,0.007525,0.007525,0.008825,0.00886,0.00885,0.007542,0.007641,0.008908,0.008861,0.009165,0.008954,0.009066,0.009053,0.008854,0.008843,0.008845,0.008916,0.009197,0.008829,0.009093,0.009013,0.008938,0.009013,0.00896,0.008947,0.00897,0.009019,0.008358,0.008906,0.008962,0.008989,0.008951,0.009022,0.009268,0.009076,0.008858,0.009028,0.008817,0.008905,0.008844,0.0088,0.008814,0.008178,0.009765,0.009027,0.00913,0.00804,0.008305,0.00891,0.008305,0.00891,0.008305,0.009127,0.006927,0.008396,0.008983,0.008951,0.005764,0.006135,0.0072,0.007268,0.00721,0.005654,0.005366,0.007044,0.005654,0.005654,0.0072,0.006789,0.005654,0.007065,0.007148,0.007464,0.007493,0.00719,0.005654,0.007191,0.006583,0.007193,0.007193,0.00732,0.00729,0.006654,0.006159,0.008342],[0.014065,0.012166,0.013682,0.011959,0.010481,0.009832,0.00917,0.011632,0.010656,0.009092,0.009216,0.008883,0.011324,0.010224,0.010511,0.009093,0.012002,0.011484,0.009258,0.009794,0.010119,0.009931,0.010414,0.01063,0.009317,0.006984,0.009124,0.009883,0.011393,0.010657,0.011049,0.009602,0.01196,0.006861,0.008935,0.010166,0.009833,0.008645,0.009233,0.011608,0.011254,0.01161,0.011604,0.009885,0.010339,0.010663,0.007521,0.009284,0.009284,0.009284,0.009284,0.009154,0.009095,0.008615,0.010896,0.011594,0.009688,0.009128,0.007877,0.010223,0.005348,0.006578,0.01085,0.009572,0.010243,0.005369,0.00539,0.010441,0.008942,0.009082,0.008691,0.008862,0.010308,0.009328,0.008622,0.009167,0.009294,0.010606,0.006439,0.006398,0.006398,0.006398,0.009415,0.006967,0.009548,0.009934,0.009923,0.00877,0.010012,0.00851,0.009842,0.010086,0.009516,0.007418,0.010224,0.009733,0.006044,0.00672,0.008621,0.011339,0.006724,0.010402,0.009104,0.009104,0.009693,0.009977,0.008855,0.008769,0.008769,0.009742,0.010325,0.009671,0.009414,0.009805,0.009525,0.010965,0.009885,0.009695,0.009667,0.010005,0.010455,0.010986,0.008188,0.008188,0.008188,0.008815,0.008161,0.008699,0.008228,0.008589,0.009604,0.009297,0.009258,0.008506,0.008804,0.009153,0.007847,0.008161,0.008804,0.009337,0.008397,0.007967,0.008719,0.008295,0.008226,0.007721,0.008809,0.005962,0.00745,0.007769,0.007769,0.007769,0.007769,0.007769,0.006418,0.007322,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.017281,0.014514,0.016982,0.014037,0.015185,0.015277,0.010703,0.013781,0.011712,0.012106,0.012436,0.012375,0.012218,0.013832,0.012807,0.010974,0.015415,0.014029,0.01104,0.012197,0.012151,0.011331,0.012487,0.01154,0.012549,0.010776,0.010992,0.012272,0.012615,0.012884,0.01271,0.01124,0.013935,0.009402,0.012666,0.011675,0.012514,0.013426,0.010442,0.01562,0.012545,0.012818,0.012853,0.01232,0.015186,0.013109,0.010508,0.01088,0.01088,0.01088,0.01088,0.010794,0.010084,0.011531,0.012241,0.013768,0.011437,0.011282,0.009726,0.013187,0.011458,0.00813,0.012269,0.012447,0.011527,0.006424,0.007563,0.011597,0.015767,0.01097,0.011385,0.011011,0.01217,0.011694,0.011492,0.011372,0.011614,0.012486,0.007381,0.007328,0.007328,0.007328,0.012475,0.011198,0.011818,0.011224,0.012202,0.009854,0.011564,0.010944,0.011548,0.011136,0.011001,0.010638,0.015242,0.011918,0.007447,0.009388,0.01131,0.013238,0.008446,0.015206,0.012105,0.012105,0.011203,0.015186,0.015144,0.015067,0.015067,0.011721,0.011647,0.011413,0.01138,0.011742,0.011288,0.012774,0.011342,0.011231,0.01134,0.011837,0.011807,0.012766,0.01091,0.010636,0.01091,0.011044,0.01089,0.011092,0.011129,0.01099,0.01101,0.011103,0.011107,0.011054,0.011156,0.011506,0.011107,0.010976,0.011541,0.011231,0.011081,0.011066,0.01112,0.010884,0.009357,0.011782,0.011241,0.011462,0.009972,0.010196,0.010557,0.010196,0.010557,0.010196,0.011661,0.009902,0.010572,0.01084,0.010557,0.008461,0.007648,0.009289,0.009439,0.009333,0.007791,0.007514,0.008707,0.007791,0.007791,0.008958,0.008329,0.007791,0.008902,0.008807,0.009381,0.009382,0.008999,0.007791,0.009011,0.008205,0.009222,0.009222,0.009297,0.009825,0.008362,0.007491,0.009467],[0.372,0.343808,0.392405,0.336347,0.30897,0.301948,0.304918,0.319039,0.311558,0.288372,0.294304,0.286154,0.333932,0.322917,0.287481,0.29108,0.353612,0.315254,0.292913,0.286154,0.304419,0.285714,0.296178,0.333333,0.285714,0.240621,0.277198,0.319039,0.333333,0.316327,0.310518,0.2976,0.315789,0.224096,0.230198,0.285714,0.30897,0.268786,0.298555,0.32069,0.334532,0.317406,0.317406,0.293839,0.299035,0.300485,0.23192,0.291994,0.291994,0.291994,0.291994,0.291994,0.282675,0.298555,0.331551,0.32069,0.304419,0.288372,0.261972,0.324607,0.005348,0.228221,0.298555,0.291536,0.299517,0.005376,0.005405,0.307438,0.298555,0.283537,0.283537,0.294304,0.296651,0.296178,0.289269,0.290172,0.288372,0.314721,0.229346,0.229346,0.229346,0.229346,0.287926,0.250674,0.295238,0.299035,0.292453,0.274742,0.304419,0.286154,0.291536,0.302439,0.289269,0.24933,0.2976,0.288372,0.216028,0.223289,0.285714,0.307438,0.234552,0.318493,0.223022,0.223022,0.290625,0.303426,0.290172,0.231056,0.231056,0.297125,0.2976,0.28972,0.287037,0.285714,0.287037,0.291994,0.29477,0.295238,0.290172,0.290172,0.295238,0.311037,0.283537,0.283537,0.283537,0.291994,0.282675,0.285714,0.282675,0.291994,0.291536,0.291536,0.292453,0.282675,0.282675,0.298555,0.284839,0.282675,0.282675,0.291536,0.283105,0.282675,0.286154,0.282675,0.282675,0.282675,0.291536,0.221165,0.252374,0.256552,0.256552,0.256552,0.256552,0.256552,0.226553,0.227106,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(total_df_closseness, by=list(total_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(total_df_closseness, by=list(total_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-adfdecc1485983d06bef" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-adfdecc1485983d06bef">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4e-05\" data-max=\"4.8e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3e-06\" data-max=\"7e-06\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.1e-05\" data-max=\"5e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8e-06\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.5e-05\" data-max=\"6.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8e-06\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.007359\" data-max=\"0.008961\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000629\" data-max=\"0.001302\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.007546\" data-max=\"0.009382\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001485\" data-max=\"0.002176\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.010195\" data-max=\"0.011992\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001484\" data-max=\"0.002156\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.174245\" data-max=\"0.281723\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.060225\" data-max=\"0.135845\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[4.8e-05,4e-05],[3e-06,7e-06],[5e-05,4.1e-05],[8e-06,1.2e-05],[6.4e-05,5.5e-05],[8e-06,1.2e-05],[0.008961,0.007359],[0.000629,0.001302],[0.009382,0.007546],[0.001485,0.002176],[0.011992,0.010195],[0.001484,0.002156],[0.281723,0.174245],[0.060225,0.135845]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
total_df_closseness <- data.frame(
total_incloseness,
total_outcloseness,
total_totalcloseness,
total_incloseness_n,
total_outcloseness_n,
total_totalcloseness_n,
total_centr_closeness) %>% round(6)

#Adding type
total_df_closseness <-cbind(total_df_closseness, V(total)$TIPO2)

#Adding names
names(total_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
total_df_closseness<-total_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(total_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-9e92a1c0a77cfc704044" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-9e92a1c0a77cfc704044">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"6.5e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"7.6e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.5e-05\" data-max=\"9.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005344\" data-max=\"0.012162\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.014065\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.006424\" data-max=\"0.017281\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[5.9e-05,5.3e-05,6.5e-05,5e-05,5.1e-05,5.1e-05,4.6e-05,5.2e-05,4.1e-05,5.2e-05,5.1e-05,5.2e-05,3.9e-05,5.1e-05,4.7e-05,4.7e-05,5.2e-05,5.5e-05,4e-05,4.8e-05,5e-05,4.2e-05,5e-05,3.3e-05,5.2e-05,4.7e-05,3.6e-05,5e-05,5e-05,5e-05,5.1e-05,4.7e-05,5.2e-05,3.6e-05,5e-05,4.3e-05,5.1e-05,5e-05,4e-05,5e-05,5.1e-05,4.7e-05,4.6e-05,5.2e-05,4.7e-05,5.1e-05,3e-05,4.2e-05,4.2e-05,4.2e-05,4.2e-05,4.2e-05,3.5e-05,4.6e-05,3.7e-05,5e-05,4.9e-05,4.2e-05,3.7e-05,5.3e-05,5e-05,3.3e-05,4.8e-05,4.7e-05,4.3e-05,2.9e-05,2.9e-05,4.8e-05,5.7e-05,4.7e-05,4.8e-05,4.8e-05,4.9e-05,4.9e-05,4.9e-05,4.8e-05,4.8e-05,5.2e-05,3.3e-05,3.3e-05,3.3e-05,3.3e-05,4.8e-05,4.3e-05,4.4e-05,4.9e-05,4.7e-05,3e-05,4.8e-05,4.7e-05,4.8e-05,3.7e-05,3.5e-05,4.6e-05,4.9e-05,4.3e-05,3e-05,4.1e-05,4.9e-05,5e-05,3.4e-05,4.2e-05,4e-05,4e-05,4.7e-05,4.8e-05,4.8e-05,4.1e-05,4.1e-05,4.8e-05,4.8e-05,4.9e-05,4.8e-05,4.9e-05,4.9e-05,4.8e-05,4.8e-05,4.8e-05,4.8e-05,4.9e-05,4.7e-05,4.9e-05,4.8e-05,4.8e-05,4.8e-05,4.8e-05,4.8e-05,4.8e-05,4.8e-05,4.5e-05,4.8e-05,4.8e-05,4.8e-05,4.8e-05,4.9e-05,5e-05,4.9e-05,4.8e-05,4.9e-05,4.7e-05,4.8e-05,4.8e-05,4.7e-05,4.7e-05,4.4e-05,5.3e-05,4.9e-05,4.9e-05,4.3e-05,4.5e-05,4.8e-05,4.5e-05,4.8e-05,4.5e-05,4.9e-05,3.7e-05,4.5e-05,4.8e-05,4.8e-05,3.1e-05,3.3e-05,3.9e-05,3.9e-05,3.9e-05,3e-05,2.9e-05,3.8e-05,3e-05,3e-05,3.9e-05,3.7e-05,3e-05,3.8e-05,3.8e-05,4e-05,4e-05,3.9e-05,3e-05,3.9e-05,3.5e-05,3.9e-05,3.9e-05,3.9e-05,3.9e-05,3.6e-05,3.3e-05,4.5e-05],[7.6e-05,6.5e-05,7.4e-05,6.4e-05,5.6e-05,5.3e-05,4.9e-05,6.3e-05,5.7e-05,4.9e-05,5e-05,4.8e-05,6.1e-05,5.5e-05,5.7e-05,4.9e-05,6.5e-05,6.2e-05,5e-05,5.3e-05,5.4e-05,5.3e-05,5.6e-05,5.7e-05,5e-05,3.8e-05,4.9e-05,5.3e-05,6.1e-05,5.7e-05,5.9e-05,5.2e-05,6.4e-05,3.7e-05,4.8e-05,5.5e-05,5.3e-05,4.6e-05,5e-05,6.2e-05,6.1e-05,6.2e-05,6.2e-05,5.3e-05,5.6e-05,5.7e-05,4e-05,5e-05,5e-05,5e-05,5e-05,4.9e-05,4.9e-05,4.6e-05,5.9e-05,6.2e-05,5.2e-05,4.9e-05,4.2e-05,5.5e-05,2.9e-05,3.5e-05,5.8e-05,5.1e-05,5.5e-05,2.9e-05,2.9e-05,5.6e-05,4.8e-05,4.9e-05,4.7e-05,4.8e-05,5.5e-05,5e-05,4.6e-05,4.9e-05,5e-05,5.7e-05,3.5e-05,3.4e-05,3.4e-05,3.4e-05,5.1e-05,3.7e-05,5.1e-05,5.3e-05,5.3e-05,4.7e-05,5.4e-05,4.6e-05,5.3e-05,5.4e-05,5.1e-05,4e-05,5.5e-05,5.2e-05,3.2e-05,3.6e-05,4.6e-05,6.1e-05,3.6e-05,5.6e-05,4.9e-05,4.9e-05,5.2e-05,5.4e-05,4.8e-05,4.7e-05,4.7e-05,5.2e-05,5.6e-05,5.2e-05,5.1e-05,5.3e-05,5.1e-05,5.9e-05,5.3e-05,5.2e-05,5.2e-05,5.4e-05,5.6e-05,5.9e-05,4.4e-05,4.4e-05,4.4e-05,4.7e-05,4.4e-05,4.7e-05,4.4e-05,4.6e-05,5.2e-05,5e-05,5e-05,4.6e-05,4.7e-05,4.9e-05,4.2e-05,4.4e-05,4.7e-05,5e-05,4.5e-05,4.3e-05,4.7e-05,4.5e-05,4.4e-05,4.2e-05,4.7e-05,3.2e-05,4e-05,4.2e-05,4.2e-05,4.2e-05,4.2e-05,4.2e-05,3.5e-05,3.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[9.3e-05,7.8e-05,9.1e-05,7.5e-05,8.2e-05,8.2e-05,5.8e-05,7.4e-05,6.3e-05,6.5e-05,6.7e-05,6.7e-05,6.6e-05,7.4e-05,6.9e-05,5.9e-05,8.3e-05,7.5e-05,5.9e-05,6.6e-05,6.5e-05,6.1e-05,6.7e-05,6.2e-05,6.7e-05,5.8e-05,5.9e-05,6.6e-05,6.8e-05,6.9e-05,6.8e-05,6e-05,7.5e-05,5.1e-05,6.8e-05,6.3e-05,6.7e-05,7.2e-05,5.6e-05,8.4e-05,6.7e-05,6.9e-05,6.9e-05,6.6e-05,8.2e-05,7e-05,5.6e-05,5.8e-05,5.8e-05,5.8e-05,5.8e-05,5.8e-05,5.4e-05,6.2e-05,6.6e-05,7.4e-05,6.1e-05,6.1e-05,5.2e-05,7.1e-05,6.2e-05,4.4e-05,6.6e-05,6.7e-05,6.2e-05,3.5e-05,4.1e-05,6.2e-05,8.5e-05,5.9e-05,6.1e-05,5.9e-05,6.5e-05,6.3e-05,6.2e-05,6.1e-05,6.2e-05,6.7e-05,4e-05,3.9e-05,3.9e-05,3.9e-05,6.7e-05,6e-05,6.4e-05,6e-05,6.6e-05,5.3e-05,6.2e-05,5.9e-05,6.2e-05,6e-05,5.9e-05,5.7e-05,8.2e-05,6.4e-05,4e-05,5e-05,6.1e-05,7.1e-05,4.5e-05,8.2e-05,6.5e-05,6.5e-05,6e-05,8.2e-05,8.1e-05,8.1e-05,8.1e-05,6.3e-05,6.3e-05,6.1e-05,6.1e-05,6.3e-05,6.1e-05,6.9e-05,6.1e-05,6e-05,6.1e-05,6.4e-05,6.3e-05,6.9e-05,5.9e-05,5.7e-05,5.9e-05,5.9e-05,5.9e-05,6e-05,6e-05,5.9e-05,5.9e-05,6e-05,6e-05,5.9e-05,6e-05,6.2e-05,6e-05,5.9e-05,6.2e-05,6e-05,6e-05,5.9e-05,6e-05,5.9e-05,5e-05,6.3e-05,6e-05,6.2e-05,5.4e-05,5.5e-05,5.7e-05,5.5e-05,5.7e-05,5.5e-05,6.3e-05,5.3e-05,5.7e-05,5.8e-05,5.7e-05,4.5e-05,4.1e-05,5e-05,5.1e-05,5e-05,4.2e-05,4e-05,4.7e-05,4.2e-05,4.2e-05,4.8e-05,4.5e-05,4.2e-05,4.8e-05,4.7e-05,5e-05,5e-05,4.8e-05,4.2e-05,4.8e-05,4.4e-05,5e-05,5e-05,5e-05,5.3e-05,4.5e-05,4e-05,5.1e-05],[0.010975,0.009768,0.012162,0.00933,0.009515,0.009413,0.008547,0.009684,0.007686,0.00969,0.009565,0.009722,0.007284,0.009458,0.008829,0.008832,0.009711,0.010246,0.007399,0.008954,0.009313,0.007834,0.009321,0.006066,0.009699,0.008831,0.006695,0.00935,0.00928,0.009372,0.009487,0.008827,0.00961,0.006712,0.009261,0.007957,0.009428,0.009347,0.007524,0.009281,0.009395,0.008739,0.008645,0.009671,0.008686,0.009452,0.005659,0.007877,0.007877,0.007877,0.007877,0.007878,0.006499,0.008623,0.00688,0.009291,0.009144,0.007735,0.006928,0.00985,0.009228,0.006181,0.008877,0.008832,0.008019,0.005348,0.005344,0.008975,0.010629,0.008748,0.009016,0.009005,0.009023,0.009071,0.009152,0.008852,0.008917,0.009695,0.006075,0.006075,0.006075,0.006075,0.008886,0.008047,0.008166,0.009051,0.008732,0.005601,0.009007,0.008741,0.008863,0.006805,0.006457,0.008603,0.009168,0.007992,0.005601,0.007604,0.0091,0.009343,0.006336,0.007732,0.007525,0.007525,0.008825,0.00886,0.00885,0.007542,0.007641,0.008908,0.008861,0.009165,0.008954,0.009066,0.009053,0.008854,0.008843,0.008845,0.008916,0.009197,0.008829,0.009093,0.009013,0.008938,0.009013,0.00896,0.008947,0.00897,0.009019,0.008358,0.008906,0.008962,0.008989,0.008951,0.009022,0.009268,0.009076,0.008858,0.009028,0.008817,0.008905,0.008844,0.0088,0.008814,0.008178,0.009765,0.009027,0.00913,0.00804,0.008305,0.00891,0.008305,0.00891,0.008305,0.009127,0.006927,0.008396,0.008983,0.008951,0.005764,0.006135,0.0072,0.007268,0.00721,0.005654,0.005366,0.007044,0.005654,0.005654,0.0072,0.006789,0.005654,0.007065,0.007148,0.007464,0.007493,0.00719,0.005654,0.007191,0.006583,0.007193,0.007193,0.00732,0.00729,0.006654,0.006159,0.008342],[0.014065,0.012166,0.013682,0.011959,0.010481,0.009832,0.00917,0.011632,0.010656,0.009092,0.009216,0.008883,0.011324,0.010224,0.010511,0.009093,0.012002,0.011484,0.009258,0.009794,0.010119,0.009931,0.010414,0.01063,0.009317,0.006984,0.009124,0.009883,0.011393,0.010657,0.011049,0.009602,0.01196,0.006861,0.008935,0.010166,0.009833,0.008645,0.009233,0.011608,0.011254,0.01161,0.011604,0.009885,0.010339,0.010663,0.007521,0.009284,0.009284,0.009284,0.009284,0.009154,0.009095,0.008615,0.010896,0.011594,0.009688,0.009128,0.007877,0.010223,0.005348,0.006578,0.01085,0.009572,0.010243,0.005369,0.00539,0.010441,0.008942,0.009082,0.008691,0.008862,0.010308,0.009328,0.008622,0.009167,0.009294,0.010606,0.006439,0.006398,0.006398,0.006398,0.009415,0.006967,0.009548,0.009934,0.009923,0.00877,0.010012,0.00851,0.009842,0.010086,0.009516,0.007418,0.010224,0.009733,0.006044,0.00672,0.008621,0.011339,0.006724,0.010402,0.009104,0.009104,0.009693,0.009977,0.008855,0.008769,0.008769,0.009742,0.010325,0.009671,0.009414,0.009805,0.009525,0.010965,0.009885,0.009695,0.009667,0.010005,0.010455,0.010986,0.008188,0.008188,0.008188,0.008815,0.008161,0.008699,0.008228,0.008589,0.009604,0.009297,0.009258,0.008506,0.008804,0.009153,0.007847,0.008161,0.008804,0.009337,0.008397,0.007967,0.008719,0.008295,0.008226,0.007721,0.008809,0.005962,0.00745,0.007769,0.007769,0.007769,0.007769,0.007769,0.006418,0.007322,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.017281,0.014514,0.016982,0.014037,0.015185,0.015277,0.010703,0.013781,0.011712,0.012106,0.012436,0.012375,0.012218,0.013832,0.012807,0.010974,0.015415,0.014029,0.01104,0.012197,0.012151,0.011331,0.012487,0.01154,0.012549,0.010776,0.010992,0.012272,0.012615,0.012884,0.01271,0.01124,0.013935,0.009402,0.012666,0.011675,0.012514,0.013426,0.010442,0.01562,0.012545,0.012818,0.012853,0.01232,0.015186,0.013109,0.010508,0.01088,0.01088,0.01088,0.01088,0.010794,0.010084,0.011531,0.012241,0.013768,0.011437,0.011282,0.009726,0.013187,0.011458,0.00813,0.012269,0.012447,0.011527,0.006424,0.007563,0.011597,0.015767,0.01097,0.011385,0.011011,0.01217,0.011694,0.011492,0.011372,0.011614,0.012486,0.007381,0.007328,0.007328,0.007328,0.012475,0.011198,0.011818,0.011224,0.012202,0.009854,0.011564,0.010944,0.011548,0.011136,0.011001,0.010638,0.015242,0.011918,0.007447,0.009388,0.01131,0.013238,0.008446,0.015206,0.012105,0.012105,0.011203,0.015186,0.015144,0.015067,0.015067,0.011721,0.011647,0.011413,0.01138,0.011742,0.011288,0.012774,0.011342,0.011231,0.01134,0.011837,0.011807,0.012766,0.01091,0.010636,0.01091,0.011044,0.01089,0.011092,0.011129,0.01099,0.01101,0.011103,0.011107,0.011054,0.011156,0.011506,0.011107,0.010976,0.011541,0.011231,0.011081,0.011066,0.01112,0.010884,0.009357,0.011782,0.011241,0.011462,0.009972,0.010196,0.010557,0.010196,0.010557,0.010196,0.011661,0.009902,0.010572,0.01084,0.010557,0.008461,0.007648,0.009289,0.009439,0.009333,0.007791,0.007514,0.008707,0.007791,0.007791,0.008958,0.008329,0.007791,0.008902,0.008807,0.009381,0.009382,0.008999,0.007791,0.009011,0.008205,0.009222,0.009222,0.009297,0.009825,0.008362,0.007491,0.009467],[0.372,0.343808,0.392405,0.336347,0.30897,0.301948,0.304918,0.319039,0.311558,0.288372,0.294304,0.286154,0.333932,0.322917,0.287481,0.29108,0.353612,0.315254,0.292913,0.286154,0.304419,0.285714,0.296178,0.333333,0.285714,0.240621,0.277198,0.319039,0.333333,0.316327,0.310518,0.2976,0.315789,0.224096,0.230198,0.285714,0.30897,0.268786,0.298555,0.32069,0.334532,0.317406,0.317406,0.293839,0.299035,0.300485,0.23192,0.291994,0.291994,0.291994,0.291994,0.291994,0.282675,0.298555,0.331551,0.32069,0.304419,0.288372,0.261972,0.324607,0.005348,0.228221,0.298555,0.291536,0.299517,0.005376,0.005405,0.307438,0.298555,0.283537,0.283537,0.294304,0.296651,0.296178,0.289269,0.290172,0.288372,0.314721,0.229346,0.229346,0.229346,0.229346,0.287926,0.250674,0.295238,0.299035,0.292453,0.274742,0.304419,0.286154,0.291536,0.302439,0.289269,0.24933,0.2976,0.288372,0.216028,0.223289,0.285714,0.307438,0.234552,0.318493,0.223022,0.223022,0.290625,0.303426,0.290172,0.231056,0.231056,0.297125,0.2976,0.28972,0.287037,0.285714,0.287037,0.291994,0.29477,0.295238,0.290172,0.290172,0.295238,0.311037,0.283537,0.283537,0.283537,0.291994,0.282675,0.285714,0.282675,0.291994,0.291536,0.291536,0.292453,0.282675,0.282675,0.298555,0.284839,0.282675,0.282675,0.291536,0.283105,0.282675,0.286154,0.282675,0.282675,0.282675,0.291536,0.221165,0.252374,0.256552,0.256552,0.256552,0.256552,0.256552,0.226553,0.227106,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(total_df_closseness, by=list(total_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(total_df_closseness, by=list(total_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-33c55ab9f8e4afab782c" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-33c55ab9f8e4afab782c">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.9e-05\" data-max=\"5.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"7e-06\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.1e-05\" data-max=\"5.7e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6e-06\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.5e-05\" data-max=\"7.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6e-06\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.007329\" data-max=\"0.009473\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000159\" data-max=\"0.001285\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.007533\" data-max=\"0.010699\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001069\" data-max=\"0.002191\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.010177\" data-max=\"0.01343\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001106\" data-max=\"0.00216\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172869\" data-max=\"0.310529\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.028396\" data-max=\"0.136194\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[5.1e-05,4.8e-05,3.9e-05],[1e-06,3e-06,7e-06],[5.7e-05,4.9e-05,4.1e-05],[6e-06,8e-06,1.2e-05],[7.2e-05,6.3e-05,5.5e-05],[6e-06,8e-06,1.2e-05],[0.009473,0.008888,0.007329],[0.000159,0.000636,0.001285],[0.010699,0.009163,0.007533],[0.001069,0.001424,0.002191],[0.01343,0.011759,0.010177],[0.001106,0.001405,0.00216],[0.310529,0.277125,0.172869],[0.028396,0.062378,0.136194]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/total_data.RData")
```

