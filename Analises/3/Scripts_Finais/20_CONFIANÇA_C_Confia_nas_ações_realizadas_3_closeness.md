# SNA Closeness 20_CONFIANÇA C) Confia nas ações realizadas por este serviço. (var18)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 20_CONFIANÇA C) Confia nas ações realizadas por este serviço. (var18)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var18_data.RData")
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
#var18<-simplify(var18) #Simplify
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
V(var18)$incloseness <- closeness(var18, mode = "in", weights = E(var18)$var18) %>% round(6)
V(var18)$outcloseness <- closeness(var18, mode = "out", weights = E(var18)$var18) %>% round(6)
V(var18)$totalcloseness <- closeness(var18, mode = "total", weights = E(var18)$var18) %>% round(4)
```

###Saving to Environment

```r
var18_incloseness<- closeness(var18, mode = "in", weights = E(var18)$var18) %>% round(6)
var18_outcloseness<- closeness(var18, mode = "out", weights = E(var18)$var18) %>% round(6)
var18_totalcloseness<- closeness(var18, mode = "total", weights = E(var18)$var18) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var18_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0001240 0.0001270 0.0001248 0.0001270 0.0001320
```

```r
sd(var18_incloseness)
```

```
## [1] 7.780749e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var18)$incloseness<-closeness(var18, weights = E(var18)$var18, mode="in")

#Get Variable
V(var18)$var18_color_degree<-round(V(var18)$incloseness,6)

#Creating brewer pallette
vertex_var18_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var18)$var18_color_degree)), "RdBu"))(
            length(unique(V(var18)$var18_color_degree)))

#Saving as Vertex properties 
V(var18)$vertex_var18_color_degree<-
  vertex_var18_color_degree[as.numeric(
  cut(V(var18)$var18_color_degree,
      breaks=length(unique(V(var18)$var18_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var18, es=E(var18), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var18))
maxC <- rep(Inf, vcount(var18))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var18, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var18)$weight)


#PLotting
plot(var18, 
     layout=co,
     edge.color=V(var18)$vertex_var18_color_degree[edge.start],
     edge.arrow.size=closeness(var18, weights = E(var18)$var18, mode="in"),
     edge.width=E(var18)$weight/mean(E(var18)$weight),
     edge.curved = TRUE,
     vertex.color=V(var18)$vertex_var18_color_degree,
     vertex.size=closeness(var18, weights = E(var18)$var18, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var18,"LABEL_COR"),
     vertex.label.cex=(closeness(var18, weights = E(var18)$var18, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var18)$var18_color_degree
b<-V(var18)$vertex_var18_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 20_CONFIANÇA C) Confia nas ações realizadas por este serviço. (var18)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var18, mode="in", weights = E(var18)$var18)), 
             sd(closeness(var18, mode="in", weights = E(var18)$var18))
             )
       )
```

![](20_CONFIANÇA_C_Confia_nas_ações_realizadas_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var18_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0005895 0.0007280 0.0006191 0.0008145 0.0012000
```

```r
sd(var18_outcloseness)
```

```
## [1] 0.0003211362
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var18)$outcloseness<-closeness(var18, weights = E(var18)$var18, mode="out")

#Get Variable
V(var18)$var18_color_degree<-round(V(var18)$outcloseness,6)

#Creating brewer pallette
vertex_var18_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var18)$var18_color_degree)), "RdBu"))(
            length(unique(V(var18)$var18_color_degree)))

#Saving as Vertex properties 
V(var18)$vertex_var18_color_degree<-
  vertex_var18_color_degree[as.numeric(
  cut(V(var18)$var18_color_degree,
      breaks=length(unique(V(var18)$var18_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var18, es=E(var18), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var18))
maxC <- rep(Inf, vcount(var18))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var18, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var18)$weight)


#PLotting
plot(var18, 
     layout=co,
     edge.color=V(var18)$vertex_var18_color_degree[edge.start],
     edge.arrow.size=closeness(var18, weights = E(var18)$var18, mode="out"),
     edge.width=E(var18)$weight/2*mean(E(var18)$weight),
     edge.curved = TRUE,
     vertex.color=V(var18)$vertex_var18_color_degree,
     vertex.size=closeness(var18, weights = E(var18)$var18, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var18,"LABEL_COR"),
     vertex.label.cex=closeness(var18, weights = E(var18)$var18, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var18)$var18_color_degree
b<-V(var18)$vertex_var18_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 20_CONFIANÇA C) Confia nas ações realizadas por este serviço. (var18)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var18, mode="out", weights = E(var18)$var18)), 
             sd(closeness(var18, mode="out", weights = E(var18)$var18))
             )
       )
```

![](20_CONFIANÇA_C_Confia_nas_ações_realizadas_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var18_totalcloseness)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000668 0.000955 0.001120 0.001102 0.001182 0.001825
```

```r
sd(var18_totalcloseness)
```

```
## [1] 0.0002215575
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var18)$allcloseness<-closeness(var18, weights = E(var18)$var18, mode="all")

#Get Variable
V(var18)$var18_color_degree<-round(V(var18)$allcloseness,6)

#Creating brewer pallette
vertex_var18_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var18)$var18_color_degree)), "RdBu"))(
            length(unique(V(var18)$var18_color_degree)))

#Saving as Vertex properties 
V(var18)$vertex_var18_color_degree<-
  vertex_var18_color_degree[as.numeric(
  cut(V(var18)$var18_color_degree,
      breaks=length(unique(V(var18)$var18_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var18, es=E(var18), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var18))
maxC <- rep(Inf, vcount(var18))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var18, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var18)$weight)


#PLotting
plot(var18, 
     layout=co,
     edge.color=V(var18)$vertex_var18_color_degree[edge.start],
     edge.arrow.size=closeness(var18, weights = E(var18)$var18, mode="all"),
     edge.width=E(var18)$weight/2*mean(E(var18)$weight),
     edge.curved = TRUE,
     vertex.color=V(var18)$vertex_var18_color_degree,
     vertex.size=closeness(var18, weights = E(var18)$var18, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var18,"LABEL_COR"),
     vertex.label.cex=(closeness(var18, weights = E(var18)$var18, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var18)$var18_color_degree
b<-V(var18)$vertex_var18_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 20_CONFIANÇA C) Confia nas ações realizadas por este serviço. (var18)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var18, mode="all", weights = E(var18)$var18)), 
             sd(closeness(var18, mode="all", weights = E(var18)$var18))
             )
       )
```

![](20_CONFIANÇA_C_Confia_nas_ações_realizadas_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var18)$incloseness_n <- closeness(var18, mode = "in",, weights = E(var18)$var18, normalized = T) %>% round(10)
V(var18)$outcloseness_n <- closeness(var18, mode = "out", normalized = T, weights = E(var18)$var18) %>% round(6)
V(var18)$totalcloseness_n <- closeness(var18, mode = "total", normalized = T, weights = E(var18)$var18) %>% round(6)
```

###Saving to Environment

```r
var18_incloseness_n<- closeness(var18, mode = "in", normalized = T, weights = E(var18)$var18) %>% round(6)
var18_outcloseness_n<- closeness(var18, mode = "out", normalized = T, weights = E(var18)$var18) %>% round(6)
var18_totalcloseness_n<- closeness(var18, mode = "total", normalized = T, weights = E(var18)$var18) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var18_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.023010 0.023580 0.023210 0.023650 0.024460
```

```r
sd(var18_incloseness_n)
```

```
## [1] 0.001445621
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var18)$incloseness_n<-closeness(var18, weights = E(var18)$var18, mode="in", normalized = T)

#Get Variable
V(var18)$var18_color_degree<-round(V(var18)$incloseness_n,6)

#Creating brewer pallette
vertex_var18_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var18)$var18_color_degree)), "RdBu"))(
            length(unique(V(var18)$var18_color_degree)))

#Saving as Vertex properties 
V(var18)$vertex_var18_color_degree<-
  vertex_var18_color_degree[as.numeric(
  cut(V(var18)$var18_color_degree,
      breaks=length(unique(V(var18)$var18_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var18, es=E(var18), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var18))
maxC <- rep(Inf, vcount(var18))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var18, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var18)$weight)


#PLotting
plot(var18, 
     layout=co,
     edge.color=V(var18)$vertex_var18_color_degree[edge.start],
     edge.arrow.size=closeness(var18, weights = E(var18)$var18, mode="in",normalized = T),
     edge.width=E(var18)$weight/10*mean(E(var18)$weight),
     edge.curved = TRUE,
     vertex.color=V(var18)$vertex_var18_color_degree,
     vertex.size=(closeness(var18, weights = E(var18)$var18, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var18,"LABEL_COR"),
     vertex.label.cex=closeness(var18, weights = E(var18)$var18, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var18)$var18_color_degree
b<-V(var18)$vertex_var18_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 20_CONFIANÇA C) Confia nas ações realizadas por este serviço. (var18)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var18, mode="in", weights = E(var18)$var18, normalized = T)), 
             sd(closeness(var18, mode="in", weights = E(var18)$var18, normalized = T))
             )
       )
```

![](20_CONFIANÇA_C_Confia_nas_ações_realizadas_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var18_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.109700 0.135400 0.115100 0.151500 0.223300
```

```r
sd(var18_outcloseness_n)
```

```
## [1] 0.05974901
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var18)$outcloseness_n<-closeness(var18, weights = E(var18)$var18, mode="out", normalized = T)

#Get Variable
V(var18)$var18_color_degree<-round(V(var18)$outcloseness_n,6)

#Creating brewer pallette
vertex_var18_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var18)$var18_color_degree)), "RdBu"))(
            length(unique(V(var18)$var18_color_degree)))

#Saving as Vertex properties 
V(var18)$vertex_var18_color_degree<-
  vertex_var18_color_degree[as.numeric(
  cut(V(var18)$var18_color_degree,
      breaks=length(unique(V(var18)$var18_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var18, es=E(var18), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var18))
maxC <- rep(Inf, vcount(var18))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var18, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var18)$weight)


#PLotting
plot(var18, 
     layout=co,
     edge.color=V(var18)$vertex_var18_color_degree[edge.start],
     edge.arrow.size=closeness(var18, weights = E(var18)$var18, mode="out",normalized = T),
     edge.width=E(var18)$weight/10*mean(E(var18)$weight),
     edge.curved = TRUE,
     vertex.color=V(var18)$vertex_var18_color_degree,
     vertex.size=(closeness(var18, weights = E(var18)$var18, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var18,"LABEL_COR"),
     vertex.label.cex=closeness(var18, weights = E(var18)$var18, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var18)$var18_color_degree
b<-V(var18)$vertex_var18_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 20_CONFIANÇA C) Confia nas ações realizadas por este serviço. (var18)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var18, mode="out", weights = E(var18)$var18, normalized = T)), 
             sd(closeness(var18, mode="out", weights = E(var18)$var18, normalized = T))
             )
       )
```

![](20_CONFIANÇA_C_Confia_nas_ações_realizadas_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var18_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1242  0.1777  0.2083  0.2050  0.2199  0.3394
```

```r
sd(var18_totalcloseness_n)
```

```
## [1] 0.04120978
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var18)$allcloseness_n<-closeness(var18, weights = E(var18)$var18, mode="all", normalized = T)

#Get Variable
V(var18)$var18_color_degree<-round(V(var18)$allcloseness_n,6)

#Creating brewer pallette
vertex_var18_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var18)$var18_color_degree)), "RdBu"))(
            length(unique(V(var18)$var18_color_degree)))

#Saving as Vertex properties 
V(var18)$vertex_var18_color_degree<-
  vertex_var18_color_degree[as.numeric(
  cut(V(var18)$var18_color_degree,
      breaks=length(unique(V(var18)$var18_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var18, es=E(var18), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var18))
maxC <- rep(Inf, vcount(var18))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var18, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var18)$weight)


#PLotting
plot(var18, 
     layout=co,
     edge.color=V(var18)$vertex_var18_color_degree[edge.start],
     edge.arrow.size=closeness(var18, weights = E(var18)$var18, mode="all",normalized = T),
     edge.width=E(var18)$weight/10*mean(E(var18)$weight),
     edge.curved = TRUE,
     vertex.color=V(var18)$vertex_var18_color_degree,
     vertex.size=(closeness(var18, weights = E(var18)$var18, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var18,"LABEL_COR"),
     vertex.label.cex=closeness(var18, weights = E(var18)$var18, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var18)$var18_color_degree
b<-V(var18)$vertex_var18_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 20_CONFIANÇA C) Confia nas ações realizadas por este serviço. (var18)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var18, mode="all", weights = E(var18)$var18, normalized = T)), 
             sd(closeness(var18, mode="all", weights = E(var18)$var18, normalized = T))
             )
       )
```

![](20_CONFIANÇA_C_Confia_nas_ações_realizadas_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var18)$incloseness_n <- closeness(var18, weights = E(var18)$var18, mode = "in", normalized = T) %>% round(6)
V(var18)$outcloseness_n <- closeness(var18, weights = E(var18)$var18, mode = "out", normalized = T) %>% round(6)
V(var18)$totalcloseness_n <- closeness(var18, weights = E(var18)$var18, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var18)$var18_centr_closeness<- centralization.closeness(var18)$res
var18_centr_closeness<- centralization.closeness(var18)$res
var18_centr_closeness_all<- centralization.closeness(var18)
```

###Centralization

```r
var18_centr_closeness_all$centralization
```

```
## [1] 0.1625197
```

###Theoretical Max

```r
var18_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var18)$var18_centr_closeness<- centralization.closeness(var18)$res

#Get Variable
V(var18)$var18_color_degree<-round(V(var18)$var18_centr_closeness,6)

#Creating brewer pallette
vertex_var18_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var18)$var18_color_degree)), "Spectral"))(
            length(unique(V(var18)$var18_color_degree)))

#Saving as Vertex properties 
V(var18)$vertex_var18_color_degree<-
  vertex_var18_color_degree[as.numeric(
  cut(V(var18)$var18_color_degree,
      breaks=length(unique(V(var18)$var18_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var18, es=E(var18), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var18))
maxC <- rep(Inf, vcount(var18))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var18, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var18)$weight)


#PLotting
plot(var18, 
     layout=co,
     edge.color=V(var18)$vertex_var18_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var18)$res,
     edge.width=E(var18)$weight/10*mean(E(var18)$weight),
     edge.curved = TRUE,
     vertex.color=V(var18)$vertex_var18_color_degree,
     vertex.size=centralization.closeness(var18)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var18,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var18)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var18)$var18_color_degree
b<-V(var18)$vertex_var18_color_degree
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
  title("Network Centralization Closeness - 20_CONFIANÇA C) Confia nas ações realizadas por este serviço. (var18)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var18)$res), 
             sd(centralization.closeness(var18)$res)
             )
       )
```

![](20_CONFIANÇA_C_Confia_nas_ações_realizadas_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var18_incloseness<- closeness(var18, weights = E(var18)$var18, mode = "in") %>% round(6)
var18_outcloseness<- closeness(var18, weights = E(var18)$var18, mode = "out") %>% round(6)
var18_totalcloseness<- closeness(var18, weights = E(var18)$var18, mode = "total") %>% round(6)
var18_incloseness_n<- closeness(var18,weights = E(var18)$var18, mode = "in", normalized = T) %>% round(6)
var18_outcloseness_n<- closeness(var18,weights = E(var18)$var18, mode = "out", normalized = T) %>% round(6)
var18_totalcloseness_n<- closeness(var18,weights = E(var18)$var18, mode = "total", normalized = T) %>% round(6)
var18_centr_closeness <- centralization.closeness(var18)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var18_df_closseness <- data.frame(
var18_incloseness,
var18_outcloseness,
var18_totalcloseness,
var18_incloseness_n,
var18_outcloseness_n,
var18_totalcloseness_n,
var18_centr_closeness) %>% round(6)

#Adding type
var18_df_closseness <-cbind(var18_df_closseness, V(var18)$LABEL_COR)

#Adding names
names(var18_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var18_df_closseness<-var18_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var18_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-f503095df8bf323df519" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-f503095df8bf323df519">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000132\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.0012\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000668\" data-max=\"0.001825\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024461\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.223289\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.124248\" data-max=\"0.339416\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[0.00013,0.000128,0.000132,0.000129,0.000129,0.000129,0.000125,0.000128,0.000122,0.000127,0.000129,0.000129,0.000123,0.000124,0.000127,0.000127,0.000129,0.00013,0.000122,0.000127,0.000127,0.000125,0.000127,0.000117,0.000116,0.000127,0.000121,0.000127,0.000129,0.000129,0.000129,0.000127,0.000129,0.000121,0.000129,0.000123,0.000128,0.000123,0.000122,0.000127,0.000127,0.000126,0.000126,0.000128,0.000125,0.000129,0.000118,0.000125,0.000125,0.000125,0.000125,0.000125,0.000123,0.000126,0.000122,0.000129,0.000127,0.00012,0.000121,0.000128,0.000131,0.000119,0.000127,0.000127,0.000125,2.9e-05,0.000118,0.000126,0.000122,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000129,0.000117,0.000117,0.000117,0.000117,0.000127,0.000124,0.000123,0.000127,0.000127,0.000117,0.000127,0.000127,0.000127,0.000118,0.00012,0.000126,0.000128,0.000124,0.000117,0.000122,0.000127,0.000128,0.000118,0.000125,0.000125,0.000125,0.000127,0.000127,0.000127,0.000125,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000128,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000124,0.000128,0.000127,0.000127,0.000124,0.000126,0.000127,0.000126,0.000127,0.000126,0.000127,0.000119,0.000129,0.00013,0.00013,0.000119,0.000119,0.000124,0.000128,0.000128,0.00012,0.000121,0.000123,0.00012,0.00012,0.000123,0.000124,0.00012,0.000125,0.000124,0.000126,0.000126,0.000123,0.00012,0.000123,0.000123,0.000125,0.000125,0.000126,0.000125,0.000123,0.000121,0.000127],[0.0012,0.000919,0.001004,0.000855,0.000923,0.000728,0.00075,0.000832,0.001058,0.000743,0.000814,0.000662,0.00086,0.000838,0.000792,0.000775,0.000891,0.000956,0.00077,0.001006,0.000911,0.00086,0.001009,0.000799,0.000749,0.000577,0.00084,0.000775,0.000923,0.000918,0.000917,0.000774,0.001037,0.000585,0.00059,0.00087,0.000847,0.000709,0.000706,0.001155,0.000945,0.000952,0.000951,0.00077,0.000764,0.000911,0.000743,0.000779,0.000779,0.000779,0.000779,0.000779,0.000744,0.000695,0.000823,0.000819,0.000794,0.000668,0.000725,0.000854,2.9e-05,0.000585,0.001062,0.000716,0.000842,2.9e-05,2.9e-05,0.000808,0.0007,0.000649,0.000649,0.000701,0.000887,0.000741,0.000698,0.000704,0.00075,0.000921,0.000611,0.000611,0.000611,0.000611,0.000745,0.000555,0.000913,0.000876,0.000761,0.000728,0.000782,0.000683,0.001024,0.00079,0.00084,0.000668,0.000781,0.000745,0.000519,0.000589,0.000759,0.000901,0.000565,0.000796,2.9e-05,2.9e-05,0.000752,0.00075,0.000686,2.9e-05,2.9e-05,0.00076,0.00076,0.000845,0.000747,0.000859,0.000746,0.001045,0.000884,0.000761,0.00099,0.000873,0.000762,0.001044,0.000735,0.000735,0.000735,0.000687,0.000646,0.000815,0.000646,0.000687,0.00077,0.00077,0.000689,0.000646,0.000646,0.000775,0.000652,0.000646,0.000646,0.000674,0.000676,0.000646,0.000662,0.000646,0.000646,0.000646,0.000674,0.000523,0.000627,0.00061,0.00061,0.00061,0.00061,0.00061,0.000541,0.000603,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001825,0.001422,0.001567,0.001431,0.001506,0.001389,0.000984,0.001181,0.001468,0.001053,0.001437,0.00146,0.001112,0.001059,0.001183,0.00105,0.001403,0.00152,0.001025,0.001441,0.001314,0.00114,0.001499,0.001026,0.000946,0.00104,0.001145,0.001309,0.001425,0.00149,0.001441,0.001155,0.001504,0.000912,0.001381,0.001163,0.001153,0.000976,0.000898,0.00161,0.001312,0.001267,0.001272,0.001196,0.000998,0.001499,0.001065,0.001002,0.001002,0.001002,0.001002,0.001002,0.000997,0.001148,0.001059,0.001433,0.001183,0.000954,0.000997,0.001342,0.001134,0.000791,0.001513,0.001147,0.001105,0.000668,0.000672,0.001263,0.00091,0.001133,0.001143,0.001185,0.001217,0.00117,0.00116,0.001148,0.001172,0.001473,0.000758,0.000758,0.000758,0.000758,0.001159,0.000966,0.001294,0.001159,0.001161,0.000925,0.001155,0.001117,0.001471,0.000997,0.001092,0.001258,0.001227,0.00106,0.000689,0.000766,0.001143,0.001312,0.000766,0.001052,0.000807,0.000807,0.001121,0.001179,0.00112,0.000807,0.000903,0.001163,0.001116,0.001145,0.001151,0.001167,0.001126,0.001484,0.001174,0.001122,0.001412,0.001269,0.001129,0.00152,0.001163,0.001161,0.001163,0.001117,0.001116,0.001189,0.00113,0.001117,0.00117,0.001172,0.001116,0.001127,0.00112,0.001221,0.001131,0.001126,0.001143,0.001119,0.001159,0.001121,0.001124,0.001116,0.000844,0.001088,0.001325,0.001136,0.000962,0.001098,0.001116,0.001098,0.001116,0.001098,0.001109,0.001142,0.001098,0.001116,0.001116,0.000754,0.000683,0.000938,0.000887,0.000864,0.000747,0.000668,0.000819,0.000747,0.000747,0.000821,0.000838,0.000747,0.000937,0.000833,0.000988,0.000956,0.00082,0.000747,0.00082,0.000822,0.000916,0.000916,0.000934,0.000943,0.000826,0.000685,0.000838],[0.024209,0.023865,0.024461,0.023929,0.023969,0.023944,0.023338,0.023834,0.022733,0.02367,0.023963,0.023991,0.022853,0.023129,0.023568,0.023625,0.023988,0.024093,0.022761,0.023574,0.02364,0.023241,0.023688,0.0218,0.021668,0.023571,0.022469,0.023667,0.02392,0.023932,0.02396,0.023586,0.024081,0.022469,0.023926,0.022839,0.023764,0.022816,0.022758,0.023589,0.023646,0.023473,0.023485,0.023868,0.02325,0.023954,0.021931,0.023212,0.023212,0.023212,0.023212,0.023212,0.0228,0.023443,0.022719,0.023926,0.02367,0.022294,0.02257,0.023806,0.024282,0.022209,0.023586,0.023589,0.023212,0.005348,0.02202,0.023426,0.022708,0.023553,0.023598,0.023688,0.023607,0.023646,0.023655,0.023592,0.023622,0.024006,0.021701,0.021701,0.021701,0.021701,0.023631,0.022989,0.022853,0.023628,0.023538,0.021714,0.023583,0.023556,0.023598,0.021934,0.022273,0.023432,0.023734,0.023045,0.021714,0.022619,0.023637,0.023728,0.02189,0.023233,0.023273,0.023273,0.023586,0.023679,0.023595,0.023273,0.023523,0.023604,0.02358,0.023649,0.023619,0.023598,0.023631,0.023589,0.023583,0.023586,0.023703,0.023767,0.023589,0.023625,0.023619,0.023586,0.023619,0.023607,0.023589,0.023595,0.023604,0.023429,0.023577,0.023595,0.023604,0.023598,0.023613,0.023712,0.023649,0.023598,0.023625,0.023574,0.023658,0.023577,0.02358,0.023574,0.023131,0.023715,0.023604,0.02364,0.023108,0.023402,0.02358,0.023402,0.02358,0.023402,0.023685,0.022164,0.02396,0.024143,0.024134,0.022201,0.022143,0.023108,0.023779,0.023746,0.022399,0.02251,0.022918,0.022399,0.022399,0.02296,0.023043,0.022399,0.02334,0.023028,0.02342,0.023432,0.022952,0.022399,0.022952,0.022963,0.023238,0.023238,0.023352,0.023265,0.022946,0.022483,0.023652],[0.223289,0.170956,0.186747,0.158974,0.171587,0.13547,0.13943,0.154742,0.196825,0.13829,0.151466,0.123097,0.159931,0.155909,0.147268,0.144186,0.165775,0.17782,0.143297,0.187123,0.169399,0.159931,0.187689,0.148681,0.139326,0.107266,0.156171,0.144074,0.171587,0.170799,0.170486,0.143963,0.192946,0.108772,0.109799,0.16188,0.157627,0.131915,0.131356,0.214781,0.175803,0.177143,0.176974,0.143297,0.142093,0.169399,0.138187,0.144973,0.144973,0.144973,0.144973,0.144973,0.138393,0.129346,0.153086,0.152334,0.147619,0.124248,0.134783,0.158839,0.005348,0.108772,0.197452,0.133142,0.156698,0.005376,0.005405,0.150242,0.130161,0.120623,0.120623,0.130343,0.164894,0.13788,0.129888,0.130986,0.13943,0.171271,0.113553,0.113553,0.113553,0.113553,0.138496,0.103161,0.169863,0.163015,0.141553,0.135371,0.14554,0.127049,0.190379,0.146919,0.156171,0.124166,0.145199,0.138496,0.096573,0.109605,0.14123,0.167568,0.105144,0.147971,0.005348,0.005348,0.13985,0.13943,0.12766,0.005348,0.005348,0.141445,0.141445,0.157095,0.13891,0.159794,0.138806,0.194357,0.164456,0.141553,0.184158,0.162445,0.14166,0.194154,0.136765,0.136765,0.136765,0.127747,0.120155,0.151589,0.120155,0.127747,0.143187,0.143187,0.128187,0.120155,0.120155,0.144186,0.121252,0.120155,0.120155,0.125337,0.125761,0.120155,0.123097,0.120155,0.120155,0.120155,0.125337,0.09728,0.116688,0.113415,0.113415,0.113415,0.113415,0.113415,0.100595,0.112183,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.339416,0.26458,0.291536,0.266094,0.28012,0.258333,0.183071,0.219599,0.273128,0.195789,0.267241,0.271533,0.206897,0.197034,0.220118,0.195378,0.26087,0.282675,0.190574,0.268012,0.244415,0.212087,0.278861,0.190769,0.17597,0.193347,0.213058,0.243455,0.264957,0.277198,0.268012,0.214781,0.279699,0.169553,0.256906,0.216279,0.214533,0.181463,0.167116,0.299517,0.244094,0.235741,0.236641,0.222488,0.185629,0.278861,0.198083,0.186373,0.186373,0.186373,0.186373,0.186373,0.185444,0.213548,0.197034,0.266476,0.220118,0.177481,0.185444,0.249664,0.210884,0.147036,0.281392,0.213303,0.205525,0.124248,0.124916,0.234848,0.169245,0.210646,0.212571,0.220379,0.226277,0.217544,0.215777,0.213548,0.218054,0.273932,0.141016,0.141016,0.141016,0.141016,0.215527,0.17971,0.240621,0.215527,0.216028,0.172063,0.214781,0.207821,0.273529,0.185444,0.203057,0.233962,0.228221,0.197243,0.128187,0.14242,0.212571,0.244094,0.14242,0.195584,0.150121,0.150121,0.20852,0.21934,0.208287,0.150121,0.168022,0.216279,0.207589,0.213058,0.214039,0.217036,0.209459,0.275964,0.21831,0.208754,0.262712,0.236041,0.209932,0.282675,0.216279,0.216028,0.216279,0.207821,0.207589,0.221165,0.210169,0.207821,0.217544,0.218054,0.207589,0.209696,0.208287,0.227106,0.210407,0.209459,0.212571,0.208054,0.215527,0.20852,0.208989,0.207589,0.156962,0.202394,0.246358,0.211364,0.178846,0.204171,0.207589,0.204171,0.207589,0.204171,0.206208,0.212329,0.204171,0.207589,0.207589,0.140166,0.126962,0.174484,0.16504,0.160622,0.13891,0.124332,0.152334,0.13891,0.13891,0.152709,0.155909,0.13891,0.174321,0.154871,0.183794,0.17782,0.152584,0.13891,0.152584,0.152961,0.17033,0.17033,0.173669,0.175472,0.153592,0.127397,0.155909],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var18_df_closseness, by=list(var18_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var18_df_closseness, by=list(var18_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-150e209bda02aa526c45" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-150e209bda02aa526c45">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000119\" data-max=\"0.000132\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.0012\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8.1e-05\" data-max=\"0.000361\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000838\" data-max=\"0.001825\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000275\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022037\" data-max=\"0.024461\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.2e-05\" data-max=\"0.004095\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.223289\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.015015\" data-max=\"0.067234\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.155909\" data-max=\"0.339416\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005484\" data-max=\"0.051147\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003883\" data-max=\"0.139638\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[0.000127,0.000122,0.000127,0.000128,0.000125,0.000128,0.000127,0.000132,0.000127,0.000127,0.000129,0.000119,0.000125,0.000129,0.000129,0.000123,0.000127,0.000128,0.00013,0.000125,0.00013,0.000127,0.000127],[3e-06,3e-06,null,null,null,null,1e-06,null,null,null,null,2.2e-05,1e-06,0,0,4e-06,null,null,null,1e-06,null,1e-06,0],[0.00087,0.000271,0.001006,0.000919,0.00075,0.000854,0.000777,0.001004,0.001009,0.000674,0.000921,0.000731,0.000895,0.000823,0.000847,0.000813,2.9e-05,0.000901,0.0012,0.000517,0.000956,0.00059,0.000753],[0.000215,0.000326,null,null,null,null,0.000133,null,null,null,null,0.00021,8.1e-05,0.000121,0.000105,0.000173,null,null,null,0.000361,null,0.00026,0.000112],[0.001259,0.00089,0.001441,0.001422,0.000984,0.001342,0.001237,0.001567,0.001499,0.001325,0.001473,0.001051,0.001163,0.001444,0.001433,0.00111,0.000838,0.001312,0.001825,0.000938,0.00152,0.001132,0.001179],[0.000275,0.000146,null,null,null,null,0.000129,null,null,null,null,0.000204,0.000147,3.5e-05,6.4e-05,0.000273,null,null,null,9e-05,null,2.9e-05,9.5e-05],[0.023612,0.02273,0.023574,0.023865,0.023338,0.023806,0.023674,0.024461,0.023688,0.023604,0.024006,0.022037,0.023301,0.023945,0.023967,0.022979,0.023652,0.023728,0.024209,0.023217,0.024093,0.023635,0.023614],[0.000487,0.000636,null,null,null,null,3.8e-05,null,null,null,null,0.004095,0.000243,2.4e-05,2.2e-05,0.000666,null,null,null,0.000241,null,0.000216,5.3e-05],[0.161812,0.050289,0.187123,0.170956,0.13943,0.158839,0.144543,0.186747,0.187689,0.125337,0.171271,0.135984,0.166526,0.153105,0.157611,0.1513,0.005348,0.167568,0.223289,0.096143,0.17782,0.109699,0.140133],[0.040008,0.060705,null,null,null,null,0.024625,null,null,null,null,0.039063,0.015015,0.022461,0.019393,0.032214,null,null,null,0.067234,null,0.048332,0.020884],[0.234213,0.16556,0.268012,0.26458,0.183071,0.249664,0.230088,0.291536,0.278861,0.246358,0.273932,0.19558,0.216388,0.268586,0.266441,0.20637,0.155909,0.244094,0.339416,0.174517,0.282675,0.210466,0.219239],[0.051147,0.027224,null,null,null,null,0.023989,null,null,null,null,0.038021,0.02737,0.006622,0.011914,0.050848,null,null,null,0.016702,null,0.005484,0.017684],[0.300585,0.099063,0.286154,0.343173,0.303922,0.323478,0.300896,0.391579,0.295238,0.29108,0.313659,0.266552,0.319611,0.301223,0.316849,0.295625,0.005348,0.306425,0.371257,0.194167,0.314189,0.234219,0.288627],[0.043014,0.125815,null,null,null,null,0.018146,null,null,null,null,0.070696,0.003883,0.033229,0.029748,0.023781,null,null,null,0.139638,null,0.102732,0.011577]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var18_df_closseness <- data.frame(
var18_incloseness,
var18_outcloseness,
var18_totalcloseness,
var18_incloseness_n,
var18_outcloseness_n,
var18_totalcloseness_n,
var18_centr_closeness) %>% round(6)

#Adding type
var18_df_closseness <-cbind(var18_df_closseness, V(var18)$TIPO1)

#Adding names
names(var18_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var18_df_closseness<-var18_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var18_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-bbdffaa0a7451e4d4d3a" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-bbdffaa0a7451e4d4d3a">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000132\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.0012\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000668\" data-max=\"0.001825\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024461\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.223289\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.124248\" data-max=\"0.339416\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[0.00013,0.000128,0.000132,0.000129,0.000129,0.000129,0.000125,0.000128,0.000122,0.000127,0.000129,0.000129,0.000123,0.000124,0.000127,0.000127,0.000129,0.00013,0.000122,0.000127,0.000127,0.000125,0.000127,0.000117,0.000116,0.000127,0.000121,0.000127,0.000129,0.000129,0.000129,0.000127,0.000129,0.000121,0.000129,0.000123,0.000128,0.000123,0.000122,0.000127,0.000127,0.000126,0.000126,0.000128,0.000125,0.000129,0.000118,0.000125,0.000125,0.000125,0.000125,0.000125,0.000123,0.000126,0.000122,0.000129,0.000127,0.00012,0.000121,0.000128,0.000131,0.000119,0.000127,0.000127,0.000125,2.9e-05,0.000118,0.000126,0.000122,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000129,0.000117,0.000117,0.000117,0.000117,0.000127,0.000124,0.000123,0.000127,0.000127,0.000117,0.000127,0.000127,0.000127,0.000118,0.00012,0.000126,0.000128,0.000124,0.000117,0.000122,0.000127,0.000128,0.000118,0.000125,0.000125,0.000125,0.000127,0.000127,0.000127,0.000125,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000128,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000124,0.000128,0.000127,0.000127,0.000124,0.000126,0.000127,0.000126,0.000127,0.000126,0.000127,0.000119,0.000129,0.00013,0.00013,0.000119,0.000119,0.000124,0.000128,0.000128,0.00012,0.000121,0.000123,0.00012,0.00012,0.000123,0.000124,0.00012,0.000125,0.000124,0.000126,0.000126,0.000123,0.00012,0.000123,0.000123,0.000125,0.000125,0.000126,0.000125,0.000123,0.000121,0.000127],[0.0012,0.000919,0.001004,0.000855,0.000923,0.000728,0.00075,0.000832,0.001058,0.000743,0.000814,0.000662,0.00086,0.000838,0.000792,0.000775,0.000891,0.000956,0.00077,0.001006,0.000911,0.00086,0.001009,0.000799,0.000749,0.000577,0.00084,0.000775,0.000923,0.000918,0.000917,0.000774,0.001037,0.000585,0.00059,0.00087,0.000847,0.000709,0.000706,0.001155,0.000945,0.000952,0.000951,0.00077,0.000764,0.000911,0.000743,0.000779,0.000779,0.000779,0.000779,0.000779,0.000744,0.000695,0.000823,0.000819,0.000794,0.000668,0.000725,0.000854,2.9e-05,0.000585,0.001062,0.000716,0.000842,2.9e-05,2.9e-05,0.000808,0.0007,0.000649,0.000649,0.000701,0.000887,0.000741,0.000698,0.000704,0.00075,0.000921,0.000611,0.000611,0.000611,0.000611,0.000745,0.000555,0.000913,0.000876,0.000761,0.000728,0.000782,0.000683,0.001024,0.00079,0.00084,0.000668,0.000781,0.000745,0.000519,0.000589,0.000759,0.000901,0.000565,0.000796,2.9e-05,2.9e-05,0.000752,0.00075,0.000686,2.9e-05,2.9e-05,0.00076,0.00076,0.000845,0.000747,0.000859,0.000746,0.001045,0.000884,0.000761,0.00099,0.000873,0.000762,0.001044,0.000735,0.000735,0.000735,0.000687,0.000646,0.000815,0.000646,0.000687,0.00077,0.00077,0.000689,0.000646,0.000646,0.000775,0.000652,0.000646,0.000646,0.000674,0.000676,0.000646,0.000662,0.000646,0.000646,0.000646,0.000674,0.000523,0.000627,0.00061,0.00061,0.00061,0.00061,0.00061,0.000541,0.000603,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001825,0.001422,0.001567,0.001431,0.001506,0.001389,0.000984,0.001181,0.001468,0.001053,0.001437,0.00146,0.001112,0.001059,0.001183,0.00105,0.001403,0.00152,0.001025,0.001441,0.001314,0.00114,0.001499,0.001026,0.000946,0.00104,0.001145,0.001309,0.001425,0.00149,0.001441,0.001155,0.001504,0.000912,0.001381,0.001163,0.001153,0.000976,0.000898,0.00161,0.001312,0.001267,0.001272,0.001196,0.000998,0.001499,0.001065,0.001002,0.001002,0.001002,0.001002,0.001002,0.000997,0.001148,0.001059,0.001433,0.001183,0.000954,0.000997,0.001342,0.001134,0.000791,0.001513,0.001147,0.001105,0.000668,0.000672,0.001263,0.00091,0.001133,0.001143,0.001185,0.001217,0.00117,0.00116,0.001148,0.001172,0.001473,0.000758,0.000758,0.000758,0.000758,0.001159,0.000966,0.001294,0.001159,0.001161,0.000925,0.001155,0.001117,0.001471,0.000997,0.001092,0.001258,0.001227,0.00106,0.000689,0.000766,0.001143,0.001312,0.000766,0.001052,0.000807,0.000807,0.001121,0.001179,0.00112,0.000807,0.000903,0.001163,0.001116,0.001145,0.001151,0.001167,0.001126,0.001484,0.001174,0.001122,0.001412,0.001269,0.001129,0.00152,0.001163,0.001161,0.001163,0.001117,0.001116,0.001189,0.00113,0.001117,0.00117,0.001172,0.001116,0.001127,0.00112,0.001221,0.001131,0.001126,0.001143,0.001119,0.001159,0.001121,0.001124,0.001116,0.000844,0.001088,0.001325,0.001136,0.000962,0.001098,0.001116,0.001098,0.001116,0.001098,0.001109,0.001142,0.001098,0.001116,0.001116,0.000754,0.000683,0.000938,0.000887,0.000864,0.000747,0.000668,0.000819,0.000747,0.000747,0.000821,0.000838,0.000747,0.000937,0.000833,0.000988,0.000956,0.00082,0.000747,0.00082,0.000822,0.000916,0.000916,0.000934,0.000943,0.000826,0.000685,0.000838],[0.024209,0.023865,0.024461,0.023929,0.023969,0.023944,0.023338,0.023834,0.022733,0.02367,0.023963,0.023991,0.022853,0.023129,0.023568,0.023625,0.023988,0.024093,0.022761,0.023574,0.02364,0.023241,0.023688,0.0218,0.021668,0.023571,0.022469,0.023667,0.02392,0.023932,0.02396,0.023586,0.024081,0.022469,0.023926,0.022839,0.023764,0.022816,0.022758,0.023589,0.023646,0.023473,0.023485,0.023868,0.02325,0.023954,0.021931,0.023212,0.023212,0.023212,0.023212,0.023212,0.0228,0.023443,0.022719,0.023926,0.02367,0.022294,0.02257,0.023806,0.024282,0.022209,0.023586,0.023589,0.023212,0.005348,0.02202,0.023426,0.022708,0.023553,0.023598,0.023688,0.023607,0.023646,0.023655,0.023592,0.023622,0.024006,0.021701,0.021701,0.021701,0.021701,0.023631,0.022989,0.022853,0.023628,0.023538,0.021714,0.023583,0.023556,0.023598,0.021934,0.022273,0.023432,0.023734,0.023045,0.021714,0.022619,0.023637,0.023728,0.02189,0.023233,0.023273,0.023273,0.023586,0.023679,0.023595,0.023273,0.023523,0.023604,0.02358,0.023649,0.023619,0.023598,0.023631,0.023589,0.023583,0.023586,0.023703,0.023767,0.023589,0.023625,0.023619,0.023586,0.023619,0.023607,0.023589,0.023595,0.023604,0.023429,0.023577,0.023595,0.023604,0.023598,0.023613,0.023712,0.023649,0.023598,0.023625,0.023574,0.023658,0.023577,0.02358,0.023574,0.023131,0.023715,0.023604,0.02364,0.023108,0.023402,0.02358,0.023402,0.02358,0.023402,0.023685,0.022164,0.02396,0.024143,0.024134,0.022201,0.022143,0.023108,0.023779,0.023746,0.022399,0.02251,0.022918,0.022399,0.022399,0.02296,0.023043,0.022399,0.02334,0.023028,0.02342,0.023432,0.022952,0.022399,0.022952,0.022963,0.023238,0.023238,0.023352,0.023265,0.022946,0.022483,0.023652],[0.223289,0.170956,0.186747,0.158974,0.171587,0.13547,0.13943,0.154742,0.196825,0.13829,0.151466,0.123097,0.159931,0.155909,0.147268,0.144186,0.165775,0.17782,0.143297,0.187123,0.169399,0.159931,0.187689,0.148681,0.139326,0.107266,0.156171,0.144074,0.171587,0.170799,0.170486,0.143963,0.192946,0.108772,0.109799,0.16188,0.157627,0.131915,0.131356,0.214781,0.175803,0.177143,0.176974,0.143297,0.142093,0.169399,0.138187,0.144973,0.144973,0.144973,0.144973,0.144973,0.138393,0.129346,0.153086,0.152334,0.147619,0.124248,0.134783,0.158839,0.005348,0.108772,0.197452,0.133142,0.156698,0.005376,0.005405,0.150242,0.130161,0.120623,0.120623,0.130343,0.164894,0.13788,0.129888,0.130986,0.13943,0.171271,0.113553,0.113553,0.113553,0.113553,0.138496,0.103161,0.169863,0.163015,0.141553,0.135371,0.14554,0.127049,0.190379,0.146919,0.156171,0.124166,0.145199,0.138496,0.096573,0.109605,0.14123,0.167568,0.105144,0.147971,0.005348,0.005348,0.13985,0.13943,0.12766,0.005348,0.005348,0.141445,0.141445,0.157095,0.13891,0.159794,0.138806,0.194357,0.164456,0.141553,0.184158,0.162445,0.14166,0.194154,0.136765,0.136765,0.136765,0.127747,0.120155,0.151589,0.120155,0.127747,0.143187,0.143187,0.128187,0.120155,0.120155,0.144186,0.121252,0.120155,0.120155,0.125337,0.125761,0.120155,0.123097,0.120155,0.120155,0.120155,0.125337,0.09728,0.116688,0.113415,0.113415,0.113415,0.113415,0.113415,0.100595,0.112183,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.339416,0.26458,0.291536,0.266094,0.28012,0.258333,0.183071,0.219599,0.273128,0.195789,0.267241,0.271533,0.206897,0.197034,0.220118,0.195378,0.26087,0.282675,0.190574,0.268012,0.244415,0.212087,0.278861,0.190769,0.17597,0.193347,0.213058,0.243455,0.264957,0.277198,0.268012,0.214781,0.279699,0.169553,0.256906,0.216279,0.214533,0.181463,0.167116,0.299517,0.244094,0.235741,0.236641,0.222488,0.185629,0.278861,0.198083,0.186373,0.186373,0.186373,0.186373,0.186373,0.185444,0.213548,0.197034,0.266476,0.220118,0.177481,0.185444,0.249664,0.210884,0.147036,0.281392,0.213303,0.205525,0.124248,0.124916,0.234848,0.169245,0.210646,0.212571,0.220379,0.226277,0.217544,0.215777,0.213548,0.218054,0.273932,0.141016,0.141016,0.141016,0.141016,0.215527,0.17971,0.240621,0.215527,0.216028,0.172063,0.214781,0.207821,0.273529,0.185444,0.203057,0.233962,0.228221,0.197243,0.128187,0.14242,0.212571,0.244094,0.14242,0.195584,0.150121,0.150121,0.20852,0.21934,0.208287,0.150121,0.168022,0.216279,0.207589,0.213058,0.214039,0.217036,0.209459,0.275964,0.21831,0.208754,0.262712,0.236041,0.209932,0.282675,0.216279,0.216028,0.216279,0.207821,0.207589,0.221165,0.210169,0.207821,0.217544,0.218054,0.207589,0.209696,0.208287,0.227106,0.210407,0.209459,0.212571,0.208054,0.215527,0.20852,0.208989,0.207589,0.156962,0.202394,0.246358,0.211364,0.178846,0.204171,0.207589,0.204171,0.207589,0.204171,0.206208,0.212329,0.204171,0.207589,0.207589,0.140166,0.126962,0.174484,0.16504,0.160622,0.13891,0.124332,0.152334,0.13891,0.13891,0.152709,0.155909,0.13891,0.174321,0.154871,0.183794,0.17782,0.152584,0.13891,0.152584,0.152961,0.17033,0.17033,0.173669,0.175472,0.153592,0.127397,0.155909],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var18_df_closseness, by=list(var18_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var18_df_closseness, by=list(var18_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-792ba1d1b378c9670daa" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-792ba1d1b378c9670daa">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000122\" data-max=\"0.000127\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000473\" data-max=\"0.000726\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000233\" data-max=\"0.000366\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000969\" data-max=\"0.0012\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000179\" data-max=\"0.000204\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022622\" data-max=\"0.023638\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00025\" data-max=\"0.002072\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.087972\" data-max=\"0.135001\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.043335\" data-max=\"0.06814\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.180146\" data-max=\"0.223215\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.033289\" data-max=\"0.038007\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.173776\" data-max=\"0.2725\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.079091\" data-max=\"0.135432\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[0.000127,0.000122],[1e-06,1.1e-05],[0.000726,0.000473],[0.000233,0.000366],[0.0012,0.000969],[0.000179,0.000204],[0.023638,0.022622],[0.000251,0.002072],[0.135001,0.087972],[0.043335,0.06814],[0.223215,0.180146],[0.033289,0.038007],[0.2725,0.173776],[0.079091,0.135432]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var18_df_closseness <- data.frame(
var18_incloseness,
var18_outcloseness,
var18_totalcloseness,
var18_incloseness_n,
var18_outcloseness_n,
var18_totalcloseness_n,
var18_centr_closeness) %>% round(6)

#Adding type
var18_df_closseness <-cbind(var18_df_closseness, V(var18)$TIPO2)

#Adding names
names(var18_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var18_df_closseness<-var18_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var18_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-8b4f8b9ea4c8ff36c300" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-8b4f8b9ea4c8ff36c300">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000132\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.0012\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000668\" data-max=\"0.001825\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024461\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.223289\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.124248\" data-max=\"0.339416\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[0.00013,0.000128,0.000132,0.000129,0.000129,0.000129,0.000125,0.000128,0.000122,0.000127,0.000129,0.000129,0.000123,0.000124,0.000127,0.000127,0.000129,0.00013,0.000122,0.000127,0.000127,0.000125,0.000127,0.000117,0.000116,0.000127,0.000121,0.000127,0.000129,0.000129,0.000129,0.000127,0.000129,0.000121,0.000129,0.000123,0.000128,0.000123,0.000122,0.000127,0.000127,0.000126,0.000126,0.000128,0.000125,0.000129,0.000118,0.000125,0.000125,0.000125,0.000125,0.000125,0.000123,0.000126,0.000122,0.000129,0.000127,0.00012,0.000121,0.000128,0.000131,0.000119,0.000127,0.000127,0.000125,2.9e-05,0.000118,0.000126,0.000122,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000129,0.000117,0.000117,0.000117,0.000117,0.000127,0.000124,0.000123,0.000127,0.000127,0.000117,0.000127,0.000127,0.000127,0.000118,0.00012,0.000126,0.000128,0.000124,0.000117,0.000122,0.000127,0.000128,0.000118,0.000125,0.000125,0.000125,0.000127,0.000127,0.000127,0.000125,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000128,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000124,0.000128,0.000127,0.000127,0.000124,0.000126,0.000127,0.000126,0.000127,0.000126,0.000127,0.000119,0.000129,0.00013,0.00013,0.000119,0.000119,0.000124,0.000128,0.000128,0.00012,0.000121,0.000123,0.00012,0.00012,0.000123,0.000124,0.00012,0.000125,0.000124,0.000126,0.000126,0.000123,0.00012,0.000123,0.000123,0.000125,0.000125,0.000126,0.000125,0.000123,0.000121,0.000127],[0.0012,0.000919,0.001004,0.000855,0.000923,0.000728,0.00075,0.000832,0.001058,0.000743,0.000814,0.000662,0.00086,0.000838,0.000792,0.000775,0.000891,0.000956,0.00077,0.001006,0.000911,0.00086,0.001009,0.000799,0.000749,0.000577,0.00084,0.000775,0.000923,0.000918,0.000917,0.000774,0.001037,0.000585,0.00059,0.00087,0.000847,0.000709,0.000706,0.001155,0.000945,0.000952,0.000951,0.00077,0.000764,0.000911,0.000743,0.000779,0.000779,0.000779,0.000779,0.000779,0.000744,0.000695,0.000823,0.000819,0.000794,0.000668,0.000725,0.000854,2.9e-05,0.000585,0.001062,0.000716,0.000842,2.9e-05,2.9e-05,0.000808,0.0007,0.000649,0.000649,0.000701,0.000887,0.000741,0.000698,0.000704,0.00075,0.000921,0.000611,0.000611,0.000611,0.000611,0.000745,0.000555,0.000913,0.000876,0.000761,0.000728,0.000782,0.000683,0.001024,0.00079,0.00084,0.000668,0.000781,0.000745,0.000519,0.000589,0.000759,0.000901,0.000565,0.000796,2.9e-05,2.9e-05,0.000752,0.00075,0.000686,2.9e-05,2.9e-05,0.00076,0.00076,0.000845,0.000747,0.000859,0.000746,0.001045,0.000884,0.000761,0.00099,0.000873,0.000762,0.001044,0.000735,0.000735,0.000735,0.000687,0.000646,0.000815,0.000646,0.000687,0.00077,0.00077,0.000689,0.000646,0.000646,0.000775,0.000652,0.000646,0.000646,0.000674,0.000676,0.000646,0.000662,0.000646,0.000646,0.000646,0.000674,0.000523,0.000627,0.00061,0.00061,0.00061,0.00061,0.00061,0.000541,0.000603,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001825,0.001422,0.001567,0.001431,0.001506,0.001389,0.000984,0.001181,0.001468,0.001053,0.001437,0.00146,0.001112,0.001059,0.001183,0.00105,0.001403,0.00152,0.001025,0.001441,0.001314,0.00114,0.001499,0.001026,0.000946,0.00104,0.001145,0.001309,0.001425,0.00149,0.001441,0.001155,0.001504,0.000912,0.001381,0.001163,0.001153,0.000976,0.000898,0.00161,0.001312,0.001267,0.001272,0.001196,0.000998,0.001499,0.001065,0.001002,0.001002,0.001002,0.001002,0.001002,0.000997,0.001148,0.001059,0.001433,0.001183,0.000954,0.000997,0.001342,0.001134,0.000791,0.001513,0.001147,0.001105,0.000668,0.000672,0.001263,0.00091,0.001133,0.001143,0.001185,0.001217,0.00117,0.00116,0.001148,0.001172,0.001473,0.000758,0.000758,0.000758,0.000758,0.001159,0.000966,0.001294,0.001159,0.001161,0.000925,0.001155,0.001117,0.001471,0.000997,0.001092,0.001258,0.001227,0.00106,0.000689,0.000766,0.001143,0.001312,0.000766,0.001052,0.000807,0.000807,0.001121,0.001179,0.00112,0.000807,0.000903,0.001163,0.001116,0.001145,0.001151,0.001167,0.001126,0.001484,0.001174,0.001122,0.001412,0.001269,0.001129,0.00152,0.001163,0.001161,0.001163,0.001117,0.001116,0.001189,0.00113,0.001117,0.00117,0.001172,0.001116,0.001127,0.00112,0.001221,0.001131,0.001126,0.001143,0.001119,0.001159,0.001121,0.001124,0.001116,0.000844,0.001088,0.001325,0.001136,0.000962,0.001098,0.001116,0.001098,0.001116,0.001098,0.001109,0.001142,0.001098,0.001116,0.001116,0.000754,0.000683,0.000938,0.000887,0.000864,0.000747,0.000668,0.000819,0.000747,0.000747,0.000821,0.000838,0.000747,0.000937,0.000833,0.000988,0.000956,0.00082,0.000747,0.00082,0.000822,0.000916,0.000916,0.000934,0.000943,0.000826,0.000685,0.000838],[0.024209,0.023865,0.024461,0.023929,0.023969,0.023944,0.023338,0.023834,0.022733,0.02367,0.023963,0.023991,0.022853,0.023129,0.023568,0.023625,0.023988,0.024093,0.022761,0.023574,0.02364,0.023241,0.023688,0.0218,0.021668,0.023571,0.022469,0.023667,0.02392,0.023932,0.02396,0.023586,0.024081,0.022469,0.023926,0.022839,0.023764,0.022816,0.022758,0.023589,0.023646,0.023473,0.023485,0.023868,0.02325,0.023954,0.021931,0.023212,0.023212,0.023212,0.023212,0.023212,0.0228,0.023443,0.022719,0.023926,0.02367,0.022294,0.02257,0.023806,0.024282,0.022209,0.023586,0.023589,0.023212,0.005348,0.02202,0.023426,0.022708,0.023553,0.023598,0.023688,0.023607,0.023646,0.023655,0.023592,0.023622,0.024006,0.021701,0.021701,0.021701,0.021701,0.023631,0.022989,0.022853,0.023628,0.023538,0.021714,0.023583,0.023556,0.023598,0.021934,0.022273,0.023432,0.023734,0.023045,0.021714,0.022619,0.023637,0.023728,0.02189,0.023233,0.023273,0.023273,0.023586,0.023679,0.023595,0.023273,0.023523,0.023604,0.02358,0.023649,0.023619,0.023598,0.023631,0.023589,0.023583,0.023586,0.023703,0.023767,0.023589,0.023625,0.023619,0.023586,0.023619,0.023607,0.023589,0.023595,0.023604,0.023429,0.023577,0.023595,0.023604,0.023598,0.023613,0.023712,0.023649,0.023598,0.023625,0.023574,0.023658,0.023577,0.02358,0.023574,0.023131,0.023715,0.023604,0.02364,0.023108,0.023402,0.02358,0.023402,0.02358,0.023402,0.023685,0.022164,0.02396,0.024143,0.024134,0.022201,0.022143,0.023108,0.023779,0.023746,0.022399,0.02251,0.022918,0.022399,0.022399,0.02296,0.023043,0.022399,0.02334,0.023028,0.02342,0.023432,0.022952,0.022399,0.022952,0.022963,0.023238,0.023238,0.023352,0.023265,0.022946,0.022483,0.023652],[0.223289,0.170956,0.186747,0.158974,0.171587,0.13547,0.13943,0.154742,0.196825,0.13829,0.151466,0.123097,0.159931,0.155909,0.147268,0.144186,0.165775,0.17782,0.143297,0.187123,0.169399,0.159931,0.187689,0.148681,0.139326,0.107266,0.156171,0.144074,0.171587,0.170799,0.170486,0.143963,0.192946,0.108772,0.109799,0.16188,0.157627,0.131915,0.131356,0.214781,0.175803,0.177143,0.176974,0.143297,0.142093,0.169399,0.138187,0.144973,0.144973,0.144973,0.144973,0.144973,0.138393,0.129346,0.153086,0.152334,0.147619,0.124248,0.134783,0.158839,0.005348,0.108772,0.197452,0.133142,0.156698,0.005376,0.005405,0.150242,0.130161,0.120623,0.120623,0.130343,0.164894,0.13788,0.129888,0.130986,0.13943,0.171271,0.113553,0.113553,0.113553,0.113553,0.138496,0.103161,0.169863,0.163015,0.141553,0.135371,0.14554,0.127049,0.190379,0.146919,0.156171,0.124166,0.145199,0.138496,0.096573,0.109605,0.14123,0.167568,0.105144,0.147971,0.005348,0.005348,0.13985,0.13943,0.12766,0.005348,0.005348,0.141445,0.141445,0.157095,0.13891,0.159794,0.138806,0.194357,0.164456,0.141553,0.184158,0.162445,0.14166,0.194154,0.136765,0.136765,0.136765,0.127747,0.120155,0.151589,0.120155,0.127747,0.143187,0.143187,0.128187,0.120155,0.120155,0.144186,0.121252,0.120155,0.120155,0.125337,0.125761,0.120155,0.123097,0.120155,0.120155,0.120155,0.125337,0.09728,0.116688,0.113415,0.113415,0.113415,0.113415,0.113415,0.100595,0.112183,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.339416,0.26458,0.291536,0.266094,0.28012,0.258333,0.183071,0.219599,0.273128,0.195789,0.267241,0.271533,0.206897,0.197034,0.220118,0.195378,0.26087,0.282675,0.190574,0.268012,0.244415,0.212087,0.278861,0.190769,0.17597,0.193347,0.213058,0.243455,0.264957,0.277198,0.268012,0.214781,0.279699,0.169553,0.256906,0.216279,0.214533,0.181463,0.167116,0.299517,0.244094,0.235741,0.236641,0.222488,0.185629,0.278861,0.198083,0.186373,0.186373,0.186373,0.186373,0.186373,0.185444,0.213548,0.197034,0.266476,0.220118,0.177481,0.185444,0.249664,0.210884,0.147036,0.281392,0.213303,0.205525,0.124248,0.124916,0.234848,0.169245,0.210646,0.212571,0.220379,0.226277,0.217544,0.215777,0.213548,0.218054,0.273932,0.141016,0.141016,0.141016,0.141016,0.215527,0.17971,0.240621,0.215527,0.216028,0.172063,0.214781,0.207821,0.273529,0.185444,0.203057,0.233962,0.228221,0.197243,0.128187,0.14242,0.212571,0.244094,0.14242,0.195584,0.150121,0.150121,0.20852,0.21934,0.208287,0.150121,0.168022,0.216279,0.207589,0.213058,0.214039,0.217036,0.209459,0.275964,0.21831,0.208754,0.262712,0.236041,0.209932,0.282675,0.216279,0.216028,0.216279,0.207821,0.207589,0.221165,0.210169,0.207821,0.217544,0.218054,0.207589,0.209696,0.208287,0.227106,0.210407,0.209459,0.212571,0.208054,0.215527,0.20852,0.208989,0.207589,0.156962,0.202394,0.246358,0.211364,0.178846,0.204171,0.207589,0.204171,0.207589,0.204171,0.206208,0.212329,0.204171,0.207589,0.207589,0.140166,0.126962,0.174484,0.16504,0.160622,0.13891,0.124332,0.152334,0.13891,0.13891,0.152709,0.155909,0.13891,0.174321,0.154871,0.183794,0.17782,0.152584,0.13891,0.152584,0.152961,0.17033,0.17033,0.173669,0.175472,0.153592,0.127397,0.155909],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var18_df_closseness, by=list(var18_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var18_df_closseness, by=list(var18_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-641197b67fc6d43747ae" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-641197b67fc6d43747ae">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000122\" data-max=\"0.000129\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000471\" data-max=\"0.000857\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000118\" data-max=\"0.000369\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000963\" data-max=\"0.001439\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.4e-05\" data-max=\"0.000201\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022606\" data-max=\"0.023942\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"9.2e-05\" data-max=\"0.002081\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.087614\" data-max=\"0.159386\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.021859\" data-max=\"0.068598\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.179202\" data-max=\"0.267622\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.009952\" data-max=\"0.037333\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172406\" data-max=\"0.308329\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.029295\" data-max=\"0.135782\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[0.000129,0.000127,0.000122],[1e-06,1e-06,1.1e-05],[0.000857,0.000704,0.000471],[0.000118,0.000239,0.000369],[0.001439,0.001164,0.000963],[5.4e-05,0.000162,0.000201],[0.023942,0.023592,0.022606],[9.2e-05,0.000232,0.002081],[0.159386,0.130907,0.087614],[0.021859,0.044377,0.068598],[0.267622,0.216454,0.179202],[0.009952,0.030071,0.037333],[0.308329,0.266869,0.172406],[0.029295,0.082605,0.135782]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var18_data.RData")
```

