# SNA Closeness 11_ACESSO C) Este serviço disponibiliza informações sobre suas ativades para o seu serviço. (var9)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 11_ACESSO C) Este serviço disponibiliza informações sobre suas ativades para o seu serviço. (var9)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var9_data.RData")
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
#var9<-simplify(var9) #Simplify
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
V(var9)$incloseness <- closeness(var9, mode = "in", weights = E(var9)$var9) %>% round(6)
V(var9)$outcloseness <- closeness(var9, mode = "out", weights = E(var9)$var9) %>% round(6)
V(var9)$totalcloseness <- closeness(var9, mode = "total", weights = E(var9)$var9) %>% round(4)
```

###Saving to Environment

```r
var9_incloseness<- closeness(var9, mode = "in", weights = E(var9)$var9) %>% round(6)
var9_outcloseness<- closeness(var9, mode = "out", weights = E(var9)$var9) %>% round(6)
var9_totalcloseness<- closeness(var9, mode = "total", weights = E(var9)$var9) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var9_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0001265 0.0001280 0.0001265 0.0001290 0.0001330
```

```r
sd(var9_incloseness)
```

```
## [1] 7.807065e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var9)$incloseness<-closeness(var9, weights = E(var9)$var9, mode="in")

#Get Variable
V(var9)$var9_color_degree<-round(V(var9)$incloseness,6)

#Creating brewer pallette
vertex_var9_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var9)$var9_color_degree)), "RdBu"))(
            length(unique(V(var9)$var9_color_degree)))

#Saving as Vertex properties 
V(var9)$vertex_var9_color_degree<-
  vertex_var9_color_degree[as.numeric(
  cut(V(var9)$var9_color_degree,
      breaks=length(unique(V(var9)$var9_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var9, es=E(var9), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var9))
maxC <- rep(Inf, vcount(var9))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var9, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var9)$weight)


#PLotting
plot(var9, 
     layout=co,
     edge.color=V(var9)$vertex_var9_color_degree[edge.start],
     edge.arrow.size=closeness(var9, weights = E(var9)$var9, mode="in"),
     edge.width=E(var9)$weight/mean(E(var9)$weight),
     edge.curved = TRUE,
     vertex.color=V(var9)$vertex_var9_color_degree,
     vertex.size=closeness(var9, weights = E(var9)$var9, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var9,"LABEL_COR"),
     vertex.label.cex=(closeness(var9, weights = E(var9)$var9, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var9)$var9_color_degree
b<-V(var9)$vertex_var9_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 11_ACESSO C) Este serviço disponibiliza informações sobre suas ativades para o seu serviço. (var9)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var9, mode="in", weights = E(var9)$var9)), 
             sd(closeness(var9, mode="in", weights = E(var9)$var9))
             )
       )
```

![](11_ACESSO_C_disponibiliza_informações_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var9_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0005900 0.0007990 0.0006955 0.0009600 0.0012890
```

```r
sd(var9_outcloseness)
```

```
## [1] 0.0003704408
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var9)$outcloseness<-closeness(var9, weights = E(var9)$var9, mode="out")

#Get Variable
V(var9)$var9_color_degree<-round(V(var9)$outcloseness,6)

#Creating brewer pallette
vertex_var9_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var9)$var9_color_degree)), "RdBu"))(
            length(unique(V(var9)$var9_color_degree)))

#Saving as Vertex properties 
V(var9)$vertex_var9_color_degree<-
  vertex_var9_color_degree[as.numeric(
  cut(V(var9)$var9_color_degree,
      breaks=length(unique(V(var9)$var9_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var9, es=E(var9), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var9))
maxC <- rep(Inf, vcount(var9))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var9, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var9)$weight)


#PLotting
plot(var9, 
     layout=co,
     edge.color=V(var9)$vertex_var9_color_degree[edge.start],
     edge.arrow.size=closeness(var9, weights = E(var9)$var9, mode="out"),
     edge.width=E(var9)$weight/2*mean(E(var9)$weight),
     edge.curved = TRUE,
     vertex.color=V(var9)$vertex_var9_color_degree,
     vertex.size=closeness(var9, weights = E(var9)$var9, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var9,"LABEL_COR"),
     vertex.label.cex=closeness(var9, weights = E(var9)$var9, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var9)$var9_color_degree
b<-V(var9)$vertex_var9_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 11_ACESSO C) Este serviço disponibiliza informações sobre suas ativades para o seu serviço. (var9)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var9, mode="out", weights = E(var9)$var9)), 
             sd(closeness(var9, mode="out", weights = E(var9)$var9))
             )
       )
```

![](11_ACESSO_C_disponibiliza_informações_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var9_totalcloseness)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000770 0.001206 0.001370 0.001347 0.001543 0.002183
```

```r
sd(var9_totalcloseness)
```

```
## [1] 0.0002894919
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var9)$allcloseness<-closeness(var9, weights = E(var9)$var9, mode="all")

#Get Variable
V(var9)$var9_color_degree<-round(V(var9)$allcloseness,6)

#Creating brewer pallette
vertex_var9_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var9)$var9_color_degree)), "RdBu"))(
            length(unique(V(var9)$var9_color_degree)))

#Saving as Vertex properties 
V(var9)$vertex_var9_color_degree<-
  vertex_var9_color_degree[as.numeric(
  cut(V(var9)$var9_color_degree,
      breaks=length(unique(V(var9)$var9_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var9, es=E(var9), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var9))
maxC <- rep(Inf, vcount(var9))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var9, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var9)$weight)


#PLotting
plot(var9, 
     layout=co,
     edge.color=V(var9)$vertex_var9_color_degree[edge.start],
     edge.arrow.size=closeness(var9, weights = E(var9)$var9, mode="all"),
     edge.width=E(var9)$weight/2*mean(E(var9)$weight),
     edge.curved = TRUE,
     vertex.color=V(var9)$vertex_var9_color_degree,
     vertex.size=closeness(var9, weights = E(var9)$var9, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var9,"LABEL_COR"),
     vertex.label.cex=(closeness(var9, weights = E(var9)$var9, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var9)$var9_color_degree
b<-V(var9)$vertex_var9_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 11_ACESSO C) Este serviço disponibiliza informações sobre suas ativades para o seu serviço. (var9)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var9, mode="all", weights = E(var9)$var9)), 
             sd(closeness(var9, mode="all", weights = E(var9)$var9))
             )
       )
```

![](11_ACESSO_C_disponibiliza_informações_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var9)$incloseness_n <- closeness(var9, mode = "in",, weights = E(var9)$var9, normalized = T) %>% round(10)
V(var9)$outcloseness_n <- closeness(var9, mode = "out", normalized = T, weights = E(var9)$var9) %>% round(6)
V(var9)$totalcloseness_n <- closeness(var9, mode = "total", normalized = T, weights = E(var9)$var9) %>% round(6)
```

###Saving to Environment

```r
var9_incloseness_n<- closeness(var9, mode = "in", normalized = T, weights = E(var9)$var9) %>% round(6)
var9_outcloseness_n<- closeness(var9, mode = "out", normalized = T, weights = E(var9)$var9) %>% round(6)
var9_totalcloseness_n<- closeness(var9, mode = "total", normalized = T, weights = E(var9)$var9) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var9_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.023520 0.023810 0.023520 0.023960 0.024710
```

```r
sd(var9_incloseness_n)
```

```
## [1] 0.001454057
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var9)$incloseness_n<-closeness(var9, weights = E(var9)$var9, mode="in", normalized = T)

#Get Variable
V(var9)$var9_color_degree<-round(V(var9)$incloseness_n,6)

#Creating brewer pallette
vertex_var9_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var9)$var9_color_degree)), "RdBu"))(
            length(unique(V(var9)$var9_color_degree)))

#Saving as Vertex properties 
V(var9)$vertex_var9_color_degree<-
  vertex_var9_color_degree[as.numeric(
  cut(V(var9)$var9_color_degree,
      breaks=length(unique(V(var9)$var9_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var9, es=E(var9), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var9))
maxC <- rep(Inf, vcount(var9))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var9, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var9)$weight)


#PLotting
plot(var9, 
     layout=co,
     edge.color=V(var9)$vertex_var9_color_degree[edge.start],
     edge.arrow.size=closeness(var9, weights = E(var9)$var9, mode="in",normalized = T),
     edge.width=E(var9)$weight/10*mean(E(var9)$weight),
     edge.curved = TRUE,
     vertex.color=V(var9)$vertex_var9_color_degree,
     vertex.size=(closeness(var9, weights = E(var9)$var9, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var9,"LABEL_COR"),
     vertex.label.cex=closeness(var9, weights = E(var9)$var9, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var9)$var9_color_degree
b<-V(var9)$vertex_var9_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 11_ACESSO C) Este serviço disponibiliza informações sobre suas ativades para o seu serviço. (var9)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var9, mode="in", weights = E(var9)$var9, normalized = T)), 
             sd(closeness(var9, mode="in", weights = E(var9)$var9, normalized = T))
             )
       )
```

![](11_ACESSO_C_disponibiliza_informações_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var9_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.109700 0.148600 0.129400 0.178500 0.239700
```

```r
sd(var9_outcloseness_n)
```

```
## [1] 0.06892307
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var9)$outcloseness_n<-closeness(var9, weights = E(var9)$var9, mode="out", normalized = T)

#Get Variable
V(var9)$var9_color_degree<-round(V(var9)$outcloseness_n,6)

#Creating brewer pallette
vertex_var9_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var9)$var9_color_degree)), "RdBu"))(
            length(unique(V(var9)$var9_color_degree)))

#Saving as Vertex properties 
V(var9)$vertex_var9_color_degree<-
  vertex_var9_color_degree[as.numeric(
  cut(V(var9)$var9_color_degree,
      breaks=length(unique(V(var9)$var9_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var9, es=E(var9), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var9))
maxC <- rep(Inf, vcount(var9))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var9, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var9)$weight)


#PLotting
plot(var9, 
     layout=co,
     edge.color=V(var9)$vertex_var9_color_degree[edge.start],
     edge.arrow.size=closeness(var9, weights = E(var9)$var9, mode="out",normalized = T),
     edge.width=E(var9)$weight/10*mean(E(var9)$weight),
     edge.curved = TRUE,
     vertex.color=V(var9)$vertex_var9_color_degree,
     vertex.size=(closeness(var9, weights = E(var9)$var9, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var9,"LABEL_COR"),
     vertex.label.cex=closeness(var9, weights = E(var9)$var9, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var9)$var9_color_degree
b<-V(var9)$vertex_var9_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 11_ACESSO C) Este serviço disponibiliza informações sobre suas ativades para o seu serviço. (var9)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var9, mode="out", weights = E(var9)$var9, normalized = T)), 
             sd(closeness(var9, mode="out", weights = E(var9)$var9, normalized = T))
             )
       )
```

![](11_ACESSO_C_disponibiliza_informações_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var9_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1432  0.2244  0.2548  0.2506  0.2870  0.4061
```

```r
sd(var9_totalcloseness_n)
```

```
## [1] 0.05384958
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var9)$allcloseness_n<-closeness(var9, weights = E(var9)$var9, mode="all", normalized = T)

#Get Variable
V(var9)$var9_color_degree<-round(V(var9)$allcloseness_n,6)

#Creating brewer pallette
vertex_var9_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var9)$var9_color_degree)), "RdBu"))(
            length(unique(V(var9)$var9_color_degree)))

#Saving as Vertex properties 
V(var9)$vertex_var9_color_degree<-
  vertex_var9_color_degree[as.numeric(
  cut(V(var9)$var9_color_degree,
      breaks=length(unique(V(var9)$var9_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var9, es=E(var9), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var9))
maxC <- rep(Inf, vcount(var9))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var9, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var9)$weight)


#PLotting
plot(var9, 
     layout=co,
     edge.color=V(var9)$vertex_var9_color_degree[edge.start],
     edge.arrow.size=closeness(var9, weights = E(var9)$var9, mode="all",normalized = T),
     edge.width=E(var9)$weight/10*mean(E(var9)$weight),
     edge.curved = TRUE,
     vertex.color=V(var9)$vertex_var9_color_degree,
     vertex.size=(closeness(var9, weights = E(var9)$var9, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var9,"LABEL_COR"),
     vertex.label.cex=closeness(var9, weights = E(var9)$var9, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var9)$var9_color_degree
b<-V(var9)$vertex_var9_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 11_ACESSO C) Este serviço disponibiliza informações sobre suas ativades para o seu serviço. (var9)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var9, mode="all", weights = E(var9)$var9, normalized = T)), 
             sd(closeness(var9, mode="all", weights = E(var9)$var9, normalized = T))
             )
       )
```

![](11_ACESSO_C_disponibiliza_informações_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var9)$incloseness_n <- closeness(var9, weights = E(var9)$var9, mode = "in", normalized = T) %>% round(6)
V(var9)$outcloseness_n <- closeness(var9, weights = E(var9)$var9, mode = "out", normalized = T) %>% round(6)
V(var9)$totalcloseness_n <- closeness(var9, weights = E(var9)$var9, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var9)$var9_centr_closeness<- centralization.closeness(var9)$res
var9_centr_closeness<- centralization.closeness(var9)$res
var9_centr_closeness_all<- centralization.closeness(var9)
```

###Centralization

```r
var9_centr_closeness_all$centralization
```

```
## [1] 0.1625197
```

###Theoretical Max

```r
var9_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var9)$var9_centr_closeness<- centralization.closeness(var9)$res

#Get Variable
V(var9)$var9_color_degree<-round(V(var9)$var9_centr_closeness,6)

#Creating brewer pallette
vertex_var9_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var9)$var9_color_degree)), "Spectral"))(
            length(unique(V(var9)$var9_color_degree)))

#Saving as Vertex properties 
V(var9)$vertex_var9_color_degree<-
  vertex_var9_color_degree[as.numeric(
  cut(V(var9)$var9_color_degree,
      breaks=length(unique(V(var9)$var9_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var9, es=E(var9), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var9))
maxC <- rep(Inf, vcount(var9))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var9, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var9)$weight)


#PLotting
plot(var9, 
     layout=co,
     edge.color=V(var9)$vertex_var9_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var9)$res,
     edge.width=E(var9)$weight/10*mean(E(var9)$weight),
     edge.curved = TRUE,
     vertex.color=V(var9)$vertex_var9_color_degree,
     vertex.size=centralization.closeness(var9)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var9,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var9)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var9)$var9_color_degree
b<-V(var9)$vertex_var9_color_degree
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
  title("Network Centralization Closeness - 11_ACESSO C) Este serviço disponibiliza informações sobre suas ativades para o seu serviço. (var9)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var9)$res), 
             sd(centralization.closeness(var9)$res)
             )
       )
```

![](11_ACESSO_C_disponibiliza_informações_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var9_incloseness<- closeness(var9, weights = E(var9)$var9, mode = "in") %>% round(6)
var9_outcloseness<- closeness(var9, weights = E(var9)$var9, mode = "out") %>% round(6)
var9_totalcloseness<- closeness(var9, weights = E(var9)$var9, mode = "total") %>% round(6)
var9_incloseness_n<- closeness(var9,weights = E(var9)$var9, mode = "in", normalized = T) %>% round(6)
var9_outcloseness_n<- closeness(var9,weights = E(var9)$var9, mode = "out", normalized = T) %>% round(6)
var9_totalcloseness_n<- closeness(var9,weights = E(var9)$var9, mode = "total", normalized = T) %>% round(6)
var9_centr_closeness <- centralization.closeness(var9)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var9_df_closseness <- data.frame(
var9_incloseness,
var9_outcloseness,
var9_totalcloseness,
var9_incloseness_n,
var9_outcloseness_n,
var9_totalcloseness_n,
var9_centr_closeness) %>% round(6)

#Adding type
var9_df_closseness <-cbind(var9_df_closseness, V(var9)$LABEL_COR)

#Adding names
names(var9_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var9_df_closseness<-var9_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var9_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-0f543ebe4b9606288905" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-0f543ebe4b9606288905">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001289\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00077\" data-max=\"0.002183\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024708\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.239691\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.143187\" data-max=\"0.406114\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[0.000132,0.00013,0.000133,0.000128,0.00013,0.000129,0.000126,0.000129,0.000127,0.000127,0.00013,0.000129,0.000127,0.000127,0.000128,0.000126,0.000129,0.000131,0.000123,0.000129,0.000129,0.000127,0.000129,0.000121,0.000117,0.000126,0.000123,0.000129,0.000128,0.00013,0.00013,0.000128,0.00013,0.000122,0.000128,0.000128,0.000129,0.000124,0.000124,0.000129,0.00013,0.000127,0.000127,0.000129,0.000128,0.000128,0.000117,0.000127,0.000127,0.000127,0.000127,0.000127,0.000125,0.000129,0.000122,0.000128,0.000129,0.000122,0.000123,0.00013,0.000131,0.00012,0.000128,0.000128,0.000128,2.9e-05,0.000118,0.000128,0.000122,0.000128,0.000129,0.000129,0.000128,0.000128,0.000129,0.000128,0.000128,0.00013,0.000121,0.000121,0.000121,0.000121,0.000128,0.000128,0.000128,0.000129,0.000129,0.000121,0.000128,0.000128,0.000128,0.00012,0.000123,0.000127,0.00013,0.000128,0.000121,0.000127,0.000129,0.00013,0.000121,0.000128,0.000126,0.000126,0.000128,0.000128,0.000128,0.000126,0.000127,0.000128,0.000128,0.000129,0.000129,0.000129,0.000129,0.000128,0.000128,0.000128,0.000128,0.00013,0.000128,0.000129,0.000128,0.000128,0.000128,0.000128,0.000128,0.000128,0.000128,0.000127,0.000128,0.000128,0.000128,0.000128,0.000128,0.000129,0.00013,0.000128,0.000129,0.000128,0.000129,0.000129,0.000128,0.000128,0.000126,0.000129,0.000128,0.000129,0.000128,0.000127,0.000128,0.000127,0.000128,0.000127,0.000128,0.000121,0.00013,0.000131,0.000131,0.000123,0.000121,0.000124,0.000132,0.00013,0.00012,0.000121,0.000126,0.00012,0.00012,0.000129,0.000125,0.00012,0.000129,0.000129,0.00013,0.000131,0.000129,0.00012,0.000129,0.000124,0.000129,0.000129,0.000126,0.000127,0.000124,0.000128,0.000128],[0.001214,0.001164,0.001047,0.001088,0.00082,0.000759,0.000807,0.001192,0.000862,0.000786,0.000736,0.000679,0.001045,0.000873,0.000825,0.000798,0.000943,0.001222,0.000796,0.001085,0.000796,0.001079,0.00108,0.000868,0.000772,0.000834,0.000893,0.001034,0.001289,0.000931,0.000985,0.000814,0.001126,0.000583,0.000529,0.000775,0.000835,0.000705,0.00098,0.001111,0.001209,0.00103,0.001028,0.001161,0.000865,0.001151,0.000789,0.000808,0.000808,0.000808,0.000808,0.000808,0.000895,0.000736,0.001089,0.000853,0.001189,0.000995,0.000724,0.000894,2.9e-05,0.000663,0.00081,0.000727,0.001152,2.9e-05,2.9e-05,0.000822,0.000812,0.000883,0.000883,0.00075,0.001083,0.001007,0.000701,0.000752,0.000752,0.000958,0.000556,0.000556,0.000556,0.000556,0.000742,0.000587,0.000796,0.00094,0.001081,0.001003,0.000985,0.000735,0.001,0.000799,0.001035,0.000711,0.001096,0.001021,0.000646,0.000678,0.000925,0.001152,0.000569,0.000925,2.9e-05,2.9e-05,0.00102,0.00102,0.000714,2.9e-05,2.9e-05,0.001144,0.001078,0.000996,0.000742,0.000845,0.000787,0.00106,0.000819,0.001016,0.001021,0.000799,0.000906,0.000855,0.000665,0.000665,0.000665,0.000861,0.000756,0.001,0.000756,0.000861,0.001015,0.001015,0.00077,0.000664,0.00088,0.000852,0.000695,0.000664,0.000756,0.000962,0.000693,0.000664,0.000885,0.00088,0.000664,0.000664,0.000739,0.000593,0.000726,0.000815,0.000815,0.000815,0.000815,0.000815,0.000582,0.000654,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.002183,0.001832,0.001976,0.001548,0.001558,0.001479,0.001088,0.00188,0.001456,0.001435,0.00158,0.001314,0.00161,0.001422,0.00134,0.001085,0.001529,0.002004,0.001215,0.001757,0.001538,0.001675,0.001706,0.001144,0.001041,0.001124,0.001242,0.001613,0.001938,0.001613,0.001527,0.001245,0.00177,0.000915,0.001242,0.001453,0.001555,0.000965,0.001383,0.001637,0.001859,0.001379,0.001464,0.001754,0.001391,0.001825,0.001088,0.00137,0.00137,0.00137,0.00137,0.00137,0.001309,0.001506,0.001656,0.001364,0.001761,0.001558,0.001026,0.001733,0.001335,0.001076,0.001266,0.001224,0.001724,0.00077,0.000868,0.001504,0.001198,0.001477,0.001605,0.001304,0.001715,0.001479,0.001422,0.001316,0.001256,0.001608,0.000798,0.000798,0.000798,0.000798,0.001235,0.001493,0.001414,0.001412,0.001792,0.00156,0.001355,0.001224,0.001605,0.001056,0.001613,0.001133,0.001692,0.001783,0.000852,0.001395,0.001608,0.001773,0.000805,0.001477,0.000942,0.000942,0.001661,0.001497,0.001225,0.000942,0.000949,0.001776,0.001721,0.001623,0.001357,0.001412,0.001412,0.001742,0.001431,0.001706,0.001634,0.001534,0.001309,0.00156,0.001247,0.001244,0.001247,0.001302,0.001294,0.001468,0.001295,0.001302,0.001587,0.00159,0.001242,0.001241,0.001493,0.001451,0.001488,0.001222,0.00137,0.001412,0.001393,0.001433,0.001517,0.001473,0.000963,0.001393,0.001294,0.001488,0.00149,0.001232,0.001239,0.001232,0.001239,0.001232,0.001376,0.001088,0.001233,0.001241,0.001239,0.00089,0.000867,0.000883,0.001344,0.001016,0.000871,0.000865,0.000984,0.000871,0.000871,0.001245,0.000883,0.000871,0.001307,0.001188,0.001271,0.00141,0.001095,0.000871,0.001095,0.000875,0.001435,0.001435,0.000936,0.001025,0.000876,0.001307,0.000943],[0.024477,0.024106,0.024708,0.023852,0.024115,0.023966,0.023485,0.024037,0.023706,0.02361,0.024128,0.023966,0.023697,0.023616,0.023782,0.023443,0.023994,0.024342,0.022819,0.024019,0.024022,0.023679,0.023926,0.022532,0.021737,0.023361,0.02283,0.023991,0.023822,0.024087,0.024106,0.023782,0.024121,0.022617,0.023846,0.023731,0.023911,0.023014,0.022971,0.024022,0.024106,0.023628,0.023685,0.024003,0.023791,0.023874,0.021854,0.023529,0.023529,0.023529,0.023529,0.023529,0.023311,0.024015,0.022633,0.023855,0.024084,0.022644,0.022952,0.024244,0.024339,0.022326,0.0238,0.023785,0.023746,0.005348,0.021939,0.023849,0.022636,0.023761,0.023932,0.023914,0.023868,0.023877,0.023963,0.023794,0.023813,0.024134,0.022488,0.022488,0.022488,0.022488,0.023825,0.023803,0.023728,0.023938,0.023923,0.022464,0.023791,0.02377,0.023816,0.022369,0.022892,0.023529,0.024112,0.023813,0.022464,0.02361,0.023957,0.02409,0.022491,0.023724,0.023503,0.023503,0.023776,0.023837,0.023791,0.023503,0.023556,0.023874,0.023868,0.023954,0.023907,0.023941,0.023944,0.02381,0.023806,0.023803,0.023898,0.024128,0.023779,0.024031,0.023871,0.023849,0.023871,0.023846,0.023846,0.023852,0.023855,0.023679,0.023831,0.023849,0.023861,0.023849,0.023868,0.024,0.02409,0.023791,0.023917,0.023785,0.023911,0.023911,0.023819,0.023779,0.023364,0.024047,0.023846,0.024084,0.023752,0.023667,0.023834,0.023667,0.023834,0.023667,0.023718,0.022456,0.024241,0.024416,0.024406,0.022969,0.022562,0.023131,0.024551,0.024112,0.022318,0.022426,0.023494,0.022318,0.022318,0.024084,0.023224,0.022318,0.024012,0.024037,0.024156,0.024298,0.023957,0.022318,0.023957,0.023111,0.023985,0.023985,0.023455,0.023592,0.0231,0.023861,0.023883],[0.225728,0.216531,0.194764,0.202394,0.152459,0.14123,0.150121,0.221692,0.160345,0.146226,0.136865,0.126359,0.194357,0.162304,0.153465,0.148444,0.175306,0.227384,0.148089,0.201735,0.148089,0.200647,0.200864,0.161458,0.143629,0.155129,0.166071,0.192347,0.239691,0.173184,0.183251,0.151466,0.209459,0.108455,0.098309,0.144074,0.155388,0.131078,0.182353,0.206667,0.224909,0.191555,0.191161,0.216028,0.1609,0.214039,0.146688,0.150364,0.150364,0.150364,0.150364,0.150364,0.166517,0.136865,0.202614,0.158568,0.221165,0.185075,0.134588,0.16622,0.005348,0.12326,0.150729,0.135174,0.214286,0.005376,0.005405,0.152961,0.151097,0.164166,0.164166,0.13943,0.201517,0.187311,0.130435,0.139955,0.13985,0.178161,0.103333,0.103333,0.103333,0.103333,0.137982,0.109155,0.147971,0.174812,0.201081,0.18656,0.183251,0.136664,0.186,0.148562,0.192547,0.132196,0.203947,0.18999,0.120077,0.126102,0.172063,0.214286,0.105802,0.172063,0.005348,0.005348,0.189796,0.189796,0.132857,0.005348,0.005348,0.212815,0.200431,0.185259,0.137982,0.157227,0.146457,0.197243,0.152334,0.189024,0.18999,0.148681,0.168478,0.158974,0.123752,0.123752,0.123752,0.160207,0.140696,0.186,0.140696,0.160207,0.188832,0.188832,0.143297,0.123424,0.163588,0.158433,0.129256,0.123424,0.140696,0.178846,0.128988,0.123424,0.164602,0.163588,0.123424,0.123424,0.137371,0.11032,0.135076,0.151589,0.151589,0.151589,0.151589,0.151589,0.108265,0.121569,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.406114,0.340659,0.367589,0.287926,0.28972,0.275148,0.202394,0.349624,0.270742,0.266858,0.293839,0.244415,0.299517,0.26458,0.24933,0.201735,0.284404,0.372745,0.226002,0.326889,0.286154,0.311558,0.317406,0.212815,0.193548,0.208989,0.231056,0.3,0.360465,0.3,0.283969,0.231631,0.329204,0.170174,0.231056,0.270349,0.289269,0.179537,0.257261,0.304419,0.345725,0.256552,0.272328,0.326316,0.258693,0.339416,0.202394,0.254795,0.254795,0.254795,0.254795,0.254795,0.243455,0.28012,0.307947,0.253752,0.327465,0.28972,0.190769,0.322357,0.248331,0.200215,0.235443,0.227662,0.32069,0.143187,0.161458,0.279699,0.222754,0.274742,0.298555,0.242503,0.319039,0.275148,0.26458,0.244737,0.233668,0.299035,0.148444,0.148444,0.148444,0.148444,0.22963,0.277612,0.263083,0.262712,0.333333,0.290172,0.252033,0.227662,0.298555,0.19641,0.3,0.210646,0.314721,0.331551,0.158433,0.259414,0.299035,0.329787,0.149638,0.274742,0.175141,0.175141,0.30897,0.278443,0.227941,0.175141,0.176471,0.330373,0.320138,0.301948,0.252374,0.262712,0.262712,0.324042,0.266094,0.317406,0.303922,0.285276,0.243455,0.290172,0.23192,0.231343,0.23192,0.242188,0.240621,0.273128,0.240933,0.242188,0.295238,0.295707,0.231056,0.230769,0.277612,0.269956,0.276786,0.227384,0.254795,0.262712,0.259053,0.266476,0.282246,0.273932,0.179191,0.259053,0.240621,0.276786,0.277198,0.229064,0.230483,0.229064,0.230483,0.229064,0.255846,0.202394,0.229346,0.230769,0.230483,0.16548,0.161179,0.164311,0.25,0.189024,0.162021,0.1609,0.183071,0.162021,0.162021,0.231631,0.164311,0.162021,0.243137,0.220903,0.236341,0.262341,0.203724,0.162021,0.203724,0.16273,0.266858,0.266858,0.174157,0.190574,0.163015,0.243137,0.175306],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var9_df_closseness, by=list(var9_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var9_df_closseness, by=list(var9_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-c69ec5d79156d58f302e" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-c69ec5d79156d58f302e">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000121\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001222\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"9.4e-05\" data-max=\"0.000392\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000943\" data-max=\"0.002183\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3e-05\" data-max=\"0.000315\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022516\" data-max=\"0.024708\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8e-06\" data-max=\"0.004192\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.227384\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.017365\" data-max=\"0.072933\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.175306\" data-max=\"0.406114\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005677\" data-max=\"0.058594\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003883\" data-max=\"0.139638\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[0.000129,0.000125,0.000129,0.00013,0.000126,0.00013,0.000129,0.000133,0.000129,0.000128,0.00013,0.000121,0.000127,0.000129,0.000129,0.000125,0.000128,0.00013,0.000132,0.000126,0.000131,0.000128,0.000128],[1e-06,4e-06,null,null,null,null,0,null,null,null,null,2.2e-05,0,1e-06,1e-06,4e-06,null,null,null,1e-06,null,1e-06,1e-06],[0.00102,0.000288,0.001085,0.001164,0.000807,0.000894,0.000831,0.001047,0.00108,0.000739,0.000958,0.000876,0.000952,0.000916,0.000841,0.000838,2.9e-05,0.001152,0.001214,0.000553,0.001222,0.000706,0.00086],[0.000258,0.000357,null,null,null,null,0.000188,null,null,null,null,0.000272,0.000111,0.000243,9.4e-05,0.000151,null,null,null,0.000392,null,0.000321,0.000146],[0.001706,0.00109,0.001757,0.001832,0.001088,0.001733,0.001515,0.001976,0.001706,0.001294,0.001608,0.001464,0.0014,0.00155,0.001522,0.001296,0.000943,0.001773,0.002183,0.001172,0.002004,0.00132,0.001452],[0.000193,0.000239,null,null,null,null,0.000112,null,null,null,null,0.000315,3e-05,0.000228,4e-05,0.000271,null,null,null,0.000213,null,0.000136,0.000166],[0.023993,0.02316,0.024019,0.024106,0.023485,0.024244,0.02402,0.024708,0.023926,0.023846,0.024134,0.022516,0.023622,0.023948,0.024025,0.023286,0.023883,0.02409,0.024477,0.023428,0.024342,0.023897,0.023879],[0.000209,0.000752,null,null,null,null,2.8e-05,null,null,null,null,0.004192,8e-06,0.000126,7.9e-05,0.000755,null,null,null,0.000206,null,0.000222,0.000101],[0.189815,0.053567,0.201735,0.216531,0.150121,0.16622,0.15462,0.194764,0.200864,0.137371,0.178161,0.162987,0.17693,0.170296,0.156332,0.155888,0.005348,0.214286,0.225728,0.102924,0.227384,0.131367,0.159904],[0.04803,0.06639,null,null,null,null,0.034923,null,null,null,null,0.050656,0.020684,0.045143,0.017365,0.02815,null,null,null,0.072933,null,0.059718,0.027168],[0.317376,0.202832,0.326889,0.340659,0.202394,0.322357,0.281736,0.367589,0.317406,0.240621,0.299035,0.272386,0.260566,0.288315,0.283091,0.241017,0.175306,0.329787,0.406114,0.217867,0.372745,0.245573,0.27005],[0.035762,0.044437,null,null,null,null,0.020828,null,null,null,null,0.058594,0.005677,0.042431,0.007374,0.050346,null,null,null,0.039646,null,0.025326,0.030795],[0.300585,0.099063,0.286154,0.343173,0.303922,0.323478,0.300896,0.391579,0.295238,0.29108,0.313659,0.266552,0.319611,0.301223,0.316849,0.295625,0.005348,0.306425,0.371257,0.194167,0.314189,0.234219,0.288627],[0.043014,0.125815,null,null,null,null,0.018146,null,null,null,null,0.070696,0.003883,0.033229,0.029748,0.023781,null,null,null,0.139638,null,0.102732,0.011577]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var9_df_closseness <- data.frame(
var9_incloseness,
var9_outcloseness,
var9_totalcloseness,
var9_incloseness_n,
var9_outcloseness_n,
var9_totalcloseness_n,
var9_centr_closeness) %>% round(6)

#Adding type
var9_df_closseness <-cbind(var9_df_closseness, V(var9)$TIPO1)

#Adding names
names(var9_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var9_df_closseness<-var9_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var9_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-4bf58717659e963e136c" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-4bf58717659e963e136c">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001289\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00077\" data-max=\"0.002183\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024708\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.239691\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.143187\" data-max=\"0.406114\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[0.000132,0.00013,0.000133,0.000128,0.00013,0.000129,0.000126,0.000129,0.000127,0.000127,0.00013,0.000129,0.000127,0.000127,0.000128,0.000126,0.000129,0.000131,0.000123,0.000129,0.000129,0.000127,0.000129,0.000121,0.000117,0.000126,0.000123,0.000129,0.000128,0.00013,0.00013,0.000128,0.00013,0.000122,0.000128,0.000128,0.000129,0.000124,0.000124,0.000129,0.00013,0.000127,0.000127,0.000129,0.000128,0.000128,0.000117,0.000127,0.000127,0.000127,0.000127,0.000127,0.000125,0.000129,0.000122,0.000128,0.000129,0.000122,0.000123,0.00013,0.000131,0.00012,0.000128,0.000128,0.000128,2.9e-05,0.000118,0.000128,0.000122,0.000128,0.000129,0.000129,0.000128,0.000128,0.000129,0.000128,0.000128,0.00013,0.000121,0.000121,0.000121,0.000121,0.000128,0.000128,0.000128,0.000129,0.000129,0.000121,0.000128,0.000128,0.000128,0.00012,0.000123,0.000127,0.00013,0.000128,0.000121,0.000127,0.000129,0.00013,0.000121,0.000128,0.000126,0.000126,0.000128,0.000128,0.000128,0.000126,0.000127,0.000128,0.000128,0.000129,0.000129,0.000129,0.000129,0.000128,0.000128,0.000128,0.000128,0.00013,0.000128,0.000129,0.000128,0.000128,0.000128,0.000128,0.000128,0.000128,0.000128,0.000127,0.000128,0.000128,0.000128,0.000128,0.000128,0.000129,0.00013,0.000128,0.000129,0.000128,0.000129,0.000129,0.000128,0.000128,0.000126,0.000129,0.000128,0.000129,0.000128,0.000127,0.000128,0.000127,0.000128,0.000127,0.000128,0.000121,0.00013,0.000131,0.000131,0.000123,0.000121,0.000124,0.000132,0.00013,0.00012,0.000121,0.000126,0.00012,0.00012,0.000129,0.000125,0.00012,0.000129,0.000129,0.00013,0.000131,0.000129,0.00012,0.000129,0.000124,0.000129,0.000129,0.000126,0.000127,0.000124,0.000128,0.000128],[0.001214,0.001164,0.001047,0.001088,0.00082,0.000759,0.000807,0.001192,0.000862,0.000786,0.000736,0.000679,0.001045,0.000873,0.000825,0.000798,0.000943,0.001222,0.000796,0.001085,0.000796,0.001079,0.00108,0.000868,0.000772,0.000834,0.000893,0.001034,0.001289,0.000931,0.000985,0.000814,0.001126,0.000583,0.000529,0.000775,0.000835,0.000705,0.00098,0.001111,0.001209,0.00103,0.001028,0.001161,0.000865,0.001151,0.000789,0.000808,0.000808,0.000808,0.000808,0.000808,0.000895,0.000736,0.001089,0.000853,0.001189,0.000995,0.000724,0.000894,2.9e-05,0.000663,0.00081,0.000727,0.001152,2.9e-05,2.9e-05,0.000822,0.000812,0.000883,0.000883,0.00075,0.001083,0.001007,0.000701,0.000752,0.000752,0.000958,0.000556,0.000556,0.000556,0.000556,0.000742,0.000587,0.000796,0.00094,0.001081,0.001003,0.000985,0.000735,0.001,0.000799,0.001035,0.000711,0.001096,0.001021,0.000646,0.000678,0.000925,0.001152,0.000569,0.000925,2.9e-05,2.9e-05,0.00102,0.00102,0.000714,2.9e-05,2.9e-05,0.001144,0.001078,0.000996,0.000742,0.000845,0.000787,0.00106,0.000819,0.001016,0.001021,0.000799,0.000906,0.000855,0.000665,0.000665,0.000665,0.000861,0.000756,0.001,0.000756,0.000861,0.001015,0.001015,0.00077,0.000664,0.00088,0.000852,0.000695,0.000664,0.000756,0.000962,0.000693,0.000664,0.000885,0.00088,0.000664,0.000664,0.000739,0.000593,0.000726,0.000815,0.000815,0.000815,0.000815,0.000815,0.000582,0.000654,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.002183,0.001832,0.001976,0.001548,0.001558,0.001479,0.001088,0.00188,0.001456,0.001435,0.00158,0.001314,0.00161,0.001422,0.00134,0.001085,0.001529,0.002004,0.001215,0.001757,0.001538,0.001675,0.001706,0.001144,0.001041,0.001124,0.001242,0.001613,0.001938,0.001613,0.001527,0.001245,0.00177,0.000915,0.001242,0.001453,0.001555,0.000965,0.001383,0.001637,0.001859,0.001379,0.001464,0.001754,0.001391,0.001825,0.001088,0.00137,0.00137,0.00137,0.00137,0.00137,0.001309,0.001506,0.001656,0.001364,0.001761,0.001558,0.001026,0.001733,0.001335,0.001076,0.001266,0.001224,0.001724,0.00077,0.000868,0.001504,0.001198,0.001477,0.001605,0.001304,0.001715,0.001479,0.001422,0.001316,0.001256,0.001608,0.000798,0.000798,0.000798,0.000798,0.001235,0.001493,0.001414,0.001412,0.001792,0.00156,0.001355,0.001224,0.001605,0.001056,0.001613,0.001133,0.001692,0.001783,0.000852,0.001395,0.001608,0.001773,0.000805,0.001477,0.000942,0.000942,0.001661,0.001497,0.001225,0.000942,0.000949,0.001776,0.001721,0.001623,0.001357,0.001412,0.001412,0.001742,0.001431,0.001706,0.001634,0.001534,0.001309,0.00156,0.001247,0.001244,0.001247,0.001302,0.001294,0.001468,0.001295,0.001302,0.001587,0.00159,0.001242,0.001241,0.001493,0.001451,0.001488,0.001222,0.00137,0.001412,0.001393,0.001433,0.001517,0.001473,0.000963,0.001393,0.001294,0.001488,0.00149,0.001232,0.001239,0.001232,0.001239,0.001232,0.001376,0.001088,0.001233,0.001241,0.001239,0.00089,0.000867,0.000883,0.001344,0.001016,0.000871,0.000865,0.000984,0.000871,0.000871,0.001245,0.000883,0.000871,0.001307,0.001188,0.001271,0.00141,0.001095,0.000871,0.001095,0.000875,0.001435,0.001435,0.000936,0.001025,0.000876,0.001307,0.000943],[0.024477,0.024106,0.024708,0.023852,0.024115,0.023966,0.023485,0.024037,0.023706,0.02361,0.024128,0.023966,0.023697,0.023616,0.023782,0.023443,0.023994,0.024342,0.022819,0.024019,0.024022,0.023679,0.023926,0.022532,0.021737,0.023361,0.02283,0.023991,0.023822,0.024087,0.024106,0.023782,0.024121,0.022617,0.023846,0.023731,0.023911,0.023014,0.022971,0.024022,0.024106,0.023628,0.023685,0.024003,0.023791,0.023874,0.021854,0.023529,0.023529,0.023529,0.023529,0.023529,0.023311,0.024015,0.022633,0.023855,0.024084,0.022644,0.022952,0.024244,0.024339,0.022326,0.0238,0.023785,0.023746,0.005348,0.021939,0.023849,0.022636,0.023761,0.023932,0.023914,0.023868,0.023877,0.023963,0.023794,0.023813,0.024134,0.022488,0.022488,0.022488,0.022488,0.023825,0.023803,0.023728,0.023938,0.023923,0.022464,0.023791,0.02377,0.023816,0.022369,0.022892,0.023529,0.024112,0.023813,0.022464,0.02361,0.023957,0.02409,0.022491,0.023724,0.023503,0.023503,0.023776,0.023837,0.023791,0.023503,0.023556,0.023874,0.023868,0.023954,0.023907,0.023941,0.023944,0.02381,0.023806,0.023803,0.023898,0.024128,0.023779,0.024031,0.023871,0.023849,0.023871,0.023846,0.023846,0.023852,0.023855,0.023679,0.023831,0.023849,0.023861,0.023849,0.023868,0.024,0.02409,0.023791,0.023917,0.023785,0.023911,0.023911,0.023819,0.023779,0.023364,0.024047,0.023846,0.024084,0.023752,0.023667,0.023834,0.023667,0.023834,0.023667,0.023718,0.022456,0.024241,0.024416,0.024406,0.022969,0.022562,0.023131,0.024551,0.024112,0.022318,0.022426,0.023494,0.022318,0.022318,0.024084,0.023224,0.022318,0.024012,0.024037,0.024156,0.024298,0.023957,0.022318,0.023957,0.023111,0.023985,0.023985,0.023455,0.023592,0.0231,0.023861,0.023883],[0.225728,0.216531,0.194764,0.202394,0.152459,0.14123,0.150121,0.221692,0.160345,0.146226,0.136865,0.126359,0.194357,0.162304,0.153465,0.148444,0.175306,0.227384,0.148089,0.201735,0.148089,0.200647,0.200864,0.161458,0.143629,0.155129,0.166071,0.192347,0.239691,0.173184,0.183251,0.151466,0.209459,0.108455,0.098309,0.144074,0.155388,0.131078,0.182353,0.206667,0.224909,0.191555,0.191161,0.216028,0.1609,0.214039,0.146688,0.150364,0.150364,0.150364,0.150364,0.150364,0.166517,0.136865,0.202614,0.158568,0.221165,0.185075,0.134588,0.16622,0.005348,0.12326,0.150729,0.135174,0.214286,0.005376,0.005405,0.152961,0.151097,0.164166,0.164166,0.13943,0.201517,0.187311,0.130435,0.139955,0.13985,0.178161,0.103333,0.103333,0.103333,0.103333,0.137982,0.109155,0.147971,0.174812,0.201081,0.18656,0.183251,0.136664,0.186,0.148562,0.192547,0.132196,0.203947,0.18999,0.120077,0.126102,0.172063,0.214286,0.105802,0.172063,0.005348,0.005348,0.189796,0.189796,0.132857,0.005348,0.005348,0.212815,0.200431,0.185259,0.137982,0.157227,0.146457,0.197243,0.152334,0.189024,0.18999,0.148681,0.168478,0.158974,0.123752,0.123752,0.123752,0.160207,0.140696,0.186,0.140696,0.160207,0.188832,0.188832,0.143297,0.123424,0.163588,0.158433,0.129256,0.123424,0.140696,0.178846,0.128988,0.123424,0.164602,0.163588,0.123424,0.123424,0.137371,0.11032,0.135076,0.151589,0.151589,0.151589,0.151589,0.151589,0.108265,0.121569,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.406114,0.340659,0.367589,0.287926,0.28972,0.275148,0.202394,0.349624,0.270742,0.266858,0.293839,0.244415,0.299517,0.26458,0.24933,0.201735,0.284404,0.372745,0.226002,0.326889,0.286154,0.311558,0.317406,0.212815,0.193548,0.208989,0.231056,0.3,0.360465,0.3,0.283969,0.231631,0.329204,0.170174,0.231056,0.270349,0.289269,0.179537,0.257261,0.304419,0.345725,0.256552,0.272328,0.326316,0.258693,0.339416,0.202394,0.254795,0.254795,0.254795,0.254795,0.254795,0.243455,0.28012,0.307947,0.253752,0.327465,0.28972,0.190769,0.322357,0.248331,0.200215,0.235443,0.227662,0.32069,0.143187,0.161458,0.279699,0.222754,0.274742,0.298555,0.242503,0.319039,0.275148,0.26458,0.244737,0.233668,0.299035,0.148444,0.148444,0.148444,0.148444,0.22963,0.277612,0.263083,0.262712,0.333333,0.290172,0.252033,0.227662,0.298555,0.19641,0.3,0.210646,0.314721,0.331551,0.158433,0.259414,0.299035,0.329787,0.149638,0.274742,0.175141,0.175141,0.30897,0.278443,0.227941,0.175141,0.176471,0.330373,0.320138,0.301948,0.252374,0.262712,0.262712,0.324042,0.266094,0.317406,0.303922,0.285276,0.243455,0.290172,0.23192,0.231343,0.23192,0.242188,0.240621,0.273128,0.240933,0.242188,0.295238,0.295707,0.231056,0.230769,0.277612,0.269956,0.276786,0.227384,0.254795,0.262712,0.259053,0.266476,0.282246,0.273932,0.179191,0.259053,0.240621,0.276786,0.277198,0.229064,0.230483,0.229064,0.230483,0.229064,0.255846,0.202394,0.229346,0.230769,0.230483,0.16548,0.161179,0.164311,0.25,0.189024,0.162021,0.1609,0.183071,0.162021,0.162021,0.231631,0.164311,0.162021,0.243137,0.220903,0.236341,0.262341,0.203724,0.162021,0.203724,0.16273,0.266858,0.266858,0.174157,0.190574,0.163015,0.243137,0.175306],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var9_df_closseness, by=list(var9_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var9_df_closseness, by=list(var9_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-01bfe459ef0753673c4d" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-01bfe459ef0753673c4d">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000124\" data-max=\"0.000128\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000526\" data-max=\"0.000819\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000273\" data-max=\"0.000418\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001225\" data-max=\"0.001437\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000239\" data-max=\"0.000308\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.02305\" data-max=\"0.02387\" data-scale=\"5\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000246\" data-max=\"0.002137\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.097892\" data-max=\"0.15237\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.050778\" data-max=\"0.077813\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.227806\" data-max=\"0.267281\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.044461\" data-max=\"0.057373\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.173776\" data-max=\"0.2725\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.079091\" data-max=\"0.135432\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[0.000128,0.000124],[1e-06,1.1e-05],[0.000819,0.000526],[0.000273,0.000418],[0.001437,0.001225],[0.000239,0.000308],[0.02387,0.02305],[0.000246,0.002137],[0.15237,0.097892],[0.050778,0.077813],[0.267281,0.227806],[0.044461,0.057373],[0.2725,0.173776],[0.079091,0.135432]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var9_df_closseness <- data.frame(
var9_incloseness,
var9_outcloseness,
var9_totalcloseness,
var9_incloseness_n,
var9_outcloseness_n,
var9_totalcloseness_n,
var9_centr_closeness) %>% round(6)

#Adding type
var9_df_closseness <-cbind(var9_df_closseness, V(var9)$TIPO2)

#Adding names
names(var9_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var9_df_closseness<-var9_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var9_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-d4121ad726b0072c3240" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-d4121ad726b0072c3240">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001289\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00077\" data-max=\"0.002183\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024708\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.239691\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.143187\" data-max=\"0.406114\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[0.000132,0.00013,0.000133,0.000128,0.00013,0.000129,0.000126,0.000129,0.000127,0.000127,0.00013,0.000129,0.000127,0.000127,0.000128,0.000126,0.000129,0.000131,0.000123,0.000129,0.000129,0.000127,0.000129,0.000121,0.000117,0.000126,0.000123,0.000129,0.000128,0.00013,0.00013,0.000128,0.00013,0.000122,0.000128,0.000128,0.000129,0.000124,0.000124,0.000129,0.00013,0.000127,0.000127,0.000129,0.000128,0.000128,0.000117,0.000127,0.000127,0.000127,0.000127,0.000127,0.000125,0.000129,0.000122,0.000128,0.000129,0.000122,0.000123,0.00013,0.000131,0.00012,0.000128,0.000128,0.000128,2.9e-05,0.000118,0.000128,0.000122,0.000128,0.000129,0.000129,0.000128,0.000128,0.000129,0.000128,0.000128,0.00013,0.000121,0.000121,0.000121,0.000121,0.000128,0.000128,0.000128,0.000129,0.000129,0.000121,0.000128,0.000128,0.000128,0.00012,0.000123,0.000127,0.00013,0.000128,0.000121,0.000127,0.000129,0.00013,0.000121,0.000128,0.000126,0.000126,0.000128,0.000128,0.000128,0.000126,0.000127,0.000128,0.000128,0.000129,0.000129,0.000129,0.000129,0.000128,0.000128,0.000128,0.000128,0.00013,0.000128,0.000129,0.000128,0.000128,0.000128,0.000128,0.000128,0.000128,0.000128,0.000127,0.000128,0.000128,0.000128,0.000128,0.000128,0.000129,0.00013,0.000128,0.000129,0.000128,0.000129,0.000129,0.000128,0.000128,0.000126,0.000129,0.000128,0.000129,0.000128,0.000127,0.000128,0.000127,0.000128,0.000127,0.000128,0.000121,0.00013,0.000131,0.000131,0.000123,0.000121,0.000124,0.000132,0.00013,0.00012,0.000121,0.000126,0.00012,0.00012,0.000129,0.000125,0.00012,0.000129,0.000129,0.00013,0.000131,0.000129,0.00012,0.000129,0.000124,0.000129,0.000129,0.000126,0.000127,0.000124,0.000128,0.000128],[0.001214,0.001164,0.001047,0.001088,0.00082,0.000759,0.000807,0.001192,0.000862,0.000786,0.000736,0.000679,0.001045,0.000873,0.000825,0.000798,0.000943,0.001222,0.000796,0.001085,0.000796,0.001079,0.00108,0.000868,0.000772,0.000834,0.000893,0.001034,0.001289,0.000931,0.000985,0.000814,0.001126,0.000583,0.000529,0.000775,0.000835,0.000705,0.00098,0.001111,0.001209,0.00103,0.001028,0.001161,0.000865,0.001151,0.000789,0.000808,0.000808,0.000808,0.000808,0.000808,0.000895,0.000736,0.001089,0.000853,0.001189,0.000995,0.000724,0.000894,2.9e-05,0.000663,0.00081,0.000727,0.001152,2.9e-05,2.9e-05,0.000822,0.000812,0.000883,0.000883,0.00075,0.001083,0.001007,0.000701,0.000752,0.000752,0.000958,0.000556,0.000556,0.000556,0.000556,0.000742,0.000587,0.000796,0.00094,0.001081,0.001003,0.000985,0.000735,0.001,0.000799,0.001035,0.000711,0.001096,0.001021,0.000646,0.000678,0.000925,0.001152,0.000569,0.000925,2.9e-05,2.9e-05,0.00102,0.00102,0.000714,2.9e-05,2.9e-05,0.001144,0.001078,0.000996,0.000742,0.000845,0.000787,0.00106,0.000819,0.001016,0.001021,0.000799,0.000906,0.000855,0.000665,0.000665,0.000665,0.000861,0.000756,0.001,0.000756,0.000861,0.001015,0.001015,0.00077,0.000664,0.00088,0.000852,0.000695,0.000664,0.000756,0.000962,0.000693,0.000664,0.000885,0.00088,0.000664,0.000664,0.000739,0.000593,0.000726,0.000815,0.000815,0.000815,0.000815,0.000815,0.000582,0.000654,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.002183,0.001832,0.001976,0.001548,0.001558,0.001479,0.001088,0.00188,0.001456,0.001435,0.00158,0.001314,0.00161,0.001422,0.00134,0.001085,0.001529,0.002004,0.001215,0.001757,0.001538,0.001675,0.001706,0.001144,0.001041,0.001124,0.001242,0.001613,0.001938,0.001613,0.001527,0.001245,0.00177,0.000915,0.001242,0.001453,0.001555,0.000965,0.001383,0.001637,0.001859,0.001379,0.001464,0.001754,0.001391,0.001825,0.001088,0.00137,0.00137,0.00137,0.00137,0.00137,0.001309,0.001506,0.001656,0.001364,0.001761,0.001558,0.001026,0.001733,0.001335,0.001076,0.001266,0.001224,0.001724,0.00077,0.000868,0.001504,0.001198,0.001477,0.001605,0.001304,0.001715,0.001479,0.001422,0.001316,0.001256,0.001608,0.000798,0.000798,0.000798,0.000798,0.001235,0.001493,0.001414,0.001412,0.001792,0.00156,0.001355,0.001224,0.001605,0.001056,0.001613,0.001133,0.001692,0.001783,0.000852,0.001395,0.001608,0.001773,0.000805,0.001477,0.000942,0.000942,0.001661,0.001497,0.001225,0.000942,0.000949,0.001776,0.001721,0.001623,0.001357,0.001412,0.001412,0.001742,0.001431,0.001706,0.001634,0.001534,0.001309,0.00156,0.001247,0.001244,0.001247,0.001302,0.001294,0.001468,0.001295,0.001302,0.001587,0.00159,0.001242,0.001241,0.001493,0.001451,0.001488,0.001222,0.00137,0.001412,0.001393,0.001433,0.001517,0.001473,0.000963,0.001393,0.001294,0.001488,0.00149,0.001232,0.001239,0.001232,0.001239,0.001232,0.001376,0.001088,0.001233,0.001241,0.001239,0.00089,0.000867,0.000883,0.001344,0.001016,0.000871,0.000865,0.000984,0.000871,0.000871,0.001245,0.000883,0.000871,0.001307,0.001188,0.001271,0.00141,0.001095,0.000871,0.001095,0.000875,0.001435,0.001435,0.000936,0.001025,0.000876,0.001307,0.000943],[0.024477,0.024106,0.024708,0.023852,0.024115,0.023966,0.023485,0.024037,0.023706,0.02361,0.024128,0.023966,0.023697,0.023616,0.023782,0.023443,0.023994,0.024342,0.022819,0.024019,0.024022,0.023679,0.023926,0.022532,0.021737,0.023361,0.02283,0.023991,0.023822,0.024087,0.024106,0.023782,0.024121,0.022617,0.023846,0.023731,0.023911,0.023014,0.022971,0.024022,0.024106,0.023628,0.023685,0.024003,0.023791,0.023874,0.021854,0.023529,0.023529,0.023529,0.023529,0.023529,0.023311,0.024015,0.022633,0.023855,0.024084,0.022644,0.022952,0.024244,0.024339,0.022326,0.0238,0.023785,0.023746,0.005348,0.021939,0.023849,0.022636,0.023761,0.023932,0.023914,0.023868,0.023877,0.023963,0.023794,0.023813,0.024134,0.022488,0.022488,0.022488,0.022488,0.023825,0.023803,0.023728,0.023938,0.023923,0.022464,0.023791,0.02377,0.023816,0.022369,0.022892,0.023529,0.024112,0.023813,0.022464,0.02361,0.023957,0.02409,0.022491,0.023724,0.023503,0.023503,0.023776,0.023837,0.023791,0.023503,0.023556,0.023874,0.023868,0.023954,0.023907,0.023941,0.023944,0.02381,0.023806,0.023803,0.023898,0.024128,0.023779,0.024031,0.023871,0.023849,0.023871,0.023846,0.023846,0.023852,0.023855,0.023679,0.023831,0.023849,0.023861,0.023849,0.023868,0.024,0.02409,0.023791,0.023917,0.023785,0.023911,0.023911,0.023819,0.023779,0.023364,0.024047,0.023846,0.024084,0.023752,0.023667,0.023834,0.023667,0.023834,0.023667,0.023718,0.022456,0.024241,0.024416,0.024406,0.022969,0.022562,0.023131,0.024551,0.024112,0.022318,0.022426,0.023494,0.022318,0.022318,0.024084,0.023224,0.022318,0.024012,0.024037,0.024156,0.024298,0.023957,0.022318,0.023957,0.023111,0.023985,0.023985,0.023455,0.023592,0.0231,0.023861,0.023883],[0.225728,0.216531,0.194764,0.202394,0.152459,0.14123,0.150121,0.221692,0.160345,0.146226,0.136865,0.126359,0.194357,0.162304,0.153465,0.148444,0.175306,0.227384,0.148089,0.201735,0.148089,0.200647,0.200864,0.161458,0.143629,0.155129,0.166071,0.192347,0.239691,0.173184,0.183251,0.151466,0.209459,0.108455,0.098309,0.144074,0.155388,0.131078,0.182353,0.206667,0.224909,0.191555,0.191161,0.216028,0.1609,0.214039,0.146688,0.150364,0.150364,0.150364,0.150364,0.150364,0.166517,0.136865,0.202614,0.158568,0.221165,0.185075,0.134588,0.16622,0.005348,0.12326,0.150729,0.135174,0.214286,0.005376,0.005405,0.152961,0.151097,0.164166,0.164166,0.13943,0.201517,0.187311,0.130435,0.139955,0.13985,0.178161,0.103333,0.103333,0.103333,0.103333,0.137982,0.109155,0.147971,0.174812,0.201081,0.18656,0.183251,0.136664,0.186,0.148562,0.192547,0.132196,0.203947,0.18999,0.120077,0.126102,0.172063,0.214286,0.105802,0.172063,0.005348,0.005348,0.189796,0.189796,0.132857,0.005348,0.005348,0.212815,0.200431,0.185259,0.137982,0.157227,0.146457,0.197243,0.152334,0.189024,0.18999,0.148681,0.168478,0.158974,0.123752,0.123752,0.123752,0.160207,0.140696,0.186,0.140696,0.160207,0.188832,0.188832,0.143297,0.123424,0.163588,0.158433,0.129256,0.123424,0.140696,0.178846,0.128988,0.123424,0.164602,0.163588,0.123424,0.123424,0.137371,0.11032,0.135076,0.151589,0.151589,0.151589,0.151589,0.151589,0.108265,0.121569,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.406114,0.340659,0.367589,0.287926,0.28972,0.275148,0.202394,0.349624,0.270742,0.266858,0.293839,0.244415,0.299517,0.26458,0.24933,0.201735,0.284404,0.372745,0.226002,0.326889,0.286154,0.311558,0.317406,0.212815,0.193548,0.208989,0.231056,0.3,0.360465,0.3,0.283969,0.231631,0.329204,0.170174,0.231056,0.270349,0.289269,0.179537,0.257261,0.304419,0.345725,0.256552,0.272328,0.326316,0.258693,0.339416,0.202394,0.254795,0.254795,0.254795,0.254795,0.254795,0.243455,0.28012,0.307947,0.253752,0.327465,0.28972,0.190769,0.322357,0.248331,0.200215,0.235443,0.227662,0.32069,0.143187,0.161458,0.279699,0.222754,0.274742,0.298555,0.242503,0.319039,0.275148,0.26458,0.244737,0.233668,0.299035,0.148444,0.148444,0.148444,0.148444,0.22963,0.277612,0.263083,0.262712,0.333333,0.290172,0.252033,0.227662,0.298555,0.19641,0.3,0.210646,0.314721,0.331551,0.158433,0.259414,0.299035,0.329787,0.149638,0.274742,0.175141,0.175141,0.30897,0.278443,0.227941,0.175141,0.176471,0.330373,0.320138,0.301948,0.252374,0.262712,0.262712,0.324042,0.266094,0.317406,0.303922,0.285276,0.243455,0.290172,0.23192,0.231343,0.23192,0.242188,0.240621,0.273128,0.240933,0.242188,0.295238,0.295707,0.231056,0.230769,0.277612,0.269956,0.276786,0.227384,0.254795,0.262712,0.259053,0.266476,0.282246,0.273932,0.179191,0.259053,0.240621,0.276786,0.277198,0.229064,0.230483,0.229064,0.230483,0.229064,0.255846,0.202394,0.229346,0.230769,0.230483,0.16548,0.161179,0.164311,0.25,0.189024,0.162021,0.1609,0.183071,0.162021,0.162021,0.231631,0.164311,0.162021,0.243137,0.220903,0.236341,0.262341,0.203724,0.162021,0.203724,0.16273,0.266858,0.266858,0.174157,0.190574,0.163015,0.243137,0.175306],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var9_df_closseness, by=list(var9_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var9_df_closseness, by=list(var9_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-fee24db7dc02563248b9" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-fee24db7dc02563248b9">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000124\" data-max=\"0.000129\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000522\" data-max=\"0.000937\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000212\" data-max=\"0.000418\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00122\" data-max=\"0.001584\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000199\" data-max=\"0.000305\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.023035\" data-max=\"0.023998\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000122\" data-max=\"0.002147\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.097089\" data-max=\"0.174279\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.039489\" data-max=\"0.077779\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.226905\" data-max=\"0.294538\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.036932\" data-max=\"0.056764\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172406\" data-max=\"0.308329\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.029295\" data-max=\"0.135782\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[0.000129,0.000128,0.000124],[1e-06,1e-06,1.2e-05],[0.000937,0.000801,0.000522],[0.000212,0.000278,0.000418],[0.001584,0.001415,0.00122],[0.000199,0.000239,0.000305],[0.023998,0.023853,0.023035],[0.000122,0.000254,0.002147],[0.174279,0.14896,0.097089],[0.039489,0.051673,0.077779],[0.294538,0.263259,0.226905],[0.036932,0.044522,0.056764],[0.308329,0.266869,0.172406],[0.029295,0.082605,0.135782]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var9_data.RData")
```

