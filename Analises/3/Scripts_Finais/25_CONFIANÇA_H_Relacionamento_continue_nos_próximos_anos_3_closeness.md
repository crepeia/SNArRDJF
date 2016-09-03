# SNA Closeness 25_CONFIANÇA H) Espera que o relacionamento com este serviço continue nos próximos anos (var23).
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 25_CONFIANÇA H) Espera que o relacionamento com este serviço continue nos próximos anos (var23).

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var23_data.RData")
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
#var23<-simplify(var23) #Simplify
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
V(var23)$incloseness <- closeness(var23, mode = "in", weights = E(var23)$var23) %>% round(6)
V(var23)$outcloseness <- closeness(var23, mode = "out", weights = E(var23)$var23) %>% round(6)
V(var23)$totalcloseness <- closeness(var23, mode = "total", weights = E(var23)$var23) %>% round(4)
```

###Saving to Environment

```r
var23_incloseness<- closeness(var23, mode = "in", weights = E(var23)$var23) %>% round(6)
var23_outcloseness<- closeness(var23, mode = "out", weights = E(var23)$var23) %>% round(6)
var23_totalcloseness<- closeness(var23, mode = "total", weights = E(var23)$var23) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var23_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0001210 0.0001240 0.0001223 0.0001250 0.0001300
```

```r
sd(var23_incloseness)
```

```
## [1] 7.639659e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var23)$incloseness<-closeness(var23, weights = E(var23)$var23, mode="in")

#Get Variable
V(var23)$var23_color_degree<-round(V(var23)$incloseness,6)

#Creating brewer pallette
vertex_var23_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var23)$var23_color_degree)), "RdBu"))(
            length(unique(V(var23)$var23_color_degree)))

#Saving as Vertex properties 
V(var23)$vertex_var23_color_degree<-
  vertex_var23_color_degree[as.numeric(
  cut(V(var23)$var23_color_degree,
      breaks=length(unique(V(var23)$var23_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var23, es=E(var23), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var23))
maxC <- rep(Inf, vcount(var23))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var23, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var23)$weight)


#PLotting
plot(var23, 
     layout=co,
     edge.color=V(var23)$vertex_var23_color_degree[edge.start],
     edge.arrow.size=closeness(var23, weights = E(var23)$var23, mode="in"),
     edge.width=E(var23)$weight/mean(E(var23)$weight),
     edge.curved = TRUE,
     vertex.color=V(var23)$vertex_var23_color_degree,
     vertex.size=closeness(var23, weights = E(var23)$var23, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var23,"LABEL_COR"),
     vertex.label.cex=(closeness(var23, weights = E(var23)$var23, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var23)$var23_color_degree
b<-V(var23)$vertex_var23_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 25_CONFIANÇA H) Espera que o relacionamento com este serviço continue nos próximos anos (var23).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var23, mode="in", weights = E(var23)$var23)), 
             sd(closeness(var23, mode="in", weights = E(var23)$var23))
             )
       )
```

![](25_CONFIANÇA_H_Relacionamento_continue_nos_próximos_anos_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var23_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0004950 0.0006540 0.0005341 0.0006775 0.0009950
```

```r
sd(var23_outcloseness)
```

```
## [1] 0.0002669632
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var23)$outcloseness<-closeness(var23, weights = E(var23)$var23, mode="out")

#Get Variable
V(var23)$var23_color_degree<-round(V(var23)$outcloseness,6)

#Creating brewer pallette
vertex_var23_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var23)$var23_color_degree)), "RdBu"))(
            length(unique(V(var23)$var23_color_degree)))

#Saving as Vertex properties 
V(var23)$vertex_var23_color_degree<-
  vertex_var23_color_degree[as.numeric(
  cut(V(var23)$var23_color_degree,
      breaks=length(unique(V(var23)$var23_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var23, es=E(var23), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var23))
maxC <- rep(Inf, vcount(var23))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var23, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var23)$weight)


#PLotting
plot(var23, 
     layout=co,
     edge.color=V(var23)$vertex_var23_color_degree[edge.start],
     edge.arrow.size=closeness(var23, weights = E(var23)$var23, mode="out"),
     edge.width=E(var23)$weight/2*mean(E(var23)$weight),
     edge.curved = TRUE,
     vertex.color=V(var23)$vertex_var23_color_degree,
     vertex.size=closeness(var23, weights = E(var23)$var23, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var23,"LABEL_COR"),
     vertex.label.cex=closeness(var23, weights = E(var23)$var23, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var23)$var23_color_degree
b<-V(var23)$vertex_var23_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 25_CONFIANÇA H) Espera que o relacionamento com este serviço continue nos próximos anos (var23).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var23, mode="out", weights = E(var23)$var23)), 
             sd(closeness(var23, mode="out", weights = E(var23)$var23))
             )
       )
```

![](25_CONFIANÇA_H_Relacionamento_continue_nos_próximos_anos_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var23_totalcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0005200 0.0008215 0.0008570 0.0008711 0.0009345 0.0014840
```

```r
sd(var23_totalcloseness)
```

```
## [1] 0.0001467854
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var23)$allcloseness<-closeness(var23, weights = E(var23)$var23, mode="all")

#Get Variable
V(var23)$var23_color_degree<-round(V(var23)$allcloseness,6)

#Creating brewer pallette
vertex_var23_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var23)$var23_color_degree)), "RdBu"))(
            length(unique(V(var23)$var23_color_degree)))

#Saving as Vertex properties 
V(var23)$vertex_var23_color_degree<-
  vertex_var23_color_degree[as.numeric(
  cut(V(var23)$var23_color_degree,
      breaks=length(unique(V(var23)$var23_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var23, es=E(var23), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var23))
maxC <- rep(Inf, vcount(var23))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var23, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var23)$weight)


#PLotting
plot(var23, 
     layout=co,
     edge.color=V(var23)$vertex_var23_color_degree[edge.start],
     edge.arrow.size=closeness(var23, weights = E(var23)$var23, mode="all"),
     edge.width=E(var23)$weight/2*mean(E(var23)$weight),
     edge.curved = TRUE,
     vertex.color=V(var23)$vertex_var23_color_degree,
     vertex.size=closeness(var23, weights = E(var23)$var23, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var23,"LABEL_COR"),
     vertex.label.cex=(closeness(var23, weights = E(var23)$var23, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var23)$var23_color_degree
b<-V(var23)$vertex_var23_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 25_CONFIANÇA H) Espera que o relacionamento com este serviço continue nos próximos anos (var23).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var23, mode="all", weights = E(var23)$var23)), 
             sd(closeness(var23, mode="all", weights = E(var23)$var23))
             )
       )
```

![](25_CONFIANÇA_H_Relacionamento_continue_nos_próximos_anos_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var23)$incloseness_n <- closeness(var23, mode = "in",, weights = E(var23)$var23, normalized = T) %>% round(10)
V(var23)$outcloseness_n <- closeness(var23, mode = "out", normalized = T, weights = E(var23)$var23) %>% round(6)
V(var23)$totalcloseness_n <- closeness(var23, mode = "total", normalized = T, weights = E(var23)$var23) %>% round(6)
```

###Saving to Environment

```r
var23_incloseness_n<- closeness(var23, mode = "in", normalized = T, weights = E(var23)$var23) %>% round(6)
var23_outcloseness_n<- closeness(var23, mode = "out", normalized = T, weights = E(var23)$var23) %>% round(6)
var23_totalcloseness_n<- closeness(var23, mode = "total", normalized = T, weights = E(var23)$var23) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var23_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.022550 0.023140 0.022760 0.023200 0.024240
```

```r
sd(var23_incloseness_n)
```

```
## [1] 0.001422878
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var23)$incloseness_n<-closeness(var23, weights = E(var23)$var23, mode="in", normalized = T)

#Get Variable
V(var23)$var23_color_degree<-round(V(var23)$incloseness_n,6)

#Creating brewer pallette
vertex_var23_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var23)$var23_color_degree)), "RdBu"))(
            length(unique(V(var23)$var23_color_degree)))

#Saving as Vertex properties 
V(var23)$vertex_var23_color_degree<-
  vertex_var23_color_degree[as.numeric(
  cut(V(var23)$var23_color_degree,
      breaks=length(unique(V(var23)$var23_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var23, es=E(var23), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var23))
maxC <- rep(Inf, vcount(var23))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var23, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var23)$weight)


#PLotting
plot(var23, 
     layout=co,
     edge.color=V(var23)$vertex_var23_color_degree[edge.start],
     edge.arrow.size=closeness(var23, weights = E(var23)$var23, mode="in",normalized = T),
     edge.width=E(var23)$weight/10*mean(E(var23)$weight),
     edge.curved = TRUE,
     vertex.color=V(var23)$vertex_var23_color_degree,
     vertex.size=(closeness(var23, weights = E(var23)$var23, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var23,"LABEL_COR"),
     vertex.label.cex=closeness(var23, weights = E(var23)$var23, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var23)$var23_color_degree
b<-V(var23)$vertex_var23_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 25_CONFIANÇA H) Espera que o relacionamento com este serviço continue nos próximos anos (var23).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var23, mode="in", weights = E(var23)$var23, normalized = T)), 
             sd(closeness(var23, mode="in", weights = E(var23)$var23, normalized = T))
             )
       )
```

![](25_CONFIANÇA_H_Relacionamento_continue_nos_próximos_anos_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var23_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.092080 0.121600 0.099330 0.126000 0.185100
```

```r
sd(var23_outcloseness_n)
```

```
## [1] 0.04967211
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var23)$outcloseness_n<-closeness(var23, weights = E(var23)$var23, mode="out", normalized = T)

#Get Variable
V(var23)$var23_color_degree<-round(V(var23)$outcloseness_n,6)

#Creating brewer pallette
vertex_var23_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var23)$var23_color_degree)), "RdBu"))(
            length(unique(V(var23)$var23_color_degree)))

#Saving as Vertex properties 
V(var23)$vertex_var23_color_degree<-
  vertex_var23_color_degree[as.numeric(
  cut(V(var23)$var23_color_degree,
      breaks=length(unique(V(var23)$var23_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var23, es=E(var23), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var23))
maxC <- rep(Inf, vcount(var23))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var23, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var23)$weight)


#PLotting
plot(var23, 
     layout=co,
     edge.color=V(var23)$vertex_var23_color_degree[edge.start],
     edge.arrow.size=closeness(var23, weights = E(var23)$var23, mode="out",normalized = T),
     edge.width=E(var23)$weight/10*mean(E(var23)$weight),
     edge.curved = TRUE,
     vertex.color=V(var23)$vertex_var23_color_degree,
     vertex.size=(closeness(var23, weights = E(var23)$var23, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var23,"LABEL_COR"),
     vertex.label.cex=closeness(var23, weights = E(var23)$var23, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var23)$var23_color_degree
b<-V(var23)$vertex_var23_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 25_CONFIANÇA H) Espera que o relacionamento com este serviço continue nos próximos anos (var23).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var23, mode="out", weights = E(var23)$var23, normalized = T)), 
             sd(closeness(var23, mode="out", weights = E(var23)$var23, normalized = T))
             )
       )
```

![](25_CONFIANÇA_H_Relacionamento_continue_nos_próximos_anos_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var23_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.09677 0.15280 0.15940 0.16200 0.17380 0.27600
```

```r
sd(var23_totalcloseness_n)
```

```
## [1] 0.02730176
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var23)$allcloseness_n<-closeness(var23, weights = E(var23)$var23, mode="all", normalized = T)

#Get Variable
V(var23)$var23_color_degree<-round(V(var23)$allcloseness_n,6)

#Creating brewer pallette
vertex_var23_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var23)$var23_color_degree)), "RdBu"))(
            length(unique(V(var23)$var23_color_degree)))

#Saving as Vertex properties 
V(var23)$vertex_var23_color_degree<-
  vertex_var23_color_degree[as.numeric(
  cut(V(var23)$var23_color_degree,
      breaks=length(unique(V(var23)$var23_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var23, es=E(var23), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var23))
maxC <- rep(Inf, vcount(var23))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var23, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var23)$weight)


#PLotting
plot(var23, 
     layout=co,
     edge.color=V(var23)$vertex_var23_color_degree[edge.start],
     edge.arrow.size=closeness(var23, weights = E(var23)$var23, mode="all",normalized = T),
     edge.width=E(var23)$weight/10*mean(E(var23)$weight),
     edge.curved = TRUE,
     vertex.color=V(var23)$vertex_var23_color_degree,
     vertex.size=(closeness(var23, weights = E(var23)$var23, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var23,"LABEL_COR"),
     vertex.label.cex=closeness(var23, weights = E(var23)$var23, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var23)$var23_color_degree
b<-V(var23)$vertex_var23_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 25_CONFIANÇA H) Espera que o relacionamento com este serviço continue nos próximos anos (var23).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var23, mode="all", weights = E(var23)$var23, normalized = T)), 
             sd(closeness(var23, mode="all", weights = E(var23)$var23, normalized = T))
             )
       )
```

![](25_CONFIANÇA_H_Relacionamento_continue_nos_próximos_anos_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var23)$incloseness_n <- closeness(var23, weights = E(var23)$var23, mode = "in", normalized = T) %>% round(6)
V(var23)$outcloseness_n <- closeness(var23, weights = E(var23)$var23, mode = "out", normalized = T) %>% round(6)
V(var23)$totalcloseness_n <- closeness(var23, weights = E(var23)$var23, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var23)$var23_centr_closeness<- centralization.closeness(var23)$res
var23_centr_closeness<- centralization.closeness(var23)$res
var23_centr_closeness_all<- centralization.closeness(var23)
```

###Centralization

```r
var23_centr_closeness_all$centralization
```

```
## [1] 0.1625197
```

###Theoretical Max

```r
var23_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var23)$var23_centr_closeness<- centralization.closeness(var23)$res

#Get Variable
V(var23)$var23_color_degree<-round(V(var23)$var23_centr_closeness,6)

#Creating brewer pallette
vertex_var23_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var23)$var23_color_degree)), "Spectral"))(
            length(unique(V(var23)$var23_color_degree)))

#Saving as Vertex properties 
V(var23)$vertex_var23_color_degree<-
  vertex_var23_color_degree[as.numeric(
  cut(V(var23)$var23_color_degree,
      breaks=length(unique(V(var23)$var23_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var23, es=E(var23), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var23))
maxC <- rep(Inf, vcount(var23))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var23, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var23)$weight)


#PLotting
plot(var23, 
     layout=co,
     edge.color=V(var23)$vertex_var23_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var23)$res,
     edge.width=E(var23)$weight/10*mean(E(var23)$weight),
     edge.curved = TRUE,
     vertex.color=V(var23)$vertex_var23_color_degree,
     vertex.size=centralization.closeness(var23)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var23,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var23)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var23)$var23_color_degree
b<-V(var23)$vertex_var23_color_degree
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
  title("Network Centralization Closeness - 25_CONFIANÇA H) Espera que o relacionamento com este serviço continue nos próximos anos (var23).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var23)$res), 
             sd(centralization.closeness(var23)$res)
             )
       )
```

![](25_CONFIANÇA_H_Relacionamento_continue_nos_próximos_anos_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var23_incloseness<- closeness(var23, weights = E(var23)$var23, mode = "in") %>% round(6)
var23_outcloseness<- closeness(var23, weights = E(var23)$var23, mode = "out") %>% round(6)
var23_totalcloseness<- closeness(var23, weights = E(var23)$var23, mode = "total") %>% round(6)
var23_incloseness_n<- closeness(var23,weights = E(var23)$var23, mode = "in", normalized = T) %>% round(6)
var23_outcloseness_n<- closeness(var23,weights = E(var23)$var23, mode = "out", normalized = T) %>% round(6)
var23_totalcloseness_n<- closeness(var23,weights = E(var23)$var23, mode = "total", normalized = T) %>% round(6)
var23_centr_closeness <- centralization.closeness(var23)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var23_df_closseness <- data.frame(
var23_incloseness,
var23_outcloseness,
var23_totalcloseness,
var23_incloseness_n,
var23_outcloseness_n,
var23_totalcloseness_n,
var23_centr_closeness) %>% round(6)

#Adding type
var23_df_closseness <-cbind(var23_df_closseness, V(var23)$LABEL_COR)

#Adding names
names(var23_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var23_df_closseness<-var23_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var23_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-30a22cb8a808056aa705" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-30a22cb8a808056aa705">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.00013\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000995\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00052\" data-max=\"0.001484\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024241\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.185075\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.096774\" data-max=\"0.275964\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[0.000128,0.000127,0.00013,0.000125,0.000125,0.000125,0.000124,0.000127,0.000121,0.000126,0.000125,0.000126,0.000119,0.000121,0.000124,0.000125,0.000126,0.000127,0.00012,0.000124,0.000126,0.000121,0.000126,0.000115,0.000114,0.000123,0.000119,0.000126,0.000125,0.000125,0.000125,0.000124,0.000125,0.000119,0.000125,0.000121,0.000126,0.000124,0.000123,0.000125,0.000125,0.000124,0.000124,0.000126,0.000124,0.000126,0.000115,0.000124,0.000124,0.000124,0.000124,0.000124,0.000115,0.000122,0.000119,0.000125,0.000125,0.000118,0.00012,0.000126,0.000127,0.000118,0.000124,0.000124,0.000121,2.9e-05,0.000115,0.000124,0.000119,0.000124,0.000125,0.000125,0.000125,0.000125,0.000125,0.000124,0.000125,0.000126,0.000115,0.000115,0.000115,0.000115,0.000125,0.000121,0.000121,0.000125,0.000123,0.000113,0.000125,0.000124,0.000124,0.000115,0.000118,0.000124,0.000125,0.000122,0.000113,0.00012,0.000125,0.000125,0.000116,0.000121,0.000126,0.000126,0.000124,0.000124,0.000124,0.000126,0.000126,0.000125,0.000124,0.000125,0.000125,0.000125,0.000125,0.000124,0.000124,0.000124,0.000125,0.000125,0.000124,0.000124,0.000125,0.000124,0.000125,0.000125,0.000124,0.000125,0.000125,0.000123,0.000124,0.000125,0.000125,0.000125,0.000125,0.000125,0.000124,0.000124,0.000125,0.000124,0.000125,0.000124,0.000124,0.000124,0.000123,0.000126,0.000125,0.000124,0.00012,0.000122,0.000124,0.000122,0.000124,0.000122,0.000124,0.000116,0.000125,0.000127,0.000127,0.000116,0.000117,0.000121,0.000125,0.000124,0.000117,0.000118,0.000125,0.000117,0.000117,0.000121,0.000121,0.000117,0.000121,0.000121,0.000122,0.000122,0.000121,0.000117,0.000121,0.000121,0.000122,0.000122,0.000123,0.000122,0.000121,0.000118,0.000126],[0.000914,0.000816,0.000995,0.000774,0.000694,0.000683,0.000703,0.00074,0.000732,0.000653,0.00067,0.000659,0.000775,0.000765,0.000784,0.000653,0.00085,0.00074,0.000671,0.000649,0.000696,0.000645,0.00067,0.000784,0.000639,0.00052,0.000621,0.000756,0.000791,0.000731,0.000707,0.000683,0.000724,0.000481,0.000483,0.000645,0.000735,0.000644,0.000676,0.000751,0.000773,0.000747,0.000747,0.000668,0.000659,0.000687,0.000496,0.000654,0.000654,0.000654,0.000654,0.000654,0.000637,0.000682,0.000787,0.000737,0.000698,0.000655,0.000584,0.000751,2.9e-05,0.00049,0.000674,0.000688,0.000683,2.9e-05,2.9e-05,0.000704,0.00069,0.000845,0.000645,0.000665,0.00067,0.00067,0.000658,0.000665,0.000657,0.000735,0.000495,0.000495,0.000495,0.000495,0.000655,0.000547,0.000677,0.000674,0.000668,0.000615,0.000688,0.00065,0.000661,0.000693,0.000656,0.00054,0.000674,0.000657,0.000459,0.000477,0.000649,0.000952,0.000504,0.000717,2.9e-05,2.9e-05,0.000663,0.000678,0.000649,2.9e-05,2.9e-05,0.000677,0.000678,0.00066,0.000654,0.000651,0.000654,0.000668,0.000676,0.000677,0.000658,0.000658,0.000668,0.00071,0.000645,0.000645,0.000645,0.00066,0.000642,0.00065,0.000642,0.00066,0.000658,0.000658,0.000661,0.000642,0.000729,0.000675,0.000648,0.000642,0.000642,0.000658,0.000644,0.000642,0.000647,0.000642,0.000642,0.000642,0.000658,0.000475,0.000552,0.000563,0.000563,0.000563,0.000563,0.000563,0.000531,0.000591,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001389,0.001225,0.001484,0.00098,0.000957,0.000941,0.00091,0.001104,0.000883,0.001,0.000914,0.000972,0.000959,0.000918,0.000997,0.00106,0.001082,0.001149,0.000858,0.000883,0.001116,0.000833,0.001112,0.00098,0.000822,0.000873,0.000884,0.001125,0.00096,0.000935,0.000927,0.000875,0.000984,0.000678,0.000892,0.000847,0.001043,0.001071,0.00106,0.000943,0.000948,0.000929,0.000955,0.001019,0.000861,0.00092,0.000695,0.001062,0.001062,0.001062,0.001062,0.001062,0.000801,0.000876,0.001012,0.000962,0.000876,0.000899,0.000857,0.001183,0.000921,0.000622,0.000865,0.000865,0.000867,0.00052,0.000652,0.000901,0.000872,0.001171,0.000845,0.00085,0.000897,0.000901,0.000857,0.000864,0.000916,0.000941,0.000617,0.000617,0.000617,0.000617,0.000849,0.000863,0.000929,0.000842,0.000951,0.000797,0.00088,0.000845,0.000847,0.000859,0.000829,0.000833,0.000856,0.000917,0.000627,0.000715,0.000844,0.001302,0.000637,0.000893,0.001052,0.001052,0.00085,0.000858,0.000853,0.001052,0.001052,0.000916,0.00085,0.00085,0.000925,0.000845,0.000843,0.000858,0.000857,0.00086,0.000851,0.000862,0.00085,0.000894,0.000845,0.000837,0.000845,0.00085,0.000837,0.000842,0.00085,0.00085,0.000835,0.000839,0.000844,0.000847,0.000978,0.000854,0.000845,0.000853,0.000868,0.000855,0.000852,0.000844,0.000841,0.000843,0.000821,0.001109,0.000917,0.000912,0.000692,0.000794,0.000835,0.000794,0.000835,0.000794,0.000934,0.000844,0.000796,0.000837,0.000835,0.000695,0.00062,0.000684,0.000732,0.000715,0.00066,0.000649,0.001024,0.00066,0.00066,0.000672,0.000677,0.00066,0.000674,0.000683,0.000786,0.000791,0.00068,0.00066,0.00068,0.000683,0.00072,0.00072,0.000726,0.000728,0.000689,0.000585,0.000814],[0.023852,0.023529,0.024241,0.023288,0.023329,0.023276,0.023083,0.023577,0.022429,0.023467,0.023314,0.023352,0.022159,0.022464,0.02312,0.023195,0.023358,0.023541,0.022262,0.02306,0.023349,0.022472,0.023364,0.021409,0.02124,0.022949,0.022046,0.023367,0.023259,0.023241,0.023294,0.02314,0.02334,0.022048,0.023259,0.022524,0.023473,0.022994,0.022963,0.023215,0.023276,0.023154,0.023129,0.023494,0.022974,0.023343,0.021303,0.02302,0.02302,0.02302,0.02302,0.02302,0.021434,0.022772,0.022106,0.023259,0.023183,0.022001,0.022281,0.023479,0.023679,0.021862,0.023149,0.02314,0.022581,0.005348,0.021443,0.023097,0.022196,0.023097,0.023163,0.023178,0.02318,0.023198,0.023206,0.02314,0.023166,0.023382,0.021471,0.021471,0.021471,0.021471,0.023169,0.022554,0.022597,0.02318,0.022904,0.02111,0.023163,0.023108,0.023152,0.021335,0.021926,0.02304,0.023178,0.022619,0.02111,0.022402,0.023198,0.023259,0.021588,0.022584,0.023488,0.023488,0.02314,0.023143,0.023149,0.023488,0.023523,0.023169,0.023152,0.023209,0.023178,0.02318,0.02318,0.023149,0.02314,0.02314,0.02316,0.023221,0.02314,0.023108,0.023178,0.023154,0.023178,0.023172,0.023154,0.023163,0.023172,0.022791,0.023146,0.023163,0.023169,0.023163,0.023175,0.023206,0.02314,0.02314,0.02318,0.023134,0.02318,0.023134,0.023126,0.023131,0.02294,0.023512,0.0233,0.023154,0.022246,0.022758,0.023146,0.022758,0.023146,0.022758,0.023091,0.021608,0.023308,0.023688,0.023679,0.021583,0.021686,0.022545,0.023206,0.023129,0.021732,0.021908,0.02327,0.021732,0.021732,0.022535,0.022548,0.021732,0.022535,0.022592,0.022727,0.022761,0.022513,0.021732,0.022513,0.022521,0.022608,0.022608,0.022791,0.022661,0.022513,0.02189,0.023446],[0.170018,0.151837,0.185075,0.143963,0.129077,0.126962,0.130802,0.137676,0.136164,0.12141,0.124581,0.12253,0.144074,0.142202,0.145882,0.12141,0.158163,0.137574,0.124832,0.120701,0.129436,0.12,0.124665,0.145768,0.118926,0.096724,0.115456,0.14059,0.147152,0.135965,0.131449,0.127049,0.134588,0.089466,0.089855,0.12,0.136765,0.119768,0.125676,0.139745,0.143852,0.13891,0.13891,0.124332,0.12253,0.127835,0.092308,0.121569,0.121569,0.121569,0.121569,0.121569,0.118396,0.126789,0.146341,0.137168,0.129888,0.121887,0.108581,0.13964,0.005348,0.091087,0.125421,0.128011,0.126962,0.005376,0.005405,0.130894,0.128276,0.157227,0.119923,0.12367,0.124665,0.124665,0.122368,0.123752,0.122288,0.136765,0.092079,0.092079,0.092079,0.092079,0.121807,0.101751,0.125931,0.125421,0.124332,0.114462,0.128011,0.120858,0.122935,0.128988,0.121967,0.100486,0.125337,0.122127,0.085439,0.08874,0.120623,0.177143,0.093656,0.133429,0.005348,0.005348,0.12326,0.126102,0.120701,0.005348,0.005348,0.125931,0.126016,0.122772,0.121648,0.121094,0.121569,0.124248,0.125676,0.125931,0.122449,0.122449,0.124166,0.132009,0.119923,0.119923,0.119923,0.122691,0.119461,0.120858,0.119461,0.122691,0.122449,0.122449,0.122935,0.119461,0.135569,0.125591,0.120466,0.119461,0.119461,0.122449,0.119691,0.119461,0.12031,0.119461,0.119461,0.119461,0.122449,0.088319,0.102762,0.104671,0.104671,0.104671,0.104671,0.104671,0.098779,0.109864,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.258333,0.227941,0.275964,0.182353,0.17799,0.174976,0.169245,0.205298,0.164311,0.186,0.170018,0.180758,0.178332,0.170799,0.185444,0.197243,0.201299,0.213793,0.15952,0.164166,0.207589,0.154871,0.206897,0.182353,0.152961,0.162445,0.164456,0.209224,0.178503,0.173832,0.172382,0.16273,0.183071,0.126016,0.165923,0.157627,0.193952,0.199143,0.197243,0.175472,0.176303,0.172862,0.17765,0.189602,0.160069,0.171113,0.129256,0.197452,0.197452,0.197452,0.197452,0.197452,0.148919,0.162872,0.188259,0.178846,0.162872,0.167266,0.159383,0.220118,0.171271,0.1156,0.1609,0.1609,0.161318,0.096774,0.121252,0.167568,0.162162,0.217799,0.157227,0.158029,0.166816,0.167568,0.159383,0.160622,0.17033,0.174976,0.114673,0.114673,0.114673,0.114673,0.157895,0.160483,0.172862,0.156566,0.176974,0.148207,0.163732,0.157095,0.157627,0.159794,0.154229,0.154871,0.159247,0.170486,0.116614,0.132952,0.156962,0.242188,0.118547,0.166071,0.195584,0.195584,0.158029,0.15952,0.158703,0.195584,0.195584,0.17033,0.158163,0.158163,0.172063,0.157095,0.15683,0.159657,0.159383,0.159931,0.158298,0.160345,0.158163,0.166369,0.157095,0.155649,0.157095,0.158029,0.155649,0.156698,0.158029,0.158029,0.155259,0.15604,0.156962,0.157494,0.181996,0.158839,0.157095,0.158703,0.161458,0.158974,0.158433,0.156962,0.156434,0.15683,0.152709,0.206208,0.170642,0.169553,0.128631,0.147736,0.155259,0.147736,0.155259,0.147736,0.173669,0.156962,0.148089,0.155649,0.155259,0.129256,0.115242,0.127223,0.136064,0.132952,0.122772,0.120779,0.190379,0.122772,0.122772,0.125084,0.125931,0.122772,0.125337,0.127049,0.146112,0.147036,0.126531,0.122772,0.126531,0.126962,0.134006,0.134006,0.135076,0.13547,0.128187,0.108899,0.151343],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var23_df_closseness, by=list(var23_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var23_df_closseness, by=list(var23_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-7dce7299005f45696e40" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-7dce7299005f45696e40">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000116\" data-max=\"0.00013\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000995\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.3e-05\" data-max=\"0.000308\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000745\" data-max=\"0.001484\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6e-06\" data-max=\"0.000159\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.021575\" data-max=\"0.024241\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4e-05\" data-max=\"0.004003\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.185075\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.002328\" data-max=\"0.057371\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.13853\" data-max=\"0.275964\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001116\" data-max=\"0.029671\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003883\" data-max=\"0.139638\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[0.000123,0.00012,0.000124,0.000127,0.000124,0.000126,0.000126,0.00013,0.000126,0.000125,0.000126,0.000116,0.000122,0.000125,0.000125,0.000121,0.000126,0.000125,0.000128,0.000125,0.000127,0.000124,0.000124],[3e-06,3e-06,null,null,null,null,0,null,null,null,null,2.1e-05,2e-06,0,1e-06,4e-06,null,null,null,1e-06,null,1e-06,1e-06],[0.000683,0.000235,0.000649,0.000816,0.000703,0.000751,0.000698,0.000995,0.00067,0.000658,0.000735,0.000605,0.000756,0.000693,0.000742,0.000676,2.9e-05,0.000952,0.000914,0.000447,0.00074,0.00053,0.000666],[0.000116,0.000278,null,null,null,null,5.7e-05,null,null,null,null,0.000166,1.3e-05,9.1e-05,9.3e-05,6.8e-05,null,null,null,0.000308,null,0.000226,4.5e-05],[0.000875,0.000745,0.000883,0.001225,0.00091,0.001183,0.001117,0.001484,0.001112,0.000917,0.000941,0.000851,0.000924,0.00094,0.000993,0.00086,0.000814,0.001302,0.001389,0.001059,0.001149,0.000832,0.000874],[0.000159,0.000118,null,null,null,null,8e-06,null,null,null,null,0.000152,8e-06,3e-05,7.7e-05,8.5e-05,null,null,null,6e-06,null,2e-05,5.5e-05],[0.022954,0.022231,0.02306,0.023529,0.023083,0.023479,0.023409,0.024241,0.023364,0.0233,0.023382,0.021575,0.022809,0.02329,0.023321,0.022576,0.023446,0.023259,0.023852,0.023187,0.023541,0.023143,0.023151],[0.000614,0.000607,null,null,null,null,8.9e-05,null,null,null,null,0.004002,0.000488,4e-05,4.2e-05,0.000676,null,null,null,0.000236,null,0.000257,6.2e-05],[0.127067,0.043594,0.120701,0.151837,0.130802,0.13964,0.129829,0.185075,0.124665,0.122449,0.136765,0.112621,0.140556,0.128944,0.138067,0.125697,0.005348,0.177143,0.170018,0.083008,0.137574,0.098526,0.123968],[0.021553,0.051752,null,null,null,null,0.01057,null,null,null,null,0.030783,0.002328,0.016844,0.017435,0.012757,null,null,null,0.057371,null,0.042087,0.008428],[0.162668,0.13853,0.164166,0.227941,0.169245,0.220118,0.207674,0.275964,0.206897,0.170642,0.174976,0.15826,0.17183,0.174859,0.184755,0.159918,0.151343,0.242188,0.258333,0.196935,0.213793,0.154656,0.162476],[0.029671,0.021989,null,null,null,null,0.00151,null,null,null,null,0.028275,0.001459,0.00553,0.014407,0.015848,null,null,null,0.001116,null,0.003768,0.010234],[0.300585,0.099063,0.286154,0.343173,0.303922,0.323478,0.300896,0.391579,0.295238,0.29108,0.313659,0.266552,0.319611,0.301223,0.316849,0.295625,0.005348,0.306425,0.371257,0.194167,0.314189,0.234219,0.288627],[0.043014,0.125815,null,null,null,null,0.018146,null,null,null,null,0.070696,0.003883,0.033229,0.029748,0.023781,null,null,null,0.139638,null,0.102732,0.011577]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var23_df_closseness <- data.frame(
var23_incloseness,
var23_outcloseness,
var23_totalcloseness,
var23_incloseness_n,
var23_outcloseness_n,
var23_totalcloseness_n,
var23_centr_closeness) %>% round(6)

#Adding type
var23_df_closseness <-cbind(var23_df_closseness, V(var23)$TIPO1)

#Adding names
names(var23_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var23_df_closseness<-var23_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var23_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-b64a519adf2460e2ec2c" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-b64a519adf2460e2ec2c">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.00013\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000995\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00052\" data-max=\"0.001484\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024241\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.185075\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.096774\" data-max=\"0.275964\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[0.000128,0.000127,0.00013,0.000125,0.000125,0.000125,0.000124,0.000127,0.000121,0.000126,0.000125,0.000126,0.000119,0.000121,0.000124,0.000125,0.000126,0.000127,0.00012,0.000124,0.000126,0.000121,0.000126,0.000115,0.000114,0.000123,0.000119,0.000126,0.000125,0.000125,0.000125,0.000124,0.000125,0.000119,0.000125,0.000121,0.000126,0.000124,0.000123,0.000125,0.000125,0.000124,0.000124,0.000126,0.000124,0.000126,0.000115,0.000124,0.000124,0.000124,0.000124,0.000124,0.000115,0.000122,0.000119,0.000125,0.000125,0.000118,0.00012,0.000126,0.000127,0.000118,0.000124,0.000124,0.000121,2.9e-05,0.000115,0.000124,0.000119,0.000124,0.000125,0.000125,0.000125,0.000125,0.000125,0.000124,0.000125,0.000126,0.000115,0.000115,0.000115,0.000115,0.000125,0.000121,0.000121,0.000125,0.000123,0.000113,0.000125,0.000124,0.000124,0.000115,0.000118,0.000124,0.000125,0.000122,0.000113,0.00012,0.000125,0.000125,0.000116,0.000121,0.000126,0.000126,0.000124,0.000124,0.000124,0.000126,0.000126,0.000125,0.000124,0.000125,0.000125,0.000125,0.000125,0.000124,0.000124,0.000124,0.000125,0.000125,0.000124,0.000124,0.000125,0.000124,0.000125,0.000125,0.000124,0.000125,0.000125,0.000123,0.000124,0.000125,0.000125,0.000125,0.000125,0.000125,0.000124,0.000124,0.000125,0.000124,0.000125,0.000124,0.000124,0.000124,0.000123,0.000126,0.000125,0.000124,0.00012,0.000122,0.000124,0.000122,0.000124,0.000122,0.000124,0.000116,0.000125,0.000127,0.000127,0.000116,0.000117,0.000121,0.000125,0.000124,0.000117,0.000118,0.000125,0.000117,0.000117,0.000121,0.000121,0.000117,0.000121,0.000121,0.000122,0.000122,0.000121,0.000117,0.000121,0.000121,0.000122,0.000122,0.000123,0.000122,0.000121,0.000118,0.000126],[0.000914,0.000816,0.000995,0.000774,0.000694,0.000683,0.000703,0.00074,0.000732,0.000653,0.00067,0.000659,0.000775,0.000765,0.000784,0.000653,0.00085,0.00074,0.000671,0.000649,0.000696,0.000645,0.00067,0.000784,0.000639,0.00052,0.000621,0.000756,0.000791,0.000731,0.000707,0.000683,0.000724,0.000481,0.000483,0.000645,0.000735,0.000644,0.000676,0.000751,0.000773,0.000747,0.000747,0.000668,0.000659,0.000687,0.000496,0.000654,0.000654,0.000654,0.000654,0.000654,0.000637,0.000682,0.000787,0.000737,0.000698,0.000655,0.000584,0.000751,2.9e-05,0.00049,0.000674,0.000688,0.000683,2.9e-05,2.9e-05,0.000704,0.00069,0.000845,0.000645,0.000665,0.00067,0.00067,0.000658,0.000665,0.000657,0.000735,0.000495,0.000495,0.000495,0.000495,0.000655,0.000547,0.000677,0.000674,0.000668,0.000615,0.000688,0.00065,0.000661,0.000693,0.000656,0.00054,0.000674,0.000657,0.000459,0.000477,0.000649,0.000952,0.000504,0.000717,2.9e-05,2.9e-05,0.000663,0.000678,0.000649,2.9e-05,2.9e-05,0.000677,0.000678,0.00066,0.000654,0.000651,0.000654,0.000668,0.000676,0.000677,0.000658,0.000658,0.000668,0.00071,0.000645,0.000645,0.000645,0.00066,0.000642,0.00065,0.000642,0.00066,0.000658,0.000658,0.000661,0.000642,0.000729,0.000675,0.000648,0.000642,0.000642,0.000658,0.000644,0.000642,0.000647,0.000642,0.000642,0.000642,0.000658,0.000475,0.000552,0.000563,0.000563,0.000563,0.000563,0.000563,0.000531,0.000591,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001389,0.001225,0.001484,0.00098,0.000957,0.000941,0.00091,0.001104,0.000883,0.001,0.000914,0.000972,0.000959,0.000918,0.000997,0.00106,0.001082,0.001149,0.000858,0.000883,0.001116,0.000833,0.001112,0.00098,0.000822,0.000873,0.000884,0.001125,0.00096,0.000935,0.000927,0.000875,0.000984,0.000678,0.000892,0.000847,0.001043,0.001071,0.00106,0.000943,0.000948,0.000929,0.000955,0.001019,0.000861,0.00092,0.000695,0.001062,0.001062,0.001062,0.001062,0.001062,0.000801,0.000876,0.001012,0.000962,0.000876,0.000899,0.000857,0.001183,0.000921,0.000622,0.000865,0.000865,0.000867,0.00052,0.000652,0.000901,0.000872,0.001171,0.000845,0.00085,0.000897,0.000901,0.000857,0.000864,0.000916,0.000941,0.000617,0.000617,0.000617,0.000617,0.000849,0.000863,0.000929,0.000842,0.000951,0.000797,0.00088,0.000845,0.000847,0.000859,0.000829,0.000833,0.000856,0.000917,0.000627,0.000715,0.000844,0.001302,0.000637,0.000893,0.001052,0.001052,0.00085,0.000858,0.000853,0.001052,0.001052,0.000916,0.00085,0.00085,0.000925,0.000845,0.000843,0.000858,0.000857,0.00086,0.000851,0.000862,0.00085,0.000894,0.000845,0.000837,0.000845,0.00085,0.000837,0.000842,0.00085,0.00085,0.000835,0.000839,0.000844,0.000847,0.000978,0.000854,0.000845,0.000853,0.000868,0.000855,0.000852,0.000844,0.000841,0.000843,0.000821,0.001109,0.000917,0.000912,0.000692,0.000794,0.000835,0.000794,0.000835,0.000794,0.000934,0.000844,0.000796,0.000837,0.000835,0.000695,0.00062,0.000684,0.000732,0.000715,0.00066,0.000649,0.001024,0.00066,0.00066,0.000672,0.000677,0.00066,0.000674,0.000683,0.000786,0.000791,0.00068,0.00066,0.00068,0.000683,0.00072,0.00072,0.000726,0.000728,0.000689,0.000585,0.000814],[0.023852,0.023529,0.024241,0.023288,0.023329,0.023276,0.023083,0.023577,0.022429,0.023467,0.023314,0.023352,0.022159,0.022464,0.02312,0.023195,0.023358,0.023541,0.022262,0.02306,0.023349,0.022472,0.023364,0.021409,0.02124,0.022949,0.022046,0.023367,0.023259,0.023241,0.023294,0.02314,0.02334,0.022048,0.023259,0.022524,0.023473,0.022994,0.022963,0.023215,0.023276,0.023154,0.023129,0.023494,0.022974,0.023343,0.021303,0.02302,0.02302,0.02302,0.02302,0.02302,0.021434,0.022772,0.022106,0.023259,0.023183,0.022001,0.022281,0.023479,0.023679,0.021862,0.023149,0.02314,0.022581,0.005348,0.021443,0.023097,0.022196,0.023097,0.023163,0.023178,0.02318,0.023198,0.023206,0.02314,0.023166,0.023382,0.021471,0.021471,0.021471,0.021471,0.023169,0.022554,0.022597,0.02318,0.022904,0.02111,0.023163,0.023108,0.023152,0.021335,0.021926,0.02304,0.023178,0.022619,0.02111,0.022402,0.023198,0.023259,0.021588,0.022584,0.023488,0.023488,0.02314,0.023143,0.023149,0.023488,0.023523,0.023169,0.023152,0.023209,0.023178,0.02318,0.02318,0.023149,0.02314,0.02314,0.02316,0.023221,0.02314,0.023108,0.023178,0.023154,0.023178,0.023172,0.023154,0.023163,0.023172,0.022791,0.023146,0.023163,0.023169,0.023163,0.023175,0.023206,0.02314,0.02314,0.02318,0.023134,0.02318,0.023134,0.023126,0.023131,0.02294,0.023512,0.0233,0.023154,0.022246,0.022758,0.023146,0.022758,0.023146,0.022758,0.023091,0.021608,0.023308,0.023688,0.023679,0.021583,0.021686,0.022545,0.023206,0.023129,0.021732,0.021908,0.02327,0.021732,0.021732,0.022535,0.022548,0.021732,0.022535,0.022592,0.022727,0.022761,0.022513,0.021732,0.022513,0.022521,0.022608,0.022608,0.022791,0.022661,0.022513,0.02189,0.023446],[0.170018,0.151837,0.185075,0.143963,0.129077,0.126962,0.130802,0.137676,0.136164,0.12141,0.124581,0.12253,0.144074,0.142202,0.145882,0.12141,0.158163,0.137574,0.124832,0.120701,0.129436,0.12,0.124665,0.145768,0.118926,0.096724,0.115456,0.14059,0.147152,0.135965,0.131449,0.127049,0.134588,0.089466,0.089855,0.12,0.136765,0.119768,0.125676,0.139745,0.143852,0.13891,0.13891,0.124332,0.12253,0.127835,0.092308,0.121569,0.121569,0.121569,0.121569,0.121569,0.118396,0.126789,0.146341,0.137168,0.129888,0.121887,0.108581,0.13964,0.005348,0.091087,0.125421,0.128011,0.126962,0.005376,0.005405,0.130894,0.128276,0.157227,0.119923,0.12367,0.124665,0.124665,0.122368,0.123752,0.122288,0.136765,0.092079,0.092079,0.092079,0.092079,0.121807,0.101751,0.125931,0.125421,0.124332,0.114462,0.128011,0.120858,0.122935,0.128988,0.121967,0.100486,0.125337,0.122127,0.085439,0.08874,0.120623,0.177143,0.093656,0.133429,0.005348,0.005348,0.12326,0.126102,0.120701,0.005348,0.005348,0.125931,0.126016,0.122772,0.121648,0.121094,0.121569,0.124248,0.125676,0.125931,0.122449,0.122449,0.124166,0.132009,0.119923,0.119923,0.119923,0.122691,0.119461,0.120858,0.119461,0.122691,0.122449,0.122449,0.122935,0.119461,0.135569,0.125591,0.120466,0.119461,0.119461,0.122449,0.119691,0.119461,0.12031,0.119461,0.119461,0.119461,0.122449,0.088319,0.102762,0.104671,0.104671,0.104671,0.104671,0.104671,0.098779,0.109864,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.258333,0.227941,0.275964,0.182353,0.17799,0.174976,0.169245,0.205298,0.164311,0.186,0.170018,0.180758,0.178332,0.170799,0.185444,0.197243,0.201299,0.213793,0.15952,0.164166,0.207589,0.154871,0.206897,0.182353,0.152961,0.162445,0.164456,0.209224,0.178503,0.173832,0.172382,0.16273,0.183071,0.126016,0.165923,0.157627,0.193952,0.199143,0.197243,0.175472,0.176303,0.172862,0.17765,0.189602,0.160069,0.171113,0.129256,0.197452,0.197452,0.197452,0.197452,0.197452,0.148919,0.162872,0.188259,0.178846,0.162872,0.167266,0.159383,0.220118,0.171271,0.1156,0.1609,0.1609,0.161318,0.096774,0.121252,0.167568,0.162162,0.217799,0.157227,0.158029,0.166816,0.167568,0.159383,0.160622,0.17033,0.174976,0.114673,0.114673,0.114673,0.114673,0.157895,0.160483,0.172862,0.156566,0.176974,0.148207,0.163732,0.157095,0.157627,0.159794,0.154229,0.154871,0.159247,0.170486,0.116614,0.132952,0.156962,0.242188,0.118547,0.166071,0.195584,0.195584,0.158029,0.15952,0.158703,0.195584,0.195584,0.17033,0.158163,0.158163,0.172063,0.157095,0.15683,0.159657,0.159383,0.159931,0.158298,0.160345,0.158163,0.166369,0.157095,0.155649,0.157095,0.158029,0.155649,0.156698,0.158029,0.158029,0.155259,0.15604,0.156962,0.157494,0.181996,0.158839,0.157095,0.158703,0.161458,0.158974,0.158433,0.156962,0.156434,0.15683,0.152709,0.206208,0.170642,0.169553,0.128631,0.147736,0.155259,0.147736,0.155259,0.147736,0.173669,0.156962,0.148089,0.155649,0.155259,0.129256,0.115242,0.127223,0.136064,0.132952,0.122772,0.120779,0.190379,0.122772,0.122772,0.125084,0.125931,0.122772,0.125337,0.127049,0.146112,0.147036,0.126531,0.122772,0.126531,0.126962,0.134006,0.134006,0.135076,0.13547,0.128187,0.108899,0.151343],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var23_df_closseness, by=list(var23_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var23_df_closseness, by=list(var23_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-1f5815fcdf034a234e02" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-1f5815fcdf034a234e02">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000119\" data-max=\"0.000125\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000404\" data-max=\"0.000629\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000184\" data-max=\"0.000306\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000794\" data-max=\"0.000927\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000122\" data-max=\"0.000144\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.02214\" data-max=\"0.023212\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000203\" data-max=\"0.002025\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.075107\" data-max=\"0.11705\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.034183\" data-max=\"0.057023\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.14776\" data-max=\"0.172446\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022691\" data-max=\"0.026733\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.173776\" data-max=\"0.2725\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.079091\" data-max=\"0.135432\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[0.000125,0.000119],[1e-06,1.1e-05],[0.000629,0.000404],[0.000184,0.000306],[0.000927,0.000794],[0.000122,0.000144],[0.023212,0.02214],[0.000203,0.002025],[0.11705,0.075107],[0.034183,0.057023],[0.172446,0.14776],[0.022691,0.026733],[0.2725,0.173776],[0.079091,0.135432]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var23_df_closseness <- data.frame(
var23_incloseness,
var23_outcloseness,
var23_totalcloseness,
var23_incloseness_n,
var23_outcloseness_n,
var23_totalcloseness_n,
var23_centr_closeness) %>% round(6)

#Adding type
var23_df_closseness <-cbind(var23_df_closseness, V(var23)$TIPO2)

#Adding names
names(var23_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var23_df_closseness<-var23_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var23_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-359aaa6cac1aec071a63" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-359aaa6cac1aec071a63">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.00013\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000995\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00052\" data-max=\"0.001484\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024241\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.185075\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.096774\" data-max=\"0.275964\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[0.000128,0.000127,0.00013,0.000125,0.000125,0.000125,0.000124,0.000127,0.000121,0.000126,0.000125,0.000126,0.000119,0.000121,0.000124,0.000125,0.000126,0.000127,0.00012,0.000124,0.000126,0.000121,0.000126,0.000115,0.000114,0.000123,0.000119,0.000126,0.000125,0.000125,0.000125,0.000124,0.000125,0.000119,0.000125,0.000121,0.000126,0.000124,0.000123,0.000125,0.000125,0.000124,0.000124,0.000126,0.000124,0.000126,0.000115,0.000124,0.000124,0.000124,0.000124,0.000124,0.000115,0.000122,0.000119,0.000125,0.000125,0.000118,0.00012,0.000126,0.000127,0.000118,0.000124,0.000124,0.000121,2.9e-05,0.000115,0.000124,0.000119,0.000124,0.000125,0.000125,0.000125,0.000125,0.000125,0.000124,0.000125,0.000126,0.000115,0.000115,0.000115,0.000115,0.000125,0.000121,0.000121,0.000125,0.000123,0.000113,0.000125,0.000124,0.000124,0.000115,0.000118,0.000124,0.000125,0.000122,0.000113,0.00012,0.000125,0.000125,0.000116,0.000121,0.000126,0.000126,0.000124,0.000124,0.000124,0.000126,0.000126,0.000125,0.000124,0.000125,0.000125,0.000125,0.000125,0.000124,0.000124,0.000124,0.000125,0.000125,0.000124,0.000124,0.000125,0.000124,0.000125,0.000125,0.000124,0.000125,0.000125,0.000123,0.000124,0.000125,0.000125,0.000125,0.000125,0.000125,0.000124,0.000124,0.000125,0.000124,0.000125,0.000124,0.000124,0.000124,0.000123,0.000126,0.000125,0.000124,0.00012,0.000122,0.000124,0.000122,0.000124,0.000122,0.000124,0.000116,0.000125,0.000127,0.000127,0.000116,0.000117,0.000121,0.000125,0.000124,0.000117,0.000118,0.000125,0.000117,0.000117,0.000121,0.000121,0.000117,0.000121,0.000121,0.000122,0.000122,0.000121,0.000117,0.000121,0.000121,0.000122,0.000122,0.000123,0.000122,0.000121,0.000118,0.000126],[0.000914,0.000816,0.000995,0.000774,0.000694,0.000683,0.000703,0.00074,0.000732,0.000653,0.00067,0.000659,0.000775,0.000765,0.000784,0.000653,0.00085,0.00074,0.000671,0.000649,0.000696,0.000645,0.00067,0.000784,0.000639,0.00052,0.000621,0.000756,0.000791,0.000731,0.000707,0.000683,0.000724,0.000481,0.000483,0.000645,0.000735,0.000644,0.000676,0.000751,0.000773,0.000747,0.000747,0.000668,0.000659,0.000687,0.000496,0.000654,0.000654,0.000654,0.000654,0.000654,0.000637,0.000682,0.000787,0.000737,0.000698,0.000655,0.000584,0.000751,2.9e-05,0.00049,0.000674,0.000688,0.000683,2.9e-05,2.9e-05,0.000704,0.00069,0.000845,0.000645,0.000665,0.00067,0.00067,0.000658,0.000665,0.000657,0.000735,0.000495,0.000495,0.000495,0.000495,0.000655,0.000547,0.000677,0.000674,0.000668,0.000615,0.000688,0.00065,0.000661,0.000693,0.000656,0.00054,0.000674,0.000657,0.000459,0.000477,0.000649,0.000952,0.000504,0.000717,2.9e-05,2.9e-05,0.000663,0.000678,0.000649,2.9e-05,2.9e-05,0.000677,0.000678,0.00066,0.000654,0.000651,0.000654,0.000668,0.000676,0.000677,0.000658,0.000658,0.000668,0.00071,0.000645,0.000645,0.000645,0.00066,0.000642,0.00065,0.000642,0.00066,0.000658,0.000658,0.000661,0.000642,0.000729,0.000675,0.000648,0.000642,0.000642,0.000658,0.000644,0.000642,0.000647,0.000642,0.000642,0.000642,0.000658,0.000475,0.000552,0.000563,0.000563,0.000563,0.000563,0.000563,0.000531,0.000591,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001389,0.001225,0.001484,0.00098,0.000957,0.000941,0.00091,0.001104,0.000883,0.001,0.000914,0.000972,0.000959,0.000918,0.000997,0.00106,0.001082,0.001149,0.000858,0.000883,0.001116,0.000833,0.001112,0.00098,0.000822,0.000873,0.000884,0.001125,0.00096,0.000935,0.000927,0.000875,0.000984,0.000678,0.000892,0.000847,0.001043,0.001071,0.00106,0.000943,0.000948,0.000929,0.000955,0.001019,0.000861,0.00092,0.000695,0.001062,0.001062,0.001062,0.001062,0.001062,0.000801,0.000876,0.001012,0.000962,0.000876,0.000899,0.000857,0.001183,0.000921,0.000622,0.000865,0.000865,0.000867,0.00052,0.000652,0.000901,0.000872,0.001171,0.000845,0.00085,0.000897,0.000901,0.000857,0.000864,0.000916,0.000941,0.000617,0.000617,0.000617,0.000617,0.000849,0.000863,0.000929,0.000842,0.000951,0.000797,0.00088,0.000845,0.000847,0.000859,0.000829,0.000833,0.000856,0.000917,0.000627,0.000715,0.000844,0.001302,0.000637,0.000893,0.001052,0.001052,0.00085,0.000858,0.000853,0.001052,0.001052,0.000916,0.00085,0.00085,0.000925,0.000845,0.000843,0.000858,0.000857,0.00086,0.000851,0.000862,0.00085,0.000894,0.000845,0.000837,0.000845,0.00085,0.000837,0.000842,0.00085,0.00085,0.000835,0.000839,0.000844,0.000847,0.000978,0.000854,0.000845,0.000853,0.000868,0.000855,0.000852,0.000844,0.000841,0.000843,0.000821,0.001109,0.000917,0.000912,0.000692,0.000794,0.000835,0.000794,0.000835,0.000794,0.000934,0.000844,0.000796,0.000837,0.000835,0.000695,0.00062,0.000684,0.000732,0.000715,0.00066,0.000649,0.001024,0.00066,0.00066,0.000672,0.000677,0.00066,0.000674,0.000683,0.000786,0.000791,0.00068,0.00066,0.00068,0.000683,0.00072,0.00072,0.000726,0.000728,0.000689,0.000585,0.000814],[0.023852,0.023529,0.024241,0.023288,0.023329,0.023276,0.023083,0.023577,0.022429,0.023467,0.023314,0.023352,0.022159,0.022464,0.02312,0.023195,0.023358,0.023541,0.022262,0.02306,0.023349,0.022472,0.023364,0.021409,0.02124,0.022949,0.022046,0.023367,0.023259,0.023241,0.023294,0.02314,0.02334,0.022048,0.023259,0.022524,0.023473,0.022994,0.022963,0.023215,0.023276,0.023154,0.023129,0.023494,0.022974,0.023343,0.021303,0.02302,0.02302,0.02302,0.02302,0.02302,0.021434,0.022772,0.022106,0.023259,0.023183,0.022001,0.022281,0.023479,0.023679,0.021862,0.023149,0.02314,0.022581,0.005348,0.021443,0.023097,0.022196,0.023097,0.023163,0.023178,0.02318,0.023198,0.023206,0.02314,0.023166,0.023382,0.021471,0.021471,0.021471,0.021471,0.023169,0.022554,0.022597,0.02318,0.022904,0.02111,0.023163,0.023108,0.023152,0.021335,0.021926,0.02304,0.023178,0.022619,0.02111,0.022402,0.023198,0.023259,0.021588,0.022584,0.023488,0.023488,0.02314,0.023143,0.023149,0.023488,0.023523,0.023169,0.023152,0.023209,0.023178,0.02318,0.02318,0.023149,0.02314,0.02314,0.02316,0.023221,0.02314,0.023108,0.023178,0.023154,0.023178,0.023172,0.023154,0.023163,0.023172,0.022791,0.023146,0.023163,0.023169,0.023163,0.023175,0.023206,0.02314,0.02314,0.02318,0.023134,0.02318,0.023134,0.023126,0.023131,0.02294,0.023512,0.0233,0.023154,0.022246,0.022758,0.023146,0.022758,0.023146,0.022758,0.023091,0.021608,0.023308,0.023688,0.023679,0.021583,0.021686,0.022545,0.023206,0.023129,0.021732,0.021908,0.02327,0.021732,0.021732,0.022535,0.022548,0.021732,0.022535,0.022592,0.022727,0.022761,0.022513,0.021732,0.022513,0.022521,0.022608,0.022608,0.022791,0.022661,0.022513,0.02189,0.023446],[0.170018,0.151837,0.185075,0.143963,0.129077,0.126962,0.130802,0.137676,0.136164,0.12141,0.124581,0.12253,0.144074,0.142202,0.145882,0.12141,0.158163,0.137574,0.124832,0.120701,0.129436,0.12,0.124665,0.145768,0.118926,0.096724,0.115456,0.14059,0.147152,0.135965,0.131449,0.127049,0.134588,0.089466,0.089855,0.12,0.136765,0.119768,0.125676,0.139745,0.143852,0.13891,0.13891,0.124332,0.12253,0.127835,0.092308,0.121569,0.121569,0.121569,0.121569,0.121569,0.118396,0.126789,0.146341,0.137168,0.129888,0.121887,0.108581,0.13964,0.005348,0.091087,0.125421,0.128011,0.126962,0.005376,0.005405,0.130894,0.128276,0.157227,0.119923,0.12367,0.124665,0.124665,0.122368,0.123752,0.122288,0.136765,0.092079,0.092079,0.092079,0.092079,0.121807,0.101751,0.125931,0.125421,0.124332,0.114462,0.128011,0.120858,0.122935,0.128988,0.121967,0.100486,0.125337,0.122127,0.085439,0.08874,0.120623,0.177143,0.093656,0.133429,0.005348,0.005348,0.12326,0.126102,0.120701,0.005348,0.005348,0.125931,0.126016,0.122772,0.121648,0.121094,0.121569,0.124248,0.125676,0.125931,0.122449,0.122449,0.124166,0.132009,0.119923,0.119923,0.119923,0.122691,0.119461,0.120858,0.119461,0.122691,0.122449,0.122449,0.122935,0.119461,0.135569,0.125591,0.120466,0.119461,0.119461,0.122449,0.119691,0.119461,0.12031,0.119461,0.119461,0.119461,0.122449,0.088319,0.102762,0.104671,0.104671,0.104671,0.104671,0.104671,0.098779,0.109864,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.258333,0.227941,0.275964,0.182353,0.17799,0.174976,0.169245,0.205298,0.164311,0.186,0.170018,0.180758,0.178332,0.170799,0.185444,0.197243,0.201299,0.213793,0.15952,0.164166,0.207589,0.154871,0.206897,0.182353,0.152961,0.162445,0.164456,0.209224,0.178503,0.173832,0.172382,0.16273,0.183071,0.126016,0.165923,0.157627,0.193952,0.199143,0.197243,0.175472,0.176303,0.172862,0.17765,0.189602,0.160069,0.171113,0.129256,0.197452,0.197452,0.197452,0.197452,0.197452,0.148919,0.162872,0.188259,0.178846,0.162872,0.167266,0.159383,0.220118,0.171271,0.1156,0.1609,0.1609,0.161318,0.096774,0.121252,0.167568,0.162162,0.217799,0.157227,0.158029,0.166816,0.167568,0.159383,0.160622,0.17033,0.174976,0.114673,0.114673,0.114673,0.114673,0.157895,0.160483,0.172862,0.156566,0.176974,0.148207,0.163732,0.157095,0.157627,0.159794,0.154229,0.154871,0.159247,0.170486,0.116614,0.132952,0.156962,0.242188,0.118547,0.166071,0.195584,0.195584,0.158029,0.15952,0.158703,0.195584,0.195584,0.17033,0.158163,0.158163,0.172063,0.157095,0.15683,0.159657,0.159383,0.159931,0.158298,0.160345,0.158163,0.166369,0.157095,0.155649,0.157095,0.158029,0.155649,0.156698,0.158029,0.158029,0.155259,0.15604,0.156962,0.157494,0.181996,0.158839,0.157095,0.158703,0.161458,0.158974,0.158433,0.156962,0.156434,0.15683,0.152709,0.206208,0.170642,0.169553,0.128631,0.147736,0.155259,0.147736,0.155259,0.147736,0.173669,0.156962,0.148089,0.155649,0.155259,0.129256,0.115242,0.127223,0.136064,0.132952,0.122772,0.120779,0.190379,0.122772,0.122772,0.125084,0.125931,0.122772,0.125337,0.127049,0.146112,0.147036,0.126531,0.122772,0.126531,0.126962,0.134006,0.134006,0.135076,0.13547,0.128187,0.108899,0.151343],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var23_df_closseness, by=list(var23_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var23_df_closseness, by=list(var23_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-497d4d02342a4db31506" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-497d4d02342a4db31506">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000119\" data-max=\"0.000125\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000398\" data-max=\"0.000713\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8.2e-05\" data-max=\"0.000303\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000788\" data-max=\"0.000954\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.3e-05\" data-max=\"0.000135\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022123\" data-max=\"0.023305\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.4e-05\" data-max=\"0.002032\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.074009\" data-max=\"0.13266\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.015202\" data-max=\"0.056402\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.146639\" data-max=\"0.17749\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.008103\" data-max=\"0.025109\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172406\" data-max=\"0.308329\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.029295\" data-max=\"0.135782\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[0.000125,0.000125,0.000119],[0,1e-06,1.1e-05],[0.000713,0.000618,0.000398],[8.2e-05,0.000194,0.000303],[0.000954,0.000926,0.000788],[4.3e-05,0.000135,0.000133],[0.023305,0.023199,0.022123],[4.4e-05,0.000214,0.002032],[0.13266,0.115023,0.074009],[0.015202,0.036057,0.056402],[0.17749,0.172309,0.146639],[0.008103,0.025109,0.024732],[0.308329,0.266869,0.172406],[0.029295,0.082605,0.135782]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var23_data.RData")
```

