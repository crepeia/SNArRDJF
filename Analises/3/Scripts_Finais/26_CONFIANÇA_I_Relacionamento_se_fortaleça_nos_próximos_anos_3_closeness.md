# SNA Closeness 26_CONFIANÇA I) Espera que o relacionamento com este serviço se fortaleça nos próximos anos. (var24)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 26_CONFIANÇA I) Espera que o relacionamento com este serviço se fortaleça nos próximos anos. (var24)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var24_data.RData")
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
#var24<-simplify(var24) #Simplify
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
V(var24)$incloseness <- closeness(var24, mode = "in", weights = E(var24)$var24) %>% round(6)
V(var24)$outcloseness <- closeness(var24, mode = "out", weights = E(var24)$var24) %>% round(6)
V(var24)$totalcloseness <- closeness(var24, mode = "total", weights = E(var24)$var24) %>% round(4)
```

###Saving to Environment

```r
var24_incloseness<- closeness(var24, mode = "in", weights = E(var24)$var24) %>% round(6)
var24_outcloseness<- closeness(var24, mode = "out", weights = E(var24)$var24) %>% round(6)
var24_totalcloseness<- closeness(var24, mode = "total", weights = E(var24)$var24) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var24_incloseness)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000029 0.000132 0.000136 0.000134 0.000137 0.000144
```

```r
sd(var24_incloseness)
```

```
## [1] 8.701104e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var24)$incloseness<-closeness(var24, weights = E(var24)$var24, mode="in")

#Get Variable
V(var24)$var24_color_degree<-round(V(var24)$incloseness,6)

#Creating brewer pallette
vertex_var24_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var24)$var24_color_degree)), "RdBu"))(
            length(unique(V(var24)$var24_color_degree)))

#Saving as Vertex properties 
V(var24)$vertex_var24_color_degree<-
  vertex_var24_color_degree[as.numeric(
  cut(V(var24)$var24_color_degree,
      breaks=length(unique(V(var24)$var24_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var24, es=E(var24), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var24))
maxC <- rep(Inf, vcount(var24))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var24, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var24)$weight)


#PLotting
plot(var24, 
     layout=co,
     edge.color=V(var24)$vertex_var24_color_degree[edge.start],
     edge.arrow.size=closeness(var24, weights = E(var24)$var24, mode="in"),
     edge.width=E(var24)$weight/mean(E(var24)$weight),
     edge.curved = TRUE,
     vertex.color=V(var24)$vertex_var24_color_degree,
     vertex.size=closeness(var24, weights = E(var24)$var24, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var24,"LABEL_COR"),
     vertex.label.cex=(closeness(var24, weights = E(var24)$var24, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var24)$var24_color_degree
b<-V(var24)$vertex_var24_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 26_CONFIANÇA I) Espera que o relacionamento com este serviço se fortaleça nos próximos anos. (var24)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var24, mode="in", weights = E(var24)$var24)), 
             sd(closeness(var24, mode="in", weights = E(var24)$var24))
             )
       )
```

![](26_CONFIANÇA_I_Relacionamento_se_fortaleça_nos_próximos_anos_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var24_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0005015 0.0006540 0.0005445 0.0006795 0.0009960
```

```r
sd(var24_outcloseness)
```

```
## [1] 0.0002568652
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var24)$outcloseness<-closeness(var24, weights = E(var24)$var24, mode="out")

#Get Variable
V(var24)$var24_color_degree<-round(V(var24)$outcloseness,6)

#Creating brewer pallette
vertex_var24_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var24)$var24_color_degree)), "RdBu"))(
            length(unique(V(var24)$var24_color_degree)))

#Saving as Vertex properties 
V(var24)$vertex_var24_color_degree<-
  vertex_var24_color_degree[as.numeric(
  cut(V(var24)$var24_color_degree,
      breaks=length(unique(V(var24)$var24_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var24, es=E(var24), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var24))
maxC <- rep(Inf, vcount(var24))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var24, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var24)$weight)


#PLotting
plot(var24, 
     layout=co,
     edge.color=V(var24)$vertex_var24_color_degree[edge.start],
     edge.arrow.size=closeness(var24, weights = E(var24)$var24, mode="out"),
     edge.width=E(var24)$weight/2*mean(E(var24)$weight),
     edge.curved = TRUE,
     vertex.color=V(var24)$vertex_var24_color_degree,
     vertex.size=closeness(var24, weights = E(var24)$var24, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var24,"LABEL_COR"),
     vertex.label.cex=closeness(var24, weights = E(var24)$var24, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var24)$var24_color_degree
b<-V(var24)$vertex_var24_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 26_CONFIANÇA I) Espera que o relacionamento com este serviço se fortaleça nos próximos anos. (var24)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var24, mode="out", weights = E(var24)$var24)), 
             sd(closeness(var24, mode="out", weights = E(var24)$var24))
             )
       )
```

![](26_CONFIANÇA_I_Relacionamento_se_fortaleça_nos_próximos_anos_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var24_totalcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0005160 0.0008145 0.0008510 0.0008666 0.0009265 0.0014750
```

```r
sd(var24_totalcloseness)
```

```
## [1] 0.0001450966
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var24)$allcloseness<-closeness(var24, weights = E(var24)$var24, mode="all")

#Get Variable
V(var24)$var24_color_degree<-round(V(var24)$allcloseness,6)

#Creating brewer pallette
vertex_var24_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var24)$var24_color_degree)), "RdBu"))(
            length(unique(V(var24)$var24_color_degree)))

#Saving as Vertex properties 
V(var24)$vertex_var24_color_degree<-
  vertex_var24_color_degree[as.numeric(
  cut(V(var24)$var24_color_degree,
      breaks=length(unique(V(var24)$var24_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var24, es=E(var24), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var24))
maxC <- rep(Inf, vcount(var24))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var24, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var24)$weight)


#PLotting
plot(var24, 
     layout=co,
     edge.color=V(var24)$vertex_var24_color_degree[edge.start],
     edge.arrow.size=closeness(var24, weights = E(var24)$var24, mode="all"),
     edge.width=E(var24)$weight/2*mean(E(var24)$weight),
     edge.curved = TRUE,
     vertex.color=V(var24)$vertex_var24_color_degree,
     vertex.size=closeness(var24, weights = E(var24)$var24, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var24,"LABEL_COR"),
     vertex.label.cex=(closeness(var24, weights = E(var24)$var24, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var24)$var24_color_degree
b<-V(var24)$vertex_var24_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 26_CONFIANÇA I) Espera que o relacionamento com este serviço se fortaleça nos próximos anos. (var24)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var24, mode="all", weights = E(var24)$var24)), 
             sd(closeness(var24, mode="all", weights = E(var24)$var24))
             )
       )
```

![](26_CONFIANÇA_I_Relacionamento_se_fortaleça_nos_próximos_anos_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var24)$incloseness_n <- closeness(var24, mode = "in",, weights = E(var24)$var24, normalized = T) %>% round(10)
V(var24)$outcloseness_n <- closeness(var24, mode = "out", normalized = T, weights = E(var24)$var24) %>% round(6)
V(var24)$totalcloseness_n <- closeness(var24, mode = "total", normalized = T, weights = E(var24)$var24) %>% round(6)
```

###Saving to Environment

```r
var24_incloseness_n<- closeness(var24, mode = "in", normalized = T, weights = E(var24)$var24) %>% round(6)
var24_outcloseness_n<- closeness(var24, mode = "out", normalized = T, weights = E(var24)$var24) %>% round(6)
var24_totalcloseness_n<- closeness(var24, mode = "total", normalized = T, weights = E(var24)$var24) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var24_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.024640 0.025380 0.024920 0.025430 0.026740
```

```r
sd(var24_incloseness_n)
```

```
## [1] 0.001614877
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var24)$incloseness_n<-closeness(var24, weights = E(var24)$var24, mode="in", normalized = T)

#Get Variable
V(var24)$var24_color_degree<-round(V(var24)$incloseness_n,6)

#Creating brewer pallette
vertex_var24_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var24)$var24_color_degree)), "RdBu"))(
            length(unique(V(var24)$var24_color_degree)))

#Saving as Vertex properties 
V(var24)$vertex_var24_color_degree<-
  vertex_var24_color_degree[as.numeric(
  cut(V(var24)$var24_color_degree,
      breaks=length(unique(V(var24)$var24_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var24, es=E(var24), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var24))
maxC <- rep(Inf, vcount(var24))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var24, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var24)$weight)


#PLotting
plot(var24, 
     layout=co,
     edge.color=V(var24)$vertex_var24_color_degree[edge.start],
     edge.arrow.size=closeness(var24, weights = E(var24)$var24, mode="in",normalized = T),
     edge.width=E(var24)$weight/10*mean(E(var24)$weight),
     edge.curved = TRUE,
     vertex.color=V(var24)$vertex_var24_color_degree,
     vertex.size=(closeness(var24, weights = E(var24)$var24, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var24,"LABEL_COR"),
     vertex.label.cex=closeness(var24, weights = E(var24)$var24, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var24)$var24_color_degree
b<-V(var24)$vertex_var24_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 26_CONFIANÇA I) Espera que o relacionamento com este serviço se fortaleça nos próximos anos. (var24)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var24, mode="in", weights = E(var24)$var24, normalized = T)), 
             sd(closeness(var24, mode="in", weights = E(var24)$var24, normalized = T))
             )
       )
```

![](26_CONFIANÇA_I_Relacionamento_se_fortaleça_nos_próximos_anos_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var24_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.093260 0.121700 0.101300 0.126400 0.185300
```

```r
sd(var24_outcloseness_n)
```

```
## [1] 0.04778976
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var24)$outcloseness_n<-closeness(var24, weights = E(var24)$var24, mode="out", normalized = T)

#Get Variable
V(var24)$var24_color_degree<-round(V(var24)$outcloseness_n,6)

#Creating brewer pallette
vertex_var24_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var24)$var24_color_degree)), "RdBu"))(
            length(unique(V(var24)$var24_color_degree)))

#Saving as Vertex properties 
V(var24)$vertex_var24_color_degree<-
  vertex_var24_color_degree[as.numeric(
  cut(V(var24)$var24_color_degree,
      breaks=length(unique(V(var24)$var24_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var24, es=E(var24), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var24))
maxC <- rep(Inf, vcount(var24))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var24, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var24)$weight)


#PLotting
plot(var24, 
     layout=co,
     edge.color=V(var24)$vertex_var24_color_degree[edge.start],
     edge.arrow.size=closeness(var24, weights = E(var24)$var24, mode="out",normalized = T),
     edge.width=E(var24)$weight/10*mean(E(var24)$weight),
     edge.curved = TRUE,
     vertex.color=V(var24)$vertex_var24_color_degree,
     vertex.size=(closeness(var24, weights = E(var24)$var24, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var24,"LABEL_COR"),
     vertex.label.cex=closeness(var24, weights = E(var24)$var24, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var24)$var24_color_degree
b<-V(var24)$vertex_var24_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 26_CONFIANÇA I) Espera que o relacionamento com este serviço se fortaleça nos próximos anos. (var24)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var24, mode="out", weights = E(var24)$var24, normalized = T)), 
             sd(closeness(var24, mode="out", weights = E(var24)$var24, normalized = T))
             )
       )
```

![](26_CONFIANÇA_I_Relacionamento_se_fortaleça_nos_próximos_anos_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var24_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.09598 0.15150 0.15830 0.16120 0.17230 0.27430
```

```r
sd(var24_totalcloseness_n)
```

```
## [1] 0.02697796
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var24)$allcloseness_n<-closeness(var24, weights = E(var24)$var24, mode="all", normalized = T)

#Get Variable
V(var24)$var24_color_degree<-round(V(var24)$allcloseness_n,6)

#Creating brewer pallette
vertex_var24_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var24)$var24_color_degree)), "RdBu"))(
            length(unique(V(var24)$var24_color_degree)))

#Saving as Vertex properties 
V(var24)$vertex_var24_color_degree<-
  vertex_var24_color_degree[as.numeric(
  cut(V(var24)$var24_color_degree,
      breaks=length(unique(V(var24)$var24_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var24, es=E(var24), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var24))
maxC <- rep(Inf, vcount(var24))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var24, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var24)$weight)


#PLotting
plot(var24, 
     layout=co,
     edge.color=V(var24)$vertex_var24_color_degree[edge.start],
     edge.arrow.size=closeness(var24, weights = E(var24)$var24, mode="all",normalized = T),
     edge.width=E(var24)$weight/10*mean(E(var24)$weight),
     edge.curved = TRUE,
     vertex.color=V(var24)$vertex_var24_color_degree,
     vertex.size=(closeness(var24, weights = E(var24)$var24, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var24,"LABEL_COR"),
     vertex.label.cex=closeness(var24, weights = E(var24)$var24, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var24)$var24_color_degree
b<-V(var24)$vertex_var24_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 26_CONFIANÇA I) Espera que o relacionamento com este serviço se fortaleça nos próximos anos. (var24)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var24, mode="all", weights = E(var24)$var24, normalized = T)), 
             sd(closeness(var24, mode="all", weights = E(var24)$var24, normalized = T))
             )
       )
```

![](26_CONFIANÇA_I_Relacionamento_se_fortaleça_nos_próximos_anos_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var24)$incloseness_n <- closeness(var24, weights = E(var24)$var24, mode = "in", normalized = T) %>% round(6)
V(var24)$outcloseness_n <- closeness(var24, weights = E(var24)$var24, mode = "out", normalized = T) %>% round(6)
V(var24)$totalcloseness_n <- closeness(var24, weights = E(var24)$var24, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var24)$var24_centr_closeness<- centralization.closeness(var24)$res
var24_centr_closeness<- centralization.closeness(var24)$res
var24_centr_closeness_all<- centralization.closeness(var24)
```

###Centralization

```r
var24_centr_closeness_all$centralization
```

```
## [1] 0.1578822
```

###Theoretical Max

```r
var24_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var24)$var24_centr_closeness<- centralization.closeness(var24)$res

#Get Variable
V(var24)$var24_color_degree<-round(V(var24)$var24_centr_closeness,6)

#Creating brewer pallette
vertex_var24_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var24)$var24_color_degree)), "Spectral"))(
            length(unique(V(var24)$var24_color_degree)))

#Saving as Vertex properties 
V(var24)$vertex_var24_color_degree<-
  vertex_var24_color_degree[as.numeric(
  cut(V(var24)$var24_color_degree,
      breaks=length(unique(V(var24)$var24_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var24, es=E(var24), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var24))
maxC <- rep(Inf, vcount(var24))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var24, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var24)$weight)


#PLotting
plot(var24, 
     layout=co,
     edge.color=V(var24)$vertex_var24_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var24)$res,
     edge.width=E(var24)$weight/10*mean(E(var24)$weight),
     edge.curved = TRUE,
     vertex.color=V(var24)$vertex_var24_color_degree,
     vertex.size=centralization.closeness(var24)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var24,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var24)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var24)$var24_color_degree
b<-V(var24)$vertex_var24_color_degree
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
  title("Network Centralization Closeness - 26_CONFIANÇA I) Espera que o relacionamento com este serviço se fortaleça nos próximos anos. (var24)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var24)$res), 
             sd(centralization.closeness(var24)$res)
             )
       )
```

![](26_CONFIANÇA_I_Relacionamento_se_fortaleça_nos_próximos_anos_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var24_incloseness<- closeness(var24, weights = E(var24)$var24, mode = "in") %>% round(6)
var24_outcloseness<- closeness(var24, weights = E(var24)$var24, mode = "out") %>% round(6)
var24_totalcloseness<- closeness(var24, weights = E(var24)$var24, mode = "total") %>% round(6)
var24_incloseness_n<- closeness(var24,weights = E(var24)$var24, mode = "in", normalized = T) %>% round(6)
var24_outcloseness_n<- closeness(var24,weights = E(var24)$var24, mode = "out", normalized = T) %>% round(6)
var24_totalcloseness_n<- closeness(var24,weights = E(var24)$var24, mode = "total", normalized = T) %>% round(6)
var24_centr_closeness <- centralization.closeness(var24)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var24_df_closseness <- data.frame(
var24_incloseness,
var24_outcloseness,
var24_totalcloseness,
var24_incloseness_n,
var24_outcloseness_n,
var24_totalcloseness_n,
var24_centr_closeness) %>% round(6)

#Adding type
var24_df_closseness <-cbind(var24_df_closseness, V(var24)$LABEL_COR)

#Adding names
names(var24_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var24_df_closseness<-var24_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var24_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-70177411072aa3624113" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-70177411072aa3624113">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000144\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000996\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000516\" data-max=\"0.001475\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.026736\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.185259\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.095975\" data-max=\"0.274336\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[0.000141,0.000139,0.000144,0.000137,0.000138,0.000137,0.000136,0.000139,0.000132,0.000139,0.000138,0.000138,0.00013,0.000132,0.000136,0.000137,0.000138,0.000139,0.000131,0.000136,0.000138,0.000132,0.000138,0.000125,0.000132,0.000135,0.000129,0.000138,0.000137,0.000137,0.000137,0.000136,0.000138,0.000129,0.000137,0.000132,0.000139,0.000136,0.000135,0.000137,0.000137,0.000137,0.000136,0.000139,0.000135,0.000138,0.000124,0.000136,0.000136,0.000136,0.000136,0.000136,0.000125,0.000134,0.00013,0.000137,0.000137,0.000129,0.000131,0.000139,0.00014,0.000128,0.000137,0.000136,0.000133,2.9e-05,0.000125,0.000136,0.000133,0.000136,0.000137,0.000137,0.000137,0.000137,0.000137,0.000136,0.000137,0.000138,0.000126,0.000126,0.000126,0.000126,0.000137,0.000133,0.000133,0.000137,0.000135,0.000123,0.000137,0.000136,0.000137,0.000125,0.000128,0.000136,0.000137,0.000133,0.000123,0.000131,0.000137,0.000137,0.000126,0.000133,0.000135,0.000135,0.000136,0.000136,0.000137,0.000135,0.000136,0.000137,0.000137,0.000137,0.000137,0.000137,0.000137,0.000137,0.000136,0.000136,0.000137,0.000137,0.000136,0.000136,0.000137,0.000137,0.000137,0.000137,0.000137,0.000137,0.000137,0.000134,0.000137,0.000137,0.000137,0.000137,0.000137,0.000137,0.000136,0.000136,0.000137,0.000136,0.000137,0.000136,0.000136,0.000136,0.000135,0.000139,0.000138,0.000137,0.000131,0.000134,0.000137,0.000134,0.000137,0.000134,0.000136,0.000126,0.000138,0.00014,0.00014,0.000126,0.000127,0.000132,0.000137,0.000136,0.000127,0.000128,0.000132,0.000127,0.000127,0.000132,0.000132,0.000127,0.000132,0.000133,0.000134,0.000134,0.000132,0.000127,0.000132,0.000132,0.000133,0.000133,0.000134,0.000133,0.000132,0.000128,0.000138],[0.000915,0.000815,0.000996,0.000773,0.000721,0.000699,0.000723,0.000738,0.000729,0.000652,0.000672,0.000659,0.000775,0.000765,0.000786,0.000654,0.000854,0.000742,0.00067,0.000648,0.000698,0.000646,0.000672,0.000786,0.000639,0.00052,0.000621,0.000754,0.000792,0.000732,0.000707,0.000684,0.000723,0.000483,0.000493,0.000646,0.000706,0.000598,0.000679,0.000754,0.000774,0.000747,0.000747,0.000669,0.00068,0.000687,0.0005,0.000657,0.000657,0.000657,0.000657,0.000657,0.000637,0.000681,0.000787,0.000739,0.000699,0.000656,0.000586,0.000752,2.9e-05,0.000491,0.000675,0.000692,0.000683,2.9e-05,2.9e-05,0.000703,0.000695,0.000645,0.000645,0.000667,0.000672,0.000671,0.00066,0.000661,0.000659,0.000734,0.000495,0.000495,0.000495,0.000495,0.000658,0.000548,0.000676,0.000675,0.000669,0.000616,0.00069,0.000651,0.000663,0.000694,0.000658,0.000541,0.00068,0.000657,0.00046,0.000477,0.000649,0.000952,0.000503,0.000734,0.000473,0.000473,0.000666,0.000688,0.000664,0.000503,0.000503,0.000678,0.000678,0.000662,0.000654,0.000651,0.000655,0.000888,0.000676,0.000678,0.00066,0.00066,0.000669,0.000712,0.000645,0.000645,0.000645,0.00066,0.000643,0.00065,0.000643,0.00066,0.000659,0.000659,0.000661,0.000643,0.000643,0.000677,0.000648,0.000643,0.000643,0.000659,0.000644,0.000643,0.000649,0.000643,0.000643,0.000643,0.000659,0.000475,0.000553,0.000562,0.000562,0.000562,0.000562,0.000562,0.000531,0.000591,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001393,0.001221,0.001475,0.00096,0.000958,0.000942,0.000918,0.001101,0.000878,0.000997,0.000911,0.000967,0.000955,0.000915,0.000991,0.00105,0.001075,0.001143,0.000856,0.00088,0.001106,0.000834,0.001107,0.00098,0.000835,0.000873,0.000887,0.001115,0.000954,0.000931,0.000923,0.000872,0.000973,0.000679,0.000894,0.000849,0.001032,0.00106,0.001052,0.000942,0.000945,0.000926,0.000935,0.001015,0.000861,0.00092,0.000697,0.001052,0.001052,0.001052,0.001052,0.001052,0.000802,0.000873,0.001012,0.000961,0.000873,0.000898,0.000857,0.001181,0.00092,0.000695,0.000862,0.000867,0.000869,0.000516,0.000652,0.000895,0.0009,0.000836,0.000842,0.000847,0.000896,0.0009,0.000854,0.000858,0.000915,0.000938,0.000615,0.000615,0.000615,0.000615,0.00085,0.000862,0.000927,0.000839,0.000949,0.000798,0.000877,0.000842,0.000845,0.000856,0.000826,0.00083,0.000856,0.000916,0.000625,0.000714,0.000841,0.001287,0.000643,0.000893,0.001046,0.001046,0.000847,0.00086,0.000853,0.001045,0.001045,0.000915,0.000847,0.000847,0.000921,0.000842,0.00084,0.00122,0.000854,0.000857,0.000848,0.000859,0.000847,0.000891,0.000842,0.000834,0.000842,0.000847,0.000834,0.00084,0.000847,0.000847,0.000832,0.000836,0.000841,0.000844,0.000842,0.000851,0.000842,0.00085,0.000865,0.00085,0.000849,0.000841,0.000838,0.00084,0.000818,0.001094,0.000916,0.000907,0.000695,0.000796,0.000832,0.000796,0.000832,0.000796,0.00093,0.000844,0.000799,0.000834,0.000832,0.000695,0.000619,0.000684,0.000723,0.000707,0.00066,0.000649,0.000671,0.00066,0.00066,0.000672,0.000677,0.00066,0.000674,0.000683,0.000786,0.000791,0.000679,0.00066,0.000679,0.000683,0.000719,0.000719,0.000727,0.000721,0.000689,0.000586,0.000811],[0.026279,0.02588,0.026736,0.025532,0.025606,0.025542,0.025316,0.025902,0.024503,0.025805,0.025588,0.025655,0.024212,0.024564,0.025358,0.025452,0.025662,0.025866,0.024326,0.025265,0.025634,0.024548,0.025648,0.02327,0.024538,0.025132,0.024037,0.025669,0.025521,0.0255,0.025563,0.025386,0.025609,0.024037,0.025521,0.024642,0.025776,0.025203,0.025166,0.025462,0.025553,0.025399,0.025316,0.025823,0.025162,0.025627,0.023129,0.025237,0.025237,0.025237,0.025237,0.025237,0.023285,0.02494,0.024103,0.025521,0.025438,0.024065,0.024326,0.025805,0.026043,0.023797,0.025396,0.025386,0.024685,0.005348,0.023282,0.025354,0.024747,0.025337,0.02541,0.025427,0.025431,0.025452,0.025462,0.025386,0.025417,0.02568,0.023393,0.023393,0.023393,0.023393,0.025417,0.024668,0.024777,0.025431,0.025111,0.022932,0.025438,0.025344,0.025396,0.023186,0.02388,0.025241,0.025427,0.024731,0.022932,0.024451,0.025452,0.025525,0.023526,0.024695,0.025176,0.025176,0.025386,0.025386,0.025396,0.025179,0.02522,0.025417,0.025396,0.025476,0.025427,0.025431,0.025431,0.025389,0.025386,0.025386,0.025406,0.025479,0.025386,0.025323,0.025427,0.025399,0.025427,0.02542,0.025399,0.02541,0.02542,0.024956,0.025389,0.02541,0.025417,0.02541,0.025427,0.025493,0.025386,0.025386,0.025427,0.025375,0.025427,0.025375,0.025365,0.025375,0.025122,0.025844,0.025578,0.025399,0.024368,0.024916,0.025389,0.024916,0.025389,0.024916,0.025303,0.023512,0.025578,0.026043,0.026032,0.023488,0.023649,0.024632,0.025413,0.025299,0.023634,0.023831,0.024548,0.023634,0.023634,0.024616,0.024632,0.023634,0.024616,0.024685,0.02486,0.0249,0.024593,0.023634,0.024593,0.024603,0.024701,0.024701,0.02493,0.024741,0.024593,0.023825,0.02573],[0.170174,0.151589,0.185259,0.143852,0.134102,0.129979,0.13449,0.137269,0.135569,0.121331,0.124916,0.12261,0.144186,0.142311,0.146226,0.121728,0.158839,0.137982,0.124665,0.120544,0.129798,0.120077,0.125,0.146112,0.118774,0.096774,0.115528,0.140271,0.147268,0.136064,0.131542,0.127136,0.134393,0.089768,0.091716,0.120077,0.131263,0.111311,0.126273,0.140271,0.143963,0.139013,0.139013,0.124415,0.126531,0.127747,0.092954,0.122127,0.122127,0.122127,0.122127,0.122127,0.118471,0.126617,0.146341,0.137371,0.129979,0.121967,0.108963,0.139955,0.005348,0.091311,0.125506,0.128631,0.127049,0.005376,0.005405,0.130802,0.129256,0.12,0.12,0.124,0.125,0.124748,0.122691,0.122853,0.12261,0.136564,0.091988,0.091988,0.091988,0.091988,0.122368,0.101918,0.125761,0.125506,0.124415,0.114532,0.128364,0.121173,0.12326,0.129077,0.122368,0.100704,0.126531,0.122208,0.085478,0.088783,0.120701,0.177143,0.093561,0.136464,0.087943,0.087943,0.123835,0.128011,0.123424,0.093608,0.093608,0.126016,0.126102,0.123097,0.121728,0.121173,0.121887,0.165187,0.125761,0.126016,0.122772,0.122772,0.124498,0.132384,0.12,0.12,0.12,0.122772,0.119537,0.120936,0.119537,0.122772,0.12253,0.12253,0.123016,0.119537,0.119537,0.125931,0.120544,0.119537,0.119537,0.12253,0.119768,0.119537,0.120623,0.119537,0.119537,0.119537,0.12253,0.088361,0.102819,0.104553,0.104553,0.104553,0.104553,0.104553,0.098831,0.109929,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.259053,0.227106,0.274336,0.178503,0.178161,0.175141,0.170799,0.204846,0.163301,0.185444,0.169399,0.179884,0.17765,0.170174,0.184341,0.195378,0.2,0.212571,0.159247,0.163588,0.205752,0.155129,0.20598,0.182353,0.155388,0.162304,0.164894,0.207358,0.177481,0.173184,0.171745,0.162162,0.180934,0.126273,0.16622,0.157895,0.19195,0.197243,0.195584,0.175141,0.175803,0.172222,0.173832,0.188832,0.160069,0.171113,0.129707,0.195584,0.195584,0.195584,0.195584,0.195584,0.149158,0.162304,0.188259,0.178674,0.162304,0.167116,0.159383,0.219599,0.171113,0.129256,0.160345,0.161179,0.161599,0.095975,0.121252,0.166517,0.167417,0.155518,0.156698,0.157494,0.166667,0.167417,0.158839,0.159657,0.170174,0.174484,0.114462,0.114462,0.114462,0.114462,0.158163,0.160345,0.172382,0.15604,0.176471,0.148444,0.163158,0.156566,0.157095,0.159247,0.153719,0.154357,0.159247,0.17033,0.116323,0.132762,0.156434,0.239382,0.119537,0.166071,0.194561,0.194561,0.157627,0.159931,0.158703,0.194357,0.194357,0.170174,0.157627,0.157627,0.171271,0.156566,0.156303,0.226829,0.158839,0.159383,0.157761,0.159794,0.157627,0.165775,0.156566,0.155129,0.156566,0.157494,0.155129,0.156171,0.157494,0.157494,0.154742,0.155518,0.156434,0.156962,0.156566,0.158298,0.156566,0.158163,0.1609,0.158029,0.157895,0.156434,0.155909,0.156303,0.152209,0.203501,0.17033,0.168784,0.129346,0.147971,0.154742,0.147971,0.154742,0.147971,0.173023,0.156962,0.148562,0.155129,0.154742,0.129256,0.11517,0.127223,0.13449,0.131449,0.122772,0.120779,0.124832,0.122772,0.122772,0.125084,0.125931,0.122772,0.125337,0.127049,0.146112,0.147036,0.126359,0.122772,0.126359,0.126962,0.133813,0.133813,0.135174,0.134102,0.128187,0.109027,0.150852],[0.372,0.343808,0.392405,0.333932,0.30897,0.301948,0.304918,0.319039,0.311558,0.288372,0.294304,0.286154,0.333932,0.322917,0.287481,0.29108,0.352273,0.315254,0.292913,0.286154,0.302932,0.285714,0.296178,0.333333,0.285714,0.240621,0.277198,0.319039,0.333333,0.316327,0.310518,0.2976,0.315789,0.224096,0.230198,0.285714,0.306931,0.267626,0.298555,0.32069,0.334532,0.317406,0.317406,0.293839,0.299035,0.300485,0.23192,0.291994,0.291994,0.291994,0.291994,0.291994,0.282675,0.297125,0.331551,0.317949,0.304419,0.288372,0.261972,0.324042,0.005348,0.228221,0.297125,0.291536,0.299517,0.005376,0.005405,0.307438,0.298555,0.283537,0.283537,0.294304,0.296651,0.296178,0.289269,0.290172,0.288372,0.314721,0.229346,0.229346,0.229346,0.229346,0.287926,0.250674,0.295238,0.299035,0.292453,0.274742,0.302932,0.286154,0.291536,0.302439,0.289269,0.248996,0.2976,0.288372,0.216028,0.223289,0.285714,0.307438,0.234552,0.318493,0.223022,0.223022,0.290625,0.301948,0.290172,0.231056,0.231056,0.295707,0.296178,0.28972,0.287037,0.285714,0.287037,0.291994,0.29477,0.295238,0.290172,0.290172,0.295238,0.311037,0.283537,0.283537,0.283537,0.291994,0.282675,0.285714,0.282675,0.291994,0.291536,0.291536,0.292453,0.282675,0.282675,0.298555,0.284839,0.282675,0.282675,0.291536,0.283105,0.282675,0.286154,0.282675,0.282675,0.282675,0.291536,0.221165,0.252374,0.256552,0.256552,0.256552,0.256552,0.256552,0.226553,0.227106,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var24_df_closseness, by=list(var24_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var24_df_closseness, by=list(var24_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-4d5456c0633cb635ca0e" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-4d5456c0633cb635ca0e">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000127\" data-max=\"0.000144\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000996\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.3e-05\" data-max=\"0.000278\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000736\" data-max=\"0.001475\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4e-06\" data-max=\"0.000153\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.023558\" data-max=\"0.026736\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.4e-05\" data-max=\"0.004505\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.185259\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.002332\" data-max=\"0.051801\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.136954\" data-max=\"0.274336\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000805\" data-max=\"0.028419\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003897\" data-max=\"0.126\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[0.000135,0.00013,0.000136,0.000139,0.000136,0.000139,0.000138,0.000144,0.000138,0.000138,0.000138,0.000127,0.000135,0.000137,0.000138,0.000134,0.000138,0.000137,0.000141,0.000136,0.000139,0.000137,0.000137],[4e-06,4e-06,null,null,null,null,1e-06,null,null,null,null,2.4e-05,4e-06,0,1e-06,3e-06,null,null,null,1e-06,null,2e-06,1e-06],[0.000683,0.000235,0.000648,0.000815,0.000723,0.000752,0.000698,0.000996,0.000672,0.000659,0.000734,0.000604,0.000756,0.000695,0.000758,0.00068,2.9e-05,0.000952,0.000915,0.000597,0.000742,0.00053,0.000667],[0.000116,0.000278,null,null,null,null,5.6e-05,null,null,null,null,0.000164,1.3e-05,8.8e-05,8.4e-05,7e-05,null,null,null,8.3e-05,null,0.000226,4.9e-05],[0.000871,0.000736,0.00088,0.001221,0.000918,0.001181,0.001105,0.001475,0.001107,0.000916,0.000938,0.000853,0.00092,0.000936,0.000992,0.000861,0.000811,0.001287,0.001393,0.00105,0.001143,0.00083,0.000869],[0.000153,0.000111,null,null,null,null,1.1e-05,null,null,null,null,0.000146,8e-06,2.6e-05,7.3e-05,8.2e-05,null,null,null,4e-06,null,1.8e-05,5.9e-05],[0.025177,0.02424,0.025265,0.02588,0.025316,0.025805,0.025716,0.026736,0.025648,0.025578,0.02568,0.023558,0.024982,0.025559,0.025603,0.02487,0.02573,0.025525,0.026279,0.02523,0.025866,0.025385,0.025398],[0.000701,0.000705,null,null,null,null,0.000113,null,null,null,null,0.004505,0.00059,5.4e-05,6e-05,0.000583,null,null,null,7.6e-05,null,0.000312,7.5e-05],[0.127058,0.043631,0.120544,0.151589,0.13449,0.139955,0.129869,0.185259,0.125,0.12253,0.136564,0.112362,0.140662,0.129232,0.140973,0.126478,0.005348,0.177143,0.170174,0.111087,0.137982,0.098554,0.123981],[0.02153,0.051801,null,null,null,null,0.010367,null,null,null,null,0.03053,0.002332,0.016315,0.015609,0.013049,null,null,null,0.015479,null,0.042117,0.009045],[0.162028,0.136954,0.163588,0.227106,0.170799,0.219599,0.205537,0.274336,0.20598,0.17033,0.174484,0.158602,0.171198,0.174023,0.184434,0.160109,0.150852,0.239382,0.259053,0.19533,0.212571,0.154303,0.161614],[0.028419,0.020605,null,null,null,null,0.001937,null,null,null,null,0.027102,0.001448,0.004804,0.013565,0.015192,null,null,null,0.000805,null,0.003439,0.011029],[0.300898,0.099209,0.286154,0.343808,0.304918,0.324042,0.301549,0.392405,0.296178,0.291536,0.314721,0.267055,0.320162,0.302578,0.321064,0.297137,0.005348,0.307438,0.372,0.268782,0.315254,0.234593,0.28946],[0.043056,0.126,null,null,null,null,0.018221,null,null,null,null,0.070865,0.003897,0.03162,0.027255,0.024497,null,null,null,0.031785,null,0.102905,0.011722]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var24_df_closseness <- data.frame(
var24_incloseness,
var24_outcloseness,
var24_totalcloseness,
var24_incloseness_n,
var24_outcloseness_n,
var24_totalcloseness_n,
var24_centr_closeness) %>% round(6)

#Adding type
var24_df_closseness <-cbind(var24_df_closseness, V(var24)$TIPO1)

#Adding names
names(var24_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var24_df_closseness<-var24_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var24_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-5ee0c3bc24f7897bdaed" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-5ee0c3bc24f7897bdaed">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000144\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000996\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000516\" data-max=\"0.001475\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.026736\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.185259\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.095975\" data-max=\"0.274336\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[0.000141,0.000139,0.000144,0.000137,0.000138,0.000137,0.000136,0.000139,0.000132,0.000139,0.000138,0.000138,0.00013,0.000132,0.000136,0.000137,0.000138,0.000139,0.000131,0.000136,0.000138,0.000132,0.000138,0.000125,0.000132,0.000135,0.000129,0.000138,0.000137,0.000137,0.000137,0.000136,0.000138,0.000129,0.000137,0.000132,0.000139,0.000136,0.000135,0.000137,0.000137,0.000137,0.000136,0.000139,0.000135,0.000138,0.000124,0.000136,0.000136,0.000136,0.000136,0.000136,0.000125,0.000134,0.00013,0.000137,0.000137,0.000129,0.000131,0.000139,0.00014,0.000128,0.000137,0.000136,0.000133,2.9e-05,0.000125,0.000136,0.000133,0.000136,0.000137,0.000137,0.000137,0.000137,0.000137,0.000136,0.000137,0.000138,0.000126,0.000126,0.000126,0.000126,0.000137,0.000133,0.000133,0.000137,0.000135,0.000123,0.000137,0.000136,0.000137,0.000125,0.000128,0.000136,0.000137,0.000133,0.000123,0.000131,0.000137,0.000137,0.000126,0.000133,0.000135,0.000135,0.000136,0.000136,0.000137,0.000135,0.000136,0.000137,0.000137,0.000137,0.000137,0.000137,0.000137,0.000137,0.000136,0.000136,0.000137,0.000137,0.000136,0.000136,0.000137,0.000137,0.000137,0.000137,0.000137,0.000137,0.000137,0.000134,0.000137,0.000137,0.000137,0.000137,0.000137,0.000137,0.000136,0.000136,0.000137,0.000136,0.000137,0.000136,0.000136,0.000136,0.000135,0.000139,0.000138,0.000137,0.000131,0.000134,0.000137,0.000134,0.000137,0.000134,0.000136,0.000126,0.000138,0.00014,0.00014,0.000126,0.000127,0.000132,0.000137,0.000136,0.000127,0.000128,0.000132,0.000127,0.000127,0.000132,0.000132,0.000127,0.000132,0.000133,0.000134,0.000134,0.000132,0.000127,0.000132,0.000132,0.000133,0.000133,0.000134,0.000133,0.000132,0.000128,0.000138],[0.000915,0.000815,0.000996,0.000773,0.000721,0.000699,0.000723,0.000738,0.000729,0.000652,0.000672,0.000659,0.000775,0.000765,0.000786,0.000654,0.000854,0.000742,0.00067,0.000648,0.000698,0.000646,0.000672,0.000786,0.000639,0.00052,0.000621,0.000754,0.000792,0.000732,0.000707,0.000684,0.000723,0.000483,0.000493,0.000646,0.000706,0.000598,0.000679,0.000754,0.000774,0.000747,0.000747,0.000669,0.00068,0.000687,0.0005,0.000657,0.000657,0.000657,0.000657,0.000657,0.000637,0.000681,0.000787,0.000739,0.000699,0.000656,0.000586,0.000752,2.9e-05,0.000491,0.000675,0.000692,0.000683,2.9e-05,2.9e-05,0.000703,0.000695,0.000645,0.000645,0.000667,0.000672,0.000671,0.00066,0.000661,0.000659,0.000734,0.000495,0.000495,0.000495,0.000495,0.000658,0.000548,0.000676,0.000675,0.000669,0.000616,0.00069,0.000651,0.000663,0.000694,0.000658,0.000541,0.00068,0.000657,0.00046,0.000477,0.000649,0.000952,0.000503,0.000734,0.000473,0.000473,0.000666,0.000688,0.000664,0.000503,0.000503,0.000678,0.000678,0.000662,0.000654,0.000651,0.000655,0.000888,0.000676,0.000678,0.00066,0.00066,0.000669,0.000712,0.000645,0.000645,0.000645,0.00066,0.000643,0.00065,0.000643,0.00066,0.000659,0.000659,0.000661,0.000643,0.000643,0.000677,0.000648,0.000643,0.000643,0.000659,0.000644,0.000643,0.000649,0.000643,0.000643,0.000643,0.000659,0.000475,0.000553,0.000562,0.000562,0.000562,0.000562,0.000562,0.000531,0.000591,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001393,0.001221,0.001475,0.00096,0.000958,0.000942,0.000918,0.001101,0.000878,0.000997,0.000911,0.000967,0.000955,0.000915,0.000991,0.00105,0.001075,0.001143,0.000856,0.00088,0.001106,0.000834,0.001107,0.00098,0.000835,0.000873,0.000887,0.001115,0.000954,0.000931,0.000923,0.000872,0.000973,0.000679,0.000894,0.000849,0.001032,0.00106,0.001052,0.000942,0.000945,0.000926,0.000935,0.001015,0.000861,0.00092,0.000697,0.001052,0.001052,0.001052,0.001052,0.001052,0.000802,0.000873,0.001012,0.000961,0.000873,0.000898,0.000857,0.001181,0.00092,0.000695,0.000862,0.000867,0.000869,0.000516,0.000652,0.000895,0.0009,0.000836,0.000842,0.000847,0.000896,0.0009,0.000854,0.000858,0.000915,0.000938,0.000615,0.000615,0.000615,0.000615,0.00085,0.000862,0.000927,0.000839,0.000949,0.000798,0.000877,0.000842,0.000845,0.000856,0.000826,0.00083,0.000856,0.000916,0.000625,0.000714,0.000841,0.001287,0.000643,0.000893,0.001046,0.001046,0.000847,0.00086,0.000853,0.001045,0.001045,0.000915,0.000847,0.000847,0.000921,0.000842,0.00084,0.00122,0.000854,0.000857,0.000848,0.000859,0.000847,0.000891,0.000842,0.000834,0.000842,0.000847,0.000834,0.00084,0.000847,0.000847,0.000832,0.000836,0.000841,0.000844,0.000842,0.000851,0.000842,0.00085,0.000865,0.00085,0.000849,0.000841,0.000838,0.00084,0.000818,0.001094,0.000916,0.000907,0.000695,0.000796,0.000832,0.000796,0.000832,0.000796,0.00093,0.000844,0.000799,0.000834,0.000832,0.000695,0.000619,0.000684,0.000723,0.000707,0.00066,0.000649,0.000671,0.00066,0.00066,0.000672,0.000677,0.00066,0.000674,0.000683,0.000786,0.000791,0.000679,0.00066,0.000679,0.000683,0.000719,0.000719,0.000727,0.000721,0.000689,0.000586,0.000811],[0.026279,0.02588,0.026736,0.025532,0.025606,0.025542,0.025316,0.025902,0.024503,0.025805,0.025588,0.025655,0.024212,0.024564,0.025358,0.025452,0.025662,0.025866,0.024326,0.025265,0.025634,0.024548,0.025648,0.02327,0.024538,0.025132,0.024037,0.025669,0.025521,0.0255,0.025563,0.025386,0.025609,0.024037,0.025521,0.024642,0.025776,0.025203,0.025166,0.025462,0.025553,0.025399,0.025316,0.025823,0.025162,0.025627,0.023129,0.025237,0.025237,0.025237,0.025237,0.025237,0.023285,0.02494,0.024103,0.025521,0.025438,0.024065,0.024326,0.025805,0.026043,0.023797,0.025396,0.025386,0.024685,0.005348,0.023282,0.025354,0.024747,0.025337,0.02541,0.025427,0.025431,0.025452,0.025462,0.025386,0.025417,0.02568,0.023393,0.023393,0.023393,0.023393,0.025417,0.024668,0.024777,0.025431,0.025111,0.022932,0.025438,0.025344,0.025396,0.023186,0.02388,0.025241,0.025427,0.024731,0.022932,0.024451,0.025452,0.025525,0.023526,0.024695,0.025176,0.025176,0.025386,0.025386,0.025396,0.025179,0.02522,0.025417,0.025396,0.025476,0.025427,0.025431,0.025431,0.025389,0.025386,0.025386,0.025406,0.025479,0.025386,0.025323,0.025427,0.025399,0.025427,0.02542,0.025399,0.02541,0.02542,0.024956,0.025389,0.02541,0.025417,0.02541,0.025427,0.025493,0.025386,0.025386,0.025427,0.025375,0.025427,0.025375,0.025365,0.025375,0.025122,0.025844,0.025578,0.025399,0.024368,0.024916,0.025389,0.024916,0.025389,0.024916,0.025303,0.023512,0.025578,0.026043,0.026032,0.023488,0.023649,0.024632,0.025413,0.025299,0.023634,0.023831,0.024548,0.023634,0.023634,0.024616,0.024632,0.023634,0.024616,0.024685,0.02486,0.0249,0.024593,0.023634,0.024593,0.024603,0.024701,0.024701,0.02493,0.024741,0.024593,0.023825,0.02573],[0.170174,0.151589,0.185259,0.143852,0.134102,0.129979,0.13449,0.137269,0.135569,0.121331,0.124916,0.12261,0.144186,0.142311,0.146226,0.121728,0.158839,0.137982,0.124665,0.120544,0.129798,0.120077,0.125,0.146112,0.118774,0.096774,0.115528,0.140271,0.147268,0.136064,0.131542,0.127136,0.134393,0.089768,0.091716,0.120077,0.131263,0.111311,0.126273,0.140271,0.143963,0.139013,0.139013,0.124415,0.126531,0.127747,0.092954,0.122127,0.122127,0.122127,0.122127,0.122127,0.118471,0.126617,0.146341,0.137371,0.129979,0.121967,0.108963,0.139955,0.005348,0.091311,0.125506,0.128631,0.127049,0.005376,0.005405,0.130802,0.129256,0.12,0.12,0.124,0.125,0.124748,0.122691,0.122853,0.12261,0.136564,0.091988,0.091988,0.091988,0.091988,0.122368,0.101918,0.125761,0.125506,0.124415,0.114532,0.128364,0.121173,0.12326,0.129077,0.122368,0.100704,0.126531,0.122208,0.085478,0.088783,0.120701,0.177143,0.093561,0.136464,0.087943,0.087943,0.123835,0.128011,0.123424,0.093608,0.093608,0.126016,0.126102,0.123097,0.121728,0.121173,0.121887,0.165187,0.125761,0.126016,0.122772,0.122772,0.124498,0.132384,0.12,0.12,0.12,0.122772,0.119537,0.120936,0.119537,0.122772,0.12253,0.12253,0.123016,0.119537,0.119537,0.125931,0.120544,0.119537,0.119537,0.12253,0.119768,0.119537,0.120623,0.119537,0.119537,0.119537,0.12253,0.088361,0.102819,0.104553,0.104553,0.104553,0.104553,0.104553,0.098831,0.109929,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.259053,0.227106,0.274336,0.178503,0.178161,0.175141,0.170799,0.204846,0.163301,0.185444,0.169399,0.179884,0.17765,0.170174,0.184341,0.195378,0.2,0.212571,0.159247,0.163588,0.205752,0.155129,0.20598,0.182353,0.155388,0.162304,0.164894,0.207358,0.177481,0.173184,0.171745,0.162162,0.180934,0.126273,0.16622,0.157895,0.19195,0.197243,0.195584,0.175141,0.175803,0.172222,0.173832,0.188832,0.160069,0.171113,0.129707,0.195584,0.195584,0.195584,0.195584,0.195584,0.149158,0.162304,0.188259,0.178674,0.162304,0.167116,0.159383,0.219599,0.171113,0.129256,0.160345,0.161179,0.161599,0.095975,0.121252,0.166517,0.167417,0.155518,0.156698,0.157494,0.166667,0.167417,0.158839,0.159657,0.170174,0.174484,0.114462,0.114462,0.114462,0.114462,0.158163,0.160345,0.172382,0.15604,0.176471,0.148444,0.163158,0.156566,0.157095,0.159247,0.153719,0.154357,0.159247,0.17033,0.116323,0.132762,0.156434,0.239382,0.119537,0.166071,0.194561,0.194561,0.157627,0.159931,0.158703,0.194357,0.194357,0.170174,0.157627,0.157627,0.171271,0.156566,0.156303,0.226829,0.158839,0.159383,0.157761,0.159794,0.157627,0.165775,0.156566,0.155129,0.156566,0.157494,0.155129,0.156171,0.157494,0.157494,0.154742,0.155518,0.156434,0.156962,0.156566,0.158298,0.156566,0.158163,0.1609,0.158029,0.157895,0.156434,0.155909,0.156303,0.152209,0.203501,0.17033,0.168784,0.129346,0.147971,0.154742,0.147971,0.154742,0.147971,0.173023,0.156962,0.148562,0.155129,0.154742,0.129256,0.11517,0.127223,0.13449,0.131449,0.122772,0.120779,0.124832,0.122772,0.122772,0.125084,0.125931,0.122772,0.125337,0.127049,0.146112,0.147036,0.126359,0.122772,0.126359,0.126962,0.133813,0.133813,0.135174,0.134102,0.128187,0.109027,0.150852],[0.372,0.343808,0.392405,0.333932,0.30897,0.301948,0.304918,0.319039,0.311558,0.288372,0.294304,0.286154,0.333932,0.322917,0.287481,0.29108,0.352273,0.315254,0.292913,0.286154,0.302932,0.285714,0.296178,0.333333,0.285714,0.240621,0.277198,0.319039,0.333333,0.316327,0.310518,0.2976,0.315789,0.224096,0.230198,0.285714,0.306931,0.267626,0.298555,0.32069,0.334532,0.317406,0.317406,0.293839,0.299035,0.300485,0.23192,0.291994,0.291994,0.291994,0.291994,0.291994,0.282675,0.297125,0.331551,0.317949,0.304419,0.288372,0.261972,0.324042,0.005348,0.228221,0.297125,0.291536,0.299517,0.005376,0.005405,0.307438,0.298555,0.283537,0.283537,0.294304,0.296651,0.296178,0.289269,0.290172,0.288372,0.314721,0.229346,0.229346,0.229346,0.229346,0.287926,0.250674,0.295238,0.299035,0.292453,0.274742,0.302932,0.286154,0.291536,0.302439,0.289269,0.248996,0.2976,0.288372,0.216028,0.223289,0.285714,0.307438,0.234552,0.318493,0.223022,0.223022,0.290625,0.301948,0.290172,0.231056,0.231056,0.295707,0.296178,0.28972,0.287037,0.285714,0.287037,0.291994,0.29477,0.295238,0.290172,0.290172,0.295238,0.311037,0.283537,0.283537,0.283537,0.291994,0.282675,0.285714,0.282675,0.291994,0.291536,0.291536,0.292453,0.282675,0.282675,0.298555,0.284839,0.282675,0.282675,0.291536,0.283105,0.282675,0.286154,0.282675,0.282675,0.282675,0.291536,0.221165,0.252374,0.256552,0.256552,0.256552,0.256552,0.256552,0.226553,0.227106,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var24_df_closseness, by=list(var24_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var24_df_closseness, by=list(var24_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-d2c1b6e91aa4a73f0d21" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-d2c1b6e91aa4a73f0d21">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00013\" data-max=\"0.000137\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000404\" data-max=\"0.000647\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000145\" data-max=\"0.000307\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00079\" data-max=\"0.000923\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000122\" data-max=\"0.00014\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.024189\" data-max=\"0.025447\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000245\" data-max=\"0.002283\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.075177\" data-max=\"0.120329\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.02695\" data-max=\"0.057072\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.146961\" data-max=\"0.171578\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022629\" data-max=\"0.02605\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.174215\" data-max=\"0.281553\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.060144\" data-max=\"0.135817\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[0.000137,0.00013],[1e-06,1.2e-05],[0.000647,0.000404],[0.000145,0.000307],[0.000923,0.00079],[0.000122,0.00014],[0.025447,0.024189],[0.000245,0.002283],[0.120329,0.075177],[0.02695,0.057072],[0.171578,0.146961],[0.022629,0.02605],[0.281553,0.174215],[0.060144,0.135817]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var24_df_closseness <- data.frame(
var24_incloseness,
var24_outcloseness,
var24_totalcloseness,
var24_incloseness_n,
var24_outcloseness_n,
var24_totalcloseness_n,
var24_centr_closeness) %>% round(6)

#Adding type
var24_df_closseness <-cbind(var24_df_closseness, V(var24)$TIPO2)

#Adding names
names(var24_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var24_df_closseness<-var24_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var24_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-328e50a82d51d35f3584" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-328e50a82d51d35f3584">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000144\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000996\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000516\" data-max=\"0.001475\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.026736\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.185259\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.095975\" data-max=\"0.274336\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[0.000141,0.000139,0.000144,0.000137,0.000138,0.000137,0.000136,0.000139,0.000132,0.000139,0.000138,0.000138,0.00013,0.000132,0.000136,0.000137,0.000138,0.000139,0.000131,0.000136,0.000138,0.000132,0.000138,0.000125,0.000132,0.000135,0.000129,0.000138,0.000137,0.000137,0.000137,0.000136,0.000138,0.000129,0.000137,0.000132,0.000139,0.000136,0.000135,0.000137,0.000137,0.000137,0.000136,0.000139,0.000135,0.000138,0.000124,0.000136,0.000136,0.000136,0.000136,0.000136,0.000125,0.000134,0.00013,0.000137,0.000137,0.000129,0.000131,0.000139,0.00014,0.000128,0.000137,0.000136,0.000133,2.9e-05,0.000125,0.000136,0.000133,0.000136,0.000137,0.000137,0.000137,0.000137,0.000137,0.000136,0.000137,0.000138,0.000126,0.000126,0.000126,0.000126,0.000137,0.000133,0.000133,0.000137,0.000135,0.000123,0.000137,0.000136,0.000137,0.000125,0.000128,0.000136,0.000137,0.000133,0.000123,0.000131,0.000137,0.000137,0.000126,0.000133,0.000135,0.000135,0.000136,0.000136,0.000137,0.000135,0.000136,0.000137,0.000137,0.000137,0.000137,0.000137,0.000137,0.000137,0.000136,0.000136,0.000137,0.000137,0.000136,0.000136,0.000137,0.000137,0.000137,0.000137,0.000137,0.000137,0.000137,0.000134,0.000137,0.000137,0.000137,0.000137,0.000137,0.000137,0.000136,0.000136,0.000137,0.000136,0.000137,0.000136,0.000136,0.000136,0.000135,0.000139,0.000138,0.000137,0.000131,0.000134,0.000137,0.000134,0.000137,0.000134,0.000136,0.000126,0.000138,0.00014,0.00014,0.000126,0.000127,0.000132,0.000137,0.000136,0.000127,0.000128,0.000132,0.000127,0.000127,0.000132,0.000132,0.000127,0.000132,0.000133,0.000134,0.000134,0.000132,0.000127,0.000132,0.000132,0.000133,0.000133,0.000134,0.000133,0.000132,0.000128,0.000138],[0.000915,0.000815,0.000996,0.000773,0.000721,0.000699,0.000723,0.000738,0.000729,0.000652,0.000672,0.000659,0.000775,0.000765,0.000786,0.000654,0.000854,0.000742,0.00067,0.000648,0.000698,0.000646,0.000672,0.000786,0.000639,0.00052,0.000621,0.000754,0.000792,0.000732,0.000707,0.000684,0.000723,0.000483,0.000493,0.000646,0.000706,0.000598,0.000679,0.000754,0.000774,0.000747,0.000747,0.000669,0.00068,0.000687,0.0005,0.000657,0.000657,0.000657,0.000657,0.000657,0.000637,0.000681,0.000787,0.000739,0.000699,0.000656,0.000586,0.000752,2.9e-05,0.000491,0.000675,0.000692,0.000683,2.9e-05,2.9e-05,0.000703,0.000695,0.000645,0.000645,0.000667,0.000672,0.000671,0.00066,0.000661,0.000659,0.000734,0.000495,0.000495,0.000495,0.000495,0.000658,0.000548,0.000676,0.000675,0.000669,0.000616,0.00069,0.000651,0.000663,0.000694,0.000658,0.000541,0.00068,0.000657,0.00046,0.000477,0.000649,0.000952,0.000503,0.000734,0.000473,0.000473,0.000666,0.000688,0.000664,0.000503,0.000503,0.000678,0.000678,0.000662,0.000654,0.000651,0.000655,0.000888,0.000676,0.000678,0.00066,0.00066,0.000669,0.000712,0.000645,0.000645,0.000645,0.00066,0.000643,0.00065,0.000643,0.00066,0.000659,0.000659,0.000661,0.000643,0.000643,0.000677,0.000648,0.000643,0.000643,0.000659,0.000644,0.000643,0.000649,0.000643,0.000643,0.000643,0.000659,0.000475,0.000553,0.000562,0.000562,0.000562,0.000562,0.000562,0.000531,0.000591,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001393,0.001221,0.001475,0.00096,0.000958,0.000942,0.000918,0.001101,0.000878,0.000997,0.000911,0.000967,0.000955,0.000915,0.000991,0.00105,0.001075,0.001143,0.000856,0.00088,0.001106,0.000834,0.001107,0.00098,0.000835,0.000873,0.000887,0.001115,0.000954,0.000931,0.000923,0.000872,0.000973,0.000679,0.000894,0.000849,0.001032,0.00106,0.001052,0.000942,0.000945,0.000926,0.000935,0.001015,0.000861,0.00092,0.000697,0.001052,0.001052,0.001052,0.001052,0.001052,0.000802,0.000873,0.001012,0.000961,0.000873,0.000898,0.000857,0.001181,0.00092,0.000695,0.000862,0.000867,0.000869,0.000516,0.000652,0.000895,0.0009,0.000836,0.000842,0.000847,0.000896,0.0009,0.000854,0.000858,0.000915,0.000938,0.000615,0.000615,0.000615,0.000615,0.00085,0.000862,0.000927,0.000839,0.000949,0.000798,0.000877,0.000842,0.000845,0.000856,0.000826,0.00083,0.000856,0.000916,0.000625,0.000714,0.000841,0.001287,0.000643,0.000893,0.001046,0.001046,0.000847,0.00086,0.000853,0.001045,0.001045,0.000915,0.000847,0.000847,0.000921,0.000842,0.00084,0.00122,0.000854,0.000857,0.000848,0.000859,0.000847,0.000891,0.000842,0.000834,0.000842,0.000847,0.000834,0.00084,0.000847,0.000847,0.000832,0.000836,0.000841,0.000844,0.000842,0.000851,0.000842,0.00085,0.000865,0.00085,0.000849,0.000841,0.000838,0.00084,0.000818,0.001094,0.000916,0.000907,0.000695,0.000796,0.000832,0.000796,0.000832,0.000796,0.00093,0.000844,0.000799,0.000834,0.000832,0.000695,0.000619,0.000684,0.000723,0.000707,0.00066,0.000649,0.000671,0.00066,0.00066,0.000672,0.000677,0.00066,0.000674,0.000683,0.000786,0.000791,0.000679,0.00066,0.000679,0.000683,0.000719,0.000719,0.000727,0.000721,0.000689,0.000586,0.000811],[0.026279,0.02588,0.026736,0.025532,0.025606,0.025542,0.025316,0.025902,0.024503,0.025805,0.025588,0.025655,0.024212,0.024564,0.025358,0.025452,0.025662,0.025866,0.024326,0.025265,0.025634,0.024548,0.025648,0.02327,0.024538,0.025132,0.024037,0.025669,0.025521,0.0255,0.025563,0.025386,0.025609,0.024037,0.025521,0.024642,0.025776,0.025203,0.025166,0.025462,0.025553,0.025399,0.025316,0.025823,0.025162,0.025627,0.023129,0.025237,0.025237,0.025237,0.025237,0.025237,0.023285,0.02494,0.024103,0.025521,0.025438,0.024065,0.024326,0.025805,0.026043,0.023797,0.025396,0.025386,0.024685,0.005348,0.023282,0.025354,0.024747,0.025337,0.02541,0.025427,0.025431,0.025452,0.025462,0.025386,0.025417,0.02568,0.023393,0.023393,0.023393,0.023393,0.025417,0.024668,0.024777,0.025431,0.025111,0.022932,0.025438,0.025344,0.025396,0.023186,0.02388,0.025241,0.025427,0.024731,0.022932,0.024451,0.025452,0.025525,0.023526,0.024695,0.025176,0.025176,0.025386,0.025386,0.025396,0.025179,0.02522,0.025417,0.025396,0.025476,0.025427,0.025431,0.025431,0.025389,0.025386,0.025386,0.025406,0.025479,0.025386,0.025323,0.025427,0.025399,0.025427,0.02542,0.025399,0.02541,0.02542,0.024956,0.025389,0.02541,0.025417,0.02541,0.025427,0.025493,0.025386,0.025386,0.025427,0.025375,0.025427,0.025375,0.025365,0.025375,0.025122,0.025844,0.025578,0.025399,0.024368,0.024916,0.025389,0.024916,0.025389,0.024916,0.025303,0.023512,0.025578,0.026043,0.026032,0.023488,0.023649,0.024632,0.025413,0.025299,0.023634,0.023831,0.024548,0.023634,0.023634,0.024616,0.024632,0.023634,0.024616,0.024685,0.02486,0.0249,0.024593,0.023634,0.024593,0.024603,0.024701,0.024701,0.02493,0.024741,0.024593,0.023825,0.02573],[0.170174,0.151589,0.185259,0.143852,0.134102,0.129979,0.13449,0.137269,0.135569,0.121331,0.124916,0.12261,0.144186,0.142311,0.146226,0.121728,0.158839,0.137982,0.124665,0.120544,0.129798,0.120077,0.125,0.146112,0.118774,0.096774,0.115528,0.140271,0.147268,0.136064,0.131542,0.127136,0.134393,0.089768,0.091716,0.120077,0.131263,0.111311,0.126273,0.140271,0.143963,0.139013,0.139013,0.124415,0.126531,0.127747,0.092954,0.122127,0.122127,0.122127,0.122127,0.122127,0.118471,0.126617,0.146341,0.137371,0.129979,0.121967,0.108963,0.139955,0.005348,0.091311,0.125506,0.128631,0.127049,0.005376,0.005405,0.130802,0.129256,0.12,0.12,0.124,0.125,0.124748,0.122691,0.122853,0.12261,0.136564,0.091988,0.091988,0.091988,0.091988,0.122368,0.101918,0.125761,0.125506,0.124415,0.114532,0.128364,0.121173,0.12326,0.129077,0.122368,0.100704,0.126531,0.122208,0.085478,0.088783,0.120701,0.177143,0.093561,0.136464,0.087943,0.087943,0.123835,0.128011,0.123424,0.093608,0.093608,0.126016,0.126102,0.123097,0.121728,0.121173,0.121887,0.165187,0.125761,0.126016,0.122772,0.122772,0.124498,0.132384,0.12,0.12,0.12,0.122772,0.119537,0.120936,0.119537,0.122772,0.12253,0.12253,0.123016,0.119537,0.119537,0.125931,0.120544,0.119537,0.119537,0.12253,0.119768,0.119537,0.120623,0.119537,0.119537,0.119537,0.12253,0.088361,0.102819,0.104553,0.104553,0.104553,0.104553,0.104553,0.098831,0.109929,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.259053,0.227106,0.274336,0.178503,0.178161,0.175141,0.170799,0.204846,0.163301,0.185444,0.169399,0.179884,0.17765,0.170174,0.184341,0.195378,0.2,0.212571,0.159247,0.163588,0.205752,0.155129,0.20598,0.182353,0.155388,0.162304,0.164894,0.207358,0.177481,0.173184,0.171745,0.162162,0.180934,0.126273,0.16622,0.157895,0.19195,0.197243,0.195584,0.175141,0.175803,0.172222,0.173832,0.188832,0.160069,0.171113,0.129707,0.195584,0.195584,0.195584,0.195584,0.195584,0.149158,0.162304,0.188259,0.178674,0.162304,0.167116,0.159383,0.219599,0.171113,0.129256,0.160345,0.161179,0.161599,0.095975,0.121252,0.166517,0.167417,0.155518,0.156698,0.157494,0.166667,0.167417,0.158839,0.159657,0.170174,0.174484,0.114462,0.114462,0.114462,0.114462,0.158163,0.160345,0.172382,0.15604,0.176471,0.148444,0.163158,0.156566,0.157095,0.159247,0.153719,0.154357,0.159247,0.17033,0.116323,0.132762,0.156434,0.239382,0.119537,0.166071,0.194561,0.194561,0.157627,0.159931,0.158703,0.194357,0.194357,0.170174,0.157627,0.157627,0.171271,0.156566,0.156303,0.226829,0.158839,0.159383,0.157761,0.159794,0.157627,0.165775,0.156566,0.155129,0.156566,0.157494,0.155129,0.156171,0.157494,0.157494,0.154742,0.155518,0.156434,0.156962,0.156566,0.158298,0.156566,0.158163,0.1609,0.158029,0.157895,0.156434,0.155909,0.156303,0.152209,0.203501,0.17033,0.168784,0.129346,0.147971,0.154742,0.147971,0.154742,0.147971,0.173023,0.156962,0.148562,0.155129,0.154742,0.129256,0.11517,0.127223,0.13449,0.131449,0.122772,0.120779,0.124832,0.122772,0.122772,0.125084,0.125931,0.122772,0.125337,0.127049,0.146112,0.147036,0.126359,0.122772,0.126359,0.126962,0.133813,0.133813,0.135174,0.134102,0.128187,0.109027,0.150852],[0.372,0.343808,0.392405,0.333932,0.30897,0.301948,0.304918,0.319039,0.311558,0.288372,0.294304,0.286154,0.333932,0.322917,0.287481,0.29108,0.352273,0.315254,0.292913,0.286154,0.302932,0.285714,0.296178,0.333333,0.285714,0.240621,0.277198,0.319039,0.333333,0.316327,0.310518,0.2976,0.315789,0.224096,0.230198,0.285714,0.306931,0.267626,0.298555,0.32069,0.334532,0.317406,0.317406,0.293839,0.299035,0.300485,0.23192,0.291994,0.291994,0.291994,0.291994,0.291994,0.282675,0.297125,0.331551,0.317949,0.304419,0.288372,0.261972,0.324042,0.005348,0.228221,0.297125,0.291536,0.299517,0.005376,0.005405,0.307438,0.298555,0.283537,0.283537,0.294304,0.296651,0.296178,0.289269,0.290172,0.288372,0.314721,0.229346,0.229346,0.229346,0.229346,0.287926,0.250674,0.295238,0.299035,0.292453,0.274742,0.302932,0.286154,0.291536,0.302439,0.289269,0.248996,0.2976,0.288372,0.216028,0.223289,0.285714,0.307438,0.234552,0.318493,0.223022,0.223022,0.290625,0.301948,0.290172,0.231056,0.231056,0.295707,0.296178,0.28972,0.287037,0.285714,0.287037,0.291994,0.29477,0.295238,0.290172,0.290172,0.295238,0.311037,0.283537,0.283537,0.283537,0.291994,0.282675,0.285714,0.282675,0.291994,0.291536,0.291536,0.292453,0.282675,0.282675,0.298555,0.284839,0.282675,0.282675,0.291536,0.283105,0.282675,0.286154,0.282675,0.282675,0.282675,0.291536,0.221165,0.252374,0.256552,0.256552,0.256552,0.256552,0.256552,0.226553,0.227106,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var24_df_closseness, by=list(var24_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var24_df_closseness, by=list(var24_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-af65d66d2b649a7773fa" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-af65d66d2b649a7773fa">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00013\" data-max=\"0.000137\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000398\" data-max=\"0.000717\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8e-05\" data-max=\"0.000303\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000784\" data-max=\"0.00095\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.1e-05\" data-max=\"0.000134\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.024169\" data-max=\"0.025579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.8e-05\" data-max=\"0.002291\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.074081\" data-max=\"0.133395\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.014803\" data-max=\"0.056454\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.145821\" data-max=\"0.176715\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.007654\" data-max=\"0.025009\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172839\" data-max=\"0.310096\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.028037\" data-max=\"0.136165\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[0.000137,0.000137,0.00013],[1e-06,1e-06,1.2e-05],[0.000717,0.000638,0.000398],[8e-05,0.000152,0.000303],[0.00095,0.000922,0.000784],[4.1e-05,0.000134,0.000129],[0.025579,0.025429,0.024169],[5.8e-05,0.000256,0.002291],[0.133395,0.118673,0.074081],[0.014803,0.028344,0.056454],[0.176715,0.171443,0.145821],[0.007654,0.025009,0.024031],[0.310096,0.276998,0.172839],[0.028037,0.062335,0.136165]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var24_data.RData")
```

