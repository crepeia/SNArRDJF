# SNA Closeness 12_ACESSO D) O seu serviço consegue acessar facilmente informações deste serviço. (var10)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 12_ACESSO D) O seu serviço consegue acessar facilmente informações deste serviço. (var10)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var10_data.RData")
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
#var10<-simplify(var10) #Simplify
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
V(var10)$incloseness <- closeness(var10, mode = "in", weights = E(var10)$var10) %>% round(6)
V(var10)$outcloseness <- closeness(var10, mode = "out", weights = E(var10)$var10) %>% round(6)
V(var10)$totalcloseness <- closeness(var10, mode = "total", weights = E(var10)$var10) %>% round(4)
```

###Saving to Environment

```r
var10_incloseness<- closeness(var10, mode = "in", weights = E(var10)$var10) %>% round(6)
var10_outcloseness<- closeness(var10, mode = "out", weights = E(var10)$var10) %>% round(6)
var10_totalcloseness<- closeness(var10, mode = "total", weights = E(var10)$var10) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var10_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0001260 0.0001280 0.0001267 0.0001290 0.0001330
```

```r
sd(var10_incloseness)
```

```
## [1] 7.861163e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var10)$incloseness<-closeness(var10, weights = E(var10)$var10, mode="in")

#Get Variable
V(var10)$var10_color_degree<-round(V(var10)$incloseness,6)

#Creating brewer pallette
vertex_var10_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var10)$var10_color_degree)), "RdBu"))(
            length(unique(V(var10)$var10_color_degree)))

#Saving as Vertex properties 
V(var10)$vertex_var10_color_degree<-
  vertex_var10_color_degree[as.numeric(
  cut(V(var10)$var10_color_degree,
      breaks=length(unique(V(var10)$var10_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var10, es=E(var10), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var10))
maxC <- rep(Inf, vcount(var10))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var10, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var10)$weight)


#PLotting
plot(var10, 
     layout=co,
     edge.color=V(var10)$vertex_var10_color_degree[edge.start],
     edge.arrow.size=closeness(var10, weights = E(var10)$var10, mode="in"),
     edge.width=E(var10)$weight/mean(E(var10)$weight),
     edge.curved = TRUE,
     vertex.color=V(var10)$vertex_var10_color_degree,
     vertex.size=closeness(var10, weights = E(var10)$var10, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var10,"LABEL_COR"),
     vertex.label.cex=(closeness(var10, weights = E(var10)$var10, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var10)$var10_color_degree
b<-V(var10)$vertex_var10_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 12_ACESSO D) O seu serviço consegue acessar facilmente informações deste serviço. (var10)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var10, mode="in", weights = E(var10)$var10)), 
             sd(closeness(var10, mode="in", weights = E(var10)$var10))
             )
       )
```

![](12_ACESSO_D_acessar_facilmente_informações_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var10_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0006545 0.0008130 0.0007060 0.0009940 0.0012120
```

```r
sd(var10_outcloseness)
```

```
## [1] 0.0003729975
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var10)$outcloseness<-closeness(var10, weights = E(var10)$var10, mode="out")

#Get Variable
V(var10)$var10_color_degree<-round(V(var10)$outcloseness,6)

#Creating brewer pallette
vertex_var10_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var10)$var10_color_degree)), "RdBu"))(
            length(unique(V(var10)$var10_color_degree)))

#Saving as Vertex properties 
V(var10)$vertex_var10_color_degree<-
  vertex_var10_color_degree[as.numeric(
  cut(V(var10)$var10_color_degree,
      breaks=length(unique(V(var10)$var10_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var10, es=E(var10), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var10))
maxC <- rep(Inf, vcount(var10))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var10, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var10)$weight)


#PLotting
plot(var10, 
     layout=co,
     edge.color=V(var10)$vertex_var10_color_degree[edge.start],
     edge.arrow.size=closeness(var10, weights = E(var10)$var10, mode="out"),
     edge.width=E(var10)$weight/2*mean(E(var10)$weight),
     edge.curved = TRUE,
     vertex.color=V(var10)$vertex_var10_color_degree,
     vertex.size=closeness(var10, weights = E(var10)$var10, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var10,"LABEL_COR"),
     vertex.label.cex=closeness(var10, weights = E(var10)$var10, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var10)$var10_color_degree
b<-V(var10)$vertex_var10_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 12_ACESSO D) O seu serviço consegue acessar facilmente informações deste serviço. (var10)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var10, mode="out", weights = E(var10)$var10)), 
             sd(closeness(var10, mode="out", weights = E(var10)$var10))
             )
       )
```

![](12_ACESSO_D_acessar_facilmente_informações_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var10_totalcloseness)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000678 0.001220 0.001422 0.001388 0.001581 0.002160
```

```r
sd(var10_totalcloseness)
```

```
## [1] 0.0002929657
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var10)$allcloseness<-closeness(var10, weights = E(var10)$var10, mode="all")

#Get Variable
V(var10)$var10_color_degree<-round(V(var10)$allcloseness,6)

#Creating brewer pallette
vertex_var10_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var10)$var10_color_degree)), "RdBu"))(
            length(unique(V(var10)$var10_color_degree)))

#Saving as Vertex properties 
V(var10)$vertex_var10_color_degree<-
  vertex_var10_color_degree[as.numeric(
  cut(V(var10)$var10_color_degree,
      breaks=length(unique(V(var10)$var10_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var10, es=E(var10), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var10))
maxC <- rep(Inf, vcount(var10))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var10, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var10)$weight)


#PLotting
plot(var10, 
     layout=co,
     edge.color=V(var10)$vertex_var10_color_degree[edge.start],
     edge.arrow.size=closeness(var10, weights = E(var10)$var10, mode="all"),
     edge.width=E(var10)$weight/2*mean(E(var10)$weight),
     edge.curved = TRUE,
     vertex.color=V(var10)$vertex_var10_color_degree,
     vertex.size=closeness(var10, weights = E(var10)$var10, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var10,"LABEL_COR"),
     vertex.label.cex=(closeness(var10, weights = E(var10)$var10, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var10)$var10_color_degree
b<-V(var10)$vertex_var10_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 12_ACESSO D) O seu serviço consegue acessar facilmente informações deste serviço. (var10)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var10, mode="all", weights = E(var10)$var10)), 
             sd(closeness(var10, mode="all", weights = E(var10)$var10))
             )
       )
```

![](12_ACESSO_D_acessar_facilmente_informações_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var10)$incloseness_n <- closeness(var10, mode = "in",, weights = E(var10)$var10, normalized = T) %>% round(10)
V(var10)$outcloseness_n <- closeness(var10, mode = "out", normalized = T, weights = E(var10)$var10) %>% round(6)
V(var10)$totalcloseness_n <- closeness(var10, mode = "total", normalized = T, weights = E(var10)$var10) %>% round(6)
```

###Saving to Environment

```r
var10_incloseness_n<- closeness(var10, mode = "in", normalized = T, weights = E(var10)$var10) %>% round(6)
var10_outcloseness_n<- closeness(var10, mode = "out", normalized = T, weights = E(var10)$var10) %>% round(6)
var10_totalcloseness_n<- closeness(var10, mode = "total", normalized = T, weights = E(var10)$var10) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var10_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.023470 0.023860 0.023570 0.024030 0.024830
```

```r
sd(var10_incloseness_n)
```

```
## [1] 0.001465033
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var10)$incloseness_n<-closeness(var10, weights = E(var10)$var10, mode="in", normalized = T)

#Get Variable
V(var10)$var10_color_degree<-round(V(var10)$incloseness_n,6)

#Creating brewer pallette
vertex_var10_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var10)$var10_color_degree)), "RdBu"))(
            length(unique(V(var10)$var10_color_degree)))

#Saving as Vertex properties 
V(var10)$vertex_var10_color_degree<-
  vertex_var10_color_degree[as.numeric(
  cut(V(var10)$var10_color_degree,
      breaks=length(unique(V(var10)$var10_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var10, es=E(var10), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var10))
maxC <- rep(Inf, vcount(var10))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var10, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var10)$weight)


#PLotting
plot(var10, 
     layout=co,
     edge.color=V(var10)$vertex_var10_color_degree[edge.start],
     edge.arrow.size=closeness(var10, weights = E(var10)$var10, mode="in",normalized = T),
     edge.width=E(var10)$weight/10*mean(E(var10)$weight),
     edge.curved = TRUE,
     vertex.color=V(var10)$vertex_var10_color_degree,
     vertex.size=(closeness(var10, weights = E(var10)$var10, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var10,"LABEL_COR"),
     vertex.label.cex=closeness(var10, weights = E(var10)$var10, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var10)$var10_color_degree
b<-V(var10)$vertex_var10_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 12_ACESSO D) O seu serviço consegue acessar facilmente informações deste serviço. (var10)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var10, mode="in", weights = E(var10)$var10, normalized = T)), 
             sd(closeness(var10, mode="in", weights = E(var10)$var10, normalized = T))
             )
       )
```

![](12_ACESSO_D_acessar_facilmente_informações_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var10_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.121800 0.151200 0.131300 0.184900 0.225500
```

```r
sd(var10_outcloseness_n)
```

```
## [1] 0.06939359
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var10)$outcloseness_n<-closeness(var10, weights = E(var10)$var10, mode="out", normalized = T)

#Get Variable
V(var10)$var10_color_degree<-round(V(var10)$outcloseness_n,6)

#Creating brewer pallette
vertex_var10_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var10)$var10_color_degree)), "RdBu"))(
            length(unique(V(var10)$var10_color_degree)))

#Saving as Vertex properties 
V(var10)$vertex_var10_color_degree<-
  vertex_var10_color_degree[as.numeric(
  cut(V(var10)$var10_color_degree,
      breaks=length(unique(V(var10)$var10_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var10, es=E(var10), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var10))
maxC <- rep(Inf, vcount(var10))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var10, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var10)$weight)


#PLotting
plot(var10, 
     layout=co,
     edge.color=V(var10)$vertex_var10_color_degree[edge.start],
     edge.arrow.size=closeness(var10, weights = E(var10)$var10, mode="out",normalized = T),
     edge.width=E(var10)$weight/10*mean(E(var10)$weight),
     edge.curved = TRUE,
     vertex.color=V(var10)$vertex_var10_color_degree,
     vertex.size=(closeness(var10, weights = E(var10)$var10, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var10,"LABEL_COR"),
     vertex.label.cex=closeness(var10, weights = E(var10)$var10, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var10)$var10_color_degree
b<-V(var10)$vertex_var10_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 12_ACESSO D) O seu serviço consegue acessar facilmente informações deste serviço. (var10)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var10, mode="out", weights = E(var10)$var10, normalized = T)), 
             sd(closeness(var10, mode="out", weights = E(var10)$var10, normalized = T))
             )
       )
```

![](12_ACESSO_D_acessar_facilmente_informações_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var10_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1262  0.2270  0.2646  0.2582  0.2941  0.4017
```

```r
sd(var10_totalcloseness_n)
```

```
## [1] 0.05449914
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var10)$allcloseness_n<-closeness(var10, weights = E(var10)$var10, mode="all", normalized = T)

#Get Variable
V(var10)$var10_color_degree<-round(V(var10)$allcloseness_n,6)

#Creating brewer pallette
vertex_var10_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var10)$var10_color_degree)), "RdBu"))(
            length(unique(V(var10)$var10_color_degree)))

#Saving as Vertex properties 
V(var10)$vertex_var10_color_degree<-
  vertex_var10_color_degree[as.numeric(
  cut(V(var10)$var10_color_degree,
      breaks=length(unique(V(var10)$var10_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var10, es=E(var10), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var10))
maxC <- rep(Inf, vcount(var10))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var10, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var10)$weight)


#PLotting
plot(var10, 
     layout=co,
     edge.color=V(var10)$vertex_var10_color_degree[edge.start],
     edge.arrow.size=closeness(var10, weights = E(var10)$var10, mode="all",normalized = T),
     edge.width=E(var10)$weight/10*mean(E(var10)$weight),
     edge.curved = TRUE,
     vertex.color=V(var10)$vertex_var10_color_degree,
     vertex.size=(closeness(var10, weights = E(var10)$var10, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var10,"LABEL_COR"),
     vertex.label.cex=closeness(var10, weights = E(var10)$var10, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var10)$var10_color_degree
b<-V(var10)$vertex_var10_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 12_ACESSO D) O seu serviço consegue acessar facilmente informações deste serviço. (var10)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var10, mode="all", weights = E(var10)$var10, normalized = T)), 
             sd(closeness(var10, mode="all", weights = E(var10)$var10, normalized = T))
             )
       )
```

![](12_ACESSO_D_acessar_facilmente_informações_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var10)$incloseness_n <- closeness(var10, weights = E(var10)$var10, mode = "in", normalized = T) %>% round(6)
V(var10)$outcloseness_n <- closeness(var10, weights = E(var10)$var10, mode = "out", normalized = T) %>% round(6)
V(var10)$totalcloseness_n <- closeness(var10, weights = E(var10)$var10, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var10)$var10_centr_closeness<- centralization.closeness(var10)$res
var10_centr_closeness<- centralization.closeness(var10)$res
var10_centr_closeness_all<- centralization.closeness(var10)
```

###Centralization

```r
var10_centr_closeness_all$centralization
```

```
## [1] 0.1628463
```

###Theoretical Max

```r
var10_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var10)$var10_centr_closeness<- centralization.closeness(var10)$res

#Get Variable
V(var10)$var10_color_degree<-round(V(var10)$var10_centr_closeness,6)

#Creating brewer pallette
vertex_var10_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var10)$var10_color_degree)), "Spectral"))(
            length(unique(V(var10)$var10_color_degree)))

#Saving as Vertex properties 
V(var10)$vertex_var10_color_degree<-
  vertex_var10_color_degree[as.numeric(
  cut(V(var10)$var10_color_degree,
      breaks=length(unique(V(var10)$var10_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var10, es=E(var10), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var10))
maxC <- rep(Inf, vcount(var10))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var10, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var10)$weight)


#PLotting
plot(var10, 
     layout=co,
     edge.color=V(var10)$vertex_var10_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var10)$res,
     edge.width=E(var10)$weight/10*mean(E(var10)$weight),
     edge.curved = TRUE,
     vertex.color=V(var10)$vertex_var10_color_degree,
     vertex.size=centralization.closeness(var10)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var10,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var10)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var10)$var10_color_degree
b<-V(var10)$vertex_var10_color_degree
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
  title("Network Centralization Closeness - 12_ACESSO D) O seu serviço consegue acessar facilmente informações deste serviço. (var10)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var10)$res), 
             sd(centralization.closeness(var10)$res)
             )
       )
```

![](12_ACESSO_D_acessar_facilmente_informações_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var10_incloseness<- closeness(var10, weights = E(var10)$var10, mode = "in") %>% round(6)
var10_outcloseness<- closeness(var10, weights = E(var10)$var10, mode = "out") %>% round(6)
var10_totalcloseness<- closeness(var10, weights = E(var10)$var10, mode = "total") %>% round(6)
var10_incloseness_n<- closeness(var10,weights = E(var10)$var10, mode = "in", normalized = T) %>% round(6)
var10_outcloseness_n<- closeness(var10,weights = E(var10)$var10, mode = "out", normalized = T) %>% round(6)
var10_totalcloseness_n<- closeness(var10,weights = E(var10)$var10, mode = "total", normalized = T) %>% round(6)
var10_centr_closeness <- centralization.closeness(var10)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var10_df_closseness <- data.frame(
var10_incloseness,
var10_outcloseness,
var10_totalcloseness,
var10_incloseness_n,
var10_outcloseness_n,
var10_totalcloseness_n,
var10_centr_closeness) %>% round(6)

#Adding type
var10_df_closseness <-cbind(var10_df_closseness, V(var10)$LABEL_COR)

#Adding names
names(var10_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var10_df_closseness<-var10_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var10_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-99124c0d613b60333897" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-99124c0d613b60333897">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001212\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000678\" data-max=\"0.00216\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.02483\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.225455\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.126187\" data-max=\"0.401728\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[0.000132,0.00013,0.000133,0.000129,0.00013,0.000129,0.000127,0.00013,0.000128,0.000127,0.00013,0.00013,0.000124,0.000126,0.000128,0.000127,0.000129,0.000131,0.000124,0.00013,0.00013,0.000127,0.00013,0.000121,0.000123,0.000126,0.000125,0.00013,0.000129,0.00013,0.00013,0.000128,0.00013,0.000122,0.000129,0.000128,0.000128,0.000127,0.000124,0.00013,0.00013,0.000127,0.000128,0.000129,0.000127,0.000129,0.000118,0.000124,0.000124,0.000124,0.000124,0.000124,0.000123,0.00013,0.000122,0.000129,0.00013,0.000122,0.000124,0.000131,0.000131,0.000121,0.000128,0.000128,0.000128,2.9e-05,0.000118,0.000128,0.000124,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.000128,0.000128,0.00013,0.000121,0.000121,0.000121,0.000121,0.000128,0.000128,0.000128,0.000129,0.000129,0.000118,0.000128,0.000128,0.000128,0.000119,0.000123,0.000128,0.00013,0.000127,0.000118,0.000127,0.000129,0.00013,0.000121,0.000124,0.000127,0.000127,0.000128,0.000128,0.000128,0.000127,0.000127,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000128,0.000128,0.000128,0.000128,0.00013,0.000128,0.00013,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.00013,0.000128,0.000129,0.000128,0.000129,0.000129,0.000128,0.000128,0.000126,0.00013,0.000128,0.00013,0.000127,0.000128,0.000129,0.000128,0.000129,0.000128,0.000128,0.00012,0.000131,0.000132,0.000132,0.000121,0.000121,0.000129,0.000131,0.00013,0.000121,0.000121,0.000129,0.000121,0.000121,0.00013,0.000125,0.000121,0.000129,0.00013,0.00013,0.00013,0.000129,0.000121,0.000129,0.000125,0.000125,0.000125,0.000128,0.000129,0.000125,0.000126,0.000129],[0.001185,0.001136,0.001029,0.001079,0.000813,0.001062,0.000793,0.001196,0.000861,0.000778,0.000847,0.000681,0.001038,0.000876,0.000833,0.000994,0.000943,0.001212,0.000786,0.001072,0.0008,0.001031,0.000876,0.001037,0.001073,0.000701,0.000842,0.00103,0.000931,0.000917,0.001101,0.001009,0.00111,0.000577,0.000681,0.001014,0.000828,0.000728,0.000957,0.001205,0.000974,0.001019,0.001018,0.001149,0.000996,0.000813,0.000791,0.000994,0.000994,0.000994,0.000994,0.000994,0.000858,0.00095,0.001073,0.000843,0.000914,0.000984,0.000718,0.000869,2.9e-05,0.000753,0.001148,0.000958,0.00093,2.9e-05,2.9e-05,0.001011,0.000835,0.00087,0.00075,0.000762,0.00113,0.001086,0.000824,0.00076,0.000848,0.000972,0.000549,0.000549,0.000549,0.000549,0.000732,0.000659,0.000782,0.00099,0.00104,0.000724,0.000845,0.000727,0.000979,0.000785,0.001034,0.000708,0.000928,0.001006,0.000517,0.000662,0.000775,0.00095,0.000688,0.000992,2.9e-05,2.9e-05,0.001001,0.000998,0.000694,2.9e-05,2.9e-05,0.001117,0.001059,0.000975,0.000833,0.000995,0.001085,0.00104,0.001124,0.000996,0.001011,0.001095,0.001092,0.00092,0.000658,0.000658,0.000658,0.000856,0.000867,0.000739,0.000867,0.000856,0.000994,0.000994,0.000881,0.000656,0.000656,0.000772,0.00069,0.000656,0.000867,0.000995,0.000783,0.000656,0.000885,0.000656,0.000656,0.000656,0.000779,0.000587,0.000653,0.000801,0.000801,0.000801,0.000801,0.000801,0.000606,0.000685,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.00216,0.001828,0.002092,0.001597,0.001572,0.001773,0.001385,0.001957,0.001515,0.001083,0.001672,0.001437,0.001575,0.00122,0.001353,0.001499,0.001437,0.002075,0.001233,0.001805,0.001618,0.001686,0.00157,0.001464,0.001623,0.001148,0.001289,0.001626,0.001447,0.001704,0.001779,0.001517,0.001855,0.000992,0.00125,0.001678,0.001479,0.001242,0.001372,0.00188,0.001653,0.001449,0.001484,0.001808,0.001499,0.001368,0.001147,0.001499,0.001499,0.001499,0.001499,0.001499,0.001215,0.001541,0.001689,0.00134,0.001546,0.001582,0.001033,0.001748,0.001393,0.001081,0.001818,0.001605,0.001497,0.000678,0.000878,0.00142,0.001233,0.001541,0.00149,0.001395,0.001808,0.001852,0.001486,0.001319,0.0013,0.001695,0.000801,0.000801,0.000801,0.000801,0.001239,0.001458,0.001422,0.001484,0.001776,0.000988,0.001289,0.001214,0.001543,0.001059,0.001712,0.001406,0.001524,0.001764,0.000842,0.001437,0.001493,0.00158,0.000883,0.001387,0.000921,0.000921,0.001658,0.001502,0.001221,0.000921,0.00094,0.001802,0.001715,0.001706,0.001379,0.001748,0.001789,0.001692,0.001815,0.001664,0.00157,0.001692,0.001718,0.001524,0.001239,0.001236,0.001239,0.001302,0.001597,0.001403,0.0016,0.001302,0.00161,0.001664,0.001449,0.001389,0.001393,0.001508,0.001517,0.001214,0.001555,0.00161,0.001376,0.001357,0.001546,0.001215,0.000975,0.001513,0.001456,0.001555,0.001155,0.001227,0.001235,0.001227,0.001235,0.001227,0.001389,0.001221,0.001401,0.001401,0.001235,0.000983,0.000852,0.001431,0.001088,0.001037,0.00096,0.000874,0.001266,0.00096,0.00096,0.001238,0.000971,0.00096,0.001395,0.001422,0.001319,0.001287,0.001129,0.00096,0.001129,0.000961,0.00098,0.00098,0.001104,0.001429,0.000984,0.000963,0.000968],[0.024548,0.024181,0.02483,0.023944,0.024222,0.024047,0.023682,0.0241,0.023764,0.023559,0.024247,0.0241,0.02308,0.023429,0.023858,0.023529,0.024022,0.024438,0.023077,0.024112,0.024131,0.023685,0.024103,0.022518,0.022842,0.02347,0.023329,0.024093,0.023929,0.024187,0.0242,0.023852,0.024238,0.02268,0.023929,0.0238,0.023889,0.023616,0.023083,0.0241,0.024165,0.023709,0.023752,0.024015,0.023625,0.024037,0.021975,0.023134,0.023134,0.023134,0.023134,0.023134,0.022949,0.024106,0.022705,0.023935,0.024181,0.02268,0.023017,0.024317,0.024365,0.022556,0.023861,0.023852,0.023813,0.005348,0.022007,0.023813,0.023048,0.02384,0.024,0.023951,0.023935,0.02396,0.024028,0.023852,0.023868,0.024241,0.022548,0.022548,0.022548,0.022548,0.023895,0.023791,0.023794,0.024006,0.023911,0.021929,0.02388,0.023846,0.023846,0.022204,0.022954,0.023803,0.024181,0.023697,0.021929,0.023679,0.024028,0.024206,0.022554,0.023134,0.023619,0.023619,0.023849,0.023874,0.023861,0.023619,0.023679,0.023978,0.023972,0.024062,0.024003,0.024,0.024009,0.023868,0.023861,0.023861,0.023898,0.024172,0.023849,0.024112,0.023938,0.02392,0.023938,0.023914,0.023969,0.023985,0.023972,0.023743,0.023901,0.023975,0.023941,0.023932,0.023954,0.02405,0.024178,0.023849,0.024047,0.023855,0.024003,0.023975,0.023849,0.023858,0.023464,0.024159,0.023883,0.024178,0.023538,0.023734,0.023904,0.023734,0.023904,0.023734,0.023797,0.022329,0.024454,0.024548,0.02448,0.022445,0.022423,0.02404,0.024387,0.024206,0.022445,0.022496,0.024003,0.022445,0.022445,0.024153,0.023247,0.022445,0.024081,0.024112,0.02426,0.024269,0.024015,0.022445,0.024015,0.023163,0.023227,0.023227,0.023849,0.024062,0.023166,0.023479,0.023997],[0.220379,0.211364,0.191358,0.200647,0.15122,0.197452,0.147502,0.222488,0.160207,0.144635,0.157627,0.126617,0.193146,0.163015,0.154871,0.184891,0.175306,0.225455,0.146112,0.199357,0.1488,0.191753,0.163015,0.192946,0.199571,0.130435,0.156698,0.191555,0.173184,0.170642,0.204846,0.187689,0.206437,0.107328,0.126617,0.188641,0.153974,0.135371,0.17799,0.224096,0.18111,0.189602,0.189409,0.213793,0.185259,0.15122,0.147036,0.184891,0.184891,0.184891,0.184891,0.184891,0.159657,0.176638,0.199571,0.15683,0.170018,0.183071,0.133621,0.161599,0.005348,0.14006,0.213548,0.178161,0.173023,0.005376,0.005405,0.188069,0.155388,0.161739,0.13943,0.14166,0.210169,0.201954,0.153339,0.141337,0.157761,0.180758,0.102142,0.102142,0.102142,0.102142,0.136164,0.12261,0.14554,0.184158,0.193347,0.134685,0.157227,0.135273,0.182174,0.145997,0.192347,0.131635,0.172542,0.187123,0.096223,0.123097,0.144074,0.176638,0.128011,0.184524,0.005348,0.005348,0.186186,0.185629,0.129167,0.005348,0.005348,0.207821,0.197034,0.181287,0.155,0.185075,0.201735,0.193347,0.208989,0.185259,0.188069,0.203724,0.203057,0.171113,0.122449,0.122449,0.122449,0.159247,0.161179,0.137472,0.161179,0.159247,0.184891,0.184891,0.163877,0.122047,0.122047,0.143519,0.128276,0.122047,0.161179,0.185075,0.145654,0.122047,0.164602,0.122047,0.122047,0.122047,0.14486,0.109219,0.121489,0.149038,0.149038,0.149038,0.149038,0.149038,0.112796,0.127485,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.401728,0.340037,0.389121,0.297125,0.292453,0.329787,0.257618,0.363992,0.281818,0.201517,0.311037,0.267241,0.292913,0.226829,0.251691,0.278861,0.267241,0.385892,0.229346,0.33574,0.300971,0.313659,0.291994,0.272328,0.301948,0.213548,0.239691,0.302439,0.269175,0.316865,0.330961,0.282246,0.345083,0.184524,0.2325,0.312081,0.275148,0.231056,0.255144,0.349624,0.307438,0.269565,0.275964,0.336347,0.278861,0.254446,0.213303,0.278861,0.278861,0.278861,0.278861,0.278861,0.226002,0.286595,0.314189,0.24933,0.287481,0.294304,0.192149,0.325175,0.259053,0.201081,0.338182,0.298555,0.278443,0.126187,0.163301,0.264205,0.229346,0.286595,0.277198,0.259414,0.336347,0.344444,0.276374,0.245383,0.241873,0.315254,0.148919,0.148919,0.148919,0.148919,0.230483,0.271137,0.26458,0.275964,0.330373,0.183794,0.239691,0.225728,0.287037,0.197034,0.318493,0.261603,0.283537,0.328042,0.156698,0.267241,0.277612,0.293839,0.164311,0.257975,0.171271,0.171271,0.308458,0.279279,0.227106,0.171271,0.174812,0.335135,0.319039,0.317406,0.256552,0.325175,0.332737,0.314721,0.337568,0.309484,0.291994,0.314721,0.319588,0.283537,0.230483,0.229913,0.230483,0.242188,0.297125,0.26087,0.2976,0.242188,0.299517,0.309484,0.269565,0.258333,0.259053,0.280543,0.282246,0.225728,0.289269,0.299517,0.255846,0.252374,0.287481,0.226002,0.181287,0.281392,0.270742,0.289269,0.214781,0.228221,0.22963,0.228221,0.22963,0.228221,0.258333,0.227106,0.260504,0.260504,0.22963,0.182891,0.158433,0.266094,0.202394,0.192946,0.178503,0.162587,0.235443,0.178503,0.178503,0.230198,0.180583,0.178503,0.259414,0.26458,0.245383,0.239382,0.209932,0.178503,0.209932,0.178674,0.182353,0.182353,0.205298,0.265714,0.183071,0.179191,0.180058],[0.372,0.343173,0.392405,0.334532,0.308458,0.301948,0.304419,0.319039,0.311037,0.288372,0.293839,0.286154,0.333932,0.322917,0.287481,0.290625,0.352273,0.315254,0.292913,0.286154,0.302932,0.285714,0.295707,0.333333,0.285714,0.240621,0.277198,0.318493,0.333333,0.316327,0.310518,0.2976,0.315789,0.223827,0.230198,0.285714,0.307438,0.267626,0.298555,0.320138,0.334532,0.317406,0.317406,0.293839,0.293375,0.300485,0.230769,0.291536,0.291536,0.291536,0.291536,0.291536,0.282675,0.2976,0.331551,0.319039,0.304419,0.288372,0.261236,0.324607,0.005348,0.227941,0.297125,0.290625,0.299517,0.005376,0.005405,0.306931,0.298555,0.283537,0.283537,0.294304,0.296651,0.296178,0.289269,0.290172,0.288372,0.314189,0.229346,0.229346,0.229346,0.229346,0.287481,0.250674,0.295238,0.299035,0.292453,0.274742,0.302439,0.286154,0.291536,0.302439,0.28882,0.24933,0.295707,0.288372,0.216028,0.223289,0.285714,0.306931,0.234552,0.313659,0.005348,0.005348,0.290625,0.298077,0.285276,0.005348,0.005348,0.296178,0.296651,0.28972,0.287037,0.285714,0.287037,0.291994,0.29477,0.295238,0.290172,0.290172,0.295238,0.311037,0.283537,0.283537,0.283537,0.291994,0.282675,0.285714,0.282675,0.291994,0.291536,0.291536,0.292453,0.282675,0.282675,0.298555,0.284839,0.282675,0.282675,0.291536,0.283105,0.282675,0.286154,0.282675,0.282675,0.282675,0.291536,0.221165,0.252374,0.256198,0.256198,0.256198,0.256198,0.256198,0.226553,0.227106,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var10_df_closseness, by=list(var10_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var10_df_closseness, by=list(var10_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-2234d6b1ac4bb9ead6ae" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-2234d6b1ac4bb9ead6ae">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000121\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001212\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000101\" data-max=\"0.000462\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000968\" data-max=\"0.00216\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6.3e-05\" data-max=\"0.00036\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022445\" data-max=\"0.02483\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.3e-05\" data-max=\"0.004192\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.225455\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.0188\" data-max=\"0.086046\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.180058\" data-max=\"0.401728\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.011751\" data-max=\"0.067059\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003897\" data-max=\"0.140003\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[0.000129,0.000125,0.00013,0.00013,0.000127,0.000131,0.00013,0.000133,0.00013,0.000128,0.00013,0.000121,0.000126,0.000129,0.000129,0.000126,0.000129,0.00013,0.000132,0.000126,0.000131,0.000129,0.000129],[2e-06,4e-06,null,null,null,null,0,null,null,null,null,2.2e-05,1e-06,1e-06,1e-06,3e-06,null,null,null,2e-06,null,1e-06,1e-06],[0.000912,0.000289,0.001072,0.001136,0.000793,0.000869,0.000829,0.001029,0.000876,0.000779,0.000972,0.00085,0.000948,0.000877,0.000939,0.000925,2.9e-05,0.00095,0.001185,0.000647,0.001212,0.000695,0.00089],[0.000235,0.000358,null,null,null,null,0.000189,null,null,null,null,0.000266,0.000101,0.000149,0.000125,0.000168,null,null,null,0.000462,null,0.000313,0.000157],[0.001554,0.00113,0.001805,0.001828,0.001385,0.001748,0.001586,0.002092,0.00157,0.001456,0.001695,0.00143,0.001334,0.00151,0.001594,0.001376,0.000968,0.00158,0.00216,0.001276,0.002075,0.001373,0.001521],[0.00036,0.000232,null,null,null,null,6.3e-05,null,null,null,null,0.000357,0.000162,0.000184,0.000169,0.000285,null,null,null,0.00027,null,0.000173,0.00018],[0.02398,0.023204,0.024112,0.024181,0.023682,0.024317,0.024128,0.02483,0.024103,0.023883,0.024241,0.022445,0.023569,0.024056,0.024097,0.023411,0.023997,0.024206,0.024548,0.02337,0.024438,0.02399,0.023949],[0.000385,0.000757,null,null,null,null,3.3e-05,null,null,null,null,0.004192,0.000198,0.00013,0.000109,0.000517,null,null,null,0.000257,null,0.000244,0.000106],[0.169679,0.053671,0.199357,0.211364,0.147502,0.161599,0.154134,0.191358,0.163015,0.14486,0.180758,0.158169,0.176308,0.163137,0.174659,0.172145,0.005348,0.176638,0.220379,0.120342,0.225455,0.129367,0.165451],[0.043612,0.066616,null,null,null,null,0.03506,null,null,null,null,0.049542,0.0188,0.027802,0.023123,0.031217,null,null,null,0.086046,null,0.058194,0.029185],[0.289101,0.210209,0.33574,0.340037,0.257618,0.325175,0.294934,0.389121,0.291994,0.270742,0.315254,0.266075,0.248197,0.280964,0.296494,0.256037,0.180058,0.293839,0.401728,0.237333,0.385892,0.255359,0.2829],[0.067059,0.043104,null,null,null,null,0.011751,null,null,null,null,0.066313,0.030219,0.034158,0.031468,0.05295,null,null,null,0.050186,null,0.032291,0.03342],[0.300898,0.099162,0.286154,0.343173,0.304419,0.324607,0.301367,0.392405,0.295707,0.291536,0.314189,0.267033,0.320162,0.302714,0.320893,0.296087,0.005348,0.306931,0.372,0.194656,0.315254,0.234499,0.289229],[0.043056,0.125946,null,null,null,null,0.01796,null,null,null,null,0.070859,0.003897,0.031779,0.02737,0.024042,null,null,null,0.140003,null,0.102884,0.011641]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var10_df_closseness <- data.frame(
var10_incloseness,
var10_outcloseness,
var10_totalcloseness,
var10_incloseness_n,
var10_outcloseness_n,
var10_totalcloseness_n,
var10_centr_closeness) %>% round(6)

#Adding type
var10_df_closseness <-cbind(var10_df_closseness, V(var10)$TIPO1)

#Adding names
names(var10_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var10_df_closseness<-var10_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var10_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-188bd725062f3681b541" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-188bd725062f3681b541">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001212\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000678\" data-max=\"0.00216\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.02483\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.225455\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.126187\" data-max=\"0.401728\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[0.000132,0.00013,0.000133,0.000129,0.00013,0.000129,0.000127,0.00013,0.000128,0.000127,0.00013,0.00013,0.000124,0.000126,0.000128,0.000127,0.000129,0.000131,0.000124,0.00013,0.00013,0.000127,0.00013,0.000121,0.000123,0.000126,0.000125,0.00013,0.000129,0.00013,0.00013,0.000128,0.00013,0.000122,0.000129,0.000128,0.000128,0.000127,0.000124,0.00013,0.00013,0.000127,0.000128,0.000129,0.000127,0.000129,0.000118,0.000124,0.000124,0.000124,0.000124,0.000124,0.000123,0.00013,0.000122,0.000129,0.00013,0.000122,0.000124,0.000131,0.000131,0.000121,0.000128,0.000128,0.000128,2.9e-05,0.000118,0.000128,0.000124,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.000128,0.000128,0.00013,0.000121,0.000121,0.000121,0.000121,0.000128,0.000128,0.000128,0.000129,0.000129,0.000118,0.000128,0.000128,0.000128,0.000119,0.000123,0.000128,0.00013,0.000127,0.000118,0.000127,0.000129,0.00013,0.000121,0.000124,0.000127,0.000127,0.000128,0.000128,0.000128,0.000127,0.000127,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000128,0.000128,0.000128,0.000128,0.00013,0.000128,0.00013,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.00013,0.000128,0.000129,0.000128,0.000129,0.000129,0.000128,0.000128,0.000126,0.00013,0.000128,0.00013,0.000127,0.000128,0.000129,0.000128,0.000129,0.000128,0.000128,0.00012,0.000131,0.000132,0.000132,0.000121,0.000121,0.000129,0.000131,0.00013,0.000121,0.000121,0.000129,0.000121,0.000121,0.00013,0.000125,0.000121,0.000129,0.00013,0.00013,0.00013,0.000129,0.000121,0.000129,0.000125,0.000125,0.000125,0.000128,0.000129,0.000125,0.000126,0.000129],[0.001185,0.001136,0.001029,0.001079,0.000813,0.001062,0.000793,0.001196,0.000861,0.000778,0.000847,0.000681,0.001038,0.000876,0.000833,0.000994,0.000943,0.001212,0.000786,0.001072,0.0008,0.001031,0.000876,0.001037,0.001073,0.000701,0.000842,0.00103,0.000931,0.000917,0.001101,0.001009,0.00111,0.000577,0.000681,0.001014,0.000828,0.000728,0.000957,0.001205,0.000974,0.001019,0.001018,0.001149,0.000996,0.000813,0.000791,0.000994,0.000994,0.000994,0.000994,0.000994,0.000858,0.00095,0.001073,0.000843,0.000914,0.000984,0.000718,0.000869,2.9e-05,0.000753,0.001148,0.000958,0.00093,2.9e-05,2.9e-05,0.001011,0.000835,0.00087,0.00075,0.000762,0.00113,0.001086,0.000824,0.00076,0.000848,0.000972,0.000549,0.000549,0.000549,0.000549,0.000732,0.000659,0.000782,0.00099,0.00104,0.000724,0.000845,0.000727,0.000979,0.000785,0.001034,0.000708,0.000928,0.001006,0.000517,0.000662,0.000775,0.00095,0.000688,0.000992,2.9e-05,2.9e-05,0.001001,0.000998,0.000694,2.9e-05,2.9e-05,0.001117,0.001059,0.000975,0.000833,0.000995,0.001085,0.00104,0.001124,0.000996,0.001011,0.001095,0.001092,0.00092,0.000658,0.000658,0.000658,0.000856,0.000867,0.000739,0.000867,0.000856,0.000994,0.000994,0.000881,0.000656,0.000656,0.000772,0.00069,0.000656,0.000867,0.000995,0.000783,0.000656,0.000885,0.000656,0.000656,0.000656,0.000779,0.000587,0.000653,0.000801,0.000801,0.000801,0.000801,0.000801,0.000606,0.000685,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.00216,0.001828,0.002092,0.001597,0.001572,0.001773,0.001385,0.001957,0.001515,0.001083,0.001672,0.001437,0.001575,0.00122,0.001353,0.001499,0.001437,0.002075,0.001233,0.001805,0.001618,0.001686,0.00157,0.001464,0.001623,0.001148,0.001289,0.001626,0.001447,0.001704,0.001779,0.001517,0.001855,0.000992,0.00125,0.001678,0.001479,0.001242,0.001372,0.00188,0.001653,0.001449,0.001484,0.001808,0.001499,0.001368,0.001147,0.001499,0.001499,0.001499,0.001499,0.001499,0.001215,0.001541,0.001689,0.00134,0.001546,0.001582,0.001033,0.001748,0.001393,0.001081,0.001818,0.001605,0.001497,0.000678,0.000878,0.00142,0.001233,0.001541,0.00149,0.001395,0.001808,0.001852,0.001486,0.001319,0.0013,0.001695,0.000801,0.000801,0.000801,0.000801,0.001239,0.001458,0.001422,0.001484,0.001776,0.000988,0.001289,0.001214,0.001543,0.001059,0.001712,0.001406,0.001524,0.001764,0.000842,0.001437,0.001493,0.00158,0.000883,0.001387,0.000921,0.000921,0.001658,0.001502,0.001221,0.000921,0.00094,0.001802,0.001715,0.001706,0.001379,0.001748,0.001789,0.001692,0.001815,0.001664,0.00157,0.001692,0.001718,0.001524,0.001239,0.001236,0.001239,0.001302,0.001597,0.001403,0.0016,0.001302,0.00161,0.001664,0.001449,0.001389,0.001393,0.001508,0.001517,0.001214,0.001555,0.00161,0.001376,0.001357,0.001546,0.001215,0.000975,0.001513,0.001456,0.001555,0.001155,0.001227,0.001235,0.001227,0.001235,0.001227,0.001389,0.001221,0.001401,0.001401,0.001235,0.000983,0.000852,0.001431,0.001088,0.001037,0.00096,0.000874,0.001266,0.00096,0.00096,0.001238,0.000971,0.00096,0.001395,0.001422,0.001319,0.001287,0.001129,0.00096,0.001129,0.000961,0.00098,0.00098,0.001104,0.001429,0.000984,0.000963,0.000968],[0.024548,0.024181,0.02483,0.023944,0.024222,0.024047,0.023682,0.0241,0.023764,0.023559,0.024247,0.0241,0.02308,0.023429,0.023858,0.023529,0.024022,0.024438,0.023077,0.024112,0.024131,0.023685,0.024103,0.022518,0.022842,0.02347,0.023329,0.024093,0.023929,0.024187,0.0242,0.023852,0.024238,0.02268,0.023929,0.0238,0.023889,0.023616,0.023083,0.0241,0.024165,0.023709,0.023752,0.024015,0.023625,0.024037,0.021975,0.023134,0.023134,0.023134,0.023134,0.023134,0.022949,0.024106,0.022705,0.023935,0.024181,0.02268,0.023017,0.024317,0.024365,0.022556,0.023861,0.023852,0.023813,0.005348,0.022007,0.023813,0.023048,0.02384,0.024,0.023951,0.023935,0.02396,0.024028,0.023852,0.023868,0.024241,0.022548,0.022548,0.022548,0.022548,0.023895,0.023791,0.023794,0.024006,0.023911,0.021929,0.02388,0.023846,0.023846,0.022204,0.022954,0.023803,0.024181,0.023697,0.021929,0.023679,0.024028,0.024206,0.022554,0.023134,0.023619,0.023619,0.023849,0.023874,0.023861,0.023619,0.023679,0.023978,0.023972,0.024062,0.024003,0.024,0.024009,0.023868,0.023861,0.023861,0.023898,0.024172,0.023849,0.024112,0.023938,0.02392,0.023938,0.023914,0.023969,0.023985,0.023972,0.023743,0.023901,0.023975,0.023941,0.023932,0.023954,0.02405,0.024178,0.023849,0.024047,0.023855,0.024003,0.023975,0.023849,0.023858,0.023464,0.024159,0.023883,0.024178,0.023538,0.023734,0.023904,0.023734,0.023904,0.023734,0.023797,0.022329,0.024454,0.024548,0.02448,0.022445,0.022423,0.02404,0.024387,0.024206,0.022445,0.022496,0.024003,0.022445,0.022445,0.024153,0.023247,0.022445,0.024081,0.024112,0.02426,0.024269,0.024015,0.022445,0.024015,0.023163,0.023227,0.023227,0.023849,0.024062,0.023166,0.023479,0.023997],[0.220379,0.211364,0.191358,0.200647,0.15122,0.197452,0.147502,0.222488,0.160207,0.144635,0.157627,0.126617,0.193146,0.163015,0.154871,0.184891,0.175306,0.225455,0.146112,0.199357,0.1488,0.191753,0.163015,0.192946,0.199571,0.130435,0.156698,0.191555,0.173184,0.170642,0.204846,0.187689,0.206437,0.107328,0.126617,0.188641,0.153974,0.135371,0.17799,0.224096,0.18111,0.189602,0.189409,0.213793,0.185259,0.15122,0.147036,0.184891,0.184891,0.184891,0.184891,0.184891,0.159657,0.176638,0.199571,0.15683,0.170018,0.183071,0.133621,0.161599,0.005348,0.14006,0.213548,0.178161,0.173023,0.005376,0.005405,0.188069,0.155388,0.161739,0.13943,0.14166,0.210169,0.201954,0.153339,0.141337,0.157761,0.180758,0.102142,0.102142,0.102142,0.102142,0.136164,0.12261,0.14554,0.184158,0.193347,0.134685,0.157227,0.135273,0.182174,0.145997,0.192347,0.131635,0.172542,0.187123,0.096223,0.123097,0.144074,0.176638,0.128011,0.184524,0.005348,0.005348,0.186186,0.185629,0.129167,0.005348,0.005348,0.207821,0.197034,0.181287,0.155,0.185075,0.201735,0.193347,0.208989,0.185259,0.188069,0.203724,0.203057,0.171113,0.122449,0.122449,0.122449,0.159247,0.161179,0.137472,0.161179,0.159247,0.184891,0.184891,0.163877,0.122047,0.122047,0.143519,0.128276,0.122047,0.161179,0.185075,0.145654,0.122047,0.164602,0.122047,0.122047,0.122047,0.14486,0.109219,0.121489,0.149038,0.149038,0.149038,0.149038,0.149038,0.112796,0.127485,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.401728,0.340037,0.389121,0.297125,0.292453,0.329787,0.257618,0.363992,0.281818,0.201517,0.311037,0.267241,0.292913,0.226829,0.251691,0.278861,0.267241,0.385892,0.229346,0.33574,0.300971,0.313659,0.291994,0.272328,0.301948,0.213548,0.239691,0.302439,0.269175,0.316865,0.330961,0.282246,0.345083,0.184524,0.2325,0.312081,0.275148,0.231056,0.255144,0.349624,0.307438,0.269565,0.275964,0.336347,0.278861,0.254446,0.213303,0.278861,0.278861,0.278861,0.278861,0.278861,0.226002,0.286595,0.314189,0.24933,0.287481,0.294304,0.192149,0.325175,0.259053,0.201081,0.338182,0.298555,0.278443,0.126187,0.163301,0.264205,0.229346,0.286595,0.277198,0.259414,0.336347,0.344444,0.276374,0.245383,0.241873,0.315254,0.148919,0.148919,0.148919,0.148919,0.230483,0.271137,0.26458,0.275964,0.330373,0.183794,0.239691,0.225728,0.287037,0.197034,0.318493,0.261603,0.283537,0.328042,0.156698,0.267241,0.277612,0.293839,0.164311,0.257975,0.171271,0.171271,0.308458,0.279279,0.227106,0.171271,0.174812,0.335135,0.319039,0.317406,0.256552,0.325175,0.332737,0.314721,0.337568,0.309484,0.291994,0.314721,0.319588,0.283537,0.230483,0.229913,0.230483,0.242188,0.297125,0.26087,0.2976,0.242188,0.299517,0.309484,0.269565,0.258333,0.259053,0.280543,0.282246,0.225728,0.289269,0.299517,0.255846,0.252374,0.287481,0.226002,0.181287,0.281392,0.270742,0.289269,0.214781,0.228221,0.22963,0.228221,0.22963,0.228221,0.258333,0.227106,0.260504,0.260504,0.22963,0.182891,0.158433,0.266094,0.202394,0.192946,0.178503,0.162587,0.235443,0.178503,0.178503,0.230198,0.180583,0.178503,0.259414,0.26458,0.245383,0.239382,0.209932,0.178503,0.209932,0.178674,0.182353,0.182353,0.205298,0.265714,0.183071,0.179191,0.180058],[0.372,0.343173,0.392405,0.334532,0.308458,0.301948,0.304419,0.319039,0.311037,0.288372,0.293839,0.286154,0.333932,0.322917,0.287481,0.290625,0.352273,0.315254,0.292913,0.286154,0.302932,0.285714,0.295707,0.333333,0.285714,0.240621,0.277198,0.318493,0.333333,0.316327,0.310518,0.2976,0.315789,0.223827,0.230198,0.285714,0.307438,0.267626,0.298555,0.320138,0.334532,0.317406,0.317406,0.293839,0.293375,0.300485,0.230769,0.291536,0.291536,0.291536,0.291536,0.291536,0.282675,0.2976,0.331551,0.319039,0.304419,0.288372,0.261236,0.324607,0.005348,0.227941,0.297125,0.290625,0.299517,0.005376,0.005405,0.306931,0.298555,0.283537,0.283537,0.294304,0.296651,0.296178,0.289269,0.290172,0.288372,0.314189,0.229346,0.229346,0.229346,0.229346,0.287481,0.250674,0.295238,0.299035,0.292453,0.274742,0.302439,0.286154,0.291536,0.302439,0.28882,0.24933,0.295707,0.288372,0.216028,0.223289,0.285714,0.306931,0.234552,0.313659,0.005348,0.005348,0.290625,0.298077,0.285276,0.005348,0.005348,0.296178,0.296651,0.28972,0.287037,0.285714,0.287037,0.291994,0.29477,0.295238,0.290172,0.290172,0.295238,0.311037,0.283537,0.283537,0.283537,0.291994,0.282675,0.285714,0.282675,0.291994,0.291536,0.291536,0.292453,0.282675,0.282675,0.298555,0.284839,0.282675,0.282675,0.291536,0.283105,0.282675,0.286154,0.282675,0.282675,0.282675,0.291536,0.221165,0.252374,0.256198,0.256198,0.256198,0.256198,0.256198,0.226553,0.227106,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var10_df_closseness, by=list(var10_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var10_df_closseness, by=list(var10_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-b22279130aca9f56f5fc" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-b22279130aca9f56f5fc">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000124\" data-max=\"0.000129\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2e-06\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00053\" data-max=\"0.000835\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00027\" data-max=\"0.000421\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001243\" data-max=\"0.001494\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000238\" data-max=\"0.000301\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.023072\" data-max=\"0.023938\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000287\" data-max=\"0.002137\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.098535\" data-max=\"0.155287\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.050296\" data-max=\"0.078287\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.231287\" data-max=\"0.277802\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.044196\" data-max=\"0.056014\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.17403\" data-max=\"0.273185\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.079274\" data-max=\"0.135649\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[0.000129,0.000124],[2e-06,1.1e-05],[0.000835,0.00053],[0.00027,0.000421],[0.001494,0.001243],[0.000238,0.000301],[0.023938,0.023072],[0.000287,0.002137],[0.155287,0.098535],[0.050296,0.078287],[0.277802,0.231287],[0.044196,0.056014],[0.273185,0.17403],[0.079274,0.135649]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var10_df_closseness <- data.frame(
var10_incloseness,
var10_outcloseness,
var10_totalcloseness,
var10_incloseness_n,
var10_outcloseness_n,
var10_totalcloseness_n,
var10_centr_closeness) %>% round(6)

#Adding type
var10_df_closseness <-cbind(var10_df_closseness, V(var10)$TIPO2)

#Adding names
names(var10_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var10_df_closseness<-var10_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var10_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-7694618e9875a08146db" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-7694618e9875a08146db">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001212\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000678\" data-max=\"0.00216\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.02483\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.225455\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.126187\" data-max=\"0.401728\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.392405\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[0.000132,0.00013,0.000133,0.000129,0.00013,0.000129,0.000127,0.00013,0.000128,0.000127,0.00013,0.00013,0.000124,0.000126,0.000128,0.000127,0.000129,0.000131,0.000124,0.00013,0.00013,0.000127,0.00013,0.000121,0.000123,0.000126,0.000125,0.00013,0.000129,0.00013,0.00013,0.000128,0.00013,0.000122,0.000129,0.000128,0.000128,0.000127,0.000124,0.00013,0.00013,0.000127,0.000128,0.000129,0.000127,0.000129,0.000118,0.000124,0.000124,0.000124,0.000124,0.000124,0.000123,0.00013,0.000122,0.000129,0.00013,0.000122,0.000124,0.000131,0.000131,0.000121,0.000128,0.000128,0.000128,2.9e-05,0.000118,0.000128,0.000124,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.000128,0.000128,0.00013,0.000121,0.000121,0.000121,0.000121,0.000128,0.000128,0.000128,0.000129,0.000129,0.000118,0.000128,0.000128,0.000128,0.000119,0.000123,0.000128,0.00013,0.000127,0.000118,0.000127,0.000129,0.00013,0.000121,0.000124,0.000127,0.000127,0.000128,0.000128,0.000128,0.000127,0.000127,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000128,0.000128,0.000128,0.000128,0.00013,0.000128,0.00013,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.000128,0.000129,0.000129,0.000129,0.000129,0.000129,0.000129,0.00013,0.000128,0.000129,0.000128,0.000129,0.000129,0.000128,0.000128,0.000126,0.00013,0.000128,0.00013,0.000127,0.000128,0.000129,0.000128,0.000129,0.000128,0.000128,0.00012,0.000131,0.000132,0.000132,0.000121,0.000121,0.000129,0.000131,0.00013,0.000121,0.000121,0.000129,0.000121,0.000121,0.00013,0.000125,0.000121,0.000129,0.00013,0.00013,0.00013,0.000129,0.000121,0.000129,0.000125,0.000125,0.000125,0.000128,0.000129,0.000125,0.000126,0.000129],[0.001185,0.001136,0.001029,0.001079,0.000813,0.001062,0.000793,0.001196,0.000861,0.000778,0.000847,0.000681,0.001038,0.000876,0.000833,0.000994,0.000943,0.001212,0.000786,0.001072,0.0008,0.001031,0.000876,0.001037,0.001073,0.000701,0.000842,0.00103,0.000931,0.000917,0.001101,0.001009,0.00111,0.000577,0.000681,0.001014,0.000828,0.000728,0.000957,0.001205,0.000974,0.001019,0.001018,0.001149,0.000996,0.000813,0.000791,0.000994,0.000994,0.000994,0.000994,0.000994,0.000858,0.00095,0.001073,0.000843,0.000914,0.000984,0.000718,0.000869,2.9e-05,0.000753,0.001148,0.000958,0.00093,2.9e-05,2.9e-05,0.001011,0.000835,0.00087,0.00075,0.000762,0.00113,0.001086,0.000824,0.00076,0.000848,0.000972,0.000549,0.000549,0.000549,0.000549,0.000732,0.000659,0.000782,0.00099,0.00104,0.000724,0.000845,0.000727,0.000979,0.000785,0.001034,0.000708,0.000928,0.001006,0.000517,0.000662,0.000775,0.00095,0.000688,0.000992,2.9e-05,2.9e-05,0.001001,0.000998,0.000694,2.9e-05,2.9e-05,0.001117,0.001059,0.000975,0.000833,0.000995,0.001085,0.00104,0.001124,0.000996,0.001011,0.001095,0.001092,0.00092,0.000658,0.000658,0.000658,0.000856,0.000867,0.000739,0.000867,0.000856,0.000994,0.000994,0.000881,0.000656,0.000656,0.000772,0.00069,0.000656,0.000867,0.000995,0.000783,0.000656,0.000885,0.000656,0.000656,0.000656,0.000779,0.000587,0.000653,0.000801,0.000801,0.000801,0.000801,0.000801,0.000606,0.000685,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.00216,0.001828,0.002092,0.001597,0.001572,0.001773,0.001385,0.001957,0.001515,0.001083,0.001672,0.001437,0.001575,0.00122,0.001353,0.001499,0.001437,0.002075,0.001233,0.001805,0.001618,0.001686,0.00157,0.001464,0.001623,0.001148,0.001289,0.001626,0.001447,0.001704,0.001779,0.001517,0.001855,0.000992,0.00125,0.001678,0.001479,0.001242,0.001372,0.00188,0.001653,0.001449,0.001484,0.001808,0.001499,0.001368,0.001147,0.001499,0.001499,0.001499,0.001499,0.001499,0.001215,0.001541,0.001689,0.00134,0.001546,0.001582,0.001033,0.001748,0.001393,0.001081,0.001818,0.001605,0.001497,0.000678,0.000878,0.00142,0.001233,0.001541,0.00149,0.001395,0.001808,0.001852,0.001486,0.001319,0.0013,0.001695,0.000801,0.000801,0.000801,0.000801,0.001239,0.001458,0.001422,0.001484,0.001776,0.000988,0.001289,0.001214,0.001543,0.001059,0.001712,0.001406,0.001524,0.001764,0.000842,0.001437,0.001493,0.00158,0.000883,0.001387,0.000921,0.000921,0.001658,0.001502,0.001221,0.000921,0.00094,0.001802,0.001715,0.001706,0.001379,0.001748,0.001789,0.001692,0.001815,0.001664,0.00157,0.001692,0.001718,0.001524,0.001239,0.001236,0.001239,0.001302,0.001597,0.001403,0.0016,0.001302,0.00161,0.001664,0.001449,0.001389,0.001393,0.001508,0.001517,0.001214,0.001555,0.00161,0.001376,0.001357,0.001546,0.001215,0.000975,0.001513,0.001456,0.001555,0.001155,0.001227,0.001235,0.001227,0.001235,0.001227,0.001389,0.001221,0.001401,0.001401,0.001235,0.000983,0.000852,0.001431,0.001088,0.001037,0.00096,0.000874,0.001266,0.00096,0.00096,0.001238,0.000971,0.00096,0.001395,0.001422,0.001319,0.001287,0.001129,0.00096,0.001129,0.000961,0.00098,0.00098,0.001104,0.001429,0.000984,0.000963,0.000968],[0.024548,0.024181,0.02483,0.023944,0.024222,0.024047,0.023682,0.0241,0.023764,0.023559,0.024247,0.0241,0.02308,0.023429,0.023858,0.023529,0.024022,0.024438,0.023077,0.024112,0.024131,0.023685,0.024103,0.022518,0.022842,0.02347,0.023329,0.024093,0.023929,0.024187,0.0242,0.023852,0.024238,0.02268,0.023929,0.0238,0.023889,0.023616,0.023083,0.0241,0.024165,0.023709,0.023752,0.024015,0.023625,0.024037,0.021975,0.023134,0.023134,0.023134,0.023134,0.023134,0.022949,0.024106,0.022705,0.023935,0.024181,0.02268,0.023017,0.024317,0.024365,0.022556,0.023861,0.023852,0.023813,0.005348,0.022007,0.023813,0.023048,0.02384,0.024,0.023951,0.023935,0.02396,0.024028,0.023852,0.023868,0.024241,0.022548,0.022548,0.022548,0.022548,0.023895,0.023791,0.023794,0.024006,0.023911,0.021929,0.02388,0.023846,0.023846,0.022204,0.022954,0.023803,0.024181,0.023697,0.021929,0.023679,0.024028,0.024206,0.022554,0.023134,0.023619,0.023619,0.023849,0.023874,0.023861,0.023619,0.023679,0.023978,0.023972,0.024062,0.024003,0.024,0.024009,0.023868,0.023861,0.023861,0.023898,0.024172,0.023849,0.024112,0.023938,0.02392,0.023938,0.023914,0.023969,0.023985,0.023972,0.023743,0.023901,0.023975,0.023941,0.023932,0.023954,0.02405,0.024178,0.023849,0.024047,0.023855,0.024003,0.023975,0.023849,0.023858,0.023464,0.024159,0.023883,0.024178,0.023538,0.023734,0.023904,0.023734,0.023904,0.023734,0.023797,0.022329,0.024454,0.024548,0.02448,0.022445,0.022423,0.02404,0.024387,0.024206,0.022445,0.022496,0.024003,0.022445,0.022445,0.024153,0.023247,0.022445,0.024081,0.024112,0.02426,0.024269,0.024015,0.022445,0.024015,0.023163,0.023227,0.023227,0.023849,0.024062,0.023166,0.023479,0.023997],[0.220379,0.211364,0.191358,0.200647,0.15122,0.197452,0.147502,0.222488,0.160207,0.144635,0.157627,0.126617,0.193146,0.163015,0.154871,0.184891,0.175306,0.225455,0.146112,0.199357,0.1488,0.191753,0.163015,0.192946,0.199571,0.130435,0.156698,0.191555,0.173184,0.170642,0.204846,0.187689,0.206437,0.107328,0.126617,0.188641,0.153974,0.135371,0.17799,0.224096,0.18111,0.189602,0.189409,0.213793,0.185259,0.15122,0.147036,0.184891,0.184891,0.184891,0.184891,0.184891,0.159657,0.176638,0.199571,0.15683,0.170018,0.183071,0.133621,0.161599,0.005348,0.14006,0.213548,0.178161,0.173023,0.005376,0.005405,0.188069,0.155388,0.161739,0.13943,0.14166,0.210169,0.201954,0.153339,0.141337,0.157761,0.180758,0.102142,0.102142,0.102142,0.102142,0.136164,0.12261,0.14554,0.184158,0.193347,0.134685,0.157227,0.135273,0.182174,0.145997,0.192347,0.131635,0.172542,0.187123,0.096223,0.123097,0.144074,0.176638,0.128011,0.184524,0.005348,0.005348,0.186186,0.185629,0.129167,0.005348,0.005348,0.207821,0.197034,0.181287,0.155,0.185075,0.201735,0.193347,0.208989,0.185259,0.188069,0.203724,0.203057,0.171113,0.122449,0.122449,0.122449,0.159247,0.161179,0.137472,0.161179,0.159247,0.184891,0.184891,0.163877,0.122047,0.122047,0.143519,0.128276,0.122047,0.161179,0.185075,0.145654,0.122047,0.164602,0.122047,0.122047,0.122047,0.14486,0.109219,0.121489,0.149038,0.149038,0.149038,0.149038,0.149038,0.112796,0.127485,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.401728,0.340037,0.389121,0.297125,0.292453,0.329787,0.257618,0.363992,0.281818,0.201517,0.311037,0.267241,0.292913,0.226829,0.251691,0.278861,0.267241,0.385892,0.229346,0.33574,0.300971,0.313659,0.291994,0.272328,0.301948,0.213548,0.239691,0.302439,0.269175,0.316865,0.330961,0.282246,0.345083,0.184524,0.2325,0.312081,0.275148,0.231056,0.255144,0.349624,0.307438,0.269565,0.275964,0.336347,0.278861,0.254446,0.213303,0.278861,0.278861,0.278861,0.278861,0.278861,0.226002,0.286595,0.314189,0.24933,0.287481,0.294304,0.192149,0.325175,0.259053,0.201081,0.338182,0.298555,0.278443,0.126187,0.163301,0.264205,0.229346,0.286595,0.277198,0.259414,0.336347,0.344444,0.276374,0.245383,0.241873,0.315254,0.148919,0.148919,0.148919,0.148919,0.230483,0.271137,0.26458,0.275964,0.330373,0.183794,0.239691,0.225728,0.287037,0.197034,0.318493,0.261603,0.283537,0.328042,0.156698,0.267241,0.277612,0.293839,0.164311,0.257975,0.171271,0.171271,0.308458,0.279279,0.227106,0.171271,0.174812,0.335135,0.319039,0.317406,0.256552,0.325175,0.332737,0.314721,0.337568,0.309484,0.291994,0.314721,0.319588,0.283537,0.230483,0.229913,0.230483,0.242188,0.297125,0.26087,0.2976,0.242188,0.299517,0.309484,0.269565,0.258333,0.259053,0.280543,0.282246,0.225728,0.289269,0.299517,0.255846,0.252374,0.287481,0.226002,0.181287,0.281392,0.270742,0.289269,0.214781,0.228221,0.22963,0.228221,0.22963,0.228221,0.258333,0.227106,0.260504,0.260504,0.22963,0.182891,0.158433,0.266094,0.202394,0.192946,0.178503,0.162587,0.235443,0.178503,0.178503,0.230198,0.180583,0.178503,0.259414,0.26458,0.245383,0.239382,0.209932,0.178503,0.209932,0.178674,0.182353,0.182353,0.205298,0.265714,0.183071,0.179191,0.180058],[0.372,0.343173,0.392405,0.334532,0.308458,0.301948,0.304419,0.319039,0.311037,0.288372,0.293839,0.286154,0.333932,0.322917,0.287481,0.290625,0.352273,0.315254,0.292913,0.286154,0.302932,0.285714,0.295707,0.333333,0.285714,0.240621,0.277198,0.318493,0.333333,0.316327,0.310518,0.2976,0.315789,0.223827,0.230198,0.285714,0.307438,0.267626,0.298555,0.320138,0.334532,0.317406,0.317406,0.293839,0.293375,0.300485,0.230769,0.291536,0.291536,0.291536,0.291536,0.291536,0.282675,0.2976,0.331551,0.319039,0.304419,0.288372,0.261236,0.324607,0.005348,0.227941,0.297125,0.290625,0.299517,0.005376,0.005405,0.306931,0.298555,0.283537,0.283537,0.294304,0.296651,0.296178,0.289269,0.290172,0.288372,0.314189,0.229346,0.229346,0.229346,0.229346,0.287481,0.250674,0.295238,0.299035,0.292453,0.274742,0.302439,0.286154,0.291536,0.302439,0.28882,0.24933,0.295707,0.288372,0.216028,0.223289,0.285714,0.306931,0.234552,0.313659,0.005348,0.005348,0.290625,0.298077,0.285276,0.005348,0.005348,0.296178,0.296651,0.28972,0.287037,0.285714,0.287037,0.291994,0.29477,0.295238,0.290172,0.290172,0.295238,0.311037,0.283537,0.283537,0.283537,0.291994,0.282675,0.285714,0.282675,0.291994,0.291536,0.291536,0.292453,0.282675,0.282675,0.298555,0.284839,0.282675,0.282675,0.291536,0.283105,0.282675,0.286154,0.282675,0.282675,0.282675,0.291536,0.221165,0.252374,0.256198,0.256198,0.256198,0.256198,0.256198,0.226553,0.227106,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var10_df_closseness, by=list(var10_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var10_df_closseness, by=list(var10_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-eef99999c6f01ab6a89c" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-eef99999c6f01ab6a89c">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000124\" data-max=\"0.00013\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000527\" data-max=\"0.000918\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000139\" data-max=\"0.000423\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00124\" data-max=\"0.001572\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000182\" data-max=\"0.000301\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.023056\" data-max=\"0.024096\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000125\" data-max=\"0.002146\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.098105\" data-max=\"0.170701\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.025776\" data-max=\"0.078794\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.230552\" data-max=\"0.292396\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.033876\" data-max=\"0.05597\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172658\" data-max=\"0.310108\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.028112\" data-max=\"0.136\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[0.00013,0.000129,0.000124],[1e-06,2e-06,1.2e-05],[0.000918,0.00082,0.000527],[0.000139,0.000282,0.000423],[0.001572,0.001482,0.00124],[0.000182,0.000243,0.000301],[0.024096,0.023917,0.023056],[0.000125,0.000297,0.002146],[0.170701,0.152581,0.098105],[0.025776,0.052519,0.078794],[0.292396,0.275589,0.230552],[0.033876,0.04515,0.05597],[0.310108,0.267376,0.172658],[0.028112,0.082779,0.136]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var10_data.RData")
```

