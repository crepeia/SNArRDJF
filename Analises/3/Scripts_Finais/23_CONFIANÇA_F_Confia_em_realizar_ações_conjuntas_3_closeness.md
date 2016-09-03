# SNA Closeness 23_CONFIANÇA F) Confia em realizar ações conjuntas com este serviço (var21).
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 23_CONFIANÇA F) Confia em realizar ações conjuntas com este serviço (var21).

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var21_data.RData")
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
#var21<-simplify(var21) #Simplify
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
V(var21)$incloseness <- closeness(var21, mode = "in", weights = E(var21)$var21) %>% round(6)
V(var21)$outcloseness <- closeness(var21, mode = "out", weights = E(var21)$var21) %>% round(6)
V(var21)$totalcloseness <- closeness(var21, mode = "total", weights = E(var21)$var21) %>% round(4)
```

###Saving to Environment

```r
var21_incloseness<- closeness(var21, mode = "in", weights = E(var21)$var21) %>% round(6)
var21_outcloseness<- closeness(var21, mode = "out", weights = E(var21)$var21) %>% round(6)
var21_totalcloseness<- closeness(var21, mode = "total", weights = E(var21)$var21) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var21_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0001230 0.0001260 0.0001239 0.0001270 0.0001310
```

```r
sd(var21_incloseness)
```

```
## [1] 7.731805e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var21)$incloseness<-closeness(var21, weights = E(var21)$var21, mode="in")

#Get Variable
V(var21)$var21_color_degree<-round(V(var21)$incloseness,6)

#Creating brewer pallette
vertex_var21_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var21)$var21_color_degree)), "RdBu"))(
            length(unique(V(var21)$var21_color_degree)))

#Saving as Vertex properties 
V(var21)$vertex_var21_color_degree<-
  vertex_var21_color_degree[as.numeric(
  cut(V(var21)$var21_color_degree,
      breaks=length(unique(V(var21)$var21_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var21, es=E(var21), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var21))
maxC <- rep(Inf, vcount(var21))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var21, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var21)$weight)


#PLotting
plot(var21, 
     layout=co,
     edge.color=V(var21)$vertex_var21_color_degree[edge.start],
     edge.arrow.size=closeness(var21, weights = E(var21)$var21, mode="in"),
     edge.width=E(var21)$weight/mean(E(var21)$weight),
     edge.curved = TRUE,
     vertex.color=V(var21)$vertex_var21_color_degree,
     vertex.size=closeness(var21, weights = E(var21)$var21, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var21,"LABEL_COR"),
     vertex.label.cex=(closeness(var21, weights = E(var21)$var21, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var21)$var21_color_degree
b<-V(var21)$vertex_var21_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 23_CONFIANÇA F) Confia em realizar ações conjuntas com este serviço (var21).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var21, mode="in", weights = E(var21)$var21)), 
             sd(closeness(var21, mode="in", weights = E(var21)$var21))
             )
       )
```

![](23_CONFIANÇA_F_Confia_em_realizar_ações_conjuntas_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var21_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0005510 0.0006970 0.0005857 0.0007700 0.0011150
```

```r
sd(var21_outcloseness)
```

```
## [1] 0.0002972202
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var21)$outcloseness<-closeness(var21, weights = E(var21)$var21, mode="out")

#Get Variable
V(var21)$var21_color_degree<-round(V(var21)$outcloseness,6)

#Creating brewer pallette
vertex_var21_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var21)$var21_color_degree)), "RdBu"))(
            length(unique(V(var21)$var21_color_degree)))

#Saving as Vertex properties 
V(var21)$vertex_var21_color_degree<-
  vertex_var21_color_degree[as.numeric(
  cut(V(var21)$var21_color_degree,
      breaks=length(unique(V(var21)$var21_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var21, es=E(var21), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var21))
maxC <- rep(Inf, vcount(var21))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var21, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var21)$weight)


#PLotting
plot(var21, 
     layout=co,
     edge.color=V(var21)$vertex_var21_color_degree[edge.start],
     edge.arrow.size=closeness(var21, weights = E(var21)$var21, mode="out"),
     edge.width=E(var21)$weight/2*mean(E(var21)$weight),
     edge.curved = TRUE,
     vertex.color=V(var21)$vertex_var21_color_degree,
     vertex.size=closeness(var21, weights = E(var21)$var21, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var21,"LABEL_COR"),
     vertex.label.cex=closeness(var21, weights = E(var21)$var21, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var21)$var21_color_degree
b<-V(var21)$vertex_var21_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 23_CONFIANÇA F) Confia em realizar ações conjuntas com este serviço (var21).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var21, mode="out", weights = E(var21)$var21)), 
             sd(closeness(var21, mode="out", weights = E(var21)$var21))
             )
       )
```

![](23_CONFIANÇA_F_Confia_em_realizar_ações_conjuntas_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var21_totalcloseness)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000608 0.000879 0.001059 0.001001 0.001113 0.001650
```

```r
sd(var21_totalcloseness)
```

```
## [1] 0.0001782153
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var21)$allcloseness<-closeness(var21, weights = E(var21)$var21, mode="all")

#Get Variable
V(var21)$var21_color_degree<-round(V(var21)$allcloseness,6)

#Creating brewer pallette
vertex_var21_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var21)$var21_color_degree)), "RdBu"))(
            length(unique(V(var21)$var21_color_degree)))

#Saving as Vertex properties 
V(var21)$vertex_var21_color_degree<-
  vertex_var21_color_degree[as.numeric(
  cut(V(var21)$var21_color_degree,
      breaks=length(unique(V(var21)$var21_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var21, es=E(var21), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var21))
maxC <- rep(Inf, vcount(var21))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var21, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var21)$weight)


#PLotting
plot(var21, 
     layout=co,
     edge.color=V(var21)$vertex_var21_color_degree[edge.start],
     edge.arrow.size=closeness(var21, weights = E(var21)$var21, mode="all"),
     edge.width=E(var21)$weight/2*mean(E(var21)$weight),
     edge.curved = TRUE,
     vertex.color=V(var21)$vertex_var21_color_degree,
     vertex.size=closeness(var21, weights = E(var21)$var21, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var21,"LABEL_COR"),
     vertex.label.cex=(closeness(var21, weights = E(var21)$var21, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var21)$var21_color_degree
b<-V(var21)$vertex_var21_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 23_CONFIANÇA F) Confia em realizar ações conjuntas com este serviço (var21).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var21, mode="all", weights = E(var21)$var21)), 
             sd(closeness(var21, mode="all", weights = E(var21)$var21))
             )
       )
```

![](23_CONFIANÇA_F_Confia_em_realizar_ações_conjuntas_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var21)$incloseness_n <- closeness(var21, mode = "in",, weights = E(var21)$var21, normalized = T) %>% round(10)
V(var21)$outcloseness_n <- closeness(var21, mode = "out", normalized = T, weights = E(var21)$var21) %>% round(6)
V(var21)$totalcloseness_n <- closeness(var21, mode = "total", normalized = T, weights = E(var21)$var21) %>% round(6)
```

###Saving to Environment

```r
var21_incloseness_n<- closeness(var21, mode = "in", normalized = T, weights = E(var21)$var21) %>% round(6)
var21_outcloseness_n<- closeness(var21, mode = "out", normalized = T, weights = E(var21)$var21) %>% round(6)
var21_totalcloseness_n<- closeness(var21, mode = "total", normalized = T, weights = E(var21)$var21) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var21_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.022830 0.023460 0.023060 0.023550 0.024370
```

```r
sd(var21_incloseness_n)
```

```
## [1] 0.001441063
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var21)$incloseness_n<-closeness(var21, weights = E(var21)$var21, mode="in", normalized = T)

#Get Variable
V(var21)$var21_color_degree<-round(V(var21)$incloseness_n,6)

#Creating brewer pallette
vertex_var21_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var21)$var21_color_degree)), "RdBu"))(
            length(unique(V(var21)$var21_color_degree)))

#Saving as Vertex properties 
V(var21)$vertex_var21_color_degree<-
  vertex_var21_color_degree[as.numeric(
  cut(V(var21)$var21_color_degree,
      breaks=length(unique(V(var21)$var21_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var21, es=E(var21), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var21))
maxC <- rep(Inf, vcount(var21))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var21, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var21)$weight)


#PLotting
plot(var21, 
     layout=co,
     edge.color=V(var21)$vertex_var21_color_degree[edge.start],
     edge.arrow.size=closeness(var21, weights = E(var21)$var21, mode="in",normalized = T),
     edge.width=E(var21)$weight/10*mean(E(var21)$weight),
     edge.curved = TRUE,
     vertex.color=V(var21)$vertex_var21_color_degree,
     vertex.size=(closeness(var21, weights = E(var21)$var21, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var21,"LABEL_COR"),
     vertex.label.cex=closeness(var21, weights = E(var21)$var21, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var21)$var21_color_degree
b<-V(var21)$vertex_var21_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 23_CONFIANÇA F) Confia em realizar ações conjuntas com este serviço (var21).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var21, mode="in", weights = E(var21)$var21, normalized = T)), 
             sd(closeness(var21, mode="in", weights = E(var21)$var21, normalized = T))
             )
       )
```

![](23_CONFIANÇA_F_Confia_em_realizar_ações_conjuntas_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var21_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.102500 0.129700 0.108900 0.143200 0.207400
```

```r
sd(var21_outcloseness_n)
```

```
## [1] 0.05530292
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var21)$outcloseness_n<-closeness(var21, weights = E(var21)$var21, mode="out", normalized = T)

#Get Variable
V(var21)$var21_color_degree<-round(V(var21)$outcloseness_n,6)

#Creating brewer pallette
vertex_var21_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var21)$var21_color_degree)), "RdBu"))(
            length(unique(V(var21)$var21_color_degree)))

#Saving as Vertex properties 
V(var21)$vertex_var21_color_degree<-
  vertex_var21_color_degree[as.numeric(
  cut(V(var21)$var21_color_degree,
      breaks=length(unique(V(var21)$var21_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var21, es=E(var21), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var21))
maxC <- rep(Inf, vcount(var21))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var21, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var21)$weight)


#PLotting
plot(var21, 
     layout=co,
     edge.color=V(var21)$vertex_var21_color_degree[edge.start],
     edge.arrow.size=closeness(var21, weights = E(var21)$var21, mode="out",normalized = T),
     edge.width=E(var21)$weight/10*mean(E(var21)$weight),
     edge.curved = TRUE,
     vertex.color=V(var21)$vertex_var21_color_degree,
     vertex.size=(closeness(var21, weights = E(var21)$var21, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var21,"LABEL_COR"),
     vertex.label.cex=closeness(var21, weights = E(var21)$var21, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var21)$var21_color_degree
b<-V(var21)$vertex_var21_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 23_CONFIANÇA F) Confia em realizar ações conjuntas com este serviço (var21).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var21, mode="out", weights = E(var21)$var21, normalized = T)), 
             sd(closeness(var21, mode="out", weights = E(var21)$var21, normalized = T))
             )
       )
```

![](23_CONFIANÇA_F_Confia_em_realizar_ações_conjuntas_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var21_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1131  0.1635  0.1970  0.1862  0.2070  0.3069
```

```r
sd(var21_totalcloseness_n)
```

```
## [1] 0.03315053
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var21)$allcloseness_n<-closeness(var21, weights = E(var21)$var21, mode="all", normalized = T)

#Get Variable
V(var21)$var21_color_degree<-round(V(var21)$allcloseness_n,6)

#Creating brewer pallette
vertex_var21_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var21)$var21_color_degree)), "RdBu"))(
            length(unique(V(var21)$var21_color_degree)))

#Saving as Vertex properties 
V(var21)$vertex_var21_color_degree<-
  vertex_var21_color_degree[as.numeric(
  cut(V(var21)$var21_color_degree,
      breaks=length(unique(V(var21)$var21_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var21, es=E(var21), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var21))
maxC <- rep(Inf, vcount(var21))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var21, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var21)$weight)


#PLotting
plot(var21, 
     layout=co,
     edge.color=V(var21)$vertex_var21_color_degree[edge.start],
     edge.arrow.size=closeness(var21, weights = E(var21)$var21, mode="all",normalized = T),
     edge.width=E(var21)$weight/10*mean(E(var21)$weight),
     edge.curved = TRUE,
     vertex.color=V(var21)$vertex_var21_color_degree,
     vertex.size=(closeness(var21, weights = E(var21)$var21, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var21,"LABEL_COR"),
     vertex.label.cex=closeness(var21, weights = E(var21)$var21, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var21)$var21_color_degree
b<-V(var21)$vertex_var21_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 23_CONFIANÇA F) Confia em realizar ações conjuntas com este serviço (var21).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var21, mode="all", weights = E(var21)$var21, normalized = T)), 
             sd(closeness(var21, mode="all", weights = E(var21)$var21, normalized = T))
             )
       )
```

![](23_CONFIANÇA_F_Confia_em_realizar_ações_conjuntas_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var21)$incloseness_n <- closeness(var21, weights = E(var21)$var21, mode = "in", normalized = T) %>% round(6)
V(var21)$outcloseness_n <- closeness(var21, weights = E(var21)$var21, mode = "out", normalized = T) %>% round(6)
V(var21)$totalcloseness_n <- closeness(var21, weights = E(var21)$var21, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var21)$var21_centr_closeness<- centralization.closeness(var21)$res
var21_centr_closeness<- centralization.closeness(var21)$res
var21_centr_closeness_all<- centralization.closeness(var21)
```

###Centralization

```r
var21_centr_closeness_all$centralization
```

```
## [1] 0.1625197
```

###Theoretical Max

```r
var21_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var21)$var21_centr_closeness<- centralization.closeness(var21)$res

#Get Variable
V(var21)$var21_color_degree<-round(V(var21)$var21_centr_closeness,6)

#Creating brewer pallette
vertex_var21_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var21)$var21_color_degree)), "Spectral"))(
            length(unique(V(var21)$var21_color_degree)))

#Saving as Vertex properties 
V(var21)$vertex_var21_color_degree<-
  vertex_var21_color_degree[as.numeric(
  cut(V(var21)$var21_color_degree,
      breaks=length(unique(V(var21)$var21_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var21, es=E(var21), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var21))
maxC <- rep(Inf, vcount(var21))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var21, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var21)$weight)


#PLotting
plot(var21, 
     layout=co,
     edge.color=V(var21)$vertex_var21_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var21)$res,
     edge.width=E(var21)$weight/10*mean(E(var21)$weight),
     edge.curved = TRUE,
     vertex.color=V(var21)$vertex_var21_color_degree,
     vertex.size=centralization.closeness(var21)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var21,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var21)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var21)$var21_color_degree
b<-V(var21)$vertex_var21_color_degree
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
  title("Network Centralization Closeness - 23_CONFIANÇA F) Confia em realizar ações conjuntas com este serviço (var21).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var21)$res), 
             sd(centralization.closeness(var21)$res)
             )
       )
```

![](23_CONFIANÇA_F_Confia_em_realizar_ações_conjuntas_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var21_incloseness<- closeness(var21, weights = E(var21)$var21, mode = "in") %>% round(6)
var21_outcloseness<- closeness(var21, weights = E(var21)$var21, mode = "out") %>% round(6)
var21_totalcloseness<- closeness(var21, weights = E(var21)$var21, mode = "total") %>% round(6)
var21_incloseness_n<- closeness(var21,weights = E(var21)$var21, mode = "in", normalized = T) %>% round(6)
var21_outcloseness_n<- closeness(var21,weights = E(var21)$var21, mode = "out", normalized = T) %>% round(6)
var21_totalcloseness_n<- closeness(var21,weights = E(var21)$var21, mode = "total", normalized = T) %>% round(6)
var21_centr_closeness <- centralization.closeness(var21)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var21_df_closseness <- data.frame(
var21_incloseness,
var21_outcloseness,
var21_totalcloseness,
var21_incloseness_n,
var21_outcloseness_n,
var21_totalcloseness_n,
var21_centr_closeness) %>% round(6)

#Adding type
var21_df_closseness <-cbind(var21_df_closseness, V(var21)$LABEL_COR)

#Adding names
names(var21_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var21_df_closseness<-var21_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var21_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-a5fc549bd68f49c75553" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-a5fc549bd68f49c75553">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001115\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000608\" data-max=\"0.00165\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024374\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.207358\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.113139\" data-max=\"0.306931\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[0.000129,0.000127,0.000131,0.000127,0.000127,0.000127,0.000125,0.000129,0.000122,0.000129,0.000127,0.000127,0.000121,0.000123,0.000126,0.000127,0.000127,0.000128,0.000121,0.000125,0.000127,0.000124,0.000127,0.000117,0.000116,0.000126,0.00012,0.000127,0.000127,0.000127,0.000127,0.000126,0.000127,0.00012,0.000127,0.000123,0.000129,0.000122,0.000122,0.000127,0.000126,0.000125,0.000125,0.000129,0.000126,0.000127,0.000116,0.000123,0.000123,0.000123,0.000123,0.000123,0.000122,0.000125,0.00012,0.000127,0.000126,0.00012,0.000121,0.000128,0.00013,0.000118,0.000126,0.000126,0.000124,2.9e-05,0.000117,0.000127,0.000122,0.000126,0.000126,0.000127,0.000126,0.000127,0.000127,0.000126,0.000126,0.000127,0.000117,0.000117,0.000117,0.000117,0.000126,0.000124,0.000123,0.000126,0.000124,0.000115,0.000126,0.000126,0.000126,0.000117,0.000119,0.000124,0.000127,0.000123,0.000115,0.000122,0.000127,0.000127,0.000118,0.000124,0.000124,0.000124,0.000126,0.000127,0.000126,0.000124,0.000125,0.000126,0.000126,0.000127,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000127,0.000127,0.000126,0.000127,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000125,0.000126,0.000126,0.000126,0.000126,0.000126,0.000127,0.000126,0.000126,0.000127,0.000126,0.000126,0.000126,0.000126,0.000126,0.000124,0.000127,0.000126,0.000126,0.000121,0.000125,0.000126,0.000125,0.000126,0.000125,0.000127,0.000118,0.000128,0.000129,0.000129,0.000118,0.000118,0.000124,0.000128,0.000128,0.000118,0.000119,0.000123,0.000118,0.000118,0.000125,0.000123,0.000118,0.000123,0.000123,0.000124,0.000124,0.000125,0.000118,0.000125,0.000123,0.000123,0.000123,0.000124,0.000125,0.000123,0.00012,0.000127],[0.001115,0.000877,0.001034,0.000893,0.000774,0.000722,0.000737,0.000801,0.000882,0.00072,0.00081,0.000676,0.000854,0.000818,0.000812,0.000723,0.000886,0.000792,0.000692,0.000728,0.000765,0.000831,0.000775,0.000796,0.000718,0.000575,0.000798,0.000769,0.000841,0.00077,0.000805,0.000774,0.000989,0.000521,0.000579,0.000831,0.000814,0.000637,0.000692,0.000813,0.000843,0.000872,0.000871,0.000742,0.00073,0.000763,0.000639,0.000723,0.000723,0.000723,0.000723,0.000723,0.000711,0.000697,0.000817,0.000786,0.000771,0.000674,0.000601,0.000814,2.9e-05,0.000511,0.000755,0.000686,0.000752,2.9e-05,2.9e-05,0.000779,0.000706,0.000661,0.000752,0.000691,0.000751,0.00071,0.000684,0.000779,0.000734,0.000784,0.000551,0.000551,0.000551,0.000551,0.00073,0.000553,0.000786,0.000752,0.000759,0.000698,0.00077,0.000685,0.000739,0.000764,0.000677,0.000636,0.000825,0.000737,0.000504,0.000571,0.000749,0.000786,0.000546,0.000755,2.9e-05,2.9e-05,0.000851,0.000709,0.000767,2.9e-05,2.9e-05,0.000746,0.000747,0.000736,0.000813,0.00084,0.000729,0.001011,0.000746,0.000747,0.000736,0.000736,0.000867,0.001032,0.000661,0.000661,0.000661,0.000783,0.000658,0.00067,0.000658,0.000684,0.000776,0.000679,0.000681,0.00075,0.000658,0.000707,0.000664,0.000658,0.000658,0.000776,0.000679,0.000658,0.000665,0.000658,0.000658,0.000658,0.000679,0.000484,0.000578,0.000591,0.000591,0.000591,0.000591,0.000591,0.000544,0.000607,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.00165,0.001318,0.001506,0.001217,0.001134,0.001119,0.00096,0.001271,0.001127,0.0012,0.001195,0.001151,0.001055,0.001012,0.001114,0.001008,0.001203,0.001211,0.0009,0.000959,0.001119,0.001096,0.001211,0.000996,0.000906,0.001014,0.001083,0.00114,0.001152,0.001136,0.001174,0.001094,0.001368,0.000765,0.001111,0.001104,0.001222,0.000828,0.000862,0.001098,0.001046,0.001116,0.001121,0.001206,0.001012,0.001129,0.000884,0.000926,0.000926,0.000926,0.000926,0.000926,0.000923,0.001086,0.00104,0.001164,0.001092,0.000902,0.000884,0.001259,0.001064,0.000683,0.001083,0.001072,0.000985,0.000608,0.000679,0.001024,0.001018,0.001059,0.001115,0.001098,0.001106,0.001114,0.001083,0.001133,0.001112,0.001129,0.000677,0.000677,0.000677,0.000677,0.001067,0.000934,0.001073,0.001068,0.00099,0.000874,0.001103,0.001059,0.00106,0.000943,0.000872,0.000905,0.001119,0.000994,0.000667,0.000778,0.001095,0.001134,0.000695,0.000963,0.000771,0.000771,0.001114,0.001105,0.001131,0.000771,0.000778,0.001109,0.001063,0.001081,0.001091,0.001111,0.00107,0.001391,0.001066,0.001067,0.001115,0.00113,0.001131,0.001431,0.001063,0.00106,0.001063,0.001127,0.001058,0.001065,0.001071,0.001067,0.001115,0.001062,0.00106,0.001116,0.001063,0.001107,0.001058,0.001066,0.00113,0.001122,0.001068,0.001057,0.001057,0.001059,0.000827,0.001136,0.000945,0.001076,0.000768,0.001034,0.001057,0.001034,0.001057,0.001034,0.001109,0.00086,0.001035,0.001058,0.001057,0.000733,0.000626,0.00078,0.000918,0.000903,0.00067,0.000661,0.000761,0.00067,0.00067,0.00086,0.000779,0.00067,0.000765,0.000775,0.000789,0.0008,0.000865,0.00067,0.000865,0.000767,0.000789,0.000789,0.000797,0.000911,0.000775,0.000637,0.00082],[0.024,0.023658,0.024374,0.023562,0.023607,0.023589,0.023201,0.024012,0.022766,0.023969,0.023619,0.023649,0.022524,0.022966,0.023446,0.023544,0.023646,0.023797,0.022595,0.02318,0.023601,0.023131,0.023646,0.021831,0.021528,0.023491,0.022321,0.023631,0.023565,0.023553,0.023598,0.023461,0.023586,0.022348,0.023565,0.022845,0.023957,0.022677,0.022622,0.023664,0.023423,0.023259,0.023291,0.023969,0.023503,0.023607,0.02151,0.022833,0.022833,0.022833,0.022833,0.022833,0.022617,0.023244,0.022385,0.023568,0.023506,0.022391,0.022535,0.023764,0.02419,0.02197,0.023467,0.023464,0.023023,0.005348,0.021706,0.02355,0.022736,0.023435,0.023473,0.023568,0.023491,0.023538,0.023541,0.023458,0.023482,0.023628,0.021785,0.021785,0.021785,0.021785,0.023506,0.023023,0.022946,0.02352,0.02306,0.021431,0.023464,0.023435,0.023449,0.021788,0.022093,0.023152,0.023598,0.022943,0.021431,0.022783,0.023532,0.023565,0.021857,0.022994,0.023131,0.023131,0.023458,0.023559,0.023467,0.023131,0.023189,0.023485,0.023467,0.023547,0.023488,0.023479,0.02352,0.023461,0.023461,0.023461,0.023592,0.023634,0.023458,0.023577,0.023488,0.02347,0.023488,0.023476,0.02347,0.023479,0.023482,0.023265,0.023458,0.023479,0.023482,0.023473,0.023491,0.023598,0.023461,0.023461,0.023565,0.023449,0.023503,0.023452,0.023449,0.023458,0.02306,0.023706,0.023402,0.023476,0.02248,0.023244,0.023461,0.023244,0.023461,0.023244,0.023613,0.021962,0.0238,0.024019,0.024009,0.021913,0.021986,0.022997,0.023874,0.023828,0.021955,0.022182,0.022786,0.021955,0.021955,0.023224,0.022912,0.021955,0.022822,0.022901,0.022971,0.023014,0.023209,0.021955,0.023209,0.022814,0.022895,0.022895,0.023031,0.02332,0.022811,0.022305,0.023571],[0.207358,0.163158,0.192347,0.166071,0.143963,0.134296,0.137067,0.148919,0.164021,0.134006,0.150607,0.125676,0.158839,0.152209,0.151097,0.13449,0.164748,0.147268,0.12872,0.135371,0.142202,0.154613,0.144074,0.147971,0.133621,0.106897,0.148444,0.142967,0.156434,0.143187,0.149758,0.143963,0.183976,0.096825,0.107639,0.154613,0.151466,0.118396,0.12872,0.15122,0.15683,0.162162,0.162021,0.138085,0.135866,0.141985,0.118926,0.13449,0.13449,0.13449,0.13449,0.13449,0.13229,0.129707,0.151961,0.146112,0.143408,0.125337,0.111846,0.151466,0.005348,0.095092,0.140377,0.12766,0.139955,0.005376,0.005405,0.144973,0.131263,0.122935,0.139955,0.128453,0.139745,0.132102,0.127223,0.144973,0.136464,0.145882,0.102479,0.102479,0.102479,0.102479,0.135866,0.102819,0.146226,0.139955,0.14123,0.129888,0.143297,0.127485,0.137371,0.142093,0.125931,0.118245,0.153465,0.137168,0.09375,0.106286,0.139222,0.146112,0.101473,0.140483,0.005348,0.005348,0.158298,0.131821,0.142748,0.005348,0.005348,0.138806,0.139013,0.136865,0.15122,0.156171,0.135569,0.188069,0.138806,0.139013,0.136865,0.136865,0.161179,0.19195,0.122853,0.122853,0.122853,0.145654,0.122449,0.124581,0.122449,0.127223,0.14441,0.126273,0.126703,0.13943,0.122449,0.131449,0.123588,0.122449,0.122449,0.14441,0.126273,0.122449,0.12367,0.122449,0.122449,0.122449,0.126273,0.089942,0.107514,0.109929,0.109929,0.109929,0.109929,0.109929,0.101197,0.112864,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.306931,0.245059,0.28012,0.226277,0.210884,0.208054,0.178503,0.236341,0.209696,0.223289,0.222222,0.214039,0.196203,0.188259,0.207127,0.1875,0.223827,0.225182,0.167417,0.178332,0.208054,0.203947,0.225182,0.185259,0.168478,0.188641,0.201517,0.212087,0.214286,0.211364,0.21831,0.203501,0.254446,0.142311,0.206667,0.205298,0.227384,0.153974,0.160345,0.204171,0.194561,0.207589,0.20852,0.224367,0.188259,0.209932,0.164456,0.172222,0.172222,0.172222,0.172222,0.172222,0.171745,0.201954,0.193347,0.216531,0.203057,0.167719,0.164456,0.234257,0.197872,0.126962,0.201517,0.199357,0.183251,0.113139,0.126359,0.190379,0.189409,0.197034,0.207358,0.204171,0.205752,0.207127,0.201517,0.210646,0.206897,0.209932,0.125846,0.125846,0.125846,0.125846,0.198506,0.173669,0.199571,0.198718,0.184158,0.162587,0.205072,0.197034,0.197243,0.175472,0.162162,0.168326,0.208054,0.184891,0.124,0.144635,0.203724,0.210884,0.129346,0.179191,0.143408,0.143408,0.207127,0.205525,0.210407,0.143408,0.144747,0.206208,0.197662,0.201081,0.202835,0.206667,0.19893,0.258693,0.198294,0.198506,0.207358,0.210169,0.210407,0.266094,0.197662,0.197243,0.197662,0.209696,0.196825,0.198083,0.199143,0.198506,0.207358,0.197452,0.197243,0.207589,0.197662,0.20598,0.196825,0.198294,0.210169,0.208754,0.198718,0.196617,0.196617,0.197034,0.153846,0.211364,0.175803,0.200215,0.142857,0.192347,0.196617,0.192347,0.196617,0.192347,0.206208,0.159931,0.192547,0.196825,0.196617,0.136264,0.116395,0.145086,0.170799,0.16787,0.124665,0.122853,0.141553,0.124665,0.124665,0.159931,0.14486,0.124665,0.142311,0.144074,0.146688,0.1488,0.1609,0.124665,0.1609,0.142638,0.146688,0.146688,0.148325,0.169399,0.144074,0.118471,0.152584],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var21_df_closseness, by=list(var21_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var21_df_closseness, by=list(var21_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-54ee72415858308b9e41" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-54ee72415858308b9e41">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000118\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001115\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.8e-05\" data-max=\"0.000335\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000818\" data-max=\"0.00165\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.1e-05\" data-max=\"3e-04\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.021949\" data-max=\"0.024374\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.004095\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.207358\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.007038\" data-max=\"0.062409\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.152154\" data-max=\"0.306931\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00215\" data-max=\"0.055844\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003883\" data-max=\"0.139638\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[0.000125,0.000121,0.000125,0.000127,0.000125,0.000128,0.000127,0.000131,0.000127,0.000126,0.000127,0.000118,0.000124,0.000127,0.000127,0.000123,0.000127,0.000127,0.000129,0.000124,0.000128,0.000126,0.000126],[3e-06,4e-06,null,null,null,null,0,null,null,null,null,2.2e-05,1e-06,0,0,4e-06,null,null,null,1e-06,null,1e-06,0],[0.000803,0.000255,0.000728,0.000877,0.000737,0.000814,0.000731,0.001034,0.000775,0.000679,0.000784,0.000687,0.000845,0.000769,0.000794,0.000755,2.9e-05,0.000786,0.001115,0.000482,0.000792,0.000562,0.000734],[0.000208,0.000305,null,null,null,null,6.3e-05,null,null,null,null,0.000192,3.8e-05,9.3e-05,8.4e-05,0.000125,null,null,null,0.000335,null,0.000244,7.7e-05],[0.001061,0.000818,0.000959,0.001318,0.00096,0.001259,0.001132,0.001506,0.001211,0.000945,0.001129,0.000977,0.001064,0.001159,0.001152,0.001,0.00082,0.001134,0.00165,0.000868,0.001211,0.001063,0.001097],[0.0003,0.000134,null,null,null,null,1.1e-05,null,null,null,null,0.000197,7.4e-05,3.3e-05,4.5e-05,0.000188,null,null,null,8.2e-05,null,2.4e-05,5e-05],[0.023163,0.022584,0.02318,0.023658,0.023201,0.023764,0.023646,0.024374,0.023646,0.023402,0.023628,0.021949,0.023112,0.023587,0.023614,0.022941,0.023571,0.023565,0.024,0.022966,0.023797,0.023504,0.023487],[0.000597,0.000672,null,null,null,null,5.4e-05,null,null,null,null,0.004094,0.000207,3.3e-05,2.9e-05,0.000713,null,null,null,0.000262,null,0.000221,6e-05],[0.14944,0.047358,0.135371,0.163158,0.137067,0.151466,0.135873,0.192347,0.144074,0.126273,0.145882,0.127883,0.157186,0.143052,0.147669,0.140367,0.005348,0.146112,0.207358,0.089621,0.147268,0.104429,0.136602],[0.038763,0.056761,null,null,null,null,0.011632,null,null,null,null,0.035807,0.007038,0.017239,0.015561,0.02321,null,null,null,0.062409,null,0.045352,0.014259],[0.197288,0.152154,0.178332,0.245059,0.178503,0.234257,0.210502,0.28012,0.225182,0.175803,0.209932,0.181704,0.197924,0.215514,0.214255,0.185919,0.152584,0.210884,0.306931,0.161492,0.225182,0.197736,0.204081],[0.055844,0.024954,null,null,null,null,0.00215,null,null,null,null,0.0366,0.013668,0.006124,0.008409,0.034972,null,null,null,0.015289,null,0.00453,0.009303],[0.300585,0.099063,0.286154,0.343173,0.303922,0.323478,0.300896,0.391579,0.295238,0.29108,0.313659,0.266552,0.319611,0.301223,0.316849,0.295625,0.005348,0.306425,0.371257,0.194167,0.314189,0.234219,0.288627],[0.043014,0.125815,null,null,null,null,0.018146,null,null,null,null,0.070696,0.003883,0.033229,0.029748,0.023781,null,null,null,0.139638,null,0.102732,0.011577]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var21_df_closseness <- data.frame(
var21_incloseness,
var21_outcloseness,
var21_totalcloseness,
var21_incloseness_n,
var21_outcloseness_n,
var21_totalcloseness_n,
var21_centr_closeness) %>% round(6)

#Adding type
var21_df_closseness <-cbind(var21_df_closseness, V(var21)$TIPO1)

#Adding names
names(var21_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var21_df_closseness<-var21_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var21_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-20d6f24b4e2d07bdcc8e" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-20d6f24b4e2d07bdcc8e">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001115\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000608\" data-max=\"0.00165\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024374\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.207358\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.113139\" data-max=\"0.306931\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[0.000129,0.000127,0.000131,0.000127,0.000127,0.000127,0.000125,0.000129,0.000122,0.000129,0.000127,0.000127,0.000121,0.000123,0.000126,0.000127,0.000127,0.000128,0.000121,0.000125,0.000127,0.000124,0.000127,0.000117,0.000116,0.000126,0.00012,0.000127,0.000127,0.000127,0.000127,0.000126,0.000127,0.00012,0.000127,0.000123,0.000129,0.000122,0.000122,0.000127,0.000126,0.000125,0.000125,0.000129,0.000126,0.000127,0.000116,0.000123,0.000123,0.000123,0.000123,0.000123,0.000122,0.000125,0.00012,0.000127,0.000126,0.00012,0.000121,0.000128,0.00013,0.000118,0.000126,0.000126,0.000124,2.9e-05,0.000117,0.000127,0.000122,0.000126,0.000126,0.000127,0.000126,0.000127,0.000127,0.000126,0.000126,0.000127,0.000117,0.000117,0.000117,0.000117,0.000126,0.000124,0.000123,0.000126,0.000124,0.000115,0.000126,0.000126,0.000126,0.000117,0.000119,0.000124,0.000127,0.000123,0.000115,0.000122,0.000127,0.000127,0.000118,0.000124,0.000124,0.000124,0.000126,0.000127,0.000126,0.000124,0.000125,0.000126,0.000126,0.000127,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000127,0.000127,0.000126,0.000127,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000125,0.000126,0.000126,0.000126,0.000126,0.000126,0.000127,0.000126,0.000126,0.000127,0.000126,0.000126,0.000126,0.000126,0.000126,0.000124,0.000127,0.000126,0.000126,0.000121,0.000125,0.000126,0.000125,0.000126,0.000125,0.000127,0.000118,0.000128,0.000129,0.000129,0.000118,0.000118,0.000124,0.000128,0.000128,0.000118,0.000119,0.000123,0.000118,0.000118,0.000125,0.000123,0.000118,0.000123,0.000123,0.000124,0.000124,0.000125,0.000118,0.000125,0.000123,0.000123,0.000123,0.000124,0.000125,0.000123,0.00012,0.000127],[0.001115,0.000877,0.001034,0.000893,0.000774,0.000722,0.000737,0.000801,0.000882,0.00072,0.00081,0.000676,0.000854,0.000818,0.000812,0.000723,0.000886,0.000792,0.000692,0.000728,0.000765,0.000831,0.000775,0.000796,0.000718,0.000575,0.000798,0.000769,0.000841,0.00077,0.000805,0.000774,0.000989,0.000521,0.000579,0.000831,0.000814,0.000637,0.000692,0.000813,0.000843,0.000872,0.000871,0.000742,0.00073,0.000763,0.000639,0.000723,0.000723,0.000723,0.000723,0.000723,0.000711,0.000697,0.000817,0.000786,0.000771,0.000674,0.000601,0.000814,2.9e-05,0.000511,0.000755,0.000686,0.000752,2.9e-05,2.9e-05,0.000779,0.000706,0.000661,0.000752,0.000691,0.000751,0.00071,0.000684,0.000779,0.000734,0.000784,0.000551,0.000551,0.000551,0.000551,0.00073,0.000553,0.000786,0.000752,0.000759,0.000698,0.00077,0.000685,0.000739,0.000764,0.000677,0.000636,0.000825,0.000737,0.000504,0.000571,0.000749,0.000786,0.000546,0.000755,2.9e-05,2.9e-05,0.000851,0.000709,0.000767,2.9e-05,2.9e-05,0.000746,0.000747,0.000736,0.000813,0.00084,0.000729,0.001011,0.000746,0.000747,0.000736,0.000736,0.000867,0.001032,0.000661,0.000661,0.000661,0.000783,0.000658,0.00067,0.000658,0.000684,0.000776,0.000679,0.000681,0.00075,0.000658,0.000707,0.000664,0.000658,0.000658,0.000776,0.000679,0.000658,0.000665,0.000658,0.000658,0.000658,0.000679,0.000484,0.000578,0.000591,0.000591,0.000591,0.000591,0.000591,0.000544,0.000607,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.00165,0.001318,0.001506,0.001217,0.001134,0.001119,0.00096,0.001271,0.001127,0.0012,0.001195,0.001151,0.001055,0.001012,0.001114,0.001008,0.001203,0.001211,0.0009,0.000959,0.001119,0.001096,0.001211,0.000996,0.000906,0.001014,0.001083,0.00114,0.001152,0.001136,0.001174,0.001094,0.001368,0.000765,0.001111,0.001104,0.001222,0.000828,0.000862,0.001098,0.001046,0.001116,0.001121,0.001206,0.001012,0.001129,0.000884,0.000926,0.000926,0.000926,0.000926,0.000926,0.000923,0.001086,0.00104,0.001164,0.001092,0.000902,0.000884,0.001259,0.001064,0.000683,0.001083,0.001072,0.000985,0.000608,0.000679,0.001024,0.001018,0.001059,0.001115,0.001098,0.001106,0.001114,0.001083,0.001133,0.001112,0.001129,0.000677,0.000677,0.000677,0.000677,0.001067,0.000934,0.001073,0.001068,0.00099,0.000874,0.001103,0.001059,0.00106,0.000943,0.000872,0.000905,0.001119,0.000994,0.000667,0.000778,0.001095,0.001134,0.000695,0.000963,0.000771,0.000771,0.001114,0.001105,0.001131,0.000771,0.000778,0.001109,0.001063,0.001081,0.001091,0.001111,0.00107,0.001391,0.001066,0.001067,0.001115,0.00113,0.001131,0.001431,0.001063,0.00106,0.001063,0.001127,0.001058,0.001065,0.001071,0.001067,0.001115,0.001062,0.00106,0.001116,0.001063,0.001107,0.001058,0.001066,0.00113,0.001122,0.001068,0.001057,0.001057,0.001059,0.000827,0.001136,0.000945,0.001076,0.000768,0.001034,0.001057,0.001034,0.001057,0.001034,0.001109,0.00086,0.001035,0.001058,0.001057,0.000733,0.000626,0.00078,0.000918,0.000903,0.00067,0.000661,0.000761,0.00067,0.00067,0.00086,0.000779,0.00067,0.000765,0.000775,0.000789,0.0008,0.000865,0.00067,0.000865,0.000767,0.000789,0.000789,0.000797,0.000911,0.000775,0.000637,0.00082],[0.024,0.023658,0.024374,0.023562,0.023607,0.023589,0.023201,0.024012,0.022766,0.023969,0.023619,0.023649,0.022524,0.022966,0.023446,0.023544,0.023646,0.023797,0.022595,0.02318,0.023601,0.023131,0.023646,0.021831,0.021528,0.023491,0.022321,0.023631,0.023565,0.023553,0.023598,0.023461,0.023586,0.022348,0.023565,0.022845,0.023957,0.022677,0.022622,0.023664,0.023423,0.023259,0.023291,0.023969,0.023503,0.023607,0.02151,0.022833,0.022833,0.022833,0.022833,0.022833,0.022617,0.023244,0.022385,0.023568,0.023506,0.022391,0.022535,0.023764,0.02419,0.02197,0.023467,0.023464,0.023023,0.005348,0.021706,0.02355,0.022736,0.023435,0.023473,0.023568,0.023491,0.023538,0.023541,0.023458,0.023482,0.023628,0.021785,0.021785,0.021785,0.021785,0.023506,0.023023,0.022946,0.02352,0.02306,0.021431,0.023464,0.023435,0.023449,0.021788,0.022093,0.023152,0.023598,0.022943,0.021431,0.022783,0.023532,0.023565,0.021857,0.022994,0.023131,0.023131,0.023458,0.023559,0.023467,0.023131,0.023189,0.023485,0.023467,0.023547,0.023488,0.023479,0.02352,0.023461,0.023461,0.023461,0.023592,0.023634,0.023458,0.023577,0.023488,0.02347,0.023488,0.023476,0.02347,0.023479,0.023482,0.023265,0.023458,0.023479,0.023482,0.023473,0.023491,0.023598,0.023461,0.023461,0.023565,0.023449,0.023503,0.023452,0.023449,0.023458,0.02306,0.023706,0.023402,0.023476,0.02248,0.023244,0.023461,0.023244,0.023461,0.023244,0.023613,0.021962,0.0238,0.024019,0.024009,0.021913,0.021986,0.022997,0.023874,0.023828,0.021955,0.022182,0.022786,0.021955,0.021955,0.023224,0.022912,0.021955,0.022822,0.022901,0.022971,0.023014,0.023209,0.021955,0.023209,0.022814,0.022895,0.022895,0.023031,0.02332,0.022811,0.022305,0.023571],[0.207358,0.163158,0.192347,0.166071,0.143963,0.134296,0.137067,0.148919,0.164021,0.134006,0.150607,0.125676,0.158839,0.152209,0.151097,0.13449,0.164748,0.147268,0.12872,0.135371,0.142202,0.154613,0.144074,0.147971,0.133621,0.106897,0.148444,0.142967,0.156434,0.143187,0.149758,0.143963,0.183976,0.096825,0.107639,0.154613,0.151466,0.118396,0.12872,0.15122,0.15683,0.162162,0.162021,0.138085,0.135866,0.141985,0.118926,0.13449,0.13449,0.13449,0.13449,0.13449,0.13229,0.129707,0.151961,0.146112,0.143408,0.125337,0.111846,0.151466,0.005348,0.095092,0.140377,0.12766,0.139955,0.005376,0.005405,0.144973,0.131263,0.122935,0.139955,0.128453,0.139745,0.132102,0.127223,0.144973,0.136464,0.145882,0.102479,0.102479,0.102479,0.102479,0.135866,0.102819,0.146226,0.139955,0.14123,0.129888,0.143297,0.127485,0.137371,0.142093,0.125931,0.118245,0.153465,0.137168,0.09375,0.106286,0.139222,0.146112,0.101473,0.140483,0.005348,0.005348,0.158298,0.131821,0.142748,0.005348,0.005348,0.138806,0.139013,0.136865,0.15122,0.156171,0.135569,0.188069,0.138806,0.139013,0.136865,0.136865,0.161179,0.19195,0.122853,0.122853,0.122853,0.145654,0.122449,0.124581,0.122449,0.127223,0.14441,0.126273,0.126703,0.13943,0.122449,0.131449,0.123588,0.122449,0.122449,0.14441,0.126273,0.122449,0.12367,0.122449,0.122449,0.122449,0.126273,0.089942,0.107514,0.109929,0.109929,0.109929,0.109929,0.109929,0.101197,0.112864,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.306931,0.245059,0.28012,0.226277,0.210884,0.208054,0.178503,0.236341,0.209696,0.223289,0.222222,0.214039,0.196203,0.188259,0.207127,0.1875,0.223827,0.225182,0.167417,0.178332,0.208054,0.203947,0.225182,0.185259,0.168478,0.188641,0.201517,0.212087,0.214286,0.211364,0.21831,0.203501,0.254446,0.142311,0.206667,0.205298,0.227384,0.153974,0.160345,0.204171,0.194561,0.207589,0.20852,0.224367,0.188259,0.209932,0.164456,0.172222,0.172222,0.172222,0.172222,0.172222,0.171745,0.201954,0.193347,0.216531,0.203057,0.167719,0.164456,0.234257,0.197872,0.126962,0.201517,0.199357,0.183251,0.113139,0.126359,0.190379,0.189409,0.197034,0.207358,0.204171,0.205752,0.207127,0.201517,0.210646,0.206897,0.209932,0.125846,0.125846,0.125846,0.125846,0.198506,0.173669,0.199571,0.198718,0.184158,0.162587,0.205072,0.197034,0.197243,0.175472,0.162162,0.168326,0.208054,0.184891,0.124,0.144635,0.203724,0.210884,0.129346,0.179191,0.143408,0.143408,0.207127,0.205525,0.210407,0.143408,0.144747,0.206208,0.197662,0.201081,0.202835,0.206667,0.19893,0.258693,0.198294,0.198506,0.207358,0.210169,0.210407,0.266094,0.197662,0.197243,0.197662,0.209696,0.196825,0.198083,0.199143,0.198506,0.207358,0.197452,0.197243,0.207589,0.197662,0.20598,0.196825,0.198294,0.210169,0.208754,0.198718,0.196617,0.196617,0.197034,0.153846,0.211364,0.175803,0.200215,0.142857,0.192347,0.196617,0.192347,0.196617,0.192347,0.206208,0.159931,0.192547,0.196825,0.196617,0.136264,0.116395,0.145086,0.170799,0.16787,0.124665,0.122853,0.141553,0.124665,0.124665,0.159931,0.14486,0.124665,0.142311,0.144074,0.146688,0.1488,0.1609,0.124665,0.1609,0.142638,0.146688,0.146688,0.148325,0.169399,0.144074,0.118471,0.152584],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var21_df_closseness, by=list(var21_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var21_df_closseness, by=list(var21_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-064a6e11f52f2cfa2b0b" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-064a6e11f52f2cfa2b0b">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000121\" data-max=\"0.000126\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000444\" data-max=\"0.000689\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000209\" data-max=\"0.00034\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000885\" data-max=\"0.001086\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000125\" data-max=\"0.000176\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022498\" data-max=\"0.023463\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000253\" data-max=\"0.002078\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.082562\" data-max=\"0.128232\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.038954\" data-max=\"0.063179\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.164631\" data-max=\"0.201906\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.0232\" data-max=\"0.032704\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.173776\" data-max=\"0.2725\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.079091\" data-max=\"0.135432\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[0.000126,0.000121],[1e-06,1.1e-05],[0.000689,0.000444],[0.000209,0.00034],[0.001086,0.000885],[0.000125,0.000176],[0.023463,0.022498],[0.000253,0.002078],[0.128231,0.082562],[0.038954,0.063179],[0.201906,0.164631],[0.0232,0.032703],[0.2725,0.173776],[0.079091,0.135432]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var21_df_closseness <- data.frame(
var21_incloseness,
var21_outcloseness,
var21_totalcloseness,
var21_incloseness_n,
var21_outcloseness_n,
var21_totalcloseness_n,
var21_centr_closeness) %>% round(6)

#Adding type
var21_df_closseness <-cbind(var21_df_closseness, V(var21)$TIPO2)

#Adding names
names(var21_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var21_df_closseness<-var21_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var21_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-1987e63df1df99ae119a" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-1987e63df1df99ae119a">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001115\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000608\" data-max=\"0.00165\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024374\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.207358\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.113139\" data-max=\"0.306931\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[0.000129,0.000127,0.000131,0.000127,0.000127,0.000127,0.000125,0.000129,0.000122,0.000129,0.000127,0.000127,0.000121,0.000123,0.000126,0.000127,0.000127,0.000128,0.000121,0.000125,0.000127,0.000124,0.000127,0.000117,0.000116,0.000126,0.00012,0.000127,0.000127,0.000127,0.000127,0.000126,0.000127,0.00012,0.000127,0.000123,0.000129,0.000122,0.000122,0.000127,0.000126,0.000125,0.000125,0.000129,0.000126,0.000127,0.000116,0.000123,0.000123,0.000123,0.000123,0.000123,0.000122,0.000125,0.00012,0.000127,0.000126,0.00012,0.000121,0.000128,0.00013,0.000118,0.000126,0.000126,0.000124,2.9e-05,0.000117,0.000127,0.000122,0.000126,0.000126,0.000127,0.000126,0.000127,0.000127,0.000126,0.000126,0.000127,0.000117,0.000117,0.000117,0.000117,0.000126,0.000124,0.000123,0.000126,0.000124,0.000115,0.000126,0.000126,0.000126,0.000117,0.000119,0.000124,0.000127,0.000123,0.000115,0.000122,0.000127,0.000127,0.000118,0.000124,0.000124,0.000124,0.000126,0.000127,0.000126,0.000124,0.000125,0.000126,0.000126,0.000127,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000127,0.000127,0.000126,0.000127,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000125,0.000126,0.000126,0.000126,0.000126,0.000126,0.000127,0.000126,0.000126,0.000127,0.000126,0.000126,0.000126,0.000126,0.000126,0.000124,0.000127,0.000126,0.000126,0.000121,0.000125,0.000126,0.000125,0.000126,0.000125,0.000127,0.000118,0.000128,0.000129,0.000129,0.000118,0.000118,0.000124,0.000128,0.000128,0.000118,0.000119,0.000123,0.000118,0.000118,0.000125,0.000123,0.000118,0.000123,0.000123,0.000124,0.000124,0.000125,0.000118,0.000125,0.000123,0.000123,0.000123,0.000124,0.000125,0.000123,0.00012,0.000127],[0.001115,0.000877,0.001034,0.000893,0.000774,0.000722,0.000737,0.000801,0.000882,0.00072,0.00081,0.000676,0.000854,0.000818,0.000812,0.000723,0.000886,0.000792,0.000692,0.000728,0.000765,0.000831,0.000775,0.000796,0.000718,0.000575,0.000798,0.000769,0.000841,0.00077,0.000805,0.000774,0.000989,0.000521,0.000579,0.000831,0.000814,0.000637,0.000692,0.000813,0.000843,0.000872,0.000871,0.000742,0.00073,0.000763,0.000639,0.000723,0.000723,0.000723,0.000723,0.000723,0.000711,0.000697,0.000817,0.000786,0.000771,0.000674,0.000601,0.000814,2.9e-05,0.000511,0.000755,0.000686,0.000752,2.9e-05,2.9e-05,0.000779,0.000706,0.000661,0.000752,0.000691,0.000751,0.00071,0.000684,0.000779,0.000734,0.000784,0.000551,0.000551,0.000551,0.000551,0.00073,0.000553,0.000786,0.000752,0.000759,0.000698,0.00077,0.000685,0.000739,0.000764,0.000677,0.000636,0.000825,0.000737,0.000504,0.000571,0.000749,0.000786,0.000546,0.000755,2.9e-05,2.9e-05,0.000851,0.000709,0.000767,2.9e-05,2.9e-05,0.000746,0.000747,0.000736,0.000813,0.00084,0.000729,0.001011,0.000746,0.000747,0.000736,0.000736,0.000867,0.001032,0.000661,0.000661,0.000661,0.000783,0.000658,0.00067,0.000658,0.000684,0.000776,0.000679,0.000681,0.00075,0.000658,0.000707,0.000664,0.000658,0.000658,0.000776,0.000679,0.000658,0.000665,0.000658,0.000658,0.000658,0.000679,0.000484,0.000578,0.000591,0.000591,0.000591,0.000591,0.000591,0.000544,0.000607,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.00165,0.001318,0.001506,0.001217,0.001134,0.001119,0.00096,0.001271,0.001127,0.0012,0.001195,0.001151,0.001055,0.001012,0.001114,0.001008,0.001203,0.001211,0.0009,0.000959,0.001119,0.001096,0.001211,0.000996,0.000906,0.001014,0.001083,0.00114,0.001152,0.001136,0.001174,0.001094,0.001368,0.000765,0.001111,0.001104,0.001222,0.000828,0.000862,0.001098,0.001046,0.001116,0.001121,0.001206,0.001012,0.001129,0.000884,0.000926,0.000926,0.000926,0.000926,0.000926,0.000923,0.001086,0.00104,0.001164,0.001092,0.000902,0.000884,0.001259,0.001064,0.000683,0.001083,0.001072,0.000985,0.000608,0.000679,0.001024,0.001018,0.001059,0.001115,0.001098,0.001106,0.001114,0.001083,0.001133,0.001112,0.001129,0.000677,0.000677,0.000677,0.000677,0.001067,0.000934,0.001073,0.001068,0.00099,0.000874,0.001103,0.001059,0.00106,0.000943,0.000872,0.000905,0.001119,0.000994,0.000667,0.000778,0.001095,0.001134,0.000695,0.000963,0.000771,0.000771,0.001114,0.001105,0.001131,0.000771,0.000778,0.001109,0.001063,0.001081,0.001091,0.001111,0.00107,0.001391,0.001066,0.001067,0.001115,0.00113,0.001131,0.001431,0.001063,0.00106,0.001063,0.001127,0.001058,0.001065,0.001071,0.001067,0.001115,0.001062,0.00106,0.001116,0.001063,0.001107,0.001058,0.001066,0.00113,0.001122,0.001068,0.001057,0.001057,0.001059,0.000827,0.001136,0.000945,0.001076,0.000768,0.001034,0.001057,0.001034,0.001057,0.001034,0.001109,0.00086,0.001035,0.001058,0.001057,0.000733,0.000626,0.00078,0.000918,0.000903,0.00067,0.000661,0.000761,0.00067,0.00067,0.00086,0.000779,0.00067,0.000765,0.000775,0.000789,0.0008,0.000865,0.00067,0.000865,0.000767,0.000789,0.000789,0.000797,0.000911,0.000775,0.000637,0.00082],[0.024,0.023658,0.024374,0.023562,0.023607,0.023589,0.023201,0.024012,0.022766,0.023969,0.023619,0.023649,0.022524,0.022966,0.023446,0.023544,0.023646,0.023797,0.022595,0.02318,0.023601,0.023131,0.023646,0.021831,0.021528,0.023491,0.022321,0.023631,0.023565,0.023553,0.023598,0.023461,0.023586,0.022348,0.023565,0.022845,0.023957,0.022677,0.022622,0.023664,0.023423,0.023259,0.023291,0.023969,0.023503,0.023607,0.02151,0.022833,0.022833,0.022833,0.022833,0.022833,0.022617,0.023244,0.022385,0.023568,0.023506,0.022391,0.022535,0.023764,0.02419,0.02197,0.023467,0.023464,0.023023,0.005348,0.021706,0.02355,0.022736,0.023435,0.023473,0.023568,0.023491,0.023538,0.023541,0.023458,0.023482,0.023628,0.021785,0.021785,0.021785,0.021785,0.023506,0.023023,0.022946,0.02352,0.02306,0.021431,0.023464,0.023435,0.023449,0.021788,0.022093,0.023152,0.023598,0.022943,0.021431,0.022783,0.023532,0.023565,0.021857,0.022994,0.023131,0.023131,0.023458,0.023559,0.023467,0.023131,0.023189,0.023485,0.023467,0.023547,0.023488,0.023479,0.02352,0.023461,0.023461,0.023461,0.023592,0.023634,0.023458,0.023577,0.023488,0.02347,0.023488,0.023476,0.02347,0.023479,0.023482,0.023265,0.023458,0.023479,0.023482,0.023473,0.023491,0.023598,0.023461,0.023461,0.023565,0.023449,0.023503,0.023452,0.023449,0.023458,0.02306,0.023706,0.023402,0.023476,0.02248,0.023244,0.023461,0.023244,0.023461,0.023244,0.023613,0.021962,0.0238,0.024019,0.024009,0.021913,0.021986,0.022997,0.023874,0.023828,0.021955,0.022182,0.022786,0.021955,0.021955,0.023224,0.022912,0.021955,0.022822,0.022901,0.022971,0.023014,0.023209,0.021955,0.023209,0.022814,0.022895,0.022895,0.023031,0.02332,0.022811,0.022305,0.023571],[0.207358,0.163158,0.192347,0.166071,0.143963,0.134296,0.137067,0.148919,0.164021,0.134006,0.150607,0.125676,0.158839,0.152209,0.151097,0.13449,0.164748,0.147268,0.12872,0.135371,0.142202,0.154613,0.144074,0.147971,0.133621,0.106897,0.148444,0.142967,0.156434,0.143187,0.149758,0.143963,0.183976,0.096825,0.107639,0.154613,0.151466,0.118396,0.12872,0.15122,0.15683,0.162162,0.162021,0.138085,0.135866,0.141985,0.118926,0.13449,0.13449,0.13449,0.13449,0.13449,0.13229,0.129707,0.151961,0.146112,0.143408,0.125337,0.111846,0.151466,0.005348,0.095092,0.140377,0.12766,0.139955,0.005376,0.005405,0.144973,0.131263,0.122935,0.139955,0.128453,0.139745,0.132102,0.127223,0.144973,0.136464,0.145882,0.102479,0.102479,0.102479,0.102479,0.135866,0.102819,0.146226,0.139955,0.14123,0.129888,0.143297,0.127485,0.137371,0.142093,0.125931,0.118245,0.153465,0.137168,0.09375,0.106286,0.139222,0.146112,0.101473,0.140483,0.005348,0.005348,0.158298,0.131821,0.142748,0.005348,0.005348,0.138806,0.139013,0.136865,0.15122,0.156171,0.135569,0.188069,0.138806,0.139013,0.136865,0.136865,0.161179,0.19195,0.122853,0.122853,0.122853,0.145654,0.122449,0.124581,0.122449,0.127223,0.14441,0.126273,0.126703,0.13943,0.122449,0.131449,0.123588,0.122449,0.122449,0.14441,0.126273,0.122449,0.12367,0.122449,0.122449,0.122449,0.126273,0.089942,0.107514,0.109929,0.109929,0.109929,0.109929,0.109929,0.101197,0.112864,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.306931,0.245059,0.28012,0.226277,0.210884,0.208054,0.178503,0.236341,0.209696,0.223289,0.222222,0.214039,0.196203,0.188259,0.207127,0.1875,0.223827,0.225182,0.167417,0.178332,0.208054,0.203947,0.225182,0.185259,0.168478,0.188641,0.201517,0.212087,0.214286,0.211364,0.21831,0.203501,0.254446,0.142311,0.206667,0.205298,0.227384,0.153974,0.160345,0.204171,0.194561,0.207589,0.20852,0.224367,0.188259,0.209932,0.164456,0.172222,0.172222,0.172222,0.172222,0.172222,0.171745,0.201954,0.193347,0.216531,0.203057,0.167719,0.164456,0.234257,0.197872,0.126962,0.201517,0.199357,0.183251,0.113139,0.126359,0.190379,0.189409,0.197034,0.207358,0.204171,0.205752,0.207127,0.201517,0.210646,0.206897,0.209932,0.125846,0.125846,0.125846,0.125846,0.198506,0.173669,0.199571,0.198718,0.184158,0.162587,0.205072,0.197034,0.197243,0.175472,0.162162,0.168326,0.208054,0.184891,0.124,0.144635,0.203724,0.210884,0.129346,0.179191,0.143408,0.143408,0.207127,0.205525,0.210407,0.143408,0.144747,0.206208,0.197662,0.201081,0.202835,0.206667,0.19893,0.258693,0.198294,0.198506,0.207358,0.210169,0.210407,0.266094,0.197662,0.197243,0.197662,0.209696,0.196825,0.198083,0.199143,0.198506,0.207358,0.197452,0.197243,0.207589,0.197662,0.20598,0.196825,0.198294,0.210169,0.208754,0.198718,0.196617,0.196617,0.197034,0.153846,0.211364,0.175803,0.200215,0.142857,0.192347,0.196617,0.192347,0.196617,0.192347,0.206208,0.159931,0.192547,0.196825,0.196617,0.136264,0.116395,0.145086,0.170799,0.16787,0.124665,0.122853,0.141553,0.124665,0.124665,0.159931,0.14486,0.124665,0.142311,0.144074,0.146688,0.1488,0.1609,0.124665,0.1609,0.142638,0.146688,0.146688,0.148325,0.169399,0.144074,0.118471,0.152584],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var21_df_closseness, by=list(var21_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var21_df_closseness, by=list(var21_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-a0330ed7c408a3548382" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-a0330ed7c408a3548382">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000121\" data-max=\"0.000127\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000442\" data-max=\"0.000795\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"9.6e-05\" data-max=\"0.000342\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000884\" data-max=\"0.001162\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7.1e-05\" data-max=\"0.000177\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022483\" data-max=\"0.023584\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.4e-05\" data-max=\"0.002088\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.082205\" data-max=\"0.147811\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.017846\" data-max=\"0.063624\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.164457\" data-max=\"0.216089\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.013167\" data-max=\"0.032847\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172406\" data-max=\"0.308329\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.029295\" data-max=\"0.135782\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[0.000127,0.000126,0.000121],[0,1e-06,1.1e-05],[0.000795,0.000672,0.000442],[9.6e-05,0.000216,0.000342],[0.001162,0.001072,0.000884],[7.1e-05,0.000127,0.000177],[0.023584,0.023446,0.022483],[5.4e-05,0.000265,0.002088],[0.147811,0.124916,0.082205],[0.017846,0.040201,0.063624],[0.216089,0.19939,0.164457],[0.013167,0.023708,0.032847],[0.308329,0.266869,0.172406],[0.029295,0.082605,0.135782]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var21_data.RData")
```

