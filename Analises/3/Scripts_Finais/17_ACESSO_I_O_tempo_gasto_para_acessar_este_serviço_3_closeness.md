# SNA Closeness 17_ACESSO I) O tempo gasto para acessar este serviço impede a realização ativades em conjunto. (var15)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 17_ACESSO I) O tempo gasto para acessar este serviço impede a realização ativades em conjunto. (var15)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var15_data.RData")
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
#var15<-simplify(var15) #Simplify
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
V(var15)$incloseness <- closeness(var15, mode = "in", weights = E(var15)$var15) %>% round(6)
V(var15)$outcloseness <- closeness(var15, mode = "out", weights = E(var15)$var15) %>% round(6)
V(var15)$totalcloseness <- closeness(var15, mode = "total", weights = E(var15)$var15) %>% round(4)
```

###Saving to Environment

```r
var15_incloseness<- closeness(var15, mode = "in", weights = E(var15)$var15) %>% round(6)
var15_outcloseness<- closeness(var15, mode = "out", weights = E(var15)$var15) %>% round(6)
var15_totalcloseness<- closeness(var15, mode = "total", weights = E(var15)$var15) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var15_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0001310 0.0001320 0.0001311 0.0001330 0.0001360
```

```r
sd(var15_incloseness)
```

```
## [1] 7.765747e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var15)$incloseness<-closeness(var15, weights = E(var15)$var15, mode="in")

#Get Variable
V(var15)$var15_color_degree<-round(V(var15)$incloseness,6)

#Creating brewer pallette
vertex_var15_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var15)$var15_color_degree)), "RdBu"))(
            length(unique(V(var15)$var15_color_degree)))

#Saving as Vertex properties 
V(var15)$vertex_var15_color_degree<-
  vertex_var15_color_degree[as.numeric(
  cut(V(var15)$var15_color_degree,
      breaks=length(unique(V(var15)$var15_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var15, es=E(var15), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var15))
maxC <- rep(Inf, vcount(var15))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var15, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var15)$weight)


#PLotting
plot(var15, 
     layout=co,
     edge.color=V(var15)$vertex_var15_color_degree[edge.start],
     edge.arrow.size=closeness(var15, weights = E(var15)$var15, mode="in"),
     edge.width=E(var15)$weight/mean(E(var15)$weight),
     edge.curved = TRUE,
     vertex.color=V(var15)$vertex_var15_color_degree,
     vertex.size=closeness(var15, weights = E(var15)$var15, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var15,"LABEL_COR"),
     vertex.label.cex=(closeness(var15, weights = E(var15)$var15, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var15)$var15_color_degree
b<-V(var15)$vertex_var15_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 17_ACESSO I) O tempo gasto para acessar este serviço impede a realização ativades em conjunto. (var15)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var15, mode="in", weights = E(var15)$var15)), 
             sd(closeness(var15, mode="in", weights = E(var15)$var15))
             )
       )
```

![](17_ACESSO_I_O_tempo_gasto_para_acessar_este_serviço_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var15_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0009390 0.0010700 0.0009938 0.0014600 0.0019880
```

```r
sd(var15_outcloseness)
```

```
## [1] 0.0005423761
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var15)$outcloseness<-closeness(var15, weights = E(var15)$var15, mode="out")

#Get Variable
V(var15)$var15_color_degree<-round(V(var15)$outcloseness,6)

#Creating brewer pallette
vertex_var15_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var15)$var15_color_degree)), "RdBu"))(
            length(unique(V(var15)$var15_color_degree)))

#Saving as Vertex properties 
V(var15)$vertex_var15_color_degree<-
  vertex_var15_color_degree[as.numeric(
  cut(V(var15)$var15_color_degree,
      breaks=length(unique(V(var15)$var15_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var15, es=E(var15), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var15))
maxC <- rep(Inf, vcount(var15))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var15, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var15)$weight)


#PLotting
plot(var15, 
     layout=co,
     edge.color=V(var15)$vertex_var15_color_degree[edge.start],
     edge.arrow.size=closeness(var15, weights = E(var15)$var15, mode="out"),
     edge.width=E(var15)$weight/2*mean(E(var15)$weight),
     edge.curved = TRUE,
     vertex.color=V(var15)$vertex_var15_color_degree,
     vertex.size=closeness(var15, weights = E(var15)$var15, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var15,"LABEL_COR"),
     vertex.label.cex=closeness(var15, weights = E(var15)$var15, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var15)$var15_color_degree
b<-V(var15)$vertex_var15_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 17_ACESSO I) O tempo gasto para acessar este serviço impede a realização ativades em conjunto. (var15)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var15, mode="out", weights = E(var15)$var15)), 
             sd(closeness(var15, mode="out", weights = E(var15)$var15))
             )
       )
```

![](17_ACESSO_I_O_tempo_gasto_para_acessar_este_serviço_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var15_totalcloseness)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000913 0.001936 0.002232 0.002163 0.002336 0.003623
```

```r
sd(var15_totalcloseness)
```

```
## [1] 0.0003198208
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var15)$allcloseness<-closeness(var15, weights = E(var15)$var15, mode="all")

#Get Variable
V(var15)$var15_color_degree<-round(V(var15)$allcloseness,6)

#Creating brewer pallette
vertex_var15_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var15)$var15_color_degree)), "RdBu"))(
            length(unique(V(var15)$var15_color_degree)))

#Saving as Vertex properties 
V(var15)$vertex_var15_color_degree<-
  vertex_var15_color_degree[as.numeric(
  cut(V(var15)$var15_color_degree,
      breaks=length(unique(V(var15)$var15_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var15, es=E(var15), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var15))
maxC <- rep(Inf, vcount(var15))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var15, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var15)$weight)


#PLotting
plot(var15, 
     layout=co,
     edge.color=V(var15)$vertex_var15_color_degree[edge.start],
     edge.arrow.size=closeness(var15, weights = E(var15)$var15, mode="all"),
     edge.width=E(var15)$weight/2*mean(E(var15)$weight),
     edge.curved = TRUE,
     vertex.color=V(var15)$vertex_var15_color_degree,
     vertex.size=closeness(var15, weights = E(var15)$var15, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var15,"LABEL_COR"),
     vertex.label.cex=(closeness(var15, weights = E(var15)$var15, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var15)$var15_color_degree
b<-V(var15)$vertex_var15_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 17_ACESSO I) O tempo gasto para acessar este serviço impede a realização ativades em conjunto. (var15)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var15, mode="all", weights = E(var15)$var15)), 
             sd(closeness(var15, mode="all", weights = E(var15)$var15))
             )
       )
```

![](17_ACESSO_I_O_tempo_gasto_para_acessar_este_serviço_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var15)$incloseness_n <- closeness(var15, mode = "in",, weights = E(var15)$var15, normalized = T) %>% round(10)
V(var15)$outcloseness_n <- closeness(var15, mode = "out", normalized = T, weights = E(var15)$var15) %>% round(6)
V(var15)$totalcloseness_n <- closeness(var15, mode = "total", normalized = T, weights = E(var15)$var15) %>% round(6)
```

###Saving to Environment

```r
var15_incloseness_n<- closeness(var15, mode = "in", normalized = T, weights = E(var15)$var15) %>% round(6)
var15_outcloseness_n<- closeness(var15, mode = "out", normalized = T, weights = E(var15)$var15) %>% round(6)
var15_totalcloseness_n<- closeness(var15, mode = "total", normalized = T, weights = E(var15)$var15) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var15_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.024380 0.024620 0.024400 0.024670 0.025300
```

```r
sd(var15_incloseness_n)
```

```
## [1] 0.001449116
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var15)$incloseness_n<-closeness(var15, weights = E(var15)$var15, mode="in", normalized = T)

#Get Variable
V(var15)$var15_color_degree<-round(V(var15)$incloseness_n,6)

#Creating brewer pallette
vertex_var15_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var15)$var15_color_degree)), "RdBu"))(
            length(unique(V(var15)$var15_color_degree)))

#Saving as Vertex properties 
V(var15)$vertex_var15_color_degree<-
  vertex_var15_color_degree[as.numeric(
  cut(V(var15)$var15_color_degree,
      breaks=length(unique(V(var15)$var15_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var15, es=E(var15), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var15))
maxC <- rep(Inf, vcount(var15))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var15, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var15)$weight)


#PLotting
plot(var15, 
     layout=co,
     edge.color=V(var15)$vertex_var15_color_degree[edge.start],
     edge.arrow.size=closeness(var15, weights = E(var15)$var15, mode="in",normalized = T),
     edge.width=E(var15)$weight/10*mean(E(var15)$weight),
     edge.curved = TRUE,
     vertex.color=V(var15)$vertex_var15_color_degree,
     vertex.size=(closeness(var15, weights = E(var15)$var15, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var15,"LABEL_COR"),
     vertex.label.cex=closeness(var15, weights = E(var15)$var15, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var15)$var15_color_degree
b<-V(var15)$vertex_var15_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 17_ACESSO I) O tempo gasto para acessar este serviço impede a realização ativades em conjunto. (var15)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var15, mode="in", weights = E(var15)$var15, normalized = T)), 
             sd(closeness(var15, mode="in", weights = E(var15)$var15, normalized = T))
             )
       )
```

![](17_ACESSO_I_O_tempo_gasto_para_acessar_este_serviço_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var15_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.174700 0.198900 0.184800 0.271500 0.369800
```

```r
sd(var15_outcloseness_n)
```

```
## [1] 0.1008924
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var15)$outcloseness_n<-closeness(var15, weights = E(var15)$var15, mode="out", normalized = T)

#Get Variable
V(var15)$var15_color_degree<-round(V(var15)$outcloseness_n,6)

#Creating brewer pallette
vertex_var15_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var15)$var15_color_degree)), "RdBu"))(
            length(unique(V(var15)$var15_color_degree)))

#Saving as Vertex properties 
V(var15)$vertex_var15_color_degree<-
  vertex_var15_color_degree[as.numeric(
  cut(V(var15)$var15_color_degree,
      breaks=length(unique(V(var15)$var15_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var15, es=E(var15), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var15))
maxC <- rep(Inf, vcount(var15))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var15, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var15)$weight)


#PLotting
plot(var15, 
     layout=co,
     edge.color=V(var15)$vertex_var15_color_degree[edge.start],
     edge.arrow.size=closeness(var15, weights = E(var15)$var15, mode="out",normalized = T),
     edge.width=E(var15)$weight/10*mean(E(var15)$weight),
     edge.curved = TRUE,
     vertex.color=V(var15)$vertex_var15_color_degree,
     vertex.size=(closeness(var15, weights = E(var15)$var15, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var15,"LABEL_COR"),
     vertex.label.cex=closeness(var15, weights = E(var15)$var15, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var15)$var15_color_degree
b<-V(var15)$vertex_var15_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 17_ACESSO I) O tempo gasto para acessar este serviço impede a realização ativades em conjunto. (var15)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var15, mode="out", weights = E(var15)$var15, normalized = T)), 
             sd(closeness(var15, mode="out", weights = E(var15)$var15, normalized = T))
             )
       )
```

![](17_ACESSO_I_O_tempo_gasto_para_acessar_este_serviço_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var15_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1699  0.3601  0.4152  0.4023  0.4346  0.6739
```

```r
sd(var15_totalcloseness_n)
```

```
## [1] 0.05949423
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var15)$allcloseness_n<-closeness(var15, weights = E(var15)$var15, mode="all", normalized = T)

#Get Variable
V(var15)$var15_color_degree<-round(V(var15)$allcloseness_n,6)

#Creating brewer pallette
vertex_var15_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var15)$var15_color_degree)), "RdBu"))(
            length(unique(V(var15)$var15_color_degree)))

#Saving as Vertex properties 
V(var15)$vertex_var15_color_degree<-
  vertex_var15_color_degree[as.numeric(
  cut(V(var15)$var15_color_degree,
      breaks=length(unique(V(var15)$var15_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var15, es=E(var15), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var15))
maxC <- rep(Inf, vcount(var15))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var15, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var15)$weight)


#PLotting
plot(var15, 
     layout=co,
     edge.color=V(var15)$vertex_var15_color_degree[edge.start],
     edge.arrow.size=closeness(var15, weights = E(var15)$var15, mode="all",normalized = T),
     edge.width=E(var15)$weight/10*mean(E(var15)$weight),
     edge.curved = TRUE,
     vertex.color=V(var15)$vertex_var15_color_degree,
     vertex.size=(closeness(var15, weights = E(var15)$var15, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var15,"LABEL_COR"),
     vertex.label.cex=closeness(var15, weights = E(var15)$var15, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var15)$var15_color_degree
b<-V(var15)$vertex_var15_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 17_ACESSO I) O tempo gasto para acessar este serviço impede a realização ativades em conjunto. (var15)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var15, mode="all", weights = E(var15)$var15, normalized = T)), 
             sd(closeness(var15, mode="all", weights = E(var15)$var15, normalized = T))
             )
       )
```

![](17_ACESSO_I_O_tempo_gasto_para_acessar_este_serviço_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var15)$incloseness_n <- closeness(var15, weights = E(var15)$var15, mode = "in", normalized = T) %>% round(6)
V(var15)$outcloseness_n <- closeness(var15, weights = E(var15)$var15, mode = "out", normalized = T) %>% round(6)
V(var15)$totalcloseness_n <- closeness(var15, weights = E(var15)$var15, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var15)$var15_centr_closeness<- centralization.closeness(var15)$res
var15_centr_closeness<- centralization.closeness(var15)$res
var15_centr_closeness_all<- centralization.closeness(var15)
```

###Centralization

```r
var15_centr_closeness_all$centralization
```

```
## [1] 0.1625197
```

###Theoretical Max

```r
var15_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var15)$var15_centr_closeness<- centralization.closeness(var15)$res

#Get Variable
V(var15)$var15_color_degree<-round(V(var15)$var15_centr_closeness,6)

#Creating brewer pallette
vertex_var15_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var15)$var15_color_degree)), "Spectral"))(
            length(unique(V(var15)$var15_color_degree)))

#Saving as Vertex properties 
V(var15)$vertex_var15_color_degree<-
  vertex_var15_color_degree[as.numeric(
  cut(V(var15)$var15_color_degree,
      breaks=length(unique(V(var15)$var15_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var15, es=E(var15), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var15))
maxC <- rep(Inf, vcount(var15))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var15, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var15)$weight)


#PLotting
plot(var15, 
     layout=co,
     edge.color=V(var15)$vertex_var15_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var15)$res,
     edge.width=E(var15)$weight/10*mean(E(var15)$weight),
     edge.curved = TRUE,
     vertex.color=V(var15)$vertex_var15_color_degree,
     vertex.size=centralization.closeness(var15)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var15,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var15)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var15)$var15_color_degree
b<-V(var15)$vertex_var15_color_degree
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
  title("Network Centralization Closeness - 17_ACESSO I) O tempo gasto para acessar este serviço impede a realização ativades em conjunto. (var15)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var15)$res), 
             sd(centralization.closeness(var15)$res)
             )
       )
```

![](17_ACESSO_I_O_tempo_gasto_para_acessar_este_serviço_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var15_incloseness<- closeness(var15, weights = E(var15)$var15, mode = "in") %>% round(6)
var15_outcloseness<- closeness(var15, weights = E(var15)$var15, mode = "out") %>% round(6)
var15_totalcloseness<- closeness(var15, weights = E(var15)$var15, mode = "total") %>% round(6)
var15_incloseness_n<- closeness(var15,weights = E(var15)$var15, mode = "in", normalized = T) %>% round(6)
var15_outcloseness_n<- closeness(var15,weights = E(var15)$var15, mode = "out", normalized = T) %>% round(6)
var15_totalcloseness_n<- closeness(var15,weights = E(var15)$var15, mode = "total", normalized = T) %>% round(6)
var15_centr_closeness <- centralization.closeness(var15)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var15_df_closseness <- data.frame(
var15_incloseness,
var15_outcloseness,
var15_totalcloseness,
var15_incloseness_n,
var15_outcloseness_n,
var15_totalcloseness_n,
var15_centr_closeness) %>% round(6)

#Adding type
var15_df_closseness <-cbind(var15_df_closseness, V(var15)$LABEL_COR)

#Adding names
names(var15_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var15_df_closseness<-var15_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var15_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-7b73171a12e12a94c418" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-7b73171a12e12a94c418">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000136\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001989\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000913\" data-max=\"0.003623\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.025299\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.369781\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.169863\" data-max=\"0.673913\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[0.000134,0.000133,0.000135,0.000133,0.000133,0.000133,0.000132,0.000133,0.000128,0.000132,0.000133,0.000133,0.00013,0.000131,0.000132,0.000132,0.000133,0.000133,0.000131,0.000132,0.000133,0.000126,0.000133,0.000128,0.000129,0.000132,0.000128,0.000133,0.000133,0.000133,0.000133,0.000132,0.000133,0.00013,0.000133,0.000128,0.000132,0.000131,0.000131,0.000133,0.000133,0.000132,0.000132,0.000132,0.000132,0.000133,0.000128,0.000131,0.000131,0.000131,0.000131,0.000131,0.000124,0.000132,0.00013,0.000133,0.000133,0.00013,0.000128,0.000133,0.000136,0.000125,0.000132,0.000132,0.000131,2.9e-05,0.000131,0.000132,0.00013,0.000132,0.000132,0.000132,0.000133,0.000133,0.000133,0.000132,0.000132,0.000133,0.000129,0.000129,0.000129,0.000129,0.000133,0.000131,0.000131,0.000132,0.000131,0.000125,0.000132,0.000132,0.000132,0.000128,0.000125,0.000132,0.000133,0.000131,0.000125,0.000126,0.000132,0.000133,0.000129,0.000131,0.000134,0.000134,0.000132,0.000132,0.000132,0.000134,0.000134,0.000132,0.000133,0.000132,0.000133,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000133,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000131,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000133,0.000132,0.000132,0.000132,0.000132,0.000133,0.000133,0.000132,0.000131,0.000131,0.000132,0.000131,0.000132,0.000131,0.000133,0.000129,0.000134,0.000136,0.000136,0.000129,0.000132,0.000133,0.000134,0.000134,0.000131,0.000134,0.000133,0.000131,0.000131,0.000133,0.000133,0.000131,0.000133,0.000134,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000134,0.000133,0.000133,0.000126,0.000135],[0.001397,0.001767,0.001988,0.001558,0.001543,0.001524,0.00156,0.001258,0.001218,0.001266,0.001255,0.001473,0.001319,0.001626,0.001479,0.000999,0.001764,0.001572,0.001508,0.001295,0.001548,0.001163,0.001502,0.001718,0.001087,0.001031,0.000926,0.001011,0.001468,0.0016,0.00156,0.001517,0.00155,0.00096,0.000962,0.001163,0.00099,0.001235,0.001534,0.001637,0.001585,0.001621,0.001621,0.001477,0.001379,0.001181,0.000818,0.001272,0.001272,0.001272,0.001272,0.000999,0.001185,0.001017,0.00123,0.001541,0.000973,0.000972,0.00119,0.001026,2.9e-05,0.001176,0.001529,0.00096,0.000976,2.9e-05,2.9e-05,0.001403,0.001397,0.00146,0.00146,0.001513,0.000975,0.000974,0.000959,0.001159,0.000954,0.001553,0.000835,0.000835,0.000835,0.000835,0.001166,0.001305,0.000984,0.000973,0.001475,0.001125,0.001558,0.001475,0.001227,0.001527,0.001495,0.000831,0.001159,0.001115,0.000932,0.00096,0.000957,0.001548,0.001182,0.001244,2.9e-05,2.9e-05,0.000956,0.001534,0.000948,2.9e-05,2.9e-05,0.001221,0.001205,0.001477,0.001464,0.000947,0.001466,0.000949,0.000961,0.001493,0.000951,0.000951,0.001508,0.001473,0.000948,0.000948,0.000948,0.000966,0.000946,0.001468,0.000946,0.000966,0.000965,0.000965,0.00146,0.001456,0.001456,0.001522,0.000947,0.001456,0.001147,0.000965,0.000947,0.000946,0.001458,0.001456,0.001456,0.000946,0.001502,0.000808,0.001279,0.00107,0.00107,0.00107,0.00107,0.00107,0.000967,0.001181,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.002674,0.002915,0.003623,0.002451,0.002545,0.002475,0.002457,0.002451,0.001675,0.002304,0.002294,0.002445,0.001802,0.002558,0.002232,0.002268,0.002882,0.002571,0.002358,0.002415,0.002488,0.001972,0.002358,0.00271,0.001715,0.00237,0.00188,0.002331,0.002353,0.002469,0.002433,0.00237,0.0025,0.001905,0.002257,0.001996,0.002198,0.002004,0.002331,0.002571,0.002545,0.002519,0.002538,0.002347,0.002294,0.002309,0.001876,0.002169,0.002169,0.002169,0.002169,0.001969,0.001828,0.002174,0.002262,0.002632,0.002227,0.001942,0.002092,0.002611,0.002618,0.001799,0.002404,0.002208,0.001852,0.001255,0.001603,0.002358,0.002119,0.002227,0.002309,0.002326,0.002494,0.002488,0.002358,0.002217,0.002392,0.002519,0.001534,0.001534,0.001534,0.001534,0.002315,0.002309,0.002128,0.002257,0.002398,0.001799,0.002421,0.002208,0.002273,0.002326,0.002237,0.002232,0.002203,0.002146,0.001357,0.001471,0.002294,0.002494,0.001773,0.002008,0.001938,0.001938,0.002208,0.002309,0.002222,0.001938,0.001938,0.002278,0.002283,0.002331,0.002342,0.002299,0.002309,0.002208,0.002203,0.002288,0.002198,0.002252,0.002342,0.002364,0.002273,0.002262,0.002273,0.002283,0.002257,0.002278,0.002299,0.001965,0.002252,0.002268,0.002278,0.002262,0.002278,0.002288,0.002198,0.002198,0.002262,0.002212,0.002237,0.002227,0.002232,0.002193,0.002212,0.002392,0.002353,0.002222,0.001845,0.001894,0.002252,0.001894,0.002252,0.001894,0.002488,0.002045,0.001934,0.002257,0.002252,0.001815,0.001645,0.001965,0.001634,0.001626,0.001866,0.001597,0.001876,0.001866,0.001866,0.001919,0.001927,0.001866,0.001919,0.001927,0.001866,0.001603,0.001869,0.001866,0.001869,0.001866,0.001873,0.001873,0.002066,0.001953,0.001942,0.000913,0.002169],[0.02492,0.024727,0.02504,0.024668,0.024711,0.024685,0.024613,0.024646,0.023737,0.024603,0.024688,0.024724,0.024156,0.024279,0.024619,0.02461,0.024701,0.024764,0.024336,0.024623,0.024704,0.023458,0.024701,0.023895,0.023932,0.024574,0.023831,0.024711,0.024655,0.024668,0.024675,0.024616,0.024721,0.024222,0.024672,0.023831,0.024606,0.024355,0.024349,0.024675,0.024685,0.024642,0.024629,0.024616,0.024577,0.024704,0.023828,0.024377,0.024377,0.024377,0.024377,0.024384,0.02298,0.024461,0.024225,0.024662,0.024662,0.024206,0.023831,0.02477,0.025299,0.02327,0.024619,0.024623,0.024311,0.005348,0.024346,0.024603,0.024244,0.02459,0.024616,0.024626,0.024652,0.024652,0.024646,0.024619,0.024639,0.024741,0.023932,0.023932,0.023932,0.023932,0.024652,0.024355,0.024384,0.024623,0.024413,0.023297,0.024632,0.024603,0.024623,0.023825,0.023297,0.024613,0.024649,0.024342,0.023297,0.023443,0.024639,0.024698,0.024009,0.024358,0.024953,0.024953,0.024623,0.024613,0.024626,0.024953,0.024953,0.024642,0.024646,0.024636,0.024672,0.024642,0.024616,0.024629,0.024626,0.024619,0.024636,0.024678,0.024613,0.024636,0.024636,0.024619,0.024636,0.024626,0.024623,0.024623,0.024642,0.024384,0.024613,0.024629,0.024626,0.024619,0.024632,0.024619,0.024632,0.024616,0.024642,0.024619,0.024665,0.024636,0.024613,0.024613,0.02458,0.02477,0.024682,0.024629,0.024279,0.024336,0.02461,0.024336,0.02461,0.024336,0.024655,0.02392,0.025,0.025234,0.025224,0.024053,0.024477,0.024813,0.02501,0.024987,0.02439,0.024953,0.024783,0.02439,0.02439,0.024803,0.024813,0.02439,0.024826,0.02484,0.024403,0.024381,0.024409,0.02439,0.024409,0.0244,0.024409,0.024409,0.02495,0.0248,0.024797,0.023512,0.025169],[0.259777,0.328622,0.369781,0.28972,0.287037,0.283537,0.290172,0.233962,0.226553,0.235443,0.233375,0.273932,0.245383,0.302439,0.275148,0.185814,0.328042,0.292453,0.280543,0.240933,0.287926,0.216279,0.279279,0.319588,0.202174,0.191753,0.172222,0.188069,0.273128,0.2976,0.290172,0.282246,0.288372,0.178503,0.178846,0.216279,0.184158,0.22963,0.285276,0.304419,0.29477,0.301459,0.301459,0.274742,0.256552,0.219599,0.152209,0.236641,0.236641,0.236641,0.236641,0.185814,0.220379,0.189217,0.228782,0.286595,0.180934,0.180758,0.221429,0.190769,0.005348,0.218824,0.284404,0.178503,0.181463,0.005376,0.005405,0.26087,0.259777,0.271533,0.271533,0.281392,0.181287,0.18111,0.178332,0.215527,0.177481,0.28882,0.155388,0.155388,0.155388,0.155388,0.216783,0.24282,0.183071,0.180934,0.274336,0.209224,0.28972,0.274336,0.228221,0.283969,0.278027,0.154613,0.215527,0.207358,0.173346,0.178503,0.17799,0.287926,0.219858,0.231343,0.005348,0.005348,0.17782,0.285276,0.176303,0.005348,0.005348,0.227106,0.224096,0.274742,0.272328,0.176136,0.272727,0.176471,0.178674,0.277612,0.176806,0.176806,0.280543,0.273932,0.176303,0.176303,0.176303,0.17971,0.17597,0.273128,0.17597,0.17971,0.179537,0.179537,0.271533,0.270742,0.270742,0.283105,0.176136,0.270742,0.213303,0.179537,0.176136,0.17597,0.271137,0.270742,0.270742,0.17597,0.279279,0.150242,0.237852,0.19893,0.19893,0.19893,0.19893,0.19893,0.179884,0.219599,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.497326,0.542274,0.673913,0.455882,0.473282,0.460396,0.457002,0.455882,0.311558,0.428571,0.426606,0.454768,0.335135,0.475703,0.415179,0.421769,0.536023,0.478149,0.438679,0.449275,0.462687,0.366864,0.438679,0.504065,0.319039,0.440758,0.349624,0.433566,0.437647,0.459259,0.452555,0.440758,0.465,0.354286,0.419865,0.371257,0.408791,0.372745,0.433566,0.478149,0.473282,0.468514,0.472081,0.43662,0.426606,0.429561,0.348968,0.403471,0.403471,0.403471,0.403471,0.366142,0.340037,0.404348,0.420814,0.489474,0.414254,0.361165,0.389121,0.48564,0.486911,0.334532,0.447115,0.410596,0.344444,0.233375,0.298077,0.438679,0.394068,0.414254,0.429561,0.432558,0.46384,0.462687,0.438679,0.412417,0.444976,0.468514,0.285276,0.285276,0.285276,0.285276,0.430556,0.429561,0.395745,0.419865,0.446043,0.334532,0.450363,0.410596,0.422727,0.432558,0.416107,0.415179,0.409692,0.399142,0.252374,0.273529,0.426606,0.46384,0.329787,0.373494,0.360465,0.360465,0.410596,0.429561,0.413333,0.360465,0.360465,0.42369,0.424658,0.433566,0.435597,0.427586,0.429561,0.410596,0.409692,0.425629,0.408791,0.418919,0.435597,0.439716,0.422727,0.420814,0.422727,0.424658,0.419865,0.42369,0.427586,0.365422,0.418919,0.421769,0.42369,0.420814,0.42369,0.425629,0.408791,0.408791,0.420814,0.411504,0.416107,0.414254,0.415179,0.407895,0.411504,0.444976,0.437647,0.413333,0.343173,0.352273,0.418919,0.352273,0.418919,0.352273,0.462687,0.380368,0.359768,0.419865,0.418919,0.337568,0.305921,0.365422,0.303922,0.302439,0.347015,0.297125,0.348968,0.347015,0.347015,0.357006,0.358382,0.347015,0.357006,0.358382,0.347015,0.298077,0.347664,0.347015,0.347664,0.347015,0.348315,0.348315,0.384298,0.363281,0.361165,0.169863,0.403471],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var15_df_closseness, by=list(var15_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var15_df_closseness, by=list(var15_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-4e6d72293a9110dfe469" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-4e6d72293a9110dfe469">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000123\" data-max=\"0.000135\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001989\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4e-06\" data-max=\"0.000608\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001904\" data-max=\"0.003623\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.8e-05\" data-max=\"0.000392\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022961\" data-max=\"0.025169\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.3e-05\" data-max=\"0.004304\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.369781\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000693\" data-max=\"0.11309\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.354037\" data-max=\"0.673913\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005083\" data-max=\"0.072845\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003883\" data-max=\"0.139638\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[0.000132,0.000131,0.000132,0.000133,0.000132,0.000133,0.000133,0.000135,0.000133,0.000133,0.000133,0.000123,0.000132,0.000133,0.000133,0.000131,0.000135,0.000133,0.000134,0.000132,0.000133,0.000132,0.000132],[1e-06,2e-06,null,null,null,null,0,null,null,null,null,2.3e-05,1e-06,0,0,1e-06,null,null,null,1e-06,null,1e-06,0],[0.001471,0.000413,0.001295,0.001767,0.00156,0.001026,0.001168,0.001988,0.001502,0.001502,0.001553,0.001113,0.001624,0.0014,0.00161,0.001399,2.9e-05,0.001548,0.001397,0.000831,0.001572,0.000894,0.001209],[0.000167,0.00053,null,null,null,null,0.00033,null,null,null,null,0.000322,4e-06,0.000218,0.000133,0.000172,null,null,null,0.000608,null,0.000415,0.000251],[0.002297,0.001904,0.002415,0.002915,0.002457,0.002611,0.002404,0.003623,0.002358,0.002353,0.002519,0.001954,0.002538,0.002405,0.002634,0.00221,0.002169,0.002494,0.002674,0.002083,0.002571,0.002177,0.002276],[0.000392,0.000312,null,null,null,null,7.9e-05,null,null,null,null,0.000352,2.8e-05,0.000115,0.000218,0.000282,null,null,null,0.000144,null,0.000162,7.6e-05],[0.024562,0.024398,0.024623,0.024727,0.024613,0.02477,0.024728,0.02504,0.024701,0.024682,0.024741,0.022961,0.02446,0.02468,0.024699,0.024416,0.025169,0.024698,0.02492,0.024585,0.024764,0.02465,0.024627],[0.000245,0.000402,null,null,null,null,3.6e-05,null,null,null,null,0.004304,0.000257,2.2e-05,1.3e-05,0.000262,null,null,null,0.00028,null,0.000255,3e-05],[0.273665,0.076767,0.240933,0.328622,0.290172,0.190769,0.217322,0.369781,0.279279,0.279279,0.28882,0.207066,0.301949,0.26033,0.299539,0.260152,0.005348,0.287926,0.259777,0.154541,0.292453,0.166206,0.224811],[0.031179,0.09862,null,null,null,null,0.061444,null,null,null,null,0.059894,0.000693,0.040631,0.024747,0.031957,null,null,null,0.11309,null,0.077248,0.046739],[0.427152,0.354037,0.449275,0.542274,0.457002,0.48564,0.447076,0.673913,0.438679,0.437647,0.468514,0.363367,0.472108,0.447291,0.4899,0.411073,0.403471,0.46384,0.497326,0.387497,0.478149,0.404997,0.423248],[0.072845,0.058009,null,null,null,null,0.014674,null,null,null,null,0.065502,0.005083,0.021411,0.04046,0.052523,null,null,null,0.026823,null,0.030177,0.014051],[0.300585,0.099063,0.286154,0.343173,0.303922,0.323478,0.300896,0.391579,0.295238,0.29108,0.313659,0.266552,0.319611,0.301223,0.316849,0.295625,0.005348,0.306425,0.371257,0.194167,0.314189,0.234219,0.288627],[0.043014,0.125815,null,null,null,null,0.018146,null,null,null,null,0.070696,0.003883,0.033229,0.029748,0.023781,null,null,null,0.139638,null,0.102732,0.011577]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var15_df_closseness <- data.frame(
var15_incloseness,
var15_outcloseness,
var15_totalcloseness,
var15_incloseness_n,
var15_outcloseness_n,
var15_totalcloseness_n,
var15_centr_closeness) %>% round(6)

#Adding type
var15_df_closseness <-cbind(var15_df_closseness, V(var15)$TIPO1)

#Adding names
names(var15_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var15_df_closseness<-var15_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var15_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-2e6db014ce96c32ed4d8" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-2e6db014ce96c32ed4d8">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000136\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001989\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000913\" data-max=\"0.003623\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.025299\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.369781\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.169863\" data-max=\"0.673913\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[0.000134,0.000133,0.000135,0.000133,0.000133,0.000133,0.000132,0.000133,0.000128,0.000132,0.000133,0.000133,0.00013,0.000131,0.000132,0.000132,0.000133,0.000133,0.000131,0.000132,0.000133,0.000126,0.000133,0.000128,0.000129,0.000132,0.000128,0.000133,0.000133,0.000133,0.000133,0.000132,0.000133,0.00013,0.000133,0.000128,0.000132,0.000131,0.000131,0.000133,0.000133,0.000132,0.000132,0.000132,0.000132,0.000133,0.000128,0.000131,0.000131,0.000131,0.000131,0.000131,0.000124,0.000132,0.00013,0.000133,0.000133,0.00013,0.000128,0.000133,0.000136,0.000125,0.000132,0.000132,0.000131,2.9e-05,0.000131,0.000132,0.00013,0.000132,0.000132,0.000132,0.000133,0.000133,0.000133,0.000132,0.000132,0.000133,0.000129,0.000129,0.000129,0.000129,0.000133,0.000131,0.000131,0.000132,0.000131,0.000125,0.000132,0.000132,0.000132,0.000128,0.000125,0.000132,0.000133,0.000131,0.000125,0.000126,0.000132,0.000133,0.000129,0.000131,0.000134,0.000134,0.000132,0.000132,0.000132,0.000134,0.000134,0.000132,0.000133,0.000132,0.000133,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000133,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000131,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000133,0.000132,0.000132,0.000132,0.000132,0.000133,0.000133,0.000132,0.000131,0.000131,0.000132,0.000131,0.000132,0.000131,0.000133,0.000129,0.000134,0.000136,0.000136,0.000129,0.000132,0.000133,0.000134,0.000134,0.000131,0.000134,0.000133,0.000131,0.000131,0.000133,0.000133,0.000131,0.000133,0.000134,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000134,0.000133,0.000133,0.000126,0.000135],[0.001397,0.001767,0.001988,0.001558,0.001543,0.001524,0.00156,0.001258,0.001218,0.001266,0.001255,0.001473,0.001319,0.001626,0.001479,0.000999,0.001764,0.001572,0.001508,0.001295,0.001548,0.001163,0.001502,0.001718,0.001087,0.001031,0.000926,0.001011,0.001468,0.0016,0.00156,0.001517,0.00155,0.00096,0.000962,0.001163,0.00099,0.001235,0.001534,0.001637,0.001585,0.001621,0.001621,0.001477,0.001379,0.001181,0.000818,0.001272,0.001272,0.001272,0.001272,0.000999,0.001185,0.001017,0.00123,0.001541,0.000973,0.000972,0.00119,0.001026,2.9e-05,0.001176,0.001529,0.00096,0.000976,2.9e-05,2.9e-05,0.001403,0.001397,0.00146,0.00146,0.001513,0.000975,0.000974,0.000959,0.001159,0.000954,0.001553,0.000835,0.000835,0.000835,0.000835,0.001166,0.001305,0.000984,0.000973,0.001475,0.001125,0.001558,0.001475,0.001227,0.001527,0.001495,0.000831,0.001159,0.001115,0.000932,0.00096,0.000957,0.001548,0.001182,0.001244,2.9e-05,2.9e-05,0.000956,0.001534,0.000948,2.9e-05,2.9e-05,0.001221,0.001205,0.001477,0.001464,0.000947,0.001466,0.000949,0.000961,0.001493,0.000951,0.000951,0.001508,0.001473,0.000948,0.000948,0.000948,0.000966,0.000946,0.001468,0.000946,0.000966,0.000965,0.000965,0.00146,0.001456,0.001456,0.001522,0.000947,0.001456,0.001147,0.000965,0.000947,0.000946,0.001458,0.001456,0.001456,0.000946,0.001502,0.000808,0.001279,0.00107,0.00107,0.00107,0.00107,0.00107,0.000967,0.001181,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.002674,0.002915,0.003623,0.002451,0.002545,0.002475,0.002457,0.002451,0.001675,0.002304,0.002294,0.002445,0.001802,0.002558,0.002232,0.002268,0.002882,0.002571,0.002358,0.002415,0.002488,0.001972,0.002358,0.00271,0.001715,0.00237,0.00188,0.002331,0.002353,0.002469,0.002433,0.00237,0.0025,0.001905,0.002257,0.001996,0.002198,0.002004,0.002331,0.002571,0.002545,0.002519,0.002538,0.002347,0.002294,0.002309,0.001876,0.002169,0.002169,0.002169,0.002169,0.001969,0.001828,0.002174,0.002262,0.002632,0.002227,0.001942,0.002092,0.002611,0.002618,0.001799,0.002404,0.002208,0.001852,0.001255,0.001603,0.002358,0.002119,0.002227,0.002309,0.002326,0.002494,0.002488,0.002358,0.002217,0.002392,0.002519,0.001534,0.001534,0.001534,0.001534,0.002315,0.002309,0.002128,0.002257,0.002398,0.001799,0.002421,0.002208,0.002273,0.002326,0.002237,0.002232,0.002203,0.002146,0.001357,0.001471,0.002294,0.002494,0.001773,0.002008,0.001938,0.001938,0.002208,0.002309,0.002222,0.001938,0.001938,0.002278,0.002283,0.002331,0.002342,0.002299,0.002309,0.002208,0.002203,0.002288,0.002198,0.002252,0.002342,0.002364,0.002273,0.002262,0.002273,0.002283,0.002257,0.002278,0.002299,0.001965,0.002252,0.002268,0.002278,0.002262,0.002278,0.002288,0.002198,0.002198,0.002262,0.002212,0.002237,0.002227,0.002232,0.002193,0.002212,0.002392,0.002353,0.002222,0.001845,0.001894,0.002252,0.001894,0.002252,0.001894,0.002488,0.002045,0.001934,0.002257,0.002252,0.001815,0.001645,0.001965,0.001634,0.001626,0.001866,0.001597,0.001876,0.001866,0.001866,0.001919,0.001927,0.001866,0.001919,0.001927,0.001866,0.001603,0.001869,0.001866,0.001869,0.001866,0.001873,0.001873,0.002066,0.001953,0.001942,0.000913,0.002169],[0.02492,0.024727,0.02504,0.024668,0.024711,0.024685,0.024613,0.024646,0.023737,0.024603,0.024688,0.024724,0.024156,0.024279,0.024619,0.02461,0.024701,0.024764,0.024336,0.024623,0.024704,0.023458,0.024701,0.023895,0.023932,0.024574,0.023831,0.024711,0.024655,0.024668,0.024675,0.024616,0.024721,0.024222,0.024672,0.023831,0.024606,0.024355,0.024349,0.024675,0.024685,0.024642,0.024629,0.024616,0.024577,0.024704,0.023828,0.024377,0.024377,0.024377,0.024377,0.024384,0.02298,0.024461,0.024225,0.024662,0.024662,0.024206,0.023831,0.02477,0.025299,0.02327,0.024619,0.024623,0.024311,0.005348,0.024346,0.024603,0.024244,0.02459,0.024616,0.024626,0.024652,0.024652,0.024646,0.024619,0.024639,0.024741,0.023932,0.023932,0.023932,0.023932,0.024652,0.024355,0.024384,0.024623,0.024413,0.023297,0.024632,0.024603,0.024623,0.023825,0.023297,0.024613,0.024649,0.024342,0.023297,0.023443,0.024639,0.024698,0.024009,0.024358,0.024953,0.024953,0.024623,0.024613,0.024626,0.024953,0.024953,0.024642,0.024646,0.024636,0.024672,0.024642,0.024616,0.024629,0.024626,0.024619,0.024636,0.024678,0.024613,0.024636,0.024636,0.024619,0.024636,0.024626,0.024623,0.024623,0.024642,0.024384,0.024613,0.024629,0.024626,0.024619,0.024632,0.024619,0.024632,0.024616,0.024642,0.024619,0.024665,0.024636,0.024613,0.024613,0.02458,0.02477,0.024682,0.024629,0.024279,0.024336,0.02461,0.024336,0.02461,0.024336,0.024655,0.02392,0.025,0.025234,0.025224,0.024053,0.024477,0.024813,0.02501,0.024987,0.02439,0.024953,0.024783,0.02439,0.02439,0.024803,0.024813,0.02439,0.024826,0.02484,0.024403,0.024381,0.024409,0.02439,0.024409,0.0244,0.024409,0.024409,0.02495,0.0248,0.024797,0.023512,0.025169],[0.259777,0.328622,0.369781,0.28972,0.287037,0.283537,0.290172,0.233962,0.226553,0.235443,0.233375,0.273932,0.245383,0.302439,0.275148,0.185814,0.328042,0.292453,0.280543,0.240933,0.287926,0.216279,0.279279,0.319588,0.202174,0.191753,0.172222,0.188069,0.273128,0.2976,0.290172,0.282246,0.288372,0.178503,0.178846,0.216279,0.184158,0.22963,0.285276,0.304419,0.29477,0.301459,0.301459,0.274742,0.256552,0.219599,0.152209,0.236641,0.236641,0.236641,0.236641,0.185814,0.220379,0.189217,0.228782,0.286595,0.180934,0.180758,0.221429,0.190769,0.005348,0.218824,0.284404,0.178503,0.181463,0.005376,0.005405,0.26087,0.259777,0.271533,0.271533,0.281392,0.181287,0.18111,0.178332,0.215527,0.177481,0.28882,0.155388,0.155388,0.155388,0.155388,0.216783,0.24282,0.183071,0.180934,0.274336,0.209224,0.28972,0.274336,0.228221,0.283969,0.278027,0.154613,0.215527,0.207358,0.173346,0.178503,0.17799,0.287926,0.219858,0.231343,0.005348,0.005348,0.17782,0.285276,0.176303,0.005348,0.005348,0.227106,0.224096,0.274742,0.272328,0.176136,0.272727,0.176471,0.178674,0.277612,0.176806,0.176806,0.280543,0.273932,0.176303,0.176303,0.176303,0.17971,0.17597,0.273128,0.17597,0.17971,0.179537,0.179537,0.271533,0.270742,0.270742,0.283105,0.176136,0.270742,0.213303,0.179537,0.176136,0.17597,0.271137,0.270742,0.270742,0.17597,0.279279,0.150242,0.237852,0.19893,0.19893,0.19893,0.19893,0.19893,0.179884,0.219599,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.497326,0.542274,0.673913,0.455882,0.473282,0.460396,0.457002,0.455882,0.311558,0.428571,0.426606,0.454768,0.335135,0.475703,0.415179,0.421769,0.536023,0.478149,0.438679,0.449275,0.462687,0.366864,0.438679,0.504065,0.319039,0.440758,0.349624,0.433566,0.437647,0.459259,0.452555,0.440758,0.465,0.354286,0.419865,0.371257,0.408791,0.372745,0.433566,0.478149,0.473282,0.468514,0.472081,0.43662,0.426606,0.429561,0.348968,0.403471,0.403471,0.403471,0.403471,0.366142,0.340037,0.404348,0.420814,0.489474,0.414254,0.361165,0.389121,0.48564,0.486911,0.334532,0.447115,0.410596,0.344444,0.233375,0.298077,0.438679,0.394068,0.414254,0.429561,0.432558,0.46384,0.462687,0.438679,0.412417,0.444976,0.468514,0.285276,0.285276,0.285276,0.285276,0.430556,0.429561,0.395745,0.419865,0.446043,0.334532,0.450363,0.410596,0.422727,0.432558,0.416107,0.415179,0.409692,0.399142,0.252374,0.273529,0.426606,0.46384,0.329787,0.373494,0.360465,0.360465,0.410596,0.429561,0.413333,0.360465,0.360465,0.42369,0.424658,0.433566,0.435597,0.427586,0.429561,0.410596,0.409692,0.425629,0.408791,0.418919,0.435597,0.439716,0.422727,0.420814,0.422727,0.424658,0.419865,0.42369,0.427586,0.365422,0.418919,0.421769,0.42369,0.420814,0.42369,0.425629,0.408791,0.408791,0.420814,0.411504,0.416107,0.414254,0.415179,0.407895,0.411504,0.444976,0.437647,0.413333,0.343173,0.352273,0.418919,0.352273,0.418919,0.352273,0.462687,0.380368,0.359768,0.419865,0.418919,0.337568,0.305921,0.365422,0.303922,0.302439,0.347015,0.297125,0.348968,0.347015,0.347015,0.357006,0.358382,0.347015,0.357006,0.358382,0.347015,0.298077,0.347664,0.347015,0.347664,0.347015,0.348315,0.348315,0.384298,0.363281,0.361165,0.169863,0.403471],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var15_df_closseness, by=list(var15_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var15_df_closseness, by=list(var15_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-b34e5eec93b2e7280aa7" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-b34e5eec93b2e7280aa7">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000129\" data-max=\"0.000132\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000758\" data-max=\"0.001166\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000415\" data-max=\"0.000606\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001971\" data-max=\"0.002303\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000222\" data-max=\"0.000334\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.024056\" data-max=\"0.024654\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00016\" data-max=\"0.002183\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.140927\" data-max=\"0.216934\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.077219\" data-max=\"0.112759\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.366579\" data-max=\"0.428431\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.041302\" data-max=\"0.06215\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.173776\" data-max=\"0.2725\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.079091\" data-max=\"0.135432\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[0.000132,0.000129],[1e-06,1.2e-05],[0.001166,0.000758],[0.000415,0.000606],[0.002303,0.001971],[0.000222,0.000334],[0.024654,0.024056],[0.00016,0.002183],[0.216934,0.140927],[0.077219,0.112759],[0.428431,0.366579],[0.041302,0.06215],[0.2725,0.173776],[0.079091,0.135432]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var15_df_closseness <- data.frame(
var15_incloseness,
var15_outcloseness,
var15_totalcloseness,
var15_incloseness_n,
var15_outcloseness_n,
var15_totalcloseness_n,
var15_centr_closeness) %>% round(6)

#Adding type
var15_df_closseness <-cbind(var15_df_closseness, V(var15)$TIPO2)

#Adding names
names(var15_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var15_df_closseness<-var15_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var15_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-75a9e65a282e067e191a" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-75a9e65a282e067e191a">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000136\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001989\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000913\" data-max=\"0.003623\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.025299\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.369781\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.169863\" data-max=\"0.673913\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[0.000134,0.000133,0.000135,0.000133,0.000133,0.000133,0.000132,0.000133,0.000128,0.000132,0.000133,0.000133,0.00013,0.000131,0.000132,0.000132,0.000133,0.000133,0.000131,0.000132,0.000133,0.000126,0.000133,0.000128,0.000129,0.000132,0.000128,0.000133,0.000133,0.000133,0.000133,0.000132,0.000133,0.00013,0.000133,0.000128,0.000132,0.000131,0.000131,0.000133,0.000133,0.000132,0.000132,0.000132,0.000132,0.000133,0.000128,0.000131,0.000131,0.000131,0.000131,0.000131,0.000124,0.000132,0.00013,0.000133,0.000133,0.00013,0.000128,0.000133,0.000136,0.000125,0.000132,0.000132,0.000131,2.9e-05,0.000131,0.000132,0.00013,0.000132,0.000132,0.000132,0.000133,0.000133,0.000133,0.000132,0.000132,0.000133,0.000129,0.000129,0.000129,0.000129,0.000133,0.000131,0.000131,0.000132,0.000131,0.000125,0.000132,0.000132,0.000132,0.000128,0.000125,0.000132,0.000133,0.000131,0.000125,0.000126,0.000132,0.000133,0.000129,0.000131,0.000134,0.000134,0.000132,0.000132,0.000132,0.000134,0.000134,0.000132,0.000133,0.000132,0.000133,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000133,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000131,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000132,0.000133,0.000132,0.000132,0.000132,0.000132,0.000133,0.000133,0.000132,0.000131,0.000131,0.000132,0.000131,0.000132,0.000131,0.000133,0.000129,0.000134,0.000136,0.000136,0.000129,0.000132,0.000133,0.000134,0.000134,0.000131,0.000134,0.000133,0.000131,0.000131,0.000133,0.000133,0.000131,0.000133,0.000134,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000131,0.000134,0.000133,0.000133,0.000126,0.000135],[0.001397,0.001767,0.001988,0.001558,0.001543,0.001524,0.00156,0.001258,0.001218,0.001266,0.001255,0.001473,0.001319,0.001626,0.001479,0.000999,0.001764,0.001572,0.001508,0.001295,0.001548,0.001163,0.001502,0.001718,0.001087,0.001031,0.000926,0.001011,0.001468,0.0016,0.00156,0.001517,0.00155,0.00096,0.000962,0.001163,0.00099,0.001235,0.001534,0.001637,0.001585,0.001621,0.001621,0.001477,0.001379,0.001181,0.000818,0.001272,0.001272,0.001272,0.001272,0.000999,0.001185,0.001017,0.00123,0.001541,0.000973,0.000972,0.00119,0.001026,2.9e-05,0.001176,0.001529,0.00096,0.000976,2.9e-05,2.9e-05,0.001403,0.001397,0.00146,0.00146,0.001513,0.000975,0.000974,0.000959,0.001159,0.000954,0.001553,0.000835,0.000835,0.000835,0.000835,0.001166,0.001305,0.000984,0.000973,0.001475,0.001125,0.001558,0.001475,0.001227,0.001527,0.001495,0.000831,0.001159,0.001115,0.000932,0.00096,0.000957,0.001548,0.001182,0.001244,2.9e-05,2.9e-05,0.000956,0.001534,0.000948,2.9e-05,2.9e-05,0.001221,0.001205,0.001477,0.001464,0.000947,0.001466,0.000949,0.000961,0.001493,0.000951,0.000951,0.001508,0.001473,0.000948,0.000948,0.000948,0.000966,0.000946,0.001468,0.000946,0.000966,0.000965,0.000965,0.00146,0.001456,0.001456,0.001522,0.000947,0.001456,0.001147,0.000965,0.000947,0.000946,0.001458,0.001456,0.001456,0.000946,0.001502,0.000808,0.001279,0.00107,0.00107,0.00107,0.00107,0.00107,0.000967,0.001181,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.002674,0.002915,0.003623,0.002451,0.002545,0.002475,0.002457,0.002451,0.001675,0.002304,0.002294,0.002445,0.001802,0.002558,0.002232,0.002268,0.002882,0.002571,0.002358,0.002415,0.002488,0.001972,0.002358,0.00271,0.001715,0.00237,0.00188,0.002331,0.002353,0.002469,0.002433,0.00237,0.0025,0.001905,0.002257,0.001996,0.002198,0.002004,0.002331,0.002571,0.002545,0.002519,0.002538,0.002347,0.002294,0.002309,0.001876,0.002169,0.002169,0.002169,0.002169,0.001969,0.001828,0.002174,0.002262,0.002632,0.002227,0.001942,0.002092,0.002611,0.002618,0.001799,0.002404,0.002208,0.001852,0.001255,0.001603,0.002358,0.002119,0.002227,0.002309,0.002326,0.002494,0.002488,0.002358,0.002217,0.002392,0.002519,0.001534,0.001534,0.001534,0.001534,0.002315,0.002309,0.002128,0.002257,0.002398,0.001799,0.002421,0.002208,0.002273,0.002326,0.002237,0.002232,0.002203,0.002146,0.001357,0.001471,0.002294,0.002494,0.001773,0.002008,0.001938,0.001938,0.002208,0.002309,0.002222,0.001938,0.001938,0.002278,0.002283,0.002331,0.002342,0.002299,0.002309,0.002208,0.002203,0.002288,0.002198,0.002252,0.002342,0.002364,0.002273,0.002262,0.002273,0.002283,0.002257,0.002278,0.002299,0.001965,0.002252,0.002268,0.002278,0.002262,0.002278,0.002288,0.002198,0.002198,0.002262,0.002212,0.002237,0.002227,0.002232,0.002193,0.002212,0.002392,0.002353,0.002222,0.001845,0.001894,0.002252,0.001894,0.002252,0.001894,0.002488,0.002045,0.001934,0.002257,0.002252,0.001815,0.001645,0.001965,0.001634,0.001626,0.001866,0.001597,0.001876,0.001866,0.001866,0.001919,0.001927,0.001866,0.001919,0.001927,0.001866,0.001603,0.001869,0.001866,0.001869,0.001866,0.001873,0.001873,0.002066,0.001953,0.001942,0.000913,0.002169],[0.02492,0.024727,0.02504,0.024668,0.024711,0.024685,0.024613,0.024646,0.023737,0.024603,0.024688,0.024724,0.024156,0.024279,0.024619,0.02461,0.024701,0.024764,0.024336,0.024623,0.024704,0.023458,0.024701,0.023895,0.023932,0.024574,0.023831,0.024711,0.024655,0.024668,0.024675,0.024616,0.024721,0.024222,0.024672,0.023831,0.024606,0.024355,0.024349,0.024675,0.024685,0.024642,0.024629,0.024616,0.024577,0.024704,0.023828,0.024377,0.024377,0.024377,0.024377,0.024384,0.02298,0.024461,0.024225,0.024662,0.024662,0.024206,0.023831,0.02477,0.025299,0.02327,0.024619,0.024623,0.024311,0.005348,0.024346,0.024603,0.024244,0.02459,0.024616,0.024626,0.024652,0.024652,0.024646,0.024619,0.024639,0.024741,0.023932,0.023932,0.023932,0.023932,0.024652,0.024355,0.024384,0.024623,0.024413,0.023297,0.024632,0.024603,0.024623,0.023825,0.023297,0.024613,0.024649,0.024342,0.023297,0.023443,0.024639,0.024698,0.024009,0.024358,0.024953,0.024953,0.024623,0.024613,0.024626,0.024953,0.024953,0.024642,0.024646,0.024636,0.024672,0.024642,0.024616,0.024629,0.024626,0.024619,0.024636,0.024678,0.024613,0.024636,0.024636,0.024619,0.024636,0.024626,0.024623,0.024623,0.024642,0.024384,0.024613,0.024629,0.024626,0.024619,0.024632,0.024619,0.024632,0.024616,0.024642,0.024619,0.024665,0.024636,0.024613,0.024613,0.02458,0.02477,0.024682,0.024629,0.024279,0.024336,0.02461,0.024336,0.02461,0.024336,0.024655,0.02392,0.025,0.025234,0.025224,0.024053,0.024477,0.024813,0.02501,0.024987,0.02439,0.024953,0.024783,0.02439,0.02439,0.024803,0.024813,0.02439,0.024826,0.02484,0.024403,0.024381,0.024409,0.02439,0.024409,0.0244,0.024409,0.024409,0.02495,0.0248,0.024797,0.023512,0.025169],[0.259777,0.328622,0.369781,0.28972,0.287037,0.283537,0.290172,0.233962,0.226553,0.235443,0.233375,0.273932,0.245383,0.302439,0.275148,0.185814,0.328042,0.292453,0.280543,0.240933,0.287926,0.216279,0.279279,0.319588,0.202174,0.191753,0.172222,0.188069,0.273128,0.2976,0.290172,0.282246,0.288372,0.178503,0.178846,0.216279,0.184158,0.22963,0.285276,0.304419,0.29477,0.301459,0.301459,0.274742,0.256552,0.219599,0.152209,0.236641,0.236641,0.236641,0.236641,0.185814,0.220379,0.189217,0.228782,0.286595,0.180934,0.180758,0.221429,0.190769,0.005348,0.218824,0.284404,0.178503,0.181463,0.005376,0.005405,0.26087,0.259777,0.271533,0.271533,0.281392,0.181287,0.18111,0.178332,0.215527,0.177481,0.28882,0.155388,0.155388,0.155388,0.155388,0.216783,0.24282,0.183071,0.180934,0.274336,0.209224,0.28972,0.274336,0.228221,0.283969,0.278027,0.154613,0.215527,0.207358,0.173346,0.178503,0.17799,0.287926,0.219858,0.231343,0.005348,0.005348,0.17782,0.285276,0.176303,0.005348,0.005348,0.227106,0.224096,0.274742,0.272328,0.176136,0.272727,0.176471,0.178674,0.277612,0.176806,0.176806,0.280543,0.273932,0.176303,0.176303,0.176303,0.17971,0.17597,0.273128,0.17597,0.17971,0.179537,0.179537,0.271533,0.270742,0.270742,0.283105,0.176136,0.270742,0.213303,0.179537,0.176136,0.17597,0.271137,0.270742,0.270742,0.17597,0.279279,0.150242,0.237852,0.19893,0.19893,0.19893,0.19893,0.19893,0.179884,0.219599,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.497326,0.542274,0.673913,0.455882,0.473282,0.460396,0.457002,0.455882,0.311558,0.428571,0.426606,0.454768,0.335135,0.475703,0.415179,0.421769,0.536023,0.478149,0.438679,0.449275,0.462687,0.366864,0.438679,0.504065,0.319039,0.440758,0.349624,0.433566,0.437647,0.459259,0.452555,0.440758,0.465,0.354286,0.419865,0.371257,0.408791,0.372745,0.433566,0.478149,0.473282,0.468514,0.472081,0.43662,0.426606,0.429561,0.348968,0.403471,0.403471,0.403471,0.403471,0.366142,0.340037,0.404348,0.420814,0.489474,0.414254,0.361165,0.389121,0.48564,0.486911,0.334532,0.447115,0.410596,0.344444,0.233375,0.298077,0.438679,0.394068,0.414254,0.429561,0.432558,0.46384,0.462687,0.438679,0.412417,0.444976,0.468514,0.285276,0.285276,0.285276,0.285276,0.430556,0.429561,0.395745,0.419865,0.446043,0.334532,0.450363,0.410596,0.422727,0.432558,0.416107,0.415179,0.409692,0.399142,0.252374,0.273529,0.426606,0.46384,0.329787,0.373494,0.360465,0.360465,0.410596,0.429561,0.413333,0.360465,0.360465,0.42369,0.424658,0.433566,0.435597,0.427586,0.429561,0.410596,0.409692,0.425629,0.408791,0.418919,0.435597,0.439716,0.422727,0.420814,0.422727,0.424658,0.419865,0.42369,0.427586,0.365422,0.418919,0.421769,0.42369,0.420814,0.42369,0.425629,0.408791,0.408791,0.420814,0.411504,0.416107,0.414254,0.415179,0.407895,0.411504,0.444976,0.437647,0.413333,0.343173,0.352273,0.418919,0.352273,0.418919,0.352273,0.462687,0.380368,0.359768,0.419865,0.418919,0.337568,0.305921,0.365422,0.303922,0.302439,0.347015,0.297125,0.348968,0.347015,0.347015,0.357006,0.358382,0.347015,0.357006,0.358382,0.347015,0.298077,0.347664,0.347015,0.347664,0.347015,0.348315,0.348315,0.384298,0.363281,0.361165,0.169863,0.403471],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var15_df_closseness, by=list(var15_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var15_df_closseness, by=list(var15_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-ab91da7c2c0a08cc4c8d" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-ab91da7c2c0a08cc4c8d">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000129\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000749\" data-max=\"0.001474\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000198\" data-max=\"0.000605\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001967\" data-max=\"0.002474\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000153\" data-max=\"0.000334\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.024048\" data-max=\"0.024691\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.5e-05\" data-max=\"0.002195\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.139327\" data-max=\"0.274236\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.036769\" data-max=\"0.112617\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.365774\" data-max=\"0.460141\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.028474\" data-max=\"0.062195\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172406\" data-max=\"0.308329\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.029295\" data-max=\"0.135782\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[0.000133,0.000132,0.000129],[0,1e-06,1.2e-05],[0.001474,0.00112,0.000749],[0.000198,0.000419,0.000605],[0.002474,0.002276,0.001967],[0.000153,0.000218,0.000334],[0.024691,0.024649,0.024048],[2.5e-05,0.000171,0.002195],[0.274236,0.208309,0.139327],[0.036769,0.077919,0.112617],[0.460141,0.423381,0.365774],[0.028474,0.040578,0.062195],[0.308329,0.266869,0.172406],[0.029295,0.082605,0.135782]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var15_data.RData")
```

