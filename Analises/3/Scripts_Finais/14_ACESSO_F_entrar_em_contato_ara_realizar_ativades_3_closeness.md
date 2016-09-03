# SNA Closeness 14_ACESSO F) Seu serviço tem facilade de entrar em contato com este serviço para realizar ativades em conjunto (var12).
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 14_ACESSO F) Seu serviço tem facilade de entrar em contato com este serviço para realizar ativades em conjunto (var12).

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var12_data.RData")
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
#var12<-simplify(var12) #Simplify
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
V(var12)$incloseness <- closeness(var12, mode = "in", weights = E(var12)$var12) %>% round(6)
V(var12)$outcloseness <- closeness(var12, mode = "out", weights = E(var12)$var12) %>% round(6)
V(var12)$totalcloseness <- closeness(var12, mode = "total", weights = E(var12)$var12) %>% round(4)
```

###Saving to Environment

```r
var12_incloseness<- closeness(var12, mode = "in", weights = E(var12)$var12) %>% round(6)
var12_outcloseness<- closeness(var12, mode = "out", weights = E(var12)$var12) %>% round(6)
var12_totalcloseness<- closeness(var12, mode = "total", weights = E(var12)$var12) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var12_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0001250 0.0001270 0.0001254 0.0001280 0.0001330
```

```r
sd(var12_incloseness)
```

```
## [1] 7.766454e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var12)$incloseness<-closeness(var12, weights = E(var12)$var12, mode="in")

#Get Variable
V(var12)$var12_color_degree<-round(V(var12)$incloseness,6)

#Creating brewer pallette
vertex_var12_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var12)$var12_color_degree)), "RdBu"))(
            length(unique(V(var12)$var12_color_degree)))

#Saving as Vertex properties 
V(var12)$vertex_var12_color_degree<-
  vertex_var12_color_degree[as.numeric(
  cut(V(var12)$var12_color_degree,
      breaks=length(unique(V(var12)$var12_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var12, es=E(var12), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var12))
maxC <- rep(Inf, vcount(var12))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var12, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var12)$weight)


#PLotting
plot(var12, 
     layout=co,
     edge.color=V(var12)$vertex_var12_color_degree[edge.start],
     edge.arrow.size=closeness(var12, weights = E(var12)$var12, mode="in"),
     edge.width=E(var12)$weight/mean(E(var12)$weight),
     edge.curved = TRUE,
     vertex.color=V(var12)$vertex_var12_color_degree,
     vertex.size=closeness(var12, weights = E(var12)$var12, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var12,"LABEL_COR"),
     vertex.label.cex=(closeness(var12, weights = E(var12)$var12, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var12)$var12_color_degree
b<-V(var12)$vertex_var12_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 14_ACESSO F) Seu serviço tem facilade de entrar em contato com este serviço para realizar ativades em conjunto (var12).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var12, mode="in", weights = E(var12)$var12)), 
             sd(closeness(var12, mode="in", weights = E(var12)$var12))
             )
       )
```

![](14_ACESSO_F_entrar_em_contato_ara_realizar_ativades_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var12_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0005880 0.0007500 0.0006471 0.0008640 0.0012350
```

```r
sd(var12_outcloseness)
```

```
## [1] 0.000340156
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var12)$outcloseness<-closeness(var12, weights = E(var12)$var12, mode="out")

#Get Variable
V(var12)$var12_color_degree<-round(V(var12)$outcloseness,6)

#Creating brewer pallette
vertex_var12_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var12)$var12_color_degree)), "RdBu"))(
            length(unique(V(var12)$var12_color_degree)))

#Saving as Vertex properties 
V(var12)$vertex_var12_color_degree<-
  vertex_var12_color_degree[as.numeric(
  cut(V(var12)$var12_color_degree,
      breaks=length(unique(V(var12)$var12_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var12, es=E(var12), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var12))
maxC <- rep(Inf, vcount(var12))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var12, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var12)$weight)


#PLotting
plot(var12, 
     layout=co,
     edge.color=V(var12)$vertex_var12_color_degree[edge.start],
     edge.arrow.size=closeness(var12, weights = E(var12)$var12, mode="out"),
     edge.width=E(var12)$weight/2*mean(E(var12)$weight),
     edge.curved = TRUE,
     vertex.color=V(var12)$vertex_var12_color_degree,
     vertex.size=closeness(var12, weights = E(var12)$var12, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var12,"LABEL_COR"),
     vertex.label.cex=closeness(var12, weights = E(var12)$var12, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var12)$var12_color_degree
b<-V(var12)$vertex_var12_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 14_ACESSO F) Seu serviço tem facilade de entrar em contato com este serviço para realizar ativades em conjunto (var12).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var12, mode="out", weights = E(var12)$var12)), 
             sd(closeness(var12, mode="out", weights = E(var12)$var12))
             )
       )
```

![](14_ACESSO_F_entrar_em_contato_ara_realizar_ativades_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var12_totalcloseness)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000662 0.001081 0.001311 0.001295 0.001515 0.002012
```

```r
sd(var12_totalcloseness)
```

```
## [1] 0.0002820708
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var12)$allcloseness<-closeness(var12, weights = E(var12)$var12, mode="all")

#Get Variable
V(var12)$var12_color_degree<-round(V(var12)$allcloseness,6)

#Creating brewer pallette
vertex_var12_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var12)$var12_color_degree)), "RdBu"))(
            length(unique(V(var12)$var12_color_degree)))

#Saving as Vertex properties 
V(var12)$vertex_var12_color_degree<-
  vertex_var12_color_degree[as.numeric(
  cut(V(var12)$var12_color_degree,
      breaks=length(unique(V(var12)$var12_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var12, es=E(var12), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var12))
maxC <- rep(Inf, vcount(var12))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var12, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var12)$weight)


#PLotting
plot(var12, 
     layout=co,
     edge.color=V(var12)$vertex_var12_color_degree[edge.start],
     edge.arrow.size=closeness(var12, weights = E(var12)$var12, mode="all"),
     edge.width=E(var12)$weight/2*mean(E(var12)$weight),
     edge.curved = TRUE,
     vertex.color=V(var12)$vertex_var12_color_degree,
     vertex.size=closeness(var12, weights = E(var12)$var12, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var12,"LABEL_COR"),
     vertex.label.cex=(closeness(var12, weights = E(var12)$var12, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var12)$var12_color_degree
b<-V(var12)$vertex_var12_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 14_ACESSO F) Seu serviço tem facilade de entrar em contato com este serviço para realizar ativades em conjunto (var12).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var12, mode="all", weights = E(var12)$var12)), 
             sd(closeness(var12, mode="all", weights = E(var12)$var12))
             )
       )
```

![](14_ACESSO_F_entrar_em_contato_ara_realizar_ativades_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var12)$incloseness_n <- closeness(var12, mode = "in",, weights = E(var12)$var12, normalized = T) %>% round(10)
V(var12)$outcloseness_n <- closeness(var12, mode = "out", normalized = T, weights = E(var12)$var12) %>% round(6)
V(var12)$totalcloseness_n <- closeness(var12, mode = "total", normalized = T, weights = E(var12)$var12) %>% round(6)
```

###Saving to Environment

```r
var12_incloseness_n<- closeness(var12, mode = "in", normalized = T, weights = E(var12)$var12) %>% round(6)
var12_outcloseness_n<- closeness(var12, mode = "out", normalized = T, weights = E(var12)$var12) %>% round(6)
var12_totalcloseness_n<- closeness(var12, mode = "total", normalized = T, weights = E(var12)$var12) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var12_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.023320 0.023590 0.023320 0.023770 0.024690
```

```r
sd(var12_incloseness_n)
```

```
## [1] 0.001444952
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var12)$incloseness_n<-closeness(var12, weights = E(var12)$var12, mode="in", normalized = T)

#Get Variable
V(var12)$var12_color_degree<-round(V(var12)$incloseness_n,6)

#Creating brewer pallette
vertex_var12_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var12)$var12_color_degree)), "RdBu"))(
            length(unique(V(var12)$var12_color_degree)))

#Saving as Vertex properties 
V(var12)$vertex_var12_color_degree<-
  vertex_var12_color_degree[as.numeric(
  cut(V(var12)$var12_color_degree,
      breaks=length(unique(V(var12)$var12_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var12, es=E(var12), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var12))
maxC <- rep(Inf, vcount(var12))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var12, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var12)$weight)


#PLotting
plot(var12, 
     layout=co,
     edge.color=V(var12)$vertex_var12_color_degree[edge.start],
     edge.arrow.size=closeness(var12, weights = E(var12)$var12, mode="in",normalized = T),
     edge.width=E(var12)$weight/10*mean(E(var12)$weight),
     edge.curved = TRUE,
     vertex.color=V(var12)$vertex_var12_color_degree,
     vertex.size=(closeness(var12, weights = E(var12)$var12, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var12,"LABEL_COR"),
     vertex.label.cex=closeness(var12, weights = E(var12)$var12, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var12)$var12_color_degree
b<-V(var12)$vertex_var12_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 14_ACESSO F) Seu serviço tem facilade de entrar em contato com este serviço para realizar ativades em conjunto (var12).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var12, mode="in", weights = E(var12)$var12, normalized = T)), 
             sd(closeness(var12, mode="in", weights = E(var12)$var12, normalized = T))
             )
       )
```

![](14_ACESSO_F_entrar_em_contato_ara_realizar_ativades_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var12_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.109300 0.139500 0.120300 0.160800 0.229600
```

```r
sd(var12_outcloseness_n)
```

```
## [1] 0.06328321
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var12)$outcloseness_n<-closeness(var12, weights = E(var12)$var12, mode="out", normalized = T)

#Get Variable
V(var12)$var12_color_degree<-round(V(var12)$outcloseness_n,6)

#Creating brewer pallette
vertex_var12_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var12)$var12_color_degree)), "RdBu"))(
            length(unique(V(var12)$var12_color_degree)))

#Saving as Vertex properties 
V(var12)$vertex_var12_color_degree<-
  vertex_var12_color_degree[as.numeric(
  cut(V(var12)$var12_color_degree,
      breaks=length(unique(V(var12)$var12_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var12, es=E(var12), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var12))
maxC <- rep(Inf, vcount(var12))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var12, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var12)$weight)


#PLotting
plot(var12, 
     layout=co,
     edge.color=V(var12)$vertex_var12_color_degree[edge.start],
     edge.arrow.size=closeness(var12, weights = E(var12)$var12, mode="out",normalized = T),
     edge.width=E(var12)$weight/10*mean(E(var12)$weight),
     edge.curved = TRUE,
     vertex.color=V(var12)$vertex_var12_color_degree,
     vertex.size=(closeness(var12, weights = E(var12)$var12, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var12,"LABEL_COR"),
     vertex.label.cex=closeness(var12, weights = E(var12)$var12, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var12)$var12_color_degree
b<-V(var12)$vertex_var12_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 14_ACESSO F) Seu serviço tem facilade de entrar em contato com este serviço para realizar ativades em conjunto (var12).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var12, mode="out", weights = E(var12)$var12, normalized = T)), 
             sd(closeness(var12, mode="out", weights = E(var12)$var12, normalized = T))
             )
       )
```

![](14_ACESSO_F_entrar_em_contato_ara_realizar_ativades_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var12_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1231  0.2011  0.2438  0.2408  0.2818  0.3742
```

```r
sd(var12_totalcloseness_n)
```

```
## [1] 0.05246392
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var12)$allcloseness_n<-closeness(var12, weights = E(var12)$var12, mode="all", normalized = T)

#Get Variable
V(var12)$var12_color_degree<-round(V(var12)$allcloseness_n,6)

#Creating brewer pallette
vertex_var12_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var12)$var12_color_degree)), "RdBu"))(
            length(unique(V(var12)$var12_color_degree)))

#Saving as Vertex properties 
V(var12)$vertex_var12_color_degree<-
  vertex_var12_color_degree[as.numeric(
  cut(V(var12)$var12_color_degree,
      breaks=length(unique(V(var12)$var12_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var12, es=E(var12), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var12))
maxC <- rep(Inf, vcount(var12))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var12, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var12)$weight)


#PLotting
plot(var12, 
     layout=co,
     edge.color=V(var12)$vertex_var12_color_degree[edge.start],
     edge.arrow.size=closeness(var12, weights = E(var12)$var12, mode="all",normalized = T),
     edge.width=E(var12)$weight/10*mean(E(var12)$weight),
     edge.curved = TRUE,
     vertex.color=V(var12)$vertex_var12_color_degree,
     vertex.size=(closeness(var12, weights = E(var12)$var12, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var12,"LABEL_COR"),
     vertex.label.cex=closeness(var12, weights = E(var12)$var12, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var12)$var12_color_degree
b<-V(var12)$vertex_var12_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 14_ACESSO F) Seu serviço tem facilade de entrar em contato com este serviço para realizar ativades em conjunto (var12).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var12, mode="all", weights = E(var12)$var12, normalized = T)), 
             sd(closeness(var12, mode="all", weights = E(var12)$var12, normalized = T))
             )
       )
```

![](14_ACESSO_F_entrar_em_contato_ara_realizar_ativades_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var12)$incloseness_n <- closeness(var12, weights = E(var12)$var12, mode = "in", normalized = T) %>% round(6)
V(var12)$outcloseness_n <- closeness(var12, weights = E(var12)$var12, mode = "out", normalized = T) %>% round(6)
V(var12)$totalcloseness_n <- closeness(var12, weights = E(var12)$var12, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var12)$var12_centr_closeness<- centralization.closeness(var12)$res
var12_centr_closeness<- centralization.closeness(var12)$res
var12_centr_closeness_all<- centralization.closeness(var12)
```

###Centralization

```r
var12_centr_closeness_all$centralization
```

```
## [1] 0.1625197
```

###Theoretical Max

```r
var12_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var12)$var12_centr_closeness<- centralization.closeness(var12)$res

#Get Variable
V(var12)$var12_color_degree<-round(V(var12)$var12_centr_closeness,6)

#Creating brewer pallette
vertex_var12_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var12)$var12_color_degree)), "Spectral"))(
            length(unique(V(var12)$var12_color_degree)))

#Saving as Vertex properties 
V(var12)$vertex_var12_color_degree<-
  vertex_var12_color_degree[as.numeric(
  cut(V(var12)$var12_color_degree,
      breaks=length(unique(V(var12)$var12_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var12, es=E(var12), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var12))
maxC <- rep(Inf, vcount(var12))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var12, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var12)$weight)


#PLotting
plot(var12, 
     layout=co,
     edge.color=V(var12)$vertex_var12_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var12)$res,
     edge.width=E(var12)$weight/10*mean(E(var12)$weight),
     edge.curved = TRUE,
     vertex.color=V(var12)$vertex_var12_color_degree,
     vertex.size=centralization.closeness(var12)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var12,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var12)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var12)$var12_color_degree
b<-V(var12)$vertex_var12_color_degree
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
  title("Network Centralization Closeness - 14_ACESSO F) Seu serviço tem facilade de entrar em contato com este serviço para realizar ativades em conjunto (var12).", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var12)$res), 
             sd(centralization.closeness(var12)$res)
             )
       )
```

![](14_ACESSO_F_entrar_em_contato_ara_realizar_ativades_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var12_incloseness<- closeness(var12, weights = E(var12)$var12, mode = "in") %>% round(6)
var12_outcloseness<- closeness(var12, weights = E(var12)$var12, mode = "out") %>% round(6)
var12_totalcloseness<- closeness(var12, weights = E(var12)$var12, mode = "total") %>% round(6)
var12_incloseness_n<- closeness(var12,weights = E(var12)$var12, mode = "in", normalized = T) %>% round(6)
var12_outcloseness_n<- closeness(var12,weights = E(var12)$var12, mode = "out", normalized = T) %>% round(6)
var12_totalcloseness_n<- closeness(var12,weights = E(var12)$var12, mode = "total", normalized = T) %>% round(6)
var12_centr_closeness <- centralization.closeness(var12)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var12_df_closseness <- data.frame(
var12_incloseness,
var12_outcloseness,
var12_totalcloseness,
var12_incloseness_n,
var12_outcloseness_n,
var12_totalcloseness_n,
var12_centr_closeness) %>% round(6)

#Adding type
var12_df_closseness <-cbind(var12_df_closseness, V(var12)$LABEL_COR)

#Adding names
names(var12_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var12_df_closseness<-var12_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var12_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-e38afc44a1da30c2c89c" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-e38afc44a1da30c2c89c">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001235\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000662\" data-max=\"0.002012\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024691\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.22963\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.123097\" data-max=\"0.374245\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[0.000131,0.00013,0.000133,0.000127,0.000128,0.000128,0.000127,0.000128,0.000126,0.000126,0.000128,0.000128,0.000125,0.000122,0.000127,0.000126,0.000128,0.000131,0.000125,0.000129,0.000129,0.000127,0.000129,0.00012,0.000119,0.000126,0.000121,0.000129,0.000127,0.000128,0.000128,0.000126,0.000128,0.000121,0.000127,0.000127,0.000128,0.000123,0.000123,0.000129,0.000128,0.000126,0.000126,0.000129,0.000126,0.000128,0.000117,0.000124,0.000124,0.000124,0.000124,0.000124,0.000119,0.000126,0.000121,0.000128,0.000127,0.000125,0.000122,0.000129,0.00013,0.000121,0.000127,0.000126,0.000126,2.9e-05,0.000117,0.000127,0.000121,0.000126,0.000128,0.000128,0.000128,0.000127,0.000128,0.000127,0.000127,0.000128,0.000119,0.000119,0.000119,0.000119,0.000128,0.000127,0.000125,0.000128,0.000128,0.000119,0.000127,0.000126,0.000127,0.000116,0.000121,0.000127,0.000127,0.000126,0.000119,0.000126,0.000128,0.000128,0.000123,0.000125,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000128,0.000127,0.000127,0.000128,0.000127,0.000128,0.000128,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000129,0.000127,0.000127,0.000127,0.000127,0.000128,0.000128,0.000128,0.000126,0.000127,0.000128,0.000128,0.000128,0.000128,0.000128,0.000127,0.000127,0.000128,0.000126,0.000127,0.000127,0.000126,0.000126,0.000126,0.000129,0.000128,0.000129,0.000125,0.000126,0.000127,0.000126,0.000127,0.000126,0.000127,0.000117,0.00013,0.000131,0.00013,0.000126,0.000122,0.000128,0.000129,0.000129,0.000119,0.00012,0.000123,0.000119,0.000119,0.000127,0.000124,0.000119,0.000124,0.000125,0.000129,0.000129,0.000128,0.000119,0.000128,0.000126,0.000128,0.000128,0.000126,0.000128,0.000123,0.000122,0.000128],[0.00097,0.001037,0.00103,0.001046,0.000762,0.000914,0.000779,0.001093,0.000864,0.00073,0.000813,0.000776,0.001071,0.000855,0.000859,0.000842,0.000987,0.000983,0.000705,0.000795,0.00087,0.000671,0.000846,0.000851,0.000813,0.000587,0.000641,0.000917,0.001115,0.000858,0.001072,0.000864,0.000995,0.000488,0.000589,0.000853,0.000916,0.000887,0.000733,0.001116,0.000903,0.001235,0.001233,0.000752,0.000862,0.000786,0.00063,0.000845,0.000845,0.000845,0.000845,0.000845,0.00067,0.000735,0.001054,0.000874,0.000808,0.000763,0.000706,0.000863,2.9e-05,0.000538,0.001047,0.000693,0.000811,2.9e-05,2.9e-05,0.001074,0.00085,0.000659,0.00075,0.000728,0.000999,0.000783,0.000824,0.000997,0.000846,0.000905,0.000613,0.000613,0.000613,0.000613,0.000668,0.000579,0.000919,0.00087,0.000886,0.000638,0.000776,0.000746,0.000838,0.000942,0.000728,0.000669,0.00083,0.000667,0.000472,0.000581,0.000874,0.000823,0.000578,0.000849,2.9e-05,2.9e-05,0.000678,0.000779,0.000674,2.9e-05,2.9e-05,0.000723,0.000944,0.000989,0.000737,0.000756,0.000916,0.000925,0.000879,0.000749,0.000944,0.000944,0.00098,0.000929,0.000659,0.000659,0.000659,0.000855,0.000657,0.000701,0.000657,0.000855,0.000959,0.000959,0.00084,0.000657,0.000657,0.00074,0.000765,0.000657,0.000867,0.00096,0.000742,0.000657,0.000883,0.000657,0.000657,0.000657,0.000708,0.000483,0.000627,0.000751,0.000751,0.000751,0.000751,0.000751,0.000563,0.000631,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001905,0.001795,0.002012,0.001587,0.001515,0.001621,0.001376,0.001832,0.001381,0.001261,0.001555,0.001543,0.001605,0.001112,0.001312,0.001348,0.001553,0.001792,0.001309,0.001517,0.001631,0.001374,0.001629,0.001134,0.001214,0.000977,0.00098,0.001582,0.001783,0.001484,0.001767,0.001368,0.001675,0.000875,0.001427,0.001548,0.001493,0.00123,0.001021,0.00177,0.001462,0.001901,0.001905,0.001587,0.001355,0.001441,0.000908,0.001348,0.001348,0.001348,0.001348,0.001348,0.000951,0.00116,0.00165,0.001462,0.001372,0.001304,0.000998,0.001623,0.001193,0.000981,0.001727,0.00108,0.001408,0.000662,0.000867,0.001704,0.001218,0.000984,0.001456,0.001311,0.001686,0.001183,0.001462,0.001577,0.00141,0.001555,0.000829,0.000829,0.000829,0.000829,0.001206,0.001359,0.001515,0.001393,0.001715,0.000938,0.00117,0.001073,0.001414,0.001585,0.00102,0.001359,0.001295,0.001414,0.000851,0.001393,0.00161,0.001529,0.001189,0.001144,0.000917,0.000917,0.001074,0.001109,0.001075,0.000917,0.001048,0.001153,0.001587,0.001739,0.001208,0.001395,0.001605,0.001618,0.001391,0.001361,0.001637,0.001637,0.001667,0.001647,0.001155,0.001126,0.001155,0.001248,0.001294,0.001294,0.001312,0.001248,0.001585,0.001621,0.001397,0.001294,0.001307,0.001397,0.001214,0.001364,0.001529,0.00159,0.001082,0.001125,0.001479,0.001072,0.000955,0.001504,0.00135,0.00137,0.001346,0.001093,0.001124,0.001093,0.001124,0.001093,0.001305,0.001111,0.001294,0.001294,0.001124,0.001255,0.000761,0.001319,0.001046,0.001038,0.000868,0.000864,0.000871,0.000868,0.000868,0.000986,0.000893,0.000868,0.000916,0.000914,0.00125,0.00125,0.001227,0.000868,0.001227,0.000968,0.001348,0.001348,0.000946,0.001316,0.00089,0.000791,0.000951],[0.024422,0.02409,0.024691,0.0237,0.023871,0.02384,0.023682,0.02377,0.023376,0.023523,0.023877,0.023868,0.023282,0.022702,0.023556,0.023429,0.023718,0.024361,0.023314,0.024,0.023923,0.023538,0.023901,0.022391,0.02218,0.023349,0.022491,0.023911,0.0237,0.023806,0.023752,0.023518,0.023828,0.02254,0.023697,0.023544,0.023721,0.022937,0.022887,0.023951,0.023764,0.023491,0.02352,0.023911,0.023432,0.023764,0.021726,0.022997,0.022997,0.022997,0.022997,0.022997,0.022204,0.023449,0.022518,0.023728,0.023691,0.023267,0.022716,0.023985,0.024247,0.022437,0.023577,0.02352,0.023438,0.005348,0.021831,0.02367,0.022518,0.023452,0.023819,0.023788,0.02377,0.023691,0.023852,0.023625,0.023658,0.023837,0.022132,0.022132,0.022132,0.022132,0.023737,0.023646,0.023332,0.023767,0.02381,0.022111,0.023544,0.0235,0.023541,0.02155,0.022537,0.023685,0.023616,0.023455,0.022111,0.023426,0.023843,0.023837,0.022929,0.023186,0.023408,0.023408,0.02352,0.02352,0.023526,0.023408,0.023758,0.023652,0.02364,0.02384,0.023679,0.023755,0.02377,0.023604,0.023592,0.023595,0.023529,0.023625,0.023559,0.023951,0.023676,0.023631,0.023676,0.023628,0.023767,0.023773,0.023776,0.023435,0.023613,0.023764,0.023785,0.023767,0.023797,0.023776,0.02358,0.02364,0.023724,0.023512,0.023589,0.023637,0.023512,0.023515,0.023343,0.023932,0.023779,0.023988,0.023323,0.023423,0.023616,0.023423,0.023616,0.023423,0.023574,0.021834,0.024222,0.02433,0.024178,0.023423,0.022611,0.023755,0.024006,0.023957,0.022177,0.022313,0.022797,0.022177,0.022177,0.023529,0.023094,0.022177,0.023065,0.02316,0.024075,0.024065,0.023785,0.022177,0.023785,0.02337,0.023861,0.023861,0.023382,0.023797,0.022853,0.022694,0.023868],[0.180407,0.192946,0.191555,0.194561,0.141768,0.170018,0.14486,0.203279,0.160761,0.135766,0.15122,0.144298,0.199143,0.158974,0.159794,0.156698,0.183613,0.182891,0.131078,0.147854,0.16188,0.124748,0.15736,0.158298,0.15122,0.109091,0.119154,0.170486,0.207358,0.159657,0.199357,0.160761,0.185075,0.090732,0.109476,0.158703,0.17033,0.164894,0.136364,0.207589,0.16787,0.22963,0.229346,0.139955,0.160345,0.146226,0.117128,0.157095,0.157095,0.157095,0.157095,0.157095,0.124581,0.136765,0.195996,0.162587,0.150364,0.141876,0.131356,0.160483,0.005348,0.100108,0.194764,0.128898,0.150852,0.005376,0.005405,0.199785,0.158029,0.12261,0.139535,0.135371,0.185814,0.145654,0.153339,0.185444,0.15736,0.168326,0.11404,0.11404,0.11404,0.11404,0.124166,0.107701,0.170956,0.161739,0.164748,0.118622,0.14441,0.138806,0.155779,0.175141,0.135371,0.124415,0.154357,0.124,0.087736,0.10814,0.162587,0.153086,0.107452,0.157895,0.005348,0.005348,0.126187,0.144973,0.125421,0.005348,0.005348,0.13449,0.175637,0.183976,0.137067,0.14059,0.17033,0.172063,0.163445,0.139222,0.175637,0.175637,0.182353,0.172702,0.12261,0.12261,0.12261,0.15911,0.122127,0.130343,0.122127,0.15911,0.178332,0.178332,0.156303,0.122127,0.122127,0.137676,0.142311,0.122127,0.161318,0.178503,0.137982,0.122127,0.164166,0.122127,0.122127,0.122127,0.131728,0.089768,0.116614,0.13964,0.13964,0.13964,0.13964,0.13964,0.10473,0.117276,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.354286,0.333932,0.374245,0.295238,0.281818,0.301459,0.255846,0.340659,0.256906,0.234552,0.289269,0.287037,0.298555,0.206897,0.244094,0.250674,0.28882,0.333333,0.243455,0.282246,0.303426,0.255495,0.302932,0.210884,0.225728,0.181641,0.182353,0.294304,0.331551,0.275964,0.328622,0.254446,0.311558,0.16273,0.265335,0.287926,0.277612,0.228782,0.18999,0.329204,0.27193,0.353612,0.354286,0.295238,0.252033,0.268012,0.168937,0.250674,0.250674,0.250674,0.250674,0.250674,0.176974,0.215777,0.306931,0.27193,0.255144,0.242503,0.185629,0.301948,0.221957,0.182532,0.321244,0.200864,0.261972,0.123097,0.161318,0.316865,0.226553,0.183071,0.270742,0.243775,0.313659,0.220118,0.27193,0.293375,0.262341,0.289269,0.154229,0.154229,0.154229,0.154229,0.224367,0.252717,0.281818,0.259053,0.319039,0.174484,0.217544,0.199571,0.263083,0.29477,0.189796,0.252717,0.240933,0.263083,0.158298,0.259053,0.299517,0.284404,0.221165,0.212815,0.170486,0.170486,0.199785,0.206208,0.2,0.170486,0.194969,0.214533,0.295238,0.323478,0.224638,0.259414,0.298555,0.300971,0.258693,0.253061,0.304419,0.304419,0.31,0.306425,0.214781,0.209459,0.214781,0.23221,0.240621,0.240621,0.244094,0.23221,0.29477,0.301459,0.259777,0.240621,0.243137,0.259777,0.225728,0.253752,0.284404,0.295707,0.201299,0.209224,0.275148,0.199357,0.17765,0.279699,0.251012,0.254795,0.250336,0.203279,0.208989,0.203279,0.208989,0.203279,0.24282,0.206667,0.240621,0.240621,0.208989,0.233375,0.141553,0.245383,0.194561,0.193146,0.161458,0.160622,0.162021,0.161458,0.161458,0.183432,0.166071,0.161458,0.17033,0.170018,0.2325,0.2325,0.228221,0.161458,0.228221,0.180058,0.250674,0.250674,0.17597,0.244737,0.16548,0.147036,0.176806],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var12_df_closseness, by=list(var12_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var12_df_closseness, by=list(var12_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-56d7a134085401aef40a" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-56d7a134085401aef40a">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00012\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001045\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000115\" data-max=\"0.000399\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000951\" data-max=\"0.002012\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.4e-05\" data-max=\"0.000558\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022275\" data-max=\"0.024691\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.1e-05\" data-max=\"0.004144\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.194302\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.021346\" data-max=\"0.074182\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.176806\" data-max=\"0.374245\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.009954\" data-max=\"0.103743\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003883\" data-max=\"0.139638\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[0.000127,0.000123,0.000129,0.00013,0.000127,0.000129,0.000129,0.000133,0.000129,0.000128,0.000128,0.00012,0.000124,0.000128,0.000128,0.000125,0.000128,0.000128,0.000131,0.000125,0.000131,0.000128,0.000127],[2e-06,4e-06,null,null,null,null,0,null,null,null,null,2.2e-05,3e-06,0,0,3e-06,null,null,null,2e-06,null,1e-06,1e-06],[0.000842,0.000275,0.000795,0.001037,0.000779,0.000863,0.000815,0.00103,0.000846,0.000708,0.000905,0.000718,0.001045,0.000881,0.000888,0.000879,2.9e-05,0.000823,0.00097,0.000567,0.000983,0.000655,0.0008],[0.000192,0.000338,null,null,null,null,0.000139,null,null,null,null,0.000234,0.000269,0.000169,0.000115,0.0002,null,null,null,0.000399,null,0.000298,0.000118],[0.001494,0.001062,0.001517,0.001795,0.001376,0.001623,0.001572,0.002012,0.001629,0.00135,0.001555,0.001304,0.001506,0.001561,0.001563,0.001401,0.000951,0.001529,0.001905,0.001178,0.001792,0.001264,0.001354],[0.000167,0.000231,null,null,null,null,6.4e-05,null,null,null,null,0.000318,0.000558,0.000133,5.4e-05,0.000305,null,null,null,0.000196,null,0.00019,0.0002],[0.023638,0.022949,0.024,0.02409,0.023682,0.023985,0.023922,0.024691,0.023901,0.023779,0.023837,0.022275,0.023096,0.023766,0.02381,0.023272,0.023868,0.023837,0.024422,0.023185,0.024361,0.023727,0.023644],[0.000275,0.000763,null,null,null,null,1.1e-05,null,null,null,null,0.004144,0.000558,7e-05,8.1e-05,0.000553,null,null,null,0.00028,null,0.000264,0.000122],[0.15652,0.051103,0.147854,0.192946,0.14486,0.160483,0.151498,0.191555,0.15736,0.131728,0.168326,0.133507,0.194302,0.16386,0.165133,0.163415,0.005348,0.153086,0.180407,0.105402,0.182891,0.12187,0.148751],[0.035614,0.062866,null,null,null,null,0.025797,null,null,null,null,0.043499,0.049961,0.031474,0.021346,0.037158,null,null,null,0.074182,null,0.055473,0.021922],[0.277941,0.197488,0.282246,0.333932,0.255846,0.301948,0.292476,0.374245,0.302932,0.251012,0.289269,0.242526,0.280254,0.290329,0.290699,0.260562,0.176806,0.284404,0.354286,0.219104,0.333333,0.235081,0.251819],[0.031051,0.042966,null,null,null,null,0.011969,null,null,null,null,0.059149,0.103743,0.024671,0.009954,0.056755,null,null,null,0.036425,null,0.035403,0.037197],[0.300585,0.099063,0.286154,0.343173,0.303922,0.323478,0.300896,0.391579,0.295238,0.29108,0.313659,0.266552,0.319611,0.301223,0.316849,0.295625,0.005348,0.306425,0.371257,0.194167,0.314189,0.234219,0.288627],[0.043014,0.125815,null,null,null,null,0.018146,null,null,null,null,0.070696,0.003883,0.033229,0.029748,0.023781,null,null,null,0.139638,null,0.102732,0.011577]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var12_df_closseness <- data.frame(
var12_incloseness,
var12_outcloseness,
var12_totalcloseness,
var12_incloseness_n,
var12_outcloseness_n,
var12_totalcloseness_n,
var12_centr_closeness) %>% round(6)

#Adding type
var12_df_closseness <-cbind(var12_df_closseness, V(var12)$TIPO1)

#Adding names
names(var12_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var12_df_closseness<-var12_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var12_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-aac2bad43d95ba5b7378" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-aac2bad43d95ba5b7378">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001235\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000662\" data-max=\"0.002012\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024691\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.22963\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.123097\" data-max=\"0.374245\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[0.000131,0.00013,0.000133,0.000127,0.000128,0.000128,0.000127,0.000128,0.000126,0.000126,0.000128,0.000128,0.000125,0.000122,0.000127,0.000126,0.000128,0.000131,0.000125,0.000129,0.000129,0.000127,0.000129,0.00012,0.000119,0.000126,0.000121,0.000129,0.000127,0.000128,0.000128,0.000126,0.000128,0.000121,0.000127,0.000127,0.000128,0.000123,0.000123,0.000129,0.000128,0.000126,0.000126,0.000129,0.000126,0.000128,0.000117,0.000124,0.000124,0.000124,0.000124,0.000124,0.000119,0.000126,0.000121,0.000128,0.000127,0.000125,0.000122,0.000129,0.00013,0.000121,0.000127,0.000126,0.000126,2.9e-05,0.000117,0.000127,0.000121,0.000126,0.000128,0.000128,0.000128,0.000127,0.000128,0.000127,0.000127,0.000128,0.000119,0.000119,0.000119,0.000119,0.000128,0.000127,0.000125,0.000128,0.000128,0.000119,0.000127,0.000126,0.000127,0.000116,0.000121,0.000127,0.000127,0.000126,0.000119,0.000126,0.000128,0.000128,0.000123,0.000125,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000128,0.000127,0.000127,0.000128,0.000127,0.000128,0.000128,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000129,0.000127,0.000127,0.000127,0.000127,0.000128,0.000128,0.000128,0.000126,0.000127,0.000128,0.000128,0.000128,0.000128,0.000128,0.000127,0.000127,0.000128,0.000126,0.000127,0.000127,0.000126,0.000126,0.000126,0.000129,0.000128,0.000129,0.000125,0.000126,0.000127,0.000126,0.000127,0.000126,0.000127,0.000117,0.00013,0.000131,0.00013,0.000126,0.000122,0.000128,0.000129,0.000129,0.000119,0.00012,0.000123,0.000119,0.000119,0.000127,0.000124,0.000119,0.000124,0.000125,0.000129,0.000129,0.000128,0.000119,0.000128,0.000126,0.000128,0.000128,0.000126,0.000128,0.000123,0.000122,0.000128],[0.00097,0.001037,0.00103,0.001046,0.000762,0.000914,0.000779,0.001093,0.000864,0.00073,0.000813,0.000776,0.001071,0.000855,0.000859,0.000842,0.000987,0.000983,0.000705,0.000795,0.00087,0.000671,0.000846,0.000851,0.000813,0.000587,0.000641,0.000917,0.001115,0.000858,0.001072,0.000864,0.000995,0.000488,0.000589,0.000853,0.000916,0.000887,0.000733,0.001116,0.000903,0.001235,0.001233,0.000752,0.000862,0.000786,0.00063,0.000845,0.000845,0.000845,0.000845,0.000845,0.00067,0.000735,0.001054,0.000874,0.000808,0.000763,0.000706,0.000863,2.9e-05,0.000538,0.001047,0.000693,0.000811,2.9e-05,2.9e-05,0.001074,0.00085,0.000659,0.00075,0.000728,0.000999,0.000783,0.000824,0.000997,0.000846,0.000905,0.000613,0.000613,0.000613,0.000613,0.000668,0.000579,0.000919,0.00087,0.000886,0.000638,0.000776,0.000746,0.000838,0.000942,0.000728,0.000669,0.00083,0.000667,0.000472,0.000581,0.000874,0.000823,0.000578,0.000849,2.9e-05,2.9e-05,0.000678,0.000779,0.000674,2.9e-05,2.9e-05,0.000723,0.000944,0.000989,0.000737,0.000756,0.000916,0.000925,0.000879,0.000749,0.000944,0.000944,0.00098,0.000929,0.000659,0.000659,0.000659,0.000855,0.000657,0.000701,0.000657,0.000855,0.000959,0.000959,0.00084,0.000657,0.000657,0.00074,0.000765,0.000657,0.000867,0.00096,0.000742,0.000657,0.000883,0.000657,0.000657,0.000657,0.000708,0.000483,0.000627,0.000751,0.000751,0.000751,0.000751,0.000751,0.000563,0.000631,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001905,0.001795,0.002012,0.001587,0.001515,0.001621,0.001376,0.001832,0.001381,0.001261,0.001555,0.001543,0.001605,0.001112,0.001312,0.001348,0.001553,0.001792,0.001309,0.001517,0.001631,0.001374,0.001629,0.001134,0.001214,0.000977,0.00098,0.001582,0.001783,0.001484,0.001767,0.001368,0.001675,0.000875,0.001427,0.001548,0.001493,0.00123,0.001021,0.00177,0.001462,0.001901,0.001905,0.001587,0.001355,0.001441,0.000908,0.001348,0.001348,0.001348,0.001348,0.001348,0.000951,0.00116,0.00165,0.001462,0.001372,0.001304,0.000998,0.001623,0.001193,0.000981,0.001727,0.00108,0.001408,0.000662,0.000867,0.001704,0.001218,0.000984,0.001456,0.001311,0.001686,0.001183,0.001462,0.001577,0.00141,0.001555,0.000829,0.000829,0.000829,0.000829,0.001206,0.001359,0.001515,0.001393,0.001715,0.000938,0.00117,0.001073,0.001414,0.001585,0.00102,0.001359,0.001295,0.001414,0.000851,0.001393,0.00161,0.001529,0.001189,0.001144,0.000917,0.000917,0.001074,0.001109,0.001075,0.000917,0.001048,0.001153,0.001587,0.001739,0.001208,0.001395,0.001605,0.001618,0.001391,0.001361,0.001637,0.001637,0.001667,0.001647,0.001155,0.001126,0.001155,0.001248,0.001294,0.001294,0.001312,0.001248,0.001585,0.001621,0.001397,0.001294,0.001307,0.001397,0.001214,0.001364,0.001529,0.00159,0.001082,0.001125,0.001479,0.001072,0.000955,0.001504,0.00135,0.00137,0.001346,0.001093,0.001124,0.001093,0.001124,0.001093,0.001305,0.001111,0.001294,0.001294,0.001124,0.001255,0.000761,0.001319,0.001046,0.001038,0.000868,0.000864,0.000871,0.000868,0.000868,0.000986,0.000893,0.000868,0.000916,0.000914,0.00125,0.00125,0.001227,0.000868,0.001227,0.000968,0.001348,0.001348,0.000946,0.001316,0.00089,0.000791,0.000951],[0.024422,0.02409,0.024691,0.0237,0.023871,0.02384,0.023682,0.02377,0.023376,0.023523,0.023877,0.023868,0.023282,0.022702,0.023556,0.023429,0.023718,0.024361,0.023314,0.024,0.023923,0.023538,0.023901,0.022391,0.02218,0.023349,0.022491,0.023911,0.0237,0.023806,0.023752,0.023518,0.023828,0.02254,0.023697,0.023544,0.023721,0.022937,0.022887,0.023951,0.023764,0.023491,0.02352,0.023911,0.023432,0.023764,0.021726,0.022997,0.022997,0.022997,0.022997,0.022997,0.022204,0.023449,0.022518,0.023728,0.023691,0.023267,0.022716,0.023985,0.024247,0.022437,0.023577,0.02352,0.023438,0.005348,0.021831,0.02367,0.022518,0.023452,0.023819,0.023788,0.02377,0.023691,0.023852,0.023625,0.023658,0.023837,0.022132,0.022132,0.022132,0.022132,0.023737,0.023646,0.023332,0.023767,0.02381,0.022111,0.023544,0.0235,0.023541,0.02155,0.022537,0.023685,0.023616,0.023455,0.022111,0.023426,0.023843,0.023837,0.022929,0.023186,0.023408,0.023408,0.02352,0.02352,0.023526,0.023408,0.023758,0.023652,0.02364,0.02384,0.023679,0.023755,0.02377,0.023604,0.023592,0.023595,0.023529,0.023625,0.023559,0.023951,0.023676,0.023631,0.023676,0.023628,0.023767,0.023773,0.023776,0.023435,0.023613,0.023764,0.023785,0.023767,0.023797,0.023776,0.02358,0.02364,0.023724,0.023512,0.023589,0.023637,0.023512,0.023515,0.023343,0.023932,0.023779,0.023988,0.023323,0.023423,0.023616,0.023423,0.023616,0.023423,0.023574,0.021834,0.024222,0.02433,0.024178,0.023423,0.022611,0.023755,0.024006,0.023957,0.022177,0.022313,0.022797,0.022177,0.022177,0.023529,0.023094,0.022177,0.023065,0.02316,0.024075,0.024065,0.023785,0.022177,0.023785,0.02337,0.023861,0.023861,0.023382,0.023797,0.022853,0.022694,0.023868],[0.180407,0.192946,0.191555,0.194561,0.141768,0.170018,0.14486,0.203279,0.160761,0.135766,0.15122,0.144298,0.199143,0.158974,0.159794,0.156698,0.183613,0.182891,0.131078,0.147854,0.16188,0.124748,0.15736,0.158298,0.15122,0.109091,0.119154,0.170486,0.207358,0.159657,0.199357,0.160761,0.185075,0.090732,0.109476,0.158703,0.17033,0.164894,0.136364,0.207589,0.16787,0.22963,0.229346,0.139955,0.160345,0.146226,0.117128,0.157095,0.157095,0.157095,0.157095,0.157095,0.124581,0.136765,0.195996,0.162587,0.150364,0.141876,0.131356,0.160483,0.005348,0.100108,0.194764,0.128898,0.150852,0.005376,0.005405,0.199785,0.158029,0.12261,0.139535,0.135371,0.185814,0.145654,0.153339,0.185444,0.15736,0.168326,0.11404,0.11404,0.11404,0.11404,0.124166,0.107701,0.170956,0.161739,0.164748,0.118622,0.14441,0.138806,0.155779,0.175141,0.135371,0.124415,0.154357,0.124,0.087736,0.10814,0.162587,0.153086,0.107452,0.157895,0.005348,0.005348,0.126187,0.144973,0.125421,0.005348,0.005348,0.13449,0.175637,0.183976,0.137067,0.14059,0.17033,0.172063,0.163445,0.139222,0.175637,0.175637,0.182353,0.172702,0.12261,0.12261,0.12261,0.15911,0.122127,0.130343,0.122127,0.15911,0.178332,0.178332,0.156303,0.122127,0.122127,0.137676,0.142311,0.122127,0.161318,0.178503,0.137982,0.122127,0.164166,0.122127,0.122127,0.122127,0.131728,0.089768,0.116614,0.13964,0.13964,0.13964,0.13964,0.13964,0.10473,0.117276,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.354286,0.333932,0.374245,0.295238,0.281818,0.301459,0.255846,0.340659,0.256906,0.234552,0.289269,0.287037,0.298555,0.206897,0.244094,0.250674,0.28882,0.333333,0.243455,0.282246,0.303426,0.255495,0.302932,0.210884,0.225728,0.181641,0.182353,0.294304,0.331551,0.275964,0.328622,0.254446,0.311558,0.16273,0.265335,0.287926,0.277612,0.228782,0.18999,0.329204,0.27193,0.353612,0.354286,0.295238,0.252033,0.268012,0.168937,0.250674,0.250674,0.250674,0.250674,0.250674,0.176974,0.215777,0.306931,0.27193,0.255144,0.242503,0.185629,0.301948,0.221957,0.182532,0.321244,0.200864,0.261972,0.123097,0.161318,0.316865,0.226553,0.183071,0.270742,0.243775,0.313659,0.220118,0.27193,0.293375,0.262341,0.289269,0.154229,0.154229,0.154229,0.154229,0.224367,0.252717,0.281818,0.259053,0.319039,0.174484,0.217544,0.199571,0.263083,0.29477,0.189796,0.252717,0.240933,0.263083,0.158298,0.259053,0.299517,0.284404,0.221165,0.212815,0.170486,0.170486,0.199785,0.206208,0.2,0.170486,0.194969,0.214533,0.295238,0.323478,0.224638,0.259414,0.298555,0.300971,0.258693,0.253061,0.304419,0.304419,0.31,0.306425,0.214781,0.209459,0.214781,0.23221,0.240621,0.240621,0.244094,0.23221,0.29477,0.301459,0.259777,0.240621,0.243137,0.259777,0.225728,0.253752,0.284404,0.295707,0.201299,0.209224,0.275148,0.199357,0.17765,0.279699,0.251012,0.254795,0.250336,0.203279,0.208989,0.203279,0.208989,0.203279,0.24282,0.206667,0.240621,0.240621,0.208989,0.233375,0.141553,0.245383,0.194561,0.193146,0.161458,0.160622,0.162021,0.161458,0.161458,0.183432,0.166071,0.161458,0.17033,0.170018,0.2325,0.2325,0.228221,0.161458,0.228221,0.180058,0.250674,0.250674,0.17597,0.244737,0.16548,0.147036,0.176806],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var12_df_closseness, by=list(var12_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var12_df_closseness, by=list(var12_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-a153235762cdc9be6057" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-a153235762cdc9be6057">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000123\" data-max=\"0.000127\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2e-06\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000478\" data-max=\"0.000771\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000247\" data-max=\"0.000376\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00117\" data-max=\"0.001386\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000245\" data-max=\"0.000284\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022848\" data-max=\"0.023671\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000287\" data-max=\"0.002114\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.08881\" data-max=\"0.143416\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.046021\" data-max=\"0.070002\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.217683\" data-max=\"0.257795\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.045498\" data-max=\"0.052752\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.173776\" data-max=\"0.2725\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.079091\" data-max=\"0.135432\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[0.000127,0.000123],[2e-06,1.1e-05],[0.000771,0.000478],[0.000247,0.000376],[0.001386,0.00117],[0.000245,0.000284],[0.023671,0.022848],[0.000287,0.002114],[0.143416,0.08881],[0.046021,0.070002],[0.257794,0.217683],[0.045498,0.052752],[0.2725,0.173776],[0.079091,0.135432]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var12_df_closseness <- data.frame(
var12_incloseness,
var12_outcloseness,
var12_totalcloseness,
var12_incloseness_n,
var12_outcloseness_n,
var12_totalcloseness_n,
var12_centr_closeness) %>% round(6)

#Adding type
var12_df_closseness <-cbind(var12_df_closseness, V(var12)$TIPO2)

#Adding names
names(var12_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var12_df_closseness<-var12_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var12_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-711139c1ee0865c041e8" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-711139c1ee0865c041e8">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001235\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000662\" data-max=\"0.002012\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024691\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.22963\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.123097\" data-max=\"0.374245\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[0.000131,0.00013,0.000133,0.000127,0.000128,0.000128,0.000127,0.000128,0.000126,0.000126,0.000128,0.000128,0.000125,0.000122,0.000127,0.000126,0.000128,0.000131,0.000125,0.000129,0.000129,0.000127,0.000129,0.00012,0.000119,0.000126,0.000121,0.000129,0.000127,0.000128,0.000128,0.000126,0.000128,0.000121,0.000127,0.000127,0.000128,0.000123,0.000123,0.000129,0.000128,0.000126,0.000126,0.000129,0.000126,0.000128,0.000117,0.000124,0.000124,0.000124,0.000124,0.000124,0.000119,0.000126,0.000121,0.000128,0.000127,0.000125,0.000122,0.000129,0.00013,0.000121,0.000127,0.000126,0.000126,2.9e-05,0.000117,0.000127,0.000121,0.000126,0.000128,0.000128,0.000128,0.000127,0.000128,0.000127,0.000127,0.000128,0.000119,0.000119,0.000119,0.000119,0.000128,0.000127,0.000125,0.000128,0.000128,0.000119,0.000127,0.000126,0.000127,0.000116,0.000121,0.000127,0.000127,0.000126,0.000119,0.000126,0.000128,0.000128,0.000123,0.000125,0.000126,0.000126,0.000126,0.000126,0.000126,0.000126,0.000128,0.000127,0.000127,0.000128,0.000127,0.000128,0.000128,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000129,0.000127,0.000127,0.000127,0.000127,0.000128,0.000128,0.000128,0.000126,0.000127,0.000128,0.000128,0.000128,0.000128,0.000128,0.000127,0.000127,0.000128,0.000126,0.000127,0.000127,0.000126,0.000126,0.000126,0.000129,0.000128,0.000129,0.000125,0.000126,0.000127,0.000126,0.000127,0.000126,0.000127,0.000117,0.00013,0.000131,0.00013,0.000126,0.000122,0.000128,0.000129,0.000129,0.000119,0.00012,0.000123,0.000119,0.000119,0.000127,0.000124,0.000119,0.000124,0.000125,0.000129,0.000129,0.000128,0.000119,0.000128,0.000126,0.000128,0.000128,0.000126,0.000128,0.000123,0.000122,0.000128],[0.00097,0.001037,0.00103,0.001046,0.000762,0.000914,0.000779,0.001093,0.000864,0.00073,0.000813,0.000776,0.001071,0.000855,0.000859,0.000842,0.000987,0.000983,0.000705,0.000795,0.00087,0.000671,0.000846,0.000851,0.000813,0.000587,0.000641,0.000917,0.001115,0.000858,0.001072,0.000864,0.000995,0.000488,0.000589,0.000853,0.000916,0.000887,0.000733,0.001116,0.000903,0.001235,0.001233,0.000752,0.000862,0.000786,0.00063,0.000845,0.000845,0.000845,0.000845,0.000845,0.00067,0.000735,0.001054,0.000874,0.000808,0.000763,0.000706,0.000863,2.9e-05,0.000538,0.001047,0.000693,0.000811,2.9e-05,2.9e-05,0.001074,0.00085,0.000659,0.00075,0.000728,0.000999,0.000783,0.000824,0.000997,0.000846,0.000905,0.000613,0.000613,0.000613,0.000613,0.000668,0.000579,0.000919,0.00087,0.000886,0.000638,0.000776,0.000746,0.000838,0.000942,0.000728,0.000669,0.00083,0.000667,0.000472,0.000581,0.000874,0.000823,0.000578,0.000849,2.9e-05,2.9e-05,0.000678,0.000779,0.000674,2.9e-05,2.9e-05,0.000723,0.000944,0.000989,0.000737,0.000756,0.000916,0.000925,0.000879,0.000749,0.000944,0.000944,0.00098,0.000929,0.000659,0.000659,0.000659,0.000855,0.000657,0.000701,0.000657,0.000855,0.000959,0.000959,0.00084,0.000657,0.000657,0.00074,0.000765,0.000657,0.000867,0.00096,0.000742,0.000657,0.000883,0.000657,0.000657,0.000657,0.000708,0.000483,0.000627,0.000751,0.000751,0.000751,0.000751,0.000751,0.000563,0.000631,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001905,0.001795,0.002012,0.001587,0.001515,0.001621,0.001376,0.001832,0.001381,0.001261,0.001555,0.001543,0.001605,0.001112,0.001312,0.001348,0.001553,0.001792,0.001309,0.001517,0.001631,0.001374,0.001629,0.001134,0.001214,0.000977,0.00098,0.001582,0.001783,0.001484,0.001767,0.001368,0.001675,0.000875,0.001427,0.001548,0.001493,0.00123,0.001021,0.00177,0.001462,0.001901,0.001905,0.001587,0.001355,0.001441,0.000908,0.001348,0.001348,0.001348,0.001348,0.001348,0.000951,0.00116,0.00165,0.001462,0.001372,0.001304,0.000998,0.001623,0.001193,0.000981,0.001727,0.00108,0.001408,0.000662,0.000867,0.001704,0.001218,0.000984,0.001456,0.001311,0.001686,0.001183,0.001462,0.001577,0.00141,0.001555,0.000829,0.000829,0.000829,0.000829,0.001206,0.001359,0.001515,0.001393,0.001715,0.000938,0.00117,0.001073,0.001414,0.001585,0.00102,0.001359,0.001295,0.001414,0.000851,0.001393,0.00161,0.001529,0.001189,0.001144,0.000917,0.000917,0.001074,0.001109,0.001075,0.000917,0.001048,0.001153,0.001587,0.001739,0.001208,0.001395,0.001605,0.001618,0.001391,0.001361,0.001637,0.001637,0.001667,0.001647,0.001155,0.001126,0.001155,0.001248,0.001294,0.001294,0.001312,0.001248,0.001585,0.001621,0.001397,0.001294,0.001307,0.001397,0.001214,0.001364,0.001529,0.00159,0.001082,0.001125,0.001479,0.001072,0.000955,0.001504,0.00135,0.00137,0.001346,0.001093,0.001124,0.001093,0.001124,0.001093,0.001305,0.001111,0.001294,0.001294,0.001124,0.001255,0.000761,0.001319,0.001046,0.001038,0.000868,0.000864,0.000871,0.000868,0.000868,0.000986,0.000893,0.000868,0.000916,0.000914,0.00125,0.00125,0.001227,0.000868,0.001227,0.000968,0.001348,0.001348,0.000946,0.001316,0.00089,0.000791,0.000951],[0.024422,0.02409,0.024691,0.0237,0.023871,0.02384,0.023682,0.02377,0.023376,0.023523,0.023877,0.023868,0.023282,0.022702,0.023556,0.023429,0.023718,0.024361,0.023314,0.024,0.023923,0.023538,0.023901,0.022391,0.02218,0.023349,0.022491,0.023911,0.0237,0.023806,0.023752,0.023518,0.023828,0.02254,0.023697,0.023544,0.023721,0.022937,0.022887,0.023951,0.023764,0.023491,0.02352,0.023911,0.023432,0.023764,0.021726,0.022997,0.022997,0.022997,0.022997,0.022997,0.022204,0.023449,0.022518,0.023728,0.023691,0.023267,0.022716,0.023985,0.024247,0.022437,0.023577,0.02352,0.023438,0.005348,0.021831,0.02367,0.022518,0.023452,0.023819,0.023788,0.02377,0.023691,0.023852,0.023625,0.023658,0.023837,0.022132,0.022132,0.022132,0.022132,0.023737,0.023646,0.023332,0.023767,0.02381,0.022111,0.023544,0.0235,0.023541,0.02155,0.022537,0.023685,0.023616,0.023455,0.022111,0.023426,0.023843,0.023837,0.022929,0.023186,0.023408,0.023408,0.02352,0.02352,0.023526,0.023408,0.023758,0.023652,0.02364,0.02384,0.023679,0.023755,0.02377,0.023604,0.023592,0.023595,0.023529,0.023625,0.023559,0.023951,0.023676,0.023631,0.023676,0.023628,0.023767,0.023773,0.023776,0.023435,0.023613,0.023764,0.023785,0.023767,0.023797,0.023776,0.02358,0.02364,0.023724,0.023512,0.023589,0.023637,0.023512,0.023515,0.023343,0.023932,0.023779,0.023988,0.023323,0.023423,0.023616,0.023423,0.023616,0.023423,0.023574,0.021834,0.024222,0.02433,0.024178,0.023423,0.022611,0.023755,0.024006,0.023957,0.022177,0.022313,0.022797,0.022177,0.022177,0.023529,0.023094,0.022177,0.023065,0.02316,0.024075,0.024065,0.023785,0.022177,0.023785,0.02337,0.023861,0.023861,0.023382,0.023797,0.022853,0.022694,0.023868],[0.180407,0.192946,0.191555,0.194561,0.141768,0.170018,0.14486,0.203279,0.160761,0.135766,0.15122,0.144298,0.199143,0.158974,0.159794,0.156698,0.183613,0.182891,0.131078,0.147854,0.16188,0.124748,0.15736,0.158298,0.15122,0.109091,0.119154,0.170486,0.207358,0.159657,0.199357,0.160761,0.185075,0.090732,0.109476,0.158703,0.17033,0.164894,0.136364,0.207589,0.16787,0.22963,0.229346,0.139955,0.160345,0.146226,0.117128,0.157095,0.157095,0.157095,0.157095,0.157095,0.124581,0.136765,0.195996,0.162587,0.150364,0.141876,0.131356,0.160483,0.005348,0.100108,0.194764,0.128898,0.150852,0.005376,0.005405,0.199785,0.158029,0.12261,0.139535,0.135371,0.185814,0.145654,0.153339,0.185444,0.15736,0.168326,0.11404,0.11404,0.11404,0.11404,0.124166,0.107701,0.170956,0.161739,0.164748,0.118622,0.14441,0.138806,0.155779,0.175141,0.135371,0.124415,0.154357,0.124,0.087736,0.10814,0.162587,0.153086,0.107452,0.157895,0.005348,0.005348,0.126187,0.144973,0.125421,0.005348,0.005348,0.13449,0.175637,0.183976,0.137067,0.14059,0.17033,0.172063,0.163445,0.139222,0.175637,0.175637,0.182353,0.172702,0.12261,0.12261,0.12261,0.15911,0.122127,0.130343,0.122127,0.15911,0.178332,0.178332,0.156303,0.122127,0.122127,0.137676,0.142311,0.122127,0.161318,0.178503,0.137982,0.122127,0.164166,0.122127,0.122127,0.122127,0.131728,0.089768,0.116614,0.13964,0.13964,0.13964,0.13964,0.13964,0.10473,0.117276,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.354286,0.333932,0.374245,0.295238,0.281818,0.301459,0.255846,0.340659,0.256906,0.234552,0.289269,0.287037,0.298555,0.206897,0.244094,0.250674,0.28882,0.333333,0.243455,0.282246,0.303426,0.255495,0.302932,0.210884,0.225728,0.181641,0.182353,0.294304,0.331551,0.275964,0.328622,0.254446,0.311558,0.16273,0.265335,0.287926,0.277612,0.228782,0.18999,0.329204,0.27193,0.353612,0.354286,0.295238,0.252033,0.268012,0.168937,0.250674,0.250674,0.250674,0.250674,0.250674,0.176974,0.215777,0.306931,0.27193,0.255144,0.242503,0.185629,0.301948,0.221957,0.182532,0.321244,0.200864,0.261972,0.123097,0.161318,0.316865,0.226553,0.183071,0.270742,0.243775,0.313659,0.220118,0.27193,0.293375,0.262341,0.289269,0.154229,0.154229,0.154229,0.154229,0.224367,0.252717,0.281818,0.259053,0.319039,0.174484,0.217544,0.199571,0.263083,0.29477,0.189796,0.252717,0.240933,0.263083,0.158298,0.259053,0.299517,0.284404,0.221165,0.212815,0.170486,0.170486,0.199785,0.206208,0.2,0.170486,0.194969,0.214533,0.295238,0.323478,0.224638,0.259414,0.298555,0.300971,0.258693,0.253061,0.304419,0.304419,0.31,0.306425,0.214781,0.209459,0.214781,0.23221,0.240621,0.240621,0.244094,0.23221,0.29477,0.301459,0.259777,0.240621,0.243137,0.259777,0.225728,0.253752,0.284404,0.295707,0.201299,0.209224,0.275148,0.199357,0.17765,0.279699,0.251012,0.254795,0.250336,0.203279,0.208989,0.203279,0.208989,0.203279,0.24282,0.206667,0.240621,0.240621,0.208989,0.233375,0.141553,0.245383,0.194561,0.193146,0.161458,0.160622,0.162021,0.161458,0.161458,0.183432,0.166071,0.161458,0.17033,0.170018,0.2325,0.2325,0.228221,0.161458,0.228221,0.180058,0.250674,0.250674,0.17597,0.244737,0.16548,0.147036,0.176806],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var12_df_closseness, by=list(var12_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var12_df_closseness, by=list(var12_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-ad3ccf2edc587fb72e59" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-ad3ccf2edc587fb72e59">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000123\" data-max=\"0.000128\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00048\" data-max=\"0.000893\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000138\" data-max=\"0.000385\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001173\" data-max=\"0.001562\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00011\" data-max=\"0.000294\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022832\" data-max=\"0.023783\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6.7e-05\" data-max=\"0.002124\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.089238\" data-max=\"0.166094\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.025708\" data-max=\"0.071712\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.218152\" data-max=\"0.290521\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.020455\" data-max=\"0.05467\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172406\" data-max=\"0.308329\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.029295\" data-max=\"0.135782\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[0.000128,0.000127,0.000123],[0,2e-06,1.1e-05],[0.000893,0.000747,0.00048],[0.000138,0.00025,0.000385],[0.001562,0.001354,0.001173],[0.00011,0.000241,0.000294],[0.023783,0.023658,0.022832],[6.7e-05,0.000303,0.002124],[0.166094,0.138862,0.089238],[0.025708,0.046437,0.071712],[0.290521,0.251757,0.218152],[0.020455,0.044912,0.05467],[0.308329,0.266869,0.172406],[0.029295,0.082605,0.135782]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var12_data.RData")
```

