# SNA Closeness 18_CONFIANÇA A) Confia na qualificação profissional da equipe deste serviço.(var16)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 18_CONFIANÇA A) Confia na qualificação profissional da equipe deste serviço.(var16)

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var16_data.RData")
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
#var16<-simplify(var16) #Simplify
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
V(var16)$incloseness <- closeness(var16, mode = "in", weights = E(var16)$var16) %>% round(6)
V(var16)$outcloseness <- closeness(var16, mode = "out", weights = E(var16)$var16) %>% round(6)
V(var16)$totalcloseness <- closeness(var16, mode = "total", weights = E(var16)$var16) %>% round(4)
```

###Saving to Environment

```r
var16_incloseness<- closeness(var16, mode = "in", weights = E(var16)$var16) %>% round(6)
var16_outcloseness<- closeness(var16, mode = "out", weights = E(var16)$var16) %>% round(6)
var16_totalcloseness<- closeness(var16, mode = "total", weights = E(var16)$var16) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var16_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0001240 0.0001270 0.0001251 0.0001280 0.0001310
```

```r
sd(var16_incloseness)
```

```
## [1] 7.772094e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var16)$incloseness<-closeness(var16, weights = E(var16)$var16, mode="in")

#Get Variable
V(var16)$var16_color_degree<-round(V(var16)$incloseness,6)

#Creating brewer pallette
vertex_var16_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var16)$var16_color_degree)), "RdBu"))(
            length(unique(V(var16)$var16_color_degree)))

#Saving as Vertex properties 
V(var16)$vertex_var16_color_degree<-
  vertex_var16_color_degree[as.numeric(
  cut(V(var16)$var16_color_degree,
      breaks=length(unique(V(var16)$var16_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var16, es=E(var16), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var16))
maxC <- rep(Inf, vcount(var16))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var16, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var16)$weight)


#PLotting
plot(var16, 
     layout=co,
     edge.color=V(var16)$vertex_var16_color_degree[edge.start],
     edge.arrow.size=closeness(var16, weights = E(var16)$var16, mode="in"),
     edge.width=E(var16)$weight/mean(E(var16)$weight),
     edge.curved = TRUE,
     vertex.color=V(var16)$vertex_var16_color_degree,
     vertex.size=closeness(var16, weights = E(var16)$var16, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var16,"LABEL_COR"),
     vertex.label.cex=(closeness(var16, weights = E(var16)$var16, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var16)$var16_color_degree
b<-V(var16)$vertex_var16_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 18_CONFIANÇA A) Confia na qualificação profissional da equipe deste serviço.(var16)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var16, mode="in", weights = E(var16)$var16)), 
             sd(closeness(var16, mode="in", weights = E(var16)$var16))
             )
       )
```

![](18_CONFIANÇA_A_Confia_na_qualificação_profissional_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var16_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0005925 0.0007480 0.0006329 0.0008240 0.0012420
```

```r
sd(var16_outcloseness)
```

```
## [1] 0.0003258344
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var16)$outcloseness<-closeness(var16, weights = E(var16)$var16, mode="out")

#Get Variable
V(var16)$var16_color_degree<-round(V(var16)$outcloseness,6)

#Creating brewer pallette
vertex_var16_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var16)$var16_color_degree)), "RdBu"))(
            length(unique(V(var16)$var16_color_degree)))

#Saving as Vertex properties 
V(var16)$vertex_var16_color_degree<-
  vertex_var16_color_degree[as.numeric(
  cut(V(var16)$var16_color_degree,
      breaks=length(unique(V(var16)$var16_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var16, es=E(var16), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var16))
maxC <- rep(Inf, vcount(var16))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var16, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var16)$weight)


#PLotting
plot(var16, 
     layout=co,
     edge.color=V(var16)$vertex_var16_color_degree[edge.start],
     edge.arrow.size=closeness(var16, weights = E(var16)$var16, mode="out"),
     edge.width=E(var16)$weight/2*mean(E(var16)$weight),
     edge.curved = TRUE,
     vertex.color=V(var16)$vertex_var16_color_degree,
     vertex.size=closeness(var16, weights = E(var16)$var16, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var16,"LABEL_COR"),
     vertex.label.cex=closeness(var16, weights = E(var16)$var16, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var16)$var16_color_degree
b<-V(var16)$vertex_var16_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 18_CONFIANÇA A) Confia na qualificação profissional da equipe deste serviço.(var16)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var16, mode="out", weights = E(var16)$var16)), 
             sd(closeness(var16, mode="out", weights = E(var16)$var16))
             )
       )
```

![](18_CONFIANÇA_A_Confia_na_qualificação_profissional_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var16_totalcloseness)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000598 0.001012 0.001153 0.001116 0.001212 0.001845
```

```r
sd(var16_totalcloseness)
```

```
## [1] 0.0002026634
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var16)$allcloseness<-closeness(var16, weights = E(var16)$var16, mode="all")

#Get Variable
V(var16)$var16_color_degree<-round(V(var16)$allcloseness,6)

#Creating brewer pallette
vertex_var16_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var16)$var16_color_degree)), "RdBu"))(
            length(unique(V(var16)$var16_color_degree)))

#Saving as Vertex properties 
V(var16)$vertex_var16_color_degree<-
  vertex_var16_color_degree[as.numeric(
  cut(V(var16)$var16_color_degree,
      breaks=length(unique(V(var16)$var16_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var16, es=E(var16), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var16))
maxC <- rep(Inf, vcount(var16))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var16, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var16)$weight)


#PLotting
plot(var16, 
     layout=co,
     edge.color=V(var16)$vertex_var16_color_degree[edge.start],
     edge.arrow.size=closeness(var16, weights = E(var16)$var16, mode="all"),
     edge.width=E(var16)$weight/2*mean(E(var16)$weight),
     edge.curved = TRUE,
     vertex.color=V(var16)$vertex_var16_color_degree,
     vertex.size=closeness(var16, weights = E(var16)$var16, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var16,"LABEL_COR"),
     vertex.label.cex=(closeness(var16, weights = E(var16)$var16, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var16)$var16_color_degree
b<-V(var16)$vertex_var16_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 18_CONFIANÇA A) Confia na qualificação profissional da equipe deste serviço.(var16)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var16, mode="all", weights = E(var16)$var16)), 
             sd(closeness(var16, mode="all", weights = E(var16)$var16))
             )
       )
```

![](18_CONFIANÇA_A_Confia_na_qualificação_profissional_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var16)$incloseness_n <- closeness(var16, mode = "in",, weights = E(var16)$var16, normalized = T) %>% round(10)
V(var16)$outcloseness_n <- closeness(var16, mode = "out", normalized = T, weights = E(var16)$var16) %>% round(6)
V(var16)$totalcloseness_n <- closeness(var16, mode = "total", normalized = T, weights = E(var16)$var16) %>% round(6)
```

###Saving to Environment

```r
var16_incloseness_n<- closeness(var16, mode = "in", normalized = T, weights = E(var16)$var16) %>% round(6)
var16_outcloseness_n<- closeness(var16, mode = "out", normalized = T, weights = E(var16)$var16) %>% round(6)
var16_totalcloseness_n<- closeness(var16, mode = "total", normalized = T, weights = E(var16)$var16) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var16_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.023040 0.023690 0.023290 0.023740 0.024450
```

```r
sd(var16_incloseness_n)
```

```
## [1] 0.001450056
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var16)$incloseness_n<-closeness(var16, weights = E(var16)$var16, mode="in", normalized = T)

#Get Variable
V(var16)$var16_color_degree<-round(V(var16)$incloseness_n,6)

#Creating brewer pallette
vertex_var16_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var16)$var16_color_degree)), "RdBu"))(
            length(unique(V(var16)$var16_color_degree)))

#Saving as Vertex properties 
V(var16)$vertex_var16_color_degree<-
  vertex_var16_color_degree[as.numeric(
  cut(V(var16)$var16_color_degree,
      breaks=length(unique(V(var16)$var16_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var16, es=E(var16), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var16))
maxC <- rep(Inf, vcount(var16))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var16, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var16)$weight)


#PLotting
plot(var16, 
     layout=co,
     edge.color=V(var16)$vertex_var16_color_degree[edge.start],
     edge.arrow.size=closeness(var16, weights = E(var16)$var16, mode="in",normalized = T),
     edge.width=E(var16)$weight/10*mean(E(var16)$weight),
     edge.curved = TRUE,
     vertex.color=V(var16)$vertex_var16_color_degree,
     vertex.size=(closeness(var16, weights = E(var16)$var16, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var16,"LABEL_COR"),
     vertex.label.cex=closeness(var16, weights = E(var16)$var16, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var16)$var16_color_degree
b<-V(var16)$vertex_var16_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 18_CONFIANÇA A) Confia na qualificação profissional da equipe deste serviço.(var16)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var16, mode="in", weights = E(var16)$var16, normalized = T)), 
             sd(closeness(var16, mode="in", weights = E(var16)$var16, normalized = T))
             )
       )
```

![](18_CONFIANÇA_A_Confia_na_qualificação_profissional_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var16_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.110200 0.139100 0.117700 0.153200 0.231100
```

```r
sd(var16_outcloseness_n)
```

```
## [1] 0.06061984
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var16)$outcloseness_n<-closeness(var16, weights = E(var16)$var16, mode="out", normalized = T)

#Get Variable
V(var16)$var16_color_degree<-round(V(var16)$outcloseness_n,6)

#Creating brewer pallette
vertex_var16_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var16)$var16_color_degree)), "RdBu"))(
            length(unique(V(var16)$var16_color_degree)))

#Saving as Vertex properties 
V(var16)$vertex_var16_color_degree<-
  vertex_var16_color_degree[as.numeric(
  cut(V(var16)$var16_color_degree,
      breaks=length(unique(V(var16)$var16_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var16, es=E(var16), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var16))
maxC <- rep(Inf, vcount(var16))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var16, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var16)$weight)


#PLotting
plot(var16, 
     layout=co,
     edge.color=V(var16)$vertex_var16_color_degree[edge.start],
     edge.arrow.size=closeness(var16, weights = E(var16)$var16, mode="out",normalized = T),
     edge.width=E(var16)$weight/10*mean(E(var16)$weight),
     edge.curved = TRUE,
     vertex.color=V(var16)$vertex_var16_color_degree,
     vertex.size=(closeness(var16, weights = E(var16)$var16, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var16,"LABEL_COR"),
     vertex.label.cex=closeness(var16, weights = E(var16)$var16, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var16)$var16_color_degree
b<-V(var16)$vertex_var16_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 18_CONFIANÇA A) Confia na qualificação profissional da equipe deste serviço.(var16)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var16, mode="out", weights = E(var16)$var16, normalized = T)), 
             sd(closeness(var16, mode="out", weights = E(var16)$var16, normalized = T))
             )
       )
```

![](18_CONFIANÇA_A_Confia_na_qualificação_profissional_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var16_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1112  0.1884  0.2145  0.2076  0.2255  0.3432
```

```r
sd(var16_totalcloseness_n)
```

```
## [1] 0.03769597
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var16)$allcloseness_n<-closeness(var16, weights = E(var16)$var16, mode="all", normalized = T)

#Get Variable
V(var16)$var16_color_degree<-round(V(var16)$allcloseness_n,6)

#Creating brewer pallette
vertex_var16_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var16)$var16_color_degree)), "RdBu"))(
            length(unique(V(var16)$var16_color_degree)))

#Saving as Vertex properties 
V(var16)$vertex_var16_color_degree<-
  vertex_var16_color_degree[as.numeric(
  cut(V(var16)$var16_color_degree,
      breaks=length(unique(V(var16)$var16_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var16, es=E(var16), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var16))
maxC <- rep(Inf, vcount(var16))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var16, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var16)$weight)


#PLotting
plot(var16, 
     layout=co,
     edge.color=V(var16)$vertex_var16_color_degree[edge.start],
     edge.arrow.size=closeness(var16, weights = E(var16)$var16, mode="all",normalized = T),
     edge.width=E(var16)$weight/10*mean(E(var16)$weight),
     edge.curved = TRUE,
     vertex.color=V(var16)$vertex_var16_color_degree,
     vertex.size=(closeness(var16, weights = E(var16)$var16, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var16,"LABEL_COR"),
     vertex.label.cex=closeness(var16, weights = E(var16)$var16, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var16)$var16_color_degree
b<-V(var16)$vertex_var16_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 18_CONFIANÇA A) Confia na qualificação profissional da equipe deste serviço.(var16)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var16, mode="all", weights = E(var16)$var16, normalized = T)), 
             sd(closeness(var16, mode="all", weights = E(var16)$var16, normalized = T))
             )
       )
```

![](18_CONFIANÇA_A_Confia_na_qualificação_profissional_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var16)$incloseness_n <- closeness(var16, weights = E(var16)$var16, mode = "in", normalized = T) %>% round(6)
V(var16)$outcloseness_n <- closeness(var16, weights = E(var16)$var16, mode = "out", normalized = T) %>% round(6)
V(var16)$totalcloseness_n <- closeness(var16, weights = E(var16)$var16, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var16)$var16_centr_closeness<- centralization.closeness(var16)$res
var16_centr_closeness<- centralization.closeness(var16)$res
var16_centr_closeness_all<- centralization.closeness(var16)
```

###Centralization

```r
var16_centr_closeness_all$centralization
```

```
## [1] 0.1625197
```

###Theoretical Max

```r
var16_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var16)$var16_centr_closeness<- centralization.closeness(var16)$res

#Get Variable
V(var16)$var16_color_degree<-round(V(var16)$var16_centr_closeness,6)

#Creating brewer pallette
vertex_var16_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var16)$var16_color_degree)), "Spectral"))(
            length(unique(V(var16)$var16_color_degree)))

#Saving as Vertex properties 
V(var16)$vertex_var16_color_degree<-
  vertex_var16_color_degree[as.numeric(
  cut(V(var16)$var16_color_degree,
      breaks=length(unique(V(var16)$var16_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var16, es=E(var16), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var16))
maxC <- rep(Inf, vcount(var16))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var16, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var16)$weight)


#PLotting
plot(var16, 
     layout=co,
     edge.color=V(var16)$vertex_var16_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var16)$res,
     edge.width=E(var16)$weight/10*mean(E(var16)$weight),
     edge.curved = TRUE,
     vertex.color=V(var16)$vertex_var16_color_degree,
     vertex.size=centralization.closeness(var16)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var16,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var16)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var16)$var16_color_degree
b<-V(var16)$vertex_var16_color_degree
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
  title("Network Centralization Closeness - 18_CONFIANÇA A) Confia na qualificação profissional da equipe deste serviço.(var16)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var16)$res), 
             sd(centralization.closeness(var16)$res)
             )
       )
```

![](18_CONFIANÇA_A_Confia_na_qualificação_profissional_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var16_incloseness<- closeness(var16, weights = E(var16)$var16, mode = "in") %>% round(6)
var16_outcloseness<- closeness(var16, weights = E(var16)$var16, mode = "out") %>% round(6)
var16_totalcloseness<- closeness(var16, weights = E(var16)$var16, mode = "total") %>% round(6)
var16_incloseness_n<- closeness(var16,weights = E(var16)$var16, mode = "in", normalized = T) %>% round(6)
var16_outcloseness_n<- closeness(var16,weights = E(var16)$var16, mode = "out", normalized = T) %>% round(6)
var16_totalcloseness_n<- closeness(var16,weights = E(var16)$var16, mode = "total", normalized = T) %>% round(6)
var16_centr_closeness <- centralization.closeness(var16)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var16_df_closseness <- data.frame(
var16_incloseness,
var16_outcloseness,
var16_totalcloseness,
var16_incloseness_n,
var16_outcloseness_n,
var16_totalcloseness_n,
var16_centr_closeness) %>% round(6)

#Adding type
var16_df_closseness <-cbind(var16_df_closseness, V(var16)$LABEL_COR)

#Adding names
names(var16_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var16_df_closseness<-var16_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var16_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-f777a0f521ccc9bdc22f" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-f777a0f521ccc9bdc22f">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001242\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000598\" data-max=\"0.001845\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024451\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.231056\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.111178\" data-max=\"0.343173\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[0.00013,0.000129,0.000131,0.000128,0.000128,0.000128,0.000127,0.000127,0.000123,0.000127,0.000128,0.000128,0.000123,0.000124,0.000127,0.000127,0.000128,0.000129,0.000122,0.000127,0.000128,0.000124,0.000128,0.000118,0.000116,0.000127,0.000121,0.000128,0.000128,0.000128,0.000128,0.000127,0.000128,0.000121,0.000128,0.000124,0.000127,0.000125,0.000125,0.000127,0.000128,0.000127,0.000127,0.000127,0.000127,0.000128,0.000116,0.000124,0.000124,0.000124,0.000124,0.000124,0.000122,0.000126,0.000121,0.000128,0.000128,0.000121,0.000122,0.000129,0.00013,0.00012,0.000127,0.000127,0.000124,2.9e-05,0.000118,0.000127,0.000121,0.000127,0.000127,0.000128,0.000128,0.000128,0.000128,0.000127,0.000127,0.000129,0.000122,0.000122,0.000122,0.000122,0.000128,0.000124,0.000124,0.000128,0.000125,0.000116,0.000127,0.000127,0.000127,0.000117,0.000121,0.000127,0.000128,0.000124,0.000116,0.000123,0.000128,0.000128,0.000118,0.000125,0.000128,0.000128,0.000127,0.000128,0.000127,0.000128,0.000129,0.000127,0.000127,0.000128,0.000127,0.000128,0.000128,0.000127,0.000127,0.000127,0.000128,0.000128,0.000127,0.000128,0.000128,0.000127,0.000128,0.000128,0.000127,0.000127,0.000128,0.000126,0.000127,0.000127,0.000127,0.000127,0.000128,0.000128,0.000128,0.000127,0.000128,0.000127,0.000128,0.000127,0.000127,0.000127,0.000127,0.000128,0.000128,0.000128,0.000124,0.000126,0.000127,0.000126,0.000127,0.000126,0.000127,0.000118,0.000129,0.00013,0.00013,0.000119,0.000121,0.000124,0.000127,0.000127,0.000119,0.00012,0.000124,0.000119,0.000119,0.000124,0.000124,0.000119,0.000124,0.000126,0.000129,0.000129,0.000124,0.000119,0.000124,0.000124,0.000124,0.000124,0.000125,0.000124,0.000124,0.000121,0.00013],[0.001166,0.000888,0.001242,0.000882,0.000782,0.000779,0.000804,0.000976,0.000962,0.00075,0.000886,0.000748,0.000877,0.000847,0.00083,0.00082,0.000912,0.00096,0.000889,0.000757,0.000914,0.000859,0.001054,0.000887,0.000735,0.000613,0.000715,0.000814,0.000929,0.000946,0.000921,0.000779,0.001015,0.000587,0.000589,0.001001,0.000861,0.000692,0.000759,0.001142,0.000957,0.000974,0.000974,0.000768,0.000756,0.000909,0.000726,0.000824,0.000824,0.000824,0.000824,0.000824,0.000733,0.000773,0.000894,0.00082,0.000789,0.000758,0.000639,0.000847,2.9e-05,0.000612,0.000762,0.000757,0.001027,2.9e-05,2.9e-05,0.00081,0.000783,0.000739,0.000739,0.000755,0.00076,0.000786,0.000758,0.000763,0.00075,0.000814,0.000543,0.000543,0.000543,0.000543,0.000748,0.000596,0.000772,0.0009,0.000765,0.000716,0.000792,0.000742,0.00087,0.000896,0.000904,0.000664,0.000779,0.000751,0.000513,0.000647,0.000759,0.000987,0.000604,0.000826,2.9e-05,2.9e-05,0.000753,0.000787,0.000751,2.9e-05,2.9e-05,0.000769,0.000769,0.000753,0.000747,0.000863,0.000746,0.001045,0.000763,0.000765,0.000961,0.000754,0.000756,0.000916,0.000738,0.000738,0.000738,0.000746,0.000736,0.000818,0.000736,0.000747,0.000864,0.000864,0.000747,0.000736,0.000736,0.000762,0.00074,0.000736,0.000736,0.000745,0.000737,0.000736,0.000744,0.000736,0.000853,0.000736,0.000745,0.000524,0.000629,0.000596,0.000596,0.000596,0.000596,0.000596,0.000586,0.000659,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001832,0.001416,0.001845,0.001266,0.00133,0.00122,0.001157,0.001302,0.001239,0.001147,0.001247,0.001319,0.001205,0.001057,0.001214,0.001196,0.001292,0.001391,0.001143,0.0013,0.00133,0.001152,0.001562,0.001089,0.000956,0.001139,0.000986,0.00134,0.001274,0.001332,0.001266,0.001186,0.001546,0.000887,0.001192,0.001395,0.001232,0.001029,0.001003,0.001616,0.001348,0.001277,0.001297,0.001185,0.001138,0.001325,0.001018,0.001114,0.001114,0.001114,0.001114,0.001114,0.001133,0.001166,0.001142,0.001242,0.0012,0.00101,0.000906,0.001299,0.001182,0.000773,0.001181,0.001157,0.001427,0.000598,0.00071,0.001284,0.001015,0.001153,0.001163,0.001218,0.001193,0.001211,0.001175,0.001178,0.001202,0.00133,0.000902,0.000902,0.000902,0.000902,0.00116,0.000986,0.001079,0.001164,0.001063,0.000917,0.001211,0.001152,0.001163,0.001153,0.001176,0.001271,0.001245,0.001046,0.000724,0.00089,0.001164,0.001488,0.000741,0.001041,0.000993,0.000993,0.001159,0.001232,0.001156,0.000993,0.001021,0.001206,0.001156,0.001175,0.001171,0.001161,0.00117,0.001471,0.001174,0.001168,0.001416,0.001238,0.00116,0.001305,0.001151,0.001148,0.001151,0.001153,0.001148,0.001229,0.001155,0.001112,0.001151,0.001153,0.001157,0.001152,0.001151,0.00123,0.001215,0.001157,0.00117,0.001153,0.001157,0.001161,0.001159,0.001152,0.001101,0.00123,0.001287,0.001178,0.000907,0.001093,0.001147,0.001093,0.001147,0.001093,0.001208,0.0011,0.001094,0.001148,0.001147,0.000791,0.000814,0.000805,0.000894,0.000817,0.000711,0.000702,0.000792,0.000711,0.000711,0.000794,0.000817,0.000711,0.000794,0.0009,0.001129,0.00113,0.000796,0.000711,0.000796,0.000796,0.000829,0.000829,0.000858,0.000827,0.000805,0.000796,0.001096],[0.024228,0.023978,0.024451,0.02377,0.023877,0.023785,0.023637,0.023694,0.022904,0.023616,0.023843,0.023871,0.022789,0.023023,0.023679,0.023592,0.023843,0.024015,0.02268,0.023694,0.023852,0.023,0.02388,0.021937,0.021613,0.023562,0.022562,0.023883,0.023764,0.023825,0.023834,0.023688,0.02388,0.022529,0.02377,0.02298,0.023652,0.023338,0.023329,0.023709,0.023834,0.023643,0.023634,0.023694,0.023568,0.023837,0.021628,0.023117,0.023117,0.023117,0.023117,0.023117,0.022744,0.023464,0.022556,0.02377,0.02377,0.022477,0.022672,0.023917,0.024263,0.022402,0.023703,0.023694,0.023154,0.005348,0.021867,0.023652,0.022597,0.023667,0.023709,0.023773,0.023721,0.023737,0.023743,0.023694,0.023712,0.023917,0.022763,0.022763,0.022763,0.022763,0.023724,0.02314,0.023149,0.023721,0.023302,0.021661,0.023697,0.02367,0.023688,0.021839,0.022494,0.023646,0.023834,0.02306,0.021661,0.022901,0.023734,0.023843,0.022012,0.023169,0.023871,0.023871,0.023694,0.023764,0.023694,0.023871,0.023929,0.023709,0.023697,0.023743,0.023712,0.023718,0.023724,0.023694,0.023694,0.023694,0.023776,0.02384,0.023694,0.023715,0.023718,0.023703,0.023718,0.023715,0.023703,0.023709,0.023715,0.023423,0.023694,0.023706,0.023712,0.023709,0.023718,0.023794,0.023816,0.023694,0.023715,0.023688,0.023718,0.023688,0.023685,0.023685,0.023547,0.023898,0.023773,0.023746,0.023037,0.023405,0.0237,0.023405,0.0237,0.023405,0.023667,0.021994,0.02396,0.024266,0.02426,0.022148,0.022434,0.023045,0.023646,0.023592,0.02208,0.022353,0.022991,0.02208,0.02208,0.023031,0.023091,0.02208,0.023031,0.023479,0.024047,0.024025,0.023017,0.02208,0.023017,0.023023,0.023071,0.023071,0.023262,0.023097,0.023011,0.022429,0.0241],[0.216783,0.165187,0.231056,0.164021,0.14554,0.144973,0.149518,0.181463,0.178846,0.139535,0.164748,0.139117,0.163158,0.157627,0.154357,0.152584,0.169553,0.178503,0.165333,0.140802,0.170018,0.159794,0.195996,0.16504,0.136664,0.113971,0.132952,0.151343,0.172862,0.17597,0.171271,0.14486,0.188832,0.109155,0.109605,0.186186,0.160207,0.128631,0.14123,0.212329,0.17799,0.18111,0.18111,0.142857,0.140696,0.169091,0.135076,0.153213,0.153213,0.153213,0.153213,0.153213,0.136364,0.143852,0.166369,0.152459,0.146688,0.141016,0.118926,0.157627,0.005348,0.113831,0.141768,0.140802,0.190965,0.005376,0.005405,0.150607,0.145654,0.137472,0.137472,0.140483,0.141337,0.146226,0.141016,0.141985,0.139535,0.151343,0.101087,0.101087,0.101087,0.101087,0.139117,0.11078,0.143629,0.167417,0.142311,0.133142,0.147268,0.138085,0.16188,0.166667,0.168174,0.123506,0.14486,0.139745,0.095434,0.12031,0.141123,0.183613,0.112319,0.153592,0.005348,0.005348,0.14006,0.146341,0.13964,0.005348,0.005348,0.142967,0.143077,0.14006,0.13891,0.160483,0.138806,0.194357,0.141985,0.142202,0.178674,0.140271,0.14059,0.17033,0.137269,0.137269,0.137269,0.138806,0.136966,0.152085,0.136966,0.13891,0.160761,0.160761,0.139013,0.136966,0.136966,0.14166,0.137676,0.136966,0.136966,0.138599,0.137168,0.136966,0.138393,0.136966,0.158568,0.136966,0.138599,0.097484,0.116981,0.110846,0.110846,0.110846,0.110846,0.110846,0.108963,0.12261,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.340659,0.263456,0.343173,0.235443,0.24734,0.226829,0.215278,0.242188,0.230483,0.213303,0.23192,0.245383,0.224096,0.196617,0.225728,0.222488,0.24031,0.258693,0.212571,0.241873,0.24734,0.214286,0.290625,0.202614,0.17782,0.211845,0.183432,0.24933,0.236943,0.24767,0.235443,0.220641,0.287481,0.16504,0.221692,0.259414,0.229064,0.191358,0.18656,0.300485,0.250674,0.237548,0.241245,0.220379,0.211604,0.246358,0.189409,0.207127,0.207127,0.207127,0.207127,0.207127,0.210646,0.216783,0.212329,0.231056,0.223289,0.187879,0.168478,0.241558,0.219858,0.143852,0.219599,0.215278,0.265335,0.111178,0.132009,0.238768,0.188832,0.214533,0.216279,0.226553,0.221957,0.225182,0.218566,0.219081,0.223558,0.24734,0.167719,0.167719,0.167719,0.167719,0.215777,0.183432,0.200647,0.216531,0.197662,0.170642,0.225182,0.214286,0.216279,0.214533,0.218824,0.236341,0.231631,0.194561,0.134588,0.16548,0.216531,0.276786,0.13788,0.193548,0.184707,0.184707,0.215527,0.229064,0.215029,0.184707,0.18999,0.224367,0.215029,0.218566,0.217799,0.216028,0.217544,0.273529,0.21831,0.21729,0.263456,0.230198,0.215777,0.24282,0.214039,0.213548,0.214039,0.214533,0.213548,0.228501,0.214781,0.206897,0.214039,0.214533,0.215278,0.214286,0.214039,0.228782,0.226002,0.215278,0.217544,0.214533,0.215278,0.216028,0.215527,0.214286,0.204846,0.228782,0.239382,0.219081,0.168784,0.203279,0.213303,0.203279,0.213303,0.203279,0.224638,0.20462,0.203501,0.213548,0.213303,0.147036,0.151343,0.149638,0.166369,0.151961,0.132196,0.130618,0.147268,0.132196,0.132196,0.147736,0.151961,0.132196,0.147619,0.167417,0.209932,0.210169,0.147971,0.132196,0.147971,0.148089,0.154229,0.154229,0.15952,0.153846,0.149638,0.148089,0.203947],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var16_df_closseness, by=list(var16_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var16_df_closseness, by=list(var16_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-e5a4dd43a4d043c29435" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-e5a4dd43a4d043c29435">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000118\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001242\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7e-05\" data-max=\"0.000381\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000897\" data-max=\"0.001845\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.3e-05\" data-max=\"0.000327\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022037\" data-max=\"0.024451\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.3e-05\" data-max=\"0.004088\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.231056\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.013023\" data-max=\"0.070884\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.166854\" data-max=\"0.343173\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.006171\" data-max=\"0.060759\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003883\" data-max=\"0.139638\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[0.000127,0.000123,0.000127,0.000129,0.000127,0.000129,0.000128,0.000131,0.000128,0.000128,0.000129,0.000118,0.000126,0.000128,0.000128,0.000124,0.00013,0.000128,0.00013,0.000126,0.000129,0.000127,0.000127],[2e-06,3e-06,null,null,null,null,0,null,null,null,null,2.2e-05,2e-06,0,0,4e-06,null,null,null,2e-06,null,1e-06,1e-06],[0.000867,0.000268,0.000757,0.000888,0.000804,0.000847,0.000821,0.001242,0.001054,0.000745,0.000814,0.000759,0.000911,0.000848,0.000824,0.000844,2.9e-05,0.000987,0.001166,0.000542,0.00096,0.000608,0.00077],[0.000208,0.000325,null,null,null,null,8.9e-05,null,null,null,null,0.000225,9e-05,0.000115,7.6e-05,0.00014,null,null,null,0.000381,null,0.000271,7e-05],[0.001267,0.000897,0.0013,0.001416,0.001157,0.001299,0.0013,0.001845,0.001562,0.001287,0.00133,0.001099,0.001167,0.001274,0.001281,0.001149,0.001096,0.001488,0.001832,0.001066,0.001391,0.001141,0.001189],[0.000327,0.00015,null,null,null,null,6.1e-05,null,null,null,null,0.000225,0.000156,4.6e-05,5.6e-05,0.000228,null,null,null,6.9e-05,null,3.3e-05,6e-05],[0.023584,0.022809,0.023694,0.023978,0.023637,0.023917,0.023878,0.024451,0.02388,0.023773,0.023917,0.022037,0.023333,0.023809,0.023835,0.023081,0.0241,0.023843,0.024228,0.023449,0.024015,0.023718,0.023714],[0.000474,0.00063,null,null,null,null,2.3e-05,null,null,null,null,0.004087,0.000438,4.1e-05,4.7e-05,0.000747,null,null,null,0.000352,null,0.00024,5.5e-05],[0.161268,0.049884,0.140802,0.165187,0.149518,0.157627,0.152776,0.231056,0.195996,0.138599,0.151343,0.141116,0.169368,0.157683,0.153355,0.157018,0.005348,0.183613,0.216783,0.100825,0.178503,0.113021,0.143287],[0.038735,0.060479,null,null,null,null,0.016573,null,null,null,null,0.041895,0.016605,0.021369,0.01403,0.025979,null,null,null,0.070884,null,0.050478,0.013023],[0.235646,0.166854,0.241873,0.263456,0.215278,0.241558,0.241817,0.343173,0.290625,0.239382,0.24734,0.204333,0.217082,0.236879,0.23816,0.213674,0.203947,0.276786,0.340659,0.198346,0.258693,0.212306,0.221211],[0.060759,0.027842,null,null,null,null,0.011333,null,null,null,null,0.04188,0.028943,0.008455,0.010423,0.042322,null,null,null,0.012735,null,0.006171,0.011134],[0.300585,0.099063,0.286154,0.343173,0.303922,0.323478,0.300896,0.391579,0.295238,0.29108,0.313659,0.266552,0.319611,0.301223,0.316849,0.295625,0.005348,0.306425,0.371257,0.194167,0.314189,0.234219,0.288627],[0.043014,0.125815,null,null,null,null,0.018146,null,null,null,null,0.070696,0.003883,0.033229,0.029748,0.023781,null,null,null,0.139638,null,0.102732,0.011577]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var16_df_closseness <- data.frame(
var16_incloseness,
var16_outcloseness,
var16_totalcloseness,
var16_incloseness_n,
var16_outcloseness_n,
var16_totalcloseness_n,
var16_centr_closeness) %>% round(6)

#Adding type
var16_df_closseness <-cbind(var16_df_closseness, V(var16)$TIPO1)

#Adding names
names(var16_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var16_df_closseness<-var16_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var16_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-468b47d9a565d452ae0d" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-468b47d9a565d452ae0d">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001242\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000598\" data-max=\"0.001845\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024451\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.231056\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.111178\" data-max=\"0.343173\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[0.00013,0.000129,0.000131,0.000128,0.000128,0.000128,0.000127,0.000127,0.000123,0.000127,0.000128,0.000128,0.000123,0.000124,0.000127,0.000127,0.000128,0.000129,0.000122,0.000127,0.000128,0.000124,0.000128,0.000118,0.000116,0.000127,0.000121,0.000128,0.000128,0.000128,0.000128,0.000127,0.000128,0.000121,0.000128,0.000124,0.000127,0.000125,0.000125,0.000127,0.000128,0.000127,0.000127,0.000127,0.000127,0.000128,0.000116,0.000124,0.000124,0.000124,0.000124,0.000124,0.000122,0.000126,0.000121,0.000128,0.000128,0.000121,0.000122,0.000129,0.00013,0.00012,0.000127,0.000127,0.000124,2.9e-05,0.000118,0.000127,0.000121,0.000127,0.000127,0.000128,0.000128,0.000128,0.000128,0.000127,0.000127,0.000129,0.000122,0.000122,0.000122,0.000122,0.000128,0.000124,0.000124,0.000128,0.000125,0.000116,0.000127,0.000127,0.000127,0.000117,0.000121,0.000127,0.000128,0.000124,0.000116,0.000123,0.000128,0.000128,0.000118,0.000125,0.000128,0.000128,0.000127,0.000128,0.000127,0.000128,0.000129,0.000127,0.000127,0.000128,0.000127,0.000128,0.000128,0.000127,0.000127,0.000127,0.000128,0.000128,0.000127,0.000128,0.000128,0.000127,0.000128,0.000128,0.000127,0.000127,0.000128,0.000126,0.000127,0.000127,0.000127,0.000127,0.000128,0.000128,0.000128,0.000127,0.000128,0.000127,0.000128,0.000127,0.000127,0.000127,0.000127,0.000128,0.000128,0.000128,0.000124,0.000126,0.000127,0.000126,0.000127,0.000126,0.000127,0.000118,0.000129,0.00013,0.00013,0.000119,0.000121,0.000124,0.000127,0.000127,0.000119,0.00012,0.000124,0.000119,0.000119,0.000124,0.000124,0.000119,0.000124,0.000126,0.000129,0.000129,0.000124,0.000119,0.000124,0.000124,0.000124,0.000124,0.000125,0.000124,0.000124,0.000121,0.00013],[0.001166,0.000888,0.001242,0.000882,0.000782,0.000779,0.000804,0.000976,0.000962,0.00075,0.000886,0.000748,0.000877,0.000847,0.00083,0.00082,0.000912,0.00096,0.000889,0.000757,0.000914,0.000859,0.001054,0.000887,0.000735,0.000613,0.000715,0.000814,0.000929,0.000946,0.000921,0.000779,0.001015,0.000587,0.000589,0.001001,0.000861,0.000692,0.000759,0.001142,0.000957,0.000974,0.000974,0.000768,0.000756,0.000909,0.000726,0.000824,0.000824,0.000824,0.000824,0.000824,0.000733,0.000773,0.000894,0.00082,0.000789,0.000758,0.000639,0.000847,2.9e-05,0.000612,0.000762,0.000757,0.001027,2.9e-05,2.9e-05,0.00081,0.000783,0.000739,0.000739,0.000755,0.00076,0.000786,0.000758,0.000763,0.00075,0.000814,0.000543,0.000543,0.000543,0.000543,0.000748,0.000596,0.000772,0.0009,0.000765,0.000716,0.000792,0.000742,0.00087,0.000896,0.000904,0.000664,0.000779,0.000751,0.000513,0.000647,0.000759,0.000987,0.000604,0.000826,2.9e-05,2.9e-05,0.000753,0.000787,0.000751,2.9e-05,2.9e-05,0.000769,0.000769,0.000753,0.000747,0.000863,0.000746,0.001045,0.000763,0.000765,0.000961,0.000754,0.000756,0.000916,0.000738,0.000738,0.000738,0.000746,0.000736,0.000818,0.000736,0.000747,0.000864,0.000864,0.000747,0.000736,0.000736,0.000762,0.00074,0.000736,0.000736,0.000745,0.000737,0.000736,0.000744,0.000736,0.000853,0.000736,0.000745,0.000524,0.000629,0.000596,0.000596,0.000596,0.000596,0.000596,0.000586,0.000659,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001832,0.001416,0.001845,0.001266,0.00133,0.00122,0.001157,0.001302,0.001239,0.001147,0.001247,0.001319,0.001205,0.001057,0.001214,0.001196,0.001292,0.001391,0.001143,0.0013,0.00133,0.001152,0.001562,0.001089,0.000956,0.001139,0.000986,0.00134,0.001274,0.001332,0.001266,0.001186,0.001546,0.000887,0.001192,0.001395,0.001232,0.001029,0.001003,0.001616,0.001348,0.001277,0.001297,0.001185,0.001138,0.001325,0.001018,0.001114,0.001114,0.001114,0.001114,0.001114,0.001133,0.001166,0.001142,0.001242,0.0012,0.00101,0.000906,0.001299,0.001182,0.000773,0.001181,0.001157,0.001427,0.000598,0.00071,0.001284,0.001015,0.001153,0.001163,0.001218,0.001193,0.001211,0.001175,0.001178,0.001202,0.00133,0.000902,0.000902,0.000902,0.000902,0.00116,0.000986,0.001079,0.001164,0.001063,0.000917,0.001211,0.001152,0.001163,0.001153,0.001176,0.001271,0.001245,0.001046,0.000724,0.00089,0.001164,0.001488,0.000741,0.001041,0.000993,0.000993,0.001159,0.001232,0.001156,0.000993,0.001021,0.001206,0.001156,0.001175,0.001171,0.001161,0.00117,0.001471,0.001174,0.001168,0.001416,0.001238,0.00116,0.001305,0.001151,0.001148,0.001151,0.001153,0.001148,0.001229,0.001155,0.001112,0.001151,0.001153,0.001157,0.001152,0.001151,0.00123,0.001215,0.001157,0.00117,0.001153,0.001157,0.001161,0.001159,0.001152,0.001101,0.00123,0.001287,0.001178,0.000907,0.001093,0.001147,0.001093,0.001147,0.001093,0.001208,0.0011,0.001094,0.001148,0.001147,0.000791,0.000814,0.000805,0.000894,0.000817,0.000711,0.000702,0.000792,0.000711,0.000711,0.000794,0.000817,0.000711,0.000794,0.0009,0.001129,0.00113,0.000796,0.000711,0.000796,0.000796,0.000829,0.000829,0.000858,0.000827,0.000805,0.000796,0.001096],[0.024228,0.023978,0.024451,0.02377,0.023877,0.023785,0.023637,0.023694,0.022904,0.023616,0.023843,0.023871,0.022789,0.023023,0.023679,0.023592,0.023843,0.024015,0.02268,0.023694,0.023852,0.023,0.02388,0.021937,0.021613,0.023562,0.022562,0.023883,0.023764,0.023825,0.023834,0.023688,0.02388,0.022529,0.02377,0.02298,0.023652,0.023338,0.023329,0.023709,0.023834,0.023643,0.023634,0.023694,0.023568,0.023837,0.021628,0.023117,0.023117,0.023117,0.023117,0.023117,0.022744,0.023464,0.022556,0.02377,0.02377,0.022477,0.022672,0.023917,0.024263,0.022402,0.023703,0.023694,0.023154,0.005348,0.021867,0.023652,0.022597,0.023667,0.023709,0.023773,0.023721,0.023737,0.023743,0.023694,0.023712,0.023917,0.022763,0.022763,0.022763,0.022763,0.023724,0.02314,0.023149,0.023721,0.023302,0.021661,0.023697,0.02367,0.023688,0.021839,0.022494,0.023646,0.023834,0.02306,0.021661,0.022901,0.023734,0.023843,0.022012,0.023169,0.023871,0.023871,0.023694,0.023764,0.023694,0.023871,0.023929,0.023709,0.023697,0.023743,0.023712,0.023718,0.023724,0.023694,0.023694,0.023694,0.023776,0.02384,0.023694,0.023715,0.023718,0.023703,0.023718,0.023715,0.023703,0.023709,0.023715,0.023423,0.023694,0.023706,0.023712,0.023709,0.023718,0.023794,0.023816,0.023694,0.023715,0.023688,0.023718,0.023688,0.023685,0.023685,0.023547,0.023898,0.023773,0.023746,0.023037,0.023405,0.0237,0.023405,0.0237,0.023405,0.023667,0.021994,0.02396,0.024266,0.02426,0.022148,0.022434,0.023045,0.023646,0.023592,0.02208,0.022353,0.022991,0.02208,0.02208,0.023031,0.023091,0.02208,0.023031,0.023479,0.024047,0.024025,0.023017,0.02208,0.023017,0.023023,0.023071,0.023071,0.023262,0.023097,0.023011,0.022429,0.0241],[0.216783,0.165187,0.231056,0.164021,0.14554,0.144973,0.149518,0.181463,0.178846,0.139535,0.164748,0.139117,0.163158,0.157627,0.154357,0.152584,0.169553,0.178503,0.165333,0.140802,0.170018,0.159794,0.195996,0.16504,0.136664,0.113971,0.132952,0.151343,0.172862,0.17597,0.171271,0.14486,0.188832,0.109155,0.109605,0.186186,0.160207,0.128631,0.14123,0.212329,0.17799,0.18111,0.18111,0.142857,0.140696,0.169091,0.135076,0.153213,0.153213,0.153213,0.153213,0.153213,0.136364,0.143852,0.166369,0.152459,0.146688,0.141016,0.118926,0.157627,0.005348,0.113831,0.141768,0.140802,0.190965,0.005376,0.005405,0.150607,0.145654,0.137472,0.137472,0.140483,0.141337,0.146226,0.141016,0.141985,0.139535,0.151343,0.101087,0.101087,0.101087,0.101087,0.139117,0.11078,0.143629,0.167417,0.142311,0.133142,0.147268,0.138085,0.16188,0.166667,0.168174,0.123506,0.14486,0.139745,0.095434,0.12031,0.141123,0.183613,0.112319,0.153592,0.005348,0.005348,0.14006,0.146341,0.13964,0.005348,0.005348,0.142967,0.143077,0.14006,0.13891,0.160483,0.138806,0.194357,0.141985,0.142202,0.178674,0.140271,0.14059,0.17033,0.137269,0.137269,0.137269,0.138806,0.136966,0.152085,0.136966,0.13891,0.160761,0.160761,0.139013,0.136966,0.136966,0.14166,0.137676,0.136966,0.136966,0.138599,0.137168,0.136966,0.138393,0.136966,0.158568,0.136966,0.138599,0.097484,0.116981,0.110846,0.110846,0.110846,0.110846,0.110846,0.108963,0.12261,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.340659,0.263456,0.343173,0.235443,0.24734,0.226829,0.215278,0.242188,0.230483,0.213303,0.23192,0.245383,0.224096,0.196617,0.225728,0.222488,0.24031,0.258693,0.212571,0.241873,0.24734,0.214286,0.290625,0.202614,0.17782,0.211845,0.183432,0.24933,0.236943,0.24767,0.235443,0.220641,0.287481,0.16504,0.221692,0.259414,0.229064,0.191358,0.18656,0.300485,0.250674,0.237548,0.241245,0.220379,0.211604,0.246358,0.189409,0.207127,0.207127,0.207127,0.207127,0.207127,0.210646,0.216783,0.212329,0.231056,0.223289,0.187879,0.168478,0.241558,0.219858,0.143852,0.219599,0.215278,0.265335,0.111178,0.132009,0.238768,0.188832,0.214533,0.216279,0.226553,0.221957,0.225182,0.218566,0.219081,0.223558,0.24734,0.167719,0.167719,0.167719,0.167719,0.215777,0.183432,0.200647,0.216531,0.197662,0.170642,0.225182,0.214286,0.216279,0.214533,0.218824,0.236341,0.231631,0.194561,0.134588,0.16548,0.216531,0.276786,0.13788,0.193548,0.184707,0.184707,0.215527,0.229064,0.215029,0.184707,0.18999,0.224367,0.215029,0.218566,0.217799,0.216028,0.217544,0.273529,0.21831,0.21729,0.263456,0.230198,0.215777,0.24282,0.214039,0.213548,0.214039,0.214533,0.213548,0.228501,0.214781,0.206897,0.214039,0.214533,0.215278,0.214286,0.214039,0.228782,0.226002,0.215278,0.217544,0.214533,0.215278,0.216028,0.215527,0.214286,0.204846,0.228782,0.239382,0.219081,0.168784,0.203279,0.213303,0.203279,0.213303,0.203279,0.224638,0.20462,0.203501,0.213548,0.213303,0.147036,0.151343,0.149638,0.166369,0.151961,0.132196,0.130618,0.147268,0.132196,0.132196,0.147736,0.151961,0.132196,0.147619,0.167417,0.209932,0.210169,0.147971,0.132196,0.147971,0.148089,0.154229,0.154229,0.15952,0.153846,0.149638,0.148089,0.203947],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var16_df_closseness, by=list(var16_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var16_df_closseness, by=list(var16_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-d7a7365a0f5310dbd3e9" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-d7a7365a0f5310dbd3e9">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000122\" data-max=\"0.000127\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000485\" data-max=\"0.000741\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00023\" data-max=\"0.000377\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00099\" data-max=\"0.001208\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000133\" data-max=\"0.000215\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022682\" data-max=\"0.023727\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000213\" data-max=\"0.002076\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.090111\" data-max=\"0.137906\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.042705\" data-max=\"0.070178\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.184156\" data-max=\"0.224699\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.024664\" data-max=\"0.039907\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.173776\" data-max=\"0.2725\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.079091\" data-max=\"0.135432\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[0.000127,0.000122],[1e-06,1.1e-05],[0.000741,0.000485],[0.00023,0.000377],[0.001208,0.00099],[0.000133,0.000215],[0.023727,0.022682],[0.000213,0.002076],[0.137906,0.090111],[0.042705,0.070178],[0.224699,0.184156],[0.024664,0.039907],[0.2725,0.173776],[0.079091,0.135432]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var16_df_closseness <- data.frame(
var16_incloseness,
var16_outcloseness,
var16_totalcloseness,
var16_incloseness_n,
var16_outcloseness_n,
var16_totalcloseness_n,
var16_centr_closeness) %>% round(6)

#Adding type
var16_df_closseness <-cbind(var16_df_closseness, V(var16)$TIPO2)

#Adding names
names(var16_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var16_df_closseness<-var16_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var16_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-fbf5c9dff908feacdb89" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-fbf5c9dff908feacdb89">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000131\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001242\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000598\" data-max=\"0.001845\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.024451\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.231056\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.111178\" data-max=\"0.343173\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[0.00013,0.000129,0.000131,0.000128,0.000128,0.000128,0.000127,0.000127,0.000123,0.000127,0.000128,0.000128,0.000123,0.000124,0.000127,0.000127,0.000128,0.000129,0.000122,0.000127,0.000128,0.000124,0.000128,0.000118,0.000116,0.000127,0.000121,0.000128,0.000128,0.000128,0.000128,0.000127,0.000128,0.000121,0.000128,0.000124,0.000127,0.000125,0.000125,0.000127,0.000128,0.000127,0.000127,0.000127,0.000127,0.000128,0.000116,0.000124,0.000124,0.000124,0.000124,0.000124,0.000122,0.000126,0.000121,0.000128,0.000128,0.000121,0.000122,0.000129,0.00013,0.00012,0.000127,0.000127,0.000124,2.9e-05,0.000118,0.000127,0.000121,0.000127,0.000127,0.000128,0.000128,0.000128,0.000128,0.000127,0.000127,0.000129,0.000122,0.000122,0.000122,0.000122,0.000128,0.000124,0.000124,0.000128,0.000125,0.000116,0.000127,0.000127,0.000127,0.000117,0.000121,0.000127,0.000128,0.000124,0.000116,0.000123,0.000128,0.000128,0.000118,0.000125,0.000128,0.000128,0.000127,0.000128,0.000127,0.000128,0.000129,0.000127,0.000127,0.000128,0.000127,0.000128,0.000128,0.000127,0.000127,0.000127,0.000128,0.000128,0.000127,0.000128,0.000128,0.000127,0.000128,0.000128,0.000127,0.000127,0.000128,0.000126,0.000127,0.000127,0.000127,0.000127,0.000128,0.000128,0.000128,0.000127,0.000128,0.000127,0.000128,0.000127,0.000127,0.000127,0.000127,0.000128,0.000128,0.000128,0.000124,0.000126,0.000127,0.000126,0.000127,0.000126,0.000127,0.000118,0.000129,0.00013,0.00013,0.000119,0.000121,0.000124,0.000127,0.000127,0.000119,0.00012,0.000124,0.000119,0.000119,0.000124,0.000124,0.000119,0.000124,0.000126,0.000129,0.000129,0.000124,0.000119,0.000124,0.000124,0.000124,0.000124,0.000125,0.000124,0.000124,0.000121,0.00013],[0.001166,0.000888,0.001242,0.000882,0.000782,0.000779,0.000804,0.000976,0.000962,0.00075,0.000886,0.000748,0.000877,0.000847,0.00083,0.00082,0.000912,0.00096,0.000889,0.000757,0.000914,0.000859,0.001054,0.000887,0.000735,0.000613,0.000715,0.000814,0.000929,0.000946,0.000921,0.000779,0.001015,0.000587,0.000589,0.001001,0.000861,0.000692,0.000759,0.001142,0.000957,0.000974,0.000974,0.000768,0.000756,0.000909,0.000726,0.000824,0.000824,0.000824,0.000824,0.000824,0.000733,0.000773,0.000894,0.00082,0.000789,0.000758,0.000639,0.000847,2.9e-05,0.000612,0.000762,0.000757,0.001027,2.9e-05,2.9e-05,0.00081,0.000783,0.000739,0.000739,0.000755,0.00076,0.000786,0.000758,0.000763,0.00075,0.000814,0.000543,0.000543,0.000543,0.000543,0.000748,0.000596,0.000772,0.0009,0.000765,0.000716,0.000792,0.000742,0.00087,0.000896,0.000904,0.000664,0.000779,0.000751,0.000513,0.000647,0.000759,0.000987,0.000604,0.000826,2.9e-05,2.9e-05,0.000753,0.000787,0.000751,2.9e-05,2.9e-05,0.000769,0.000769,0.000753,0.000747,0.000863,0.000746,0.001045,0.000763,0.000765,0.000961,0.000754,0.000756,0.000916,0.000738,0.000738,0.000738,0.000746,0.000736,0.000818,0.000736,0.000747,0.000864,0.000864,0.000747,0.000736,0.000736,0.000762,0.00074,0.000736,0.000736,0.000745,0.000737,0.000736,0.000744,0.000736,0.000853,0.000736,0.000745,0.000524,0.000629,0.000596,0.000596,0.000596,0.000596,0.000596,0.000586,0.000659,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001832,0.001416,0.001845,0.001266,0.00133,0.00122,0.001157,0.001302,0.001239,0.001147,0.001247,0.001319,0.001205,0.001057,0.001214,0.001196,0.001292,0.001391,0.001143,0.0013,0.00133,0.001152,0.001562,0.001089,0.000956,0.001139,0.000986,0.00134,0.001274,0.001332,0.001266,0.001186,0.001546,0.000887,0.001192,0.001395,0.001232,0.001029,0.001003,0.001616,0.001348,0.001277,0.001297,0.001185,0.001138,0.001325,0.001018,0.001114,0.001114,0.001114,0.001114,0.001114,0.001133,0.001166,0.001142,0.001242,0.0012,0.00101,0.000906,0.001299,0.001182,0.000773,0.001181,0.001157,0.001427,0.000598,0.00071,0.001284,0.001015,0.001153,0.001163,0.001218,0.001193,0.001211,0.001175,0.001178,0.001202,0.00133,0.000902,0.000902,0.000902,0.000902,0.00116,0.000986,0.001079,0.001164,0.001063,0.000917,0.001211,0.001152,0.001163,0.001153,0.001176,0.001271,0.001245,0.001046,0.000724,0.00089,0.001164,0.001488,0.000741,0.001041,0.000993,0.000993,0.001159,0.001232,0.001156,0.000993,0.001021,0.001206,0.001156,0.001175,0.001171,0.001161,0.00117,0.001471,0.001174,0.001168,0.001416,0.001238,0.00116,0.001305,0.001151,0.001148,0.001151,0.001153,0.001148,0.001229,0.001155,0.001112,0.001151,0.001153,0.001157,0.001152,0.001151,0.00123,0.001215,0.001157,0.00117,0.001153,0.001157,0.001161,0.001159,0.001152,0.001101,0.00123,0.001287,0.001178,0.000907,0.001093,0.001147,0.001093,0.001147,0.001093,0.001208,0.0011,0.001094,0.001148,0.001147,0.000791,0.000814,0.000805,0.000894,0.000817,0.000711,0.000702,0.000792,0.000711,0.000711,0.000794,0.000817,0.000711,0.000794,0.0009,0.001129,0.00113,0.000796,0.000711,0.000796,0.000796,0.000829,0.000829,0.000858,0.000827,0.000805,0.000796,0.001096],[0.024228,0.023978,0.024451,0.02377,0.023877,0.023785,0.023637,0.023694,0.022904,0.023616,0.023843,0.023871,0.022789,0.023023,0.023679,0.023592,0.023843,0.024015,0.02268,0.023694,0.023852,0.023,0.02388,0.021937,0.021613,0.023562,0.022562,0.023883,0.023764,0.023825,0.023834,0.023688,0.02388,0.022529,0.02377,0.02298,0.023652,0.023338,0.023329,0.023709,0.023834,0.023643,0.023634,0.023694,0.023568,0.023837,0.021628,0.023117,0.023117,0.023117,0.023117,0.023117,0.022744,0.023464,0.022556,0.02377,0.02377,0.022477,0.022672,0.023917,0.024263,0.022402,0.023703,0.023694,0.023154,0.005348,0.021867,0.023652,0.022597,0.023667,0.023709,0.023773,0.023721,0.023737,0.023743,0.023694,0.023712,0.023917,0.022763,0.022763,0.022763,0.022763,0.023724,0.02314,0.023149,0.023721,0.023302,0.021661,0.023697,0.02367,0.023688,0.021839,0.022494,0.023646,0.023834,0.02306,0.021661,0.022901,0.023734,0.023843,0.022012,0.023169,0.023871,0.023871,0.023694,0.023764,0.023694,0.023871,0.023929,0.023709,0.023697,0.023743,0.023712,0.023718,0.023724,0.023694,0.023694,0.023694,0.023776,0.02384,0.023694,0.023715,0.023718,0.023703,0.023718,0.023715,0.023703,0.023709,0.023715,0.023423,0.023694,0.023706,0.023712,0.023709,0.023718,0.023794,0.023816,0.023694,0.023715,0.023688,0.023718,0.023688,0.023685,0.023685,0.023547,0.023898,0.023773,0.023746,0.023037,0.023405,0.0237,0.023405,0.0237,0.023405,0.023667,0.021994,0.02396,0.024266,0.02426,0.022148,0.022434,0.023045,0.023646,0.023592,0.02208,0.022353,0.022991,0.02208,0.02208,0.023031,0.023091,0.02208,0.023031,0.023479,0.024047,0.024025,0.023017,0.02208,0.023017,0.023023,0.023071,0.023071,0.023262,0.023097,0.023011,0.022429,0.0241],[0.216783,0.165187,0.231056,0.164021,0.14554,0.144973,0.149518,0.181463,0.178846,0.139535,0.164748,0.139117,0.163158,0.157627,0.154357,0.152584,0.169553,0.178503,0.165333,0.140802,0.170018,0.159794,0.195996,0.16504,0.136664,0.113971,0.132952,0.151343,0.172862,0.17597,0.171271,0.14486,0.188832,0.109155,0.109605,0.186186,0.160207,0.128631,0.14123,0.212329,0.17799,0.18111,0.18111,0.142857,0.140696,0.169091,0.135076,0.153213,0.153213,0.153213,0.153213,0.153213,0.136364,0.143852,0.166369,0.152459,0.146688,0.141016,0.118926,0.157627,0.005348,0.113831,0.141768,0.140802,0.190965,0.005376,0.005405,0.150607,0.145654,0.137472,0.137472,0.140483,0.141337,0.146226,0.141016,0.141985,0.139535,0.151343,0.101087,0.101087,0.101087,0.101087,0.139117,0.11078,0.143629,0.167417,0.142311,0.133142,0.147268,0.138085,0.16188,0.166667,0.168174,0.123506,0.14486,0.139745,0.095434,0.12031,0.141123,0.183613,0.112319,0.153592,0.005348,0.005348,0.14006,0.146341,0.13964,0.005348,0.005348,0.142967,0.143077,0.14006,0.13891,0.160483,0.138806,0.194357,0.141985,0.142202,0.178674,0.140271,0.14059,0.17033,0.137269,0.137269,0.137269,0.138806,0.136966,0.152085,0.136966,0.13891,0.160761,0.160761,0.139013,0.136966,0.136966,0.14166,0.137676,0.136966,0.136966,0.138599,0.137168,0.136966,0.138393,0.136966,0.158568,0.136966,0.138599,0.097484,0.116981,0.110846,0.110846,0.110846,0.110846,0.110846,0.108963,0.12261,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.340659,0.263456,0.343173,0.235443,0.24734,0.226829,0.215278,0.242188,0.230483,0.213303,0.23192,0.245383,0.224096,0.196617,0.225728,0.222488,0.24031,0.258693,0.212571,0.241873,0.24734,0.214286,0.290625,0.202614,0.17782,0.211845,0.183432,0.24933,0.236943,0.24767,0.235443,0.220641,0.287481,0.16504,0.221692,0.259414,0.229064,0.191358,0.18656,0.300485,0.250674,0.237548,0.241245,0.220379,0.211604,0.246358,0.189409,0.207127,0.207127,0.207127,0.207127,0.207127,0.210646,0.216783,0.212329,0.231056,0.223289,0.187879,0.168478,0.241558,0.219858,0.143852,0.219599,0.215278,0.265335,0.111178,0.132009,0.238768,0.188832,0.214533,0.216279,0.226553,0.221957,0.225182,0.218566,0.219081,0.223558,0.24734,0.167719,0.167719,0.167719,0.167719,0.215777,0.183432,0.200647,0.216531,0.197662,0.170642,0.225182,0.214286,0.216279,0.214533,0.218824,0.236341,0.231631,0.194561,0.134588,0.16548,0.216531,0.276786,0.13788,0.193548,0.184707,0.184707,0.215527,0.229064,0.215029,0.184707,0.18999,0.224367,0.215029,0.218566,0.217799,0.216028,0.217544,0.273529,0.21831,0.21729,0.263456,0.230198,0.215777,0.24282,0.214039,0.213548,0.214039,0.214533,0.213548,0.228501,0.214781,0.206897,0.214039,0.214533,0.215278,0.214286,0.214039,0.228782,0.226002,0.215278,0.217544,0.214533,0.215278,0.216028,0.215527,0.214286,0.204846,0.228782,0.239382,0.219081,0.168784,0.203279,0.213303,0.203279,0.213303,0.203279,0.224638,0.20462,0.203501,0.213548,0.213303,0.147036,0.151343,0.149638,0.166369,0.151961,0.132196,0.130618,0.147268,0.132196,0.132196,0.147736,0.151961,0.132196,0.147619,0.167417,0.209932,0.210169,0.147971,0.132196,0.147971,0.148089,0.154229,0.154229,0.15952,0.153846,0.149638,0.148089,0.203947],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var16_df_closseness, by=list(var16_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var16_df_closseness, by=list(var16_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-caae508c5a78ea944972" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-caae508c5a78ea944972">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000122\" data-max=\"0.000128\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000481\" data-max=\"0.000859\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000106\" data-max=\"0.000378\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000984\" data-max=\"0.001302\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8.2e-05\" data-max=\"0.000208\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022665\" data-max=\"0.023828\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"4.7e-05\" data-max=\"0.002085\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.089457\" data-max=\"0.159825\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.019743\" data-max=\"0.07037\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.182992\" data-max=\"0.242125\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.015216\" data-max=\"0.03877\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172406\" data-max=\"0.308329\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.029295\" data-max=\"0.135782\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[0.000128,0.000127,0.000122],[0,1e-06,1.1e-05],[0.000859,0.000723,0.000481],[0.000106,0.000238,0.000378],[0.001302,0.001196,0.000984],[8.2e-05,0.000136,0.000208],[0.023828,0.023714,0.022665],[4.7e-05,0.000224,0.002085],[0.159825,0.134442,0.089457],[0.019743,0.044186,0.07037],[0.242125,0.222452,0.182992],[0.015216,0.025288,0.03877],[0.308329,0.266869,0.172406],[0.029295,0.082605,0.135782]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var16_data.RData")
```

