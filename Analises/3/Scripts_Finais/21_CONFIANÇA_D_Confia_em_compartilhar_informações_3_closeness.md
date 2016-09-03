# SNA Closeness 21_CONFIANÇA D) Confia em compartilhar informações internas com este serviço. (var19) 
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 21_CONFIANÇA D) Confia em compartilhar informações internas com este serviço. (var19) 

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/var19_data.RData")
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
#var19<-simplify(var19) #Simplify
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
V(var19)$incloseness <- closeness(var19, mode = "in", weights = E(var19)$var19) %>% round(6)
V(var19)$outcloseness <- closeness(var19, mode = "out", weights = E(var19)$var19) %>% round(6)
V(var19)$totalcloseness <- closeness(var19, mode = "total", weights = E(var19)$var19) %>% round(4)
```

###Saving to Environment

```r
var19_incloseness<- closeness(var19, mode = "in", weights = E(var19)$var19) %>% round(6)
var19_outcloseness<- closeness(var19, mode = "out", weights = E(var19)$var19) %>% round(6)
var19_totalcloseness<- closeness(var19, mode = "total", weights = E(var19)$var19) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var19_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0001250 0.0001270 0.0001254 0.0001280 0.0001330
```

```r
sd(var19_incloseness)
```

```
## [1] 7.76893e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var19)$incloseness<-closeness(var19, weights = E(var19)$var19, mode="in")

#Get Variable
V(var19)$var19_color_degree<-round(V(var19)$incloseness,6)

#Creating brewer pallette
vertex_var19_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var19)$var19_color_degree)), "RdBu"))(
            length(unique(V(var19)$var19_color_degree)))

#Saving as Vertex properties 
V(var19)$vertex_var19_color_degree<-
  vertex_var19_color_degree[as.numeric(
  cut(V(var19)$var19_color_degree,
      breaks=length(unique(V(var19)$var19_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var19, es=E(var19), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var19))
maxC <- rep(Inf, vcount(var19))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var19, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var19)$weight)


#PLotting
plot(var19, 
     layout=co,
     edge.color=V(var19)$vertex_var19_color_degree[edge.start],
     edge.arrow.size=closeness(var19, weights = E(var19)$var19, mode="in"),
     edge.width=E(var19)$weight/mean(E(var19)$weight),
     edge.curved = TRUE,
     vertex.color=V(var19)$vertex_var19_color_degree,
     vertex.size=closeness(var19, weights = E(var19)$var19, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var19,"LABEL_COR"),
     vertex.label.cex=(closeness(var19, weights = E(var19)$var19, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var19)$var19_color_degree
b<-V(var19)$vertex_var19_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 21_CONFIANÇA D) Confia em compartilhar informações internas com este serviço. (var19) ", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var19, mode="in", weights = E(var19)$var19)), 
             sd(closeness(var19, mode="in", weights = E(var19)$var19))
             )
       )
```

![](21_CONFIANÇA_D_Confia_em_compartilhar_informações_3_closeness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var19_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0006410 0.0007500 0.0006411 0.0008280 0.0011830
```

```r
sd(var19_outcloseness)
```

```
## [1] 0.0003291916
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var19)$outcloseness<-closeness(var19, weights = E(var19)$var19, mode="out")

#Get Variable
V(var19)$var19_color_degree<-round(V(var19)$outcloseness,6)

#Creating brewer pallette
vertex_var19_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var19)$var19_color_degree)), "RdBu"))(
            length(unique(V(var19)$var19_color_degree)))

#Saving as Vertex properties 
V(var19)$vertex_var19_color_degree<-
  vertex_var19_color_degree[as.numeric(
  cut(V(var19)$var19_color_degree,
      breaks=length(unique(V(var19)$var19_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var19, es=E(var19), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var19))
maxC <- rep(Inf, vcount(var19))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var19, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var19)$weight)


#PLotting
plot(var19, 
     layout=co,
     edge.color=V(var19)$vertex_var19_color_degree[edge.start],
     edge.arrow.size=closeness(var19, weights = E(var19)$var19, mode="out"),
     edge.width=E(var19)$weight/2*mean(E(var19)$weight),
     edge.curved = TRUE,
     vertex.color=V(var19)$vertex_var19_color_degree,
     vertex.size=closeness(var19, weights = E(var19)$var19, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var19,"LABEL_COR"),
     vertex.label.cex=closeness(var19, weights = E(var19)$var19, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var19)$var19_color_degree
b<-V(var19)$vertex_var19_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 21_CONFIANÇA D) Confia em compartilhar informações internas com este serviço. (var19) ", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var19, mode="out", weights = E(var19)$var19)), 
             sd(closeness(var19, mode="out", weights = E(var19)$var19))
             )
       )
```

![](21_CONFIANÇA_D_Confia_em_compartilhar_informações_3_closeness_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var19_totalcloseness)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000698 0.001043 0.001175 0.001182 0.001317 0.001919
```

```r
sd(var19_totalcloseness)
```

```
## [1] 0.0002199841
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var19)$allcloseness<-closeness(var19, weights = E(var19)$var19, mode="all")

#Get Variable
V(var19)$var19_color_degree<-round(V(var19)$allcloseness,6)

#Creating brewer pallette
vertex_var19_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var19)$var19_color_degree)), "RdBu"))(
            length(unique(V(var19)$var19_color_degree)))

#Saving as Vertex properties 
V(var19)$vertex_var19_color_degree<-
  vertex_var19_color_degree[as.numeric(
  cut(V(var19)$var19_color_degree,
      breaks=length(unique(V(var19)$var19_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var19, es=E(var19), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var19))
maxC <- rep(Inf, vcount(var19))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var19, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var19)$weight)


#PLotting
plot(var19, 
     layout=co,
     edge.color=V(var19)$vertex_var19_color_degree[edge.start],
     edge.arrow.size=closeness(var19, weights = E(var19)$var19, mode="all"),
     edge.width=E(var19)$weight/2*mean(E(var19)$weight),
     edge.curved = TRUE,
     vertex.color=V(var19)$vertex_var19_color_degree,
     vertex.size=closeness(var19, weights = E(var19)$var19, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var19,"LABEL_COR"),
     vertex.label.cex=(closeness(var19, weights = E(var19)$var19, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var19)$var19_color_degree
b<-V(var19)$vertex_var19_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 21_CONFIANÇA D) Confia em compartilhar informações internas com este serviço. (var19) ", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var19, mode="all", weights = E(var19)$var19)), 
             sd(closeness(var19, mode="all", weights = E(var19)$var19))
             )
       )
```

![](21_CONFIANÇA_D_Confia_em_compartilhar_informações_3_closeness_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var19)$incloseness_n <- closeness(var19, mode = "in",, weights = E(var19)$var19, normalized = T) %>% round(10)
V(var19)$outcloseness_n <- closeness(var19, mode = "out", normalized = T, weights = E(var19)$var19) %>% round(6)
V(var19)$totalcloseness_n <- closeness(var19, mode = "total", normalized = T, weights = E(var19)$var19) %>% round(6)
```

###Saving to Environment

```r
var19_incloseness_n<- closeness(var19, mode = "in", normalized = T, weights = E(var19)$var19) %>% round(6)
var19_outcloseness_n<- closeness(var19, mode = "out", normalized = T, weights = E(var19)$var19) %>% round(6)
var19_totalcloseness_n<- closeness(var19, mode = "total", normalized = T, weights = E(var19)$var19) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var19_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.023210 0.023600 0.023330 0.023740 0.024790
```

```r
sd(var19_incloseness_n)
```

```
## [1] 0.00144708
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var19)$incloseness_n<-closeness(var19, weights = E(var19)$var19, mode="in", normalized = T)

#Get Variable
V(var19)$var19_color_degree<-round(V(var19)$incloseness_n,6)

#Creating brewer pallette
vertex_var19_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var19)$var19_color_degree)), "RdBu"))(
            length(unique(V(var19)$var19_color_degree)))

#Saving as Vertex properties 
V(var19)$vertex_var19_color_degree<-
  vertex_var19_color_degree[as.numeric(
  cut(V(var19)$var19_color_degree,
      breaks=length(unique(V(var19)$var19_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var19, es=E(var19), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var19))
maxC <- rep(Inf, vcount(var19))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var19, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var19)$weight)


#PLotting
plot(var19, 
     layout=co,
     edge.color=V(var19)$vertex_var19_color_degree[edge.start],
     edge.arrow.size=closeness(var19, weights = E(var19)$var19, mode="in",normalized = T),
     edge.width=E(var19)$weight/10*mean(E(var19)$weight),
     edge.curved = TRUE,
     vertex.color=V(var19)$vertex_var19_color_degree,
     vertex.size=(closeness(var19, weights = E(var19)$var19, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var19,"LABEL_COR"),
     vertex.label.cex=closeness(var19, weights = E(var19)$var19, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var19)$var19_color_degree
b<-V(var19)$vertex_var19_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 21_CONFIANÇA D) Confia em compartilhar informações internas com este serviço. (var19) ", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var19, mode="in", weights = E(var19)$var19, normalized = T)), 
             sd(closeness(var19, mode="in", weights = E(var19)$var19, normalized = T))
             )
       )
```

![](21_CONFIANÇA_D_Confia_em_compartilhar_informações_3_closeness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var19_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.119200 0.139400 0.119200 0.154000 0.220100
```

```r
sd(var19_outcloseness_n)
```

```
## [1] 0.06124846
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var19)$outcloseness_n<-closeness(var19, weights = E(var19)$var19, mode="out", normalized = T)

#Get Variable
V(var19)$var19_color_degree<-round(V(var19)$outcloseness_n,6)

#Creating brewer pallette
vertex_var19_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var19)$var19_color_degree)), "RdBu"))(
            length(unique(V(var19)$var19_color_degree)))

#Saving as Vertex properties 
V(var19)$vertex_var19_color_degree<-
  vertex_var19_color_degree[as.numeric(
  cut(V(var19)$var19_color_degree,
      breaks=length(unique(V(var19)$var19_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var19, es=E(var19), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var19))
maxC <- rep(Inf, vcount(var19))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var19, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var19)$weight)


#PLotting
plot(var19, 
     layout=co,
     edge.color=V(var19)$vertex_var19_color_degree[edge.start],
     edge.arrow.size=closeness(var19, weights = E(var19)$var19, mode="out",normalized = T),
     edge.width=E(var19)$weight/10*mean(E(var19)$weight),
     edge.curved = TRUE,
     vertex.color=V(var19)$vertex_var19_color_degree,
     vertex.size=(closeness(var19, weights = E(var19)$var19, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var19,"LABEL_COR"),
     vertex.label.cex=closeness(var19, weights = E(var19)$var19, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var19)$var19_color_degree
b<-V(var19)$vertex_var19_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 21_CONFIANÇA D) Confia em compartilhar informações internas com este serviço. (var19) ", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var19, mode="out", weights = E(var19)$var19, normalized = T)), 
             sd(closeness(var19, mode="out", weights = E(var19)$var19, normalized = T))
             )
       )
```

![](21_CONFIANÇA_D_Confia_em_compartilhar_informações_3_closeness_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var19_totalcloseness_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1298  0.1940  0.2186  0.2199  0.2449  0.3570
```

```r
sd(var19_totalcloseness_n)
```

```
## [1] 0.04091557
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var19)$allcloseness_n<-closeness(var19, weights = E(var19)$var19, mode="all", normalized = T)

#Get Variable
V(var19)$var19_color_degree<-round(V(var19)$allcloseness_n,6)

#Creating brewer pallette
vertex_var19_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var19)$var19_color_degree)), "RdBu"))(
            length(unique(V(var19)$var19_color_degree)))

#Saving as Vertex properties 
V(var19)$vertex_var19_color_degree<-
  vertex_var19_color_degree[as.numeric(
  cut(V(var19)$var19_color_degree,
      breaks=length(unique(V(var19)$var19_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var19, es=E(var19), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var19))
maxC <- rep(Inf, vcount(var19))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var19, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var19)$weight)


#PLotting
plot(var19, 
     layout=co,
     edge.color=V(var19)$vertex_var19_color_degree[edge.start],
     edge.arrow.size=closeness(var19, weights = E(var19)$var19, mode="all",normalized = T),
     edge.width=E(var19)$weight/10*mean(E(var19)$weight),
     edge.curved = TRUE,
     vertex.color=V(var19)$vertex_var19_color_degree,
     vertex.size=(closeness(var19, weights = E(var19)$var19, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var19,"LABEL_COR"),
     vertex.label.cex=closeness(var19, weights = E(var19)$var19, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var19)$var19_color_degree
b<-V(var19)$vertex_var19_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 21_CONFIANÇA D) Confia em compartilhar informações internas com este serviço. (var19) ", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var19, mode="all", weights = E(var19)$var19, normalized = T)), 
             sd(closeness(var19, mode="all", weights = E(var19)$var19, normalized = T))
             )
       )
```

![](21_CONFIANÇA_D_Confia_em_compartilhar_informações_3_closeness_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var19)$incloseness_n <- closeness(var19, weights = E(var19)$var19, mode = "in", normalized = T) %>% round(6)
V(var19)$outcloseness_n <- closeness(var19, weights = E(var19)$var19, mode = "out", normalized = T) %>% round(6)
V(var19)$totalcloseness_n <- closeness(var19, weights = E(var19)$var19, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var19)$var19_centr_closeness<- centralization.closeness(var19)$res
var19_centr_closeness<- centralization.closeness(var19)$res
var19_centr_closeness_all<- centralization.closeness(var19)
```

###Centralization

```r
var19_centr_closeness_all$centralization
```

```
## [1] 0.1625197
```

###Theoretical Max

```r
var19_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var19)$var19_centr_closeness<- centralization.closeness(var19)$res

#Get Variable
V(var19)$var19_color_degree<-round(V(var19)$var19_centr_closeness,6)

#Creating brewer pallette
vertex_var19_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var19)$var19_color_degree)), "Spectral"))(
            length(unique(V(var19)$var19_color_degree)))

#Saving as Vertex properties 
V(var19)$vertex_var19_color_degree<-
  vertex_var19_color_degree[as.numeric(
  cut(V(var19)$var19_color_degree,
      breaks=length(unique(V(var19)$var19_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var19, es=E(var19), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var19))
maxC <- rep(Inf, vcount(var19))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var19, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var19)$weight)


#PLotting
plot(var19, 
     layout=co,
     edge.color=V(var19)$vertex_var19_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var19)$res,
     edge.width=E(var19)$weight/10*mean(E(var19)$weight),
     edge.curved = TRUE,
     vertex.color=V(var19)$vertex_var19_color_degree,
     vertex.size=centralization.closeness(var19)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var19,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var19)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var19)$var19_color_degree
b<-V(var19)$vertex_var19_color_degree
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
  title("Network Centralization Closeness - 21_CONFIANÇA D) Confia em compartilhar informações internas com este serviço. (var19) ", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var19)$res), 
             sd(centralization.closeness(var19)$res)
             )
       )
```

![](21_CONFIANÇA_D_Confia_em_compartilhar_informações_3_closeness_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var19_incloseness<- closeness(var19, weights = E(var19)$var19, mode = "in") %>% round(6)
var19_outcloseness<- closeness(var19, weights = E(var19)$var19, mode = "out") %>% round(6)
var19_totalcloseness<- closeness(var19, weights = E(var19)$var19, mode = "total") %>% round(6)
var19_incloseness_n<- closeness(var19,weights = E(var19)$var19, mode = "in", normalized = T) %>% round(6)
var19_outcloseness_n<- closeness(var19,weights = E(var19)$var19, mode = "out", normalized = T) %>% round(6)
var19_totalcloseness_n<- closeness(var19,weights = E(var19)$var19, mode = "total", normalized = T) %>% round(6)
var19_centr_closeness <- centralization.closeness(var19)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var19_df_closseness <- data.frame(
var19_incloseness,
var19_outcloseness,
var19_totalcloseness,
var19_incloseness_n,
var19_outcloseness_n,
var19_totalcloseness_n,
var19_centr_closeness) %>% round(6)

#Adding type
var19_df_closseness <-cbind(var19_df_closseness, V(var19)$LABEL_COR)

#Adding names
names(var19_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var19_df_closseness<-var19_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var19_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-cd2f5c8e9fbce7fd31b7" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-cd2f5c8e9fbce7fd31b7">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001183\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000698\" data-max=\"0.001919\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.02479\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.220118\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.129798\" data-max=\"0.357006\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[0.000131,0.000127,0.000132,0.000127,0.000127,0.000129,0.000126,0.00013,0.000128,0.00013,0.000129,0.000128,0.000126,0.000124,0.000127,0.00013,0.000128,0.000129,0.000122,0.000126,0.000129,0.000128,0.000127,0.000118,0.000119,0.00013,0.000121,0.000128,0.000127,0.000127,0.000127,0.000127,0.000128,0.000121,0.000127,0.000128,0.00013,0.000124,0.000123,0.000127,0.000129,0.000126,0.000126,0.00013,0.000126,0.000128,0.000117,0.000124,0.000124,0.000124,0.000124,0.000124,0.000126,0.000126,0.000121,0.000127,0.000127,0.000119,0.000122,0.000129,0.000133,0.000119,0.000127,0.000127,0.000128,2.9e-05,0.000117,0.000126,0.000121,0.000127,0.000128,0.000127,0.000127,0.000127,0.000129,0.000127,0.000127,0.000129,0.000121,0.000121,0.000121,0.000121,0.000127,0.000125,0.000128,0.000128,0.000128,0.00012,0.000127,0.000127,0.000128,0.000117,0.000124,0.000125,0.000127,0.000128,0.00012,0.000128,0.000129,0.000128,0.000126,0.000125,0.000126,0.000126,0.000127,0.000127,0.000127,0.000126,0.000127,0.000127,0.000127,0.000129,0.000127,0.000128,0.000128,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000129,0.000127,0.000127,0.000128,0.000127,0.000127,0.000127,0.000127,0.000127,0.000125,0.000128,0.000127,0.000127,0.000123,0.000126,0.000127,0.000126,0.000127,0.000126,0.00013,0.000118,0.00013,0.00013,0.00013,0.000122,0.000118,0.000126,0.000132,0.000129,0.000119,0.00012,0.000123,0.000119,0.000119,0.000125,0.000124,0.000119,0.000123,0.000125,0.000128,0.00013,0.000125,0.000119,0.000125,0.000123,0.000126,0.000126,0.000126,0.000126,0.000123,0.000129,0.000128],[0.001164,0.001067,0.001143,0.000898,0.000954,0.000746,0.000776,0.001183,0.000969,0.000986,0.000827,0.000717,0.000889,0.000847,0.000841,0.000747,0.000907,0.000887,0.000826,0.00089,0.000792,0.001038,0.000791,0.00114,0.000749,0.000584,0.000966,0.000831,0.000888,0.00088,0.000833,0.000796,0.001034,0.000668,0.000597,0.000983,0.000881,0.000801,0.001038,0.000837,0.000922,0.000831,0.000831,0.001114,0.000968,0.0008,0.000777,0.000763,0.000763,0.000763,0.000763,0.000763,0.000792,0.000742,0.001106,0.000891,0.00081,0.000719,0.000733,0.000882,2.9e-05,0.000645,0.000794,0.00072,0.001008,2.9e-05,2.9e-05,0.000797,0.000879,0.000704,0.000703,0.00075,0.00079,0.000757,0.000714,0.000746,0.000763,0.000885,0.000641,0.000641,0.000641,0.000641,0.00076,0.0007,0.000983,0.000852,0.000851,0.000715,0.000838,0.000714,0.000769,0.000791,0.000829,0.000675,0.000784,0.000762,0.000513,0.000662,0.000878,0.000822,0.000581,0.000812,2.9e-05,2.9e-05,0.000762,0.000772,0.000708,2.9e-05,2.9e-05,0.000776,0.000775,0.000767,0.000884,0.000879,0.00076,0.000771,0.000785,0.000784,0.000768,0.001073,0.000787,0.00082,0.000703,0.000703,0.000703,0.000745,0.000701,0.000711,0.000701,0.000745,0.000861,0.000743,0.000748,0.000701,0.000701,0.000758,0.000706,0.000701,0.000701,0.000743,0.000702,0.000701,0.000708,0.000701,0.000701,0.000701,0.000743,0.000558,0.000617,0.000766,0.000766,0.000766,0.000766,0.000766,0.000584,0.000657,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001919,0.001548,0.001825,0.001276,0.001351,0.001397,0.00104,0.001815,0.001403,0.00157,0.001387,0.001248,0.001235,0.001072,0.001202,0.001406,0.001321,0.001422,0.001129,0.001238,0.001412,0.001605,0.001206,0.001595,0.000984,0.00137,0.001425,0.001276,0.001238,0.001299,0.001214,0.001182,0.001608,0.001012,0.00119,0.001575,0.001513,0.001159,0.001499,0.001163,0.001475,0.001152,0.001122,0.001661,0.001427,0.001282,0.001046,0.001078,0.001078,0.001078,0.001078,0.001078,0.001149,0.001189,0.001541,0.001255,0.001175,0.000967,0.000969,0.001565,0.001453,0.000957,0.001272,0.001161,0.001456,0.000962,0.000837,0.001073,0.001241,0.001152,0.001397,0.001186,0.001377,0.001389,0.001406,0.001188,0.001359,0.001456,0.000804,0.000804,0.000804,0.000804,0.001164,0.001109,0.001433,0.001393,0.001431,0.000945,0.001189,0.001151,0.001279,0.001022,0.001122,0.001025,0.001183,0.001445,0.000734,0.001359,0.001401,0.001233,0.001138,0.001075,0.00099,0.00099,0.001156,0.001168,0.001159,0.00099,0.000998,0.001318,0.001157,0.001406,0.001245,0.001395,0.001393,0.001166,0.001159,0.001159,0.001159,0.001587,0.001164,0.001111,0.001185,0.001179,0.001185,0.001185,0.001176,0.001183,0.001186,0.001185,0.001233,0.001181,0.001179,0.001183,0.001182,0.001399,0.001149,0.001155,0.001305,0.001179,0.001175,0.001152,0.001147,0.001151,0.000912,0.001316,0.001107,0.001172,0.000895,0.001157,0.001175,0.001157,0.001175,0.001157,0.001473,0.000987,0.001159,0.001176,0.001175,0.000869,0.000698,0.000874,0.00117,0.000933,0.000859,0.000833,0.000863,0.000859,0.000859,0.000864,0.000867,0.000859,0.000864,0.000868,0.001075,0.001195,0.000921,0.000859,0.000921,0.000864,0.000948,0.000948,0.000941,0.000959,0.00087,0.001147,0.000907],[0.024295,0.023706,0.024548,0.023706,0.023709,0.023935,0.023352,0.024216,0.023806,0.024115,0.023941,0.023785,0.023464,0.022989,0.023577,0.02414,0.023788,0.024065,0.022636,0.023452,0.023938,0.023837,0.023709,0.022004,0.02219,0.024093,0.022418,0.023728,0.023673,0.023655,0.023703,0.023592,0.023776,0.022426,0.023673,0.023831,0.024159,0.02298,0.022926,0.02358,0.024022,0.023491,0.02339,0.02419,0.023414,0.023743,0.021701,0.023045,0.023045,0.023045,0.023045,0.023045,0.023402,0.023482,0.022458,0.023673,0.023628,0.022206,0.022766,0.024053,0.02479,0.022119,0.023628,0.023598,0.02384,0.005348,0.021775,0.023361,0.022537,0.023565,0.023886,0.023664,0.02367,0.023694,0.023914,0.023589,0.023616,0.024056,0.022537,0.022537,0.022537,0.022537,0.023625,0.023215,0.023782,0.023892,0.023846,0.02227,0.023595,0.023565,0.023843,0.021808,0.022986,0.023285,0.023631,0.023813,0.02227,0.023782,0.023907,0.023731,0.023361,0.023267,0.023458,0.023458,0.023595,0.023592,0.023601,0.023458,0.023532,0.023619,0.023601,0.023923,0.023625,0.023889,0.023898,0.023601,0.023589,0.023589,0.023598,0.023628,0.023592,0.023458,0.023667,0.023649,0.023667,0.023655,0.023649,0.023658,0.023661,0.023547,0.023634,0.023655,0.023658,0.023655,0.02367,0.023914,0.023589,0.023589,0.023743,0.023601,0.023646,0.023589,0.023577,0.023586,0.023209,0.02384,0.023637,0.023604,0.022923,0.023523,0.023634,0.023523,0.023634,0.023523,0.024184,0.021986,0.024093,0.024206,0.024197,0.022741,0.022007,0.023432,0.024613,0.023951,0.022156,0.022254,0.022884,0.022156,0.022156,0.023157,0.022977,0.022156,0.022966,0.023198,0.023877,0.024153,0.023314,0.022156,0.023314,0.022915,0.02339,0.02339,0.023467,0.023438,0.022909,0.023957,0.023734],[0.216531,0.198506,0.212571,0.167116,0.177481,0.138702,0.144298,0.220118,0.180233,0.183432,0.153846,0.133429,0.165333,0.157627,0.156434,0.139013,0.168631,0.164894,0.153592,0.16548,0.147385,0.193146,0.147152,0.212087,0.139222,0.108708,0.17971,0.154613,0.165187,0.163588,0.154871,0.148089,0.192347,0.124166,0.111045,0.182891,0.163877,0.149038,0.193146,0.155649,0.171429,0.154485,0.154485,0.207127,0.180058,0.1488,0.144522,0.141985,0.141985,0.141985,0.141985,0.141985,0.147268,0.138085,0.205752,0.165775,0.150607,0.133813,0.136364,0.164021,0.005348,0.12,0.147736,0.134006,0.1875,0.005376,0.005405,0.148207,0.163445,0.130894,0.13071,0.13943,0.146919,0.140802,0.132857,0.138702,0.141876,0.164602,0.119231,0.119231,0.119231,0.119231,0.141337,0.130161,0.182891,0.158433,0.158298,0.133047,0.155909,0.132762,0.143077,0.147152,0.154229,0.125506,0.145768,0.141768,0.095385,0.123179,0.163301,0.152961,0.10814,0.150974,0.005348,0.005348,0.141768,0.143629,0.131635,0.005348,0.005348,0.144298,0.144186,0.142638,0.164456,0.163445,0.141445,0.143408,0.145997,0.145768,0.142857,0.199571,0.146341,0.152459,0.13071,0.13071,0.13071,0.138496,0.130343,0.13229,0.130343,0.138496,0.160207,0.138187,0.139117,0.130343,0.130343,0.140909,0.131356,0.130343,0.130343,0.13829,0.130618,0.130343,0.131635,0.130343,0.130343,0.130343,0.138187,0.103853,0.114815,0.142529,0.142529,0.142529,0.142529,0.142529,0.108645,0.122288,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.357006,0.287926,0.339416,0.237245,0.251351,0.259777,0.193347,0.337568,0.26087,0.291994,0.257975,0.23221,0.22963,0.199357,0.223558,0.261603,0.245707,0.26458,0.209932,0.230198,0.262712,0.298555,0.224367,0.296651,0.183071,0.254795,0.264957,0.237245,0.230198,0.241558,0.225728,0.219858,0.299035,0.188259,0.221429,0.292913,0.281392,0.215527,0.278861,0.216279,0.274336,0.214286,0.208754,0.30897,0.265335,0.238462,0.194561,0.200431,0.200431,0.200431,0.200431,0.200431,0.213793,0.221165,0.286595,0.233375,0.218566,0.179884,0.180233,0.29108,0.270349,0.17799,0.236641,0.216028,0.270742,0.178846,0.155649,0.199571,0.230769,0.214286,0.259777,0.220641,0.256198,0.258333,0.261603,0.220903,0.252717,0.270742,0.149518,0.149518,0.149518,0.149518,0.216531,0.206208,0.266476,0.259053,0.266094,0.175803,0.221165,0.214039,0.237852,0.190184,0.208754,0.190574,0.220118,0.268786,0.136564,0.252717,0.260504,0.229346,0.211604,0.2,0.184158,0.184158,0.215029,0.21729,0.215527,0.184158,0.185629,0.245059,0.215278,0.261603,0.231631,0.259414,0.259053,0.216783,0.215527,0.215527,0.215527,0.295238,0.216531,0.206667,0.220379,0.21934,0.220379,0.220379,0.218824,0.220118,0.220641,0.220379,0.229346,0.219599,0.21934,0.220118,0.219858,0.26014,0.213793,0.214781,0.24282,0.21934,0.218566,0.214286,0.213303,0.214039,0.169553,0.244737,0.20598,0.218054,0.166517,0.215278,0.218566,0.215278,0.218566,0.215278,0.273932,0.183613,0.215527,0.218824,0.218566,0.161599,0.129798,0.162587,0.217544,0.173507,0.159794,0.154871,0.160483,0.159794,0.159794,0.160761,0.161318,0.159794,0.160761,0.161458,0.2,0.222222,0.171271,0.159794,0.171271,0.160622,0.176303,0.176303,0.174976,0.178332,0.16188,0.213303,0.168631],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var19_df_closseness, by=list(var19_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var19_df_closseness, by=list(var19_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-0e98bfa384f4e4fc1ff7" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-0e98bfa384f4e4fc1ff7">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000121\" data-max=\"0.000132\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"2.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001164\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.1e-05\" data-max=\"0.000388\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000907\" data-max=\"0.001919\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.8e-05\" data-max=\"0.000379\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022557\" data-max=\"0.024548\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"9e-05\" data-max=\"0.004219\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.216531\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.002222\" data-max=\"0.072136\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.168631\" data-max=\"0.357006\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.007081\" data-max=\"0.07047\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.003883\" data-max=\"0.139638\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[0.000127,0.000123,0.000126,0.000127,0.000126,0.000129,0.000128,0.000132,0.000127,0.000127,0.000129,0.000121,0.000125,0.000127,0.000128,0.000124,0.000128,0.000128,0.000131,0.000125,0.000129,0.000127,0.000127],[3e-06,4e-06,null,null,null,null,1e-06,null,null,null,null,2.3e-05,1e-06,1e-06,1e-06,3e-06,null,null,null,2e-06,null,1e-06,1e-06],[0.000858,0.0003,0.00089,0.001067,0.000776,0.000882,0.000775,0.001143,0.000791,0.000743,0.000885,0.000817,0.000839,0.000815,0.000869,0.0008,2.9e-05,0.000822,0.001164,0.000543,0.000887,0.000632,0.000763],[0.000216,0.000373,null,null,null,null,6.7e-05,null,null,null,null,0.000258,1.1e-05,0.0001,0.000109,9.9e-05,null,null,null,0.000388,null,0.000271,7.4e-05],[0.001326,0.001005,0.001238,0.001548,0.00104,0.001565,0.001335,0.001825,0.001206,0.001107,0.001456,0.001314,0.001112,0.001265,0.001356,0.001125,0.000907,0.001233,0.001919,0.001118,0.001422,0.00119,0.001236],[0.000379,0.000225,null,null,null,null,7e-05,null,null,null,null,0.000295,5.7e-05,5.7e-05,3.8e-05,0.000133,null,null,null,0.000166,null,5.5e-05,0.000106],[0.023574,0.022931,0.023452,0.023706,0.023352,0.024053,0.023835,0.024548,0.023709,0.023637,0.024056,0.022557,0.02324,0.023728,0.023811,0.023128,0.023734,0.023731,0.024295,0.023265,0.024065,0.023721,0.023658],[0.000577,0.000792,null,null,null,null,0.000105,null,null,null,null,0.004219,0.000355,9e-05,0.000115,0.000455,null,null,null,0.000355,null,0.000217,0.000113],[0.15953,0.055833,0.16548,0.198506,0.144298,0.164021,0.144114,0.212571,0.147152,0.138187,0.164602,0.151985,0.156056,0.151517,0.161605,0.148779,0.005348,0.152961,0.216531,0.101043,0.164894,0.117464,0.141966],[0.040112,0.069495,null,null,null,null,0.012461,null,null,null,null,0.048006,0.002222,0.018602,0.020322,0.018433,null,null,null,0.072136,null,0.05039,0.013778],[0.246629,0.186969,0.230198,0.287926,0.193347,0.29108,0.248231,0.339416,0.224367,0.20598,0.270742,0.244345,0.206822,0.235353,0.252278,0.20923,0.168631,0.229346,0.357006,0.208021,0.26458,0.221414,0.229821],[0.07047,0.041822,null,null,null,null,0.013088,null,null,null,null,0.054891,0.010556,0.01055,0.007081,0.024721,null,null,null,0.030812,null,0.010213,0.019779],[0.300585,0.099063,0.286154,0.343173,0.303922,0.323478,0.300896,0.391579,0.295238,0.29108,0.313659,0.266552,0.319611,0.301223,0.316849,0.295625,0.005348,0.306425,0.371257,0.194167,0.314189,0.234219,0.288627],[0.043014,0.125815,null,null,null,null,0.018146,null,null,null,null,0.070696,0.003883,0.033229,0.029748,0.023781,null,null,null,0.139638,null,0.102732,0.011577]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var19_df_closseness <- data.frame(
var19_incloseness,
var19_outcloseness,
var19_totalcloseness,
var19_incloseness_n,
var19_outcloseness_n,
var19_totalcloseness_n,
var19_centr_closeness) %>% round(6)

#Adding type
var19_df_closseness <-cbind(var19_df_closseness, V(var19)$TIPO1)

#Adding names
names(var19_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var19_df_closseness<-var19_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var19_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-d1674b706e582db3d3a0" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-d1674b706e582db3d3a0">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001183\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000698\" data-max=\"0.001919\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.02479\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.220118\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.129798\" data-max=\"0.357006\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[0.000131,0.000127,0.000132,0.000127,0.000127,0.000129,0.000126,0.00013,0.000128,0.00013,0.000129,0.000128,0.000126,0.000124,0.000127,0.00013,0.000128,0.000129,0.000122,0.000126,0.000129,0.000128,0.000127,0.000118,0.000119,0.00013,0.000121,0.000128,0.000127,0.000127,0.000127,0.000127,0.000128,0.000121,0.000127,0.000128,0.00013,0.000124,0.000123,0.000127,0.000129,0.000126,0.000126,0.00013,0.000126,0.000128,0.000117,0.000124,0.000124,0.000124,0.000124,0.000124,0.000126,0.000126,0.000121,0.000127,0.000127,0.000119,0.000122,0.000129,0.000133,0.000119,0.000127,0.000127,0.000128,2.9e-05,0.000117,0.000126,0.000121,0.000127,0.000128,0.000127,0.000127,0.000127,0.000129,0.000127,0.000127,0.000129,0.000121,0.000121,0.000121,0.000121,0.000127,0.000125,0.000128,0.000128,0.000128,0.00012,0.000127,0.000127,0.000128,0.000117,0.000124,0.000125,0.000127,0.000128,0.00012,0.000128,0.000129,0.000128,0.000126,0.000125,0.000126,0.000126,0.000127,0.000127,0.000127,0.000126,0.000127,0.000127,0.000127,0.000129,0.000127,0.000128,0.000128,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000129,0.000127,0.000127,0.000128,0.000127,0.000127,0.000127,0.000127,0.000127,0.000125,0.000128,0.000127,0.000127,0.000123,0.000126,0.000127,0.000126,0.000127,0.000126,0.00013,0.000118,0.00013,0.00013,0.00013,0.000122,0.000118,0.000126,0.000132,0.000129,0.000119,0.00012,0.000123,0.000119,0.000119,0.000125,0.000124,0.000119,0.000123,0.000125,0.000128,0.00013,0.000125,0.000119,0.000125,0.000123,0.000126,0.000126,0.000126,0.000126,0.000123,0.000129,0.000128],[0.001164,0.001067,0.001143,0.000898,0.000954,0.000746,0.000776,0.001183,0.000969,0.000986,0.000827,0.000717,0.000889,0.000847,0.000841,0.000747,0.000907,0.000887,0.000826,0.00089,0.000792,0.001038,0.000791,0.00114,0.000749,0.000584,0.000966,0.000831,0.000888,0.00088,0.000833,0.000796,0.001034,0.000668,0.000597,0.000983,0.000881,0.000801,0.001038,0.000837,0.000922,0.000831,0.000831,0.001114,0.000968,0.0008,0.000777,0.000763,0.000763,0.000763,0.000763,0.000763,0.000792,0.000742,0.001106,0.000891,0.00081,0.000719,0.000733,0.000882,2.9e-05,0.000645,0.000794,0.00072,0.001008,2.9e-05,2.9e-05,0.000797,0.000879,0.000704,0.000703,0.00075,0.00079,0.000757,0.000714,0.000746,0.000763,0.000885,0.000641,0.000641,0.000641,0.000641,0.00076,0.0007,0.000983,0.000852,0.000851,0.000715,0.000838,0.000714,0.000769,0.000791,0.000829,0.000675,0.000784,0.000762,0.000513,0.000662,0.000878,0.000822,0.000581,0.000812,2.9e-05,2.9e-05,0.000762,0.000772,0.000708,2.9e-05,2.9e-05,0.000776,0.000775,0.000767,0.000884,0.000879,0.00076,0.000771,0.000785,0.000784,0.000768,0.001073,0.000787,0.00082,0.000703,0.000703,0.000703,0.000745,0.000701,0.000711,0.000701,0.000745,0.000861,0.000743,0.000748,0.000701,0.000701,0.000758,0.000706,0.000701,0.000701,0.000743,0.000702,0.000701,0.000708,0.000701,0.000701,0.000701,0.000743,0.000558,0.000617,0.000766,0.000766,0.000766,0.000766,0.000766,0.000584,0.000657,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001919,0.001548,0.001825,0.001276,0.001351,0.001397,0.00104,0.001815,0.001403,0.00157,0.001387,0.001248,0.001235,0.001072,0.001202,0.001406,0.001321,0.001422,0.001129,0.001238,0.001412,0.001605,0.001206,0.001595,0.000984,0.00137,0.001425,0.001276,0.001238,0.001299,0.001214,0.001182,0.001608,0.001012,0.00119,0.001575,0.001513,0.001159,0.001499,0.001163,0.001475,0.001152,0.001122,0.001661,0.001427,0.001282,0.001046,0.001078,0.001078,0.001078,0.001078,0.001078,0.001149,0.001189,0.001541,0.001255,0.001175,0.000967,0.000969,0.001565,0.001453,0.000957,0.001272,0.001161,0.001456,0.000962,0.000837,0.001073,0.001241,0.001152,0.001397,0.001186,0.001377,0.001389,0.001406,0.001188,0.001359,0.001456,0.000804,0.000804,0.000804,0.000804,0.001164,0.001109,0.001433,0.001393,0.001431,0.000945,0.001189,0.001151,0.001279,0.001022,0.001122,0.001025,0.001183,0.001445,0.000734,0.001359,0.001401,0.001233,0.001138,0.001075,0.00099,0.00099,0.001156,0.001168,0.001159,0.00099,0.000998,0.001318,0.001157,0.001406,0.001245,0.001395,0.001393,0.001166,0.001159,0.001159,0.001159,0.001587,0.001164,0.001111,0.001185,0.001179,0.001185,0.001185,0.001176,0.001183,0.001186,0.001185,0.001233,0.001181,0.001179,0.001183,0.001182,0.001399,0.001149,0.001155,0.001305,0.001179,0.001175,0.001152,0.001147,0.001151,0.000912,0.001316,0.001107,0.001172,0.000895,0.001157,0.001175,0.001157,0.001175,0.001157,0.001473,0.000987,0.001159,0.001176,0.001175,0.000869,0.000698,0.000874,0.00117,0.000933,0.000859,0.000833,0.000863,0.000859,0.000859,0.000864,0.000867,0.000859,0.000864,0.000868,0.001075,0.001195,0.000921,0.000859,0.000921,0.000864,0.000948,0.000948,0.000941,0.000959,0.00087,0.001147,0.000907],[0.024295,0.023706,0.024548,0.023706,0.023709,0.023935,0.023352,0.024216,0.023806,0.024115,0.023941,0.023785,0.023464,0.022989,0.023577,0.02414,0.023788,0.024065,0.022636,0.023452,0.023938,0.023837,0.023709,0.022004,0.02219,0.024093,0.022418,0.023728,0.023673,0.023655,0.023703,0.023592,0.023776,0.022426,0.023673,0.023831,0.024159,0.02298,0.022926,0.02358,0.024022,0.023491,0.02339,0.02419,0.023414,0.023743,0.021701,0.023045,0.023045,0.023045,0.023045,0.023045,0.023402,0.023482,0.022458,0.023673,0.023628,0.022206,0.022766,0.024053,0.02479,0.022119,0.023628,0.023598,0.02384,0.005348,0.021775,0.023361,0.022537,0.023565,0.023886,0.023664,0.02367,0.023694,0.023914,0.023589,0.023616,0.024056,0.022537,0.022537,0.022537,0.022537,0.023625,0.023215,0.023782,0.023892,0.023846,0.02227,0.023595,0.023565,0.023843,0.021808,0.022986,0.023285,0.023631,0.023813,0.02227,0.023782,0.023907,0.023731,0.023361,0.023267,0.023458,0.023458,0.023595,0.023592,0.023601,0.023458,0.023532,0.023619,0.023601,0.023923,0.023625,0.023889,0.023898,0.023601,0.023589,0.023589,0.023598,0.023628,0.023592,0.023458,0.023667,0.023649,0.023667,0.023655,0.023649,0.023658,0.023661,0.023547,0.023634,0.023655,0.023658,0.023655,0.02367,0.023914,0.023589,0.023589,0.023743,0.023601,0.023646,0.023589,0.023577,0.023586,0.023209,0.02384,0.023637,0.023604,0.022923,0.023523,0.023634,0.023523,0.023634,0.023523,0.024184,0.021986,0.024093,0.024206,0.024197,0.022741,0.022007,0.023432,0.024613,0.023951,0.022156,0.022254,0.022884,0.022156,0.022156,0.023157,0.022977,0.022156,0.022966,0.023198,0.023877,0.024153,0.023314,0.022156,0.023314,0.022915,0.02339,0.02339,0.023467,0.023438,0.022909,0.023957,0.023734],[0.216531,0.198506,0.212571,0.167116,0.177481,0.138702,0.144298,0.220118,0.180233,0.183432,0.153846,0.133429,0.165333,0.157627,0.156434,0.139013,0.168631,0.164894,0.153592,0.16548,0.147385,0.193146,0.147152,0.212087,0.139222,0.108708,0.17971,0.154613,0.165187,0.163588,0.154871,0.148089,0.192347,0.124166,0.111045,0.182891,0.163877,0.149038,0.193146,0.155649,0.171429,0.154485,0.154485,0.207127,0.180058,0.1488,0.144522,0.141985,0.141985,0.141985,0.141985,0.141985,0.147268,0.138085,0.205752,0.165775,0.150607,0.133813,0.136364,0.164021,0.005348,0.12,0.147736,0.134006,0.1875,0.005376,0.005405,0.148207,0.163445,0.130894,0.13071,0.13943,0.146919,0.140802,0.132857,0.138702,0.141876,0.164602,0.119231,0.119231,0.119231,0.119231,0.141337,0.130161,0.182891,0.158433,0.158298,0.133047,0.155909,0.132762,0.143077,0.147152,0.154229,0.125506,0.145768,0.141768,0.095385,0.123179,0.163301,0.152961,0.10814,0.150974,0.005348,0.005348,0.141768,0.143629,0.131635,0.005348,0.005348,0.144298,0.144186,0.142638,0.164456,0.163445,0.141445,0.143408,0.145997,0.145768,0.142857,0.199571,0.146341,0.152459,0.13071,0.13071,0.13071,0.138496,0.130343,0.13229,0.130343,0.138496,0.160207,0.138187,0.139117,0.130343,0.130343,0.140909,0.131356,0.130343,0.130343,0.13829,0.130618,0.130343,0.131635,0.130343,0.130343,0.130343,0.138187,0.103853,0.114815,0.142529,0.142529,0.142529,0.142529,0.142529,0.108645,0.122288,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.357006,0.287926,0.339416,0.237245,0.251351,0.259777,0.193347,0.337568,0.26087,0.291994,0.257975,0.23221,0.22963,0.199357,0.223558,0.261603,0.245707,0.26458,0.209932,0.230198,0.262712,0.298555,0.224367,0.296651,0.183071,0.254795,0.264957,0.237245,0.230198,0.241558,0.225728,0.219858,0.299035,0.188259,0.221429,0.292913,0.281392,0.215527,0.278861,0.216279,0.274336,0.214286,0.208754,0.30897,0.265335,0.238462,0.194561,0.200431,0.200431,0.200431,0.200431,0.200431,0.213793,0.221165,0.286595,0.233375,0.218566,0.179884,0.180233,0.29108,0.270349,0.17799,0.236641,0.216028,0.270742,0.178846,0.155649,0.199571,0.230769,0.214286,0.259777,0.220641,0.256198,0.258333,0.261603,0.220903,0.252717,0.270742,0.149518,0.149518,0.149518,0.149518,0.216531,0.206208,0.266476,0.259053,0.266094,0.175803,0.221165,0.214039,0.237852,0.190184,0.208754,0.190574,0.220118,0.268786,0.136564,0.252717,0.260504,0.229346,0.211604,0.2,0.184158,0.184158,0.215029,0.21729,0.215527,0.184158,0.185629,0.245059,0.215278,0.261603,0.231631,0.259414,0.259053,0.216783,0.215527,0.215527,0.215527,0.295238,0.216531,0.206667,0.220379,0.21934,0.220379,0.220379,0.218824,0.220118,0.220641,0.220379,0.229346,0.219599,0.21934,0.220118,0.219858,0.26014,0.213793,0.214781,0.24282,0.21934,0.218566,0.214286,0.213303,0.214039,0.169553,0.244737,0.20598,0.218054,0.166517,0.215278,0.218566,0.215278,0.218566,0.215278,0.273932,0.183613,0.215527,0.218824,0.218566,0.161599,0.129798,0.162587,0.217544,0.173507,0.159794,0.154871,0.160483,0.159794,0.159794,0.160761,0.161318,0.159794,0.160761,0.161458,0.2,0.222222,0.171271,0.159794,0.171271,0.160622,0.176303,0.176303,0.174976,0.178332,0.16188,0.213303,0.168631],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var19_df_closseness, by=list(var19_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var19_df_closseness, by=list(var19_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-c25f3b34f262d59441a9" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-c25f3b34f262d59441a9">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000123\" data-max=\"0.000127\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.1e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.00051\" data-max=\"0.000737\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000223\" data-max=\"4e-04\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001098\" data-max=\"0.001243\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000158\" data-max=\"0.000262\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022882\" data-max=\"0.023663\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000259\" data-max=\"0.002132\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.094959\" data-max=\"0.136997\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.041562\" data-max=\"0.074446\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.204267\" data-max=\"0.231277\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.02939\" data-max=\"0.048789\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.173776\" data-max=\"0.2725\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.079091\" data-max=\"0.135432\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[0.000127,0.000123],[1e-06,1.1e-05],[0.000737,0.000511],[0.000223,0.0004],[0.001243,0.001098],[0.000158,0.000262],[0.023663,0.022882],[0.000259,0.002132],[0.136997,0.094959],[0.041562,0.074446],[0.231277,0.204267],[0.02939,0.048789],[0.2725,0.173776],[0.079091,0.135432]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var19_df_closseness <- data.frame(
var19_incloseness,
var19_outcloseness,
var19_totalcloseness,
var19_incloseness_n,
var19_outcloseness_n,
var19_totalcloseness_n,
var19_centr_closeness) %>% round(6)

#Adding type
var19_df_closseness <-cbind(var19_df_closseness, V(var19)$TIPO2)

#Adding names
names(var19_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var19_df_closseness<-var19_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var19_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-26572f8345a521b5e257" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-26572f8345a521b5e257">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000133\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.001183\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000698\" data-max=\"0.001919\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.02479\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.220118\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.129798\" data-max=\"0.357006\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.391579\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[0.000131,0.000127,0.000132,0.000127,0.000127,0.000129,0.000126,0.00013,0.000128,0.00013,0.000129,0.000128,0.000126,0.000124,0.000127,0.00013,0.000128,0.000129,0.000122,0.000126,0.000129,0.000128,0.000127,0.000118,0.000119,0.00013,0.000121,0.000128,0.000127,0.000127,0.000127,0.000127,0.000128,0.000121,0.000127,0.000128,0.00013,0.000124,0.000123,0.000127,0.000129,0.000126,0.000126,0.00013,0.000126,0.000128,0.000117,0.000124,0.000124,0.000124,0.000124,0.000124,0.000126,0.000126,0.000121,0.000127,0.000127,0.000119,0.000122,0.000129,0.000133,0.000119,0.000127,0.000127,0.000128,2.9e-05,0.000117,0.000126,0.000121,0.000127,0.000128,0.000127,0.000127,0.000127,0.000129,0.000127,0.000127,0.000129,0.000121,0.000121,0.000121,0.000121,0.000127,0.000125,0.000128,0.000128,0.000128,0.00012,0.000127,0.000127,0.000128,0.000117,0.000124,0.000125,0.000127,0.000128,0.00012,0.000128,0.000129,0.000128,0.000126,0.000125,0.000126,0.000126,0.000127,0.000127,0.000127,0.000126,0.000127,0.000127,0.000127,0.000129,0.000127,0.000128,0.000128,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000126,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000127,0.000129,0.000127,0.000127,0.000128,0.000127,0.000127,0.000127,0.000127,0.000127,0.000125,0.000128,0.000127,0.000127,0.000123,0.000126,0.000127,0.000126,0.000127,0.000126,0.00013,0.000118,0.00013,0.00013,0.00013,0.000122,0.000118,0.000126,0.000132,0.000129,0.000119,0.00012,0.000123,0.000119,0.000119,0.000125,0.000124,0.000119,0.000123,0.000125,0.000128,0.00013,0.000125,0.000119,0.000125,0.000123,0.000126,0.000126,0.000126,0.000126,0.000123,0.000129,0.000128],[0.001164,0.001067,0.001143,0.000898,0.000954,0.000746,0.000776,0.001183,0.000969,0.000986,0.000827,0.000717,0.000889,0.000847,0.000841,0.000747,0.000907,0.000887,0.000826,0.00089,0.000792,0.001038,0.000791,0.00114,0.000749,0.000584,0.000966,0.000831,0.000888,0.00088,0.000833,0.000796,0.001034,0.000668,0.000597,0.000983,0.000881,0.000801,0.001038,0.000837,0.000922,0.000831,0.000831,0.001114,0.000968,0.0008,0.000777,0.000763,0.000763,0.000763,0.000763,0.000763,0.000792,0.000742,0.001106,0.000891,0.00081,0.000719,0.000733,0.000882,2.9e-05,0.000645,0.000794,0.00072,0.001008,2.9e-05,2.9e-05,0.000797,0.000879,0.000704,0.000703,0.00075,0.00079,0.000757,0.000714,0.000746,0.000763,0.000885,0.000641,0.000641,0.000641,0.000641,0.00076,0.0007,0.000983,0.000852,0.000851,0.000715,0.000838,0.000714,0.000769,0.000791,0.000829,0.000675,0.000784,0.000762,0.000513,0.000662,0.000878,0.000822,0.000581,0.000812,2.9e-05,2.9e-05,0.000762,0.000772,0.000708,2.9e-05,2.9e-05,0.000776,0.000775,0.000767,0.000884,0.000879,0.00076,0.000771,0.000785,0.000784,0.000768,0.001073,0.000787,0.00082,0.000703,0.000703,0.000703,0.000745,0.000701,0.000711,0.000701,0.000745,0.000861,0.000743,0.000748,0.000701,0.000701,0.000758,0.000706,0.000701,0.000701,0.000743,0.000702,0.000701,0.000708,0.000701,0.000701,0.000701,0.000743,0.000558,0.000617,0.000766,0.000766,0.000766,0.000766,0.000766,0.000584,0.000657,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.001919,0.001548,0.001825,0.001276,0.001351,0.001397,0.00104,0.001815,0.001403,0.00157,0.001387,0.001248,0.001235,0.001072,0.001202,0.001406,0.001321,0.001422,0.001129,0.001238,0.001412,0.001605,0.001206,0.001595,0.000984,0.00137,0.001425,0.001276,0.001238,0.001299,0.001214,0.001182,0.001608,0.001012,0.00119,0.001575,0.001513,0.001159,0.001499,0.001163,0.001475,0.001152,0.001122,0.001661,0.001427,0.001282,0.001046,0.001078,0.001078,0.001078,0.001078,0.001078,0.001149,0.001189,0.001541,0.001255,0.001175,0.000967,0.000969,0.001565,0.001453,0.000957,0.001272,0.001161,0.001456,0.000962,0.000837,0.001073,0.001241,0.001152,0.001397,0.001186,0.001377,0.001389,0.001406,0.001188,0.001359,0.001456,0.000804,0.000804,0.000804,0.000804,0.001164,0.001109,0.001433,0.001393,0.001431,0.000945,0.001189,0.001151,0.001279,0.001022,0.001122,0.001025,0.001183,0.001445,0.000734,0.001359,0.001401,0.001233,0.001138,0.001075,0.00099,0.00099,0.001156,0.001168,0.001159,0.00099,0.000998,0.001318,0.001157,0.001406,0.001245,0.001395,0.001393,0.001166,0.001159,0.001159,0.001159,0.001587,0.001164,0.001111,0.001185,0.001179,0.001185,0.001185,0.001176,0.001183,0.001186,0.001185,0.001233,0.001181,0.001179,0.001183,0.001182,0.001399,0.001149,0.001155,0.001305,0.001179,0.001175,0.001152,0.001147,0.001151,0.000912,0.001316,0.001107,0.001172,0.000895,0.001157,0.001175,0.001157,0.001175,0.001157,0.001473,0.000987,0.001159,0.001176,0.001175,0.000869,0.000698,0.000874,0.00117,0.000933,0.000859,0.000833,0.000863,0.000859,0.000859,0.000864,0.000867,0.000859,0.000864,0.000868,0.001075,0.001195,0.000921,0.000859,0.000921,0.000864,0.000948,0.000948,0.000941,0.000959,0.00087,0.001147,0.000907],[0.024295,0.023706,0.024548,0.023706,0.023709,0.023935,0.023352,0.024216,0.023806,0.024115,0.023941,0.023785,0.023464,0.022989,0.023577,0.02414,0.023788,0.024065,0.022636,0.023452,0.023938,0.023837,0.023709,0.022004,0.02219,0.024093,0.022418,0.023728,0.023673,0.023655,0.023703,0.023592,0.023776,0.022426,0.023673,0.023831,0.024159,0.02298,0.022926,0.02358,0.024022,0.023491,0.02339,0.02419,0.023414,0.023743,0.021701,0.023045,0.023045,0.023045,0.023045,0.023045,0.023402,0.023482,0.022458,0.023673,0.023628,0.022206,0.022766,0.024053,0.02479,0.022119,0.023628,0.023598,0.02384,0.005348,0.021775,0.023361,0.022537,0.023565,0.023886,0.023664,0.02367,0.023694,0.023914,0.023589,0.023616,0.024056,0.022537,0.022537,0.022537,0.022537,0.023625,0.023215,0.023782,0.023892,0.023846,0.02227,0.023595,0.023565,0.023843,0.021808,0.022986,0.023285,0.023631,0.023813,0.02227,0.023782,0.023907,0.023731,0.023361,0.023267,0.023458,0.023458,0.023595,0.023592,0.023601,0.023458,0.023532,0.023619,0.023601,0.023923,0.023625,0.023889,0.023898,0.023601,0.023589,0.023589,0.023598,0.023628,0.023592,0.023458,0.023667,0.023649,0.023667,0.023655,0.023649,0.023658,0.023661,0.023547,0.023634,0.023655,0.023658,0.023655,0.02367,0.023914,0.023589,0.023589,0.023743,0.023601,0.023646,0.023589,0.023577,0.023586,0.023209,0.02384,0.023637,0.023604,0.022923,0.023523,0.023634,0.023523,0.023634,0.023523,0.024184,0.021986,0.024093,0.024206,0.024197,0.022741,0.022007,0.023432,0.024613,0.023951,0.022156,0.022254,0.022884,0.022156,0.022156,0.023157,0.022977,0.022156,0.022966,0.023198,0.023877,0.024153,0.023314,0.022156,0.023314,0.022915,0.02339,0.02339,0.023467,0.023438,0.022909,0.023957,0.023734],[0.216531,0.198506,0.212571,0.167116,0.177481,0.138702,0.144298,0.220118,0.180233,0.183432,0.153846,0.133429,0.165333,0.157627,0.156434,0.139013,0.168631,0.164894,0.153592,0.16548,0.147385,0.193146,0.147152,0.212087,0.139222,0.108708,0.17971,0.154613,0.165187,0.163588,0.154871,0.148089,0.192347,0.124166,0.111045,0.182891,0.163877,0.149038,0.193146,0.155649,0.171429,0.154485,0.154485,0.207127,0.180058,0.1488,0.144522,0.141985,0.141985,0.141985,0.141985,0.141985,0.147268,0.138085,0.205752,0.165775,0.150607,0.133813,0.136364,0.164021,0.005348,0.12,0.147736,0.134006,0.1875,0.005376,0.005405,0.148207,0.163445,0.130894,0.13071,0.13943,0.146919,0.140802,0.132857,0.138702,0.141876,0.164602,0.119231,0.119231,0.119231,0.119231,0.141337,0.130161,0.182891,0.158433,0.158298,0.133047,0.155909,0.132762,0.143077,0.147152,0.154229,0.125506,0.145768,0.141768,0.095385,0.123179,0.163301,0.152961,0.10814,0.150974,0.005348,0.005348,0.141768,0.143629,0.131635,0.005348,0.005348,0.144298,0.144186,0.142638,0.164456,0.163445,0.141445,0.143408,0.145997,0.145768,0.142857,0.199571,0.146341,0.152459,0.13071,0.13071,0.13071,0.138496,0.130343,0.13229,0.130343,0.138496,0.160207,0.138187,0.139117,0.130343,0.130343,0.140909,0.131356,0.130343,0.130343,0.13829,0.130618,0.130343,0.131635,0.130343,0.130343,0.130343,0.138187,0.103853,0.114815,0.142529,0.142529,0.142529,0.142529,0.142529,0.108645,0.122288,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.357006,0.287926,0.339416,0.237245,0.251351,0.259777,0.193347,0.337568,0.26087,0.291994,0.257975,0.23221,0.22963,0.199357,0.223558,0.261603,0.245707,0.26458,0.209932,0.230198,0.262712,0.298555,0.224367,0.296651,0.183071,0.254795,0.264957,0.237245,0.230198,0.241558,0.225728,0.219858,0.299035,0.188259,0.221429,0.292913,0.281392,0.215527,0.278861,0.216279,0.274336,0.214286,0.208754,0.30897,0.265335,0.238462,0.194561,0.200431,0.200431,0.200431,0.200431,0.200431,0.213793,0.221165,0.286595,0.233375,0.218566,0.179884,0.180233,0.29108,0.270349,0.17799,0.236641,0.216028,0.270742,0.178846,0.155649,0.199571,0.230769,0.214286,0.259777,0.220641,0.256198,0.258333,0.261603,0.220903,0.252717,0.270742,0.149518,0.149518,0.149518,0.149518,0.216531,0.206208,0.266476,0.259053,0.266094,0.175803,0.221165,0.214039,0.237852,0.190184,0.208754,0.190574,0.220118,0.268786,0.136564,0.252717,0.260504,0.229346,0.211604,0.2,0.184158,0.184158,0.215029,0.21729,0.215527,0.184158,0.185629,0.245059,0.215278,0.261603,0.231631,0.259414,0.259053,0.216783,0.215527,0.215527,0.215527,0.295238,0.216531,0.206667,0.220379,0.21934,0.220379,0.220379,0.218824,0.220118,0.220641,0.220379,0.229346,0.219599,0.21934,0.220118,0.219858,0.26014,0.213793,0.214781,0.24282,0.21934,0.218566,0.214286,0.213303,0.214039,0.169553,0.244737,0.20598,0.218054,0.166517,0.215278,0.218566,0.215278,0.218566,0.215278,0.273932,0.183613,0.215527,0.218824,0.218566,0.161599,0.129798,0.162587,0.217544,0.173507,0.159794,0.154871,0.160483,0.159794,0.159794,0.160761,0.161318,0.159794,0.160761,0.161458,0.2,0.222222,0.171271,0.159794,0.171271,0.160622,0.176303,0.176303,0.174976,0.178332,0.16188,0.213303,0.168631],[0.371257,0.343173,0.391579,0.333333,0.303426,0.296178,0.303922,0.317949,0.310518,0.287926,0.293375,0.285714,0.333333,0.322357,0.287037,0.290172,0.350943,0.314189,0.292913,0.286154,0.301948,0.285276,0.295238,0.332737,0.285714,0.24031,0.276786,0.318493,0.332737,0.315789,0.31,0.297125,0.315789,0.223289,0.224096,0.285276,0.306425,0.267241,0.298077,0.319039,0.333932,0.316865,0.316865,0.293375,0.292453,0.298555,0.229913,0.290625,0.290625,0.290625,0.290625,0.290625,0.282246,0.297125,0.331551,0.317406,0.303922,0.287926,0.261236,0.323478,0.005348,0.227662,0.296651,0.290172,0.299035,0.005376,0.005405,0.306931,0.298555,0.283105,0.283105,0.293375,0.295707,0.295707,0.288372,0.28972,0.287481,0.313659,0.229346,0.229346,0.229346,0.229346,0.286595,0.250336,0.295238,0.298555,0.291994,0.274336,0.301948,0.285276,0.290625,0.301948,0.288372,0.248663,0.294304,0.287926,0.215777,0.223022,0.285276,0.306425,0.234552,0.312605,0.005348,0.005348,0.290172,0.298077,0.284839,0.005348,0.005348,0.295238,0.295707,0.28882,0.286595,0.285276,0.286154,0.291536,0.294304,0.29477,0.289269,0.289269,0.294304,0.31,0.283105,0.283105,0.283105,0.291536,0.282246,0.285276,0.282246,0.291536,0.29108,0.29108,0.291994,0.282246,0.282246,0.2976,0.284404,0.282246,0.282246,0.29108,0.282675,0.282246,0.285276,0.282246,0.282246,0.282246,0.29108,0.220903,0.252033,0.256198,0.256198,0.256198,0.256198,0.256198,0.226277,0.226829,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var19_df_closseness, by=list(var19_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - sd

```r
aggdata_sd <-aggregate(var19_df_closseness, by=list(var19_df_closseness$Type), FUN=sd, na.rm=TRUE) 

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

<!--html_preserve--><div id="htmlwidget-4ebac9fe274428249939" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-4ebac9fe274428249939">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000123\" data-max=\"0.000128\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1e-06\" data-max=\"1.2e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000508\" data-max=\"0.000852\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000106\" data-max=\"0.000402\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.001097\" data-max=\"0.001333\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000114\" data-max=\"0.000264\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.022868\" data-max=\"0.023789\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000134\" data-max=\"0.002143\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.094424\" data-max=\"0.158457\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.019662\" data-max=\"0.074779\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.203981\" data-max=\"0.247942\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.021214\" data-max=\"0.049023\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.172406\" data-max=\"0.308329\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.029295\" data-max=\"0.135782\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[0.000128,0.000127,0.000123],[1e-06,1e-06,1.2e-05],[0.000852,0.000718,0.000508],[0.000106,0.000231,0.000402],[0.001333,0.001229,0.001097],[0.000114,0.000159,0.000264],[0.023789,0.023646,0.022868],[0.000134,0.000266,0.002143],[0.158457,0.133569,0.094424],[0.019662,0.042925,0.074779],[0.247942,0.228568,0.203981],[0.021214,0.029586,0.049023],[0.308329,0.266869,0.172406],[0.029295,0.082605,0.135782]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(SD)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(SD)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(SD)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(SD)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(SD)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(SD)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(SD)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var19_data.RData")
```

