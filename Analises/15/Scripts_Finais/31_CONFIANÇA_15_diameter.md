# SNA Diameter 31_CONFIANÇA
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 31_CONFIANÇA

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/14_kcore_confianca.RData")
```
##Reload packages

```r
suppressMessages(library(RColorBrewer))
suppressMessages(library(car))
suppressMessages(library(xtable))
suppressMessages(library(igraph))
suppressMessages(library(miniCRAN))
suppressMessages(library(magrittr))
suppressMessages(library(keyplayer))
suppressMessages(library(dplyr))
suppressMessages(library(feather))
suppressMessages(library(visNetwork))
suppressMessages(library(knitr))
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
#confianca<-simplify(confianca) #Simplify
```


#Diameter - length of the longest geodesic.

##Diameter Non-weigthed 

```
## [1] 6
```

```
## + 7/187 vertices, named:
## [1] EA_DQCT_ Centro Terapêutico Reconstruir (escritório)                          
## [2] EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)                             
## [3] ASS_HOS_ Hospital de Pronto Socorro – HPS                                     
## [4] CRAS_AS_ CRAS Sudeste Costa Carvalho                                          
## [5] AJU_MUT_ Grupo A.A. Reunidos                                                  
## [6] AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora
## [7] AJU_MUT_ ALANON Grupo Libertação
```
##Diameter Weigthed 

```
## [1] 119
```

```
## + 7/187 vertices, named:
## [1] EA_DQCT_ Centro Terapêutico Reconstruir (escritório)
## [2] EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)   
## [3] ASS_HOS_ Hospital de Pronto Socorro – HPS           
## [4] CRAS_AS_ CRAS Nordeste Grama                        
## [5] EA_DQCT_ Centro de Recuperação SOS Vida             
## [6] EA_DQCT_ Associação Beneficente Cristã Restituir    
## [7] AJU_MUT_ ALANON Grupo Libertação
```
##Plotting Diameter
##Plotting Betweenness Centrality - (Vertex)

```r
set.seed(123)
d <- get.diameter(confianca, weights =E(confianca)$confianca )
E(confianca)$color_d <- "grey"
E(confianca)$width_d <- 0.1
E(confianca, path=d)$color_d <- "red"
E(confianca, path=d)$width_d <- 2
V(confianca)$label.color_d <- "blue"
V(confianca)$color_d  <- "SkyBlue2"
V(confianca)$color_label  <- "Non diameter"
V(confianca)[d]$label.color_d <- "black"
V(confianca)[d]$color_d <- "red"
V(confianca)[d]$color_label <- "Diameter"

#Plotting based only on degree measures 
edge.start <- ends(confianca, es=E(confianca), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(confianca))
maxC <- rep(Inf, vcount(confianca))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(confianca, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(confianca)$confianca)

#Plotting
plot(confianca, 
     layout=co,
     edge.color=E(confianca)$color_d,
     edge.arrow.size=(betweenness(confianca, weights = E(confianca)$confianca)+1)/100000,
     edge.width=E(confianca)$width_d,
     edge.curved = TRUE,
     vertex.color=V(confianca)$color_d,
     vertex.size=degree(confianca)/5,
     vertex.frame.color="black",
     vertex.label.color=V(confianca)$label.color_d,
     vertex.label=get.vertex.attribute(confianca,"LABEL_COR"),
     vertex.label.cex=(betweenness(confianca, weights = E(confianca)$confianca)+1)/10000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(confianca)$color_label 
b<-V(confianca)$color_d
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
  title("Diameter of 31_CONFIANÇA", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels =    sprintf("Diameter: %.2f",diameter(confianca, weights = E(confianca)$confianca))
             )
```

![](31_CONFIANÇA_15_diameter_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/15_diameter_confianca.RData") 
```


