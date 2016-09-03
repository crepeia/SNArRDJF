# SNA Diameter 29_ACESSO_POSITIVO
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 29_ACESSO_POSITIVO

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/14_kcore_acesso_positivo.RData")
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
#acesso_positivo<-simplify(acesso_positivo) #Simplify
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
## [1] 97
```

```
## + 7/187 vertices, named:
## [1] EA_DQCT_ Centro Terapêutico Reconstruir (escritório)    
## [2] EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)       
## [3] ASS_HOS_ Hospital de Pronto Socorro – HPS               
## [4] URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)
## [5] ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza          
## [6] EA_DQCT_ Associação Beneficente Cristã Restituir        
## [7] AJU_MUT_ ALANON Grupo Libertação
```
##Plotting Diameter
##Plotting Betweenness Centrality - (Vertex)

```r
set.seed(123)
d <- get.diameter(acesso_positivo, weights =E(acesso_positivo)$acesso_positivo )
E(acesso_positivo)$color_d <- "grey"
E(acesso_positivo)$width_d <- 0.1
E(acesso_positivo, path=d)$color_d <- "red"
E(acesso_positivo, path=d)$width_d <- 2
V(acesso_positivo)$label.color_d <- "blue"
V(acesso_positivo)$color_d  <- "SkyBlue2"
V(acesso_positivo)$color_label  <- "Non diameter"
V(acesso_positivo)[d]$label.color_d <- "black"
V(acesso_positivo)[d]$color_d <- "red"
V(acesso_positivo)[d]$color_label <- "Diameter"

#Plotting based only on degree measures 
edge.start <- ends(acesso_positivo, es=E(acesso_positivo), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(acesso_positivo))
maxC <- rep(Inf, vcount(acesso_positivo))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(acesso_positivo, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(acesso_positivo)$acesso_positivo)

#Plotting
plot(acesso_positivo, 
     layout=co,
     edge.color=E(acesso_positivo)$color_d,
     edge.arrow.size=(betweenness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo)+1)/100000,
     edge.width=E(acesso_positivo)$width_d,
     edge.curved = TRUE,
     vertex.color=V(acesso_positivo)$color_d,
     vertex.size=degree(acesso_positivo)/5,
     vertex.frame.color="black",
     vertex.label.color=V(acesso_positivo)$label.color_d,
     vertex.label=get.vertex.attribute(acesso_positivo,"LABEL_COR"),
     vertex.label.cex=(betweenness(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo)+1)/10000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(acesso_positivo)$color_label 
b<-V(acesso_positivo)$color_d
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
  title("Diameter of 29_ACESSO_POSITIVO", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels =    sprintf("Diameter: %.2f",diameter(acesso_positivo, weights = E(acesso_positivo)$acesso_positivo))
             )
```

![](29_ACESSO_POSITIVO_15_diameter_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/15_diameter_acesso_positivo.RData") 
```


