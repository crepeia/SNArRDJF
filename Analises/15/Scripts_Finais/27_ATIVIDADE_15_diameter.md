# SNA Diameter 27_ATIVIDADE
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 27_ATIVIDADE

`#########################
`# Basic Preparation #####
`#########################

#Loading objects generated with previous script 

```r
rm(list = ls()) # removing previous objects to be sure that we don't have objects conflicts name
load("~/SNArRDJF/Robject/14_kcore_atividade.RData")
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
#atividade<-simplify(atividade) #Simplify
```


#Diameter - length of the longest geodesic.

##Diameter Non-weigthed 

```
## [1] 6
```

```
## + 7/187 vertices, named:
## [1] AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)
## [2] AJU_MUT_ Grupo A.A. Ipiranga                       
## [3] UAP_URB_ Santa Efigênia                            
## [4] ASS_HOS_ Hospital de Pronto Socorro – HPS          
## [5] CRE_SOC_ CREAS Idoso e Mulher                      
## [6] ASS_HOS_ Clínica Vila Verde (Hospital Dia)         
## [7] EA_DQCT_ Vila Verde (Unidade Borboleta)
```
##Diameter Weigthed 

```
## [1] 16
```

```
## + 7/187 vertices, named:
## [1] EA_DQCT_ Vila Verde (Unidade Bromélias)             
## [2] ASS_HOS_ Clínica Vila Verde (Hospital Dia)          
## [3] ASS_HOS_ Hospital Ana Nery                          
## [4] CRAS_AS_ CRAS Nordeste Grama                        
## [5] EA_DQCT_ Centro de Recuperação SOS Vida             
## [6] EA_DQCT_ Associação Beneficente Cristã Restituir    
## [7] EA_DQCT_ Centro Terapêutico Reconstruir (escritório)
```
##Plotting Diameter
##Plotting Betweenness Centrality - (Vertex)

```r
set.seed(123)
d <- get.diameter(atividade, weights =E(atividade)$atividade )
E(atividade)$color_d <- "grey"
E(atividade)$width_d <- 0.1
E(atividade, path=d)$color_d <- "red"
E(atividade, path=d)$width_d <- 2
V(atividade)$label.color_d <- "blue"
V(atividade)$color_d  <- "SkyBlue2"
V(atividade)$color_label  <- "Non diameter"
V(atividade)[d]$label.color_d <- "black"
V(atividade)[d]$color_d <- "red"
V(atividade)[d]$color_label <- "Diameter"

#Plotting based only on degree measures 
edge.start <- ends(atividade, es=E(atividade), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(atividade))
maxC <- rep(Inf, vcount(atividade))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(atividade, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(atividade)$atividade)

#Plotting
plot(atividade, 
     layout=co,
     edge.color=E(atividade)$color_d,
     edge.arrow.size=(betweenness(atividade, weights = E(atividade)$atividade)+1)/100000,
     edge.width=E(atividade)$width_d,
     edge.curved = TRUE,
     vertex.color=V(atividade)$color_d,
     vertex.size=degree(atividade)/5,
     vertex.frame.color="black",
     vertex.label.color=V(atividade)$label.color_d,
     vertex.label=get.vertex.attribute(atividade,"LABEL_COR"),
     vertex.label.cex=(betweenness(atividade, weights = E(atividade)$atividade)+1)/10000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(atividade)$color_label 
b<-V(atividade)$color_d
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
  title("Diameter of 27_ATIVIDADE", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels =    sprintf("Diameter: %.2f",diameter(atividade, weights = E(atividade)$atividade))
             )
```

![](27_ATIVIDADE_15_diameter_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/15_diameter_atividade.RData") 
```


