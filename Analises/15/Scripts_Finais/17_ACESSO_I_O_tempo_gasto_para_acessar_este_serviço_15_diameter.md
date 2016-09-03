# SNA Diameter 17_ACESSO I) O tempo gasto para acessar este serviço impede a realização ativades em conjunto. (var15)
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
load("~/SNArRDJF/Robject/14_kcore_var15.RData")
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
#var15<-simplify(var15) #Simplify
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
## [1] 9
```

```
## + 6/187 vertices, named:
## [1] AJU_MUT_ Grupo A.A. Ipiranga                            
## [2] UAP_URB_ Ipiranga                                       
## [3] URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)
## [4] CRAS_AS_ CRAS Nordeste Grama                            
## [5] EA_DQCT_ Centro de Recuperação SOS Vida                 
## [6] EA_DQCT_ Comunidade Terapêutica Família em Cristo
```
##Plotting Diameter
##Plotting Betweenness Centrality - (Vertex)

```r
set.seed(123)
d <- get.diameter(var15, weights =E(var15)$var15 )
E(var15)$color_d <- "grey"
E(var15)$width_d <- 0.1
E(var15, path=d)$color_d <- "red"
E(var15, path=d)$width_d <- 2
V(var15)$label.color_d <- "blue"
V(var15)$color_d  <- "SkyBlue2"
V(var15)$color_label  <- "Non diameter"
V(var15)[d]$label.color_d <- "black"
V(var15)[d]$color_d <- "red"
V(var15)[d]$color_label <- "Diameter"

#Plotting based only on degree measures 
edge.start <- ends(var15, es=E(var15), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var15))
maxC <- rep(Inf, vcount(var15))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var15, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var15)$var15)

#Plotting
plot(var15, 
     layout=co,
     edge.color=E(var15)$color_d,
     edge.arrow.size=(betweenness(var15, weights = E(var15)$var15)+1)/100000,
     edge.width=E(var15)$width_d,
     edge.curved = TRUE,
     vertex.color=V(var15)$color_d,
     vertex.size=degree(var15)/5,
     vertex.frame.color="black",
     vertex.label.color=V(var15)$label.color_d,
     vertex.label=get.vertex.attribute(var15,"LABEL_COR"),
     vertex.label.cex=(betweenness(var15, weights = E(var15)$var15)+1)/10000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var15)$color_label 
b<-V(var15)$color_d
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
  title("Diameter of 17_ACESSO I) O tempo gasto para acessar este serviço impede a realização ativades em conjunto. (var15)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels =    sprintf("Diameter: %.2f",diameter(var15, weights = E(var15)$var15))
             )
```

![](17_ACESSO_I_O_tempo_gasto_para_acessar_este_serviço_15_diameter_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/15_diameter_var15.RData") 
```


