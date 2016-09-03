# SNA Interactive Plotting 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)
Leonardo Martins  
17 de julho de 2016  
SNA Descritive Analysis from "Projeto Redes de Atenção às pessoas que consomem álcool e outras Drogas em Juiz de Fora-MG   Brazil"  - SNArRDJF

Here you can find a basic script to analysis data from SNArRDJF - this script was elaborated considering its use for orther matrix adjacency data from SNArRDJF - Here we are going to analyse:

# 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)

`#########################
`# Basic Preparation #####
`#########################

##Loading objects generated with 1.Principal.Rmd Script - Please run this script only after run 1.Principal.Rmd

##Reload packages

```r
suppressMessages(library(RColorBrewer))
suppressMessages(library(car))
suppressMessages(library(xtable))
suppressMessages(library(igraph))
suppressMessages(library(magrittr))
suppressMessages(library(keyplayer))
suppressMessages(library(dplyr))
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




#Removing CAPS AD - not here

```r
#var2_without_CAPS<-delete_vertices(var2,V(var2)[V2_LABEL_ID=="q170_CAPS...CAPS.AD"])
var2_with_CAPS<-var2
```

##Vertices and Edges Number

```r
#1. Vertices and Edges
var2_with_CAPS_vcount<-vcount(var2_with_CAPS)
var2_with_CAPS_ecount<-ecount(var2_with_CAPS)
```
##Vertice Number
Each vertice represents a service - named as an actor in our network

```r
vcount(var2_with_CAPS)
```

```
## [1] 187
```
##Edge Number
Each edge represents a connection between two services named as a tie

```r
ecount(var2_with_CAPS)
```

```
## [1] 934
```

#Density - The proportion of present edges from all possible edges in the network.
The density of a binary network is simply the proportion of all possible ties that are actually present.

For a valued network, density is defined as the sum of the ties divided by the number of possible ties (i.e. the ratio of all tie strength that is actually present to the number of possible ties).  

The density of a network may give us insights into such phenomena as the speed at which information diffuses among the nodes, and the extent to which actors have high levels of social capital and/or social constraint.


##Edge Density
The density of a graph is the ratio of the number of edges and the number of possible edges.

```r
edge_density_var2_with_CAPS<-edge_density(var2_with_CAPS) #The proportion of present edges from all possible edges in the network.
edge_density_var2_with_CAPS
```

```
## [1] 0.02685297
```
##Edge connectivity - Adhesion
The edge connectivity of a graph or two vertices, this is recently also called group adhesion.

```r
edge_connectivity(var2_with_CAPS, source =NULL, target =NULL, checks = T) #source and target can be replaced - their are here just as default
```

```
## [1] 0
```
###Adhesion example
In order to use this we need to call source and target using the number of each vertex instead of the name - type in R to get numbers


```r
#Names and numbers

# list all if you have no idea about services id
# V(var2_with_CAPS)$name 

# list all three first (you can use c(1:3))
V(var2_with_CAPS)$name[1]  # just as an example
```

```
## [1] "ASS_HOS_ Hospital de Pronto Socorro – HPS"
```

```r
V(var2_with_CAPS)$name[2]  # just as an example
```

```
## [1] "AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)"
```

```r
V(var2_with_CAPS)$name[3]  # just as an example
```

```
## [1] "CAPS_AD"
```

```r
# list by others id's
V(var2_with_CAPS)$name[6]  # just as an example
```

```
## [1] "CRE_SOC_ CREAS Norte"
```

```r
V(var2_with_CAPS)$name[150]  # just as an example
```

```
## [1] "UAP_RUR_ Buiéié"
```

```r
V(var2_with_CAPS)$name[185]  # just as an example
```

```
## [1] "AJU_MUT_ Grupo A.A. Liberdade"
```
##Edge Connectivity - "CAPS_AD"==3 and "UAPS RURAL Buiéié"==150
Point connection calculates the number of nodes that would have to be removed in order for one actor to no longer be able to reach another.  If there are many different pathways that connect two actors, they have high "connectivity" in the sense that there are multiple ways for a signal to reach from one to the other - lower number - worse resilience 


```r
edge_connectivity(var2_with_CAPS, source =150, target =3, checks = T) 
```

```
## [1] 0
```

#Vertex Connectivity - Group Cohesion

It is the minimum number of vertices needed to remove to make the graph not strongly connected. (If the graph is not strongly connected then this is zero.). The cohesion of a graph (as defined by White and Harary, see references), is the vertex connectivity of the graph. This is calculated by cohesion.

These three functions essentially calculate the same measure(s), more precisely vertex_connectivity is the most general, the other two are included only for the ease of using more descriptive function names.


```r
vertex_connectivity(var2_with_CAPS, source = NULL, target = NULL, checks = TRUE)
```

```
## [1] 0
```

##Cohesion example

In order to use this we need to call source and target using the number of each vertex instead of the name - type in R to get numbers - see example above for more details 

##Vertex Connectivity - "CRE_SOC_ CREAS Norte"==6 and "AJU_MUT_ Grupo A.A. Liberdade" ==185
Minimum number of vertices needed to remove to make the vertex not connected by any vertex - it leads an error in case of using two connected vertex 

```r
vertex_connectivity(var2_with_CAPS, source =6, target =185, checks = T) 
```

```
## [1] 0
```

#Centrality Measures

• For undirected graphs:

– Actor centrality - involvement (connections) with other actors

• For directed graphs:

– Actor centrality - source of the ties (outgoing edges)

– Actor prestige - recipient of many ties (incoming edges)

In general - high centrality degree means direct contact with many other actors

##Centrality Degree (number of ties/nearest neighbors).

##Saving in igrpah object

```r
V(var2_with_CAPS)$var2_indegree<-degree(var2_with_CAPS, mode = "in") # Actor prestige - recipient of many ties (incoming edges)
V(var2_with_CAPS)$var2_outdegree <- degree(var2_with_CAPS, mode = "out") # Actor centrality - source of the ties (outgoing edges)
V(var2_with_CAPS)$var2_totaldegree <- degree(var2_with_CAPS, mode = "total")
```
##Saving in Global Environment as an object

```r
var2_with_CAPS_indegree<-degree(var2_with_CAPS, mode = "in")
var2_with_CAPS_outdegree<-degree(var2_with_CAPS, mode = "out")
var2_with_CAPS_totaldegree<-degree(var2_with_CAPS, mode = "total")
```

#Centrality Degree Descriptive Statistics - non-normalized

##Centrality Degree Descriptive Statistics - In

```r
##in
summary(var2_with_CAPS_indegree)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   4.000   4.995   5.000  52.000
```

```r
sd(var2_with_CAPS_indegree)
```

```
## [1] 5.795342
```
##Histogram var2_with_CAPS degree - In

```r
hist(degree(var2_with_CAPS, mode = "in", normalized = F), ylab="Frequency", xlab="Degree",  breaks=vcount(var2_with_CAPS)/10, main="Histogram of Indegree Nodes - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)")
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

##Centrality Degree Descriptive Statistics - Out

```r
##out
summary(var2_with_CAPS_outdegree)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   0.000   1.000   4.995   4.000  92.000
```

```r
sd(var2_with_CAPS_outdegree)
```

```
## [1] 12.13256
```

##Histogram var2_with_CAPS degree - Out

```r
hist(degree(var2_with_CAPS, mode = "out", normalized = F), ylab="Frequency", xlab="Degree",  breaks=vcount(var2_with_CAPS)/10, main="Histogram of Outdegree Nodes - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)")
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

##Centrality Degree Descriptive Statistics - All

```r
##all
summary(var2_with_CAPS_totaldegree)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   5.000   9.989   9.000 144.000
```

```r
sd(var2_with_CAPS_totaldegree)
```

```
## [1] 16.54222
```
##Histogram var2_with_CAPS degree - All

```r
hist(degree(var2_with_CAPS, mode = "all", normalized = F), ylab="Frequency", xlab="Degree",  breaks=vcount(var2_with_CAPS)/10, main="Histogram of All Degree Nodes - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)")
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

#Compute strength - weighted

A slightly more nuanced metric is “strength centrality”, which is defined as the sum of the weights of all the connections for a given node. This is also sometimes called “weighted degree centrality”

```r
V(var2_with_CAPS)$var2_strength<- strength(var2_with_CAPS, weights=E(var2_with_CAPS)$weight)
var2_strength<- strength(var2_with_CAPS, weights=E(var2_with_CAPS)$weight)
```
##Strength Stats

```r
summary(var2_strength)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   5.000   9.989   9.000 144.000
```

```r
sd(var2_strength)
```

```
## [1] 16.54222
```
##Histogram var2_with_CAPS degree - All

```r
hist(strength(var2_with_CAPS, weights=E(var2_with_CAPS)$weight), ylab="Frequency", xlab="Degree",  breaks=vcount(var2_with_CAPS)/10, main="Histogram of Strength Degree Nodes - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)")
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

#Centrality Degree Normalized

##Centrality Degree Normalized saving igraph object

```r
V(var2_with_CAPS)$var2_indegree_n<-degree(var2_with_CAPS, mode = "in", normalized = T)
V(var2_with_CAPS)$var2_outdegree_n<- degree(var2_with_CAPS, mode = "out", normalized = T)
V(var2_with_CAPS)$var2_totaldegree_n<- degree(var2_with_CAPS, mode = "total", normalized = T)
```
##Saving in Global Environment as an object

```r
var2_with_CAPS_indegree_n<-degree(var2_with_CAPS, mode = "in", normalized = T)
var2_with_CAPS_outdegree_n<-degree(var2_with_CAPS, mode = "out", normalized = T)
var2_with_CAPS_totaldegree_n<-degree(var2_with_CAPS, mode = "total", normalized = T)
```
##Centrality Degree Normalized Descriptive Statistics - in

```r
summary(var2_with_CAPS_indegree_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.00000 0.01075 0.02151 0.02685 0.02688 0.27960
```

```r
sd(var2_with_CAPS_indegree_n)
```

```
## [1] 0.03115775
```
##Histogram var2_with_CAPS degree normalized - in

```r
hist(degree(var2_with_CAPS, mode = "in", normalized = T), ylab="Frequency", xlab="Normalized Degree",  breaks=vcount(var2_with_CAPS)/10, main="Histogram of Normalized Indegree Nodes - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)")
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-29-1.png)<!-- -->

##Centrality Degree Normalized Descriptive Statistics - out

```r
summary(var2_with_CAPS_outdegree_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000000 0.000000 0.005376 0.026850 0.021510 0.494600
```

```r
sd(var2_with_CAPS_outdegree_n)
```

```
## [1] 0.06522879
```

##Histogram var2_with_CAPS degree normalized - out

```r
hist(degree(var2_with_CAPS, mode = "out", normalized = T), ylab="Frequency", xlab="Normalized Degree",  breaks=vcount(var2_with_CAPS)/10, main="Histogram of Normalized Outdegree Nodes - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)")
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-31-1.png)<!-- -->

##Centrality Degree Normalized Descriptive Statistics - all

```r
summary(var2_with_CAPS_totaldegree_n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.00000 0.01075 0.02688 0.05371 0.04839 0.77420
```

```r
sd(var2_with_CAPS_totaldegree_n)
```

```
## [1] 0.08893667
```

##Histogram var2_with_CAPS degree normalized - all

```r
hist(degree(var2_with_CAPS, mode = "all", normalized = T), ylab="Frequency", xlab="Normalized Degree",  breaks=vcount(var2_with_CAPS)/10, main="Histogram of Normalized All Degree Nodes - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)")
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-33-1.png)<!-- -->

#Centralization Degree

```r
V(var2_with_CAPS)$var2_centr_degree <- centralization.degree(var2_with_CAPS)$res
var2_centr_degree <- centralization.degree(var2_with_CAPS)
```

##Centralization

```r
var2_centr_degree$centralization
```

```
## [1] 0.3621806
```
##Theoretical Max

```r
var2_centr_degree$theoretical_max
```

```
## [1] 69192
```

#Degree distribution considering total equal one

```r
var2_with_CAPS_degree.distribution<-degree.distribution(var2_with_CAPS)
```

##Degree distribution Descriptive Stats

```r
summary(var2_with_CAPS_degree.distribution)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000000 0.000000 0.000000 0.006897 0.005348 0.171100
```

```r
sd(var2_with_CAPS_degree.distribution)
```

```
## [1] 0.02156529
```

##Histogram var2_with_CAPS distribution degree

```r
hist(degree.distribution(var2_with_CAPS), breaks=vcount(var2_with_CAPS)/10, ylab="Frequency", xlab="Degree Distribuition", main="Histogram of Degree Distribuition - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)")
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-39-1.png)<!-- -->

##Degree Probability Distribution

```r
dd <- degree.distribution(var2_with_CAPS, cumulative=T, mode="all")
```

##Degree Probability Distribution - Plot Cumulative Frequency

```r
plot(dd, pch=19, cex=1, col="orange", xlab="Degree", ylab="Cumulative Frequency", main= "Cumulative Frequency of 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2) ")
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-41-1.png)<!-- -->

#Log-Log Degree Distribution - Scale Free Network - Does it fit to power law ?

```r
dd.var2_with_CAPS <- degree.distribution(var2_with_CAPS)
d <- 1:max(degree(var2_with_CAPS))-1
ind <- (dd.var2_with_CAPS != 0)
```

##Plot Log-Log Degree Distribution

```r
plot(d[ind], 
     dd.var2_with_CAPS[ind], 
     log="xy", 
     col="blue",
     xlab=c("Log-Degree"), 
     ylab=c("Log-Intensity"),
     main="Log-Log Degree Distribution For 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)"
     )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-43-1.png)<!-- -->

#Average Neighbor Degree versus Vertex Degree (log-log scale for 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2))

The neighborhood of a given order y of a vertex v includes all vertices which are closer to v than the order. Ie. order y=0 is always v itself, order 1 is v plus its immediate neighbors, order 2 is order 1 plus the immediate neighbors of the vertices in order 1, etc.

##Simplify graph first 

```r
var2_with_CAPS_simplified<-simplify(var2_with_CAPS)
```

##Average Neighbor Degree versus vertex degree (log-log scale for var2_with_CAPS)

```r
var2_with_CAPS_a.nn.deg <- graph.knn(var2_with_CAPS_simplified, weights =E(var2_with_CAPS_simplified)$weight)$knn %>% round(1)
```

##Saving to igraph object

```r
V(var2_with_CAPS_simplified)$var2_with_CAPS_a.nn.deg <- graph.knn(var2_with_CAPS_simplified, weights=E(var2_with_CAPS_simplified)$weight)$knn
```

##Table Average Neighbor Degree

```r
d<-cbind(V(var2_with_CAPS_simplified)$LABEL_COR,var2_with_CAPS_a.nn.deg)
datatable(d)
```

<!--html_preserve--><div id="htmlwidget-a628d66f3b4c2895262d" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-a628d66f3b4c2895262d">{"x":{"filter":"none","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],["14.8","17.7","15.3","35.8","50.6","45","16.8","23.6","42.8","17.5","25.5","47.1","16.3","16.8","91.8","43.9","22.5","35.3","2","33.6","39.6","21.2","45.5","8.4","120","18.2","50.5","26.4","38.7","37.8","45.5","65.6","40.2","37","44.6","32.4","38.3","22.2","50.8","22.2","32.8","48.3","48.1","51.4","33.6","32.3","37","34.3","34.3","34.3","34.3","34.3","NaN","36.5","3.5","42.7","56.4","36","37","49.9","22.2","5.5","63.5","76","15.6","NaN","13.3","39.7","15","99","109","81.1","75.4","75.3","57","82.7","57.5","37.7","9","9","9","9","75.8","29.8","10.1","93","30.7","10","39.4","71.8","80.6","NaN","5.5","21.3","59.9","55.5","10","12","71.8","73.6","6.5","34.8","19","19","80.6","63.2","74","19","20","53.5","49.9","52.7","59.7","95.4","72.2","74.8","75.5","58.2","70.6","49.8","80.6","35.1","90","90","90","76.6","117.8","88","83.5","59.8","109","90","106.8","97.8","90","57.4","93.3","89.5","80.9","92.8","51.2","84.7","99.6","92","144","33.4","40","86","24.2","91.5","109","91.5","109","91.5","25","29.3","91.5","109","109","38","NaN","26","36","19.5","37","19.5","37","37","37","37","37","37","37","37","37","36","37","37","37","37","37","37","29.7","37","37","NaN","NaN"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>V1\u003c/th>\n      <th>var2_with_CAPS_a.nn.deg\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Plotting Average Neighbor Degree versus vertex degree

```r
plot(degree(var2_with_CAPS_simplified), 
     var2_with_CAPS_a.nn.deg, 
     log="xy", 
     col="goldenrod", 
     xlab=c("Log Vertex Degree"),
     ylab=c("Log Average Neighbor Degree"),
     main="Average Neighbor Degree vs Vertex Degree - Log-Log Scale for 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)"
     )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-48-1.png)<!-- -->

#Average Weighted Neighbor Degree versus vertex degree (log-log scale for weighted 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2))

```r
var2_with_CAPS_a.nn.deg_w <- graph.knn(var2_with_CAPS_simplified, weights=E(var2_with_CAPS_simplified)$weight)$knn %>% round(1)
```

##Saving to igraph object

```r
V(var2_with_CAPS_simplified)$var2_with_CAPS_a.nn.deg_w <-var2_with_CAPS_a.nn.deg <- graph.knn(var2_with_CAPS_simplified, weights=E(var2_with_CAPS_simplified)$weight)$knn
```

##Average Weighted Neighbor Descriptive

```r
summary(var2_with_CAPS_a.nn.deg_w)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    2.00   29.80   38.30   49.72   74.80  144.00       6
```

```r
sd(var2_with_CAPS_a.nn.deg_w, na.rm = T)
```

```
## [1] 29.79951
```

##Table Average Neighbor Degree Weighted

```r
d<-cbind(V(var2_with_CAPS_simplified)$LABEL_COR,var2_with_CAPS_a.nn.deg_w)
datatable(d)
```

<!--html_preserve--><div id="htmlwidget-562f679dafd25ff3049c" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-562f679dafd25ff3049c">{"x":{"filter":"none","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],["14.8","17.7","15.3","35.8","50.6","45","16.8","23.6","42.8","17.5","25.5","47.1","16.3","16.8","91.8","43.9","22.5","35.3","2","33.6","39.6","21.2","45.5","8.4","120","18.2","50.5","26.4","38.7","37.8","45.5","65.6","40.2","37","44.6","32.4","38.3","22.2","50.8","22.2","32.8","48.3","48.1","51.4","33.6","32.3","37","34.3","34.3","34.3","34.3","34.3","NaN","36.5","3.5","42.7","56.4","36","37","49.9","22.2","5.5","63.5","76","15.6","NaN","13.3","39.7","15","99","109","81.1","75.4","75.3","57","82.7","57.5","37.7","9","9","9","9","75.8","29.8","10.1","93","30.7","10","39.4","71.8","80.6","NaN","5.5","21.3","59.9","55.5","10","12","71.8","73.6","6.5","34.8","19","19","80.6","63.2","74","19","20","53.5","49.9","52.7","59.7","95.4","72.2","74.8","75.5","58.2","70.6","49.8","80.6","35.1","90","90","90","76.6","117.8","88","83.5","59.8","109","90","106.8","97.8","90","57.4","93.3","89.5","80.9","92.8","51.2","84.7","99.6","92","144","33.4","40","86","24.2","91.5","109","91.5","109","91.5","25","29.3","91.5","109","109","38","NaN","26","36","19.5","37","19.5","37","37","37","37","37","37","37","37","37","36","37","37","37","37","37","37","29.7","37","37","NaN","NaN"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>V1\u003c/th>\n      <th>var2_with_CAPS_a.nn.deg_w\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Plotting Average Neighbor Degree versus vertex degree

```r
plot(degree(var2_with_CAPS_simplified), 
     var2_with_CAPS_a.nn.deg, 
     log="xy", 
     col="goldenrod", 
     xlab=c("Log Vertex Degree"),
     ylab=c("Log Average Neighbor Degree"),
     main="Average Weighted Neighbor Degree vs Vertex Degree - Log-Log Scale For Weighted 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)"
     )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-53-1.png)<!-- -->

#Degree Centralities Dinamic Table

##Getting Degree Measures

```r
var2_with_CAPS_indegree<-degree(var2_with_CAPS, mode = "in")
var2_with_CAPS_outdegree<-degree(var2_with_CAPS, mode = "out")
var2_with_CAPS_totaldegree<-degree(var2_with_CAPS, mode = "total")
var2_strength<- strength(var2_with_CAPS, weights=E(var2_with_CAPS)$weight)
var2_with_CAPS_indegree_n<-degree(var2_with_CAPS, mode = "in", normalized = T) %>% round(3)
var2_with_CAPS_outdegree_n<-degree(var2_with_CAPS, mode = "out", normalized = T) %>% round(3)
var2_with_CAPS_totaldegree_n<-degree(var2_with_CAPS, mode = "total", normalized = T) %>% round(3)
var2_centr_degree <- centralization.degree(var2_with_CAPS)$res
var2_with_CAPS_a.nn.deg <- graph.knn(var2_with_CAPS_simplified)$knn %>% round(1)
var2_with_CAPS_a.nn.deg_w <- graph.knn(var2_with_CAPS_simplified, weights=E(var2_with_CAPS_simplified)$weight)$knn %>% round(1)
```

##Creating a dataframe of measures

```r
var2_with_CAPS_df_degree <- data.frame(var2_with_CAPS_indegree,
var2_with_CAPS_outdegree, 
var2_with_CAPS_totaldegree,
var2_with_CAPS_indegree_n, 
var2_with_CAPS_outdegree_n,
var2_with_CAPS_totaldegree_n,
var2_strength,
var2_centr_degree,
var2_with_CAPS_a.nn.deg,
var2_with_CAPS_a.nn.deg_w) %>% round(3)

#Adding type
var2_with_CAPS_df_degree <-cbind(var2_with_CAPS_df_degree, V(var2_with_CAPS)$LABEL_COR)

#Adding names
names(var2_with_CAPS_df_degree) <- c("In Degree", "Out Degree", "Total Degree","In Degree Normalized", "Out Degree Normalized", "Total Degree Normalized", "Strength","Centralization Degree","Average Neighbor Degree","Average Weighted Neighbor Degree","Type")

#Ordering Variables
var2_with_CAPS_df_degree<-var2_with_CAPS_df_degree[c("Type","In Degree", "Out Degree", "Total Degree","In Degree Normalized", "Out Degree Normalized", "Total Degree Normalized", "Strength","Centralization Degree","Average Neighbor Degree","Average Weighted Neighbor Degree")]
```

##General tabel - DT

```r
datatable(var2_with_CAPS_df_degree, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-765ae113e23037de0f3c" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-765ae113e23037de0f3c">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"52\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"92\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"144\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"0.28\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"0.495\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"0.774\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"144\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"144\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2\" data-max=\"144\" data-scale=\"1\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2\" data-max=\"144\" data-scale=\"1\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[31,18,52,10,15,10,6,5,1,2,14,14,1,3,4,2,12,13,1,4,12,3,11,1,0,1,2,16,10,10,14,5,16,2,10,2,2,2,1,14,14,9,7,4,1,20,2,1,1,1,1,1,0,6,1,10,8,1,2,16,4,2,6,5,2,0,1,5,2,3,3,4,5,5,6,4,6,22,4,4,4,4,5,3,7,4,2,1,5,3,3,0,2,4,4,4,2,3,4,7,1,5,1,1,5,5,5,1,2,8,8,6,6,4,4,5,6,6,5,10,5,6,4,4,4,4,3,4,5,3,3,4,3,4,4,5,4,5,6,4,8,5,3,5,0,21,12,4,4,2,3,2,3,2,1,2,2,3,3,1,0,3,1,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,3,2,2,0,0],[89,45,92,27,3,0,27,18,5,2,10,0,17,9,0,8,58,0,0,5,9,5,9,37,1,3,2,25,25,18,5,4,2,0,2,3,5,3,4,31,27,4,4,1,6,0,0,8,8,8,8,8,0,9,35,6,0,0,0,2,0,2,0,3,3,0,2,4,2,0,0,3,0,1,5,3,2,10,4,4,4,4,0,1,5,2,1,1,14,2,5,0,2,3,6,2,0,0,1,0,1,18,0,0,0,7,1,0,0,0,1,3,1,1,1,1,2,5,5,2,0,12,0,0,0,1,1,2,1,1,0,0,2,1,0,4,2,1,1,0,1,1,2,1,1,1,2,0,2,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[120,63,144,37,18,10,33,23,6,4,24,14,18,12,4,10,70,13,1,9,21,8,20,38,1,4,4,41,35,28,19,9,18,2,12,5,7,5,5,45,41,13,11,5,7,20,2,9,9,9,9,9,0,15,36,16,8,1,2,18,4,4,6,8,5,0,3,9,4,3,3,7,5,6,11,7,8,32,8,8,8,8,5,4,12,6,3,2,19,5,8,0,4,7,10,6,2,3,5,7,2,23,1,1,5,12,6,1,2,8,9,9,7,5,5,6,8,11,10,12,5,18,4,4,4,5,4,6,6,4,3,4,5,5,4,9,6,6,7,4,9,6,5,6,1,22,14,4,6,2,3,2,3,2,2,3,2,3,3,1,0,3,1,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,3,2,2,0,0],[0.167,0.097,0.28,0.054,0.081,0.054,0.032,0.027,0.005,0.011,0.075,0.075,0.005,0.016,0.022,0.011,0.065,0.07,0.005,0.022,0.065,0.016,0.059,0.005,0,0.005,0.011,0.086,0.054,0.054,0.075,0.027,0.086,0.011,0.054,0.011,0.011,0.011,0.005,0.075,0.075,0.048,0.038,0.022,0.005,0.108,0.011,0.005,0.005,0.005,0.005,0.005,0,0.032,0.005,0.054,0.043,0.005,0.011,0.086,0.022,0.011,0.032,0.027,0.011,0,0.005,0.027,0.011,0.016,0.016,0.022,0.027,0.027,0.032,0.022,0.032,0.118,0.022,0.022,0.022,0.022,0.027,0.016,0.038,0.022,0.011,0.005,0.027,0.016,0.016,0,0.011,0.022,0.022,0.022,0.011,0.016,0.022,0.038,0.005,0.027,0.005,0.005,0.027,0.027,0.027,0.005,0.011,0.043,0.043,0.032,0.032,0.022,0.022,0.027,0.032,0.032,0.027,0.054,0.027,0.032,0.022,0.022,0.022,0.022,0.016,0.022,0.027,0.016,0.016,0.022,0.016,0.022,0.022,0.027,0.022,0.027,0.032,0.022,0.043,0.027,0.016,0.027,0,0.113,0.065,0.022,0.022,0.011,0.016,0.011,0.016,0.011,0.005,0.011,0.011,0.016,0.016,0.005,0,0.016,0.005,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.005,0.011,0.011,0.011,0.011,0.011,0.011,0.016,0.011,0.011,0,0],[0.478,0.242,0.495,0.145,0.016,0,0.145,0.097,0.027,0.011,0.054,0,0.091,0.048,0,0.043,0.312,0,0,0.027,0.048,0.027,0.048,0.199,0.005,0.016,0.011,0.134,0.134,0.097,0.027,0.022,0.011,0,0.011,0.016,0.027,0.016,0.022,0.167,0.145,0.022,0.022,0.005,0.032,0,0,0.043,0.043,0.043,0.043,0.043,0,0.048,0.188,0.032,0,0,0,0.011,0,0.011,0,0.016,0.016,0,0.011,0.022,0.011,0,0,0.016,0,0.005,0.027,0.016,0.011,0.054,0.022,0.022,0.022,0.022,0,0.005,0.027,0.011,0.005,0.005,0.075,0.011,0.027,0,0.011,0.016,0.032,0.011,0,0,0.005,0,0.005,0.097,0,0,0,0.038,0.005,0,0,0,0.005,0.016,0.005,0.005,0.005,0.005,0.011,0.027,0.027,0.011,0,0.065,0,0,0,0.005,0.005,0.011,0.005,0.005,0,0,0.011,0.005,0,0.022,0.011,0.005,0.005,0,0.005,0.005,0.011,0.005,0.005,0.005,0.011,0,0.011,0,0,0,0,0,0.005,0.005,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0.645,0.339,0.774,0.199,0.097,0.054,0.177,0.124,0.032,0.022,0.129,0.075,0.097,0.065,0.022,0.054,0.376,0.07,0.005,0.048,0.113,0.043,0.108,0.204,0.005,0.022,0.022,0.22,0.188,0.151,0.102,0.048,0.097,0.011,0.065,0.027,0.038,0.027,0.027,0.242,0.22,0.07,0.059,0.027,0.038,0.108,0.011,0.048,0.048,0.048,0.048,0.048,0,0.081,0.194,0.086,0.043,0.005,0.011,0.097,0.022,0.022,0.032,0.043,0.027,0,0.016,0.048,0.022,0.016,0.016,0.038,0.027,0.032,0.059,0.038,0.043,0.172,0.043,0.043,0.043,0.043,0.027,0.022,0.065,0.032,0.016,0.011,0.102,0.027,0.043,0,0.022,0.038,0.054,0.032,0.011,0.016,0.027,0.038,0.011,0.124,0.005,0.005,0.027,0.065,0.032,0.005,0.011,0.043,0.048,0.048,0.038,0.027,0.027,0.032,0.043,0.059,0.054,0.065,0.027,0.097,0.022,0.022,0.022,0.027,0.022,0.032,0.032,0.022,0.016,0.022,0.027,0.027,0.022,0.048,0.032,0.032,0.038,0.022,0.048,0.032,0.027,0.032,0.005,0.118,0.075,0.022,0.032,0.011,0.016,0.011,0.016,0.011,0.011,0.016,0.011,0.016,0.016,0.005,0,0.016,0.005,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.005,0.011,0.011,0.011,0.011,0.011,0.011,0.016,0.011,0.011,0,0],[120,63,144,37,18,10,33,23,6,4,24,14,18,12,4,10,70,13,1,9,21,8,20,38,1,4,4,41,35,28,19,9,18,2,12,5,7,5,5,45,41,13,11,5,7,20,2,9,9,9,9,9,0,15,36,16,8,1,2,18,4,4,6,8,5,0,3,9,4,3,3,7,5,6,11,7,8,32,8,8,8,8,5,4,12,6,3,2,19,5,8,0,4,7,10,6,2,3,5,7,2,23,1,1,5,12,6,1,2,8,9,9,7,5,5,6,8,11,10,12,5,18,4,4,4,5,4,6,6,4,3,4,5,5,4,9,6,6,7,4,9,6,5,6,1,22,14,4,6,2,3,2,3,2,2,3,2,3,3,1,0,3,1,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,3,2,2,0,0],[120,63,144,37,18,10,33,23,6,4,24,14,18,12,4,10,70,13,1,9,21,8,20,38,1,4,4,41,35,28,19,9,18,2,12,5,7,5,5,45,41,13,11,5,7,20,2,9,9,9,9,9,0,15,36,16,8,1,2,18,4,4,6,8,5,0,3,9,4,3,3,7,5,6,11,7,8,32,8,8,8,8,5,4,12,6,3,2,19,5,8,0,4,7,10,6,2,3,5,7,2,23,1,1,5,12,6,1,2,8,9,9,7,5,5,6,8,11,10,12,5,18,4,4,4,5,4,6,6,4,3,4,5,5,4,9,6,6,7,4,9,6,5,6,1,22,14,4,6,2,3,2,3,2,2,3,2,3,3,1,0,3,1,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,3,2,2,0,0],[14.8,17.7,15.3,35.8,50.6,45,16.8,23.6,42.8,17.5,25.5,47.1,16.3,16.8,91.8,43.9,22.5,35.3,2,33.6,39.6,21.2,45.5,8.4,120,18.2,50.5,26.4,38.7,37.8,45.5,65.6,40.2,37,44.6,32.4,38.3,22.2,50.8,22.2,32.8,48.3,48.1,51.4,33.6,32.3,37,34.3,34.3,34.3,34.3,34.3,null,36.5,3.5,42.7,56.4,36,37,49.9,22.2,5.5,63.5,76,15.6,null,13.3,39.7,15,99,109,81.1,75.4,75.3,57,82.7,57.5,37.7,9,9,9,9,75.8,29.8,10.1,93,30.7,10,39.4,71.8,80.6,null,5.5,21.3,59.9,55.5,10,12,71.8,73.6,6.5,34.8,19,19,80.6,63.2,74,19,20,53.5,49.9,52.7,59.7,95.4,72.2,74.8,75.5,58.2,70.6,49.8,80.6,35.1,90,90,90,76.6,117.8,88,83.5,59.8,109,90,106.8,97.8,90,57.4,93.3,89.5,80.9,92.8,51.2,84.7,99.6,92,144,33.4,40,86,24.2,91.5,109,91.5,109,91.5,25,29.3,91.5,109,109,38,null,26,36,19.5,37,19.5,37,37,37,37,37,37,37,37,37,36,37,37,37,37,37,37,29.7,37,37,null,null],[14.8,17.7,15.3,35.8,50.6,45,16.8,23.6,42.8,17.5,25.5,47.1,16.3,16.8,91.8,43.9,22.5,35.3,2,33.6,39.6,21.2,45.5,8.4,120,18.2,50.5,26.4,38.7,37.8,45.5,65.6,40.2,37,44.6,32.4,38.3,22.2,50.8,22.2,32.8,48.3,48.1,51.4,33.6,32.3,37,34.3,34.3,34.3,34.3,34.3,null,36.5,3.5,42.7,56.4,36,37,49.9,22.2,5.5,63.5,76,15.6,null,13.3,39.7,15,99,109,81.1,75.4,75.3,57,82.7,57.5,37.7,9,9,9,9,75.8,29.8,10.1,93,30.7,10,39.4,71.8,80.6,null,5.5,21.3,59.9,55.5,10,12,71.8,73.6,6.5,34.8,19,19,80.6,63.2,74,19,20,53.5,49.9,52.7,59.7,95.4,72.2,74.8,75.5,58.2,70.6,49.8,80.6,35.1,90,90,90,76.6,117.8,88,83.5,59.8,109,90,106.8,97.8,90,57.4,93.3,89.5,80.9,92.8,51.2,84.7,99.6,92,144,33.4,40,86,24.2,91.5,109,91.5,109,91.5,25,29.3,91.5,109,109,38,null,26,36,19.5,37,19.5,37,37,37,37,37,37,37,37,37,36,37,37,37,37,37,37,29.7,37,37,null,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Degree\u003c/th>\n      <th>Out Degree\u003c/th>\n      <th>Total Degree\u003c/th>\n      <th>In Degree Normalized\u003c/th>\n      <th>Out Degree Normalized\u003c/th>\n      <th>Total Degree Normalized\u003c/th>\n      <th>Strength\u003c/th>\n      <th>Centralization Degree\u003c/th>\n      <th>Average Neighbor Degree\u003c/th>\n      <th>Average Weighted Neighbor Degree\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(. ~ Type, var2_with_CAPS_df_degree, function(x) c(mean=mean(x)))

#Removing Type variable
names(aggdata_mean) <- c("Group", "In Degree(M)", "Out Degree(M)", "Total Degree(M)","In Degree Normalized(M)", "Out Degree Normalized(M)", "Total Degree Normalized(M)", "Strength(M)","Centralization Degree(M)","Average Neighbor Degree(M)","Average Weighted Neighbor Degree(M)")
```

##Aggregating data from previous table - median

```r
aggdata_median <-aggregate(. ~ Type, var2_with_CAPS_df_degree, function(x) c(median=median(x)))

#Removing Type variable
#aggdata_median<-aggdata_median[,-c(2)]
names(aggdata_median) <- c("Group", "In Degree(median)", "Out Degree(median)", "Total Degree(median)","In Degree Normalized(median)", "Out Degree Normalized(median)", "Total Degree Normalized(median)", "Strength(median)","Centralization Degree(median)","Average Neighbor Degree(median)","Average Weighted Neighbor Degree(median)")
```

##Merging mean and median

```r
total_table <- merge(aggdata_mean,aggdata_median,by="Group")

#Rounding
Group<-total_table[,c(1)] #Keeping group
total_table<-total_table[,-c(1)] %>% round(2) #Rouding
total_table<-cbind(Group,total_table) #Binding toghter

#Organizing Variabels
total_table<-total_table[c("Group","In Degree(M)","In Degree(median)", "Out Degree(M)", "Out Degree(median)","Total Degree(M)", "Total Degree(median)", "In Degree Normalized(M)", "In Degree Normalized(median)", "Out Degree Normalized(M)", "Out Degree Normalized(median)", "Total Degree Normalized(M)", "Total Degree Normalized(median)", "Strength(M)","Strength(median)", "Centralization Degree(M)","Centralization Degree(median)","Average Neighbor Degree(M)","Average Neighbor Degree(median)","Average Weighted Neighbor Degree(M)", "Average Weighted Neighbor Degree(median)")]
```

##Plotting final table with round

```r
datatable(total_table, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-fead6f6076cda9770d3c" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-fead6f6076cda9770d3c">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.25\" data-max=\"52\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1\" data-max=\"52\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"92\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"92\" data-scale=\"1\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.84\" data-max=\"144\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2\" data-max=\"144\" data-scale=\"1\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.01\" data-max=\"0.29\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"0.29\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"0.5\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"0.5\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.02\" data-max=\"0.77\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.01\" data-max=\"0.77\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.84\" data-max=\"144\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2\" data-max=\"144\" data-scale=\"1\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.84\" data-max=\"144\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2\" data-max=\"144\" data-scale=\"1\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"14.8\" data-max=\"92.07\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"14.8\" data-max=\"91.5\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"14.8\" data-max=\"92.07\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"14.8\" data-max=\"91.5\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[11.33,2.21,4,18,6,16,16.33,52,11,12,22,2.47,6,12.44,12.33,3.82,7,31,1.25,13,3.37,5],[14,2,4,18,6,16,16,52,11,12,22,2,6,10,12,2,7,31,1,13,3,5],[10.33,2.4,5,45,27,2,11.67,92,9,2,10,4.12,6.5,10.33,20.33,7.27,0,89,4.58,0,0.47,2.15],[2,0,5,45,27,2,9,92,9,2,10,2,6.5,6,3,4,0,89,6,0,0,1],[21.67,4.6,9,63,33,18,28,144,20,14,32,6.59,12.5,22.78,32.67,11.09,7,120,5.83,13,3.84,7.15],[18,2,9,63,33,18,22,144,20,14,32,5,12.5,20,18,7,7,120,7,13,4,6],[0.06,0.01,0.02,0.1,0.03,0.09,0.09,0.28,0.06,0.06,0.12,0.01,0.03,0.07,0.07,0.02,0.04,0.17,0.01,0.07,0.02,0.03],[0.08,0.01,0.02,0.1,0.03,0.09,0.09,0.28,0.06,0.06,0.12,0.01,0.03,0.05,0.06,0.01,0.04,0.17,0,0.07,0.02,0.03],[0.06,0.01,0.03,0.24,0.14,0.01,0.06,0.5,0.05,0.01,0.05,0.02,0.04,0.06,0.11,0.04,0,0.48,0.02,0,0,0.01],[0.01,0,0.03,0.24,0.14,0.01,0.05,0.5,0.05,0.01,0.05,0.01,0.04,0.03,0.02,0.02,0,0.48,0.03,0,0,0],[0.12,0.02,0.05,0.34,0.18,0.1,0.15,0.77,0.11,0.08,0.17,0.04,0.07,0.12,0.18,0.06,0.04,0.64,0.03,0.07,0.02,0.04],[0.1,0.01,0.05,0.34,0.18,0.1,0.12,0.77,0.11,0.08,0.17,0.03,0.07,0.11,0.1,0.04,0.04,0.64,0.04,0.07,0.02,0.03],[21.67,4.6,9,63,33,18,28,144,20,14,32,6.59,12.5,22.78,32.67,11.09,7,120,5.83,13,3.84,7.15],[18,2,9,63,33,18,22,144,20,14,32,5,12.5,20,18,7,7,120,7,13,4,6],[21.67,4.6,9,63,33,18,28,144,20,14,32,6.59,12.5,22.78,32.67,11.09,7,120,5.83,13,3.84,7.15],[18,2,9,63,33,18,22,144,20,14,32,5,12.5,20,18,7,7,120,7,13,4,6],[32.4,29.4,33.6,17.7,16.8,49.9,33.13,15.3,45.5,40,37.7,24.09,32.55,38.89,39.37,45.55,73.6,14.8,30.45,35.3,92.07,74.98],[32.8,37,33.6,17.7,16.8,49.9,33.4,15.3,45.5,40,37.7,21.2,32.55,38.7,45,34.8,73.6,14.8,34.3,35.3,91.5,75.45],[32.4,29.4,33.6,17.7,16.8,49.9,33.13,15.3,45.5,40,37.7,24.09,32.55,38.89,39.37,45.55,73.6,14.8,30.45,35.3,92.07,74.98],[32.8,37,33.6,17.7,16.8,49.9,33.4,15.3,45.5,40,37.7,21.2,32.55,38.7,45,34.8,73.6,14.8,34.3,35.3,91.5,75.45]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Degree(M)\u003c/th>\n      <th>In Degree(median)\u003c/th>\n      <th>Out Degree(M)\u003c/th>\n      <th>Out Degree(median)\u003c/th>\n      <th>Total Degree(M)\u003c/th>\n      <th>Total Degree(median)\u003c/th>\n      <th>In Degree Normalized(M)\u003c/th>\n      <th>In Degree Normalized(median)\u003c/th>\n      <th>Out Degree Normalized(M)\u003c/th>\n      <th>Out Degree Normalized(median)\u003c/th>\n      <th>Total Degree Normalized(M)\u003c/th>\n      <th>Total Degree Normalized(median)\u003c/th>\n      <th>Strength(M)\u003c/th>\n      <th>Strength(median)\u003c/th>\n      <th>Centralization Degree(M)\u003c/th>\n      <th>Centralization Degree(median)\u003c/th>\n      <th>Average Neighbor Degree(M)\u003c/th>\n      <th>Average Neighbor Degree(median)\u003c/th>\n      <th>Average Weighted Neighbor Degree(M)\u003c/th>\n      <th>Average Weighted Neighbor Degree(median)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a dataframe of measures (Natureza Governamental)

```r
var2_with_CAPS_df_degree <- data.frame(var2_with_CAPS_indegree,
var2_with_CAPS_outdegree, 
var2_with_CAPS_totaldegree,
var2_with_CAPS_indegree_n, 
var2_with_CAPS_outdegree_n,
var2_with_CAPS_totaldegree_n,
var2_strength,
var2_centr_degree,
var2_with_CAPS_a.nn.deg,
var2_with_CAPS_a.nn.deg_w) %>% round(3)

#Adding type
var2_with_CAPS_df_degree <-cbind(var2_with_CAPS_df_degree, V(var2_with_CAPS)$TIPO1)

#Adding names
names(var2_with_CAPS_df_degree) <- c("In Degree", "Out Degree", "Total Degree","In Degree Normalized", "Out Degree Normalized", "Total Degree Normalized", "Strength","Centralization Degree","Average Neighbor Degree","Average Weighted Neighbor Degree","Type")

#Ordering Variables
var2_with_CAPS_df_degree<-var2_with_CAPS_df_degree[c("Type","In Degree", "Out Degree", "Total Degree","In Degree Normalized", "Out Degree Normalized", "Total Degree Normalized", "Strength","Centralization Degree","Average Neighbor Degree","Average Weighted Neighbor Degree")]
```

##General tabel - DT

```r
datatable(var2_with_CAPS_df_degree, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-3b3b0f7f3ec28cf216f2" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-3b3b0f7f3ec28cf216f2">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"52\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"92\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"144\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"0.28\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"0.495\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"0.774\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"144\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"144\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2\" data-max=\"144\" data-scale=\"1\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2\" data-max=\"144\" data-scale=\"1\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[31,18,52,10,15,10,6,5,1,2,14,14,1,3,4,2,12,13,1,4,12,3,11,1,0,1,2,16,10,10,14,5,16,2,10,2,2,2,1,14,14,9,7,4,1,20,2,1,1,1,1,1,0,6,1,10,8,1,2,16,4,2,6,5,2,0,1,5,2,3,3,4,5,5,6,4,6,22,4,4,4,4,5,3,7,4,2,1,5,3,3,0,2,4,4,4,2,3,4,7,1,5,1,1,5,5,5,1,2,8,8,6,6,4,4,5,6,6,5,10,5,6,4,4,4,4,3,4,5,3,3,4,3,4,4,5,4,5,6,4,8,5,3,5,0,21,12,4,4,2,3,2,3,2,1,2,2,3,3,1,0,3,1,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,3,2,2,0,0],[89,45,92,27,3,0,27,18,5,2,10,0,17,9,0,8,58,0,0,5,9,5,9,37,1,3,2,25,25,18,5,4,2,0,2,3,5,3,4,31,27,4,4,1,6,0,0,8,8,8,8,8,0,9,35,6,0,0,0,2,0,2,0,3,3,0,2,4,2,0,0,3,0,1,5,3,2,10,4,4,4,4,0,1,5,2,1,1,14,2,5,0,2,3,6,2,0,0,1,0,1,18,0,0,0,7,1,0,0,0,1,3,1,1,1,1,2,5,5,2,0,12,0,0,0,1,1,2,1,1,0,0,2,1,0,4,2,1,1,0,1,1,2,1,1,1,2,0,2,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[120,63,144,37,18,10,33,23,6,4,24,14,18,12,4,10,70,13,1,9,21,8,20,38,1,4,4,41,35,28,19,9,18,2,12,5,7,5,5,45,41,13,11,5,7,20,2,9,9,9,9,9,0,15,36,16,8,1,2,18,4,4,6,8,5,0,3,9,4,3,3,7,5,6,11,7,8,32,8,8,8,8,5,4,12,6,3,2,19,5,8,0,4,7,10,6,2,3,5,7,2,23,1,1,5,12,6,1,2,8,9,9,7,5,5,6,8,11,10,12,5,18,4,4,4,5,4,6,6,4,3,4,5,5,4,9,6,6,7,4,9,6,5,6,1,22,14,4,6,2,3,2,3,2,2,3,2,3,3,1,0,3,1,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,3,2,2,0,0],[0.167,0.097,0.28,0.054,0.081,0.054,0.032,0.027,0.005,0.011,0.075,0.075,0.005,0.016,0.022,0.011,0.065,0.07,0.005,0.022,0.065,0.016,0.059,0.005,0,0.005,0.011,0.086,0.054,0.054,0.075,0.027,0.086,0.011,0.054,0.011,0.011,0.011,0.005,0.075,0.075,0.048,0.038,0.022,0.005,0.108,0.011,0.005,0.005,0.005,0.005,0.005,0,0.032,0.005,0.054,0.043,0.005,0.011,0.086,0.022,0.011,0.032,0.027,0.011,0,0.005,0.027,0.011,0.016,0.016,0.022,0.027,0.027,0.032,0.022,0.032,0.118,0.022,0.022,0.022,0.022,0.027,0.016,0.038,0.022,0.011,0.005,0.027,0.016,0.016,0,0.011,0.022,0.022,0.022,0.011,0.016,0.022,0.038,0.005,0.027,0.005,0.005,0.027,0.027,0.027,0.005,0.011,0.043,0.043,0.032,0.032,0.022,0.022,0.027,0.032,0.032,0.027,0.054,0.027,0.032,0.022,0.022,0.022,0.022,0.016,0.022,0.027,0.016,0.016,0.022,0.016,0.022,0.022,0.027,0.022,0.027,0.032,0.022,0.043,0.027,0.016,0.027,0,0.113,0.065,0.022,0.022,0.011,0.016,0.011,0.016,0.011,0.005,0.011,0.011,0.016,0.016,0.005,0,0.016,0.005,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.005,0.011,0.011,0.011,0.011,0.011,0.011,0.016,0.011,0.011,0,0],[0.478,0.242,0.495,0.145,0.016,0,0.145,0.097,0.027,0.011,0.054,0,0.091,0.048,0,0.043,0.312,0,0,0.027,0.048,0.027,0.048,0.199,0.005,0.016,0.011,0.134,0.134,0.097,0.027,0.022,0.011,0,0.011,0.016,0.027,0.016,0.022,0.167,0.145,0.022,0.022,0.005,0.032,0,0,0.043,0.043,0.043,0.043,0.043,0,0.048,0.188,0.032,0,0,0,0.011,0,0.011,0,0.016,0.016,0,0.011,0.022,0.011,0,0,0.016,0,0.005,0.027,0.016,0.011,0.054,0.022,0.022,0.022,0.022,0,0.005,0.027,0.011,0.005,0.005,0.075,0.011,0.027,0,0.011,0.016,0.032,0.011,0,0,0.005,0,0.005,0.097,0,0,0,0.038,0.005,0,0,0,0.005,0.016,0.005,0.005,0.005,0.005,0.011,0.027,0.027,0.011,0,0.065,0,0,0,0.005,0.005,0.011,0.005,0.005,0,0,0.011,0.005,0,0.022,0.011,0.005,0.005,0,0.005,0.005,0.011,0.005,0.005,0.005,0.011,0,0.011,0,0,0,0,0,0.005,0.005,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0.645,0.339,0.774,0.199,0.097,0.054,0.177,0.124,0.032,0.022,0.129,0.075,0.097,0.065,0.022,0.054,0.376,0.07,0.005,0.048,0.113,0.043,0.108,0.204,0.005,0.022,0.022,0.22,0.188,0.151,0.102,0.048,0.097,0.011,0.065,0.027,0.038,0.027,0.027,0.242,0.22,0.07,0.059,0.027,0.038,0.108,0.011,0.048,0.048,0.048,0.048,0.048,0,0.081,0.194,0.086,0.043,0.005,0.011,0.097,0.022,0.022,0.032,0.043,0.027,0,0.016,0.048,0.022,0.016,0.016,0.038,0.027,0.032,0.059,0.038,0.043,0.172,0.043,0.043,0.043,0.043,0.027,0.022,0.065,0.032,0.016,0.011,0.102,0.027,0.043,0,0.022,0.038,0.054,0.032,0.011,0.016,0.027,0.038,0.011,0.124,0.005,0.005,0.027,0.065,0.032,0.005,0.011,0.043,0.048,0.048,0.038,0.027,0.027,0.032,0.043,0.059,0.054,0.065,0.027,0.097,0.022,0.022,0.022,0.027,0.022,0.032,0.032,0.022,0.016,0.022,0.027,0.027,0.022,0.048,0.032,0.032,0.038,0.022,0.048,0.032,0.027,0.032,0.005,0.118,0.075,0.022,0.032,0.011,0.016,0.011,0.016,0.011,0.011,0.016,0.011,0.016,0.016,0.005,0,0.016,0.005,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.005,0.011,0.011,0.011,0.011,0.011,0.011,0.016,0.011,0.011,0,0],[120,63,144,37,18,10,33,23,6,4,24,14,18,12,4,10,70,13,1,9,21,8,20,38,1,4,4,41,35,28,19,9,18,2,12,5,7,5,5,45,41,13,11,5,7,20,2,9,9,9,9,9,0,15,36,16,8,1,2,18,4,4,6,8,5,0,3,9,4,3,3,7,5,6,11,7,8,32,8,8,8,8,5,4,12,6,3,2,19,5,8,0,4,7,10,6,2,3,5,7,2,23,1,1,5,12,6,1,2,8,9,9,7,5,5,6,8,11,10,12,5,18,4,4,4,5,4,6,6,4,3,4,5,5,4,9,6,6,7,4,9,6,5,6,1,22,14,4,6,2,3,2,3,2,2,3,2,3,3,1,0,3,1,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,3,2,2,0,0],[120,63,144,37,18,10,33,23,6,4,24,14,18,12,4,10,70,13,1,9,21,8,20,38,1,4,4,41,35,28,19,9,18,2,12,5,7,5,5,45,41,13,11,5,7,20,2,9,9,9,9,9,0,15,36,16,8,1,2,18,4,4,6,8,5,0,3,9,4,3,3,7,5,6,11,7,8,32,8,8,8,8,5,4,12,6,3,2,19,5,8,0,4,7,10,6,2,3,5,7,2,23,1,1,5,12,6,1,2,8,9,9,7,5,5,6,8,11,10,12,5,18,4,4,4,5,4,6,6,4,3,4,5,5,4,9,6,6,7,4,9,6,5,6,1,22,14,4,6,2,3,2,3,2,2,3,2,3,3,1,0,3,1,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,3,2,2,0,0],[14.8,17.7,15.3,35.8,50.6,45,16.8,23.6,42.8,17.5,25.5,47.1,16.3,16.8,91.8,43.9,22.5,35.3,2,33.6,39.6,21.2,45.5,8.4,120,18.2,50.5,26.4,38.7,37.8,45.5,65.6,40.2,37,44.6,32.4,38.3,22.2,50.8,22.2,32.8,48.3,48.1,51.4,33.6,32.3,37,34.3,34.3,34.3,34.3,34.3,null,36.5,3.5,42.7,56.4,36,37,49.9,22.2,5.5,63.5,76,15.6,null,13.3,39.7,15,99,109,81.1,75.4,75.3,57,82.7,57.5,37.7,9,9,9,9,75.8,29.8,10.1,93,30.7,10,39.4,71.8,80.6,null,5.5,21.3,59.9,55.5,10,12,71.8,73.6,6.5,34.8,19,19,80.6,63.2,74,19,20,53.5,49.9,52.7,59.7,95.4,72.2,74.8,75.5,58.2,70.6,49.8,80.6,35.1,90,90,90,76.6,117.8,88,83.5,59.8,109,90,106.8,97.8,90,57.4,93.3,89.5,80.9,92.8,51.2,84.7,99.6,92,144,33.4,40,86,24.2,91.5,109,91.5,109,91.5,25,29.3,91.5,109,109,38,null,26,36,19.5,37,19.5,37,37,37,37,37,37,37,37,37,36,37,37,37,37,37,37,29.7,37,37,null,null],[14.8,17.7,15.3,35.8,50.6,45,16.8,23.6,42.8,17.5,25.5,47.1,16.3,16.8,91.8,43.9,22.5,35.3,2,33.6,39.6,21.2,45.5,8.4,120,18.2,50.5,26.4,38.7,37.8,45.5,65.6,40.2,37,44.6,32.4,38.3,22.2,50.8,22.2,32.8,48.3,48.1,51.4,33.6,32.3,37,34.3,34.3,34.3,34.3,34.3,null,36.5,3.5,42.7,56.4,36,37,49.9,22.2,5.5,63.5,76,15.6,null,13.3,39.7,15,99,109,81.1,75.4,75.3,57,82.7,57.5,37.7,9,9,9,9,75.8,29.8,10.1,93,30.7,10,39.4,71.8,80.6,null,5.5,21.3,59.9,55.5,10,12,71.8,73.6,6.5,34.8,19,19,80.6,63.2,74,19,20,53.5,49.9,52.7,59.7,95.4,72.2,74.8,75.5,58.2,70.6,49.8,80.6,35.1,90,90,90,76.6,117.8,88,83.5,59.8,109,90,106.8,97.8,90,57.4,93.3,89.5,80.9,92.8,51.2,84.7,99.6,92,144,33.4,40,86,24.2,91.5,109,91.5,109,91.5,25,29.3,91.5,109,109,38,null,26,36,19.5,37,19.5,37,37,37,37,37,37,37,37,37,36,37,37,37,37,37,37,29.7,37,37,null,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Degree\u003c/th>\n      <th>Out Degree\u003c/th>\n      <th>Total Degree\u003c/th>\n      <th>In Degree Normalized\u003c/th>\n      <th>Out Degree Normalized\u003c/th>\n      <th>Total Degree Normalized\u003c/th>\n      <th>Strength\u003c/th>\n      <th>Centralization Degree\u003c/th>\n      <th>Average Neighbor Degree\u003c/th>\n      <th>Average Weighted Neighbor Degree\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var2_with_CAPS_df_degree, by=list(var2_with_CAPS_df_degree$Type), FUN=mean, na.rm=TRUE)

#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
names(aggdata_mean) <- c("Group", "In Degree(M)", "Out Degree(M)", "Total Degree(M)","In Degree Normalized(M)", "Out Degree Normalized(M)", "Total Degree Normalized(M)", "Strength(M)","Centralization Degree(M)","Average Neighbor Degree(M)","Average Weighted Neighbor Degree(M)")
```

##Aggregating data from previous table - median

```r
aggdata_median <-aggregate(. ~ Type, var2_with_CAPS_df_degree, function(x) c(median=median(x)))

#Removing Type variable
#aggdata_median<-aggdata_median[,-c(2)]
names(aggdata_median) <- c("Group", "In Degree(median)", "Out Degree(median)", "Total Degree(median)","In Degree Normalized(median)", "Out Degree Normalized(median)", "Total Degree Normalized(median)", "Strength(median)","Centralization Degree(median)","Average Neighbor Degree(median)","Average Weighted Neighbor Degree(median)")
```

##Merging mean and median

```r
total_table <- merge(aggdata_mean,aggdata_median,by="Group")

#Rounding
Group<-total_table[,c(1)] #Keeping group
total_table<-total_table[,-c(1)] %>% round(2) #Rouding
total_table<-cbind(Group,total_table) #Binding toghter

#Organizing Variabels
total_table<-total_table[c("Group","In Degree(M)","In Degree(median)", "Out Degree(M)", "Out Degree(median)","Total Degree(M)", "Total Degree(median)", "In Degree Normalized(M)", "In Degree Normalized(median)", "Out Degree Normalized(M)", "Out Degree Normalized(median)", "Total Degree Normalized(M)", "Total Degree Normalized(median)", "Strength(M)","Strength(median)", "Centralization Degree(M)","Centralization Degree(median)","Average Neighbor Degree(M)","Average Neighbor Degree(median)","Average Weighted Neighbor Degree(M)", "Average Weighted Neighbor Degree(median)")]
```

##Plotting final table with round

```r
datatable(total_table, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-b8ec9d292eba964fc556" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-b8ec9d292eba964fc556">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.5\" data-max=\"6.81\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2\" data-max=\"5\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.32\" data-max=\"6.22\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1\" data-max=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.82\" data-max=\"13.04\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3\" data-max=\"7\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.01\" data-max=\"0.04\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.01\" data-max=\"0.03\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.02\" data-max=\"0.03\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"0.01\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.03\" data-max=\"0.08\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.02\" data-max=\"0.04\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.82\" data-max=\"13.04\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3\" data-max=\"7\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.82\" data-max=\"13.04\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3\" data-max=\"7\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"30.83\" data-max=\"62.78\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"34.2\" data-max=\"59.8\" data-scale=\"1\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"30.83\" data-max=\"62.78\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"34.2\" data-max=\"59.8\" data-scale=\"1\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[6.81,2.51],[5,2],[6.22,3.32],[2,1],[13.04,5.82],[7,3],[0.04,0.01],[0.03,0.01],[0.03,0.02],[0.01,0],[0.07,0.03],[0.04,0.02],[13.04,5.82],[7,3],[13.04,5.82],[7,3],[62.78,30.83],[59.8,34.2],[62.78,30.83],[59.8,34.2]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Degree(M)\u003c/th>\n      <th>In Degree(median)\u003c/th>\n      <th>Out Degree(M)\u003c/th>\n      <th>Out Degree(median)\u003c/th>\n      <th>Total Degree(M)\u003c/th>\n      <th>Total Degree(median)\u003c/th>\n      <th>In Degree Normalized(M)\u003c/th>\n      <th>In Degree Normalized(median)\u003c/th>\n      <th>Out Degree Normalized(M)\u003c/th>\n      <th>Out Degree Normalized(median)\u003c/th>\n      <th>Total Degree Normalized(M)\u003c/th>\n      <th>Total Degree Normalized(median)\u003c/th>\n      <th>Strength(M)\u003c/th>\n      <th>Strength(median)\u003c/th>\n      <th>Centralization Degree(M)\u003c/th>\n      <th>Centralization Degree(median)\u003c/th>\n      <th>Average Neighbor Degree(M)\u003c/th>\n      <th>Average Neighbor Degree(median)\u003c/th>\n      <th>Average Weighted Neighbor Degree(M)\u003c/th>\n      <th>Average Weighted Neighbor Degree(median)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a dataframe of measures (Setores)

```r
var2_with_CAPS_df_degree <- data.frame(var2_with_CAPS_indegree,
var2_with_CAPS_outdegree, 
var2_with_CAPS_totaldegree,
var2_with_CAPS_indegree_n, 
var2_with_CAPS_outdegree_n,
var2_with_CAPS_totaldegree_n,
var2_strength,
var2_centr_degree,
var2_with_CAPS_a.nn.deg,
var2_with_CAPS_a.nn.deg_w) %>% round(3)

#Adding type
var2_with_CAPS_df_degree <-cbind(var2_with_CAPS_df_degree, V(var2_with_CAPS)$TIPO2)

#Adding names
names(var2_with_CAPS_df_degree) <- c("In Degree", "Out Degree", "Total Degree","In Degree Normalized", "Out Degree Normalized", "Total Degree Normalized", "Strength","Centralization Degree","Average Neighbor Degree","Average Weighted Neighbor Degree","Type")

#Ordering Variables
var2_with_CAPS_df_degree<-var2_with_CAPS_df_degree[c("Type","In Degree", "Out Degree", "Total Degree","In Degree Normalized", "Out Degree Normalized", "Total Degree Normalized", "Strength","Centralization Degree","Average Neighbor Degree","Average Weighted Neighbor Degree")]
```

##General tabel - DT

```r
datatable(var2_with_CAPS_df_degree, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-db946aee761977fbec6f" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-db946aee761977fbec6f">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"52\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"92\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"144\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"0.28\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"0.495\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"0.774\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"144\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"144\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2\" data-max=\"144\" data-scale=\"1\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2\" data-max=\"144\" data-scale=\"1\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[31,18,52,10,15,10,6,5,1,2,14,14,1,3,4,2,12,13,1,4,12,3,11,1,0,1,2,16,10,10,14,5,16,2,10,2,2,2,1,14,14,9,7,4,1,20,2,1,1,1,1,1,0,6,1,10,8,1,2,16,4,2,6,5,2,0,1,5,2,3,3,4,5,5,6,4,6,22,4,4,4,4,5,3,7,4,2,1,5,3,3,0,2,4,4,4,2,3,4,7,1,5,1,1,5,5,5,1,2,8,8,6,6,4,4,5,6,6,5,10,5,6,4,4,4,4,3,4,5,3,3,4,3,4,4,5,4,5,6,4,8,5,3,5,0,21,12,4,4,2,3,2,3,2,1,2,2,3,3,1,0,3,1,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,3,2,2,0,0],[89,45,92,27,3,0,27,18,5,2,10,0,17,9,0,8,58,0,0,5,9,5,9,37,1,3,2,25,25,18,5,4,2,0,2,3,5,3,4,31,27,4,4,1,6,0,0,8,8,8,8,8,0,9,35,6,0,0,0,2,0,2,0,3,3,0,2,4,2,0,0,3,0,1,5,3,2,10,4,4,4,4,0,1,5,2,1,1,14,2,5,0,2,3,6,2,0,0,1,0,1,18,0,0,0,7,1,0,0,0,1,3,1,1,1,1,2,5,5,2,0,12,0,0,0,1,1,2,1,1,0,0,2,1,0,4,2,1,1,0,1,1,2,1,1,1,2,0,2,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[120,63,144,37,18,10,33,23,6,4,24,14,18,12,4,10,70,13,1,9,21,8,20,38,1,4,4,41,35,28,19,9,18,2,12,5,7,5,5,45,41,13,11,5,7,20,2,9,9,9,9,9,0,15,36,16,8,1,2,18,4,4,6,8,5,0,3,9,4,3,3,7,5,6,11,7,8,32,8,8,8,8,5,4,12,6,3,2,19,5,8,0,4,7,10,6,2,3,5,7,2,23,1,1,5,12,6,1,2,8,9,9,7,5,5,6,8,11,10,12,5,18,4,4,4,5,4,6,6,4,3,4,5,5,4,9,6,6,7,4,9,6,5,6,1,22,14,4,6,2,3,2,3,2,2,3,2,3,3,1,0,3,1,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,3,2,2,0,0],[0.167,0.097,0.28,0.054,0.081,0.054,0.032,0.027,0.005,0.011,0.075,0.075,0.005,0.016,0.022,0.011,0.065,0.07,0.005,0.022,0.065,0.016,0.059,0.005,0,0.005,0.011,0.086,0.054,0.054,0.075,0.027,0.086,0.011,0.054,0.011,0.011,0.011,0.005,0.075,0.075,0.048,0.038,0.022,0.005,0.108,0.011,0.005,0.005,0.005,0.005,0.005,0,0.032,0.005,0.054,0.043,0.005,0.011,0.086,0.022,0.011,0.032,0.027,0.011,0,0.005,0.027,0.011,0.016,0.016,0.022,0.027,0.027,0.032,0.022,0.032,0.118,0.022,0.022,0.022,0.022,0.027,0.016,0.038,0.022,0.011,0.005,0.027,0.016,0.016,0,0.011,0.022,0.022,0.022,0.011,0.016,0.022,0.038,0.005,0.027,0.005,0.005,0.027,0.027,0.027,0.005,0.011,0.043,0.043,0.032,0.032,0.022,0.022,0.027,0.032,0.032,0.027,0.054,0.027,0.032,0.022,0.022,0.022,0.022,0.016,0.022,0.027,0.016,0.016,0.022,0.016,0.022,0.022,0.027,0.022,0.027,0.032,0.022,0.043,0.027,0.016,0.027,0,0.113,0.065,0.022,0.022,0.011,0.016,0.011,0.016,0.011,0.005,0.011,0.011,0.016,0.016,0.005,0,0.016,0.005,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.005,0.011,0.011,0.011,0.011,0.011,0.011,0.016,0.011,0.011,0,0],[0.478,0.242,0.495,0.145,0.016,0,0.145,0.097,0.027,0.011,0.054,0,0.091,0.048,0,0.043,0.312,0,0,0.027,0.048,0.027,0.048,0.199,0.005,0.016,0.011,0.134,0.134,0.097,0.027,0.022,0.011,0,0.011,0.016,0.027,0.016,0.022,0.167,0.145,0.022,0.022,0.005,0.032,0,0,0.043,0.043,0.043,0.043,0.043,0,0.048,0.188,0.032,0,0,0,0.011,0,0.011,0,0.016,0.016,0,0.011,0.022,0.011,0,0,0.016,0,0.005,0.027,0.016,0.011,0.054,0.022,0.022,0.022,0.022,0,0.005,0.027,0.011,0.005,0.005,0.075,0.011,0.027,0,0.011,0.016,0.032,0.011,0,0,0.005,0,0.005,0.097,0,0,0,0.038,0.005,0,0,0,0.005,0.016,0.005,0.005,0.005,0.005,0.011,0.027,0.027,0.011,0,0.065,0,0,0,0.005,0.005,0.011,0.005,0.005,0,0,0.011,0.005,0,0.022,0.011,0.005,0.005,0,0.005,0.005,0.011,0.005,0.005,0.005,0.011,0,0.011,0,0,0,0,0,0.005,0.005,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0.645,0.339,0.774,0.199,0.097,0.054,0.177,0.124,0.032,0.022,0.129,0.075,0.097,0.065,0.022,0.054,0.376,0.07,0.005,0.048,0.113,0.043,0.108,0.204,0.005,0.022,0.022,0.22,0.188,0.151,0.102,0.048,0.097,0.011,0.065,0.027,0.038,0.027,0.027,0.242,0.22,0.07,0.059,0.027,0.038,0.108,0.011,0.048,0.048,0.048,0.048,0.048,0,0.081,0.194,0.086,0.043,0.005,0.011,0.097,0.022,0.022,0.032,0.043,0.027,0,0.016,0.048,0.022,0.016,0.016,0.038,0.027,0.032,0.059,0.038,0.043,0.172,0.043,0.043,0.043,0.043,0.027,0.022,0.065,0.032,0.016,0.011,0.102,0.027,0.043,0,0.022,0.038,0.054,0.032,0.011,0.016,0.027,0.038,0.011,0.124,0.005,0.005,0.027,0.065,0.032,0.005,0.011,0.043,0.048,0.048,0.038,0.027,0.027,0.032,0.043,0.059,0.054,0.065,0.027,0.097,0.022,0.022,0.022,0.027,0.022,0.032,0.032,0.022,0.016,0.022,0.027,0.027,0.022,0.048,0.032,0.032,0.038,0.022,0.048,0.032,0.027,0.032,0.005,0.118,0.075,0.022,0.032,0.011,0.016,0.011,0.016,0.011,0.011,0.016,0.011,0.016,0.016,0.005,0,0.016,0.005,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.011,0.005,0.011,0.011,0.011,0.011,0.011,0.011,0.016,0.011,0.011,0,0],[120,63,144,37,18,10,33,23,6,4,24,14,18,12,4,10,70,13,1,9,21,8,20,38,1,4,4,41,35,28,19,9,18,2,12,5,7,5,5,45,41,13,11,5,7,20,2,9,9,9,9,9,0,15,36,16,8,1,2,18,4,4,6,8,5,0,3,9,4,3,3,7,5,6,11,7,8,32,8,8,8,8,5,4,12,6,3,2,19,5,8,0,4,7,10,6,2,3,5,7,2,23,1,1,5,12,6,1,2,8,9,9,7,5,5,6,8,11,10,12,5,18,4,4,4,5,4,6,6,4,3,4,5,5,4,9,6,6,7,4,9,6,5,6,1,22,14,4,6,2,3,2,3,2,2,3,2,3,3,1,0,3,1,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,3,2,2,0,0],[120,63,144,37,18,10,33,23,6,4,24,14,18,12,4,10,70,13,1,9,21,8,20,38,1,4,4,41,35,28,19,9,18,2,12,5,7,5,5,45,41,13,11,5,7,20,2,9,9,9,9,9,0,15,36,16,8,1,2,18,4,4,6,8,5,0,3,9,4,3,3,7,5,6,11,7,8,32,8,8,8,8,5,4,12,6,3,2,19,5,8,0,4,7,10,6,2,3,5,7,2,23,1,1,5,12,6,1,2,8,9,9,7,5,5,6,8,11,10,12,5,18,4,4,4,5,4,6,6,4,3,4,5,5,4,9,6,6,7,4,9,6,5,6,1,22,14,4,6,2,3,2,3,2,2,3,2,3,3,1,0,3,1,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,3,2,2,0,0],[14.8,17.7,15.3,35.8,50.6,45,16.8,23.6,42.8,17.5,25.5,47.1,16.3,16.8,91.8,43.9,22.5,35.3,2,33.6,39.6,21.2,45.5,8.4,120,18.2,50.5,26.4,38.7,37.8,45.5,65.6,40.2,37,44.6,32.4,38.3,22.2,50.8,22.2,32.8,48.3,48.1,51.4,33.6,32.3,37,34.3,34.3,34.3,34.3,34.3,null,36.5,3.5,42.7,56.4,36,37,49.9,22.2,5.5,63.5,76,15.6,null,13.3,39.7,15,99,109,81.1,75.4,75.3,57,82.7,57.5,37.7,9,9,9,9,75.8,29.8,10.1,93,30.7,10,39.4,71.8,80.6,null,5.5,21.3,59.9,55.5,10,12,71.8,73.6,6.5,34.8,19,19,80.6,63.2,74,19,20,53.5,49.9,52.7,59.7,95.4,72.2,74.8,75.5,58.2,70.6,49.8,80.6,35.1,90,90,90,76.6,117.8,88,83.5,59.8,109,90,106.8,97.8,90,57.4,93.3,89.5,80.9,92.8,51.2,84.7,99.6,92,144,33.4,40,86,24.2,91.5,109,91.5,109,91.5,25,29.3,91.5,109,109,38,null,26,36,19.5,37,19.5,37,37,37,37,37,37,37,37,37,36,37,37,37,37,37,37,29.7,37,37,null,null],[14.8,17.7,15.3,35.8,50.6,45,16.8,23.6,42.8,17.5,25.5,47.1,16.3,16.8,91.8,43.9,22.5,35.3,2,33.6,39.6,21.2,45.5,8.4,120,18.2,50.5,26.4,38.7,37.8,45.5,65.6,40.2,37,44.6,32.4,38.3,22.2,50.8,22.2,32.8,48.3,48.1,51.4,33.6,32.3,37,34.3,34.3,34.3,34.3,34.3,null,36.5,3.5,42.7,56.4,36,37,49.9,22.2,5.5,63.5,76,15.6,null,13.3,39.7,15,99,109,81.1,75.4,75.3,57,82.7,57.5,37.7,9,9,9,9,75.8,29.8,10.1,93,30.7,10,39.4,71.8,80.6,null,5.5,21.3,59.9,55.5,10,12,71.8,73.6,6.5,34.8,19,19,80.6,63.2,74,19,20,53.5,49.9,52.7,59.7,95.4,72.2,74.8,75.5,58.2,70.6,49.8,80.6,35.1,90,90,90,76.6,117.8,88,83.5,59.8,109,90,106.8,97.8,90,57.4,93.3,89.5,80.9,92.8,51.2,84.7,99.6,92,144,33.4,40,86,24.2,91.5,109,91.5,109,91.5,25,29.3,91.5,109,109,38,null,26,36,19.5,37,19.5,37,37,37,37,37,37,37,37,37,36,37,37,37,37,37,37,29.7,37,37,null,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Degree\u003c/th>\n      <th>Out Degree\u003c/th>\n      <th>Total Degree\u003c/th>\n      <th>In Degree Normalized\u003c/th>\n      <th>Out Degree Normalized\u003c/th>\n      <th>Total Degree Normalized\u003c/th>\n      <th>Strength\u003c/th>\n      <th>Centralization Degree\u003c/th>\n      <th>Average Neighbor Degree\u003c/th>\n      <th>Average Weighted Neighbor Degree\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var2_with_CAPS_df_degree, by=list(var2_with_CAPS_df_degree$Type), FUN=mean, na.rm=TRUE)

#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
names(aggdata_mean) <- c("Group", "In Degree(M)", "Out Degree(M)", "Total Degree(M)","In Degree Normalized(M)", "Out Degree Normalized(M)", "Total Degree Normalized(M)", "Strength(M)","Centralization Degree(M)","Average Neighbor Degree(M)","Average Weighted Neighbor Degree(M)")
```

##Aggregating data from previous table - median

```r
aggdata_median <-aggregate(. ~ Type, var2_with_CAPS_df_degree, function(x) c(median=median(x)))

#Removing Type variable
#aggdata_median<-aggdata_median[,-c(2)]
names(aggdata_median) <- c("Group", "In Degree(median)", "Out Degree(median)", "Total Degree(median)","In Degree Normalized(median)", "Out Degree Normalized(median)", "Total Degree Normalized(median)", "Strength(median)","Centralization Degree(median)","Average Neighbor Degree(median)","Average Weighted Neighbor Degree(median)")
```

##Merging mean and median

```r
total_table <- merge(aggdata_mean,aggdata_median,by="Group")

#Rounding
Group<-total_table[,c(1)] #Keeping group
total_table<-total_table[,-c(1)] %>% round(2) #Rouding
total_table<-cbind(Group,total_table) #Binding toghter

#Organizing Variabels
total_table<-total_table[c("Group","In Degree(M)","In Degree(median)", "Out Degree(M)", "Out Degree(median)","Total Degree(M)", "Total Degree(median)", "In Degree Normalized(M)", "In Degree Normalized(median)", "Out Degree Normalized(M)", "Out Degree Normalized(median)", "Total Degree Normalized(M)", "Total Degree Normalized(median)", "Strength(M)","Strength(median)", "Centralization Degree(M)","Centralization Degree(median)","Average Neighbor Degree(M)","Average Neighbor Degree(median)","Average Weighted Neighbor Degree(M)", "Average Weighted Neighbor Degree(median)")]
```

##Plotting final table with round

```r
datatable(total_table, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-de4ccccc6361a5dca884" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-de4ccccc6361a5dca884">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.38\" data-max=\"13.4\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2\" data-max=\"14\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.38\" data-max=\"12.87\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1\" data-max=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.77\" data-max=\"26.27\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3\" data-max=\"20\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.01\" data-max=\"0.08\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.01\" data-max=\"0.08\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.02\" data-max=\"0.08\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"0.03\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.03\" data-max=\"0.15\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.02\" data-max=\"0.11\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.77\" data-max=\"26.27\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3\" data-max=\"20\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5.77\" data-max=\"26.27\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3\" data-max=\"20\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"30.35\" data-max=\"66.71\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"33.6\" data-max=\"72.2\" data-scale=\"1\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"30.35\" data-max=\"66.71\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"33.6\" data-max=\"72.2\" data-scale=\"1\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[13.4,5.82,2.38],[14,4,2],[12.87,5.07,3.38],[6,1,1],[26.27,10.89,5.77],[20,6,3],[0.07,0.03,0.01],[0.08,0.02,0.01],[0.07,0.03,0.02],[0.03,0,0],[0.14,0.06,0.03],[0.11,0.03,0.02],[26.27,10.89,5.77],[20,6,3],[26.27,10.89,5.77],[20,6,3],[38.59,66.71,30.35],[38.7,72.2,33.6],[38.59,66.71,30.35],[38.7,72.2,33.6]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Degree(M)\u003c/th>\n      <th>In Degree(median)\u003c/th>\n      <th>Out Degree(M)\u003c/th>\n      <th>Out Degree(median)\u003c/th>\n      <th>Total Degree(M)\u003c/th>\n      <th>Total Degree(median)\u003c/th>\n      <th>In Degree Normalized(M)\u003c/th>\n      <th>In Degree Normalized(median)\u003c/th>\n      <th>Out Degree Normalized(M)\u003c/th>\n      <th>Out Degree Normalized(median)\u003c/th>\n      <th>Total Degree Normalized(M)\u003c/th>\n      <th>Total Degree Normalized(median)\u003c/th>\n      <th>Strength(M)\u003c/th>\n      <th>Strength(median)\u003c/th>\n      <th>Centralization Degree(M)\u003c/th>\n      <th>Centralization Degree(median)\u003c/th>\n      <th>Average Neighbor Degree(M)\u003c/th>\n      <th>Average Neighbor Degree(median)\u003c/th>\n      <th>Average Weighted Neighbor Degree(M)\u003c/th>\n      <th>Average Weighted Neighbor Degree(median)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#Network plotting based only on degree measures 

```r
#Set Seed
set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var2_with_CAPS, es=E(var2_with_CAPS), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2_with_CAPS))
maxC <- rep(Inf, vcount(var2_with_CAPS))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2_with_CAPS, niter=10000, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2_with_CAPS)$weight)

#PLotting
plot(var2_with_CAPS, 
     layout=co,
     edge.color=V(var2_with_CAPS)$color[edge.start],
     edge.arrow.size=(degree(var2_with_CAPS)+1)/(100*mean(degree(var2_with_CAPS))),
     edge.width=E(var2_with_CAPS)$weight/(10*mean(E(var2_with_CAPS)$weight)),
     edge.curved = TRUE,
     vertex.size=log((degree(var2_with_CAPS)+2))*(0.5*mean(degree(var2_with_CAPS))),
     vertex.frame.color="#ffffff",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2_with_CAPS,"LABEL_COR"),
     vertex.label.cex=log(degree(var2_with_CAPS)+2)/mean(degree(var2_with_CAPS)),
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2]))
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var2_with_CAPS)$LABEL_COR
b<-V(var2_with_CAPS)$color
c<-table(a,b)
d<-as.data.frame(c)
e<-subset(d, d$Freq>0)
f<-t(e$a)
g<-t(e$b)

#Adding Legend
legend(x=range(co[,1])[2], y=range(co[,2])[2],
       legend=as.character(f),
       pch=21,
       col = "#777777", 
       pt.bg=as.character(g),
       pt.cex=2,
       bty="n", 
       ncol=1,
       lty=1,
       cex = .5)

#Adding Title
  title("Network Vertex Degree Sized - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ", cex = .5)
  text(x=range(co[,1])[1], y=range(co[,2])[1], labels = 
   sprintf("Median In Degree: %.2f\n Median Out Degree: %.2f",
     median(degree(var2_with_CAPS, mode="in")), 
     median(degree(var2_with_CAPS, mode="out"))
   ))
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-73-1.png)<!-- -->

#Network plotting based only on degree measures 

```r
#Set Seed
set.seed(123)

#Get Variable
V(var2_with_CAPS)$var2_color_degree<-V(var2_with_CAPS)$var2_totaldegree %>% round(1)

#Creating brewer pallette
var2_vertex_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var2_with_CAPS)$var2_color_degree)), "RdBu"))(
            length(unique(V(var2_with_CAPS)$var2_color_degree)))

#Saving as Vertex properties 
V(var2_with_CAPS)$var2_vertex_color_degree<- var2_vertex_color_degree[as.numeric(cut(degree(var2_with_CAPS),breaks =length(unique(V(var2_with_CAPS)$var2_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var2_with_CAPS, es=E(var2_with_CAPS), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2_with_CAPS))
maxC <- rep(Inf, vcount(var2_with_CAPS))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2_with_CAPS, niter=10000, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2_with_CAPS)$weight)

#PLotting
plot(var2_with_CAPS, 
     layout=co,
     #edge.color=V(var2_with_CAPS)$color[edge.start],
     edge.arrow.size=(degree(var2_with_CAPS)+1)/3000,
     edge.width=E(var2_with_CAPS)$weight/10,
     edge.curved = TRUE,
     vertex.color=V(var2_with_CAPS)$var2_vertex_color_degree,
     vertex.size=log((degree(var2_with_CAPS)+2))*10,
     vertex.size=20,
     vertex.frame.color="#ffffff",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2_with_CAPS,"LABEL_COR"),
     vertex.label.cex=log((degree(var2_with_CAPS)+2))/10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var2_with_CAPS)$var2_color_degree
b<-V(var2_with_CAPS)$var2_vertex_color_degree
c<-table(a,b)
d<-as.data.frame(c)
e<-subset(d, d$Freq>0)
e<-e[order(e$a,decreasing=T),] 
f<-t(e$a)
g<-t(e$b)

#Adding Legend
legend(x=range(co[,1])[2], y=range(co[,2])[2],
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
  title("Network Vertex Degree Sized and Red to Blue - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text(x=range(co[,1])[1], y=range(co[,2])[1], labels = 
   sprintf("Median In Degree: %.2f\nMedian Out Degree: %.2f",
     median(degree(var2_with_CAPS, mode="in")), 
     median(degree(var2_with_CAPS, mode="out"))
   ))
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-74-1.png)<!-- -->

#Network Plotting Centralization - Degree Measures - Using Spectral Color as Distance Measure Representation

```r
#Set Seed
set.seed(123)

#Get Variable
V(var2_with_CAPS)$var2_color_degree<-V(var2_with_CAPS)$var2_centr_degree

#Creating brewer pallette
var2_vertex_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var2_with_CAPS)$var2_color_degree)), "Spectral"))(
            length(unique(V(var2_with_CAPS)$var2_color_degree)))

#Saving as Vertex properties 
V(var2_with_CAPS)$var2_vertex_color_degree<- var2_vertex_color_degree[as.numeric(cut(V(var2_with_CAPS)$var2_color_degree,breaks =length(unique(V(var2_with_CAPS)$var2_color_degree))))]

#Plotting based only on degree measures 
edge.start <- ends(var2_with_CAPS, es=E(var2_with_CAPS), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2_with_CAPS))
maxC <- rep(Inf, vcount(var2_with_CAPS))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2_with_CAPS, niter=10000, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2_with_CAPS)$weight)

#PLotting
plot(var2_with_CAPS, 
     layout=co,
     edge.color=V(var2_with_CAPS)$var2_vertex_color_degree[edge.start],
     edge.arrow.size=(degree(var2_with_CAPS)+1)/10000,
     edge.width=E(var2_with_CAPS)$weight/10,
     edge.curved = TRUE,
     vertex.color=V(var2_with_CAPS)$var2_vertex_color_degree,
     vertex.size=log((V(var2_with_CAPS)$var2_centr_degree+2))*10,
     vertex.frame.color="#ffffff",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2_with_CAPS,"LABEL_COR"),
     vertex.label.cex=log((degree(var2_with_CAPS)+2))/10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2]))
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var2_with_CAPS)$var2_color_degree
b<-V(var2_with_CAPS)$var2_vertex_color_degree
c<-table(a,b)
d<-as.data.frame(c)
e<-subset(d, d$Freq>0)
e<-e[order(e$a,decreasing=T),] 
f<-t(e$a)
g<-t(e$b)

#Adding Legend
legend(x=range(co[,1])[2], y=range(co[,2])[2],
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
  title("Network Vertex Centralization Degree Sized Spectral Colored - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text(x=range(co[,1])[1], y=range(co[,2])[1], labels = 
   sprintf("Median In Degree: %.2f\nMedian Out Degree: %.2f",
     median(degree(var2_with_CAPS, mode="in")), 
     median(degree(var2_with_CAPS, mode="out"))
   ))
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-75-1.png)<!-- -->

#Alternative vizualization using degree in order to genarete sub-graphs - Higher than median degree network

```r
#Set Seed
set.seed(123)

# Network elements with lower than meadian degree
higherthanmedian.network_var2_with_CAPS<-V(var2_with_CAPS)[degree(var2_with_CAPS)<median(degree(var2_with_CAPS))] 

#Deleting vertices based in intersection betewenn var2_with_CAPS 
high_var2_with_CAPS<-delete.vertices(var2_with_CAPS, higherthanmedian.network_var2_with_CAPS)

#Plotting based only on degree measures 
edge.start <- ends(high_var2_with_CAPS, es=E(high_var2_with_CAPS), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(high_var2_with_CAPS))
maxC <- rep(Inf, vcount(high_var2_with_CAPS))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(high_var2_with_CAPS, niter=10000, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(high_var2_with_CAPS)$weight)

#PLotting
plot(high_var2_with_CAPS, 
     layout=co,
     edge.color=V(high_var2_with_CAPS)$color[edge.start],
     edge.arrow.size=(degree(high_var2_with_CAPS)+1)/1000,
     edge.width=E(high_var2_with_CAPS)$weight/10,
     edge.curved = TRUE,
     vertex.size=log((V(high_var2_with_CAPS)$var2_centr_degree+2))*10,
     vertex.frame.color="#ffffff",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(high_var2_with_CAPS,"LABEL_COR"),
     vertex.label.cex=log((degree(high_var2_with_CAPS)+2))/10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2]))
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(high_var2_with_CAPS)$LABEL_COR
b<-V(high_var2_with_CAPS)$color
c<-table(a,b)
d<-as.data.frame(c)
e<-subset(d, d$Freq>0)
f<-t(e$a)
g<-t(e$b)

#Adding Legend
legend(x=range(co[,1])[2], y=range(co[,2])[2],
       legend=as.character(f),
       pch=21,
       col = "#777777", 
       pt.bg=as.character(g),
       pt.cex=3,
       bty="n", 
       ncol=1,
       lty=1,
       cex = .5)

#Adding Title
  title("Network Higher Than Median Degree - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text(x=range(co[,1])[1], y=range(co[,2])[1], labels = 
   sprintf("Mean In Degree: %.2f\n Mean Out Degree: %.2f",
     mean(degree(high_var2_with_CAPS, mode="in")), 
     mean(degree(high_var2_with_CAPS, mode="out"))
   )
  )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-76-1.png)<!-- -->

#Alternative vizualization using degree in order to genarete sub-graphs - Lower than median degree network 

```r
#Set Seed
set.seed(123)

# Network elements with lower than meadian degree
lowerthanmedian.network_var2_with_CAPS<-V(var2_with_CAPS)[degree(var2_with_CAPS)>median(degree(var2_with_CAPS))] 

#Deleting vertices based in intersection betewenn var2_with_CAPS 
small_var2_with_CAPS<-delete.vertices(var2_with_CAPS, lowerthanmedian.network_var2_with_CAPS)

#Plotting based only on degree measures 
edge.start <- ends(small_var2_with_CAPS, es=E(small_var2_with_CAPS), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(small_var2_with_CAPS))
maxC <- rep(Inf, vcount(small_var2_with_CAPS))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(small_var2_with_CAPS, niter=10000, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(small_var2_with_CAPS)$weight)

#PLotting
plot(small_var2_with_CAPS, 
     layout=co,
     edge.color=V(small_var2_with_CAPS)$color[edge.start],
     edge.arrow.size=(degree(small_var2_with_CAPS)+1)/1000,
     edge.width=E(small_var2_with_CAPS)$weight/10,
     edge.curved = TRUE,
     vertex.size=log((V(small_var2_with_CAPS)$var2_centr_degree+2))*20,
     vertex.frame.color="#ffffff",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(small_var2_with_CAPS,"LABEL_COR"),
     vertex.label.cex=log((degree(small_var2_with_CAPS)+2))/3,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2]))
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(small_var2_with_CAPS)$LABEL_COR
b<-V(small_var2_with_CAPS)$color
c<-table(a,b)
d<-as.data.frame(c)
e<-subset(d, d$Freq>0)
f<-t(e$a)
g<-t(e$b)

#Adding Legend
legend(x=range(co[,1])[2], y=range(co[,2])[2],
       legend=as.character(f),
       pch=21,
       col = "#777777", 
       pt.bg=as.character(g),
       pt.cex=4,
       bty="n", 
       ncol=1,
       lty=1,
       cex = .5)

#Adding Title
  title("Network Smaller Than Median Degree - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text(x=range(co[,1])[1], y=range(co[,2])[1], labels = 
   sprintf("Mean In Degree: %.2f\nMean Out Degree: %.2f",
     mean(degree(small_var2_with_CAPS, mode="in")), 
     mean(degree(small_var2_with_CAPS, mode="out"))
   )
  )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-77-1.png)<!-- -->

#Plotting using Average Neighbor Degree

```r
#Set Seed
set.seed(123)

#Plotting based only on degree measures 
edge.start <- ends(var2_with_CAPS_simplified, es=E(var2_with_CAPS_simplified), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2_with_CAPS_simplified))
maxC <- rep(Inf, vcount(var2_with_CAPS_simplified))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2_with_CAPS_simplified, niter=10000, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2_with_CAPS_simplified)$weight)

#Plotting based only on degree measures  #var2_with_CAPS_simplified_a.nn.deg
V(var2_with_CAPS_simplified)$var2_with_CAPS_a.nn.deg<-as.numeric(graph.knn(var2_with_CAPS_simplified)$knn)
V(var2_with_CAPS_simplified)$var2_with_CAPS_a.nn.deg[V(var2_with_CAPS_simplified)$var2_with_CAPS_a.nn.deg=="NaN"]<-0

#PLotting
plot(var2_with_CAPS_simplified, 
     layout=co,
     edge.color=V(var2_with_CAPS_simplified)$color[edge.start],
     edge.arrow.size=sqrt((V(var2_with_CAPS_simplified)$var2_with_CAPS_a.nn.deg)^2+1)/1000,
     edge.width=E(var2_with_CAPS_simplified)$weight/80,
     edge.curved = TRUE,
     vertex.color=V(var2_with_CAPS_simplified)$color,
     vertex.size=(sqrt((V(var2_with_CAPS_simplified)$var2_with_CAPS_a.nn.deg)^2))/1,
     vertex.frame.color="#ffffff",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2_with_CAPS_simplified,"LABEL_COR"),
     vertex.label.cex=(sqrt((V(var2_with_CAPS_simplified)$var2_with_CAPS_a.nn.deg)^2)+1)/250,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2]))
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var2_with_CAPS_simplified)$LABEL_COR
b<-V(var2_with_CAPS_simplified)$color
c<-table(a,b)
d<-as.data.frame(c)
e<-subset(d, d$Freq>0)
f<-t(e$a)
g<-t(e$b)

#Adding Legend
legend(x=range(co[,1])[2], 
       y=range(co[,2])[2],
       legend=as.character(f),
       inset=2,
       pch=21,
       col = "#777777", 
       pt.bg=as.character(g),
       pt.cex=2,
       bty="n", 
       ncol=1,
       lty=3,
       lwd=2,
       x.intersp=2,
       y.intersp=4,
       cex = .3)

#Adding Title
  title("Network Average Neighbor Degree Sized - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text(x=range(co[,1])[1], y=range(co[,2])[1], labels = 
   sprintf("Median Average Neighbor Degree: %.2f",
     median((var2_with_CAPS_a.nn.deg+1))
   ))
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-78-1.png)<!-- -->

#Plotting using Average Neighbor Degree

```r
#Set Seed
set.seed(123)

#Plotting based only on degree measures 
edge.start <- ends(var2_with_CAPS_simplified, es=E(var2_with_CAPS_simplified), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2_with_CAPS_simplified))
maxC <- rep(Inf, vcount(var2_with_CAPS_simplified))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2_with_CAPS_simplified, niter=10000, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2_with_CAPS_simplified)$weight)


#Plotting based only on degree measures  #var2_with_CAPS_a.nn.deg
V(var2_with_CAPS_simplified)$var2_with_CAPS_a.nn.deg_w<-as.numeric(graph.knn(var2_with_CAPS_simplified, weights = E(var2_with_CAPS_simplified)$weight)$knn)
V(var2_with_CAPS_simplified)$var2_with_CAPS_a.nn.deg_w[V(var2_with_CAPS_simplified)$var2_with_CAPS_a.nn.deg_w=="NaN"]<-0

#PLotting
plot(var2_with_CAPS_simplified, 
     layout=co,
     edge.color=V(var2_with_CAPS_simplified)$color[edge.start],
     edge.arrow.size=sqrt((V(var2_with_CAPS_simplified)$var2_with_CAPS_a.nn.deg_w)^2+1)/1000,
     edge.width=E(var2_with_CAPS_simplified)$weight/100,
     edge.curved = TRUE,
     vertex.color=V(var2_with_CAPS_simplified)$color,
     vertex.size=(sqrt((V(var2_with_CAPS_simplified)$var2_with_CAPS_a.nn.deg_w)^2))/5,
     vertex.frame.color="#ffffff",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2_with_CAPS_simplified,"LABEL_COR"),
     vertex.label.cex=(sqrt((V(var2_with_CAPS_simplified)$var2_with_CAPS_a.nn.deg_w)^2)+1)/500,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2]))
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var2_with_CAPS_simplified)$LABEL_COR
b<-V(var2_with_CAPS_simplified)$color
c<-table(a,b)
d<-as.data.frame(c)
e<-subset(d, d$Freq>0)
f<-t(e$a)
g<-t(e$b)

#Adding Legend
legend(x=range(co[,1])[2], y=range(co[,2])[2],
       legend=as.character(f),
       pch=21,
       col = "#777777", 
       pt.bg=as.character(g),
       pt.cex=4,
       bty="n", 
       ncol=1,
       lty=1,
       cex = .5)

#Adding Title
  title("Network Average Weighted Neighbor Degree Sized - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text(x=range(co[,1])[1], y=range(co[,2])[1], labels = 
   sprintf("Median Average Weighted Neighbor Degree: %.2f",
     median((var2_with_CAPS_a.nn.deg_w+1))
   ))
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-79-1.png)<!-- -->

#Circle Degree ***Too intensive computation*** #code

```r
#Circle Degree ***Too intense computation***
#A_var2_with_CAPS <- get.adjacency(var2_with_CAPS, sparse=FALSE)
#detach("package:igraph", unload=TRUE)
#library(network)
#g <- network::as.network.matrix(A_var2_with_CAPS)
#library(sna)
#gplot.target(g, degree(g), main="Circle Degree")
#library(igraph)
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
V(var2_with_CAPS)$var2_incloseness <- closeness(var2_with_CAPS, mode = "in", weights = E(var2_with_CAPS)$var2) %>% round(6)
V(var2_with_CAPS)$var2_outcloseness <- closeness(var2_with_CAPS, mode = "out", weights = E(var2_with_CAPS)$var2) %>% round(6)
V(var2_with_CAPS)$var2_totalcloseness <- closeness(var2_with_CAPS, mode = "total", weights = E(var2_with_CAPS)$var2) %>% round(4)
```

###Saving to Environment

```r
var2_with_CAPS_incloseness<- closeness(var2_with_CAPS, mode = "in", weights = E(var2_with_CAPS)$var2) %>% round(6)
var2_with_CAPS_outcloseness<- closeness(var2_with_CAPS, mode = "out", weights = E(var2_with_CAPS)$var2) %>% round(6)
var2_with_CAPS_totalcloseness<- closeness(var2_with_CAPS, mode = "total", weights = E(var2_with_CAPS)$var2) %>% round(6)
```

##Closeness Non-normalized - IN

```r
summary(var2_with_CAPS_incloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 2.900e-05 6.300e-05 6.300e-05 6.195e-05 6.400e-05 6.800e-05
```

```r
sd(var2_with_CAPS_incloseness)
```

```
## [1] 7.035801e-06
```

###Network Plotting Based On Non-normalized Closeness - IN

```r
V(var2_with_CAPS)$var2_incloseness<-closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="in")

#Get Variable
V(var2_with_CAPS)$var2_color_degree<-round(V(var2_with_CAPS)$var2_incloseness,6)

#Creating brewer pallette
var2_vertex_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var2_with_CAPS)$var2_color_degree)), "RdBu"))(
            length(unique(V(var2_with_CAPS)$var2_color_degree)))

#Saving as Vertex properties 
V(var2_with_CAPS)$var2_vertex_color_degree<-
  var2_vertex_color_degree[as.numeric(
  cut(V(var2_with_CAPS)$var2_color_degree,
      breaks=length(unique(V(var2_with_CAPS)$var2_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var2_with_CAPS, es=E(var2_with_CAPS), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2_with_CAPS))
maxC <- rep(Inf, vcount(var2_with_CAPS))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2_with_CAPS, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2_with_CAPS)$weight)


#PLotting
plot(var2_with_CAPS, 
     layout=co,
     edge.color=V(var2_with_CAPS)$var2_vertex_color_degree[edge.start],
     edge.arrow.size=closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="in"),
     edge.width=E(var2_with_CAPS)$weight/mean(E(var2_with_CAPS)$weight),
     edge.curved = TRUE,
     vertex.color=V(var2_with_CAPS)$var2_vertex_color_degree,
     vertex.size=closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="in")*10^5,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2_with_CAPS,"LABEL_COR"),
     vertex.label.cex=(closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="in")+10^-5)*2000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var2_with_CAPS)$var2_color_degree
b<-V(var2_with_CAPS)$var2_vertex_color_degree
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
  title("Network Closeness Degree Sized and Colored In - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var2_with_CAPS, mode="in", weights = E(var2_with_CAPS)$var2)), 
             sd(closeness(var2_with_CAPS, mode="in", weights = E(var2_with_CAPS)$var2))
             )
       )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-84-1.png)<!-- -->

##Closeness Non-normalized - OUT

```r
summary(var2_with_CAPS_outcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0000290 0.0004480 0.0002792 0.0004825 0.0005380
```

```r
sd(var2_with_CAPS_outcloseness)
```

```
## [1] 0.0002246621
```


###Network Plotting Based On Non-normalized Closeness - OUT

```r
V(var2_with_CAPS)$var2_outcloseness<-closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="out")

#Get Variable
V(var2_with_CAPS)$var2_color_degree<-round(V(var2_with_CAPS)$var2_outcloseness,6)

#Creating brewer pallette
var2_vertex_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var2_with_CAPS)$var2_color_degree)), "RdBu"))(
            length(unique(V(var2_with_CAPS)$var2_color_degree)))

#Saving as Vertex properties 
V(var2_with_CAPS)$var2_vertex_color_degree<-
  var2_vertex_color_degree[as.numeric(
  cut(V(var2_with_CAPS)$var2_color_degree,
      breaks=length(unique(V(var2_with_CAPS)$var2_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var2_with_CAPS, es=E(var2_with_CAPS), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2_with_CAPS))
maxC <- rep(Inf, vcount(var2_with_CAPS))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2_with_CAPS, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2_with_CAPS)$weight)


#PLotting
plot(var2_with_CAPS, 
     layout=co,
     edge.color=V(var2_with_CAPS)$var2_vertex_color_degree[edge.start],
     edge.arrow.size=closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="out"),
     edge.width=E(var2_with_CAPS)$weight/2*mean(E(var2_with_CAPS)$weight),
     edge.curved = TRUE,
     vertex.color=V(var2_with_CAPS)$var2_vertex_color_degree,
     vertex.size=closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="out")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2_with_CAPS,"LABEL_COR"),
     vertex.label.cex=closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="out")*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var2_with_CAPS)$var2_color_degree
b<-V(var2_with_CAPS)$var2_vertex_color_degree
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
  title("Network Closeness Degree Sized and Colored OUT - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var2_with_CAPS, mode="out", weights = E(var2_with_CAPS)$var2)), 
             sd(closeness(var2_with_CAPS, mode="out", weights = E(var2_with_CAPS)$var2))
             )
       )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-86-1.png)<!-- -->

##Closeness Non-normalized - ALL

```r
summary(var2_with_CAPS_totalcloseness)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000290 0.0006120 0.0006440 0.0006133 0.0006480 0.0007160
```

```r
sd(var2_with_CAPS_totalcloseness)
```

```
## [1] 0.0001105428
```

###Network Plotting Based On Non-normalized Closeness - ALL

```r
V(var2_with_CAPS)$var2_allcloseness<-closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="all")

#Get Variable
V(var2_with_CAPS)$var2_color_degree<-round(V(var2_with_CAPS)$var2_allcloseness,6)

#Creating brewer pallette
var2_vertex_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var2_with_CAPS)$var2_color_degree)), "RdBu"))(
            length(unique(V(var2_with_CAPS)$var2_color_degree)))

#Saving as Vertex properties 
V(var2_with_CAPS)$var2_vertex_color_degree<-
  var2_vertex_color_degree[as.numeric(
  cut(V(var2_with_CAPS)$var2_color_degree,
      breaks=length(unique(V(var2_with_CAPS)$var2_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var2_with_CAPS, es=E(var2_with_CAPS), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2_with_CAPS))
maxC <- rep(Inf, vcount(var2_with_CAPS))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2_with_CAPS, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2_with_CAPS)$weight)


#PLotting
plot(var2_with_CAPS, 
     layout=co,
     edge.color=V(var2_with_CAPS)$var2_vertex_color_degree[edge.start],
     edge.arrow.size=closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="all"),
     edge.width=E(var2_with_CAPS)$weight/2*mean(E(var2_with_CAPS)$weight),
     edge.curved = TRUE,
     vertex.color=V(var2_with_CAPS)$var2_vertex_color_degree,
     vertex.size=closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="all")*10^4,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2_with_CAPS,"LABEL_COR"),
     vertex.label.cex=(closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="all")+0.00001)*200,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var2_with_CAPS)$var2_color_degree
b<-V(var2_with_CAPS)$var2_vertex_color_degree
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
  title("Network Closeness Degree Sized and Colored all - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median all Closennes:%.4f\nSD all Closennes: %.5f",
             median(closeness(var2_with_CAPS, mode="all", weights = E(var2_with_CAPS)$var2)), 
             sd(closeness(var2_with_CAPS, mode="all", weights = E(var2_with_CAPS)$var2))
             )
       )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-88-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var2_with_CAPS)$var2_incloseness_n <- closeness(var2_with_CAPS, mode = "in",, weights = E(var2_with_CAPS)$var2, normalized = T) %>% round(10)
V(var2_with_CAPS)$var2_outcloseness_n <- closeness(var2_with_CAPS, mode = "out", normalized = T, weights = E(var2_with_CAPS)$var2) %>% round(6)
V(var2_with_CAPS)$var2_totalcloseness_n <- closeness(var2_with_CAPS, mode = "total", normalized = T, weights = E(var2_with_CAPS)$var2) %>% round(6)
```

###Saving to Environment

```r
var2_with_CAPS_incloseness_n<- closeness(var2_with_CAPS, mode = "in", normalized = T, weights = E(var2_with_CAPS)$var2) %>% round(6)
var2_with_CAPS_outcloseness_n<- closeness(var2_with_CAPS, mode = "out", normalized = T, weights = E(var2_with_CAPS)$var2) %>% round(6)
var2_with_CAPS_totalcloseness_n<- closeness(var2_with_CAPS, mode = "total", normalized = T, weights = E(var2_with_CAPS)$var2) %>% round(6)
```

###Closeness Normalized  - IN

```r
summary(var2_with_CAPS_incloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.011690 0.011800 0.011530 0.011830 0.012570
```

```r
sd(var2_with_CAPS_incloseness_n)
```

```
## [1] 0.001320909
```

##Network Plotting Based On Normalized Closeness - IN

```r
V(var2_with_CAPS)$var2_incloseness_n<-closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="in", normalized = T)

#Get Variable
V(var2_with_CAPS)$var2_color_degree<-round(V(var2_with_CAPS)$var2_incloseness_n,6)

#Creating brewer pallette
var2_vertex_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var2_with_CAPS)$var2_color_degree)), "RdBu"))(
            length(unique(V(var2_with_CAPS)$var2_color_degree)))

#Saving as Vertex properties 
V(var2_with_CAPS)$var2_vertex_color_degree<-
  var2_vertex_color_degree[as.numeric(
  cut(V(var2_with_CAPS)$var2_color_degree,
      breaks=length(unique(V(var2_with_CAPS)$var2_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var2_with_CAPS, es=E(var2_with_CAPS), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2_with_CAPS))
maxC <- rep(Inf, vcount(var2_with_CAPS))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2_with_CAPS, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2_with_CAPS)$weight)


#PLotting
plot(var2_with_CAPS, 
     layout=co,
     edge.color=V(var2_with_CAPS)$var2_vertex_color_degree[edge.start],
     edge.arrow.size=closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="in",normalized = T),
     edge.width=E(var2_with_CAPS)$weight/1000*mean(E(var2_with_CAPS)$weight),
     edge.curved = TRUE,
     vertex.color=V(var2_with_CAPS)$var2_vertex_color_degree,
     vertex.size=(closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="in",normalized = T))*1000,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2_with_CAPS,"LABEL_COR"),
     vertex.label.cex=closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="in",normalized = T)*10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var2_with_CAPS)$var2_color_degree
b<-V(var2_with_CAPS)$var2_vertex_color_degree
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
  title("Network Closeness Degree Sized Normalized In - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median In Closennes:%.4f\nSD In Closennes: %.5f",
             median(closeness(var2_with_CAPS, mode="in", weights = E(var2_with_CAPS)$var2, normalized = T)), 
             sd(closeness(var2_with_CAPS, mode="in", weights = E(var2_with_CAPS)$var2, normalized = T))
             )
       )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-92-1.png)<!-- -->
###Closeness Normalized  - OUT

```r
summary(var2_with_CAPS_outcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.005348 0.083300 0.051910 0.089750 0.100000
```

```r
sd(var2_with_CAPS_outcloseness_n)
```

```
## [1] 0.04181229
```

##Network Plotting Based On Normalized Closeness - OUT


```r
V(var2_with_CAPS)$var2_outcloseness_n<-closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="out", normalized = T)

#Get Variable
V(var2_with_CAPS)$var2_color_degree<-round(V(var2_with_CAPS)$var2_outcloseness_n,6)

#Creating brewer pallette
var2_vertex_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var2_with_CAPS)$var2_color_degree)), "RdBu"))(
            length(unique(V(var2_with_CAPS)$var2_color_degree)))

#Saving as Vertex properties 
V(var2_with_CAPS)$var2_vertex_color_degree<-
  var2_vertex_color_degree[as.numeric(
  cut(V(var2_with_CAPS)$var2_color_degree,
      breaks=length(unique(V(var2_with_CAPS)$var2_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var2_with_CAPS, es=E(var2_with_CAPS), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2_with_CAPS))
maxC <- rep(Inf, vcount(var2_with_CAPS))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2_with_CAPS, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2_with_CAPS)$weight)


#PLotting
plot(var2_with_CAPS, 
     layout=co,
     edge.color=V(var2_with_CAPS)$var2_vertex_color_degree[edge.start],
     edge.arrow.size=closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="out",normalized = T),
     edge.width=E(var2_with_CAPS)$weight/10*mean(E(var2_with_CAPS)$weight),
     edge.curved = TRUE,
     vertex.color=V(var2_with_CAPS)$var2_vertex_color_degree,
     vertex.size=(closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="out",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2_with_CAPS,"LABEL_COR"),
     vertex.label.cex=closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="out",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var2_with_CAPS)$var2_color_degree
b<-V(var2_with_CAPS)$var2_vertex_color_degree
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
  title("Network Closeness Degree Sized Normalized OUT - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median OUT Closennes:%.4f\nSD OUT Closennes: %.5f",
             median(closeness(var2_with_CAPS, mode="out", weights = E(var2_with_CAPS)$var2, normalized = T)), 
             sd(closeness(var2_with_CAPS, mode="out", weights = E(var2_with_CAPS)$var2, normalized = T))
             )
       )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-94-1.png)<!-- -->

###Closeness Normalized - ALL

```r
summary(var2_with_CAPS_totalcloseness_n)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005348 0.113900 0.119800 0.114100 0.120500 0.133200
```

```r
sd(var2_with_CAPS_totalcloseness_n)
```

```
## [1] 0.02056888
```

##Network Plotting Based On Normalized Closeness - ALL

```r
V(var2_with_CAPS)$var2_allcloseness_n<-closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="all", normalized = T)

#Get Variable
V(var2_with_CAPS)$var2_color_degree<-round(V(var2_with_CAPS)$var2_allcloseness_n,6)

#Creating brewer pallette
var2_vertex_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var2_with_CAPS)$var2_color_degree)), "RdBu"))(
            length(unique(V(var2_with_CAPS)$var2_color_degree)))

#Saving as Vertex properties 
V(var2_with_CAPS)$var2_vertex_color_degree<-
  var2_vertex_color_degree[as.numeric(
  cut(V(var2_with_CAPS)$var2_color_degree,
      breaks=length(unique(V(var2_with_CAPS)$var2_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var2_with_CAPS, es=E(var2_with_CAPS), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2_with_CAPS))
maxC <- rep(Inf, vcount(var2_with_CAPS))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2_with_CAPS, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2_with_CAPS)$weight)


#PLotting
plot(var2_with_CAPS, 
     layout=co,
     edge.color=V(var2_with_CAPS)$var2_vertex_color_degree[edge.start],
     edge.arrow.size=closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="all",normalized = T),
     edge.width=E(var2_with_CAPS)$weight/10*mean(E(var2_with_CAPS)$weight),
     edge.curved = TRUE,
     vertex.color=V(var2_with_CAPS)$var2_vertex_color_degree,
     vertex.size=(closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="all",normalized = T))*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2_with_CAPS,"LABEL_COR"),
     vertex.label.cex=closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="all",normalized = T)*1.5,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var2_with_CAPS)$var2_color_degree
b<-V(var2_with_CAPS)$var2_vertex_color_degree
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
  title("Network Closeness Degree Sized Normalized ALL - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median ALL Closennes:%.4f\nSD ALL Closennes: %.5f",
             median(closeness(var2_with_CAPS, mode="all", weights = E(var2_with_CAPS)$var2, normalized = T)), 
             sd(closeness(var2_with_CAPS, mode="all", weights = E(var2_with_CAPS)$var2, normalized = T))
             )
       )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-96-1.png)<!-- -->

##Closeness Normalized 

###Saving to Igraph object

```r
V(var2_with_CAPS)$var2_incloseness_n <- closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode = "in", normalized = T) %>% round(6)
V(var2_with_CAPS)$var2_outcloseness_n <- closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode = "out", normalized = T) %>% round(6)
V(var2_with_CAPS)$var2_totalcloseness_n <- closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode = "total", normalized = T) %>% round(6)
```

##Centralization Closseness

```r
V(var2_with_CAPS)$var2_centr_closeness<- centralization.closeness(var2_with_CAPS)$res
var2_centr_closeness<- centralization.closeness(var2_with_CAPS)$res
var2_with_CAPS_centr_closeness_all<- centralization.closeness(var2_with_CAPS)
```

###Centralization

```r
var2_with_CAPS_centr_closeness_all$centralization
```

```
## [1] 0.04861136
```

###Theoretical Max

```r
var2_with_CAPS_centr_closeness_all$theoretical_max
```

```
## [1] 185.0053
```

##Network Plotting Based On Centralization Closeness

```r
V(var2_with_CAPS)$var2_centr_closeness<- centralization.closeness(var2_with_CAPS)$res

#Get Variable
V(var2_with_CAPS)$var2_color_degree<-round(V(var2_with_CAPS)$var2_centr_closeness,6)

#Creating brewer pallette
var2_vertex_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var2_with_CAPS)$var2_color_degree)), "Spectral"))(
            length(unique(V(var2_with_CAPS)$var2_color_degree)))

#Saving as Vertex properties 
V(var2_with_CAPS)$var2_vertex_color_degree<-
  var2_vertex_color_degree[as.numeric(
  cut(V(var2_with_CAPS)$var2_color_degree,
      breaks=length(unique(V(var2_with_CAPS)$var2_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var2_with_CAPS, es=E(var2_with_CAPS), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2_with_CAPS))
maxC <- rep(Inf, vcount(var2_with_CAPS))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2_with_CAPS, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2_with_CAPS)$weight)


#PLotting
plot(var2_with_CAPS, 
     layout=co,
     edge.color=V(var2_with_CAPS)$var2_vertex_color_degree[edge.start],
     edge.arrow.size=centralization.closeness(var2_with_CAPS)$res,
     edge.width=E(var2_with_CAPS)$weight/10*mean(E(var2_with_CAPS)$weight),
     edge.curved = TRUE,
     vertex.color=V(var2_with_CAPS)$var2_vertex_color_degree,
     vertex.size=centralization.closeness(var2_with_CAPS)$res*100,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2_with_CAPS,"LABEL_COR"),
     vertex.label.cex=centralization.closeness(var2_with_CAPS)$res,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)


#Solving Problems with legend rendering 
a<-V(var2_with_CAPS)$var2_color_degree
b<-V(var2_with_CAPS)$var2_vertex_color_degree
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
  title("Network Centralization Closeness - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf(
             "Median Centralization Closeness:%.4f\nSD Centralization Closeness: %.5f",
             median(centralization.closeness(var2_with_CAPS)$res), 
             sd(centralization.closeness(var2_with_CAPS)$res)
             )
       )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-101-1.png)<!-- -->

#Closeness Dinamic Table
##Getting Closeness Measures

```r
var2_with_CAPS_incloseness<- closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode = "in") %>% round(6)
var2_with_CAPS_outcloseness<- closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode = "out") %>% round(6)
var2_with_CAPS_totalcloseness<- closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode = "total") %>% round(6)
var2_with_CAPS_incloseness_n<- closeness(var2_with_CAPS,weights = E(var2_with_CAPS)$var2, mode = "in", normalized = T) %>% round(6)
var2_with_CAPS_outcloseness_n<- closeness(var2_with_CAPS,weights = E(var2_with_CAPS)$var2, mode = "out", normalized = T) %>% round(6)
var2_with_CAPS_totalcloseness_n<- closeness(var2_with_CAPS,weights = E(var2_with_CAPS)$var2, mode = "total", normalized = T) %>% round(6)
var2_centr_closeness <- centralization.closeness(var2_with_CAPS)$res %>% round(6)
```

##Creating a datagrame of measures

```r
var2_with_CAPS_df_closseness <- data.frame(
var2_with_CAPS_incloseness,
var2_with_CAPS_outcloseness,
var2_with_CAPS_totalcloseness,
var2_with_CAPS_incloseness_n,
var2_with_CAPS_outcloseness_n,
var2_with_CAPS_totalcloseness_n,
var2_centr_closeness) %>% round(6)

#Adding type
var2_with_CAPS_df_closseness <-cbind(var2_with_CAPS_df_closseness, V(var2_with_CAPS)$LABEL_COR)

#Adding names
names(var2_with_CAPS_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var2_with_CAPS_df_closseness<-var2_with_CAPS_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var2_with_CAPS_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-269c0a7fbdc010718045" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-269c0a7fbdc010718045">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"6.8e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000538\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000716\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.012574\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.1\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.133238\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.1\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.5e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.2e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.8e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.2e-05,2.9e-05,6.2e-05,6.2e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,2.9e-05,6.3e-05,6.2e-05,6.4e-05,6.4e-05,6.2e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,2.9e-05,6.2e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.6e-05,6.6e-05,6.6e-05,6.6e-05,6.4e-05,6.3e-05,6.7e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,2.9e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.7e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,2.9e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.2e-05,6.2e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,2.9e-05,6.3e-05,6.2e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.2e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,2.9e-05,2.9e-05],[0.000515,0.000509,0.000524,0.000509,0.000483,2.9e-05,0.000491,0.000505,0.000484,0.000465,0.000471,2.9e-05,0.000494,0.000482,2.9e-05,0.000488,0.000517,2.9e-05,2.9e-05,0.000452,0.000487,0.000478,0.000485,0.000538,0.000517,0.000438,0.000473,0.000495,0.000505,0.000492,0.000486,0.000482,0.000476,2.9e-05,0.000416,0.000476,0.000485,0.000459,0.000482,0.000508,0.000515,0.000487,0.000487,0.00048,0.000511,2.9e-05,2.9e-05,0.000488,0.000488,0.000488,0.000488,0.000488,2.9e-05,0.00049,0.000499,0.00048,2.9e-05,2.9e-05,2.9e-05,0.000483,2.9e-05,0.000414,2.9e-05,0.00048,0.00047,2.9e-05,2.9e-05,0.000496,0.000445,2.9e-05,2.9e-05,0.000482,2.9e-05,0.000467,0.000483,0.00048,0.000448,0.000502,3e-05,3e-05,3e-05,3e-05,2.9e-05,0.000491,3e-05,0.000481,0.000446,2.9e-05,0.000492,0.000449,0.000484,2.9e-05,0.000414,0.000447,0.000483,0.000483,2.9e-05,2.9e-05,0.000441,2.9e-05,2.9e-05,0.000492,2.9e-05,2.9e-05,2.9e-05,0.000487,0.000455,2.9e-05,2.9e-05,2.9e-05,0.000448,0.000459,0.000448,0.000479,2.9e-05,0.000455,0.000473,0.000478,0.000484,0.00048,2.9e-05,0.000483,2.9e-05,2.9e-05,2.9e-05,0.000448,0.000479,0.00048,0.000479,0.000448,2.9e-05,2.9e-05,0.000481,0.000479,2.9e-05,0.000483,0.00048,0.000479,0.000479,2.9e-05,0.000448,0.000479,0.00048,0.000479,0.000526,0.000479,0.000481,2.9e-05,0.000476,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000445,0.000445,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000705,0.000674,0.000716,0.000663,0.000651,0.000649,0.000657,0.000669,0.000645,0.0006,0.000654,0.000652,0.000641,0.000622,0.000644,0.000648,0.000678,0.00065,0.000509,0.000646,0.000655,0.000647,0.000652,0.00068,0.000626,0.000619,0.000659,0.00066,0.000657,0.000655,0.000652,0.000648,0.000656,0.000612,0.00065,0.000641,0.000649,0.000605,0.000644,0.000664,0.000663,0.000649,0.000647,0.000645,0.000647,0.000654,0.000612,0.000647,0.000647,0.000647,0.000647,0.000647,2.9e-05,0.000659,0.000635,0.000654,0.00065,0.00057,0.000612,0.000669,0.00062,0.000565,0.000646,0.000645,0.000606,2.9e-05,0.00057,0.000638,0.000617,0.000642,0.000644,0.000645,0.000661,0.00066,0.000648,0.000645,0.000664,0.000664,0.000561,0.000561,0.000561,0.000561,0.000644,0.000619,0.000622,0.000645,0.00062,0.000576,0.000654,0.000646,0.000645,2.9e-05,0.000565,0.000629,0.000646,0.000662,0.000576,0.000599,0.000647,0.000648,0.00056,0.000652,0.000585,0.000585,0.000645,0.000648,0.000645,0.000585,0.000593,0.000649,0.000649,0.000648,0.000645,0.000644,0.000646,0.000645,0.000645,0.000646,0.000646,0.000647,0.000645,0.00065,0.000644,0.000644,0.000644,0.000645,0.000644,0.000646,0.000647,0.000633,0.000644,0.000644,0.000644,0.000646,0.000644,0.000647,0.000644,0.000648,0.000648,0.000644,0.000647,0.000645,0.000644,0.000645,0.000635,0.000655,0.000651,0.000644,0.000609,0.000631,0.000644,0.000631,0.000644,0.000631,0.000614,0.000619,0.000631,0.000644,0.000644,0.000606,2.9e-05,0.000613,0.00057,0.00057,0.000612,0.00057,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.00057,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.00062,0.000612,0.000612,2.9e-05,2.9e-05],[0.011834,0.011813,0.011861,0.011812,0.011818,0.012098,0.011794,0.011766,0.011692,0.011697,0.011817,0.011957,0.011622,0.011748,0.011941,0.01179,0.011817,0.011951,0.012574,0.011796,0.011815,0.011743,0.011812,0.011596,0.005348,0.011526,0.011528,0.011819,0.011814,0.011812,0.011819,0.011804,0.01182,0.011658,0.011814,0.011695,0.011759,0.011746,0.011728,0.011774,0.011813,0.01181,0.011801,0.011759,0.011737,0.01196,0.011658,0.011699,0.011699,0.011699,0.011699,0.011699,0.005348,0.011781,0.011523,0.011813,0.01195,0.011583,0.011658,0.011824,0.011837,0.011689,0.011944,0.011804,0.011695,0.005348,0.011583,0.011762,0.011742,0.01194,0.011941,0.011802,0.011947,0.011807,0.011809,0.011803,0.011807,0.011831,0.012358,0.012358,0.012358,0.012358,0.011945,0.01167,0.012444,0.011805,0.011751,0.011683,0.011803,0.011801,0.0118,0.005348,0.011689,0.011763,0.011801,0.011711,0.011821,0.011834,0.011806,0.011951,0.012509,0.011767,0.011865,0.011865,0.011945,0.011802,0.011804,0.011865,0.011882,0.011948,0.011807,0.01181,0.011805,0.011802,0.011945,0.011804,0.011806,0.011806,0.011802,0.011806,0.011945,0.011799,0.011944,0.011944,0.011944,0.011803,0.011801,0.011804,0.011807,0.011776,0.011941,0.011944,0.011801,0.011803,0.011944,0.011806,0.011803,0.011805,0.011805,0.011941,0.011809,0.011804,0.0118,0.011804,0.005348,0.011819,0.011807,0.011941,0.011751,0.011912,0.011941,0.011912,0.011941,0.011912,0.011455,0.011528,0.011912,0.011941,0.011941,0.011656,0.005348,0.011662,0.011583,0.011719,0.011658,0.011719,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011583,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.01185,0.011658,0.011658,0.005348,0.005348],[0.095778,0.094753,0.097433,0.094753,0.089855,0.005348,0.091356,0.093939,0.090116,0.086471,0.087653,0.005348,0.091897,0.089639,0.005348,0.090732,0.096074,0.005348,0.005348,0.084087,0.090511,0.088953,0.090247,0.1,0.096174,0.0814,0.088027,0.092079,0.093845,0.091535,0.090423,0.089639,0.088571,0.005348,0.077371,0.088529,0.09016,0.085399,0.089682,0.094416,0.095876,0.090511,0.090511,0.089209,0.094995,0.005348,0.005348,0.090732,0.090732,0.090732,0.090732,0.090732,0.005348,0.091087,0.092814,0.089337,0.005348,0.005348,0.005348,0.089812,0.005348,0.077051,0.005348,0.089294,0.087365,0.005348,0.005405,0.09217,0.082777,0.005348,0.005348,0.089595,0.005348,0.086916,0.089899,0.089294,0.083371,0.093373,0.005524,0.005524,0.005524,0.005524,0.005348,0.091311,0.005525,0.089552,0.082888,0.005376,0.09149,0.083483,0.089985,0.005348,0.077051,0.08311,0.089812,0.089899,0.005348,0.005348,0.082047,0.005348,0.005376,0.091581,0.005348,0.005348,0.005348,0.090643,0.084661,0.005348,0.005348,0.005348,0.083333,0.085321,0.083333,0.089166,0.005376,0.084661,0.088027,0.088995,0.089942,0.089209,0.005348,0.089899,0.005348,0.005348,0.005348,0.083296,0.089166,0.089337,0.089166,0.083333,0.005348,0.005348,0.089552,0.089166,0.005348,0.089899,0.089251,0.089166,0.089166,0.005348,0.083333,0.089166,0.089209,0.089166,0.097843,0.089166,0.089552,0.005348,0.088571,0.005348,0.005348,0.005348,0.005348,0.005348,0.08274,0.082703,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.131171,0.125337,0.133238,0.12326,0.121094,0.120701,0.122127,0.124498,0.12,0.111578,0.121648,0.121331,0.119307,0.115744,0.119768,0.120544,0.126102,0.120858,0.094705,0.120233,0.121887,0.12031,0.121252,0.126531,0.116468,0.115099,0.12253,0.122691,0.122127,0.121887,0.121331,0.120466,0.121967,0.113901,0.120858,0.119231,0.120779,0.112523,0.119845,0.123588,0.12326,0.120623,0.120388,0.119923,0.120388,0.121569,0.113901,0.12031,0.12031,0.12031,0.12031,0.12031,0.005348,0.12261,0.11802,0.121648,0.120858,0.105983,0.113901,0.124498,0.115385,0.105144,0.120077,0.12,0.112796,0.005348,0.106104,0.118622,0.114673,0.119461,0.119768,0.119923,0.122853,0.122772,0.120544,0.119923,0.123588,0.123588,0.10426,0.10426,0.10426,0.10426,0.119845,0.115099,0.115672,0.12,0.115313,0.107081,0.121648,0.120077,0.119923,0.005348,0.105144,0.116908,0.120155,0.123097,0.107081,0.111444,0.12031,0.120544,0.104202,0.121331,0.108899,0.108899,0.119923,0.120544,0.119923,0.108899,0.11032,0.120779,0.120779,0.120466,0.12,0.119845,0.120077,0.119923,0.12,0.120077,0.120077,0.12031,0.119923,0.120858,0.119845,0.119845,0.119845,0.12,0.119768,0.120077,0.120388,0.117722,0.119768,0.119845,0.119768,0.120155,0.119845,0.12031,0.119845,0.120466,0.120544,0.119845,0.12031,0.119923,0.119768,0.119923,0.118095,0.121887,0.121094,0.119768,0.113276,0.117424,0.119768,0.117424,0.119768,0.117424,0.11418,0.11517,0.117424,0.119768,0.119768,0.112796,0.005348,0.11411,0.105983,0.106043,0.113901,0.106043,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.105983,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.115313,0.113901,0.113901,0.005348,0.005348],[0.095778,0.094753,0.097433,0.094753,0.089855,0.005348,0.091356,0.093939,0.090116,0.086471,0.087653,0.005348,0.091897,0.089639,0.005348,0.090732,0.096074,0.005348,0.005348,0.084087,0.090511,0.088953,0.090247,0.1,0.096174,0.0814,0.088027,0.092079,0.093845,0.091535,0.090423,0.089639,0.088571,0.005348,0.077371,0.088529,0.09016,0.085399,0.089682,0.094416,0.095876,0.090511,0.090511,0.089209,0.094995,0.005348,0.005348,0.090732,0.090732,0.090732,0.090732,0.090732,0.005348,0.091087,0.092814,0.089337,0.005348,0.005348,0.005348,0.089812,0.005348,0.077051,0.005348,0.089294,0.087365,0.005348,0.005405,0.09217,0.082777,0.005348,0.005348,0.089595,0.005348,0.086916,0.089899,0.089294,0.083371,0.093373,0.005524,0.005524,0.005524,0.005524,0.005348,0.091311,0.005525,0.089552,0.082888,0.005376,0.09149,0.083483,0.089985,0.005348,0.077051,0.08311,0.089812,0.089899,0.005348,0.005348,0.082047,0.005348,0.005376,0.091581,0.005348,0.005348,0.005348,0.090643,0.084661,0.005348,0.005348,0.005348,0.083333,0.085321,0.083333,0.089166,0.005376,0.084661,0.088027,0.088995,0.089942,0.089209,0.005348,0.089899,0.005348,0.005348,0.005348,0.083296,0.089166,0.089337,0.089166,0.083333,0.005348,0.005348,0.089552,0.089166,0.005348,0.089899,0.089251,0.089166,0.089166,0.005348,0.083333,0.089166,0.089209,0.089166,0.097843,0.089166,0.089552,0.005348,0.088571,0.005348,0.005348,0.005348,0.005348,0.005348,0.08274,0.082703,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var2_with_CAPS_df_closseness, by=list(var2_with_CAPS_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - median

```r
aggdata_median <-aggregate(. ~ Type, var2_with_CAPS_df_closseness, function(x) c(median=median(x))) 


names(aggdata_median) <- c("Group","In Closeness(median)", "Out Closeness(median)", "Total Closeness(median)","In Closeness Normalized(median)", "Out Closeness Normalized(median)", "Total Closeness Normalized(median)", "Centralization Closeness(median)")

#Removing Type variable
#aggdata_median<-aggdata_median[,-c(2)]

#Merging mean and median
total_table <- merge(aggdata_mean,aggdata_median,by="Group")

#Rounding
Group<-total_table[,c(1)] #Keeping group
total_table<-total_table[,-c(1)] %>% round(6) #Rouding
total_table<-cbind(Group,total_table) #Binding toghter

#Organizing Variabels
total_table<-total_table[c("Group","In Closeness(M)", "In Closeness(median)", "Out Closeness(M)", "Out Closeness(median)", "Total Closeness(M)","Total Closeness(median)","In Closeness Normalized(M)", "In Closeness Normalized(median)", "Out Closeness Normalized(M)", "Out Closeness Normalized(median)", "Total Closeness Normalized(M)","Total Closeness Normalized(median)", "Centralization Closeness(M)","Centralization Closeness(median)")]
```

##Plotting final table with round for Closseness

```r
datatable(total_table, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-99bcb61d93cc3bb9d61e" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-99bcb61d93cc3bb9d61e">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"6.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"6.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000524\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000524\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000716\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000716\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.011951\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.011951\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.097433\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.097433\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.133238\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.133238\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.097433\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.097433\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[6.4e-05,6.1e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,5.9e-05,6.3e-05,6.4e-05,6.4e-05,5.8e-05,2.9e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05],[6.4e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,2.9e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05],[0.000489,9.7e-05,0.000452,0.000509,0.000491,0.000483,0.000487,0.000524,0.000485,0.000481,0.000502,0.000352,0.000484,0.00038,0.000343,0.000411,2.9e-05,2.9e-05,0.000515,0.000332,2.9e-05,0.000167,0.000344],[0.000476,2.9e-05,0.000452,0.000509,0.000491,0.000483,0.000487,0.000524,0.000485,0.000481,0.000502,0.000465,0.000484,0.00048,0.000483,0.000492,2.9e-05,2.9e-05,0.000515,0.000485,2.9e-05,2.9e-05,0.000476],[0.000643,0.000568,0.000646,0.000674,0.000657,0.000669,0.000657,0.000716,0.000652,0.000651,0.000664,0.000558,0.000636,0.000655,0.000659,0.000622,2.9e-05,0.000648,0.000705,0.000623,0.00065,0.000641,0.000647],[0.000656,0.000612,0.000646,0.000674,0.000657,0.000669,0.000655,0.000716,0.000652,0.000651,0.000664,0.00062,0.000636,0.000654,0.000651,0.000638,2.9e-05,0.000648,0.000705,0.000646,0.00065,0.000644,0.000646],[0.011795,0.011312,0.011796,0.011813,0.011794,0.011824,0.011818,0.011861,0.011812,0.011807,0.011831,0.011056,0.011779,0.011846,0.011911,0.010742,0.005348,0.011951,0.011834,0.01177,0.011951,0.011891,0.011844],[0.011813,0.011658,0.011796,0.011813,0.011794,0.011824,0.011819,0.011861,0.011812,0.011807,0.011831,0.011697,0.011779,0.011814,0.011818,0.011767,0.005348,0.011951,0.011834,0.011737,0.011951,0.011912,0.011806],[0.091006,0.018011,0.084087,0.094753,0.091356,0.089812,0.090585,0.097433,0.090247,0.089552,0.093373,0.065442,0.090075,0.070624,0.063759,0.076463,0.005348,0.005348,0.095778,0.061739,0.005348,0.031008,0.063946],[0.088571,0.005348,0.084087,0.094753,0.091356,0.089812,0.090511,0.097433,0.090247,0.089552,0.093373,0.086471,0.090075,0.089337,0.089855,0.091581,0.005348,0.005348,0.095778,0.090207,0.005348,0.005348,0.088511],[0.119501,0.105675,0.120233,0.125337,0.122127,0.124498,0.122155,0.133238,0.121252,0.121094,0.123588,0.103702,0.118184,0.12174,0.122632,0.115756,0.005348,0.120544,0.131171,0.115957,0.120858,0.119281,0.120359],[0.121967,0.113901,0.120233,0.125337,0.122127,0.124498,0.121887,0.133238,0.121252,0.121094,0.123588,0.115313,0.118184,0.121648,0.121094,0.118622,0.005348,0.120544,0.131171,0.120078,0.120858,0.119768,0.120038],[0.091006,0.018011,0.084087,0.094753,0.091356,0.089812,0.090585,0.097433,0.090247,0.089552,0.093373,0.065442,0.090075,0.070624,0.063759,0.076463,0.005348,0.005348,0.095778,0.061739,0.005348,0.031008,0.063946],[0.088571,0.005348,0.084087,0.094753,0.091356,0.089812,0.090511,0.097433,0.090247,0.089552,0.093373,0.086471,0.090075,0.089337,0.089855,0.091581,0.005348,0.005348,0.095778,0.090207,0.005348,0.005348,0.088511]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(median)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(median)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(median)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(median)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(median)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(median)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(median)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Natureza Governamental)

```r
var2_with_CAPS_df_closseness <- data.frame(
var2_with_CAPS_incloseness,
var2_with_CAPS_outcloseness,
var2_with_CAPS_totalcloseness,
var2_with_CAPS_incloseness_n,
var2_with_CAPS_outcloseness_n,
var2_with_CAPS_totalcloseness_n,
var2_centr_closeness) %>% round(6)

#Adding type
var2_with_CAPS_df_closseness <-cbind(var2_with_CAPS_df_closseness, V(var2_with_CAPS)$TIPO1)

#Adding names
names(var2_with_CAPS_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var2_with_CAPS_df_closseness<-var2_with_CAPS_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var2_with_CAPS_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-f5bfcf3006ddeb237a76" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-f5bfcf3006ddeb237a76">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"6.8e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000538\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000716\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.012574\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.1\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.133238\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.1\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.5e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.2e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.8e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.2e-05,2.9e-05,6.2e-05,6.2e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,2.9e-05,6.3e-05,6.2e-05,6.4e-05,6.4e-05,6.2e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,2.9e-05,6.2e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.6e-05,6.6e-05,6.6e-05,6.6e-05,6.4e-05,6.3e-05,6.7e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,2.9e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.7e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,2.9e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.2e-05,6.2e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,2.9e-05,6.3e-05,6.2e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.2e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,2.9e-05,2.9e-05],[0.000515,0.000509,0.000524,0.000509,0.000483,2.9e-05,0.000491,0.000505,0.000484,0.000465,0.000471,2.9e-05,0.000494,0.000482,2.9e-05,0.000488,0.000517,2.9e-05,2.9e-05,0.000452,0.000487,0.000478,0.000485,0.000538,0.000517,0.000438,0.000473,0.000495,0.000505,0.000492,0.000486,0.000482,0.000476,2.9e-05,0.000416,0.000476,0.000485,0.000459,0.000482,0.000508,0.000515,0.000487,0.000487,0.00048,0.000511,2.9e-05,2.9e-05,0.000488,0.000488,0.000488,0.000488,0.000488,2.9e-05,0.00049,0.000499,0.00048,2.9e-05,2.9e-05,2.9e-05,0.000483,2.9e-05,0.000414,2.9e-05,0.00048,0.00047,2.9e-05,2.9e-05,0.000496,0.000445,2.9e-05,2.9e-05,0.000482,2.9e-05,0.000467,0.000483,0.00048,0.000448,0.000502,3e-05,3e-05,3e-05,3e-05,2.9e-05,0.000491,3e-05,0.000481,0.000446,2.9e-05,0.000492,0.000449,0.000484,2.9e-05,0.000414,0.000447,0.000483,0.000483,2.9e-05,2.9e-05,0.000441,2.9e-05,2.9e-05,0.000492,2.9e-05,2.9e-05,2.9e-05,0.000487,0.000455,2.9e-05,2.9e-05,2.9e-05,0.000448,0.000459,0.000448,0.000479,2.9e-05,0.000455,0.000473,0.000478,0.000484,0.00048,2.9e-05,0.000483,2.9e-05,2.9e-05,2.9e-05,0.000448,0.000479,0.00048,0.000479,0.000448,2.9e-05,2.9e-05,0.000481,0.000479,2.9e-05,0.000483,0.00048,0.000479,0.000479,2.9e-05,0.000448,0.000479,0.00048,0.000479,0.000526,0.000479,0.000481,2.9e-05,0.000476,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000445,0.000445,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000705,0.000674,0.000716,0.000663,0.000651,0.000649,0.000657,0.000669,0.000645,0.0006,0.000654,0.000652,0.000641,0.000622,0.000644,0.000648,0.000678,0.00065,0.000509,0.000646,0.000655,0.000647,0.000652,0.00068,0.000626,0.000619,0.000659,0.00066,0.000657,0.000655,0.000652,0.000648,0.000656,0.000612,0.00065,0.000641,0.000649,0.000605,0.000644,0.000664,0.000663,0.000649,0.000647,0.000645,0.000647,0.000654,0.000612,0.000647,0.000647,0.000647,0.000647,0.000647,2.9e-05,0.000659,0.000635,0.000654,0.00065,0.00057,0.000612,0.000669,0.00062,0.000565,0.000646,0.000645,0.000606,2.9e-05,0.00057,0.000638,0.000617,0.000642,0.000644,0.000645,0.000661,0.00066,0.000648,0.000645,0.000664,0.000664,0.000561,0.000561,0.000561,0.000561,0.000644,0.000619,0.000622,0.000645,0.00062,0.000576,0.000654,0.000646,0.000645,2.9e-05,0.000565,0.000629,0.000646,0.000662,0.000576,0.000599,0.000647,0.000648,0.00056,0.000652,0.000585,0.000585,0.000645,0.000648,0.000645,0.000585,0.000593,0.000649,0.000649,0.000648,0.000645,0.000644,0.000646,0.000645,0.000645,0.000646,0.000646,0.000647,0.000645,0.00065,0.000644,0.000644,0.000644,0.000645,0.000644,0.000646,0.000647,0.000633,0.000644,0.000644,0.000644,0.000646,0.000644,0.000647,0.000644,0.000648,0.000648,0.000644,0.000647,0.000645,0.000644,0.000645,0.000635,0.000655,0.000651,0.000644,0.000609,0.000631,0.000644,0.000631,0.000644,0.000631,0.000614,0.000619,0.000631,0.000644,0.000644,0.000606,2.9e-05,0.000613,0.00057,0.00057,0.000612,0.00057,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.00057,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.00062,0.000612,0.000612,2.9e-05,2.9e-05],[0.011834,0.011813,0.011861,0.011812,0.011818,0.012098,0.011794,0.011766,0.011692,0.011697,0.011817,0.011957,0.011622,0.011748,0.011941,0.01179,0.011817,0.011951,0.012574,0.011796,0.011815,0.011743,0.011812,0.011596,0.005348,0.011526,0.011528,0.011819,0.011814,0.011812,0.011819,0.011804,0.01182,0.011658,0.011814,0.011695,0.011759,0.011746,0.011728,0.011774,0.011813,0.01181,0.011801,0.011759,0.011737,0.01196,0.011658,0.011699,0.011699,0.011699,0.011699,0.011699,0.005348,0.011781,0.011523,0.011813,0.01195,0.011583,0.011658,0.011824,0.011837,0.011689,0.011944,0.011804,0.011695,0.005348,0.011583,0.011762,0.011742,0.01194,0.011941,0.011802,0.011947,0.011807,0.011809,0.011803,0.011807,0.011831,0.012358,0.012358,0.012358,0.012358,0.011945,0.01167,0.012444,0.011805,0.011751,0.011683,0.011803,0.011801,0.0118,0.005348,0.011689,0.011763,0.011801,0.011711,0.011821,0.011834,0.011806,0.011951,0.012509,0.011767,0.011865,0.011865,0.011945,0.011802,0.011804,0.011865,0.011882,0.011948,0.011807,0.01181,0.011805,0.011802,0.011945,0.011804,0.011806,0.011806,0.011802,0.011806,0.011945,0.011799,0.011944,0.011944,0.011944,0.011803,0.011801,0.011804,0.011807,0.011776,0.011941,0.011944,0.011801,0.011803,0.011944,0.011806,0.011803,0.011805,0.011805,0.011941,0.011809,0.011804,0.0118,0.011804,0.005348,0.011819,0.011807,0.011941,0.011751,0.011912,0.011941,0.011912,0.011941,0.011912,0.011455,0.011528,0.011912,0.011941,0.011941,0.011656,0.005348,0.011662,0.011583,0.011719,0.011658,0.011719,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011583,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.01185,0.011658,0.011658,0.005348,0.005348],[0.095778,0.094753,0.097433,0.094753,0.089855,0.005348,0.091356,0.093939,0.090116,0.086471,0.087653,0.005348,0.091897,0.089639,0.005348,0.090732,0.096074,0.005348,0.005348,0.084087,0.090511,0.088953,0.090247,0.1,0.096174,0.0814,0.088027,0.092079,0.093845,0.091535,0.090423,0.089639,0.088571,0.005348,0.077371,0.088529,0.09016,0.085399,0.089682,0.094416,0.095876,0.090511,0.090511,0.089209,0.094995,0.005348,0.005348,0.090732,0.090732,0.090732,0.090732,0.090732,0.005348,0.091087,0.092814,0.089337,0.005348,0.005348,0.005348,0.089812,0.005348,0.077051,0.005348,0.089294,0.087365,0.005348,0.005405,0.09217,0.082777,0.005348,0.005348,0.089595,0.005348,0.086916,0.089899,0.089294,0.083371,0.093373,0.005524,0.005524,0.005524,0.005524,0.005348,0.091311,0.005525,0.089552,0.082888,0.005376,0.09149,0.083483,0.089985,0.005348,0.077051,0.08311,0.089812,0.089899,0.005348,0.005348,0.082047,0.005348,0.005376,0.091581,0.005348,0.005348,0.005348,0.090643,0.084661,0.005348,0.005348,0.005348,0.083333,0.085321,0.083333,0.089166,0.005376,0.084661,0.088027,0.088995,0.089942,0.089209,0.005348,0.089899,0.005348,0.005348,0.005348,0.083296,0.089166,0.089337,0.089166,0.083333,0.005348,0.005348,0.089552,0.089166,0.005348,0.089899,0.089251,0.089166,0.089166,0.005348,0.083333,0.089166,0.089209,0.089166,0.097843,0.089166,0.089552,0.005348,0.088571,0.005348,0.005348,0.005348,0.005348,0.005348,0.08274,0.082703,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.131171,0.125337,0.133238,0.12326,0.121094,0.120701,0.122127,0.124498,0.12,0.111578,0.121648,0.121331,0.119307,0.115744,0.119768,0.120544,0.126102,0.120858,0.094705,0.120233,0.121887,0.12031,0.121252,0.126531,0.116468,0.115099,0.12253,0.122691,0.122127,0.121887,0.121331,0.120466,0.121967,0.113901,0.120858,0.119231,0.120779,0.112523,0.119845,0.123588,0.12326,0.120623,0.120388,0.119923,0.120388,0.121569,0.113901,0.12031,0.12031,0.12031,0.12031,0.12031,0.005348,0.12261,0.11802,0.121648,0.120858,0.105983,0.113901,0.124498,0.115385,0.105144,0.120077,0.12,0.112796,0.005348,0.106104,0.118622,0.114673,0.119461,0.119768,0.119923,0.122853,0.122772,0.120544,0.119923,0.123588,0.123588,0.10426,0.10426,0.10426,0.10426,0.119845,0.115099,0.115672,0.12,0.115313,0.107081,0.121648,0.120077,0.119923,0.005348,0.105144,0.116908,0.120155,0.123097,0.107081,0.111444,0.12031,0.120544,0.104202,0.121331,0.108899,0.108899,0.119923,0.120544,0.119923,0.108899,0.11032,0.120779,0.120779,0.120466,0.12,0.119845,0.120077,0.119923,0.12,0.120077,0.120077,0.12031,0.119923,0.120858,0.119845,0.119845,0.119845,0.12,0.119768,0.120077,0.120388,0.117722,0.119768,0.119845,0.119768,0.120155,0.119845,0.12031,0.119845,0.120466,0.120544,0.119845,0.12031,0.119923,0.119768,0.119923,0.118095,0.121887,0.121094,0.119768,0.113276,0.117424,0.119768,0.117424,0.119768,0.117424,0.11418,0.11517,0.117424,0.119768,0.119768,0.112796,0.005348,0.11411,0.105983,0.106043,0.113901,0.106043,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.105983,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.115313,0.113901,0.113901,0.005348,0.005348],[0.095778,0.094753,0.097433,0.094753,0.089855,0.005348,0.091356,0.093939,0.090116,0.086471,0.087653,0.005348,0.091897,0.089639,0.005348,0.090732,0.096074,0.005348,0.005348,0.084087,0.090511,0.088953,0.090247,0.1,0.096174,0.0814,0.088027,0.092079,0.093845,0.091535,0.090423,0.089639,0.088571,0.005348,0.077371,0.088529,0.09016,0.085399,0.089682,0.094416,0.095876,0.090511,0.090511,0.089209,0.094995,0.005348,0.005348,0.090732,0.090732,0.090732,0.090732,0.090732,0.005348,0.091087,0.092814,0.089337,0.005348,0.005348,0.005348,0.089812,0.005348,0.077051,0.005348,0.089294,0.087365,0.005348,0.005405,0.09217,0.082777,0.005348,0.005348,0.089595,0.005348,0.086916,0.089899,0.089294,0.083371,0.093373,0.005524,0.005524,0.005524,0.005524,0.005348,0.091311,0.005525,0.089552,0.082888,0.005376,0.09149,0.083483,0.089985,0.005348,0.077051,0.08311,0.089812,0.089899,0.005348,0.005348,0.082047,0.005348,0.005376,0.091581,0.005348,0.005348,0.005348,0.090643,0.084661,0.005348,0.005348,0.005348,0.083333,0.085321,0.083333,0.089166,0.005376,0.084661,0.088027,0.088995,0.089942,0.089209,0.005348,0.089899,0.005348,0.005348,0.005348,0.083296,0.089166,0.089337,0.089166,0.083333,0.005348,0.005348,0.089552,0.089166,0.005348,0.089899,0.089251,0.089166,0.089166,0.005348,0.083333,0.089166,0.089209,0.089166,0.097843,0.089166,0.089552,0.005348,0.088571,0.005348,0.005348,0.005348,0.005348,0.005348,0.08274,0.082703,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var2_with_CAPS_df_closseness, by=list(var2_with_CAPS_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - median

```r
aggdata_median <-aggregate(. ~ Type, var2_with_CAPS_df_closseness, function(x) c(median=median(x))) 

names(aggdata_median) <- c("Group","In Closeness(median)", "Out Closeness(median)", "Total Closeness(median)","In Closeness Normalized(median)", "Out Closeness Normalized(median)", "Total Closeness Normalized(median)", "Centralization Closeness(median)")

#Removing Type variable
#aggdata_median<-aggdata_median[,-c(2)]

#Merging mean and median
total_table <- merge(aggdata_mean,aggdata_median,by="Group")

#Rounding
Group<-total_table[,c(1)] #Keeping group
total_table<-total_table[,-c(1)] %>% round(6) #Rouding
total_table<-cbind(Group,total_table) #Binding toghter

#Organizing Variabels
total_table<-total_table[c("Group","In Closeness(M)", "In Closeness(median)", "Out Closeness(M)", "Out Closeness(median)", "Total Closeness(M)","Total Closeness(median)","In Closeness Normalized(M)", "In Closeness Normalized(median)", "Out Closeness Normalized(M)", "Out Closeness Normalized(median)", "Total Closeness Normalized(M)","Total Closeness Normalized(median)", "Centralization Closeness(M)","Centralization Closeness(median)")]
```

##Plotting final table with round for Closseness

```r
datatable(total_table, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-b310bb6576b978ffd8d4" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-b310bb6576b978ffd8d4">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6e-05\" data-max=\"6.3e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6.3e-05\" data-max=\"6.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000211\" data-max=\"0.000329\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000477\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000575\" data-max=\"0.000641\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000612\" data-max=\"0.000646\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.01119\" data-max=\"0.011783\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.011658\" data-max=\"0.011812\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.039185\" data-max=\"0.061214\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005376\" data-max=\"0.088783\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.107025\" data-max=\"0.119249\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.113901\" data-max=\"0.120155\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.039185\" data-max=\"0.061214\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005376\" data-max=\"0.088783\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[6.3e-05,6e-05],[6.4e-05,6.3e-05],[0.000329,0.000211],[0.000477,2.9e-05],[0.000641,0.000575],[0.000646,0.000612],[0.011783,0.01119],[0.011812,0.011658],[0.061214,0.039185],[0.088783,0.005376],[0.119249,0.107025],[0.120155,0.113901],[0.061214,0.039185],[0.088783,0.005376]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(median)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(median)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(median)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(median)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(median)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(median)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(median)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Creating a datagrame of measures (Setores)

```r
var2_with_CAPS_df_closseness <- data.frame(
var2_with_CAPS_incloseness,
var2_with_CAPS_outcloseness,
var2_with_CAPS_totalcloseness,
var2_with_CAPS_incloseness_n,
var2_with_CAPS_outcloseness_n,
var2_with_CAPS_totalcloseness_n,
var2_centr_closeness) %>% round(6)

#Adding type
var2_with_CAPS_df_closseness <-cbind(var2_with_CAPS_df_closseness, V(var2_with_CAPS)$TIPO2)

#Adding names
names(var2_with_CAPS_df_closseness) <- c("In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized", "Total Closeness Normalized","Centralization Closeness","Type")

#Ordering Variables
var2_with_CAPS_df_closseness<-var2_with_CAPS_df_closseness[c("Type","In Closeness", "Out Closeness", "Total Closeness","In Closeness Normalized", "Out Closeness Normalized","Total Closeness Normalized", "Centralization Closeness")]
```

##General tabel - DT

```r
datatable(var2_with_CAPS_df_closseness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-c21cd6d0fd0cbf6b95d4" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-c21cd6d0fd0cbf6b95d4">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"6.8e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000538\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000716\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.012574\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.1\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.133238\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005348\" data-max=\"0.1\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.5e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.2e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.8e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.2e-05,2.9e-05,6.2e-05,6.2e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,2.9e-05,6.3e-05,6.2e-05,6.4e-05,6.4e-05,6.2e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,2.9e-05,6.2e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.6e-05,6.6e-05,6.6e-05,6.6e-05,6.4e-05,6.3e-05,6.7e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,2.9e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.4e-05,6.7e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.4e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,2.9e-05,6.4e-05,6.3e-05,6.4e-05,6.3e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.4e-05,6.2e-05,6.2e-05,6.4e-05,6.4e-05,6.4e-05,6.3e-05,2.9e-05,6.3e-05,6.2e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.2e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.3e-05,6.4e-05,6.3e-05,6.3e-05,2.9e-05,2.9e-05],[0.000515,0.000509,0.000524,0.000509,0.000483,2.9e-05,0.000491,0.000505,0.000484,0.000465,0.000471,2.9e-05,0.000494,0.000482,2.9e-05,0.000488,0.000517,2.9e-05,2.9e-05,0.000452,0.000487,0.000478,0.000485,0.000538,0.000517,0.000438,0.000473,0.000495,0.000505,0.000492,0.000486,0.000482,0.000476,2.9e-05,0.000416,0.000476,0.000485,0.000459,0.000482,0.000508,0.000515,0.000487,0.000487,0.00048,0.000511,2.9e-05,2.9e-05,0.000488,0.000488,0.000488,0.000488,0.000488,2.9e-05,0.00049,0.000499,0.00048,2.9e-05,2.9e-05,2.9e-05,0.000483,2.9e-05,0.000414,2.9e-05,0.00048,0.00047,2.9e-05,2.9e-05,0.000496,0.000445,2.9e-05,2.9e-05,0.000482,2.9e-05,0.000467,0.000483,0.00048,0.000448,0.000502,3e-05,3e-05,3e-05,3e-05,2.9e-05,0.000491,3e-05,0.000481,0.000446,2.9e-05,0.000492,0.000449,0.000484,2.9e-05,0.000414,0.000447,0.000483,0.000483,2.9e-05,2.9e-05,0.000441,2.9e-05,2.9e-05,0.000492,2.9e-05,2.9e-05,2.9e-05,0.000487,0.000455,2.9e-05,2.9e-05,2.9e-05,0.000448,0.000459,0.000448,0.000479,2.9e-05,0.000455,0.000473,0.000478,0.000484,0.00048,2.9e-05,0.000483,2.9e-05,2.9e-05,2.9e-05,0.000448,0.000479,0.00048,0.000479,0.000448,2.9e-05,2.9e-05,0.000481,0.000479,2.9e-05,0.000483,0.00048,0.000479,0.000479,2.9e-05,0.000448,0.000479,0.00048,0.000479,0.000526,0.000479,0.000481,2.9e-05,0.000476,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,0.000445,0.000445,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05,2.9e-05],[0.000705,0.000674,0.000716,0.000663,0.000651,0.000649,0.000657,0.000669,0.000645,0.0006,0.000654,0.000652,0.000641,0.000622,0.000644,0.000648,0.000678,0.00065,0.000509,0.000646,0.000655,0.000647,0.000652,0.00068,0.000626,0.000619,0.000659,0.00066,0.000657,0.000655,0.000652,0.000648,0.000656,0.000612,0.00065,0.000641,0.000649,0.000605,0.000644,0.000664,0.000663,0.000649,0.000647,0.000645,0.000647,0.000654,0.000612,0.000647,0.000647,0.000647,0.000647,0.000647,2.9e-05,0.000659,0.000635,0.000654,0.00065,0.00057,0.000612,0.000669,0.00062,0.000565,0.000646,0.000645,0.000606,2.9e-05,0.00057,0.000638,0.000617,0.000642,0.000644,0.000645,0.000661,0.00066,0.000648,0.000645,0.000664,0.000664,0.000561,0.000561,0.000561,0.000561,0.000644,0.000619,0.000622,0.000645,0.00062,0.000576,0.000654,0.000646,0.000645,2.9e-05,0.000565,0.000629,0.000646,0.000662,0.000576,0.000599,0.000647,0.000648,0.00056,0.000652,0.000585,0.000585,0.000645,0.000648,0.000645,0.000585,0.000593,0.000649,0.000649,0.000648,0.000645,0.000644,0.000646,0.000645,0.000645,0.000646,0.000646,0.000647,0.000645,0.00065,0.000644,0.000644,0.000644,0.000645,0.000644,0.000646,0.000647,0.000633,0.000644,0.000644,0.000644,0.000646,0.000644,0.000647,0.000644,0.000648,0.000648,0.000644,0.000647,0.000645,0.000644,0.000645,0.000635,0.000655,0.000651,0.000644,0.000609,0.000631,0.000644,0.000631,0.000644,0.000631,0.000614,0.000619,0.000631,0.000644,0.000644,0.000606,2.9e-05,0.000613,0.00057,0.00057,0.000612,0.00057,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.00057,0.000612,0.000612,0.000612,0.000612,0.000612,0.000612,0.00062,0.000612,0.000612,2.9e-05,2.9e-05],[0.011834,0.011813,0.011861,0.011812,0.011818,0.012098,0.011794,0.011766,0.011692,0.011697,0.011817,0.011957,0.011622,0.011748,0.011941,0.01179,0.011817,0.011951,0.012574,0.011796,0.011815,0.011743,0.011812,0.011596,0.005348,0.011526,0.011528,0.011819,0.011814,0.011812,0.011819,0.011804,0.01182,0.011658,0.011814,0.011695,0.011759,0.011746,0.011728,0.011774,0.011813,0.01181,0.011801,0.011759,0.011737,0.01196,0.011658,0.011699,0.011699,0.011699,0.011699,0.011699,0.005348,0.011781,0.011523,0.011813,0.01195,0.011583,0.011658,0.011824,0.011837,0.011689,0.011944,0.011804,0.011695,0.005348,0.011583,0.011762,0.011742,0.01194,0.011941,0.011802,0.011947,0.011807,0.011809,0.011803,0.011807,0.011831,0.012358,0.012358,0.012358,0.012358,0.011945,0.01167,0.012444,0.011805,0.011751,0.011683,0.011803,0.011801,0.0118,0.005348,0.011689,0.011763,0.011801,0.011711,0.011821,0.011834,0.011806,0.011951,0.012509,0.011767,0.011865,0.011865,0.011945,0.011802,0.011804,0.011865,0.011882,0.011948,0.011807,0.01181,0.011805,0.011802,0.011945,0.011804,0.011806,0.011806,0.011802,0.011806,0.011945,0.011799,0.011944,0.011944,0.011944,0.011803,0.011801,0.011804,0.011807,0.011776,0.011941,0.011944,0.011801,0.011803,0.011944,0.011806,0.011803,0.011805,0.011805,0.011941,0.011809,0.011804,0.0118,0.011804,0.005348,0.011819,0.011807,0.011941,0.011751,0.011912,0.011941,0.011912,0.011941,0.011912,0.011455,0.011528,0.011912,0.011941,0.011941,0.011656,0.005348,0.011662,0.011583,0.011719,0.011658,0.011719,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.011583,0.011658,0.011658,0.011658,0.011658,0.011658,0.011658,0.01185,0.011658,0.011658,0.005348,0.005348],[0.095778,0.094753,0.097433,0.094753,0.089855,0.005348,0.091356,0.093939,0.090116,0.086471,0.087653,0.005348,0.091897,0.089639,0.005348,0.090732,0.096074,0.005348,0.005348,0.084087,0.090511,0.088953,0.090247,0.1,0.096174,0.0814,0.088027,0.092079,0.093845,0.091535,0.090423,0.089639,0.088571,0.005348,0.077371,0.088529,0.09016,0.085399,0.089682,0.094416,0.095876,0.090511,0.090511,0.089209,0.094995,0.005348,0.005348,0.090732,0.090732,0.090732,0.090732,0.090732,0.005348,0.091087,0.092814,0.089337,0.005348,0.005348,0.005348,0.089812,0.005348,0.077051,0.005348,0.089294,0.087365,0.005348,0.005405,0.09217,0.082777,0.005348,0.005348,0.089595,0.005348,0.086916,0.089899,0.089294,0.083371,0.093373,0.005524,0.005524,0.005524,0.005524,0.005348,0.091311,0.005525,0.089552,0.082888,0.005376,0.09149,0.083483,0.089985,0.005348,0.077051,0.08311,0.089812,0.089899,0.005348,0.005348,0.082047,0.005348,0.005376,0.091581,0.005348,0.005348,0.005348,0.090643,0.084661,0.005348,0.005348,0.005348,0.083333,0.085321,0.083333,0.089166,0.005376,0.084661,0.088027,0.088995,0.089942,0.089209,0.005348,0.089899,0.005348,0.005348,0.005348,0.083296,0.089166,0.089337,0.089166,0.083333,0.005348,0.005348,0.089552,0.089166,0.005348,0.089899,0.089251,0.089166,0.089166,0.005348,0.083333,0.089166,0.089209,0.089166,0.097843,0.089166,0.089552,0.005348,0.088571,0.005348,0.005348,0.005348,0.005348,0.005348,0.08274,0.082703,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348],[0.131171,0.125337,0.133238,0.12326,0.121094,0.120701,0.122127,0.124498,0.12,0.111578,0.121648,0.121331,0.119307,0.115744,0.119768,0.120544,0.126102,0.120858,0.094705,0.120233,0.121887,0.12031,0.121252,0.126531,0.116468,0.115099,0.12253,0.122691,0.122127,0.121887,0.121331,0.120466,0.121967,0.113901,0.120858,0.119231,0.120779,0.112523,0.119845,0.123588,0.12326,0.120623,0.120388,0.119923,0.120388,0.121569,0.113901,0.12031,0.12031,0.12031,0.12031,0.12031,0.005348,0.12261,0.11802,0.121648,0.120858,0.105983,0.113901,0.124498,0.115385,0.105144,0.120077,0.12,0.112796,0.005348,0.106104,0.118622,0.114673,0.119461,0.119768,0.119923,0.122853,0.122772,0.120544,0.119923,0.123588,0.123588,0.10426,0.10426,0.10426,0.10426,0.119845,0.115099,0.115672,0.12,0.115313,0.107081,0.121648,0.120077,0.119923,0.005348,0.105144,0.116908,0.120155,0.123097,0.107081,0.111444,0.12031,0.120544,0.104202,0.121331,0.108899,0.108899,0.119923,0.120544,0.119923,0.108899,0.11032,0.120779,0.120779,0.120466,0.12,0.119845,0.120077,0.119923,0.12,0.120077,0.120077,0.12031,0.119923,0.120858,0.119845,0.119845,0.119845,0.12,0.119768,0.120077,0.120388,0.117722,0.119768,0.119845,0.119768,0.120155,0.119845,0.12031,0.119845,0.120466,0.120544,0.119845,0.12031,0.119923,0.119768,0.119923,0.118095,0.121887,0.121094,0.119768,0.113276,0.117424,0.119768,0.117424,0.119768,0.117424,0.11418,0.11517,0.117424,0.119768,0.119768,0.112796,0.005348,0.11411,0.105983,0.106043,0.113901,0.106043,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.105983,0.113901,0.113901,0.113901,0.113901,0.113901,0.113901,0.115313,0.113901,0.113901,0.005348,0.005348],[0.095778,0.094753,0.097433,0.094753,0.089855,0.005348,0.091356,0.093939,0.090116,0.086471,0.087653,0.005348,0.091897,0.089639,0.005348,0.090732,0.096074,0.005348,0.005348,0.084087,0.090511,0.088953,0.090247,0.1,0.096174,0.0814,0.088027,0.092079,0.093845,0.091535,0.090423,0.089639,0.088571,0.005348,0.077371,0.088529,0.09016,0.085399,0.089682,0.094416,0.095876,0.090511,0.090511,0.089209,0.094995,0.005348,0.005348,0.090732,0.090732,0.090732,0.090732,0.090732,0.005348,0.091087,0.092814,0.089337,0.005348,0.005348,0.005348,0.089812,0.005348,0.077051,0.005348,0.089294,0.087365,0.005348,0.005405,0.09217,0.082777,0.005348,0.005348,0.089595,0.005348,0.086916,0.089899,0.089294,0.083371,0.093373,0.005524,0.005524,0.005524,0.005524,0.005348,0.091311,0.005525,0.089552,0.082888,0.005376,0.09149,0.083483,0.089985,0.005348,0.077051,0.08311,0.089812,0.089899,0.005348,0.005348,0.082047,0.005348,0.005376,0.091581,0.005348,0.005348,0.005348,0.090643,0.084661,0.005348,0.005348,0.005348,0.083333,0.085321,0.083333,0.089166,0.005376,0.084661,0.088027,0.088995,0.089942,0.089209,0.005348,0.089899,0.005348,0.005348,0.005348,0.083296,0.089166,0.089337,0.089166,0.083333,0.005348,0.005348,0.089552,0.089166,0.005348,0.089899,0.089251,0.089166,0.089166,0.005348,0.083333,0.089166,0.089209,0.089166,0.097843,0.089166,0.089552,0.005348,0.088571,0.005348,0.005348,0.005348,0.005348,0.005348,0.08274,0.082703,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348,0.005348]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>In Closeness\u003c/th>\n      <th>Out Closeness\u003c/th>\n      <th>Total Closeness\u003c/th>\n      <th>In Closeness Normalized\u003c/th>\n      <th>Out Closeness Normalized\u003c/th>\n      <th>Total Closeness Normalized\u003c/th>\n      <th>Centralization Closeness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var2_with_CAPS_df_closseness, by=list(var2_with_CAPS_df_closseness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","In Closeness(M)", "Out Closeness(M)", "Total Closeness(M)","In Closeness Normalized(M)", "Out Closeness Normalized(M)", "Total Closeness Normalized(M)","Centralization Closeness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```

##Aggregating data from previous table - median

```r
aggdata_median <-aggregate(. ~ Type, var2_with_CAPS_df_closseness, function(x) c(median=median(x))) 

names(aggdata_median) <- c("Group","In Closeness(median)", "Out Closeness(median)", "Total Closeness(median)","In Closeness Normalized(median)", "Out Closeness Normalized(median)", "Total Closeness Normalized(median)", "Centralization Closeness(median)")

#Removing Type variable
#aggdata_median<-aggdata_median[,-c(2)]

#Merging mean and median
total_table <- merge(aggdata_mean,aggdata_median,by="Group")

#Rounding
Group<-total_table[,c(1)] #Keeping group
total_table<-total_table[,-c(1)] %>% round(6) #Rouding
total_table<-cbind(Group,total_table) #Binding toghter

#Organizing Variabels
total_table<-total_table[c("Group","In Closeness(M)", "In Closeness(median)", "Out Closeness(M)", "Out Closeness(median)", "Total Closeness(M)","Total Closeness(median)","In Closeness Normalized(M)", "In Closeness Normalized(median)", "Out Closeness Normalized(M)", "Out Closeness Normalized(median)", "Total Closeness Normalized(M)","Total Closeness Normalized(median)", "Centralization Closeness(M)","Centralization Closeness(median)")]
```

##Plotting final table with round for Closseness

```r
datatable(total_table, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-0302ffc450cffd33a570" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-0302ffc450cffd33a570">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6e-05\" data-max=\"6.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"6.3e-05\" data-max=\"6.4e-05\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000213\" data-max=\"0.000396\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.9e-05\" data-max=\"0.000483\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000574\" data-max=\"0.000657\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.000612\" data-max=\"0.000654\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.01118\" data-max=\"0.011854\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.011658\" data-max=\"0.011817\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.039631\" data-max=\"0.073647\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005376\" data-max=\"0.089855\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.106843\" data-max=\"0.122158\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.113901\" data-max=\"0.121648\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.039631\" data-max=\"0.073647\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.005376\" data-max=\"0.089855\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[6.4e-05,6.3e-05,6e-05],[6.4e-05,6.3e-05,6.3e-05],[0.000396,0.000315,0.000213],[0.000483,0.000463,2.9e-05],[0.000657,0.000639,0.000574],[0.000654,0.000646,0.000612],[0.011854,0.011774,0.01118],[0.011817,0.011807,0.011658],[0.073647,0.058625,0.039631],[0.089855,0.086158,0.005376],[0.122158,0.118807,0.106843],[0.121648,0.120038,0.113901],[0.073647,0.058625,0.039631],[0.089855,0.086158,0.005376]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>In Closeness(M)\u003c/th>\n      <th>In Closeness(median)\u003c/th>\n      <th>Out Closeness(M)\u003c/th>\n      <th>Out Closeness(median)\u003c/th>\n      <th>Total Closeness(M)\u003c/th>\n      <th>Total Closeness(median)\u003c/th>\n      <th>In Closeness Normalized(M)\u003c/th>\n      <th>In Closeness Normalized(median)\u003c/th>\n      <th>Out Closeness Normalized(M)\u003c/th>\n      <th>Out Closeness Normalized(median)\u003c/th>\n      <th>Total Closeness Normalized(M)\u003c/th>\n      <th>Total Closeness Normalized(median)\u003c/th>\n      <th>Centralization Closeness(M)\u003c/th>\n      <th>Centralization Closeness(median)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11,12,13,14,15]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Betweenness - Number of shortest paths going through the actor σst(i)

High betweenness centrality - vertex lies on many shortest paths. Probability that a communication from s
to t will go through i - considering σst(i).

Betweenness measures the number of shortest paths between nodes in the network that go through the node in question. Nodes with relatively high betweenness are likely to be key conduits of information flow across a network, and their removal may have a large impact on spreading phenomena.

##Betweenness Centrality -(Vertex)

Betweenness centrality based on a broker position connecting others or Number of geodesics that pass through the node or the edge. A higher number means an more important node.

#Adding weight equal one for all analysis 

```r
E(var2_with_CAPS)$equalone<-1
```

###Saving objects

```r
V(var2_with_CAPS)$var2_betweenness <- betweenness(var2_with_CAPS, weights = E(var2_with_CAPS)$equalone) %>% round(6)
var2_with_CAPS_betweenness <- betweenness(var2_with_CAPS, weights = E(var2_with_CAPS)$equalone) %>% round(6)

#Z Score 
V(var2_with_CAPS)$var2_with_CAPS_betweenness_zscore <- (V(var2_with_CAPS)$var2_betweenness - mean(V(var2_with_CAPS)$var2_betweenness))/sd(V(var2_with_CAPS)$var2_betweenness)

#Normalized
V(var2_with_CAPS)$var2_with_CAPS_betweenness_norm <- (V(var2_with_CAPS)$var2_betweenness - min(V(var2_with_CAPS)$var2_betweenness))/max(V(var2_with_CAPS)$var2_betweenness)-min(V(var2_with_CAPS)$var2_betweenness)
```
###Betweenness Centrality - all

```r
summary(var2_with_CAPS_betweenness)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##    0.000    0.000    0.245  235.300   50.000 7914.000
```

```r
sd(var2_with_CAPS_betweenness)
```

```
## [1] 849.4386
```

##Betweenness Centrality Weighted - (Vertex)

```r
V(var2_with_CAPS)$var2_betweenness_w <- betweenness(var2_with_CAPS, weights=E(var2_with_CAPS)$var2) %>% round(6)
var2_betweenness_w <- betweenness(var2_with_CAPS, weights=E(var2_with_CAPS)$var2) %>% round(6)
```
### Descriptive Betweenness Centrality Weighted - (Vertex) - all

```r
summary(var2_betweenness_w)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##    0.000    0.000    0.245  235.300   50.000 7914.000
```

```r
sd(var2_betweenness_w)
```

```
## [1] 849.4386
```
##Centralization Betweenness

```r
V(var2_with_CAPS)$var2_centr_betweenness <- centralization.betweenness(var2_with_CAPS)$res
var2_centr_betweenness <- centralization.betweenness(var2_with_CAPS)
```
###Centralization

```r
var2_centr_betweenness$centralization
```

```
## [1] 0.2243677
```
###Theoretical Max

```r
var2_centr_betweenness$theoretical_max
```

```
## [1] 6400260
```
#Betweenness Vertex Centrality Dinamic Table

```r
#Betweenness Vertex Centrality Measures Dinamic Table
#Getting  Measures
var2_with_CAPS_betweenness <- betweenness(var2_with_CAPS, weights=E(var2_with_CAPS)$equalone) %>% round(6)
var2_betweenness_w <- betweenness(var2_with_CAPS, weights=E(var2_with_CAPS)$var2) %>% round(6)
var2_centr_betweenness <- centralization.betweenness(var2_with_CAPS)$res %>% round(6)

#Creating a dataframe of measures
var2_with_CAPS_df_betweenness <- data.frame(var2_with_CAPS_betweenness,
var2_betweenness_w,
var2_centr_betweenness) %>% round(6)

#Adding type
var2_with_CAPS_df_betweenness <-cbind(var2_with_CAPS_df_betweenness, V(var2_with_CAPS)$LABEL_COR)

#Adding names
names(var2_with_CAPS_df_betweenness) <- c("Betweenness", "Betweenness Weighted", "Centralization Betweenness","Type")

#Ordering Variables
var2_with_CAPS_df_betweenness<-var2_with_CAPS_df_betweenness[c("Type","Betweenness", "Betweenness Weighted", "Centralization Betweenness")]
```
## General tabel for Betweenness

```r
datatable(var2_with_CAPS_df_betweenness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-f0fc743bb28e956f6d98" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-f0fc743bb28e956f6d98">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"7914.493831\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"7914.493831\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"7914.493831\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[3549.348644,1249.546833,7914.493831,478.797711,31.148501,0,240.628294,973.552803,7.527778,0,436.829664,0,277.197244,160.609413,0,47.706421,1003.192308,0,0,51.116781,526.528028,380.506044,53.275545,3627.863102,0,1.666667,114.728114,758.571128,253.655795,46.920456,545.638048,49.275722,134.878303,0,8.808369,0,82.26419,22.830159,0.541667,1187.408388,4301.339128,630.715092,66.851618,0.291667,3595,0,0,0.245401,0.245401,0.245401,0.245401,0.245401,0,840.773782,734,81.293363,0,0,0,529.080736,0,0,0,0.966667,347.114164,0,0,20.471223,89.149531,0,0,1.317406,0,12.212219,50.716505,0,4.587302,1323.066299,0,0,0,0,0,3552.863102,632,31.761254,36.466667,0,532.247752,1.566667,20.623626,0,0,554.076923,1.2,219.70267,0,0,273.986996,0,109,120.321552,0,0,0,5.060929,0,0,0,0,2.563228,21.145851,1.942857,0,0.733333,0.45,2.67197,11.14899,4.77316,1.797403,0,44.987657,0,0,0,0.333333,0,1.278571,9.370285,0.333333,0,0,1.067406,1.99395,0,112.23844,0,1.99395,1.99395,0,179.840476,58.393803,0,0,0,1.879545,369.942152,0,220.664286,0,0,0,0,0,3.177137,4.177137,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[3549.348644,1249.546833,7914.493831,478.797711,31.148501,0,240.628294,973.552803,7.527778,0,436.829664,0,277.197244,160.609413,0,47.706421,1003.192308,0,0,51.116781,526.528028,380.506044,53.275545,3627.863102,0,1.666667,114.728114,758.571128,253.655795,46.920456,545.638048,49.275722,134.878303,0,8.808369,0,82.26419,22.830159,0.541667,1187.408388,4301.339128,630.715092,66.851618,0.291667,3595,0,0,0.245401,0.245401,0.245401,0.245401,0.245401,0,840.773782,734,81.293363,0,0,0,529.080736,0,0,0,0.966667,347.114164,0,0,20.471223,89.149531,0,0,1.317406,0,12.212219,50.716505,0,4.587302,1323.066299,0,0,0,0,0,3552.863102,632,31.761254,36.466667,0,532.247752,1.566667,20.623626,0,0,554.076923,1.2,219.70267,0,0,273.986996,0,109,120.321552,0,0,0,5.060929,0,0,0,0,2.563228,21.145851,1.942857,0,0.733333,0.45,2.67197,11.14899,4.77316,1.797403,0,44.987657,0,0,0,0.333333,0,1.278571,9.370285,0.333333,0,0,1.067406,1.99395,0,112.23844,0,1.99395,1.99395,0,179.840476,58.393803,0,0,0,1.879545,369.942152,0,220.664286,0,0,0,0,0,3.177137,4.177137,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[3549.348644,1249.546833,7914.493831,478.797711,31.148501,0,240.628294,973.552803,7.527778,0,436.829664,0,277.197244,160.609413,0,47.706421,1003.192308,0,0,51.116781,526.528028,380.506044,53.275545,3627.863102,0,1.666667,114.728114,758.571128,253.655795,46.920456,545.638048,49.275722,134.878303,0,8.808369,0,82.26419,22.830159,0.541667,1187.408388,4301.339128,630.715092,66.851618,0.291667,3595,0,0,0.245401,0.245401,0.245401,0.245401,0.245401,0,840.773782,734,81.293363,0,0,0,529.080736,0,0,0,0.966667,347.114164,0,0,20.471223,89.149531,0,0,1.317406,0,12.212219,50.716505,0,4.587302,1323.066299,0,0,0,0,0,3552.863102,632,31.761254,36.466667,0,532.247752,1.566667,20.623626,0,0,554.076923,1.2,219.70267,0,0,273.986996,0,109,120.321552,0,0,0,5.060929,0,0,0,0,2.563228,21.145851,1.942857,0,0.733333,0.45,2.67197,11.14899,4.77316,1.797403,0,44.987657,0,0,0,0.333333,0,1.278571,9.370285,0.333333,0,0,1.067406,1.99395,0,112.23844,0,1.99395,1.99395,0,179.840476,58.393803,0,0,0,1.879545,369.942152,0,220.664286,0,0,0,0,0,3.177137,4.177137,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Betweenness\u003c/th>\n      <th>Betweenness Weighted\u003c/th>\n      <th>Centralization Betweenness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->
##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var2_with_CAPS_df_betweenness, by=list(var2_with_CAPS_df_betweenness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Betweenness(M)", "Betweenness Weighted(M)", "Centralization Betweenness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - median

```r
aggdata_median <-aggregate(. ~ Type, var2_with_CAPS_df_betweenness, function(x) c(median=median(x)))  

names(aggdata_median) <- c("Group","Betweenness(median)", "Betweenness Weighted(median)", "Centralization Betweenness(median)")

#Removing Type variable
#aggdata_median<-aggdata_median[,-c(2)]
```
##Merging mean and median

```r
total_table<- merge(aggdata_mean,aggdata_median,by="Group")

#Rounding
Group<-total_table[,c(1)] #Keeping group
total_table<-total_table[,-c(1)] %>% round(1) #Rouding
total_table<-cbind(Group,total_table) #Binding toghter

#Organizing Variabels
total_table<-total_table[c("Group","Betweenness(M)","Betweenness(median)","Betweenness Weighted(M)", "Betweenness Weighted(median)", "Centralization Betweenness(M)","Centralization Betweenness(median)")]
```
##Plotting final table with round

```r
datatable(total_table, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-73f4d8396b6b05616c31" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-73f4d8396b6b05616c31">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"7914.5\" data-scale=\"1\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"7914.5\" data-scale=\"1\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"7914.5\" data-scale=\"1\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"7914.5\" data-scale=\"1\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"7914.5\" data-scale=\"1\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"7914.5\" data-scale=\"1\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[1552.3,188.5,51.1,1249.5,240.6,529.1,429,7914.5,53.3,369.9,1323.1,151.5,395.7,205.8,344.8,475.7,0,0,3549.3,6,0,1.7,46],[220.7,0,51.1,1249.5,240.6,529.1,526.5,7914.5,53.3,369.9,1323.1,0.3,395.7,81.3,31.1,66.9,0,0,3549.3,0.2,0,0,1.3],[1552.3,188.5,51.1,1249.5,240.6,529.1,429,7914.5,53.3,369.9,1323.1,151.5,395.7,205.8,344.8,475.7,0,0,3549.3,6,0,1.7,46],[220.7,0,51.1,1249.5,240.6,529.1,526.5,7914.5,53.3,369.9,1323.1,0.3,395.7,81.3,31.1,66.9,0,0,3549.3,0.2,0,0,1.3],[1552.3,188.5,51.1,1249.5,240.6,529.1,429,7914.5,53.3,369.9,1323.1,151.5,395.7,205.8,344.8,475.7,0,0,3549.3,6,0,1.7,46],[220.7,0,51.1,1249.5,240.6,529.1,526.5,7914.5,53.3,369.9,1323.1,0.3,395.7,81.3,31.1,66.9,0,0,3549.3,0.2,0,0,1.3]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Betweenness(M)\u003c/th>\n      <th>Betweenness(median)\u003c/th>\n      <th>Betweenness Weighted(M)\u003c/th>\n      <th>Betweenness Weighted(median)\u003c/th>\n      <th>Centralization Betweenness(M)\u003c/th>\n      <th>Centralization Betweenness(median)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Betweenness Vertex Centrality Dinamic Table (Natureza Governamental)

```r
#Betweenness Vertex Centrality Measures Dinamic Table
#Getting  Measures
var2_with_CAPS_betweenness <- betweenness(var2_with_CAPS, weights=E(var2_with_CAPS)$equalone) %>% round(6)
var2_betweenness_w <- betweenness(var2_with_CAPS, weights=E(var2_with_CAPS)$var2) %>% round(6)
var2_centr_betweenness <- centralization.betweenness(var2_with_CAPS)$res %>% round(6)

#Creating a dataframe of measures
var2_with_CAPS_df_betweenness <- data.frame(var2_with_CAPS_betweenness,
var2_betweenness_w,
var2_centr_betweenness) %>% round(6)

#Adding type
var2_with_CAPS_df_betweenness <-cbind(var2_with_CAPS_df_betweenness, V(var2_with_CAPS)$TIPO1)

#Adding names
names(var2_with_CAPS_df_betweenness) <- c("Betweenness", "Betweenness Weighted", "Centralization Betweenness","Type")

#Ordering Variables
var2_with_CAPS_df_betweenness<-var2_with_CAPS_df_betweenness[c("Type","Betweenness", "Betweenness Weighted", "Centralization Betweenness")]
```
## General tabel for Betweenness

```r
datatable(var2_with_CAPS_df_betweenness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-b403bb6ebb030b6ba438" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-b403bb6ebb030b6ba438">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"7914.493831\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"7914.493831\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"7914.493831\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[3549.348644,1249.546833,7914.493831,478.797711,31.148501,0,240.628294,973.552803,7.527778,0,436.829664,0,277.197244,160.609413,0,47.706421,1003.192308,0,0,51.116781,526.528028,380.506044,53.275545,3627.863102,0,1.666667,114.728114,758.571128,253.655795,46.920456,545.638048,49.275722,134.878303,0,8.808369,0,82.26419,22.830159,0.541667,1187.408388,4301.339128,630.715092,66.851618,0.291667,3595,0,0,0.245401,0.245401,0.245401,0.245401,0.245401,0,840.773782,734,81.293363,0,0,0,529.080736,0,0,0,0.966667,347.114164,0,0,20.471223,89.149531,0,0,1.317406,0,12.212219,50.716505,0,4.587302,1323.066299,0,0,0,0,0,3552.863102,632,31.761254,36.466667,0,532.247752,1.566667,20.623626,0,0,554.076923,1.2,219.70267,0,0,273.986996,0,109,120.321552,0,0,0,5.060929,0,0,0,0,2.563228,21.145851,1.942857,0,0.733333,0.45,2.67197,11.14899,4.77316,1.797403,0,44.987657,0,0,0,0.333333,0,1.278571,9.370285,0.333333,0,0,1.067406,1.99395,0,112.23844,0,1.99395,1.99395,0,179.840476,58.393803,0,0,0,1.879545,369.942152,0,220.664286,0,0,0,0,0,3.177137,4.177137,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[3549.348644,1249.546833,7914.493831,478.797711,31.148501,0,240.628294,973.552803,7.527778,0,436.829664,0,277.197244,160.609413,0,47.706421,1003.192308,0,0,51.116781,526.528028,380.506044,53.275545,3627.863102,0,1.666667,114.728114,758.571128,253.655795,46.920456,545.638048,49.275722,134.878303,0,8.808369,0,82.26419,22.830159,0.541667,1187.408388,4301.339128,630.715092,66.851618,0.291667,3595,0,0,0.245401,0.245401,0.245401,0.245401,0.245401,0,840.773782,734,81.293363,0,0,0,529.080736,0,0,0,0.966667,347.114164,0,0,20.471223,89.149531,0,0,1.317406,0,12.212219,50.716505,0,4.587302,1323.066299,0,0,0,0,0,3552.863102,632,31.761254,36.466667,0,532.247752,1.566667,20.623626,0,0,554.076923,1.2,219.70267,0,0,273.986996,0,109,120.321552,0,0,0,5.060929,0,0,0,0,2.563228,21.145851,1.942857,0,0.733333,0.45,2.67197,11.14899,4.77316,1.797403,0,44.987657,0,0,0,0.333333,0,1.278571,9.370285,0.333333,0,0,1.067406,1.99395,0,112.23844,0,1.99395,1.99395,0,179.840476,58.393803,0,0,0,1.879545,369.942152,0,220.664286,0,0,0,0,0,3.177137,4.177137,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[3549.348644,1249.546833,7914.493831,478.797711,31.148501,0,240.628294,973.552803,7.527778,0,436.829664,0,277.197244,160.609413,0,47.706421,1003.192308,0,0,51.116781,526.528028,380.506044,53.275545,3627.863102,0,1.666667,114.728114,758.571128,253.655795,46.920456,545.638048,49.275722,134.878303,0,8.808369,0,82.26419,22.830159,0.541667,1187.408388,4301.339128,630.715092,66.851618,0.291667,3595,0,0,0.245401,0.245401,0.245401,0.245401,0.245401,0,840.773782,734,81.293363,0,0,0,529.080736,0,0,0,0.966667,347.114164,0,0,20.471223,89.149531,0,0,1.317406,0,12.212219,50.716505,0,4.587302,1323.066299,0,0,0,0,0,3552.863102,632,31.761254,36.466667,0,532.247752,1.566667,20.623626,0,0,554.076923,1.2,219.70267,0,0,273.986996,0,109,120.321552,0,0,0,5.060929,0,0,0,0,2.563228,21.145851,1.942857,0,0.733333,0.45,2.67197,11.14899,4.77316,1.797403,0,44.987657,0,0,0,0.333333,0,1.278571,9.370285,0.333333,0,0,1.067406,1.99395,0,112.23844,0,1.99395,1.99395,0,179.840476,58.393803,0,0,0,1.879545,369.942152,0,220.664286,0,0,0,0,0,3.177137,4.177137,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Betweenness\u003c/th>\n      <th>Betweenness Weighted\u003c/th>\n      <th>Centralization Betweenness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->
##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var2_with_CAPS_df_betweenness, by=list(var2_with_CAPS_df_betweenness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Betweenness(M)", "Betweenness Weighted(M)", "Centralization Betweenness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - median

```r
aggdata_median <-aggregate(. ~ Type, var2_with_CAPS_df_betweenness, function(x) c(median=median(x)))  

names(aggdata_median) <- c("Group","Betweenness(median)", "Betweenness Weighted(median)", "Centralization Betweenness(median)")

#Removing Type variable
#aggdata_median<-aggdata_median[,-c(2)]
```
##Merging mean and median

```r
total_table<- merge(aggdata_mean,aggdata_median,by="Group")

#Rounding
Group<-total_table[,c(1)] #Keeping group
total_table<-total_table[,-c(1)] %>% round(6) #Rouding
total_table<-cbind(Group,total_table) #Binding toghter

#Organizing Variabels
total_table<-total_table[c("Group","Betweenness(M)","Betweenness(median)","Betweenness Weighted(M)", "Betweenness Weighted(median)", "Centralization Betweenness(M)","Centralization Betweenness(median)")]
```
##Plotting final table with round

```r
datatable(total_table, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-d5b1e66a69405f3bee79" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-d5b1e66a69405f3bee79">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"221.097843\" data-max=\"245.66917\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1.133703\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"221.097843\" data-max=\"245.66917\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1.133703\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"221.097843\" data-max=\"245.66917\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1.133703\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[245.66917,221.097843],[1.133703,0],[245.66917,221.097843],[1.133703,0],[245.66917,221.097843],[1.133703,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Betweenness(M)\u003c/th>\n      <th>Betweenness(median)\u003c/th>\n      <th>Betweenness Weighted(M)\u003c/th>\n      <th>Betweenness Weighted(median)\u003c/th>\n      <th>Centralization Betweenness(M)\u003c/th>\n      <th>Centralization Betweenness(median)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Betweenness Vertex Centrality Dinamic Table (Setores)

```r
#Betweenness Vertex Centrality Measures Dinamic Table
#Getting  Measures
var2_with_CAPS_betweenness <- betweenness(var2_with_CAPS, weights=E(var2_with_CAPS)$equalone) %>% round(6)
var2_betweenness_w <- betweenness(var2_with_CAPS, weights=E(var2_with_CAPS)$var2) %>% round(1)
var2_centr_betweenness <- centralization.betweenness(var2_with_CAPS)$res %>% round(6)

#Creating a dataframe of measures
var2_with_CAPS_df_betweenness <- data.frame(var2_with_CAPS_betweenness,
var2_betweenness_w,
var2_centr_betweenness) %>% round(6)

#Adding type
var2_with_CAPS_df_betweenness <-cbind(var2_with_CAPS_df_betweenness, V(var2_with_CAPS)$TIPO2)

#Adding names
names(var2_with_CAPS_df_betweenness) <- c("Betweenness", "Betweenness Weighted", "Centralization Betweenness","Type")

#Ordering Variables
var2_with_CAPS_df_betweenness<-var2_with_CAPS_df_betweenness[c("Type","Betweenness", "Betweenness Weighted", "Centralization Betweenness")]
```
## General tabel for Betweenness

```r
datatable(var2_with_CAPS_df_betweenness, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-0cc6548dad460d6c9cf9" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-0cc6548dad460d6c9cf9">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"7914.493831\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"7914.5\" data-scale=\"1\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"7914.493831\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["ASS_HOS_ Hospital de Pronto Socorro – HPS","AMB_SAM_ Centro de Atenção à Saúde Mental (CASM)","CAPS_AD","CRAS_AS_ CRAS Sudeste Costa Carvalho","CRE_SOC_ CREAS Infância e Juventude","CRE_SOC_ CREAS Norte","ASS_HOS_ Serviço de Controle e Prevenção e Tratamento do Tabagismo (SECOPTT)","EA_DQCT_ Centro de Recuperação Resgatando Vidas (Escritório)","EA_DQCT_ Comunidade Terapêutica Geração de Adoradores – CTGA","EA_DQCT_ Centro de Recuperação Resgatando Vidas","CRAS_AS_ CRAS Norte Benfica","CRAS_AS_ CRAS Centro","EA_DQCT_ Associação Beneficente Cristã Restituir","ENT_SOC_ Território Aliança pela Vida  Zona Norte","UAP_URB_ Santa Cecília","RES_TER_ Casa V1","CRE_SOC_ CREAS Idoso e Mulher","URG_EME_ Serviço de Atendimento Móvel de Urgência (SAMU)","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) (Matriz)","ASS_HOS_ Centro de Referência em Álcool e Drogas (RADCAS)  Hospital Universitário","CAPS_CO_ CAPS HU","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI.1","CAPS_CO_ CAPS IJ","AJU_MUT_ Escritório de Serviços Locais dos Álcoólicos Anônimos de Juiz de Fora","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN) Dom Bosco","AJU_MUT_ ALANON Grupo Harmonia","AJU_MUT_ ALANON Grupo Só Por Hoje","CAPS_CO_ CAPS Leste","CRAS_AS_ CRAS Leste São Benedito","CRAS_AS_ CRAS Nordeste Grama","CRAS_AS_ CRAS Leste Linhares","UAP_URB_ Parque Guarani","ACO_INS_ Casa da Cidadania","AJU_MUT_ Grupo A.A. Vinte e Cinco de Abril","CRAS_AS_ CRAS Sudeste Olavo Costa","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI","EA_DQCT_ Centro de Recuperação Juiz de Fora Contra as Drogas","RES_TER_ Casa 1","RES_TER_ Casa 7","ENT_SOC_ Associação Casa Viva","ACO_INS_ Núcleo Cidadão de Rua Hebert de Souza","CON_RUA_ Equipe de Consultório na Rua (UAPS de referência Vila Ideal)","CON_RUA_ Equipe de Consultório na Rua (Associação Casa Viva)","EA_DQCT_ Associação Projeto Amor e Restauração – APAR","ENT_SOC_ Casa São Camilo de Lelis","CRA_SOC_ CRAS Sul Ipiranga","AJU_MUT_ Grupo A.A. Ipiranga","RES_TER_ Casa V2","RES_TER_ Casa V3","RES_TER_ Casa V4","RES_TER_ Casa V5","RES_TER_ Casa V6","EA_DQCT_ Comunidade Terapêutica Família em Cristo","UAP_URB_ São Pedro","AJU_MUT_ Grupo A.A Primeiro Passo","CRAS_AS_ CRAS Oeste São Pedro","UAP_URB_ Alto Grajaú","AJU_MUT_ Pastoral da Sobriedade","AJU_MUT_ Grupo A.A Juiz de Fora (Sala de abordagem)","ASS_HOS_ Hospital Ana Nery","AJU_MUT_ Grupo A.A. Central","EA_DQCT_ Vila Verde (Unidade Bromélias)","UAP_URB_ Santo Antônio","UAP_URB_ Grama","EA_DQCT_ Centro de Recuperação SOS Vida","EA_DQCT_ Centro de Tratamento Deville","AJU_MUT_ NARANON Grupo Parque Halfeld","ENT_SOC_ Fundação Maria Mãe","ENT_SOC_ Grupo Espírita de Ajuda aos Enfermos (GEDAE)","UAP_URB_ Cidade do Sol","UAP_URB_ Bairro Industrial","UAP_URB_ Milho Branco","UAP_URB_ Nova Era","UAP_URB_ Santa Cruz","UAP_URB_ São Judas Tadeu","UAP_URB_ Marumbí","UAP_URB_ Progresso","CRE_SOC_ CREAS População de Rua (CentroPop)","AJU_MUT_ Grupo de Apoio Benfica de Amor Exigente (GABENAE)","AJU_MUT_ Grupo de Apoio São Mateus de Amor Exigente (GASMAE)","AJU_MUT_ Grupo de Amor Exigente Linhares","AJU_MUT_ Grupo de Amor Exigente Santa Terezinha","UAP_URB_ Furtado de Menezes","AJU_MUT_ Grupo A.A. Reunidos","AJU_MUT_ Grupo de Apoio São Pedro do Amor Exigente (GASPAE)","UAP_URB_ Jóquei Clube II","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT","EA_DQCT_ Centro Terapêutico Reconstruir (fazenda)","UAP_URB_ Bairro de Lourdes","UAP_URB_ Vila Olavo Costa","UAP_URB_ Cruzeiro do Sul","AJU_MUT_ Igreja Batista Resplandecente Estrela do Amanhã  IBREM","EA_DQCT_ Vila Verde (Unidade Borboleta)","ASS_HOS_ Clínica Vila Verde (Hospital Dia)","UAP_URB_ Ipiranga","EA_DQCT_ Centro Metodista de Assistência aos Toxicômanos – CEMAT (Escritório)","EA_DQCT_ Centro Terapêutico Reconstruir (escritório)","EA_DQCT_ Grupo de Apoio à Família e aos Dependentes Químicos – GAFADEQUI (Administração, triagem e apoio aos familiares)","UAP_URB_ Barreira do Triunfo","ASS_HOS_ Casa de Saúde Esperança","ENT_SOC_ Associação Beneficente e Cultural Amigos do Noivo (ABAN)","ENT_SOC_ Casa de Acolhimento à Infância e Adolescente (CAIA)","RES_TER_ Casa 3","RES_TER_ Casa 4","UAP_URB_ Filgueiras","UAP_URB_ Jardim da Lua","UAP_URB_ Granjas Betânea","RES_TER_ Casa 5","RES_TER_ Casa 6","UAP_URB_ Borboleta","UAP_URB_ Santos Dumont","UAP_RUR_ Igrejinha","UAP_URB_ Dom Bosco","UAP_URB_ Benfica","UAP_URB_ Jóquei Clube I","UAP_URB_ Nossa Senhora Aparecida","UAP_URB_ Santa Cândida/São Sebastião","UAP_URB_ São Benedito","UAP_URB_ Santa Efigênia","UAP_URB_ Santa Luzia","UAP_URB_ Bandeirantes","ENT_SOC_ Instituto Veredas (Projeto, Assessoria e Prática em Saúde Mental) // Serviço de Referência para Adolescentes do Instituto Veredas","UAP_RUR_ Monte Verde","UAP_RUR_ Pirapetinga","UAP_RUR_ Torreões","UAP_RUR_ Caeté","UAP_RUR_ Chapéu D'Uvas","UAP_RUR_ Humaitá","UAP_RUR_ Rosário de Minas","UAP_RUR_ Sarandira","UAP_RUR_ Toledos","UAP_RUR_ Valadares","UAP_URB_ Esplanada","UAP_URB_ Jardim Natal","UAP_URB_ Monte Castelo","UAP_URB_ Vila Esperança","UAP_URB_ Linhares","UAP_URB_ Santa Rita","UAP_URB_ Jardim Esperança","UAP_URB_ Retiro","UAP_URB_ Vila Ideal","UAP_URB_ Teixeiras","UAP_URB_ Vale Verde","UAP_URB_ Nossa Senhora das Graças","ENT_SOC_ ONG Saída","CAPS_CO_ CAPS Casa Viva","CEN_CON_ Associação TRABALHARTE","UAP_URB_ Centro Sul","ACO_INS_ Sociedade São Vicente de Paulo","UAP_RUR_ Buiéié","UAP_RUR_ Jacutinga","UAP_RUR_ Palmital","UAP_RUR_ Pires","UAP_RUR_ Privilégio","AJU_MUT_ Grupo N.A. Glória","AJU_MUT_ Grupo A.A. 29 de Junho","UAP_RUR_ Dias Tavares","UAP_RUR_ Paula Lima","UAP_RUR_ Penido","AJU_MUT_ ALANON Grupo Libertação","AJU_MUT_ GEVE","AJU_MUT_ Grupo A.A. Libertação","AJU_MUT_ Grupo N.A. Libertação","AJU_MUT_ NARANON Grupo Glória","AJU_MUT_ Grupo A.A. Azul e Branco","AJU_MUT_ NARANON Grupo Renascer","AJU_MUT_ Grupo A.A. Cidade do Sol","AJU_MUT_ Grupo A.A. Caminho Da Salvação","AJU_MUT_ Grupo A.A. Dois de Março","AJU_MUT_ Grupo A.A. Corrente da Sobriedade","AJU_MUT_ Grupo A.A. Milho Branco","AJU_MUT_ Grupo A.A. Luz Divina","AJU_MUT_ Grupo A.A. Nova Era","AJU_MUT_ Grupo A.A União","AJU_MUT_ Grupo A.A. Bairu","AJU_MUT_ Grupo N.A. Caminho Verdade","AJU_MUT_ Grupo A.A. Bonfim","AJU_MUT_ Grupo A.A. Linhares","AJU_MUT_ Grupo A.A Redenção Abolição","AJU_MUT_ Grupo A.A. Progresso","AJU_MUT_ Grupo A.A. Estrela D´Alva","AJU_MUT_ Grupo A.A. Primeira Tradição","AJU_MUT_ Grupo A.A. Estrela do Oriente","AJU_MUT_ Grupo A.A Duas Vidas","AJU_MUT_ Grupo A.A. Liberdade","AJU_MUT_ Grupo N.A Rendição","ASS_HOS_ Hospital de Toxicômanos"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[3549.348644,1249.546833,7914.493831,478.797711,31.148501,0,240.628294,973.552803,7.527778,0,436.829664,0,277.197244,160.609413,0,47.706421,1003.192308,0,0,51.116781,526.528028,380.506044,53.275545,3627.863102,0,1.666667,114.728114,758.571128,253.655795,46.920456,545.638048,49.275722,134.878303,0,8.808369,0,82.26419,22.830159,0.541667,1187.408388,4301.339128,630.715092,66.851618,0.291667,3595,0,0,0.245401,0.245401,0.245401,0.245401,0.245401,0,840.773782,734,81.293363,0,0,0,529.080736,0,0,0,0.966667,347.114164,0,0,20.471223,89.149531,0,0,1.317406,0,12.212219,50.716505,0,4.587302,1323.066299,0,0,0,0,0,3552.863102,632,31.761254,36.466667,0,532.247752,1.566667,20.623626,0,0,554.076923,1.2,219.70267,0,0,273.986996,0,109,120.321552,0,0,0,5.060929,0,0,0,0,2.563228,21.145851,1.942857,0,0.733333,0.45,2.67197,11.14899,4.77316,1.797403,0,44.987657,0,0,0,0.333333,0,1.278571,9.370285,0.333333,0,0,1.067406,1.99395,0,112.23844,0,1.99395,1.99395,0,179.840476,58.393803,0,0,0,1.879545,369.942152,0,220.664286,0,0,0,0,0,3.177137,4.177137,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[3549.3,1249.5,7914.5,478.8,31.1,0,240.6,973.6,7.5,0,436.8,0,277.2,160.6,0,47.7,1003.2,0,0,51.1,526.5,380.5,53.3,3627.9,0,1.7,114.7,758.6,253.7,46.9,545.6,49.3,134.9,0,8.8,0,82.3,22.8,0.5,1187.4,4301.3,630.7,66.9,0.3,3595,0,0,0.2,0.2,0.2,0.2,0.2,0,840.8,734,81.3,0,0,0,529.1,0,0,0,1,347.1,0,0,20.5,89.1,0,0,1.3,0,12.2,50.7,0,4.6,1323.1,0,0,0,0,0,3552.9,632,31.8,36.5,0,532.2,1.6,20.6,0,0,554.1,1.2,219.7,0,0,274,0,109,120.3,0,0,0,5.1,0,0,0,0,2.6,21.1,1.9,0,0.7,0.4,2.7,11.1,4.8,1.8,0,45,0,0,0,0.3,0,1.3,9.4,0.3,0,0,1.1,2,0,112.2,0,2,2,0,179.8,58.4,0,0,0,1.9,369.9,0,220.7,0,0,0,0,0,3.2,4.2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[3549.348644,1249.546833,7914.493831,478.797711,31.148501,0,240.628294,973.552803,7.527778,0,436.829664,0,277.197244,160.609413,0,47.706421,1003.192308,0,0,51.116781,526.528028,380.506044,53.275545,3627.863102,0,1.666667,114.728114,758.571128,253.655795,46.920456,545.638048,49.275722,134.878303,0,8.808369,0,82.26419,22.830159,0.541667,1187.408388,4301.339128,630.715092,66.851618,0.291667,3595,0,0,0.245401,0.245401,0.245401,0.245401,0.245401,0,840.773782,734,81.293363,0,0,0,529.080736,0,0,0,0.966667,347.114164,0,0,20.471223,89.149531,0,0,1.317406,0,12.212219,50.716505,0,4.587302,1323.066299,0,0,0,0,0,3552.863102,632,31.761254,36.466667,0,532.247752,1.566667,20.623626,0,0,554.076923,1.2,219.70267,0,0,273.986996,0,109,120.321552,0,0,0,5.060929,0,0,0,0,2.563228,21.145851,1.942857,0,0.733333,0.45,2.67197,11.14899,4.77316,1.797403,0,44.987657,0,0,0,0.333333,0,1.278571,9.370285,0.333333,0,0,1.067406,1.99395,0,112.23844,0,1.99395,1.99395,0,179.840476,58.393803,0,0,0,1.879545,369.942152,0,220.664286,0,0,0,0,0,3.177137,4.177137,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Betweenness\u003c/th>\n      <th>Betweenness Weighted\u003c/th>\n      <th>Centralization Betweenness\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var2_with_CAPS_df_betweenness, by=list(var2_with_CAPS_df_betweenness$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Betweenness(M)", "Betweenness Weighted(M)", "Centralization Betweenness(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - median

```r
aggdata_median <-aggregate(. ~ Type, var2_with_CAPS_df_betweenness, function(x) c(median=median(x)))  

names(aggdata_median) <- c("Group","Betweenness(median)", "Betweenness Weighted(median)", "Centralization Betweenness(median)")

#Removing Type variable
#aggdata_median<-aggdata_median[,-c(2)]
```
##Merging mean and median

```r
total_table<- merge(aggdata_mean,aggdata_median,by="Group")

#Rounding
Group<-total_table[,c(1)] #Keeping group
total_table<-total_table[,-c(1)] %>% round(6) #Rouding
total_table<-cbind(Group,total_table) #Binding toghter

#Organizing Variabels
total_table<-total_table[c("Group","Betweenness(M)","Betweenness(median)","Betweenness Weighted(M)", "Betweenness Weighted(median)", "Centralization Betweenness(M)","Centralization Betweenness(median)")]
```
##Plotting final table with round

```r
datatable(total_table, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-f8167f656351ae4863fd" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-f8167f656351ae4863fd">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"193.508436\" data-max=\"576.371196\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"134.878303\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"193.503191\" data-max=\"576.366667\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"134.9\" data-scale=\"2\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"193.508436\" data-max=\"576.371196\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"134.878303\" data-scale=\"6\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[576.371196,193.508436,220.046655],[134.878303,0.391666,0],[576.366667,193.503191,220.05],[134.9,0.35,0],[576.371196,193.508436,220.046655],[134.878303,0.391666,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Betweenness(M)\u003c/th>\n      <th>Betweenness(median)\u003c/th>\n      <th>Betweenness Weighted(M)\u003c/th>\n      <th>Betweenness Weighted(median)\u003c/th>\n      <th>Centralization Betweenness(M)\u003c/th>\n      <th>Centralization Betweenness(median)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Plotting Betweenness Centrality - (Vertex)

```r
set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var2_with_CAPS, es=E(var2_with_CAPS), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2_with_CAPS))
maxC <- rep(Inf, vcount(var2_with_CAPS))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2_with_CAPS, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2_with_CAPS)$weight)

#Plotting
plot(var2_with_CAPS, 
     layout=co,
     edge.color=V(var2_with_CAPS)$color[edge.start],
     edge.arrow.size=(betweenness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2)+1)/100000,
     edge.width=E(var2_with_CAPS)$weight/10*mean(E(var2_with_CAPS)$weight),
     edge.curved = TRUE,
     vertex.color=V(var2_with_CAPS)$color,
     vertex.size=betweenness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2 )/150,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2_with_CAPS,"LABEL_COR"),
     vertex.label.cex=(betweenness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2)+1)/10000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var2_with_CAPS)$LABEL_COR
b<-V(var2_with_CAPS)$color
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
    title("Network Vertex Betweenness Sized - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels =    sprintf("Median Betweenness: %.2f\nSD Betweenness: %.2f",
     median(betweenness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2)), 
     sd(betweenness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2))
             )
       )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-144-1.png)<!-- -->

##Network Plotting Based On Centralization Betweenness

```r
#Get Variable
V(var2_with_CAPS)$var2_centr_betweenness<-(centralization.betweenness(var2_with_CAPS)$res)/100
V(var2_with_CAPS)$var2_color_degree<-round(V(var2_with_CAPS)$var2_centr_betweenness,6)

#Creating brewer pallette
var2_vertex_color_degree<-
  colorRampPalette(brewer.pal(length(unique(
          V(var2_with_CAPS)$var2_color_degree)), "Spectral"))(
            length(unique(V(var2_with_CAPS)$var2_color_degree)))

#Saving as Vertex properties 
V(var2_with_CAPS)$var2_vertex_color_degree<-
  var2_vertex_color_degree[as.numeric(
  cut(V(var2_with_CAPS)$var2_color_degree,
      breaks=length(unique(V(var2_with_CAPS)$var2_color_degree))))]

set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var2_with_CAPS, es=E(var2_with_CAPS), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2_with_CAPS))
maxC <- rep(Inf, vcount(var2_with_CAPS))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2_with_CAPS, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights = E(var2_with_CAPS)$weight)


#PLotting
plot(var2_with_CAPS, 
     layout=co,
     edge.color=V(var2_with_CAPS)$var2_vertex_color_degree[edge.start],
     edge.arrow.size=closeness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2, mode="out"),
     edge.width=E(var2_with_CAPS)$weight/10*mean(E(var2_with_CAPS)$weight),
     edge.curved = TRUE,
     vertex.color=V(var2_with_CAPS)$var2_vertex_color_degree,
     vertex.size=(centralization.betweenness(var2_with_CAPS)$res+1)/100,
     vertex.frame.color="white",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2_with_CAPS,"LABEL_COR"),
     vertex.label.cex=(centralization.betweenness(var2_with_CAPS)$res + 1)/10000,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var2_with_CAPS)$var2_color_degree
b<-V(var2_with_CAPS)$var2_vertex_color_degree
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
  title("Network Centralization Betweenness - 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", sub = "Source: from authors ")
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels = sprintf("Median Betweenness:%.0f\nSDBetweenness: %.0f",
     median(betweenness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2)), 
     sd(betweenness(var2_with_CAPS, weights = E(var2_with_CAPS)$var2))
             )
       )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-145-1.png)<!-- -->

#Reciprocity Default
Reciprocity Default - Proportion of mutual connections - probability that hte opposite counterpart of a directed graph is also included

```r
reciprocity(var2_with_CAPS, mode="default")
```

```
## [1] 0.3040685
```

#Reciprocity Ratio
Reciprocity Ratio - Probability  of mutual connections between a vertex pair - if we know - probability that hte opposite counterpart of a directed graph is also included in the 

```r
reciprocity(var2_with_CAPS, mode="ratio")
```

```
## [1] 0.1792929
```

#Dyad Census
A dyad consists of an unordered pair of actors and links that exist between two actors of the pair classified by mutal non-mutual and no connection in a directed graphs

Dyads are 2-subgraphs where a subgraph is a subset of actors taken from the complete set of network actors and all links
between them. See more here <http://file.scirp.org/pdf/SN_2013012915270187.pdf>

##Dyad Census 
Number of pairs with mutual connections "mut" and number of pairs with non-mutual connections "asym"

```r
dyad.census_var2_with_CAPS<-dyad.census(var2_with_CAPS)
```
##Mutual connections.

```r
dyad.census_var2_with_CAPS_mut<-dyad.census_var2_with_CAPS$mut
dyad.census_var2_with_CAPS_mut
```

```
## [1] 142
```
##Non-mutual connections.

```r
dyad.census_var2_with_CAPS_asym<-dyad.census_var2_with_CAPS$asym
dyad.census_var2_with_CAPS_asym
```

```
## [1] 650
```
##No connection between them.

```r
dyad.census_var2_with_CAPS_null<-dyad.census_var2_with_CAPS$null
dyad.census_var2_with_CAPS_null
```

```
## [1] 16599
```
#Triad Census - Check this out in order to understand triad lables

The studies about transitivity in social networks led Holland and Leinhardt (1975) to propose that the local structure in social networks can be expressed by the triad census or triad count, the numbers of triads of any kinds.

You can see more here:
<http://www.stats.ox.ac.uk/~snijders/Trans_Triads_ha.pdf>


```r
#Triad Census 
tc_var2_with_CAPS <- triad.census(var2_with_CAPS)

#Triad Census Label 
census_labels = c('T.003',
                  'T.012',
                  'T.102',
                  'T.021D',
                  'T.021U',
                  'T.021C',
                  'T.111D',
                  'T.111U',
                  'T.030T',
                  'T.030C',
                  'T.201',
                  'T.120D',
                  'T.120U',
                  'T.120C',
                  'T.210',
                  'T.300')

ordering = c('1',
                  '2',
                  '3',
                  '4',
                  '5',
                  '6',
                  '7',
                  '8',
                  '13',
                  '10',
                  '9',
                  '14',
                  '15',
                  '11',
                  '12',
                  '16')

#Saving in a dataframe for further studies
triad_df_var2_with_CAPS <- data.frame(census_labels,tc_var2_with_CAPS)
write.csv(triad_df_var2_with_CAPS, "~/SNArRDJF/Banco Redes R/var2_with_CAPS_complet_triads.csv")
```
##Triad Census Types 
The following labels gives the 16 different triads for directed graphs. The coding refers to the numbers of mutual, asymmetric, and null dyads, with a further identifying letter: Up, Down, Cyclical, Transitive.

E.g., 1-2-0-D has *1* mutual, *2* asymmetric, *0* null dyads, and the *Down* orientation.

###Describing triads


```r
triad_df_var2_with_CAPS
```

```
##    census_labels tc_var2_with_CAPS
## 1          T.003            947034
## 2          T.012             87794
## 3          T.102             17904
## 4         T.021D              8264
## 5         T.021U              1198
## 6         T.021C              2731
## 7         T.111D              1019
## 8         T.111U              4197
## 9         T.030T               428
## 10        T.030C                18
## 11         T.201               908
## 12        T.120D               102
## 13        T.120U               424
## 14        T.120C               104
## 15         T.210               256
## 16         T.300                64
```

###Triads Tables Recoding

```r
#Recoding different types of triads 
triad_df_var2_with_CAPS$type[triad_df_var2_with_CAPS$census_labels=="T.003"]<-"Vacuously Transitive"
triad_df_var2_with_CAPS$type[triad_df_var2_with_CAPS$census_labels=="T.012"]<-"Vacuously Transitive"
triad_df_var2_with_CAPS$type[triad_df_var2_with_CAPS$census_labels=="T.102"]<-"Vacuously Transitive"
triad_df_var2_with_CAPS$type[triad_df_var2_with_CAPS$census_labels=="T.021D"]<-"Vacuously Transitive"
triad_df_var2_with_CAPS$type[triad_df_var2_with_CAPS$census_labels=="T.021U"]<-"Vacuously Transitive"
triad_df_var2_with_CAPS$type[triad_df_var2_with_CAPS$census_labels=="T.021C"]<-"Intransitive"
triad_df_var2_with_CAPS$type[triad_df_var2_with_CAPS$census_labels=="T.111D"]<-"Intransitive"
triad_df_var2_with_CAPS$type[triad_df_var2_with_CAPS$census_labels=="T.111U"]<-"Intransitive"
triad_df_var2_with_CAPS$type[triad_df_var2_with_CAPS$census_labels=="T.030T"]<-"Transitive"
triad_df_var2_with_CAPS$type[triad_df_var2_with_CAPS$census_labels=="T.030C"]<-"Intransitive"
triad_df_var2_with_CAPS$type[triad_df_var2_with_CAPS$census_labels=="T.201"]<-"Intransitive"
triad_df_var2_with_CAPS$type[triad_df_var2_with_CAPS$census_labels=="T.120D"]<-"Transitive"
triad_df_var2_with_CAPS$type[triad_df_var2_with_CAPS$census_labels=="T.120U"]<-"Transitive"
triad_df_var2_with_CAPS$type[triad_df_var2_with_CAPS$census_labels=="T.120C"]<-"Intransitive"
triad_df_var2_with_CAPS$type[triad_df_var2_with_CAPS$census_labels=="T.210"]<-"Intransitive"
triad_df_var2_with_CAPS$type[triad_df_var2_with_CAPS$census_labels=="T.300"]<-"Transitive"
```

Graphical Ilustration 

![<http://knoesis.wright.edu/sites/default/files/icnc15.pdf>](figures/img.png)

###Triads Tables

```r
datatable(triad_df_var2_with_CAPS)
```

<!--html_preserve--><div id="htmlwidget-ca4fd138802a1ee517dd" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-ca4fd138802a1ee517dd">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"],["T.003","T.012","T.102","T.021D","T.021U","T.021C","T.111D","T.111U","T.030T","T.030C","T.201","T.120D","T.120U","T.120C","T.210","T.300"],[947034,87794,17904,8264,1198,2731,1019,4197,428,18,908,102,424,104,256,64],["Vacuously Transitive","Vacuously Transitive","Vacuously Transitive","Vacuously Transitive","Vacuously Transitive","Intransitive","Intransitive","Intransitive","Transitive","Intransitive","Intransitive","Transitive","Transitive","Intransitive","Intransitive","Transitive"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>census_labels\u003c/th>\n      <th>tc_var2_with_CAPS\u003c/th>\n      <th>type\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":2},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Transitivity - clustering coefficient

##Transitivity Global
Socialnetwork is treated as an undirected network global - ratio of triangles (direction disregarded) to connected triples.


```r
transitivity(var2_with_CAPS, type="global")
```

```
## [1] 0.186092
```

# Transitivity Local
Ratio of triangles to connected triples each vertex is part of.

```r
V(var2_with_CAPS)$var2_transitivity_local<-transitivity(var2_with_CAPS, type="local") 
var2_with_CAPS_transitivity_local<-transitivity(var2_with_CAPS, type="local") #local - ratio of triangles to connected triples each vertex is part of.
```
#Descriptive Statistics for Local Transitivity by Vertex 

```r
summary(var2_with_CAPS_transitivity_local[which(var2_with_CAPS_transitivity_local != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0000  0.2606  0.4000  0.5078  0.7667  1.0000
```

```r
sd(var2_with_CAPS_transitivity_local[which(var2_with_CAPS_transitivity_local != Inf)])
```

```
## [1] 0.3222097
```

#Barrat's Weighted Transitivity by Edges (Barrat's)

```r
V(var2_with_CAPS)$var2_transitivity_barrat<-transitivity(var2_with_CAPS, weights=E(var2_with_CAPS)$weight, type="barrat")

var2_with_CAPS_transitivity_barrat<-transitivity(var2_with_CAPS, weights=E(var2_with_CAPS)$weight, type="barrat")
```

#Descriptive Statistics for Barrat Weighted Transitivity by Vertex 

```r
summary(var2_with_CAPS_transitivity_barrat[which(var2_with_CAPS_transitivity_barrat != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0000  0.4509  0.8000  0.7617  1.0000  2.0000
```

```r
sd(var2_with_CAPS_transitivity_barrat[which(var2_with_CAPS_transitivity_barrat != Inf)])
```

```
## [1] 0.4052682
```

#Transitivity  Measures Dinamic Table

```r
#Getting  Measures

var2_with_CAPS_transitivity_local<-transitivity(var2_with_CAPS, type="local") %>% round(3)

var2_with_CAPS_transitivity_barrat<-transitivity(var2_with_CAPS, weights=E(var2_with_CAPS)$weight, type="barrat") %>% round(3)

#Creating a datagrame of measures
var2_with_CAPS_transitivity_df <- data.frame(var2_with_CAPS_transitivity_local,var2_with_CAPS_transitivity_barrat) %>% round(3)
```

#Transitivity  Measures Dinamic Table

```r
var2_with_CAPS_transitivity_df <-cbind(var2_with_CAPS_transitivity_df, V(var2_with_CAPS)$LABEL_COR)

#Adding names
names(var2_with_CAPS_transitivity_df) <- c("Local", "Barrat's Weighted","Type")

#Ordering Variables
var2_with_CAPS_transitivity_df<-var2_with_CAPS_transitivity_df[c("Type", "Local", "Barrat's Weighted")]
```
## General tabel - DT 

```r
datatable(var2_with_CAPS_transitivity_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-4606c95b06be076f417f" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-4606c95b06be076f417f">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[0.058,0.091,0.045,0.221,0.438,0.4,0.062,0.146,0.467,0.167,0.17,0.615,0.19,0.152,0.667,0.4,0.113,0.346,null,0.389,0.2,0.25,0.3,0.051,null,0.167,0.333,0.145,0.23,0.336,0.433,0.306,0.373,1,0.606,0.5,0.476,0,0.6,0.154,0.2,0.282,0.345,0.5,0.429,0.326,1,0.306,0.306,0.306,0.306,0.306,null,0.286,0.046,0.492,0.429,null,1,0.229,0.167,0.167,0.467,0.464,0.2,null,0.667,0.556,0,0.667,1,0.429,0.4,0.4,0.255,0.476,0.5,0.25,0.214,0.214,0.214,0.214,0.7,0.167,0.091,0.333,0.333,1,0.17,0.7,0.357,null,0.167,0.095,0.4,0.467,1,1,0.6,0.952,0,0.316,null,null,1,0.364,0.667,null,0,0.679,0.528,0.306,0.429,0.4,0.7,0.467,0.393,0.273,0.333,0.303,1,0.392,0.667,0.667,0.667,0.9,0.5,0.4,0.267,0.833,1,0.667,0.3,0.4,0.667,0.444,0.4,0.4,0.333,1,0.361,0.467,0.6,0.467,null,0.234,0.143,0.833,0.2,1,1,1,1,1,0,0.333,1,1,1,null,null,0.667,null,1,1,1,1,1,1,1,1,1,1,1,1,null,1,1,1,1,1,1,0.333,1,1,null,null],[0.095,0.139,0.084,0.419,0.804,0.511,0.112,0.221,0.6,0.667,0.277,0.912,0.248,0.303,1.333,0.578,0.211,0.538,null,0.5,0.362,0.339,0.587,0.053,null,0.167,0.333,0.269,0.467,0.581,0.769,0.694,0.605,1,0.924,0.65,0.667,0,0.8,0.226,0.359,0.545,0.609,0.7,0.524,0.505,1,0.403,0.403,0.403,0.403,0.403,null,0.476,0.048,0.821,0.786,null,1,0.408,0.167,0.667,0.867,1.071,0.3,null,0.667,0.889,0,1.333,1.667,0.905,0.6,0.633,0.545,1.143,0.75,0.444,0.857,0.857,0.857,0.857,1.1,0.25,0.364,0.733,0.333,1,0.301,1.1,0.75,null,0.667,0.238,0.822,0.6,1,1.333,0.8,1.333,0,0.502,null,null,1.7,0.803,1.333,null,0,1.036,0.903,0.458,0.833,0.85,0.9,1.033,1,0.736,0.722,0.485,1.7,0.585,1.167,1.167,1.167,1.4,1.167,0.833,0.533,1,1.667,1.167,0.85,0.8,1.167,0.736,1.1,0.933,0.857,2,0.639,1.033,1.15,1.133,null,0.385,0.286,1.333,0.367,1,1.667,1,1.667,1,0,0.333,1,1.667,1.667,null,null,0.667,null,1,1,1,1,1,1,1,1,1,1,1,1,null,1,1,1,1,1,1,0.333,1,1,null,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Local\u003c/th>\n      <th>Barrat's Weighted\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var2_with_CAPS_transitivity_df, by=list(var2_with_CAPS_transitivity_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Local(M)", "Barrat's Weighted(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - median

```r
aggdata_median <-aggregate(. ~ Type, var2_with_CAPS_transitivity_df, function(x) c(median=median(x))) 

names(aggdata_median) <- c("Group","Local(median)", "Barrat's Weighted(median)")

#Removing Type variable
#aggdata_median<-aggdata_median[,-c(2)]
```
##Merging mean and median

```r
total_table <- merge(aggdata_mean,aggdata_median,by="Group")

#Rounding
Group<-total_table[,c(1)] #Keeping group
total_table<-total_table[,-c(1)] %>% round(3) #Rouding
total_table<-cbind(Group,total_table) #Binding toghter

#Organizing Variabels
total_table<-total_table[c("Group","Local(M)","Local(median)", "Barrat's Weighted(M)","Barrat's Weighted(median)")]
```
##Final table with round - Transitivity

```r
datatable(total_table, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-a7152e1acfa0a1646e59" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-a7152e1acfa0a1646e59">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.045\" data-max=\"0.952\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.045\" data-max=\"1\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.084\" data-max=\"1.333\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.084\" data-max=\"1.333\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[0.258,0.689,0.389,0.091,0.062,0.229,0.193,0.045,0.3,0.143,0.25,0.419,0.217,0.381,0.317,0.274,0.952,0.058,0.281,0.346,0.783,0.504],[0.2,1,0.389,0.091,0.062,0.229,0.2,0.045,0.3,0.143,0.25,0.333,0.217,0.336,0.4,0.33,0.952,0.058,0.306,0.346,0.9,0.436],[0.444,0.764,0.5,0.139,0.112,0.408,0.339,0.084,0.587,0.286,0.444,0.602,0.424,0.631,0.509,0.417,1.333,0.095,0.377,0.538,1.179,0.962],[0.367,1,0.5,0.139,0.112,0.408,0.362,0.084,0.587,0.286,0.444,0.65,0.424,0.581,0.511,0.513,1.333,0.095,0.403,0.538,1.167,0.884]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Local(M)\u003c/th>\n      <th>Local(median)\u003c/th>\n      <th>Barrat's Weighted(M)\u003c/th>\n      <th>Barrat's Weighted(median)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Transitivity  Measures Dinamic Table

```r
#Getting  Measures

var2_with_CAPS_transitivity_local<-transitivity(var2_with_CAPS, type="local") %>% round(3)

var2_with_CAPS_transitivity_barrat<-transitivity(var2_with_CAPS, weights=E(var2_with_CAPS)$weight, type="barrat") %>% round(3)

#Creating a datagrame of measures
var2_with_CAPS_transitivity_df <- data.frame(var2_with_CAPS_transitivity_local,var2_with_CAPS_transitivity_barrat) %>% round(3)
```

#Transitivity  Measures Dinamic Table

```r
var2_with_CAPS_transitivity_df <-cbind(var2_with_CAPS_transitivity_df, V(var2_with_CAPS)$TIPO1)

#Adding names
names(var2_with_CAPS_transitivity_df) <- c("Local", "Barrat's Weighted","Type")

#Ordering Variables
var2_with_CAPS_transitivity_df<-var2_with_CAPS_transitivity_df[c("Type", "Local", "Barrat's Weighted")]
```
## General tabel - DT 

```r
datatable(var2_with_CAPS_transitivity_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-cbe3a6975313972dbd4b" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-cbe3a6975313972dbd4b">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[0.058,0.091,0.045,0.221,0.438,0.4,0.062,0.146,0.467,0.167,0.17,0.615,0.19,0.152,0.667,0.4,0.113,0.346,null,0.389,0.2,0.25,0.3,0.051,null,0.167,0.333,0.145,0.23,0.336,0.433,0.306,0.373,1,0.606,0.5,0.476,0,0.6,0.154,0.2,0.282,0.345,0.5,0.429,0.326,1,0.306,0.306,0.306,0.306,0.306,null,0.286,0.046,0.492,0.429,null,1,0.229,0.167,0.167,0.467,0.464,0.2,null,0.667,0.556,0,0.667,1,0.429,0.4,0.4,0.255,0.476,0.5,0.25,0.214,0.214,0.214,0.214,0.7,0.167,0.091,0.333,0.333,1,0.17,0.7,0.357,null,0.167,0.095,0.4,0.467,1,1,0.6,0.952,0,0.316,null,null,1,0.364,0.667,null,0,0.679,0.528,0.306,0.429,0.4,0.7,0.467,0.393,0.273,0.333,0.303,1,0.392,0.667,0.667,0.667,0.9,0.5,0.4,0.267,0.833,1,0.667,0.3,0.4,0.667,0.444,0.4,0.4,0.333,1,0.361,0.467,0.6,0.467,null,0.234,0.143,0.833,0.2,1,1,1,1,1,0,0.333,1,1,1,null,null,0.667,null,1,1,1,1,1,1,1,1,1,1,1,1,null,1,1,1,1,1,1,0.333,1,1,null,null],[0.095,0.139,0.084,0.419,0.804,0.511,0.112,0.221,0.6,0.667,0.277,0.912,0.248,0.303,1.333,0.578,0.211,0.538,null,0.5,0.362,0.339,0.587,0.053,null,0.167,0.333,0.269,0.467,0.581,0.769,0.694,0.605,1,0.924,0.65,0.667,0,0.8,0.226,0.359,0.545,0.609,0.7,0.524,0.505,1,0.403,0.403,0.403,0.403,0.403,null,0.476,0.048,0.821,0.786,null,1,0.408,0.167,0.667,0.867,1.071,0.3,null,0.667,0.889,0,1.333,1.667,0.905,0.6,0.633,0.545,1.143,0.75,0.444,0.857,0.857,0.857,0.857,1.1,0.25,0.364,0.733,0.333,1,0.301,1.1,0.75,null,0.667,0.238,0.822,0.6,1,1.333,0.8,1.333,0,0.502,null,null,1.7,0.803,1.333,null,0,1.036,0.903,0.458,0.833,0.85,0.9,1.033,1,0.736,0.722,0.485,1.7,0.585,1.167,1.167,1.167,1.4,1.167,0.833,0.533,1,1.667,1.167,0.85,0.8,1.167,0.736,1.1,0.933,0.857,2,0.639,1.033,1.15,1.133,null,0.385,0.286,1.333,0.367,1,1.667,1,1.667,1,0,0.333,1,1.667,1.667,null,null,0.667,null,1,1,1,1,1,1,1,1,1,1,1,1,null,1,1,1,1,1,1,0.333,1,1,null,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Local\u003c/th>\n      <th>Barrat's Weighted\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var2_with_CAPS_transitivity_df, by=list(var2_with_CAPS_transitivity_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Local(M)", "Barrat's Weighted(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - median

```r
aggdata_median <-aggregate(. ~ Type, var2_with_CAPS_transitivity_df, function(x) c(median=median(x))) 

names(aggdata_median) <- c("Group","Local(median)", "Barrat's Weighted(median)")

#Removing Type variable
#aggdata_median<-aggdata_median[,-c(2)]
```
##Merging mean and median

```r
total_table <- merge(aggdata_mean,aggdata_median,by="Group")

#Rounding
Group<-total_table[,c(1)] #Keeping group
total_table<-total_table[,-c(1)] %>% round(3) #Rouding
total_table<-cbind(Group,total_table) #Binding toghter

#Organizing Variabels
total_table<-total_table[c("Group","Local(M)","Local(median)", "Barrat's Weighted(M)","Barrat's Weighted(median)")]
```
##Final table with round - Transitivity

```r
datatable(total_table, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-a31f41d2cd0be0209b78" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-a31f41d2cd0be0209b78">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.476\" data-max=\"0.557\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.4\" data-max=\"0.467\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.672\" data-max=\"0.819\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.667\" data-max=\"0.802\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[0.476,0.557],[0.4,0.467],[0.819,0.672],[0.802,0.667]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Local(M)\u003c/th>\n      <th>Local(median)\u003c/th>\n      <th>Barrat's Weighted(M)\u003c/th>\n      <th>Barrat's Weighted(median)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Transitivity  Measures Dinamic Table

```r
#Getting  Measures

var2_with_CAPS_transitivity_local<-transitivity(var2_with_CAPS, type="local") %>% round(3)

var2_with_CAPS_transitivity_barrat<-transitivity(var2_with_CAPS, weights=E(var2_with_CAPS)$weight, type="barrat") %>% round(3)

#Creating a datagrame of measures
var2_with_CAPS_transitivity_df <- data.frame(var2_with_CAPS_transitivity_local,var2_with_CAPS_transitivity_barrat) %>% round(3)
```

#Transitivity  Measures Dinamic Table

```r
var2_with_CAPS_transitivity_df <-cbind(var2_with_CAPS_transitivity_df, V(var2_with_CAPS)$TIPO2)

#Adding names
names(var2_with_CAPS_transitivity_df) <- c("Local", "Barrat's Weighted","Type")

#Ordering Variables
var2_with_CAPS_transitivity_df<-var2_with_CAPS_transitivity_df[c("Type", "Local", "Barrat's Weighted")]
```
## General tabel - DT 

```r
datatable(var2_with_CAPS_transitivity_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-8eba78fdfc649b586f3a" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-8eba78fdfc649b586f3a">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"1\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[0.058,0.091,0.045,0.221,0.438,0.4,0.062,0.146,0.467,0.167,0.17,0.615,0.19,0.152,0.667,0.4,0.113,0.346,null,0.389,0.2,0.25,0.3,0.051,null,0.167,0.333,0.145,0.23,0.336,0.433,0.306,0.373,1,0.606,0.5,0.476,0,0.6,0.154,0.2,0.282,0.345,0.5,0.429,0.326,1,0.306,0.306,0.306,0.306,0.306,null,0.286,0.046,0.492,0.429,null,1,0.229,0.167,0.167,0.467,0.464,0.2,null,0.667,0.556,0,0.667,1,0.429,0.4,0.4,0.255,0.476,0.5,0.25,0.214,0.214,0.214,0.214,0.7,0.167,0.091,0.333,0.333,1,0.17,0.7,0.357,null,0.167,0.095,0.4,0.467,1,1,0.6,0.952,0,0.316,null,null,1,0.364,0.667,null,0,0.679,0.528,0.306,0.429,0.4,0.7,0.467,0.393,0.273,0.333,0.303,1,0.392,0.667,0.667,0.667,0.9,0.5,0.4,0.267,0.833,1,0.667,0.3,0.4,0.667,0.444,0.4,0.4,0.333,1,0.361,0.467,0.6,0.467,null,0.234,0.143,0.833,0.2,1,1,1,1,1,0,0.333,1,1,1,null,null,0.667,null,1,1,1,1,1,1,1,1,1,1,1,1,null,1,1,1,1,1,1,0.333,1,1,null,null],[0.095,0.139,0.084,0.419,0.804,0.511,0.112,0.221,0.6,0.667,0.277,0.912,0.248,0.303,1.333,0.578,0.211,0.538,null,0.5,0.362,0.339,0.587,0.053,null,0.167,0.333,0.269,0.467,0.581,0.769,0.694,0.605,1,0.924,0.65,0.667,0,0.8,0.226,0.359,0.545,0.609,0.7,0.524,0.505,1,0.403,0.403,0.403,0.403,0.403,null,0.476,0.048,0.821,0.786,null,1,0.408,0.167,0.667,0.867,1.071,0.3,null,0.667,0.889,0,1.333,1.667,0.905,0.6,0.633,0.545,1.143,0.75,0.444,0.857,0.857,0.857,0.857,1.1,0.25,0.364,0.733,0.333,1,0.301,1.1,0.75,null,0.667,0.238,0.822,0.6,1,1.333,0.8,1.333,0,0.502,null,null,1.7,0.803,1.333,null,0,1.036,0.903,0.458,0.833,0.85,0.9,1.033,1,0.736,0.722,0.485,1.7,0.585,1.167,1.167,1.167,1.4,1.167,0.833,0.533,1,1.667,1.167,0.85,0.8,1.167,0.736,1.1,0.933,0.857,2,0.639,1.033,1.15,1.133,null,0.385,0.286,1.333,0.367,1,1.667,1,1.667,1,0,0.333,1,1.667,1.667,null,null,0.667,null,1,1,1,1,1,1,1,1,1,1,1,1,null,1,1,1,1,1,1,0.333,1,1,null,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Local\u003c/th>\n      <th>Barrat's Weighted\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var2_with_CAPS_transitivity_df, by=list(var2_with_CAPS_transitivity_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Local(M)", "Barrat's Weighted(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - median

```r
aggdata_median <-aggregate(. ~ Type, var2_with_CAPS_transitivity_df, function(x) c(median=median(x))) 

names(aggdata_median) <- c("Group","Local(median)", "Barrat's Weighted(median)")

#Removing Type variable
#aggdata_median<-aggdata_median[,-c(2)]
```
##Merging mean and median

```r
total_table <- merge(aggdata_mean,aggdata_median,by="Group")

#Rounding
Group<-total_table[,c(1)] #Keeping group
total_table<-total_table[,-c(1)] %>% round(3) #Rouding
total_table<-cbind(Group,total_table) #Binding toghter

#Organizing Variabels
total_table<-total_table[c("Group","Local(M)","Local(median)", "Barrat's Weighted(M)","Barrat's Weighted(median)")]
```
##Final table with round - Transitivity

```r
datatable(total_table, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-01060fc378d9942f9be4" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-01060fc378d9942f9be4">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.347\" data-max=\"0.554\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.336\" data-max=\"0.467\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.574\" data-max=\"0.862\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.511\" data-max=\"0.841\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[0.347,0.501,0.554],[0.336,0.414,0.467],[0.574,0.862,0.667],[0.511,0.841,0.667]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Local(M)\u003c/th>\n      <th>Local(median)\u003c/th>\n      <th>Barrat's Weighted(M)\u003c/th>\n      <th>Barrat's Weighted(median)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->



#Distances and paths

Defined as the shortest distance between each pair of nodes in the network (in both directions for directed graphs).

##Average path length between any two given nodes

Calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs). 

This function does not consider edge weights currently and uses a breadth-first search.

```r
mean_distance(var2_with_CAPS, directed=T, unconnected = T)
```

```
## [1] 3.372042
```
##Shortest Paths

```r
#Shortest Paths
var2_with_CAPS_sp_in <- shortest.paths(var2_with_CAPS, mode='in', weights=E(var2_with_CAPS)$var2) #in

var2_with_CAPS_sp_out <- shortest.paths(var2_with_CAPS, mode='out', weights=E(var2_with_CAPS)$var2) # out

var2_with_CAPS_sp_all <- shortest.paths(var2_with_CAPS, mode='all', weights=E(var2_with_CAPS)$var2) # all
```
##Descriptive Shortest Paths - IN

```r
summary(var2_with_CAPS_sp_in[which(var2_with_CAPS_sp_in != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   3.000   3.338   4.000   9.000
```

```r
sd(var2_with_CAPS_sp_in[which(var2_with_CAPS_sp_in != Inf)])
```

```
## [1] 1.763743
```
##Descriptive  Shortest Paths - OUT

```r
summary(var2_with_CAPS_sp_out[which(var2_with_CAPS_sp_out != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   3.000   3.338   4.000   9.000
```

```r
sd(var2_with_CAPS_sp_out[which(var2_with_CAPS_sp_out != Inf)])
```

```
## [1] 1.763743
```

##Descriptive  Shortest Paths - ALL

```r
summary(var2_with_CAPS_sp_all[which(var2_with_CAPS_sp_all != Inf)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.000   2.000   2.552   3.000   6.000
```

```r
sd(var2_with_CAPS_sp_all[which(var2_with_CAPS_sp_all != Inf)])
```

```
## [1] 0.8393559
```

#Length of all shortest paths in the graph:

```r
#All shortest paths 
distances_dist_all_var2_with_CAPS<-distances(var2_with_CAPS, mode="all", weights=E(var2_with_CAPS)$var2)
#distances_sp_all_var2_with_CAPS

distances_dist_all_var2_with_CAPS[distances_dist_all_var2_with_CAPS=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_all_var2_with_CAPS_vec <- vector()
for (i in 1:vcount(var2_with_CAPS)) {
    distances_sp_all_var2_with_CAPS_vec[i] <- 
    mean(distances_dist_all_var2_with_CAPS[i,],na.rm=T)
}
#Adding to igraph object
V(var2_with_CAPS)$var2_sp_all<-distances_sp_all_var2_with_CAPS_vec
```

#In shortest paths 

```r
distances_dist_in_var2_with_CAPS<-distances(var2_with_CAPS, mode="in",weights=E(var2_with_CAPS)$var2)
#distances_sp_in_var2_with_CAPS

distances_dist_in_var2_with_CAPS[distances_dist_in_var2_with_CAPS=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_in_var2_with_CAPS_vec <- vector()
for (i in 1:vcount(var2_with_CAPS)) {
    distances_sp_in_var2_with_CAPS_vec[i] <- mean(distances_dist_in_var2_with_CAPS[i,], na.rm=T)
}

#Adding to igraph object
V(var2_with_CAPS)$var2_sp_in<-distances_sp_in_var2_with_CAPS_vec
```

#Out shortest paths 

```r
distances_dist_out_var2_with_CAPS<-distances(var2_with_CAPS, mode="out", weights=E(var2_with_CAPS)$var2)

distances_dist_out_var2_with_CAPS[distances_dist_out_var2_with_CAPS=="Inf"]<-NA

#Mean Reachbility by Vertex
distances_sp_out_var2_with_CAPS_vec <- vector()
for (i in 1:vcount(var2_with_CAPS)) {
    distances_sp_out_var2_with_CAPS_vec[i] <- 
    mean(distances_dist_out_var2_with_CAPS[i,], na.rm = T)
}

#Adding to igraph object
V(var2_with_CAPS)$var2_sp_out<-distances_sp_out_var2_with_CAPS_vec
```

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var2_with_CAPS_shortpath_df <- data.frame(distances_sp_in_var2_with_CAPS_vec, distances_sp_out_var2_with_CAPS_vec, distances_sp_all_var2_with_CAPS_vec) %>% round(3)

#Adding type
var2_with_CAPS_shortpath_df <-cbind(var2_with_CAPS_shortpath_df, V(var2_with_CAPS)$LABEL_COR)

#Adding names
names(var2_with_CAPS_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var2_with_CAPS_shortpath_df<-var2_with_CAPS_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var2_with_CAPS_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-cb816ca7e3e42d411dcc" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-cb816ca7e3e42d411dcc">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"6.895\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"5.128\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"4.652\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Pronto Socorro","Ambulatório de Saúde Mental","CAPSAD","CRAS","CREAS","CREAS","Ambulatório Tabagismo","Clínicas e CT","Clínicas e CT","Clínicas e CT","CRAS","CRAS","Clínicas e CT","Consultório na Rua","UAPS URBANA","Residência Terapeutica","CREAS","SAMU","Entidades Socioassistenciais","Ambulatório AD","CAPS","Clínicas e CT","CAPSi","Ajuda Mútua","Entidades Socioassistenciais","Ajuda Mútua","Ajuda Mútua","CAPS","CRAS","CRAS","CRAS","UAPS URBANA","Acolhimento Institucional","Ajuda Mútua","CRAS","Clínicas e CT","Clínicas e CT","Residência Terapeutica","Residência Terapeutica","Entidades Socioassistenciais","Acolhimento Institucional","Consultório na Rua","Entidades Socioassistenciais","Clínicas e CT","Entidades Socioassistenciais","CRAS","Ajuda Mútua","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Residência Terapeutica","Clínicas e CT","UAPS URBANA","Ajuda Mútua","CRAS","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","Assistência Hospitalar","Ajuda Mútua","Clínicas e CT","UAPS URBANA","UAPS URBANA","Clínicas e CT","Clínicas e CT","Ajuda Mútua","Entidades Socioassistenciais","Entidades Socioassistenciais","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Centro POP","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Ajuda Mútua","Ajuda Mútua","UAPS URBANA","Clínicas e CT","Clínicas e CT","UAPS URBANA","UAPS URBANA","UAPS URBANA","Ajuda Mútua","Clínicas e CT","Clínicas e CT","UAPS URBANA","Clínicas e CT","Clínicas e CT","Clínicas e CT","UAPS URBANA","Hospital Psiquiátrico","Entidades Socioassistenciais","Entidades Socioassistenciais","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS URBANA","Residência Terapeutica","Residência Terapeutica","UAPS URBANA","UAPS URBANA","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","UAPS URBANA","Entidades Socioassistenciais","CAPS","Centro de Convivência","UAPS URBANA","Acolhimento Institucional","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","UAPS RURAL","UAPS RURAL","UAPS RURAL","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Ajuda Mútua","Hospital Judiciário"],[1.885,2.154,1.548,2.173,2.096,2.142,2.404,2.76,3.721,3.654,2.106,2.114,4.644,3,2.305,2.452,2.106,2.181,5.225,2.375,2.135,3.058,2.173,4.99,0,5.923,5.904,2.087,2.144,2.173,2.077,2.269,2.067,5.914,2.144,3.683,2.846,3.019,3.26,2.654,2.163,2.192,2.308,2.846,3.135,2.076,5.914,3.635,3.635,3.635,3.635,3.635,0,2.567,5.962,2.163,2.2,6.895,5.914,2.019,3.619,3.769,2.276,2.269,3.683,0,6.895,2.817,3.077,2.324,2.305,2.298,2.238,2.24,2.212,2.288,2.231,1.923,4.266,4.266,4.266,4.266,2.257,4.01,3.312,2.26,2.952,5.59,2.288,2.317,2.327,0,3.769,2.798,2.317,3.471,5.547,3.657,2.25,2.181,4.273,2.75,3.257,3.257,2.267,2.298,2.269,3.257,3.048,2.229,2.231,2.202,2.26,2.298,2.257,2.269,2.25,2.25,2.298,2.25,2.267,2.337,2.276,2.276,2.276,2.288,2.308,2.279,2.24,2.635,2.305,2.276,2.308,2.288,2.276,2.25,2.288,2.26,2.26,2.305,2.212,2.279,2.327,2.269,0,2.077,2.231,2.314,2.952,2.667,2.305,2.667,2.305,2.667,6.885,5.904,2.667,2.305,2.305,5.933,0,5.857,6.895,6.84,5.914,6.84,5.914,5.914,5.914,5.914,5.914,5.914,5.914,5.914,5.914,6.895,5.914,5.914,5.914,5.914,5.914,5.914,3.448,5.914,5.914,0,0],[2.492,2.609,2.307,2.609,3.207,0,3.017,2.704,3.173,3.659,3.497,0,2.95,3.235,0,3.095,2.458,0,0,4,3.123,3.324,3.156,2.034,3.472,4.408,3.447,2.927,2.715,2.994,3.134,3.235,3.374,0,5.073,3.38,3.168,3.81,3.229,2.648,2.48,3.123,3.123,3.291,2.581,0,0,3.095,3.095,3.095,3.095,3.095,0,3.05,2.838,3.274,0,0,0,3.212,0,5.128,0,3.279,3.536,0,0.667,2.916,4.196,0,0,3.24,0,3.598,3.201,3.279,4.106,2.771,1.286,1.286,1.286,1.286,0,3.022,1,3.246,4.179,0.5,3,4.089,3.19,0,5.128,4.145,3.212,3.201,0,0,4.307,0,0.5,2.989,0,0,0,3.106,3.916,0,0,0,4.112,3.821,4.112,3.296,0.5,3.916,3.447,3.318,3.196,3.291,0,3.201,0,0,0,4.117,3.296,3.274,3.296,4.112,0,0,3.246,3.296,0,3.201,3.285,3.296,3.296,0,4.112,3.296,3.291,3.296,3.289,3.296,3.246,0,3.374,0,0,0,0,0,4.201,4.207,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[1.635,2,1.514,2.138,2.287,2.315,2.215,2.055,2.365,3.011,2.249,2.271,2.414,2.68,2.381,2.326,1.95,2.304,4.652,2.348,2.232,2.343,2.276,1.923,2.624,2.729,2.188,2.177,2.215,2.232,2.271,2.331,2.227,2.823,2.304,2.42,2.309,2.934,2.376,2.116,2.138,2.32,2.337,2.37,2.337,2.254,2.823,2.343,2.343,2.343,2.343,2.343,0,2.182,2.508,2.249,2.304,3.497,2.823,2.055,2.707,3.575,2.359,2.365,2.912,0,3.486,2.464,2.762,2.403,2.381,2.37,2.166,2.171,2.326,2.37,2.116,2.116,3.657,3.657,3.657,3.657,2.376,2.729,2.685,2.365,2.713,3.398,2.249,2.359,2.37,0,3.575,2.591,2.354,2.149,3.398,3.022,2.343,2.326,3.663,2.271,3.238,3.238,2.37,2.326,2.37,3.238,3.116,2.309,2.309,2.331,2.365,2.376,2.359,2.37,2.365,2.359,2.359,2.343,2.37,2.304,2.376,2.376,2.376,2.365,2.381,2.359,2.337,2.53,2.381,2.376,2.381,2.354,2.376,2.343,2.376,2.331,2.326,2.376,2.343,2.37,2.381,2.37,2.503,2.232,2.287,2.381,2.873,2.552,2.381,2.552,2.381,2.552,2.801,2.724,2.552,2.381,2.381,2.912,0,2.807,3.497,3.492,2.823,3.492,2.823,2.823,2.823,2.823,2.823,2.823,2.823,2.823,2.823,3.497,2.823,2.823,2.823,2.823,2.823,2.823,2.713,2.823,2.823,0,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var2_with_CAPS_shortpath_df, by=list(var2_with_CAPS_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - median

```r
aggdata_median <-aggregate(. ~ Type, var2_with_CAPS_shortpath_df, function(x) c(median=median(x)))

names(aggdata_median) <- c("Group", "Short Path IN(median)", "Short Path OUT(median)","Short Path ALL(median)")

#Removing Type variable
#aggdata_median<-aggdata_median[,-c(2)]
```
##Merging mean and median

```r
total_table <- merge(aggdata_mean,aggdata_median,by="Group")

#Rounding
Group<-total_table[,c(1)] #Keeping group
total_table<-total_table[,-c(1)] %>% round(3) #Rouding
total_table<-cbind(Group,total_table) #Binding toghter

#Organizing Variabels
total_table<-total_table[,c("Group","Short Path IN(M)","Short Path IN(median)","Short Path OUT(M)","Short Path OUT(median)","Short Path ALL(M)","Short Path ALL(median)")]
```
##Final table with round - Short Path

```r
datatable(total_table, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-1cd4c5f177f0c9df6419" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-1cd4c5f177f0c9df6419">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Acolhimento Institucional&quot;,&quot;Ajuda Mútua&quot;,&quot;Ambulatório AD&quot;,&quot;Ambulatório de Saúde Mental&quot;,&quot;Ambulatório Tabagismo&quot;,&quot;Assistência Hospitalar&quot;,&quot;CAPS&quot;,&quot;CAPSAD&quot;,&quot;CAPSi&quot;,&quot;Centro de Convivência&quot;,&quot;Centro POP&quot;,&quot;Clínicas e CT&quot;,&quot;Consultório na Rua&quot;,&quot;CRAS&quot;,&quot;CREAS&quot;,&quot;Entidades Socioassistenciais&quot;,&quot;Hospital Judiciário&quot;,&quot;Hospital Psiquiátrico&quot;,&quot;Pronto Socorro&quot;,&quot;Residência Terapeutica&quot;,&quot;SAMU&quot;,&quot;UAPS RURAL&quot;,&quot;UAPS URBANA&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"5.31\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"5.914\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"4\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"4\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.746\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"2.823\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"],["Acolhimento Institucional","Ajuda Mútua","Ambulatório AD","Ambulatório de Saúde Mental","Ambulatório Tabagismo","Assistência Hospitalar","CAPS","CAPSAD","CAPSi","Centro de Convivência","Centro POP","Clínicas e CT","Consultório na Rua","CRAS","CREAS","Entidades Socioassistenciais","Hospital Judiciário","Hospital Psiquiátrico","Pronto Socorro","Residência Terapeutica","SAMU","UAPS RURAL","UAPS URBANA"],[2.394,5.31,2.375,2.154,2.404,2.019,2.1,1.548,2.173,2.231,1.923,3.287,2.596,2.13,2.115,2.598,0,2.181,1.885,3.31,2.181,2.382,2.278],[2.163,5.914,2.375,2.154,2.404,2.019,2.087,1.548,2.173,2.231,1.923,3.654,2.596,2.144,2.106,2.75,0,2.181,1.885,3.258,2.181,2.305,2.269],[3.076,0.673,4,2.609,3.017,3.212,3.115,2.307,3.156,3.246,2.771,2.709,3.179,2.588,1.888,2.629,0,0,2.492,2.134,0,1.153,2.455],[3.374,0,4,2.609,3.017,3.212,3.123,2.307,3.156,3.246,2.771,3.201,3.179,2.994,2.458,2.989,0,0,2.492,3.095,0,0,3.243],[2.413,2.746,2.348,2,2.215,2.055,2.214,1.514,2.276,2.287,2.116,2.454,2.5,2.243,2.184,2.73,0,2.326,1.635,2.682,2.304,2.417,2.34],[2.227,2.823,2.348,2,2.215,2.055,2.232,1.514,2.276,2.287,2.116,2.42,2.5,2.249,2.287,2.464,0,2.326,1.635,2.359,2.304,2.381,2.362]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(median)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(median)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(median)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var2_with_CAPS_shortpath_df <- data.frame(distances_sp_in_var2_with_CAPS_vec, distances_sp_out_var2_with_CAPS_vec, distances_sp_all_var2_with_CAPS_vec) %>% round(3)

#Adding type
var2_with_CAPS_shortpath_df <-cbind(var2_with_CAPS_shortpath_df, V(var2_with_CAPS)$TIPO1)

#Adding names
names(var2_with_CAPS_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var2_with_CAPS_shortpath_df<-var2_with_CAPS_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var2_with_CAPS_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-46b85b6108851ee213c8" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-46b85b6108851ee213c8">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"6.895\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"5.128\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"4.652\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Não Governamental","Governamental","Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Governamental","Governamental","Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Não Governamental","Governamental"],[1.885,2.154,1.548,2.173,2.096,2.142,2.404,2.76,3.721,3.654,2.106,2.114,4.644,3,2.305,2.452,2.106,2.181,5.225,2.375,2.135,3.058,2.173,4.99,0,5.923,5.904,2.087,2.144,2.173,2.077,2.269,2.067,5.914,2.144,3.683,2.846,3.019,3.26,2.654,2.163,2.192,2.308,2.846,3.135,2.076,5.914,3.635,3.635,3.635,3.635,3.635,0,2.567,5.962,2.163,2.2,6.895,5.914,2.019,3.619,3.769,2.276,2.269,3.683,0,6.895,2.817,3.077,2.324,2.305,2.298,2.238,2.24,2.212,2.288,2.231,1.923,4.266,4.266,4.266,4.266,2.257,4.01,3.312,2.26,2.952,5.59,2.288,2.317,2.327,0,3.769,2.798,2.317,3.471,5.547,3.657,2.25,2.181,4.273,2.75,3.257,3.257,2.267,2.298,2.269,3.257,3.048,2.229,2.231,2.202,2.26,2.298,2.257,2.269,2.25,2.25,2.298,2.25,2.267,2.337,2.276,2.276,2.276,2.288,2.308,2.279,2.24,2.635,2.305,2.276,2.308,2.288,2.276,2.25,2.288,2.26,2.26,2.305,2.212,2.279,2.327,2.269,0,2.077,2.231,2.314,2.952,2.667,2.305,2.667,2.305,2.667,6.885,5.904,2.667,2.305,2.305,5.933,0,5.857,6.895,6.84,5.914,6.84,5.914,5.914,5.914,5.914,5.914,5.914,5.914,5.914,5.914,6.895,5.914,5.914,5.914,5.914,5.914,5.914,3.448,5.914,5.914,0,0],[2.492,2.609,2.307,2.609,3.207,0,3.017,2.704,3.173,3.659,3.497,0,2.95,3.235,0,3.095,2.458,0,0,4,3.123,3.324,3.156,2.034,3.472,4.408,3.447,2.927,2.715,2.994,3.134,3.235,3.374,0,5.073,3.38,3.168,3.81,3.229,2.648,2.48,3.123,3.123,3.291,2.581,0,0,3.095,3.095,3.095,3.095,3.095,0,3.05,2.838,3.274,0,0,0,3.212,0,5.128,0,3.279,3.536,0,0.667,2.916,4.196,0,0,3.24,0,3.598,3.201,3.279,4.106,2.771,1.286,1.286,1.286,1.286,0,3.022,1,3.246,4.179,0.5,3,4.089,3.19,0,5.128,4.145,3.212,3.201,0,0,4.307,0,0.5,2.989,0,0,0,3.106,3.916,0,0,0,4.112,3.821,4.112,3.296,0.5,3.916,3.447,3.318,3.196,3.291,0,3.201,0,0,0,4.117,3.296,3.274,3.296,4.112,0,0,3.246,3.296,0,3.201,3.285,3.296,3.296,0,4.112,3.296,3.291,3.296,3.289,3.296,3.246,0,3.374,0,0,0,0,0,4.201,4.207,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[1.635,2,1.514,2.138,2.287,2.315,2.215,2.055,2.365,3.011,2.249,2.271,2.414,2.68,2.381,2.326,1.95,2.304,4.652,2.348,2.232,2.343,2.276,1.923,2.624,2.729,2.188,2.177,2.215,2.232,2.271,2.331,2.227,2.823,2.304,2.42,2.309,2.934,2.376,2.116,2.138,2.32,2.337,2.37,2.337,2.254,2.823,2.343,2.343,2.343,2.343,2.343,0,2.182,2.508,2.249,2.304,3.497,2.823,2.055,2.707,3.575,2.359,2.365,2.912,0,3.486,2.464,2.762,2.403,2.381,2.37,2.166,2.171,2.326,2.37,2.116,2.116,3.657,3.657,3.657,3.657,2.376,2.729,2.685,2.365,2.713,3.398,2.249,2.359,2.37,0,3.575,2.591,2.354,2.149,3.398,3.022,2.343,2.326,3.663,2.271,3.238,3.238,2.37,2.326,2.37,3.238,3.116,2.309,2.309,2.331,2.365,2.376,2.359,2.37,2.365,2.359,2.359,2.343,2.37,2.304,2.376,2.376,2.376,2.365,2.381,2.359,2.337,2.53,2.381,2.376,2.381,2.354,2.376,2.343,2.376,2.331,2.326,2.376,2.343,2.37,2.381,2.37,2.503,2.232,2.287,2.381,2.873,2.552,2.381,2.552,2.381,2.552,2.801,2.724,2.552,2.381,2.381,2.912,0,2.807,3.497,3.492,2.823,3.492,2.823,2.823,2.823,2.823,2.823,2.823,2.823,2.823,2.823,3.497,2.823,2.823,2.823,2.823,2.823,2.823,2.713,2.823,2.823,0,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var2_with_CAPS_shortpath_df, by=list(var2_with_CAPS_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - median

```r
aggdata_median <-aggregate(. ~ Type, var2_with_CAPS_shortpath_df, function(x) c(median=median(x)))

names(aggdata_median) <- c("Group", "Short Path IN(median)", "Short Path OUT(median)","Short Path ALL(median)")

#Removing Type variable
#aggdata_median<-aggdata_median[,-c(2)]
```
##Merging mean and median

```r
total_table <- merge(aggdata_mean,aggdata_median,by="Group")

#Rounding
Group<-total_table[,c(1)] #Keeping group
total_table<-total_table[,-c(1)] %>% round(3) #Rouding
total_table<-cbind(Group,total_table) #Binding toghter

#Organizing Variabels
total_table<-total_table[,c("Group","Short Path IN(M)","Short Path IN(median)","Short Path OUT(M)","Short Path OUT(median)","Short Path ALL(M)","Short Path ALL(median)")]
```
##Final table with round - Short Path

```r
datatable(total_table, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-da0946a4c866ef636c49" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-da0946a4c866ef636c49">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Governamental&quot;,&quot;Não Governamental&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.348\" data-max=\"4.346\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.272\" data-max=\"4.273\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.495\" data-max=\"2.216\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.5\" data-max=\"3.095\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.326\" data-max=\"2.668\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.351\" data-max=\"2.823\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2"],["Governamental","Não Governamental"],[2.348,4.346],[2.272,4.273],[2.216,1.495],[3.095,0.5],[2.326,2.668],[2.351,2.823]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(median)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(median)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(median)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

#Reachbility Measures Dinamic Table

```r
#Creating a datagrame of measures
var2_with_CAPS_shortpath_df <- data.frame(distances_sp_in_var2_with_CAPS_vec, distances_sp_out_var2_with_CAPS_vec, distances_sp_all_var2_with_CAPS_vec) %>% round(3)

#Adding type
var2_with_CAPS_shortpath_df <-cbind(var2_with_CAPS_shortpath_df, V(var2_with_CAPS)$TIPO2)

#Adding names
names(var2_with_CAPS_shortpath_df) <- c("Short Path IN", "Short Path OUT","Short Path ALL","Type") 

#Ordering Variables
var2_with_CAPS_shortpath_df<-var2_with_CAPS_shortpath_df[c("Type", "Short Path IN", "Short Path OUT","Short Path ALL")]
```
## General tabel - DT 

```r
datatable(var2_with_CAPS_shortpath_df, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-9c8f0abe34a885a29e48" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-9c8f0abe34a885a29e48">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"6.895\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"5.128\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"4.652\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187"],["Saúde","Saúde","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Assistência Social","Saúde","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Assistência Social","Assistência Social","Assistência Social","Saúde","Assistência Social","Terceiro Setor","Assistência Social","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Assistência Social","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Terceiro Setor","Assistência Social","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Assistência Social","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Saúde","Saúde","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Saúde","Saúde","Saúde","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Terceiro Setor","Saúde"],[1.885,2.154,1.548,2.173,2.096,2.142,2.404,2.76,3.721,3.654,2.106,2.114,4.644,3,2.305,2.452,2.106,2.181,5.225,2.375,2.135,3.058,2.173,4.99,0,5.923,5.904,2.087,2.144,2.173,2.077,2.269,2.067,5.914,2.144,3.683,2.846,3.019,3.26,2.654,2.163,2.192,2.308,2.846,3.135,2.076,5.914,3.635,3.635,3.635,3.635,3.635,0,2.567,5.962,2.163,2.2,6.895,5.914,2.019,3.619,3.769,2.276,2.269,3.683,0,6.895,2.817,3.077,2.324,2.305,2.298,2.238,2.24,2.212,2.288,2.231,1.923,4.266,4.266,4.266,4.266,2.257,4.01,3.312,2.26,2.952,5.59,2.288,2.317,2.327,0,3.769,2.798,2.317,3.471,5.547,3.657,2.25,2.181,4.273,2.75,3.257,3.257,2.267,2.298,2.269,3.257,3.048,2.229,2.231,2.202,2.26,2.298,2.257,2.269,2.25,2.25,2.298,2.25,2.267,2.337,2.276,2.276,2.276,2.288,2.308,2.279,2.24,2.635,2.305,2.276,2.308,2.288,2.276,2.25,2.288,2.26,2.26,2.305,2.212,2.279,2.327,2.269,0,2.077,2.231,2.314,2.952,2.667,2.305,2.667,2.305,2.667,6.885,5.904,2.667,2.305,2.305,5.933,0,5.857,6.895,6.84,5.914,6.84,5.914,5.914,5.914,5.914,5.914,5.914,5.914,5.914,5.914,6.895,5.914,5.914,5.914,5.914,5.914,5.914,3.448,5.914,5.914,0,0],[2.492,2.609,2.307,2.609,3.207,0,3.017,2.704,3.173,3.659,3.497,0,2.95,3.235,0,3.095,2.458,0,0,4,3.123,3.324,3.156,2.034,3.472,4.408,3.447,2.927,2.715,2.994,3.134,3.235,3.374,0,5.073,3.38,3.168,3.81,3.229,2.648,2.48,3.123,3.123,3.291,2.581,0,0,3.095,3.095,3.095,3.095,3.095,0,3.05,2.838,3.274,0,0,0,3.212,0,5.128,0,3.279,3.536,0,0.667,2.916,4.196,0,0,3.24,0,3.598,3.201,3.279,4.106,2.771,1.286,1.286,1.286,1.286,0,3.022,1,3.246,4.179,0.5,3,4.089,3.19,0,5.128,4.145,3.212,3.201,0,0,4.307,0,0.5,2.989,0,0,0,3.106,3.916,0,0,0,4.112,3.821,4.112,3.296,0.5,3.916,3.447,3.318,3.196,3.291,0,3.201,0,0,0,4.117,3.296,3.274,3.296,4.112,0,0,3.246,3.296,0,3.201,3.285,3.296,3.296,0,4.112,3.296,3.291,3.296,3.289,3.296,3.246,0,3.374,0,0,0,0,0,4.201,4.207,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[1.635,2,1.514,2.138,2.287,2.315,2.215,2.055,2.365,3.011,2.249,2.271,2.414,2.68,2.381,2.326,1.95,2.304,4.652,2.348,2.232,2.343,2.276,1.923,2.624,2.729,2.188,2.177,2.215,2.232,2.271,2.331,2.227,2.823,2.304,2.42,2.309,2.934,2.376,2.116,2.138,2.32,2.337,2.37,2.337,2.254,2.823,2.343,2.343,2.343,2.343,2.343,0,2.182,2.508,2.249,2.304,3.497,2.823,2.055,2.707,3.575,2.359,2.365,2.912,0,3.486,2.464,2.762,2.403,2.381,2.37,2.166,2.171,2.326,2.37,2.116,2.116,3.657,3.657,3.657,3.657,2.376,2.729,2.685,2.365,2.713,3.398,2.249,2.359,2.37,0,3.575,2.591,2.354,2.149,3.398,3.022,2.343,2.326,3.663,2.271,3.238,3.238,2.37,2.326,2.37,3.238,3.116,2.309,2.309,2.331,2.365,2.376,2.359,2.37,2.365,2.359,2.359,2.343,2.37,2.304,2.376,2.376,2.376,2.365,2.381,2.359,2.337,2.53,2.381,2.376,2.381,2.354,2.376,2.343,2.376,2.331,2.326,2.376,2.343,2.37,2.381,2.37,2.503,2.232,2.287,2.381,2.873,2.552,2.381,2.552,2.381,2.552,2.801,2.724,2.552,2.381,2.381,2.912,0,2.807,3.497,3.492,2.823,3.492,2.823,2.823,2.823,2.823,2.823,2.823,2.823,2.823,2.823,3.497,2.823,2.823,2.823,2.823,2.823,2.823,2.713,2.823,2.823,0,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Type\u003c/th>\n      <th>Short Path IN\u003c/th>\n      <th>Short Path OUT\u003c/th>\n      <th>Short Path ALL\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Aggregating data from previous table - mean

```r
aggdata_mean <-aggregate(var2_with_CAPS_shortpath_df, by=list(var2_with_CAPS_shortpath_df$Type), FUN=mean, na.rm=TRUE)

names(aggdata_mean) <- c("Group","Type","Short Path IN(M)", "Short Path OUT(M)","Short Path ALL(M)")
  
#Removing Type variable
aggdata_mean<-aggdata_mean[,-c(2)]
```
##Aggregating data from previous table - median

```r
aggdata_median <-aggregate(. ~ Type, var2_with_CAPS_shortpath_df, function(x) c(median=median(x)))

names(aggdata_median) <- c("Group", "Short Path IN(median)", "Short Path OUT(median)","Short Path ALL(median)")

#Removing Type variable
#aggdata_median<-aggdata_median[,-c(2)]
```
##Merging mean and median

```r
total_table <- merge(aggdata_mean,aggdata_median,by="Group")

#Rounding
Group<-total_table[,c(1)] #Keeping group
total_table<-total_table[,-c(1)] %>% round(3) #Rouding
total_table<-cbind(Group,total_table) #Binding toghter

#Organizing Variabels
total_table<-total_table[,c("Group","Short Path IN(M)","Short Path IN(median)","Short Path OUT(M)","Short Path OUT(median)","Short Path ALL(M)","Short Path ALL(median)")]
```
##Final table with round - Short Path

```r
datatable(total_table, filter = 'top')
```

<!--html_preserve--><div id="htmlwidget-3989ff5452098698bdf3" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-3989ff5452098698bdf3">{"x":{"filter":"top","filterHTML":"<tr>\n  <td>\u003c/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Assistência Social&quot;,&quot;Saúde&quot;,&quot;Terceiro Setor&quot;]\">\u003c/select>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.111\" data-max=\"4.375\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.114\" data-max=\"4.458\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1.512\" data-max=\"2.506\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.5\" data-max=\"3.101\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.214\" data-max=\"2.673\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\">\u003c/span>\n    \u003c/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"2.249\" data-max=\"2.823\" data-scale=\"3\">\u003c/div>\n      <span style=\"float: left;\">\u003c/span>\n      <span style=\"float: right;\">\u003c/span>\n    \u003c/div>\n  \u003c/td>\n\u003c/tr>","data":[["1","2","3"],["Assistência Social","Saúde","Terceiro Setor"],[2.111,2.383,4.375],[2.114,2.277,4.458],[2.506,2.148,1.512],[2.771,3.101,0.5],[2.214,2.344,2.673],[2.249,2.359,2.823]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Group\u003c/th>\n      <th>Short Path IN(M)\u003c/th>\n      <th>Short Path IN(median)\u003c/th>\n      <th>Short Path OUT(M)\u003c/th>\n      <th>Short Path OUT(median)\u003c/th>\n      <th>Short Path ALL(M)\u003c/th>\n      <th>Short Path ALL(median)\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Histogram from shortest path length between each pair of vertices. 

```r
sp<-distance_table(var2_with_CAPS, directed = TRUE)
short_paths<-c(sp$unconnected, sp$res)
labels<-c("unconnected", "one", "two","three", "four", "five", "six")
sphist<-as.data.frame(cbind(labels, short_paths))
names(sphist)<-c("Short Paths Length - Vertex Pairs","Count")
datatable(sphist)
```

<!--html_preserve--><div id="htmlwidget-7d1542dfdad7f2a77c30" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-7d1542dfdad7f2a77c30">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10"],["unconnected","one","two","three","four","five","six","unconnected","one","two"],["16233","934","6914","4286","2083","1047","1956","1035","266","28"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> \u003c/th>\n      <th>Short Paths Length - Vertex Pairs\u003c/th>\n      <th>Count\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


##Simplify Graph - removing loops and duble edges 

```r
#var2_with_CAPS_u<-simplify(var2_with_CAPS) #Simplify

var2_with_CAPS_u<-as.undirected(var2_with_CAPS, mode="collapse",edge.attr.comb=list(weight="mean","ignore"))
```

#Find cliques (complete subgraphs of an undirected graph)
Graph clique is a subset of vertices of a graph such that every two vertices in the clique are adjacent. - ***To check****

##Number of cliques - subgraphs

```r
cliques_var2_with_CAPS_u<-cliques(var2_with_CAPS_u) # list of cliques 
length(cliques_var2_with_CAPS_u)
```

```
## [1] 6244
```
##Number of cliques by cliques size

```r
cliques_var2_with_CAPS_u_size<-sapply(cliques(var2_with_CAPS_u), length) 
cliques_var2_with_CAPS_u_size_t<-table(cliques_var2_with_CAPS_u_size)
cliques_var2_with_CAPS_u_size_t
```

```
## cliques_var2_with_CAPS_u_size
##    1    2    3    4    5    6    7    8    9 
##  187  792 1396 1582 1275  705  250   52    5
```

##Cliques Bar Plot Sizes Frequency

```r
barplot(cliques_var2_with_CAPS_u_size_t)
title(main = "Cliques Sizes Frequency - Bar Plot 4_REFERENCIA DE RECEBIMENTO CONTRAREFERENCIA (var2)", font.main = 4)
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-212-1.png)<!-- -->

##Size of largest clique 
A maximum clique is a clique that cannot be extended by including one more adjacent vertex (not included in larger one). 

```r
clique_num(var2_with_CAPS_u)
```

```
## [1] 9
```
##Number of maximal cliques

```r
count_max_cliques(var2_with_CAPS_u)
```

```
## [1] 369
```
##Finding of largest cliques

```r
largest_cliques<-largest_cliques(var2_with_CAPS_u) # cliques with max number of nodes
length(largest_cliques)
```

```
## [1] 5
```

##Plotting the largest cliques - important to consider connectivite 

```r
#Coloring largest clique as gold and others one as gray
vcol <- rep("grey80", vcount(var2_with_CAPS_u))
vcol[unlist(largest_cliques(var2_with_CAPS_u))] <- "gold"

#Saving gray and gold as igraph attribute
V(var2_with_CAPS_u)$vcol<-vcol

#Saving labels to display as legend
V(var2_with_CAPS_u)$vcollabel[V(var2_with_CAPS_u)$vcol=="gold"]<-"Largets Clique"
V(var2_with_CAPS_u)$vcollabel [V(var2_with_CAPS_u)$vcol=="grey80"]<-"Others"
```
##Plotting Clique Size

```r
set.seed(123)
#Plotting based only on degree measures 
edge.start <- ends(var2_with_CAPS_u, es=E(var2_with_CAPS_u), names=F)[,1]

# Fixing ego
minC <- rep(-Inf, vcount(var2_with_CAPS_u))
maxC <- rep(Inf, vcount(var2_with_CAPS_u))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(var2_with_CAPS_u, niter=10^4, minx=minC, maxx=maxC,miny=minC, maxy=maxC, weights=E(var2_with_CAPS_u)$var2_with_CAPS)

#Plotting
plot(var2_with_CAPS_u, 
     layout=co,
     edge.color=V(var2_with_CAPS_u)$vcol[edge.start],
     #edge.arrow.size=E(var2_with_CAPS_u)$var2_with_CAPS/2000*mean(E(var2_with_CAPS_u)$var2_with_CAPS),
     #edge.width=E(var2_with_CAPS_u)$var2_with_CAPS/20*mean(E(var2_with_CAPS_u)$var2_with_CAPS),
     edge.curved = TRUE,
     vertex.color=vcol,
     vertex.size=log(degree(var2_with_CAPS_u)+2)*10,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label=get.vertex.attribute(var2_with_CAPS_u,"LABEL_COR"),
     vertex.label.cex=log(degree(var2_with_CAPS_u)+2)/10,
     vertex.label.dist=0,
     rescale=F,
     xlim=range(co[,1]), 
     ylim=range(co[,2])
     )
axis(1)
axis(2)

#Solving Problems with legend rendering 
a<-V(var2_with_CAPS_u)$vcollabel
b<-V(var2_with_CAPS_u)$vcol
c<-table(a,b)
d<-as.data.frame(c)
e<-subset(d, d$Freq>0)
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
  title("Network Vertex Degree Sized - classfied by largest clique vs. others", sub = "Source: from authors ")  
  text( 
    x=range(co[,1])[1],
    y=range(co[,2])[1], 
      labels =       sprintf("Size of largest clique: %.1f\nNumber of maximal cliques: %.1f",
     clique_num(var2_with_CAPS_u), 
     count_max_cliques(var2_with_CAPS_u)
             )
       )
```

![](4_REFERENCIA_DE_RECEBIMENTO_CONTRAREFERENCIA_Analise_com_CAPS_files/figure-html/unnamed-chunk-217-1.png)<!-- -->



```r
# Re-generate dataframes for both nodes and edges, now containing
# calculated network attributes
var2_with_CAPS_node_list <- get.data.frame(var2_with_CAPS, what = "vertices")

#Write Node List
write.csv(var2_with_CAPS_node_list, "~/SNArRDJF/Tese_Pedro/Completa/Banco/var2_with_CAPS_node_list_df.csv")

# Determine a community for each edge. If two nodes belong to the
# same community, label the edge with that community. If not,
# the edge community value is 'NA'
var2_with_CAPS_edge_list <- get.data.frame(var2_with_CAPS, what = "edges")

#Write Node List
write.csv(var2_with_CAPS_edge_list, "~/SNArRDJF/Tese_Pedro/Completa/Banco/var2_with_CAPS_edge_list.csv")

#Write Node List
write.csv(var2_with_CAPS_edge_list, "~/SNArRDJF/Tese_Pedro/Completa/Banco/var2_with_CAPS_edge_list.csv")
```


#Saving objects with new variables and changes

```r
save.image("~/SNArRDJF/Robject/var2_with_CAPS_dataframe.RData")  
```
