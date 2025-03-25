library(shiny)
library(DT) 
library(dplyr)
library(readxl)
library(igraph)
library(ggplot2)
library(visNetwork)
library(pheatmap)
library(plotly)

merge_ <- read_excel("data/dataset_shinyapp.xlsx")
sorted_Diseases_merge_ <- sort(unique(merge_$disease))
sorted_miRNA_merge_ <- sort(unique(merge_$miRNA))

DEMC_ <- read_excel("data/DEMC_.xlsx", sheet = "DA IMPORTARE")

sorted_Diseases_DEMC_ <- sort(unique(DEMC_$disease))
sorted_miRNA_DEMC_ <- sort(unique(DEMC_$miRNA))

tab_parentschildren <- read_excel("data/tab_parents-children.xlsx")
tab_parentschildren <- tab_parentschildren %>%
  filter(obsolete != TRUE)

#mirna-gene interaction
mirtarbase <- read_excel("data/disease-miRNA_MERGE.xlsx", sheet = "disease-miRNA_MERGE")
universe <- unique(mirtarbase$Target.Gene)

#ontology ed enrichment
ontology <- read_excel("data/reactome_ontology.xlsx", sheet = "splittati")
ontology <- ontology[, c("genes", "name")]  
ontology <- na.omit(ontology)

ontology2 <- merge(ontology, mirtarbase[,c("Target.Gene","miRNA")], by.x = "genes", by.y = "Target.Gene")
ontology2=ontology2[,c("name","genes","miRNA")]


tabella_3p <- read_excel("data/disease-miRNA_MERGE.xlsx", sheet = "tabella_3p")
tabella_5p <- read_excel("data/disease-miRNA_MERGE.xlsx", sheet = "tabella_5p")
tabella_altro <- read_excel("data/disease-miRNA_MERGE.xlsx", sheet = "tabella_altro")

tab_metabolism <- read_excel("data/tab_metabolism.xlsx")

source("config.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server=function(input,output,session){
  server(input,output,session,merge_,DEMC_,tab_parentschildren, sorted_Diseases_merge_,
         sorted_Diseases_DEMC_,sorted_miRNA_merge_,sorted_miRNA_DEMC_,mirtarbase[,c("Target.Gene","miRNA")],
         tabella_3p,tabella_5p,tabella_altro,tab_metabolism,universe,ontology,ontology2)
  
})
