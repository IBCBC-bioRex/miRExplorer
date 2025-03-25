background=HTML("
    body {
      background-color: #f0f8ff;
    }
    .main-panel {
      background-color: #ffffff;
      padding: 15px;
      border-radius: 10px;
      box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
    }
    .filter-panel {
      position: sticky;
      top: 0;
      z-index: 1000;
      background-color: #ffffff;
      padding: 15px;
      box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
      margin-bottom: 15px;
    }
    /* Cambia il background solo dei riquadri di visualizzazione della rete */
    .vis-network {
      background-color: white !important;
    }
  ")


#lista_proveforti=c("Luciferase.Reporter.Assay", "qPCR", "Western.Blot")
#sceltaproveforti=c("Luciferase reporter assay"="Luciferase.Reporter.Assay","qRT PCR"="qPCR","Western blot"="Western.Blot")
#sceltaprovedeboli=c("Microarray"="Microarrays","Next Generation Sequencing"="Next.Generation.Sequencing","pSILAC"="pSILAC")
#lista_provedeboli=c("Microarrays", "Next.Generation.Sequencing", "pSILAC")

lista_proveforti=c("Luciferase.Reporter.Assay", "Reporter.assay", "qPCR", "In.Situ.Hybridization")
sceltaproveforti = c("In Situ Hybridization" = "In.Situ.Hybridization",
                     "Luciferase reporter assay" = "Luciferase.Reporter.Assay",
                     "qPCR" = "qPCR",
                     "Reporter assay" = "Reporter.assay")
lista_provedeboli=c("Microarray", "Sequencing", "Western.Blot", "Proteomics", "Immunohistochemistry", "Flow.Cytometry", 
                    "Immunofluorescence", "Northern.Blot", "ELISA", "Immunoprecipitation", "ChIP", "ChIP.seq", "Immunocytochemistry")
sceltaprovedeboli = c("ChIP" = "ChIP",
                      "ChIP-seq" = "ChIP.seq",
                      "ELISA" = "ELISA",
                      "Flow Cytometry" = "Flow.Cytometry",
                      "Immunocytochemistry" = "Immunocytochemistry",
                      "Immunofluorescence" = "Immunofluorescence",
                      "Immunohistochemistry" = "Immunohistochemistry",
                      "Immunoprecipitation" = "Immunoprecipitation",
                      "Microarray" = "Microarray",
                      "Northern Blot" = "Northern.Blot",
                      "Proteomics" = "Proteomics",
                      "Sequencing" = "Sequencing",
                      "Western Blot" = "Western.Blot")

documentation=renderUI({
  tagList(
    h4("Data"),
    p("There are currently several biological databases containing information on miRNAs and their association with diseases (HMDD[1], MirCancer[2], dbDEMC[3]) or target genes (MirTarBase[4], TarBase[5])."),
    p("As these databases are often difficult to access, a user-friendly web application was developed to provide an overview of the role of miRNAs in the literature and their interactions with genes and diseases by merging information from the previously mentioned databases."),
    
    br(),
    
    p(HTML("<b><i>ABOUT miRNA-DISEASE INTERACTION:</i></b>")),
    DTOutput("tab_merge"),
    br(),
    p(HTML("<b><i>ABOUT miRNA-GENE INTERACTION:</i></b>")),
    DTOutput("tab_mirtarbase"),
    
    # I riferimenti esistenti
    h4("References"),
    tags$ol(
      tags$li("Cui C, Zhong B, Fan R, Cui Q. (2024). \"HMDD v4.0: a database for experimentally supported human microRNA-disease associations\". Nucleic Acids Research, 52(D1): D1327-D1332. doi:10.1093/nar/gkad717."),
      tags$li("Xie B, Ding Q, Han H, Wu D. miRCancer: a microRNA-cancer association database constructed by text mining on literature. Bioinformatics. 2013 Mar 1;29(5):638-44. doi: 10.1093/bioinformatics/btt014. Epub 2013 Jan 16. PMID: 23325619."),
      tags$li("Feng Xu, Yifan Wang, Yunchao Ling, Chenfen Zhou, Haizhou Wang, Andrew E Teschendorff, Yi Zhao, Haitao Zhao, Yungang He, Guoqing Zhang, Zhen Yang. dbDEMC 3.0: Functional exploration of differentially expressed miRNAs in cancers of human and model organisms. Genomics, Proteomics & Bioinformatics. 2022, 20(3):446-454. [ doi: 10.1016/j.gpb.2022.04.0062022 ]"),
      tags$li("Hsu SD, Lin FM, Wu WY, Liang C, Huang WC, Chan WL, Tsai WT, Chen GZ, Lee CJ, Chiu CM, Chien CH, Wu MC, Huang CY, Tsou AP, Huang HD (2011). \"miRTarBase: a database curates experimentally validated microRNA-target Interactions\". Nucleic Acids Research, 39(Database issue): D163-9. doi:10.1093/nar/gkq1107. PMC 3013699. PMID 21071411."),
      tags$li("Giorgos Skoufos, Panos Kakoulidis, Spyros Tastsoglou, Elissavet Zacharopoulou, Vasiliki Kotsira, Marios Miliotis, Galatea Mavromati, Dimitris Grigoriadis, Maria Zioga, Angeliki Velli, Ioanna Koutou, Dimitra Karagkouni, Steve Stavropoulos, Filippos S Kardaras, Anna Lifousi, Eustathia Vavalou, Armen Ovsepian, Anargyros Skoulakis, Sotiris K Tasoulis, Spiros V Georgakopoulos, Vassilis P Plagianakos, Artemis G Hatzigeorgiou, TarBase-v9.0 extends experimentally supported miRNA–gene interactions to cell-types and virally encoded miRNAs, Nucleic Acids Research, Volume 52, Issue D1, 5 January 2024, Pages D304–D310, https://doi.org/10.1093/nar/gkad1071")
    ))
})

TABELLA1_DOC <- renderDT({
  data1 <- data.frame(
    DATABASE = c("HMDD", "Mircancer", "dbDEMC"),
    ASSOCIATIONS = c(53530, 9080, 33610),
    miRNAs = c(1911, 1034, 2203),
    DISEASE = c(2360, 129, 32),
    REFERENCES=c(37090, 7271, 175)
  )
  datatable(data1,
            options = list(
              paging = FALSE,          
              searching = FALSE,       
              ordering = FALSE,         
              info = FALSE,             
              lengthChange = FALSE     
            ),
            rownames = FALSE          
  )
})

TABELLA2_DOC <- renderDT({
  data2 <- data.frame(
    DATABASE = c("miRTarBase", "TarBase"),
    ASSOCIATIONS = c(49158, 152760),
    miRNAs = c(757, 396),
    GENES = c(10316, 17618),
    REFERENCES=c(7619, 946)
    )
  datatable(data2,
            options = list(
              paging = FALSE,          
              searching = FALSE,       
              ordering = FALSE,         
              info = FALSE,             
              lengthChange = FALSE     
            ),
            rownames = FALSE          
  )
})


choosemiRNA <- list(
  maxItems = 10, 
  placeholder = "Choose miRNA",
  create = TRUE
)

choosedisease <- list(
  maxItems = 10, 
  placeholder = "Choose Disease",
  create = TRUE
)

network_color <- function(g, lista){
  V(g)$title <- V(g)$name
  V(g)$color <- ifelse(V(g)$name %in% lista, "blue", "red")  
  
  return(g)
}
  

network_table <- function(g){
  degree_centrality <- degree(g)
  betweenness_centrality <- round(betweenness(g), 5)
  closeness_centrality <- round(closeness(g), 5)
  
  summary_table <- data.frame(
    Node = V(g)$name,
    Degree = degree_centrality,
    Betweenness = betweenness_centrality,
    Closeness = closeness_centrality
  )
}

metrics <- function(summary_table){
  renderDT({
  datatable(summary_table, options = list(paging = TRUE, searching = TRUE, ordering = TRUE), rownames = FALSE)
})}

download_csv <- function(summary_table, stringa){
  downloadHandler(
  filename = function() { paste(stringa, Sys.Date(), ".csv", sep = "") },
  content = function(file) { write.csv(summary_table, file, row.names = FALSE) }
)}

download_png <- function(g, stringa){
  vis_data=toVisNetworkData(g)
  vis <- visNetwork(vis_data$nodes, vis_data$edges)
  visExport(vis, type='png', name=stringa)
  }


visualizz_network <- function(g){
  vis_data=toVisNetworkData(g)
  return (visNetwork(vis_data$nodes, vis_data$edges) %>% visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, collapse = TRUE, manipulation = TRUE))
}


#funzione per enrichment
enrichment_test <- function(pathway_genes, gene_list, universe) {
  gene_intersection <- intersect(pathway_genes, gene_list)
  m <- length(gene_intersection)  # Geni in comune
  n <- length(gene_list)         # Geni nella lista di interesse
  N <- length(universe)          # Totale dei geni nell'universo
  K <- length(pathway_genes)     # Geni nel pathway
  
  p_value <- phyper(m-1, K, N-K, n, lower.tail = FALSE)
  
  #p_value <- round(p_value, 6)
  
  return(p_value)
}


enrichment_test2 <- function(pathway_genes, gene_list, universe) {
  gene_intersection <- intersect(pathway_genes, gene_list)
  m <- length(gene_intersection)  # Geni in comune
  n <- length(gene_list)         # Geni nella lista di interesse
  N <- length(universe)          # Totale dei geni nell'universo
  K <- length(pathway_genes)     # Geni nel pathway
  
  Fold_enrichment <- (m/K)/(n/N)
  
  Fold_enrichment <- round(Fold_enrichment, 6)
  
  return(Fold_enrichment)
}

enrichment_test3 <- function(pathway_genes, gene_list) {
  gene_intersection <- intersect(pathway_genes, gene_list)
  m <- length(gene_intersection)  # Geni in comune
  K <- length(pathway_genes)     # Geni nel pathway
  paste(m,K,sep="/")
}


