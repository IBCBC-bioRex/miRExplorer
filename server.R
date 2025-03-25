server <- function(input,output,
                   session,
                   merge_,DEMC_,
                   tab_parentschildren,sorted_Diseases_merge_,sorted_Diseases_DEMC_,
                   sorted_miRNA_merge_,sorted_miRNA_DEMC_,mirtarbase, tabella_3p,tabella_5p,tabella_altro,
                   tab_metabolism,universe,reactome_ontology,reactome_ontology2) {
  
####################################################
#SECTION 1: DOCUMENTATION
output$references <- documentation
output$tab_merge <- TABELLA1_DOC
output$tab_mirtarbase <- TABELLA2_DOC
####################################################
# Possibility to upload own dataset
uploaded_data <- reactive({
  req(input$file)
  if (grepl("\\.csv$", input$file$name)) {
    read.csv(input$file$datapath)
  } else if (grepl("\\.xlsx$", input$file$name)) {
    readxl::read_excel(input$file$datapath)
  }
})

# Reactive function for selecting the dataset (merge_ or DEMC_)
selected_data <- reactive({
  if (input$dataset_choice == "merge_") {
    return(merge_)
  } else if (input$dataset_choice == "DEMC_") {
    return(DEMC_)
  } else if (input$dataset_choice == "Uploaded") {
    return(uploaded_data())
  }
})

# Dynamically update miRNA and Disease dropdown based on the selected dataset
observeEvent(input$dataset_choice, {
  # Update disease choices based on selected dataset
  if (input$dataset_choice == "merge_") {
    updateSelectizeInput(session, "selected_Disease",
                         choices = sorted_Diseases_merge_,  # Diseases from merge_
                         selected = "", 
                         server = TRUE)
  } else if (input$dataset_choice == "DEMC_") {
    updateSelectizeInput(session, "selected_Disease",
                         choices = sorted_Diseases_DEMC_,  # Diseases from DEMC_
                         selected = "", 
                         server = TRUE)
  } else if (input$dataset_choice == "Uploaded") {
    updateSelectizeInput(session, "selected_Disease",
                         choices = sort(unique(uploaded_data()$disease)),
                         selected = "", 
                         server = TRUE)
  }
  
  # Update miRNA choices based on selected dataset
  if (input$dataset_choice == "merge_") {
    updateSelectizeInput(session, "selected_mirna",
                         choices = sorted_miRNA_merge_,  # miRNAs from merge_
                         selected = "", 
                         server = TRUE)
  } else if (input$dataset_choice == "DEMC_") {
    updateSelectizeInput(session, "selected_mirna",
                         choices = sorted_miRNA_DEMC_,  # miRNAs from DEMC_
                         selected = "", 
                         server = TRUE)
  }else if (input$dataset_choice == "Uploaded") {
    updateSelectizeInput(session, "selected_mirna",
                         choices = sort(unique(uploaded_data()$miRNA)),
                         selected = "", 
                         server = TRUE)
  }
})

# Dynamically update the select input based on the selection (miRNA or Disease)
output$dynamic_select <- renderUI({
  if (input$selection == "miRNA") {
    selectizeInput("selected_mirna", "Select miRNA:",
                   choices = NULL,  # Initially set to NULL, will be updated via observeEvent
                   selected = "",
                   multiple = TRUE)
  } else if (input$selection == "Disease") {
    selectizeInput("selected_Disease", "Select Disease:",
                   choices = NULL,  # Initially set to NULL, will be updated via observeEvent
                   selected = "",
                   multiple = TRUE)
  }
})

# Populate the choices for miRNA immediately after ‘miRNA’ is selected
observeEvent(input$selection, {
  if (input$selection == "miRNA") {

     if (input$dataset_choice == "merge_") {
        updateSelectizeInput(session, "selected_mirna",
                             choices = sorted_miRNA_merge_,  # miRNAs from merge_
                             server = TRUE)
      } else if (input$dataset_choice == "DEMC_") {
        updateSelectizeInput(session, "selected_mirna",
                             choices = sorted_miRNA_DEMC_,  # miRNAs from DEMC_
                             server = TRUE)
      } else if (input$dataset_choice == "Uploaded") {
        updateSelectizeInput(session, "selected_mirna",
                             choices = sort(unique(uploaded_data()$miRNA)),
                             server = TRUE)
      } 
  }
  
  if (input$selection == "Disease") {
    # If the user has uploaded a file, load Diseases from the uploaded file
 if (input$dataset_choice == "merge_") {
        updateSelectizeInput(session, "selected_Disease",
                             choices = sorted_Diseases_merge_,  # Diseases from merge_
                             server = TRUE)
      } else if (input$dataset_choice == "DEMC_") {
        updateSelectizeInput(session, "selected_Disease",
                             choices = sorted_Diseases_DEMC_,  # Diseases from DEMC_
                             server = TRUE)
      }else if (input$dataset_choice == "Uploaded") {
        updateSelectizeInput(session, "selected_Disease",
                             choices = sort(unique(uploaded_data()$disease)),
                             server = TRUE)
      }
  }
})

# Handle changes dynamically when switching between "Disease" and "miRNA"
observeEvent(input$selected_Disease, {
  if (length(input$selected_Disease) > 0) {
    updateSelectizeInput(session, "selected_mirna",
                         choices = NULL,  # Clear miRNA choices until updated
                         selected = NULL, 
                         server = TRUE)
  }
})

observeEvent(input$selected_mirna, {
  if (length(input$selected_mirna) > 0) {
    updateSelectizeInput(session, "selected_Disease",
                         choices = NULL,  # Clear Disease choices until updated
                         selected = NULL, 
                         server = TRUE)
  }
})


####################################################
# SECTION 2: CONTEXT
output$disease_selector <- renderUI({
  if (length(input$selected_Disease) > 1) {
    selectizeInput("specific_disease", "Select a Disease to View:",
                   choices = input$selected_Disease,
                   selected = input$selected_Disease[1],  
                   multiple = FALSE,
                   options = list(create = FALSE))
  } else {
    return(NULL)  
  }
})

output$miRNA_disease_selector <- renderUI({
  if (length(input$selected_mirna) > 0) {
    diseases_for_mirna <- unique(selected_data()[selected_data()$miRNA %in% input$selected_mirna, "disease"])
    if (length(diseases_for_mirna) > 0) {
      selectizeInput("selected_mirna_disease", "Select Disease for Selected miRNA:",
                     choices = diseases_for_mirna,
                     selected = diseases_for_mirna[1],  
                     multiple = FALSE,
                     options = list(create = FALSE))
    } else {
      return(NULL)  
    }
  } else {
    return(NULL)  
  }
})

# Rendering the plot based on the selected disease
output$graph_plot <- renderPlot({
  disease_to_plot <- if (!is.null(input$selected_mirna_disease)) {
    input$selected_mirna_disease 
  } else if (length(input$selected_Disease) > 1 && !is.null(input$specific_disease)) {
    input$specific_disease 
  } else {
    input$selected_Disease  
  }
  
  if (length(disease_to_plot) > 0) {
    doid <- selected_data()[selected_data()$disease %in% disease_to_plot, "DOID"]
    doid <- unique(doid)
    
    if (length(doid) == 0) {
      return(NULL) 
    }
    
    network_data <- data.frame(
      from = tab_parentschildren$parents,
      to = tab_parentschildren$id
    )
    
    g <- graph_from_data_frame(d = network_data, directed = TRUE)
    
    parents_total <- tab_parentschildren$parents
    node_data <- tab_parentschildren[tab_parentschildren$id %in% doid, ]
    if (nrow(node_data) == 0) {
      return(NULL)
    }
    
    children <- node_data$children
    ancestors <- node_data$ancestors
    
    children <- if (!is.na(children) && length(children) > 0) strsplit(children, split = "; ")[[1]] else character(0)
    ancestors <- if (!is.na(ancestors) && length(ancestors) > 0) strsplit(ancestors, split = "; ")[[1]] else character(0)
    
    if (!exists("parents_total") || is.null(parents_total)) {
      parents_total <- character(0)
    } else {
      parents_total <- strsplit(parents_total, split = "; ")[[1]]
    }
    
    risultati <- vapply(children, function(x) x %in% parents_total, logical(1))
    
    children_cartelle <- children[risultati]
    children_foglie <- children[!risultati]
    
    children_to_use <- na.omit(c(children_cartelle, children_foglie))
    
    doid_to_name <- setNames(tab_parentschildren$name, tab_parentschildren$id)
    
    existing_nodes <- V(g)$name  
    
    valid_vids <- intersect(c(ancestors, children_to_use), existing_nodes)
    
    sotto_grafo <- induced_subgraph(g, vids = valid_vids)
    
    V(sotto_grafo)$color <- ifelse(V(sotto_grafo)$name %in% children_foglie, "green", "red")
    
    V(sotto_grafo)$label <- doid_to_name[V(sotto_grafo)$name]
    
    plot(sotto_grafo, vertex.size = 10, vertex.label.cex = 0.8)
  }
})

####################################################
# SECTION 3: MIRNA-DISEASE INTERACTION

# Reactive function to get the filtered data from the manually selected dataset
get_filtered_data <- reactive({
  data_to_use <- selected_data() 
  
  #apply filter for count_threshold
  count_filter <- input$count_threshold
  data_to_use <- data_to_use[data_to_use$Count >= count_filter, ]
  
  # Apply miRNA filter if 'miRNA' is selected and miRNA(s) are chosen
  if (input$selection == "miRNA" && length(input$selected_mirna) > 0) {
    data_to_use <- data_to_use %>% filter(miRNA %in% input$selected_mirna)
    
    # Apply Disease filter if 'Disease' is selected and Disease(s) are chosen
  } else if (input$selection == "Disease" && length(input$selected_Disease) > 0) {
    data_to_use <- data_to_use %>% filter(disease %in% input$selected_Disease)
  }
  
  return(data_to_use)
  })

output$miRNA1_info <- renderText({
  filtered_data <- get_filtered_data()
  if (is.null(filtered_data)) return(NULL)
  
  num_unique_miRNA <- length(unique(filtered_data$miRNA))  
  result <- paste("Total unique miRNA: ", num_unique_miRNA)
})

#visualisation of the miRNA-disease interaction table
output$miRNA_Disease_Interaction <- renderDT({
  req(get_filtered_data())
  #print(which(colnames(get_filtered_data()) == "Count"))
  datatable(get_filtered_data(), 
            options = list(order = list(4, 'desc')))
  })

#download button of the miRNA-disease interaction table
output$download_Disease_data <- downloadHandler(
  filename = function() { paste("miRNA_Disease_Interaction", Sys.Date(), ".csv", sep = "") },
  content = function(file) { write.csv(get_filtered_data(), file, row.names = FALSE) }
  ) 

#NETWORK
#creation of the miRNA-disease interaction table network
output$network_mirna_Disease <- renderVisNetwork({
  filtered_data <- get_filtered_data()
  if (is.null(filtered_data)) return(NULL)
  
  #add a slider input for filtering Count values
  count_filter <- input$count_threshold
  filtered_data <- filtered_data[filtered_data$Count >= count_filter, ]
  
  if (nrow(filtered_data) == 0) return(NULL)
  
  #create a dataframe of links with miRNA, disease and Count as a label
  edges <- data.frame(
    from = filtered_data$miRNA, #miRNA as node of origin
    to = filtered_data$disease, #disease as node of destination 
    label = as.character(filtered_data$Count) #count as interation number
    )
  
  g <- graph_from_data_frame(d = edges, directed = FALSE)
  lista <- filtered_data$miRNA
  g <- network_color(g, lista)
  
  summary_table <- network_table(g)
  
  #visualisation of the miRNA-disease network metrics table
  output$network_metrics_mirna_Disease_ui <- renderUI({
    tagList(
      DTOutput("network_metrics_mirna_Disease")
      )
    })
  output$network_metrics_mirna_Disease <- metrics(summary_table)
  
  #download of the miRNA-disease network metrics table
  stringa <- "miRNA_Disease_Network_Metrics"
  output$download_metrics_mirna_Disease <- download_csv(summary_table, stringa)
  
  vis_data=toVisNetworkData(g)
  
  #download of the miRNA-disease network
  output$download_network_mirna_Disease <- downloadHandler(
    filename = function()
      { paste("miRNA_Disease_Network", Sys.Date(), ".html", sep = "") },
    content = function(con) {
      visNetwork(vis_data$nodes, vis_data$edges) %>% visOptions(highlightNearest = TRUE, 
                                                                  nodesIdSelection = TRUE, 
                                                                  collapse = TRUE, 
                                                                  manipulation = TRUE)  %>% visSave (con)}
    )
  #visualisation of the miRNA-disease network
  return(visualizz_network(g))
  })
####################################################
#SECTION 4: MIRNA-GENE INTERACTION 
  
#TABLE
#filter to select/deselect the type of experiment (strong/weak test)
observeEvent(input$select_all_strong, {
  updateCheckboxGroupInput(session, "selected_strong_tests",
                           selected = lista_proveforti)
  })

observeEvent(input$select_all_weak, {
  updateCheckboxGroupInput(session, "selected_weak_tests",
                           selected = lista_provedeboli)
  })

observeEvent(input$deselect_all_strong, {
  if (all(lista_proveforti %in% input$selected_strong_tests)) {
    updateCheckboxGroupInput(session, "selected_strong_tests", selected = character(0))
    }
  })

observeEvent(input$deselect_all_weak, {
  if (all(lista_provedeboli %in% input$selected_weak_tests)) {
    updateCheckboxGroupInput(session, "selected_weak_tests", selected = character(0))
    }
  })
  
#reactive function to get miRNAs from the previous table of miRNA-disease interaction to extract them from the three dataframe ‘table-3p’, ‘table_5p’ and ‘table_other’
get_mirna_gene_Interaction <- reactive({
  selected_mirna <- unique(get_filtered_data()$miRNA)
  if (length(selected_mirna) == 0) return(NULL)
  
  # Controlla se i valori hanno già il suffisso -3p o -5p
  has_suffix <- grepl("-3p$|-5p$", selected_mirna)
  
  mirna_3p_data <- tabella_3p[tabella_3p$miRNA %in% ifelse(has_suffix, selected_mirna, paste0(selected_mirna, "-3p")), ]
  mirna_5p_data <- tabella_5p[tabella_5p$miRNA %in% ifelse(has_suffix, selected_mirna, paste0(selected_mirna, "-5p")), ]
  mirna_Other_data <- tabella_altro[tabella_altro$miRNA %in% selected_mirna, ]
  
  #Filtro sulla variabile 'Count' applicato a tutte le tabelle
  mirna_3p_data <- mirna_3p_data[as.numeric(mirna_3p_data$Count) >= input$min_count_gene, ]
  mirna_5p_data <- mirna_5p_data[as.numeric(mirna_5p_data$Count) >= input$min_count_gene, ]
  mirna_Other_data <- mirna_Other_data[as.numeric(mirna_Other_data$Count) >= input$min_count_gene, ]
  
  #user-selected strong and weak tests are taken
  selected_strong_tests <- input$selected_strong_tests
  selected_weak_tests <- input$selected_weak_tests
  
  if (length(selected_strong_tests) > 0) {
    for (test in selected_strong_tests) {
      if (test != "Select.All" && test %in% colnames(mirna_3p_data)) {
        mirna_3p_data <- mirna_3p_data[mirna_3p_data[[test]] == 1, ]
        mirna_5p_data <- mirna_5p_data[mirna_5p_data[[test]] == 1, ]
        mirna_Other_data <- mirna_Other_data[mirna_Other_data[[test]] == 1, ]
      }
      }}
  
  if (length(selected_weak_tests) > 0) {
    for (test in selected_weak_tests) {
      if (test != "Select.All" && test %in% colnames(mirna_3p_data)) {
        mirna_3p_data <- mirna_3p_data[mirna_3p_data[[test]] == 1, ]
        mirna_5p_data <- mirna_5p_data[mirna_5p_data[[test]] == 1, ]
        mirna_Other_data <- mirna_Other_data[mirna_Other_data[[test]] == 1, ]
      }
      }}
  
  #a list of three data frames containing filtered data for the selected miRNAs is returned
  list(mirna_3p_data = mirna_3p_data, mirna_5p_data = mirna_5p_data, mirna_Other_data = mirna_Other_data)
  })

#visualisation of miRNA-gene interaction in three different tables depending on input$mirna_type
output$miRNA_Gene_Interaction_Table <- renderDT({
  mirna_gene_data <- get_mirna_gene_Interaction()
  if (is.null(mirna_gene_data)) return(NULL)
  
  if (input$mirna_type == "miRNA-3p") {
    data <- mirna_gene_data$mirna_3p_data
    } else if (input$mirna_type == "miRNA-5p") {
      data <- mirna_gene_data$mirna_5p_data
      } else {
        data <- mirna_gene_data$mirna_Other_data
        }
  
  #filter for Count
  data <- data[data$Count >= input$min_count_gene, ]
  
  data <- data[, c("miRNA", "Target.Gene", "Count", "References.PMID")]
  colnames(data)[colnames(data) == "Target.Gene"] <- "Target Gene"
  colnames(data)[colnames(data) == "References.PMID"] <- "PMID"
  
  datatable(data, options = list(paging = TRUE, searching = TRUE, order= list(3, 'desc')))
  })

output$genes2_info <- renderText({
  mirna_gene_data <- get_mirna_gene_Interaction()
  if (is.null(mirna_gene_data)) return(NULL)
  
  if (input$mirna_type == "miRNA-3p") {
    data <- mirna_gene_data$mirna_3p_data
  } else if (input$mirna_type == "miRNA-5p") {
    data <- mirna_gene_data$mirna_5p_data
  } else {
    data <- mirna_gene_data$mirna_Other_data
  }
  
  data <- data[data$Count >= input$min_count_gene, ]
  num_unique_genes <- length(unique(data$Target.Gene))
  paste("Total unique target genes:", num_unique_genes)
  })


#download of the miRNA-gene interaction table
output$download_mirna_gene_data <- downloadHandler(
  filename = function() {
    paste("miRNA_gene_interaction_", Sys.Date(), ".csv", sep = "")
    },
  content = function(file) {
    
    #get filtered data by selection (miRNA-3p, miRNA-5p or other)
    mirna_gene_data <- get_mirna_gene_Interaction()
    if (is.null(mirna_gene_data)) return(NULL)
    
    if (input$mirna_type == "miRNA-3p") {
      data <- mirna_gene_data$mirna_3p_data
      } else if (input$mirna_type == "miRNA-5p") {
        data <- mirna_gene_data$mirna_5p_data
        } else {
          data <- mirna_gene_data$mirna_Other_data
          }
    
    data <- data[, c("miRNA", "Target.Gene", "Count", "References.PMID")]
    colnames(data)[colnames(data) == "Target.Gene"] <- "Target Gene"
    colnames(data)[colnames(data) == "References.PMID"] <- "PMID"
    
    write.csv(data, file, row.names = FALSE)
    }
  )
  
#NETWORK
output$network_mirna_gene <- renderVisNetwork({
  mirna_gene_data <- get_mirna_gene_Interaction()
  if (is.null(mirna_gene_data)) return(NULL)
  
  mirna_3p_data <- mirna_gene_data$mirna_3p_data
  mirna_5p_data <- mirna_gene_data$mirna_5p_data
  mirna_Other_data <- mirna_gene_data$mirna_Other_data
  
  #filter data according to a user-specified minimum count value
  mirna_3p_data <- mirna_3p_data[as.numeric(mirna_3p_data$Count) >= input$min_count_gene, ]
  mirna_5p_data <- mirna_5p_data[as.numeric(mirna_5p_data$Count) >= input$min_count_gene, ]
  mirna_Other_data <- mirna_Other_data[as.numeric(mirna_Other_data$Count) >= input$min_count_gene, ]
  
  #create interaction dataframe for network visualisation
  d.1 = rbind(
    data.frame(from = mirna_3p_data$miRNA, to = mirna_3p_data$Target.Gene,
               label = as.character(mirna_3p_data$Count)),
    data.frame(from = mirna_5p_data$miRNA, to = mirna_5p_data$Target.Gene,
               label = as.character(mirna_5p_data$Count)),
    data.frame(from = mirna_Other_data$miRNA, to = mirna_Other_data$Target.Gene,
               label = as.character(mirna_Other_data$Count)))
  d.1$font.size = 20
  
  #create the graph with nodes and arcs
  g <- graph_from_data_frame(
    d = d.1,
    vertices = unique(c(mirna_3p_data$miRNA, mirna_5p_data$miRNA,
                        mirna_3p_data$Target.Gene, mirna_5p_data$Target.Gene,
                        mirna_Other_data$miRNA, mirna_Other_data$Target.Gene)),
    directed = TRUE
    )
  
  V(g)$color <- ifelse(grepl("-3p$|-5p$", V(g)$name), "green",
                       ifelse(V(g)$name %in% mirna_gene_data$mirna_Other_data$miRNA, "green", "red"))
  V(g)$shape <- ifelse(grepl("-3p$|-5p$", V(g)$name), "square", "circle")
  V(g)$title <- V(g)$name
  
  #calculate and visualisation mirna-gene interaction network metrics
  summary_table <- network_table(g)
  output$network_metrics_mirna_gene_ui <- renderUI({
    tagList(
      DTOutput("network_metrics")
      )
    })
  
  output$network_metrics <- metrics(summary_table)
  
  #download mirna-gene interaction network metrics
  stringa <- "Network_Metrics"
  output$download_metrics <- download_csv(summary_table, stringa)
  
  vis_data=toVisNetworkData(g)
  
  #download of the miRNA-gene interaction network
  output$download_network <- downloadHandler(
    filename = function()
      { paste("network_mirna_gene", Sys.Date(), ".html", sep = "") },
    content = function(con) {
      visNetwork(vis_data$nodes, vis_data$d.1) %>% visOptions(highlightNearest = TRUE,
                                                              nodesIdSelection = TRUE,
                                                              collapse = TRUE,
                                                              manipulation = TRUE)  %>% visSave (con)}
    )
  #visualisation of the miRNA-gene interaction network
  return(visualizz_network(g))
  })
  
#ENRICHMENT
get_ontology <- reactive({
  if (!is.null(input$file_input)) {  # Verifica se è stato caricato un file
    # Se il file è un CSV
    if (grepl("\\.csv$", input$file_input$name)) {
      ontology_data <- read.csv(input$file_input$datapath)
    }
    
    # Se il file è un XLSX
    else if (grepl("\\.xlsx$", input$file_input$name)) {
      ontology_data <- readxl::read_xlsx(input$file_input$datapath)
    }
    
    # Se il file non è né CSV né XLSX, mostra un messaggio di errore
    else {
      stop("File format not supported. Please upload a CSV or XLSX file.")
    }
  } else {
    # Se non è stato caricato alcun file, usa il dataset predefinito
    ontology_data <- reactome_ontology  # Assicurati che 'reactome_ontology' sia definito
  }
  
  return(ontology_data)
})

get_target_genes <- function() {
  mirna_gene_data <- get_mirna_gene_Interaction()
  if (is.null(mirna_gene_data)) return(NULL)
  
  mirna_3p_data <- mirna_gene_data$mirna_3p_data
  mirna_5p_data <- mirna_gene_data$mirna_5p_data
  mirna_Other_data <- mirna_gene_data$mirna_Other_data
  
  #filtering of data according to the minimum value of ‘Count’ provided by the input
  mirna_3p_data <- mirna_3p_data[as.numeric(mirna_3p_data$Count) >= input$min_count_gene, ]
  mirna_5p_data <- mirna_5p_data[as.numeric(mirna_5p_data$Count) >= input$min_count_gene, ]
  mirna_Other_data <- mirna_Other_data[as.numeric(mirna_Other_data$Count) >= input$min_count_gene, ]
  
  #extraction of unique target genes from filtered tables
  target_genes_3p <- mirna_3p_data$Target.Gene
  target_genes_5p <- mirna_5p_data$Target.Gene
  target_genes_other <- mirna_Other_data$Target.Gene
  
  mirna_3p <- unique(mirna_3p_data$miRNA)
  mirna_5p <- unique(mirna_5p_data$miRNA)
  mirna_Other <- unique(mirna_Other_data$miRNA)
  
  #creation of the unified list of target genes and miRNAs
  target_genes <- c(target_genes_3p, target_genes_5p, target_genes_other)
  gene_list <- unique(target_genes)
  all_mirnas <- unique(c(mirna_3p, mirna_5p, mirna_Other))
  
  
  ontology=get_ontology()
  
  #associating gene-pathway interactions with reference miRNA
  get_ontology2= reactive({
    if (!is.null(input$file_input)) {  # Verifica se è stato caricato un file
      # Se il file è un CSV
      ontology_merged <- merge(ontology, mirtarbase, by.x = "genes", by.y = "Target.Gene")
      ontology_merged=ontology_merged[,c("name","genes","miRNA")]
      return(ontology_merged)
    } else {
      return(reactome_ontology2)  
    }

  })
  ontology2=get_ontology2()
  
  ontology3=ontology2[ontology2$miRNA %in% all_mirnas,]
  
  
  ontology <- ontology %>%
    group_by(name) %>%  
    summarize(geni = list(genes), dimension = length(genes) )
  
  #hypergeometric analysis
  ontology$p_value=sapply(ontology$geni, function(x) enrichment_test(x, gene_list, universe))
  ontology$coverage <-sapply(ontology$geni, function(x) enrichment_test3(x, gene_list))
  ontology$Fold_enrichment <-sapply(ontology$geni, function(x) enrichment_test2(x, gene_list,universe))
  
  #calculation of the correct value for multiple testing with the Benjamini-Hochberg method and filter for pvalue < 0.05
  ontology$q_value <- p.adjust(ontology$p_value, method = "BH")
  enriched_pathways <- ontology[ontology$p_value < 0.05, ]
  
  
  
  
  #merge of interactions by name (pathway name)
  enriched_pathways <- merge(enriched_pathways, ontology3, by.x = "name", by.y = "name")
  #we take the genes that are left over from the previous filtering
  enriched_pathways=enriched_pathways[enriched_pathways$genes %in% gene_list,]
  
  enriched_pathways <- enriched_pathways[, !names(enriched_pathways) %in% c("geni")]
  enriched_pathways <- enriched_pathways[, c("miRNA", "name", "p_value", "q_value", "coverage", "Fold_enrichment")]
  
  enriched_pathways <- enriched_pathways[!duplicated(enriched_pathways), ]
  
  # Filtro aggiuntivo per il "Pathway size: Min." (coverage >= valore selezionato)
  min_size <- input$min_pathway_size
  enriched_pathways <- enriched_pathways[enriched_pathways$coverage >= min_size, ]
  
  return(enriched_pathways)
}


#visualisation of the enrichment table
output$enrichment_results_table <- renderDT({
  enriched_pathways <- get_target_genes()
  if (is.null(enriched_pathways)) return(NULL)
  
  # Raggruppamento delle righe con stesso nome, p_value, q_value, coverage e Fold_enrichment
  enriched_pathways <- enriched_pathways %>%
    group_by(name, p_value, q_value, coverage, Fold_enrichment) %>%
    summarise(miRNA = paste(unique(miRNA), collapse = ", "), .groups = "drop")
  
  enriched_pathways <- enriched_pathways %>%
    select(miRNA, name, p_value, q_value, coverage, Fold_enrichment)

  datatable(enriched_pathways, options = list(paging = TRUE, searching = TRUE))
  })


#download of the enrichment table
output$download_enrichment_results <- downloadHandler(
  filename = function() {
    paste("enriched_pathways_", Sys.Date(), ".csv", sep = "")
    },
  content = function(file) {
    enriched_pathways <- get_target_genes()
    if (is.null(enriched_pathways)) return(NULL)
    
    write.csv(enriched_pathways, file, row.names = FALSE)
    }
  )

#BARPLOT
output$enrichment_barplot <- renderPlot({
  enriched_pathways <- get_target_genes()
  if (is.null(enriched_pathways)) return(NULL)
  
  enriched_pathways$q_value <- -log10(enriched_pathways$q_value)
  
  #ordering pathways according to p-value
  enriched_pathways <- enriched_pathways[order(dplyr::desc(enriched_pathways$q_value)), ]

  num_categories <- input$showCategory
  
  unique_descriptors <- unique(enriched_pathways$name)[1:input$showCategory]
  
  # Filtra il dataframe per includere solo i descrittori selezionati
  enriched_pathways <- enriched_pathways %>% filter(name %in% unique_descriptors)
  enriched_pathways <- enriched_pathways[, c("name", "p_value", "q_value", "coverage", "Fold_enrichment")]
  
  enriched_pathways <- enriched_pathways[!duplicated(enriched_pathways), ]

  #barplot construction and visualisation
  ggplot(enriched_pathways, aes(x = reorder(name, q_value), y = q_value,fill=Fold_enrichment)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "Barplot",
         x = "Pathway", y = "q_value") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 10))
})  

# Aggiungi la funzionalità per scaricare il barplot
output$download_enrichment_barplot <- downloadHandler(
  filename = function() {
    paste("enrichment_barplot", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = last_plot(), device = "png", width = 10, height = 6, dpi = 300, bg = "white")
    }
)

###da qui  
output$enrichment_heatmap <- renderPlot({
  if (input$view_option != "Heatmap") return(NULL)
  enriched_pathways <- get_target_genes()
  if (is.null(enriched_pathways)) return(NULL)
  
  #enriched_pathways=enriched_pathways[enriched_pathways$q_value<0.05]
  enriched_pathways <- enriched_pathways[, c("name", "miRNA", "Fold_enrichment", "q_value"), drop = FALSE]
  
  # Ordino per q_value
  enriched_pathways <- enriched_pathways %>% arrange(q_value)
  
  # Aggiungi un contatore per rendere unici i nomi duplicati
  enriched_pathways$name <- as.character(enriched_pathways$name)
  
  # Ottieni i primi 10 descrittori unici
  unique_descriptors <- unique(enriched_pathways$name)[1:input$num_description]
  
  # Filtra il dataframe per includere solo i descrittori selezionati
  enriched_pathways <- enriched_pathways %>% filter(name %in% unique_descriptors)
  
  #heatmap(mat,Colv = NA, Rowv = NA, scale="column", margins = c(5, 10))
  ggplot(enriched_pathways, aes(x = miRNA, y = name, fill = Fold_enrichment)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "blue") +# Colori per il riempimento
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_tile(aes(x = miRNA, y = name), fill = "transparent", color = "grey", size = 0.5)  # Griglia
    
})


# Aggiungi la funzionalità per scaricare la heatmap
output$download_enrichment_heatmap <- downloadHandler(
  filename = function() {
    paste("enrichment_heatmap", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = last_plot(), device = "png", width = 10, height = 6, dpi = 300, bg = "white")
  }
)


####################################################
#SECTION 5: METABOLIC FUNCTIONS 

#TABLE
get_metabolism_data <- reactive({
  mirna_gene_data <- get_mirna_gene_Interaction()
  if (is.null(mirna_gene_data)) return(NULL)
  
  genes_3p <- unique(mirna_gene_data$mirna_3p_data$Target.Gene)
  genes_5p <- unique(mirna_gene_data$mirna_5p_data$Target.Gene)
  genes_Other <- unique(mirna_gene_data$mirna_Other_data$Target.Gene)
  
  mirna_3p <- unique(mirna_gene_data$mirna_3p_data$miRNA)
  mirna_5p <- unique(mirna_gene_data$mirna_5p_data$miRNA)
  mirna_Other <- unique(mirna_gene_data$mirna_Other_data$miRNA)
  
  all_genes <- unique(c(genes_3p, genes_5p, genes_Other))
  all_mirnas <- unique(c(mirna_3p, mirna_5p, mirna_Other))
  
  #filtro per i mirna considerati e per i geni considerati
  metabolism_data <- tab_metabolism[tab_metabolism$MIRNA %in% all_mirnas, ]
  metabolism_data <- metabolism_data[metabolism_data$gene %in% all_genes, ]
  
  metabolism_data <- metabolism_data %>%
    group_by(MIRNA, Reaction) %>%
    mutate(group = paste(unique(gene), collapse = ",")) %>%
    ungroup()

  # Aggiunta della variabile "essenzialità"
  metabolism_data <- metabolism_data %>%
    rowwise() %>%
    mutate(essentiality = case_when(
      grepl(" or ", GPR) ~ "YES",  
      grepl(" and ", GPR) ~ {
        group_genes <- unlist(strsplit(group, ","))
        gpr_genes <- unlist(strsplit(GPR, " and "))
        
        if (setequal(group_genes, gpr_genes)) "YES" else "NO"
      },
      !grepl(" or | and ", GPR) ~ "YES", 
      TRUE ~ "NO"  
    )) %>%
    ungroup()

  
  return(metabolism_data)
  })

output$metabolic_genes_info <- renderText({
  metabolism_data <- get_metabolism_data()
  if (is.null(metabolism_data)) return(NULL)
  
  unique_genes <- unique(metabolism_data$gene)
  num_unique_genes <- length(unique_genes)
  
  result <- paste("Total unique genes: ", num_unique_genes)
  
  # Se la casella è selezionata, aggiungi l'elenco dei geni
  if (input$show_genes) {
    result <- paste(result, "\nMetabolic genes are: ", paste(unique_genes, collapse = ", "))
    }
  })

output$Metabolic_functions_Table <- renderDT({
  metabolism_data <- get_metabolism_data()
  if (is.null(metabolism_data)) return(NULL)
  
  selected_columns <- c("MIRNA", "gene", "Reaction", "Formula", "Gruppi", "group", "essentiality")
  
  # Se il checkbox è selezionato, aggiungi la colonna GPR
  if (input$show_GPR) {
    selected_columns <- c(selected_columns, "GPR")
  }
  
  # Raggruppamento per variabili tranne MIRNA, e concatenamento dei miRNA separati da virgola
  metabolism_data <- metabolism_data %>%
    group_by(Reaction, Formula, Gruppi, group, essentiality) %>%
    mutate(MIRNA = paste(unique(MIRNA), collapse = ", ")) %>%
    ungroup()
  
  # Rimuovere righe duplicate
  metabolism_data <- metabolism_data %>%
    distinct(Reaction, Formula, Gruppi, group, essentiality, MIRNA, .keep_all = TRUE)
  
  datatable(metabolism_data[, selected_columns, drop = FALSE], 
            options = list(paging = TRUE, searching = TRUE))
  })

stringa <- "Metabolic_Functions"
output$download_Metabolic_Table <- download_csv(get_metabolism_data(), stringa)
  
#NETWORK
get_metabolic_network_data <- reactive({
  metabolism_data <- get_metabolism_data()
  if (is.null(metabolism_data)) return(NULL)
  
  network_data <- data.frame(
    from = metabolism_data$gene,
    to = metabolism_data$Reaction)
  
  return(network_data)
  })
  
output$network_metabolic <- renderVisNetwork({
  metabolism_network_data <- get_metabolic_network_data()
  metabolism_data <- get_metabolism_data()
  if (is.null(metabolism_network_data)) return(NULL)
  
  g <- graph_from_data_frame(d = metabolism_network_data, directed = FALSE)
  lista <- metabolism_data$gene
  g <- network_color(g,lista)
  summary_table <- network_table(g)
  
  output$network_metrics_metabolic_ui <- renderUI({
    tagList(
      DTOutput("network_metrics_metabolic")
      )
    })
  
  output$network_metrics_metabolic <- metrics(summary_table)
  
  stringa <-"Metabolic_Network_Metrics"
  output$download_metrics_metabolic <- download_csv(summary_table, stringa)
  
  vis_data <- toVisNetworkData(g)
  visNetwork(vis_data$nodes, vis_data$edges) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, collapse = TRUE, manipulation = TRUE)
  
  output$download_network_metabolic <- downloadHandler(
    filename = function()
      { paste("network_metabolic", Sys.Date(), ".html", sep = "") },
    content = function(con) {
      visNetwork(vis_data$nodes, vis_data$edges) %>% visOptions(highlightNearest = TRUE,
                                                                nodesIdSelection = TRUE,
                                                                collapse = TRUE,
                                                                manipulation = TRUE)  %>% visSave (con)}
    )
  return(visualizz_network(g))
  })

}


  
