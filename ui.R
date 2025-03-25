ui <- fluidPage(
  
  tags$style(background),
  
  titlePanel(HTML("<strong><em>ShinyMir</em></strong>")),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("selection", "• Choose between:",
                   choices = c("miRNA", "Disease")),
      uiOutput("dynamic_select"),
      fileInput("file", "• Upload miRNA-Disease Interaction File", 
                accept = c(".csv", ".xlsx")),
      
      # Add a radio button to select between merge_ and DEMC_
      radioButtons("dataset_choice", "• Select Database:",
                   choices = c("Database MirCancer&HMDD" = "merge_", "Database DEMC" = "DEMC_","Uploaded" = "Uploaded"), 
                   selected = "merge_")
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Documentation",  
                           uiOutput("references")),
                  tabPanel("Context",
                           plotOutput("graph_plot"),
                           uiOutput("disease_selector"),
                           uiOutput("miRNA_disease_selector")
                  ),
                  
                  tabPanel("miRNA-Disease Interaction",  
                           fluidRow(
                             column(12, 
                                    sliderInput("count_threshold", 
                                                "Select Count Threshold", 
                                                min = 1, 
                                                max = 10, 
                                                value = 2, 
                                                step = 1)
                             )
                           ),
                           tabsetPanel(
                             tabPanel("Table", 
                                      h4(textOutput("miRNA1_info")),
                                      fluidRow(),
                                      DTOutput("miRNA_Disease_Interaction"), 
                                      downloadButton("download_Disease_data", "Download miRNA-Disease Interaction")
                             ),
                             tabPanel("Network", 
                                      fluidRow(
                                        column(12, visNetworkOutput("network_mirna_Disease"), 
                                               downloadButton("download_network_mirna_Disease", "Download miRNA-Disease Network as Image"),
                                               downloadButton("download_metrics_mirna_Disease", "Download miRNA-Disease Network Metrics")
                                        )
                                      ),
                                      column(12, uiOutput("network_metrics_mirna_Disease_ui"))
                             )
                           )
                  ),
                  
                  tabPanel("miRNA-Gene Interaction",  
                           fluidRow(
                             column(6,
                                    actionButton("select_all_strong", "Select All"),
                                    actionButton("deselect_all_strong", "Deselect All"),
                                    checkboxGroupInput("selected_strong_tests", "Select Strong Assays:",
                                                       choices = sceltaproveforti,
                                                       selected = "Luciferase.Reporter.Assay")
                             ),
                             column(6,
                                    actionButton("select_all_weak", "Select All"),
                                    actionButton("deselect_all_weak", "Deselect All"),
                                    checkboxGroupInput("selected_weak_tests", "Select Weak Assays:",
                                                       choices = sceltaprovedeboli,
                                                       selected = character(0))
                             )
                           ), 
                           
                           tabsetPanel(
                             tabPanel("Table", 
                                      selectInput("mirna_type", "Choose miRNA type:",
                                                  choices = c("miRNA-5p", "miRNA-3p", "Other"), 
                                                  selected = "miRNA-5p"),
                                      h4(textOutput("genes2_info")),
                                      sliderInput("min_count_gene", "Min. Count for Interaction:", 
                                                  min = 1, max = 10, value = 2, step = 1), 
                                      DTOutput("miRNA_Gene_Interaction_Table"),
                                      downloadButton("download_mirna_gene_data", "Download miRNA-Gene Interaction Table")
                             ),
                             tabPanel("Network",  
                                      fluidRow(
                                        column(12, visNetworkOutput("network_mirna_gene")),  
                                        downloadButton("download_network", "Download Network as Image"),  
                                        downloadButton("download_metrics", "Download Network Metrics")
                                      ),
                                      column(12, 
                                             uiOutput("network_metrics_mirna_gene_ui")  
                                      )
                             ),
                             tabPanel("Enrichment",  
                                      h4("Enrichment Analysis Results"),
                                      
                                      selectInput("view_option", "Select View", choices = c("Table", "Barplot", "Heatmap")),
                                      
                                      # Aggiungi il filtro Pathway size: Min.
                                      sliderInput("min_pathway_size", 
                                                  "Pathway size: Min.", 
                                                  min = 2, max = 20, value = 2, step = 1),
                                      
                                      # Aggiungi il fileInput per caricare un file CSV o XLSX
                                      fileInput("file_input", "Upload Reactome Ontology File", 
                                                accept = c(".csv", ".xlsx")),
                                      
                                      conditionalPanel(
                                        condition = "input.view_option == 'Table'",

                                        DTOutput("enrichment_results_table"), 
                                        downloadButton("download_enrichment_results", "Download Enrichment Table")
                                      ),
                                      
                                      conditionalPanel(
                                        condition = "input.view_option == 'Barplot'",
                                        sliderInput("showCategory", 
                                                    label = "Number of Categories to Show in Barplot", 
                                                    min = 5, max = 30, value = 10),
                                        plotOutput("enrichment_barplot"),
                                        downloadButton("download_enrichment_barplot", "Download Barplot")
                                      ),
                                      
                                      conditionalPanel(
                                        condition = "input.view_option == 'Heatmap'",
                                        sliderInput("num_description", 
                                                    "Number of Descriptions to Show:", 
                                                    min = 5, max = 30, value = 10),
                                        plotOutput("enrichment_heatmap"),
                                        downloadButton("download_enrichment_heatmap", "Download Heatmap")
                                      )
                             )
                  )),
                  tabPanel("Metabolic functions",  
                           checkboxInput("show_genes", "Show Metabolic Genes", value = FALSE),
                           h4(textOutput("metabolic_genes_info")),
                           tabsetPanel(
                             tabPanel("Table", 
                                      checkboxInput("show_GPR", "Show GPR column", value = FALSE),
                                      DTOutput("Metabolic_functions_Table"), 
                                      downloadButton("download_Metabolic_Table", "Download Metabolic Table")
                             ),
                             tabPanel("Network", 
                                      fluidRow(
                                        column(12, visNetworkOutput("network_metabolic")),
                                        downloadButton("download_network_metabolic", "Download Metabolic Network as Image"),
                                        downloadButton("download_metrics_metabolic", "Download Metabolic Network Metrics")
                                      ),
                                      column(12, 
                                             uiOutput("network_metrics_metabolic_ui")  
                                      )
                             )
                           )
                  )
      )
    )))
