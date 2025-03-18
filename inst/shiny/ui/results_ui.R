#' UI Module for AALM model results
#' @param id Module ID for namespacing
resultsUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        fluidRow(
            box(
                width = 12,
                title = "Model Controls",
                status = "primary",
                solidHeader = TRUE,
                
                fluidRow(
                    column(4,
                        actionButton(
                            ns("run_model"),
                            "Run Model",
                            class = "btn-lg btn-primary",
                            icon = icon("play")
                        )
                    ),
                    column(4,
                        downloadButton(
                            ns("download_results"),
                            "Download Results",
                            class = "btn-lg"
                        )
                    ),
                    column(4,
                        selectInput(
                            ns("output_type"),
                            "Output Type",
                            choices = c(
                                "Blood Lead" = "blood",
                                "Bone Lead" = "bone",
                                "Tissue Lead" = "tissue",
                                "Mass Balance" = "mass"
                            )
                        )
                    )
                )
            )
        ),
        
        fluidRow(
            # Results plots
            box(
                width = 8,
                title = "Model Results",
                status = "primary",
                solidHeader = TRUE,
                
                plotlyOutput(ns("results_plot"), height = "400px")
            ),
            
            # Summary statistics
            box(
                width = 4,
                title = "Summary Statistics",
                status = "info",
                
                tableOutput(ns("summary_stats"))
            )
        ),
        
        fluidRow(
            # Detailed results table
            box(
                width = 12,
                title = "Detailed Results",
                status = "primary",
                
                DTOutput(ns("results_table"))
            )
        ),
        
        fluidRow(
            # Mass balance check
            box(
                width = 12,
                title = "Mass Balance",
                status = "warning",
                collapsible = TRUE,
                collapsed = TRUE,
                
                verbatimTextOutput(ns("mass_balance"))
            )
        )
    )
}