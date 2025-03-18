#' UI Module for Bayesian Analysis of AALM Model
#' @param id Module ID for namespacing
bayesianUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        fluidRow(
            box(
                width = 6,
                title = "Data Input",
                status = "primary",
                solidHeader = TRUE,
                
                radioButtons(
                    ns("analysis_type"),
                    "Analysis Type",
                    choices = c(
                        "Individual" = "individual",
                        "Population" = "population"
                    )
                ),
                
                fileInput(
                    ns("data_file"),
                    "Upload Blood Lead Data (CSV)",
                    accept = c("text/csv", ".csv"),
                    multiple = FALSE
                ),
                
                numericInput(
                    ns("n_chains"),
                    "Number of MCMC Chains",
                    value = 4,
                    min = 1,
                    max = 8
                ),
                
                numericInput(
                    ns("n_iter"),
                    "Iterations per Chain",
                    value = 10000,
                    min = 1000,
                    max = 100000
                ),
                
                actionButton(
                    ns("run_mcmc"),
                    "Run Bayesian Analysis",
                    class = "btn-primary",
                    icon = icon("calculator")
                )
            ),
            
            box(
                width = 6,
                title = "Prior Settings",
                status = "primary",
                solidHeader = TRUE,
                
                # Soil ingestion prior
                sliderInput(
                    ns("soil_prior_mean"),
                    "Soil Ingestion Prior Mean (g/day)",
                    min = 0,
                    max = 0.2,
                    value = 0.05,
                    step = 0.01
                ),
                
                # Dust ingestion prior
                sliderInput(
                    ns("dust_prior_mean"),
                    "Dust Ingestion Prior Mean (g/day)",
                    min = 0,
                    max = 0.2,
                    value = 0.05,
                    step = 0.01
                ),
                
                checkboxInput(
                    ns("use_informative_priors"),
                    "Use Informative Priors",
                    value = TRUE
                )
            )
        ),
        
        fluidRow(
            box(
                width = 12,
                title = "MCMC Results",
                status = "info",
                solidHeader = TRUE,
                
                tabsetPanel(
                    tabPanel("Posterior Distributions",
                        plotlyOutput(ns("posterior_plot"))
                    ),
                    tabPanel("Trace Plots",
                        plotlyOutput(ns("trace_plot"))
                    ),
                    tabPanel("Summary Statistics",
                        DTOutput(ns("mcmc_summary"))
                    ),
                    tabPanel("Diagnostics",
                        verbatimTextOutput(ns("diagnostics"))
                    )
                )
            )
        ),
        
        fluidRow(
            box(
                width = 12,
                title = "Help",
                status = "warning",
                collapsible = TRUE,
                collapsed = TRUE,
                
                tags$ul(
                    tags$li("Upload CSV file with columns: 'id', 'blood_pb'"),
                    tags$li("Individual analysis: single row of data"),
                    tags$li("Population analysis: multiple measurements"),
                    tags$li("MCMC chains: more chains = better convergence check"),
                    tags$li("Iterations: more iterations = better precision")
                )
            )
        )
    )
}