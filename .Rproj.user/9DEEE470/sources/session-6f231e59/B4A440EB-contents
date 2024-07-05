# Shiny app dashboard to explore hypotheses in invasion biology
# author: Maud Bernard-Verdier
# source: orkg.org
# deployed: https://maudbernardverdier.shinyapps.io/Hypothesis-evidence-explorer/

# TODO ####
# fix the chronology plot for interaction and formatting

# Load packages ####

#library(tidyverse)
library(dplyr)
library(readr)

#library(shiny)
require(shinyWidgets)
#require(shinyjs)
require(shinythemes)
#library(sysfonts)

library(ggplot2)
library(plotly)
library(DT)

#### import and pre-process data - RUN ONCE ####
 source('resources/scripts/process_HK.R')
# deprecated: source("resources/scripts/Hypothesis index.R") 

# # Get colours and style themes ####
 source("resources/scripts/ggplot_HiK_theme.R")

# Load custom functions ####
source("resources/functions/app_helper.R")

# Load plotting functions ####
source("resources/functions/plot_chrono.R")
source("resources/functions/plot_piechart.R")
source("resources/functions/plot_barplot.R")
source("resources/functions/plot_overview.R")



# User Interface ####
ui <- bootstrapPage(
  navbarPage(theme = shinytheme("paper"),
             collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#27596B;" class="active" href="#">Evidence for hypotheses in Invasion Biology</a>'),
             id="nav",
             windowTitle = "Hypotheses in Invasion Biology",
             
             
             # second page: Interactive exploration of evidence supporting individual hyps
             #  tabPanel("Evidence of support",
             sidebarLayout(
               div(
                 sidebarPanel(
                   "Explore the evidence available for major hypotheses in Invasion Biology.
                   You can filter studies by taxa, habitats or research method",
                   tags$br(),
                   tags$br(),
                   selectInput(inputId = 'hyp',
                               label = 'Select a hypothesis',
                               choices =  c("All",unique(total_df$hypothesis)),
                               multiple = FALSE,
                               selected = "All"),
                   
                   pickerInput(inputId = 'taxa',
                               'Select a taxonomic group',
                               choices = taxa_groups,
                               multiple = TRUE,
                               selected = taxa_groups,
                               options = list(`actions-box` = TRUE,
                                              `none-selected-text` = "Please make a selection!")
                   ),
                   
                   uiOutput("habitat_selector"), 
                   
                   uiOutput("method_selector"),
                   
                   
                   HTML('<a style="text-decoration:none;cursor:default;color:#27596B;" class="active" href="#">This is a project of the Hi Knowledge initiative</a>'),
                   tags$br(),
                   tags$a(href="https://hi-knowledge.org/", "hi-knowledge.org")
                 ),
                 style = 'max-width:900px;'
                 ),
               
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Overview",
                            div(plotlyOutput('overview_chart', height = "500px"), style = 'max-width: 1200px;'),
                            tags$br(),
                   ),
                   
                   
                   # Panel 1: support for the hypothesis
                   tabPanel("Focus",
                            div(
                             h5(textOutput("hyp_description")),
                              fluidRow(
                                column(plotlyOutput('support_piechart', height = "150px"),
                                       width = 5),
                                column( p(textOutput("support_summary"), 
                                          style="text-align:left;color:#27596B;padding:15px;border-radius:10px"),
                                        width = 5)
                              ),
                              fluidRow(plotOutput('chronology', height = "350px")),
                              style = 'max-width: 800px;'
                            )
                   ),
                   
                   # Panel 2: Filtered data table
                   tabPanel("Distribution",  
                            div(  
                              fluidRow( 
                                column(6,
                                       plotlyOutput("support_habitats", height = "200px")
                                       ),
                                column(6, 
                                       plotlyOutput("support_methods", height = "200px")
                                       ),
                                column(6,
                                       plotlyOutput("support_taxa", height = "300px")
                                       ),
                                column(6,
                                       plotlyOutput("support_continents", height = "300px")
                                       )
                                ),
                              tags$br(),
                              style = 'max-width: 1200px;'
                            )
                   ),
                   
                   # Panel 2: Filtered data table
                   tabPanel("Data",  
                            
                            div(
                              DT::DTOutput("filtered_data") #TODO update type of table output for more interaction +add years
                              ,style = 'max-width: 3000px;'
                            )
                          
                   )
                 )
               )
             ),
             #),
             
             # footer: about the project
             footer = list(
               hr(), 'This interactive website was built by Maud Bernard-Verdier using R shiny, with data from the 2018 book "Invasion biology: hypotheses and evidence", by Jeschke & Heger (eds), and currently curated by the', tags$a(href="https://orkg.org", " Open Research Knowledge Graph project"),'. This work was produced within the enKORE project, a', tags$a(href="https://hi-knowledge.org/", "Hi Knowledge initiative") ,' funded by the Volkswagen Stiftung, Germany.',
                  tags$br(),
                  tags$br()
                  )
             
  )
)
  
  # Server:   ####

server <- function(input, output, session) {
  

  
 # Conditional filter selection
  

  hyp_filter <- reactive({
    req(input$hyp)
    total_df %>% 
      { if (! "All" %in% input$hyp) {
        dplyr::filter(.,hypothesis == input$hyp)} else {.} 
      }
  })
  
    hyp_taxa <- reactive({
      req(hyp_filter())
      req(input$taxa)
      hyp_filter() %>%
        { if (! "All" %in% input$taxa) {
          dplyr::filter(., .detect_items(taxa_group, input$taxa))} else {.} 
        }
    })
    
    output$habitat_selector <- renderUI({
      selectInput(inputId = "hab",
                  label = "Select a habitat",
                  choices =  c("All", unique(unlist(hyp_taxa()$Habitat_list))))
    })
    
    hyp_taxa_hab <- reactive({
      req(hyp_taxa())
      hyp_taxa() %>% 
        { if (! "All" %in% input$hab) {
          dplyr::filter(., .detect_items(Habitat_list, input$hab))} else {.}
          }
    })
    
    output$method_selector <- renderUI({
      req(hyp_taxa_hab())
      selectInput(inputId = "method",
                  label = "Select a research method",
                  choices =  c("All", unique(unlist( hyp_taxa_hab()$Research_Method))))
    })

  filtered_df <- reactive({
    req(input$method)
   hyp_taxa_hab() %>% 
      { if (! "All" %in% input$method) {
        dplyr::filter(., .detect_items(Research_Method, input$method))} else {.}
      }
  })

  support_perc <- reactive({
    req(filtered_df)
    df <-  filtered_df()
    counts <- df %>% count(support_for_hypothesis, sort = FALSE)
    return(round(counts$n[which(counts$support_for_hypothesis == "Supported")]/sum(counts$n)*100,2))
  })
  
  
  
  # overview figure
  output$overview_chart <- renderPlotly( {
    req(filtered_df())
    plot_overview (filtered_df())
  })
  
  # Plot chronology figure
  output$chronology <- renderPlot( {
    req(filtered_df())
    plot_chrono(filtered_df())
  })
  
  # hyp piechart
  output$support_piechart <- renderPlotly( {
    req(filtered_df())
    plot_piechart(filtered_df())
  })
  
  # comments
  output$hyp_description <- renderText({
    req(input$hyp)
    if (input$hyp == "All") {
      paste("All 10 hypotheses"
      )
    } else {
     paste(input$hyp)
    }
  })
  
  output$support_summary <- renderText({
    req(input$hyp)
    if (! input$hyp == "All") {
      paste(
        "The hypothesis is supported in ",
        support_perc(),
        "% of the ",
        length(unique(filtered_df()$study)),
        " studies included in the database."
      )
    } else {
      paste(
        "Hypotheses are supported in ",
        support_perc(),
        "% of the ",
        length(unique(filtered_df()$study)),
        " studies included in the database."
        )
    }
  })
      
  # Distribution
  output$support_habitats <- renderPlotly( {
    req(filtered_df())
    plot_barplot(filtered_df(),
                 group_col = "Habitat_list",
                 grouping = habitat_groups,
                 legend.show = TRUE,
                 title_text = "Habitat"
                 )
  })
  output$support_methods <- renderPlotly( {
    req(filtered_df())
    plot_barplot(filtered_df(),
                 group_col = "Research_Method",
                 grouping = method_groups,
                 legend.show = FALSE,
                 title_text = "Research method"
    )
  })
  
  output$support_taxa <- renderPlotly( {
    req(filtered_df())
    plot_barplot(filtered_df(),
                 group_col = "taxa_group",
                 grouping =  taxa_groups,
                 legend.show = FALSE,
                 title_text = "Taxonomic group"
    )
  })
  
  output$support_continents <- renderPlotly( {
    req(filtered_df())
    plot_barplot(filtered_df(),
                 group_col = "continents",
                 grouping = continents_vec,
                 legend.show = FALSE,
                 title_text = "Continent"
    )
  })
  
  # Data table
  output$filtered_data = DT::renderDT({
    req(filtered_df())
    display_columns <- c("Title","support_for_hypothesis","Investigated_species","Habitat","Research_Method", "continents","Study_date", "hypothesis", "DOI") 
    df <-  as.data.frame(filtered_df())
    rownames(df) <- df$index
    df <-  df[, display_columns]
 datatable(df,
           rownames = FALSE,
           extensions = 'Buttons',
           options = list(
             dom = 'Bfrtip',
             exportOptions = list(header = ""),
             buttons = list(
               list(
                 extend = "csv", 
                 filename = 'export',
                 text = "Download", 
                 title = NULL
                 )
               )
           )
 )
 
  },            server = FALSE)
}
  
 
shinyApp(ui = ui, server = server)
