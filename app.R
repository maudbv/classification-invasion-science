# Shiny app dashboard to explore hypotheses in invasion biology
# author: Maud Bernard-Verdier
# source: orkg.org
# deployed: https://maudbernardverdier.shinyapps.io/Invasion-biology-maps/

#### TODO
# improve page layout : with sub plots for sub hyps
# sort problem of repeated study names for enemy release for instance

source("resources/initiate_app.R")

# User Interface ####
ui <- bootstrapPage(
  navbarPage(theme = shinythemes::shinytheme("paper"),
             collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#CC6D7B" class="active" href="#">Maps of Invasion Biology</a>'),
             id="nav",
             windowTitle = "Conceptual maps of Invasion Biology",

             # 1: network visualization
            tabPanel("Conceptual scheme",
                               visNetworkOutput("tripartite_network",width = "auto",height = "600px"),
                               tags$br(),
                               HTML('<a style="text-decoration:none;cursor:default;color:#808080" href="#">Hierarchical scheme illustrating the distribution of hypotheses among nine major research questions and four themes in invasion biology. The network is based on ongoing expert assessment and classification of hypotheses within the enKORE project</a>'),
                               tags$br()
                      ),
                      
                      #2: Martin Ender's network
            tabPanel("Similarity network",
                     visNetworkOutput("martin_network",width = "auto",height = "700px"),
                     tags$br(),
                     "Network of similarity between 39 hypotheses according to Enders et al. 2020, Global Ecology and Biogeography",
                     tags$br()
            ),

             # Third page: about the project
             tabPanel("About the project",
                      "These vizualisations were built by Maud Bernard-Verdier using R shiny."
             )
  )
)


# Server:   ####

server <- function(input, output, session) {
  
  
  # Martin's network
  output$martin_network <- renderVisNetwork({
    plot_martin_network(nodes_martin, edges_martin)
  })

  # tripartite network of hypotheses
  output$tripartite_network<- renderVisNetwork({
    plot_3L_network(nodes_3L, edges_3L)
  })
}


thematic_shiny()
shinyApp(ui = ui, server = server)
