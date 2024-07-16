# Shiny app dashboard to explore conceptual scheme and hypotheses in invasion biology
# author: Maud Bernard-Verdier
# source: orkg.org
# deployed: https://maudbernardverdier.shinyapps.io/Classification-scheme-invasion-science/

#### TODO
# improve page layout : with sub plots for sub hyps
# sort problem of repeated study names for enemy release for instance
library(DT)

source("resources/initiate_app.R")

# User Interface ####
ui <- bootstrapPage(
  navbarPage(theme = shinythemes::shinytheme("paper"),
             collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#CC6D7B" class="active" href="#"> Classification scheme of invasion science </a>'),
             id="nav",
             windowTitle = "Classification scheme of invasion Science",
             
             # 1: network visualization
             tabPanel("Scheme",
                      div(class= "container",
                          style="width: 100%; text-align: center; padding-left: 20px; padding-right:20px;",
                          div(style = "height: 700px; padding-top: 0px; padding-right:20px; padding-left:20px;",
                              visNetworkOutput("tripartite_network", height = "100%")
                              ),
                          p(style ="text-align: left;text-decoration:none;cursor:default;color:#808080",
                            "Hierarchical conceptual scheme illustrating the distribution of hypotheses among five main themes in invasion science, subdivided into 10 major research questions. The network is based on expert assessment and classification of hypotheses within the enKORE project, as described in Musseau et al. (in revision for Bioscience)"
                          )
                          )
                      ),
             
             # Panel 2: Filtered data table
             tabPanel("Data",  
                      
                      div(style = "margin-right:20px; margin-left:20px;",
                               tabsetPanel(

                                 tabPanel("Hypotheses-Questions",
                                          DT::DTOutput("rhrq_DT") 
                                          ,style = 'max-width: 3000px;'
                                 ),
                                 
                                 tabPanel("Hypotheses defintions",
                                          DT::DTOutput("hyp_mat_DT") 
                                          ,style = 'max-width: 3000px;'
                                 ),
                                 
                                 tabPanel("Themes-Questions",
                                          DT::DTOutput("theme_rq_mat") 
                                          ,style = 'max-width: 3000px;'
                                 )
                               )
                      )
             ),
             
             
             #           #2: Martin Ender's network
             # tabPanel("Hypothesis similarity network",
             #          visNetworkOutput("martin_network",width = "auto",height = "700px"),
             #          tags$br(),
             #          "Network of similarity between 39 hypotheses according to Enders et al. 2020, Global Ecology and Biogeography",
             #          tags$br()
             # ),
             
             # Third page: about the project
             tabPanel("About the project",
                      div(class= "container",
                          style="width: 100%; text-align: center; padding-left: 20px; padding-right:20px;",
                      div(img(src="hi-knowledge workshops logo.svg",
                              align = "left", width = "30%", style = "margin-right: 35px;")),
                      div(style="margin-top: 30px; text-align: left;",
                          p(
                             'This work is part of the enKORE project (2020-2024), a project of the',
                            a(href = "https://hi-knowledge.org","Hi Knowledge initiative")
                            ),
                            p( 'The Hi Knowledge initiative, led by Jonathan Jeschke and Tina Heger, is an online hub with interactive visualisation tools to make data and knowledge better accessible and comprehensible. It is also a community of people—ecologists, philosophers, practitioners—sharing an interest in knowledge synthesis in invasion ecology and beyond.'),
                          p('If you are interested in joining our community, please ',
                               a( href = "mailto:hiknowledgeinitiative@gmail.com","contact us")
                            ),
                          ),

                      div(style = "display: inline-block; width:90%;  margin-top:30px; margin-bottom:30px; text-align: center;",
                        img( src="IGB_dt_farbe_pos.jpg", align = "left", width = "25%",
                                style = "margin-left: 35px; display: inline-block;"),
                        img( src="1418px-Fub-logo.svg.png", align = "left", width = "25%",
                            style = "margin-left: 35px;display: inline-block; "),
                        img(src="Logo_Volkswagenstiftung.jpg", align = "left", width = "25%",
                            style = "float: left;display: inline-block;  margin-left: 35px; ")
                      ),
                      
                      div( style = "clear:both; text-align: left;",
                        "Webpage built by ",
                        a(href = "mailto:maudbv@gmail.com","Maud Bernard-Verdier"),
                        "using R shiny.",
                        br(),
                        "This project is funded by the",
                        a( href = "https://www.volkswagenstiftung.de/de","Volkswagen Stiftung.")
                      )
             )
             )
  )
)


# Server:   ####

server <- function(input, output, session) {
  
  
  # # Martin's network
  # output$martin_network <- renderVisNetwork({
  #   plot_martin_network(nodes_martin, edges_martin)
  # })
  
  # tripartite network of hypotheses
  output$tripartite_network<- renderVisNetwork({
    plot_3L_network(nodes_3L, edges_3L)
  })
  
  
  # Datatable of RH-RQ links
  
  
  
  output$rhrq_DT = DT::renderDT({
    df <-  as.data.frame(rhrq_mat)
    rownames(df) <- paste( hyp_mat[match(rownames(rhrq_mat),hyp_mat$Acronym), "Hypothesis_label"], 
                           " (",
                           rownames(rhrq_mat),
                           ")", sep = ""
    )
    # a custom table container
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, colspan = 1,  'Hypotheses'),
          th(colspan = 2, 'Pathways & Introduction'),
          th(colspan = 3, 'Invasion success'),
          th(colspan = 3, 'Invasion impact'),
          th(colspan = 2, 'Managing biological invasions')
        ),
        tr(
          lapply(colnames(rhrq_mat), th)
        )
      )
    ))
    datatable(df,
              container = sketch,
              rownames = TRUE,
              extensions = 'Buttons',
              options = list(
                pageLength =10, 
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
    
  },
  server = FALSE)
  
  #("theme_rq_mat") 
  output$theme_rq_mat = DT::renderDT({
    df <-  as.data.frame(theme_rq_mat[,c("Theme","RQ_abb", "Research question")] )
    datatable(df,
              rownames = TRUE,
              extensions = 'Buttons',
              filter = "top",
              options = list(
                dom = 'Bfrtip',
                pageLength = 20,
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
    
  },
  server = FALSE)
  
  
  # Hypothesis table
  
  output$hyp_mat_DT = DT::renderDT({
    
    df <-  hyp_mat %>%
      filter(Studied.by=="invasion biology", Origin %in% c("ME", "ME + ED")) %>%
      select(Hypothesis_label, Definition, Key_ref, Wikidata)
    
    datatable(df,
              rownames = TRUE,
              extensions = 'Buttons',
              filter =  "top",
              caption = HTML("<p> Table of hypotheses for invasion biology adapted from <i> A conceptual map of invasion biology: Integrating hypotheses into a consensus network </i> by Enders et al., Global Ecology and Biogeography, 2020. <a href = https://doi.org/10.1111/geb.13082> https://doi.org/10.1111/geb.13082</a> </p>"),
              options = list(
                pageLength = 15,
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
    
  },
  server = FALSE)
  
}




thematic_shiny()
shinyApp(ui = ui, server = server)
