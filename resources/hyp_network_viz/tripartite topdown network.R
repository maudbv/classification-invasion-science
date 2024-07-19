# Tripartite network for top down representation of themes-RQ-RH

# Libraries
library(readr)
library(igraph)
library(networkD3)
library(ggraph)
library(visNetwork)
library(dplyr)
library(grDevices)

# FUNCTION: Build the scheme as nodes and edges of a graph object ####
build_scheme <- function(rhrq_data = rhrq_mat) {
  
# RQ-Hyp network
network_hypRQ <- graph_from_incidence_matrix(
  as.matrix(rhrq_data),
  directed = TRUE,
  mode = "in")

# plot(network_hypRQ)

# Theme network
network_themeRQ <- graph_from_data_frame(
  as.matrix(theme_rq_mat[,c("Theme","RQ_abb")]),
  directed = TRUE)

# plot(network_themeRQ)

# combine networks into 3 layers ####
network_3layers <- igraph::union(a = network_themeRQ , b = network_hypRQ)
V(network_3layers)$layer = c(rep(1, 5), rep(2,11), rep(3,39))


# # Check that layers match the items:
# plot.igraph(network_3layers,
#             vertex.color=c("firebrick","#009EEE","white")[V(network_3layers)$layer],)

# Match hypothesis names
V(network_3layers)$full_name <- "NA"
V(network_3layers)$full_name[V(network_3layers)$layer == 1] <- 
  V(network_3layers)$name[V(network_3layers)$layer == 1]

V(network_3layers)$full_name [V(network_3layers)$layer == 2] <-  
  theme_rq_mat[match(V(network_3layers)$name[V(network_3layers)$layer == 2],
                     theme_rq_mat$RQ_abb),"Research Question"]

V(network_3layers)$full_name[V(network_3layers)$layer == 3] <-  
  hyp_mat[match(V(network_3layers)$name[V(network_3layers)$layer == 3],
        hyp_mat$Acronym),"Hypothesis_label"]

V(network_3layers)$def[V(network_3layers)$layer == 3] <-  
  hyp_mat[match(V(network_3layers)$name[V(network_3layers)$layer == 3],
                hyp_mat$Acronym),"Definition"]

V(network_3layers)$def[V(network_3layers)$layer == 1] <- ""
V(network_3layers)$def[V(network_3layers)$layer == 2] <- ""

# Create labels
V(network_3layers)$label = V(network_3layers)$full_name
V(network_3layers)$label[1:5] = c("Introduction\npathways","Invasion success\n& invasibility","Invasion\nimpact", "Managing\nbiological invasions", "Meta-Invasion\nscience")
V(network_3layers)$label[V(network_3layers)$layer == 2] <- 
  as.character(theme_rq_mat[match(V(network_3layers)$full_name[V(network_3layers)$layer == 2],  theme_rq_mat$`Research Question`),"Research question_2lines"])

# add ltext afjustment?
V(network_3layers)$adj = 0.5
V(network_3layers)$adj[V(network_3layers)$layer == 3] <- 0

# Interactive hierarchical plot with VisNetwork  #########
#nodes_3L
nodes_3L <- data.frame(
  id = 1 : length(vertex_attr(network_3layers)$name),
  as.data.frame(vertex_attr(network_3layers))[,-1]
  )

# # Invert order of nodes
# nodes_3L$id = length(vertex_attr(network_3layers)$name):1
# nodes_3L$id[1:4] = nodes_3L$id[4:1] # reorder themes

# type of node
nodes_3L$type =  c("Theme", "Research Question","Hypothesis")[nodes_3L$layer]
nodes_3L$level = nodes_3L$layer

# make levels into groups for formatting
nodes_3L$group = nodes_3L$level

# Format
nodes_3L$color = c("#0BCF72","#31688E", "#440154","#F4D021","#FF9F00",
                   rep("#80E8B7",2),
                   rep("#8EC2E6", 3),
                   rep("#C59DCF",3),
                   rep("#FDE724", 2),
                   "#ECBB69",
                   rep("white", 39)
                   )

nodes_3L$shape = c("ellipse","box", "box")[nodes_3L$level]
nodes_3L$borderWidth  = 0
nodes_3L$heightConstraint.minimum = c(200, 160, 20)[nodes_3L$level]
nodes_3L$heightConstraint.valign = "center"

nodes_3L$margin.top = c(10, 20, 0)[nodes_3L$level]
nodes_3L$margin.bottom = c(10, 20, 0)[nodes_3L$level]
nodes_3L$margin.left =c(10, 50, 0)[nodes_3L$level]
nodes_3L$margin.right = c(10, 50, 0)[nodes_3L$level]

#nodes_3L$color = c("grey","coral", "white")[nodes_3L$level]
nodes_3L$font.color = c("white", "black","black")[nodes_3L$level]
nodes_3L$font.size = c(90,60,50)[nodes_3L$level]
nodes_3L$font.face = "verdana"
nodes_3L$shadow = FALSE 
nodes_3L$font.align = c("center", "center", "left")[nodes_3L$level]


# Tool tip on hover
nodes_3L$title = paste0("<p style=\"font-color: white;\"> <i>",
                        nodes_3L$type,
                        "</i><br><b>",
                        nodes_3L$full_name,
                        "</b><br>",
                        nodes_3L$def,
                        "<br></p> "
)


# correct labels to be displayed with line returns
nodes_3L$label = gsub("\\\\n", "\n", nodes_3L$label)


#Edges
edges_3L <- as.data.frame(as_edgelist(network_3layers, names = TRUE))
colnames(edges_3L) <- c("from", "to")
edges_3L$from <- nodes_3L$id[match(edges_3L$from, nodes_3L$name)]
edges_3L$to <- nodes_3L$id[match(edges_3L$to, nodes_3L$name)]

# # stop edges at node borders and left align hypotheses : does not work for now
# nodes_3L$widthConstraint.minimum = c(NA, NA,1300)[nodes_3L$level]
# nodes_3L$widthConstraint.valign = c("center", "center", "left")[nodes_3L$level] 
# edges_3L$endPointOffset.from = + 650
# edges_3L$endPointOffset.to = - 650
# edges_3L$arrowStrikethrough = TRUE

# Set X and Y position of nodes manually

# Get number of nodes in each layer
nt = nrow(filter(nodes_3L, layer == 1))
nq = nrow(filter(nodes_3L, layer == 2))
nh = nrow(filter(nodes_3L, layer == 3))

# set Y
yt = (seq(1:nt)*1.5 / nt)* (nh)* 50
yq = (seq(1:nq) / nq)* (nh)* 50
yh = (seq(1:nh) / nh)* (nh)* 50

scale_range <-function(x, scalerange=c(0, 1)) {
 ((x - min(x))/(max(x) - min(x)))*(scalerange[2] - scalerange[1]) + scalerange[1]
  }
yq = scale_range(yq, scalerange = range(yt)+ c(-200, 000))
yh = scale_range(yh, scalerange = range(yt)+ c(-300, 300) )

nodes_3L$y = c(yt, yq, yh)

# set X
nodes_3L$x = nodes_3L$layer/3* 4000 + c(0,0, 500)[nodes_3L$layer]

# remove edge and RQ node for meta invasion science (AFTER having made space for it on XY positions
rm_id <- nodes_3L[ which(nodes_3L$name == "Meta-invasion questions"), "id"]
edges_3L <- edges_3L[-which(edges_3L$to == rm_id),]
nodes_3L <- nodes_3L[ -which(nodes_3L$id == rm_id), ]

scheme = list(n = nodes_3L, e = edges_3L)
return(scheme)
}

# Plot network function ####
plot_3L_network <- function(n = nodes_3L, e = edges_3L) {
  p <- visNetwork::visNetwork(
    n,
    e,
    width = "100%",
    height = "800px",
    # main = list(
    #   text = "Conceptual scheme for Invasion Science",
    #   style = "font-family:verdana;color:#0085AF;font-size:18px;text-align:center;"),
    margin = -0.2) %>%
    visEdges(
      shadow = FALSE,
      color = list(color = "black", highlight = "#C62F4B")
    ) %>%
    visOptions(highlightNearest = list(enabled = T, degree = 1, hover = FALSE),
               autoResize = FALSE,
               manipulation = FALSE) %>%
    visInteraction(navigationButtons = TRUE,
                   zoomView = FALSE ) %>%
    visPhysics(enabled = FALSE)
  
 p <-  p %>%
    visInteraction(tooltipStyle = 'position: fixed; text-align: left; visibility:hidden;padding: 5px;font-family: verdana;font-size:14px;font-color:#00000;
                   background-color: white;-moz-border-radius: 3px;*-webkit-border-radius: 3px;border-radius: 3px; border: 1px solid #808074;
                   box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);max-width:400px;
                   word-break: break-word;
                    line-height: 1.4')
  
  return(p)
}

# Run function to create original scheme ####
scheme_original <- build_scheme()
# (p <- plot_3L_network(scheme_original$n, scheme_original$e))

# Tooltip style default:
# 'position: fixed;visibility:hidden;padding: 5px;font-family: verdana;
# font-size:14px;font-color:#000000;background-color: #f5f4ed;-moz-border-radius: 3px;*
# -webkit-border-radius: 3px;border-radius: 3px; border: 1px solid #808074;
# box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);max-width:400px;word-break: break-all'

