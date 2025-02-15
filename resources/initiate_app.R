# Config and  load dependencies


# Load packages ####
library(shiny)
library(ggplot2)
library(plotly)
library(sysfonts)
library(dplyr)
require(shinyWidgets)
require(shinyjs)
require(shinythemes)
library(readr)
library(igraph)
library(networkD3)
library(ggraph)
library(tidyverse)
library(visNetwork)
library(thematic)
library(DT)

# import and pre-process data ####
source("resources/Hypothesis index.R")
# source("resources/hyp_network_viz/martin_network_viz.R")

#source("resources/hyp_network_viz/hyp-trait network invasion+urban.R")
source("resources/hyp_network_viz/tripartite topdown network.R")

# Get colours and style themes ####
source("resources/ggplot_HiK_theme.R")


