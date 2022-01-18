library(shiny)
library(tercen)
library(dplyr)
library(tidyr)


############################################
#### TERCEN API
#### This part should not be included in ui.R and server.R scripts
# http://localhost:5402/admin/w/4268ffd55d3487c8c40b156b3f075c82/ds/44a2b9cb-6a0e-44d8-9839-7839117c020b

getCtx <- function(session) {
  ctx <- tercenCtx(stepId = "44a2b9cb-6a0e-44d8-9839-7839117c020b",
                   workflowId = "4268ffd55d3487c8c40b156b3f075c82")
  return(ctx)
}

## DO NOT MODIFY
getValues <- function(session) {
  ctx <- getCtx(session)
  values <- list()
  
  #values$data <- ctx %>%
  #  select(.y, .ri, .axisIndex) %>%
  #  spread(key = .axisIndex, value = .y) %>%
  #  mutate(rnames = ctx$rselect()[[1]])
  
  #colnames(values$data) <- c("rowIndex",
  #                           unlist(ctx$yAxis),
  #                           "rowNames")
  
  values$data <- ctx %>%
    select()
  values$data[, names(ctx$rselect())] <- ctx$rselect()[[1]]
  

  names(values$data) <- make.names(names(values$data))
  
  return(values)
}

getColorvars <- function(session) {
  ctx <- getCtx(session)
  
  make.names(unlist(ctx$colors))
}

getLabelvars <- function(session) {
  ctx <- getCtx(session)

  make.names(unlist(ctx$labels))
}


####
############################################

##############
##### UI #####
##############

source("global.R")

ui <- dashboardPage(
  title = "CORAL",
  dashboardHeader(
    title = tags$a(span(img(src = "logos/coral-logo-white2.png", height = 60, align = "left")), href = "http://phanstiel-lab.med.unc.edu/CORAL/"), titleWidth = 600,
    tags$li(
      class = "dropdown",
      tags$style(".main-header {max-height: 60px}"),
      tags$style(".main-header .logo {height: 60px;}"),
      tags$style(".sidebar-toggle {height: 60px; padding-top: 10px !important;}"),
      tags$style(".navbar {min-height:0px !important}")
    )
  ),
  dashboardSidebar
  (
    sidebarMenu(
      id = "sidebartabs",
      menuItem("Info", tabName = "Info", icon = icon("info")),
      menuItem("Visualize", tabName = "Visualize", icon = icon("eye")),
      tags$style(".left-side, .main-sidebar {padding-top: 60px}")
    ),
    collapsed = TRUE,
    disable = TRUE
  ),
  dashboardBody(
    tags$head(
      tags$head(tags$link(rel = "shortcut icon", href = "http://phanstiel-lab.med.unc.edu/CORAL/favicon.ico")),
      
      # adds the d3 library needed to draw the plot
      tags$script(src = "javascript/d3.v3.min.js"),
      
      # the js script holding the code to make the custom output
      tags$script(src = "javascript/circleNetwork.js"),
      tags$script(src = "javascript/collapsableForceNetwork.js"),
      
      # the stylesheet, paste all that was between the <style> tags from your example in the graph_style.css file
      tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")
      
      # # try to resize plot according to window size
      # tags$head(tags$style("#plot1{height:100vh;}")),
      # tags$head(tags$style("#InfoAbout{text-align: left; color: #fff; background-color: #0571B0; height:43px; font-size: 120%; line-height: 140%; letter-spacing: .25px; border-radius: 0;}")),
      # tags$head(tags$style("#InfoUsage{text-align: left; color: #fff; background-color: #0571B0; height:43px; font-size: 120%; line-height: 140%; letter-spacing: .25px; border-radius: 0;}")),
      # tags$head(tags$style("#InfoOther{text-align: left; color: #fff; background-color: #0571B0; height:43px; font-size: 120%; line-height: 140%; letter-spacing: .25px; border-radius: 0;}"))
    ),
    
    # Fix a bug in the texboxInput funciton that doesn't respect width= "100%"
    tags$style(HTML(".shiny-input-container:not(.shiny-input-container-inline) {width: 100%;}")),
    tabItems(
      # First tab content
      tabItem(
        tabName = "Visualize",
        fluidRow(
          width = 12,
          column(
            width = 3,
            # choose between tabs
            radioGroupButtons(
              inputId = "dashboardchooser", label = NULL,
              choices = c("Info", "Plot"),
              selected = "Plot",
              justified = TRUE, status = "primary",
              checkIcon = list(yes = "", no = "")
            ),
            fluidRow(
              width = 12,
              box(
                width = 12, title = "Tercen API", 
                status = "danger", solidHeader = TRUE, collapsible = FALSE,
                sliderInput("Threshold", "Threshold:", 1.2, min = 0, max = 3, step = 0.1),
                selectInput(inputId = "colorVar", label = "Branch color variable",
                            choices = getColorvars(), selected = getColorvars()[0],
                            width="100%"),
                selectInput(inputId = "labelVar", label = "Branch size variable",
                            choices = getLabelvars(), selected = getLabelvars()[0],
                            width="100%"),
                actionButton("parseInput", "Load Tercen data")
              ) # //: BOX
            ), # //: FLUIDROW,
            
            
            fluidRow(
              width = 12,
              box(
                width = 12, title = "Branch Color", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                selectInput(
                  inputId = "branchcolortype", label = "Color Scheme",
                  choices = c("Uniform", "Manual", "Categorical", "Quantitative"),
                  multiple = FALSE, selected = "Quantitative", width = "100%"
                ),
                
                # if single color
                conditionalPanel(
                  condition = "input.branchcolortype == 'Uniform'",
                  colourInput("col_branch_single", "Color", BG_col1, showColour = "both")
                ),
                
                # if manual selection
                conditionalPanel(
                  condition = "input.branchcolortype == 'Manual'",
                  
                  # choose between selecting and pasting in
                  radioButtons(
                    inputId = "branchmanuallyinputmethod", label = "Input Method",
                    choices = c("browse", "paste"), inline = TRUE
                  ),
                  
                  # if select
                  conditionalPanel(
                    condition = "input.branchmanuallyinputmethod == 'browse'",
                    selectInput(inputId = "KinasesManual", label = "Kinases", choices = svginfo$dataframe$id.coral, multiple = TRUE, width = "100%")
                  ),
                  # if paste
                  conditionalPanel(
                    condition = "input.branchmanuallyinputmethod == 'paste'",
                    textAreaInput("KinasesManualBranchText", "Kinases",
                                  height = "100px", width = "100%",
                                  value = ""
                    )
                  ),
                  selectInput(
                    inputId = "branchManualIDtype", label = "Identifier",
                    choices = c("coralID", "uniprot", "ensembl", "entrez", "HGNC"),
                    multiple = FALSE, selected = "uniprot", width = "100%"
                  ),
                  fluidRow(
                    width = 12,
                    column(6, colourInput("col_select_bg", "Default Color", BG_col1, showColour = "both")),
                    column(6, colourInput("col_select", "Selection Color", Cor_col, showColour = "both"))
                  ),
                  fluidRow(
                    width = 12,
                    column(6, textInput(inputId = "branch_nonselect_label", label = "Default Label", value = "not selected")),
                    column(6, textInput(inputId = "branch_select_label", label = "Selection Label", value = "selected"))
                  ),
                  
                  # add ability to reverse palette
                  actionButton(inputId = "KinasesManualBranchRevPalette", "Reverse Palette", width = "100%")
                ),
                
                # if Categorical
                conditionalPanel(
                  condition = "input.branchcolortype == 'Categorical'",
                  prettyCheckbox(inputId = "loadexamplebranchgroup", label = "load default kinase groups", value = FALSE, shape = "round", status = "primary"),
                  textAreaInput("branchGroupBox", "Kinases & Category",
                                height = "100px", width = "100%",
                                value = ""
                  ),
                  selectInput(
                    inputId = "branchGroupIDtype", label = "Identifier",
                    choices = c("coralID", "uniprot", "ensembl", "entrez", "HGNC"),
                    multiple = FALSE, selected = "uniprot", width = "100%"
                  ),
                  
                  
                  # add option to manually pick groups
                  prettyCheckbox(
                    inputId = "manualgroupcols_branch", "manual category entry",
                    value = FALSE, shape = "round", status = "primary"
                  ),
                  conditionalPanel(
                    condition = "input.manualgroupcols_branch == true",
                    textAreaInput("ManualBranchCategories", "Categories", height = "100px", width = "100%", value = "")
                  ),
                  fluidRow(
                    width = 12,
                    column(
                      6,
                      radioButtons(
                        inputId = "branchgroupcolorpalettetype", label = "Palette Type",
                        choices = c("prebuilt", "manual"), inline = FALSE
                      )
                    ),
                    column(
                      6,
                      conditionalPanel(
                        condition = "input.branchgroupcolorpalettetype == 'prebuilt'",
                        radioButtons_withHTML("branchgroupcolorpalette_qaul", "Palette", choices = qualitative_palette_choices, inline = FALSE)
                      ),
                      conditionalPanel(
                        condition = "input.branchgroupcolorpalettetype == 'manual'",
                        h6("Select Colors"),
                        fluidRow(
                          style = "margin-top: 20px",
                          column(width = 1, colourInput("branchgroupcol1", "", defaultpalette[1], showColour = "both"), style = "margin-top:-29px"),
                          column(width = 1, colourInput("branchgroupcol2", "", defaultpalette[2], showColour = "both"), style = "margin-top:-29px"),
                          column(width = 1, colourInput("branchgroupcol3", "", defaultpalette[3], showColour = "both"), style = "margin-top:-29px")
                        ),
                        fluidRow(
                          column(width = 1, colourInput("branchgroupcol4", "", defaultpalette[4], showColour = "both"), style = "margin-top:-29px"),
                          column(width = 1, colourInput("branchgroupcol5", "", defaultpalette[5], showColour = "both"), style = "margin-top:-29px"),
                          column(width = 1, colourInput("branchgroupcol6", "", defaultpalette[6], showColour = "both"), style = "margin-top:-29px")
                        ),
                        fluidRow(
                          column(width = 1, colourInput("branchgroupcol7", "", defaultpalette[7], showColour = "both"), style = "margin-top:-29px"),
                          column(width = 1, colourInput("branchgroupcol8", "", defaultpalette[8], showColour = "both"), style = "margin-top:-29px"),
                          column(width = 1, colourInput("branchgroupcol9", "", defaultpalette[9], showColour = "both"), style = "margin-top:-29px")
                        ),
                        fluidRow(
                          column(width = 1, colourInput("branchgroupcol10", "", defaultpalette[10], showColour = "both"), style = "margin-top:-29px"),
                          column(width = 1, colourInput("branchgroupcol11", "", defaultpalette[11], showColour = "both"), style = "margin-top:-29px"),
                          column(width = 1, colourInput("branchgroupcol12", "", defaultpalette[12], showColour = "both"), style = "margin-top:-29px")
                        )
                      ) # end conditional
                    ) # end col
                  ), # end row
                  
                  colourInput("defaultbranchcolor_categorical", "Default Color", BG_col1, showColour = "both")
                ),
                
                
                # if Quantitative
                conditionalPanel(
                  condition = "input.branchcolortype == 'Quantitative'",
                  prettyCheckbox(inputId = "loadexamplebranchvalue", label = "load example data", value = FALSE, shape = "round", status = "primary"),
                  textAreaInput("branchValueBox", "Kinases & Value",
                                height = "100px", width = "100%",
                                value = ""
                  ),
                  selectInput(
                    inputId = "branchValueIDtype", label = "Identifier",
                    choices = c("coralID", "uniprot", "ensembl", "entrez", "HGNC"),
                    multiple = FALSE, selected = "uniprot", width = "100%"
                  ),
                  textInput(inputId = "quantvaluenamebranchcolor", label = "Legend Subtitle", value = "Effect Size (Median Kinase Statistic)"),
                  fluidRow(
                    width = 12,
                    column(6, numericInput(inputId = "minheat", label = "Min Value", value = -3)),
                    column(6, numericInput(inputId = "maxheat", label = "Max Value", value = 3))
                  ),
                  fluidRow(
                    width = 12,
                    column(
                      6,
                      radioButtons(
                        inputId = "branchcolorpalettetype", label = "Palette Type",
                        choices = c("sequential", "divergent", "manual 2-color", "manual 3-color"), inline = FALSE
                      )
                    ),
                    column(
                      6,
                      conditionalPanel(
                        condition = "input.branchcolorpalettetype == 'sequential'",
                        radioButtons_withHTML("branchcolorpalette_seq", "Palette", choices = sequential_palette_choices, inline = FALSE),
                        prettyCheckbox("reversebranchpalettesequential", label = "Reverse Palette", shape = "round")
                      ),
                      conditionalPanel(
                        condition = "input.branchcolorpalettetype == 'divergent'",
                        radioButtons_withHTML("branchcolorpalette_div", "Palette", choices = divergent_palette_choices, inline = FALSE),
                        prettyCheckbox("reversebranchpalettedivergent", label = "Reverse Palette", shape = "round")
                      ),
                      conditionalPanel(
                        condition = "input.branchcolorpalettetype == 'manual 2-color'",
                        colourInput("branch2col_low", "Low", HM2_low, showColour = "both"),
                        colourInput("branch2col_hi", "High", HM2_hi, showColour = "both")
                      ),
                      conditionalPanel(
                        condition = "input.branchcolorpalettetype == 'manual 3-color'",
                        colourInput("branch3col_low", "Low", HM_low, showColour = "both"),
                        colourInput("branch3col_med", "Med", HM_med, showColour = "both"),
                        colourInput("branch3col_hi", "High", HM_hi, showColour = "both")
                      )
                    ) # end column
                  ), # end row
                  
                  conditionalPanel(
                    condition = "input.branchcolorpalettetype == 'manual 2-color'",
                    # add ability to reverse palette
                    actionButton(inputId = "KinasesBranchValue2RevPalette", "Reverse Palette", width = "100%")
                  ),
                  conditionalPanel(
                    condition = "input.branchcolorpalettetype == 'manual 3-color'",
                    # add ability to reverse palette
                    actionButton(inputId = "KinasesBranchValue3RevPalette", "Reverse Palette", width = "100%")
                  ),
                  tags$br(),
                  prettyRadioButtons(
                    inputId = "BranchValueMissingKinase", label = "Color Missing Kinases", choices = c("automatically", "manually"),
                    selected = "automatically", inline = TRUE
                  ),
                  conditionalPanel(
                    condition = "input.BranchValueMissingKinase == 'manually'",
                    colourInput(inputId = "BranchValueMissingKinaseColor", "Missing Kinase Color", value = BG_col1, showColour = "both")
                  )
                ) # end conditional panel
              ), # end box
              
              # ---- NODE COLOR ---- #
              
              box(
                width = 12, title = "Node Color", status = "success", solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                selectInput(
                  inputId = "nodecolortype", label = "Color Scheme",
                  #  choices = c("None","Same as branches","Uniform","Manual","Categorical","Quantitative"),
                  choices = c("None", "Uniform", "Manual", "Categorical", "Quantitative"),
                  multiple = FALSE, selected = "Quantitative", width = "100%"
                ),
                
                # if single color
                conditionalPanel(
                  condition = "input.nodecolortype == 'Uniform'",
                  colourInput("col_node_single", "Color", BG_col1)
                ),
                
                # if manual selection
                conditionalPanel(
                  condition = "input.nodecolortype == 'Manual'",
                  
                  # choose between selecting and pasting in
                  radioButtons(
                    inputId = "nodemanuallyinputmethod", label = "Input Method",
                    choices = c("browse", "paste"), inline = TRUE
                  ),
                  
                  # if select
                  conditionalPanel(
                    condition = "input.nodemanuallyinputmethod == 'browse'",
                    selectInput(inputId = "KinasesManualNode", label = "Kinases", choices = svginfo$dataframe$id.coral, multiple = TRUE, width = "100%")
                  ),
                  # if paste
                  conditionalPanel(
                    condition = "input.nodemanuallyinputmethod == 'paste'",
                    textAreaInput("KinasesManualNodeText", "Kinases",
                                  height = "100px", width = "100%",
                                  value = ""
                    )
                  ),
                  selectInput(
                    inputId = "NodeManualIDtype", label = "Identifier",
                    choices = c("coralID", "uniprot", "ensembl", "entrez", "HGNC"),
                    multiple = FALSE, selected = "coralID", width = "100%"
                  ),
                  fluidRow(
                    width = 12,
                    column(6, colourInput("col_node_bg", "Default Color", BG_col1, showColour = "both")),
                    column(6, colourInput("col_sel_node", "Selection Color", Cor_col, showColour = "both"))
                  ),
                  fluidRow(
                    width = 12,
                    column(6, textInput(inputId = "node_nonselect_label", label = "Default Label", value = "not selected")),
                    column(6, textInput(inputId = "node_select_label", label = "Selection Label", value = "selected"))
                  ),
                  
                  # add ability to reverse palette
                  actionButton(inputId = "KinasesManualNodeRevPalette", "Reverse Palette", width = "100%")
                ),
                
                # if Categorical
                conditionalPanel(
                  condition = "input.nodecolortype == 'Categorical'",
                  prettyCheckbox(inputId = "loadexamplennodegroup", label = "load default kinase groups", value = FALSE, shape = "round", status = "primary"),
                  textAreaInput("nodeGroupBox", "Kinases & Category",
                                height = "100px", width = "100%",
                                value = ""
                  ),
                  selectInput(
                    inputId = "nodeGroupIDtype", label = "Identifier",
                    choices = c("coralID", "uniprot", "ensembl", "entrez", "HGNC"),
                    multiple = FALSE, selected = "uniprot", width = "100%"
                  ),
                  
                  # add option to manually pick groups
                  prettyCheckbox(
                    inputId = "manualgroupcols_node", "manual category entry",
                    value = FALSE, shape = "round", status = "primary"
                  ),
                  conditionalPanel(
                    condition = "input.manualgroupcols_node == true",
                    textAreaInput("ManualNodeCategories", "Categories", height = "100px", width = "100%", value = "")
                  ),
                  fluidRow(
                    width = 12,
                    column(
                      6,
                      radioButtons(
                        inputId = "nodegroupcolorpalettetype", label = "Palette Type",
                        choices = c("prebuilt", "manual"), inline = FALSE
                      )
                    ),
                    column(
                      6,
                      conditionalPanel(
                        condition = "input.nodegroupcolorpalettetype == 'prebuilt'",
                        radioButtons_withHTML("nodegroupcolorpalette_qaul", "Palette", choices = qualitative_palette_choices, inline = FALSE)
                      ),
                      conditionalPanel(
                        condition = "input.nodegroupcolorpalettetype == 'manual'",
                        h6("Select Colors"),
                        fluidRow(
                          style = "margin-top: 20px",
                          column(width = 1, colourInput("nodegroupcol1", "", defaultpalette[1], showColour = "both"), style = "margin-top:-29px"),
                          column(width = 1, colourInput("nodegroupcol2", "", defaultpalette[2], showColour = "both"), style = "margin-top:-29px"),
                          column(width = 1, colourInput("nodegroupcol3", "", defaultpalette[3], showColour = "both"), style = "margin-top:-29px")
                        ),
                        fluidRow(
                          column(width = 1, colourInput("nodegroupcol4", "", defaultpalette[4], showColour = "both"), style = "margin-top:-29px"),
                          column(width = 1, colourInput("nodegroupcol5", "", defaultpalette[5], showColour = "both"), style = "margin-top:-29px"),
                          column(width = 1, colourInput("nodegroupcol6", "", defaultpalette[6], showColour = "both"), style = "margin-top:-29px")
                        ),
                        fluidRow(
                          column(width = 1, colourInput("nodegroupcol7", "", defaultpalette[7], showColour = "both"), style = "margin-top:-29px"),
                          column(width = 1, colourInput("nodegroupcol8", "", defaultpalette[8], showColour = "both"), style = "margin-top:-29px"),
                          column(width = 1, colourInput("nodegroupcol9", "", defaultpalette[9], showColour = "both"), style = "margin-top:-29px")
                        ),
                        fluidRow(
                          column(width = 1, colourInput("nodegroupcol10", "", defaultpalette[10], showColour = "both"), style = "margin-top:-29px"),
                          column(width = 1, colourInput("nodegroupcol11", "", defaultpalette[11], showColour = "both"), style = "margin-top:-29px"),
                          column(width = 1, colourInput("nodegroupcol12", "", defaultpalette[12], showColour = "both"), style = "margin-top:-29px")
                        )
                      ) # end conditional
                    ) # end col
                  ), # end row
                  
                  colourInput("defaultnodecolor_categorical", "Default Color", BG_col1, showColour = "both")
                ),
                
                # if Quantitative
                conditionalPanel(
                  condition = "input.nodecolortype == 'Quantitative'",
                  prettyCheckbox(inputId = "loadexamplennodevalue", label = "load example data", value = FALSE, shape = "round", status = "primary"),
                  textAreaInput("nodeValueBox", "Kinases & Value",
                                height = "100px", width = "100%",
                                value = ""
                  ),
                  selectInput(
                    inputId = "nodeValueIDtype", label = "Identifier",
                    choices = c("coralID", "uniprot", "ensembl", "entrez", "HGNC"),
                    multiple = FALSE, selected = "uniprot", width = "100%"
                  ),
                  textInput(inputId = "quantvaluenamenodecolor", label = "Legend Subtitle", value = "Effect Size (Median Kinase Statistic)"),
                  fluidRow(
                    width = 12,
                    column(6, numericInput(inputId = "nodeminheat", label = "Min Value", value = -3)),
                    column(6, numericInput(inputId = "nodemaxheat", label = "Max Value", value = 3))
                  ),
                  fluidRow(
                    width = 12,
                    column(
                      6,
                      radioButtons(
                        inputId = "nodecolorpalettetype", label = "Palette Type",
                        choices = c("sequential", "divergent", "manual 2-color", "manual 3-color"), inline = FALSE
                      )
                    ),
                    column(
                      6,
                      conditionalPanel(
                        condition = "input.nodecolorpalettetype == 'sequential'",
                        radioButtons_withHTML("nodecolorpalette_seq", "Palette", choices = sequential_palette_choices, inline = FALSE),
                        prettyCheckbox("reversenodepalettesequential", label = "Reverse Palette", shape = "round")
                      ),
                      conditionalPanel(
                        condition = "input.nodecolorpalettetype == 'divergent'",
                        radioButtons_withHTML("nodecolorpalette_div", "Palette", choices = divergent_palette_choices, inline = FALSE),
                        prettyCheckbox("reversenodepalettedivergent", label = "Reverse Palette", shape = "round")
                      ),
                      conditionalPanel(
                        condition = "input.nodecolorpalettetype == 'manual 2-color'",
                        colourInput("node2col_low", "Low", HM2_low, showColour = "both"),
                        colourInput("node2col_hi", "High", HM2_hi, showColour = "both")
                      ),
                      conditionalPanel(
                        condition = "input.nodecolorpalettetype == 'manual 3-color'",
                        colourInput("node3col_low", "Low", HM_low, showColour = "both"),
                        colourInput("node3col_med", "Med", HM_med, showColour = "both"),
                        colourInput("node3col_hi", "High", HM_hi, showColour = "both")
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.nodecolorpalettetype == 'manual 2-color'",
                    # add ability to reverse palette
                    actionButton(inputId = "KinasesNodeValue2RevPalette", "Reverse Palette", width = "100%")
                  ),
                  conditionalPanel(
                    condition = "input.nodecolorpalettetype == 'manual 3-color'",
                    # add ability to reverse palette
                    actionButton(inputId = "KinasesNodeValue3RevPalette", "Reverse Palette", width = "100%")
                  ),
                  tags$br(),
                  div(
                    prettyRadioButtons(
                      inputId = "NodeValueMissingKinase", label = "Color Missing Kinases", choices = c("automatically", "manually"),
                      selected = "automatically", inline = TRUE
                    ),
                    conditionalPanel(
                      condition = "input.NodeValueMissingKinase == 'manually'",
                      colourInput("NodeValueMissingKinaseColor", "Missing Kinase Color", value = BG_col1, showColour = "both")
                    )
                  )
                ), # end conditional
                
                tags$br(),
                div(prettyCheckbox(inputId = "colorsubnodes", label = "color intermediate nodes", value = TRUE, shape = "round", status = "primary"))
              ), # end box
              
              # ---- NODE SIZE ---- #
              
              box(
                width = 12, title = "Node Size", status = "info", solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                
                # only allow node size modification if nodes are colored
                
                conditionalPanel(
                  condition = "input.nodecolortype == 'None'",
                  tags$b("Node Size disabled when Node Color Set to 'None'."),
                  tags$br(),
                  "In order to set node size please adust node color options in the box above."
                ),
                conditionalPanel(
                  condition = "input.nodecolortype != 'None'",
                  selectInput(
                    inputId = "nodesizetype", label = "Scaling Scheme",
                    choices = c("One Size", "Quantitative"),
                    multiple = FALSE, selected = "Quantitative", width = "100%"
                  ),
                  
                  # if single color
                  conditionalPanel(
                    condition = "input.nodesizetype == 'One Size'",
                    sliderInput("size_node_single", "Size", value = 5, min = 0, max = 20, step = 0.25)
                  ),
                  
                  # if Quantitative
                  conditionalPanel(
                    condition = "input.nodesizetype == 'Quantitative'",
                    prettyCheckbox(inputId = "loadexamplennodesizevalue", label = "load example data", value = FALSE, shape = "round", status = "primary"),
                    textAreaInput("nodesizeValueBox", "Kinases & Value",
                                  height = "100px", width = "100%",
                                  value = ""
                    ),
                    selectInput(
                      inputId = "nodesizeValueIDtype", label = "Identifier",
                      choices = c("coralID", "uniprot", "ensembl", "entrez", "HGNC"),
                      multiple = FALSE, selected = "uniprot", width = "100%"
                    ),
                    textInput(inputId = "quantvaluenamenodesize", label = "Legend Subtitle", value = "Kinase Rank (Median Final Score)"),
                    sliderInput("nodesizeValueslider", label = "Size Range", value = c(3, 9), min = 0, max = 20, step = 0.25),
                    
                    # how to handle for values not provides
                    prettyRadioButtons(
                      inputId = "nodesizefornotprovidedquantitative", label = "Missing Kinases",
                      choices = c("show", "hide"),
                      selected = "hide"
                    ),
                    prettyCheckbox("Manuallysetdatarange", "manually set data range", value = TRUE, shape = "round", status = "primary"),
                    conditionalPanel(
                      condition = "input.Manuallysetdatarange == true",
                      fluidRow(
                        width = 12,
                        column(6, numericInput(inputId = "nodesizevaluemin", label = "Min Value", value = 0)),
                        column(6, numericInput(inputId = "nodesizevaluemax", label = "Max Value", value = 1))
                      )
                    )
                  ) # end box
                ) # end conditional
              ),
              
              # ---- ADVANCED SETTINGS ---- #
              
              # fluidRow(width=12,
              box(
                width = 12,
                title = "Advanced Settings", status = "warning", solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                prettyRadioButtons("AdvancedSections",
                                   label = "",
                                   choices = c("Title", "Labels", "Node"),
                                   selected = "Labels", inline = TRUE
                ),
                conditionalPanel(
                  condition = "input.AdvancedSections == 'Title'",
                  # text box for title
                  textInput(inputId = "titleinput", label = "Title", value = "")
                ),
                conditionalPanel(
                  condition = "input.AdvancedSections == 'Labels'",
                  
                  # Choose Label
                  selectInput(
                    inputId = "kinaselabelselect", label = "Label Identifier",
                    choices = c("Default", "coralID", "uniprot", "ensembl", "entrez", "HGNC"),
                    multiple = FALSE, selected = "Helvetica", width = "100%"
                  ),
                  
                  # Choose Font
                  selectInput(
                    inputId = "fontfamilyselect", label = "Font",
                    choices = c("Helvetica", "Arial", "Verdana", "Trebuchet MS", "Times New Roman", "Garamond"),
                    multiple = FALSE, selected = "Helvetica", width = "100%"
                  ),
                  selectInput(
                    inputId = "fontcolorselect", label = "Label Color Scheme",
                    choices = c("Same as Branch", "Single Color", "Manual"),
                    multiple = FALSE, selected = "Manual", width = "100%"
                  ),
                  conditionalPanel(
                    condition = "input.fontcolorselect == 'Single Color'|| input.fontcolorselect == 'Same as Branch'",
                    # Slider for font size
                    sliderInput("fontsize", "Label Font Size", min = 0, max = 8, value = 4, step = 0.05, ticks = F)
                  ),
                  conditionalPanel(
                    condition = "input.fontcolorselect == 'Single Color'",
                    colourInput("fontcolorchoose", "Label Color", "#000000")
                  ),
                  
                  
                  # options for manually highlighting labels
                  conditionalPanel(
                    condition = "input.fontcolorselect == 'Manual'",
                    textAreaInput("KinasesManualLabelsText", "Kinases", height = "100px", width = "100%", value = ""),
                    selectInput(
                      inputId = "labelsManualIDtype", label = "Identifier",
                      choices = c("coralID", "uniprot", "ensembl", "entrez", "HGNC"),
                      multiple = FALSE, selected = "uniprot", width = "100%"
                    ),
                    fluidRow(
                      width = 12,
                      column(6, colourInput("fontcolorbackground", "Default Color", "#D3D3D3")),
                      column(6, colourInput("fontcolorselection", "Selection Color", "#000000"))
                    ),
                    fluidRow(
                      width = 12,
                      column(6, sliderInput("fontsizebackground", "Default Size", min = 0, max = 8, value = 0, step = 0.05, ticks = F)),
                      column(6, sliderInput("fontsizeselection", "Selection Size", min = 0, max = 8, value = 7, step = 0.05, ticks = F))
                    )
                  ),
                  colourInput("groupcolorchoose", "Group Color", "#000000")
                ),
                conditionalPanel(
                  condition = "input.AdvancedSections == 'Node'",
                  sliderInput("Node_Opacity", "Opacity", value = 1, min = 0, max = 1, step = 0.01),
                  
                  # How to color stroke of node
                  selectInput(
                    inputId = "nodestrokecolselect", label = "Node Stroke Color Scheme",
                    choices = c("Same as Node", "Single Color", "Selected"),
                    multiple = FALSE, selected = "Same as Node", width = "100%"
                  ),
                  
                  # Node stroke as single color
                  conditionalPanel(
                    condition = "input.nodestrokecolselect == 'Single Color'",
                    colourInput("nodestrokecol", "Node Stroke Color", "#ffffff")
                  ),
                  # Node stroke by selected
                  conditionalPanel(
                    condition = "input.nodestrokecolselect == 'Selected'",
                    textAreaInput("NodeStrokeSelect", "Selected Kinases",
                                  height = "100px", width = "100%",
                                  value = ""
                    ),
                    selectInput(
                      inputId = "NodeStrokeSelectIDtype", label = "Identifier",
                      choices = c("coralID", "uniprot", "ensembl", "entrez", "HGNC"),
                      multiple = FALSE, selected = "coralID", width = "100%"
                    ),
                    colourInput("NodeStrokeSelect_BG", "Not Selected Color", "#ffffff"),
                    colourInput("NodeStrokeSelect_FG", "Selected Color", Cor_col)
                  ) # end condition
                )
              ) # end box
            ), # end row
            
            # ---- DOWNLOAD ---- #
            
            conditionalPanel(
              condition = "input.tabboxselected == 'Tree'",
              radioButtons(inputId = "downloadtype", label = "Download Filetype", choices = c("PNG", "SVG"), selected = "PNG"),
              conditionalPanel(
                condition = "input.downloadtype == 'PNG'",
                sliderTextInput(inputId = "pngDPI",
                                label = "PNG Quality (DPI)",
                                choices = c(72, 150, 200, 300, 600),
                                selected = 150)
              ),
              tipify(downloadButton(outputId = "downloadtree", label = "Download"),
                     title = "Plots download in SVG vector format, which can be opened in web browsers and graphics software. To easily convert to PDF, we recommend opening the SVG in Safari and selecting File > Export to PDF. To convert using another browser, choose the Save as PDF option in the Print menu.",
                     placement = "bottom", trigger = "hover"
              )
            ),
            conditionalPanel(
              condition = "input.tabboxselected == 'Circle'",
              tipify(tags$a(id = "downloadcircle", class = "btn btn-default", "Download"),
                     title = "Plots download in SVG vector format, which can be opened in web browsers and graphics software. To easily convert to PDF, we recommend opening the SVG in Safari and selecting File > Export to PDF. To convert using another browser, choose the Save as PDF option in the Print menu.",
                     placement = "bottom", trigger = "hover"
              )
            ),
            conditionalPanel(
              condition = "input.tabboxselected == 'Force'",
              tipify(tags$a(id = "downloadforce", class = "btn btn-default", "Download"),
                     title = "Plots download in SVG vector format, which can be opened in web browsers and graphics software. To easily convert to PDF, we recommend opening the SVG in Safari and selecting File > Export to PDF. To convert using another browser, choose the Save as PDF option in the Print menu.",
                     placement = "bottom", trigger = "hover"
              )
            )
          ), # end column
          
          column(
            width = 9,
            tabBox
            (
              id = "tabboxselected", width = 12, height = 1000,
              tabPanel
              ("Tree",
                width = 12,
                div(id = "treediv")
              ),
              tabPanel
              ("Circle",
                width = 12,
                shinyjs::useShinyjs(),
                div(id = "circlediv")
              ),
              tabPanel
              ("Force",
                width = 12,
                shinyjs::useShinyjs(),
                div(id = "forcediv")
              )
            ),
            conditionalPanel(
              condition = "input.tabboxselected == 'Tree'",
              div(id = "treedisclaimer", width = 12)
            ),
            conditionalPanel(
              condition = "input.tabboxselected == 'Circle'",
              div(id = "circledisclaimer", width = 12)
            ),
            conditionalPanel(
              condition = "input.tabboxselected == 'Force'",
              div(id = "forcedisclaimer", width = 12)
            )
          )
        ),
        fluidRow(
          width = 12,
          box(
            width = 12, style = "margin-bottom: 45px",
            DT::dataTableOutput("KinaseTable")
          )
        )
      ),
      tabItem(
        tabName = "Info",
        fluidRow(
          width = 12,
          column(
            width = 3,
            
            # choose between tabs
            radioGroupButtons(
              inputId = "dashboardchooser2", label = NULL,
              choices = c("Info", "Plot"),
              selected = "Plot",
              justified = TRUE, status = "primary",
              checkIcon = list(yes = "", no = "")
            ),
            div(
              actionButton("InfoBranchColorButton", label = "Branch Color")
            ),
            tags$br(),
            div(
              actionButton("InfoNodeColorButton", label = "Node Color")
            ),
            tags$br(),
            div(
              actionButton("InfoNodeSizeButton", label = "Node Size")
            ),
            tags$br(),
            div(
              actionButton("InfoAdvancedSettingsButton", label = "Advanced Settings")
            ),
            tags$br(),
            div(
              actionButton("InfoAboutButton", label = "About")
            )
          ), # end column
          
          column
          (
            width = 9, style = "margin-bottom: 40px",
            div(id = "InfoBox")
          )
        ) # end row
      )
    ), # /tabItems
    tags$footer(tags$a(span(img(src = "logos/PhanstielLab.png", height = 16, align = "left", style = "")), href = "http://phanstiel-lab.med.unc.edu/"),
                align = "left", style = "
              position: absolute;
              bottom:0;
              width:101%;
              height:40px;
              color: #eceff3;
              padding: 12px 25px 0px 20px;
              margin-left: -20px;
              background-color: #b8c2cc;
              z-index: 1000;",
                tags$ul("© 2018 Phanstiel Lab", align = "right", style = "font-weight:300") # c4c8cc
    )
  )
)

##################
##### SERVER #####
##################

server <- shinyServer(function(input, output, session) {
  
  # ----------- LOAD TXT DATA --------- #
  
  txtInputDataAll <- reactive({
    # if (length(input$txtInput1) == 0) {
    #   examplebranchvaluedata <- ""
    # }
    if (length(input$txtInput1) > 0) {
      input_fileloc <- input$txtInput1$datapath
      
      for (i in 1:length(input_fileloc)) {
        if (i == 1) {
          df <- read.csv(input_fileloc[i], header = TRUE, sep = "\t")
        } else {
          df_append <- read.csv(input_fileloc[i], header = TRUE, sep = "\t")
          df <- df %>% full_join(df_append)
        }
      }
    }
    df
  })
  
  
  
  # ----------------- MAKE DIVS & USER FILES FOR TREE, CIRCLE, AND FORCE ---------------- #
  
  # set all of the temp files
  subdffile <- tempfile(pattern = "subdf", tmpdir = "tempfiles", fileext = ".txt") # temp file used to make json file
  svgoutfile <- tempfile(pattern = "kintreeout", tmpdir = "tempfiles", fileext = ".svg") # session specific tree svg file
  
  # these plots need to be created here (in server) rather that in UI so that they are unique to each session.  Otherwise there will
  # be cross talk between multiple people using the web server at once
  insertUI(selector = "#treediv", where = "afterEnd", ui = source("R/renderTree.R", local = TRUE)$value)
  insertUI(selector = "#circlediv", where = "afterEnd", ui = div(id = "circlelayout", class = "circleNetwork", jsonfilename = outputjsonshort))
  insertUI(selector = "#forcediv", where = "afterEnd", ui = div(id = "forcelayout", class = "collapsableForceNetwork", jsonfilename = outputjsonshort))
  
  # ----------------- UPDATE MANUAL KINASE SELECTION ---------------- #
  
  # branch color
  observe({
    idtype <- paste("id.", input$branchManualIDtype, sep = "")
    print(idtype)
    if (idtype == "id.coralID") {
      idtype <- "id.coral"
    }
    idstodisplay <- svginfo$dataframe[, which(names(svginfo$dataframe) == idtype)]
    idstodisplay <- unique(idstodisplay)
    idstodisplay <- idstodisplay[which(idstodisplay != "NA")]
    updateSelectInput(session, inputId = "KinasesManual", choices = idstodisplay)
  })
  
  # node color
  observe({
    idtype <- paste("id.", input$NodeManualIDtype, sep = "")
    if (idtype == "id.coralID") {
      idtype <- "id.coral"
    }
    idstodisplay <- svginfo$dataframe[, which(names(svginfo$dataframe) == idtype)]
    idstodisplay <- unique(idstodisplay)
    idstodisplay <- idstodisplay[which(idstodisplay != "NA")]
    updateSelectInput(session, inputId = "KinasesManualNode", choices = idstodisplay)
  })
  
  # ----------------- PALETTE REVERSALS ---------------- #
  
  observeEvent(input$KinasesManualBranchRevPalette, {
    orig_col_select_bg <- input$col_select_bg
    orig_col_select <- input$col_select
    updateColourInput(session, "col_select_bg", value = orig_col_select)
    updateColourInput(session, "col_select", value = orig_col_select_bg)
  })
  
  observeEvent(input$KinasesBranchValue2RevPalette, {
    orig_branch2col_low <- input$branch2col_low
    orig_branch2col_hi <- input$branch2col_hi
    updateColourInput(session, "branch2col_low", value = orig_branch2col_hi)
    updateColourInput(session, "branch2col_hi", value = orig_branch2col_low)
  })
  
  observeEvent(input$KinasesBranchValue3RevPalette, {
    orig_branch3col_low <- input$branch3col_low
    orig_branch3col_hi <- input$branch3col_hi
    updateColourInput(session, "branch3col_low", value = orig_branch3col_hi)
    updateColourInput(session, "branch3col_hi", value = orig_branch3col_low)
  })
  
  observeEvent(input$KinasesManualNodeRevPalette, {
    orig_col_node_bg <- input$col_node_bg
    orig_col_sel_node <- input$col_sel_node
    updateColourInput(session, "col_node_bg", value = orig_col_sel_node)
    updateColourInput(session, "col_sel_node", value = orig_col_node_bg)
  })
  
  observeEvent(input$KinasesNodeValue2RevPalette, {
    orig_node2col_low <- input$node2col_low
    orig_node2col_hi <- input$node2col_hi
    updateColourInput(session, "node2col_low", value = orig_node2col_hi)
    updateColourInput(session, "node2col_hi", value = orig_node2col_low)
  })
  
  observeEvent(input$KinasesNodeValue3RevPalette, {
    orig_node3col_low <- input$node3col_low
    orig_node3col_hi <- input$node3col_hi
    updateColourInput(session, "node3col_low", value = orig_node3col_hi)
    updateColourInput(session, "node3col_hi", value = orig_node3col_low)
  })
  
  # ----------------- INFO PAGES ---------------- #
  
  # Define a function to remove and replace info boxes
  replaceinfobox <- function(infoboxcode) {
    # remove any existing info boxes
    removeUI(selector = "#InfoBranchColorBox")
    removeUI(selector = "#InfoNodeColorBox")
    removeUI(selector = "#InfoNodeSizeBox")
    removeUI(selector = "#InfoAdvancedOptionsBox")
    removeUI(selector = "#InfoAboutBox")
    # load code to insert selected info box
    insertUI(selector = "#InfoBox", where = "afterEnd", ui = source(infoboxcode, local = TRUE)$value)
  }
  
  
  # Make the about page the default page
  replaceinfobox("R/InfoAbout.R")
  
  # Load info box according to button click
  observeEvent(input$InfoBranchColorButton, {
    replaceinfobox("R/InfoBranchColor.R")
  })
  observeEvent(input$InfoNodeColorButton, {
    replaceinfobox("R/InfoNodeColor.R")
  })
  observeEvent(input$InfoNodeSizeButton, {
    replaceinfobox("R/InfoNodeSize.R")
  })
  observeEvent(input$InfoAdvancedSettingsButton, {
    replaceinfobox("R/InfoAdvancedOptions.R")
  })
  observeEvent(input$InfoAboutButton, {
    replaceinfobox("R/InfoAbout.R")
  })
  
  # Update selected tab
  observe({
    if (input$dashboardchooser == "Info") {
      updateTabItems(session, inputId = "sidebartabs", selected = "Info")
      updateRadioGroupButtons(session, inputId = "dashboardchooser2", selected = "Info")
    }
  })
  
  observe({
    if (input$dashboardchooser2 == "Plot") {
      updateTabItems(session, inputId = "sidebartabs", selected = "Visualize")
      updateRadioGroupButtons(session, inputId = "dashboardchooser", selected = "Plot")
    }
  })
  
  # ----------------- LOAD EXAMPLE DATA ---------------- #
  
  # Load example data for branches color by group
  observe({
    if (input$loadexamplebranchgroup == FALSE) {
      kinasegroupinfobr <- ""
    }
    if (input$loadexamplebranchgroup == TRUE) {
      kinasegroupinfobr <- paste(apply(data.frame(svginfo$dataframe$id.coral, svginfo$dataframe$kinase.group), 1, paste, collapse = "\t"), collapse = "\n")
      updateTextInput(session, "branchGroupIDtype", value = "coralID")
    }
    updateTextInput(session, "branchGroupBox", value = kinasegroupinfobr)
  })
  
  # Load example data for nodes color by group
  observe({
    if (input$loadexamplennodegroup == FALSE) {
      kinasegroupinfono <- ""
    }
    if (input$loadexamplennodegroup == TRUE) {
      kinasegroupinfono <- paste(apply(data.frame(svginfo$dataframe$id.coral, svginfo$dataframe$kinase.group), 1, paste, collapse = "\t"), collapse = "\n")
      updateTextInput(session, "nodeGroupIDtype", value = "coralID")
    }
    updateTextInput(session, "nodeGroupBox", value = kinasegroupinfono)
  })
  
  # Load example data for branches color Quantitative
  observe({
    if (input$loadexamplebranchvalue == FALSE) {
      examplebranchvaluedata <- ""
    }
    if (input$loadexamplebranchvalue == TRUE) {
      examplebranchvaluedata <- rna_data
      updateTextInput(session, "branchValueIDtype", value = "HGNC")
    }
    updateRadioButtons(session, "branchcolorpalettetype", selected = "divergent")
    updateTextInput(session, "branchValueBox", value = examplebranchvaluedata)
  })
  
  # Load example data for nodes color Quantitative
  observe({
    if (input$loadexamplennodevalue == FALSE) {
      examplenodevaluedata <- ""
    }
    if (input$loadexamplennodevalue == TRUE) {
      examplenodevaluedata <- rna_data
      updateTextInput(session, "nodeValueIDtype", value = "HGNC")
    }
    updateRadioButtons(session, "nodecolorpalettetype", selected = "divergent")
    updateTextInput(session, "nodeValueBox", value = examplenodevaluedata)
  })
  
  # Load example data for nodes size Quantitative
  observe({
    if (input$loadexamplennodesizevalue == FALSE) {
      examplenodesizevaluedata <- ""
    }
    if (input$loadexamplennodesizevalue == TRUE) {
      examplenodesizevaluedata <- rna_abs_data
      
      # set the correct ID type
      updateTextInput(session, "nodesizeValueIDtype", value = "HGNC")
      
      # update the manual inpout checkbox
      updatePrettyCheckbox(session, "Manuallysetdatarange", value = TRUE)
      
      # Set the input range
      updateNumericInput(session, "nodesizevaluein", value = 0)
      updateNumericInput(session, "nodesizevaluemax", value = 100)
      
      # Set the size range
      updateSliderInput(session, "nodesizeValueslider", value = c(4, 16))
    }
    updateTextInput(session, "nodesizeValueBox", value = examplenodesizevaluedata)
  })
  
  # ----------------- PLOT FROM TXT FILE ---------------- #
  
  observeEvent(input$parseInput, {
    kinaseData <- getValues()$data
    
    # # Only run this if an additional file is uploaded
    
    # if (input$Addextrafile == TRUE) {
    #   if (file.exists(input$txtInput2$datapath)) {
    #     kinaseData2 <- txtInputData2()
    
    #     kinaseData <- rbind(kinaseData, kinaseData2)
    #   }
    # }
    
    # Score cutoff at 1.2
    kinaseData <- kinaseData %>% filter(Median.Final.score > input$Threshold)
    kinaseNodeSize <<- kinaseData[, c("Kinase.Uniprot.ID", input$labelVar)]
    kinaseNodeColor <<- kinaseData[, c("Kinase.Uniprot.ID", input$colorVar)]
    
    # Update branch color
    updateTextInput(session, "branchValueIDtype", value = "uniprot")
    kinaseBranches <- paste(apply(data.frame(kinaseNodeColor$Kinase.Uniprot.ID, kinaseNodeColor[, input$colorVar]), 1, paste, collapse = "\t"), collapse = "\n")
    updateTextInput(session, "branchValueBox", value = kinaseBranches)
    # Update branch color data range
    minColor <- floor(min(kinaseNodeColor[, input$colorVar]))
    maxColor <- ceiling(max(kinaseNodeColor[, input$colorVar]))
    if (maxColor < abs(minColor)) {
      maxColor <- minColor * -1 # reverse min branch color
    } else if (maxColor > abs(minColor) | maxColor == abs(minColor)) {
      minColor <- maxColor * -1
    }
    updateNumericInput(session, "minheat", value = minColor)
    updateNumericInput(session, "maxheat", value = maxColor)
    
    
    
    # Update node color
    updateTextInput(session, "nodeValueIDtype", value = "uniprot")
    updateRadioButtons(session, "nodecolorpalettetype", selected = "divergent")
    kinaseNodeColors <- paste(apply(data.frame(kinaseNodeColor$Kinase.Uniprot.ID, kinaseNodeColor[, input$colorVar]), 1, paste, collapse = "\t"), collapse = "\n")
    updateTextInput(session, "nodeValueBox", value = kinaseNodeColors)
    # Update node color data range
    updateNumericInput(session, "nodeminheat", value = minColor)
    updateNumericInput(session, "nodemaxheat", value = maxColor)
    
    # Update node size
    updateTextInput(session, "nodesizeValueIDtype", value = "uniprot")
    kinaseNodeSizes <- paste(apply(data.frame(kinaseNodeSize$Kinase.Uniprot.ID, kinaseNodeSize[, input$labelVar]), 1, paste, collapse = "\t"), collapse = "\n")
    updateTextInput(session, "nodesizeValueBox", value = kinaseNodeSizes)
    # Update node size data range, cutoff 1.2
    minSizeDataRange <- 1
    maxMedianFinalScore <- max(kinaseNodeSize[, input$labelVar])
    maxSizeDataRange <- ceiling(maxMedianFinalScore / 0.5) * 0.5
    updateNumericInput(session, "nodesizevaluemin", value = minSizeDataRange)
    updateNumericInput(session, "nodesizevaluemax", value = maxSizeDataRange)
    
    
    # Only plot kinases that are in the dataset
    kinaseNames <- paste(apply(data.frame(kinaseNodeColor$Kinase.Uniprot.ID), 1, paste, collapse = "\t"), collapse = "\n")
    updateTextInput(session, "KinasesManualLabelsText", value = kinaseNames)
  })
  
  
  
  
  # ----------------- MAIN REACTIVE FUNCTION ---------------- #
  
  newdf <- reactive({
    
    # refresh when refresh button is clicked
    input$refreshForcePlot
    
    # get current values
    tempdf <- svginfo$dataframe
    
    # set font family
    tempdf$text.font <- paste("'", input$fontfamilyselect, "'", sep = "")
    
    # establish legend
    legend <- c()
    # Set initial yoffset
    yoffset <- 79.125
    
    # get current values
    tempdf$text.size <- input$fontsize
    
    # Single branch color
    if (input$branchcolortype == "Uniform") {
      tempdf$branch.col <- input$col_branch_single
    }
    
    # Manually select branches to color
    if (input$branchcolortype == "Manual") {
      # set colors based on selected ids
      selkinases <- ""
      if (input$branchmanuallyinputmethod == "browse") {
        selkinases <- input$KinasesManual
      }
      if (input$branchmanuallyinputmethod == "paste") {
        selkinases <- unlist(strsplit(split = "\n", x = input$KinasesManualBranchText))
      }
      
      selkinasescoral <- ""
      if (length(selkinases) > 0) {
        # convert selected to coral ids
        kinasestoconvert <- data.frame(kin1 = selkinases, kin2 = selkinases)
        selkinasesconverted <- convertID(tempdf, kinasestoconvert, inputtype = input$branchManualIDtype)
        if (nrow(selkinasesconverted) > 0) {
          selkinasescoral <- selkinasesconverted[, 1]
        }
      }
      
      # recolor based on selection
      tempdf$branch.col <- color.by.selected(df = tempdf, sel = selkinasescoral, bg.col = input$col_select_bg, sel.col = input$col_select)
      
      # reorder based on selected ids
      # tempdf = tempdf[order(tempdf$id.coral %in% selkinasescoral, decreasing = FALSE),]
      tempdf$branchorder <- order(tempdf$id.coral %in% selkinasescoral, decreasing = FALSE)
      
      # build legend for Branch Color (manual selection)
      lines_and_offset <- build.group.legend(yoffset = yoffset, groupslabels = c(input$branch_select_label, input$branch_nonselect_label), groupcolors = c(input$col_select, input$col_select_bg), elementtype = "Branch", fontfamily = input$fontfamilyselect)
      lines <- lines_and_offset[[1]]
      yoffset <- lines_and_offset[[2]] + 14
      legend <- c(legend, lines)
    }
    
    # color branches by group
    if (input$branchcolortype == "Categorical") {
      # define color palette
      if (input$branchgroupcolorpalettetype == "prebuilt") {
        branchgroupcolpalette <- unlist(qualpalettes[input$branchgroupcolorpalette_qaul])
      }
      if (input$branchgroupcolorpalettetype == "manual") {
        branchgroupcolpalette <- c(
          input$branchgroupcol1, input$branchgroupcol2, input$branchgroupcol3, input$branchgroupcol4,
          input$branchgroupcol5, input$branchgroupcol6, input$branchgroupcol7, input$branchgroupcol8,
          input$branchgroupcol9, input$branchgroupcol10, input$branchgroupcol11, input$branchgroupcol12
        )
      }
      
      # read in text area input
      recolordf <- read.text.input(input$branchGroupBox)
      
      # convert to coral id
      recolordf <- convertID(tempdf, recolordf, inputtype = input$branchGroupIDtype)
      
      if (nrow(recolordf) > 0) {
        # check for user supplied groups
        categories <- NULL
        if (input$manualgroupcols_branch == TRUE) {
          categories <- unlist(strsplit(input$ManualBranchCategories, split = "\n"))
          if (length(categories) == 0) {
            categories <- NULL
          }
        }
        
        # set colors based on group
        newcolors_and_colormapping <- color.by.group(df = tempdf, recolordf = recolordf, bg.col = input$defaultbranchcolor_categorical, colors = branchgroupcolpalette, categories = categories)
        tempdf$branch.col <- newcolors_and_colormapping[[1]]
        tempdf$branch.group <- newcolors_and_colormapping[[2]]
        branch.group.colormapping <- newcolors_and_colormapping[[3]]
        
        # reorder based on branch color
        # tempdf = tempdf[order(tempdf$branch.group),]
        
        # reorder based in whether kinase was in text box
        # tempdf = tempdf[order(tempdf$id.coral %in% recolordf[,1], decreasing = FALSE),]
        tempdf$branchorder <- order(tempdf$id.coral %in% recolordf[, 1], decreasing = FALSE)
        
        # build legend for Branch Color (by group)
        lines_and_offset <- build.group.legend(yoffset = yoffset, groupslabels = names(branch.group.colormapping), groupcolors = branch.group.colormapping, elementtype = "Branch", fontfamily = input$fontfamilyselect)
        lines <- lines_and_offset[[1]]
        yoffset <- lines_and_offset[[2]] + 14
        legend <- c(legend, lines)
      }
    }
    
    # color branches Quantitative
    if (input$branchcolortype == "Quantitative") {
      # read in text area input
      recolordf <- read.text.input(input$branchValueBox)
      
      # convert to coral id
      recolordf <- convertID(tempdf, recolordf, inputtype = input$branchValueIDtype)
      
      if (nrow(recolordf) > 0) {
        # establish palette
        if (input$branchcolorpalettetype == "sequential") {
          palettecolors <- unlist(seqpalettes[input$branchcolorpalette_seq])
          if (input$reversebranchpalettesequential == TRUE) {
            palettecolors <- rev(palettecolors)
          }
          branchcolpalette <- colorRampPalette(palettecolors)(11)
          bg.col <- unlist(seqpalettes[input$branchcolorpalette_seq])[1]
        }
        if (input$branchcolorpalettetype == "divergent") {
          palettecolors <- unlist(divpalettes[input$branchcolorpalette_div])
          if (input$reversebranchpalettedivergent == TRUE) {
            palettecolors <- rev(palettecolors)
          }
          branchcolpalette <- colorRampPalette(palettecolors)(11)
          
          bg.col <- unlist(divpalettes[input$branchcolorpalette_div])[2]
        }
        if (input$branchcolorpalettetype == "manual 2-color") {
          branchcolpalette <- colorRampPalette(c(input$branch2col_low, input$branch2col_hi))(11)
          bg.col <- input$branch2col_low
        }
        if (input$branchcolorpalettetype == "manual 3-color") {
          branchcolpalette <- colorRampPalette(c(input$branch3col_low, input$branch3col_med, input$branch3col_hi))(11)
          bg.col <- input$branch3col_med
        }
        
        # recolor missing kinases accordingly
        if (input$BranchValueMissingKinase == "manually") {
          print("asdfdas")
          bg.col <- input$BranchValueMissingKinaseColor
          print(bg.col)
        }
        
        # set colors based on group
        newcolors_and_colormapping <- color.by.value(df = tempdf, recolordf = recolordf, colors = branchcolpalette, heatrange = c(input$minheat, input$maxheat), bg.col = bg.col)
        tempdf$branch.col <- newcolors_and_colormapping[[1]]
        tempdf$branch.val <- newcolors_and_colormapping[[2]]
        
        # reorder based on branch value
        # tempdf = tempdf[order(abs(tempdf$branch.val), decreasing = FALSE,na.last = FALSE),]
        tempdf$branchorder <- order(abs(tempdf$branch.val), decreasing = FALSE, na.last = FALSE)
        
        # add legend info
        lines_and_offset <- build.value.legend(yoffset = yoffset, minval = input$minheat, maxval = input$maxheat, palette = branchcolpalette, elementtype = "Branch", fontfamily = input$fontfamilyselect, subtitle = input$quantvaluenamebranchcolor)
        lines <- lines_and_offset[[1]]
        yoffset <- lines_and_offset[[2]] + 14
        legend <- c(legend, lines)
      }
    }
    
    # ------------------ NODE COLOR ------------------ #
    
    # color nodes by single color
    if (input$nodecolortype == "None") {
      tempdf$node.col <- "none"
    }
    
    # color nodes by single color
    if (input$nodecolortype == "Uniform") {
      tempdf$node.col <- input$col_node_single
    }
    
    if (input$nodecolortype == "Same as branches") {
      tempdf$node.col <- tempdf$branch.col
    }
    
    # Manually select nodes to color
    if (input$nodecolortype == "Manual") {
      # set colors based on selected ids
      selkinases <- ""
      if (input$nodemanuallyinputmethod == "browse") {
        selkinases <- input$KinasesManualNode
      }
      if (input$nodemanuallyinputmethod == "paste") {
        selkinases <- unlist(strsplit(split = "\n", x = input$KinasesManualNodeText))
      }
      
      selkinasescoral <- ""
      tempdf$node.selected <- -1
      if (length(selkinases) > 0) {
        # convert selected to coral ids
        kinasestoconvert <- data.frame(kin1 = selkinases, kin2 = selkinases)
        selkinasesconverted <- convertID(tempdf, kinasestoconvert, inputtype = input$NodeManualIDtype)
        if (nrow(selkinasesconverted) > 0) {
          selkinasescoral <- selkinasesconverted[, 1]
        }
        
        # note them as selected so we can add them to the top of the plot later
        tempdf$node.selected[which(tempdf$id.coral %in% selkinasescoral)] <- 1
      }
      
      # recolor based on selection
      tempdf$node.col <- color.by.selected(df = tempdf, sel = selkinasescoral, bg.col = input$col_node_bg, sel.col = input$col_sel_node)
      
      # # build legend for Node Color (Manual Selection)
      lines_and_offset <- build.group.legend(yoffset = yoffset, groupslabels = c(input$node_select_label, input$node_nonselect_label), groupcolors = c(input$col_sel_node, input$col_node_bg), elementtype = "Node", fontfamily = input$fontfamilyselect)
      lines <- lines_and_offset[[1]]
      yoffset <- lines_and_offset[[2]] + 14
      legend <- c(legend, lines)
    }
    
    # color nodes by group
    if (input$nodecolortype == "Categorical") {
      # define color palette
      if (input$nodegroupcolorpalettetype == "prebuilt") {
        nodegroupcolpalette <- unlist(qualpalettes[input$nodegroupcolorpalette_qaul])
      }
      if (input$nodegroupcolorpalettetype == "manual") {
        nodegroupcolpalette <- c(
          input$nodegroupcol1, input$nodegroupcol2, input$nodegroupcol3, input$nodegroupcol4,
          input$nodegroupcol5, input$nodegroupcol6, input$nodegroupcol7, input$nodegroupcol8,
          input$nodegroupcol9, input$nodegroupcol10, input$nodegroupcol11, input$nodegroupcol12
        )
      }
      
      # read in text area input
      recolordf <- read.text.input(input$nodeGroupBox)
      
      # convert to coral id
      recolordf <- convertID(tempdf, recolordf, inputtype = input$nodeGroupIDtype)
      
      if (nrow(recolordf) > 0) {
        # check for user supplied groups
        categories <- NULL
        if (input$manualgroupcols_node == TRUE) {
          categories <- unlist(strsplit(input$ManualNodeCategories, split = "\n"))
          if (length(categories) == 0) {
            categories <- NULL
          }
        }
        
        # set colors based on group
        newcolors_and_colormapping <- color.by.group(df = tempdf, recolordf = recolordf, bg.col = input$defaultnodecolor_categorical, colors = nodegroupcolpalette, categories = categories)
        tempdf$node.col <- newcolors_and_colormapping[[1]]
        tempdf$node.group <- newcolors_and_colormapping[[2]]
        node.group.colormapping <- newcolors_and_colormapping[[3]]
        
        # build legend for Branch Color (by group)
        lines_and_offset <- build.group.legend(yoffset = yoffset, groupslabels = names(node.group.colormapping), groupcolors = node.group.colormapping, elementtype = "Node", fontfamily = input$fontfamilyselect)
        lines <- lines_and_offset[[1]]
        yoffset <- lines_and_offset[[2]] + 14
        legend <- c(legend, lines)
      }
    }
    
    # color nodes Quantitative
    if (input$nodecolortype == "Quantitative") {
      # read in text area input
      recolordf <- read.text.input(input$nodeValueBox)
      
      # convert to coral id
      recolordf <- convertID(tempdf, recolordf, inputtype = input$nodeValueIDtype)
      
      if (nrow(recolordf) > 0) {
        # establish palette
        if (input$nodecolorpalettetype == "sequential") {
          palettecolors <- unlist(seqpalettes[input$nodecolorpalette_seq])
          if (input$reversenodepalettesequential == TRUE) {
            palettecolors <- rev(palettecolors)
          }
          nodecolpalette <- colorRampPalette(palettecolors)(11)
          
          bg.col <- unlist(seqpalettes[input$nodecolorpalette_seq])[1]
        }
        if (input$nodecolorpalettetype == "divergent") {
          palettecolors <- unlist(divpalettes[input$nodecolorpalette_div])
          if (input$reversenodepalettedivergent == TRUE) {
            palettecolors <- rev(palettecolors)
          }
          nodecolpalette <- colorRampPalette(palettecolors)(11)
          bg.col <- unlist(divpalettes[input$nodecolorpalette_div])[2]
        }
        if (input$nodecolorpalettetype == "manual 2-color") {
          nodecolpalette <- colorRampPalette(c(input$node2col_low, input$node2col_hi))(11)
          bg.col <- input$node2col_low
        }
        if (input$nodecolorpalettetype == "manual 3-color") {
          nodecolpalette <- colorRampPalette(c(input$node3col_low, input$node3col_med, input$node3col_hi))(11)
          bg.col <- input$node3col_med
        }
        
        # recolor missing kinases accordingly
        if (input$NodeValueMissingKinase == "manually") {
          bg.col <- input$NodeValueMissingKinaseColor
        }
        
        # set colors based on value
        newcolors_and_colormapping <- color.by.value(df = tempdf, recolordf = recolordf, colors = nodecolpalette, heatrange = c(input$nodeminheat, input$nodemaxheat), bg.col = bg.col)
        tempdf$node.col <- newcolors_and_colormapping[[1]]
        tempdf$node.val <- newcolors_and_colormapping[[2]]
        
        # reorder based on branch color
        # tempdf = tempdf[order(abs(tempdf$node.val), decreasing = FALSE,na.last = FALSE),]
        tempdf$nodeorder <- order(abs(tempdf$node.val), decreasing = FALSE, na.last = FALSE)
        
        # add legend info
        lines_and_offset <- build.value.legend(yoffset = yoffset, minval = input$nodeminheat, maxval = input$nodemaxheat, palette = nodecolpalette, elementtype = "Node", fontfamily = input$fontfamilyselect, subtitle = input$quantvaluenamenodecolor)
        lines <- lines_and_offset[[1]]
        yoffset <- lines_and_offset[[2]] + 14
        legend <- c(legend, lines)
      }
    }
    
    # ------------------ NODE SIZE ------------------ #
    
    # color nodes by single color
    if (input$nodesizetype == "One Size") {
      tempdf$node.radius <- input$size_node_single
    }
    
    # color nodes by single color
    if (input$nodesizetype == "Quantitative") {
      # read in text area input
      resizedf <- read.text.input(input$nodesizeValueBox)
      
      # convert to coral id
      resizedf <- convertID(tempdf, resizedf, inputtype = input$nodesizeValueIDtype)
      
      if (nrow(resizedf) > 0) {
        radii_and_mapping <- resizes.by.value(
          df = tempdf, resizedf = resizedf, sizerange = input$nodesizeValueslider,
          controlledrange = input$Manuallysetdatarange, minvalue = input$nodesizevaluemin, maxvalue = input$nodesizevaluemax, showall = input$nodesizefornotprovidedquantitative
        )
        
        # Get correct limits for legend
        if (input$Manuallysetdatarange == FALSE) {
          minvalforlegend <- min(as.numeric(resizedf[, 2]))
          maxvalforlegend <- max(as.numeric(resizedf[, 2]))
        }
        if (input$Manuallysetdatarange == TRUE) {
          minvalforlegend <- input$nodesizevaluemin
          maxvalforlegend <- input$nodesizevaluemax
        }
        
        tempdf$node.radius <- radii_and_mapping[[1]]
        tempdf$node.val.radius <- radii_and_mapping[[2]]
        
        # add legend info
        lines_and_offset <- build.nodesize.legend(yoffset = yoffset, minval = minvalforlegend, maxval = maxvalforlegend, minsize = input$nodesizeValueslider[1], maxsize = input$nodesizeValueslider[2], fontfamily = input$fontfamilyselect, subtitle = input$quantvaluenamenodesize)
        lines <- lines_and_offset[[1]]
        yoffset <- lines_and_offset[[2]] + 14
        legend <- c(legend, lines)
      }
    }
    
    # ------------------ ADVANCED OPTIONS ------------------ #
    
    tempdf$node.opacity <- input$Node_Opacity
    
    # text color
    if (input$fontcolorselect == "Same as Branch") {
      tempdf$text.col <- tempdf$branch.col
    }
    
    if (input$fontcolorselect == "Single Color") {
      tempdf$text.col <- input$fontcolorchoose
    }
    
    # Change Color and Size of Font for Selected kinases
    if (input$fontcolorselect == "Manual") {
      selkinases <- unlist(strsplit(split = "\n", x = input$KinasesManualLabelsText))
      
      selkinasescoral <- ""
      if (length(selkinases) > 0) {
        # convert selected to coral ids
        kinasestoconvert <- data.frame(kin1 = selkinases, kin2 = selkinases)
        selkinasesconverted <- convertID(tempdf, kinasestoconvert, inputtype = input$labelsManualIDtype)
        if (nrow(selkinasesconverted) > 0) {
          selkinasescoral <- selkinasesconverted[, 1]
        }
      }
      
      # set background color and font size
      tempdf$text.col <- input$fontcolorbackground
      tempdf$text.size <- input$fontsizebackground
      
      # set selected font color and size
      tempdf$text.col[which(tempdf$id.coral %in% selkinasescoral)] <- input$fontcolorselection
      tempdf$text.size[which(tempdf$id.coral %in% selkinasescoral)] <- input$fontsizeselection
    }
    
    
    
    # Node stroke color
    if (input$nodestrokecolselect == "Single Color") {
      tempdf$node.strokecol <- input$nodestrokecol
    }
    if (input$nodestrokecolselect == "Same as Node") {
      tempdf$node.strokecol <- tempdf$node.col
    }
    if (input$nodestrokecolselect == "Selected") {
      tempdf$node.strokecol <- input$NodeStrokeSelect_BG
      
      # read in text area input
      kinases <- unlist(strsplit(x = input$NodeStrokeSelect, split = "\\n"))
      
      if (length(kinases) > 0) {
        df <- data.frame(kinases = kinases, again = kinases)
        
        # convert IDs
        selectedkinasesforstroke <- convertID(tempdf, df, inputtype = input$NodeStrokeSelectIDtype)
        
        if (nrow(selectedkinasesforstroke) > 0) {
          # set colors based on selected ids
          tempdf$node.strokecol <- color.by.selected(df = tempdf, sel = selectedkinasesforstroke[, 1], bg.col = input$NodeStrokeSelect_BG, sel.col = input$NodeStrokeSelect_FG)
        }
      }
    }
    
    return(list(tempdf, legend))
  }) # end reactive
  
  # ----------------- PLOTS ---------------- #
  
  # build the manning tree
  output$plot1 <- renderSvgPanZoom({
    
    # recolor the official matrix
    dfandlegend <- newdf()
    svginfo$dataframe <- dfandlegend[[1]]
    svginfo$legend <- dfandlegend[[2]]
    
    # set title
    svginfo$title <- input$titleinput
    
    if (!dir.exists(dirname(svgoutfile))) {
      dir.create(dirname(svgoutfile), showWarnings = F)
    }
    
    # Write SVG file
    writekinasetree(svginfo, destination = svgoutfile, font = input$fontfamilyselect, labelselect = input$kinaselabelselect, groupcolor = input$groupcolorchoose)
    
    # Render SVG
    svgPanZoom(svgoutfile, viewBox = F, controlIconsEnabled = F)
  })
  
  # output to the graph div
  output$circlelayout <- reactive({
    # recolor the official matrix
    dfandlegend <- newdf()
    svginfo$dataframe <- dfandlegend[[1]]
    svginfo$legend <- dfandlegend[[2]]
    
    # replace none color for D3 plots
    allnodescoloreddf <- svginfo$dataframe
    # allnodescoloreddf$node.col[which(allnodescoloreddf$node.col == "none")] = BG_col1
    
    # color nodes by single color
    if (input$nodecolortype == "None") {
      allnodescoloreddf$node.col <- input$col_node_single
    }
    
    # modify color subnodes based on coloring options
    if (input$nodestrokecolselect == "Single Color") {
      BGstrolecol <- input$nodestrokecol
    }
    if (input$nodestrokecolselect == "Selected") {
      BGstrolecol <- input$NodeStrokeSelect_BG
    }
    if (input$nodestrokecolselect == "Same as Node") {
      allnodescoloreddf$node.strokecol <- allnodescoloreddf$node.col
      BGstrolecol <- "#ffffff"
    }
    
    # Write kinome_tree.json (based on current dataframe)
    if (!dir.exists("www/json")) {
      dir.create("www/json")
    }
    makejson(allnodescoloreddf,
             tmp = subdffile, output = outputjson, BGcol = BG_col1, BGstrolecol = BGstrolecol, colsubnodes = input$colorsubnodes,
             labelselect = input$kinaselabelselect, defaultnoderadius = input$size_node_single, legend = svginfo$legend,
             xshift = 80, yshift = 60
    )
    
    # Make this reactive to any change in input paramters
    x <- reactiveValuesToList(input)
  })
  
  # output to the graph div
  output$forcelayout <- reactive({
    
    # recolor the official matrix
    dfandlegend <- newdf()
    svginfo$dataframe <- dfandlegend[[1]]
    svginfo$legend <- dfandlegend[[2]]
    
    # replace none color for D3 plots
    allnodescoloreddf <- svginfo$dataframe
    allnodescoloreddf$node.col[which(allnodescoloreddf$node.col == "none")] <- BG_col1
    
    # color nodes by single color
    if (input$nodecolortype == "None") {
      allnodescoloreddf$node.col <- input$col_node_single
    }
    
    # modify color subnodes based on coloring options
    if (input$nodestrokecolselect == "Single Color") {
      BGstrolecol <- input$nodestrokecol
    }
    if (input$nodestrokecolselect == "Selected") {
      BGstrolecol <- input$NodeStrokeSelect_BG
    }
    if (input$nodestrokecolselect == "Same as Node") {
      allnodescoloreddf$node.strokecol <- allnodescoloreddf$node.col
      BGstrolecol <- "#ffffff"
    }
    
    # Write kinome_tree.json (based on current dataframe)
    makejson(allnodescoloreddf,
             tmp = subdffile, output = outputjson, BGcol = BG_col1, BGstrolecol = BGstrolecol,
             colsubnodes = input$colorsubnodes, labelselect = input$kinaselabelselect,
             defaultnoderadius = input$size_node_single, legend = svginfo$legend,
             xshift = 85, yshift = 35, noderadiusexpansion = 1.5
    )
    
    # Make this reactive to any change in input paramters
    x <- reactiveValuesToList(input)
  })
  
  # ----------------- DATA TABLE ---------------- #
  
  # build the table
  output$KinaseTable <- DT::renderDataTable({
    dfandlegend <- newdf()
    simpldf <- dfandlegend[[1]][, c("id.coral", "id.longname", "kinase.family", "kinase.group", "branch.val", "branch.col", "node.col", "node.radius")]
    
    # reverse the data frame so that colored kinases appear first
    simpldf <- simpldf[dim(simpldf)[1]:1, ]
    
    # convert branch colors to rgb
    mycolors <- simpldf$branch.col
    rgbcolors <- apply(
      grDevices::col2rgb(mycolors), 2,
      function(rgb) sprintf("rgb(%s)", paste(rgb, collapse = ","))
    )
    tgtbranch <- sprintf('<span style="color:%s">&#9608;</span>', rgbcolors)
    
    newdf <- data.frame(
      kinase = simpldf$id.coral,
      name = simpldf$id.longname,
      family = simpldf$kinase.family,
      group = simpldf$kinase.group
    )
    
    # add node info if present
    if ("none" %in% simpldf$node.col == F) {
      # convert node colors to rgb
      mycolors <- simpldf$node.col
      
      rgbcolors <- apply(
        grDevices::col2rgb(mycolors), 2,
        function(rgb) sprintf("rgb(%s)", paste(rgb, collapse = ","))
      )
      tgtnode <- sprintf('<span style="color:%s">&#11044;</span>', rgbcolors)
      
      newdf$node_radius <- simpldf$node.radius
      newdf$node_color <- tgtnode
    }
    
    # add branch color
    newdf$branch_color <- tgtbranch
    
    datatable(newdf, escape = FALSE)
  })
  
  # ----------------- DOWNLOAD ---------------- #
  
  output$downloadtree <- downloadHandler(
    filename <- function(file) {
      if (input$downloadtype == "SVG") {
        paste("CORAL", ".", "tree", ".", "svg", sep = "")
      } else if (input$downloadtype == "PNG") {
        paste("CORAL", ".", "tree", ".", "png", sep = "")
      }
    },
    content <- function(file) {
      if (input$downloadtype == "SVG") {
        file.copy(svgoutfile, file)
      } else if (input$downloadtype == "PNG") {
        #pngDPI <- as.numeric(input$pngDPI)
        #img <- magick::image_read_svg(svgoutfile, width = 2000, height = 2000)
        #magick::image_write(img, file, format = "png", flatten = FALSE, density = pngDPI)
      }
    }
    
    # content = function(file) {
    # file.copy(svgoutfile, file)
    # content <- function(file) {
    #  rsvg_svg(svgoutfile, file)
    # }
    
    # content <- function(file) {
    #   if (input$downloadtype == 'pdf') {
    #     rsvg_pdf(svgoutfile, file)
    #   } else if (input$downloadtype == 'svg') {
    #     rsvg_svg(svgoutfile, file)
    #   } else {
    #     showNotification('Unrecognized Output Image Type')
    #   }
    # }
  )
  
  # ----------------- DELETE TEMP FILES WHEN SESSION ENDS ---------------- #
  
  session$onSessionEnded(function() {
    if (file.exists(outputjson)) {
      file.remove(outputjson)
    }
    if (file.exists(subdffile)) {
      file.remove(subdffile)
    }
    if (file.exists(svgoutfile)) {
      file.remove(svgoutfile)
    }
  })
  
  # ----------------- ADD FORCE NETWORK DISCLAIMER ---------------- #
  
  # ----------------- ADD CIRCLE NETWORK DISCLAIMER ---------------- #
  
  insertUI(
    selector = "#treedisclaimer", where = "afterEnd",
    ui = box(
      width = 12,
      "The kinome tree plots generated by CORAL make use the data generated by",
      tags$a("Manning et al., Science, 2002", href = "http://science.sciencemag.org/content/298/5600/1912.long", target = "_blank"),
      " and are based on a figure generated by ",
      tags$a("Cell Signaling Technology", href = "https://www.cellsignal.com/contents/science/protein-kinases-introduction/kinases", target = "_blank"),
      "."
    )
  ) # end tree disclaimer
  
  insertUI(
    selector = "#circledisclaimer", where = "afterEnd",
    ui = box(
      width = 12,
      "The circle network plots generated by CORAL make use the ",
      tags$a("D3.js", href = "https://d3js.org", target = "_blank"),
      "javascript library. The code to generate the visualization above was",
      "modified from ",
      tags$a("code", href = "https://bl.ocks.org/mbostock/4339607", target = "_blank"),
      "written by Mike Bostock."
    )
  ) # end circle disclaimer
  
  
  insertUI(
    selector = "#forcedisclaimer", where = "afterEnd",
    ui = box(
      width = 12,
      "The force network plots generated by CORAL make use the ",
      tags$a("D3.js", href = "https://d3js.org", target = "_blank"),
      "javascript library. The code to generate the visualization above was",
      "modified from ",
      tags$a("code", href = "https://bl.ocks.org/mbostock/4062045", target = "_blank"),
      "written by Mike Bostock.",
      tags$br(),
      tags$br(),
      actionButton(inputId = "refreshForcePlot", label = "Refresh Plot")
    )
  ) # end force disclaimer
})

# Run the application
app <- runApp(shinyApp(ui, server))