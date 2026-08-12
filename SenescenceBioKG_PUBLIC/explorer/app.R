suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(visNetwork)
  library(DT)
  library(dplyr)
  library(readr)
  library(stringr)
  library(htmltools)
})

app_dir <- normalizePath(Sys.getenv("SBK_APP_DIR", unset = getwd()), winslash = "/", mustWork = TRUE)
root <- normalizePath(file.path(app_dir, ".."), winslash = "/", mustWork = TRUE)
source(file.path(app_dir, "R", "data_loader.R"), encoding = "UTF-8")
source(file.path(app_dir, "R", "query_engine.R"), encoding = "UTF-8")
source(file.path(app_dir, "R", "network_helpers.R"), encoding = "UTF-8")
sbk <- load_sbk_explorer_data(root)

short_cq_names <- c(
  CQ01 = "Response-target-regeneration", CQ02 = "Response-release",
  CQ03 = "Response-mechanism-regeneration", CQ04 = "Cargo-target-animal validation",
  CQ05 = "Cell-mechanism-regeneration", CQ06 = "Composition-fabrication-validation",
  CQ07 = "Release-mechanism", CQ08 = "Multi-cargo regeneration",
  CQ09 = "Multi-cell mechanism", CQ10 = "Mechanism-endpoint-animal validation",
  CQ11 = "Response-target missing endpoint", CQ12 = "Target-mechanism missing endpoint",
  CQ13 = "Cargo-endpoint missing mechanism", CQ14 = "Human-derived relation coverage",
  CQ15 = "High evidence-density materials"
)

choices_for <- function(relation) {
  sort(unique(sbk$edge_view$Target[sbk$edge_view$Relation == relation]))
}
all_choice <- function(x) c("All" = "", setNames(x, x))

theme <- bs_theme(
  version = 5, bg = "#FFFFFF", fg = "#222222", primary = "#2A9D8F",
  base_font = font_collection("Arial", "Helvetica", "sans-serif")
)

ui <- page_navbar(
  title = div(class = "brand-block",
              div(class = "brand-title", "SenescenceBioKG Explorer"),
              div(class = "brand-subtitle", "Full-text evidence-supported knowledge graph")),
  theme = theme,
  header = tags$head(tags$link(rel = "stylesheet", href = "custom.css")),

  nav_panel("Explore",
    div(class = "page-shell",
      div(class = "explore-grid",
        div(class = "filter-panel",
          div(class = "section-label", "Filters"),
          selectizeInput("material_filter", "Material", choices = sort(unique(sbk$material_table$Material)),
                         selected = sbk$default_material, options = list(maxOptions = 500)),
          selectInput("relation_filter", "Relation", choices = all_choice(sort(unique(sbk$edge_view$Relation)))),
          selectizeInput("stimulus_filter", "Stimulus", choices = all_choice(choices_for("RESPONDS_TO"))),
          selectizeInput("target_filter", "Target cell", choices = all_choice(choices_for("TARGETS"))),
          selectizeInput("mechanism_filter", "Mechanism", choices = all_choice(choices_for("MODULATES"))),
          selectizeInput("endpoint_filter", "Regenerative endpoint", choices = all_choice(choices_for("PROMOTES"))),
          selectizeInput("validation_filter", "Validation stage", choices = all_choice(choices_for("VALIDATED_AT"))),
          selectizeInput("record_filter", "Record_ID", choices = all_choice(sort(unique(sbk$edge_view$Record_ID)))),
          actionButton("clear_filters", "Reset", class = "btn-outline-secondary w-100")
        ),
        div(class = "network-panel",
          visNetworkOutput("kg_network", height = "650px"),
          uiOutput("network_note")
        ),
        div(class = "details-panel",
          div(class = "section-label", "Selected item details"),
          uiOutput("selected_details")
        )
      )
    )
  ),

  nav_panel("Materials",
    div(class = "page-shell", div(class = "table-panel",
      div(class = "section-label", "Material evidence matrix"),
      DTOutput("materials_table")
    ))
  ),

  nav_panel("Evidence",
    div(class = "page-shell", div(class = "table-panel",
      div(class = "section-label", "Evidence provenance"),
      DTOutput("evidence_table")
    ))
  ),

  nav_panel("Competency Queries",
    div(class = "page-shell", div(class = "cq-panel",
      div(class = "section-label", "Validated query benchmark"),
      fluidRow(
        column(8, selectInput("cq_select", "Competency query",
          choices = setNames(names(short_cq_names), paste(names(short_cq_names), short_cq_names, sep = " | ")))),
        column(4, br(), actionButton("run_cq", "Run query", class = "btn-primary w-100"))
      ),
      uiOutput("cq_summary"),
      DTOutput("cq_table")
    ))
  ),

  nav_panel("About",
    div(class = "page-shell", div(class = "about-panel",
      div(class = "section-label", "Frozen core"),
      div(class = "metric-row",
        div(class = "metric-chip", div(class = "metric-value", "283"), div(class = "metric-label", "Full-text studies")),
        div(class = "metric-chip", div(class = "metric-value", "654"), div(class = "metric-label", "Canonical nodes")),
        div(class = "metric-chip", div(class = "metric-value", "1,868"), div(class = "metric-label", "Supported edges"))
      ),
      p(class = "boundary-note", "Human-derived is not Clinical. Query benchmark metrics test implementation agreement against an independent reference computation; they are not measures of biomedical truth.")
    ))
  )
)

server <- function(input, output, session) {
  observeEvent(input$clear_filters, {
    updateSelectizeInput(session, "material_filter", selected = sbk$default_material)
    for (id in c("relation_filter", "stimulus_filter", "target_filter", "mechanism_filter",
                 "endpoint_filter", "validation_filter", "record_filter")) {
      updateSelectizeInput(session, id, selected = "")
    }
  })

  filtered_edges <- reactive({
    filter_sbk_edges(
      sbk, material = input$material_filter %||% "", relation = input$relation_filter %||% "",
      stimulus = input$stimulus_filter %||% "", target_cell = input$target_filter %||% "",
      mechanism = input$mechanism_filter %||% "", endpoint = input$endpoint_filter %||% "",
      validation = input$validation_filter %||% "", record_id = input$record_filter %||% ""
    )
  })

  network_payload <- reactive(build_vis_payload(sbk, filtered_edges()))

  output$kg_network <- renderVisNetwork({
    payload <- network_payload()
    validate(need(nrow(payload$edges) > 0, "No supported edges match these filters."))
    visNetwork(payload$nodes, payload$edges, width = "100%", height = "650px") %>%
      visNodes(font = list(face = "Arial", color = "#222222"), borderWidth = 1) %>%
      visEdges(smooth = list(enabled = TRUE, type = "dynamic"),
               font = list(face = "Arial", color = "#44515a", strokeWidth = 3, strokeColor = "#ffffff")) %>%
      visPhysics(solver = "forceAtlas2Based", stabilization = list(enabled = TRUE, iterations = 350)) %>%
      visInteraction(hover = TRUE, navigationButtons = TRUE, keyboard = TRUE) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
                 nodesIdSelection = FALSE) %>%
      visEvents(
        selectEdge = "function(p){ if(p.edges.length){ Shiny.setInputValue('selected_edge_id', p.edges[0], {priority:'event'}); } }",
        selectNode = "function(p){ if(p.nodes.length){ Shiny.setInputValue('selected_node_id', p.nodes[0], {priority:'event'}); } }"
      )
  })

  output$network_note <- renderUI({
    p <- network_payload()
    div(class = "network-note",
        if (p$truncated) "A capped overview is shown; refine filters for a complete one-hop view."
        else paste0(nrow(p$nodes), " nodes | ", nrow(p$edges), " supported edges"))
  })

  selected_edge <- reactive({
    e <- filtered_edges()
    if (nrow(e) == 0) return(NULL)
    edge_id <- input$selected_edge_id
    if (is.null(edge_id) || !edge_id %in% e$Edge_ID) edge_id <- e$Edge_ID[[1]]
    edge_detail_row(sbk, edge_id)
  })

  output$selected_details <- renderUI({
    e <- selected_edge()
    if (is.null(e)) return(div(class = "boundary-note", "Select a material or refine filters."))
    tagList(
      div(class = "detail-kv", div(class = "detail-k", "Material"), div(class = "detail-v", e$Material)),
      div(class = "detail-kv", div(class = "detail-k", "Relation"), div(class = "detail-v", e$Relation)),
      div(class = "detail-kv", div(class = "detail-k", "Target"), div(class = "detail-v", e$Target)),
      div(class = "detail-kv", div(class = "detail-k", "Record_ID"), div(class = "detail-v", e$Record_ID)),
      div(class = "detail-kv", div(class = "detail-k", "Evidence_ID"), div(class = "detail-v", e$Evidence_ID)),
      div(class = "evidence-box",
          div(class = "detail-k", paste(e$Evidence_source, "|", e$Evidence_location)),
          div(class = "detail-k", paste("DOI:", ifelse(is.na(e$DOI) || e$DOI == "", "Not available", e$DOI))),
          div(style = "margin-top:7px;", "Verbatim evidence excerpt is not redistributed in the public release. Please consult the cited source using the DOI and evidence location."))
    )
  })

  output$materials_table <- renderDT({
    datatable(sbk$material_table %>%
      select(Material, DesignMode, Stimulus, TargetCell, Mechanism,
             RegenerativeEndpoint, ValidationStage, Record_ID),
      filter = "top", rownames = FALSE,
      options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE))
  })

  output$evidence_table <- renderDT({
    ev <- sbk$edge_view %>%
      transmute(Record_ID, Material, Relation, Target, Evidence_ID,
                DOI, `Evidence source` = Evidence_source,
                `Evidence location` = Evidence_location,
                `Evidence strength` = Evidence_strength)
    datatable(ev, filter = "top", rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE))
  })

  active_cq <- eventReactive(input$run_cq, input$cq_select, ignoreInit = FALSE)
  cq_rows <- reactive(cq_result(sbk, active_cq()))

  output$cq_summary <- renderUI({
    id <- active_cq()
    meta <- sbk$cqs %>% filter(CQ_ID == id) %>% slice_head(n = 1)
    div(
      p(class = "cq-question", meta$Question),
      div(class = "metric-row",
        div(class = "metric-chip", div(class = "metric-value", nrow(cq_rows())), div(class = "metric-label", "Returned rows")),
        div(class = "metric-chip", div(class = "metric-value", n_distinct(cq_rows()$Record_ID)), div(class = "metric-label", "Studies"))
      )
    )
  })

  output$cq_table <- renderDT({
    datatable(cq_rows() %>% select(Material, Relation_path, Target_entities, Record_ID, Evidence_ID),
              rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE, autoWidth = TRUE))
  })
}

shinyApp(ui, server)
