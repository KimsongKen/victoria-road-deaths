# Interactive map of Victorian crash clusters (Shiny + leaflet).
# Run from the project folder with:  shiny::runApp("app")
# Requires the pipeline outputs (run_all.R) in ../output.

suppressPackageStartupMessages({
  library(shiny); library(leaflet); library(dplyr); library(DT)
})

out_dir <- normalizePath(file.path(if (dir.exists("output")) "." else "..", "output"), mustWork = TRUE)
addResourcePath("figs", file.path(out_dir, "figures"))

crash <- readRDS(file.path(out_dir, "crash_clustered.rds")) |>
  filter(!is.na(lat), !is.na(lon)) |>
  select(accident_no, date, year, time, day_of_week, cluster, severity, region, lga,
         speed_band, light, weather, surface, geometry, road_class, dca_code, dca_desc, lat, lon)
prof <- readRDS(file.path(out_dir, "profiles.rds"))
profiles <- prof$profiles
stories  <- prof$stories
dca_cmp  <- readRDS(file.path(out_dir, "dca_comparison.rds"))

k <- nlevels(crash$cluster)
cluster_labels <- setNames(levels(crash$cluster), sprintf("%s. %s", stories$cluster, stories$name))
Sys.setenv(CRASH_PROJECT_DIR = dirname(out_dir))
source(file.path(dirname(out_dir), "R", "00_config.R"))   # shared colours and basemap
pal_cols <- unname(situation_colours(k))
pal <- colorFactor(pal_cols, levels = levels(crash$cluster))
MAX_POINTS <- 20000
pct <- function(x, d = 0) paste0(formatC(100 * x, format = "f", digits = d), "%")

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .story-card { background:#f7fafc; border-left:6px solid var(--c); padding:12px 14px; border-radius:6px; }
    .story-card h4 { margin-top:0; } .story-card p { font-size:13px; line-height:1.45; }
    .stat { display:inline-block; margin-right:14px; font-size:13px; } .stat b { font-size:16px; }
  "))),
  titlePanel("Victorian road crashes 2012-2024: data-driven clusters vs official DCA categories"),
  sidebarLayout(
    sidebarPanel(width = 3,
      checkboxGroupInput("clusters", "Clusters", choices = cluster_labels, selected = cluster_labels),
      sliderInput("years", "Year", min(crash$year), max(crash$year),
                  value = range(crash$year), step = 1, sep = ""),
      checkboxGroupInput("severity", "Severity", choices = levels(crash$severity),
                         selected = c("Fatal", "Serious injury", "Other injury")),
      checkboxGroupInput("region", "Region", choices = levels(crash$region),
                         selected = levels(crash$region)),
      selectInput("story_cluster", "Story card for cluster", choices = cluster_labels),
      helpText("Tip: click any crash on the map to open its cluster's story card.")
    ),
    mainPanel(width = 9,
      tabsetPanel(
        tabPanel("Map",
          br(), textOutput("n_shown"),
          leafletOutput("map", height = 560),
          br(), uiOutput("story")),
        tabPanel("Cluster profiles", br(), DTOutput("profile_table"),
                 br(), img(src = "figs/severity_by_cluster.png", width = "100%"),
                 img(src = "figs/region_by_cluster.png", width = "70%")),
        tabPanel("Compare with DCA", br(),
                 img(src = "figs/dca_heatmap.png", width = "100%"),
                 h4("Agreement between clusters and official categories"), tableOutput("agree"),
                 h4("Does the cluster tell us more about severity than DCA alone?"), tableOutput("sev")),
        tabPanel("About", br(), tags$div(
          p("Clusters were built with Multiple Correspondence Analysis + k-means on 17 situational features",
            "(time, light, weather, surface, speed zone, road class and geometry, traffic control, vehicles",
            "involved and road-user mix). DCA code, accident type, severity and location were left out of the",
            "clustering so they can be compared against it."),
          p("Story cards are generated from templates in R. 'Typical crash' uses the most common traits of the",
            "cluster; 'Real example' is the actual crash whose traits are most typical of its cluster."),
          p("Map shows at most ", format(MAX_POINTS, big.mark = ","), " randomly chosen crashes from the",
            "current filter so the browser stays responsive; profile figures use every crash.")))
      )
    )
  )
)

server <- function(input, output, session) {
  filtered <- reactive({
    crash |> filter(cluster %in% input$clusters, year >= input$years[1], year <= input$years[2],
                    severity %in% input$severity, region %in% input$region)
  })
  shown <- reactive({
    d <- filtered()
    # Fatal crashes are always drawn; the rest are sampled down to the cap.
    if (nrow(d) > MAX_POINTS) {
      fatal <- d[d$severity == "Fatal", ]
      rest  <- d[d$severity != "Fatal", ]
      set.seed(1)
      d <- bind_rows(rest[sample(nrow(rest), max(MAX_POINTS - nrow(fatal), 0)), ], fatal)
    }
    d
  })
  output$n_shown <- renderText(sprintf("%s crashes match the filters; showing %s on the map.",
    format(nrow(filtered()), big.mark = ","), format(nrow(shown()), big.mark = ",")))

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addProviderTiles(BASEMAP) |>
      addTiles(urlTemplate = BASEMAP_LABELS, options = tileOptions(pane = "overlayPane")) |>
      fitBounds(141, -39.2, 150, -34) |>
      addLegend("bottomright", colors = pal_cols, labels = names(cluster_labels),
                title = "Cluster", opacity = 0.9)
  })
  observe({
    d <- shown()
    leafletProxy("map", data = d) |>
      clearMarkers() |>
      addCircleMarkers(lng = ~lon, lat = ~lat, layerId = ~accident_no,
                       radius = ~ifelse(severity == "Fatal", 5, 3), stroke = FALSE,
                       fillColor = ~pal(cluster), fillOpacity = ~ifelse(severity == "Fatal", 0.95, 0.55),
                       popup = ~sprintf("<b>Cluster %s</b><br>%s %s, %s<br>%s, %s<br>%s, %s, %s<br>DCA %s: %s<br><b>%s</b>",
                                        cluster, day_of_week, format(date, "%d %b %Y"), time, lga, region,
                                        speed_band, light, weather, dca_code, dca_desc, severity))
  })
  observeEvent(input$map_marker_click, {
    cl <- crash$cluster[match(input$map_marker_click$id, crash$accident_no)]
    if (!is.na(cl)) updateSelectInput(session, "story_cluster", selected = as.character(cl))
  })

  output$story <- renderUI({
    j <- input$story_cluster
    s <- stories[stories$cluster == j, ]; p <- profiles[profiles$cluster == j, ]
    div(class = "story-card", style = sprintf("--c:%s", pal_cols[as.integer(j)]),
      h4(sprintf("Cluster %s: %s", j, s$name)),
      div(span(class = "stat", HTML(sprintf("<b>%s</b> of crashes", pct(p$share, 1)))),
          span(class = "stat", HTML(sprintf("<b>%s</b> killed/seriously injured (x%.2f avg)", pct(p$ksi_rate), p$ksi_relative))),
          span(class = "stat", HTML(sprintf("<b>%s</b> of all deaths", pct(p$share_of_deaths)))),
          span(class = "stat", HTML(sprintf("<b>%s</b> metro", pct(p$metro))))),
      p(tags$b("Typical crash. "), s$typical_story),
      p(tags$b(sub(":.*", ". ", s$real_story)), sub("^[^:]*: ", "", s$real_story)),
      p(tags$b("Profile. "), s$description))
  })

  output$profile_table <- renderDT({
    profiles |>
      transmute(Cluster = cluster, Name = name, Crashes = n, Share = pct(share, 1),
                `KSI rate` = pct(ksi_rate), `Fatal rate` = pct(fatal_rate, 1),
                `KSI vs avg` = sprintf("x%.2f", ksi_relative), `Share of deaths` = pct(share_of_deaths),
                Where = where, `Most crashes in` = top_lgas, `Most over-represented in` = overrep_lgas,
                `Top DCA group` = sprintf("%s (%s)", top_dca_group, pct(top_dca_share)),
                `DCA groups for 80%` = dca_groups_for_80pct)
  }, options = list(pageLength = 15, scrollX = TRUE, dom = "t"), rownames = FALSE)

  output$agree <- renderTable(dca_cmp$agree, digits = 3)
  output$sev <- renderTable(dca_cmp$sev_models |>
    transmute(Outcome = ifelse(outcome == "ksi", "Killed or seriously injured", "Fatal"),
              Model = model, AIC = round(AIC), `McFadden R2` = mcfadden_r2, AUC,
              `LRT p (adding cluster)` = ifelse(is.na(lrt_p_cluster_added), "",
                                                format.pval(lrt_p_cluster_added, digits = 2))), digits = 3)
}

shinyApp(ui, server)
