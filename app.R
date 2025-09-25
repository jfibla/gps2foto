# app.R — GPS tagging con bslib (ES / CA / EN) — Solo descarga al ordenador
# ------------------------------------------------------------------------

library(shiny)
library(bslib)
library(leaflet)
library(exiftoolr)
library(dplyr)
library(DT)
library(mime)
library(leaflet.extras)

# ---------- Diccionario i18n ----------
i18n <- list(
  es = list(
    title      = "Asignar coordenadas a JPG con clic en mapa (OSM)",
    selectlang = "Idioma",
    photos     = "Selecciona fotos JPG",
    readmeta   = "Leer metadatos",
    viewer     = "Visor de fotos",
    choose     = "Elige foto:",
    prev       = "⟵ Anterior",
    `next`     = "Siguiente ⟶",
    dllast     = "Descargar la última copia",
    dlzip      = "Descargar todas (ZIP)",
    theme      = "Tema",
    darkmode   = "Modo oscuro",
    saveone    = "Asignar GPS a la foto seleccionada",
    saveall    = "Asignar GPS a TODAS las seleccionadas"
  ),
  ca = list(
    title      = "Assignar coordenades a JPG amb clic al mapa (OSM)",
    selectlang = "Idioma",
    photos     = "Selecciona fotos JPG",
    readmeta   = "Llegir metadades",
    viewer     = "Visor de fotos",
    choose     = "Tria foto:",
    prev       = "⟵ Anterior",
    `next`     = "Següent ⟶",
    dllast     = "Descarregar l'última còpia",
    dlzip      = "Descarregar totes (ZIP)",
    theme      = "Tema",
    darkmode   = "Mode fosc",
    saveone    = "Assignar GPS a la foto seleccionada",
    saveall    = "Assignar GPS a TOTES les seleccionades"
  ),
  en = list(
    title      = "Assign GPS coordinates to JPG by map click (OSM)",
    selectlang = "Language",
    photos     = "Select JPG photos",
    readmeta   = "Read metadata",
    viewer     = "Photo viewer",
    choose     = "Choose photo:",
    prev       = "⟵ Previous",
    `next`     = "Next ⟶",
    dllast     = "Download last copy",
    dlzip      = "Download all (ZIP)",
    theme      = "Theme",
    darkmode   = "Dark mode",
    saveone    = "Assign GPS to selected photo",
    saveall    = "Assign GPS to ALL selected photos"
  )
)

# ---------- Helpers ----------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
tr <- function(key, lang) i18n[[lang]][[key]] %||% key
ensure_gps_cols <- function(df) {
  if (!"GPSLatitude"  %in% names(df)) df$GPSLatitude  <- NA_real_
  if (!"GPSLongitude" %in% names(df)) df$GPSLongitude <- NA_real_
  df$GPSLatitude  <- suppressWarnings(as.numeric(df$GPSLatitude))
  df$GPSLongitude <- suppressWarnings(as.numeric(df$GPSLongitude))
  df
}

# ---------- UI con bslib ----------
ui <- page_navbar(
  title = "gps2foto",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#0d6efd"),
  nav_spacer(),
  nav_item(
    div(class = "d-flex gap-3 align-items-center px-3",
        selectInput("lang", label = NULL,
                    choices = c("Español" = "es", "Català" = "ca", "English" = "en"),
                    selected = "ca", width = "160px"),
        checkboxInput("dark", label = NULL, value = FALSE),
        tags$span(id = "darklabel", "Dark")
    )
  ),
  nav_panel(
    "App",
    layout_sidebar(
      sidebar = sidebar(
        width = 360,
        
        # 1) Subir fotos
        uiOutput("photos_ui"),
        
        # 2) Visor de fotos
        h5(textOutput("viewer_txt")),
        uiOutput("picker_row"),
        div(class = "d-flex gap-2 mb-2",
            actionButton("prev_photo", label = "…"),
            actionButton("next_photo", label = "…")),
        imageOutput("photo_view", height = "auto"),
        hr(),
        
        # 3) Leer metadatos
        actionButton("read_meta", label = "…"),
        hr(),
        
        # 4) Asignación de coordenadas (clic en mapa)
        verbatimTextOutput("click_info"),
        hr(),
        
        # 5) Guardar GPS
        actionButton("write_one", label = "…"),
        actionButton("write_sel", label = "…"),
        hr(),
        
        # 6) Descarga al ordenador
        uiOutput("downloads_ui")
      ),
      card(
        h4(textOutput("title_txt")),
        card_header("Mapa & Tabla"),
        fillable = TRUE,
        leafletOutput("map", height = 520),
        div(class = "mt-3"),
        DTOutput("tbl")
      )
    )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 500*1024^2)
  
  output$photos_ui <- renderUI({
    lang <- input$lang %||% "es"
    fileInput("photos", tr("photos", lang),
              multiple = TRUE, accept = c(".jpg", ".jpeg"))
  })
  
  output$downloads_ui <- renderUI({
    lang <- input$lang %||% "es"
    div(class = "d-flex gap-2",
        downloadButton("dl_last", tr("dllast", lang)),
        downloadButton("dl_zip",  tr("dlzip",  lang))
    )
  })
  
  
  # Tema dinámico
  observe({
    session$setCurrentTheme(
      if (isTRUE(input$dark))
        bs_theme(version = 5, bootswatch = "darkly", primary = "#66b2ff")
      else
        bs_theme(version = 5, bootswatch = "flatly", primary = "#0d6efd")
    )
    lang <- input$lang %||% "es"
    updateCheckboxInput(session, "dark", label = tr("darkmode", lang))
  })
  
  rv <- reactiveValues(df = NULL, last_click = NULL)
  active_idx <- reactiveVal(NA_integer_)
  recent_dests <- reactiveVal(character(0))
  
  # Etiquetas dinámicas
  observe({
    lang <- input$lang %||% "es"
    output$title_txt  <- renderText(tr("title",  lang))
    output$viewer_txt <- renderText(tr("viewer", lang))
    updateActionButton(session, "read_meta", label = tr("readmeta", lang))
    updateActionButton(session, "prev_photo",  label = tr("prev",  lang))
    updateActionButton(session, "next_photo",  label = tr("next",  lang))
    updateActionButton(session, "write_one",   label = tr("saveone", lang))
    updateActionButton(session, "write_sel",   label = tr("saveall", lang))
  })
  
  # Mapa
  output$map <- renderLeaflet({
    m <- leaflet() |>
      addProviderTiles("OpenStreetMap", group = "OSM") |>
      addProviderTiles("CartoDB.Positron", group = "CartoDB Light") |>
      addProviderTiles("Esri.WorldImagery", group = "Satélite ESRI") |>
      addLayersControl(
        baseGroups = c("OSM", "CartoDB Light", "Satélite ESRI"),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      setView(lng = 2.17, lat = 41.38, zoom = 5)
    if ("addSearchOSM" %in% getNamespaceExports("leaflet.extras")) {
      m <- leaflet.extras::addSearchOSM(m)
    }
    m
  })
  
  # Carga fotos
  observeEvent(input$photos, {
    req(input$photos)
    rv$df <- tibble::tibble(
      file = input$photos$datapath,
      name = input$photos$name
    ) |> ensure_gps_cols()
    active_idx(if (nrow(rv$df)) 1 else NA_integer_)
    output$tbl <- renderDT(rv$df |> select(name, GPSLatitude, GPSLongitude),
                           selection = "multiple", options = list(pageLength = 5))
  })
  
  # Tabla
  output$tbl <- renderDT({
    req(rv$df)
    rv$df |> select(name, GPSLatitude, GPSLongitude)
  }, selection = "multiple", options = list(pageLength = 5))
  
  # Selector fotos
  output$picker_row <- renderUI({
    req(rv$df)
    lang <- input$lang %||% "es"
    selectInput("photo_choice", tr("choose", lang),
                choices = setNames(seq_len(nrow(rv$df)), rv$df$name),
                selected = active_idx(), width = "100%")
  })
  observeEvent(input$photo_choice, { if (!is.null(input$photo_choice)) active_idx(as.integer(input$photo_choice)) }, ignoreInit = TRUE)
  observeEvent(input$tbl_rows_selected, { if (length(input$tbl_rows_selected)) active_idx(input$tbl_rows_selected[1]) }, ignoreInit = TRUE)
  observeEvent(input$prev_photo, { req(rv$df); n <- nrow(rv$df); i <- active_idx(); if (is.na(i)) i <- 1; active_idx(if (i <= 1) n else i - 1) })
  observeEvent(input$next_photo, { req(rv$df); n <- nrow(rv$df); i <- active_idx(); if (is.na(i)) i <- 1; active_idx(if (i >= n) 1 else i + 1) })
  
  output$photo_view <- renderImage({
    req(rv$df); i <- active_idx(); req(!is.na(i), i >= 1, i <= nrow(rv$df))
    f <- rv$df$file[i]
    list(src = f, contentType = tryCatch(mime::guess_type(f), error = function(e) "image/jpeg"),
         alt = rv$df$name[i], width = "100%")
  }, deleteFile = FALSE)
  
  # Leer metadatos + centrar mapa si hay GPS
  observeEvent(input$read_meta, {
    req(rv$df)
    meta <- try(exiftoolr::exif_read(rv$df$file, tags = c("GPSLatitude","GPSLongitude")), silent = TRUE)
    if (!(inherits(meta, "try-error")) && nrow(meta)) {
      if (!"GPSLatitude" %in% names(meta))  meta$GPSLatitude  <- NA
      if (!"GPSLongitude" %in% names(meta)) meta$GPSLongitude <- NA
      meta2 <- meta %>% transmute(
        file = SourceFile,
        GPSLatitude  = suppressWarnings(as.numeric(GPSLatitude)),
        GPSLongitude = suppressWarnings(as.numeric(GPSLongitude))
      )
      rv$df <- rv$df %>% select(-any_of(c("GPSLatitude","GPSLongitude"))) %>%
        left_join(meta2, by = "file") %>% ensure_gps_cols()
    } else {
      rv$df <- ensure_gps_cols(rv$df)
      showNotification("No se pudieron leer metadatos EXIF.", type = "warning")
    }
    output$tbl <- renderDT(rv$df %>% select(name, GPSLatitude, GPSLongitude),
                           selection = "multiple", options = list(pageLength = 5))
    
    leafletProxy("map") %>% clearMarkers()
    has_gps <- rv$df %>% filter(!is.na(GPSLatitude), !is.na(GPSLongitude))
    if (nrow(has_gps)) {
      leafletProxy("map") %>%
        addMarkers(data = has_gps, lng = ~GPSLongitude, lat = ~GPSLatitude, popup = ~name, group = "gps") %>%
        flyToBounds(lng1 = min(has_gps$GPSLongitude), lat1 = min(has_gps$GPSLatitude),
                    lng2 = max(has_gps$GPSLongitude), lat2 = max(has_gps$GPSLatitude))
    }
  })
  
  # Clic en mapa
  observeEvent(input$map_click, {
    rv$last_click <- c(lat = input$map_click$lat, lon = input$map_click$lng)
    output$click_info <- renderText(sprintf("Último clic: lat=%.6f, lon=%.6f", rv$last_click["lat"], rv$last_click["lon"]))
    leafletProxy("map") %>% clearGroup("last") %>% addMarkers(lng = rv$last_click["lon"], lat = rv$last_click["lat"], popup = "Nuevo GPS", group = "last")
  })
  
  # Escribir GPS (siempre a carpeta temporal y para descarga)
  write_gps <- function(files, lat, lon) {
    stopifnot(length(files) >= 1)
    
    outdir <- file.path(tempdir(), "gps2foto")
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    
    lat_ref <- if (lat >= 0) "N" else "S"
    lon_ref <- if (lon >= 0) "E" else "W"
    args_base <- c(
      sprintf("-EXIF:GPSLatitude=%.8f", abs(lat)),
      paste0("-EXIF:GPSLatitudeRef=", lat_ref),
      sprintf("-EXIF:GPSLongitude=%.8f", abs(lon)),
      paste0("-EXIF:GPSLongitudeRef=", lon_ref),
      "-P"
    )
    
    unique_dest <- function(path) {
      if (!file.exists(path)) return(path)
      # si ya existe, crea un sufijo incremental
      base <- tools::file_path_sans_ext(basename(path))
      ext  <- tools::file_ext(path); dir <- dirname(path); k <- 1
      repeat {
        cand <- file.path(dir, sprintf("%s-%d.%s", base, k, ext))
        if (!file.exists(cand)) return(cand)
        k <- k + 1
      }
    }
    
    dests <- character(length(files))
    for (i in seq_along(files)) {
      f <- files[i]
      if (!file.exists(f)) stop(sprintf("Archivo no existe: %s", f))
      
      orig_name <- rv$df$name[rv$df$file == f]
      base <- tools::file_path_sans_ext(orig_name)
      ext  <- tools::file_ext(orig_name)
      
      dest <- unique_dest(file.path(outdir, sprintf("%s_gps.%s", base, ext)))
      rc <- try(exiftoolr::exif_call(c(args_base, "-o", dest, f)), silent = TRUE)
      if (inherits(rc, "try-error") || !file.exists(dest)) {
        stop(sprintf("ExifTool falló al crear la copia:\n origen: %s\n destino: %s", f, dest))
      }
      dests[i] <- dest
    }
    dests
  }
  
  # Guardar una
  observeEvent(input$write_one, {
    req(rv$df, rv$last_click)
    idx   <- if (length(input$tbl_rows_selected)) input$tbl_rows_selected[1] else 1
    files <- rv$df$file[idx]
    
    dests <- write_gps(files, rv$last_click["lat"], rv$last_click["lon"])
    recent_dests(unique(c(recent_dests(), dests)))
    showNotification(paste0("Copia guardada en: ", dests[1]), type = "message", duration = 6)
    
    meta <- exiftoolr::exif_read(dests, tags = c("GPSLatitude","GPSLongitude"))
    if (!inherits(meta, "try-error") && nrow(meta)) {
      meta2 <- meta %>%
        transmute(file = SourceFile, name = basename(SourceFile),
                  GPSLatitude = suppressWarnings(as.numeric(GPSLatitude)),
                  GPSLongitude = suppressWarnings(as.numeric(GPSLongitude)))
      rv$df[idx, ] <- meta2
      rv$df <- ensure_gps_cols(rv$df)
      output$tbl <- renderDT(rv$df %>% select(name, GPSLatitude, GPSLongitude),
                             selection = "multiple", options = list(pageLength = 5))
      leafletProxy("map") %>% addMarkers(
        data = meta2, lng = ~GPSLongitude, lat = ~GPSLatitude, popup = ~name, group = "gps_new"
      ) %>% flyTo(lng = meta2$GPSLongitude[1], lat = meta2$GPSLatitude[1], zoom = 12)
    }
  })
  
  # Guardar varias
  observeEvent(input$write_sel, {
    req(rv$df, rv$last_click)
    idx   <- if (length(input$tbl_rows_selected)) input$tbl_rows_selected else seq_len(nrow(rv$df))
    files <- rv$df$file[idx]
    
    dests <- write_gps(files, rv$last_click["lat"], rv$last_click["lon"])
    recent_dests(unique(c(recent_dests(), dests)))
    showNotification(paste("Copias guardadas:", paste(basename(dests), collapse = ", ")),
                     type = "message", duration = 6)
    
    meta <- exiftoolr::exif_read(dests, tags = c("GPSLatitude","GPSLongitude"))
    if (!inherits(meta, "try-error") && nrow(meta)) {
      meta2 <- meta %>%
        transmute(file = SourceFile, name = basename(SourceFile),
                  GPSLatitude = suppressWarnings(as.numeric(GPSLatitude)),
                  GPSLongitude = suppressWarnings(as.numeric(GPSLongitude)))
      rv$df[idx, ] <- meta2
      rv$df <- ensure_gps_cols(rv$df)
      output$tbl <- renderDT(rv$df %>% select(name, GPSLatitude, GPSLongitude),
                             selection = "multiple", options = list(pageLength = 5))
      leafletProxy("map") %>% addMarkers(
        data = meta2, lng = ~GPSLongitude, lat = ~GPSLatitude, popup = ~name, group = "gps_new"
      ) %>% flyToBounds(lng1 = min(meta2$GPSLongitude), lat1 = min(meta2$GPSLatitude),
                        lng2 = max(meta2$GPSLongitude), lat2 = max(meta2$GPSLatitude))
    }
  })
  
  # Descargas
  output$dl_last <- downloadHandler(
    filename = function() {
      dests <- recent_dests(); if (!length(dests)) return("sin_archivo.txt")
      basename(dests[length(dests)])
    },
    content = function(file) {
      dests <- recent_dests(); req(length(dests) > 0)
      file.copy(dests[length(dests)], file, overwrite = TRUE)
    }
  )
  output$dl_zip <- downloadHandler(
    filename = function() paste0("gps2foto_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
    content  = function(file) {
      dests <- unique(recent_dests())
      validate(need(length(dests) > 0, "No hay copias para descargar."))
      old <- setwd(dirname(dests[1])); on.exit(setwd(old), add = TRUE)
      utils::zip(zipfile = file, files = basename(dests), flags = "-j")
    }
  )
}

shinyApp(ui, server)
