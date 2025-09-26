# app.R — GPS tagging con bslib (ES / CA / EN) — Solo descarga al ordenador
# ------------------------ ~/Library/CloudStorage/Dropbox/GPS_to_FOTO/app.R ----------------------------

library(shiny)
library(bslib)
library(leaflet)
library(exiftoolr)
library(dplyr)
library(DT)
library(mime)
library(leaflet.extras)

# --- Asegura que ExifTool está disponible (shinyapps.io y local) ---
ensure_exiftool <- function() {
  # Intenta usar lo ya configurado
  try(exiftoolr::configure_exiftoolr(quiet = TRUE), silent = TRUE)
  v <- try(exiftoolr::exif_call("-ver"), silent = TRUE)
  if (!inherits(v, "try-error")) return(invisible(TRUE))
  
  # Si falla, instala y reconfigura
  try(exiftoolr::install_exiftool(), silent = TRUE)
  try(exiftoolr::configure_exiftoolr(quiet = TRUE), silent = TRUE)
  v2 <- try(exiftoolr::exif_call("-ver"), silent = TRUE)
  
  invisible(!inherits(v2, "try-error"))
}

# ---------- Diccionario i18n ----------
i18n <- list(
  es = list(
    title      = "Asignar coordenadas a JPG con clic en mapa (OSM)",
    selectlang = "Idioma",
    photos_txt = "Selecciona fotos JPG",
    readmeta   = "Leer metadatos",
    viewer     = "Visor de fotos",
    choose     = "Elige foto:",
    prev       = "⟵",
    `next`     = "⟶",
    dllast     = "Descargar la última copia",
    dlzip      = "Descargar todas (ZIP)",
    theme      = "Tema",
    darkmode   = "Modo oscuro",
    saveone    = "Asignar GPS a la foto seleccionada",
    saveall    = "Asignar GPS a TODAS las seleccionadas",
    banner_info = "<ol style='margin-left:1.2rem;'>
      <b>Como proceder:</b>
      <li>Carga la(s) foto(s).</li>
      <li>Visualízalas en el visor (opción de zoom).</li>
      <li>Lee los metadatos.</li>
      <li>Ve a la tabla y selecciona la foto.</li>
      <li>Ve al mapa, busca la localización y haz clic.</li>
      <li>Asigna las coordenadas a la foto.</li>
      <li>Guarda la última foto o varias en un zip.</li>
    </ol>",    
    banner_max  = "💾 Tamaño recomendado máximo: %d MB por foto o el sumatorio de todas las fotos cargadas.",
    banner_warn = "Si la foto es más grande, puede no cargarse correctamente en el servidor.",
    banner_tip  = "Consejo: reduce la resolución o súbelas en lotes más pequeños.",
    ignored_big = "Se han ignorado por tamaño (> %d MB): %s",
    no_valid    = "No se ha cargado ningún archivo válido.",
    exif_read_fail = "No se pudieron leer metadatos EXIF.",
    last_click_lbl = "at=%.6f, lon=%.6f",
    new_gps_popup  = "Nuevo GPS",
    copy_saved_in  = "Copia guardada en: %s",
    copies_saved   = "Copias guardadas: %s",
    file_missing   = "El archivo no existe: %s",
    exif_fail_copy = "ExifTool falló al crear la copia:\n origen: %s\n destino: %s",
    zip_empty      = "No hay copias para descargar.",
    card_title     = "Mapa & Tabla",
    enlarge = "Ampliar",
    zoom    = "Zoom (%)",
    close   = "Cerrar",
    gps = "Coordenadas asignadas",
    info = "Info",
    github_title = "💻 Código en GitHub",
    github_msg   = "Puedes descargar el código desde aquí:",
    github_btn   = "Cerrar"
  ),
  ca = list(
    title      = "Assignar coordenades a JPG amb clic al mapa (OSM)",
    selectlang = "Idioma",
    photos_txt = "Selecciona fotos JPG",
    readmeta   = "Llegir metadades",
    viewer     = "Visor de fotos",
    choose     = "Tria foto:",
    prev       = "⟵",
    `next`     = "⟶",
    dllast     = "Descarregar l'última còpia",
    dlzip      = "Descarregar totes (ZIP)",
    theme      = "Tema",
    darkmode   = "Mode fosc",
    saveone    = "Assignar GPS a la foto seleccionada",
    saveall    = "Assignar GPS a TOTES les seleccionades",
    banner_info = "<ol style='margin-left:1.2rem;'>
      <b>Com procedir:</b>
      <li>Carrega la(es) foto(s).</li>
      <li>Visualitza-les al visor (opció de zoom).</li>
      <li>Llegeix les metadades.</li>
      <li>Vés a la taula i selecciona la foto.</li>
      <li>Vés al mapa, cerca la localització i fes clic.</li>
      <li>Assigna les coordenades a la foto.</li>
      <li>Guarda la darrera foto o varies en un zip.</li>
    </ol>",    
    banner_max  = "💾 Mida màxima recomanada: %d MB per foto o el sumatori de totes les fotos carregades.",
    banner_warn = "Si la foto és més gran, pot no carregar-se correctament al servidor.",
    banner_tip  = "Consell: redueix la resolució o puja-les en lots més petits.",
    ignored_big = "S'han ignorat per mida (> %d MB): %s",
    no_valid    = "No s'ha carregat cap fitxer vàlid.",
    exif_read_fail = "No s'han pogut llegir metadades EXIF.",
    last_click_lbl = "lat=%.6f, lon=%.6f",
    new_gps_popup  = "Nou GPS",
    copy_saved_in  = "Còpia desada a: %s",
    copies_saved   = "Còpies desades: %s",
    file_missing   = "L'arxiu no existeix: %s",
    exif_fail_copy = "ExifTool ha fallat en crear la còpia:\n origen: %s\n destí: %s",
    zip_empty      = "No hi ha còpies per descarregar.",
    card_title     = "Mapa i Taula",
    enlarge = "Ampliar",
    zoom    = "Zoom (%)",
    close   = "Tancar",
    gps = "Coordenades assignades",
    info = "Info",
    github_title = "💻 Codi al GitHub",
    github_msg   = "Pots descarregar el codi des d’aquí:",
    github_btn   = "Tancar"
  ),
  en = list(
    title      = "Assign GPS coordinates to JPG by map click (OSM)",
    selectlang = "Language",
    photos_txt = "Select JPG photos",
    readmeta   = "Read metadata",
    viewer     = "Photo viewer",
    choose     = "Choose photo:",
    prev       = "⟵",
    `next`     = "⟶",
    dllast     = "Download last copy",
    dlzip      = "Download all (ZIP)",
    theme      = "Theme",
    darkmode   = "Dark mode",
    saveone    = "Assign GPS to selected photo",
    saveall    = "Assign GPS to ALL selected photos",
    banner_info = "<ol style='margin-left:1.2rem;'>
      <b>How to run:</b>
      <li>Upload the photo(s).</li>
      <li>Preview them in the viewer (zoom available).</li>
      <li>Read the metadata.</li>
      <li>Go to the table and select the photo.</li>
      <li>Go to the map, find the location and click.</li>
      <li>Assign the coordinates to the photo.</li>
      <li>Save the last photo or several in a zip.</li>
    </ol>",    
    banner_max  = "💾 Recommended max size: %d MB per photo or summatory of loaded photos.",
    banner_warn = "If the photo is larger, it may fail to upload on the server.",
    banner_tip  = "Tip: reduce the resolution or upload in smaller batches.",
    ignored_big = "Ignored due to size (> %d MB): %s",
    no_valid    = "No valid file was uploaded.",
    exif_read_fail = "Could not read EXIF metadata.",
    last_click_lbl = "lat=%.6f, lon=%.6f",
    new_gps_popup  = "New GPS",
    copy_saved_in  = "Copy saved in: %s",
    copies_saved   = "Copies saved: %s",
    file_missing   = "File does not exist: %s",
    exif_fail_copy = "ExifTool failed to create the copy:\n origin: %s\n dest: %s",
    zip_empty      = "There are no copies to download.",
    card_title     = "Map & Table",
    enlarge = "Enlarge",
    zoom    = "Zoom (%)",
    close   = "Close",
    gps = "Assigned coordinates",
    info = "Info",
    github_title = "💻 GitHub Code",
    github_msg   = "You can download the code from here:",
    github_btn   = "Close"
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

MAX_MB_PER_FILE <- 200  # límite recomendado por archivo


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
        tags$span(id = "darklabel", ".")
    )
  ),
  nav_panel(
    ".",
    layout_sidebar(
      sidebar = sidebar(
        width = 360,
        
        # Infos en la mateixa línia
        div(class = "d-flex align-items-center gap-3 mb-2",
            actionLink("show_info",   label = NULL, icon = icon("info-circle")),
            textOutput("info_btn_txt"),
            actionLink("show_github", label = NULL, icon = icon("github")),
            textOutput("gh_btn_txt")
        ),
        hr(),
        
        # 1) Subir fotos
        uiOutput("photos_ui"),
        hr(),
        
        # 2) Visor de fotos
        h5(textOutput("viewer_txt")),
        uiOutput("picker_row"),
        div(class = "d-flex gap-2 mb-2",
            actionButton("prev_photo", label = "…",
                         class = "btn-sm",
                         style = "padding:2px 8px; font-size:0.85em;"),
            actionButton("next_photo", label = "…",
                         class = "btn-sm",
                         style = "padding:2px 8px; font-size:0.85em;"),
            actionButton("open_modal", label = "…", class = "btn-sm")
        ),
        imageOutput("photo_view", height = "auto"),
        hr(),
        
        # 3) Leer metadatos
        actionButton("read_meta", label = "…"),
        hr(),
        
        # 4) Asignación de coordenadas (clic en mapa)
        h6(textOutput("gps_txt")),
        verbatimTextOutput("click_info"),
        hr(),
        
        # 5) Guardar GPS (actionButton)
        div(class = "d-flex gap-2",
            actionButton("write_one", "…", class = "btn-primary btn-sm"),
            actionButton("write_sel", "…", class = "btn-secondary btn-sm")
        ),
        hr(),
        
        # 6) Descargas
        uiOutput("downloads_ui")
      ),
      card(
        h4(textOutput("title_txt")),
        card_header(textOutput("card_title_txt")),
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
  
  ensure_exiftool()
  
  # Tags que llegirem quan reescrivim i rellegim EXIF
  tags_needed <- c(
    "Model","LensModel","DateTimeOriginal",
    "ImageWidth","ImageHeight","FileSize",
    "GPSLatitude","GPSLongitude"
  )
  
  # Asegurem que meta té totes les columnes; si falta alguna, la crea amb NA
  ensure_meta_cols <- function(meta) {
    for (nm in tags_needed) {
      if (!nm %in% names(meta)) meta[[nm]] <- NA
    }
    meta
  }
  
  # Texto al lado del icono de info (según idioma)
  output$info_btn_txt <- renderText({
    tr("info", input$lang %||% "es")
  })
  
  output$gh_btn_txt <- renderText({
    "Github"
  })
  
  # Modal con info de tamaños recomendados
  observeEvent(input$show_info, {
    lang <- input$lang %||% "es"
    
    showModal(
      modalDialog(
        title = "ℹ️ Info",
        easyClose = TRUE,
        size = "l",
        footer = modalButton(tr("close", lang)),
        tagList(
          div(
            class = "mb-3",
            HTML(sprintf("<p style='margin-bottom:0.6rem;'>%s</p>", tr("banner_info", lang)))
          ),
          tags$hr(),
          # --- INFO DE grandària ---
          div(
            class = "small",
            HTML(sprintf(
              "<p><strong>%s</strong></p>",
              sprintf(tr("banner_max", lang), MAX_MB_PER_FILE)
            )),
           HTML(sprintf("<p>%s</p>", tr("banner_warn", lang))),
           HTML(sprintf("<p>%s</p>", tr("banner_tip",  lang)))
          )
        )
      )
    )
  })
  
  
  # Modal con enlace a GitHub
  observeEvent(input$show_github, {
    lang <- input$lang %||% "es"
    showModal(modalDialog(
      title = tr("github_title", lang),
      easyClose = TRUE,
      size = "s",
      HTML(sprintf('<p>%s<br>
                  <a href="https://github.com/jfibla/gps2foto" target="_blank">
                  https://github.com/jfibla/gps2foto</a></p>',
                   tr("github_msg", lang))),
      footer = modalButton(tr("github_btn", lang))
    ))
  })
  
  
  output$photos_ui <- renderUI({
    lang <- input$lang %||% "es"
    fileInput("photos", tr("photos_txt", lang),
              multiple = TRUE, accept = c(".jpg", ".jpeg"))
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
    output$title_txt       <- renderText(tr("title",  lang))
    output$viewer_txt      <- renderText(tr("viewer", lang))
    output$gps_txt         <- renderText(tr("gps",    lang))
    output$card_title_txt  <- renderText(tr("card_title", lang))
    updateActionButton(session, "read_meta",  label = tr("readmeta", lang))
    updateActionButton(session, "prev_photo", label = tr("prev",     lang))
    updateActionButton(session, "next_photo", label = tr("next",     lang))
    updateActionButton(session, "open_modal", label = tr("enlarge",  lang))
    updateActionButton(session, "write_one",  label = tr("saveone",  lang))
    updateActionButton(session, "write_sel",  label = tr("saveall",  lang))
    updateActionButton(session, "dl_last",  label = tr("dllast",  lang))
    updateActionButton(session, "dl_zip",  label = tr("dlzip",  lang))
    # Descargas (no necesitan update*; ya están traducidas en UI? Las dejamos así:)
    # para traducirlas también:
    # removeUI/addUI sería excesivo; más simple, recrearlas vía renderUI si quieres.
  })
  
  # Modal de ampliación
  zoom_pct <- reactiveVal(120)
  observeEvent(input$open_modal, {
    lang <- input$lang %||% "es"
    showModal(modalDialog(
      title = tr("viewer", lang),
      easyClose = TRUE,
      size = "l",
      footer = tagList(modalButton(tr("close", lang))),
      div(
        sliderInput("zoom_modal", tr("zoom", lang), min = 50, max = 200,
                    value = zoom_pct(), step = 10, width = "100%"),
        div(style = "overflow:auto; text-align:center;",
            imageOutput("photo_modal", height = "auto"))
      )
    ))
  })
  observeEvent(input$zoom_modal, {
    if (!is.null(input$zoom_modal)) zoom_pct(input$zoom_modal)
  })
  output$photo_modal <- renderImage({
    req(rv$df); i <- active_idx(); req(!is.na(i), i >= 1, i <= nrow(rv$df))
    f <- rv$df$file[i]
    w <- as.integer(8 * (zoom_pct() %||% 120))
    list(src = f,
         contentType = tryCatch(mime::guess_type(f), error = function(e) "image/jpeg"),
         width = w,
         alt = rv$df$name[i])
  }, deleteFile = FALSE)
  
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
    lang <- input$lang %||% "es"
    df <- input$photos
    
    if (!is.null(df$size)) {
      overs <- which(df$size > MAX_MB_PER_FILE * 1024^2)
      if (length(overs)) {
        noms <- paste(df$name[overs], collapse = ", ")
        showNotification(
          sprintf(tr("ignored_big", lang), MAX_MB_PER_FILE, noms),
          type = "warning", duration = 8
        )
        df <- df[-overs, , drop = FALSE]
      }
    }
    
    validate(need(nrow(df) > 0, tr("no_valid", lang)))
    
    rv$df <- tibble::tibble(
      file = df$datapath,
      name = df$name
    ) |> ensure_gps_cols()
    
    active_idx(if (nrow(rv$df)) 1 else NA_integer_)
    output$tbl <- DT::renderDT(rv$df |> dplyr::select(name, GPSLatitude, GPSLongitude),
                               selection = "multiple", options = list(pageLength = 10))
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
  
  # Llegir metadades + centrar mapa si hi ha GPS
  observeEvent(input$read_meta, {
    req(rv$df)
    lang <- input$lang %||% "es"
    
    # llegim metadades extra
    tags_needed <- c("Model", "LensModel", "DateTimeOriginal",
                     "ImageWidth", "ImageHeight", "FileSize",
                     "GPSLatitude", "GPSLongitude")
    
    
    meta <- try(exiftoolr::exif_read(rv$df$file, tags = tags_needed), silent = TRUE)
    if (!(inherits(meta, "try-error")) && nrow(meta)) {
      meta <- ensure_meta_cols(meta)
      meta2 <- meta %>% dplyr::transmute(
        file = .data$SourceFile,
        Model, LensModel, DateTimeOriginal,
        ImageWidth, ImageHeight, FileSize,
        GPSLatitude  = suppressWarnings(as.numeric(.data$GPSLatitude)),
        GPSLongitude = suppressWarnings(as.numeric(.data$GPSLongitude))
      )
      rv$df <- rv$df %>%
        dplyr::select(-dplyr::any_of(c(
          "Model","LensModel","DateTimeOriginal",
          "ImageWidth","ImageHeight","FileSize",
          "GPSLatitude","GPSLongitude"
        ))) %>%
        dplyr::left_join(meta2, by = "file") %>%
        ensure_gps_cols()
    } else {
      rv$df <- ensure_gps_cols(rv$df)
      showNotification(tr("exif_read_fail", input$lang %||% "es"), type = "warning")
    }
       
    # refresquem taula → mostra tots els camps seleccionats
    output$tbl <- renderDT(
      rv$df %>% dplyr::select(
        name, Model, LensModel, DateTimeOriginal,
        ImageWidth, ImageHeight, FileSize,
        GPSLatitude, GPSLongitude
      ),
      selection = "multiple", options = list(pageLength = 5, scrollX = TRUE)
    )
    
    # actualitza marcadors del mapa si hi ha GPS
    leafletProxy("map") %>% clearMarkers()
    has_gps <- rv$df %>% filter(!is.na(GPSLatitude), !is.na(GPSLongitude))
    if (nrow(has_gps)) {
      leafletProxy("map") %>%
        addMarkers(data = has_gps, lng = ~GPSLongitude, lat = ~GPSLatitude,
                   popup = ~name, group = "gps") %>%
        flyToBounds(lng1 = min(has_gps$GPSLongitude), lat1 = min(has_gps$GPSLatitude),
                    lng2 = max(has_gps$GPSLongitude), lat2 = max(has_gps$GPSLatitude))
    }
  })
  
  
  # Clic en mapa
  observeEvent(input$map_click, {
    lang <- input$lang %||% "es"
    rv$last_click <- c(lat = input$map_click$lat, lon = input$map_click$lng)
    output$click_info <- renderText(sprintf(tr("last_click_lbl", lang),
                                            rv$last_click["lat"], rv$last_click["lon"]))
    leafletProxy("map") %>% clearGroup("last") %>%
      addMarkers(lng = rv$last_click["lon"], lat = rv$last_click["lat"],
                 popup = tr("new_gps_popup", lang), group = "last")
  })
  
  # Escribir GPS (siempre a tempdir para descarga)
  write_gps <- function(files, lat, lon) {
    stopifnot(length(files) >= 1)
    lang <- input$lang %||% "es"
    
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
      if (!file.exists(f)) stop(sprintf(tr("file_missing", lang), f))
      
      orig_name <- rv$df$name[rv$df$file == f]
      base <- tools::file_path_sans_ext(orig_name)
      ext  <- tools::file_ext(orig_name)
      
      dest <- unique_dest(file.path(outdir, sprintf("%s_gps.%s", base, ext)))
      rc <- try(exiftoolr::exif_call(c(args_base, "-o", dest, f)), silent = TRUE)
      if (inherits(rc, "try-error") || !file.exists(dest)) {
        stop(sprintf(tr("exif_fail_copy", lang), f, dest))
      }
      dests[i] <- dest
    }
    dests
  }
  
  observeEvent(input$write_one, {
    req(rv$df, rv$last_click)
    lang  <- input$lang %||% "es"
    idx   <- if (length(input$tbl_rows_selected)) input$tbl_rows_selected[1] else 1
    files <- rv$df$file[idx]
    
    dests <- write_gps(files, rv$last_click["lat"], rv$last_click["lon"])
    recent_dests(unique(c(recent_dests(), dests)))
    showNotification(sprintf(tr("copy_saved_in", lang), dests[1]),
                     type = "message", duration = 6)
    
    # Rellegim EXIF amb TOTS els tags que volem mostrar
    meta <- exiftoolr::exif_read(dests, tags = tags_needed)
    if (!inherits(meta, "try-error") && nrow(meta)) {
      meta <- ensure_meta_cols(meta)
      meta2 <- meta %>%
        dplyr::transmute(
          file = .data$SourceFile,
          name = basename(.data$SourceFile),
          Model, LensModel, DateTimeOriginal,
          ImageWidth, ImageHeight, FileSize,
          GPSLatitude  = suppressWarnings(as.numeric(.data$GPSLatitude)),
          GPSLongitude = suppressWarnings(as.numeric(.data$GPSLongitude))
        )
      
      # Actualitza NOMÉS columnes comunes (evita l’error de “recycle”)
      common <- intersect(names(rv$df), names(meta2))
      rv$df[idx, common] <- meta2[1, common, drop = FALSE]
      
      # Refresca taula i mapa
      output$tbl <- DT::renderDT(
        rv$df %>% dplyr::select(
          name, Model, LensModel, DateTimeOriginal,
          ImageWidth, ImageHeight, FileSize,
          GPSLatitude, GPSLongitude
        ),
        selection = "multiple", options = list(pageLength = 5, scrollX = TRUE)
      )
      
      leafletProxy("map") %>%
        addMarkers(
          data = meta2, lng = ~GPSLongitude, lat = ~GPSLatitude,
          popup = ~name, group = "gps_new"
        ) %>%
        flyTo(lng = meta2$GPSLongitude[1], lat = meta2$GPSLatitude[1], zoom = 12)
    }
  })
  
  observeEvent(input$write_sel, {
    req(rv$df, rv$last_click)
    lang  <- input$lang %||% "es"
    idx   <- if (length(input$tbl_rows_selected)) input$tbl_rows_selected else seq_len(nrow(rv$df))
    files <- rv$df$file[idx]
    
    dests <- write_gps(files, rv$last_click["lat"], rv$last_click["lon"])
    recent_dests(unique(c(recent_dests(), dests)))
    showNotification(
      sprintf(tr("copies_saved", lang), paste(basename(dests), collapse = ", ")),
      type = "message", duration = 6
    )
    
    meta <- exiftoolr::exif_read(dests, tags = tags_needed)
    if (!inherits(meta, "try-error") && nrow(meta)) {
      meta <- ensure_meta_cols(meta)
      meta2 <- meta %>%
        dplyr::transmute(
          file = .data$SourceFile,
          name = basename(.data$SourceFile),
          Model, LensModel, DateTimeOriginal,
          ImageWidth, ImageHeight, FileSize,
          GPSLatitude  = suppressWarnings(as.numeric(.data$GPSLatitude)),
          GPSLongitude = suppressWarnings(as.numeric(.data$GPSLongitude))
        )
      
      # igualem files: `idx` i `meta2` han de tenir el mateix ordre/longitud
      if (length(idx) != nrow(meta2)) {
        # per seguretat, fem match per nom de fitxer (basename)
        key_old <- basename(rv$df$file[idx])
        key_new <- meta2$name
        match_pos <- match(key_old, key_new)
        meta2 <- meta2[match_pos, , drop = FALSE]
      }
      
      if (length(idx) != nrow(meta2)) {
        key_old <- basename(rv$df$file[idx])
        key_new <- meta2$name
        match_pos <- match(key_old, key_new)
        meta2 <- meta2[match_pos, , drop = FALSE]
      }
      
      common <- intersect(names(rv$df), names(meta2))
      rv$df[idx, common] <- meta2[, common, drop = FALSE]
      
      output$tbl <- DT::renderDT(
        rv$df %>% dplyr::select(
          name, Model, LensModel, DateTimeOriginal,
          ImageWidth, ImageHeight, FileSize,
          GPSLatitude, GPSLongitude
        ),
        selection = "multiple", options = list(pageLength = 5, scrollX = TRUE)
      )
      
      leafletProxy("map") %>%
        addMarkers(
          data = meta2, lng = ~GPSLongitude, lat = ~GPSLatitude,
          popup = ~name, group = "gps_new"
        ) %>%
        flyToBounds(
          lng1 = min(meta2$GPSLongitude, na.rm = TRUE),
          lat1 = min(meta2$GPSLatitude,  na.rm = TRUE),
          lng2 = max(meta2$GPSLongitude, na.rm = TRUE),
          lat2 = max(meta2$GPSLatitude,  na.rm = TRUE)
        )
    }
  })
  
  

  # Descargas
  
  output$downloads_ui <- renderUI({
    lang <- input$lang %||% "es"
    div(class = "d-flex gap-2",
        downloadButton("dl_last", tr("dllast", lang)),
        downloadButton("dl_zip",  tr("dlzip",  lang))
    )
  })
  
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
