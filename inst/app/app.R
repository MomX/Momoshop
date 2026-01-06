library(shiny)
library(magick)
library(tidyverse)
library(bslib)
library(shinyjs)
library(base64enc)
library(shinyWidgets)

# ============================================================================
# MOMOSHOP - A Morphometric Image Processing Pipeline Builder
# ============================================================================
# This app provides an interactive interface for building, previewing, and
# exporting image processing pipelines using the magick package.
#
# The workflow is: Load images → Add operations → Preview results → Export code
#
# Key features:
# - Real-time preview with live parameter adjustment (debounced 500ms)
# - Responsive mosaic layout that adapts to window width
# - Separate and Composite view modes for detailed comparison
# - Reproducible R code generation for all pipelines
# - Full pipeline control (edit, reorder, enable/disable operations)
#
# ============================================================================

# ============================================================================
# OPERATION DEFINITIONS
# ============================================================================
# This function defines all available image processing operations that users
# can add to their pipeline. Each operation includes:
# - label: Human-readable name shown in the UI
# - category: Grouping for organization (Colorspace, Pixel Ops, Geometry, etc.)
# - tooltip: Helpful description shown on hover
# - params: Parameter definitions (sliders, selects, radio buttons, text inputs)
#
# This structure makes it easy to add new operations without modifying server logic.

get_operation_defs <- function() {
  list(
    # ---- COLORSPACE OPERATIONS ----
    # Work with color spaces and individual color channels
    channel = list(
      label = "Extract Channel",
      category = "1. Colorspace",
      tooltip = "Extract a single color channel from the image.",
      params = list(channel = list(type = "radio", label = "Channel", options = c("Alpha", "Black", "Blue", "Chroma", "Cyan", "Gray", "Green", "Hue", "Lightness", "Luminosity", "Magenta", "Opacity", "Red", "Saturation", "Yellow"), default = "Red"))
    ),

    colorspace = list(
      label = "Convert Colorspace",
      category = "1. Colorspace",
      tooltip = "Convert between RGB, HSL, HSV, Grayscale.",
      params = list(colorspace = list(type = "radio", label = "Colorspace", options = c("Gray", "RGB", "HSL", "HSV"), default = "RGB"))
    ),

    # ---- PIXEL OPERATIONS ----
    # Modify pixel values directly (brightness, contrast, color intensity, etc.)
    modulate = list(
      label = "Modulate (Brightness/Saturation/Hue)",
      category = "2. Pixel Operations",
      tooltip = "Adjust brightness, saturation, and hue. 100% = no change.",
      params = list(
        brightness = list(type = "slider", label = "Brightness (%)", min = 0, max = 200, step = 1, default = 100),
        saturation = list(type = "slider", label = "Saturation (%)", min = 0, max = 200, step = 1, default = 100),
        hue = list(type = "slider", label = "Hue Shift (%)", min = 0, max = 200, step = 1, default = 100)
      )
    ),

    negate = list(
      label = "Negate (Invert Colors)",
      category = "2. Pixel Operations",
      tooltip = "Invert all colors in the image.",
      params = list()
    ),

    enhance = list(
      label = "Enhance",
      category = "2. Pixel Operations",
      tooltip = "Enhance image quality by sharpening.",
      params = list()
    ),

    normalize = list(
      label = "Normalize",
      category = "2. Pixel Operations",
      tooltip = "Stretch histogram to full range.",
      params = list()
    ),

    invert = list(
      label = "Invert",
      category = "2. Pixel Operations",
      tooltip = "Invert pixel values (different from Negate). Useful for mask preparation.",
      params = list()
    ),

    # ---- GEOMETRY OPERATIONS ----
    # Transform the spatial layout and orientation of images
    rotate = list(
      label = "Rotate",
      category = "3. Geometry",
      tooltip = "Rotate by degrees. Positive = clockwise.",
      params = list(degrees = list(type = "slider", label = "Degrees", min = -180, max = 180, step = 1, default = 0))
    ),

    flip_h = list(
      label = "Flip Horizontal",
      category = "3. Geometry",
      tooltip = "Mirror left-to-right.",
      params = list()
    ),

    flip_v = list(
      label = "Flip Vertical",
      category = "3. Geometry",
      tooltip = "Flip upside down.",
      params = list()
    ),

    trim = list(
      label = "Trim (Auto-Crop)",
      category = "3. Geometry",
      tooltip = "Remove uniform borders.",
      params = list()
    ),

    # ---- SEGMENTATION & FILTERS ----
    # Detect edges, smooth, segment, or apply mathematical filters
    # Essential for morphometric analysis and outline extraction
    blur = list(
      label = "Blur (Gaussian)",
      category = "4. Segmentation & Filters",
      tooltip = "Gaussian blur for noise reduction.",
      params = list(
        radius = list(type = "slider", label = "Radius (pixels)", min = 0, max = 20, step = 0.5, default = 1),
        sigma = list(type = "slider", label = "Sigma (profile)", min = 0, max = 5, step = 0.5, default = 0.5)
      )
    ),

    threshold = list(
      label = "Threshold (Binary)",
      category = "4. Segmentation & Filters",
      tooltip = "Convert to black and white.",
      params = list(
        type = list(type = "select", label = "Type", options = c("black", "white"), default = "black"),
        threshold = list(type = "slider", label = "Threshold Value", min = 0, max = 255, step = 1, default = 127),
        channel = list(type = "select", label = "Channel", options = c("all", "red", "green", "blue", "cyan", "magenta", "yellow", "black"), default = "all")
      )
    ),

    quantize = list(
      label = "Quantize (Colors)",
      category = "4. Segmentation & Filters",
      tooltip = "Reduce to N colors.",
      params = list(colors = list(type = "slider", label = "Number of Colors", min = 2, max = 12, step = 1, default = 8))
    ),

    level = list(
      label = "Level (Contrast)",
      category = "4. Segmentation & Filters",
      tooltip = "Remap tonal range.",
      params = list(
        black_point = list(type = "slider", label = "Black Point", min = 0, max = 255, step = 1, default = 0),
        white_point = list(type = "slider", label = "White Point", min = 0, max = 255, step = 1, default = 255)
      )
    ),

    fill = list(
      label = "Flood Fill",
      category = "4. Segmentation & Filters",
      tooltip = "Fill connected region.",
      params = list(
        point = list(type = "text", label = "Point (+x+y)", default = "+0+0"),
        color = list(type = "text", label = "Fill Color", default = "white"),
        fuzz = list(type = "slider", label = "Fuzz", min = 0, max = 100, step = 0.5, default = 10)
      )
    ),

    morphology = list(
      label = "Morphology",
      category = "4. Segmentation & Filters",
      tooltip = "Dilate, Erode, Open, Close, Smooth.",
      params = list(
        method = list(type = "select", label = "Method", options = c("Dilate", "Erode", "Open", "Close", "Smooth"), default = "Dilate"),
        kernel = list(type = "select", label = "Kernel", options = c("Box", "Circle", "Diamond", "Disk", "Octagon", "Plus", "Ring"), default = "Disk"),
        radius = list(type = "slider", label = "Radius", min = 1, max = 10, step = 1, default = 1),
        iterations = list(type = "slider", label = "Iterations", min = 1, max = 10, step = 1, default = 1)
      )
    ),

    edge = list(
      label = "Edge Detection",
      category = "4. Segmentation & Filters",
      tooltip = "Detect edges in the image. Essential for outline extraction and contour analysis.",
      params = list(radius = list(type = "slider", label = "Radius", min = 0, max = 5, step = 0.5, default = 1))
    ),

    despeckle = list(
      label = "Despeckle",
      category = "4. Segmentation & Filters",
      tooltip = "Remove small noise artifacts while preserving edges.",
      params = list()
    )
  )
}

# ============================================================================
# IMAGE PROCESSING FUNCTIONS
# ============================================================================

# Apply a single operation to an image
# This function wraps the magick functions and handles errors gracefully.
# If an operation fails, it returns the unmodified image so the pipeline
# continues working (fail-safe approach).
apply_operation <- function(image, op_name, params) {
  tryCatch({
    switch(op_name,
           channel = image_channel(image, params$channel),
           colorspace = image_convert(image, colorspace = params$colorspace),
           modulate = image_modulate(image, brightness = params$brightness, saturation = params$saturation, hue = params$hue),
           negate = image_negate(image),
           enhance = image_enhance(image),
           normalize = image_normalize(image),
           invert = image_negate(image),  # Invert is functionally equivalent to negate in magick
           rotate = image_rotate(image, params$degrees),
           flip_h = image_flip(image),
           flip_v = image_flop(image),
           trim = image_trim(image),
           blur = image_blur(image, radius = params$radius, sigma = params$sigma),
           threshold = image_threshold(image, type = params$type, threshold = paste0(params$threshold, "%"), channel = params$channel),
           quantize = image_quantize(image, max = as.integer(params$colors)),
           level = image_level(image, black = as.integer(params$black_point), white = as.integer(params$white_point)),
           fill = image_fill(image, point = params$point, color = params$color, fuzz = params$fuzz),
           morphology = image_morphology(image, method = params$method, kernel = paste0(params$kernel, ":", as.integer(params$radius)), iterations = as.integer(params$iterations)),
           edge = image_edge(image, radius = params$radius),
           despeckle = image_despeckle(image),
           image
    )
  }, error = function(e) {
    # If something goes wrong, log it and return the unchanged image
    message(paste("Error in", op_name, ":", e$message))
    image
  })
}

# Apply the entire pipeline sequentially
# Each operation is applied in order, only if it's enabled.
# This allows users to disable operations without deleting them.
apply_pipeline <- function(image, pipeline) {
  if (length(pipeline) == 0) return(image)
  result <- image
  for (step in pipeline) {
    if (!isFALSE(step$enabled)) {
      result <- apply_operation(result, step$op, step$params)
    }
  }
  result
}

# Generate reproducible R code from the pipeline
# This creates a human-readable R script that can be run independently.
# Only enabled operations are included in the output.
# Parameters are formatted intelligently: integers as integers, floats with 2 decimals.
pipeline_to_code <- function(pipeline) {
  if (length(pipeline) == 0) return("image")
  code <- "image %>%\n"
  enabled_ops <- Filter(function(x) !isFALSE(x$enabled), pipeline)
  for (i in seq_along(enabled_ops)) {
    step <- enabled_ops[[i]]
    if (length(step$params) > 0) {
      # Format each parameter appropriately
      param_strs <- mapply(function(k, v) {
        if (is.character(v)) sprintf('%s = "%s"', k, v)
        else if (is.numeric(v)) if (v == floor(v)) sprintf('%s = %d', k, as.integer(v)) else sprintf('%s = %.2f', k, v)
        else sprintf('%s = %s', k, v)
      }, names(step$params), step$params, SIMPLIFY = TRUE)
      func_call <- sprintf("  image_%s(%s)", step$op, paste(param_strs, collapse = ", "))
    } else {
      func_call <- sprintf("  image_%s()", step$op)
    }
    code <- paste0(code, func_call)
    # Add pipe operator between operations (except after the last one)
    if (i < length(enabled_ops)) code <- paste0(code, " %>%\n")
  }
  code
}

# ============================================================================
# USER INTERFACE (UI)
# ============================================================================
# The UI is built with bslib (Bootstrap Shiny library) for a modern, dark theme.
# Structure: Sidebar (controls) + Main content (3-tab interface)

ui <- page_sidebar(
  theme = bs_theme(preset = "darkly"),

  # ---- STYLING & SCRIPTS ----
  tags$head(
    tags$style(HTML("
      /* Category labels for organizing operations */
      .op-category-title { font-weight: bold; color: #0d6efd; font-size: 11px; text-transform: uppercase; margin-top: 12px; margin-bottom: 6px; letter-spacing: 1px; }

      /* Buttons for adding operations */
      .add-op-btn { width: 100%; padding: 8px; background: #0d6efd; color: white; border: none; border-radius: 4px; cursor: pointer; font-size: 12px; margin-bottom: 4px; transition: all 0.2s; }
      .add-op-btn:hover { background: #0b5ed7; transform: translateX(2px); }

      /* Pipeline operation nodes - the main display in Pipeline tab */
      .pipeline-node { border-left: 4px solid #0d6efd; border-radius: 4px; padding: 12px; margin-bottom: 12px; background: #f8f9fa; color: #212529; display: flex; justify-content: space-between; align-items: flex-start; transition: all 0.2s; }
      .pipeline-node:hover { background: #fff; box-shadow: 0 2px 8px rgba(0,0,0,0.1); }
      .pipeline-node.muted { opacity: 0.6; border-left-color: #999; background: #e9ecef; }

      /* Text styling within pipeline nodes */
      .node-title { font-weight: bold; font-size: 14px; margin-bottom: 6px; }
      .node-params { font-size: 12px; color: #666; line-height: 1.5; word-break: break-word; }
      .param-item { margin: 3px 0; padding: 2px 0; }
      .param-key { font-weight: 600; color: #0d6efd; }
      .param-value { color: #495057; font-family: monospace; background: #e9ecef; padding: 1px 4px; border-radius: 2px; }

      /* Action buttons (edit, delete, move, etc.) */
      .node-actions { display: flex; gap: 4px; margin-left: 12px; flex-shrink: 0; }
      .action-btn { padding: 6px 8px; font-size: 13px; background: #e9ecef; border: 1px solid #dee2e6; border-radius: 3px; cursor: pointer; transition: all 0.15s; }
      .action-btn:hover { background: #dee2e6; border-color: #adb5bd; }

      /* Code display box with dark theme */
      .code-box { background: #1a1a1a; color: #e0e0e0; padding: 20px; border-radius: 6px; font-family: 'Courier New', monospace; font-size: 14px; max-height: 500px; overflow-y: auto; border: 1px solid #444; line-height: 1.6; box-shadow: inset 0 2px 8px rgba(0,0,0,0.3); }

      /* Tooltip styling (appears on hover) */
      [title]:hover::after { content: attr(title); position: absolute; bottom: 125%; left: 50%; transform: translateX(-50%); padding: 10px 12px; background: #333; color: #fff; border-radius: 4px; font-size: 12px; white-space: normal; max-width: 200px; word-wrap: break-word; z-index: 1000; pointer-events: none; box-shadow: 0 2px 8px rgba(0,0,0,0.3); font-weight: normal; line-height: 1.4; }
      [title]:hover::before { content: ''; position: absolute; bottom: 120%; left: 50%; transform: translateX(-50%); width: 0; height: 0; border-left: 5px solid transparent; border-right: 5px solid transparent; border-top: 5px solid #333; z-index: 1000; pointer-events: none; }

      /* File input (hidden, accessed via drop zone) */
      #image_files { display: none; }

      /* Drag and drop zone styling */
      .drop-zone { border: 2px dashed #0d6efd; border-radius: 4px; padding: 20px; text-align: center; color: #999; font-size: 12px; cursor: pointer; transition: all 0.2s; }
      .drop-zone:hover { background: #f0f7ff; border-color: #0b5ed7; }
      .drop-zone.dragover { background: #e7f1ff; border-color: #0d6efd; color: #0d6efd; }

      /* Composite controls (overlay slider, etc.) */
      .composite-controls { background: #f5f5f5; padding: 15px; border-radius: 4px; margin-bottom: 15px; border-left: 4px solid #0d6efd; }
      .composite-controls label { font-weight: 600; font-size: 13px; margin-bottom: 8px; display: block; }
      .control-row { margin-bottom: 12px; }
      .control-row:last-child { margin-bottom: 0; }

      /* Inline editor for editing parameters in the sidebar */
      #inline_editor { padding: 15px; background: #f5f5f5; border-radius: 4px; border-left: 4px solid #0d6efd; margin-bottom: 15px; }
      #inline_editor h6 { color: #0d6efd; margin-bottom: 12px; margin-top: 0; font-size: 13px; font-weight: 600; }
      #inline_editor label { color: #333; font-weight: 600; font-size: 12px; }
      #inline_editor .form-control, #inline_editor .form-select { background: #fff; border: 1px solid #ddd; color: #333; font-size: 12px; }
      #inline_editor .form-control:focus, #inline_editor .form-select:focus { border-color: #0d6efd; box-shadow: 0 0 0 0.2rem rgba(13, 110, 253, 0.15); }

      /* Footer styling */
      footer { background: #1a1a1a; color: #999; padding: 15px; text-align: center; font-size: 12px; border-top: 1px solid #333; margin-top: 30px; }
      footer a { color: #0d6efd; text-decoration: none; }
      footer a:hover { text-decoration: underline; }
    "))
  ),

  # ---- DRAG AND DROP JAVASCRIPT ----
  # This script enables drag-and-drop functionality for the image upload zone
  tags$script(HTML("
    $(document).ready(function() {
      var dropZone = document.querySelector('.drop-zone');
      if (dropZone) {
        // Highlight zone when user drags files over it
        dropZone.addEventListener('dragover', function(e) { e.preventDefault(); e.stopPropagation(); dropZone.classList.add('dragover'); });
        // Remove highlight when user drags away
        dropZone.addEventListener('dragleave', function(e) { e.preventDefault(); e.stopPropagation(); dropZone.classList.remove('dragover'); });
        // Handle the drop - pass files to Shiny
        dropZone.addEventListener('drop', function(e) { e.preventDefault(); e.stopPropagation(); dropZone.classList.remove('dragover'); var files = e.dataTransfer.files; var fileInput = document.getElementById('image_files'); if (fileInput && files.length > 0) { fileInput.files = files; $(fileInput).trigger('change'); } });
        // Click on drop zone to open file browser
        dropZone.addEventListener('click', function() { document.getElementById('image_files').click(); });
      }
    });
  ")),

  shinyjs::useShinyjs(),

  # ---- SIDEBAR ----
  sidebar = sidebar(
    h5("🕊️ Momoshop", style = "font-weight: 700; letter-spacing: 0.5px; margin-bottom: 20px;"),

    # IMAGE MANAGEMENT SECTION
    accordion(
      accordion_panel(
        "📷 Images",
        # Hidden file input - accessed via drag/drop or click
        tags$input(id = "image_files", type = "file", accept = "image/*", multiple = TRUE, style = "display: none;"),
        # Drag and drop zone (also clickable)
        div(class = "drop-zone", "📁 Drop images here or click to browse"),
        # Thumbnail grid showing loaded images
        uiOutput("image_selector"),
        # Image metadata (dimensions, colorspace, filename)
        uiOutput("image_info"),
        hr(),
        # Toggle between Separate (side-by-side) and Composite (overlay) views
        div(style = "width: 100%; margin-bottom: 15px;",
            switchInput("view_mode", "View Mode", onLabel = "Composite", offLabel = "Separate", value = FALSE, width = "100%")
        ),
        # Canvas size slider - controls display size of all images
        sliderInput("img_size", "Canvas Size", min = 200, max = 1200, value = 400, step = 100),
        # In Separate mode: option to show original images alongside processed
        conditionalPanel("input.view_mode == false",
                         div(style = "width: 100%; margin-bottom: 15px;",
                             switchInput("show_original", "Show Original", onLabel = "Yes", offLabel = "No", value = FALSE, width = "100%")
                         )
        ),
        # In Composite mode: opacity slider for overlay blending
        conditionalPanel("input.view_mode == true",
                         div(class = "composite-controls",
                             div(class = "control-row",
                                 sliderInput("overlay_alpha", "Overlay Opacity", min = 0, max = 100, value = 50, step = 1, width = "100%")
                             )
                         )
        )
      )
    ),

    # OPERATIONS SECTION
    accordion(
      accordion_panel(
        "📦 Operations",
        # Inline editor appears here when user clicks "Edit" on a pipeline operation
        uiOutput("inline_editor_ui"),
        hr(style = "margin-top: 15px; margin-bottom: 15px;"),
        # Dynamically generated buttons for all available operations
        uiOutput("ops_buttons")
      )
    )
  ),

  # ---- MAIN CONTENT AREA ----
  # Three-tab interface: Preview, Pipeline, Code
  navset_card_tab(
    nav_panel("Preview",
              div(style = "padding: 20px;",
                  uiOutput("image_display")  # Shows images with current pipeline applied
              )
    ),
    nav_panel("Pipeline",
              div(style = "padding: 20px;",
                  uiOutput("pipeline_display")  # Shows the processing steps in order
              )
    ),
    nav_panel("Code",
              div(style = "padding: 20px;",
                  h6("R Pipeline Code", style = "color: #0d6efd; font-weight: bold; margin-bottom: 15px; font-size: 14px;"),
                  div(class = "code-box", verbatimTextOutput("code_display")),  # Reproducible R code
                  br(),
                  actionButton("copy_btn", "📋 Copy Code", class = "btn btn-success btn-sm"),
                  actionButton("dl_btn", "⬇️ Download", class = "btn btn-info btn-sm")
              )
    )
  ),

  # ---- FOOTER ----
  tags$footer(
    "Part of ",
    tags$strong("MomX"),
    " — Morphometric Toolkit • ",
    tags$a(href = "https://github.com/MomX", target = "_blank", "GitHub"),
    " • ",
    tags$a(href = "https://momx.github.io/", target = "_blank", "Docs")
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================
# The server function handles all reactive computations, user interactions,
# and updates to the UI. It uses Shiny's reactive programming model.

server <- function(input, output, session) {
  # Increase max file upload size to 100MB (default is 5MB)
  options(shiny.maxRequestSize = 100 * 1024 ^ 2)

  # ---- REACTIVE VALUES ----
  # These store the application state and update reactively
  rv <- reactiveValues(
    images = list(),           # List of loaded magick image objects
    current_idx = 1,           # Currently selected image index (for thumbnails)
    pipeline = list(),         # List of processing steps (operations)
    editing_id = NULL,         # ID of the operation currently being edited
    temp_params = NULL,        # Temporary parameters while editing (before save)
    next_id = 1                # Counter for assigning unique IDs to operations
  )

  # ---- IMAGE LOADING ----
  # When user uploads images (via drag/drop or file browser), read them
  observeEvent(input$image_files, {
    if (!is.null(input$image_files)) {
      rv$images <- lapply(input$image_files$datapath, function(path) image_read(path))
      rv$current_idx <- 1  # Reset to first image
    }
  })

  # ---- IMAGE SELECTOR (THUMBNAILS) ----
  # Display a grid of small thumbnails for quick image navigation
  output$image_selector <- renderUI({
    if (length(rv$images) == 0) return(p("No images", style = "color: #999; font-size: 12px;"))
    div(style = "border: 1px solid #444; border-radius: 4px; padding: 8px; margin-bottom: 10px; display: grid; grid-template-columns: repeat(3, 1fr); gap: 6px;",
        lapply(seq_along(rv$images), function(i) {
          # Generate small base64-encoded thumbnail for display
          b64 <- base64enc::base64encode(image_write(image_scale(rv$images[[i]], "80x80!"), format = "jpg"))
          div(style = paste0("border: ", if (i == rv$current_idx) "3px solid #0d6efd" else "1px solid #666", "; border-radius: 4px; cursor: pointer; overflow: hidden;"),
              onclick = sprintf("Shiny.setInputValue('select_img', %d);", i),
              img(src = sprintf("data:image/jpeg;base64,%s", b64), style = "width: 100%; display: block;")
          )
        })
    )
  })

  observeEvent(input$select_img, { rv$current_idx <- input$select_img })

  # ---- IMAGE INFO ----
  # Show metadata about the selected image (dimensions, colorspace, filename)
  output$image_info <- renderUI({
    if (length(rv$images) == 0 || is.null(rv$current_idx)) return(NULL)
    idx <- rv$current_idx
    if (idx > length(rv$images)) idx <- 1
    img_info <- image_info(rv$images[[idx]])
    filepath <- input$image_files$name[idx]
    div(style = "padding: 10px 0; font-size: 11px; margin-top: 10px;",
        p(sprintf("📐 %dx%d", img_info$width[1], img_info$height[1]), style = "margin: 2px 0; color: #fff;"),
        p(sprintf("🎨 %s", img_info$colorspace[1]), style = "margin: 2px 0; color: #fff;"),
        p(sprintf("📁 %s", filepath), style = "margin: 2px 0; word-break: break-all; color: #ccc;")
    )
  })

  # ---- OPERATION BUTTONS ----
  # Generate buttons for all available operations, organized by category
  output$ops_buttons <- renderUI({
    ops_defs <- get_operation_defs()
    categories <- sort(unique(sapply(ops_defs, function(x) x$category)))
    lapply(categories, function(cat) {
      ops_in_cat <- unique(names(Filter(function(x) x$category == cat, ops_defs)))
      tagList(
        div(class = "op-category-title", cat),  # Category header
        lapply(ops_in_cat, function(op) {
          def <- ops_defs[[op]]
          # Each button has a tooltip explaining what it does
          actionButton(paste0("add_", op), def$label, class = "add-op-btn", title = def$tooltip)
        })
      )
    })
  })

  # ---- ADD OPERATION LISTENER ----
  # When user clicks an operation button, add it to the pipeline
  lapply(names(get_operation_defs()), function(op_name) {
    observeEvent(input[[paste0("add_", op_name)]], {
      def <- get_operation_defs()[[op_name]]
      # Initialize parameters with their default values
      params <- setNames(lapply(names(def$params), function(p) def$params[[p]]$default), names(def$params))
      # Add the new operation to the pipeline
      rv$pipeline[[length(rv$pipeline) + 1]] <- list(
        id = rv$next_id,
        op = op_name,
        params = params,
        enabled = TRUE
      )
      rv$next_id <- rv$next_id + 1
      # If the operation has parameters, open the inline editor
      if (length(def$params) > 0) {
        rv$editing_id <- rv$next_id - 1
        rv$temp_params <- params
      }
    })
  })

  # ---- INLINE EDITOR UI ----
  # Shows parameter editing controls in the sidebar when user clicks "Edit"
  output$inline_editor_ui <- renderUI({
    if (is.null(rv$editing_id)) return(NULL)  # Don't show if not editing

    # Find which operation we're editing
    edited_op <- NULL
    for (step in rv$pipeline) {
      if (!is.null(step) && step$id == rv$editing_id) {
        edited_op <- step$op
        break
      }
    }
    if (is.null(edited_op)) return(NULL)

    # Render the editor panel
    div(
      id = "inline_editor",
      h6("Edit Parameters"),
      build_modal_ui(edited_op, rv$temp_params),
      div(style = "display: flex; gap: 8px; margin-top: 15px; padding-top: 12px; border-top: 1px solid #ddd;",
          actionButton("inline_save", "✓ Save", class = "btn btn-success btn-sm"),
          actionButton("inline_cancel", "Cancel", class = "btn btn-secondary btn-sm")
      )
    )
  })

  # ---- PIPELINE OPERATION HANDLERS ----
  # These handle user interactions with the pipeline (edit, toggle, delete, reorder)

  observeEvent(input$pipeline_edit_click, {
    step_id <- input$pipeline_edit_click
    for (i in seq_along(rv$pipeline)) {
      if (!is.null(rv$pipeline[[i]]) && rv$pipeline[[i]]$id == step_id) {
        rv$editing_id <- step_id
        rv$temp_params <- rv$pipeline[[i]]$params
        break
      }
    }
  })

  observeEvent(input$pipeline_toggle_click, {
    # Enable/disable an operation without removing it from the pipeline
    step_id <- input$pipeline_toggle_click
    for (i in seq_along(rv$pipeline)) {
      if (!is.null(rv$pipeline[[i]]) && rv$pipeline[[i]]$id == step_id) {
        rv$pipeline[[i]]$enabled <- !rv$pipeline[[i]]$enabled
        rv$pipeline <- rv$pipeline  # Force reactive update
        break
      }
    }
  })

  observeEvent(input$pipeline_delete_click, {
    # Remove an operation from the pipeline entirely
    step_id <- input$pipeline_delete_click
    rv$pipeline <- Filter(function(x) is.null(x) || x$id != step_id, rv$pipeline)
  })

  observeEvent(input$pipeline_up_click, {
    # Move an operation earlier in the pipeline
    step_id <- input$pipeline_up_click
    for (i in seq_along(rv$pipeline)) {
      if (!is.null(rv$pipeline[[i]]) && rv$pipeline[[i]]$id == step_id && i > 1) {
        temp <- rv$pipeline[[i]]
        rv$pipeline[[i]] <- rv$pipeline[[i - 1]]
        rv$pipeline[[i - 1]] <- temp
        break
      }
    }
  })

  observeEvent(input$pipeline_down_click, {
    # Move an operation later in the pipeline
    step_id <- input$pipeline_down_click
    for (i in seq_along(rv$pipeline)) {
      if (!is.null(rv$pipeline[[i]]) && rv$pipeline[[i]]$id == step_id && i < length(rv$pipeline)) {
        temp <- rv$pipeline[[i]]
        rv$pipeline[[i]] <- rv$pipeline[[i + 1]]
        rv$pipeline[[i + 1]] <- temp
        break
      }
    }
  })

  # ---- LIVE PREVIEW WITH DEBOUNCING ----
  # As user adjusts parameters in the editor, update the pipeline parameters
  # Debounced at 500ms to avoid too many re-renders while dragging sliders
  temp_observer <- reactive({
    if (!is.null(rv$editing_id)) {
      for (step in rv$pipeline) {
        if (!is.null(step) && step$id == rv$editing_id) {
          op_def <- get_operation_defs()[[step$op]]
          # Update temp_params from the input controls
          for (param_name in names(op_def$params)) {
            val <- input[[paste0("param_", param_name)]]
            if (!is.null(val)) {
              rv$temp_params[[param_name]] <- val
            }
          }
          # Apply the temp params to the pipeline for live preview
          for (i in seq_along(rv$pipeline)) {
            if (!is.null(rv$pipeline[[i]]) && rv$pipeline[[i]]$id == rv$editing_id) {
              rv$pipeline[[i]]$params <- rv$temp_params
              break
            }
          }
          break
        }
      }
    }
  })
  debounce(temp_observer, 500)  # Wait 500ms after user stops adjusting before re-rendering

  # ---- SAVE/CANCEL INLINE EDITOR ----
  observeEvent(input$inline_save, {
    # Confirm the edits and close the editor
    if (!is.null(rv$editing_id)) {
      for (i in seq_along(rv$pipeline)) {
        if (!is.null(rv$pipeline[[i]]) && rv$pipeline[[i]]$id == rv$editing_id) {
          rv$pipeline[[i]]$params <- rv$temp_params
          break
        }
      }
    }
    rv$editing_id <- NULL
    rv$temp_params <- NULL
  })

  observeEvent(input$inline_cancel, {
    # Discard edits and close the editor
    rv$editing_id <- NULL
    rv$temp_params <- NULL
  })

  # ---- IMAGE DISPLAY (PREVIEW TAB) ----
  # Show processed images with current pipeline applied
  # Layout adapts based on window width and view mode
  output$image_display <- renderUI({
    if (length(rv$images) == 0) return(div(style = "text-align: center; color: #999; padding: 50px;", "Upload images to get started"))

    if (input$view_mode == FALSE) {
      # SEPARATE VIEW: Processed and original images paired, responsive grid
      div(
        lapply(seq_along(rv$images), function(idx) {
          canvas_size <- paste0(input$img_size, "x", input$img_size)
          orig <- image_scale(rv$images[[idx]], canvas_size)
          proc <- apply_pipeline(orig, rv$pipeline)
          proc_b64 <- base64enc::base64encode(image_write(proc, format = "jpg"))

          img_name <- input$image_files$name[idx]

          # Build list of images to display for this item
          result_list <- list(
            div(style = "text-align: center;",
                p(sprintf("%s [processed]", img_name), style = "color: #999; font-size: 11px; margin-bottom: 6px;"),
                img(src = sprintf("data:image/jpeg;base64,%s", proc_b64), style = sprintf("width: %dpx; object-fit: contain; border-radius: 4px;", input$img_size))
            )
          )

          # Optionally add the original image
          if (isTRUE(input$show_original)) {
            orig_b64 <- base64enc::base64encode(image_write(orig, format = "jpg"))
            result_list[[2]] <- div(style = "text-align: center;",
                                    p(sprintf("%s [original]", img_name), style = "color: #999; font-size: 11px; margin-bottom: 6px;"),
                                    img(src = sprintf("data:image/jpeg;base64,%s", orig_b64), style = sprintf("width: %dpx; object-fit: contain; border-radius: 4px;", input$img_size))
            )
          }

          # Use responsive grid: as many columns as possible given 250px minimum
          div(style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 15px; margin-bottom: 20px;", result_list)
        })
      )
    } else {
      # COMPOSITE VIEW: Original with processed overlay, adjustable opacity
      alpha <- input$overlay_alpha / 100
      composite_imgs <- lapply(seq_along(rv$images), function(i) {
        canvas_size <- paste0(input$img_size, "x", input$img_size)
        img_scaled <- image_scale(rv$images[[i]], canvas_size)
        list(
          orig = image_write(img_scaled, format = "jpg"),
          proc = image_write(apply_pipeline(img_scaled, rv$pipeline), format = "jpg"),
          name = input$image_files$name[i]
        )
      })
      # Responsive grid layout
      div(style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 15px;",
          lapply(composite_imgs, function(img_data) {
            orig_b64_tmp <- base64enc::base64encode(img_data$orig)
            proc_b64_tmp <- base64enc::base64encode(img_data$proc)
            div(style = "text-align: center;",
                p(img_data$name, style = "color: #999; font-size: 11px; margin-bottom: 6px;"),
                div(style = "position: relative; display: inline-block;",
                    img(src = sprintf("data:image/jpeg;base64,%s", orig_b64_tmp), style = sprintf("width: %dpx; display: block; border-radius: 4px;", input$img_size)),
                    # Processed image overlaid with adjustable opacity
                    img(src = sprintf("data:image/jpeg;base64,%s", proc_b64_tmp), style = sprintf("position: absolute; top: 0; left: 0; width: %dpx; opacity: %.2f; border-radius: 4px;", input$img_size, alpha))
                )
            )
          })
      )
    }
  })

  # ---- PIPELINE DISPLAY (PIPELINE TAB) ----
  # Show the processing pipeline with controls
  output$pipeline_display <- renderUI({
    if (length(rv$pipeline) == 0) return(p("No operations. Add one from the sidebar.", style = "color: #999;"))
    lapply(seq_along(rv$pipeline), function(i) {
      step <- rv$pipeline[[i]]
      # Format parameters for display
      if (length(step$params) > 0) {
        params_str <- paste(lapply(names(step$params), function(k) {
          sprintf('<div class="param-item"><span class="param-key">%s:</span> <span class="param-value">%s</span></div>', k, step$params[[k]])
        }), collapse = "")
      } else {
        params_str <- '<div class="param-item" style="color: #999;"><em>No parameters</em></div>'
      }

      div(class = if (step$enabled) "pipeline-node" else "pipeline-node muted",
          div(class = "node-info",
              div(class = "node-title", get_operation_defs()[[step$op]]$label),
              HTML(params_str)
          ),
          # Action buttons for this operation
          div(class = "node-actions",
              actionButton(paste0("pipeline_toggle_", step$id), if (step$enabled) "👁️" else "🙈", class = "action-btn", title = if (step$enabled) "Disable" else "Enable", onclick = sprintf("Shiny.setInputValue('pipeline_toggle_click', %d);", step$id)),
              actionButton(paste0("pipeline_edit_", step$id), "✏️", class = "action-btn", title = "Edit", onclick = sprintf("Shiny.setInputValue('pipeline_edit_click', %d);", step$id)),
              if (i > 1) actionButton(paste0("pipeline_up_", step$id), "⬆️", class = "action-btn", title = "Move up", onclick = sprintf("Shiny.setInputValue('pipeline_up_click', %d);", step$id)),
              if (i < length(rv$pipeline)) actionButton(paste0("pipeline_down_", step$id), "⬇️", class = "action-btn", title = "Move down", onclick = sprintf("Shiny.setInputValue('pipeline_down_click', %d);", step$id)),
              actionButton(paste0("pipeline_delete_", step$id), "🗑️", class = "action-btn", title = "Delete", onclick = sprintf("Shiny.setInputValue('pipeline_delete_click', %d);", step$id))
          )
      )
    })
  })

  # ---- CODE DISPLAY (CODE TAB) ----
  # Show reproducible R code for the pipeline
  output$code_display <- renderText(pipeline_to_code(rv$pipeline))

  # Copy code to clipboard
  observeEvent(input$copy_btn, {
    shinyjs::runjs(sprintf("navigator.clipboard.writeText(%s).then(() => alert('Copied!'));", jsonlite::toJSON(pipeline_to_code(rv$pipeline))))
  })

  # Download code as R file
  output$dl_btn <- downloadHandler(
    filename = "pipeline.R",
    content = function(file) writeLines(pipeline_to_code(rv$pipeline), file)
  )
}

# ============================================================================
# HELPER FUNCTION: BUILD PARAMETER INPUT CONTROLS
# ============================================================================
# Dynamically creates input controls based on parameter type
# Supports: sliders, select dropdowns, radio buttons, text inputs

build_modal_ui <- function(op_name, params) {
  op_def <- get_operation_defs()[[op_name]]
  if (length(op_def$params) == 0) return(p("No parameters"))
  # Create an input control for each parameter
  lapply(names(op_def$params), function(pname) {
    pdef <- op_def$params[[pname]]
    current_val <- params[[pname]] %||% pdef$default
    div(style = "margin-bottom: 15px;",
        tags$label(tags$strong(pdef$label)),
        if (pdef$type == "slider") {
          sliderInput(paste0("param_", pname), NULL, pdef$min, pdef$max, pdef$step, value = current_val)
        } else if (pdef$type == "select") {
          selectInput(paste0("param_", pname), NULL, pdef$options, selected = current_val)
        } else if (pdef$type == "radio") {
          radioButtons(paste0("param_", pname), NULL, pdef$options, selected = current_val, inline = FALSE)
        } else if (pdef$type == "text") {
          textInput(paste0("param_", pname), NULL, value = current_val)
        }
    )
  })
}
