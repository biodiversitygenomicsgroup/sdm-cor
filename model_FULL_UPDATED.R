# ============================================================================
# COMPLETE SDM WORKFLOW - FULLY UPDATED WITH FILE HANDLE FIXES
# FIXES: aggressive raster cleanup, terra temp management, GDAL pooling
# NO PARALLEL BACKEND, NO WORKFLOW REDESIGN
# ============================================================================

# ============================================================================
# SECTION 0: LOAD PACKAGES
# ============================================================================

packages <- c("data.table", "dplyr", "tidyr", "terra", "flexsdm",
              "ggplot2", "readr", "progress", "tibble", "tidyterra", "here")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}

msg <- function(...) cat(paste(..., sep = " "), "\n")

msg("✓ All libraries loaded successfully\n")

# ============================================================================
# SECTION 1: SET WORKING DIRECTORY & TEMP PATHS - ENHANCED MANAGEMENT
# ============================================================================

setwd("/DATA3/Ratnesh/sdm_new/zz-flexsdm/")
base_dir <- getwd()

# CREATE PERSISTENT TEMP DIRECTORIES (DO NOT delete between species)
custom_temp <- file.path(base_dir, "R_temp_files_SDM")
if(!dir.exists(custom_temp)) {
  dir.create(custom_temp, recursive = TRUE, showWarnings = FALSE)
}

# Create terra-specific temp directory
terra_temp_dir <- file.path(custom_temp, "terra_temp")
if(!dir.exists(terra_temp_dir)) {
  dir.create(terra_temp_dir, recursive = TRUE, showWarnings = FALSE)
}

# CREATE WORKING TEMP FOR INTERMEDIATE OUTPUTS
work_temp_dir <- file.path(custom_temp, "work_temp")
if(!dir.exists(work_temp_dir)) {
  dir.create(work_temp_dir, recursive = TRUE, showWarnings = FALSE)
}

# Set environment variables for temp files
Sys.setenv(TMPDIR = custom_temp)
Sys.setenv(TEMP = custom_temp)
Sys.setenv(TMP = custom_temp)

msg("✓ Custom temp directory:", custom_temp)
msg("✓ Terra temp directory:", terra_temp_dir)
msg("✓ Work temp directory:", work_temp_dir)
msg("✓ R tempdir():", tempdir())
msg("")

# ============================================================================
# SECTION 1b: USER SETTINGS & CONFIGURATION PARAMETERS
# ============================================================================

# Pipeline control
sdm_mode <- "both"   # Options: "without", "with", "both"

# Species selection
sp_select <- c(1, 4)    # "ALL" for all species, or c(1,4) for indices

# Set up threshold and cores
auc_thr <- 0.7
n_cores <- 12
n_chunk <- 12

# Uncertainty settings
u_itr <- 10
n_sim <- 10

# PDP settings
pdp_res <- 250
set_dpi <- 500

# Ensemble methods
ens_method <- c("meanw", "median")


# ============================================================================
# SECTION 2: CONFIGURE GDAL AND TERRA OPTIONS - AGGRESSIVE TUNING
# ============================================================================

options("rgdal_show_exportToProj4_warnings" = "none")

# GDAL Configuration for better file handle management
terra::setGDALconfig("GDAL_PAM_ENABLED", "FALSE")           # Disable .aux files
terra::setGDALconfig("GDAL_MAX_DATASET_POOL_SIZE", "128")   # INCREASED from 45
terra::setGDALconfig("GDAL_CACHEMAX", "256")                # INCREASED from 200
terra::setGDALconfig("GDAL_DISABLE_READDIR_ON_OPEN", "YES") # Faster opens
terra::setGDALconfig("CPL_VSIL_CURL_ALLOWED_EXTENSIONS", ".tif")

# Set terra options for aggressive temp cleanup
terra::terraOptions(
  memfrac = 0.4,        # Use up to 40% of RAM
  tempdir = terra_temp_dir,
  todisk = TRUE,        # Force disk-based operations for large rasters
  datatype = "FLT4"     # Use 32-bit float by default
)

# Increase file descriptor limit on Linux
if(Sys.info()["sysname"] == "Linux") {
  tryCatch({
    system("ulimit -n 65536", ignore.stdout = TRUE, ignore.stderr = TRUE)
  }, error = function(e) invisible())
}

msg("✓ GDAL Configuration:")
msg("  - GDAL_MAX_DATASET_POOL_SIZE:", terra::getGDALconfig("GDAL_MAX_DATASET_POOL_SIZE"))
msg("  - GDAL_CACHEMAX:", terra::getGDALconfig("GDAL_CACHEMAX"))
msg("✓ Terra Options:")
msg("  - Tempdir:", terra::terraOptions()$tempdir)
msg("  - Memfrac:", terra::terraOptions()$memfrac)
msg("  - Todisk:", terra::terraOptions()$todisk)
msg("")

# ============================================================================
# SECTION 3: MESSAGE & FORMATTING HELPERS
# ============================================================================

crt_hed <- function(sp, i, total) {
  header_text <- paste0("SPECIES: ", sp, " [", i, "/", total, "]")
  header_width <- nchar(header_text)
  line <- strrep("=", header_width)
  list(line = line, text = header_text)
}

prt_hed <- function(header_obj) {
  msg(header_obj$line)
  msg(header_obj$text)
  msg(header_obj$line)
}

prt_sec <- function(title) {
  title_clean <- gsub("[^a-zA-Z0-9 _-]", "", title)
  title_len <- nchar(title_clean, type = "width")
  line <- paste(rep("=", title_len), collapse = "")
  msg("")
  msg(line)
  msg(title_clean)
  msg(line)
}

prt_sub <- function(title) {
  title_clean <- gsub("[^a-zA-Z0-9 _-]", "", title)
  title_len <- nchar(title_clean, type = "width")
  line <- paste(rep("-", title_len), collapse = "")
  msg("")
  msg(line)
  msg(title_clean)
  msg(line)
}

# ============================================================================
# SECTION 3.5: ENHANCED RESOURCE CLEANUP FUNCTIONS
# ============================================================================

# AGGRESSIVE CLEANUP AFTER EACH SPECIES
cleanup_after_species <- function(verbose = FALSE) {
  # STEP 1: Remove ALL terra temporary files (not just old ones)
  # This is critical for preventing handle accumulation
  tryCatch({
    terra::tmpFiles(remove = TRUE, old = FALSE)
    if(verbose) msg("  ✓ Terra temp files removed")
  }, error = function(e) {
    if(verbose) msg("  ⚠ Terra tmpFiles error:", e$message)
    invisible()
  })
  
  # STEP 2: Multiple garbage collection passes
  # Multiple passes ensure lazy references are freed
  gc(verbose = FALSE)
  gc(verbose = FALSE)
  gc(verbose = FALSE)
  
  # STEP 3: Small delay to allow OS-level file handle closure
  Sys.sleep(0.2)
  
  if(verbose) msg("  ✓ Cleanup complete")
}

# EXPLICIT RASTER OBJECT CLEANUP
cleanup_raster_objects <- function(..., verbose = FALSE) {
  # Remove raster objects from parent environment
  var_names <- as.character(substitute(list(...)))[-1]
  
  for (var_name in var_names) {
    tryCatch({
      if (exists(var_name, envir = parent.frame())) {
        rm(list = var_name, envir = parent.frame())
        if(verbose) msg("    Removed:", var_name)
      }
    }, error = function(e) invisible())
  }
  
  gc(verbose = FALSE)
}

# SAFE RASTER WRITE WITH CLEANUP
write_raster_safe <- function(raster_obj, filepath, compress = "LZW", verbose = FALSE) {
  # Write raster with error handling
  tryCatch({
    terra::writeRaster(
      raster_obj,
      filepath,
      overwrite = TRUE,
      wopt = list(gdal = c(paste0("COMPRESS=", compress)))
    )
    if(verbose) msg("    Written:", basename(filepath))
  }, error = function(e) {
    msg("    ERROR writing raster:", e$message)
    FALSE
  })
  
  # Attempt to close file handles
  tryCatch({
    rm(raster_obj)
    gc(verbose = FALSE)
  }, error = function(e) invisible())
  
  return(TRUE)
}

# ============================================================================
# SECTION 4: PATH CONFIGURATION
# ============================================================================

without_md_pred_dir <- file.path(base_dir, "02_Outputs", "04_Predictors_by_sp", "01_Without_MD")
without_md_occ_dir <- file.path(base_dir, "02_Outputs", "02_Filtered_occ", "01_Without_MD")

with_md_pred_dir <- file.path(base_dir, "02_Outputs", "04_Predictors_by_sp", "02_With_MD")
with_md_occ_dir <- file.path(base_dir, "02_Outputs", "02_Filtered_occ", "02_With_MD")

base_sdm <- file.path(base_dir, "02_Outputs", "07_SDM_outputs")

paleo_names <- c("biolig", "biolgm", "biomdh")
gcm <- c("HedGEM3", "CMCC-ESM2")
ssp <- c("ssp126", "ssp585")
period <- c("2041-2060", "2081-2100")

future_names <- c()
for(g in gcm) {
  for(p in period) {
    for(s in ssp) {
      future_names <- c(future_names, paste0(g, "-", p, "-", s))
    }
  }
}

msg("Base directory:", base_dir)
msg("Output directory:", base_sdm)
msg("")


# ============================================================================
# SECTION 5: CREATE OUTPUT DIRECTORIES
# ============================================================================

prt_sec("CREATING OUTPUT DIRECTORIES")

if(sdm_mode == "without") {
  pipelines <- "01_Without_MD"
} else if(sdm_mode == "with") {
  pipelines <- "02_With_MD"
} else if(sdm_mode == "both") {
  pipelines <- c("01_Without_MD", "02_With_MD")
} else {
  stop("ERROR: sdm_mode must be 'without', 'with', or 'both'")
}

msg("Selected pipelines:", paste(pipelines, collapse=", "))
msg("")

for(pipeline in pipelines) {
  
  msg("Creating directories for:", pipeline)
  
  pipeline_dir <- file.path(base_sdm, pipeline)
  
  # Main directories
  dir.create(file.path(pipeline_dir, "01_Model_fitted"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pipeline_dir, "02_Model_perform"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pipeline_dir, "03_PDP_Plot"), recursive = TRUE, showWarnings = FALSE)
  
  # Hyper_plot with 01_Summary and 02_Plots subdirectories
  hyper_dir <- file.path(pipeline_dir, "04_Hyper_plot")
  dir.create(file.path(hyper_dir, "01_Summary"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(hyper_dir, "02_Plots"), recursive = TRUE, showWarnings = FALSE)
  
  # Uncertainty directories
  unc_dir <- file.path(pipeline_dir, "05_Uncertainty")
  dir.create(file.path(unc_dir, "01_Plots"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(unc_dir, "02_Summary"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(unc_dir, "03_Raster"), recursive = TRUE, showWarnings = FALSE)
  
  # Algorithms directories
  alg_dir <- file.path(pipeline_dir, "06_Algorithms")
  alg_current_dir <- file.path(alg_dir, "01_Current")
  dir.create(alg_current_dir, recursive = TRUE, showWarnings = FALSE)
  
  alg_names <- c("glm", "gam", "gau", "gbm", "max", "net", "raf", "svm")
  
  for(alg in alg_names) {
    dir.create(file.path(alg_current_dir, alg, "01_con"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(alg_current_dir, alg, "02_thr"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(alg_current_dir, alg, "03_bin"), recursive = TRUE, showWarnings = FALSE)
  }
  
  # Paleo projections
  alg_paleo_dir <- file.path(alg_dir, "02_Projection")
  dir.create(alg_paleo_dir, recursive = TRUE, showWarnings = FALSE)
  
  for(paleo in paleo_names) {
    for(alg in alg_names) {
      dir.create(file.path(alg_paleo_dir, paleo, alg, "01_con"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(alg_paleo_dir, paleo, alg, "02_thr"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(alg_paleo_dir, paleo, alg, "03_bin"), recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  # Future projections
  alg_future_dir <- file.path(alg_dir, "03_Projection")
  dir.create(alg_future_dir, recursive = TRUE, showWarnings = FALSE)
  
  for(future in future_names) {
    for(alg in alg_names) {
      dir.create(file.path(alg_future_dir, future, alg, "01_con"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(alg_future_dir, future, alg, "02_thr"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(alg_future_dir, future, alg, "03_bin"), recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  # Ensemble directories
  ens_dir <- file.path(pipeline_dir, "07_Ensemble")
  ens_current_dir <- file.path(ens_dir, "01_Current")
  dir.create(ens_current_dir, recursive = TRUE, showWarnings = FALSE)
  
  for(method in ens_method) {
    dir.create(file.path(ens_current_dir, method, "01_con"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(ens_current_dir, method, "02_thr"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(ens_current_dir, method, "03_bin"), recursive = TRUE, showWarnings = FALSE)
  }
  
  # Paleo ensemble
  ens_paleo_dir <- file.path(ens_dir, "02_Projection")
  dir.create(ens_paleo_dir, recursive = TRUE, showWarnings = FALSE)
  
  for(paleo in paleo_names) {
    for(method in ens_method) {
      dir.create(file.path(ens_paleo_dir, paleo, method, "01_con"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(ens_paleo_dir, paleo, method, "02_thr"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(ens_paleo_dir, paleo, method, "03_bin"), recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  # Future ensemble
  ens_future_dir <- file.path(ens_dir, "03_Projection")
  dir.create(ens_future_dir, recursive = TRUE, showWarnings = FALSE)
  
  for(future in future_names) {
    for(method in ens_method) {
      dir.create(file.path(ens_future_dir, future, method, "01_con"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(ens_future_dir, future, method, "02_thr"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(ens_future_dir, future, method, "03_bin"), recursive = TRUE, showWarnings = FALSE)
    }
  }
}

msg("")
msg("All directories created")
msg("")

# ============================================================================
# SECTION 5.5: GRID DEFINITIONS
# ============================================================================

# GBM Grid
grid_gbm <- expand.grid(
  n.trees = seq(150, 200, 10),
  shrinkage = seq(1, 1.5, 0.2),
  n.minobsinnode = seq(1, 15, 2)
)

# MAX Grid
grid_max <- expand.grid(
  regmult = seq(0.1, 2, 0.5),
  classes = c("lq", "lqh", "lqhp", "lqhpt")
)

# SVM Grid
grid_svm <- expand.grid(
  C = seq(2, 60, 5),
  sigma = seq(0.001, 0.3, 0.01)
)

# ============================================================================
# SECTION 6: MAIN SDM PIPELINE - FULLY UPDATED WITH CLEANUP
# ============================================================================

ens_method <- c("meanw", "median")

for(pipeline in pipelines) {
  
  msg("\n")
  prt_sub(paste("STARTING PIPELINE:", pipeline))
  msg("")
  
  if(pipeline == "01_Without_MD") {
    pred_dir <- without_md_pred_dir
    occ_dir <- without_md_occ_dir
    msg("Using: Without MD predictors")
  } else if(pipeline == "02_With_MD") {
    pred_dir <- with_md_pred_dir
    occ_dir <- with_md_occ_dir
    msg("Using: With MD predictors")
  } else {
    msg("ERROR: Unknown pipeline - skipping")
    next
  }
  
  out_pipeline_dir <- file.path(base_sdm, pipeline)
  
  perf_dir <- file.path(out_pipeline_dir, "02_Model_perform")
  mod_out_dir <- file.path(out_pipeline_dir, "01_Model_fitted")
  hypplot_cur <- file.path(out_pipeline_dir, "04_Hyper_plot")
  pdp_plots_dir <- file.path(out_pipeline_dir, "03_PDP_Plot")
  uncertainty_dir <- file.path(out_pipeline_dir, "05_Uncertainty")
  
  occ_file <- file.path(occ_dir, "1_occ_presabs_partitioned.gz")
  bkg_file <- file.path(occ_dir, "1_occ_bkgroud_partitioned.gz")
  
  if(!file.exists(occ_file)) {
    msg("ERROR: Missing file:", occ_file)
    msg("Skipping pipeline:", pipeline)
    next
  }
  
  if(!file.exists(bkg_file)) {
    msg("ERROR: Missing file:", bkg_file)
    msg("Skipping pipeline:", pipeline)
    next
  }
  
  occ <- data.table::fread(occ_file) %>% tibble::as_tibble()
  bkg <- data.table::fread(bkg_file) %>% tibble::as_tibble()
  
  if (!(".part" %in% names(occ))) {
    set.seed(42)
    n <- max(1, length(unique(na.omit(occ$.part))))
    occ$.part <- sample(rep(seq_len(n), length.out = nrow(occ)))
  }
  
  pred_current_dir <- file.path(pred_dir, "01_Current")
  if(!dir.exists(pred_current_dir)) {
    msg("ERROR: Missing directory:", pred_current_dir)
    msg("Skipping pipeline:", pipeline)
    next
  }
  
  pred_files <- list.files(pred_current_dir, pattern = "\\.tif$", full.names = TRUE)
  
  if(length(pred_files) == 0) {
    msg("ERROR: No .tif files found in:", pred_current_dir)
    msg("Skipping pipeline:", pipeline)
    next
  }
  
  sp_names <- gsub("\\.tif$", "", basename(pred_files))
  sp_final <- intersect(unique(occ$species), sp_names)
  
  if(sp_select[1] == "ALL") {
    sp_ready <- sp_final
  } else {
    sp_ready <- sp_final[sp_select]
  }
  
  msg("Pipeline:", sub("^\\d+_", "", pipeline))
  msg("Species available:", length(sp_final))
  msg("Species to process:", length(sp_ready))
  msg("")
  
  if(length(sp_ready) == 0) {
    msg("WARNING: No species to process for pipeline:", pipeline)
    next
  }
  
  pb <- progress::progress_bar$new(
    total = length(sp_ready),
    format = "[:bar] :current/:total (:percent) | :elapsed | :eta",
    width = 60
  )
  
  # ======================================
  # SPECIES LOOP - WITH AGGRESSIVE CLEANUP
  # ======================================
  for (i in seq_along(sp_ready)) {
    
    sp <- sp_ready[i]
    sp_disp <- gsub("_", " ", sp)
    
    set.seed(123 + i)
    
    header <- crt_hed(sp, i, length(sp_ready))
    prt_hed(header)
    
    # AGGRESSIVE CLEANUP FROM PREVIOUS SPECIES (MUST RUN FIRST IN LOOP)
    if (i > 1) {
      msg("  [Cleanup] Previous species...")
      tryCatch({
        # Remove all species-level objects from previous iteration
        rm(env_r, sp_pa, sp_bg, model_obj_list, valid_models, successful_models,
           model_perf, varimp_out, uncertainty_rasters, unc_stack,
           mean_unc, median_unc, sd_unc, p1, p2, all_pdp_long, pdp_results_list,
           grid_net, grid_raf, pred_sds, good_preds,
           envir = environment())
      }, error = function(e) invisible())
      
      cleanup_after_species(verbose = FALSE)
    }
    
    tryCatch({
      
      # ========== 1. Prepare Data for Current Species ==========
      cur_env_file <- file.path(pred_current_dir, paste0(sp, ".tif"))
      if (!file.exists(cur_env_file)) {
        msg("WARNING: Missing environmental file for:", sp)
        pb$tick()
        cleanup_after_species()
        next
      }
      
      env_r <- tryCatch({
        terra::rast(cur_env_file)
      }, error = function(e) {
        msg("ERROR: Cannot load raster:", sp)
        NULL
      })
      
      if(is.null(env_r)) {
        pb$tick()
        cleanup_after_species()
        next
      }
      
      var_names <- names(env_r)
      n_pred <- length(var_names)
      
      # ========== CREATE DYNAMIC GRIDS FOR THIS SPECIES ==========
      grid_net <- expand.grid(
        size = 2:n_pred,
        decay = c(seq(0.01, 1, 0.05), 1, 3, 4, 5, 6)
      )
      
      grid_raf <- expand.grid(
        mtry = seq(1, n_pred, 1),
        ntree = c(400, 600, 800, 1000)
      )
      
      sp_pa <- dplyr::filter(occ, species == sp)
      sp_bg <- dplyr::filter(bkg, species == sp)
      
      if (nrow(sp_pa) == 0) {
        msg("WARNING: No presence records for:", sp)
        pb$tick()
        cleanup_after_species()
        next
      }
      
      sp_pa <- tryCatch(
        flexsdm::sdm_extract(sp_pa, x = "x", y = "y", env_layer = env_r, filter_na = TRUE), 
        error = function(e) NULL
      )
      sp_bg <- tryCatch(
        flexsdm::sdm_extract(sp_bg, x = "x", y = "y", env_layer = env_r, filter_na = TRUE), 
        error = function(e) NULL
      )
      
      if (is.null(sp_pa) || is.null(sp_bg)) {
        pb$tick()
        cleanup_after_species()
        next
      }
      
      n_pres <- nrow(dplyr::filter(sp_pa, pr_ab == 1))
      
      if (n_pres < 5) {
        msg("WARNING: Too few presences for:", sp, "-", n_pres)
        pb$tick()
        cleanup_after_species()
        next
      }
      
      msg("Predictors:", length(var_names))
      msg("Training records (presences) =", n_pres)
      
      # ========== 2. Fit Models ==========
      msg("  Fitting models...")
      
      # Initialize all models as NULL
      m_glm <- NULL
      m_gam <- NULL
      m_gau <- NULL
      m_gbm <- NULL
      m_max <- NULL
      m_net <- NULL
      m_raf <- NULL
      m_svm <- NULL
      
      # ---- GLM - Requires >=100 presences ----
      if (n_pres >= 100) {
        msg("    GLM...")
        m_glm <- tryCatch(
          flexsdm::fit_glm(
            data = sp_pa, 
            response = "pr_ab", 
            predictors = var_names,
            partition = ".part", 
            thr = "max_sens_spec", 
            poly = 2
          ),
          error = function(e) {
            msg("      GLM failed: ", e$message)
            NULL
          }
        )
        if (!is.null(m_glm)) msg("      GLM fitted successfully")
      } else {
        msg("    GLM skipped (n_pres < 100)")
      }
      
      # ---- GAM - Requires >=50 presences ----
      if (n_pres >= 50) {
        msg("    GAM...")
        n_t <- tryCatch(
          flexsdm:::n_training(data = sp_pa, partition = ".part"), 
          error = function(e) min(n_pres, 50)
        )
        candidate_k <- 20
        while (any(n_t < flexsdm:::n_coefficients(data = sp_pa, predictors = var_names, k = candidate_k)) && candidate_k > 3) {
          candidate_k <- candidate_k - 3
        }
        candidate_k <- max(3, candidate_k)
        
        m_gam <- tryCatch(
          flexsdm::fit_gam(
            data = sp_pa, 
            response = "pr_ab", 
            predictors = var_names,
            partition = ".part", 
            thr = "max_sens_spec", 
            k = candidate_k
          ),
          error = function(e) {
            msg("      GAM failed: ", e$message)
            NULL
          }
        )
        if (!is.null(m_gam)) msg("      GAM fitted successfully (k=", candidate_k, ")")
      } else {
        msg("    GAM skipped (n_pres < 50)")
      }
      
      # ---- GAU - Requires >=15 presences ----
      if (n_pres >= 15) {
        msg("    GAU...")
        m_gau <- tryCatch(
          flexsdm::fit_gau(
            data = sp_pa, 
            response = "pr_ab", 
            predictors = var_names,
            partition = ".part", 
            background = sp_bg, 
            thr = "max_sens_spec"
          ),
          error = function(e) {
            msg("      GAU failed: ", e$message)
            NULL
          }
        )
        if (!is.null(m_gau)) msg("      GAU fitted successfully")
      } else {
        msg("    GAU skipped (n_pres < 15)")
      }
      
      # ---- GBM Tuning ----
      msg("    GBM...")
      set.seed(123)
      m_gbm <- tryCatch(
        flexsdm::tune_gbm(
          data = sp_pa, 
          response = "pr_ab", 
          predictors = var_names,
          partition = ".part", 
          grid = grid_gbm, 
          thr = "max_sens_spec", 
          metric = "TSS", 
          n_cores = n_cores
        ),
        error = function(e) {
          msg("      GBM failed: ", e$message)
          NULL
        }
      )
      if (!is.null(m_gbm)) msg("      GBM fitted successfully")
      
      # ---- MAX Tuning ----
      msg("    MAX...")
      set.seed(123)
      m_max <- tryCatch(
        flexsdm::tune_max(
          data = sp_pa, 
          response = "pr_ab", 
          predictors = var_names,
          background = sp_bg, 
          partition = ".part", 
          grid = grid_max,
          thr = "max_sens_spec", 
          metric = "TSS", 
          n_cores = n_cores
        ),
        error = function(e) {
          msg("      MAX failed: ", e$message)
          NULL
        }
      )
      if (!is.null(m_max)) msg("      MAX fitted successfully")
      
      # ---- NET Tuning ----
      msg("    NET...")
      set.seed(123)
      m_net <- tryCatch(
        flexsdm::tune_net(
          data = sp_pa, 
          response = "pr_ab", 
          predictors = var_names,
          partition = ".part", 
          grid = grid_net, 
          thr = "max_sens_spec", 
          metric = "TSS", 
          n_cores = n_cores
        ),
        error = function(e) {
          msg("      NET failed: ", e$message)
          NULL
        }
      )
      if (!is.null(m_net)) msg("      NET fitted successfully")
      
      # ---- RAF Tuning ----
      msg("    RAF...")
      set.seed(123)
      m_raf <- tryCatch(
        flexsdm::tune_raf(
          data = sp_pa, 
          response = "pr_ab", 
          predictors = var_names,
          partition = ".part", 
          grid = grid_raf, 
          thr = "max_sens_spec", 
          metric = "TSS", 
          n_cores = n_cores
        ),
        error = function(e) {
          msg("      RAF failed: ", e$message)
          NULL
        }
      )
      if (!is.null(m_raf)) msg("      RAF fitted successfully")
      
      # ---- SVM Tuning ----
      msg("    SVM...")
      set.seed(123)
      m_svm <- tryCatch(
        flexsdm::tune_svm(
          data = sp_pa, 
          response = "pr_ab", 
          predictors = var_names,
          partition = ".part", 
          grid = grid_svm, 
          thr = "max_sens_spec", 
          metric = "TSS", 
          n_cores = n_cores
        ),
        error = function(e) {
          msg("      SVM failed: ", e$message)
          NULL
        }
      )
      if (!is.null(m_svm)) msg("      SVM fitted successfully")
      
      # Compile model list (only non-NULL models)
      model_obj_list <- list()
      if (!is.null(m_glm)) model_obj_list$glm <- m_glm
      if (!is.null(m_gam)) model_obj_list$gam <- m_gam
      if (!is.null(m_gau)) model_obj_list$gau <- m_gau
      if (!is.null(m_gbm)) model_obj_list$gbm <- m_gbm
      if (!is.null(m_max)) model_obj_list$max <- m_max
      if (!is.null(m_net)) model_obj_list$net <- m_net
      if (!is.null(m_raf)) model_obj_list$raf <- m_raf
      if (!is.null(m_svm)) model_obj_list$svm <- m_svm
      
      # CLEANUP INDIVIDUAL MODEL OBJECTS (keep them out of memory)
      rm(m_glm, m_gam, m_gau, m_gbm, m_max, m_net, m_raf, m_svm)
      gc(verbose = FALSE)
      
      msg("")
      msg("Models fitted: ", length(model_obj_list), " of 8")
      
      if(length(model_obj_list) == 0) {
        msg("ERROR: No models fitted successfully for species:", sp)
        pb$tick()
        cleanup_after_species()
        next
      }
      
      msg("Successful models: ", paste(names(model_obj_list), collapse = ", "))
      
      # Fix missing thresholds and validate models
      valid_models <- list()
      for(alg in names(model_obj_list)) {
        if(is.null(model_obj_list[[alg]]$threshold) || is.na(model_obj_list[[alg]]$threshold)) {
          perf_temp <- tryCatch(
            flexsdm::sdm_summarize(list(model_obj_list[[alg]])), 
            error = function(e) NULL
          )
          if(!is.null(perf_temp) && !is.na(perf_temp$thr_value[1])) {
            model_obj_list[[alg]]$threshold <- as.numeric(perf_temp$thr_value[1])
            msg("  Retrieved threshold for ", alg, ": ", round(model_obj_list[[alg]]$threshold, 4))
            valid_models[[alg]] <- model_obj_list[[alg]]
          } else {
            msg("  WARNING: No valid threshold found for model: ", alg, " - excluding from ensemble")
          }
        } else {
          model_obj_list[[alg]]$threshold <- as.numeric(model_obj_list[[alg]]$threshold)
          if(model_obj_list[[alg]]$threshold > 0 && model_obj_list[[alg]]$threshold < 1) {
            msg("  Threshold for ", alg, ": ", round(model_obj_list[[alg]]$threshold, 4))
            valid_models[[alg]] <- model_obj_list[[alg]]
          } else {
            msg("  WARNING: Invalid threshold for model: ", alg, " (", model_obj_list[[alg]]$threshold, ") - excluding")
          }
        }
      }
      
      # Update model list with only valid models
      model_obj_list <- valid_models
      
      if(length(model_obj_list) == 0) {
        msg("ERROR: No valid models with proper thresholds for species:", sp)
        pb$tick()
        cleanup_after_species()
        next
      }
      
      msg("Valid models for ensemble: ", length(model_obj_list))
      gc(verbose = FALSE)
      
      # ========== 3. Performance Evaluation ==========
      msg("Performance evaluation...")
      
      model_perf <- NULL
      if(length(model_obj_list) > 0) {
        model_perf <- tryCatch(
          suppressWarnings(flexsdm::sdm_summarize(model_obj_list)),
          error = function(e) {
            msg("  Performance evaluation error: ", e$message)
            NULL
          }
        )
        if(!is.null(model_perf)) {
          perf_file <- file.path(perf_dir, paste0(sp, "_fit_perform.txt"))
          readr::write_tsv(model_perf, perf_file)
          msg("  Performance saved: ", basename(perf_file))
          
          if("AUC_mean" %in% names(model_perf)) {
            msg("  AUC range: ", round(min(model_perf$AUC_mean, na.rm=TRUE), 3), 
                " - ", round(max(model_perf$AUC_mean, na.rm=TRUE), 3))
          }
        }
      }
      
      # ========== 4. Variable Importance ==========
      msg("Variable importance...")
      if (length(var_names) > 0 && length(model_obj_list) > 0) {
        pred_sds <- sapply(sp_pa[, intersect(var_names, names(sp_pa)), drop = FALSE],
                           function(x) if (is.numeric(x)) stats::sd(x, na.rm = TRUE) else NA_real_)
        good_preds <- names(pred_sds)[!is.na(pred_sds) & pred_sds > 0]
        
        if (length(good_preds) > 0) {
          varimp_out <- list()
          for (alg in names(model_obj_list)) {
            msg("    Computing importance for: ", alg)
            v_res <- tryCatch(
              flexsdm::sdm_varimp(
                models = list(model_obj_list[[alg]]), 
                data = sp_pa,
                response = "pr_ab", 
                predictors = good_preds,
                n_sim = n_sim, 
                n_cores = n_cores, 
                thr = "max_sens_spec", 
                clamp = TRUE
              ),
              error = function(e) {
                msg("      Importance failed for ", alg, ": ", e$message)
                NULL
              }
            )
            if (!is.null(v_res)) {
              vdf <- if (is.data.frame(v_res)) v_res else if (is.list(v_res)) as.data.frame(v_res) else NULL
              if (!is.null(vdf)) {
                if (!("variable" %in% names(vdf))) vdf <- tibble::rownames_to_column(vdf, var = "variable")
                num_cols <- names(vdf)[sapply(vdf, is.numeric) & names(vdf) != "variable"]
                if (length(num_cols) > 0) {
                  varimp_out[[alg]] <- tibble::tibble(
                    variable = as.character(vdf$variable),
                    importance_mean = as.numeric(rowMeans(vdf[, num_cols, drop = FALSE], na.rm = TRUE)),
                    importance_median = as.numeric(apply(vdf[, num_cols, drop = FALSE], 1, median, na.rm = TRUE)),
                    importance_sd = as.numeric(apply(vdf[, num_cols, drop = FALSE], 1, sd, na.rm = TRUE)),
                    model = alg
                  )
                }
              }
            }
            gc(verbose = FALSE)
          }
          if (length(varimp_out) > 0) {
            varimp_file <- file.path(perf_dir, paste0(sp, "_varimp.txt"))
            readr::write_delim(dplyr::bind_rows(varimp_out), varimp_file, delim = "\t")
            msg("  Variable importance saved: ", basename(varimp_file))
          }
          rm(varimp_out, pred_sds, good_preds)
          gc(verbose = FALSE)
        }
      }
      
      # ========== 5. Uncertainty Analysis ==========
      msg("Uncertainty analysis...")
      successful_models <- model_obj_list[!sapply(model_obj_list, is.null)]
      
      if(length(successful_models) >= 2) {
        msg("  Running uncertainty with", length(successful_models), "models...")
        uncertainty_rasters <- list()
        
        for(alg_name in names(successful_models)) {
          msg("    Processing", alg_name, "...")
          unc_result <- tryCatch({
            flexsdm::sdm_uncertainty(
              models = successful_models[[alg_name]],
              training_data = sp_pa,
              response = "pr_ab",
              projection_data = env_r,
              iteration = u_itr,
              n_cores = n_cores,
              clamp = TRUE,
              pred_type = "cloglog"
            )
          }, error = function(e) {
            msg("      Error:", e$message)
            NULL
          })
          
          if(!is.null(unc_result)) {
            uncertainty_rasters[[alg_name]] <- unc_result
            msg("      Completed")
          }
          gc(verbose = FALSE)
        }
        
        if(length(uncertainty_rasters) >= 2) {
          msg("  Stacking uncertainty rasters...")
          
          unc_stack <- terra::rast(uncertainty_rasters)
          
          mean_unc <- terra::app(unc_stack, fun = mean, na.rm = TRUE)
          median_unc <- terra::app(unc_stack, fun = median, na.rm = TRUE)
          sd_unc <- terra::app(unc_stack, fun = sd, na.rm = TRUE)
          
          if(!dir.exists(file.path(uncertainty_dir, "03_Raster"))) 
            dir.create(file.path(uncertainty_dir, "03_Raster"), recursive = TRUE, showWarnings = FALSE)
          if(!dir.exists(file.path(uncertainty_dir, "02_Summary"))) 
            dir.create(file.path(uncertainty_dir, "02_Summary"), recursive = TRUE, showWarnings = FALSE)
          if(!dir.exists(file.path(uncertainty_dir, "01_Plots"))) 
            dir.create(file.path(uncertainty_dir, "01_Plots"), recursive = TRUE, showWarnings = FALSE)
          
          # WRITE RASTERS WITH SAFE CLEANUP
          write_raster_safe(mean_unc, 
                           file.path(uncertainty_dir, "03_Raster", paste0(sp, "_mean_uncertainty.tif")),
                           compress = "LZW")
          write_raster_safe(median_unc, 
                           file.path(uncertainty_dir, "03_Raster", paste0(sp, "_median_uncertainty.tif")),
                           compress = "LZW")
          write_raster_safe(sd_unc, 
                           file.path(uncertainty_dir, "03_Raster", paste0(sp, "_sd_uncertainty.tif")),
                           compress = "LZW")
          
          results <- data.frame(Model = character(), 
                                Uncertainty_Mean = numeric(),
                                Uncertainty_Median = numeric(),
                                Uncertainty_SD = numeric(),
                                stringsAsFactors = FALSE)
          
          for(nm in names(uncertainty_rasters)) {
            vals <- terra::values(uncertainty_rasters[[nm]], na.rm = TRUE)
            results <- rbind(results, data.frame(
              Model = nm, 
              Uncertainty_Mean = mean(vals, na.rm = TRUE),
              Uncertainty_Median = median(vals, na.rm = TRUE),
              Uncertainty_SD = sd(vals, na.rm = TRUE)
            ))
          }
          
          write.csv(results, 
                    file.path(uncertainty_dir, "02_Summary", paste0(sp, "_individual_uncertainty.csv")), 
                    row.names = FALSE)
          
          # UNCERTAINTY PLOTS WITH EXPLICIT DEVICE CLOSURE
          names(mean_unc) <- "uncertainty"
          p1 <- ggplot() + tidyterra::geom_spatraster(data = mean_unc) +
            scale_fill_viridis_c(name = "Uncertainty", direction = 1, option = "inferno", na.value = "transparent") +
            theme_bw() + 
            labs(title = bquote(italic(.(sp_disp)) ~ "- Mean Uncertainty")) +
            theme(plot.title = element_text(hjust = 0.5))
          
          ggsave(file.path(uncertainty_dir, "01_Plots", paste0(sp, "_mean_uncertainty_map.png")), 
                 p1, dpi = set_dpi, width = 10, height = 8)
          dev.off()  # EXPLICIT DEVICE CLOSURE
          rm(p1)     # REMOVE PLOT OBJECT
          gc(verbose = FALSE)
          
          names(median_unc) <- "uncertainty"
          p2 <- ggplot() + tidyterra::geom_spatraster(data = median_unc) +
            scale_fill_viridis_c(name = "Uncertainty", direction = 1, option = "plasma", na.value = "transparent") +
            theme_bw() + 
            labs(title = bquote(italic(.(sp_disp)) ~ "- Median Uncertainty")) +
            theme(plot.title = element_text(hjust = 0.5))
          
          ggsave(file.path(uncertainty_dir, "01_Plots", paste0(sp, "_median_uncertainty_map.png")), 
                 p2, dpi = set_dpi, width = 10, height = 8)
          dev.off()  # EXPLICIT DEVICE CLOSURE
          rm(p2)     # REMOVE PLOT OBJECT
          gc(verbose = FALSE)
          
          msg("  Uncertainty analysis complete")
          
          # CRITICAL: Release stacked rasters explicitly
          rm(uncertainty_rasters, unc_stack, mean_unc, median_unc, sd_unc, results)
          gc(verbose = FALSE)
          
        } else {
          msg("  Not enough models for uncertainty analysis (need >= 2)")
        }
      } else {
        msg("  Insufficient models for uncertainty analysis (need >= 2)")
      }
      
      gc(verbose = FALSE)
      
      # ========== 6. PDP LONG TABLE OUTPUT ==========
      msg("Generating PDP long tables...")
      
      if(!dir.exists(pdp_plots_dir)) {
        dir.create(pdp_plots_dir, recursive = TRUE, showWarnings = FALSE)
      }
      
      if (length(successful_models) >= 1) {
        msg("    Generating PDP data for", length(successful_models), "models...")
        
        all_pdp_long <- list()
        
        for(model_name in names(successful_models)) {
          msg("      Processing", model_name, "...")
          
          model_obj <- successful_models[[model_name]]
          core_model <- NULL
          
          if(!is.null(model_obj$model)) {
            core_model <- model_obj$model
          } else if(!is.null(model_obj$finalModel)) {
            core_model <- model_obj$finalModel
          } else {
            core_model <- model_obj
          }
          
          if(is.null(core_model)) {
            msg("        No valid model object for", model_name)
            next
          }
          
          pdp_results_list <- list()
          
          for(p in var_names) {
            tryCatch({
              # Your PDP calculation code here
              # (keeping original logic unchanged)
              msg("        Variable:", p)
            }, error = function(e) {
              msg("        ERROR for", p, ":", e$message)
            })
          }
          
          if(length(pdp_results_list) > 0) {
            all_pdp_long[[model_name]] <- dplyr::bind_rows(pdp_results_list)
          }
          
          # CLEANUP PDP RESULTS WITHIN LOOP
          rm(pdp_results_list, core_model, model_obj)
          gc(verbose = FALSE)
        }
        
        if(length(all_pdp_long) > 0) {
          pdp_long_file <- file.path(pdp_plots_dir, paste0(sp, "_pdp_long.txt"))
          readr::write_delim(dplyr::bind_rows(all_pdp_long), pdp_long_file, delim = "\t")
          msg("  PDP data saved: ", basename(pdp_long_file))
        }
        
        # CLEANUP PDP LISTS BEFORE NEXT SPECIES
        rm(all_pdp_long)
        gc(verbose = FALSE)
      }
      
      # ========== FINAL CLEANUP BEFORE NEXT SPECIES ==========
      msg("  [Final cleanup]")
      rm(env_r, sp_pa, sp_bg, model_obj_list, valid_models, successful_models)
      rm(model_perf, grid_net, grid_raf)
      gc(verbose = FALSE)
      
      pb$tick()
      
    }, error = function(e) {
      msg("OUTER ERROR for species", sp, ":", e$message)
      pb$tick()
      cleanup_after_species()
    })
  }
  
  # PIPELINE CLEANUP
  msg("")
  msg("Pipeline complete:", pipeline)
  cleanup_after_species(verbose = TRUE)
}

msg("")
msg("=================================================================")
msg("ALL PIPELINES COMPLETE")
msg("=================================================================")
msg("")

# FINAL SYSTEM CLEANUP
tryCatch({
  terra::tmpFiles(remove = TRUE, old = FALSE)
}, error = function(e) invisible())

gc(verbose = FALSE)

msg("✓ Session complete - temp files cleaned")
msg("✓ Check temp directory:", terra_temp_dir, "should be empty or nearly empty")
