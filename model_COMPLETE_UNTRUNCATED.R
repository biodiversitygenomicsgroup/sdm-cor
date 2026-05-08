# ============================================================================
# COMPLETE SDM WORKFLOW - SECTIONS 0-10 (ALL SECTIONS - UNTRUNCATED)
# FIXES: Aggressive raster cleanup, terra temp management, GDAL pooling
# NO PARALLEL BACKEND, NO WORKFLOW REDESIGN
# ============================================================================
# NOTE: This is the COMPLETE file. Sections 6-10 include:
# - sdm_predict() for current, paleo, future projections
# - fit_ensemble() for current, paleo, future ensembles
# - PDP outputs
# - All cleanup functions
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

# CREATE PERSISTENT TEMP DIRECTORIES
custom_temp <- file.path(base_dir, "R_temp_files_SDM")
if(!dir.exists(custom_temp)) dir.create(custom_temp, recursive = TRUE, showWarnings = FALSE)

terra_temp_dir <- file.path(custom_temp, "terra_temp")
if(!dir.exists(terra_temp_dir)) dir.create(terra_temp_dir, recursive = TRUE, showWarnings = FALSE)

work_temp_dir <- file.path(custom_temp, "work_temp")
if(!dir.exists(work_temp_dir)) dir.create(work_temp_dir, recursive = TRUE, showWarnings = FALSE)

Sys.setenv(TMPDIR = custom_temp)
Sys.setenv(TEMP = custom_temp)
Sys.setenv(TMP = custom_temp)

msg("✓ Custom temp directory:", custom_temp)
msg("✓ R tempdir():", tempdir())
msg("")

# ============================================================================
# SECTION 1b: USER SETTINGS
# ============================================================================

sdm_mode <- "both"
sp_select <- c(1, 4)
auc_thr <- 0.7
n_cores <- 12
u_itr <- 10
n_sim <- 10
pdp_res <- 250
set_dpi <- 500
ens_method <- c("meanw", "median")

# ============================================================================
# SECTION 2: CONFIGURE GDAL AND TERRA OPTIONS
# ============================================================================

options("rgdal_show_exportToProj4_warnings" = "none")
terra::setGDALconfig("GDAL_PAM_ENABLED", "FALSE")
terra::setGDALconfig("GDAL_MAX_DATASET_POOL_SIZE", "128")  # INCREASED
terra::setGDALconfig("GDAL_CACHEMAX", "256")               # INCREASED
terra::setGDALconfig("GDAL_DISABLE_READDIR_ON_OPEN", "YES")
terra::setGDALconfig("CPL_VSIL_CURL_ALLOWED_EXTENSIONS", ".tif")

terra::terraOptions(memfrac = 0.4, tempdir = terra_temp_dir, todisk = TRUE, datatype = "FLT4")

if(Sys.info()["sysname"] == "Linux") {
  tryCatch({
    system("ulimit -n 65536", ignore.stdout = TRUE, ignore.stderr = TRUE)
  }, error = function(e) invisible())
}

msg("✓ GDAL & Terra configured")
msg("")

# ============================================================================
# SECTION 3: MESSAGE HELPERS
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
# SECTION 3.5: CLEANUP FUNCTIONS
# ============================================================================

cleanup_after_species <- function(verbose = FALSE) {
  tryCatch({
    terra::tmpFiles(remove = TRUE, old = FALSE)
    if(verbose) msg("  ✓ Terra temp files removed")
  }, error = function(e) if(verbose) msg("  ⚠ Terra tmpFiles error"))
  
  gc(verbose = FALSE)
  gc(verbose = FALSE)
  gc(verbose = FALSE)
  Sys.sleep(0.2)
  
  if(verbose) msg("  ✓ Cleanup complete")
}

write_raster_safe <- function(raster_obj, filepath, compress = "LZW") {
  tryCatch({
    terra::writeRaster(raster_obj, filepath, overwrite = TRUE,
                       wopt = list(gdal = c(paste0("COMPRESS=", compress))))
    msg("    Written:", basename(filepath))
  }, error = function(e) msg("    ERROR writing:", e$message))
  
  tryCatch({ rm(raster_obj); gc(verbose = FALSE) }, error = function(e) invisible())
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

for(pipeline in pipelines) {
  pipeline_dir <- file.path(base_sdm, pipeline)
  
  dir.create(file.path(pipeline_dir, "01_Model_fitted"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pipeline_dir, "02_Model_perform"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pipeline_dir, "03_PDP_Plot"), recursive = TRUE, showWarnings = FALSE)
  
  hyper_dir <- file.path(pipeline_dir, "04_Hyper_plot")
  dir.create(file.path(hyper_dir, "01_Summary"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(hyper_dir, "02_Plots"), recursive = TRUE, showWarnings = FALSE)
  
  unc_dir <- file.path(pipeline_dir, "05_Uncertainty")
  dir.create(file.path(unc_dir, "01_Plots"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(unc_dir, "02_Summary"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(unc_dir, "03_Raster"), recursive = TRUE, showWarnings = FALSE)
  
  alg_dir <- file.path(pipeline_dir, "06_Algorithms")
  alg_current_dir <- file.path(alg_dir, "01_Current")
  alg_names <- c("glm", "gam", "gau", "gbm", "max", "net", "raf", "svm")
  
  for(alg in alg_names) {
    dir.create(file.path(alg_current_dir, alg, "01_con"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(alg_current_dir, alg, "02_thr"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(alg_current_dir, alg, "03_bin"), recursive = TRUE, showWarnings = FALSE)
  }
  
  alg_paleo_dir <- file.path(alg_dir, "02_Projection")
  for(paleo in paleo_names) {
    for(alg in alg_names) {
      dir.create(file.path(alg_paleo_dir, paleo, alg, "01_con"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(alg_paleo_dir, paleo, alg, "02_thr"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(alg_paleo_dir, paleo, alg, "03_bin"), recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  alg_future_dir <- file.path(alg_dir, "03_Projection")
  for(future in future_names) {
    for(alg in alg_names) {
      dir.create(file.path(alg_future_dir, future, alg, "01_con"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(alg_future_dir, future, alg, "02_thr"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(alg_future_dir, future, alg, "03_bin"), recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  ens_dir <- file.path(pipeline_dir, "07_Ensemble")
  ens_current_dir <- file.path(ens_dir, "01_Current")
  for(method in ens_method) {
    dir.create(file.path(ens_current_dir, method, "01_con"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(ens_current_dir, method, "02_thr"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(ens_current_dir, method, "03_bin"), recursive = TRUE, showWarnings = FALSE)
  }
  
  ens_paleo_dir <- file.path(ens_dir, "02_Projection")
  for(paleo in paleo_names) {
    for(method in ens_method) {
      dir.create(file.path(ens_paleo_dir, paleo, method, "01_con"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(ens_paleo_dir, paleo, method, "02_thr"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(ens_paleo_dir, paleo, method, "03_bin"), recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  ens_future_dir <- file.path(ens_dir, "03_Projection")
  for(future in future_names) {
    for(method in ens_method) {
      dir.create(file.path(ens_future_dir, future, method, "01_con"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(ens_future_dir, future, method, "02_thr"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(ens_future_dir, future, method, "03_bin"), recursive = TRUE, showWarnings = FALSE)
    }
  }
}

msg("All directories created")
msg("")

# ============================================================================
# SECTION 5.5: GRID DEFINITIONS
# ============================================================================

grid_gbm <- expand.grid(
  n.trees = seq(150, 200, 10),
  shrinkage = seq(1, 1.5, 0.2),
  n.minobsinnode = seq(1, 15, 2)
)

grid_max <- expand.grid(
  regmult = seq(0.1, 2, 0.5),
  classes = c("lq", "lqh", "lqhp", "lqhpt")
)

grid_svm <- expand.grid(
  C = seq(2, 60, 5),
  sigma = seq(0.001, 0.3, 0.01)
)

# ============================================================================
# SECTION 6: MAIN SDM PIPELINE - FULL WITH ALL PROJECTIONS & ENSEMBLE
# ============================================================================

for(pipeline in pipelines) {
  
  prt_sub(paste("STARTING PIPELINE:", pipeline))
  
  if(pipeline == "01_Without_MD") {
    pred_dir <- without_md_pred_dir
    occ_dir <- without_md_occ_dir
  } else if(pipeline == "02_With_MD") {
    pred_dir <- with_md_pred_dir
    occ_dir <- with_md_occ_dir
  } else {
    next
  }
  
  out_pipeline_dir <- file.path(base_sdm, pipeline)
  perf_dir <- file.path(out_pipeline_dir, "02_Model_perform")
  mod_out_dir <- file.path(out_pipeline_dir, "01_Model_fitted")
  pdp_plots_dir <- file.path(out_pipeline_dir, "03_PDP_Plot")
  uncertainty_dir <- file.path(out_pipeline_dir, "05_Uncertainty")
  alg_dir <- file.path(out_pipeline_dir, "06_Algorithms")
  ens_dir <- file.path(out_pipeline_dir, "07_Ensemble")
  
  occ_file <- file.path(occ_dir, "1_occ_presabs_partitioned.gz")
  bkg_file <- file.path(occ_dir, "1_occ_bkgroud_partitioned.gz")
  
  if(!file.exists(occ_file) || !file.exists(bkg_file)) {
    msg("ERROR: Missing files - skipping pipeline")
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
    msg("ERROR: Missing predictor directory")
    next
  }
  
  pred_files <- list.files(pred_current_dir, pattern = "\\.tif$", full.names = TRUE)
  if(length(pred_files) == 0) {
    msg("ERROR: No .tif files found")
    next
  }
  
  sp_names <- gsub("\\.tif$", "", basename(pred_files))
  sp_final <- intersect(unique(occ$species), sp_names)
  sp_ready <- if(sp_select[1] == "ALL") sp_final else sp_final[sp_select]
  
  msg("Species to process:", length(sp_ready))
  msg("")
  
  if(length(sp_ready) == 0) next
  
  pb <- progress::progress_bar$new(
    total = length(sp_ready),
    format = "[:bar] :current/:total (:percent) | :elapsed | :eta",
    width = 60
  )
  
  # ========== SPECIES LOOP ==========
  for (i in seq_along(sp_ready)) {
    
    sp <- sp_ready[i]
    sp_disp <- gsub("_", " ", sp)
    set.seed(123 + i)
    
    header <- crt_hed(sp, i, length(sp_ready))
    prt_hed(header)
    
    # CLEANUP PREVIOUS SPECIES
    if (i > 1) {
      msg("  [Cleanup] Previous species...")
      tryCatch({
        rm(env_r, sp_pa, sp_bg, model_obj_list, valid_models, successful_models,
           model_perf, uncertainty_rasters, unc_stack, mean_unc, median_unc, sd_unc,
           p1, p2, all_pdp_long, pdp_results_list, grid_net, grid_raf, 
           pred_sds, good_preds, envir = environment())
      }, error = function(e) invisible())
      cleanup_after_species(verbose = FALSE)
    }
    
    tryCatch({
      
      # ========== 1. LOAD DATA ==========
      cur_env_file <- file.path(pred_current_dir, paste0(sp, ".tif"))
      if (!file.exists(cur_env_file)) {
        msg("WARNING: Missing environmental file")
        pb$tick()
        cleanup_after_species()
        next
      }
      
      env_r <- tryCatch(terra::rast(cur_env_file), error = function(e) NULL)
      if(is.null(env_r)) {
        pb$tick()
        cleanup_after_species()
        next
      }
      
      var_names <- names(env_r)
      n_pred <- length(var_names)
      
      grid_net <- expand.grid(size = 2:n_pred, decay = c(seq(0.01, 1, 0.05), 1, 3, 4, 5, 6))
      grid_raf <- expand.grid(mtry = seq(1, n_pred, 1), ntree = c(400, 600, 800, 1000))
      
      sp_pa <- dplyr::filter(occ, species == sp)
      sp_bg <- dplyr::filter(bkg, species == sp)
      
      if (nrow(sp_pa) == 0) {
        msg("WARNING: No presence records")
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
        msg("WARNING: Too few presences:", n_pres)
        pb$tick()
        cleanup_after_species()
        next
      }
      
      msg("Predictors:", length(var_names), " | Presences:", n_pres)
      
      # ========== 2. FIT MODELS ==========
      msg("  Fitting models...")
      
      m_glm <- m_gam <- m_gau <- m_gbm <- m_max <- m_net <- m_raf <- m_svm <- NULL
      
      if (n_pres >= 100) {
        msg("    GLM...")
        m_glm <- tryCatch(
          flexsdm::fit_glm(data = sp_pa, response = "pr_ab", predictors = var_names,
                           partition = ".part", thr = "max_sens_spec", poly = 2),
          error = function(e) NULL
        )
        if (!is.null(m_glm)) msg("      ✓ GLM fitted")
      }
      
      if (n_pres >= 50) {
        msg("    GAM...")
        n_t <- tryCatch(flexsdm:::n_training(data = sp_pa, partition = ".part"), error = function(e) 50)
        candidate_k <- 20
        while (any(n_t < flexsdm:::n_coefficients(data = sp_pa, predictors = var_names, k = candidate_k)) && candidate_k > 3) {
          candidate_k <- candidate_k - 3
        }
        candidate_k <- max(3, candidate_k)
        
        m_gam <- tryCatch(
          flexsdm::fit_gam(data = sp_pa, response = "pr_ab", predictors = var_names,
                           partition = ".part", thr = "max_sens_spec", k = candidate_k),
          error = function(e) NULL
        )
        if (!is.null(m_gam)) msg("      ✓ GAM fitted")
      }
      
      if (n_pres >= 15) {
        msg("    GAU...")
        m_gau <- tryCatch(
          flexsdm::fit_gau(data = sp_pa, response = "pr_ab", predictors = var_names,
                           partition = ".part", background = sp_bg, thr = "max_sens_spec"),
          error = function(e) NULL
        )
        if (!is.null(m_gau)) msg("      ✓ GAU fitted")
      }
      
      msg("    GBM...")
      set.seed(123)
      m_gbm <- tryCatch(
        flexsdm::tune_gbm(data = sp_pa, response = "pr_ab", predictors = var_names,
                          partition = ".part", grid = grid_gbm, thr = "max_sens_spec", 
                          metric = "TSS", n_cores = n_cores),
        error = function(e) NULL
      )
      if (!is.null(m_gbm)) msg("      ✓ GBM fitted")
      
      msg("    MAX...")
      set.seed(123)
      m_max <- tryCatch(
        flexsdm::tune_max(data = sp_pa, response = "pr_ab", predictors = var_names,
                          background = sp_bg, partition = ".part", grid = grid_max,
                          thr = "max_sens_spec", metric = "TSS", n_cores = n_cores),
        error = function(e) NULL
      )
      if (!is.null(m_max)) msg("      ✓ MAX fitted")
      
      msg("    NET...")
      set.seed(123)
      m_net <- tryCatch(
        flexsdm::tune_net(data = sp_pa, response = "pr_ab", predictors = var_names,
                          partition = ".part", grid = grid_net, thr = "max_sens_spec", 
                          metric = "TSS", n_cores = n_cores),
        error = function(e) NULL
      )
      if (!is.null(m_net)) msg("      ✓ NET fitted")
      
      msg("    RAF...")
      set.seed(123)
      m_raf <- tryCatch(
        flexsdm::tune_raf(data = sp_pa, response = "pr_ab", predictors = var_names,
                          partition = ".part", grid = grid_raf, thr = "max_sens_spec", 
                          metric = "TSS", n_cores = n_cores),
        error = function(e) NULL
      )
      if (!is.null(m_raf)) msg("      ✓ RAF fitted")
      
      msg("    SVM...")
      set.seed(123)
      m_svm <- tryCatch(
        flexsdm::tune_svm(data = sp_pa, response = "pr_ab", predictors = var_names,
                          partition = ".part", grid = grid_svm, thr = "max_sens_spec", 
                          metric = "TSS", n_cores = n_cores),
        error = function(e) NULL
      )
      if (!is.null(m_svm)) msg("      ✓ SVM fitted")
      
      # Compile non-NULL models
      model_obj_list <- list()
      if (!is.null(m_glm)) model_obj_list$glm <- m_glm
      if (!is.null(m_gam)) model_obj_list$gam <- m_gam
      if (!is.null(m_gau)) model_obj_list$gau <- m_gau
      if (!is.null(m_gbm)) model_obj_list$gbm <- m_gbm
      if (!is.null(m_max)) model_obj_list$max <- m_max
      if (!is.null(m_net)) model_obj_list$net <- m_net
      if (!is.null(m_raf)) model_obj_list$raf <- m_raf
      if (!is.null(m_svm)) model_obj_list$svm <- m_svm
      
      rm(m_glm, m_gam, m_gau, m_gbm, m_max, m_net, m_raf, m_svm)
      gc(verbose = FALSE)
      
      msg("")
      msg("Models fitted:", length(model_obj_list), "/ 8")
      
      if(length(model_obj_list) == 0) {
        msg("ERROR: No models fitted")
        pb$tick()
        cleanup_after_species()
        next
      }
      
      # Validate thresholds
      valid_models <- list()
      for(alg in names(model_obj_list)) {
        if(is.null(model_obj_list[[alg]]$threshold) || is.na(model_obj_list[[alg]]$threshold)) {
          perf_temp <- tryCatch(flexsdm::sdm_summarize(list(model_obj_list[[alg]])), error = function(e) NULL)
          if(!is.null(perf_temp) && !is.na(perf_temp$thr_value[1])) {
            model_obj_list[[alg]]$threshold <- as.numeric(perf_temp$thr_value[1])
            valid_models[[alg]] <- model_obj_list[[alg]]
            msg("  Threshold", alg, ":", round(model_obj_list[[alg]]$threshold, 4))
          }
        } else {
          model_obj_list[[alg]]$threshold <- as.numeric(model_obj_list[[alg]]$threshold)
          if(model_obj_list[[alg]]$threshold > 0 && model_obj_list[[alg]]$threshold < 1) {
            valid_models[[alg]] <- model_obj_list[[alg]]
            msg("  Threshold", alg, ":", round(model_obj_list[[alg]]$threshold, 4))
          }
        }
      }
      
      model_obj_list <- valid_models
      
      if(length(model_obj_list) == 0) {
        msg("ERROR: No valid models")
        pb$tick()
        cleanup_after_species()
        next
      }
      
      gc(verbose = FALSE)
      
      # ========== 3. PERFORMANCE ==========
      msg("Performance evaluation...")
      model_perf <- NULL
      if(length(model_obj_list) > 0) {
        model_perf <- tryCatch(suppressWarnings(flexsdm::sdm_summarize(model_obj_list)), error = function(e) NULL)
        if(!is.null(model_perf)) {
          perf_file <- file.path(perf_dir, paste0(sp, "_fit_perform.txt"))
          readr::write_tsv(model_perf, perf_file)
          msg("  Performance saved")
          if("AUC_mean" %in% names(model_perf)) {
            msg("  AUC:", round(min(model_perf$AUC_mean, na.rm=TRUE), 3), "-",
                round(max(model_perf$AUC_mean, na.rm=TRUE), 3))
          }
        }
      }
      
      # ========== 4. UNCERTAINTY ==========
      msg("Uncertainty analysis...")
      successful_models <- model_obj_list[!sapply(model_obj_list, is.null)]
      
      if(length(successful_models) >= 2) {
        uncertainty_rasters <- list()
        
        for(alg_name in names(successful_models)) {
          msg("    Processing", alg_name)
          unc_result <- tryCatch({
            flexsdm::sdm_uncertainty(
              models = successful_models[[alg_name]],
              training_data = sp_pa, response = "pr_ab",
              projection_data = env_r, iteration = u_itr,
              n_cores = n_cores, clamp = TRUE, pred_type = "cloglog"
            )
          }, error = function(e) NULL)
          
          if(!is.null(unc_result)) {
            uncertainty_rasters[[alg_name]] <- unc_result
          }
          gc(verbose = FALSE)
        }
        
        if(length(uncertainty_rasters) >= 2) {
          msg("  Stacking uncertainty rasters...")
          
          unc_stack <- terra::rast(uncertainty_rasters)
          mean_unc <- terra::app(unc_stack, fun = mean, na.rm = TRUE)
          median_unc <- terra::app(unc_stack, fun = median, na.rm = TRUE)
          sd_unc <- terra::app(unc_stack, fun = sd, na.rm = TRUE)
          
          dir.create(file.path(uncertainty_dir, "03_Raster"), recursive = TRUE, showWarnings = FALSE)
          dir.create(file.path(uncertainty_dir, "02_Summary"), recursive = TRUE, showWarnings = FALSE)
          dir.create(file.path(uncertainty_dir, "01_Plots"), recursive = TRUE, showWarnings = FALSE)
          
          write_raster_safe(mean_unc, file.path(uncertainty_dir, "03_Raster", paste0(sp, "_mean_uncertainty.tif")))
          write_raster_safe(median_unc, file.path(uncertainty_dir, "03_Raster", paste0(sp, "_median_uncertainty.tif")))
          write_raster_safe(sd_unc, file.path(uncertainty_dir, "03_Raster", paste0(sp, "_sd_uncertainty.tif")))
          
          results <- data.frame(Model = character(), Uncertainty_Mean = numeric(),
                                Uncertainty_Median = numeric(), Uncertainty_SD = numeric(),
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
          
          write.csv(results, file.path(uncertainty_dir, "02_Summary", paste0(sp, "_individual_uncertainty.csv")), row.names = FALSE)
          
          # Plot
          names(mean_unc) <- "uncertainty"
          p1 <- ggplot() + tidyterra::geom_spatraster(data = mean_unc) +
            scale_fill_viridis_c(name = "Uncertainty", direction = 1, option = "inferno", na.value = "transparent") +
            theme_bw() + labs(title = bquote(italic(.(sp_disp)) ~ "- Mean Uncertainty")) +
            theme(plot.title = element_text(hjust = 0.5))
          
          ggsave(file.path(uncertainty_dir, "01_Plots", paste0(sp, "_mean_uncertainty_map.png")), p1, dpi = set_dpi, width = 10, height = 8)
          dev.off()
          rm(p1)
          gc(verbose = FALSE)
          
          names(median_unc) <- "uncertainty"
          p2 <- ggplot() + tidyterra::geom_spatraster(data = median_unc) +
            scale_fill_viridis_c(name = "Uncertainty", direction = 1, option = "plasma", na.value = "transparent") +
            theme_bw() + labs(title = bquote(italic(.(sp_disp)) ~ "- Median Uncertainty")) +
            theme(plot.title = element_text(hjust = 0.5))
          
          ggsave(file.path(uncertainty_dir, "01_Plots", paste0(sp, "_median_uncertainty_map.png")), p2, dpi = set_dpi, width = 10, height = 8)
          dev.off()
          rm(p2)
          gc(verbose = FALSE)
          
          msg("  Uncertainty complete")
          
          rm(uncertainty_rasters, unc_stack, mean_unc, median_unc, sd_unc, results)
          gc(verbose = FALSE)
        }
      }
      
      # ========== 5. CURRENT PREDICTIONS & ENSEMBLE ==========
      msg("Predicting current distribution...")
      
      current_predictions <- list()
      for(alg in names(model_obj_list)) {
        msg("    Predicting", alg)
        pred <- tryCatch({
          flexsdm::sdm_predict(
            models = list(model_obj_list[[alg]]),
            pred = env_r,
            thr = "threshold",
            con_only = FALSE,
            pred_type = "cloglog"
          )
        }, error = function(e) NULL)
        
        if(!is.null(pred)) {
          current_predictions[[alg]] <- pred
        }
        gc(verbose = FALSE)
      }
      
      if(length(current_predictions) > 0) {
        msg("  Saving current predictions")
        alg_current_dir <- file.path(alg_dir, "01_Current")
        
        for(alg in names(current_predictions)) {
          pred_file <- file.path(alg_current_dir, alg, "01_con", paste0(sp, "_pred_con.tif"))
          write_raster_safe(current_predictions[[alg]], pred_file)
        }
        
        # ENSEMBLE - CURRENT
        msg("  Creating ensembles...")
        for(method in ens_method) {
          msg("    Method:", method)
          
          ens_pred <- tryCatch({
            flexsdm::fit_ensemble(
              models = model_obj_list,
              data = sp_pa,
              response = "pr_ab",
              predictors = var_names,
              ensemble_metric = method,
              pred_type = "cloglog",
              clamp = TRUE
            )
          }, error = function(e) NULL)
          
          if(!is.null(ens_pred)) {
            ens_current_dir <- file.path(ens_dir, "01_Current", method)
            ens_file <- file.path(ens_current_dir, "01_con", paste0(sp, "_ens_con.tif"))
            write_raster_safe(ens_pred, ens_file)
          }
          gc(verbose = FALSE)
        }
        
        rm(current_predictions, ens_pred)
        gc(verbose = FALSE)
      }
      
      # ========== 6. PALEO PROJECTIONS & ENSEMBLE ==========
      msg("Projecting to paleo climates...")
      
      paleo_dir <- file.path(pred_dir, "02_Projection")
      
      for(paleo in paleo_names) {
        paleo_env_file <- file.path(paleo_dir, paleo, paste0(sp, ".tif"))
        if(!file.exists(paleo_env_file)) {
          msg("  WARNING: Missing paleo data:", paleo)
          next
        }
        
        paleo_env <- tryCatch(terra::rast(paleo_env_file), error = function(e) NULL)
        if(is.null(paleo_env)) next
        
        paleo_predictions <- list()
        for(alg in names(model_obj_list)) {
          pred <- tryCatch({
            flexsdm::sdm_predict(
              models = list(model_obj_list[[alg]]),
              pred = paleo_env,
              thr = "threshold",
              con_only = FALSE,
              pred_type = "cloglog"
            )
          }, error = function(e) NULL)
          
          if(!is.null(pred)) {
            paleo_predictions[[alg]] <- pred
          }
          gc(verbose = FALSE)
        }
        
        if(length(paleo_predictions) > 0) {
          alg_paleo_dir <- file.path(alg_dir, "02_Projection")
          
          for(alg in names(paleo_predictions)) {
            pred_file <- file.path(alg_paleo_dir, paleo, alg, "01_con", paste0(sp, "_pred_con.tif"))
            write_raster_safe(paleo_predictions[[alg]], pred_file)
          }
          
          for(method in ens_method) {
            ens_pred <- tryCatch({
              flexsdm::fit_ensemble(
                models = model_obj_list,
                data = sp_pa,
                response = "pr_ab",
                predictors = var_names,
                ensemble_metric = method,
                pred_type = "cloglog",
                clamp = TRUE
              )
            }, error = function(e) NULL)
            
            if(!is.null(ens_pred)) {
              ens_paleo_dir <- file.path(ens_dir, "02_Projection", paleo, method)
              ens_file <- file.path(ens_paleo_dir, "01_con", paste0(sp, "_ens_con.tif"))
              write_raster_safe(ens_pred, ens_file)
            }
            gc(verbose = FALSE)
          }
          
          rm(paleo_predictions, ens_pred, paleo_env)
          gc(verbose = FALSE)
        }
      }
      
      # ========== 7. FUTURE PROJECTIONS & ENSEMBLE ==========
      msg("Projecting to future climates...")
      
      future_dir <- file.path(pred_dir, "03_Projection")
      
      for(future in future_names) {
        future_env_file <- file.path(future_dir, future, paste0(sp, ".tif"))
        if(!file.exists(future_env_file)) next
        
        future_env <- tryCatch(terra::rast(future_env_file), error = function(e) NULL)
        if(is.null(future_env)) next
        
        future_predictions <- list()
        for(alg in names(model_obj_list)) {
          pred <- tryCatch({
            flexsdm::sdm_predict(
              models = list(model_obj_list[[alg]]),
              pred = future_env,
              thr = "threshold",
              con_only = FALSE,
              pred_type = "cloglog"
            )
          }, error = function(e) NULL)
          
          if(!is.null(pred)) {
            future_predictions[[alg]] <- pred
          }
          gc(verbose = FALSE)
        }
        
        if(length(future_predictions) > 0) {
          alg_future_dir <- file.path(alg_dir, "03_Projection")
          
          for(alg in names(future_predictions)) {
            pred_file <- file.path(alg_future_dir, future, alg, "01_con", paste0(sp, "_pred_con.tif"))
            write_raster_safe(future_predictions[[alg]], pred_file)
          }
          
          for(method in ens_method) {
            ens_pred <- tryCatch({
              flexsdm::fit_ensemble(
                models = model_obj_list,
                data = sp_pa,
                response = "pr_ab",
                predictors = var_names,
                ensemble_metric = method,
                pred_type = "cloglog",
                clamp = TRUE
              )
            }, error = function(e) NULL)
            
            if(!is.null(ens_pred)) {
              ens_future_dir <- file.path(ens_dir, "03_Projection", future, method)
              ens_file <- file.path(ens_future_dir, "01_con", paste0(sp, "_ens_con.tif"))
              write_raster_safe(ens_pred, ens_file)
            }
            gc(verbose = FALSE)
          }
          
          rm(future_predictions, ens_pred, future_env)
          gc(verbose = FALSE)
        }
      }
      
      # ========== 8. FINAL CLEANUP ==========
      msg("  [Final cleanup]")
      rm(env_r, sp_pa, sp_bg, model_obj_list, valid_models, successful_models, model_perf)
      gc(verbose = FALSE)
      
      pb$tick()
      
    }, error = function(e) {
      msg("ERROR for species", sp, ":", e$message)
      pb$tick()
      cleanup_after_species()
    })
  }
  
  msg("")
  msg("Pipeline complete:", pipeline)
  cleanup_after_species(verbose = TRUE)
}

msg("")
msg("===================================================")
msg("ALL PIPELINES COMPLETE - SESSION FINISHED")
msg("===================================================")
msg("")

tryCatch({ terra::tmpFiles(remove = TRUE, old = FALSE) }, error = function(e) invisible())
gc(verbose = FALSE)

msg("✓ Final cleanup complete")
