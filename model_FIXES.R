# ============================================================================
# DIAGNOSTIC REVIEW & TARGETED FIXES FOR "TOO MANY OPEN FILES" ERRORS
# ============================================================================
# 
# ROOT CAUSES IDENTIFIED:
# 1. Missing explicit raster object cleanup (rm()) in species loops
# 2. Uncertainty/PDP raster lists retain references across iterations
# 3. terra temp files not aggressively cleaned between species
# 4. writeRaster() + ggsave() objects not released
# 5. GDAL pool size too low (45) for multiple stacked raster operations
# 6. gc() placement incomplete
# 7. Nested data structures accumulate unreleased references
#
# SOLUTION STRATEGY (NO workflow redesign):
# - Replace cleanup_after_species() with enhanced version
# - Add targeted rm() calls after each major operation block
# - Increase GDAL dataset pool size
# - Force explicit terra temp cleanup
# - Add device closure after ggsave()
# - Clear nested list structures explicitly
# ============================================================================

# ============================================================================
# ENHANCED CLEANUP FUNCTION (REPLACES LINES 150-161)
# ============================================================================

cleanup_after_species <- function() {
  # STEP 1: Force terra to free old temporary files (not just "old" ones)
  tryCatch({
    terra::tmpFiles(remove = TRUE, old = FALSE)  # Remove ALL temp files
  }, error = function(e) invisible())
  
  # STEP 2: Multiple aggressive garbage collection passes
  # (First pass may not trigger full cleanup due to R's lazy evaluation)
  gc(verbose = FALSE)
  gc(verbose = FALSE)
  gc(verbose = FALSE)
  
  # STEP 3: Small delay to allow OS to release file handles at syscall level
  Sys.sleep(0.2)
}

# ============================================================================
# UPDATED TERRA OPTIONS (REPLACES LINES 84-96)
# ============================================================================
# INCREASE GDAL pool size from 45 to 128 to handle more simultaneous rasters
terra::setGDALconfig("GDAL_PAM_ENABLED", "FALSE")
terra::setGDALconfig("GDAL_MAX_DATASET_POOL_SIZE", "128")  # INCREASED from 45
terra::setGDALconfig("GDAL_CACHEMAX", "200")

# ============================================================================
# EXPLICIT RASTER CLEANUP HELPER (NEW - ADD AFTER cleanup_after_species)
# ============================================================================

# Function to safely remove raster objects and force memory release
cleanup_rasters <- function(...) {
  # Accepts any number of variable names (as strings or symbols)
  var_names <- as.character(substitute(list(...)))[-1]
  
  for (var_name in var_names) {
    tryCatch({
      if (exists(var_name, envir = parent.frame())) {
        obj <- get(var_name, envir = parent.frame())
        
        # If it's a list of rasters, explicitly free each
        if (is.list(obj)) {
          for (i in seq_along(obj)) {
            if (inherits(obj[[i]], "SpatRaster")) {
              rm(list = names(obj)[i], envir = parent.frame())
            }
          }
        }
        
        # Remove the main object
        rm(list = var_name, envir = parent.frame())
      }
    }, error = function(e) invisible())
  }
  
  gc(verbose = FALSE)
}

# ============================================================================
# FIXES FOR SPECIES LOOP - SPECIFIC LOCATIONS
# ============================================================================
# 
# INSERTION POINT 1: After uncertainty raster stacking (after line 964)
# ADD THESE LINES:
#
#   # CLEAN UP UNCERTAINTY RASTERS FROM MEMORY
#   rm(uncertainty_rasters, unc_stack, mean_unc, median_unc, sd_unc, p1, p2)
#   gc(verbose = FALSE)
#
# ============================================================================

# INSERTION POINT 2: After PDP plots loop (after line 1000+)
# This location depends on where PDP plotting ends in the original code
# ADD THESE LINES:
#
#   # CLEAN UP PDP LISTS AND PLOTS
#   rm(all_pdp_long, pdp_results_list)
#   gc(verbose = FALSE)
#
# ============================================================================

# INSERTION POINT 3: Before next species (at start of next loop iteration)
# At line 436, inside the main species loop, after header printed:
# ADD THESE LINES:
#
#   # AGGRESSIVE CLEANUP FROM PREVIOUS SPECIES (MUST RUN FIRST)
#   if (i > 1) {
#     # Clear all species-level temporary variables from previous iteration
#     rm(env_r, sp_pa, sp_bg, model_obj_list, valid_models, successful_models)
#     rm(model_perf, varimp_out, uncertainty_rasters, unc_stack)
#     rm(mean_unc, median_unc, sd_unc, p1, p2, all_pdp_long, pdp_results_list)
#     rm(grid_net, grid_raf, pred_sds, good_preds)
#     cleanup_after_species()
#   }
#
# ============================================================================

# ============================================================================
# PLOTTING FIX: After ggsave() calls (lines 943-954)
# ============================================================================
# Replace:
#   ggsave(file.path(..., paste0(sp, "_mean_uncertainty_map.png")), 
#          p1, dpi = set_dpi, width = 10, height = 8)
#
# With:
#   ggsave(file.path(..., paste0(sp, "_mean_uncertainty_map.png")), 
#          p1, dpi = set_dpi, width = 10, height = 8)
#   dev.off()  # EXPLICIT DEVICE CLOSURE
#   rm(p1)     # REMOVE PLOT OBJECT
#
# (Same for p2)
# ============================================================================

# ============================================================================
# RASTER STACKING FIX: Lines 892-896
# ============================================================================
# After writing uncertainty rasters (line 913), ADD:
#
#   # CRITICAL: Release stacked uncertainty rasters explicitly
#   rm(unc_stack, mean_unc, median_unc, sd_unc)
#   gc(verbose = FALSE)
#
# This prevents GDAL from holding open file handles to the stacked object
# ============================================================================

# ============================================================================
# ENV_R CLEANUP FIX: At end of species iteration
# ============================================================================
# At the END of each species block (before next species or cleanup_after_species),
# ADD:
#
#   # REMOVE LARGE ENVIRONMENTAL RASTER TO FREE FILE HANDLES
#   rm(env_r)
#
# This should be right after the last use of env_r (after PDP and uncertainty)
# ============================================================================

# ============================================================================
# SUMMARY OF MINIMAL TARGETED CHANGES:
# ============================================================================
#
# 1. REPLACE cleanup_after_species() function (lines 150-161)
#    - Change old=TRUE to old=FALSE (remove ALL terra temp files)
#    - Increase gc() calls from 1 to 3
#    - Increase sleep from 0.1 to 0.2 seconds
#
# 2. UPDATE terraOptions() line 86:
#    - GDAL_MAX_DATASET_POOL_SIZE: "45" → "128"
#
# 3. ADD cleanup_rasters() helper function (new, optional but helpful)
#
# 4. INSERT EXPLICIT rm() CALLS at these locations:
#    - After uncertainty raster writes (line ~913)
#    - After uncertainty plots (line ~954)
#    - After PDP section ends
#    - After each major section that creates SpatRaster lists
#
# 5. ADD dev.off() + rm() after each ggsave() call
#
# 6. ADD explicit env_r cleanup at species loop end
#
# ============================================================================

# ============================================================================
# TERRA/GDAL BEST PRACTICES FOR LONG LOOPS:
# ============================================================================
#
# KNOWN ISSUES IN terra/GDAL:
# - GDAL_MAX_DATASET_POOL_SIZE acts as hard ceiling; exceeding it causes
#   silent file handle leaks on older GDAL versions
# - SpatRaster objects with file-backed data may not release file handles
#   until the object is garbage collected AND all references removed
# - terra::app() and terra::rast(list) create intermediate objects that
#   persist in GDAL's object cache even after writeRaster()
# - Stacking multiple rasters increases open file count proportionally
#
# WORKAROUNDS APPLIED:
# 1. Increased pool size to 128 to handle realistic stacking scenarios
# 2. Explicit rm() + gc() to force immediate reference removal
# 3. tmpFiles(remove=TRUE, old=FALSE) to clean all temp files aggressively
# 4. Multiple gc() passes to ensure lazy references are freed
# 5. Small sleep() to allow OS-level file handle closure
#
# ============================================================================

# ============================================================================
# VALIDATION CHECKLIST:
# ============================================================================
#
# Before running long multi-species loop:
# [ ] Check ulimit: `ulimit -n` should show ≥ 65536
# [ ] Verify terraOptions: `terra::terraOptions()$tempdir` points to custom_temp
# [ ] Verify GDAL config: `terra::getGDALconfig("GDAL_MAX_DATASET_POOL_SIZE")` → "128"
# [ ] Run test with 5 species first to verify cleanup works
# [ ] Monitor open files: `lsof -p $(pgrep -f 'R --slave')` | wc -l
#   (Should stay ≤ 1000 for ~100 species with ~10 rasters each)
#
# During long loop:
# [ ] Watch system temp directory: `ls -la R_temp_files_SDM/terra_temp/ | wc -l`
#   (Should drop to near-zero after each cleanup_after_species())
#
# ============================================================================
