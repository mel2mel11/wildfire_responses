# ============================================================
# 03_google_trends_harmonize_air_quality.r
#
# PURPOSE:
#   Harmonize Google Trends data while allowing incomplete
#   DMAs to be dropped.
#
# SCENARIOS:
#
#   Scenario 1: chunks 1–5
#   Scenario 2: chunks 2–5
#   Scenario 3: chunks 3–5
#
# For each scenario:
#   - identify DMAs missing required chunks
#   - drop those DMAs
#   - harmonize remaining DMAs
#   - save retained/dropped DMA lists
#   - save diagnostics
#
# IMPORTANT:
#   Incomplete DMAs are expected and DO NOT stop the script.
# ============================================================

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)


# 3. LOAD HELPER
source("scripts/01_trend_helper_functions.R")


# 4. OUTPUT DIRECTORY

output_dir <- "trends_harmonized_air_quality"

dir.create(
  output_dir,
  showWarnings = FALSE,
  recursive = TRUE
)


# 5. LOAD DATA
chunks <- load_chunks()

dma_vec <- get_dma_vec()

raw_data <- load_checkpoints()



# 6. INITIAL DATA STATUS

cat("HARMONIZATION INPUT\n")

cat(
  "DMA universe: ",
  length(dma_vec),
  "\n"
)

cat(
  "Chunks available: ",
  paste(
    sort(unique(raw_data$chunk_id)),
    collapse = ", "
  ),
  "\n"
)

cat(
  "DMA × chunk combinations available: ",
  n_distinct(
    raw_data$geo_req,
    raw_data$chunk_id
  ),
  "\n"
)


# 7. RUN SCENARIO 1: CHUNKS 1–5

scenario_1 <- run_harmonization_scenario(
  raw_data = raw_data,
  dma_vec = dma_vec,
  chunk_ids = 1:5,
  scenario_name = "chunks_1_to_5"
)


# 8. RUN SCENARIO 2: CHUNKS 2–5

scenario_2 <- run_harmonization_scenario(
  raw_data = raw_data,
  dma_vec = dma_vec,
  chunk_ids = 2:5,
  scenario_name = "chunks_2_to_5"
)


# 9. RUN SCENARIO 3: CHUNKS 3–5

scenario_3 <- run_harmonization_scenario(
  raw_data = raw_data,
  dma_vec = dma_vec,
  chunk_ids = 3:5,
  scenario_name = "chunks_3_to_5"
)

#  10. DIAGNOSE FAILED TRANSITIONS

failed_transitions <- scenario_3$diagnostics %>%
  filter(!success, reason == "No positive overlap")

cat("\n")
cat("FAILED TRANSITION OVERLAP DIAGNOSTICS\n")

print(
  failed_overlap_diagnostics,
  n = Inf
)

write.csv(
  failed_overlap_diagnostics,
  file.path(
    output_dir,
    "failed_transition_positive_years.csv"
  ),
  row.names = FALSE
)

# 11. COMBINE SCENARIO SUMMARIES

scenario_comparison <- bind_rows(
  scenario_1$summary,
  scenario_2$summary,
  scenario_3$summary
)

# 12. PRINT COMPARISON

cat("HARMONIZATION SCENARIO COMPARISON\n")

print(
  scenario_comparison
)

# SAVE COMPARISON

write.csv(
  scenario_comparison,
  file.path(
    output_dir,
    "harmonization_scenario_comparison.csv"
  ),
  row.names = FALSE
)


# 13. SAVE DMA-LEVEL SUMMARIES

write.csv(
  scenario_1$dma_summary,
  file.path(
    output_dir,
    "dma_summary_chunks_1_to_5.csv"
  ),
  row.names = FALSE
)

write.csv(
  scenario_2$dma_summary,
  file.path(
    output_dir,
    "dma_summary_chunks_2_to_5.csv"
  ),
  row.names = FALSE
)

write.csv(
  scenario_3$dma_summary,
  file.path(
    output_dir,
    "dma_summary_chunks_3_to_5.csv"
  ),
  row.names = FALSE
)


# 14. CREATE DROPPED DMA TABLES

dropped_1 <- scenario_1$dma_summary |>
  filter(
    !harmonized
  )

dropped_2 <- scenario_2$dma_summary |>
  filter(
    !harmonized
  )

dropped_3 <- scenario_3$dma_summary |>
  filter(
    !harmonized
  )


# 15. SAVE DROPPED DMA LISTS

write.csv(
  dropped_1,
  file.path(
    output_dir,
    "dropped_dmas_chunks_1_to_5.csv"
  ),
  row.names = FALSE
)

write.csv(
  dropped_2,
  file.path(
    output_dir,
    "dropped_dmas_chunks_2_to_5.csv"
  ),
  row.names = FALSE
)

write.csv(
  dropped_3,
  file.path(
    output_dir,
    "dropped_dmas_chunks_3_to_5.csv"
  ),
  row.names = FALSE
)


# 17. SAVE HARMONIZED DATA

saveRDS(
  scenario_1$data,
  file.path(
    output_dir,
    "harmonized_chunks_1_to_5.rds"
  )
)

saveRDS(
  scenario_2$data,
  file.path(
    output_dir,
    "harmonized_chunks_2_to_5.rds"
  )
)

saveRDS(
  scenario_3$data,
  file.path(
    output_dir,
    "harmonized_chunks_3_to_5.rds"
  )
)


# 18. SAVE HARMONIZATION DIAGNOSTICS

write.csv(
  scenario_1$diagnostics,
  file.path(
    output_dir,
    "harmonization_diagnostics_chunks_1_to_5.csv"
  ),
  row.names = FALSE
)

write.csv(
  scenario_2$diagnostics,
  file.path(
    output_dir,
    "harmonization_diagnostics_chunks_2_to_5.csv"
  ),
  row.names = FALSE
)

write.csv(
  scenario_3$diagnostics,
  file.path(
    output_dir,
    "harmonization_diagnostics_chunks_3_to_5.csv"
  ),
  row.names = FALSE
)


# 21. PRINT FINAL RECOMMENDATION INFORMATION

cat("FINAL SCENARIO COUNTS\n")


for (i in seq_len(nrow(scenario_comparison))) {
  
  x <- scenario_comparison[i, ]
  
  cat(
    "\n",
    x$scenario,
    "\n",
    "  Total DMAs: ",
    x$total_dmas,
    "\n",
    "  Complete DMAs: ",
    x$complete_data_dmas,
    "\n",
    "  Harmonized DMAs: ",
    x$harmonized_dmas,
    "\n",
    "  Dropped DMAs: ",
    x$dropped_dmas,
    "\n",
    "  Percent harmonized: ",
    round(
      x$percent_harmonized,
      1
    ),
    "%\n",
    sep = ""
  )
}

