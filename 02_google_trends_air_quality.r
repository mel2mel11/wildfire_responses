# 02_google_trends_air_quality.r
#
#   1. Load existing checkpoints
#   2. Identify missing DMA × chunk combinations
#   3. Attempt to repair missing requests
#   4. Record every repair attempt
#   5. Create DMA-level completeness/exclusion table
#
# IMPORTANT:
#   Incomplete DMAs are EXPECTED.
#   This script does NOT require all 210 DMAs to be complete.

rm(list = ls())

library(gtrendsR)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)


# 1. load helper functions and settings
source("scripts/01_trend_helper_functions.R")

chunks <- load_chunks()
dma_vec <- get_dma_vec()
raw_existing <- load_checkpoints()


# 2. initial status
cat("INITIAL GOOGLE TRENDS STATUS\n")

cat(
  "Total DMA universe: ",
  length(dma_vec),
  "\n"
)

cat(
  "Total chunks: ",
  nrow(chunks),
  "\n"
)

cat(
  "Possible DMA × chunk combinations: ",
  length(dma_vec) * nrow(chunks),
  "\n"
)

cat(
  "Existing checkpoints: ",
  n_distinct(
    raw_existing$geo_req,
    raw_existing$chunk_id
  ),
  "\n"
)


# 3. build initial coverage
coverage <- build_coverage(
  dma_vec = dma_vec,
  chunks = chunks,
  raw_existing = raw_existing
)


cat(
  "Missing DMA × chunk combinations: ",
  nrow(coverage$missing),
  "\n"
)


# 5. REPAIR MISSING REQUESTS


if (nrow(coverage$missing) > 0) {
  
  cat("\n")
  cat("MISSING REQUESTS:\n")
  
  print(coverage$missing)
  
  repair_log <- repair_missing_checkpoints(
    missing_requests = coverage$missing,
    keyword = KEYWORD
  )
  
} else {
  
  cat("\n")
  cat("NO MISSING REQUESTS.\n")
  
  repair_log <- tibble()
}


# 7. reload all checkpoints
raw_final <- load_checkpoints(KEYWORD)


# 8. final coverage
coverage_final <- build_coverage(
  dma_vec = dma_vec,
  chunks = chunks,
  raw_existing = raw_final
)


cat("FINAL EXTRACTION STATUS\n")

cat(
  "Total possible combinations: ",
  length(dma_vec) * nrow(chunks),
  "\n"
)

cat(
  "Successful combinations: ",
  nrow(coverage_final$coverage) -
    nrow(coverage_final$missing),
  "\n"
)

cat(
  "Still-missing combinations: ",
  nrow(coverage_final$missing),
  "\n"
)


# 9. final missing requests
if (nrow(coverage_final$missing) > 0) {
  
  cat("\n")
  cat("REQUESTS THAT REMAIN MISSING:\n")
  
  print(
    coverage_final$missing
  )
  
} else {
  
  cat("\n")
  cat(
    "All requested DMA × chunk combinations are available.\n"
  )
}


# 10. DMA COMPLETENESS

dma_exclusion <- build_dma_exclusion_table(
  dma_vec = dma_vec,
  chunks = chunks,
  raw_existing = raw_final
)

# 13. SAVE DMA COMPLETENESS TABLE

write.csv(
  dma_exclusion,
  file.path(
    failure_dir,
    "dma_completeness_final.csv"
  ),
  row.names = FALSE
)

# 14. SAVE CURRENT FAILURE / REPAIR LOG

if (nrow(repair_log) > 0) {
  
  write.csv(
    repair_log,
    file.path(
      failure_dir,
      "latest_repair_log.csv"
    ),
    row.names = FALSE
  )
}

# 15. PRINT DMA COMPLETENESS

cat("DMA COMPLETENESS\n")

print(
  dma_exclusion |>
    count(
      complete,
      name = "n_dmas"
    )
)


# 16. PRINT DMAS THAT MUST BE DROPPED
dropped_dmas <- dma_exclusion |>
  filter(
    !retain_for_all_chunks
  )

cat("DMAs INCOMPLETE FOR 2006–2026\n")
cat("====================================================\n")

cat(
  "Number dropped: ",
  nrow(dropped_dmas),
  "\n"
)

if (nrow(dropped_dmas) > 0) {
  
  print(
    dropped_dmas |>
      select(
        geo_req,
        n_chunks_saved,
        n_chunks_missing,
        missing_chunks,
        exclusion_reason
      )
  )
}


# 17. SAVE RAW DATA FOR SCRIPT 3

saveRDS(
  raw_final,
  file.path(
    failure_dir,
    "raw_checkpoints_final.rds"
  )
)



# 18. FINAL MESSAGE

cat("SCRIPT 2 COMPLETE\n")
cat("Raw checkpoints saved.\n")
cat("DMA completeness table saved.\n")
cat("Repair/failure logs saved.\n")
cat("Incomplete DMAs will be handled by Script 3.\n")
cat("The presence of incomplete DMAs is NOT an error.\n")