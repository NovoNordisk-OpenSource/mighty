# ADSL with 1 base domain
#   - 4 predecessors
#   - 1 renamed predecessor
#   - 1 derivation

# Load mighty
library(mighty)

# Generate ADaM program
results <- mighty::generate_adam_code(
  adam_specifications = "study_dir",
  path_connector_config = "_connector.yml"
)

# Print rendered ADaM program
results$programs$`1_ADSL` |> cat()
