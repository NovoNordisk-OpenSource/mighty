# ADSL with 1 base domain (DM)
#   - 4 columns kept from DM
#   - 1 copied column (PLANNED_ARM)
#   - 1 derived column (AGE_GRP1)

# Load mighty
library(mighty)

# Generate ADaM program
results <- mighty::generate_adam_code(
  adam_specifications = "study_dir",
  path_connector_config = "_connector.yml"
)

# Print rendered ADaM program
results$programs$`1_ADSL` |> cat()
