# 'Prepare environment for testing
prepare_test <- function(
  trial_path,
  yml,
  sdtm_testdata,
  adam_testdata = c(),
  remove_data = NULL,
  trial_metadata_basename = "trial_metadata_0001.yml"
) {
  path_trial_metadata <- test_path("fixtures", trial_metadata_basename)
  # Ensure symbolic link created in setup_testdata is not removed
  # until test is completed
  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = trial_path,
    sdtm_domains = sdtm_testdata,
    adam_domains = adam_testdata,
    remove_cols = remove_data,
    env = parent.frame()
  )
  list(
    trial_path = trial_path,
    path_trial_metadata = path_trial_metadata
  )
}
