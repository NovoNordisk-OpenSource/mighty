# filter deps to multiple keyless domains error when cross-domain check disabled

    Code
      generate_adam_code(adam_specifications = adam_specifications,
        path_trial_metadata = path_trial_metadata, path_trial = trial_path,
        check_cross_domain_adam_dependencies = FALSE)
    Condition
      Error in `get_filter_join_keys_external_domains()`:
      ! Filter for "ADLB" references 2 unknown domains: "ADAE" and "ADVS"
      i Ensure these domains have join keys specified in trial metadata

