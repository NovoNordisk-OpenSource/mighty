# filter deps to multiple keyless domains error when cross-domain check disabled

    Code
      generate_adam_code(adam_specifications = adam_specifications,
        path_connector_config = trial_path, check_cross_domain_adam_dependencies = FALSE)
    Condition
      Error:
      ! Specification validation errors found:
      
      [Unknown domains in filter]
      x Filter for ADLB references 2 unknown domains: ADAE and ADVS
      i These domains must have join keys defined to be used in filters
      
      Suggestions:
      * Add key definitions to '_mighty.yml' for the referenced domains
      * Verify the domain names are spelled correctly
      * Ensure all domains used in filters are defined in your trial metadata

