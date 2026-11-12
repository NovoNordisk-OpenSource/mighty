# filter deps to multiple keyless domains error when cross-domain check disabled

    Code
      generate_adam_code(adam_specifications = adam_specifications,
        path_connector_config = get_temp_connector_config_path(),
        check_cross_domain_adam_dependencies = FALSE)
    Condition
      Error:
      ! Specification validation errors found:
      
      [Unknown domains in filter]
      x Filter for ADLB references 2 unknown domains: ADAE and ADVS
      i These domains must have join keys defined to be used in filters
      
      Suggestions:
      * Add key definitions to '_mighty.yml' /external_data for the referenced domains
      * Verify the domain names are spelled correctly
      * Ensure all domains used in filters are defined in your trial metadata

# Error when a filter uses a col_rename source and joins a column with its name

    Code
      add_filter_domain_actions(rename_filter_actions(), filter_depend_cols = list(
        ADLB = c("XL.LBSEQ", "LBSEQ")), filter_domain = list(ADLB = NA), domain_keys = list(
        ADLB = "USUBJID", XL = "USUBJID"))
    Condition
      Error:
      ! Specification validation errors found:
      
      [Filter uses a renamed column under both names]
      x Filter for ADLB uses LBSEQ and also joins XL.LBSEQ, which has the same name, so LBSEQ is renamed to SRCSEQ before the filter
      
      Suggestions:
      * Use only the new name in the filter
      * Or list the source column under columns to keep it as a separate column

# Error when a filter uses a col_rename source and its output

    Code
      get_filter_outputs(rename_filter_actions(), c("LBSEQ", "SRCSEQ"))
    Condition
      Error:
      ! Specification validation errors found:
      
      [Filter uses a renamed column under both names]
      x Filter for ADLB uses LBSEQ, but it also needs SRCSEQ, which is LBSEQ renamed
      
      Suggestions:
      * Use only the new name in the filter
      * Or list the source column under columns to keep it as a separate column

