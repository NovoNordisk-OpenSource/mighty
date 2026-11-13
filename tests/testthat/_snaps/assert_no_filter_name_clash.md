# filter on another dataset's column errors when the column is copied

    Code
      check_filter_clash(actions, list(ADLB = "SV.VISIT"))
    Condition
      Error:
      ! Specification validation errors found:
      
      [Filter column name clash]
      x Filter for ADLB depends on SV.VISIT, but ADLB already has a column VISIT when the filter runs (copied from LB)
      
      Suggestions:
      * To filter on the other dataset's column, the dataset's column with the same name must not exist yet when the filter runs
      * For a copied column, copy it from the other dataset instead with `method: SV.VISIT`. The column then holds the other dataset's values, not the starting dataset's
      * To filter on the dataset's own column, write it without the prefix (e.g. `VISIT`). The two columns can hold different values, so check that this is the column you mean

# filter on another dataset's column errors when a needed component builds it

    Code
      check_filter_clash(actions, list(ADLB = c("SV.VISIT", "VISIT")))
    Condition
      Error:
      ! Specification validation errors found:
      
      [Filter column name clash]
      x Filter for ADLB depends on SV.VISIT, but ADLB already has a column VISIT when the filter runs (built by component 'visit_label_01.R')
      
      Suggestions:
      * To filter on the other dataset's column, the dataset's column with the same name must not exist yet when the filter runs
      * For a column built by a component, the filter must not need that component, directly or through other components
      * To filter on the dataset's own column, write it without the prefix (e.g. `VISIT`). The two columns can hold different values, so check that this is the column you mean

# dependencies that differ only in case are reported once

    Code
      check_filter_clash(actions, list(ADLB = c("SV.VISIT", "sv.visit")))
    Condition
      Error:
      ! Specification validation errors found:
      
      [Filter column name clash]
      x Filter for ADLB depends on SV.VISIT, but ADLB already has a column VISIT when the filter runs (copied from LB)
      
      Suggestions:
      * To filter on the other dataset's column, the dataset's column with the same name must not exist yet when the filter runs
      * For a copied column, copy it from the other dataset instead with `method: SV.VISIT`. The column then holds the other dataset's values, not the starting dataset's
      * To filter on the dataset's own column, write it without the prefix (e.g. `VISIT`). The two columns can hold different values, so check that this is the column you mean

# all clashes in all datasets are listed in one error

    Code
      check_filter_clash(actions, list(ADLB = c("SV.VISIT", "ADSL.SEX"), ADVS = "SV.VISIT"),
      list(ADLB = list(list(LB = NA_character_)), ADVS = list(list(VS = NA_character_))))
    Condition
      Error:
      ! Specification validation errors found:
      
      [Filter column name clash]
      x Filter for ADLB depends on SV.VISIT, but ADLB already has a column VISIT when the filter runs (copied from LB)
      x Filter for ADLB depends on ADSL.SEX, but ADLB already has a column SEX when the filter runs (copied from LB)
      x Filter for ADVS depends on SV.VISIT, but ADVS already has a column VISIT when the filter runs (copied from VS)
      
      Suggestions:
      * To filter on the other dataset's column, the dataset's column with the same name must not exist yet when the filter runs
      * For a copied column, copy it from the other dataset instead with `method: SV.VISIT`. The column then holds the other dataset's values, not the starting dataset's
      * To filter on the dataset's own column, write it without the prefix (e.g. `VISIT`). The two columns can hold different values, so check that this is the column you mean

# the same column joined in from two other datasets errors

    Code
      check_filter_clash(adlb_base, list(ADLB = c("SV.VISIT", "TV.VISIT")))
    Condition
      Error:
      ! Specification validation errors found:
      
      [Filter column name clash]
      x Filter for ADLB depends on SV.VISIT and TV.VISIT, so a column named VISIT is joined in more than once
      
      Suggestions:
      * A filter can join a column name from only one other dataset. Keep only one of the dependencies with the same column name

# issue #86 spec errors: global filter on SV.VISIT with VISIT copied from LB

    Code
      generate_clash_study(adlb_yml, get_temp_connector_config_path())
    Condition
      Error:
      ! Specification validation errors found:
      
      [Filter column name clash]
      x Filter for ADLB depends on SV.VISIT, but ADLB already has a column VISIT when the filter runs (copied from LB)
      
      Suggestions:
      * To filter on the other dataset's column, the dataset's column with the same name must not exist yet when the filter runs
      * For a copied column, copy it from the other dataset instead with `method: SV.VISIT`. The column then holds the other dataset's values, not the starting dataset's
      * To filter on the dataset's own column, write it without the prefix (e.g. `VISIT`). The two columns can hold different values, so check that this is the column you mean

# global filter on SV.VISIT errors when the filter needs a component built from VISIT

    Code
      generate_clash_study(adlb_yml, get_temp_connector_config_path())
    Condition
      Error:
      ! Specification validation errors found:
      
      [Filter column name clash]
      x Filter for ADLB depends on SV.VISIT, but ADLB already has a column VISIT when the filter runs (built by component 'visit_label_01.R')
      
      Suggestions:
      * To filter on the other dataset's column, the dataset's column with the same name must not exist yet when the filter runs
      * For a column built by a component, the filter must not need that component, directly or through other components
      * To filter on the dataset's own column, write it without the prefix (e.g. `VISIT`). The two columns can hold different values, so check that this is the column you mean

