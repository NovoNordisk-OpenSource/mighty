# Column depends with wrong prefix should fail

    x YAML validation failed for ADLB with the following error(s):
    
    Caused by error in `S7schema::validate_list()`:
    ! /columns/2/depends must match exactly one schema in oneOf
    * must match pattern "^(?:rows|parameters)\.[a-z_][a-z0-9_]*$"
    * must be array

# Row depends with wrong prefix should fail

    x YAML validation failed for ADLB with the following error(s):
    
    Caused by error in `S7schema::validate_list()`:
    ! /rows/1/depends must match exactly one schema in oneOf
    * must match pattern "^(?:rows|parameters)\.[a-z_][a-z0-9_]*$"
    * must be array

# Parameter depends with wrong prefix should fail

    x YAML validation failed for ADLB with the following error(s):
    
    Caused by error in `S7schema::validate_list()`:
    ! /parameters/1/depends must match exactly one schema in oneOf
    * must match pattern "^(?:rows|parameters)\.[a-z_][a-z0-9_]*$"
    * must be array

