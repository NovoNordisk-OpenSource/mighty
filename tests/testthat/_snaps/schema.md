# Invalid properties in column_metadata fail yaml validation checks

    x YAML validation failed for temp_test_file.yml
    * Error location: column_metadata → item 1 | Error message: Unexpected field 'invalid_property' found. Remove this field or check for typos.
    * Error location: column_metadata → item 2 | Error message: Required field 'column' is missing. Please add this field.
    * Error location: column_metadata → item 2 | Error message: Unexpected field 'col' found. Remove this field or check for typos.
    * Error location: column_metadata → item 3 | Error message: Expected type 'object' but got list. Please check the data type.
    * Error location: column_metadata → item 4 | Error message: Required field 'column' is missing. Please add this field.
    * Error location: column_metadata → item 5 | Error message: Required field 'column' is missing. Please add this field.
    * Error location: column_metadata → item 6 | Error message: Required field 'column' is missing. Please add this field.
    * Error location: column_metadata → item 7 → column | Error message: Expected type 'string' but got list. Please check the data type.
    * Error location: column_metadata → item 8 → column | Error message: Expected type 'string' but got list. Please check the data type.

# Invalid parameter specifications for row fail yaml validation checks

    x YAML validation failed for temp_test_file.yml
    * Error location: row_actions → item 1 → parameters → item 1 → parm1 | Error message: Expected type 'c("string", "number", "boolean", "integer")' but got character. Please check the data type.
    * Error location: row_actions → item 1 → parameters → item 2 | Error message: Expected type 'object' but got character. Please check the data type.
    * Error location: row_actions → item 1 → parameters → item 3 | Error message: Expected type 'object' but got character. Please check the data type.

# Missing row fields fail yaml validation checks

    x YAML validation failed for temp_test_file.yml
    * Error location: row_actions → item 1 | Error message: Required field 'id' is missing. Please add this field.
    * Error location: row_actions → item 1 | Error message: Required field 'code_id' is missing. Please add this field.
    * Error location: row_actions → item 1 | Error message: Unexpected field 'id2' found. Remove this field or check for typos.

# Multiple business logic validations fail

    x Validation failed for temp_test_file.yml with the following error(s):
    * The following columns have parameters but no code_id:
    *  • VAR1
    * The following columns are defined multiple times:
    *  • VAR1

