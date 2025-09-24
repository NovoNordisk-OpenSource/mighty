# Invalid properties in column_metadata fail yaml validation checks

    x YAML validation failed for temp_test_file.yml
    * Error location: column_action → STUDYID | Error message: Expected type 'c("object", "null")' but got list. Please check the data type.
    * Error location: column_action → AVAL -> code_iid | Error message: Unexpected field 'code_iid' found. Remove this field or check for typos.

# Invalid parameter specifications for row fail yaml validation checks

    x YAML validation failed for temp_test_file.yml
    * Error location: row_action → X → parameters → item 1 → parm1 | Error message: Expected type 'c("string", "number", "boolean", "integer")' but got character. Please check the data type.
    * Error location: row_action → X → parameters → item 2 | Error message: Expected type 'object' but got character. Please check the data type.
    * Error location: row_action → X → parameters → item 3 | Error message: Expected type 'object' but got character. Please check the data type.

# Missing/invalid row fields fail yaml validation checks

    x YAML validation failed for temp_test_file.yml
    * Error location: row_action → X -> code_id | Error message: Required field 'code_id' is missing. Please add this field.
    * Error location: row_action → X -> codee | Error message: Unexpected field 'codee' found. Remove this field or check for typos.
    * Error location: row_action → Y | Error message: Expected type 'object' but got list. Please check the data type.

# Multiple business logic validations fail

    x Validation failed for temp_test_file.yml with the following error(s):
    * The following columns have parameters but no code_id:
    *  • VAR1
    * The following row actions are not defined, but are listed as row dependencies for either a column or another row action:
    *  • A

