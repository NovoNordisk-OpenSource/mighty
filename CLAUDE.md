# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**mighty** is an R package that powers the mightyverse - a declarative paradigm for building ADaM (Analysis Data Model) datasets in clinical trials. It emphasizes specifying *what* you need rather than *how* to create it, allowing users to focus on dataset structure, derivations, and intent while abstracting away tedious processes.

Key capabilities:
- Declarative YAML-based ADaM dataset specifications
- Code component orchestration and compilation
- Dependency tracking for columns and rows
- Automated ADaM program generation

## Development Commands

### Testing
- `devtools::test()` - Run all tests
- `testthat::test_file("tests/testthat/test-filename.R")` - Run specific test file
- `devtools::check()` - Run R CMD check (includes tests, documentation checks, etc.)

### Documentation
- `devtools::document()` - Generate documentation from roxygen2 comments
- `devtools::build_vignettes()` - Build package vignettes

### Package Development
- `devtools::load_all()` - Load package for development
- `devtools::install()` - Install package locally
- `devtools::build()` - Build source package

## Architecture Overview

### Core Workflow Pipeline
The package follows a structured pipeline for ADaM code generation:

1. **YAML Specification Reading** (`read_adam_domain_yml.R`)
   - Parses YAML files containing ADaM domain specifications
   - Validates against JSON schema (`inst/schemas/domain_schema.json`)
   - Supports both yq and native R YAML parsing

2. **Action Setup and Processing** (`setup_actions.R`, `organize_actions.R`)
   - Converts YAML specs into internal action-based data model
   - Creates dependency graphs between actions
   - Handles different node types: col_copy, col_mutate, col_echo, col_compute, row_compute

3. **Code Generation** (`generate_adam_code.R`, `render_code.R`)
   - Orchestrates the complete ADaM code generation process
   - Uses Mustache templates (`inst/components/`) for code rendering
   - Produces executable R programs for ADaM dataset creation

### Node Types and Dependencies
- **col_copy**: Absorbed by domain_init nodes, no independent existence
- **col_mutate**: Independent nodes with domain_init as direct parent
- **col_echo**: Can have various parents, cannot source "core" variables
- **row_compute**: Row operations on existing columns, no new column creation
- **col_compute**: Derivation nodes requiring code_id references

### Key Data Structures
- Actions are represented as data.table objects with metadata
- Dependency edges track relationships between actions
- Code components are R functions with roxygen2 metadata for dependencies/outputs

## File Structure Patterns

### R Functions
- Function files follow snake_case naming (e.g., `generate_adam_code.R`)
- Functions use roxygen2 documentation with @param, @return, @export
- Dependencies specified in roxygen comments: `@depends`, `@outputs`

### Test Structure
- Tests in `tests/testthat/` follow `test-filename.R` pattern
- Fixtures stored in `tests/testthat/fixtures/`
- Snapshot testing used for complex outputs (`_snaps/` directories)

### YAML Specifications
- Domain specifications follow standardized schema
- Located in `inst/yaml_*/` directories for different versions
- Must include: table_metadata, init, column_action sections


## Code Components

Code components are R functions that implement specific derivations or transformations:
- Standard components referenced by name (e.g., `der_complsfl`)
- Custom components specified with file paths (e.g., `path/to/custom/component.R`)
- Must follow roxygen2 conventions for metadata extraction
- Templates stored in `inst/components/` using Mustache syntax

## External Dependencies

Key package dependencies:
- **data.table**: Primary data manipulation framework
- **whisker**: Mustache template rendering
- **checkmate**: Argument validation
- **testthat**: Testing framework

## Git


### PRs

When making PR messages, follow the following format:

```md
## Summary
(Summary of made changes with explanation of what and why)

## Changes Made
- Change 1
- ...

## Testing
- [Summary of testing done to ensure new feature QC'd]
- ...


```

### Issues

#### Features

When making a feature issue, follow the following template:

```md
## Feature Request

### Description
[Provide a clear and concise description of the feature you are requesting. What problem does it aim to solve?]

### Proposed Solution
[Outline a proposed solution or approach for implementing the feature. How do you envision the feature working?]

### Use Case
[Describe a specific use case or scenario where this feature would be beneficial or necessary.]

### Additional Context
[Add any additional context, information, or examples that support or clarify the feature request.]

### Impact
[Explain the potential impact of this feature on the package and its users. How would it improve the package or benefit the user community?]

### Related Issues
[Are there any related issues or pull requests that are relevant to this feature request?]
```

#### Bug reports

When writing a bug issue, follow this template:

```md
## Bug Report

### Description
[Provide a clear and concise description of the bug. What behavior did you observe that you believe to be a bug?]

### Reproducible Example
[Include a minimal, complete, and verifiable example that demonstrates the bug. This could be a code snippet, dataset, or specific steps to reproduce the issue.]

### Expected Behavior
[Describe what you expected to happen when you encountered the bug.]

### Actual Behavior
[Explain what actually happened when you encountered the bug.]

### Environment
- R Version: [e.g., 4.5.1]
- Package Version: [e.g., 1.2.3]
- Operating System: [e.g., Windows 10, macOS 11.1]

### Additional Context
[Add any additional context, information, or examples that can help in understanding and reproducing the bug.]

### Impact
[Explain the impact or consequences of this bug. How does it affect the package's functionality or the user experience?]

### Related Issues
[Are there any related issues, pull requests, or discussions that are relevant to this bug report?]
```


## Writing style
- When writing prose, minimize computer science jargon - speak plainly. Avoid superlatives, hyperbole, and over-the-top descriptions.
- When writing prose, be concise


## Misc
- Do not make changes to files in the `man/`, as these files are generated from the code base
