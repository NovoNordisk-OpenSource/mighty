# parameters accept R code

    {
      "type": "character",
      "attributes": {
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["glue", "character"]
        }
      },
      "value": ["# ADSL-A-B-fn_AB-list(param_1 = \"100\", param_2 = \"s:This is a regular string\", param_3 = \"min(1, 100)\") -----------------------------------------------------\nparams <- list(param_1 = 100, param_2 = \"This is a regular string\", \n    param_3 = min(1, 100), param_4 = NULL, param_5 = min(6, 7))\n\n  # Some comment\n  sum(params$param_5)\n  ADSL <- params\n  \n# Remove interim objects\nrm(params)\n"]
    }

---

    {
      "type": "character",
      "attributes": {
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["glue", "character"]
        }
      },
      "value": ["# ADSL-D-fn_mixed_defaults_and_user_params-list(param_user = \"s:User-supplied string\") -----------------------------------------------------\nparams <- list(param_defualt = 5, param_user = \"User-supplied string\")\n\n\n  ADSL <- c(ADSL, params)\n  \n# Remove interim objects\nrm(params)\n"]
    }

---

    {
      "type": "character",
      "attributes": {
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["glue", "character"]
        }
      },
      "value": ["# ADSL-C-fn_no_params -----------------------------------------------------\n\n  ADSL <- c(ADSL)\n  "]
    }

