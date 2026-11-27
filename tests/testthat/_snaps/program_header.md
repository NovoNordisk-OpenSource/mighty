# gen_program_header creates the full header

    Code
      cat(gen_program_header(actions, part = 1, n_parts = 1, generated_at = fixed_time))
    Output
      # ******************************************************************************
      # Description:   Creates the ADSL analysis dataset.
      # Input:         DM, VS
      # Output:        ADSL
      # Usage notes:   Generated from the Mighty study specification.
      #                Modify the specification or source components and regenerate;
      #                do not modify this generated program directly.
      # Generated:     2026-11-27T13:03:12Z
      # Generator:     mighty v9.9.9
      # ******************************************************************************

# generate_adam_code adds headers with one shared generation time

    Code
      cat(headers, sep = "\n\n")
    Output
      # ******************************************************************************
      # Description:   Creates the ADSL analysis dataset (part 1 of 2).
      # Input:         DM, DM_VACCINE
      # Output:        ADSL
      # Usage notes:   Generated from the Mighty study specification.
      #                Modify the specification or source components and regenerate;
      #                do not modify this generated program directly.
      # Generated:     2026-11-27T13:03:12Z
      # Generator:     mighty v9.9.9
      # ******************************************************************************
      
      # ******************************************************************************
      # Description:   Creates the ADLB analysis dataset.
      # Input:         ADSL, LB, SV
      # Output:        ADLB
      # Usage notes:   Generated from the Mighty study specification.
      #                Modify the specification or source components and regenerate;
      #                do not modify this generated program directly.
      # Generated:     2026-11-27T13:03:12Z
      # Generator:     mighty v9.9.9
      # ******************************************************************************
      
      # ******************************************************************************
      # Description:   Creates the ADSL analysis dataset (part 2 of 2).
      # Input:         ADLB, ADSL
      # Output:        ADSL
      # Usage notes:   Generated from the Mighty study specification.
      #                Modify the specification or source components and regenerate;
      #                do not modify this generated program directly.
      # Generated:     2026-11-27T13:03:12Z
      # Generator:     mighty v9.9.9
      # ******************************************************************************

