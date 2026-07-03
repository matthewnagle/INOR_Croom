get_implant_classification(joint = "Knee")             # all knee classifications
get_implant_classification(fixation = "Cemented")      # all cemented cases
get_implant_classification(case_id = "200000100")      # one case

# Knee: OKS by system (Triathlon vs Attune vs PFC Sigma etc.)
summarise_proms_by_implant("knee", by = "knee_system")
summarise_proms_by_implant("knee", by = "knee_system", stage = "6 month presentation")

# Knee: OKS by cemented vs uncemented
summarise_proms_by_implant("knee", by = "fixation")

# Hip: OHS by femoral stem (Exeter vs Corail vs Accolade II etc.)
summarise_proms_by_implant("hip", by = "femoral_component")

# Hip: OHS by acetabular cup (Pinnacle vs Trident etc.)
summarise_proms_by_implant("hip", by = "acetabular_component")

# Hip: OHS cemented vs uncemented vs hybrid
summarise_proms_by_implant("hip", by = "fixation")

# EQ-5D by fixation type
summarise_proms_by_implant("eq5d", by = "fixation")

# Group by implant only (as before)
summarise_proms_by_implant("hip", by = "femoral_component")

# Group by implant AND consultant
summarise_proms_by_implant("hip", by = "femoral_component AND consultant")
summarise_proms_by_implant("knee", by = "knee_system AND consultant")
summarise_proms_by_implant("knee", by = "fixation AND consultant")

# Filter to one consultant, group by implant
summarise_proms_by_implant("hip", by = "femoral_component", consultant = "Nagle, Matthew")

# Combine everything
summarise_proms_by_implant("hip", by = "acetabular_component AND consultant",
                           stage = "2 year presentation")

# Example: find all case IDs for Corail stems used by a specific consultant
get_implant_classification(joint = "Hip") %>%
  left_join(
    preop %>% select(FORM_RESPONSE_GROUP_ID, `Admitting consultant`),
    by = "FORM_RESPONSE_GROUP_ID"
  ) %>%
  filter(
    femoral_component == "Actis",
    `Admitting consultant` == "Nagle, Matthew"
  ) %>%
  select(FORM_RESPONSE_GROUP_ID, `Admitting consultant`,
         femoral_component, acetabular_component, fixation_type)
