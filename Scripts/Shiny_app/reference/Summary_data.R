
# =============================================================================
# SECTION 1: PROMS
# =============================================================================
summarise_proms_by_stage("knee")

# OKS by consultant across all stages
summarise_proms_by_consultant("knee")

# OKS by consultant at a specific time point
summarise_proms_by_consultant("knee", stage = "2 year presentation")

# OHS for a specific consultant across all stages
summarise_proms_by_consultant("hip", consultant = "Healy, Laura")

# EQ-5D by consultant at 6 months
summarise_proms_by_consultant("eq5d", stage = "6 month presentation")

# =============================================================================
# SECTION 2: COMPLICATONS
# =============================================================================

# All consultants, all complications, with rates
summarise_complications_by_consultant()

# One specific consultant
summarise_complications_by_consultant(consultant = "Nagle, Matthew")

# Knee cases only
summarise_complications_by_consultant(joint = "Knee")

# Just wound infections across all consultants
summarise_complications_by_consultant(complication_type = "wound_infection")

# =============================================================================
# SECTION 3: DEMOGRAPHICS
# =============================================================================

result <- summarise_demographics_by_consultant()
result$summary        # age, sex, BMI, comorbidity rate per consultant
result$comorbidities  # breakdown by type (Cardiac, Renal, etc.) per consultant

# Filter to knee cases only
result <- summarise_demographics_by_consultant(joint = "Knee")

# One specific consultant
result <- summarise_demographics_by_consultant(consultant = "Nagle, Matthew")

summarise_demographics()                # all cases
summarise_demographics(joint = "Hip")   # hip cases only

# =============================================================================
# SECTION 4: SURGICAL TIMES
# =============================================================================

# View surgical times for specific cases
get_surgical_times(joint = "Knee")
get_surgical_times(mrn = "D193111")

# Summary stats (duration in minutes from knife-to-skin to wound closure)
summarise_surgical_times()                                          # overall
summarise_surgical_times(by = "consultant")                         # grouped by consultant
summarise_surgical_times(by = "joint")                              # hip vs knee
summarise_surgical_times(consultant = "Nagle, Matthew")             # one consultant, overall
summarise_surgical_times(by = "joint", consultant = "Nagle, Matthew") # one consultant, by joint
summarise_surgical_times(by = "consultant", joint = "Knee")           # by hospital, knee only


# =============================================================================
# SECTION 5: IMPLANTS
# =============================================================================
get_implants(case_id = "100172757")

list_implant_types(joint = "Hip")                              # all hip implant types
list_implant_types(joint = "Knee", manufacturer = "Stryker")   # Stryker knee implants
list_implant_types(consultant = "Healy, Laura")                # what a consultant uses
list_implant_types(joint = "Knee", hospital = "Croom Orthopaedic Hospital")

export <- list_implant_types(joint = "Hip") 

write_csv(export, "Output/export4.csv")
