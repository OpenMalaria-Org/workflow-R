# Example: plotting the results
#
# Standalone example of how to use the data produced by main.R
pacman::p_load(data.table, nanoparquet, ggplot2, patchwork)

experiment_folder = "experiment"

# Example plotting
##################

# Load saved results if starting from an existing experiment folder
scenarios <- nanoparquet::read_parquet(file.path(experiment_folder, "scenarios.parquet"))
df <- data.table::as.data.table(nanoparquet::read_parquet(file.path(experiment_folder, "output.parquet"))) # parquet loses the data.table class, RDS kept it

d = df[complete.cases(df), ] # remove NA values
d = d[!d$survey == 1,] # remove first survey (due to accumulating data during the burn-in period)
d = d[, .(value = sum(value)), by = .(index, measure, survey)] # aggregate age-groups

# merge with the scenarios table to have all the metadata (eir, access, seed, etc.) in the same data.table
d = merge(d, scenarios, by = 'index')
setorder(d, eir, seed, survey, measure)

nPatent = d[measure == 3]
nUncomp = d[measure == 14]
nHost = d[measure == 0]
inputEIR = d[measure == 35]
simulatedEIR = d[measure == 36]

prevalence = copy(nPatent)
prevalence[, value := nPatent$value / nHost$value]

incidence = copy(nUncomp)
incidence[, value := nUncomp$value / nHost$value]

p_prevalence = ggplot(prevalence, aes(survey, value, color = factor(eir), group = eir)) +
    stat_summary(fun = mean, geom = "line") +
    labs(x = "Survey", y = "Prevalence", color = "EIR") +
    theme_minimal(base_size = 14)

p_incidence = ggplot(incidence, aes(survey, value, color = factor(eir), group = eir)) +
    stat_summary(fun = mean, geom = "line") +
    labs(x = "Survey", y = "Clinical Incidence", color = "EIR") +
    theme_minimal(base_size = 14)

eir = rbind(
    copy(inputEIR)[, type := "input"],
    copy(simulatedEIR)[, type := "simulated"]
)

p_eir = ggplot(eir, aes(survey, value, color = factor(eir), linetype = type, group = interaction(eir, type))) +
    stat_summary(fun = mean, geom = "line") +
    labs(x = "Survey", y = "EIR", color = "EIR", linetype = "") +
    theme_minimal(base_size = 14)

p = p_prevalence / p_incidence / p_eir
print(p)

# Age-restricted results and per age group results
##################################################

# The <ageGroup> block of base.xml defines 8 output age groups (ageGroup index 1 to 8):
# 0-0.5, 0.5-1, 1-2, 2-5, 5-10, 10-15, 15-20 and 20-100 years.
age_labels = c("0-0.5", "0.5-1", "1-2", "2-5", "5-10", "10-15", "15-20", "20-100")

a = df[complete.cases(df), ]
a = a[!a$survey == 1, ] # remove first survey (due to accumulating data during the burn-in period)
a = a[ageGroup > 0, .(value = sum(value)), by = .(index, measure, ageGroup)] # sum over all the surveys, keep each age group (ageGroup 0 = measures without age groups, e.g. EIR)
a = merge(a, scenarios, by = 'index')
a[, ageGroup := factor(ageGroup, levels = seq_along(age_labels), labels = age_labels)]
setorder(a, eir, seed, measure, ageGroup)

# Prevalence for three age ranges: aggregate sum(nPatent) / sum(nHost) over the age groups of the range
age_ranges = list(
    "2-10 years" = c("2-5", "5-10"),
    "10-100 years" = c("10-15", "15-20", "20-100"),
    "all ages" = age_labels
)

prevalenceRanges = rbindlist(lapply(names(age_ranges), function(rng) {
    nPatent = a[measure == 3 & ageGroup %in% age_ranges[[rng]], .(value = sum(value)), by = .(index, eir, seed)]
    nHost = a[measure == 0 & ageGroup %in% age_ranges[[rng]], .(value = sum(value)), by = .(index, eir, seed)]

    prev = copy(nPatent)
    prev[, value := nPatent$value / nHost$value]
    prev[, ageRange := rng]
}))

# EIR (x-axis) vs prevalence (y-axis): one figure per age range (one per plot page), averaged over the seeds
for (rng in names(age_ranges)) {
    eir_prev = prevalenceRanges[ageRange == rng, .(value = mean(value)), by = eir]

    p_eir_prevalence = ggplot(eir_prev, aes(eir, value)) +
        geom_point(aes(color = factor(eir)), size = 2) +
        geom_line() +
        scale_x_log10(name = "Annual EIR", breaks = eir_prev$eir) +
        labs(y = paste0("Prevalence (", rng, ")"), color = "EIR") +
        theme_minimal(base_size = 14)
    print(p_eir_prevalence)
}

# Prevalence and incidence for each age group: aggregate sum(nPatent) / sum(nHost)
nPatent = a[measure == 3]
nUncomp = a[measure == 14]
nHost = a[measure == 0]

prevalenceAge = copy(nPatent)
prevalenceAge[, value := nPatent$value / nHost$value]

incidenceAge = copy(nUncomp)
incidenceAge[, value := nUncomp$value / nHost$value]

# Prevalence (x-axis) vs incidence (y-axis): one curve per age group, averaged over the seeds
prev_inc = merge(
    prevalenceAge[, .(prevalence = mean(value)), by = .(eir, ageGroup)],
    incidenceAge[, .(incidence = mean(value)), by = .(eir, ageGroup)],
    by = c('eir', 'ageGroup')
)
setorder(prev_inc, ageGroup, eir) # geom_path follows the row order: one curve, traced from the lowest to the highest EIR

p_prevalence_incidence = ggplot(prev_inc, aes(prevalence, incidence, color = ageGroup)) +
    geom_point(size = 2) +
    geom_path() +
    labs(x = "Prevalence", y = "Clinical Incidence", color = "Age group (years)",
         subtitle = "One curve per age group") +
    theme_minimal(base_size = 14)

print(p_prevalence_incidence)
