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
