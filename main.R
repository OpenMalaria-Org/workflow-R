# Session -> Set Working Directory -> To Source File Location
source("pacman.R")
source("scenarios.R")
source("extract.R")
source("run.R")

pacman::p_load(data.table, xml2, ggplot2, patchwork)

# IMPORTANT: If using Slurm, check and adapt job.sh to your system
slurm = list(
    account = "chitnis",
    partition = "scicore",
    job_name = "OpenMalaria",
    qos = "30min",
    time = "00:30:00",
    mem_per_cpu = "1G",
    cpus_per_task = 16,
    batch_size = 16
    # number of job in array = N / batch_size
    # if batch_size = cpus_per_task then one OM instance = one CPU = faster
    # if batch_size > cpus_per_task then multiple OM instances per cpus = slower but less Slurm jobs
    # just leave it 16 / 16, 32 / 32, 64 / 64
)

if (!nzchar(Sys.which("sbatch"))) slurm <- NULL # slurm = NULL if not available, e.g. running on a laptop

om = list(
    version = 50,
    path = normalizePath("../fork/openMalaria-50.0"),
    output_format = "bin.gz" # "bin", "bin.gz", "txt", or "txt.gz"
)

experiment_folder = "experiment"

mapping <- s(
    "snippets/default.xml",
    "demography/@popSize" = 1000,
    "monitoring/@startDate" = "1950-01-01", # 50 years burnin (start date - 50 years)
    "monitoring/surveys/surveyTime" = "2000-01-01", # start date
    "monitoring/surveys/surveyTime/@repeatStep" = "5d", # survey every 5d (1y is also common)
    "monitoring/surveys/surveyTime/@repeatEnd" = "2020-01-01", # end date
    "interventions/@name" = "GVI example",
    "interventions" = xml_new_root("human"),
    "interventions/human" = read_xml("snippets/GVI.component.xml"),
    "interventions/human" = read_xml("snippets/GVI.deployment.xml"),
    "interventions/human/deployment[@name='GVI_example']/timed/deploy/@coverage" = 0.7,
    "entomology/@scaledAnnualEIR" = list(eir = c(5, 20)),
    "healthSystem/DecisionTree5Day/pSeekOfficialCareUncomplicated1/@value" = list(access = 0.04),
    "healthSystem/DecisionTree5Day/pSeekOfficialCareUncomplicated2/@value" = list(access = 0.04),
    "model/computationParameters/@iseed" = list(seed = 1:3)
)

scenarios <- create_scenarios(mapping) # full-factorial list expansion

# Optional: modify the scenario table
# scenarios <- scenarios[sample(.N, 4)]
# scenarios[, access := runif(.N, 0.02, 0.20)]
# scenarios[, index := .I]

write_scenarios(mapping, scenarios, experiment_folder, om, validate_xml = TRUE, overwrite = FALSE)
run(scenarios, experiment_folder, om, slurm, overwrite = FALSE)
df <- extract(scenarios, experiment_folder, overwrite = FALSE)

# Example plotting
##################

# Load saved results if starting from an existing experiment folder
scenarios <- fread(file.path(experiment_folder, "scenarios.csv"))
df <- fread(file.path(experiment_folder, "output.csv"))

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
