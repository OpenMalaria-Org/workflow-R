# Session -> Set Working Directory -> To Source File Location
source("pacman.R")
source("scenarios.R")
source("extract.R")
source("run.R")

pacman::p_load(data.table, xml2, lhs, ggplot2, patchwork)

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

# Creation of the XML template
# ############################
#
# The template defines how each scenario XML is created:
# - the first argument is the base XML file to use
# - XPath expressions are used to select where values are written
# - scalars or strings are fixed across all scenarios
# - XML nodes can be inserted for more flexibility
# - variable(name) are placeholders and will be replaced during scenario generation
# The same variable can be referenced by multiple XPaths.
template <- create_template(
    # Base XML specification
    "snippets/base.xml",
    
    # Fixed parameters
    "demography/@popSize" = 1000,
    "monitoring/@startDate" = "1950-01-01", # 50 years burnin (start date - 50 years)
    "monitoring/surveys/surveyTime" = "2000-01-01", # start date
    "monitoring/surveys/surveyTime/@repeatStep" = "5d", # survey every 5d (1y is also common)
    "monitoring/surveys/surveyTime/@repeatEnd" = "2020-01-01", # end date
    
    # Using snippets for more flexibility
    "interventions/@name" = "GVI example",
    "interventions" = xml_new_root("human"),
    "interventions/human" = read_xml("snippets/GVI.component.xml"),
    "interventions/human" = read_xml("snippets/GVI.deployment.xml"),
    "interventions/human/deployment[@name='GVI_example']/timed/deploy/@coverage" = 0.7,
    
    # Placeholder variables
    "entomology/@scaledAnnualEIR" = variable(eir),
    "healthSystem/DecisionTree5Day/pSeekOfficialCareUncomplicated1/@value" = variable(access),
    "healthSystem/DecisionTree5Day/pSeekOfficialCareUncomplicated2/@value" = variable(access),
    "model/computationParameters/@iseed" = variable(seed)
)

# Creation of the scenarios table
# ###############################
#
# The table contains one row per scenario and:
# - it must contain an index column
# - it must reference all previously declared variable names
# Additional columns are allowed.

# Example 1: full-Factorial expansion using the Cross-Join (CJ) product:
scenarios <- CJ(
    eir = c(5, 20),
    access = 0.04,
    seed = 1:3
)
scenarios[, index := .I]
setcolorder(scenarios, "index")

# Example 2: Latin hypercube sampling:
#n <- 6
#x <- lhs::randomLHS(n, 2)
#
#scenarios <- data.table(
#  eir = qunif(x[, 1], 5, 20),
#  access = qunif(x[, 2], 0.02, 0.20),
#  seed = 1:n
#)
#scenarios[, index := .I]
#setcolorder(scenarios, "index")

write_scenarios(template, scenarios, experiment_folder, om, validate_xml = TRUE, overwrite = FALSE)
run(scenarios, experiment_folder, om, slurm, overwrite = FALSE)
df <- extract(scenarios, experiment_folder, overwrite = FALSE)

# The workflow ends here.

# Example plotting
##################

# Load saved results if starting from an existing experiment folder
scenarios <- fread(file.path(experiment_folder, "scenarios.csv"))
df <- readRDS(file.path(experiment_folder, "output.rds"))

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
