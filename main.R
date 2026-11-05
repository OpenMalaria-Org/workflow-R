# Session -> Set Working Directory -> To Source File Location
source("pacman.R")
source("scenarios.R")
source("extract.R")
source("run.R")

pacman::p_load(data.table, xml2, lhs, nanoparquet)

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
    version = 49,
    path = "path_to_openmalaria_release", # where the executable openMalaria lives
    output_format = "txt" # "bin", "bin.gz", "txt", or "txt.gz"
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
    "demography/@popSize" = 2000,
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

# The workflow ends here. See example.plot.R for an example of how to use the data.
