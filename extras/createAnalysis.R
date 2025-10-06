# code to create analysis script
library(Strategus) # need https://github.com/OHDSI/Strategus/tree/v1.0-plpv-mt-modules branch

#====================================================================================
#COHORT IDS
#====================================================================================

targetIds <- c(21773, 21769, 21770, 21772,21771)
outcomeIds <- c(21803, 21804, 21805, 21806, 21669, 21670, 21671, 21672, 21673, 21674, 21675, 21676, 21677, 21678, 21774, 21775, 21776, 21777, 21778, 21779, 21780, 21781, 21782, 21783, 21784, 21789, 21790, 21791, 21792, 21793, 21794, 21795, 21796, 21797, 21799, 21800, 21801, 21802)

library(Strategus)
ROhdsiWebApi::authorizeWebApi(
  baseUrl = Sys.getenv('baseUrl'), 
  authMethod = 'windows', 
  webApiUsername = keyring::key_get('webApiUsername', 'all'), 
  webApiPassword =  keyring::key_get('webApiPassword', 'all')
    )

# get the cohort ids from ATLAS used as the targets and outcomes
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  cohortIds = c(targetIds, outcomeIds), 
  generateStats = T,
  baseUrl = Sys.getenv('baseUrl')
  )

#============================================================================================
#COHORT GENERATOR
#============================================================================================

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create the settings & validate them
cohortSharedResourcesSpecifications <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
cgModuleSettingsCreator$validateCohortSharedResourceSpecifications(cohortSharedResourcesSpecifications)

cgModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications()

#=========================================================================================
#COHORT DIAGNOSTICS SPECIFICATIONS
#========================================================================================

cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cdModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet,
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE,
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE
)

#===============================================================================================
#CohortIncidence
#===============================================================================================

ciModuleSettingsCreator <- CohortIncidenceModule$new()
targets <- list(
  CohortIncidence::createCohortRef(id = , name = "", cohortId = ),
  CohortIncidence::createCohortRef(id = , name = "", cohortId = ),
  CohortIncidence::createCohortRef(id = , name = "", cohortId = ),
  CohortIncidence::createCohortRef(id = , name = "", cohortId = ),
  CohortIncidence::createCohortRef(id = , name = "", cohortId = ),
  CohortIncidence::createCohortRef(id = , name = "", cohortId = )
)

outcomes <- list(
  CohortIncidence::createOutcomeDef(id = , name = "", cohortId = , cleanWindow = 9999),
  CohortIncidence::createOutcomeDef(id = , name = "", cohortId = , cleanWindow = 9999),
  CohortIncidence::createOutcomeDef(id = , name = "", cohortId = , cleanWindow = 9999),
  CohortIncidence::createOutcomeDef(id = , name = "", cohortId = , cleanWindow = 9999),
  CohortIncidence::createOutcomeDef(id = , name = "", cohortId = , cleanWindow = 9999),
  CohortIncidence::createOutcomeDef(id = , name = "", cohortId = , cleanWindow = 9999),
  CohortIncidence::createOutcomeDef(id = , name = "", cohortId = , cleanWindow = 9999)
)

tars <- list(
  CohortIncidence::createTimeAtRiskDef(id = 1, startWith = "start", endWith = "end"),
  CohortIncidence::createTimeAtRiskDef(id = 2, startWith = "start", endWith = "start", endOffset = 365)
)
analysis1 <- CohortIncidence::createIncidenceAnalysis(
  targets = c(,,,,,,),
  outcomes = c(,,,,),
  tars = c(1, 2)
)

irDesign <- CohortIncidence::createIncidenceDesign(
  targetDefs = targets,
  outcomeDefs = outcomes,
  tars = tars,
  analysisList = list(analysis1),
  strataSettings = CohortIncidence::createStrataSettings(
    byYear = TRUE,
    byGender = TRUE
  )
)

ciModuleSpecifications <- ciModuleSettingsCreator$createModuleSpecifications(
  irDesign = irDesign$toList()
)

#===============================================================================================
#TreatmentPatterns
#===============================================================================================

tpModuleSettingCreator <- TreatmentPatternsModule$new()
tpModuleSpecifications <- tpModuleSettingsCreator$createModuleSpecifications(

TreatmentPatternsModule$execute(
connectionDetails,
analysisSpecifications,
executionSettings
)
  
TreatmentPatternsModule$createResultsDataModel(
resultsConnectionDetails,
resultsDatabaseSchema,
tablePrefix = self$tablePrefix
)

TreatmentPatternsModule$getResultsDataModelSpecification(tablePrefix = "")

TreatmentPatternsModule$uploadResults(
resultsConnectionDetails,
analysisSpecifications,
resultsDataModelSettings
)

TreatmentPatternsModule$createModuleSpecifications(
cohorts,
includeTreatments = NULL,
indexDateOffset = NULL,
minEraDuration = 0,
splitEventCohorts = NULL,
splitTime = NULL,
eraCollapseSize = 30,
combinationWindow = 30,
minPostCombinationDuration = 30,
filterTreatments = "First",
maxPathLength = 5,
ageWindow = 5,
minCellCount = 1,
censorType = "minCellCount",
overlapMethod = "truncate",
concatTargets = TRUE,
startAnchor = "startDate",
windowStart = 0,
endAnchor = "endDate",
windowEnd = 0
)

TreatmentPatternsModule$validateModuleSpecifications(moduleSpecifications)

TreatmentPatternsModule$clone(deep = FALSE)

#===============================================================================================
#Characterization
#===============================================================================================

cModuleSettingsCreator <- CharacterizationModule$new()
cModuleSpecifications <- cModuleSettingsCreator$createModuleSpecifications(
  targetIds = targetIds,
  outcomeIds = outcomeIds,
  minPriorObservation = 365,
  outcomeWashoutDays = 365,
  dechallengeStopInterval = 30,
  dechallengeEvaluationWindow = 30,
  riskWindowStart = 0, 
  startAnchor = 'cohort start', 
  riskWindowEnd = 365, 
  endAnchor = 'cohort start,
  covariateSettings = FeatureExtraction::createDefaultCovariateSettings(),
  minCharacterizationMean = .01
)

#===============================================================================================
#ANALYSIS SPECIFICATIONS CDM MODULES
#===============================================================================================

analysisSpecifications <- createEmptyAnalysisSpecificiations() |>
  addSharedResources(cohortSharedResourcesSpecifications) |>
  addCohortDiagnosticsModuleSpecifications(cdModuleSpecifications) |>
  addCohortGeneratorModuleSpecifications(cgModuleSpecifications) |>
  addCohortIncidenceModuleSpecifications(ciModuleSpecifications) |>
  addTreatmentPatternsModuleSpecifications(tpModuleSpecifications) |>
  addCharacterizationModuleSpecifications(cModuleSpecifications) 

ParallelLogger::saveSettingsToJson(
  object = analysisSpecifications,
  fileName = "inst/study_execution_jsons/validation.json"
)

