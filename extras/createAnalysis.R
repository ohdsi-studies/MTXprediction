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

#=========================================================================================
#COHORT DIAGNOSTICS SPECIFICATIONS
#========================================================================================

cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cdModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
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

#============================================================================================
#COHORT GENERATOR
#============================================================================================

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create the settings & validate them
cohortSharedResourcesSpecifications <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
cgModuleSettingsCreator$validateCohortSharedResourceSpecifications(cohortSharedResourcesSpecifications)

cgModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications()

#===============================================================================================
#CohortIncidence
#===============================================================================================

#===============================================================================================
#TreatmentPatterns
#===============================================================================================

#===============================================================================================
#Characterization
#===============================================================================================


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

