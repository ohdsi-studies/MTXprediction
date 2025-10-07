library(Strategus) # need https://github.com/OHDSI/Strategus/tree/v1.0-plpv-mt-modules branch
library(CohortIncidence)


#====================================================================================
#COHORT IDS
#====================================================================================

targetIds <- c(21773, 21769, 21770, 21772,21771)
outcomeIds <- c(21803, 21804, 21805, 21806, 21669, 21670, 21671, 21672, 21673, 21674, 21675, 21676, 21677, 21678, 21774, 21775, 21776, 21777, 21778, 21779, 21780, 21781, 21782, 21783, 21784, 21789, 21790, 21791, 21792, 21793, 21794, 21795, 21796, 21797, 21799, 21800, 21801, 21802)

library(Strategus)
ROhdsiWebApi::authorizeWebApi(
  baseUrl = Sys.getenv('baseUrl'), 
  authMethod = 'windows', 
  webApiUsername = Sys.getenv('webApiUsername'), 
  webApiPassword =  Sys.getenv('webApiPassword')
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

# Give a name to the outcome ID set 
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId %in% outcomeIds) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 0)

#TAR
timeAtRisks <- tibble(
  riskWindowStart  = 0,
  startAnchor = 'cohort start',
  riskWindowEnd  = 365,
  endAnchor = 'cohort end'
)

ciModuleSettingsCreator <- CohortIncidenceModule$new()

tcIds <- cohortDefinitionSet %>%
  filter(!cohortId %in% oList$outcomeCohortId & isSubset) %>%
  pull(cohortId)
targetList <- lapply(
  tcIds,
  function(cohortId) {
    CohortIncidence::createCohortRef(
      id = cohortId, 
      name = cohortDefinitionSet$cohortName[cohortDefinitionSet$cohortId == cohortId]
    )
  }
)
outcomeList <- lapply(
  seq_len(nrow(oList)),
  function(i) {
    CohortIncidence::createOutcomeDef(
      id = i, 
      name = cohortDefinitionSet$cohortName[cohortDefinitionSet$cohortId == oList$outcomeCohortId[i]], 
      cohortId = oList$outcomeCohortId[i], 
      cleanWindow = oList$cleanWindow[i]
    )
  }
)

tars <- list()
for (i in seq_len(nrow(timeAtRisks))) {
  tars[[i]] <- CohortIncidence::createTimeAtRiskDef(
    id = i, 
    startWith = gsub("cohort ", "", timeAtRisks$startAnchor[i]), 
    endWith = gsub("cohort ", "", timeAtRisks$endAnchor[i]), 
    startOffset = timeAtRisks$riskWindowStart[i],
    endOffset = timeAtRisks$riskWindowEnd[i]
  )
}
analysis1 <- CohortIncidence::createIncidenceAnalysis(
  targets = tcIds,
  outcomes = seq_len(nrow(oList)),
  tars = seq_along(tars)
)


irDesign <- CohortIncidence::createIncidenceDesign(
  targetDefs = targetList,
  outcomeDefs = outcomeList,
  tars = tars,
  analysisList = list(analysis1),
  #studyWindow = irStudyWindow,
  strataSettings = CohortIncidence::createStrataSettings(
    byYear = TRUE,
    byGender = TRUE,
    byAge = TRUE,
    ageBreaks = seq(0, 110, by = 10)
  )
)
cohortIncidenceModuleSpecifications <- ciModuleSettingsCreator$createModuleSpecifications(
  irDesign = irDesign$toList()
)


ciModuleSpecifications <- ciModuleSettingsCreator$createModuleSpecifications(
  irDesign = irDesign$toList()
)

#===============================================================================================
#TreatmentPatterns
#===============================================================================================

# Create dataframe for TreatmentPatterns
cohorts <- cohortDefinitionSet %>%
  select (cohortId, cohortName)

cohorts$type <- ifelse(cohorts$cohortId %in% oList$outcomeCohortId, 'event', 'target')

# Remove certain IDs to try to overcome stuck
#cohorts = cohorts%>%filter(cohortId>...)

tpModuleSettingsCreator <- TreatmentPatternsModule$new()
tpModuleSpecifications <- tpModuleSettingsCreator$createModuleSpecifications(
  cohorts, 
  includeTreatments = "startDate",
  indexDateOffset = 0,
  minEraDuration = 1,
  splitEventCohorts = NULL,
  splitTime = NULL,
  eraCollapseSize = 7,
  combinationWindow = 1,
  minPostCombinationDuration = 1,
  filterTreatments = "All",
  maxPathLength = 7,
  ageWindow = 10,
  minCellCount = 5,
  censorType = "minCellCount"
)

#===============================================================================================
#Characterization
#===============================================================================================

cModuleSettingsCreator <- CharacterizationModule$new()
cModuleSpecifications <- cModuleSettingsCreator$createModuleSpecifications(
  targetIds = cohortDefinitionSet$cohortId,
  outcomeIds = oList$outcomeCohortId,
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
#PatientLevelPrediction
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
  fileName = "./inst/study_execution_jsons/validation.json"
)
