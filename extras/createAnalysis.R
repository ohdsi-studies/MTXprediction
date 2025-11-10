library(Strategus) # need https://github.com/OHDSI/Strategus/tree/v1.0-plpv-mt-modules branch
library(CohortIncidence)


#====================================================================================
#COHORT IDS
#====================================================================================

targetIds <- c(21773, 21769, 21770, 21772,21771)
outcomeIds <- c(21803, 21804, 21805, 21806,21669, 21670, 21671, 21672, 21673, 21674, 21675, 21676, 21677, 21678, 21774, 21775, 21776, 21777, 21778, 21779, 21780, 21781, 21782, 21783, 21784, 21789, 21790, 21791, 21792, 21793, 21794, 21795, 21796, 21797, 21799, 21800, 21801, 21802, 22003)

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

ciModuleSettingsCreator <- CohortIncidenceModule$new()

targets <- list(
  CohortIncidence::createCohortRef(id=21773,name="Inflammatory diseases"), 
  CohortIncidence::createCohortRef(id=21769,name="Psoriasis"), 
  CohortIncidence::createCohortRef(id=21770,name="Atopic dermatitis"), 
  CohortIncidence::createCohortRef(id=21772,name="Rheumatoid arthritis"),
  CohortIncidence::createCohortRef(id=21771,name="Psoriatic arthritis") 
)

outcomes <- list(
  CohortIncidence::createOutcomeDef(id=1,name="Drugscombined",cohortId=21803),
  CohortIncidence::createOutcomeDef(id=2,name="Psoriasisdrugs",cohortId=21804),
  CohortIncidence::createOutcomeDef(id=3,name="Atopicdermatitisdrugs",cohortId=21805),
  CohortIncidence::createOutcomeDef(id=4,name="RA&PAdrugs",cohortId=21806)
)

tars <- list(
  CohortIncidence::createTimeAtRiskDef(id=1,startWith="start", startOffset = 0, endWith="end", endOffset = 183), 
  CohortIncidence::createTimeAtRiskDef(id=2,startWith="start", startOffset = 0, endWith="end",endOffset=365) 
  )

analysis1 <- CohortIncidence::createIncidenceAnalysis(
  targets = c(21773,21769,21770,21772,21771),
  outcomes = c(1,2,3,4),
  tars = c(1,2)
)

irDesign <- CohortIncidence::createIncidenceDesign(
  targetDefs = targets,
  outcomeDefs = outcomes,
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

ciModuleSpecifications <- ciModuleSettingsCreator$createModuleSpecifications(
  irDesign = irDesign$toList()
)

#===============================================================================================
#TreatmentPatterns
#===============================================================================================
#remotes::install_github("darwin-eu-dev/TreatmentPatterns@338")

# Create dataframe for TreatmentPatterns
cohorts <- cohortDefinitionSet |>
  select (cohortId, cohortName)

oList <- cohortDefinitionSet |>
  filter(.data$cohortId %in% outcomeIds) |>
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) |>
  select(outcomeCohortId, outcomeCohortName) |>
  mutate(cleanWindow = 0)

cohorts$type <- ifelse(cohorts$cohortId %in% oList$outcomeCohortId, 'event', 'target')

#TREATMENTPATTERNSAD

cohortsAD <- cohorts[c(3,11,26:33, 44),]

# Remove certain IDs to try to overcome stuck
#cohorts = cohorts%>%filter(cohortId>...)

tpModuleSettingsCreatorAD <- TreatmentPatternsModule$new()
tpModuleSpecificationsAD <- tpModuleSettingsCreatorAD$createModuleSpecifications(
  cohorts = cohortsAD, 
  minEraDuration = 30,
  splitEventCohorts = NULL,
  splitTime = NULL,
  eraCollapseSize = 30,
  combinationWindow = 30,
  minPostCombinationDuration = 30,
  filterTreatments = "First",
  maxPathLength = 5, 
  ageWindow = 10, 
  minCellCount = 5, 
  censorType = "minCellCount",
  overlapMethod = "truncate",
  concatTargets = FALSE,
  startAnchor = "startDate",
  windowStart = 0,
  endAnchor = "endDate",
  windowEnd = 365
)


#TREATMENTPATTERNSP

cohortsP <- cohorts[c(2, 10:25, 44),]

# Remove certain IDs to try to overcome stuck
#cohorts = cohorts%>%filter(cohortId>...)

tpModuleSettingsCreatorP <- TreatmentPatternsModule$new()
tpModuleSpecificationsP <- tpModuleSettingsCreatorP$createModuleSpecifications(
  cohorts = cohortsP, 
  minEraDuration = 30,
  splitEventCohorts = NULL,
  splitTime = NULL,
  eraCollapseSize = 30,
  combinationWindow = 30,
  minPostCombinationDuration = 30,
  filterTreatments = "First",
  maxPathLength = 5, 
  ageWindow = 10, 
  minCellCount = 5, 
  censorType = "minCellCount",
  overlapMethod = "truncate",
  concatTargets = FALSE,
  startAnchor = "startDate",
  windowStart = 0,
  endAnchor = "endDate",
  windowEnd = 365
)

#TREATMENTPATTERNSPA

cohortsPA <- cohorts[c(5,13,14,23,24,31,32,34,35,37:44),]

# Remove certain IDs to try to overcome stuck
#cohorts = cohorts%>%filter(cohortId>...)

tpModuleSettingsCreatorPA <- TreatmentPatternsModule$new()
tpModuleSpecificationsPA <- tpModuleSettingsCreatorPA$createModuleSpecifications(
  cohorts = cohortsPA, 
  minEraDuration = 30,
  splitEventCohorts = NULL,
  splitTime = NULL,
  eraCollapseSize = 30,
  combinationWindow = 30,
  minPostCombinationDuration = 30,
  filterTreatments = "First",
  maxPathLength = 5, 
  ageWindow = 10, 
  minCellCount = 5, 
  censorType = "minCellCount",
  overlapMethod = "truncate",
  concatTargets = FALSE,
  startAnchor = "startDate",
  windowStart = 0,
  endAnchor = "endDate",
  windowEnd = 365
)

#TREATMENTPATTERNSRA

cohortsRA <- cohorts[c(4,13,14,23,24,31,32,34,35,37:44),]

# Remove certain IDs to try to overcome stuck
#cohorts = cohorts%>%filter(cohortId>...)

tpModuleSettingsCreatorRA <- TreatmentPatternsModule$new()
tpModuleSpecificationsRA <- tpModuleSettingsCreatorRA$createModuleSpecifications(
  cohorts = cohortsRA, 
  minEraDuration = 30,
  splitEventCohorts = NULL,
  splitTime = NULL,
  eraCollapseSize = 30,
  combinationWindow = 30,
  minPostCombinationDuration = 30,
  filterTreatments = "First",
  maxPathLength = 5, 
  ageWindow = 10, 
  minCellCount = 5, 
  censorType = "minCellCount",
  overlapMethod = "truncate",
  concatTargets = FALSE,
  startAnchor = "startDate",
  windowStart = 0,
  endAnchor = "endDate",
  windowEnd = 365
)

#splitEventCohorts = NULL,
#splitTime = NULL,
#eraCollapseSize = 30,
#overlapMethod = "truncate",
#concatTargets = TRUE, 
#startAnchor = "startDate",
#windowStart = 0,
#endAnchor = "endDate",
#windowEnd = 0
#===============================================================================================
#Characterization
#===============================================================================================

cModuleSettingsCreator <- CharacterizationModule$new()
cModuleSpecifications <- cModuleSettingsCreator$createModuleSpecifications(
  targetIds = c(21773, 21769, 21770, 21772,21771),
  outcomeIds = 21803,
  minPriorObservation = 365,
  outcomeWashoutDays = 365,
  dechallengeStopInterval = 30,
  dechallengeEvaluationWindow = 30,
  riskWindowStart = 0, 
  startAnchor = 'cohort start', 
  riskWindowEnd = 365, 
  endAnchor = 'cohort start',
  covariateSettings = FeatureExtraction::createDefaultCovariateSettings(),
  minCharacterizationMean = .01
  )

#===============================================================================================
#PatientLevelPrediction
#===============================================================================================

plpModule <- PatientLevelPredictionModule$new()

makeModelDesignSettings <- function(targetId, outcomeId, popSettings, covarSettings) {
  invisible(PatientLevelPrediction::createModelDesign(
    targetId = 21770,
    outcomeId = 21805,
    restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(), #NOG NAAR KIJKEN
    populationSettings = popSettings, #NOG NAAR KIJKEN
    covariateSettings = covarSettings, #NOG NAAR KIJKEN
    preprocessSettings = PatientLevelPrediction::createPreprocessSettings(), #NOG NAAR KIJKEN
    modelSettings = PatientLevelPrediction::setLassoLogisticRegression(),
    splitSettings = PatientLevelPrediction::createDefaultSplitSetting(),
    runCovariateSummary = T
  ))
}

plpPopulationSettings <- PatientLevelPrediction::createStudyPopulationSettings(
  startAnchor = "cohort start",
  riskWindowStart = 1,
  endAnchor = "cohort start",
  riskWindowEnd = 365,
  minTimeAtRisk = 1
)
plpCovarSettings <- FeatureExtraction::createDefaultCovariateSettings()

modelDesignList <- list()
for (i in 1:length(exposureOfInterestIds)) {
  for (j in 1:length(outcomeOfInterestIds)) {
    modelDesignList <- append(
      modelDesignList,
      list(
        makeModelDesignSettings(
          targetId = exposureOfInterestIds[i],
          outcomeId = outcomeOfInterestIds[j],
          popSettings = plpPopulationSettings,
          covarSettings = plpCovarSettings
        )
      )
    )
  }
}

plpModuleSpecifications <- plpModule$createModuleSpecifications(
  modelDesignList = modelDesignList
)

#===============================================================================================
#ANALYSIS SPECIFICATIONS CDM MODULES
#===============================================================================================

analysisSpecifications <- createEmptyAnalysisSpecifications() |>
  addSharedResources(cohortSharedResourcesSpecifications) |>
  addCohortDiagnosticsModuleSpecifications(cdModuleSpecifications) |>
  addCohortGeneratorModuleSpecifications(cgModuleSpecifications) |>
  addCohortIncidenceModuleSpecifications(ciModuleSpecifications) |>
  addTreatmentPatternsModuleSpecifications(tpModuleSpecificationsAD) |>
  #addTreatmentPatternsModuleSpecifications(tpModuleSpecificationsP) |>
  #addTreatmentPatternsModuleSpecifications(tpModuleSpecificationsPA) |>
  #addTreatmentPatternsModuleSpecifications(tpModuleSpecificationsRA) |>
  addCharacterizationModuleSpecifications(cModuleSpecifications) |>
  addModuleSpecifications(plpModuleSpecifications)

ParallelLogger::saveSettingsToJson(
  object = analysisSpecifications,
  fileName = "C:/Users/ASaelman/MTXpredictionnetwork/inst/study_execution_jsons/validation.json"
)
