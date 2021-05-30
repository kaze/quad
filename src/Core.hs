module Core where

import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock.POSIX as Time
import qualified Docker
import RIO
import qualified RIO.List as List
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NonEmpty
import qualified RIO.Text as Text

data Pipeline
  = Pipeline
    { steps :: NonEmpty Step
    }
  deriving(Eq, Show, Generic, Aeson.FromJSON)

data Step
  = Step
    { name :: StepName
    , commands :: NonEmpty Text
    , image :: Docker.Image
    }
  deriving (Eq, Show, Generic, Aeson.FromJSON)

data Build
  = Build
    { pipeline :: Pipeline
    , state :: BuildState
    , completedSteps :: Map StepName StepResult
    , volume :: Docker.Volume
    }

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build =
  if allSucceeded
    then case nextStep of
      Just step -> Right step
      Nothing -> Left BuildSucceeded
    else Left BuildFailed
  where
    allSucceeded = List.all ((==) StepSucceeded) build.completedSteps
    nextStep = List.find f build.pipeline.steps
    f step = not $ Map.member step.name build.completedSteps

data StepResult
  = StepFailed Docker.ContainerExitCode
  | StepSucceeded
  deriving(Eq, Show)

exitCodeToStepResult :: Docker.ContainerExitCode -> StepResult
exitCodeToStepResult exit =
  if Docker.exitCodeToInt exit == 0
     then StepSucceeded
     else StepFailed exit

data BuildState
  = BuildReady
  | BuildRunning BuildRunningState
  | BuildFinished BuildResult
  deriving (Eq, Show)

data BuildRunningState
  = BuildRunningState
    { step :: StepName
    , container :: Docker.ContainerId
    }
  deriving (Eq, Show)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  | BuildUnexpectedState Text
  deriving (Eq, Show)

newtype StepName = StepName Text
  deriving(Eq, Show, Ord, Generic, Aeson.FromJSON)

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

data Log = Log
  { output :: ByteString
  , step :: StepName
  }
  deriving (Eq, Show)

type LogCollection = Map StepName CollectionStatus

data CollectionStatus
  = CollectionReady
  | CollectingLogs Docker.ContainerId Time.POSIXTime
  | CollectionFinished
  deriving (Eq, Show)

initLogCollection :: Pipeline -> LogCollection
initLogCollection pipeline =
  Map.fromList $ NonEmpty.toList steps
  where
    steps = pipeline.steps <&> \step -> (step.name, CollectionReady)

updateLogCollection :: BuildState -> Time.POSIXTime -> LogCollection -> LogCollection
updateLogCollection state lastCollection collection =
  Map.mapWithKey f collection
  where
    update step since nextState =
      case state of
        BuildRunning state ->
          if state.step == step
            then CollectingLogs state.container since
            else nextState
        _ -> nextState

    f step = \case
      CollectionReady ->
        update step 0 CollectionReady

      CollectingLogs _ _ ->
        update step lastCollection CollectionFinished

      CollectionFinished ->
        CollectionFinished

runLogCollection :: Docker.Service -> Time.POSIXTime -> LogCollection -> IO [Log]
runLogCollection docker collectionUntil collection = do
  logs <- Map.traverseWithKey f collection
  pure $ concat (Map.elems logs)
  where
    f step = \case
      CollectionReady -> pure []

      CollectionFinished -> pure []

      CollectingLogs container since -> do
        let options = Docker.FetchLogOptions
                        { container = container
                        , since = since
                        , until = collectionUntil
                        }

        output <- docker.fetchLogs options

        pure [Log {step = step, output = output}]

collectLogs :: Docker.Service -> LogCollection -> Build -> IO (LogCollection, [Log])
collectLogs docker collection build = do
  now <- Time.getPOSIXTime
  logs <- runLogCollection docker now collection
  let newCollection = updateLogCollection build.state now collection
  pure (newCollection, logs)

progress :: Docker.Service -> Build -> IO Build
progress docker build =
  case build.state of
    BuildReady ->
      case buildHasNextStep build of
        Left result ->
          pure $ build{state = BuildFinished result}

        Right step -> do
          let script = Text.unlines $ ["set -ex"] <> NonEmpty.toList step.commands
          let options = Docker.CreateContainerOptions
                          {image = step.image
                          , script = script
                          , volume = build.volume
                          }
          docker.pullImage step.image
          container <- docker.createContainer options
          docker.startContainer container

          let s = BuildRunningState
                    { step = step.name
                    , container = container
                    }
          pure $ build{state = BuildRunning s }

    BuildRunning state -> do
      status <- docker.containerStatus state.container

      case status of
        Docker.ContainerRunning ->
          pure build

        Docker.ContainerExited exit -> do
          let result = exitCodeToStepResult exit
          pure build
                { completedSteps = Map.insert state.step result build.completedSteps
                , state = BuildReady
                }

        Docker.ContainerOther other -> do
          let s = BuildUnexpectedState other
          pure build {state = BuildFinished s}

    BuildFinished _ ->
      pure build
