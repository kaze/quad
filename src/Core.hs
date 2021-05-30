module Core where

import qualified Docker
import RIO
import qualified RIO.List as List
import qualified RIO.Map as Map

data Pipeline
  = Pipeline
    { steps :: NonEmpty Step
    }

data Step
  = Step
    { name :: StepName
    , commands :: NonEmpty Text
    , image :: Docker.Image
    }
  deriving (Eq, Show)

data Build
  = Build
    { pipeline :: Pipeline
    , state :: BuildState
    , completedSteps :: Map StepName StepResult
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
    }
  deriving (Eq, Show)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  deriving (Eq, Show)

newtype StepName = StepName Text
  deriving(Eq, Show, Ord)

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step


progress :: Docker.Service -> Build -> IO Build
progress docker build =
  case build.state of
    BuildReady ->
      case buildHasNextStep build of
        Left result ->
          pure $ build{state = BuildFinished result}
        Right step -> do
          let options = Docker.CreateContainerOptions step.image
          container <- docker.createContainer options
          docker.startContainer container

          let s = BuildRunningState { step = step.name }
          pure $ build{state = BuildRunning s }

    BuildRunning state -> do
      let exit = Docker.ContainerExitCode 0
          result = exitCodeToStepResult exit

      pure build
        { state = BuildReady
        , completedSteps
          = Map.insert state.step result build.completedSteps
        }

    BuildFinished _ ->
      pure build