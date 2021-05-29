module Core where

import RIO

data Pipeline
  = Pipeline
    { steps :: NonEmpty Step
    }

data Step
  = Step
    { name :: StepName
    , commands :: NonEmpty Text
    , image :: Image
    }
  deriving (Eq, Show)

data Build
  = Build
    { pipeline :: Pipeline
    , state :: BuildState
    , completedSteps :: Map StepName StepResult
    }

data StepResult
  = StepFailed ContainerExitCode
  | StepSucceeded
  deriving(Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

exitCodeToStepResult :: ContainerExitCode -> StepResult
exitCodeToStepResult exit =
  if exitCodeToInt exit == 0
     then StepSucceded
     else StepFailed exit

data BuildState
  = BuildReady
  | BuildRunning
  | BuildFinished BuildResult
  deriving (Eq, Show)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  deriving (Eq, Show)

newtype StepName = StepName Text
  deriving(Eq, Show, Ord)

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

newtype Image = Image Text
  deriving (Eq, Show)

imageToText :: Image -> Text
imageToText (Image image) = image

progress :: Build -> IO Build
progress build =
  case build.state of
    BuildReady -> undefined -- TODO
    BuildRunning -> undefined -- TODO
    BuildFinished _ ->
      pure build
