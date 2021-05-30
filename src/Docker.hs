module Docker where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import qualified Data.Aeson.Types as Aeson.Types
import qualified Network.HTTP.Simple as HTTP
import qualified Socket
import RIO

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

newtype Image = Image Text
  deriving (Eq, Show)

imageToText :: Image -> Text
imageToText (Image image) = image

data CreateContainerOptions
  = CreateContainerOptions
    { image :: Image
    }

newtype ContainerId = ContainerId Text
  deriving (Eq, Show)

containerIdToText :: ContainerId -> Text
containerIdToText (ContainerId c) = c


-- Docker Service functions

parseResponse
  :: HTTP.Response ByteString
  -> (Aeson.Value -> Aeson.Types.Parser a)
  -> IO a
parseResponse res parser = do
  let result = do
        value <- Aeson.eitherDecodeStrict (HTTP.getResponseBody res)
        Aeson.Types.parseEither parser value

  case result of
    Left e -> throwString e
    Right status -> pure status

createContainer_ :: CreateContainerOptions -> IO ContainerId
createContainer_ options = do
  manager <- Socket.newManager "/var/run/docker.sock"

  let image = imageToText options.image
  let body = Aeson.object
              [ ("Image", Aeson.toJSON image)
              , ("Tty", Aeson.toJSON True)
              , ("Labels", Aeson.object [("quad", "")])
              , ("Cmd", "echo hello")
              , ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"])
              ]

  let req = HTTP.defaultRequest
          & HTTP.setRequestManager manager
          & HTTP.setRequestPath "/v1.40/containers/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body

  let parser = Aeson.withObject "create-container" $ \o -> do
        cId <- o .: "Id"
        pure $ ContainerId cId

  res <- HTTP.httpBS req

  parseResponse res parser

startContainer_ :: ContainerId -> IO ()
startContainer_ container = do
  manager <- Socket.newManager "/var/run/docker.sock"

  let path = "/v1.40/containers/" <> containerIdToText container <> "/start"

  let req = HTTP.defaultRequest
          & HTTP.setRequestManager manager
          & HTTP.setRequestPath (encodeUtf8 path)
          & HTTP.setRequestMethod "POST"

  void $ HTTP.httpBS req


-- Docker Service

data Service
  = Service
    { createContainer :: CreateContainerOptions -> IO ContainerId,
      startContainer :: ContainerId -> IO ()
    }

createService :: IO Service
createService = do
  pure Service
    { createContainer = createContainer_
    , startContainer = startContainer_
    }
