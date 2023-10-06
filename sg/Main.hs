{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (catch)
import Data.Aeson (toJSON, fromJSON, Result (..))
import Data.ByteString.Char8 (ByteString)
import Lens.Micro (Getting, (^.), ix)
import Network.HTTP.Simple (httpJSON)
import System.Console.Haskeline (runInputT, defaultSettings, getInputLineWithInitial)
import System.Environment (getEnv, getArgs)
import System.Exit (die, exitFailure, exitSuccess)
import System.IO.Error (isDoesNotExistError)
import System.Process (callCommand)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as ByteString
import qualified Network.HTTP.Simple as HTTP
  ( RequestHeaders
  , Request
  , parseRequest
  , setRequestHeaders
  , getResponseBody
  , setRequestBodyJSON
  , setRequestMethod
  )

import SG.Role (Role (..))
import SG.Request (Request (..))
import SG.Message (Message (..))
import SG.Response (Response (..))
import qualified SG.Response as Response
import qualified SG.Message as Message

main :: IO ()
main = do
  apiKey <- getAPIKeyFromEnvironmentVariable
    `catch` handleUnauthenticated
  systemMessage <- getSystemMessage
    `catch` handleSystemMessageMissing
  getArgs >>= \case
    [prompt] -> sendChatCompletionRequest systemMessage apiKey prompt
    _ -> exitFailure

getHTTPRequest :: HTTP.RequestHeaders -> Request -> IO HTTP.Request
getHTTPRequest headers request
  = HTTP.setRequestHeaders headers
  . HTTP.setRequestMethod method
  . HTTP.setRequestBodyJSON (toJSON request)
  <$> HTTP.parseRequest chatCompletionEndpointURL
  where
    method :: ByteString
    method = "POST"

    chatCompletionEndpointURL :: String
    chatCompletionEndpointURL = "https://api.openai.com/v1/chat/completions"

sendChatCompletionRequest :: Message -> String -> String -> IO ()
sendChatCompletionRequest systemMessage apiKey userPrompt = do
  httpRequest <- getHTTPRequest (getRequestHeaders apiKey) request
  httpResonse <- httpJSON httpRequest
  case (fromJSON $ HTTP.getResponseBody httpResonse) of
    Success response -> (confirmCommand $ response^.getFirstChoiceMessageContent) >>= \case
      (Just command) -> callCommand command
      _ -> exitSuccess
    Error e -> die e
  where
    userMessage :: Message
    userMessage = Message
      { role = User
      , content = userPrompt
      }

    request :: Request
    request = Request
      { model
      , messages =
        [ systemMessage
        , userMessage
        ]
      }

model :: String
model = "gpt-4"

getFirstChoiceMessageContent :: Getting String Response String
getFirstChoiceMessageContent = Response.choices . ix 0 . Response.message . Message.content

confirmCommand :: String -> IO (Maybe String)
confirmCommand = runInputT defaultSettings . getInputLineWithInitial mempty . (,mempty)

getRequestHeaders :: String -> HTTP.RequestHeaders
getRequestHeaders apiKey =
  [ ("Content-Type", "application/json")
  , ("Authorization"
    , ByteString.concat ["Bearer ", ByteString.pack apiKey]
    )
  ]

getAPIKeyFromEnvironmentVariable :: IO String
getAPIKeyFromEnvironmentVariable = getEnv apiKeyEnvironmentVariableName

apiKeyEnvironmentVariableName :: String
apiKeyEnvironmentVariableName = "OPENAI_API_KEY"

systemMessagePath :: String
systemMessagePath = "system_message"

getSystemMessage :: IO Message
getSystemMessage = do
  content <- readFile systemMessagePath
  return $! Message
    { role = System
    , content
    }

handleUnauthenticated :: IOError -> IO a
handleUnauthenticated e
  | isDoesNotExistError e = die $ printf
    "Failed to obtain an API key: the %s environment variable is not set"
    apiKeyEnvironmentVariableName
  | otherwise = ioError e

handleSystemMessageMissing :: IOError -> IO a
handleSystemMessageMissing e
  | isDoesNotExistError e = die "Failed to load system message"
  | otherwise = ioError e
