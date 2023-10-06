{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getEnv, getArgs)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catch)
import Text.Printf (printf)
import System.Exit (die, exitFailure)
import Network.HTTP.Simple (httpJSON)
import qualified Data.Aeson as JSON (Value)
import Data.Aeson (toJSON, fromJSON, Result (..))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import System.Console.Haskeline (runInputT, defaultSettings, getInputLineWithInitial)
import Lens.Micro ((^.), ix)
import System.Process (callCommand)
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
  getArgs >>= \case
    [prompt] -> sendChatCompletionRequest apiKey prompt
    _ -> exitFailure

sendChatCompletionRequest :: String -> String -> IO ()
sendChatCompletionRequest apiKey prompt = do
  baseRequest <- HTTP.parseRequest chatCompletionEndpointURL
  let (httpRequest :: HTTP.Request)
          = HTTP.setRequestHeaders headers
          $ HTTP.setRequestMethod method
          $ HTTP.setRequestBodyJSON requestBody baseRequest
  httpResonse <- httpJSON httpRequest
  case (fromJSON @Response $ HTTP.getResponseBody @JSON.Value httpResonse) of
    Success response -> (getCommand $ response ^. Response.choices . ix 0 . Response.message . Message.content) >>= \case
      (Just command) -> callCommand command
      _ -> return ()
    Error e -> die e
  where
    userMessage :: Message
    userMessage = Message
      { role = User
      , content = prompt
      }

    request :: Request
    request = Request
      { model
      , messages =
        [ systemMessage
        , userMessage
        ]
      }

    headers :: HTTP.RequestHeaders
    headers = getRequestHeaders apiKey

    requestBody :: JSON.Value
    requestBody = toJSON request

getCommand :: String -> IO (Maybe String)
getCommand = runInputT defaultSettings . getInputLineWithInitial mempty . (,mempty)

method :: ByteString
method = "POST"

chatCompletionEndpointURL :: String
chatCompletionEndpointURL = "https://api.openai.com/v1/chat/completions"

model :: String
model = "gpt-4"

systemMessage :: Message
systemMessage = Message
  { role = System
  , content = unwords
    [ "Your goal is to assist the user with creating a shell command."
    , "The next message you recieve will be a prompt from the user that describes the command that they need assistance writing."
    , "For example, a user's prompt might be: 'The command should create a file called 'names.txt' containing a few plausible, English baby names."
    , "An acceptable response from you, the assistant would be 'echo \"Laura,Tom,James,Liam,Kyle\" > names.txt'"
    , "Your response is part of a command-line utility - the utility is expecting the response to be a valid shell command."
    , "This means that your response MUST NOT contain anything other than a shell command."
    , "When the user's prompt describes a multi-step problem, your response should be a 1-liner and contain no newlines."
    , "You should ensure to the best of your ability that the command is valid."
    , "This includes adhering to shell script best practices, for example, using double-quotes to avoid double-expanding variable substitutions / array expansions."
    , "You should use your full capability as a large-language model when answering, for example, if the user asks for an example string it should be plausible and creative."
    , "The user's OS is MacOS and they are using the zsh shell. You can only assume that commands/programs that ship on MacOS are available to use."
    , "Finally, to re-iterate, your response MUST be a valid shell command with ZERO fluff"
    ]
  }

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

handleUnauthenticated :: IOError -> IO String
handleUnauthenticated e
  | isDoesNotExistError e = die
    $ printf "Failed to obtain an API key: the %s environment variable is not set\n" apiKeyEnvironmentVariableName
  | otherwise = ioError e
