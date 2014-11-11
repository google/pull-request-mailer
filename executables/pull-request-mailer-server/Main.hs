{-

Copyright 2014 Google Inc. All rights reserved.

Use of this source code is governed by a BSD-style
license that can be found in the LICENSE file or at
https://developers.google.com/open-source/licenses/bsd

-}

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (for_)
import Data.Monoid
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL
import Github.Auth
import Github.PullRequests
import Github.Repos.Webhooks.Validate (isValidPayload)
import Network.HTTP.Types.Status (forbidden403)
import Options.Applicative hiding (header)
import System.IO (stderr)
import System.Posix.Process (forkProcess, getProcessStatus)
import Web.Scotty

import Github.PullRequests.Emailer
import Github.PullRequests.Emailer.Opts


-- | A helper function that parses given data to a JSON object.
parse :: (A.FromJSON a) => BL.ByteString -> ActionM a
parse payload =
    maybe (raise $ "jsonData - no parse: " <> TL.decodeUtf8 payload) return
    . A.decode $ payload


main :: IO ()
main = do
  opts <- parseOptsAndEnv id
            -- TODO udate description for the server
            ( progDesc "Receive GitHub pull request webbooks and send\
                       \ the patch series via email"
            )

  case opts of
    Opts { optsSecret             = Nothing } ->
      die $ "The server needs to have " ++ secretEnvVar ++ " set to verify\
            \ GitHub's webhooks."

    Opts { optsNoThreadTracking   = False
         , optsAuth               = Nothing
         } ->
      die $ "Thread tracking requires " ++ tokenEnvVar ++ " to be set."

    Opts { optsNoThreadTracking   = False
         , optsDiscussionLocation = Nothing
         } ->
      die $ "Thread tracking requires --discussion-location."

    -- Passive mode (no thread tracking).
    Opts { optsNoThreadTracking   = True
         , optsAuth               = m'auth
         , optsSecret             = Just secret
         , optsRecipient          = recipient
         , optsPostCheckoutHook   = checkoutHookCmd
         } ->
      pullRequestToThreadServer m'auth secret recipient checkoutHookCmd Nothing

    -- Normal mode (thread tracking).
    Opts { optsNoThreadTracking   = False
         , optsAuth               = m'auth@(Just _)
         , optsSecret             = Just secret
         , optsDiscussionLocation = m'loc@(Just _)
         , optsRecipient          = recipient
         , optsPostCheckoutHook   = checkoutHookCmd
         } ->
      pullRequestToThreadServer m'auth secret recipient checkoutHookCmd m'loc


-- | Runs an action in a separate unix process. Blocks until finished.
forkWait :: IO () -> IO ()
forkWait f = forkProcess f >>= void . getProcessStatus True False


pullRequestToThreadServer :: Maybe GithubAuth -- ^ Github authentication
                          -> String           -- ^ Hook verification secret
                          -> String           -- ^ recipient email address
                          -> Maybe String     -- ^ post-checkout hook program
                          -> Maybe String     -- ^ discussion location; Nothing
                                              --   disables posting/tracking
                          -> IO ()
pullRequestToThreadServer m'auth
                          secret
                          recipient
                          checkoutHookCmd
                          m'discussionLocation =

  scotty 8014 $ do
    -- The exception-catching `defaultHandler` must come before the other
    -- routes to catch its exceptions, see http://stackoverflow.com/q/26747855
    defaultHandler $ \errText -> do
      -- We do not want to disclose the text of IO exceptions to the client;
      -- we log them to stderr instead.
      liftIO $ TL.hPutStrLn stderr errText
      text "Internal server error\r\n"

    post "/" $ do
      digest <- fmap TL.unpack <$> header "X-Hub-Signature"
      payload <- body

      if isValidPayload secret digest (BL.toStrict payload)
        then do
          run payload
          text ""
        else do
          status forbidden403
          text "Invalid or missing hook verification digest"
  where
    run payload = do

      pre <- parse payload :: ActionM PullRequestEvent

      when (pullRequestEventAction pre `elem`
            [ PullRequestOpened, PullRequestSynchronized ]) . liftIO $ do
        -- TODO: Logging
        let pr = pullRequestEventPullRequest pre
            prid = detailedPullRequestToPRID pr

        -- Fork process so that cd'ing into temporary directories doesn't
        -- change the cwd of the server.
        forkWait $ do
          -- Pull code, send the mail.
          tInfo <- pullRequestToThread m'auth prid recipient checkoutHookCmd

          -- Post comment into PR if enabled and we have auth.
          for_ m'auth $ \auth ->
            for_ m'discussionLocation $ \discussionLocation ->
              postEmailerInfoComment auth prid discussionLocation tInfo
