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
import Github.Auth
import Github.PullRequests
import Github.Repos.Webhooks.Validate (isValidPayload)
import Network.HTTP.Types.Status (forbidden403)
import Options.Applicative hiding (header)
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
    Opts { optsAuth               = m'auth
         , optsSecret             = Just secret
         , optsDiscussionLocation = m'loc
         , optsNoThreadTracking   = trackingOff
         , optsRecipient          = recipient
         , optsPostCheckoutHook   = checkoutHookCmd
         } ->
      pullRequestToThreadServer m'auth secret recipient checkoutHookCmd
                                (if trackingOff then Nothing else m'loc)
    _ -> die $ "The server needs to have " ++ secretEnvVar ++ " set to verify\
               \ GitHub's webhooks."


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
