{-

Copyright 2014 Google Inc. All rights reserved.

Use of this source code is governed by a BSD-style
license that can be found in the LICENSE file or at
https://developers.google.com/open-source/licenses/bsd

-}

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.IO.Class
import Crypto.Hash
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Monoid
import Data.String (IsString(..))
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TL
import Github.Auth
import Github.PullRequests
import Options.Applicative hiding (header)
import Web.Scotty

import Github.PullRequests.Emailer
import Github.PullRequests.Emailer.Opts


-- | A helper function that parses given data to a JSON object.
parse :: (A.FromJSON a) => BL.ByteString -> ActionM a
parse payload =
    maybe (raise $ "jsonData - no parse: " <> TL.decodeUtf8 payload) return
    . A.decode $ payload


-- | Verifies that a given body has been properly signed by GitHub.
verifySecret :: B.ByteString      -- ^ the secret
             -> Maybe TL.Text     -- ^ the hash provided by the remote party
                                  -- (if any), including 'sha1='
             -> BL.ByteString     -- ^ the body
             -> Bool
verifySecret secret shaOpt payload = Just sign == shaOpt
  where
    hm = hmac secret (BL.toStrict payload) :: HMAC SHA1
    sign = "sha1=" <> (fromString . show . hmacGetDigest $ hm) :: TL.Text


main :: IO ()
main = do
  opts <- execParser $ info (helper <*> optsParser)
            ( fullDesc
                -- TODO udate description for the server
                <> progDesc "Receive GitHub pull request webbooks and send\
                            \ the patch series via email"
            )

  case opts of
    Opts { optsAuth               = Just auth
         , optsDiscussionLocation = Just loc
         , optsNoThreadTracking   = False
         , optsRecipient          = recipient
         , optsPostCheckoutHook   = checkoutHookCmd
         } ->
      pullRequestToThreadServer auth recipient checkoutHookCmd loc
    _ -> die "The server needs to have thread tracking enabled and requires\
             \ --github-oauth-token and --discussion-location."


pullRequestToThreadServer :: GithubAuth   -- ^ Github authentication
                          -> String       -- ^ recipient email address
                          -> Maybe String -- ^ post-checkout hook program
                          -> String       -- ^ discussion location
                          -> IO ()
pullRequestToThreadServer auth recipient checkoutHookCmd discussionLocation =

  scotty 8014 $ do
    post "/" $ do
      shaOpt <- header "X-Hub-Signature"
      payload <- body

      {-
      unless (verifySecret mySecret shaOpt payload)
             (raise "Invalid or missing SHA1 sum")
      -}

      pre <- parse payload :: ActionM PullRequestEvent

      when (pullRequestEventAction pre `elem`
            [ PullRequestOpened, PullRequestSynchronized ]) . liftIO $ do
        -- TODO: Logging
        let pr = pullRequestEventPullRequest pre
            prid = detailedPullRequestToPRID pr

        pullRequestToThread (Just auth) prid recipient checkoutHookCmd
          >>= postEmailerInfoComment auth prid discussionLocation

      text ""
