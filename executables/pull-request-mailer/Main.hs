{-

Copyright 2014 Google Inc. All rights reserved.

Use of this source code is governed by a BSD-style
license that can be found in the LICENSE file or at
https://developers.google.com/open-source/licenses/bsd

-}

module Main where

import Control.Monad
import Options.Applicative

import Github.PullRequests.Emailer
import Github.PullRequests.Emailer.Opts


main :: IO ()
main = do
  ( prid,
    opts@Opts
      { optsRecipient          = recipient
      , optsPostCheckoutHook   = checkoutHookCmd
      , optsAuth               = m'auth
      }
    ) <- parseOptsAndEnv
           (\optsParse -> (,) <$> pridParser <*> optsParse)
           (progDesc "Sends a GitHub pull request as a patch series via email")

  -- When checking command line arguments for consistency, make sure to handle
  -- any conflicts before doing any IO.

  case opts of
    Opts { optsNoThreadTracking = True } ->
      void $ pullRequestToThread m'auth prid recipient checkoutHookCmd

    Opts{ optsNoThreadTracking = False, optsAuth = Nothing } ->
      die "No authentication token was given, so we cannot track\
          \the email thread on Github and future versions of the pull\
          \request cannot be sent in reply to this one.\n\
          \Pass --no-thread-tracking if you want this."

    Opts{ optsNoThreadTracking = False, optsDiscussionLocation = Nothing } ->
      die "No --discussion-location was given. It is needed for thread\
          \ tracking. Pass --no-thread-tracking if you don't need it."

    Opts { optsAuth               = Just auth
         , optsDiscussionLocation = Just loc
         , optsNoThreadTracking   = False
         } -> do
      pullRequestToThread m'auth prid recipient checkoutHookCmd
        >>= postEmailerInfoComment auth prid loc
