{-

Copyright 2014 Google Inc. All rights reserved.

Use of this source code is governed by a BSD-style
license that can be found in the LICENSE file or at
https://developers.google.com/open-source/licenses/bsd

-}

module Github.PullRequests.Emailer.Opts
  ( Opts(..)
  , optsParser
  , pridParser
  ) where

import Github.Auth
import Options.Applicative

import Github.PullRequests.Emailer


-- | Command line arguments to this program.
data Opts = Opts
  { optsRecipient          :: String
  , optsPostCheckoutHook   :: Maybe String
  , optsAuth               :: Maybe GithubAuth
  , optsNoThreadTracking   :: Bool
  , optsDiscussionLocation :: Maybe String
  } deriving (Eq, Ord, Show)


-- | Command line argument parser.
optsParser :: Parser Opts
optsParser = Opts
  <$> strOption
        ( long "to"
          <> metavar "EMAIL"
          <> help "Email recipient"
        )
  <*> optional (strOption
        ( long "post-checkout-hook"
          <> metavar "PROGRAM"
          <> help "A program in the cloned direcotry just after checkout"
        )
      )
  <*> optional (GithubOAuth <$> strOption
        ( long "github-oauth-token"
          <> metavar "TOKEN"
          <> help "Auth token needed to post information to the pull request.\
                  \ You can generate one at\
                  \ https://github.com/settings/applications"
        )
      )
  <*> switch
        ( long "no-thread-tracking"
          <> help "Disable posting thread message ID and patch iteration\
                  \ count into the pull request. When active, future versions\
                  \ of the PR can not be sent as reply to the created email\
                  \ thread"
        )
  <*> optional (strOption
        ( long "discussion-location"
          <> metavar "STRING"
          <> help "The place where the contents of the PR are discussed (as\
                  \ opposed to the discussion being in PR comments. Example:\
                  \ 'the mailing list project@example.com'."
        )
      )


-- | Command line argument parser for pull request identifiers.
pridParser :: Parser PRID
pridParser =
  PRID
    <$> argument str
          ( metavar "USER"
            <> help "GitHub user who owns the repo containing the pull request"
          )
    <*> argument str
          ( metavar "REPO"
            <> help "Repo containing the pull request"
          )
    <*> argument auto
          ( metavar "N"
            <> help "Number of the pull request"
          )
