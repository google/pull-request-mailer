{-

Copyright 2014 Google Inc. All rights reserved.

Use of this source code is governed by a BSD-style
license that can be found in the LICENSE file or at
https://developers.google.com/open-source/licenses/bsd

-}

{-# LANGUAGE DeriveGeneric #-}

module Github.PullRequests.Emailer.MsgId
  ( MsgId(fromMsgId) -- do not export constructor, see `MsgId`
  , mkMsgId
  ) where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import qualified Text.Parsec as P
import qualified Text.ParserCombinators.Parsec.Rfc2822 as Rfc2822


-- | A string that is known to be a valid "msg-id" according to RFC 2822.
--
-- We call this @MsgId@ instead of @MessageId@ because in RFC 2822,
-- @message-id@ means something of form "Message-Id: msg-id", and this
-- data type shall only represent the inner msg-id part.
--
-- We care a lot about the constructor not being exported because we pass
-- this to `git format-patch --in-reply-to=` which allows inserting arbitrary
-- strings into the generated emails (email header injection), so we need
-- to ensure that any `MsgId` has been validated.
--
-- See also:
--   <http://en.wikipedia.org/wiki/Email_injection>
--   <http://permalink.gmane.org/gmane.comp.version-control.git/256483>
newtype MsgId = MsgId
  { fromMsgId :: String
  } deriving (Eq, Ord, Show, Generic)


-- | Checks whether a string is a valid @msg-id@ according to RFC 2822.
mkMsgId :: (Monad m) => String -> m MsgId
mkMsgId s = case P.runParser Rfc2822.msg_id () "" s of
  Left _      -> fail $ "Invalid msg-id: " ++ show s
  Right msgId -> return $ MsgId msgId


instance FromJSON MsgId where
  parseJSON (String s) = mkMsgId (T.unpack s)
  parseJSON _          = fail "msg-id must be a string in JSON"
instance ToJSON MsgId where
  toJSON = toJSON . fromMsgId
