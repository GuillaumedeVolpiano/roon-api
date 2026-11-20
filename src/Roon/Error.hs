-- |
-- Module      : Roon.Error
-- Description : What can go wrong when talking to a Roon Server
-- License     : BSD-3-Clause
module Roon.Error
  ( RoonError (..)
  ) where

import Data.Text (Text)
import Roon.Moo.Message (MooParseError)

-- | Failures reported through the @Error RoonError@ effect.
data RoonError
  = -- | The WebSocket could not be opened. Carries the cause.
    ConnectFailed Text
  | -- | The connection closed, or the server stopped answering pings.
    ConnectionLost
  | -- | The server sent a frame that is not a MOO message.
    ProtocolViolation MooParseError
  | -- | A request was answered with an unexpected status. Carries the
    -- request name, then the reply name.
    UnexpectedReply Text Text
  | -- | A reply had the expected status but not the expected body.
    -- Carries the request name, then the decoding error.
    UnexpectedReplyBody Text Text
  | -- | No reply came in time. Carries the request name.
    TimedOut Text
  | -- | Browsing ended on an error message from Roon. Carries the message.
    BrowseFailed Text
  | -- | An item expected while browsing, such as an action, is not there.
    -- Carries what was looked for.
    BrowseItemMissing Text
  deriving stock (Eq, Show)
