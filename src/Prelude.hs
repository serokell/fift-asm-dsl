-- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | User @universum@ as the default prelude.
module Prelude
       ( module Universum
       ) where

import Universum hiding (drop, (>>), (>>=), swap, MaybeT)