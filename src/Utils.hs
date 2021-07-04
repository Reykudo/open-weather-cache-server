module Utils where

import Control.Monad.Trans.Maybe (MaybeT (MaybeT))

hoistMaybe :: (Monad m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . return
