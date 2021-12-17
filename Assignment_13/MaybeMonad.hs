module MaybeMonad where

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap = fmap

stripMaybe :: Maybe (Maybe a) -> Maybe a
stripMaybe mx = do { x <- mx; x }
--stripMaybe = join
--stripMaybe mx = mx >>= id

applyMaybe:: (a -> Maybe b) -> Maybe a -> Maybe b
applyMaybe f mx = mx >>= f
--applyMaybe = flip (>>=)
--applyMaybe = (=<<)
