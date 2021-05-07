module Game.Types where

--------------------------------------------------------------------------------
import Vec
import SF
import Input
--------------------------------------------------------------------------------

-- Hooks.
data Hook = NoHook | HookTravelling (Vec World) | HookAttached (Vec World)
type HookAttachedCheck = Vec World -> Maybe (Vec World)
type HookMode = HookAttachedCheck -> Mode (Input, Vec World, Vec World) Hook

