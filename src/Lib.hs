{-# LANGUAGE ImplicitParams, ScopedTypeVariables, RankNTypes, ConstraintKinds, MultiParamTypeClasses #-}

module Lib where

import GHC.Stack
import Unsafe.Coerce
import GHC.Stack.Types
import Control.Exception

emptyCallStack :: IO ()
emptyCallStack = putStrLn $ show callStack

giveCallStack :: HasCallStack => IO ()
giveCallStack = putStrLn $ show callStack

getSrcLoc :: HasCallStack => SrcLoc
getSrcLoc = snd $ head $ getCallStack callStack

mkGithubLink :: HasCallStack => String
mkGithubLink =
    concat
        [ "https://www.github.com/bitemyapp/esqueleto/blob/master/"
        , srcLocFile srcLoc
        , "#L", show $ srcLocStartLine srcLoc
        , "-"
        , "L", show $ srcLocEndLine srcLoc
        ]
  where
    srcLoc = getSrcLoc

mkGithubLinkFrozen :: HasCallStack => String
mkGithubLinkFrozen = withThawedCallStack $
    concat
        [ "https://www.github.com/bitemyapp/esqueleto/blob/master/"
        , srcLocFile srcLoc
        , "#L", show $ srcLocStartLine srcLoc
        , "-"
        , "L", show $ srcLocEndLine srcLoc
        ]
  where
    srcLoc = withFrozenCallStack getSrcLoc

wat :: HasCallStack => IO ()
wat = do
    wrap "unfrozen" (printSrcLoc getSrcLoc)
    withFrozenCallStack $ wrap "dolla" (printSrcLoc getSrcLoc)
    withFrozenCallStack wrap "undolla" (printSrcLoc getSrcLoc)

printSrcLoc :: SrcLoc -> IO ()
printSrcLoc = putStrLn . prettySrcLoc

wrap :: HasCallStack => String -> IO a -> IO a
wrap message action = do
    putStrLn $ concat
        [ "Beginning ", message
        , ", called at ", prettySrcLoc getSrcLoc
        ]
    a <- action
    putStrLn $ "Ending " <> message
    pure a

thawCallStack :: CallStack -> CallStack
thawCallStack stack =
    case stack of
        FreezeCallStack stk -> stk
        _ -> stack

withThawedCallStack :: HasCallStack => (HasCallStack => r) -> r
withThawedCallStack action =
    let ?callStack = popCallStack (thawCallStack callStack)
     in action

withSrcLoc :: (HasCallStack => String -> r) -> r
withSrcLoc mkStr = withThawedCallStack (mkStr (withFrozenCallStack mkGithubLinkFrozen))

boom :: Int
boom = error "oh no"

boomStack :: HasCallStack => Int
boomStack = error "oh no, but with a trace"

moreContextPlease :: IO ()
moreContextPlease =
    addStackFrame $ do
        print boom

moreContextPleaseStacked :: HasCallStack => IO ()
moreContextPleaseStacked =
    addStackFrame $ do
        print boomStack

mkStackFrameLines :: CallStack -> [String]
mkStackFrameLines =
    map formatFrame . getCallStack
  where
    formatFrame (fn, srcLoc) =
        fn <> ", called at " <> prettySrcLoc srcLoc

addStackFrame :: HasCallStack => IO a -> IO a
addStackFrame action = do
    let newLines =
            map ("  " <>) $ mkStackFrameLines callStack
        appendLoc locs =
            unlines
                (locs : newLines)
    action `catch` \(ErrorCallWithLocation err loc) ->
        throwIO $ ErrorCallWithLocation err (appendLoc loc)
