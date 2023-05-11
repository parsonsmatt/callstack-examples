{-# language BlockArguments, RankNTypes #-}

import Lib
import GHC.Stack (HasCallStack, withFrozenCallStack)

example :: String -> IO () -> IO ()
example msg x =  do
    putStrLn msg
    x
    putStrLn ""

withCallStack :: HasCallStack => (HasCallStack => r) -> r
withCallStack a = a

main :: HasCallStack => IO ()
main = do
    example "emptyCallStack" do
        emptyCallStack

    example "giveCallStack" do
        giveCallStack

    example "withCallStack, giveCallStack" do
        withCallStack giveCallStack

    example "print getSrcLoc" do
        print getSrcLoc

    example "mkGithubLink" do
        putStrLn mkGithubLink

    example "mkGithubLinkFrozen" do putStrLn mkGithubLinkFrozen

    example "wat" wat

    example "frozen githublink" do
        putStrLn (withFrozenCallStack $ withSrcLoc id)

    example "moreContextPlease" do
        moreContextPleaseStacked


