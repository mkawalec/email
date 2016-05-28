module Main where


import React.Flux
import Frontend.MessageSidebar

main :: IO ()
main = reactRender "messages" messageSidebar ()
