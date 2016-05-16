module Main where

import Prelude

import Control.Monad.Eff (Eff())
import Data.Generic (Generic, gEq, gCompare)
import Data.Functor.Coproduct 
import Data.Maybe

import Halogen
import Halogen.Util (awaitBody, runHalogenAff)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Data.Functor (($>))

data Query a = ToggleState a

data State = State { message :: String }

ui :: forall g. Component State Query g
ui = component { render, eval }
  where

  render :: State -> ComponentHTML Query
  render (State state) =
    H.div_
      [ H.h1_
          [ H.text "Hello world!" ]
      , H.p_
          [ H.text "Why not toggle this button:" ]
      , H.button
          [ E.onClick (E.input_ ToggleState) ]
          [ H.text state.message ]
      ]

  eval :: Natural Query (ComponentDSL State Query g)
  eval (ToggleState next) = do
    modify (\(State state) -> State { message: state.message ++ " next" })
    pure next

derive instance genericSecondSlot :: Generic SecondSlot
instance eqSecondSlot :: Eq SecondSlot where eq = gEq
instance ordSecondSlot :: Ord SecondSlot where compare = gCompare

data PState = PState { message :: String }
data PQuery a = ToggleTick a

type StateP g = ParentState PState State PQuery Query g SecondSlot
type QueryP = Coproduct PQuery (ChildF SecondSlot Query)

newtype SecondSlot = SecondSlot String

initialState :: PState
initialState = PState { message: "I am the one who parents!" }

cpt2 :: forall g. (Functor g) => Component (StateP g) QueryP g
cpt2 = parentComponent { render, eval, peek }
  where

  render :: PState -> ParentHTML State PQuery Query g SecondSlot
  render (PState st) =
    H.div_
      [ 
        H.h1_ 
          [ H.text st.message ]
      , H.button
          [ E.onClick (E.input_ ToggleTick) ]
          [ H.text "clickme" ]
      , H.slot (SecondSlot "A") \_ -> { component: ui, initialState: State { message: "I am the one who childs!" } }
      ]

  eval :: Natural PQuery (ParentDSL PState State PQuery Query g SecondSlot)
  eval (ToggleTick next) = modify (\(PState state) -> PState { message: state.message ++ " lol" }) $> next

  peek = Nothing
    

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI cpt2 (parentState initialState) body
