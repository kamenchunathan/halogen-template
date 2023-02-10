module App where

import Prelude

import Data.Int (decimal, toStringAs)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = { count :: Int }

initialState :: forall input. input -> State
initialState _ = { count: 0 }

data Action = Increment | Decrement

handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Increment -> H.modify_ \{ count } -> { count: count + 1 }
  Decrement -> H.modify_ \{ count } -> { count: count - 1 }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render { count } =
  HH.div
    [ HP.class_ $ ClassName "w-5/6 mx-auto" ]
    [ HH.h1
        [ HP.class_ $ ClassName "text-center text-3xl font-semibold p-4" ]
        [ HH.text "Home page" ]
    , HH.div
        []
        [ HH.h2
            [ HP.class_ $ ClassName "text-center text-lg p-4" ]
            [ HH.text "Purescript, Typescript and Tailwind template" ]
        , HH.p
            [ HP.class_ $ ClassName "text-lg px-4 text-center" ]
            [ HH.text "A template for a Web App Written in purescript using the halogen framework" ]
        , HH.div
            [ HP.class_ $ ClassName "w-4/6 mx-auto text-center py-8" ]
            [ HH.span
                [ HP.class_ $ ClassName "text-xl px-4" ]
                [ HH.text "Counter: " ]
            , HH.span
                [ HP.class_ $ ClassName "text-4xl px-4" ]
                [ HH.text $ toStringAs decimal count ]
            ]
        , HH.div
            [ HP.class_ $ ClassName "flex justify-center space-x-8" ]
            [ HH.button
                [ HP.class_ $ ClassName "text-white text-center text-xl p-2 bg-gray-400 rounded-xl"
                , HE.onClick \_ -> Increment
                ]
                [ HH.text "Increment" ]
            , HH.button
                [ HP.class_ $ ClassName "text-white text-center text-xl p-2 bg-gray-400 rounded-xl"
                , HE.onClick \_ -> Decrement
                ]
                [ HH.text "Decrement" ]
            ]
        ]
    ]

component :: forall q o m. MonadEffect m => H.Component q Unit o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction }
  }
