module Pux.Devtool where

import Data.List ((:), index, List, singleton)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Control.Monad.Eff (Eff)
import Pux (App, Config, CoreEffects, EffModel, noEffects)
import Pux (start) as Pux
import Pux.CSS hiding (App(..), div, button, span, map, h1)
import Pux.CSS (style) as PuxCSS
import Pux.Html (button, div, h1, Html, span, text, svg, path)
import Pux.Html (style) as PuxHtml
import Pux.Html.Attributes (className, d, viewBox, dangerouslySetInnerHTML)
import Pux.Html.Attributes (style) as PuxAttr
import Pux.Html.Events (onClick)
import Prelude ((#), (<), (>), (<<<), (<>), (+), (-), ($), const, map, negate, not, bind, pure, show)

data Action a
  = AppAction a
  | StepBack
  | StepForward
  | Rewind
  | FastForward
  | Clear
  | ToggleOpen

type State s =
  { actions :: List String
  , states :: List s
  , init :: s
  , length :: Int
  , index :: Int
  , opened :: Boolean
  , width :: Number
  }

type Options = {
  opened :: Boolean
}

defaultOptions :: Options
defaultOptions = {
  opened: true
}

init :: forall s. s -> Options -> State s
init s o = do
  { actions: singleton "App initialized. Awaiting action..."
  , states: singleton s
  , init: s
  , length: 1
  , index: 0
  , opened: o.opened
  , width: 360.0
  }

selectedState :: forall s. State s -> s
selectedState s = fromMaybe s.init (index s.states s.index)

selectedAction :: forall s. State s -> String
selectedAction s = fromMaybe "Awaiting action..." (index s.actions s.index)

foreign import actionToString :: forall a. a -> String

foreign import stateToString :: forall s. s -> String

start :: forall a s e. (Config s a e) -> Options -> Eff (CoreEffects e) (App s (Action a))
start config options = do
  app <- Pux.start
    { initialState: init config.initialState options
    , update: update config.update
    , view: view config.view
    , inputs: map (map AppAction) config.inputs
    }

  pure $ app { state = map selectedState app.state }

update :: forall a s e.
          (a -> s -> EffModel s a e) ->
          Action a -> State s -> EffModel (State s) (Action a) e
update appUpdate (AppAction action) state =
  let effmodel = appUpdate action (selectedState state)
  in
  { state: state
      { actions = actionToString action : state.actions
      , states = effmodel.state : state.states
      , length = state.length + 1
      , index = 0
      }
  , effects: map (map AppAction) effmodel.effects
  }
update appUpdate StepBack state = noEffects
  state
    { index = if (state.index < (state.length - 1))
              then (state.index + 1)
              else (state.length - 1)
    }
update appUpdate StepForward state =
  noEffects
    state { index = if (state.index > 1) then (state.index - 1) else 0 }
update appUpdate Rewind state =
  noEffects $ state { index = state.length - 1 }
update appUpdate FastForward state =
  noEffects $ state { index = 0 }
update appUpdate Clear state =
  noEffects $ state { index = 0
                    , length = 1
                    , states = singleton (selectedState state)
                    , actions = singleton (selectedAction state)
                    }
update appUpdate ToggleOpen state =
  noEffects $ state { opened = not state.opened }

view :: forall s a. (s -> Html a) -> State s -> Html (Action a)
view appView state =
  div
    []
    [ PuxHtml.style [] [ text $ """
        .pux-devtool {
          z-index: 16777271;
        }

        .pux-devtool-container {
          font-family: sans-serif;
          font-size: 14px;
          line-height: 1.5;
          position: relative;
          padding: 1em;
          overflow: scroll;
          height: 100%;
        }

        .pux-devtool-icon {
          display: inline-block;
          height: 1.5em;
        }

        .pux-devtool-icon svg {
          fill: #E6E6E6;
          height: 1em;
          width: 1em;
          vertical-align: middle;
        }

        .pux-devtool-container h1 .pux-devtool-icon {
          margin-right: .25em;
        }

        .pux-devtool-actions {
          position: absolute;
          top: 1em;
          right: 0;
        }

        .pux-devtool-states, .pux-devtool-actions button {
          display: inline-block;
          margin-right: .5em;
          vertical-align: middle;
        }

        .pux-devtool button {
          color: #E6E6E6;
          border: none;
          border-radius: 3px;
          background: #272129;
          padding: .2em;
        }

        .pux-devtool button:hover {
          cursor: pointer;
          color: #FFFFFF;
          text-decoration: underline;
        }

        .pux-devtool button:active {
          background: #272129;
          cursor: pointer;
        }
    """ ]
    , div
        [ className "pux-devtool"
        , PuxCSS.style do
            position fixed
            right (0.0# px)
            width $ px
              if state.opened then (state.width) else 0.0
            top (0.0# px)
            height (100.0# pct)
            overflow visible
            backgroundColor (rgb 66 66 84)
            color (rgb 249 249 249)
        ]
        [ div
            [ className "pux-devtool-container" ]
            [ h1
                [ PuxCSS.style do
                    fontSize (1.2# em)
                    marginTop (0.0# px)
                    fontWeight (weight 400.0)
                ]
                [ span
                    [ className "pux-devtool-icon gear" ]
                    [ svg
                        [ viewBox "0 0 12 14" ]
                        [ path [ d "M8 7q0-0.828-0.586-1.414t-1.414-0.586-1.414 0.586-0.586 1.414 0.586 1.414 1.414 0.586 1.414-0.586 0.586-1.414zM12 6.148v1.734q0 0.094-0.062 0.18t-0.156 0.102l-1.445 0.219q-0.148 0.422-0.305 0.711 0.273 0.391 0.836 1.078 0.078 0.094 0.078 0.195t-0.070 0.18q-0.211 0.289-0.773 0.844t-0.734 0.555q-0.094 0-0.203-0.070l-1.078-0.844q-0.344 0.18-0.711 0.297-0.125 1.062-0.227 1.453-0.055 0.219-0.281 0.219h-1.734q-0.109 0-0.191-0.066t-0.090-0.168l-0.219-1.438q-0.383-0.125-0.703-0.289l-1.102 0.836q-0.078 0.070-0.195 0.070-0.109 0-0.195-0.086-0.984-0.891-1.289-1.313-0.055-0.078-0.055-0.18 0-0.094 0.062-0.18 0.117-0.164 0.398-0.52t0.422-0.551q-0.211-0.391-0.32-0.773l-1.43-0.211q-0.102-0.016-0.164-0.098t-0.062-0.184v-1.734q0-0.094 0.062-0.18t0.148-0.102l1.453-0.219q0.109-0.359 0.305-0.719-0.312-0.445-0.836-1.078-0.078-0.094-0.078-0.187 0-0.078 0.070-0.18 0.203-0.281 0.77-0.84t0.738-0.559q0.102 0 0.203 0.078l1.078 0.836q0.344-0.18 0.711-0.297 0.125-1.062 0.227-1.453 0.055-0.219 0.281-0.219h1.734q0.109 0 0.191 0.066t0.090 0.168l0.219 1.438q0.383 0.125 0.703 0.289l1.109-0.836q0.070-0.070 0.187-0.070 0.102 0 0.195 0.078 1.008 0.93 1.289 1.328 0.055 0.062 0.055 0.172 0 0.094-0.062 0.18-0.117 0.164-0.398 0.52t-0.422 0.551q0.203 0.391 0.32 0.766l1.43 0.219q0.102 0.016 0.164 0.098t0.062 0.184z" ] [] ]
                    ]
                , text "Pux Devtool" ]
            , div
                [ className "pux-devtool-actions" ]
                [ span
                    [ className "pux-devtool-states" ]
                    [ text ((show (state.length - state.index))
                      <> " / " <> (show state.length))
                    ]
                , button
                    [ onClick (const Rewind) ]
                    [ span
                        [ className "pux-devtool-icon fast-backward" ]
                        [ svg
                            [ viewBox "0 0 14 14" ]
                            [ path [ d "M13.648 1.102q0.148-0.148 0.25-0.102t0.102 0.25v11.5q0 0.203-0.102 0.25t-0.25-0.102l-5.547-5.547q-0.070-0.070-0.102-0.148v5.547q0 0.203-0.102 0.25t-0.25-0.102l-5.547-5.547q-0.070-0.070-0.102-0.148v5.297q0 0.203-0.148 0.352t-0.352 0.148h-1q-0.203 0-0.352-0.148t-0.148-0.352v-11q0-0.203 0.148-0.352t0.352-0.148h1q0.203 0 0.352 0.148t0.148 0.352v5.297q0.031-0.086 0.102-0.148l5.547-5.547q0.148-0.148 0.25-0.102t0.102 0.25v5.547q0.031-0.086 0.102-0.148z" ] [] ]
                        ]
                    ]
                , button
                    [ onClick (const StepBack) ]
                    [ span
                        [ className "pux-devtool-icon step-backward" ]
                        [ svg
                            [ viewBox "0 0 8 14" ]
                            [ path [ d "M7.648 1.102q0.148-0.148 0.25-0.102t0.102 0.25v11.5q0 0.203-0.102 0.25t-0.25-0.102l-5.547-5.547q-0.070-0.070-0.102-0.148v5.297q0 0.203-0.148 0.352t-0.352 0.148h-1q-0.203 0-0.352-0.148t-0.148-0.352v-11q0-0.203 0.148-0.352t0.352-0.148h1q0.203 0 0.352 0.148t0.148 0.352v5.297q0.031-0.086 0.102-0.148z" ] [] ]
                        ]
                    ]
                , button
                    [ onClick (const StepForward) ]
                    [ span
                        [ className "pux-devtool-icon step-forward" ]
                        [ svg
                            [ viewBox "0 0 8 14" ]
                            [ path [ d "M0.352 12.898q-0.148 0.148-0.25 0.102t-0.102-0.25v-11.5q0-0.203 0.102-0.25t0.25 0.102l5.547 5.547q0.062 0.062 0.102 0.148v-5.297q0-0.203 0.148-0.352t0.352-0.148h1q0.203 0 0.352 0.148t0.148 0.352v11q0 0.203-0.148 0.352t-0.352 0.148h-1q-0.203 0-0.352-0.148t-0.148-0.352v-5.297q-0.039 0.078-0.102 0.148z" ] [] ]
                        ]
                    ]
                , button
                    [ onClick (const FastForward) ]
                    [ span
                        [ className "pux-devtool-icon fast-forward" ]
                        [ svg
                            [ viewBox "0 0 14 14" ]
                            [ path [ d "M0.352 12.898q-0.148 0.148-0.25 0.102t-0.102-0.25v-11.5q0-0.203 0.102-0.25t0.25 0.102l5.547 5.547q0.062 0.062 0.102 0.148v-5.547q0-0.203 0.102-0.25t0.25 0.102l5.547 5.547q0.062 0.062 0.102 0.148v-5.297q0-0.203 0.148-0.352t0.352-0.148h1q0.203 0 0.352 0.148t0.148 0.352v11q0 0.203-0.148 0.352t-0.352 0.148h-1q-0.203 0-0.352-0.148t-0.148-0.352v-5.297q-0.039 0.078-0.102 0.148l-5.547 5.547q-0.148 0.148-0.25 0.102t-0.102-0.25v-5.547q-0.039 0.078-0.102 0.148z" ] [] ]
                        ]
                    ]
                , button
                    [ onClick (const Clear) ]
                    [ span
                        [ className "pux-devtool-icon trash" ]
                        [ svg
                            [ viewBox "0 0 11 14" ]
                            [ path [ d "M4 5.75v4.5q0 0.109-0.070 0.18t-0.18 0.070h-0.5q-0.109 0-0.18-0.070t-0.070-0.18v-4.5q0-0.109 0.070-0.18t0.18-0.070h0.5q0.109 0 0.18 0.070t0.070 0.18zM6 5.75v4.5q0 0.109-0.070 0.18t-0.18 0.070h-0.5q-0.109 0-0.18-0.070t-0.070-0.18v-4.5q0-0.109 0.070-0.18t0.18-0.070h0.5q0.109 0 0.18 0.070t0.070 0.18zM8 5.75v4.5q0 0.109-0.070 0.18t-0.18 0.070h-0.5q-0.109 0-0.18-0.070t-0.070-0.18v-4.5q0-0.109 0.070-0.18t0.18-0.070h0.5q0.109 0 0.18 0.070t0.070 0.18zM9 11.406v-7.406h-7v7.406q0 0.172 0.055 0.316t0.113 0.211 0.082 0.066h6.5q0.023 0 0.082-0.066t0.113-0.211 0.055-0.316zM3.75 3h3.5l-0.375-0.914q-0.055-0.070-0.133-0.086h-2.477q-0.078 0.016-0.133 0.086zM11 3.25v0.5q0 0.109-0.070 0.18t-0.18 0.070h-0.75v7.406q0 0.648-0.367 1.121t-0.883 0.473h-6.5q-0.516 0-0.883-0.457t-0.367-1.105v-7.438h-0.75q-0.109 0-0.18-0.070t-0.070-0.18v-0.5q0-0.109 0.070-0.18t0.18-0.070h2.414l0.547-1.305q0.117-0.289 0.422-0.492t0.617-0.203h2.5q0.312 0 0.617 0.203t0.422 0.492l0.547 1.305h2.414q0.109 0 0.18 0.070t0.070 0.18z" ] [] ]
                        ]
                    ]
                ]
            , div [ PuxAttr.style
                      [ Tuple "marginTop" "1em", Tuple "fontWeight" "bold" ]
                  ]
                  [ text (selectedAction state) ]
            , div
                [ PuxCSS.style do
                    fontSize (0.8# em)
                    marginTop (1.0# em)
                , dangerouslySetInnerHTML (stateToString (selectedState state))
                ]
                []
            ]
        , div
            [ className "toggle-hide"
            , onClick (const ToggleOpen)
            , PuxAttr.style $ [ Tuple "lineHeight" "30px"
                    , Tuple "cursor" "pointer"
                    , Tuple "textAlign" "center"
                    , Tuple "verticalAlign" "middle"
                    ] <> css do
                position absolute
                backgroundColor (rgb 66 66 84)
                case state.opened of
                  true -> borderRadius (3.0# px) (0.0# px) (0.0#px) (3.0# px)
                  _ -> borderRadius (0.0# px) (3.0# px) (3.0#px) (0.0# px)
                top (50.0# pct)
                left (-12.0# px)
                height (30.0# px)
                width (12.0# px)
                let dValue = if state.opened then 0.0 else 180.0
                transform <<< rotate $ dValue # deg
            ]
            [ span
                [ className "pux-devtool-icon caret-right" ]
                [ svg
                    [ viewBox "0 0 5 14" ]
                    [ path [ d "M4.5 7q0 0.203-0.148 0.352l-3.5 3.5q-0.148 0.148-0.352 0.148t-0.352-0.148-0.148-0.352v-7q0-0.203 0.148-0.352t0.352-0.148 0.352 0.148l3.5 3.5q0.148 0.148 0.148 0.352z" ] [] ]
                ]
            ]
        ]
    , div
        [ className "pux-devtool-app-container"
        , PuxCSS.style do
            marginRight $ px
              if state.opened then state.width else 0.0
        ]
        [ map AppAction (appView (selectedState state)) ]
    ]
