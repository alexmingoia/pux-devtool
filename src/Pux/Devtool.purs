module Pux.Devtool where

import Data.Maybe.Unsafe (fromJust)
import Data.List ((:), index, List, singleton)
import Control.Monad.Eff (Eff)
import Pux (App, Config, CoreEffects, EffModel, noEffects)
import Pux.Html (button, div, h1, Html, i, span, text)
import Pux.Html.Attributes (className, style, dangerouslySetInnerHTML)
import Pux.Html.Events (onClick)
import Prelude ((<), (>), (++), (+), (-), ($), const, map, not, bind, return, show)

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
  , length :: Int
  , index :: Int
  , opened :: Boolean
  , width :: Int
  }

init :: forall s. s -> State s
init s =
  { actions: singleton "App initialized. Awaiting action..."
  , states: singleton s
  , length: 1
  , index: 0
  , opened: true
  , width: 360
  }

selectedState :: forall s. State s -> s
selectedState s = fromJust $ index s.states s.index

selectedAction :: forall s. State s -> String
selectedAction s = fromJust $ index s.actions s.index

start :: forall a s e. (Config s a e) -> Eff (CoreEffects e) (App s (Action a))
start config = do
  app <- Pux.start
    { initialState: init config.initialState
    , update: update config.update
    , view: view config.view
    , inputs: map (map AppAction) config.inputs
    }

  return $ app { state = map selectedState app.state }

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
    [ Pux.Html.style [] [ text $ """
        @import 'https://maxcdn.bootstrapcdn.com/font-awesome/4.6.1/css/font-awesome.min.css';

        .pux-devtool-container {
          position: relative;
          padding: 1em;
          overflow: scroll;
          height: 100%;
        }

        .pux-devtool-container h1 i {
          padding-right: .5em;
        }

        .pux-devtool-actions {
          position: absolute;
          top: 1em;
          right: 0;
        }

        .pux-devtool-actions span, .pux-devtool-actions button {
          display: inline-block;
          margin-right: .5em;
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
        , style
            { position: "fixed"
            , right: 0
            , width: if state.opened then (show state.width ++ "px") else "0px"
            , top: 0
            , height: "100%"
            , overflow: "visible"
            , background: "#424254"
            , color: "#F9F9F9"
            }
        ]
        [ div
            [ className "pux-devtool-container" ]
            [ h1
                [ style { fontSize: "1.2em", marginTop: 0, fontWeight: "normal" } ]
                [ i [ className "fa fa-cog" ] [], text "Pux Devtool" ]
            , div
                [ className "pux-devtool-actions" ]
                [ span
                    []
                    [ text ((show (state.length - state.index))
                      ++ " / " ++ (show state.length))
                    ]
                , button
                    [ onClick (const Rewind) ]
                    [ i [ className "fa fa-fast-backward" ] [] ]
                , button
                    [ onClick (const StepBack) ]
                    [ i [ className "fa fa-step-backward" ] [] ]
                , button
                    [ onClick (const StepForward) ]
                    [ i [ className "fa fa-step-forward" ] [] ]
                , button
                    [ onClick (const FastForward) ]
                    [ i [ className "fa fa-fast-forward" ] [] ]
                , button
                    [ onClick (const Clear) ]
                    [ i [ className "fa fa-trash" ] [] ]
                ]
            , div [ style { marginTop: "1em", fontWeight: "bold" } ] [ text (selectedAction state) ]
            , div
                [ style { fontSize: ".9em", marginTop: "1em" }
                , dangerouslySetInnerHTML (stateToString (selectedState state))
                ]
                []
            ]
        , div
            [ className "toggle-hide"
            , onClick (const ToggleOpen)
            , style
                { position: "absolute"
                , background: "#424254"
                , borderTopLeftRadius: "3px"
                , borderBottomLeftRadius: "3px"
                , top: "50%"
                , left: "-16px"
                , height: "30px"
                , lineHeight: "30px"
                , cursor: "pointer"
                , width: "16px"
                , textAlign: "center"
                , verticalAlign: "middle"
                }
            ]
            [ i [ className "fa fa-chevron-right" ] []
            ]
        ]
    , div
        [ className "pux-devtool-app-container"
        , style
            { marginRight: if state.opened then show state.width ++ "px" else "0px"
            }
        ]
        [ map AppAction (appView (selectedState state)) ]
    ]

foreign import actionToString :: forall a. a -> String

foreign import stateToString :: forall s. s -> String
