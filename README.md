# pux-devtool

> A time-travelling debugger for
> [Pux](https://github.com/alexmingoia/purescript-pux) applications.

![Pux Devtool animation](support/pux-devtool.gif)

## Usage

Replace `Pux.start` with `Pux.Devtool.start`, add `Pux.Devtool.Options` and that's it!

```purescript
main = do
  app <- Pux.Devtool.start
    { initialState: initialState
    , update: update
    , view: view
    , inputs: []
    }
    Pux.Devtool.defaultOptions
    -- or replace `Pux.Devtool.defaultOptions` with custom `Options`, e.g.
    -- { opened: false }

  renderToDOM "#app" app.html
```
