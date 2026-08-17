# Changelog

## 2.1.0.1 — 2026-08-13

- Use a unique temporary Elm build directory per compilation so
  concurrent Elm builds no longer race on a shared
  `.om-elm-build-dir`. The directory is removed after the build
  finishes.


## 2.1.0.0 — 2026-07-07

- Added `System.Elm.Compile` with `compileElm` for embedding compiled
  Elm JavaScript or HTML in your Haskell program at build time.
- Removed the deprecated `elmSite` function; use `elmSiteDev`
  instead.
