{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
  Core compile-time utilities for embedding Elm programs in Haskell.

  This module runs the @elm@ compiler during Template Haskell
  splices and returns the compiled output as a 'String'. It is the
  low-level building block used by higher-level modules such as
  "System.Elm.Middleware".

  == Quick start

  1) Ensure @elm@ is available when GHC runs Template Haskell. If
  you use Cabal, add 'System.Elm.Middleware.requireElm' to your
  @Setup.hs@ (see "System.Elm.Middleware" for details).

  2) Call 'compileElm' from a Template Haskell splice:

  > import Language.Haskell.TH
  > import System.Elm.Compile
  >
  > embeddedJs :: Q String
  > embeddedJs = compileElm Dev JavaScript "frontend/Main.elm"

  3) Splice the result into your program:

  > myJs :: String
  > myJs = $(embeddedJs)

  == Choosing a 'Mode'

  'Mode' controls which flags are passed to @elm make@:

  * 'Dev' — no extra flags (the default for local development)
  * 'Debug' — passes @--debug@ (debug runtime, better errors)
  * 'Optimize' — passes @--optimize@ (smaller, faster output)

  Examples:

  > compileElm Dev JavaScript "frontend/Main.elm"
  >
  > compileElm Debug JavaScript "frontend/Main.elm"
  >
  > compileElm Optimize JavaScript "frontend/Main.elm"

  == Choosing an 'ElmOutputFormat'

  'ElmOutputFormat' selects whether @elm make@ produces a standalone
  JavaScript bundle or a complete HTML page:

  * 'JavaScript' — compile to @.js@ (typical for SPAs loaded by your
    own HTML template)
  * 'Html' — compile to a self-contained @.html@ page

  Examples:

  > -- JavaScript bundle for a custom HTML shell
  > compileElm Dev JavaScript "frontend/Main.elm"
  >
  > -- Full HTML page served as-is
  > compileElm Dev Html "frontend/Main.elm"

  == Embedding in other types

  'compileElm' returns a plain 'String'. Convert it however your
  application needs:

  > import qualified Data.Text as T
  >
  > embeddedText :: Q T.Text
  > embeddedText = T.pack <$> compileElm Dev JavaScript "frontend/Main.elm"

  > import qualified Data.ByteString.Char8 as BS8
  >
  > embeddedBytes :: Q BS8.ByteString
  > embeddedBytes = BS8.pack <$> compileElm Dev JavaScript "frontend/Main.elm"

  == Using with typed splices

  For a typed expression splice, bind the compiled content in @Q@ and
  lift it:

  > import Language.Haskell.TH
  >
  > myJs :: String
  > myJs = $(do
  >   js <- compileElm Optimize JavaScript "frontend/Main.elm"
  >   [| js |]
  >   )

  == Recompilation

  'compileElm' registers the @.elm@ source file as a Template Haskell
  dependency via 'addDependentFile'. GHC can re-run the splice when
  that file changes, but that only effectively works if the @.elm@
  file is also listed under @extra-source-files@ in your @.cabal@ file
  — otherwise Cabal may not rebuild the Haskell module when the Elm
  source changes.

  Example:

  > extra-source-files:
  >   frontend/Main.elm

  == Relationship to "System.Elm.Middleware"

  If you want to serve compiled Elm as WAI 'Middleware', use
  "System.Elm.Middleware" instead — it wraps 'compileElm' for that
  purpose. Use this module when you want the compiled output
  directly (for example, embedding JavaScript in a HTML template you
  write yourself, or serving it through your own HTTP layer).

-}
module System.Elm.Compile (
  Mode (..),
  ElmOutputFormat (..),
  compileElm,
)
where

import Control.Exception.Safe (bracket, tryAny)
import Control.Monad (Monad((>>=)), void)
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile)
import Prelude
  ( Bool(True), Functor(fmap), Maybe(Just, Nothing), MonadFail(fail)
  , Semigroup((<>)), Show(show), ($), (++), (.), Eq, FilePath, String, putStrLn
  )
import System.Directory (removeDirectoryRecursive)
import System.Exit (ExitCode(ExitSuccess))
import System.Posix
  ( ProcessStatus(Exited), executeFile, forkProcess, getProcessStatus
  , mkdtemp
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

data Mode
  = Optimize
  | Dev
  | Debug
  deriving stock (Eq, Show)


data ElmOutputFormat
  = JavaScript
  | Html
  deriving stock (Eq, Show)


buildDirPrefix :: FilePath
buildDirPrefix = ".om-elm-build-dir-"


{- |
  Compile an Elm program at Template Haskell time.

  This function invokes @elm make@, reads the compiler output from a
  unique temporary build directory, and returns the contents as a
  'String'. The temporary directory is deleted after compilation
  finishes, including when compilation fails. The caller is responsible
  for interpreting the result — for example, choosing an HTTP content
  type or converting to 'Text'.

  == Parameters

  * __mode__ — compiler flags ('Dev', 'Debug', or 'Optimize')
  * __format__ — output shape ('JavaScript' or 'Html')
  * __elmFile__ — path to the @.elm@ entrypoint, relative to the
    project root (same path you would pass to @elm make@ on the command
    line)

  == Basic examples

  Compile a JavaScript bundle with default (dev) settings:

  > compileElm Dev JavaScript "frontend/Main.elm"

  Compile an optimized JavaScript bundle:

  > compileElm Optimize JavaScript "frontend/Main.elm"

  Compile a self-contained HTML page:

  > compileElm Dev Html "frontend/Main.elm"

  == Splicing into top-level definitions

  > myJs :: String
  > myJs = $(compileElm Dev JavaScript "frontend/Main.elm")

  > myHtml :: String
  > myHtml = $(compileElm Dev Html "frontend/Main.elm")

  == Splicing into a larger Template Haskell computation

  > import Language.Haskell.TH
  >
  > myJs :: String
  > myJs = $(do
  >   js <- compileElm Optimize JavaScript "frontend/Main.elm"
  >   [| js |]
  >   )

  == Combining mode and format

  > -- Debug runtime, JavaScript output
  > compileElm Debug JavaScript "frontend/Main.elm"
  >
  > -- Optimized production build, HTML output
  > compileElm Optimize Html "frontend/Main.elm"
  >
  > -- Dev build, JavaScript output (most common for SPAs)
  > compileElm Dev JavaScript "frontend/Main.elm"

  == Requirements

  * The @elm@ executable must be on @PATH@ when GHC runs this splice.
  * The @elmFile@ path must exist at compile time.
  * GHC can re-run the splice when @elmFile@ changes (via
    'addDependentFile'), but that only effectively works if @elmFile@
    is also listed under @extra-source-files@ in your @.cabal@ file
    (see the module documentation for an example).

  See the module documentation for setup instructions and more
  embedding examples.

-}
compileElm
  :: Mode
  -> ElmOutputFormat
  -> FilePath
  -> Q String
compileElm mode format elmFile = do
  addDependentFile elmFile
  runIO $
    bracket
      (mkdtemp buildDirPrefix)
      (\dir -> void . tryAny $ removeDirectoryRecursive dir)
      (\dir -> do
        putStrLn $ "Compiling elm file: " ++ elmFile
        let
          flags :: [String]
          flags =
            case mode of
              Debug -> ["--debug"]
              Dev -> []
              Optimize -> ["--optimize"]

          outputFile :: FilePath
          outputFile = buildFile dir format
        forkProcess
          (
            executeFile "elm" True ([
                "make",
                elmFile,
                "--output=" <> outputFile
              ] ++ flags) Nothing
          ) >>= getProcessStatus True True >>= \case
              Nothing -> fail "elm should have ended."
              Just (Exited ExitSuccess) ->
                fmap BS8.unpack (BS.readFile outputFile)
              e -> fail $ "elm failed with: " ++ show e
      )


buildFile :: FilePath -> ElmOutputFormat -> FilePath
buildFile dir = \case
  JavaScript -> dir <> "/elm.js"
  Html -> dir <> "/elm.html"
