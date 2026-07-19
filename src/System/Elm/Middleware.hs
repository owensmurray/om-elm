{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

{- |
  This module contains utilities for compiling and bundling Elm programs
  directly into your Haskell executable binary. It is useful for the
  case where you want to bundle a front-end Elm web app along with the
  backing services that support it, and especially for when the two
  components are part of the same codebase. It produces WAI Middleware,
  and is thus compatible with a wide range server-side frameworks.

  Usage is designed to be as simple as possible. There are 3 steps:

  1) Change your .cabal file to use a \"Custom\" build type, and add
  the appropriate custom build dependencies.

  > build-type: Custom
  > ...
  > custom-setup
  >   setup-depends:
  >     Cabal,
  >     base,
  >     om-elm

  2) Modify your @Setup.hs@ file, using 'requireElm'.

  3) Include a 'Middleware' template-haskell splice, using 'elmSiteDev',
  in the appropriate place in your code.

  See the function documnetation for more details.

-}
module System.Elm.Middleware
  ( requireElm
  , elmSiteDebug
  , elmSiteDev
  , elmSiteOptimize
  , PathInfo
  )
where

import Data.Map (Map)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Distribution.Simple (UserHooks(hookedPrograms, preConf), simpleUserHooks)
import Distribution.Simple.Program
  ( Program, configureAllKnownPrograms, defaultProgramDb, requireProgram
  , simpleProgram
  )
import Distribution.Simple.Setup
  ( ConfigFlags(configVerbosity), fromFlagOrDefault
  )
import Distribution.Verbosity (normal)
import Language.Haskell.TH (Code(examineCode), Q, TExp)
import Network.HTTP.Types (methodNotAllowed405, ok200)
import Network.Wai
  ( Request(pathInfo, requestMethod), Application, Middleware, responseLBS
  )
import Prelude
  ( Applicative(pure), Bool(True), Eq((==)), Foldable(length), Functor(fmap)
  , Maybe(Just, Nothing), Traversable(mapM), ($), (++), (=<<), FilePath, String
  , reverse, take
  )
import Safe (lastMay)
import System.Elm.Compile
  ( ElmOutputFormat(Html, JavaScript), Mode(Debug, Dev, Optimize), compileElm
  )
import qualified Data.Map as Map
import qualified Data.Text as T

{- |
  Add the elm program requirements to a set of build hooks. This is
  expected to be used in your Setup.hs file thusly:

  > import Distribution.Simple (defaultMainWithHooks, simpleUserHooks)
  > import System.Elm.Middleware (requireElm)
  >
  > main = defaultMainWithHooks (requireElm simpleUserHooks)

-}
requireElm :: UserHooks -> UserHooks
requireElm hooks =
    hooks {
      hookedPrograms = hookedPrograms hooks ++ [elmProg],

      preConf = \args flags -> do
        let verbosity = fromFlagOrDefault normal (configVerbosity flags)
        db <- configureAllKnownPrograms verbosity defaultProgramDb
        _ <- requireProgram verbosity elmProg db
        preConf simpleUserHooks args flags
    }
  where
    {- | A description of the elm program.  -}
    elmProg :: Program
    elmProg = simpleProgram "elm"


{- |
  Template Haskell method to create a 'Middleware' that serves a set of
  elm programs. The elm programs are compiled into HTML at compile time,
  and that HTML is included directly in your executable.

  The parameter is a map of 'pathInfo's to elm program module file. The
  elm program located at the file is served whenever the pathInfo matches
  that of the request. Any non-matching request is forwarded to the
  downstream 'Application'.

  The typed template-haskell splice:

  > $$(elmSiteDev $ Map.fromList [
  >     (["app.js"], "elm/Your/Elm/Module/App.elm")
  >   ])

  will construct a WAI 'Middleware' which serves the compiled elm program on
  @/app.js@.

  Neither the @--optimize@ nor the @--debug@ flags are passed to the
  elm compiler.

-}
elmSiteDev  :: Map PathInfo FilePath -> Q (TExp Middleware)
elmSiteDev = elmSite2 Dev


{- | Like 'elmSiteDev', but serve the debug elm runtime. -}
elmSiteDebug :: Map PathInfo FilePath -> Q (TExp Middleware)
elmSiteDebug = elmSite2 Debug


{- | Like 'elmSiteDev', but pass @--optimize@ to the elm compiler. -}
elmSiteOptimize :: Map PathInfo FilePath -> Q (TExp Middleware)
elmSiteOptimize = elmSite2 Optimize


elmSite2 :: Mode -> Map PathInfo FilePath -> Q (TExp Middleware)
elmSite2 mode spec =
    buildMiddleware =<< mapM compileResource (Map.toList spec)
  where
    {- | Construct the middleware from a set of compiled elm resources. -}
    buildMiddleware :: [([String], (String, String))] -> Q (TExp Middleware)
    buildMiddleware resources =
      examineCode
        [||
          let
            apps = Map.fromList[
                (uriPath, buildApp contentTypeHeader content)
                | (fmap T.pack -> uriPath, (contentTypeHeader, content)) <- resources
              ]
            {- | Build the application that serves a single elm resource. -}
            buildApp :: String -> String -> Application
            buildApp contentTypeHeader content req respond = respond $
              case requestMethod req of
                "GET" ->
                  responseLBS
                    ok200
                    [("Content-Type", fromString contentTypeHeader)]
                    (fromString content)
                _ -> responseLBS methodNotAllowed405 [("Allow", "GET")] ""
          in
            \downstream req respond ->
              case Map.lookup (pathInfo req) apps of
                Nothing -> downstream req respond
                Just app -> app req respond
        ||]

    compileResource
      :: (PathInfo, FilePath)
      -> Q ([String], (String, String))
    compileResource (uriPathText, elmFile) = do
      let
        uriPath :: [String]
        uriPath = fmap T.unpack uriPathText

        format :: ElmOutputFormat
        format = outputFormat uriPath
      content <- compileElm mode format elmFile
      pure (uriPath, (contentType format, content))

    outputFormat :: [String] -> ElmOutputFormat
    outputFormat uriPath =
      case lastMay uriPath of
        Just (endsWith ".js" -> True) -> JavaScript
        _ -> Html

    contentType :: ElmOutputFormat -> String
    contentType = \case
      JavaScript -> "text/javascript"
      Html -> "text/html"

    endsWith :: String -> String -> Bool
    endsWith ending str =
      take (length ending) (reverse str) == reverse ending


{- | A WAI uri path, as per the meaning of 'pathInfo'. -}
type PathInfo = [Text]

