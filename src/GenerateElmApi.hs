{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module GenerateElmApi where

import           Servant.Elm  ( DefineElm (DefineElm), Proxy (Proxy), defElmImports, defElmOptions,
                                generateElmModuleWith )

import           SampleServer

main :: IO ()
main =
  generateElmModuleWith
    defElmOptions
    ["DatumApi"]
    defElmImports
    "elm-dir"
    [DefineElm (Proxy :: Proxy Datum), DefineElm (Proxy :: Proxy DatumType)]
    (Proxy :: Proxy DatumAPI)
