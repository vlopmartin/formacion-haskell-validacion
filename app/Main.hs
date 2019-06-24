{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Web.Scotty
import Usuario
import qualified Data.Aeson as A

main :: IO ()
main = scotty 3000 $ do
  get "/ejemplo" $ json $ Usuario "a" "b" 20
  post "/validar" $ do
    (e :: String) <- param "email"
    (p :: String) <- param "password"
    (a :: Int) <- param "edad"
    case (validarUsuario e p a) of
      Valido u -> json u
      Invalido -> return ()

instance A.ToJSON Usuario
