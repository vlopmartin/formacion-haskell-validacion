{-# LANGUAGE DeriveGeneric #-}

module Usuario where

import Data.Char
import Control.Monad
import GHC.Generics

-- NOTACIÓN DE REGISTRO

-- Esta notación permite declarar tipos de dato junto con funciones de conveniencia
-- No hace nada nuevo, es sólo una abreviatura
data Usuario = Usuario {email :: String, password :: String, edad :: Int}
  deriving (Show, Eq, Generic)

-- VALORES CON VALIDACIÓN

-- Creamos un nuevo tipo de dato para validación de strings (StringValidado)
-- Este tipo de dato podrá contener un string (Válido String) o nada (Inválido)
data Validado a = Valido a | Invalido
  deriving (Show, Eq)
-- Declaramos una función validarSinDigitos que valide que un string
-- no tenga dígitos
-- isDigit :: Char -> Bool
-- any :: (a -> Bool) -> [a] -> Bool

validarSinDigitos :: String -> Validado String
validarSinDigitos str
  | any isDigit str = Invalido
  | otherwise = Valido str

-- Declaramos una función validarConArroba que valide que un string contenga
-- una @
-- elem :: a -> [a] -> Bool
validarConArroba :: String -> Validado String
validarConArroba str 
  | '@' `elem` str = Valido str
  | otherwise = Invalido


-- TIPOS PARAMETRIZADOS

-- Cambiamos el tipo de dato StringValidado para poder contener cualquier tipo,
-- no sólo String (Validado a)

-- Declaramos una validación numérica validarPositivo para que la edad no sea negativa
validarPositivo :: Int -> Validado Int
validarPositivo n
  | (<) n 0 = Invalido
  | otherwise = Valido n

-- OPERACIONES SOBRE CONTEXTO (TRANSFORMACIONES)

-- Declaramos una función minusculas que convierta un Validado String a minúsculas,
-- y una función mayorDeEdad que aumente un Validado Int a 18 si es menor
-- toLower :: Char -> Char

minusculas :: Validado String -> Validado String
minusculas (Valido str) =  Valido (map toLower str)
minusculas (Invalido) = Invalido

mayorDeEdad :: Validado Int -> Validado Int
mayorDeEdad (Valido n)
  | n < 18 = Valido 18
  | otherwise = Valido n
mayorDeEdad (Invalido) = Invalido  
 
-- Declaramos una función auxiliar (transformarValidado) para poder aplicar
-- funciones arbitrarias a un Validado a
transformarValidado :: (a -> b) -> Validado a -> Validado b
transformarValidado f (Valido val) = Valido (f val)
transformarValidado _ (Invalido) = Invalido  

-- Definimos una instancia de Functor para Validado
-- fmap :: (a -> b) -> Validado a -> Validado b
instance Functor Validado where
   fmap = transformarValidado

-- OPERACIONES DE VARIOS ARGUMENTOS

-- Declaramos una función que devuelva un Validado Usuario, a partir de
-- un email, una contraseña y una edad validadas
--construirUsuario :: Validado String -> Validado String -> Validado Int -> Validado Usuario
--construirUsuario   

-- Definimos una instancia de Applicative para Validado
-- (<*>) :: Validado (a -> b) -> Validado a -> Validado b
instance Applicative Validado where
  (<*>) (Invalido) _ = Invalido
  (<*>) _ (Invalido) = Invalido
  (<*>) (Valido fun) (Valido val) = Valido (fun val)
  pure = Valido

-- OPERACIONES ENCADENABLES

-- Declaramos una función validarMenorQueCien para validar
-- que la edad es menor que 100

validarMenorQueCien :: Int -> Validado Int
validarMenorQueCien n
  | n<100 = Valido n
  | otherwise = Invalido

  

-- Necesitamos una forma de aplicar las dos validaciones al mismo número
validarEdad :: Int -> Validado Int
validarEdad = validarMenorQueCien <=< validarPositivo




-- Definimos una instancia de Monad para Validado
-- (>>=) :: Validado a -> (a -> Validado b) -> Validado b
instance Monad Validado where
  (Valido x) >>= f = f x
  Invalido >>= _ = Invalido

-- Por último, declaramos una función validarUsuario que, dado un email,
-- una clave y una edad, lo valide con todas las funciones que hemos declarado

validarUsuario :: String -> String -> Int -> Validado Usuario
validarUsuario e p a = do
  ve <- map toLower <$> validarConArroba e
  vp <- validarSinDigitos p
  va <- validarEdad a
  return $ Usuario ve vp va