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

-- Declaramos una función validarSinDigitos que valide que un string
-- no tenga dígitos
-- isDigit :: Char -> Bool
-- any :: (a -> Bool) -> [a] -> Bool

-- Declaramos una función validarConArroba que valide que un string contenga
-- una @
-- elem :: a -> [a] -> Bool


-- TIPOS PARAMETRIZADOS

-- Cambiamos el tipo de dato StringValidado para poder contener cualquier tipo,
-- no sólo String (Validado a)

-- Declaramos una validación numérica validarPositivo para que la edad no sea negativa


-- OPERACIONES SOBRE CONTEXTO (TRANSFORMACIONES)

-- Declaramos una función minusculas que convierta un Validado String a minúsculas,
-- y una función mayorDeEdad que aumente un Validado Int a 18 si es menor
-- toLower :: Char -> Char

 
-- Declaramos una función auxiliar (transformarValidado) para poder aplicar
-- funciones arbitrarias a un Validado a

-- Definimos una instancia de Functor para Validado
-- fmap :: (a -> b) -> Validado a -> Validado b


-- OPERACIONES DE VARIOS ARGUMENTOS

-- Declaramos una función que devuelva un Validado Usuario, a partir de
-- un email, una contraseña y una edad validadas

-- Definimos una instancia de Applicative para Validado
-- (<*>) :: Validado (a -> b) -> Validado a -> Validado b

-- OPERACIONES ENCADENABLES

-- Declaramos una función validarMenorQueCien para validar
-- que la edad es menor que 100

-- Necesitamos una forma de aplicar las dos validaciones al mismo número
-- validarEdad :: Int -> Validado Int

-- Definimos una instancia de Monad para Validado


-- Por último, declaramos una función validarUsuario que, dado un email,
-- una clave y una edad, lo valide con todas las funciones que hemos declarado

