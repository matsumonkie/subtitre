{-# LANGUAGE OverloadedStrings #-}

module Dico (

) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Dico = M.Map T.Text T.Text

dico :: Dico
dico = M.fromList [ ("butler", "majordome")
                  , ("look up", "regarder")
                  ]

type App = Reader Dico (ExceptT UserError IO T.Text)

data UserError = NoTranslation | NoInput

translate :: T.Text -> ReaderT Dico (ExceptT UserError IO) T.Text
translate word = do
  liftIO $ TIO.putStrLn $ "translating..."
  translations <- ask
  let translated = M.lookup word translations
  maybe (lift $ throwE NoTranslation) return translated

promptWord :: ExceptT UserError IO T.Text
promptWord = do
  liftIO $ TIO.putStr $ "translate: "
  line <- liftIO $ TIO.getLine
  if T.null line then
    throwE NoInput
  else
    return line

errorHandler :: UserError -> ExceptT UserError IO T.Text
errorHandler error = do
  liftIO $ TIO.putStrLn $ case error of
    NoInput -> "no input :("
    NoTranslation -> "no translation :("
  throwE error

main :: IO ()
main = do
  TIO.putStrLn "welcome :-)"
  runExceptT translateDialog
  return ()

translateDialog :: ExceptT UserError IO ()
translateDialog = do
  translated <- (promptWord >>= tr) `catchE` errorHandler
  report translated
  return ()
  where
    tr = \word -> runReaderT (translate word) dico
    report = \translated -> liftIO $ TIO.putStrLn $ wrapText translated

wrapText :: T.Text -> T.Text
wrapText text =
  T.append "[" $ T.append text "]"

foo :: (Monad m) => m a -> (a -> m b) -> m b
foo a f =
  a >>= f
