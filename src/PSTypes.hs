module PSTypes
  (
    ThrowsError,
    LispError(..),
    LispVal(..),
    Env,
    IOThrowsError,
    getVar,
    setVar,
    defineVar,
    liftThrows,
    nullEnv,
    bindVars
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Data.IORef
import           System.IO
import           Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
           | List [LispVal]
           | DottedList [LispVal] LispVal
           | Number Integer
           | String String
           | Bool Bool
           | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
           | Func { params :: [String], vararg :: (Maybe String),
                    body :: [LispVal], closure :: Env}
           | IOFunc ([LispVal] -> IOThrowsError LispVal)
           | Port Handle

instance Show LispVal where show = showVal
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList first rest) = "(" ++ unwordsList first ++ " . " ++ showVal rest ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = _, closure = _}) =
  "(lambda (" ++ unwords (map show args) ++
  (case varargs of
     Nothing  -> ""
     Just arg -> "." ++ arg) ++ ") ...)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError
showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ (unwords . map show $ found)
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default s) = s

type ThrowsError = Either LispError

type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ExceptT LispError IO

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

isBound :: Env -> String -> IO Bool
isBound envRef varname = readIORef envRef >>= return . maybe False (const True) . lookup varname

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
     then setVar envRef var value >> return value
     else liftIO $ do
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef ((var, valueRef) : env)
          return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)
