module Harakiri.TypeCheck (typeCheck) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad (forM_, when, void, unless)
import Data.Fix
import Data.Functor.Compose
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Prelude hiding (seq)

import Harakiri.Expr

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text

data FuncInfo = FuncInfo
    { numArgs    :: !Int
    , returnType :: !(Maybe Type)
    }

data CheckState = CheckState
    { funEnv          :: !(HashMap Text FuncInfo)
    , definedVars     :: !(HashSet Text)
    , curPos          :: !SrcSpan
    , sourceCode      :: !Text
    , inLoop          :: !Bool
    , curFunName      :: !Text
    , curFinfo        :: !FuncInfo
    , curNesting      :: !Int
    , returnInNonVoid :: !Bool
    }

type TypeCheckM = StateT CheckState (Except Text)

typeCheck :: Text -> [Function PosExpr] -> Maybe Text
typeCheck src funcs = case runExcept (evalStateT checkAll initState) of
    Left err -> Just err
    Right{}  -> Nothing
  where checkAll = do
            mapM_ checkFunction funcs
            env <- gets funEnv
            case HashMap.lookup "main" env of
                Nothing -> prettyError "function main is undefined"
                Just fInfo ->
                    when (numArgs fInfo /= 0) $
                        prettyError "main function must take 0 arguments"
        checkFunction func = do
            let fName = funName func
                fInfo = FuncInfo { numArgs    = length $ funArgs func
                                 , returnType = Nothing
                                 }
                fBody = funBody func
            isExists <- checkFuncExists fName
            when isExists (prettyError $ "function " <> fName <> " is already defined")
            insertFuncInfo fName fInfo
            resetCheckState fName (funArgs func) fInfo
            void $ adi (typeCheckExprF . annotated . getCompose) setContext fBody
            retInNonVoid <- gets returnInNonVoid
            unless retInNonVoid $
                prettyError $ "function " <> fName <> " must return a value"
            return ()
        initState = CheckState { funEnv          = HashMap.empty
                               , definedVars     = HashSet.empty
                               , curPos          = SrcSpan iniPos iniPos
                               , inLoop          = False
                               , sourceCode      = src
                               , curFunName      = ""
                               , curFinfo        = FuncInfo 0 Nothing
                               , curNesting      = 0
                               , returnInNonVoid = True
                               }
        iniPos = SourcePos "" 1 1

typeCheckExprF :: ExprF (TypeCheckM Type) -> TypeCheckM Type
typeCheckExprF = \case
    IntLit{} -> return TInt
    Var var  -> do
        isExists <- checkVarExists var
        unless isExists (prettyError $ "undefined variable " <> var)
        return TInt
    Neg typ  -> typeMismatchError typ "trying negate non-integer typ"
    Binop typ1 op typ2 -> do
        void $ typeMismatchError typ1 msg
        void $ typeMismatchError typ2 msg
        return TInt
      where msg = "expected int in " <> showBinop op <> " operation"
    Call fn args -> do
        env <- gets funEnv
        case HashMap.lookup fn env of
            Nothing -> prettyError $ "undefined function " <> fn
            Just fInfo
              | length args /= numArgs fInfo
              -> prettyError $ "function " <> fn <> " expected "
                             <> showText (numArgs fInfo) <> " arguments"
                             <> " but given " <> showText (length args)
              | otherwise -> do
                  forM_ args $ \arg ->
                      typeMismatchError arg "non-integer arguments are not allowed"
                  return $ fromMaybe TVoid (returnType fInfo)
    Echo args -> do
        forM_ args $ \case
            StrArg{}  -> return ()
            ExprArg e -> void e
        return TVoid
    Input  -> return TInt
    Assign var val -> do
        void $ typeMismatchError val ("trying to assign non-integer value to " <> var)
        defineVar var
        return TVoid
    If cond th el -> do
        beforeIfVars <- gets definedVars
        void $ typeMismatchError cond "non-integer statement in if condition"
        void $ th
        modify (\st -> st { definedVars = beforeIfVars })
        maybe (return ()) void el
        modify (\st -> st { definedVars = beforeIfVars })
        return TVoid
    While cond body -> do
        beforeWhileVars <- gets definedVars
        void $ typeMismatchError cond "non-integer statement in while condition"
        void $ body
        modify (\st -> st { definedVars = beforeWhileVars })
        return TVoid
    Break -> do
        isInLoop <- gets inLoop
        unless isInLoop $ prettyError "break statement outside the while loop"
        return TVoid
    Seq seq -> do
        sequence_ seq
        return TVoid
    Return mval -> do
        actualType <- case mval of
            Nothing  -> return TVoid
            Just val -> do
                nesting <- gets curNesting
                setReturnInNonVoid (nesting == 0)
                typeMismatchError val " trying to return non-integer value"
        fInfo <- gets curFinfo
        fName <- gets curFunName
        case returnType fInfo of
            Nothing -> do
                let newfInfo = fInfo { returnType = Just actualType }
                updateFuncInfo newfInfo
            Just expectedType
              | actualType /= expectedType
              -> prettyError $  " wrong type of return statement in function " <> fName
                             <> ": expected " <> showType expectedType
                             <> " but given " <> showType actualType
              | otherwise -> return ()
        return TVoid

typeMismatchError :: TypeCheckM Type -> Text -> TypeCheckM Type
typeMismatchError ma msg = do
    res <- ma
    when (res /= TInt) $ prettyError msg
    return res

setContext :: (PosExpr -> TypeCheckM Type) -> PosExpr -> TypeCheckM Type
setContext f = \case
    expr@(AnnE ann If{}) -> withinNesting $ do
        setCurPos ann
        f expr
    expr@(AnnE ann While{}) -> withinLoop $ withinNesting $ do
        setCurPos ann
        f expr
    expr -> do
        setCurPos (annotation $ getCompose $ unFix expr)
        f expr

prettyError :: Text -> TypeCheckM a
prettyError err = do
    SourcePos fpath ln cl <- gets (spanBegin . curPos)
    src <- gets sourceCode
    let strLn = showText ln
        margin = Text.replicate (Text.length strLn + 2) " "
        errMsg =  Text.pack fpath <> ":" <> strLn <> ":" <> showText cl
               <> ": error:\n" <> err <> "\n"
        line = Text.takeWhile (/='\n')
             . (!!(ln-1))
             . iterate (Text.tail . Text.dropWhile (/= '\n')) $ src
        prettyLine =  margin <> "|\n " <> strLn <> " | " <> line <> "\n"
                   <> margin <> "|\n"
    lift $ throwE $ errMsg <> prettyLine

resetCheckState :: Text -> [Text] -> FuncInfo -> TypeCheckM ()
resetCheckState fName args fInfo
  | length args /= HashSet.size defVars
  = prettyError $ "duplicated arguments in function " <> fName
  | otherwise = modify newState
  where defVars = HashSet.fromList args
        newState st = st { definedVars     = defVars
                         , curNesting      = 0
                         , inLoop          = False
                         , returnInNonVoid = True
                         , curFunName      = fName
                         , curFinfo        = fInfo
                         }

insertFuncInfo :: Text -> FuncInfo -> TypeCheckM ()
insertFuncInfo fName fInfo =
    modify (\st -> st { funEnv = HashMap.insert fName fInfo $ funEnv st })

updateFuncInfo :: FuncInfo -> TypeCheckM ()
updateFuncInfo fInfo =
    modify $ \st -> st { funEnv   = HashMap.insert (curFunName st) fInfo $ funEnv st
                       , curFinfo = fInfo
                       }

checkFuncExists :: Text -> TypeCheckM Bool
checkFuncExists fName = gets (HashMap.member fName . funEnv)

defineVar :: Text -> TypeCheckM ()
defineVar var = modify (\st -> st { definedVars = HashSet.insert var $ definedVars st })

checkVarExists :: Text -> TypeCheckM Bool
checkVarExists var = gets (HashSet.member var . definedVars)

withinLoop :: TypeCheckM a -> TypeCheckM a
withinLoop ma = do
    modify (\st -> st { inLoop = True })
    res <- ma
    modify (\st -> st { inLoop = False })
    return res

withinNesting :: TypeCheckM a -> TypeCheckM a
withinNesting ma = do
    modify (\st -> st { curNesting = curNesting st - 1})
    res <- ma
    modify (\st -> st { curNesting = curNesting st + 1})
    return res

setReturnInNonVoid :: Bool -> TypeCheckM ()
setReturnInNonVoid val = modify (\st -> st { returnInNonVoid = val })

setCurPos :: SrcSpan -> TypeCheckM ()
setCurPos pos = modify (\st -> st { curPos = pos })

adi :: Functor f => (f a -> a) -> ((Fix f -> a) -> Fix f -> a) -> Fix f -> a
adi f g = g (f . fmap (adi f g) . unFix)

showText :: Show a => a -> Text
showText = Text.pack . show
