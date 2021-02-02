{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Harakiri.IR.Translate
    ( TransResult(..)
    , translateToIR
    ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Fix (foldFix)
import Data.IntMap (IntMap)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, ViewR(..), viewr, (|>), (><))
import Data.Text (Text)
import Prelude hiding (seq)

import Harakiri.Expr (Function, ExprF, stripAnnotation)
import Harakiri.IR.Types
import Harakiri.TypeCheck (TypedFunctions, getTypedFunctions)

import qualified Data.IntMap as IntMap
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence as Seq

import qualified Harakiri.Expr as Expr

type TransM m = ( MonadReader TransContext m
                , MonadState TransState m
                , MonadError Text m
                )

data TransResult = TransResult
    { strings   :: !(IntMap Text)
    , functions :: ![Function Temp (Seq IR)]
    }

data TransState = TransState
    { nextTemp     :: !Temp
    , nextLabel    :: !Label
    , nextStringId :: !Int
    , stringMap    :: !(HashMap Text Int)
    , variables    :: !(HashMap Text Temp)
    , code         :: !(Seq IR)
    , dstTemp      :: !(Maybe Temp)
    }

data TransContext = TransContext
    { endLabel     :: !(Maybe Label)
    , trueLabel    :: !(Maybe Label)
    , falseLabel   :: !(Maybe Label)
    , condValue    :: !Bool
    , binopToRelop :: !Bool
    }

translateToIR :: TypedFunctions -> Either Text TransResult
translateToIR typedFuncs =
    case runExcept (runReaderT (runStateT (mapM run stripedFuncs) initState) initCtx) of
        Left err -> Left err
        Right (transFuncs, st) ->
            Right $ TransResult { functions = transFuncs
                                , strings   = convertStrings $ stringMap st
                                }
  where stripedFuncs = map (fmap stripAnnotation) $ getTypedFunctions typedFuncs
        convertStrings =
            HashMap.foldlWithKey' (\acc str ind -> IntMap.insert ind str acc) IntMap.empty
        run func = do
            local (const initCtx) $ do
                resetState
                args <- mapM newVarTemp (Expr.funArgs func)
                void $ foldFix transExprF (Expr.funBody func)
                body <- gets code
                let transFunc = func { Expr.funArgs = args
                                     , Expr.funBody = body
                                     }
                return transFunc
        resetState = modify $ \st -> st { variables = HashMap.empty
                                        , code      = Seq.empty
                                        , dstTemp   = Nothing
                                        }
        initState = TransState { nextTemp     = T 0
                               , nextLabel    = L 0
                               , nextStringId = 0
                               , stringMap    = HashMap.empty
                               , variables    = HashMap.empty
                               , code         = Seq.empty
                               , dstTemp      = Nothing
                               }
        initCtx = TransContext { endLabel     = Nothing
                               , trueLabel    = Nothing
                               , falseLabel   = Nothing
                               , condValue    = False
                               , binopToRelop = False
                               }

transExprF :: TransM m => ExprF (m (Maybe Operand)) -> m (Maybe Operand)
transExprF = \case
    Expr.IntLit i -> do
        mdst <- getDstTempMaybe
        case mdst of
            Nothing  -> operandToRelop $ returnOperand (Const i)
            Just dst -> do
                emitIR (Move dst (Const i))
                return Nothing
    Expr.Var var -> do
        mdst <- getDstTempMaybe
        src <- Temp <$> getVarTemp var
        case mdst of
            Nothing -> operandToRelop (returnOperand src)
            Just dst -> do
                emitIR (Move dst src)
                return Nothing
    Expr.Neg mop -> operandToRelop $ do
        dst <- getDstTemp
        src <- withCondValue True (getOperand mop)
        emitIR (Neg dst src)
        return (Just $ Temp dst)
    Expr.Binop mop1 binop mop2 -> case binop of
        Expr.Add -> transBinop mop1 Add mop2
        Expr.Sub -> transBinop mop1 Sub mop2
        Expr.Mul -> transBinop mop1 Mul mop2
        Expr.Div -> transBinop mop1 Div mop2
        Expr.Gt  -> transRelop mop1 Gt mop2
        Expr.Lt  -> transRelop mop1 Lt mop2
        Expr.Ge  -> transRelop mop1 Ge mop2
        Expr.Le  -> transRelop mop1 Le mop2
        Expr.Eq  -> transRelop mop1 Eq mop2
        Expr.Ne  -> transRelop mop1 Ne mop2
        Expr.And -> do
            needValue <- asks condValue
            trueLbl <- newLabel
            withBinopToRelop True $ withCondValue False $
                if needValue
                   then do
                       dst <- getDstTemp
                       nextLbl <- newLabel
                       falseLbl <- newLabel
                       exitLbl <- newLabel
                       void $ withTrueFalseLabels nextLbl falseLbl mop1
                       emitIR (Label nextLbl)
                       void $ withTrueFalseLabels trueLbl falseLbl mop2
                       emitIR (Label trueLbl)
                       emitIR (Move dst (Const 1))
                       emitIR (Branch exitLbl)
                       emitIR (Label falseLbl)
                       emitIR (Move dst (Const 0))
                       emitIR (Label exitLbl)
                       return (Just $ Temp dst)
                   else do
                       void $ withTrueLabel trueLbl mop1
                       emitIR (Label trueLbl)
                       void mop2
                       return Nothing
        Expr.Or -> do
            needValue <- asks condValue
            falseLbl <- newLabel
            withBinopToRelop True $ withCondValue False $
                if needValue
                   then do
                       dst <- getDstTemp
                       nextLbl <- newLabel
                       trueLbl <- newLabel
                       exitLbl <- newLabel
                       void $ withTrueFalseLabels trueLbl nextLbl mop1
                       emitIR (Label nextLbl)
                       void $ withTrueFalseLabels trueLbl falseLbl mop2
                       emitIR (Label falseLbl)
                       emitIR (Move dst (Const 0))
                       emitIR (Branch exitLbl)
                       emitIR (Label trueLbl)
                       emitIR (Move dst (Const 1))
                       emitIR (Label exitLbl)
                       return (Just $ Temp dst)
                   else do
                       void $ withFalseLabel falseLbl mop1
                       emitIR (Label falseLbl)
                       void mop2
                       return Nothing
    Expr.Call fn margs -> do
        dst <- getDstTemp
        args <- traverse (withCondValue True . getOperand) margs
        emitIR (Call dst fn args)
        returnOperand (Temp dst)
    Expr.Echo echoargs -> do
        operands <- forM echoargs $ \case
            Expr.StrArg str  -> EchoString <$> getStringId str
            Expr.ExprArg mop -> do
                op <- withCondValue True (getOperand mop)
                case op of
                    Const c -> return (EchoConst c)
                    Temp t  -> return (EchoTemp t)
        mapM_ (emitIR . Echo) operands
        return Nothing
    Expr.Input -> do
        dst <- getDstTemp
        emitIR (Input dst)
        returnOperand (Temp dst)
    Expr.Assign var mop -> do
        vars <- gets variables
        case HashMap.lookup var vars of
            Nothing -> do
                dst <- newTemp
                setDstTemp dst
                void $ withCondValue True mop
                insertVarTemp var dst
            Just dst -> do
                setDstTemp dst
                void $ withCondValue True mop
        return Nothing
    Expr.If mcond mth mel -> do
        trueLbl <- newLabel
        case mel of
            Nothing -> do
                (thCode, falseLbl) <- backpatchIfNeed
                    (\falseLbl -> withTrueFalseLabels trueLbl falseLbl mth)
                withTrueFalseLabels trueLbl falseLbl $ do
                    void $ withBinopToRelop True mcond
                    emitIR (Label trueLbl)
                    modify (\st -> st { code = code st >< thCode })
                    emitIR (Label falseLbl)
            Just el -> do
                falseLbl <- newLabel
                (elCode, nextLbl) <- backpatchIfNeed
                    (const $ withTrueFalseLabels trueLbl falseLbl el)
                withTrueFalseLabels trueLbl falseLbl $ do
                    void $ withBinopToRelop True mcond
                    emitIR (Label trueLbl)
                    void mth
                    emitIR (Branch nextLbl)
                    emitIR (Label falseLbl)
                    modify (\st -> st { code = code st >< elCode })
                    emitIR (Label nextLbl)
        return Nothing
    Expr.While mcond mbody -> do
        beginLbl <- getBeginLabel
        trueLbl <- newLabel
        falseLbl <- newLabel
        withEndLabel falseLbl $
            withTrueFalseLabels trueLbl falseLbl $ do
                emitIR (Label beginLbl)
                void $ withBinopToRelop True mcond
                emitIR (Label trueLbl)
                void mbody
                emitIR (Branch beginLbl)
                emitIR (Label falseLbl)
        return Nothing
    Expr.Break -> do
        lbl <- getEndLabel
        emitIR (Branch lbl)
        return Nothing
    Expr.Seq seq -> do
        sequence_ seq
        return Nothing
    Expr.Return mret -> do
        ret <- fromMaybe (return Nothing) (withCondValue True <$> mret)
        emitIR (Return ret)
        return Nothing
  where returnOperand = return . Just

operandToRelop :: TransM m => m (Maybe Operand) -> m (Maybe Operand)
operandToRelop op = do
    binToRel <- asks binopToRelop
    if binToRel
       then transRelop op Ne (return $ Just $ Const 0)
       else op

transBinop :: TransM m
           => m (Maybe Operand)
           -> Binop
           -> m (Maybe Operand)
           -> m (Maybe Operand)
transBinop mop1 binop mop2 = operandToRelop $ do
    dst <- getDstTemp
    src1 <- withCondValue True (getOperand mop1)
    src2 <- withCondValue True (getOperand mop2)
    emitIR (Binop binop dst src1 src2)
    return (Just $ Temp dst)

transRelop :: TransM m
           => m (Maybe Operand)
           -> Relop
           -> m (Maybe Operand)
           -> m (Maybe Operand)
transRelop mop1 relop mop2 = do
    needValue <- asks condValue
    src1 <- withBinopToRelop False $ getOperand mop1
    src2 <- withBinopToRelop False $ getOperand mop2

    (trueLbl, falseLbl) <- if needValue
                              then (,) <$> newLabel <*> newLabel
                              else (,) <$> getTrueLabel <*> getFalseLabel
    emitIR (BranchIf relop src1 src2 trueLbl)

    res <- if needValue
              then do
                  dst <- getDstTemp
                  emitIR (Move dst (Const 0))
                  emitIR (Branch falseLbl)
                  emitIR (Label trueLbl)
                  emitIR (Move dst (Const 1))
                  emitIR (Label falseLbl)
                  return (Just $ Temp dst)
              else do
                  emitIR (Branch falseLbl)
                  return Nothing

    return res

backpatchIfNeed :: TransM m => (Label -> m a) -> m (Seq IR, Label)
backpatchIfNeed ma = do
    newLbl <- newLabel
    bakCode <- gets code
    modify (\st -> st { code = Seq.empty })
    void $ ma newLbl
    newCode <- gets code
    modify (\st -> st { code = bakCode })
    return $ case viewr newCode of
        EmptyR                -> (newCode, newLbl)
        restCode :> lastInstr -> case lastInstr of
            Label lbl
              | lbl == newLbl -> (restCode, newLbl)
              | otherwise     -> (patch restCode newLbl lbl, lbl)
            _                 -> (newCode, newLbl)
  where patch pcode repLbl toLbl = case viewr pcode of
            EmptyR -> pcode
            restCode :> lastInstr -> case lastInstr of
                Label lbl
                  | lbl == repLbl -> patch restCode repLbl toLbl |> Label toLbl
                  | otherwise     -> patch restCode repLbl toLbl |> lastInstr
                _                 -> patch restCode repLbl toLbl |> lastInstr

emitIR :: TransM m => IR -> m ()
emitIR ir = modify (\st -> st { code = code st |> ir })

getOperand :: TransM m => m (Maybe Operand) -> m Operand
getOperand ma = do
    mop <- ma
    case mop of
        Just op -> return op
        Nothing -> throwError "unexpected nothing instead of operand"

newTemp :: TransM m => m Temp
newTemp = do
    temp@(T tid) <- gets nextTemp
    modify (\st -> st { nextTemp = T (tid + 1) })
    return temp

newLabel :: TransM m => m Label
newLabel = do
    lbl@(L lid) <- gets nextLabel
    modify (\st -> st { nextLabel = L (lid + 1) })
    return lbl

getStringId :: TransM m => Text -> m Int
getStringId str = do
    strIds <- gets stringMap
    case HashMap.lookup str strIds of
        Nothing -> do
            nextId <- gets nextStringId
            let newStrIds = HashMap.insert str nextId strIds
            modify $ \st -> st { nextStringId = nextId + 1
                               , stringMap    = newStrIds
                               }
            return nextId
        Just strId -> return strId

getBeginLabel :: TransM m => m Label
getBeginLabel = do
    codeSeq <- gets code
    case viewr codeSeq of
        EmptyR -> newLabel
        restCode :> lastInstr -> case lastInstr of
            Label lbl -> do
                modify (\st -> st { code = restCode })
                return lbl
            _ -> newLabel

insertVarTemp :: TransM m => Text -> Temp -> m ()
insertVarTemp var temp =
    modify (\st -> st { variables = HashMap.insert var temp $ variables st })

newVarTemp :: TransM m => Text -> m Temp
newVarTemp var = do
    vars <- gets variables
    case HashMap.lookup var vars of
        Nothing -> do
            temp <- newTemp
            modify (\st -> st { variables = HashMap.insert var temp $ variables st })
            return temp
        Just temp -> return temp

getVarTemp :: TransM m => Text -> m Temp
getVarTemp var = do
    mtemp <- gets (HashMap.lookup var . variables)
    maybe (throwError $ "undefined variable " <> var) return mtemp

withEndLabel :: TransM m => Label -> m a -> m a
withEndLabel lbl = local (\ctx -> ctx { endLabel = Just lbl })

withTrueLabel :: TransM m => Label -> m a -> m a
withTrueLabel lbl = local (\ctx -> ctx { trueLabel = Just lbl })

withFalseLabel :: TransM m => Label -> m a -> m a
withFalseLabel lbl = local (\ctx -> ctx { falseLabel = Just lbl })

withTrueFalseLabels :: TransM m => Label -> Label -> m a -> m a
withTrueFalseLabels trueLbl falseLbl = local $ \ctx -> ctx { trueLabel  = Just trueLbl
                                                           , falseLabel = Just falseLbl
                                                           }

withCondValue :: TransM m => Bool -> m a -> m a
withCondValue val = local (\ctx -> ctx { condValue = val })

getDstTemp :: TransM m => m Temp
getDstTemp = do
    mdst <- getDstTempMaybe
    fromMaybe newTemp (return <$> mdst)

getDstTempMaybe :: TransM m => m (Maybe Temp)
getDstTempMaybe = do
    mdst <- gets dstTemp
    modify (\st -> st { dstTemp = Nothing })
    return mdst

setDstTemp :: TransM m => Temp -> m ()
setDstTemp temp = modify (\st -> st { dstTemp = Just temp })

withBinopToRelop :: TransM m => Bool -> m a -> m a
withBinopToRelop val = local (\ctx -> ctx { binopToRelop = val })

getEndLabel :: TransM m => m Label
getEndLabel = do
    lbl <- asks endLabel
    maybe (throwError "end label doesn't exist") return lbl

getTrueLabel :: TransM m => m Label
getTrueLabel = do
    lbl <- asks trueLabel
    maybe (throwError "true label doesn't exist") return lbl

getFalseLabel :: TransM m => m Label
getFalseLabel = do
    lbl <- asks falseLabel
    maybe (throwError "false label doesn't exist") return lbl
