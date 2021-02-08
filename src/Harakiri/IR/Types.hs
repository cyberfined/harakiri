module Harakiri.IR.Types
    ( IR(..)
    , Temp(..)
    , Label(..)
    , Operand(..)
    , EchoOperand(..)
    , Binop(..)
    , Relop(..)
    , showFunction
    , showIR
    , showOperand
    , showEchoOperand
    , showTemp
    , showLabel
    , showBinop
    , showRelop
    ) where

import Data.Text (Text, pack, intercalate)

import Harakiri.Expr.Types (Function(..), showFunctionType)

showFunction :: Function Temp [IR] -> Text
showFunction fn =  "def " <> funName fn
                <> "(" <> textArgs <> ")" <> showFunctionType (funType fn)
                <> " {\n" <> textBody <> "\n}"
  where textArgs = intercalate "," $ map showTemp $ funArgs fn
        textBody = foldl (\str ir -> str <> showIR ir <> "\n") "" (funBody fn)

data IR
    = Neg !Temp !Operand
    | Binop !Binop !Temp !Operand !Operand
    | Move !Temp !Operand
    | Input !Temp
    | CallFunc !Temp !Text ![Operand]
    | CallProc !Text ![Operand]
    | Echo !EchoOperand
    | Load !Temp !Operand
    | Save !Operand !Operand
    | Label !Label
    | Branch !Label
    | BranchIf !Relop !Operand !Operand !Label
    | Return !(Maybe Operand)

showIR :: IR -> Text
showIR = \case
    Neg dst src -> "neg " <> showTemp dst <> ", " <> showOperand src
    Binop op dst src1 src2 -> showBinop op <> " " <> showTemp dst
                           <> ", " <> showOperand src1
                           <> ", " <> showOperand src2
    Move dst src -> "move " <> showTemp dst <> ", " <> showOperand src
    Input dst    -> "input " <> showTemp dst
    CallFunc dst fn args -> "call " <> showTemp dst <> ", " <> fn
                     <> "(" <> intercalate ", " (map showOperand args) <> ")"
    CallProc fn args -> "call " <> fn
                     <> "(" <> intercalate ", " (map showOperand args) <> ")"
    Echo src     -> "echo " <> showEchoOperand src
    Load dst src -> "load " <> showTemp dst <> ", " <> showOperand src
    Save src dst -> "save " <> showOperand src <> ", " <> showOperand dst
    Label lbl    -> showLabel lbl <> ":"
    Branch lbl   -> "goto " <> showLabel lbl
    BranchIf op src1 src2 lbl -> "if " <> showOperand src1 <> " " <> showRelop op <> " "
                              <> showOperand src2 <> " goto " <> showLabel lbl
    Return msrc -> "ret" <> maybe "" (\src -> " " <> showOperand src) msrc

data Operand
    = Temp !Temp
    | Const !Int

showOperand :: Operand -> Text
showOperand = \case
    Temp t  -> showTemp t
    Const c -> pack (show c)

data EchoOperand
    = EchoTemp !Temp
    | EchoConst !Int
    | EchoString !Int

showEchoOperand :: EchoOperand -> Text
showEchoOperand = \case
    EchoTemp t   -> showTemp t
    EchoConst c  -> pack (show c)
    EchoString s -> "str(" <> pack (show s) <> ")"

newtype Temp = T Int

showTemp :: Temp -> Text
showTemp (T t) = "t" <> pack (show t)

newtype Label = L Int deriving Eq

showLabel :: Label -> Text
showLabel (L l) = "l" <> pack (show l)

data Binop = Add | Sub | Mul | Div

showBinop :: Binop -> Text
showBinop = \case
    Add -> "add"
    Sub -> "sub"
    Mul -> "mul"
    Div -> "div"

data Relop
    = Lt | Gt | Le | Ge
    | Eq | Ne

showRelop :: Relop -> Text
showRelop = \case
    Lt -> "lt"
    Gt -> "gt"
    Le -> "le"
    Ge -> "ge"
    Eq -> "eq"
    Ne -> "ne"
