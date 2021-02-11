module Harakiri.IR.Types
    ( ShowReg(..)
    , IR(..)
    , Temp(..)
    , Label(..)
    , Operand(..)
    , EchoOperand(..)
    , MoveOperand(..)
    , Binop(..)
    , Relop(..)
    , showFunction
    , showIR
    , showOperand
    , showEchoOperand
    , showMoveOperand
    , operandToMove
    , showTemp
    , showLabel
    , showBinop
    , showRelop
    ) where

import Data.Text (Text, intercalate)

import Harakiri.Expr.Types (Function(..), showFunctionType)
import Harakiri.Utils

class ShowReg a where
    showReg :: a -> Text

showFunction :: ShowReg a => Function a [IR a] -> Text
showFunction fn =  "def " <> funName fn
                <> "(" <> textArgs <> ")" <> showFunctionType (funType fn)
                <> " {\n" <> textBody <> "\n}"
  where textArgs = intercalate "," $ map showReg $ funArgs fn
        textBody = foldl (\str ir -> str <> showIR ir <> "\n") "" (funBody fn)

data IR a
    = Neg !a !(Operand a)
    | Binop !Binop !a !(Operand a) !(Operand a)
    | Move !a !(MoveOperand a)
    | Input !a
    | CallFunc !a !Text ![Operand a]
    | CallProc !Text ![Operand a]
    | Echo !(EchoOperand a)
    | Load !a !(Operand a)
    | Save !(MoveOperand a) !(Operand a)
    | Label !Label
    | Branch !Label
    | BranchIf !Relop !(Operand a) !(Operand a) !Label
    | Return !(Maybe (Operand a))

showIR :: ShowReg a => IR a -> Text
showIR = \case
    Neg dst src -> "neg " <> showReg dst <> ", " <> showOperand src
    Binop op dst src1 src2 -> showBinop op <> " " <> showReg dst
                           <> ", " <> showOperand src1
                           <> ", " <> showOperand src2
    Move dst src -> "move " <> showReg dst <> ", " <> showMoveOperand src
    Input dst    -> "input " <> showReg dst
    CallFunc dst fn args -> "call " <> showReg dst <> ", " <> fn
                     <> "(" <> intercalate ", " (map showOperand args) <> ")"
    CallProc fn args -> "call " <> fn
                     <> "(" <> intercalate ", " (map showOperand args) <> ")"
    Echo src     -> "echo " <> showEchoOperand src
    Load dst src -> "load " <> showReg dst <> ", " <> showOperand src
    Save src dst -> "save " <> showMoveOperand src <> ", " <> showOperand dst
    Label lbl    -> showLabel lbl <> ":"
    Branch lbl   -> "goto " <> showLabel lbl
    BranchIf op src1 src2 lbl -> "if " <> showOperand src1 <> " " <> showRelop op <> " "
                              <> showOperand src2 <> " goto " <> showLabel lbl
    Return msrc -> "ret" <> maybe "" (\src -> " " <> showOperand src) msrc

data Operand a
    = Temp !a
    | Const !Int

showOperand :: ShowReg a => Operand a -> Text
showOperand = \case
    Temp t  -> showReg t
    Const c -> showText c

data EchoOperand a
    = EchoTemp !a
    | EchoConst !Int
    | EchoString !Int

showEchoOperand :: ShowReg a => EchoOperand a -> Text
showEchoOperand = \case
    EchoTemp t   -> showReg t
    EchoConst c  -> showText c
    EchoString s -> "str(" <> showText s <> ")"

data MoveOperand a
    = MoveTemp !a
    | MoveConst !Int
    | MoveString !StringLabel

showMoveOperand :: ShowReg a => MoveOperand a -> Text
showMoveOperand = \case
    MoveTemp t   -> showReg t
    MoveConst c  -> showText c
    MoveString s -> showStringLabel s

operandToMove :: Operand a -> MoveOperand a
operandToMove = \case
    Const i -> MoveConst i
    Temp t  -> MoveTemp t

newtype Temp = T Int

instance Enum Temp where
    toEnum = T
    fromEnum (T t) = t

instance ShowReg Temp where
    showReg = showTemp

showTemp :: Temp -> Text
showTemp (T t) = "t" <> showText t

newtype Label = L Int deriving Eq

showLabel :: Label -> Text
showLabel (L l) = "l" <> showText l

newtype StringLabel = StringLabel Int deriving Eq

showStringLabel :: StringLabel -> Text
showStringLabel (StringLabel l) = "str" <> showText l

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
