module TastierMachine.Instructions where
import Data.Int (Int8, Int16)
import Data.List (elem)

data Instruction = Add
                 | Sub
                 | Mul
                 | Div
                 | Equ
                 | Lss
                 | Gtr
                 | NtEqu
                 | LssEq
                 | GtrEq
                 | Neg
                 | Load
                 | Sto
                 | Call
                 | CallNonVoid
                 | LoadG
                 | StoG
                 | Const
                 | Enter
                 | Jmp
                 | FJmp
                 | Ret
                 | RetValue
                 | Leave
                 | Read
                 | Write
                 | WriteStr
                 | WriteMul
                 | MemLoad
                 | StackLoad
                 | MemStore
                 | StackStore
                 | Halt
                 | Dup
                 | Nop
                 deriving (Eq, Ord, Show, Enum)

data InstructionWord = Nullary Instruction
                     | Unary Instruction Int16
                     | Binary Instruction Int16 Int16
                     deriving (Eq, Show)

arguments :: Instruction -> Int
arguments i =
  if i `elem` [Load, Sto, Call, CallNonVoid] then 2
  else if i `elem` [LoadG, StoG, Const, Enter, Jmp, FJmp] then 1
  else 0
