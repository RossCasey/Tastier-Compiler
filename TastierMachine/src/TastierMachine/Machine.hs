{-# LANGUAGE DoAndIfThenElse #-}
{- |
  This module contains the implementation of the virtual machine which
  executes the instruction set from "TastierMachine.Instructions".

  The virtual processor has 4096 words of random-access data memory,
  and 4096 words of stack memory.

  The size of a machine word is 16 bits, and the storage format is
  big-endian (the first byte in the word is the most significant),
  or in other words, the two bytes in the word are 16 continuous
  bits in memory, going from most significant (bit 15) down to least
  significant (bit 0).

  The machine has three state registers which can be loaded onto the
  stack for manipulation, and stored back again.
  Our calling convention for procedures is as follows:

  stack frame layout and pointer locations:                 DMA
                                                            DMA
        *                         *                         DMA
  top ->*                         *                         DMA
        * local variables         *                         DMA
        ***************************                         DMA
        * dynamic link (dl)       *                         DMA
        * static link (sl)        *                         DMA
        * lexic level delta (lld) *                         DMA
  bp -> * return address          *                         DMA
        ***************************                         DMA
                                                            DMA
  dl  - rbp of calling procedure's frame for popping stack  DMA
  sl  - rbp of enclosing procedure for addressing variables DMA
  lld - ll difference (delta) between a called procedure    DMA
        and its calling procudure                           DMA

-}
module TastierMachine.Machine where
import qualified TastierMachine.Instructions as Instructions
import Data.Int (Int8, Int16)
import Data.Char (intToDigit,chr)
import Numeric (showIntAtBase)
import Data.Bits (complement)
import Data.Array ((//), (!), Array, elems)
import Control.Monad.RWS.Lazy (RWS, put, get, ask, tell, local)
import System.IO.Unsafe (unsafePerformIO)
import System.IO (hFlush, stdout)
import Data.List (intersperse, intercalate)

{-- Int is a pointer to the address in memory --}
stringPrint :: Int -> [Int16] -> String
stringPrint addr listA = let ptAddr = addr - 3  {-Load instructions subtract 3 -}
                             len = fromIntegral $ listA !! ptAddr
                             stAddr = ptAddr + 1
                             endAddr = stAddr + (len - 1)
                             valueList = map (listA!!) [stAddr..endAddr]
                             charList = map (chr . fromIntegral) valueList
                             str = show charList
                             stringNoQuotes = map (str!!) [1..len]
                         in stringNoQuotes

intPrint :: Int -> String
intPrint num = show (fromIntegral num)

{-- Returns every second element, starting with the first element --}
everyEven :: (Num a) => (Eq a) => [a] -> [a]
everyEven list = case drop 1 list of
                  (x:xs) -> x : everyEven xs
                  [] -> []

{-- Returns every second element, starting with the second element --}
everyOdd :: (Num a) => (Eq a) => [a] -> [a]
everyOdd (x:xs) = (everyEven (x:(x:xs)))

{-- Calls the relevant print depending on whether arg is string or int --}
valueOrPointer :: (Int, Int) -> [Int16] -> String
valueOrPointer (1,value) _ = (intPrint (fromIntegral value))
valueOrPointer (3,pointer) mem = (stringPrint pointer mem)
valueOrPointer (_,_) _ = error "Invalid argument for write statement"


debug' m@(Machine rpc rtp rbp imem _ _) = do {
  putStrLn $
    concat $
      intersperse "\t| " $
        (zipWith (++)
          ["rpc: ", "rtp: ", "rbp: "]
          [show rpc, show rtp, show rbp])
        ++
        [(show $ imem ! rpc)];
  hFlush stdout;
  return m
}

debug = unsafePerformIO . debug'

data Machine = Machine { rpc :: Int16,  -- ^ next instruction to execute
                         rtp :: Int16,  -- ^ top of the stack
                         rbp :: Int16,  -- ^ base of the stack

                         imem :: (Array Int16 Instructions.InstructionWord),
                                                      -- ^ instruction memory
                         dmem :: (Array Int16 Int16), -- ^ data memory
                         smem :: (Array Int16 Int16)  -- ^ stack memory
                       }
                       deriving (Show)

{-
  This function implements the internal state machine executing the
  instructions.
-}

run :: RWS [Int16] [String] Machine ()
run = do
  machine'@(Machine rpc rtp rbp imem dmem smem) <- get
  let machine = debug machine'
  let instructionWord = imem ! rpc

  case instructionWord of
    Instructions.Nullary i ->
      case i of
        Instructions.Halt -> do
          return ()

        Instructions.Dup -> do
          put $ machine { rpc = rpc + 1, rtp = rtp + 1,
                          smem = (smem // [(rtp, smem ! (rtp-1))]) }
          run

        Instructions.Nop -> do
          put $ machine { rpc = rpc + 1 }
          run

        Instructions.Add -> do
          let a = smem ! (rtp-1)
          let b = smem ! (rtp-2)
          let result = b + a
          put $ machine { rpc = rpc + 1, rtp = rtp - 1,
                          smem = (smem // [(rtp-2, result)]) }
          run

        Instructions.Sub    -> do
          let a = smem ! (rtp-1)
          let b = smem ! (rtp-2)
          let result = b - a
          put $ machine { rpc = rpc + 1, rtp = rtp - 1,
                          smem = (smem // [(rtp-2, result)]) }
          run

        Instructions.Mul    -> do
          let a = smem ! (rtp-1)
          let b = smem ! (rtp-2)
          let result = b * a
          put $ machine { rpc = rpc + 1, rtp = rtp - 1,
                          smem = (smem // [(rtp-2, result)]) }
          run

        Instructions.Div    -> do
          let a = smem ! (rtp-1)
          let b = smem ! (rtp-2)
          let result = b `div` a
          put $ machine { rpc = rpc + 1, rtp = rtp - 1,
                          smem = (smem // [(rtp-2, result)]) }
          run

        Instructions.Equ    -> do
          let a = smem ! (rtp-1)
          let b = smem ! (rtp-2)
          let result = fromIntegral $ fromEnum (b == a)
          put $ machine { rpc = rpc + 1, rtp = rtp - 1,
                          smem = (smem // [(rtp-2, result)]) }
          run

        Instructions.Lss    -> do
          let a = smem ! (rtp-1)
          let b = smem ! (rtp-2)
          let result = fromIntegral $ fromEnum (b < a)
          put $ machine { rpc = rpc + 1, rtp = rtp - 1,
                          smem = (smem // [(rtp-2, result)]) }
          run

        Instructions.Gtr    -> do
          let a = smem ! (rtp-1)
          let b = smem ! (rtp-2)
          let result = fromIntegral $ fromEnum (b > a)
          put $ machine { rpc = rpc + 1, rtp = rtp - 1,
                          smem = (smem // [(rtp-2, result)]) }
          run

        Instructions.NtEqu  -> do
          let a = smem ! (rtp-1)
          let b = smem ! (rtp-2)
          let result = fromIntegral $ fromEnum (b /= a)
          put $ machine { rpc = rpc + 1, rtp = rtp - 1,
                          smem = (smem // [(rtp-2, result)]) }
          run

        Instructions.LssEq  -> do
          let a = smem ! (rtp-1)
          let b = smem ! (rtp-2)
          let result = fromIntegral $ fromEnum (b <= a)
          put $ machine { rpc = rpc + 1, rtp = rtp - 1,
                          smem = (smem // [(rtp-2, result)]) }
          run

        Instructions.GtrEq  -> do
          let a = smem ! (rtp-1)
          let b = smem ! (rtp-2)
          let result = fromIntegral $ fromEnum (b >= a)
          put $ machine { rpc = rpc + 1, rtp = rtp - 1,
                          smem = (smem // [(rtp-2, result)]) }
          run

        Instructions.Neg    -> do
          let a = smem ! (rtp-1)
          let result = complement a
          put $ machine { rpc = rpc + 1, smem = (smem // [(rtp-1, result)]) }
          run


        Instructions.Ret    -> do
          {-
            The return address is on top of stack, set the pc to that address
          -}
          put $ machine { rpc = (smem ! (rtp-1)), rtp = rtp - 1 }
          run


        Instructions.RetValue -> do
          {-
          The return address is on top of stack, set the pc to that address
          -}
          let returnValue = smem ! (rtp-1)
          put $ machine { rpc = rpc + 1, rtp = rtp - 1, smem = (smem // [(rbp-1, returnValue)]) }
          run

        Instructions.Read   -> do
          value <- ask
	  case value of
            (i:rest) -> do
              put $ machine { rpc = rpc + 1, rtp = rtp + 1,
                              smem = (smem // [(rtp, i)]) }
              local tail run
            [] -> error $ "Read instruction issued but no data left to read"

        Instructions.Write  -> do
          tell $ [show $ smem ! (rtp-1)]
          put $ machine { rpc = rpc + 1, rtp = rtp - 1 }
          run


        Instructions.WriteStr  -> do
          {-- Get pointer from stack --}

          let ptAddr = fromIntegral $ smem ! (rtp-1)

          {-- Convert data memory to list --}
          let memList = elems dmem

          {-- Get output string --}
          let str = (stringPrint ptAddr memList)
          {-- Remove quotes from string --}
          let strNoQuotes = filter (/='"') str
          tell $ [strNoQuotes]
          put $ machine { rpc = rpc + 1, rtp = rtp - 1 }
          run



        {-
          This function takes a number of arguments on the stack in the following format:
          Stack: <top> [2][1][intValue][3][pointer to string]
          where:
                  - 2 is the number of arguments passed to the write function
                  - 1 is the type of intValue (1 = integer, 3 = string)
                  - 3 is the type of pointer to string

          The function will print all the arguments passed to it on the stack on a single line.
        -}
        Instructions.WriteMul  -> do
          {-- Get the number of arguments involved in the print statement --}
          let numArgs = (fromIntegral(smem ! (rtp-1))) * 2

          {-- Get a list of the arguments from the stack --}
          let args = map (fromIntegral . (smem!)) [(fromIntegral(rtp - 2)),((fromIntegral(rtp - 2))-1)..(fromIntegral(rtp - (numArgs + 1)))]

          {-- Split list into type list and value list --}
          let typeList = everyOdd args
          let valueList = everyEven args

          {-- Combine list into (type,value) tuple --}
          let revTupleList = zip typeList valueList
          let tupleList = reverse revTupleList

          {-- Convert data memory to list --}
          let memList = elems dmem

          {-- Get output string for each argument --}
          let strings = map (\x -> (valueOrPointer x memList)) tupleList

          {-- Combine string list into string --}
          let combStr = intercalate "" strings

          {-- Remove quotes from string --}
          let betterString = filter (/='"') combStr
          tell $ [betterString]
          put $ machine { rpc = rpc + 1, rtp = rtp - (1 + numArgs) }
          run

        {-
          This function loads the value from memory that is pointed to
          by the value at the top of the stack and places the value on
          the top of the stack
        -}
        Instructions.MemLoad  -> do
          let address = (smem ! (rtp-1))
          let adjustedMemory = address - 3
          let result = dmem ! adjustedMemory
          put $ machine { rpc = rpc + 1, smem = (smem // [(rtp-1, result)]) }
          run

        {-
          This function loads the value from the stack that is pointed to by
          the value at the top fo the stack. And places the loaded value on top
          of the stack
        -}
        Instructions.StackLoad  -> do
          let address = (smem ! (rtp-1))
          let result = smem ! address
          put $ machine { rpc = rpc + 1, smem = (smem // [(rtp-1, result)]) }
          run

        {-
          This function stores the value on the top of the stack into the memory
          location that is pointed to by the next highest value on the stack.
        -}
        Instructions.MemStore  -> do
          let value = (smem ! (rtp - 1))
          let address = (smem ! (rtp - 2))
          let adjustedMemory = address - 3
          put $ machine { rpc = rpc + 1, rtp = rtp - 2,
                          dmem = (dmem // [(adjustedMemory, value)]) }
          run


        {-
          This function stores the second highest value on the stack into
          the stack position pointed to by the value at the top of the stack 
        -}
        Instructions.StackStore -> do
          let value = (smem ! (rtp - 2))
          let address = (smem ! (rtp - 1))
          put $ machine { rpc = rpc + 1, rtp = rtp - 2,
                          smem = (smem // [(address,value)]) }
          run


        Instructions.Leave  -> do
          {-
            When we're leaving a procedure, we have to reset rbp to the
            value it had in the calling context. Our calling convention is
            that we store the return address at the bottom of the stack
            frame (at rbp), and the old base pointer in the dynamic link
            field (at rbp+3).

            We reset rbp to whatever the value of the dynamic link field is.
            Since we created the stack frame for this procedure, the one
            from which we're now returning, at the *top* of the stack when
            it was called, we know that when we called the procedure, rtp
            was equal to whatever value rbp has *at present*. However, we
            want to leave the return address on the top of the stack for RET
            to jump to. We can simply set rtp to one past the current rbp
            (effectively popping off the local variables of this procedure,
            the dynamic link, static link, and lexical level delta fields).
          -}
          put $ machine { rpc = rpc + 1, rtp = rbp+1, rbp = (smem ! (rbp+3)) }
          run

    Instructions.Unary i a ->
      case i of
        Instructions.StoG   -> do
          -- memory mapped control and status registers implemented here
          case a of
            0 -> put $ machine { rpc = (smem ! (rtp-1)), rtp = rtp - 1 }
            1 -> put $ machine { rpc = rpc + 1, rtp = (smem ! (rtp-1)) }
            2 -> put $ machine { rpc = rpc + 1, rtp = rtp - 1,
                                 rbp = (smem ! (rtp-1)) }
            _ -> put $ machine { rpc = rpc + 1, rtp = rtp - 1,
                                 dmem = (dmem // [(a-3, (smem ! (rtp-1)))]) }
          run

        Instructions.LoadG  -> do
          -- memory mapped control and status registers implemented here
          case a of
            0 -> put $ machine { rpc = rpc + 1, rtp = rtp + 1,
                                 smem = (smem // [(rtp, rpc)]) }
            1 -> put $ machine { rpc = rpc + 1, rtp = rtp + 1,
                                 smem = (smem // [(rtp, rtp)]) }
            2 -> put $ machine { rpc = rpc + 1, rtp = rtp + 1,
                                 smem = (smem // [(rtp, rbp)]) }
            _ -> put $ machine { rpc = rpc + 1, rtp = rtp + 1,
                                 smem = (smem // [(rtp, (dmem ! (a-3)))]) }
          run

        Instructions.Const  -> do
          put $ machine { rpc = rpc + 1, rtp = rtp + 1,
                          smem = (smem // [(rtp, a)]) }
          run

        Instructions.Enter  -> do
          {-
            ENTER has to set up both the static and dynamic link fields of
            the stack frame which is being entered. The dynamic link (the
            base pointer of the stack frame from which this procedure was
            called) needs to go on the top of stack, as specified by our
            calling convention.

            The static link (the base pointer of the stack frame belonging
            to the *enclosing* procedure (the one where this procedure was
            defined, and we should have access to the local variables)
            should be placed at rbp+2. If we are calling a procedure
            which is defined in the same scope, then the static link is just
            a copy of whatever the current static link is. However, if that
            isn't the case, then we need to call followChain to find out
            what the base pointer was in the stack frame where the procedure
            we're entering was defined.
          -}
          let lexicalLevelDelta = (smem ! (rtp-1))
          let staticLink = (followChain 0 lexicalLevelDelta rbp smem)
          put $ machine { rpc = rpc + 1, rtp = rtp+a+2, rbp = rtp-2,
                          smem = (smem // [(rtp, staticLink), (rtp+1, rbp)]) }
          run

        Instructions.Jmp  -> do
          put $ machine { rpc = a }
          run

        Instructions.FJmp -> do
          let jump = smem ! (rtp-1)
          if jump == 0 then do
            put $ machine { rtp = rtp - 1, rpc = a }
            run
          else do
            put $ machine { rtp = rtp - 1, rpc = rpc + 1 }
            run

    Instructions.Binary i a b ->
      case i of
        Instructions.Load   -> do
          {-
            Load gets a variable from a calling frame onto the top of the
            stack. We follow the chain of links to find the stack frame the
            variable is in, add b (the address of the variable in that
            frame) and add two, because each frame has the dynamic link and
            static link stored just before the start of the real stack
            variables, but it's the static link whose address we get from
            followChain.
          -}
          let loadAddr = (followChain 0 a rbp smem) + 4 + b
          put $ machine { rpc = rpc + 1, rtp = rtp + 1,
                          smem = (smem // [(rtp, (smem ! loadAddr))]) }
          run

        Instructions.Sto    -> do --Store updates a variable in a calling frame
          let storeAddr = (followChain 0 a rbp smem) + 4 + b
          put $ machine { rpc = rpc + 1, rtp = rtp - 1,
                          smem = (smem // [(storeAddr, (smem ! (rtp-1)))]) }
          run



        Instructions.Call   -> do
          {-
            CALL gets passed the lexical level delta in slot a, and the
            address of the procedure in slot b. CALL pushes the return
            address onto the stack, then the lexical level delta, so when
            the called procedure does ENTER, the stack contains the lexical
            level delta at (rtp - 1) and the return address at (rtp - 2).
          -}
          put $ machine { rpc = b, rtp = rtp + 2,
                          smem = (smem // [(rtp, (rpc+1)), (rtp+1, a)]) }
          run

        {--
        Instructions.CallNonVoid   -> do
          {-
          CALL gets passed the lexical level delta in slot a, and the
          address of the procedure in slot b. CALL pushes the return
          address onto the stack, then the lexical level delta, so when
          the called procedure does ENTER, the stack contains the lexical
          level delta at (rtp - 1) and the return address at (rtp - 2).

          One extra space is left on the stack to store the return value
          of the function.
          -}
          put $ machine { rpc = b, rtp = rtp + 3,
                          smem = (smem // [(rtp+1, (rpc+1)), (rtp+2, a)]) }
          run
        --}
{-
  followChain follows the static link chain to find the absolute address in
  stack memory of the base of the stack frame (n-limit) levels down the call
  stack. Each time we unwind one call, we recurse with rbp set to the base
  pointer of the stack frame which we just unwound. When we've unwound the
  correct number of stack frames, as indicated by the argument *limit*, we
  return the base pointer of the stack frame we've unwound our way into.
-}

followChain :: Int16 -> Int16 -> Int16 -> (Array Int16 Int16) -> Int16
followChain limit n rbp smem =
  if n > limit then
    followChain limit (n-1) (smem ! (rbp+2)) smem
  else rbp
