%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[MachMisc]{Description of various machine-specific things}

\begin{code}
#include "nativeGen/NCG.h"

module MachMisc (

	sizeOf, primRepToSize,

	eXTRA_STK_ARGS_HERE,

	volatileSaves, volatileRestores,

	targetMaxDouble, targetMaxInt, targetMinDouble, targetMinInt,

	underscorePrefix,
	fmtAsmLbl,
	exactLog2,

        stixFor_stdout, stixFor_stderr, stixFor_stdin,

	Instr(..),  IF_ARCH_i386(Operand(..) COMMA,)
	Cond(..),
	Size(..)
	
#if alpha_TARGET_ARCH
	, RI(..)
#endif
#if i386_TARGET_ARCH
#endif
#if sparc_TARGET_ARCH
	, RI(..), riZero
#endif
    ) where

#include "HsVersions.h"
-- #include "config.h"

import AbsCSyn		( MagicId(..) ) 
import AbsCUtils	( magicIdPrimRep )
import CLabel           ( CLabel )
import Const		( mkMachInt, Literal(..) )
import MachRegs		( stgReg, callerSaves, RegLoc(..),
			  Imm(..), Reg(..), 
			  MachRegsAddr(..)
			)
import PrimRep		( PrimRep(..) )
import SMRep		( SMRep(..) )
import Stix		( StixTree(..), StixReg(..), CodeSegment )
import Panic		( panic )
import Char		( isDigit )
import GlaExts		( word2Int#, int2Word#, shiftRL#, and#, (/=#) )
import Outputable	( text )
\end{code}

\begin{code}
underscorePrefix :: Bool   -- leading underscore on assembler labels?

#ifdef LEADING_UNDERSCORE
underscorePrefix = True
#else
underscorePrefix = False
#endif

---------------------------
fmtAsmLbl :: String -> String  -- for formatting labels

fmtAsmLbl s
  =  IF_ARCH_alpha(
     {- The alpha assembler likes temporary labels to look like $L123
	instead of L123.  (Don't toss the L, because then Lf28
	turns into $f28.)
     -}
     '$' : s
     ,{-otherwise-}
     s
     )

---------------------------
stixFor_stdout, stixFor_stderr, stixFor_stdin :: StixTree
#if i386_TARGET_ARCH
-- Linux glibc 2 / libc6
stixFor_stdout  = StInd PtrRep (StLitLbl (text "stdout"))
stixFor_stderr  = StInd PtrRep (StLitLbl (text "stderr"))
stixFor_stdin   = StInd PtrRep (StLitLbl (text "stdin"))
#endif

#if alpha_TARGET_ARCH
stixFor_stdout = error "stixFor_stdout: not implemented for Alpha"
stixFor_stderr = error "stixFor_stderr: not implemented for Alpha"
stixFor_stdin  = error "stixFor_stdin: not implemented for Alpha"
#endif

#if sparc_TARGET_ARCH
stixFor_stdout = error "stixFor_stdout: not implemented for Sparc"
stixFor_stderr = error "stixFor_stderr: not implemented for Sparc"
stixFor_stdin  = error "stixFor_stdin: not implemented for Sparc"
#endif

#if 0
Here's some old stuff from which it shouldn't be too hard to
implement the above for Alpha/Sparc.

cvtLitLit :: String -> String

--
-- Rather than relying on guessing, use FILE_SIZE to compute the
-- _iob offsets.
--
cvtLitLit "stdin"  = IF_ARCH_alpha("_iob+0" {-probably OK...-}
		    ,IF_ARCH_i386("stdin"
		    ,IF_ARCH_sparc("__iob+0x0"{-probably OK...-}
		    ,)))

cvtLitLit "stdout" = IF_ARCH_alpha("_iob+"++show (``FILE_SIZE''::Int)
		    ,IF_ARCH_i386("stdout"
		    ,IF_ARCH_sparc("__iob+"++show (``FILE_SIZE''::Int)
		    ,)))
cvtLitLit "stderr" = IF_ARCH_alpha("_iob+"++show (2*(``FILE_SIZE''::Int))
		    ,IF_ARCH_i386("stderr"
		    ,IF_ARCH_sparc("__iob+"++show (2*(``FILE_SIZE''::Int))
		    ,)))
#endif

\end{code}

% ----------------------------------------------------------------

We (allegedly) put the first six C-call arguments in registers;
where do we start putting the rest of them?
\begin{code}
eXTRA_STK_ARGS_HERE :: Int
eXTRA_STK_ARGS_HERE
  = IF_ARCH_alpha(0, IF_ARCH_i386(23{-6x4bytes-}, IF_ARCH_sparc(23,???)))
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Size of a @PrimRep@, in bytes.

\begin{code}
sizeOf :: PrimRep -> Integer{-in bytes-}
    -- the result is an Integer only because it's more convenient

sizeOf pr = case (primRepToSize pr) of
  IF_ARCH_alpha({B -> 1; BU -> 1; {-W -> 2; WU -> 2; L -> 4; SF -> 4;-} _ -> 8},)
  IF_ARCH_sparc({B -> 1; BU -> 1; {-HW -> 2; HWU -> 2;-} W -> 4; {-D -> 8;-} F -> 4; DF -> 8},)
  IF_ARCH_i386( {B -> 1; {-S -> 2;-} L -> 4; F -> 4; DF -> 8 },)
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Now the volatile saves and restores.  We add the basic guys to the
list of ``user'' registers provided.  Note that there are more basic
registers on the restore list, because some are reloaded from
constants.

(@volatileRestores@ used only for wrapper-hungry PrimOps.)

\begin{code}
volatileSaves, volatileRestores :: [MagicId] -> [StixTree]

save_cands    = [BaseReg,Sp,Su,SpLim,Hp,HpLim]
restore_cands = save_cands

volatileSaves vols
  = map save ((filter callerSaves) (save_cands ++ vols))
  where
    save x = StAssign (magicIdPrimRep x) loc reg
      where
	reg = StReg (StixMagicId x)
	loc = case stgReg x of
		Save loc -> loc
		Always _ -> panic "volatileSaves"

volatileRestores vols
  = map restore ((filter callerSaves) (restore_cands ++ vols))
  where
    restore x = StAssign (magicIdPrimRep x) reg loc
      where
	reg = StReg (StixMagicId x)
	loc = case stgReg x of
		Save loc -> loc
		Always _ -> panic "volatileRestores"
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Obviously slightly weedy
(Note that the floating point values aren't terribly important.)
ToDo: Fix!(JSM)
\begin{code}
targetMinDouble = MachDouble (-1.7976931348623157e+308)
targetMaxDouble = MachDouble (1.7976931348623157e+308)
targetMinInt = mkMachInt (-2147483648)
targetMaxInt = mkMachInt 2147483647
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

This algorithm for determining the $\log_2$ of exact powers of 2 comes
from GCC.  It requires bit manipulation primitives, and we use GHC
extensions.  Tough.

\begin{code}
w2i x = word2Int# x
i2w x = int2Word# x

exactLog2 :: Integer -> Maybe Integer
exactLog2 x
  = if (x <= 0 || x >= 2147483648) then
       Nothing
    else
       case (fromInteger x) of { I# x# ->
       if (w2i ((i2w x#) `and#` (i2w (0# -# x#))) /=# x#) then
	  Nothing
       else
	  Just (toInteger (I# (pow2 x#)))
       }
  where
    shiftr x y = shiftRL# x y

    pow2 x# | x# ==# 1# = 0#
            | otherwise = 1# +# pow2 (w2i (i2w x# `shiftr` 1#))
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

\begin{code}
data Cond
#if alpha_TARGET_ARCH
  = ALWAYS	-- For BI (same as BR)
  | EQQ		-- For CMP and BI (NB: "EQ" is a 1.3 Prelude name)
  | GE		-- For BI only
  | GTT		-- For BI only (NB: "GT" is a 1.3 Prelude name)
  | LE		-- For CMP and BI
  | LTT		-- For CMP and BI (NB: "LT" is a 1.3 Prelude name)
  | NE		-- For BI only
  | NEVER	-- For BI (null instruction)
  | ULE		-- For CMP only
  | ULT		-- For CMP only
#endif
#if i386_TARGET_ARCH
  = ALWAYS	-- What's really used? ToDo
  | EQQ
  | GE
  | GEU
  | GTT
  | GU
  | LE
  | LEU
  | LTT
  | LU
  | NE
  | NEG
  | POS
#endif
#if sparc_TARGET_ARCH
  = ALWAYS	-- What's really used? ToDo
  | EQQ
  | GE
  | GEU
  | GTT
  | GU
  | LE
  | LEU
  | LTT
  | LU
  | NE
  | NEG
  | NEVER
  | POS
  | VC
  | VS
#endif
\end{code}

\begin{code}
data Size
#if alpha_TARGET_ARCH
    = B	    -- byte
    | BU
--  | W	    -- word (2 bytes): UNUSED
--  | WU    -- : UNUSED
--  | L	    -- longword (4 bytes): UNUSED
    | Q	    -- quadword (8 bytes)
--  | FF    -- VAX F-style floating pt: UNUSED
--  | GF    -- VAX G-style floating pt: UNUSED
--  | DF    -- VAX D-style floating pt: UNUSED
--  | SF    -- IEEE single-precision floating pt: UNUSED
    | TF    -- IEEE double-precision floating pt
#endif
#if i386_TARGET_ARCH
    = B	    -- byte (lower)
--  | HB    -- higher byte **UNUSED**
--  | S	    -- : UNUSED
    | L
    | F	    -- IEEE single-precision floating pt
    | DF    -- IEEE single-precision floating pt
#endif
#if sparc_TARGET_ARCH
    = B     -- byte (signed)
    | BU    -- byte (unsigned)
--  | HW    -- halfword, 2 bytes (signed): UNUSED
--  | HWU   -- halfword, 2 bytes (unsigned): UNUSED
    | W	    -- word, 4 bytes
--  | D	    -- doubleword, 8 bytes: UNUSED
    | F	    -- IEEE single-precision floating pt
    | DF    -- IEEE single-precision floating pt
#endif

primRepToSize :: PrimRep -> Size

primRepToSize PtrRep	    = IF_ARCH_alpha( Q,  IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize CodePtrRep    = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize DataPtrRep    = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize RetRep	    = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize CostCentreRep = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize CharRep	    = IF_ARCH_alpha( BU, IF_ARCH_i386( L, IF_ARCH_sparc( BU,)))
primRepToSize IntRep	    = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize WordRep	    = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize AddrRep	    = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize FloatRep	    = IF_ARCH_alpha( TF, IF_ARCH_i386( F, IF_ARCH_sparc( F ,)))
primRepToSize DoubleRep	    = IF_ARCH_alpha( TF, IF_ARCH_i386( DF,IF_ARCH_sparc( DF,)))
primRepToSize ArrayRep	    = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize ByteArrayRep  = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize WeakPtrRep    = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize ForeignObjRep  = IF_ARCH_alpha( Q, IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize StablePtrRep  = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
\end{code}

%************************************************************************
%*									*
\subsection{Machine's assembly language}
%*									*
%************************************************************************

We have a few common ``instructions'' (nearly all the pseudo-ops) but
mostly all of @Instr@ is machine-specific.

\begin{code}
data Instr
  = COMMENT FAST_STRING		-- comment pseudo-op
  | SEGMENT CodeSegment		-- {data,text} segment pseudo-op
  | LABEL   CLabel		-- global label pseudo-op
  | ASCII   Bool		-- True <=> needs backslash conversion
	    String		-- the literal string
  | DATA    Size
	    [Imm]
\end{code}

\begin{code}
#if alpha_TARGET_ARCH

-- data Instr continues...

-- Loads and stores.

	      |	LD	      Size Reg MachRegsAddr -- size, dst, src
	      | LDA	      Reg MachRegsAddr      -- dst, src
	      | LDAH	      Reg MachRegsAddr      -- dst, src
	      | LDGP	      Reg MachRegsAddr      -- dst, src
	      | LDI	      Size Reg Imm     -- size, dst, src
	      | ST	      Size Reg MachRegsAddr -- size, src, dst

-- Int Arithmetic.

	      | CLR	      Reg		    -- dst
	      | ABS	      Size RI Reg	    -- size, src, dst
	      | NEG	      Size Bool RI Reg	    -- size, overflow, src, dst
	      | ADD	      Size Bool Reg RI Reg  -- size, overflow, src, src, dst
	      | SADD	      Size Size Reg RI Reg  -- size, scale, src, src, dst
	      | SUB	      Size Bool Reg RI Reg  -- size, overflow, src, src, dst
	      | SSUB	      Size Size Reg RI Reg  -- size, scale, src, src, dst
	      | MUL	      Size Bool Reg RI Reg  -- size, overflow, src, src, dst
	      | DIV	      Size Bool Reg RI Reg  -- size, unsigned, src, src, dst
	      | REM	      Size Bool Reg RI Reg  -- size, unsigned, src, src, dst

-- Simple bit-twiddling.

	      | NOT	      RI Reg
	      | AND	      Reg RI Reg
	      | ANDNOT	      Reg RI Reg
	      | OR	      Reg RI Reg
	      | ORNOT	      Reg RI Reg
	      | XOR	      Reg RI Reg
	      | XORNOT	      Reg RI Reg
	      | SLL	      Reg RI Reg
	      | SRL	      Reg RI Reg
	      | SRA	      Reg RI Reg

	      | ZAP	      Reg RI Reg
	      | ZAPNOT	      Reg RI Reg

	      | NOP

-- Comparison

	      | CMP	      Cond Reg RI Reg

-- Float Arithmetic.

	      | FCLR	      Reg
	      | FABS	      Reg Reg
	      | FNEG	      Size Reg Reg
	      | FADD	      Size Reg Reg Reg
	      | FDIV	      Size Reg Reg Reg
	      | FMUL	      Size Reg Reg Reg
	      | FSUB	      Size Reg Reg Reg
	      | CVTxy	      Size Size Reg Reg
	      | FCMP	      Size Cond Reg Reg Reg
	      | FMOV	      Reg Reg

-- Jumping around.

	      | BI	      Cond Reg Imm
	      | BF	      Cond Reg Imm
	      | BR	      Imm
	      | JMP	      Reg MachRegsAddr Int
	      | BSR	      Imm Int
	      | JSR	      Reg MachRegsAddr Int

-- Alpha-specific pseudo-ops.

	      | FUNBEGIN CLabel
	      | FUNEND CLabel

data RI
  = RIReg Reg
  | RIImm Imm

#endif {- alpha_TARGET_ARCH -}
\end{code}

Intel, in their infinite wisdom, selected a stack model for floating
point registers on x86.  That might have made sense back in 1979 --
nowadays we can see it for the nonsense it really is.  A stack model
fits poorly with the existing nativeGen infrastructure, which assumes
flat integer and FP register sets.  Prior to this commit, nativeGen
could not generate correct x86 FP code -- to do so would have meant
somehow working the register-stack paradigm into the register
allocator and spiller, which sounds very difficult.
  
We have decided to cheat, and go for a simple fix which requires no
infrastructure modifications, at the expense of generating ropey but
correct FP code.  All notions of the x86 FP stack and its insns have
been removed.  Instead, we pretend (to the instruction selector and
register allocator) that x86 has six floating point registers, %fake0
.. %fake5, which can be used in the usual flat manner.  We further
claim that x86 has floating point instructions very similar to SPARC
and Alpha, that is, a simple 3-operand register-register arrangement.
Code generation and register allocation proceed on this basis.
  
When we come to print out the final assembly, our convenient fiction
is converted to dismal reality.  Each fake instruction is
independently converted to a series of real x86 instructions.
%fake0 .. %fake5 are mapped to %st(0) .. %st(5).  To do reg-reg
arithmetic operations, the two operands are pushed onto the top of the
FP stack, the operation done, and the result copied back into the
relevant register.  There are only six %fake registers because 2 are
needed for the translation, and x86 has 8 in total.

\begin{code}
#if i386_TARGET_ARCH

-- data Instr continues...

-- Moves.

	      |	MOV	      Size Operand Operand
	      | MOVZX	      Size Operand Operand -- size is the size of operand 2
	      | MOVSX	      Size Operand Operand -- size is the size of operand 2

-- Load effective address (also a very useful three-operand add instruction :-)

	      | LEA           Size Operand Operand

-- Int Arithmetic.

	      | ADD	      Size Operand Operand
	      | SUB	      Size Operand Operand

-- Multiplication (signed and unsigned), Division (signed and unsigned),
-- result in %eax, %edx.

	      | IMUL	      Size Operand Operand
	      | IDIV	      Size Operand

-- Simple bit-twiddling.

	      | AND	      Size Operand Operand
	      | OR	      Size Operand Operand
	      | XOR	      Size Operand Operand
	      | NOT	      Size Operand
	      | NEGI	      Size Operand -- NEG instruction (name clash with Cond)
	      | SHL	      Size Operand Operand -- 1st operand must be an Imm or CL
	      | SAR	      Size Operand Operand -- 1st operand must be an Imm or CL
	      | SHR	      Size Operand Operand -- 1st operand must be an Imm or CL
	      | NOP

-- Float Arithmetic. -- ToDo for 386

-- Note that we cheat by treating G{ABS,MOV,NEG} of doubles as single instructions
-- right up until we spit them out.

              -- all the 3-operand fake fp insns are src1 src2 dst
              -- and furthermore are constrained to be fp regs only.
    	      | GMOV	      Reg Reg -- src(fpreg), dst(fpreg)
              | GLD           Size MachRegsAddr Reg -- src, dst(fpreg)
              | GST           Size Reg MachRegsAddr -- src(fpreg), dst

    	      | GFTOD	      Reg Reg -- src(fpreg), dst(fpreg)
              | GFTOI         Reg Reg -- src(fpreg), dst(intreg)

    	      | GDTOF	      Reg Reg -- src(fpreg), dst(fpreg)
              | GDTOI         Reg Reg -- src(fpreg), dst(intreg)

              | GITOF         Reg Reg -- src(intreg), dst(fpreg)
              | GITOD         Reg Reg -- src(intreg), dst(fpreg)

	      | GADD	      Size Reg Reg Reg -- src1, src2, dst
	      | GDIV	      Size Reg Reg Reg -- src1, src2, dst
	      | GSUB	      Size Reg Reg Reg -- src1, src2, dst
	      | GMUL	      Size Reg Reg Reg -- src1, src2, dst

    	      | GCMP	      Size Reg Reg -- src1, src2

     	      | GABS	      Size Reg Reg -- src, dst
    	      | GNEG	      Size Reg Reg -- src, dst
    	      | GSQRT	      Size Reg Reg -- src, dst

-- Comparison

	      | TEST          Size Operand Operand
	      | CMP           Size Operand Operand
	      | SETCC         Cond Operand

-- Stack Operations.

	      | PUSH          Size Operand
	      | POP           Size Operand
	      | PUSHA
	      | POPA

-- Jumping around.

	      | JMP	      Operand -- target
	      | JXX	      Cond CLabel -- target
	      | CALL	      Imm

-- Other things.

	      | CLTD -- sign extend %eax into %edx:%eax

data Operand
  = OpReg  Reg	        -- register
  | OpImm  Imm	        -- immediate value
  | OpAddr MachRegsAddr	-- memory reference

#endif {- i386_TARGET_ARCH -}
\end{code}

\begin{code}
#if sparc_TARGET_ARCH

-- data Instr continues...

-- Loads and stores.

	      | LD	      Size MachRegsAddr Reg -- size, src, dst
	      | ST	      Size Reg MachRegsAddr -- size, src, dst

-- Int Arithmetic.

	      | ADD	      Bool Bool Reg RI Reg -- x?, cc?, src1, src2, dst
	      | SUB	      Bool Bool Reg RI Reg -- x?, cc?, src1, src2, dst

-- Simple bit-twiddling.

	      | AND	      Bool Reg RI Reg -- cc?, src1, src2, dst
	      | ANDN	      Bool Reg RI Reg -- cc?, src1, src2, dst
	      | OR	      Bool Reg RI Reg -- cc?, src1, src2, dst
	      | ORN	      Bool Reg RI Reg -- cc?, src1, src2, dst
	      | XOR	      Bool Reg RI Reg -- cc?, src1, src2, dst
	      | XNOR	      Bool Reg RI Reg -- cc?, src1, src2, dst
	      | SLL	      Reg RI Reg -- src1, src2, dst
	      | SRL	      Reg RI Reg -- src1, src2, dst
	      | SRA	      Reg RI Reg -- src1, src2, dst
	      | SETHI	      Imm Reg -- src, dst
	      | NOP	      -- Really SETHI 0, %g0, but worth an alias

-- Float Arithmetic.

-- Note that we cheat by treating F{ABS,MOV,NEG} of doubles as single instructions
-- right up until we spit them out.

    	      | FABS	      Size Reg Reg -- src dst
	      | FADD	      Size Reg Reg Reg -- src1, src2, dst
    	      | FCMP	      Bool Size Reg Reg -- exception?, src1, src2, dst
	      | FDIV	      Size Reg Reg Reg -- src1, src2, dst
    	      | FMOV	      Size Reg Reg -- src, dst
	      | FMUL	      Size Reg Reg Reg -- src1, src2, dst
    	      | FNEG	      Size Reg Reg -- src, dst
    	      | FSQRT	      Size Reg Reg -- src, dst
	      | FSUB	      Size Reg Reg Reg -- src1, src2, dst
    	      | FxTOy	      Size Size Reg Reg -- src, dst

-- Jumping around.

	      | BI	      Cond Bool Imm -- cond, annul?, target
    	      | BF  	      Cond Bool Imm -- cond, annul?, target

	      | JMP	      MachRegsAddr      -- target
	      | CALL	      Imm Int Bool -- target, args, terminal

data RI = RIReg Reg
	| RIImm Imm

riZero :: RI -> Bool

riZero (RIImm (ImmInt 0))	    = True
riZero (RIImm (ImmInteger 0))	    = True
riZero (RIReg (FixedReg ILIT(0)))   = True
riZero _			    = False

#endif {- sparc_TARGET_ARCH -}
\end{code}
