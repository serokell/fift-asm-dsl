module FiftAsm.Builder
       ( buildInstr
       , indentation
       ) where

import Fmt

import FiftAsm.Instr


indentation :: Int
indentation = 2

instance Buildable Natural where
    build = build @Integer . toInteger

-- Build with indentation
buildInstr :: Instr inp out -> Builder
buildInstr (Seq a b) =
    let l = buildInstr a in
    let r = buildInstr b in
    if l == "" then r
    else l <> "\n" <> r
buildInstr Ignore = ""
buildInstr (Comment t) = "// " +| t |+ ""
buildInstr SWAP   = "SWAP"
buildInstr (PUSH (_ :: Proxy i)) = "s" <> buildWithInt (natVal @i @Proxy Proxy) "PUSH"
buildInstr (POP (_ :: Proxy i)) = "s" <> buildWithInt (natVal @i @Proxy Proxy) "POP"
buildInstr (PUSHINT x) = buildWithInt x "INT"
buildInstr TRUE = "TRUE"
buildInstr FALSE = "FALSE"
buildInstr DROP = "DROP"
buildInstr (ROLL (_ :: Proxy n)) = buildWithInt (natVal @n @Proxy Proxy) "ROLL"
buildInstr (ROLLREV (_ :: Proxy n)) = buildWithInt (natVal @n @Proxy Proxy) "ROLLREV"
buildInstr (REVERSE_PREFIX (_ :: Proxy n)) = buildWithInt (natVal @n @Proxy Proxy) "0 REVERSE"
buildInstr (XCHG (_ :: Proxy i)) = "s0 s" <> buildWithInt (natVal @i @Proxy Proxy) "XCHG"
buildInstr (XCHG' (_ :: Proxy i) (_ :: Proxy j)) =
    "s"+|natVal @i @Proxy Proxy|+" s"+|natVal @j @Proxy Proxy|+" XCHG"
buildInstr (XCHG2 (_ :: Proxy i) (_ :: Proxy j)) =
    "s"+|natVal @i @Proxy Proxy|+" s"+|natVal @j @Proxy Proxy|+" XCHG2"
buildInstr (XCPU (_ :: Proxy i) (_ :: Proxy j)) =
    "s"+|natVal @i @Proxy Proxy|+" s"+|natVal @j @Proxy Proxy|+" XCPU"
buildInstr (XC2PU (_ :: Proxy i) (_ :: Proxy j) (_ :: Proxy k)) =
    "s"+|natVal @i @Proxy Proxy|+" s"+|natVal @j @Proxy Proxy|+" s"+|natVal @k @Proxy Proxy|+" XC2PU"

buildInstr PUSHROOT = "PUSHROOT"
buildInstr POPROOT = "POPROOT"
buildInstr ADD = "ADD"
buildInstr INC = "INC"
buildInstr EQUAL = "EQUAL"
buildInstr GEQ = "GEQ"
buildInstr LEQ = "LEQ"
buildInstr GREATER  = "GREATER"
buildInstr LESS  = "LESS"
buildInstr LSHIFT = "LSHIFT"
buildInstr RSHIFT = "RSHIFT"
buildInstr NEWC     = "NEWC"
buildInstr ENDC     = "ENDC"
buildInstr (STU x) = buildWithInt x "STU"
buildInstr STSLICE = "STSLICE"
buildInstr STREF   = "STREF"
buildInstr CTOS    = "CTOS"
buildInstr ENDS    = "ENDS"
buildInstr (LDU x) = buildWithInt x "LDU"
buildInstr (PLDU x) = buildWithInt x "PLDU"
buildInstr LDSLICEX = "LDSLICEX"
buildInstr PLDSLICEX = "PLDSLICEX"
buildInstr LDREF    = "LDREF"
buildInstr PLDREF    = "PLDREF"
buildInstr SREFS = "SREFS"
buildInstr NEWDICT = "NEWDICT"
buildInstr DICTEMPTY = "DICTEMPTY"
buildInstr LDDICT    = "LDDICT"
buildInstr PLDDICT   = "PLDDICT"
buildInstr DICTGET   = "DICTGET"
buildInstr DICTUGET = "DICTUGET"
buildInstr STDICT   = "STDICT"
buildInstr DICTREMMIN = "DICTREMMIN"
buildInstr DICTSET    = "DICTSET"
buildInstr DICTUSET   = "DICTUSET"
buildInstr DICTDEL    = "DICTDEL"
buildInstr DICTUDEL   = "DICTUDEL"
buildInstr MAYBE_TO_BOOL = "DUP"
buildInstr (IF_JUST t f)
    = "IF:<{\n" <> indentF indentation (buildInstr t)
    <> "}>ELSE<{\n" <> indentF indentation (buildInstr f)
    <> "}>\n"
buildInstr (IFELSE t f)
    = "IF:<{\n" <> indentF indentation (buildInstr t) <>
    "}>ELSE<{\n" <> indentF indentation (buildInstr f)
    <> "}>\n"
buildInstr (FMAP_MAYBE act) = "IF:<{\n" <> indentF indentation (buildInstr act) <> " TRUE }>ELSE<{ FALSE }>\n"
buildInstr JUST    = "1 INT"
buildInstr NOTHING = "0 INT"
buildInstr (WHILE inv body)
    = "WHILE:<{\n" <> indentF indentation (buildInstr inv)
    <> "}>DO<{\n" <> indentF indentation (buildInstr body)
    <> "}>\n"
buildInstr (THROWIF e)    = buildWithInt (fromEnum e) "THROWIF"
buildInstr (THROWIFNOT e) = buildWithInt (fromEnum e) "THROWIFNOT"
buildInstr HASHCU   = "HASHCU"
buildInstr HASHSU   = "HASHSU"
buildInstr SHA256U  = "SHA256U"
buildInstr CHKSIGNS = "CHKSIGNS"
buildInstr CHKSIGNU = "CHKSIGNU"
buildInstr NOW      = "NOW"
buildInstr SENDRAWMSG = "SENDRAWMSG"
buildInstr ACCEPT = "ACCEPT"
buildInstr (THROW e) = buildWithInt (fromEnum e) "THROW"
buildInstr PAIR      = "PAIR"
buildInstr UNPAIR    = "UNPAIR"
buildInstr (CALL s)  = "" +| s |+ " CALL"
buildInstr MYADDR    = "MYADDR"

buildWithInt :: (Num a, Buildable a) => a -> Text -> Builder
buildWithInt x instr = "" +| x |+ " " +| instr |+ ""
