module FiftAsm.Builder
       ( AsProgram (..)
       ) where

import Fmt
import qualified Data.Map as M

import FiftAsm.Instr
import FiftAsm.DSL ((:->) (..), Subroutine (..))

newtype AsProgram inp out = AsProgram (inp :-> out)

indentation :: Int
indentation = 2

instance Buildable (AsProgram inp out) where
    build (AsProgram (I instr rs)) =
        foldMap declSubroutine (M.keys rs) <>
        M.foldMapWithKey buildSubroutine rs <>
        "\nmain PROC:<{\n" <> indentF indentation (buildInstr instr) <> "}>"

declSubroutine :: String -> Builder
declSubroutine name =
    "DECLPROC " <> build name <> "\n"

buildSubroutine :: String -> Subroutine -> Builder
buildSubroutine name (Subroutine instr) =
    build name <> " PROC:<{\n" <> indentF indentation (buildInstr instr) <> "}>\n"

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
buildInstr PUSHROOT = "PUSHROOT"
buildInstr POPROOT = "POPROOT"
buildInstr INC = "INC"
buildInstr EQUAL = "EQUAL"
buildInstr GEQ = "GEQ"
buildInstr LEQ = "LEQ"
buildInstr GREATER  = "GREATER"
buildInstr NEWC     = "NEWC"
buildInstr ENDC     = "ENDC"
buildInstr (STU x) = buildWithInt x "STU"
buildInstr STSLICE = "STSLICE"
buildInstr STREF   = "STREF"
buildInstr CTOS    = "CTOS"
buildInstr ENDS    = "ENDS"
buildInstr (LDU x) = buildWithInt x "LDU"
buildInstr LDSLICEX = "LDSLICEX"
buildInstr LDREF    = "LDREF"
buildInstr NEWDICT = "NEWDICT"
buildInstr DICTEMPTY = "DICTEMPTY"
buildInstr LDDICT    = "LDDICT"
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
buildInstr (FMAP_MAYBE act) = "IF:<{\n" <> indentF indentation (buildInstr act) <> "}>\n"
buildInstr JUST    = "1 INT"
buildInstr NOTHING = "0 INT"
buildInstr (WHILE inv body)
    = "WHILE:<{\n" <> indentF indentation (buildInstr inv)
    <> "}>DO<{\n" <> indentF indentation (buildInstr body)
    <> "}>\n"
buildInstr (THROWIF e)    = buildWithInt (fromEnum e) "THROWIF"
buildInstr (THROWIFNOT e) = buildWithInt (fromEnum e) "THROWIFNOT"
buildInstr HASHCU   = "HASHCU"
buildInstr SHA256U  = "SHA256U"
buildInstr CHKSIGNS = "CHKSIGNS"
buildInstr CHKSIGNU = "CHKSIGNU"
buildInstr NOW      = "NOW"
buildInstr SENDRAWMSG = "SENDRAWMSG"
buildInstr (THROW e) = buildWithInt (fromEnum e) "THROW"
buildInstr PAIR      = "PAIR"
buildInstr UNPAIR    = "UNPAIR"
buildInstr (CALL s)  = "" +| s |+ " CALL"

buildWithInt :: (Num a, Buildable a) => a -> Text -> Builder
buildWithInt x instr = "" +| x |+ " " +| instr |+ ""

