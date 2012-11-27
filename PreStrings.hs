module PreStrings where
import Data.List(mapAccumL)
import FString

(preStrTable1, [
  fsEmpty, fsUnderscore,
  fsStringMod,    fsString, fsIntMod,  fsInt, fsCharMod, fsChar, fsBind, fsBind_, fsReturn,
  fsStar, fsComma, fsRArrow, fsBRArrow, fsNil, fsCons, fsIntegerMod, fsInteger,
  fsAdd, fsErrorMod, fsUndefined, fsUndefinedT, fsUndefinedTT, fsTrue, fsFalse,
  fsDoubleMod, fsDouble, fsDEF
  ]
 ) = mapAccumL hmkFString emptyStrTable 
 ["",      "_",
  "System$String","String","System$Int","Int", "System$Char", "Char", ">>=", ">>", "return",
  "*",    ",",     "->",     "|->",     "Nil", ":", "System$Integer", "Integer",
  "+", "System$Error", "undefined", "Undefined", "UndefinedT", "True", "False",
  "System$Double","Double","DEF"
 ]

(preStrTable, [
  fsIdMod, fsSubst, fsTrans, fsRefl, fsEquiv
  ]
 ) = mapAccumL hmkFString preStrTable1
 ["System$Id", "subst", "trans", "refl", "==="
 ]
