module MiscId where
import Position(noPosition)
import Id
import ISyntax
import PreStrings

charType = ISelect (IGVar charModId) charId
stringType = ISelect (IGVar stringModId) stringId
intType = ISelect (IGVar intModId) intId
doubleType = ISelect (IGVar doubleModId) doubleId
integerType = ISelect (IGVar integerModId) integerId

undefinedValue = ISelect (IGVar errorModId) undefinedId
undefinedType = ISelect (IGVar errorModId) undefinedTId
undefinedTypeType = ISelect (IGVar errorModId) undefinedTTId

charId = mkId noPosition fsChar
charModId = mkId noPosition fsCharMod
stringId = mkId noPosition fsString
stringModId = mkId noPosition fsStringMod
intId = mkId noPosition fsInt
intModId = mkId noPosition fsIntMod
integerId = mkId noPosition fsInteger
integerModId = mkId noPosition fsIntegerMod
errorModId = mkId noPosition fsErrorMod
doubleId = mkId noPosition fsDouble
doubleModId = mkId noPosition fsDoubleMod

nilId = mkId noPosition fsNil
consId = mkId noPosition fsCons

undefinedId = mkId noPosition fsUndefined
undefinedTId = mkId noPosition fsUndefinedT
undefinedTTId = mkId noPosition fsUndefinedTT

trueId = mkId noPosition fsTrue
falseId = mkId noPosition fsFalse
