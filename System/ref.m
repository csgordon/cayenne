module -- Ref
import Pref    : *a -> Ref *a;
import Passign : Ref *a -> *a -> Unit;
import Pderef  : Ref *a -> *a;
import Peqref  : Ref *a -> Ref *a -> _Bool;

export _Pref, _Passign, _Pderef, _Peqref;

    _Pref _x = Pref _x
and _Passign _x _y = Passign _x _y
and _Pderef _x = Pderef _x
and _Peqref _x _y = if Peqref _x _y then 1 else 0
end

