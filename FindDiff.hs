module FindDiff(findDiff) where
import Id
import CSyntax

newtype SEM s a = M (s -> Either () (s, a))

instance Monad (SEM s) where
    return a = M $ \ s -> Right (s, a)
    M a >>= f = M $ \ s ->
        case a s of
	Left e -> Left e
	Right (s', b) ->
	    let M f' = f b
	    in  f' s'

run :: s -> SEM s a -> Either () (s, a)
run s (M m) = m s

err :: SEM s a
err = M (\s->Left ())

handle :: SEM s a -> SEM s a -> SEM s a
handle (M a) (M b) = M $ \ s ->
    case a s of
    Left _ -> b s
    r -> r

-----

data State = S CExpr Bool CExpr

type M a = SEM State a

noMatch :: CExpr -> M CExpr
noMatch hole = M $ \ (S e b _) -> if b then Left () else Right (S e True hole, e)

must :: Bool -> M ()
must True = return ()
must False = err

class Diff a where
    diff :: a -> a -> M a

findDiff :: CExpr -> CExpr -> CExpr -> (CExpr, CExpr)
findDiff ie e1 e2 = 
	case run (S ie False ie) $ diff e1 e2 of
	Right (S _ _ hole, e) -> (e, hole)

diffE :: CExpr -> CExpr -> M CExpr
diffE (CUniv a1 e1) (CUniv a2 e2) = do
    a <- diff a1 a2
    e <- diff e1 e2
    return (CUniv a e)
diffE (Clam a1 e1) (Clam a2 e2) = do
    a <- diff a1 a2
    e <- diff e1 e2
    return (Clam a e)
diffE (CArrow b1 a1 e1) (CArrow b2 a2 e2) = do
    must (b1 == b2)
    a <- diff a1 a2
    e <- diff e1 e2
    return (CArrow b1 a e)
diffE (Clet ds1 e1) (Clet ds2 e2) = do
    ds <- diff ds1 ds2
    e <- diff e1 e2
    return (Clet ds e)
diffE (CProduct p cs1) (CProduct _ cs2) = do
    cs <- diff cs1 cs2
    return (CProduct p cs)
diffE (CRecord ps1 p ds1) (CRecord ps2 _ ds2) = do
    must (ps1 == ps2)
    ds <- diff ds1 ds2
    return (CRecord ps1 p ds)
diffE (Copen e1 a1 e1') (Copen e2 a2 e2') = do
    must (a1 == a2)
    e <- diff e1 e2
    e' <- diff e1' e2'
    return (Copen e a1 e')
diffE (CSelect e1 i1) (CSelect e2 i2) = do
    must (i1 == i2)
    e <- diff e1 e2
    return (CSelect e i1)
diffE (CSum ss1) (CSum ss2) = do
    ss <- diff ss1 ss2
    return (CSum ss)
diffE (CCon i1 t1) (CCon i2 t2) = do
    must (i1 == i2)
    t <- diff t1 t2
    return (CCon i1 t)
diffE (Ccase e1 as1) (Ccase e2 as2) = do
    e <- diff e1 e2
    as <- diff as1 as2
    return (Ccase e as)
diffE e@(CStar _ i1 j1) (CStar _ i2 j2) = do
    must (i1 == i2 && j1 == j2)
    return e
diffE e@(CVar i1) (CVar i2) = do
    must (i1 == i2)
    return e
diffE (CApply f1 es1) (CApply f2 es2) = do
    f <- diff f1 f2
    es <- diff es1 es2
    return (CApply f es)
diffE e@(CLit _ l1) (CLit _ l2) = do
    must (l1 == l2)
    return e
diffE (CBinOp e1 i1 e1') (CBinOp e2 i2 e2') = do
    must (i1 == i2)
    e <- diff e1 e2
    e' <- diff e1' e2'
    return (CBinOp e i1 e')
diffE (Cdo e1 bs1) (Cdo e2 bs2) = do
    e <- diff e1 e2
    bs <- diff bs1 bs2
    return (Cdo e bs)
diffE (CWarn m e1) (CWarn _ e2) = do
    e <- diff e1 e2
    return (CWarn m e)
diffE (CHasType e1 t1) (CHasType e2 t2) = do
    e <- diff e1 e2
    t <- diff t1 t2
    return (CHasType e t)
diffE (Cif c1 t1 e1) (Cif c2 t2 e2) = do
    c <- diff c1 c2
    t <- diff t1 t2
    e <- diff e1 e2
    return (Cif c t e)
diffE (Cnative s1 e1) (Cnative s2 e2) = do
    must (s1 == s2)
    e <- diff e1 e2
    return (Cnative s1 e)
diffE (CEqProof c1) (CEqProof c2) = do
    c <- diff c1 c2
    return (CEqProof c)
-- use hole in first arg
diffE (CHole e1) _ = 
    noMatch e1
-- ignore hole in the second arg
diffE e1 (CHole e2) =
    diffE e1 e2
diffE _ _ = err

instance Diff CExpr where
    diff e1 e2 = diffE e1 e2 `handle` noMatch e1

instance (Diff a, Diff b) => Diff (a, b) where
    diff (x1, y1) (x2, y2) = do
	x <- diff x1 x2
	y <- diff y1 y2
	return (x, y)

instance Diff Bool where
    diff b1 b2 = do
    	must (b1 == b2)
	return b1

instance Diff Id where
    diff i1 i2 = do
    	must (i1 == i2)
	return i1

instance Diff CArg where
    diff a@(CArg b1 i1) (CArg b2 i2) = do
	must (b1 == b2 && i1 == i2)
	return a
    diff (CArgT i1 t1) (CArgT i2 t2) = do
	must (i1 == i2)
	t <- diff t1 t2
	return (CArgT i1 t)
    diff _ _ = err

instance (Diff a) => Diff [a] where
    diff [] [] = return []
    diff (x1:xs1) (x2:xs2) = do
    	x <- diff x1 x2
	xs <- diff xs1 xs2
	return (x:xs)
    diff _ _ = err

instance Diff CDef where
    diff (CDef ps1 d1) (CDef ps2 d2) = do
	must (ps1 == ps2)
	d <- diff d1 d2
	return (CDef ps1 d)

instance Diff CDefn where
    diff (CValueT i1 as1 t1 e1) (CValueT i2 as2 t2 e2) = do
        must (i1 == i2)
	as <- diff as1 as2
	t <- diff t1 t2
	e <- diff e1 e2
	return (CValueT i1 as t e)
    diff (CValueS i1 t1 cs1) (CValueS i2 t2 cs2) = do
        must (i1 == i2)
	t <- diff t1 t2
	cs <- diff cs1 cs2
	return (CValueS i1 t cs)
    diff (Ctype i1 as1 t1) (Ctype i2 as2 t2) = do
        must (i1 == i2)
	as <- diff as1 as2
	t <- diff t1 t2
	return (Ctype i1 as t)
    diff (Cdata i1 as1 ss1) (Cdata i2 as2 ss2) = do
        must (i1 == i2)
	as <- diff as1 as2
	ss <- diff ss1 ss2
	return (Cdata i1 as ss)
    diff (CValue i1 e1) (CValue i2 e2) = do
        must (i1 == i2)
	e <- diff e1 e2
	return (CValue i1 e)
    diff (CNative i1 t1 s1) (CNative i2 t2 s2) = do
        must (i1 == i2 && s1 == s2)
	t <- diff t1 t2
	return (CNative i1 t s1)
    diff (CDSign i1 t1) (CDSign i2 t2) = do
        must (i1 == i2)
	t <- diff t1 t2
	return (CDSign i1 t)
    diff (Cpostulate i1 t1) (Cpostulate i2 t2) = do
        must (i1 == i2)
	t <- diff t1 t2
	return (Cpostulate i1 t)
    diff (CDopen e1 o1) (CDopen e2 o2) = do
        must (o1 == o2)
	e <- diff e1 e2
	return (CDopen e o1)
    diff _ _ = err

instance Diff CSumType where
    diff (CSId i1 t1) (CSId i2 t2) = do
	must (i1 == i2)
	t <- diff t1 t2
	return (CSId i1 t)
    diff (CSType t1) (CSType t2) = do
	t <- diff t1 t2
	return (CSType t)
    diff _ _ = err

instance Diff CClause where
    diff (CClause ps1 e1) (CClause ps2 e2) = do
    	ps <- diff ps1 ps2
	e <- diff e1 e2
	return (CClause ps e)

instance Diff CPat where
    diff p1 p2 = do
    	must (p1 == p2)
	return p1

instance Diff CSign where
    diff (CSign i1 t1) (CSign i2 t2) = do
	must (i1 == i2)
	t <- diff t1 t2
	return (CSign i1 t)
    diff (CSignDef d1) (CSignDef d2) = do
    	d <- diff d1 d2
	return (CSignDef d)
    diff (CSignType i1 as1) (CSignType i2 as2) = do
	must (i1 == i2)
	as <- diff as1 as2
	return (CSignType i1 as)
    diff _ _ = err

instance Diff CEqChain where
    diff (CEqAtom e1) (CEqAtom e2) = do
	e <- diff e1 e2
	return (CEqAtom e)
    diff (CEqDef c1 e1) (CEqDef c2 e2) = do
	c <- diff c1 c2
	e <- diff e1 e2
	return (CEqDef c e)
    diff (CEqBy c1 p1 e1) (CEqBy c2 p2 e2) = do
	c <- diff c1 c2
	p <- diff p1 p2
	e <- diff e1 e2
	return (CEqBy c p e)
    diff _ _ = err

instance Diff CBind where
    diff (CBind a1 e1) (CBind a2 e2) = do
	a <- diff a1 a2
	e <- diff e1 e2
	return (CBind a e)
    diff (CBind_ e1) (CBind_ e2) = do
	e <- diff e1 e2
	return (CBind_ e)
    diff (CBLet ds1) (CBLet ds2) = do
	ds <- diff ds1 ds2
	return (CBLet ds)
    diff _ _ = err
