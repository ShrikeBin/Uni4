module Bonus where
import Control.Applicative
import Data.Functor.Identity


{-
Zadanie 98 — Na wykładzie wprowadziliśmy funktor V 3(X) = X × X × X i pokazaliśmy jak nadać
temu funktorowi strukturę aplikatywną. Interpertujmy V 3(Double) jako standardową trójwymiarową
przestrzeń wektorową R3 nad liczbami rzeczywistymi. Oprogramuj mnożenie wektorów przez skalar, do-
dawanie wektorów, iloczyn skalarny wektorów, normę (długość wektora), odległość między wektorami
i.t.d.
-}

newtype V3 a = V3 (a, a, a) deriving (Eq, Show)

instance Functor V3 where
    fmap :: (a -> b) -> V3 a -> V3 b
    fmap f (V3 (x, y, z)) = V3 (f x, f y, f z)

instance Applicative V3 where
    pure :: a -> V3 a
    pure x = V3 (x, x, x)

    (<*>) :: V3 (a -> b) -> V3 a -> V3 b
    V3 (f1, f2, f3) <*> V3 (x1, x2, x3) = V3 (f1 x1, f2 x2, f3 x3)

add :: Num a => V3 a -> V3 a -> V3 a
add (V3 (x1, y1, z1)) (V3 (x2, y2, z2)) = V3 (x1 + x2, y1 + y2, z1 + z2)

scale :: Num a => a -> V3 a -> V3 a
scale k (V3 (x, y, z)) = V3 (k * x, k * y, k * z)

norm :: Floating a => V3 a -> a
norm (V3 (x, y, z)) = sqrt (x^2 + y^2 + z^2)

distance :: Floating a => V3 a -> V3 a -> a
distance v1 v2 = norm (add v1 (scale (-1) v2))

cross :: Num a => V3 a -> V3 a -> V3 a
cross (V3 (x1, y1, z1)) (V3 (x2, y2, z2)) =
    V3 (y1 * z2 - z1 * y2,
        z1 * x2 - x1 * z2,
        x1 * y2 - y1 * x2)

dot :: Num a => V3 a -> V3 a -> a
dot (V3 (x1, y1, z1)) (V3 (x2, y2, z2)) =
    x1 * x2 + y1 * y2 + z1 * z2


{-
Zadanie 99 — Na wykładzie omawialiśmy rekurencyjną drzewiastą strukturę danych modelującą dzia-
łania arytmetyczne +, ·, / oraz funkcję sqrt. Rozbuduj wprowadzony funktor o kilka innych funkcji
elementarnych, takich jak sin, cos, log operację unarną − oraz napisz odpowiedni ewaluator.
-}   

data Term a = LL a
            | VAR String
            | SQRT (Term a)
            | SIN (Term a)
            | COS (Term a)
            | LOG (Term a)
            | NEG (Term a)              -- unary minus
            | PLUS (Term a) (Term a)
            | MULT (Term a) (Term a)
            | DIV (Term a) (Term a)
            deriving (Eq, Show, Functor)
mbSqrt:: Maybe Double -> Maybe Double
mbSqrt (Just x) = if x<0 then Nothing else pure (sqrt x)
mbSqrt Nothing  = Nothing

mbDiv :: Maybe Double -> Maybe Double -> Maybe Double
mbDiv mbx mby = if mby == Just 0 then Nothing
                else liftA2 (/) mbx mby

mbSin :: Maybe Double -> Maybe Double
mbSin (Just x) = Just (sin x)
mbSin Nothing  = Nothing

mbCos :: Maybe Double -> Maybe Double
mbCos (Just x) = Just (cos x)
mbCos Nothing  = Nothing

mbLog :: Maybe Double -> Maybe Double
mbLog (Just x) = if x <= 0 then Nothing else Just (log x)
mbLog Nothing  = Nothing

mbNeg :: Maybe Double -> Maybe Double
mbNeg (Just x) = Just (-x)
mbNeg Nothing  = Nothing

evalTerm :: (Applicative f) =>
            Term (f (Maybe Double)) ->
            (String -> f (Maybe Double)) ->
            f (Maybe Double)
evalTerm (LL x) env        = x
evalTerm (VAR v) env       = env v
evalTerm (SQRT t) env      = fmap mbSqrt (evalTerm t env)
evalTerm (SIN t) env       = fmap mbSin (evalTerm t env)
evalTerm (COS t) env       = fmap mbCos (evalTerm t env)
evalTerm (LOG t) env       = fmap mbLog (evalTerm t env)
evalTerm (NEG t) env       = fmap mbNeg (evalTerm t env)
evalTerm (PLUS t1 t2) env  = liftA2 (liftA2 (+)) (evalTerm t1 env) (evalTerm t2 env)
evalTerm (MULT t1 t2) env  = liftA2 (liftA2 (*)) (evalTerm t1 env) (evalTerm t2 env)
evalTerm (DIV t1 t2) env   = liftA2 mbDiv (evalTerm t1 env) (evalTerm t2 env)

-- Obsługa list zmiennych
newtype ListaVar f  = LV { getListVar :: [(String, f (Maybe Double))] }

getVar ::(Applicative f) => 
       ListaVar f -> String -> f (Maybe Double)
getVar (LV []) _           = pure Nothing
getVar (LV ((x,v):ls)) var = if x==var then v
                             else getVar (LV ls) var

runTerm :: (Applicative f) => 
        Term (f (Maybe Double)) -> ListaVar f -> f (Maybe Double)
runTerm term subst = evalTerm term (getVar subst)

term01 :: Term (Identity (Maybe Double))
term01 = PLUS 
           (NEG (LOG (PLUS (COS (VAR "x")) (SIN (VAR "y"))))) 
           (LOG (PLUS (COS (VAR "y")) (SIN (VAR "x"))))
--  Przykładowy termin:
--  log(cos(y)+sin(x)) − log(cos(x)+sin(y))

subst :: ListaVar Identity
subst = LV [ ("x", Identity (Just 0.5))
          , ("y", Identity (Just 1.0)) ]


{-
Zadanie 108 — Załóżmy, że operator >>= spełnia własności monady. Operację >=> definiujemy wzorem:
( f >=> g ) x = ( f x ) >== g
Pokaż, że operator >=> spełnia prawa monady sformułowane w jezyku >=> .

====================================================================================================================================
Rozwiązanie:

zakładamy, że operator >>= spełnia prawa monady, czyli:
(A1) (mx >>= return) = mx
(A2) ((return x) >>= f) = f x
(A3) ((mx >>= f) >>= g) = (mx >>= (\x -> (f x) >>= g))
(A4) mf <*> mx = mf >>= (\f -> mx >>= (\x -> return (f x)))


określmy operator >=> jako:
( f >=> g ) x = ( f x ) >>= g

Pokażmy, że operator >=> spełnia prawa monady wyrażone w języku >=>:
(A1) (f >=> return) = f
(A2) (return >=> f) = f
(A3) ((f >=> g) >=> h) = (f >=> (g >=> h))

(A1) (f >=> return) x
= (f x) >>= return    -- ponieważ >>= spełnia prawa monady
= f x

(A2) (return >=> f) x
= (return x) >>= f    -- ponieważ >>= spełnia prawa monady
= f x

(A3) ((f >=> g) >=> h) x
= ((f >=> g) x) >>= h
= ((f x) >>= g) >>= h  -- prawo łączności >>=
= (f x) >>= (((\y -> g) y) >>= h)
= f x >>= (g >=> h)
= (f >=> (g >=> h)) x

CKD.
-}