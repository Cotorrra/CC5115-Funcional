-- Chequeo de Tautologías

-- Una fórmula de logica propocicional
data Formula = Const Bool
            | Var Char
            | Not Formula
            | And Formula Formula
            | Imply Formula Formula

-- A ==> (A && B)
ff1 :: Formula
ff1 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

-- (A && (A ==> B)) ==> B
ff2 :: Formula
ff2 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- Associative table that maps keys
-- of type k to values of type v
type Assoc k v = [(k,v)]
find :: Eq k => k -> Assoc k v -> v
find key table = head [v' | (k',v')<-table, k'==key]

-- Variable valuation
type Valuation = Assoc Char Bool

eval :: Formula -> Valuation -> Bool
eval (Const b) _ = b
eval (Var x) s = find x s
eval (Not f) s = not (eval f s)
eval (And f1 f2) s = (eval f1 s) && (eval f2 s)
eval (Imply f1 f2) s = (eval f1 s) <= eval f2 s

