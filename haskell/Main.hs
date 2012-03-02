module Main where

import Language.Keen.Syntax
import Language.Keen.Parser

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad.State
import Debug.Trace (trace, traceShow)
import Data.List (intercalate)

lambda x e = Value (Closure Map.empty x e)

evaluate environment expression = case expression of
    Value v -> v
    Variable x -> environment Map.! x
    Annotate e _ -> evaluate environment e
    Apply e1 e2 ->
        let Closure environment' x e = evaluate environment e1 in
        let v = evaluate environment e2 in
        let environment'' = Map.insert x v (environment' `Map.union` environment) in
        evaluate environment'' e
    Let [] e' -> evaluate environment e'
    Let ((x, e):bindings) e' ->
        let v = evaluate environment e in
        evaluate (Map.insert x v environment) (Let bindings e')
    
main = return ()

-- For increasing polymorphism, split lets into sepearte lets:
-- 1) Start with subsequences consisting of single bindings.
-- 2) As long as any subsequence contains a reference to a later 
--    subsequence, merge these two subsequences and anything in between.
-- Note that this preserves the binding order, as required by the semantics.

data TypeState = TypeState {
    next :: Int,
    variables :: Map Name Type,
    errors :: [String]
    } deriving Show
    
data Forall = Forall [Name] Type
    deriving Show

resolveVariable :: Name -> State TypeState Type
resolveVariable x = do
    state <- get
    case Map.lookup x (variables state) of
        Just t -> resolveType t
        Nothing -> return (FlexibleType x)

resolveType :: Type -> State TypeState Type
resolveType t = case t of
    FlexibleType x -> resolveVariable x
    RigidType x -> return (RigidType x)
    NumberType -> return NumberType
    StringType -> return StringType
    UnitType -> return UnitType
    FunctionType t1 t2 -> do
        t1' <- resolveType t1
        t2' <- resolveType t2
        return (FunctionType t1' t2')

occurs :: Name -> Type -> Bool
occurs x t = case t of
    FlexibleType x' -> x == x'
    RigidType x' -> x == x'
    NumberType -> False
    StringType -> False
    UnitType -> False
    FunctionType t1 t2 -> occurs x t1 || occurs x t2

unifyVariable :: Name -> Type -> State TypeState ()
unifyVariable x t = do
    x' <- resolveVariable x
    t' <- resolveType t
    when (x' /= t') $ case x' of
        FlexibleType x'' -> do
            if occurs x'' t'
                then report ("Infinite type in main at line 5, column 10.")
                else modify (\state -> state { variables = Map.insert x'' t' (variables state) })
        _ -> unify x' t'

unify :: Type -> Type -> State TypeState ()
unify t1 t2 = case (t1, t2) of
    (FlexibleType x, t) -> unifyVariable x t
    (t, FlexibleType x) -> unifyVariable x t
    (RigidType x, RigidType x') | x == x' -> return ()
    (NumberType, NumberType) -> return ()
    (StringType, StringType) -> return ()
    (UnitType, UnitType) -> return ()
    (FunctionType t1 t2, FunctionType t1' t2') -> do
        unify t1 t1'
        unify t2 t2'
    (t, t') -> report ("Incompatible types in main at line 5, column 10.\n  Expected:  " ++ show t ++ "\n     Found:  " ++ show t')

report :: String -> State TypeState ()
report problem = do
    modify (\state -> state { errors = problem : errors state })

freshVariable :: State TypeState Type
freshVariable = do
    state <- get
    put (state { next = next state + 1 })
    return (FlexibleType ("_" ++ show (next state)))

check :: Map Name Forall -> Expression -> State TypeState Type
check environment expression = case expression of
    Value v -> case v of
        Unit -> return UnitType
        Number _ -> return NumberType
        String _ -> return StringType
        Closure environment' x e -> do
            if not (Map.null environment')
                then error "Unexpected closure in main at line 5, column 10."
                else do
                    t' <- freshVariable
                    t'' <- check (Map.insert x (Forall [] t') environment) e
                    return (FunctionType t' t'')
    Variable x -> instantiate (environment Map.! x)
    Annotate e t -> do
        t' <- check environment e
        unify t' t
        return t
    Apply e1 e2 -> do
        e1' <- check environment e1
        e2' <- check environment e2
        e' <- freshVariable
        unify e1' (FunctionType e2' e')
        return e'
    Let bindings e -> do
        ts <- replicateM (length bindings) freshVariable
        let environment' = Map.fromList (zip (map fst bindings) (map (Forall []) ts))
        ts' <- mapM (check (Map.union environment' environment)) (map snd bindings)
        mapM_ (uncurry unify) (zip ts ts')
        poly <- mapM (generalize environment) ts
        let environment'' = Map.fromList (zip (map fst bindings) poly)
        check (Map.union environment'' environment) e

generalize :: Map Name Forall -> Type -> State TypeState Forall
generalize environment t = do
    t' <- resolveType t
    mono <- freeEnvironment environment
    let poly = freeType t'
    return (Forall (Set.toList (poly Set.\\ mono)) t')

instantiate :: Forall -> State TypeState Type
instantiate (Forall [] t) = return t
instantiate (Forall (x:xs) t) = do
    t' <- freshVariable
    instantiate (Forall xs (replace x t' t))

replace :: Name -> Type -> Type -> Type
replace x t' t = case t of
    FlexibleType x' | x == x' -> t'
    RigidType x' | x == x' -> t'
    FunctionType t1 t2 -> FunctionType (replace x t' t1) (replace x t' t2)
    _ -> t

freeEnvironment :: Map Name Forall -> State TypeState (Set Name)
freeEnvironment environment = do
    xs <- mapM free' (Map.elems environment)
    return (Set.unions xs)
    where
        free' (Forall xs t) = do
            t' <- resolveType t
            return (freeType t' Set.\\ Set.fromList xs)

freeType :: Type -> Set Name
freeType t = case t of
    FlexibleType x -> Set.singleton x
    RigidType x -> Set.singleton x
    FunctionType t1 t2 -> freeType t1 `Set.union` freeType t2
    _ -> Set.empty

typeCheck :: Expression -> Either String Forall
typeCheck expression = 
    let (t, s) = flip runState (TypeState { next = 1, variables = Map.empty, errors = [] }) $ do
        t <- check Map.empty expression
        generalize Map.empty t in
    case errors s of
        [] -> Right t
        es -> Left (intercalate "\n" $ reverse es)

typeCheckIO :: Expression -> IO ()
typeCheckIO expression = do
    case typeCheck expression of
        Right t -> print t
        Left s -> putStrLn s

