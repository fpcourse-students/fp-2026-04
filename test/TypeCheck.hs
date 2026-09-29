{- |
Проверка задач, где ответ — тип: не ломает сборку тестов при неверном ответе и не печатает
ожидаемый ответ.

'synonymShape' достаёт определение синонима типа через Template Haskell и рендерит его правую
часть в строку. Имена переменных в строку не попадают: параметры синонима и переменные под
@forall@ нумеруются в порядке связывания, поэтому @forall c. (a -> b -> c) -> c@ и
@forall r. (x -> y -> r) -> r@ дают одну и ту же форму. GHC при этом не проверяет, подходит ли
тип к какому-либо выражению, — сравнивается только запись.
-}
module TypeCheck
  ( synonymShape
  , assertShape
  ) where

import Control.Monad (when)
import Data.List qualified as List
import Language.Haskell.TH
import Test.HUnit (Test (..), assertBool)
import TodoException (todo)

-- | Сплайс: форма синонима типа — число параметров и правая часть.
synonymShape :: Name -> Q Exp
synonymShape name = reify name >>= \case
  TyConI (TySynD _ params rhs) ->
    stringE $ show (length params) <> " " <> render (map binderName params) rhs
  _ -> stringE "<not a type synonym>"

-- | Форма совпадает с ожидаемой; правая часть @Todo@ — задача не начата.
assertShape :: String -> String -> String -> Test
assertShape what expected actual = TestCase do
  when (drop 1 (dropWhile (/= ' ') actual) == "Todo") $ todo what
  assertBool (what <> ": тип не тот, что требуется") $ actual == expected

binderName :: TyVarBndr flag -> Name
binderName = \case
  PlainTV n _ -> n
  KindedTV n _ _ -> n

-- | Рендер дерева типа: аппликация — @(f x y)@, стрелка — @(-> a b)@, квантор —
-- @(forall v3 v4 t)@; имена типов без модулей, @[Char]@ печатается как @String@.
render :: [Name] -> Type -> String
render = go
  where
    go env = \case
      ForallT binders _ body ->
        let env' = env ++ map binderName binders
            bound = ["v" <> show i | i <- [length env + 1 .. length env']]
        in "(forall " <> unwords bound <> " " <> go env' body <> ")"
      AppT ListT (ConT c) | nameBase c == "Char" -> "String"
      t@AppT {} -> let (h, args) = spine t in
        "(" <> unwords (go env h : map (go env) args) <> ")"
      SigT t _ -> go env t
      ParensT t -> go env t
      InfixT l n r -> go env $ AppT (AppT (ConT n) l) r
      UInfixT l n r -> go env $ AppT (AppT (ConT n) l) r
      VarT n -> maybe (nameBase n) (\i -> "v" <> show (i + 1)) $ List.elemIndex n env
      ConT n -> nameBase n
      TupleT 0 -> "()"
      TupleT n -> "(" <> replicate (n - 1) ',' <> ")"
      ArrowT -> "->"
      ListT -> "[]"
      _ -> "<unsupported>"

-- | Голова аппликации и её аргументы: @f x y@ → @(f, [x, y])@.
spine :: Type -> (Type, [Type])
spine = \case
  AppT f x -> let (h, args) = spine f in (h, args ++ [x])
  t -> (t, [])
