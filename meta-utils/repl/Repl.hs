-- | Область видимости для @make repl@: GHCi открывает этот модуль, а вместе
-- с ним всё, что импортировано сюда, то есть все уровни домашки сразу.
-- Редактировать не нужно; одноимённые функции разных уровней различайте
-- квалификацией: @Level1.f@, @Level2.f@.
--
-- В этой домашке @fst@, @snd@, @either@ и @pred@ — ваши функции из задач 1.1, 2.1 и 2.2;
-- стандартные доступны под полными именами: @Prelude.fst@.
module Repl () where

import Prelude hiding (fst, snd, either, pred)
import Examples
import Level1
import Level2
import Level3
