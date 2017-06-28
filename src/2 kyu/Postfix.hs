-- A Simple Postfix Language
-- http://www.codewars.com/kata/55a4de202949dca9bd000088/

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Postfix where

data Add = Add
data Push = Push
data End = End

class Postfix stack result
    where run :: stack -> result

instance Postfix (st, Int) r => Postfix ((st, Int), Int) (Add -> r)
    where run ((st, n), m) _ =  run (st, n + m)

instance (a ~ Int , Postfix (st, Int) r) => Postfix st (Push -> a -> r)
    where run st _ n = run (st, n)

instance (a ~ Int, st ~ (v, Int)) => Postfix st (End -> a)
    where run (_, n) _ = n

begin = run ()
push  = Push
add   = Add
end   = End
