module System$MonadUtil = 

open System$HO use id in

struct

concrete
Monad :: #1.0
Monad = sig
   type m _
   o :: System$Monad m     -- operations

join :: (M :: Monad) -> (a :: #) |-> M.m (M.m a) -> M.m a
join M |a f = do M.o
      (m :: M.m a) <- f
      m

-- XXX if M is hidden:
-- ../cayenne: Error: Internal compiler error: typeOf local not found: M#10004
