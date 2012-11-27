module example$usestack = 
#include Prelude
open example$StackToQueue example$StackL use enqueue, first, empty in
putStrLn (show (first (enqueue 1 (enqueue 2 empty))))

