Control.lazysml := true;
open Lazy;
datatype 'x StreamCell = Nil | Cons of 'x * 'x Stream
withtype 'x Stream = 'x StreamCell Lazy.susp;

