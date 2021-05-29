signature QUEUE =
sig
  type 'x Queue
  exception EMPTY
  val empty : 'x Queue
  val isEmpty : 'x Queue -> bool
  val snoc : 'x Queue * 'x -> 'x Queue
  val head : 'x Queue -> 'x
  val tail : 'x Queue -> 'x Queue
end

datatype 'x Queue = Queue of { F: 'x list, R: 'x list}

val empty = Nil
fun isEmpty(Queue{F=[], ...}) = true
  | isEmpty(Queue{...}) = false
fun queue{F=[], R=rs} = Queue{F=rev(rs), R=[]}
  | queue{F=fs, R=rs} = Queue{F=fs, R=rs}
fun snoc(Queue{F=fs, R=rs}, r') = queue{F=fs, R=(r'::rs)}
fun head(Queue{F=(f::fs), ...}) = f
  | head(Queue{...}) = raise Fail "WTF?"
fun tail(Queue{F=(f::fs), R=rs}) = queue{F=fs, R=rs}

(* head(queue{F=[1], R=[2]}) *)
(* isEmpty(queue{F=[1], R=[2]}) *)
(* snoc(queue{F=[1], R=[2]}, 3) *)
(* tail(tail(queue{F=[1], R=[2]})) *)
(* http://www.cs.cmu.edu/~rwh/isml/book.pdf *)
(* https://learnxinyminutes.com/docs/standard-ml/ *)
