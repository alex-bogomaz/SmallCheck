namespace SmallCheck

type Serial<'a> =    
    abstract member series : int -> seq<'a>
    abstract member coseries : (int -> seq<'b>) -> int -> seq<'a -> 'b>

