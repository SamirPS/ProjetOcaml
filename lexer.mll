(* File lexer.mll *)
        {
        open Parser        (* The type token is defined in parser.mli *)
        exception Eof
        
        }

        let a=['a' 'A']
        let b=['b' 'B']
        let c=['c' 'C']
        let d=['d' 'D']
        let e=['e' 'E']
        let f=['f' 'F']
        let g=['g' 'G']
        let h=['h' 'H']
        let i=['i' 'I']
        let j=['j' 'J']
        let k=['k' 'K']
        let l=['l' 'L']
        let m=['m' 'M']
        let n=['n' 'N']
        let o=['o' 'O']
        let p=['p' 'P']
        let q=['q' 'Q']
        let r=['r' 'R']
        let s=['s' 'S']
        let t=['t' 'T']
        let u=['u' 'U']
        let v=['v' 'V']
        let w=['w' 'W']
        let x=['x' 'X']
        let y=['y' 'Y']
        let z=['z' 'Z']
        let spa =[' ']

       


        rule token = parse
            [' ' '\t']     { token lexbuf }     (* skip blanks *)
          | ['\n' ]        { EOL }
          | a t          {AT}
          | l a b e l          {LABEL}
          | c o l o r          {COLOR}
          | t o             {TO}
          | s i z e          {SIZE}
          | i n i t i a l          {INITIAL}
          | f i n a l         {FINAL}
          | b g c o l o r      {BGCOLOR}
          | c r e a t e spa n o d e { CREATENODE }
          |  c r e a t e spa e d g e spa f r o m { CREATEFROM }
          |  d u m p { DUMP }
          | r e m o v e spa n o d e {REMOVE}
          | r e m o v e spa e d g e spa f r o m  {REMOVEEDGE}
          | m o v e   {MOVE}
          | ['a'-'z' 'A'-'Z' '0'-'9' ]+ as lxm {ID(lxm)}
          | ['0'-'9']+ as lxm { NUM(lxm) }
          | eof            { raise Eof }