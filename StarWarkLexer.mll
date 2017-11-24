{

open Lexing
open ParseCommon
open StarWarkParser

}

let num =
  "-"?['0'-'9']+

let lbl =
  ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

let comment =
  "#"[^'\n']*

let white =
  [' ' '\t']+

let line =
  '\r' | '\n' | "\r\n"

rule token = parse
  | "false" { NUM 0 }
  | "true"  { NUM 1 }

  | "@"  { DEREF }
  | "~"  { LNOT  }
  | "!"  { NOT   }
  | "&"  { LAND  }
  | "|"  { LOR   }
  | "^"  { LXOR  }
  | "<<" { LSL   }
  | ">>" { LSR   }
  | "+"  { ADD   }
  | "-"  { SUB   }
  | "*"  { MUL   }
  | "/"  { DIV   }
  | "%"  { MOD   }
  | "="  { EQ    }
  | "!=" { NEQ   }
  | "<"  { LT    }
  | "<=" { LE    }
  | ">"  { GT    }
  | ">=" { GE    }
  | "&&" { CONJ  }
  | "||" { DISJ  }
  | "?"  { HUH   }
  | ":"  { COLON }

  | "DATA"  { DATA  }
  | "<-"    { LARR  }
  | "if"    { IF    }
  | "goto"  { GOTO  }
  | "spawn" { SPAWN }

  | lbl as x { LBL x }
  | num as x { NUM (int_of_string x) }

  (* misc *)
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | eof  { EOF    }
  | line { next_line lexbuf; NL }

  (* ignore *)
  | comment { token lexbuf }
  | white   { token lexbuf }

  (* error *)
  | _ as c
    { raise (ParseCommon.Error
        (Printf.sprintf "Unexpected char: %c" c)) }
