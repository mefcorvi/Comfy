start
  = item

tuple
  = "{" _ items:items _ "}" { return { tuple: items }; }
  / "{" _ "}" { return { tuple: [] } }

list
  = "[" _ items:items _ "]" { return items; }
  / "[" _ "]" { return []; }

items
  = head:item _ "," _ tail:items { var result = [ head ]; for (var i = 0; i < tail.length; i++) result.push(tail[i]); return result; }
  / head:item _ { return [head] }

item
  = integer
  / atom
  / binaryString
  / string
  / pid
  / variable
  / list
  / tuple

pid
  = "<" id:[0-9.]+ ">" { return { pid: id.join("") }; }
  / "<" id:variable ">" { return { pid: id }; }

integer "integer"
  = digits:[0-9]+ { return parseInt(digits.join(""), 10); }

atom
  = char:[a-z] chars:[a-z0-9_]* { return { atom: char + chars.join("") }; }
  / "'" chars:[^']+ "'" { return { atom: chars.join("") }; }

binaryString
  = "<<" string:string ">>" { return { binaryString: string }; }
  / "<<" string:variable ">>" { return { binaryString: string}; }

string "string"
  = '"' '"' _             { return "";    }
  / '"' chars:chars '"' _ { return chars; }

chars
  = chars:char+ { return chars.join(""); }

char
  = [^"\\\0-\x1F\x7f]
  / '\\"'  { return '"';  }
  / "\\\\" { return "\\"; }
  / "\\/"  { return "/";  }
  / "\\b"  { return "\b"; }
  / "\\f"  { return "\f"; }
  / "\\n"  { return "\n"; }
  / "\\r"  { return "\r"; }
  / "\\t"  { return "\t"; }

variable
  = head:[A-Z_] tail:[A-Za-z0-9_]* { return { variable: head + tail.join("") }; }

_ "whitespace"
  = whitespace*

whitespace
  = [ \t\n\r]