|                               Operator | Type                                                          | Associativity   |
| -------------------------------------: | :------------------------------------------------------------ | --------------- |
|                               type { } | Instanciate                                                   | LTR             |
|            `()` <br /> `[]` <br /> `.` | Call <br/> Subscript <br/> Get                                | LTR LTR LTR     |
|              `!` <br /> `-` <br /> `+` | Not <br/> Negate <br/> Identity                               | RTL RTL RTL     |
|             `\*` <br /> `/` <br /> `%` | Multiply <br/> Divide <br/> Modulo                            | LTR LTR LTR     |
|                         `+` <br /> `-` | Add <br/> Subtract                                            | LTR LTR         |
| `<` <br /> `<=` <br /> `>` <br /> `>=` | Less <br/> Less or equal <br/> Greater <br/> Greater or equal | LTR LTR LTR LTR |
|                       `==` <br /> `!=` | Equals <br/> Not Equal                                        | LTR LTR         |
|                                   `&&` | And                                                           | LTR             |
|                                 `\|\|` | Or                                                            | LTR             |
|                                    `=` | Assignment                                                    | LTR             |
