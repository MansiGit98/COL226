# COL226 Assignment: Abstract Syntax Trees (AST) for WHILE

## Context Free Grammer
### G = <N, T, P, S>

#### Non terminals N = { P, BLK, DS, VL, VR, TY, CS, CL, Cmd, EXP, IntEXP, IntF, AriOP, BoolEXP, BoF, ReLOP }

#### Terminals T : All terminals written in Italic format

#### Start Symbol S is P here.

#### Production Rules :

- P -> _program identifier ::_ BLK
- BLK -> DS CS
- DS -> _var_ VL _:_ TY _;_ DS | e
- VL -> _identifier_ VR
- VR -> _,_ VL | e
- TY -> _int_ | _bool_
- CS -> _{_  CL  _}_
- CL -> Cmd _;_ CL | e
- Cmd -> _identifier :=_ EXP | _read identifier_ | _write_ IntEXP |
 _if_ BoolEXP _then_ CS _else_ CS _endif_ | _while_ BoolEXP _do_ CS _endwh_
- EXP -> IntEXP | BoolEXP
- IntEXP -> IntF AriOP IntF | IntF
- IntF -> _numeral_ | _identifier_ | _(_ IntEXP _)_ | _~_ IntF
- AriOP -> _PLUS_ | _MINUS_ | _TIMES_ | _DIV_ | _MOD_
- BoolEXP -> BoF RelOP BoF | BoF
- BoF -> _TT_ | _FF_ | _identifier_ | _numeral_ | _(_ BoolEXP _)_ | 
_!_ BoF | ~ _identifier_ | _~_ _numeral_
- RelOP -> _AND_ | _OR_ | _LT_ | _LEQ_ | _GT_ | _GEQ_ | _EQ_ | _NEQ_

## Datatype of AST

datatype AST = Void | Leaf of Token | St of (Token * AST) | Bt of (Token * AST * AST) | Tt of (Token * AST * AST * AST)

Where Token is constructor with names -

PROG, BLK, DEC, INT, BOOL, CMD, TT, FF, NOT, AND, OR, LT, LEQ, EQ, GT, GEQ, NEQ, NEG, SET, IDENTIFIER m, NUMERAL n, READ, WRITE, ITE, WH , PLUS, MINUS, TIMES, DIV, MOD

## Syntax-directed translation

- P.val := Bt(PROG, identifier.val, BLK.val)
- BLK.val := Bt(BLK, DS.val, CS.val)
- DS.val := Tt(DEC, VL.val, TY.val, DS.val)
- VL.val := St(identifier.val, VR.val)
- VR.val := VL.val
- TY.val := Leaf(INT)
- TY.val := Leaf(BOOL)
- CS.val := CL.val
- CL.val := Bt(CMD, Cmd.val, CL.val)
- Cmd.val := Bt(SET, identifier.val,EXP.val)
- Cmd.val := St(READ, identifier.val)
- Cmd.val := St(WRITE, IntEXP.val)
- Cmd.val := Tt(ITE, BooLEXP.val, CS.val, CS.val)
- Cmd.val := Bt(WH, BooLEXP.val, CS.val)
- EXP.val := IntEXP.val
- EXP.val := BoolEXP.val
- IntEXP.val := Bt(AriOP.val, IntF.val, IntF.val)
- IntEXP.val := IntF.val
- IntF.val := numeral.val
- IntF.val := identifier.val
- IntF.val := IntEXP.val
- IntF.val := St(NEG, IntF.val)
- BoolEXP.val := Bt(RelOP.val, BoF.val, BoF.val)
- BoolEXP.val := BoF.val
- BoF.val := Leaf(TT)
- BoF.val := Leaf(FF)
- BoF.val := identifier.val
- BoF.val := numeral.val
- BoF.val := BoolEXP.val
- BoF.val := St(NOT, BoF.val)
- RelOP.val := Leaf(LT)
- RelOP.val := Leaf(LEQ)
- RelOP.val := Leaf(GT)
- RelOP.val := Leaf(GEQ)
- RelOP.val := Leaf(EQ)
- RelOP.val := Leaf(NEQ)
- RelOP.val := Leaf(AND)
- RelOP.val := Leaf(OR)
- AriOP.val := Leaf(PLUS)
- AriOP.val := Leaf(MINUS)
- AriOP.val := Leaf(TIMES)
- AriOP.val := Leaf(DIV)
- AriOP.val := Leaf(MOD)
- identifier.val := Leaf(INDENTIFIER m)
- numeral.val := Leaf(NUMERAL n)

## Other Design Decision

For Lexical analysis and Parsing of WHILE Language Program I used my SML-NJ code. For converting the program into the stream of tokens 
I used makeToken() function written in Tokenize.sml file. It read the WHILE Language program written in program.txt file and 
tokeise it and then store the list of tokens in a variable defined as list. This list is then provided to parser for 
parsing and generating Abstract Syntax Tree.

For parsing I used SML-NJ code written in Parsing.sml file. I writted the sml program such that it use recursive decent parsing method for generating Abstract Syntax Tree. I first created a context free grammmar of WHILE Language that is recursive decent parsable that is it is not left recursive and LL(1) parsable. I first calculate first() and follow() of the non terminals and implement the program according to it.

## Other Implementation Decision

I choose to write my own sml program for lexical analysis and parsing instead of using ML-Lex and ML-yacc because I thought by implementing this methods by myself provide me more deeper view of compiler design.

## Acknowledgements

I used SML-NJ Documentation for understanding sml syntax. And for Lexical analysis and Parsing technique I use the Lecture slides.

### Thanking You
### Mansi
### 2020CS10357


