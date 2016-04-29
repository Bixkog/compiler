lexer(Tokens) -->
   white_space,
   (  (  ":=",      !, { Token = tAssgn }
      ;	 ",",       !, { Token = tColon }
      ;  ";",       !, { Token = tSColon }
      ;  "(",       !, { Token = tLParen }
      ;  ")",       !, { Token = tRParen }
      ;  "+",       !, { Token = tPlus }
      ;  "-",       !, { Token = tMinus }
      ;  "*",       !, { Token = tTimes }
      ;  "=",       !, { Token = tEq }
      ;  "<>",      !, { Token = tNeq }
      ;  "<=",      !, { Token = tLeq }
      ;  "<",       !, { Token = tLt }
      ;  ">=",      !, { Token = tGeq }
      ;  ">",       !, { Token = tGt }
      ;  digit(D),  !,
            number(D, N),
            { Token = tNumber(N) }
      ;  letter(L), !, identifier(L, Id),
            {  member((Id, Token), [ (and, tAnd),
				     (begin, tBegin),
				     (call, tCall),
                                     (div, tDiv),
                                     (do, tDo),
                                     (done, tDone),
                                     (else, tElse),
				     (end, tEnd),
                                     (fi, tFi),
                                     (if, tIf),
				     (local, tLocal),
                                     (mod, tMod),
                                     (not, tNot),
                                     (or, tOr),
				     (procedure, tProcedure),
				     (program, tProgram),
				     (read, tRead),
				     (return, tReturn),
				     (value, tValue),
                                     (then, tThen),
                                     (while, tWhile),
				     (write, tWrite)]),
               !
            ;  Token = tVar(Id)
            }
      ;  [_],
            { Token = tUnknown }
      ),
      !,
         { Tokens = [Token | TokList] },
      lexer(TokList)
   ;  [],
         { Tokens = [] }
   ).

white_space --> "(*", !, comment.
white_space -->
   [Char], { code_type(Char, space) }, !, white_space.
white_space -->
   [].

comment --> ([_], comment; "*)").

digit(D) -->
   [D],
      { code_type(D, digit) }.

digits([D|T]) -->
   digit(D),
   !,
   digits(T).
digits([]) -->
   [].

number(D, N) -->
   digits(Ds),
      { number_chars(N, [D|Ds]) }.

letter(L) -->
   [L], { (code_type(L, alpha))}.

alphanum([A|T]) -->
   [A], { (code_type(A, alnum);A == 95;A == 96) }, !, alphanum(T).
alphanum([]) -->
   [].

identifier(L, Id) -->
   alphanum(As),
      { atom_codes(Id, [L|As]) }.

/*
   SYNTAX ANALYSIS

   Context-free grammar:

      program --> instruction | instruction program
      instruction --> "while" bool_expr "do" program "done"
                    | "if" bool_expr "then" program "else" program "fi"
                    | "if" bool_expr "then" program "fi"
                    | "skip" ";"
                    | variable ":=" artith_expr ";"
      arith_expr --> arith_expr additive_op summand | summand
      summand --> summand multiplicative_op factor | factor
      factor --> "(" arith_expr ")" | constant | variable
      additive_op --> "+" | "-"
      multiplicative_op --> "*" | "div" | "mod"
      bool_expr --> bool_expr "or" disjunct | disjunct
      disjunct --> disjunct "and" conjunct | conjunct
      conjunct --> "(" bool_expr ")" | "not" conjunct | "true" | "false"
                 | arith_expr rel_op arith_expr
      rel_op --> "=" | "<>" | "<" | "<=" | ">" | ">="

   To get a complete parser it suffices to replace character terminals
   in the grammar above with lexical tens, eliminate left recursion and
   add appropriate semantic actions generating abstract syntax trees.
*/
% program----------------------------------------------------------------
parser(program(EV,EF,I)) --> [tProgram],!,[_], block(EV,EF,I).
block(EV,EF,I) --> declarations(EV,EF), [tBegin], instructions(I), [tEnd].
% deklaracje-------------------------------------------------------------
declarations(EV,[F|EF]) --> procDecl(F),!,declarations(EV,EF).
declarations(EV,EF) --> varDecl(EV),!, fDeclarations(EF).
declarations([],[]) --> [].

fDeclarations([F|EF]) --> procDecl(F),!,fDeclarations(EF).
fDeclarations([]) --> [].

varDecl([V|EV]) --> [tLocal],[tVar(Id)],{V = var(Id,Adr)}, cvarDecl(EV).
cvarDecl([V|EV]) --> [tColon], [tVar(Id)],{V = var(Id,Adr)} , cvarDecl(EV).
cvarDecl([]) --> [].

procDecl(F) --> [tProcedure], [tVar(Id)],
		[tLParen], procArgs(Args), [tRParen],
		block(FEV,FEF,FI),
		{F = func(Id,Args,FEV,FEF,FI)}.

procArgs([A|Args]) --> procArg(A),!, cprocArgs(Args).
procArgs([]) --> [].

cprocArgs([A|Args]) --> [tColon],!, procArg(A), cprocArgs(Args).
cprocArgs([]) --> [].

procArg(A) --> [tValue],!, [tVar(Id)],{A = arg(Id, value)}.
procArg(A) --> [tVar(Id)], {A = arg(Id, name)}.
% instrukcje--------------------------------------------------------------
instructions([I|IS]) --> instruction(I), [tSColon],!, instructions(IS).
instructions([I]) --> instruction(I).
instruction(I) --> (
    [tVar(Id)],!,[tAssgn], arithmeticExpr(E), {I = assign(var(Id),expr(E))} ;
    [tIf],!, boolExpr(B),
            [tThen], instructions(Iif),
            [tElse], instructions(Ielse),[tFi],
	    {I = if(B,Iif,Ielse)};
    [tIf],!, boolExpr(B),
            [tThen], instructions(Iif),
	    {I = if(B,Iif)};
    [tWhile],!, boolExpr(B),
            [tDo],instructions(Ido),[tDone],
	    {I = while(B,Ido)};
    [tCall],!, fcall(F), {I = F};
    [tReturn],!, arithmeticExpr(E),{I = return(expr(E))};
    [tRead],!, [tVar(Id)], {I = read(var(Id))};
    [tWrite],!, arithmeticExpr(E), {I = write(expr(E))}
).
% wyrazenia-arytmetyczne--------------------------------------------------
arithmeticExpr(E) --> addtiveExpr(E).
addtiveExpr(E) --> multExpr(M),raddExpr(M,E).
raddExpr(M,E) --> addtiveOp(Op), !, multExpr(M2), {N =.. [Op,M,M2]}, raddExpr(N,E).
raddExpr(E,E) --> [].

multExpr(M) --> simpleExpr(S),rmultExpr(S,M).
rmultExpr(S,M) --> multOp(Op), !, simpleExpr(S2), {N =.. [Op,S,S2]}, rmultExpr(N,M).
rmultExpr(M,M) --> [].

simpleExpr(S) --> ( patomicExpr(S), !; [tMinus], patomicExpr(Sn), {S = neg(Sn)}).
patomicExpr(S) --> ( atomicExpr(S), !; [tLParen], arithmeticExpr(S), [tRParen]).
atomicExpr(S) --> (
    fcall(F),!, {S = F};
    [tVar(Id)],!, {S = var(Id)};
    [tNum(S)]).

addtiveOp(plus) --> [tPlus],!.
addtiveOp(minus) --> [tMinus].

multOp(mult) --> [tTimes],!.
multOp(div) --> [tDiv],!.
multOp(mod) --> [tMod].
% wolanie-funkcji---------------------------------------------------------
fcall(F) --> [tVar(Id)], [tLParen], realArgs(A), [tRParen], {F = funCall(Id,A)}.
realArgs([A|Args]) --> arithmeticExpr(A),!, crealArgs(Args).
realArgs([]) --> [].

crealArgs([A|Args]) --> [tColon],!, arithmeticExpr(A),crealArgs(Args).
crealArgs([]) --> [].
% wyrazenia-logiczne-------------------------------------------------------
boolExpr(E) --> orExpr(E).
orExpr(E) --> andExpr(A),rorExpr(A,E).
rorExpr(A,E) --> [tOr], !, andExpr(A2), {N = or(A,A2)}, rorExpr(N,E).
rorExpr(E,E) --> [].

andExpr(A) --> cond(C), randExpr(C,A).
randExpr(C,A) --> [tAnd], !, cond(C2), {N = and(C,C2)}, randExpr(N,A).
randExpr(A,A) --> [].

cond(C) --> [tLParen],!,boolExpr(C),[tPParen].
cond(C) --> arithmeticExpr(E1), relationalOp(Op), arithmeticExpr(E2), {C =.. [Op,E1,E2]}.

relationalOp(Op) -->(
    [tEq],!,{Op = eq};
    [tLeq],!,{Op = geq};
    [tGeq],!,{Op = geq};
    [tLt],!,{Op = lt};
    [tGt],!,{Op = gt};
    [tNeq],!,{Op = neq}).

% TWI, Mar 15, 2009
