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
parser(program(EV,EF,I)) --> [tProgram],[_], block(EV,EF,I).
block(EV,EF,I) --> declarations(EV,EF), [tBegin], instructions(I), [tEnd].
% deklaracje-------------------------------------------------------------
declarations(EV,[F|EF]) --> procDecl(F),!,declarations(EV,EF).
declarations(EV,EF) --> varDecl(EV),!, fDeclarations(EF).
declarations([],[]) --> [].

fDeclarations([F|EF]) --> procDecl(F),!,fDeclarations(EF).
fDeclarations([]) --> [].

varDecl([V|EV]) --> [tLocal],[tVar(Id)],{V = var(Id,Adr)}, cvarDecl(EV).
cvarDecl([V|EV]) --> [tColon],!, [tVar(Id)],{V = var(Id,Adr)} , cvarDecl(EV).
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
instructions([I|IS]) --> instruction(I), cinstructions(IS).
cinstructions([I|IS]) --> [tSColon], instruction(I), cinstructions(IS).
cinstructions([]) --> [].
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
    [tWrite], arithmeticExpr(E), {I = write(expr(E))}
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
    [tNumber(S)]).

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

% makro-asembler----------------------------------------------------------
%%	store(Addr): ACC -> Addr
%%	load(Addr): Addr -> ACC
%%	concat: [list] -> list
concat([H],H):-!.
concat([H|L],R):-
	concat(L,R2),
	append(H,R2,R).

%%	labels counter
%
:-nb_setval(counter,1).
get_label_id(X):-
	b_getval(counter,X),
	N is X + 1,
	b_setval(counter,N).


makro_compile_instruction(assign(var(LValueId),expr(RValueId)),EV,EF,Makro):-!,
	memberchk(var(LValueId,LVAddr),EV),
	makro_compile_expression(RValueId,EV,EF,Makro_expr),
	append(Makro_expr,[store(LVAddr)],Makro).

%%	m_c_condition: Condition -> ACC = 0 if true ; ACC < 0 if false
makro_compile_instruction(if(Condition,Then,Else),EV,EF,Makro):-!,
	makro_compile_condition(Condition, EV, EF, Makro_Condition),
	makro_compile(Then, EV, EF, Makro_Then),
	makro_compile(Else, EV, EF, Makro_Else),
	get_label_id(Else_id),
	get_label_id(End_id),
	concat([Makro_Condition, [branchn(label(Else_id))],
		Makro_Then, [jump(label(Else_id)),label(End_id)],
		Makro_Else, [label(End_id)]],Makro).

makro_compile_instruction(if(Condition,Then),EV,EF,Makro):-!,
	makro_compile_condition(Condition, EV, EF, Makro_Condition),
	makro_compile(Then, EV, EF, Makro_Then),
	get_label_id(End_id),
	concat([Makro_Condition,[branchn(label(End_id))],Makro_Then,[label(End_id)]],Makro).
makro_compile_instruction(while(Condition,Do),EV,EF,Makro):-!,
	makro_compile_condition(Condition, EV, EF, Makro_Condition),
	makro_compile_condition(Do, EV, EF, Makro_Do),
	get_label_id(Start_id),
	get_label_id(End_id),
	concat([[label(Start_id)],Makro_Condition,
	       [branchn(label(End_id))]
	       ,Makro_Do,
	       [jump(label(Start_id)),label(End_id)]],Makro).
%makro_compile_instruction(funCall(Id,Args),EV,EF,Makro):-
%makro_compile_instruction(return(expr(E)),EV,EF,Makro):-
%%	write: ACC --> O
makro_compile_instruction(read(var(X)),EV,EF,Makro):-!,
	memberchk(var(X,Addr),EV),
	Makro = [read(Addr)].
makro_compile_instruction(write(expr(E)),EV,EF,Makro):-
	makro_compile_expression(E,EV,EF,Makro_expr),
	append(Makro_expr,[write],Makro).


makro_compile_expression(E,EV,EF,Makro_expr):-
	makro_compile_expression(0,E,EV,EF,Makro_expr).

makro_compile_expression(N,E,EV,EF,Makro_expr):-
	E =.. [Op, E1, E2],!,
	makro_compile_expression(N,E1,EV,EF,Makro_expr1),
	N2 is N+1,
	makro_compile_expression(N2,E2,EV,EF,Makro_expr2),
	concat([Makro_expr1,[push],Makro_expr2, [swapd,pop, Op]],Makro_expr).
makro_compile_expression(N,var(X),EV,EF,Makro_expr):-!,
	memberchk(var(X,Addr),EV),
	Makro_expr = [load(Addr)].
makro_compile_expression(N, funCall(Id,Args),EV,EF,Makro_expr):-!,
	call_function(N,funCall(Id,Args),EV,EF,Makro_expr).
makro_compile_expression(N, Number, EV, EF, [const(Number)]).







