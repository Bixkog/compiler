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
      ),!,{ Tokens = [Token | TokList] }, lexer(TokList)
   ;  [],{ Tokens = [] }
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
declarations(EV,EF) --> varDecl(V),!, declarations(EV2,EF),{append(V,EV2,EV)}.
declarations([],[]) --> [].

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

addtiveOp(add) --> [tPlus],!.
addtiveOp(sub) --> [tMinus].

multOp(mul) --> [tTimes],!.
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

makro_compile([],_,_,[]).
makro_compile([I|IS],EV,EF,Makro):-
	makro_compile_instruction(I,EV,EF,Makro_I),
	makro_compile(IS,EV,EF,Makro_IS),
	append(Makro_I,Makro_IS,Makro).



makro_compile_instruction(assign(var(LValueId),expr(RValueId)),EV,EF,Makro):-!,
	memberchk(var(LValueId,LVAddr),EV),
	makro_compile_expression(RValueId,EV,EF,Makro_expr),
	append(Makro_expr,[store(LVAddr)],Makro).

makro_compile_instruction(if(Condition,Then,Else),EV,EF,Makro):-!,
	makro_compile_condition(Condition, EV, EF, Makro_Condition),
	makro_compile(Then, EV, EF, Makro_Then),
	makro_compile(Else, EV, EF, Makro_Else),
	get_label_id(Else_id),
	get_label_id(End_id),
	concat([Makro_Condition, [branchn(Else_id)],
		Makro_Then, [jump(Else_id),label(End_id)],
		Makro_Else, [label(End_id)]],Makro).

makro_compile_instruction(if(Condition,Then),EV,EF,Makro):-!,
	makro_compile_condition(Condition, EV, EF, Makro_Condition),
	makro_compile(Then, EV, EF, Makro_Then),
	get_label_id(End_id),
	concat([Makro_Condition,[branchn(End_id)],Makro_Then,[label(End_id)]],Makro).
makro_compile_instruction(while(Condition,Do),EV,EF,Makro):-!,
	makro_compile_condition(Condition, EV, EF, Makro_Condition),
	makro_compile(Do, EV, EF, Makro_Do),
	get_label_id(Start_id),
	get_label_id(End_id),
	concat([[label(Start_id)],Makro_Condition,
	       [branchn(End_id)]
	       ,Makro_Do,
	       [jump(Start_id),label(End_id)]],Makro).
makro_compile_instruction(return(expr(E)),EV,EF,Makro):-!,
	makro_compile_expression(E,EV,EF,Makro).


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

makro_compile_expression(N, funCall(Id,Args),EV,EF,Makro_expr):-!,
	call_function(N,funCall(Id,Args),EV,EF,Makro_expr).
makro_compile_expression(N,E,EV,EF,Makro_expr):-
	E =.. [Op, E1, E2],!,
	makro_compile_expression(N,E1,EV,EF,Makro_expr1),
	N2 is N+1,
	makro_compile_expression(N2,E2,EV,EF,Makro_expr2),
	concat([Makro_expr1,[push],Makro_expr2, [swapd,pop, Op]],Makro_expr).
makro_compile_expression(N,var(X),EV,EF,Makro_expr):-!,
	memberchk(var(X,Addr),EV),
	Makro_expr = [load(Addr)].
makro_compile_expression(N, Number, EV, EF, [const(Number)]).

%%	m_c_condition: Condition -> ACC = 0 if true ; ACC < 0 if false
%%	branch should not destroy ACC
makro_compile_codition(or(C1,C2), EV, EF, Makro_Condition):-
	makro_compile_condition(C1, EV, EF, Makro_Condition1),
	makro_compile_condition(C2, EV, EF, Makro_Condition2),
	get_label_id(True_id),
	concat([Makro_Condition1, [branchz(True_id)],
		Makro_Condition2, [label(True_id)]]
	       ,Makro_Condition).
makro_compile_codition(and(C1,C2), EV, EF, Makro_Condition):-
	makro_compile_condition(C1, EV, EF, Makro_Condition1),
	makro_compile_condition(C2, EV, EF, Makro_Condition2),
	get_label_id(False_id),
	concat([Makro_Condition1, [branchn(False_id)],
		Makro_Condition2, [label(False_id)]]
	       ,Makro_Condition).
makro_compile_condition(not(C), EV, EF, Makro_Condition):-
	makro_compile_condition(C, EV, EF, Makro_Condition1),
	get_label_id(False_id),
	get_label_id(End_id),
	concat([Makro_Condition1,[branchz(False_id),
		const(0),jump(End_id),
	        label(False_id),const(-1),
		label(End_id)]],Makro_Condition).
makro_compile_condition(R_Cond, EV, EF, Makro_Condition):-
	R_Cond =.. [Op,E1,E2],
	makro_compile_expression(E1,EV,EF,Makro_expr1),
	makro_compile_expression(E2,EV,EF,Makro_expr2),

	(   Op == eq,!, Swap = false, get_label_id(End_id),
	    Jumps = [branchz(End_id),const(-1),label(End_id)]

	;   Op == neq,!, Swap = false, get_label_id(False_id),get_label_id(End_id),
	    Jumps = [branchz(False_id),const(0),jump(End_id),label(False_id),
		    const(-1),label(End_id)]

	;   Op == geq,!, Swap = false, get_label_id(End_id),
	    Jumps = [branchn(End_id),const(0),label(End_id)]

	;   Op == gt,!, Swap = true, get_label_id(True_id), get_label_id(End_id),
	    Jumps = [branchn(True_id),const(-1), jump(End_id),
		     label(True_id),const(0),label(End_id)]

	;   Op == lt,!, Swap = false, get_label_id(True_id), get_label_id(End_id),
	    Jumps = [branchn(True_id),const(-1), jump(End_id),
		     label(True_id),const(0),label(End_id)]

	;   Op == leq, Swap = true, get_label_id(End_id),
	    Jumps = [branchn(End_id),const(0),label(End_id)]),
	(   Swap == false,!,
	    concat([Makro_expr1, [push],Makro_expr2,[swapd,pop,sub]],Start)
	;   concat([Makro_expr2, [push],Makro_expr1,[swapd,pop,sub]],Start)),

	concat([Start,Jumps],Makro_Condition).


%% push: ACC -> stack
assembly_makro_instruction(push,Makro):-!,
	Makro = [swapd,const(65535),swapa, load, swapa, swapd, store,
		 const(1), swapd, swapa, sub, store].
%% pop: stack -> ACC, saves DR
assembly_makro_instruction(pop,Makro):-!,
	Makro = [const(65535),swapa,load,swapd,swapa,const(1),swapd,
		 sub,swapa,swapd,const(65535),swapa,store,swapa,load].

assembly_makro_instruction(load(Addr),Makro):-!,
	Makro = [const(Addr),swapa,load].

assembly_makro_instruction(store(Addr),Makro):-!,
	Makro = [swapa,const(Addr),swapa,store].
%saves ACC, DR
assembly_makro_instruction(branchn(Label_id),Makro):-!,
	Makro = [swapa,const(label(Label_id)),swapa,branchn].
%saves ACC, DR
assembly_makro_instruction(branchz(Label_id),Makro):-!,
	Makro = [swapa,const(label(Label_id)),swapa,branchz].
%saves ACC, DR
assembly_makro_instruction(jump(Label_id),Makro):-!,
	Makro = [swapa,const(label(Label_id)),swapa,jump].
% ACC mod DR -> ACC
assembly_makro_instruction(mod,Makro):-
	Makro = [div,const(-16),swapd,shift].

first_assembly([],[]):-!.
first_assembly([M|Makros],FASSEMBLY):-
	(assembly_makro_instruction(M,Assembly_inst)
	;Assembly_inst = [M]),
	first_assembly(Makros,FArest),
	append(Assembly_inst,FArest,FASSEMBLY).

pack_length([],0):-!.
pack_length([label(_)|L],N):-!,
	pack_length(L,N).
pack_length([_|L],N):-
	pack_length(L,N2),
	N is N2	+ 1.

generate_NOPS(0,[]):-!.
generate_NOPS(N,[nop|NOPS]):-
	N2 is N-1,
	generate_NOPS(N2,NOPS).

fill_with_NOPS(L,R):-
	pack_length(L,N),
	NOP_C is 4-N,
	generate_NOPS(NOP_C,NOPS),
	append(L,NOPS,R).

pack(AIS,P,[P|L]):-
	pack_length(P,4),!,
	pack(AIS,[],L).

pack([label(Id)|AIS],[],[label(Id)|L]):-!,
	pack(AIS,[],L).
pack([label(Id)|AIS],P,[P2,label(Id)|L]):-!,
	fill_with_NOPS(P,P2),
	pack(AIS,[],L).

pack([AI|AIS],P,[P2|L]):-
	memberchk(AI,[jump,branchn,branchz]),!,
	append(P,[AI],P1),
	fill_with_NOPS(P1,P2),
	pack(AIS,[],L).

pack([AI|AIS],P,L):-
	append(P,[AI],P2),!,
	pack(AIS,P2,L).

pack([],P,[P]).


dec_to_machine_word(N,MW):-
	N>=0,!,
	MW = N.
dec_to_machine_word(N,MW):-
	MW is 65536+N.

assembly_const([],[]).
assembly_const([label(Id)|L],[label(Id)|R]):-!,
	assembly_const(L,R).
assembly_const([H|L],[E|R]):-
	get_consts(H,Cs,E),
	Cs == [],!,
	assembly_const(L,R).
assembly_const([H|L],[E,Cs|R]):-
	get_consts(H,Cs,E),
	assembly_const(L,R).


get_consts([],[],[]).
get_consts([const(C)|L],[C|R],[const|E]):-!,
	get_consts(L,R,E).
get_consts([H|L],R,[H|E]):-
	get_consts(L,R,E).

set_labels(I,R,ML):-
	set_labels(I,R,ML,0).


