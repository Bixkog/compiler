% Wojciech Oziebly 15.06.2016
% wersja za 22pkt
%lexer to zmodyfikowany lexer z while_parser

lexer(Tokens) -->
   white_space,
   (  (  ":=",      !, { Token = tAssgn }
      ;  ",",       !, { Token = tColon }
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
            {  member((Id, Token), 
                                [(and, tAnd),
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
                                (write, tWrite)]
                     ),
               !
            ;  Token = tVar(Id)
            }
      ),!,{ Tokens = [Token | TokList] }, lexer(TokList)
   ;  [],{ Tokens = [] }
   ).

white_space --> "(*", !, comment, white_space.
white_space -->
   [Char], { code_type(Char, space) }, !, white_space.
white_space -->
   [].

comment --> ( "*)",!;[_], comment).

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
   [A], { (code_type(A, alnum);A == 95;A == 39) }, !, alphanum(T).
alphanum([]) -->
   [].

identifier(L, Id) -->
   alphanum(As),
      { atom_codes(Id, [L|As]) }.


% parser----------------------------------------------------------------
parser(program(EV,EF,I)) --> [tProgram],[_], block(EV,EF,I).
block(EV,EF,I) --> declarations(EV,EF), [tBegin], instructions(I), [tEnd].
% deklaracje-------------------------------------------------------------
declarations(EV,[F|EF]) --> procDecl(F),!,declarations(EV,EF).
declarations(EV,EF) --> varDecl(V),!, declarations(EV2,EF),{append(V,EV2,EV)}.
declarations([],[]) --> [].
%                                           v(Id,Addr)
varDecl([V|EV]) --> [tLocal],[tVar(Id)],{V = v(Id,_)}, cvarDecl(EV).
cvarDecl([V|EV]) --> [tColon],!, [tVar(Id)],{V = v(Id,_)} , cvarDecl(EV).
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
cinstructions([I|IS]) --> [tSColon],!, instruction(I), cinstructions(IS).
cinstructions([]) --> [].
instruction(I) --> (
    [tVar(Id)],!,[tAssgn], arithmeticExpr(E), {I = assign(v(Id),expr(E))} ;
    [tIf], boolExpr(B),
            [tThen], instructions(Iif),
            [tElse],!, instructions(Ielse),[tFi],
        {I = if(B,Iif,Ielse)};
    [tIf],!, boolExpr(B),
            [tThen], instructions(Iif),[tFi],
        {I = if(B,Iif)};
    [tWhile],!, boolExpr(B),
            [tDo],instructions(Ido),[tDone],
        {I = while(B,Ido)};
    [tCall],!, fcall(F), {I = F};
    [tReturn],!, arithmeticExpr(E),{I = return(expr(E))};
    [tRead],!, [tVar(Id)], {I = read(v(Id))};
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
    [tVar(Id)],!, {S = v(Id)};
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

cond(C) --> [tNot],!, cond(C1),{ C = not(C1)}.
cond(C) --> arithmeticExpr(E1),!, relationalOp(Op), arithmeticExpr(E2), 
            {C =.. [Op,E1,E2]}.
cond(C) --> [tLParen],boolExpr(C),[tRParen].


relationalOp(Op) -->(
    [tEq],!,{Op = eq};
    [tLeq],!,{Op = leq};
    [tGeq],!,{Op = geq};
    [tLt],!,{Op = lt};
    [tGt],!,{Op = gt};
    [tNeq],!,{Op = neq}).

% makro-asembler----------------------------------------------------------
%%  store(Addr): ACC -> Addr
%%  load(Addr): Addr -> ACC
%%  concat: [list] -> list
concat([H],H):-!.
concat([H|L],R):-
    concat(L,R2),
    append(H,R2,R).


makro_compile([],_,_,[]).
makro_compile([I|IS],EV,EF,Makro):-
    makro_compile_instruction(I,EV,EF,Makro_I),
    makro_compile(IS,EV,EF,Makro_IS),
    append(Makro_I,Makro_IS,Makro).



makro_compile_instruction(assign(v(LValueId),expr(RValueId)),EV,EF,Makro):-!,
    memberchk(v(LValueId,LVAddr),EV),
    makro_compile_expression(RValueId,EV,EF,Makro_expr),
    append(Makro_expr,[store(LVAddr)],Makro).

makro_compile_instruction(if(Condition,Then,Else),EV,EF,Makro):-!,
    makro_compile_condition(Condition, EV, EF, Makro_Condition),
    makro_compile(Then, EV, EF, Makro_Then),
    makro_compile(Else, EV, EF, Makro_Else),
    concat([Makro_Condition, [branchn(Else_id)],
        Makro_Then, [jump(End_id),label(Else_id)],
        Makro_Else, [label(End_id)]],Makro).

makro_compile_instruction(if(Condition,Then),EV,EF,Makro):-!,
    makro_compile_condition(Condition, EV, EF, Makro_Condition),
    makro_compile(Then, EV, EF, Makro_Then),
    concat([Makro_Condition,[branchn(End_id)],Makro_Then,[label(End_id)]],Makro).
makro_compile_instruction(while(Condition,Do),EV,EF,Makro):-!,
    makro_compile_condition(Condition, EV, EF, Makro_Condition),
    makro_compile(Do, EV, EF, Makro_Do),
    concat([[label(Start_id)],Makro_Condition,
           [branchn(End_id)]
           ,Makro_Do,
           [jump(Start_id),label(End_id)]],Makro).



%%  write: ACC --> O
makro_compile_instruction(read(v(X)),EV,_,Makro):-!,
    memberchk(v(X,Addr),EV),
    Makro = [read(Addr)].
makro_compile_instruction(write(expr(E)),EV,EF,Makro):-
    makro_compile_expression(E,EV,EF,Makro_expr),
    append(Makro_expr,[write],Makro).



% wyliczona wartosc jest w ACC
makro_compile_expression(E,EV,EF,Makro_expr):-
    E =.. [Op, E1, E2],!,
    makro_compile_expression(E1,EV,EF,Makro_expr1),
    makro_compile_expression(E2,EV,EF,Makro_expr2),
    concat([Makro_expr1,[push],Makro_expr2, [swapd,pop, Op]],Makro_expr).
makro_compile_expression(v(X),EV,_,Makro_expr):-!,
    memberchk(v(X,Addr),EV),
    Makro_expr = [load(Addr)].
makro_compile_expression(neg(Number), _, _, Makro):-
    number(Number),!,
    NegNumber is (-1)*Number,
    Makro = [const(NegNumber)].

makro_compile_expression(neg(Expr), EV, EF, Makro):-!,
    makro_compile_expression(Expr,EV,EF,Me),
    append(Me, [swapd,const(-1),mul],Makro).
makro_compile_expression(Number, _, _, [const(Number)]).

%%  m_c_condition: Condition -> ACC = 0 if true ; ACC < 0 if false
%%  branch should not destroy ACC
makro_compile_condition(or(C1,C2), EV, EF, Makro_Condition):-!,
    makro_compile_condition(C1, EV, EF, Makro_Condition1),
    makro_compile_condition(C2, EV, EF, Makro_Condition2),
    concat([Makro_Condition1, [branchz(True_id)],
        Makro_Condition2, [label(True_id)]]
           ,Makro_Condition).
makro_compile_condition(and(C1,C2), EV, EF, Makro_Condition):-!,
    makro_compile_condition(C1, EV, EF, Makro_Condition1),
    makro_compile_condition(C2, EV, EF, Makro_Condition2),
    concat([Makro_Condition1, [branchn(False_id)],
        Makro_Condition2, [label(False_id)]]
           ,Makro_Condition).
makro_compile_condition(not(C), EV, EF, Makro_Condition):-!,
    makro_compile_condition(C, EV, EF, Makro_Condition1),
    concat([Makro_Condition1,[branchz(False_id),
        const(0),branchz(End_id),
            label(False_id),const(-1),
        label(End_id)]],Makro_Condition).
makro_compile_condition(R_Cond, EV, EF, Makro_Condition):-
    R_Cond =.. [Op,E1,E2],
    makro_compile_expression(E1,EV,EF,Makro_expr1),
    makro_compile_expression(E2,EV,EF,Makro_expr2),

    (   Op == eq,!, Swap = false,
        Jumps = [branchz(End_id),const(-1),label(End_id)],
        concat([Makro_expr1,[branchn(Lesser_id)],Makro_expr2,
            [branchn(End_id),jump(Test_id),label(Lesser_id)],
            Makro_expr2,[branchn(Test_id),const(-1),branchn(End_id),label(Test_id)]],PreTest)

    ;   Op == neq,!, Swap = false,
        Jumps = [branchz(False_id),label(True_id),const(0),branchz(End_id),label(False_id),
            const(-1),label(End_id)],
        concat([Makro_expr1,[branchn(Lesser_id)],Makro_expr2,[branchn(True_id),jump(Test_id),
                label(Lesser_id)],Makro_expr2,[branchn(Test_id),jump(True_id),
                label(Test_id)]],PreTest)

    ;   Op == geq,!, Swap = false,
        Jumps = [branchn(End_id),label(True_id),const(0),label(End_id)],
        concat([Makro_expr1,[branchn(Lesser_id)],Makro_expr2,[branchn(True_id),
            jump(Test_id),label(Lesser_id)],Makro_expr2,[branchn(Test_id),const(-1),branchn(End_id),
            label(Test_id)]],PreTest)

    ;   Op == gt,!, Swap = true,
        Jumps = [branchn(True_id),label(False_id),const(-1), branchn(End_id), 
        label(True_id),const(0),label(End_id)],
        concat([Makro_expr1,[branchn(Lesser_id)],Makro_expr2,[branchn(True_id),
            jump(Test_id),label(Lesser_id)],Makro_expr2,[branchn(Test_id),jump(False_id),
            label(Test_id)]],PreTest)

    ;   Op == lt,!, Swap = false,
        Jumps = [branchn(True_id),const(-1), branchn(End_id),
             label(True_id),const(0),label(End_id)],
        concat([Makro_expr1,[branchn(Lesser_id)],Makro_expr2,[branchn(End_id),
            jump(Test_id),label(Lesser_id)],Makro_expr2,[branchn(Test_id),jump(True_id),
            label(Test_id)]],PreTest)

    ;   Op == leq, Swap = true,
        Jumps = [branchn(End_id),label(True_id),const(0),label(End_id)],
        concat([Makro_expr1,[branchn(Lesser_id)],Makro_expr2,[branchn(End_id),
            jump(Test_id),label(Lesser_id)],Makro_expr2,[branchn(Test_id),jump(True_id),
            label(Test_id)]],PreTest)
    ),
    (   Swap == false,!,
        concat([Makro_expr1, [push],Makro_expr2,[swapd,pop,sub]],Start)
    ;   concat([Makro_expr2, [push],Makro_expr1,[swapd,pop,sub]],Start)),
    concat([PreTest,Start,Jumps],Makro_Condition).


%% push: ACC -> stack
assembly_makro_instruction(push,Makro):-!,
    Makro = [swapd,const(65535),swapa, load, swapa, swapd, store,
         const(1), swapd, swapa, sub, store].
%% pop: stack -> ACC, saves DR
assembly_makro_instruction(pop,Makro):-!,
    Makro = [const(65535),swapa,load,swapd,swapa,const(1),swapd,
         add,swapa,swapd,const(65535),swapa,store,swapa,load].

assembly_makro_instruction(read(Addr),Makro):-!,
    Makro = [const(v(Addr)),swapa,const(1),syscall,store].

assembly_makro_instruction(write,Makro):-!,
    Makro = [swapd,const(2),syscall].

assembly_makro_instruction(load(Addr),Makro):-!,
    Makro = [const(v(Addr)),swapa,load].

assembly_makro_instruction(store(Addr),Makro):-!,
    Makro = [swapa,const(v(Addr)),swapa,store].
%saves ACC, DR
assembly_makro_instruction(branchn(Label_id),Makro):-!,
    Makro = [swapa,const(Label_id),swapa,branchn].
%saves ACC, DR
assembly_makro_instruction(branchz(Label_id),Makro):-!,
    Makro = [swapa,const(Label_id),swapa,branchz].
%saves AR, DR
assembly_makro_instruction(jump(Label_id),Makro):-!,
    Makro = [const(Label_id), jump].
% ACC mod DR -> ACC
assembly_makro_instruction(mod,Makro):-
    Makro = [div,const(-16),swapd,shift].

first_assembly([],[const(0),syscall]):-!.
first_assembly([M|Makros],FASSEMBLY):-
    (assembly_makro_instruction(M,Assembly_inst),!
    ;Assembly_inst = [M]),
    first_assembly(Makros,FArest),
    append(Assembly_inst,FArest,FASSEMBLY).

pack_length([],0):-!.
pack_length([label(_)|L],N):-!,
    pack_length(L,N).
pack_length([_|L],N):-
    pack_length(L,N2),
    N is N2 + 1.

generate_NOPS(0,[]):-!.
generate_NOPS(N,[nop|NOPS]):-
    N2 is N-1,
    generate_NOPS(N2,NOPS).

fill_with_NOPS(L,R):-
    pack_length(L,N),
    NOP_C is 4-N,
    generate_NOPS(NOP_C,NOPS),
    append(L,NOPS,R).
% pack : pakuje instrukcje w bloki biorac pod uwage etykiety i skoki
pack(AIS,P,[P|L]):-
    pack_length(P,4),!,
    pack(AIS,[],L).

pack([label(Id)|AIS],[],[label(Id)|L]):-!,
    var(Id),
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

pack([],[],[]):-!.
pack([],P,[P]).

% -32768;32767 -> 0;655535
dec_to_machine_word(N,MW):-
    N>=0,!,
    MW = N.
dec_to_machine_word(N,MW):-
    MW is 65536+N.

% const(X) -> [... const ...][X]
assembly_const([],[]):-!.
assembly_const([label(Id)|L],[label(Id)|R]):-!,
    assembly_const(L,R).
assembly_const([H|L],[E|R]):-
    get_consts(H,Cs,E),
    Cs == [],!,
    assembly_const(L,R).
assembly_const([H|L],R):-
    get_consts(H,Cs,E),
    assembly_const(L,R2),
    concat([[E],Cs,R2],R).



get_consts([],[],[]):-!.
get_consts([const(C)|L],[MW|R],[const|E]):-!,
    (number(C),!,dec_to_machine_word(C,MW);MW = C),
    get_consts(L,R,E).
get_consts([H|L],R,[H|E]):-
    get_consts(L,R,E).

%unifikacja adresow etykiet
set_labels(L,R,S):-
    set_labels(L,R,0,S).
set_labels([],[],S,S):-!.
set_labels([Label|L],R,C,S):-
    nonvar(Label),
    Label = label(Id),!,
    Id = C,
    set_labels(L,R,C,S).
set_labels([B|L],[B|R],C,S):-
    C1 is C+1,
    set_labels(L,R,C1,S).

make_word(L,R):-
    make_word(L,0,R).
make_word([],R,R).
make_word([I|IS],CS,R):-
    (   I == nop,!, C = 0
    ;   I == syscall,!, C = 1
    ;   I == load,!, C = 2
    ;   I == store,!, C = 3
    ;   I == swapa,!, C = 4
    ;   I == swapd,!, C = 5
    ;   I == branchz,!, C = 6
    ;   I == branchn,!, C = 7
    ;   I == jump,!, C = 8
    ;   I == const,!, C = 9
    ;   I == add,!, C = 10
    ;   I == sub,!, C = 11
    ;   I == mul,!, C = 12
    ;   I == div,!, C = 13
    ;   I == shift,!, C = 14
    ;   I == nand,!, C = 15
    ;   C = I),
    CS2 is CS*16+C,
    make_word(IS,CS2,R).

%ustawianie adresow zmiennych
set_var(L,R,Addr,EV):-
    unify_var(EV,Addr),
    set_var(L,R).

unify_var([],_):-!.
unify_var([v(_,X)|L],Addr):-
    NA is Addr + 1,
    X is NA,
    unify_var(L,NA).

set_var([],[]):-!.
set_var([v(X)|L],[X|R]):-!,
    set_var(L,R).
set_var([H|L],[H|R]):-
    set_var(L,R).

convert_to_words([],[]):-!.
convert_to_words([H|L],[LW|R]):-
    (is_list(H),!,make_word(H,LW);LW=H),
    convert_to_words(L,R).


algol16(Source,SextiumBin):-
    phrase(lexer(Tokens),Source),

    phrase(parser(program(EV,EF,I)),Tokens),
    makro_compile(I,EV,EF,Makro),

    first_assembly(Makro, ASM),
    pack([const(65535),swapa,const(65534),store|ASM],[],PASM),
    assembly_const(PASM,PASM2),
    set_labels(PASM2,PASM3,Size),
    set_var(PASM3,PASM4,Size,EV),
    convert_to_words(PASM4,SextiumBin).

writelist([]):-!.
writelist([H|L]):-
    writeln(H),
    writelist(L).

%do wczytywania z pliku
main:-
    open("input",read,STR),
    read_stream_to_codes(STR,Input),
    algol16(Input,Bin),
    writelist(Bin).





