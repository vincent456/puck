:-module(typing, [subtype/3]).

% :-op(800,xfx, <:).
% :-op(800,xfx, =:).
% :-op(750, yfx, =>).
% :-op(750, yfx, *).

% T<:T.

% (T1=>T2)<:(S1=>S2):-(T2<:S2), (S1<:T1).

% %T1*T2<:S1*S2:- T1<:S1 , T2<: S2.
% %% alternately an n-uplet can be represented by a list
% []<:[].
% [T1|T2]<:[S1|S2]:- T1<:S1, T2<:S2.



subtype(_, T, T).

subtype(G, Id1, Id2):- 'isa*'(Id1, Id2, G).

subtype(G, Id1, Id2):-
    struct_type(Id1, T1, G), struct_type(Id2, T2, G), subtype(T1, T2, G).
    
%% /!\ need to distinguish covariant and contravariant position for self_type !!!!

%% an tuple is represented by a list
subtype(G, tuple(T1), tuple(T2)):- maplist(call(subtype(G)), T1, T2).

subtype(G, arrow(T1,T2) , arrow(S1,S2)):-
    subtype(G, T2, S2), subtype(G, S1, T1).

%object types are structured with assoc
subtype(G, object(T1), object(T2)):- map_assoc(call(has_sub_method(G, T1)), T2).
    
has_sub_method(G, T1, (MName, MType)):- 
    get_assoc(MName, T1, (MName, MSType)),
    subtype(G, MSType, MType).
