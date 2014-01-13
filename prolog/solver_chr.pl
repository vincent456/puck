
%% :- module(solver,[graph/0]).
:- use_module(library(chr)).

%% :- ensure_loaded('constraints.pl').
:- chr_option(check_guard_bindings, on).
:- chr_option(debug, on).

%% node(FullName,Kind,LocalName,ArgumentTypes).

:- chr_type 
    node_t(X) ---> node(X), folded(node_t(X)).%, moved(node_t(X)).
%% Move = fold puis merge si personne n'utilise le noeud original ??

:- chr_constraint 
       declare/1,
   hideFrom/2,
   uses/2,
   contains/2,
   'contains*'/2,
   graph/0.

isFriendOf(node(a), node(b)).

graph <=> 
declare(node(root)),
declare(node(sa)), 
declare(node(a)), 
declare(node(sb)), 
declare(node(b)),
declare(node(c)),

contains(node(root), node(sa)),
contains(node(root), node(sb)),
contains(node(root), node(c)),
contains(node(sa), node(a)),
contains(node(sb), node(b)),

uses(node(a), node(b)),
uses(node(c), node(b)),

% constraints
hideFrom(node(sb), node(sa)),
hideFrom(node(b), node(c)).

%% :- chr_constraint leq/2.
%% reflexivity  @ leq(X,X) <=> true.
%% antisymmetry @ leq(X,Y), leq(Y,X) <=> X = Y.
%% idempotence  @ leq(X,Y) \ leq(X,Y) <=> true.
%% transitivity @ leq(X,Y), leq(Y,Z) ==> leq(X,Z).

declare(X) ==> 'contains*'(X,X).
contains(X,Z) ==> 'contains*'(X,Z).
'contains*'(X,Y), 'contains*'(Y,Z) ==> X \==Y , Y\==Z | 'contains*'(X,Z).



%% %%% if a folded node has already been introduced, redirect uses toward it
hideFrom(SB,SA), declare(folded(B)), 
'contains*'(SB,B), 'contains*'(SA,A) \ uses(A,B) <=> \+ isFriendOf(A,B) | 
			 uses(A,folded(B)).
%la présence d'une contrainte chr dans la garde la rends vrai ... on ne peut utiliser que des prédicats built-in dans celles ci
%

%% intro -> trouver une meilleur façon de choisir le "lieu" de l'intro
hideFrom(SB,SA), 'contains*'(SB,B), 'contains*'(SA,A), contains(C, SB) \ uses(A,B) <=>  \+ isFriendOf(A,B) |
			       declare(folded(B)), contains(C, folded(B)), uses(A,folded(B)), uses(folded(B),B).
							 
