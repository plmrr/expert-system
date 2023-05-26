start :-
    write('Expert system - animal identification'),
    nl,
    ask_number,
    ask_features,
    test(Animal),
    write('Your animal is: '),
    write(Animal), undo.


test(zebra) :- zebra, !.
test(tiger) :- tiger, !.
test(fennec) :- fennec, !.
test(giraffe) :- giraffe, !.
test(penguin) :- penguin, !.
test(seal) :- seal, !.
test(seagull) :- seagull, !.
test(carp) :- carp, !.
test(scorpion) :- scorpion, !.
test(camel) :- camel, !.
test(spider) :- spider, !.
test(lobster) :- lobster, !.
test(fly) :- fly, !.
test(salmon) :- salmon, !.
test(sorry_do_not_know_this_animal).


penguin :-
    verify_number(1),
    verify_features([]),
    verify(has_feathers),
    not(verify(flies)).
seal :-
    verify_number(2),
    verify_features([]),
    verify(lives_near_sea).

seagull :-
    verify_number(2),
    verify_features([]),
    verify(has_feathers),
    verify(lives_near_sea),
    verify(flies).

carp :-
    verify_number(0),
    verify_features([]),
    not(verify(lives_near_sea)).

scorpion :-
    verify_number(4),
    verify_features([]),
    verify(lives_in_desert),
    verify(has_claws).

camel :-
    verify_number(2),
    verify_features([]),
    verify(lives_in_desert),
    not(verify(eats_meat)).

spider :-
    verify_number(4),
    verify_features([]),
   not(verify(has_claws)).

lobster :-
    verify_number(5),
    verify_features([]),
    verify(has_claws),
    verify(lives_near_sea).

zebra :-
    verify_number(2),
    sort(['black_stripes'], L),
    verify_features(L),
    not(verify(eats_meat)).

fennec :-
    verify_number(2),
    sort(['yellow_color', 'big_ears'], L),
    verify_features(L),
    verify(eats_meat),
    verify(lives_in_desert).

giraffe :-
    verify_number(2),
    sort(['long_neck', 'long_legs'], L),
    verify_features(L),
    not(verify(eats_meat)).

tiger :-
    verify_number(2),
    sort(['black_stripes', 'yellow_color'], L),
    verify_features(L),
    verify(eats_meat).

fly :-
    verify_number(3),
    verify_features([]),
    verify(flies).

salmon :-
    verify_number(0),
    verify_features([]),
    verify(lives_near_sea).


ask(Question) :-
    write('Does your animal: '),
    write(Question), write('? (yes./no.) '),
    read(Response), nl,
    ( (Response == yes)
    ->
    assert(yes(Question)) ;
    assert(no(Question)), fail).

ask_number :-
    write('How many pairs of legs or appendages does your animal have? (number in the range 0-5) '),
    read(Response),
    nl,
    assert(num(Response)).

ask_features :-
    write("Select features from the list: ['black_stripes', 'long_neck', 'yellow_color', 'big_ears', 'long_legs']"),
    nl,
    write("Enter features in the format - ['feature1', 'feature2', ...]."),
    nl,
    write("If your animal does not have any of the above features, enter an empty list - []."),
    nl,
    read(Response),
    sort(Response, Sorted),
    nl,
    assert(features(Sorted)).


:- dynamic yes/1,no/1,num/1,features/1.


verify(S) :- (yes(S) -> true ; (no(S) -> fail ; ask(S))).

verify_number(N) :- (num(N) -> true ; fail).

verify_features(F) :- (features(F) -> true ; fail ).

undo :- retract(yes(_)),fail.
undo :- retract(no(_)),fail.
undo :- retract(num(_)),fail.
undo :- retract(features(_)),fail.
undo.
