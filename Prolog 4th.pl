offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 4), bus).

offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 4), hotel).


customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).

customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel, 100).



customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding, 50).




perm([],[]).
perm([H|T],L):-
	perm(T,R),
	insert(H,R,L).
insert(X,L,[X|L]).
insert(X,[H|T],[H|T1]):-
	insert(X,T,T1).

possibleSubset([H|T],R):-
	helper4([H|T],[],R).
helper4([H|T],L,R):-
	append([H],L,X1),
	helper4(T,X1,R)
	;
	append([],L,X2),
	helper4(T,X2,R).
helper4([],L,R):-
	perm(L,R).







containsActivityshuffle([],[]).
containsActivityshuffle([H|T],[D|T]):-
			(H=activity(L),possibleSubset(L,M),D=activity(M)).

containsActivityshuffle([H|T],[H|R2]):-
			H\=activity(_),containsActivityshuffle(T,R2).


choosePreferences([H|T],ChosenPreferences):-
	containsActivityshuffle([H|T],M),
	possibleSubset(M,ChosenPreferences).





preferenceSatisfaction(O,C,P,S):-
	helper(O,C,P,0,S).


helper(offer(D,A,Cost,_,_,_,_,_),C,P,Acc,S):-
	customerPreferredActivity(C,Activity,X),
	member(activity(Y),P),
	member(Activity,Y),
        NAcc is Acc+X,
	helper1(offer(D,A,Cost,_,_,_,_,_),C,P,NAcc,S).
helper(offer(D,A,Cost,_,_,_,_,_),C,P,Acc,S):-
	\+member(activity(_),P),

       helper1(offer(D,A,Cost,_,_,_,_,_),C,P,Acc,S).



helper1(offer(D,_,Cost,_,_,_,_,_),C,P,Acc,S):-
	%member(dest(D),P),
	%member(budget(B),P),Cost<B,
	%%O=offer(D,_,C,_,_,_,_,_),
	customerPreferredAccommodation(C,Accomodation,X),
	offerAccommodation(offer(D,_,Cost,_,_,_,_,_),Accomodation),
	member(accommodation(Accomodation),P),
	NAcc is Acc+X,
	helper2(offer(D,_,Cost,_,_,_,_,_),C,P,NAcc,S).

helper1(offer(D,A,Cost,_,_,_,_,_),C,P,Acc,S):-
	(\+ member(accommodation(_),P))
	,
	%(customerPreferredAccommodation(C,Accomodation,_),
	%member(accomodation(X),P),X\=Accomodation),

       helper2(offer(D,A,Cost,_,_,_,_,_),C,P,Acc,S).
helper2(offer(_,_,_,_,_,_,_,_),C,P,Acc,S):-
	%member(dest(D),P),
	%member(budget(B),P),Cost<B,
	customerPreferredMean(C,Mean,X),
	offerMean(offer(_,_,_,_,_,_,_,_),Mean),
	member(means(Mean),P),
	NAcc is Acc+X,
	helper3(_,_,_,NAcc,S).
helper2(_,_,P,Acc,S):-
      (\+member(means(_),P))
       ,
       %(customerPreferredMean(C,Mean,_),
	%member(means(X),P),X\=Mean),

	helper3(_,_,_,Acc,S).

helper3(_,_,_,S,S).




%% new One
overlapPeriod(period(X-Y-Z,M-N-O),period(A-B-C,Q-W-E)):-
	((X<A , A<M ) ;(A=M ,B<N); (A=M , B=N , C<O))
	;
	(X=A,B>Y); (X=A,B=Y,C>Z) .

overlapPeriod(period(X-Y-Z,M-N-O),period(A-B-C,Q-W-E)):-
	(Q<M , Q>X) ; (Q=M , W<N); (Q=M , W<N , E<O)
	;
	(Q=X , W>Y); (Q=X , W=Y  , E>Z).








getOffer([dest(D),period(F1,F2),activity(H), budget(BU)],O):-
	offerMean(offer(D,P,X,A1,A2,period(L1,L2),A3,A4),_),
	subset(H,P),X=<BU,overlapPeriod(period(L1,L2),period(F1,F2)),
	O=offer(D,P,X,A1,A2,period(L1,L2),A3, A4).

getOffer([dest(D),period(F1,F2),activity(H), budget(BU),accommodation(AC)],O):-
	offerAccommodation(offer(D,P , X,A1,A2,period(L1,L2),A3,A4),AC),
	subset(H,P),X=<BU,overlapPeriod(period(L1,L2),period(F1,F2)),
	O=offer(D,P , X,A1,A2,period(L1,L2),A3,A4).





recommendOfferForCustomer(Prefs,C,O):-
       choosePreferences(Prefs,C),
       getOffer(C,O).




recommendOffer(Originals,Prefs,O,C):-
    setof(offer(A,B,C0,D,E,F,H5,I),offerMean(offer(A,B,C0,D,E,F,H5,I),_),[H|T]),
    sorting(H,Originals,Prefs,[],Sorted),
    recommendHelper(Originals,Sorted,[],[H|T],0,O,C,Prefs).
recommendHelper(_,_,C,[H],_,H,C,_).
%recommendHelper(_,_,C,[H,_|_],Acc,H,C,_):-
	%H=offer(_,_,_,_,_,_,_,N),
	%% Acc=N.
recommendHelper(Originals,[],C,[H,Hf|T],_,H,C,Prefs).
recommendHelper(Originals,[],C,[H,Hf|T],_,H,C,Prefs):-
	sorting(Hf,Originals,Prefs,[],Sorted),
        recommendHelper(Originals,Sorted,[],[Hf|T],0,_,[],Prefs).
recommendHelper(Originals,[H0|T0],Customers,[H|T],Acc,O,C,Prefs):-
	H=offer(_,_,_,_,_,_,_,N),
	Acc<N,
	append([H0],Customers,Nc),
	NAcc is Acc +1,
	recommendHelper(Originals,T0,Nc,[H|T],NAcc,O,C,Prefs).
recommendHelper(Originals,[],C,[_,Hf|T],_,O,C,Prefs):-
	sorting(Hf,Originals,Prefs,[],Sorted),
        recommendHelper(Originals,Sorted,[],[Hf|T],0,O,[],Prefs).
recommendHelper(Originals,_,C,[H,Hf|T],Acc,O,C,Prefs):-
	H=offer(_,_,_,_,_,_,_,N),
	Acc=N,
	sorting(Hf,Originals,Prefs,[],Sorted),
        recommendHelper(Originals,Sorted,[],[Hf|T],0,O,[],Prefs).
max(O,[FC|RC],[FP|RP],_,_,S0,C0,P0):-
	 preferenceSatisfaction(O,FC,FP,S1),
	 S1>S0,
	 max(O,RC,RP,FC,FP,S1,C0,P0).
max(O,[FC|RC],[FP|RP],C,P,S0,C0,P0):-
	 preferenceSatisfaction(O,FC,FP,S1),
	 S0>S1,
	 max(O,RC,RP,C,P,S0,C0,P0).
max(_,[],[],C,P,_,C,P).

sorting(O,[FC|RC],[FP|RP],Acc,Sx):-
	preferenceSatisfaction(O,FC,FP,S),
	S\=0,
	max(O,RC,RP,FC,FP,S,C,P),
	delete([FC|RC],C,L),
	delete([FP|RP],P,L1),
	Acc\=[],
	append(Acc,[C],NAcc),
	sorting(O,L,L1,NAcc,Sx).
sorting(O,[FC|RC],[FP|RP],Acc,Sx):-
	preferenceSatisfaction(O,FC,FP,S),
	S\=0,
	max(O,RC,RP,FC,FP,S,C,P),
	delete([FC|RC],C,L),
	delete([FP|RP],P,L1),
	Acc=[],
	append([C],Acc,NAcc),
	sorting(O,L,L1,NAcc,Sx).
sorting(_,[],[],Acc,Acc).


%%%%Evaluation:-

getAllActivities(L):-

setof(X,Y^Z^customerPreferredActivity(Y,X,Z),L).

mostPreferredActivity(C,A):-
	getAllActivities(L),
	mostHelper(C,A,L).
mostHelper(C,A,[H0,H1|T]):-
	customerPreferredActivity(C,H0,X0),
       customerPreferredActivity(C,H1,X1),
       X0>X1,
       delete([H0,H1|T],H1,NL),
       mostHelper(C,A,NL).
mostHelper(C,A,[H0,H1|T]):-
	customerPreferredActivity(C,H0,X0),
       customerPreferredActivity(C,H1,X1),
       X1>X0,
       delete([H0,H1|T],H0,NL),
       mostHelper(C,A,NL).
mostHelper(C,A,[H0,H1|T]):-
	customerPreferredActivity(C,H0,X0),
       customerPreferredActivity(C,H1,X1),
       X0=X1,
       delete([H0,H1|T],H1,NL),
       mostHelper(C,A,NL).
mostHelper(_,A,[A]).




















