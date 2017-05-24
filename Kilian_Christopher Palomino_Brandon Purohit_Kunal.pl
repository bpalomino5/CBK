% Christopher Kilian
% Brandon Palomino
% Kunal Purohit
% CS 352 - Spring 2017
% Prolog Project

/*
Following grammer handles:
the color of the car is blue
the capacity of the bottle is one liter

Console:
s([the,color,of,the,car,is,blue],[]).
true.

s([the,capacity,of,the,bottle,is,one,liter],[]).
true.

Implementation:
pp: prepositional phrase in between noun phrase and verb phrase
prep: preposition statement
i.e. [of,the]
since thats the only prepositonal phrase in our example
*/

s --> np(subject), pp, vp.
np(_) --> det, n.
np(X) --> det, pro(X).
np(X) --> pro(X).
pp --> prep, n.
vp --> v, np(object).
vp --> v.
det --> [the].
det --> [one].
n --> [car].
n --> [bottle].
v --> [is].
pro(subject) --> [color].
pro(subject) --> [capacity].
pro(object) --> [blue].
pro(object) --> [liter].
prep --> [of, the].

/*
Added Parser that parses input and returns tree to first argument
ex.
?- s(T,[the,color,of,the,car,is,blue],[]).
T = s(np(det(the), pro(color)), pp(prep(of, the), n(car)), vp(v(is), np(pro(blue)))) .
*/

s(s(NP,PP,VP)) --> np(subject,NP), pp(PP), vp(VP).
np(_,np(Det,N)) --> det(Det), n(N).             
np(X,np(Det,Pro)) --> det(Det), pro(X,Pro).
np(X,np(Pro)) --> pro(X,Pro).
pp(pp(PREP,N)) --> prep(PREP), n(N).
vp(vp(V,NP)) --> v(V), np(object,NP).
vp(vp(V)) --> v(V).
det(det(the)) --> [the]. 
det(det(one)) --> [one].              
n(n(car)) --> [car].       
n(n(bottle)) --> [bottle].
v(v(is)) --> [is].
pro(subject,pro(color)) --> [color].
pro(subject,pro(capacity)) --> [capacity].
pro(object,pro(blue)) --> [blue].
pro(object,pro(liter)) --> [liter].
prep(prep(of, the)) --> [of, the].