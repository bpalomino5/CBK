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