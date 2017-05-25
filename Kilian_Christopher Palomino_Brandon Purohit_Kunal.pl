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

Additional implementation (for questions):
noun phrase (interrogative), verb phrase

Adding pronoun "what" - classified as an interrogative, along with "who", "why", "where", "when" (not adding these yet)
useful classifications can be seen here: https://www.ucl.ac.uk/internet-grammar/nouns/pronoun.htm 
And phrase structures can be seen here: https://faculty.washington.edu/wassink/LING200/lect14_syntax2.pdf 
Other useful definitions:
A prepositional phrase is a group of words containing a preposition, a noun or pronoun object of the preposition, and any modifiers of the object.

English sentence structure rules (note values in parenthesis are optional):
s --> np, vp
np --> (det) (adj) n (pp)
vp --> v (np) (pp) (adv)
pp --> p (np)

Ex: The color of the car is blue.
s(np(det(the), n(color), pp(p(of), np(det(the), n(car)))), vp(v(is), np(n(blue))))

Todo: How to handle a sentence like "what color is the car?" --> doesn't follow standard rules I've seen so far
since it allows a noun directly following a pronoun. In our language, perhaps allow for a verb phrase that starts with a noun phrase?
*/


s --> np(subject), vp.
s --> np(interrogative), vp.
np(_) --> n.
np(_) --> det, n.
np(_) --> det, n, pp.
np(X) --> det, pro(X).
np(X) --> pro(X).
pp --> prep, np(subject).  % note that "subject" is a placeholder here for now
vp --> v, np(object).
vp --> v.
det --> [the].
det --> [one].
n --> [car].
n --> [bottle].
n --> [color].
n --> [capacity].
n --> [blue].
n --> [liter].
v --> [is].
pro(subject) --> [he].
pro(subject) --> [she].
pro(object) --> [him].
pro(object) --> [her].
pro(interrogative) --> [what].
prep --> [of].

/*
Edited parser to match updated DCG
ex 1: ?- s(T, [the, color, of, the, car, is, blue], []).
T = s(np(det(the), n(color), pp(prep(of), np(det(the), n(car)))), vp(v(is), np(n(blue)))) 

ex 2: ?- s(T, [what, is, the, color, of, the, car], []).
T = s(np(pro(what)), vp(v(is), np(det(the), n(color), pp(prep(of), np(det(the), n(car))))))
*/

s(s(NP,VP)) --> np(subject,NP), vp(VP).
s(s(NP,VP)) --> np(interrogative,NP), vp(VP).
np(_,np(N)) --> n(N).      
np(_,np(Det,N)) --> det(Det), n(N).
np(_,np(Det,N,PP)) --> det(Det), n(N), pp(PP).                  
np(X,np(Det,Pro)) --> det(Det), pro(X,Pro).
np(X,np(Pro)) --> pro(X,Pro).
pp(pp(PREP,NP)) --> prep(PREP), np(subject,NP). %note that "subject" here is placeholder --> need to figure out proper np type for prep phrases
vp(vp(V,NP)) --> v(V), np(object,NP).
vp(vp(V)) --> v(V).
det(det(the)) --> [the]. 
det(det(one)) --> [one].              
n(n(car)) --> [car].       
n(n(bottle)) --> [bottle].
n(n(color)) --> [color].
n(n(capacity)) --> [capacity].
n(n(blue)) --> [blue].
n(n(liter)) --> [liter].
v(v(is)) --> [is].
pro(subject,pro(he)) --> [he].
pro(subject,pro(she)) --> [she].
pro(object,pro(him)) --> [him].
pro(object,pro(her)) --> [her].
pro(interrogative,pro(what)) --> [what].
prep(prep(of)) --> [of].


/*
First attempt at "execute" function
Idea being that execute will first run the passed sentence through the parser, and then will send the parsed sentence
to another function which will in turn read the parsed sentence and pull out the relevant data.
*/
execute([], R) :- R = 'You submitted a blank sentence!'.
execute(List, Response) :- s(T, List, []), readParsed(T, Response).
execute(_, R) :- R = 'I can\'t understand this sentence.'.

/*
The first argument of the sentence is the noun phrase. If the first argument to the noun phrase is the pronoun "what", it's a question and should be handled accordingly.
If the sentence is not a question, it can be handled as a statement instead.
*/
readParsed(ParsedSentence, Response) :- arg(1, ParsedSentence, X), arg(1, X, pro(what)), processQuestion(ParsedSentence, Response).
readParsed(ParsedSentence, Response) :- processStatement(ParsedSentence, Response).

/*
Handle the dissection of questions
*/
processQuestion(ParsedSentence, Response) :- Response = 'That was a question!'.   % fill in with more question processing


/*
Handle the dissection of statements
*/
processStatement(ParsedSentence, Response) :- Response = 'That was a statement.'.    % fill in with more statement processing 















