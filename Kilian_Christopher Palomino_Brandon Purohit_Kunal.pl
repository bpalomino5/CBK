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


:- dynamic fact/3. % declaration that there is a dynamic predicate named "fact" with an arity of 3



/*
Edited parser to match updated DCG
ex 1: ?- s(T, [the, color, of, the, car, is, blue], []).
T = s(np(det(the), n(color), pp(prep(of), np(det(the), n(car)))), vp(v(is), np(n(blue)))) 

ex 2: ?- s(T, [what, is, the, color, of, the, car], []).
T = s(np(pro(what)), vp(v(is), np(det(the), n(color), pp(prep(of), np(det(the), n(car))))))
*/

% DCG/Parse Tree found here:

s(s(NP, VP)) --> np(subject,NP); np(interrogative,NP), vp(VP).
np(_,np(N)) --> n(N).      
np(_,np(Det, N)) --> det(Det), n(N).
np(_,np(Adj, N)) --> adj(Adj), n(N).
np(_,np(Det, Adj, N)) --> det(Det), adj(Adj), n(N).
np(_,np(Adj, N, PP)) --> adj(Adj), n(N), pp(PP).
np(_,np(Det, Adj, N, PP)) --> det(Det), adj(Adj), n(N), pp(PP).
np(_,np(Det, N, PP)) --> det(Det), n(N), pp(PP).           
np(X,np(Det, Pro)) --> det(Det), pro(X, Pro).
np(X,np(Pro)) --> pro(X,Pro).       
pp(pp(PREP, NP)) --> prep(PREP), np(subject, NP). %note that "subject" here is placeholder --> need to figure out proper np type for prep phrases
vp(vp(V, NP)) --> v(V), np(object, NP).
vp(vp(V)) --> v(V).
det(det(Word)) --> [Word], {lex(Word, det)}.
n(n(Word)) --> [Word], {lex(Word, n)}.
v(v(Word)) --> [Word], {lex(Word, v)}.
adj(adj(Word)) --> [Word], {lex(Word, adj)}.
pro(subject, pro(he)) --> [he].
pro(subject, pro(she)) --> [she].
pro(object, pro(him)) --> [him].
pro(object, pro(her)) --> [her].
pro(interrogative, pro(Word)) --> [Word], {lex(Word, pro(interrogative))}.
prep(prep(of)) --> [of].


% Lexicon defitions found here:

lex(the, X) :- !, X = det.
lex(one, X) :- !, X = det.
lex(a, X) :- !, X = det.
lex(an, X) :- !, X = det.
lex(is, X) :- !, X = v.
lex(he, X) :- !, X = pro(subject).
lex(she, X) :- !, X = pro(subject).
lex(him, X) :- !, X = pro(object).
lex(her, X) :- !, X = pro(object).
lex(what, X) :- !, X = pro(interrogative).
lex(_, n).
lex(_, adj).


/*
First attempt at "execute" function
Idea being that execute will first run the passed sentence through the parser, and then will send the parsed sentence
to another function which will in turn read the parsed sentence and pull out the relevant data.
*/
execute([], R) :- !, R = "You submitted a blank sentence!".
execute(List, Response) :- s(T, List, []), readParsed(T, Response), !.
execute(_, R) :- R = "I can\'t understand this sentence".

/*
The first argument of the sentence is the noun phrase. If the first argument to the noun phrase is the pronoun "what", it's a question and should be handled accordingly.
If the sentence is not a question, it can be handled as a statement instead.
*/
readParsed(ParsedSentence, Response) :- arg(1, ParsedSentence, X), arg(1, X, pro(what)), !, processQuestion(ParsedSentence, Response).
readParsed(ParsedSentence, Response) :- processStatement(ParsedSentence, Response).

/*
Handle the dissection of questions
*/
processQuestion(ParsedSentence, Response) :- arg(2, ParsedSentence, VP), arg(2, VP, NP), arg(2, NP, n(Attribute)), arg(3, NP, PP), arg(2, PP, NNP), 
                                                                       arg(2, NNP, n(Object)), checkDBQuestion(Attribute,Object,Response).
																	   

checkDBQuestion(Attribute,Object,Response) :- fact(Attribute,Object,Value), !, string_concat("It\'s ", Value, Response).
checkDBQuestion(_,_,Response) :- Response = "I don\'t know".   % printMessage(Attribute,Object,'unknown', Response). MODIFIED

% printMessage predicate
% Added to help clean up printing to user
% Concatenates Attribute, Object, and Value to a single line string, and unifies that string to Response
% Used in checkDBQuestion and checkDB
printMessage(Attribute, Object, Value, Response) :- term_string(Attribute, Att), term_string(Object, Obj), string_concat("The ",Att, P1), string_concat(P1, " of the ", P2), string_concat(P2, Obj, P3), string_concat(P3, " is ", P4), string_concat(P4, Value, Response).

%ASK PROFESSOR - Is this sentence return val OK, or does it need to exactly match the assignment outline?

/*
Handle the dissection of statements
Sample sentence breakdown handled here:
First example here looks at arg 1 of the parsed sentence which is np(det(the), n(color), pp(prep(of), np(det(the), n(car)))), unifying with "NP"
also gets the second argument for the parse sentence, which is vp(v(is), np(n(blue))), unifying this with VP
then it looks at arg 2 of NP which is n(color), and "color" is unified with "Attribute"
then looks at arg 3 of the noun phrase which is pp(prep(of), np(det(the), n(car))), unifying with PP
then looks at arg 2 of PP which is np(det(the), n(car)), unifying with NNP (new noun phrase)
then looks at arg 2 of NNP which is n(car), unifying "car" with "Object"
then looks at arg 2 of VP which is np(n(blue)) and unifies with NNNP (new new noun phrase)
finally looking at arg 1 of NNNP which is n(blue), unifying blue with "Value"
*/
processStatement(ParsedSentence, Response) :- arg(1, ParsedSentence, NP), arg(2, ParsedSentence, VP),
                                                                       arg(2, NP, n(Attribute)), arg(3, NP, PP), arg(2, PP, NNP), 
                                                                       arg(2, NNP, n(Object)), arg(2, VP, NNNP), getValue(NNNP, Value), checkDB(Attribute, Object, Value, Response).

% getValue predicate
% Handles getting single or multi-word properties from the term NP, which is the term inside VP of the parsed tree.
% Concatenates the property values together and unifies to term Value.
% Used in processStatement
getValue(NP, Value) :- functor(NP,_,A), A=:=1, !, arg(1, NP, n(N)), term_string(N, Value). %modified to change value to a string in advance
getValue(NP, Value) :- functor(NP,_,A), A=:=2, arg(1, NP, det(D)), !, arg(2, NP, n(N)), term_string(D, V1), term_string(N, V2), string_concat(V1, " ", P1), string_concat(P1, V2, Value).
getValue(NP, Value) :- functor(NP,_,A), A=:=2, arg(1, NP, adj(Adj)), !, arg(2, NP, n(N)), term_string(Adj, V1), term_string(N, V2), string_concat(V1, " ", P1), string_concat(P1, V2, Value).

checkDB(Attribute, Object, Value, Response) :- fact(Attribute, Object, Value), !, Response = "I know".
checkDB(Attribute, Object, _, Response) :- fact(Attribute, Object, X), !, printMessage(Attribute, Object, X, Response).
checkDB(Attribute, Object, Value, Response) :- assert(fact(Attribute, Object, Value)), !, Response = "OK".



/*
05\26\17 update:
Hey guys, I talked to the professor after class yesterday and got a good explanation of how we should handle the checking\adding of info to our DB. We can't use a variable
as the functor because Prolog won't allow that, but we CAN create a predefined functor to store all of our data in.
In other words, we can't use a statement like: Attribute(Object, Value)
but we can say: fact(Attribute, Object, Value)

Another option is to use the "functor" function defined in the slides to build these items on the fly. The professor said that wasn't necessary for this project,
but after we've got this thing working I might go ahead and try that simply because it might be more flexible and might earn us some extra points!

I've currently added our first data entry/check. If you run:
?- execute([the, color, of, the, car, is, blue], X).
it will add the fact to the DB and return "OK."
Can test by first running: fact(color, car, blue). which will return "false" at first. Then run the above execute statement, and then the fact statement a second time.
The second time it will return "true".
*/









