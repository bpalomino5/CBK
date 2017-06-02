/*
Christopher Kilian
Brandon Palomino
Kunal Purohit

CS 352 - Spring 2017
Prolog Project

This prolog program is designed to parse simple statements of fact about the properties of objects, and simple questions of those facts.
It can handle statements in the form: "the <attribute> of the <object> is <value>" (for example, "the length of the rod is 10 inches")
It can handle questions in two forms, either: "what is the <attribute> of the <object>" (for example, "what is the capacity of the bottle")
or the form: "what <attribute> is the <object>" (for example, "what color is the car")

The words used for <attribute>, <object>, and <value> are open ended since this program uses an open lexicon with regards to nouns and adjectives, allowing any values for these
three attributes.
*/

% Allow for the dynamic creation of "fact"'s in the database at runtime - "fact" is designed to hold statement information in the form: fact(Attribute, Object, Value)
:- dynamic fact/3. 


/*
DCG\Parse Tree found here:
This DCG will create a parsed tree of the sentences provided to the "execute" predicate at runtime.
It is designed to handle sentences of the types as described in the comment at the top of this program.
Note that "values" can be either single or double word in nature - for example, the value can be something like "blue", "green", "smooth", etc or it can have the form "20 inches", "dark orange", etc.
The DCG uses a lexicon (found below the DCG) which allows for an open lexicon with regards to either nouns or adjectives, allowing sentences of the above formats to 
be accepted whether the noun\adjective in question has been seen before or not. 
*/
s(s(NP, VP)) --> np(subject,NP); np(interrogative,NP), vp(VP).
np(_, np(N)) --> n(N).      
np(_, np(Det, N)) --> det(Det), n(N).
np(_, np(Adj, N)) --> adj(Adj), n(N).
np(_, np(Det, Adj, N)) --> det(Det), adj(Adj), n(N).
np(_, np(Adj, N, PP)) --> adj(Adj), n(N), pp(PP).
np(_, np(Det, Adj, N, PP)) --> det(Det), adj(Adj), n(N), pp(PP).
np(_, np(Det, N, PP)) --> det(Det), n(N), pp(PP).           
np(X, np(Det, Pro)) --> det(Det), pro(X, Pro).
np(X, np(Pro)) --> pro(X,Pro).       
np(X, np(Pro, N)) --> pro(X, Pro), n(N).       
pp(pp(Prep, NP)) --> prep(Prep), np(_, NP).
vp(vp(V, NP)) --> v(V), np(object, NP).
vp(vp(V)) --> v(V).
det(det(Word)) --> [Word], {lex(Word, det)}.
n(n(Word)) --> [Word], {lex(Word, n)}.
v(v(Word)) --> [Word], {lex(Word, v)}.
adj(adj(Word)) --> [Word], {lex(Word, adj)}.
pro(interrogative, pro(Word)) --> [Word], {lex(Word, pro(interrogative))}.
prep(prep(of)) --> [of].


/*
Lexicon definitions found here:
Note that the lexicon is designed to allow for open use of nouns or adjectives.
*/
lex(the, X) :- !, X = det.
lex(a, X) :- !, X = det.
lex(an, X) :- !, X = det.
lex(is, X) :- !, X = v.
lex(what, X) :- !, X = pro(interrogative).
lex(_, n).
lex(_, adj).


/*
The "execute" predicate has an arity of 2, and expects the user to enter a sentence as a list of words as the first argument, and a variable as the second argument (the value to be returned)
execute will first run the passed sentence through the parser, and then will send the parsed sentence to another predicate which will in turn read the parsed sentence and pull out the relevant data.
*/
execute([], R) :- !, R = "You submitted a blank sentence!".
execute(List, Response) :- s(T, List, []), readParsed(T, Response), !.
execute(_, R) :- R = "I can\'t understand this sentence".


/*
"readParsed" predicate has an arity of 2 and expects a fully parsed sentence tree as its first argument, along with an uninstantiated variable as the second (this is the response to be returned)
This predicate will examine the arguments of the sentence tree to determine if it's a question or a statement. If it's a question, then the first argument to the noun phrase is the pronoun "what", 
and if this is the case the sentence tree is passed to the "processQuestion" predicate for handling.
If the sentence is not a question, it can be handled as a statement instead and is passed to the "processStatement" predicate instead.
*/
readParsed(ParsedSentence, Response) :- arg(1, ParsedSentence, X), arg(1, X, pro(what)), !, processQuestion(ParsedSentence, Response).
readParsed(ParsedSentence, Response) :- processStatement(ParsedSentence, Response).


/*
"processQuestion" predicate has an arity of 2, and expects the fully parsed sentence tree question as the first argument with an uninstantiated variable as its second (to be unified with the return value)
This predicate makes extensive use of the "arg" predicate to break down the sentence. Since the sentence is a question, the expected structure is known (see comment for the DCG), and the arg predicates
can be called in a sequential fashion to pull out the relevant "object" and "attribute" information. Those values, along with the uninstantiated response variable, are then passed to the "checkDBQuestion"
predicate for database evaluation and response unification.
The first rule handles questions of the form: "what is the <attribute> of the <object>"
The second rule handles questions of the form: "what <attribute> is the <object>"
*/
processQuestion(ParsedSentence, Response) :- arg(2, ParsedSentence, VP), arg(2, VP, NP), arg(2, NP, n(Attribute)), arg(3, NP, PP), arg(2, PP, NNP), 
                                                                         arg(2, NNP, n(Object)), !, checkDBQuestion(Attribute,Object,Response).																   
processQuestion(ParsedSentence, Response) :- arg(1, ParsedSentence, NP), arg(2, ParsedSentence, VP), arg(2, NP, n(Attribute)), arg(2, VP, NNP),
                                                                         arg(2, NNP, n(Object)), !, checkDBQuestion(Attribute,Object,Response).
																	   										   
																	   
/*
"processStatement" predicate has an arity of 2, and expects the fully parsed sentence tree statement as the first argument with an uninstantiated variable as its second (to be unified with the return value)
This predicate makes extensive use of the "arg" predicate to break down the sentence (working similarly to the "processQuestion" predicate). Since the sentence is a statement, the expected structure is known 
(see comment for the DCG), and the arg predicates can be called in a sequential fashion to pull out the relevant "attribute", "object", and "value" information from the sentence.
Those values, along with the uninstantiated response variable, are then passed to the "checkDB" predicate for database evaluation and response unification.

The following is a sample breakdown of a sentence "the color of the car is blue" to demonstrate the operation of these "process" predicates:
looks at arg 1 of the parsed sentence which is np(det(the), n(color), pp(prep(of), np(det(the), n(car)))), unifying with "NP"
next gets the second argument for the parse sentence, which is vp(v(is), np(n(blue))), unifying this with VP
then it looks at arg 2 of NP which is n(color), and "color" is unified with "Attribute"
then looks at arg 3 of the noun phrase which is pp(prep(of), np(det(the), n(car))), unifying with PP
then looks at arg 2 of PP which is np(det(the), n(car)), unifying with NNP (new noun phrase)
then looks at arg 2 of NNP which is n(car), unifying "car" with "Object"
then looks at arg 2 of VP which is np(n(blue)) and unifies with NNNP (new new noun phrase)
finally, NNNP is passed to the "getValue" predicate which will determine if it's a 1 or 2 word value, and unify the string representation of that value with "Value"
*/
processStatement(ParsedSentence, Response) :- arg(1, ParsedSentence, NP), arg(2, ParsedSentence, VP),
                                                                          arg(2, NP, n(Attribute)), arg(3, NP, PP), arg(2, PP, NNP), 
                                                                          arg(2, NNP, n(Object)), arg(2, VP, NNNP), getValue(NNNP, Value), checkDB(Attribute, Object, Value, Response).											   
																	   
																	   
/*
"checkDB" predicate has an arity of 4, and expects "Attribute" and "Object" and "Value" to be instantiated. "Response" is always expected to be
uninstantiated at first. This predicate is designed to handle the values passed in from a statement sentence. The first rule checks to see if the dynamic "fact" with these three values already exists 
in the DB, and if it does, unifies "Response" to be "I know". If the combination of these three things in "fact" doesn't exist in the DB, the second rule checks to see if
the "Attribute" and "Object" combination exists in the DB. If it does, the "Value" associated with those other two items is unified to "X", and all three values (along with "Response") are passed to the 
"printMessage" predicate for response handling. Finally, if neither of these rules applies, then a new "fact" needs to be created in the DB, and this is handled by the "assert" predicate call.
*/
checkDB(Attribute, Object, Value, Response) :- fact(Attribute, Object, Value), !, Response = "I know".
checkDB(Attribute, Object, _, Response) :- fact(Attribute, Object, X), !, printMessage(Attribute, Object, X, Response).
checkDB(Attribute, Object, Value, Response) :- assert(fact(Attribute, Object, Value)), !, Response = "OK".	


/*
The "checkDBQuestion" predicate has an arity of 3 and operates somewhat similarly to the "checkDB" predicate, but this one is designed to handle questions rather than statements.
In a question sentence, only "Attribute" and "Object" can be retrieved from the sentence, and these two values are expected to be instantiated by this predicate. "Response", as usual, is expected
to be uninstantiated. The first rule checks to see if the "Attribute" and "Object" combination exists in the database. If it does, the value associated with that pairing is unified with "Value", and this 
value is concatenated with "It's " and finally unified to "Response". If the fact doesn't exist in the DB, the second rule simply unifies the Response to "I don't know".
*/
checkDBQuestion(Attribute,Object,Response) :- fact(Attribute,Object,Value), !, string_concat("It\'s ", Value, Response).
checkDBQuestion(_,_,Response) :- Response = "I don\'t know".


/*
The "getValue" predicate handles getting single or multi-word values from the term NP which appears inside the VP of the parsed tree.
It concatenates the property values together as a single string and unifies to term Value. In the case that it's a single word value, that value is still converted
to a string before being unified to "Value" (this is done so that "Value" handling can always assume a string by the "check" and "printMessage" predicates).
*/
getValue(NP, Value) :- functor(NP,_,A), A=:=1, !, arg(1, NP, n(N)), term_string(N, Value).
getValue(NP, Value) :- functor(NP,_,A), A=:=2, arg(1, NP, det(D)), !, arg(2, NP, n(N)), term_string(D, V1), term_string(N, V2), string_concat(V1, " ", P1), string_concat(P1, V2, Value).
getValue(NP, Value) :- functor(NP,_,A), A=:=2, arg(1, NP, adj(Adj)), !, arg(2, NP, n(N)), term_string(Adj, V1), term_string(N, V2), string_concat(V1, " ", P1), string_concat(P1, V2, Value).


/*
The "printMessage" predicate has an arity of 4, and expects all passed values except "Response" to be instantiated. It's essentially a helper function, designed to be used when a statement has 
been processed that found contradictory information already in the DB. For example, if fact(color, car, "blue") already exists in the DB and the statement "the color of the car is green" is processed, 
this predicate is used to build the return response stating that the car is in fact "blue". Primarily used this way to clean up Response unification in the "checkDB" predicate.
Concatenates Attribute, Object, and Value to a single line string, and unifies that string to Response.
*/
printMessage(Attribute, Object, Value, Response) :- term_string(Attribute, Att), term_string(Object, Obj), string_concat("No, the ",Att, P1), string_concat(P1, " of the ", P2), string_concat(P2, Obj, P3), string_concat(P3, " is ", P4), string_concat(P4, Value, Response).


