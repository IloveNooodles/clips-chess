Author: Monica Kumaran
Project: Fantastic Books and Where to Find Them 

(deftemplate book (slot t) (slot a) (multislot genre) (multislot story) (multislot tone) (multislot style))

(deftemplate foundbook (slot t))

(deffunction printthis (?string)
	(bind ?i 1); a counter

	/*
 	The while loop goes letter by letter through the string. When it finds an underscore it
	concatenates the two pieces of string around the underscore
	with a space where the underscore used to be. 
	*/
	(while (< ?i (str-length ?string))
	(if (eq (str-compare (sub-string ?i ?i ?string) "_") 0) then 
	(bind ?string (str-cat (sub-string 1 (- ?i 1) ?string) " " (sub-string (+ ?i 1) (str-length ?string) ?string))) )
	(++ ?i))

	(printout t ?string crlf)
)

(deffunction readthis (?string)
	(bind ?i 1); a counter

	/*
 	The while loop goes letter by letter through the string. When it finds a space it concatenates
	the two pieces of string around the space with an underscore where the space used to be. 
	*/
	(while (< ?i (str-length ?string))
	(if (eq (str-compare (sub-string ?i ?i ?string) " ") 0) then 
	(bind ?string (str-cat (sub-string 1 (- ?i 1) ?string) "_" (sub-string (+ ?i 1) (str-length ?string) ?string))) )
	(++ ?i))

	(return ?string)
)

(deffunction ncontains (?char ?multislot)
	(if (integerp (member$ ?char ?multislot)) then (return FALSE) else (return TRUE))
)

(deffunction prefer (?list)
	(bind ?i 1)
	(bind ?string "Do you prefer "); first part of question
	
	(if (< (length$ ?list) 2) then 
	/* A while loop adds in all (except the last) elements of the list */
	((while (< ?i (length$ ?list))
	(bind ?string (str-cat ?string (nth$ ?i ?list) ", ")); adds in i-th element from i = 1 to i = list length - 1
	(++ ?i)))
	else (bind ?string (str-cat ?string (first$ ?list))));if the list has two elements it adds in the first element
							     ;without a comma

	;the last element is added, in the form "or ____ books?"
	(bind ?string (str-cat ?string " or " (nth$ (length$ ?list) ?list) " books?"))
	
	;question asked and answer received
	(printthis ?string)
	(bind ?ans (readthis (readline)))
	(assert-string (str-cat "(must " ?ans ")"))
	
)

(deffacts library
	(book (t The_Harry_Potter_series) (a J.K._Rowling) (genre fantasy) (story plot-driven world-building) (tone atmospheric suspenseful))
	(book (t The_Lightning_Thief) (a Rick_Riordan) (genre fantasy adventure book_to_movie mythological) (story plot-driven) (tone funny))
	(book (t The_Fault_in_Our_Stars) (a John_Green) (genre realistic) (story character-driven))
	(book (t Looking_for_Alaska) (a John_Green) (genre coming-of-age mystery realistic) (story character-driven))
	(book (t The_Hunger_Games) (a Suzanne_Collins) (genre book_to_movie science_fiction dystopia) (story plot-driven))	
	(book (t Little_Brother) (a Cory_Doctorow) (genre science_fiction dystopia) (story plot-driven) (tone thought_provoking) (style jargon-filled))
	(book (t Matched) (a Allyson_Condie) (genre science_fiction dystopia) (story character-driven) (tone thought_provoking) (style lyrical))
	(book (t Candor) (a Pam_Bachorz) (genre dystopia) (style fast-paced) (tone disturbing))
	(book (t Cinder) (a Marissa_Meyer) (story plot-driven world-building) (genre dystopia science_fiction) (tone romantic))
	(book (t Monsters_of_Men) (a Patrick_Ness) (genre science_fiction dystopia) (story plot-driven) (tone bleak menacing thought_provoking))
	(book (t Catch-22) (a Joseph_Heller) (story character-driven) (genre antiwar_story black_humor book_to_movie war_story satirical multiple_perspectives) (tone darkly_humorous moving sobering) (style stylistically_complex witty))
	(book (t The_Catcher_in_the_Rye) (a J.D._Salinger) (genre coming-of-age first_person psychological) (tone darkly_humorous moving reflective))
	(book (t Howl's_Moving_Castle) (a Diana_Wynne_Jones) (genre book_to_movie fantasy humorous) (style compelling) (tone atmospheric funny offbeat romantic) (story world-building))
	(book (t The_Woman_Who_Died_A_Lot) (a Jasper_Fforde) (genre alternative_history fantasy first_person literary metafiction mystery) (story intricately_plotted plot-driven) (tone dramatic funny) (style witty stylistically_complex))
	(book (t Ready_Player_One) (a Ernest_Cline) (genre coming-of-age first_person science_fiction) (story action-packed intricately_plotted) (tone funny offbeat nostalgic) (style jargon-filled))
	(book (t Incarceron) (a Catherine_Fisher) (genre science_fiction) (story world-building action-packed intricately_plotted) (tone bleak suspenseful) (style compelling lyrical))
	(book (t The_Oracle_Betrayed) (a Catherine_Fisher) (genre fantasy) (story plot-driven world-building) (tone suspenseful) (style compelling))
	(book (t Starcrossed) (a Elizabeth_Bunce) (genre fantasy) (story world-building) (tone sarcastic suspenseful) (style stylistically_complex))
	(book (t The_Da_Vinci_Code) (a Dan_Brown) (genre book_to_movie suspense) (story plot-driven intricately-plotted) (tone atmospheric suspenseful) (style descriptive jargon-filled))		
	(book (t The_Rook) (a Daniel_O'Malley) (genre diary suspense urban_fantasy) (story action-packed) (tone offbeat) (style gritty))
	(book (t Pride_and_Prejudice) (a Jane_Austen) (genre book_to_movie domestic_fiction love_story) (story character-driven) (tone atmospheric romantic) (style engaging))
	(book (t The_Great_Gatsby) (a F.Scott_Fitzgerald) (genre book_to_movie) (story character-driven) (tone atmospheric romantic) (style lyrical stylistically_complex))
	(book (t The_Time_Traveler's_Wife) (a Audrey_Niffenegger) (genre literary multiple_perspectives domestic first_person) (story character-driven intricately-plotted) (tone romantic thought-provoking) (style witty lyrical))
	(book (t Twilight) (a Stephenie_Meyer) (genre book_to_movie first_person paranormal_romance) (story character-driven) (tone suspenseful))
	(book (t The_Hobbit) (a J.R.R._Tolkien) (genre book_to_movie epic fantasy) (story world-building plot-driven) (tone upbeat) (style stylistically_complex))
	(book (t Good_Omens) (a Neil_Gaiman) (genre humorous satirical) (tone darkly_humorous upbeat) (style witty))
	(book (t Interworld) (a Neil_Gaiman) (genre fantasy science_fiction) (story plot-driven) (tone sarcastic) (style compelling descriptive))
	(book (t Nation) (a Terry_Pratchett) (genre adventure fantasy) (story character-driven) (tone funny thought-provoking) (style engaging))
	(book (t The_Night_Circus) (a Erin_Morgenstern) (genre fantasy) (tone atmospheric romantic) (style detail_rich))
	(book (t The_Godfather) (a Mario_Puzo) (genre book_to_movie family_saga) (story character-driven) (tone dramatic haunting violent) (style compelling gritty))
	(book (t Ship_Breaker) (a Paolo_Bacigalupi) (genre science_fiction) (story action-packed world-building) (tone bleak) (style compelling))
	(book (t The_Casual_Vacancy) (a J.K._Rowling) (genre black_humor satirical) (story character-driven) (tone bleak darkly_humorous))
	(book (t Nancy_Drew_mysteries) (a Carolyn_Keene) (genre mystery) (story plot-driven) (tone suspenseful))
	(book (t Hardy_Boys_mysteries) (a Frankline_W._Dixon) (genre mystery) (story plot-driven))
	(book (t The_House_of_the_Scorpion) (a Nancy_Farmer) (genre science_fiction dystopia) (story world-building) (tone suspenseful))
	(book (t The_Shadow_Speaker) (a Nnedi_Okorafor-Mbachu) (genre science_fiction fantasy adventure bildungsroman historical_drama) (story world-building African plot-driven) (tone serious))
)

(defrule startup
	=>
	(printout t "Welcome to the Book Guru!" crlf)
	(printout t "I will recommend your perfect (fiction) book." crlf)
	(printout t "You can skip the first question by typing in \"skip\"!" crlf)
	(printout t crlf)
	(printout t "What was the last fiction you enjoyed?" crlf)
	(bind ?lastbook (readthis (readline)))
	(if (neq (str-compare ?lastbook "skip") 0);user can skip first question by typing "skip"
	then 
	(assert-string (str-cat "(foundbook (t " ?lastbook "))"));asserts as foundbook b/c might be recognized
	
	(printout t "What about it did you enjoy?" crlf)
	(bind ?appeal (readthis (readline)))
	(assert (must (sym-cat ?appeal))); the user given appeal factor is asserted 
		
	(printout t "Was there anything you did not like? (If so, enter the characteristic)" crlf)
	(bind ?dislike (readthis (readline)))
	(assert (must-not (sym-cat ?dislike)))
	
	else (assert (question)));if user skips question will assert (question)
				 ;to trigger question asking rules
	
)

(defrule must-have
	(must ?char)

	/* For each slot of book the value there is checked to make sure it does not contain ?char. For
	slot a the function neq is used to compare the slot value and ?char. The rest of the slots are multislots, 
	so the list contained in the multislot is assigned to a variable and sent through the function ncontains.*/

	?book <- (book (a ?a&:(neq ?char ?a)) (genre $?genre&:(ncontains ?char ?genre)) (story $?story&:(ncontains ?char ?story))
	(tone $?tone&:(ncontains ?char ?tone)) (style $?style&:(ncontains ?char ?style)))

	/* This makes sure that there is at least one book in the database which has the required characteristic.
	Otherwise, all the books could be retracted, giving an inaccurate recommendation. */
	(exists (or 
	(book (a ?char))
	(book (genre $? ?char $?))
	(book (story $? char $?))
	(book (tone $? ?char $?))
	(book (style $? ?char $?))))
	=>
	(retract ?book)	
)

(defrule must-not-have
	(must-not ?char)

	/* 
	
	The or constraint is used to find books which have the ?char.
	For each slot of book the value there is checked to make sure it does not contain ?char. For
	the slots t and a the function eq is used to compare the slot value and ?char. The rest of the slots are multislots, 
	so the list contained in the multislot is assigned to a variable and sent through the function contains.
	
	*/

	?book <- (or (book (a ?char))
		(book (genre $? ?char $?))
		(book (story $? char $?))
		(book (tone $? ?char $?))
		(book (style $? ?char $?)))
	=>
	(retract ?book)	
)

(defrule recommEND
	(declare (salience 1));this rule must fire as soon as it's activated so I gave it a higher salience
	?count <- (accumulate (bind ?bookcount 0);the accumulate function lets me keep a count for how many times
			(++ ?bookcount)		 ;a pattern, in this case a book, passes through
			?bookcount
			(book (t ?t) (a ?a)))
	(test (= ?count 1)); if there is only one book left
	(book (t ?t) (a ?a)); the book is matched for here so I have a handle for the title and author I'm printing out
	=>
	(printthis (str-cat "I recommend " ?t " by " ?a "!"))
	(clear)	
)

(defrule askq 
	(ask ?char)

	;makes sure that at least one book has that ?char
	(exists (or 
	(book (a ?char))
	(book (genre $? ?char $?))
	(book (story $? char $?))
	(book (tone $? ?char $?))
	(book (style $? ?char $?))))

	;and that ?char has not already been asked about
	(not (or (must ?char) (must-not ?char)))
	=>
	(printthis (str-cat "Do you want to read a " ?char " novel?")); before printing out the string, all underscores are replaced with spaces
	(bind ?ans (read)); takes in user response
	
	;if response is yes then asserted as must, if no then asserted as must-not
	(if (eq (str-compare (sub-string 1 1 ?ans) "y") 0) then (assert-string (str-cat "(must " ?char ")")) )
	(if (eq (str-compare (sub-string 1 1 ?ans) "n") 0) then (assert-string (str-cat "(must-not " ?char ")")) )
)


(deffunction ask-multifield (?list)
	(bind ?i 1)
	(while (<= ?i (length$ ?list)) (assert (ask (nth$ ?i ?list))) (++ ?i))	
)

(defrule recognize-book	

	;if book and foundbook have same title
	?book <- (book (t ?t))
	(foundbook (t ?t))

	=>
	;this will allow other question-asking rules to begin asking questions
	(assert (question))
	
	;will ask questions about every known characteristic about the recognized book
	;the first few questions have a tendency to render the following planned questions
	;obsolete
	(ask-multifield ?book.genre)
	(ask-multifield ?book.tone)
	(ask-multifield ?book.style)
	(ask-multifield ?book.story)

	;retracts book so system won't recommend the book the person has already read
	(retract ?book)	
)

(defrule dont-recognize-book
	(foundbook (t ?t))
	(not (book (t ?t)))
	=>
	(assert (question))
)


(defrule ask-question-about-genre
	(question);the requirement of the (question) assertion is to make sure that 
		  ; recognize-book would be able to ask questions before this rule
	
	;takes two books which have at least one characteristic in the the genre field
	?book1 <- (book (genre $?genre&:(> (length$ ?genre) 0)))
	?book2 <- (book (genre $?genre2&:(> (length$ ?genre2) 0)))
	
	;testing to make sure that the books don't have the exact same genres.
	;&~ constraint doesn't work because the genres can have the same characteristics
	;written in different orders and the system will think
	;they have different genres, so I created a list of the genres in ?genre2
	;that ?genre doesn't have and made sure that there's something in it

	(test (neq (length$ (complement$ ?genre ?genre2)) 0))

	=>
	;creates list of genres from both books that the other book doesn't have
	(bind ?list (create$ (complement$ ?genre ?genre2) (complement$ ?genre2 ?genre)))
		
	;asks about characteristics in list
	(bind ?i 1)
	(while (< ?i (length$ ?list)) (assert (ask (nth$ ?i ?list))) (++ ?i))
)

(defrule ask-question-about-tone
	(question)
	?book1 <- (book (tone $?tone&:(> (length$ ?tone) 0)))
	?book2 <- (book (tone $?tone2&:(> (length$ ?tone2) 0)))
	
	(test (neq (length$ (complement$ ?tone ?tone2)) 0))

	=>
	(bind ?list (create$ (complement$ ?tone ?tone2) (complement$ ?tone2 ?tone)))
	(bind ?i 1)
	(while (< ?i (length$ ?list)) (assert (ask (nth$ ?i ?list))) (++ ?i))
)


(defrule ask-question-about-story
	(question)
	?book1 <- (book (story $?story&:(> (length$ ?story) 0)))
	?book2 <- (book (story $?story2&:(> (length$ ?story2) 0)))
	
	(test (neq (length$ (complement$ ?story ?story2)) 0))

	=>
	(bind ?list (create$ (complement$ ?story ?story2) (complement$ ?story2 ?story)))
	(if (= (length$ ?list) 1) then (assert (ask ?list)) else (prefer ?list))
	
)


(defrule ask-question-about-style
	(question)
	?book1 <- (book (style $?style&:(> (length$ ?style) 0)))
	?book2 <- (book (style $?style2&:(> (length$ ?style2) 0)))
	
	(test (neq (length$ (complement$ ?style ?style2)) 0))

	=>
	(bind ?list (create$ (complement$ ?style ?style2) (complement$ ?style2 ?style)))
	;characteristics are asked one by one rather than in the prefer construct b/c the 
	;values in the style slot tend not to have too much distinction between them
	(bind ?i 1)
	(while (< ?i (length$ ?list)) (assert (ask (nth$ ?i ?list))) (++ ?i))
)


(reset)

(run)
