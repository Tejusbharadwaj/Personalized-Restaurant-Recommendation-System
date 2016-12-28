;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Personalized Restaurant Recommendations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
A.1 Module MAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module MAIN


(deftemplate question
(slot text)
(slot type)
    (multislot valid)
(slot ident))

(deftemplate answer
(slot ident)
(slot text))

(deftemplate recommendation
(slot form)
(slot explanation))

(deffacts question-data
"The questions the system can ask."

(question (ident Chinese) (type yes-no)
(text "Do you feel like eating Chinese food?"))
(question (ident Indian) (type yes-no)
(text "Do you feel like eating Indian Food?"))
(question (ident Mexican) (type yes-no)
(text "Do you feel like eating Mexican food"))
(question (ident Italian) (type yes-no)
(text "Do you feel like eating Italian food"))
(question (ident Mediterranian) (type yes-no)
(text "Do you feel like eating Mediterranean food"))
(question (ident American) (type yes-no)
(text "Do you feel like eating American food"))
(question (ident TakeOut) (type yes-no)
(text "Would you like take out?"))
(question (ident DineIn) (type yes-no)
(text "Would you like to Dine In?"))
(question (ident Expensive) (type number)
(text "How much are you ready to spend? (1 - 3)")))
(defglobal ?*crlf* = "
")


A.2 Module ask
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module ask

(defmodule ask)


;if(numberp ?answer) then

(deffunction ask-user (?question ?type )
"Ask a question, and return the answer"
(bind ?answer "")
(while (not (is-of-type ?answer ?type )) do
(printout t ?question " ")
(if (eq ?type yes-no) then
(printout t "(yes or no) "))
(bind ?answer (read))
        )
(return ?answer))

(deffunction is-of-type (?answer ?type )
"Check that the answer has the right form"
(if (eq ?type yes-no) then
(return (or (eq ?answer yes) (eq ?answer no)))
else if (eq ?type number) then
            (if (numberp ?answer) then
            	(return (or (eq ?answer 1) (eq ?answer 2) (eq ?answer 3))))
          ))





(defrule ask::ask-question-by-id
"Given the identifier of a question, ask it and assert the answer"
(declare (auto-focus TRUE))
(MAIN::question (ident ?id) (text ?text) (type ?type))
(not (MAIN::answer (ident ?id)))
?ask <- (MAIN::ask ?id)
=>
(bind ?answer (ask-user ?text ?type))
(assert (answer (ident ?id) (text ?answer)))
(retract ?ask)
(return))


A.3 Module startup

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module startup

(defmodule startup)

(defrule print-banner
=>
(printout t "Type your name and press Enter> ")
(bind ?name (read))
(printout t crlf "**********************************" crlf)
(printout t " Hello, " ?name "." crlf)
(printout t " Welcome to the Restaurant advisor" crlf)
(printout t " Please answer the questions and" crlf)
(printout t " I will tell you where to eat" crlf)
(printout t " That may interest you :" crlf)
(printout t "**********************************" crlf crlf))


A.4 Module interview
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module interview
(defmodule interview)

(defrule request-Chinese
=>
(assert (ask Chinese)))

(defrule request-Indian
=>
(assert (ask Indian)))


(defrule request-Expensive
=>
          		
(assert      
    (ask Expensive)))

(defrule request-Mexican
=>
(assert (ask Mexican)))

(defrule request-Italian
=>
(assert (ask Italian)))

(defrule request-Mediterranian
=>
(assert (ask Mediterranian)))

(defrule request-American
=>
(assert (ask American)))

(defrule request-TakeOut
=>
(assert (ask TakeOut)))

(defrule request-DineIn
=>
(assert (ask DineIn)))


A.5 Module recommend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module recommend

(defmodule recommend)



(defrule PiatHeaven
(answer (ident Mediterranian) (text yes))
    (answer (ident Expensive) ( text 1))    
=>
(assert(recommendation (form PitaHeaven) (explanation "1250 S Michigan Ave, Chicago, IL 60605"))))

(defrule BabaPita
(answer (ident Mediterranian) (text yes))
    (answer (ident Expensive) ( text 2))        
=>
(assert(recommendation (form BabaPita) (explanation "2343 W Taylor St, Chicago, IL 60612"))))

(defrule Avec
(answer (ident Mediterranian) (text yes))
    (answer (ident Expensive) ( text 3)) 
    (answer (ident DineIn) ( text yes))    
=>
(assert(recommendation (form Avec) (explanation "615 W Randolph St, Chicago, IL 60661"))))

(defrule GhareebNawaz
(answer (ident Indian) (text yes))
    (answer (ident Expensive) ( text 1))    
=>
(assert(recommendation (form GhareebNawaz) (explanation "807 W Roosevelt Rd, Chicago, IL 60608"))))
    
(defrule MughalIndia
(answer (ident Indian) (text yes))
    (answer (ident Expensive) ( text 2))    
=>
(assert(recommendation (form MughalIndia) (explanation "560 W Van Buren St, Chicago, IL 60607"))))

(defrule IndiaGarden
(answer (ident Indian) (text yes))
    (answer (ident Expensive) ( text 3))
    (answer (ident DineIn) ( text yes))    
=>
(assert(recommendation (form IndiaGarden) (explanation "247 E Ontario St, Chicago, IL 60611"))))

(defrule Chilango
(answer (ident Mexican) (text yes))
    (answer (ident Expensive) ( text 1))    
=>
(assert(recommendation (form Chilago) (explanation "1437 W Taylor St, Chicago, IL 60607"))))
    
(defrule más
(answer (ident Mexican) (text yes))
    (answer (ident Expensive) ( text 2))    
=>
(assert(recommendation (form más) (explanation "800 W Washington Blvd, Chicago, IL 60607"))))

(defrule Topolobampo
(answer (ident Mexican) (text yes))
    (answer (ident Expensive) ( text 3)) 
    (answer (ident DineIn) ( text yes))   
=>
(assert(recommendation (form Topolobampo) (explanation " 445 North Clark St, Chicago, IL 60654"))))
    
(defrule J-C-Inn
(answer (ident Italian) (text yes))
    (answer (ident Expensive) ( text 1))    
=>
(assert(recommendation (form J-C-Inn) (explanation "558 W Van Buren St, Chicago, IL 60607"))))

(defrule Rosebud
(answer (ident Italian) (text yes))
    (answer (ident Expensive) ( text 2))    
=>
(assert(recommendation (form Rosebud) (explanation "1500 W Taylor St, Chicago, IL 60607"))))

(defrule Spiaggia
(answer (ident Italian) (text yes))
    (answer (ident Expensive) ( text 3)) 
    (answer (ident DineIn) ( text yes))   
=>
(assert(recommendation (form Spiaggia) (explanation "980 N Michigan Ave, Chicago, IL 60611"))))

(defrule Hana
(answer (ident Chinese) (text yes))
    (answer (ident Expensive) ( text 1))    
=>
(assert(recommendation (form Hana) (explanation "1311 W Taylor St, Chicago, IL 60607"))))

(defrule Jason'sWok
(answer (ident Chinese) (text yes))
    (answer (ident Expensive) ( text 2))    
=>
(assert(recommendation (form Jason'sWok) (explanation "1014 S Western Ave, Chicago, IL 60612"))))

(defrule ShanghaiTerrace 
(answer (ident Chinese) (text yes))
    (answer (ident Expensive) ( text 3)) 
    (answer (ident DineIn) ( text yes))   
=>
(assert(recommendation (form ShanghaiTerrace ) (explanation "The Peninsula Chicago, 108 E Superior St, Chicago, IL 60611"))))

(defrule ParkTavern
(answer (ident American) (text yes))
    (answer (ident Expensive) ( text 1))    
=>
(assert(recommendation (form ParkTavern) (explanation " 1645 W Jackson Blvd, Chicago, IL 60612"))))

(defrule Allium 
(answer (ident American) (text yes))
    (answer (ident Expensive) ( text 2))    
=>
(assert(recommendation (form Allium) (explanation " Four Seasons Hotel Chicago, 120 E Delaware Pl, Chicago, IL 60611"))))

(defrule Grace 
(answer (ident American) (text yes))
    (answer (ident Expensive) ( text 3))  
    (answer (ident DineIn) ( text yes))  
=>
(assert(recommendation (form Grace ) (explanation " 652 W Randolph St, Chicago, IL 60661"))))


A.6 Module report 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module report

(defmodule report)

(defrule sort-and-print
?r1 <- (recommendation (form ?f1) (explanation ?e))
(not (recommendation (form ?f2&:(< (str-compare ?f2 ?f1) 0))))
=>
(printout t "****************************** "  crlf)
    (printout t "****************************** "  crlf)
    (printout t "Recommendations for you - - " crlf crlf)
    (printout t ?f1 crlf)
    (printout t "Location: " ?e crlf )
    (printout t "****************************** "  crlf)
    (printout t "****************************** "  crlf crlf crlf)

(retract ?r1))


A.7 Control commands


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test data
(deffunction run-system ()
(reset)
(focus startup interview recommend report)
    
(run))
s
(while TRUE
;(run)
(run-system))