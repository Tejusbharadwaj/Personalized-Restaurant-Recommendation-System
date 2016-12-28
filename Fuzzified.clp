;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Personalized Restaurant Recommendations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
A.1 Module MAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module MAIN

(deftemplate Expensive (declare (ordered TRUE)))
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
;fuzzy variavles declaration
(defglobal ?*fuzzy-frequency* = 
    (new nrc.fuzzy.FuzzyVariable "Expensive" 0.00 10.00))
(defglobal ?*fuzzy-distance* = 
    (new nrc.fuzzy.FuzzyVariable "dist" 0.00 100.00))



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
(question (ident dist) (type number)
(text "How far do you want the restaurant to be?
            (Enter Approx Distance in Miles (1 - 100 miles))
            (Ans =>) "))
(question (ident DineIn) (type yes-no)
(text "Would you like to Dine In? 
            (An expensive restaurant might only offer dine in and 
             a cheap restaurant offers only take out)"))
(question (ident Expensive) (type number)
(text "How much are you ready to spend? (1(least expensive) to 10 (most expensive))")))
(defglobal ?*crlf* = "
")

(deffunction freq-range-store(?ans)
 (if (and(>= ?ans 0) (<= ?ans 3.00)) then (assert(theFreq (new nrc.fuzzy.FuzzyValue ?*fuzzy-frequency* "low"))))
    (if (and(>= ?ans 4.00) (<= ?ans 7.00)) then (assert(theFreq (new nrc.fuzzy.FuzzyValue ?*fuzzy-frequency* "medium"))))
    (if (and(>= ?ans 8.00) (<= ?ans 10)) then (assert(theFreq (new nrc.fuzzy.FuzzyValue ?*fuzzy-frequency* "high"))))
    )

(deffunction freq-range-dist(?ans)
    (if (and(>= ?ans 0) (<= ?ans 33.00)) then (assert(theDist (new nrc.fuzzy.FuzzyValue ?*fuzzy-distance* "close"))))
    (if (and(> ?ans 33.00) (<= ?ans 66.00)) then (assert(theDist (new nrc.fuzzy.FuzzyValue ?*fuzzy-distance* "med"))))
    (if (and(> ?ans 66.00) (<= ?ans 100)) then (assert(theDist (new nrc.fuzzy.FuzzyValue ?*fuzzy-distance* "far"))))

    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				;;	;;	;;	FUZZYFYING ;;  ;; ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmodule finit)
(defrule initial
	(declare (salience 100))
=>
(load-package nrc.fuzzy.jess.FuzzyFunctions)
		(bind ?xhigh (create$ 8.0 10.0))
    	(bind ?yhigh (create$ 0.0 1.0))
    	(bind ?xmedium  (create$ 4.0 7.0)) 
		(bind ?ymedium  (create$ 0.0 1.0)) 
		(bind ?xlow  (create$ 0.0 3.0)) 
		(bind ?ylow  (create$ 1.0 0.0))

    	
    	(?*fuzzy-frequency* addTerm "high" ?xhigh ?yhigh 2)
	    (?*fuzzy-frequency* addTerm "medium" ?xmedium ?ymedium 2)
     	(?*fuzzy-frequency* addTerm "low" ?xlow ?ylow 2)
   
        (bind ?xhigh1 (create$ 67.00 100.0))
    	(bind ?yhigh1 (create$ 0.0 1.0))
    	(bind ?xmedium1  (create$ 34.00 66.00)) 
		(bind ?ymedium1  (create$ 0.0 1.0)) 
		(bind ?xlow1  (create$ 0.0 33.00)) 
		(bind ?ylow1  (create$ 1.0 0.0))

    	
    	(?*fuzzy-distance* addTerm "far" ?xhigh1 ?yhigh1 2)
	    (?*fuzzy-distance* addTerm "med" ?xmedium1 ?ymedium1 2)
     	(?*fuzzy-distance* addTerm "close" ?xlow1 ?ylow1 2)
)


    	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


A.2 Module ask
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module ask

(defmodule ask)


;if(numberp ?answer) then

(deffunction ask-user (?question ?type ?ident)
"Ask a question, and return the answer"
(bind ?answer "")
(while (not (is-of-type ?answer ?type ?ident)) do
(printout t ?question " ")
(if (eq ?type yes-no) then
(printout t "(yes or no) "))
       ; (else (eq ?type number)
        ;    then
         ;   (printout t "Ans=> "))
(bind ?answer (read)))
        
        (if(eq ?ident Expensive) then (freq-range-store ?answer))
    (if(eq ?ident dist) then (freq-range-dist ?answer))
    
        
(return ?answer))

(deffunction is-of-type (?answer ?type ?ident)
"Check that the answer has the right form"
(if (eq ?type yes-no) then
(return (or (eq ?answer yes) (eq ?answer no)))
else if (eq ?type number) then
(if (numberp ?answer) then
(if (eq ?ident Expensive) then
(return (and (>= ?answer 0) (<= ?answer 10)))
else(if (eq ?ident dist) then
(return (and (>= ?answer 0) (<= ?answer 100)))
 
)))))



(defrule ask::ask-question-by-id
"Given the identifier of a question, ask it and assert the answer"
(declare (auto-focus TRUE))
(MAIN::question (ident ?id) (text ?text) (type ?type))
(not (MAIN::answer (ident ?id)))
?ask <- (MAIN::ask ?id)
=>
(bind ?answer (ask-user ?text ?type ?id))
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

(defrule request-dist
=>
(assert (ask dist)))

(defrule request-DineIn
=>
(assert (ask DineIn)))


A.5 Module recommend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module recommend

(defmodule recommend)



(defrule PiatHeaven
(answer (ident Mediterranian) (text yes))
     (theFreq ?x&: (fuzzy-match ?x "low"))
    (theDist ?d&: (fuzzy-match ?d "far"))   
=>
(assert(recommendation (form PitaHeaven) (explanation "1250 S Michigan Ave, Chicago, IL 60605"))))

(defrule Jubrano
(answer (ident Mediterranian) (text yes))
     (theFreq ?x&: (fuzzy-match ?x "low"))
    (or (theDist ?d&: (fuzzy-match ?d "close")) (theDist ?d&: (fuzzy-match ?d "med")))  
=>
(assert(recommendation (form Jubrano) (explanation "1250 W Taylor ST, Chicago, IL 60607"))))


(defrule BabaPita
(answer (ident Mediterranian) (text yes))
     (theFreq ?x&: (fuzzy-match ?x "medium")) 
        (or (theDist ?d&: (fuzzy-match ?d "close")) (theDist ?d&: (fuzzy-match ?d "med")))     
=>
(assert(recommendation (form BabaPita) (explanation "2343 W Taylor St, Chicago, IL 60612"))))

(defrule Taxim
(answer (ident Mediterranian) (text yes))
     (theFreq ?x&: (fuzzy-match ?x "high"))    
    (or (answer (ident DineIn) (text yes)) (answer (ident DineIn) (text no)))
    (theDist ?d&: (fuzzy-match ?d "far"))     
=>
(assert(recommendation (form Taxim) (explanation "1558 N Milwaukee Ave, Chicago, IL 60622"))))

(defrule Avec
(answer (ident Mediterranian) (text yes))
   (theFreq ?x&: (fuzzy-match ?x "high"))
    (answer (ident DineIn) ( text yes))     
    (theDist ?d&: (or (fuzzy-match ?d "close") (fuzzy-match ?d "med")))    
=>
(assert(recommendation (form Avec) (explanation "615 W Randolph St, Chicago, IL 60661"))))



(defrule GhareebNawaz
(answer (ident Indian) (text yes))
    (theFreq ?x&: (fuzzy-match ?x "low"))
     (or (theDist ?d&: (fuzzy-match ?d "close")) (theDist ?d&: (fuzzy-match ?d "med")))   
=>
(assert(recommendation (form GhareebNawaz) (explanation "807 W Roosevelt Rd, Chicago, IL 60608"))))

(defrule NepalHouse
(answer (ident Indian) (text yes))
    (theFreq ?x&: (fuzzy-match ?x "low"))
    (theDist ?d&: (fuzzy-match ?d "far"))    
=>
(assert(recommendation (form NepalHouse) (explanation "100 S State, Chicago, IL 60605"))))
    
(defrule MughalIndia
(answer (ident Indian) (text yes))
   (theFreq ?x&: (fuzzy-match ?x "medium")) 
    ;(or (answer (ident DineIn) (text yes)) (answer (ident DineIn) (text no))) 
     (or (theDist ?d&: (fuzzy-match ?d "close")) (theDist ?d&: (fuzzy-match ?d "med")))       
=>
(assert(recommendation (form MughalIndia) (explanation "560 W Van Buren St, Chicago, IL 60607"))))

(defrule Hema
(answer (ident Indian) (text yes))
   (theFreq ?x&: (fuzzy-match ?x "medium")) 
    ;(or (answer (ident DineIn) (text yes)) (answer (ident DineIn) (text no))) 
     (theDist ?d&: (fuzzy-match ?d "far"))      
=>
(assert(recommendation (form Hema) (explanation "2462 W Devon Ave, Chicago, IL 60607"))))

(defrule IndiaGarden
(answer (ident Indian) (text yes))
     (theFreq ?x&: (fuzzy-match ?x "high"))
    (answer (ident DineIn) ( text yes))    
     (or (theDist ?d&: (fuzzy-match ?d "close")) (theDist ?d&: (fuzzy-match ?d "med"))) 
=>
(assert(recommendation (form IndiaGarden) (explanation "247 E Ontario St, Chicago, IL 60611"))))

(defrule Sabri
(answer (ident Indian) (text yes))
     (theFreq ?x&: (fuzzy-match ?x "high"))
    (answer (ident DineIn) ( text yes))    
   (theDist ?d&: (fuzzy-match ?d "far"))
=>
(assert(recommendation (form Sabri) (explanation "2514 W Devon, Chicago, IL 60611"))))



(defrule Chilango
(answer (ident Mexican) (text yes))
    (theFreq ?x&: (fuzzy-match ?x "low"))
    (or (theDist ?d&: (fuzzy-match ?d "close")) (theDist ?d&: (fuzzy-match ?d "med")))  
=>
(assert(recommendation (form Chilago) (explanation "1437 W Taylor St, Chicago, IL 60607"))))

(defrule LoLo
(answer (ident Mexican) (text yes))
    (theFreq ?x&: (fuzzy-match ?x "low"))
    (theDist ?d&: (fuzzy-match ?d "far")) 
=>
(assert(recommendation (form LoLo) (explanation "1000 S Halstead St, Chicago, IL 60637"))))
    
(defrule más
(answer (ident Mexican) (text yes))
     (theFreq ?x&: (fuzzy-match ?x "medium")) 
         (or (theDist ?d&: (fuzzy-match ?d "close")) (theDist ?d&: (fuzzy-match ?d "med")))  

    (or (answer (ident DineIn) (text yes)) (answer (ident DineIn) (text no)))       
=>
(assert(recommendation (form más) (explanation "800 W Washington Blvd, Chicago, IL 60607"))))

(defrule másUno
(answer (ident Mexican) (text yes))
     (theFreq ?x&: (fuzzy-match ?x "medium")) 
    (theDist ?d&: (fuzzy-match ?d "far")) 

    (or (answer (ident DineIn) (text yes)) (answer (ident DineIn) (text no)))       
=>
(assert(recommendation (form más) (explanation "1800 W Wisconsin Blvd, Chicago, IL 60627"))))

(defrule Topolobampo
(answer (ident Mexican) (text yes))
     (theFreq ?x&: (fuzzy-match ?x "high")) 
         (or (theDist ?d&: (fuzzy-match ?d "close")) (theDist ?d&: (fuzzy-match ?d "med")))  

    (answer (ident DineIn) ( text yes))   
=>
(assert(recommendation (form Topolobampo) (explanation "558 W Van Buren St, Chicago, IL 60607"))))

    
(defrule J-C-Inn
(answer (ident Italian) (text yes))
     (theFreq ?x&: (fuzzy-match ?x "high")) 
    (theDist ?d&: (fuzzy-match ?d "far")) 
    (answer (ident DineIn) (text yes))  
=>
(assert(recommendation (form J-C-Inn) (explanation "445 North Clark St, Chicago, IL 60654"))))



(defrule Pizzanos
(answer (ident Italian) (text yes))
    (theFreq ?x&: (fuzzy-match ?x "low"))
         (or (theDist ?d&: (fuzzy-match ?d "close")) (theDist ?d&: (fuzzy-match ?d "med")))  
    (answer (ident DineIn) (text no))  
=>
(assert(recommendation (form Pizzanos) (explanation "100 N Mishigan Ave, Chicago, IL 606017"))))

(defrule Leonas
(answer (ident Italian) (text yes))
    (theFreq ?x&: (fuzzy-match ?x "low"))
    (theDist ?d&: (fuzzy-match ?d "far")) 
    (answer (ident DineIn) (text no))  
=>
(assert(recommendation (form Leonas) (explanation "3877 N Elston Ave, Chicago, IL 60638"))))

(defrule Rosebud
(answer (ident Italian) (text yes))
    (theFreq ?x&: (fuzzy-match ?x "medium"))
             (or (theDist ?d&: (fuzzy-match ?d "close")) (theDist ?d&: (fuzzy-match ?d "med")))  
  
    (or (answer (ident DineIn) (text yes)) (answer (ident DineIn) (text no)))      
=>
(assert(recommendation (form Rosebud) (explanation "1500 W Taylor St, Chicago, IL 60607"))))

(defrule SanosPizzeria
(answer (ident Italian) (text yes))
    (theFreq ?x&: (fuzzy-match ?x "medium"))
    (theDist ?d&: (fuzzy-match ?d "far")) 
  
    (or (answer (ident DineIn) (text yes)) (answer (ident DineIn) (text no)))      
=>
(assert(recommendation (form SanosPizzeria) (explanation "4469 W Lawrence Ave, Chicago, IL 60630"))))

(defrule Spiaggia
(answer (ident Italian) (text yes))
     (theFreq ?x&: (fuzzy-match ?x "high"))
              (or (theDist ?d&: (fuzzy-match ?d "close")) (theDist ?d&: (fuzzy-match ?d "med")))  
 
    (answer (ident DineIn) ( text yes))   
=>
(assert(recommendation (form Spiaggia) (explanation "980 N Michigan Ave, Chicago, IL 60611"))))

(defrule LaConcodia
(answer (ident Italian) (text yes))
     (theFreq ?x&: (fuzzy-match ?x "high"))
    (theDist ?d&: (fuzzy-match ?d "far")) 
 
    (answer (ident DineIn) ( text yes))   
=>
(assert(recommendation (form LaConcodia) (explanation "3724 W Montrose Ave, Chicago, IL 60624"))))

(defrule Hana
(answer (ident Chinese) (text yes))
    (theFreq ?x&: (fuzzy-match ?x "low"))
    (theDist ?d&: (fuzzy-match ?d "close")) 
    (answer (ident DineIn) (text no))
=>
(assert(recommendation (form Hana) (explanation "1311 W Taylor St, Chicago, IL 60607"))))

(defrule KingWok
(answer (ident Chinese) (text yes))
    (theFreq ?x&: (fuzzy-match ?x "low"))
    (theDist ?d&: (fuzzy-match ?d "far")) 
    (answer (ident DineIn) (text no))
=>
(assert(recommendation (form KingWok) (explanation "500 N Western Ave, Chicago, IL 60648"))))

(defrule HongKong
(answer (ident Chinese) (text yes))
    (theFreq ?x&: (fuzzy-match ?x "low"))
    (theDist ?d&: (fuzzy-match ?d "med")) 
    (answer (ident DineIn) (text no))
=>
(assert(recommendation (form HongKong) (explanation "1005 S Western Ave, Chicago, IL 60648"))))

(defrule Jason'sWok
(answer (ident Chinese) (text yes))
     (theFreq ?x&: (fuzzy-match ?x "medium")) 
    (theDist ?d&: (fuzzy-match ?d "close"))
    (or (answer (ident DineIn) (text yes)) (answer (ident DineIn) (text no)))       
=>
(assert(recommendation (form Jason'sWok) (explanation "1014 S Western Ave, Chicago, IL 60612"))))

(defrule Chings
(answer (ident Chinese) (text yes))
     (theFreq ?x&: (fuzzy-match ?x "medium")) 
    (theDist ?d&: (fuzzy-match ?d "med"))
    (or (answer (ident DineIn) (text yes)) (answer (ident DineIn) (text no)))       
=>
(assert(recommendation (form Chings) (explanation "1014 W Madison St, Chicago, IL 60612"))))

(defrule BeijingBytes
(answer (ident Chinese) (text yes))
     (theFreq ?x&: (fuzzy-match ?x "medium")) 
    (theDist ?d&: (fuzzy-match ?d "far"))
    (or (answer (ident DineIn) (text yes)) (answer (ident DineIn) (text no)))       
=>
(assert(recommendation (form BeijingBytes) (explanation "1014 N Belmont ave, Chicago, IL 60652"))))

(defrule ShanghaiTerrace 
(answer (ident Chinese) (text yes))
     (theFreq ?x&: (fuzzy-match ?x "high"))
    (theDist ?d&: (fuzzy-match ?d "close"))
    (answer (ident DineIn) ( text yes))   
=>
(assert(recommendation (form ShanghaiTerrace ) (explanation "The Peninsula Chicago, 108 E Superior St, Chicago, IL 60611"))))

(defrule BeijingTerrace 
(answer (ident Chinese) (text yes))
     (theFreq ?x&: (fuzzy-match ?x "high"))
    (theDist ?d&: (fuzzy-match ?d "far"))
    (answer (ident DineIn) ( text yes))   
=>
(assert(recommendation (form BeijingTerrace ) (explanation " 108 E Tinley Park St, IL 60411"))))

(defrule LinconChina 
(answer (ident Chinese) (text yes))
     (theFreq ?x&: (fuzzy-match ?x "high"))
    (theDist ?d&: (fuzzy-match ?d "med"))
    (answer (ident DineIn) ( text yes))   
=>
(assert(recommendation (form LinconChina ) (explanation " 1008 E Lincon Park St, IL 60411"))))

(defrule ParkTavern
(answer (ident American) (text yes))
    (theFreq ?x&: (fuzzy-match ?x "low"))  
              (or (theDist ?d&: (fuzzy-match ?d "close")) (theDist ?d&: (fuzzy-match ?d "med")))  
    (answer (ident DineIn) (text no))
    
=>
(assert(recommendation (form ParkTavern) (explanation " 1645 W Jackson Blvd, Chicago, IL 60612"))))

(defrule Allium 
(answer (ident American) (text yes))
    (theFreq ?x&: (fuzzy-match ?x "medium")) 
              (or (theDist ?d&: (fuzzy-match ?d "close")) (theDist ?d&: (fuzzy-match ?d "med")))  
 =>
(assert(recommendation (form Allium) (explanation " Four Seasons Hotel Chicago, 120 E Delaware Pl, Chicago, IL 60611"))))

(defrule Grace 
(answer (ident American) (text yes))
     (theFreq ?x&: (fuzzy-match ?x "high"))
              (or (theDist ?d&: (fuzzy-match ?d "close")) (theDist ?d&: (fuzzy-match ?d "med")))  

    (answer (ident DineIn) ( text yes))  
=>
(assert(recommendation (form Grace ) (explanation " 652 W Randolph St, Chicago, IL 60661"))))





(defrule HuddleHouse
(answer (ident American) (text yes))
    (theFreq ?x&: (fuzzy-match ?x "low"))  
    (theDist ?d&: (fuzzy-match ?d "far")) 
    (answer (ident DineIn) (text no))
    
=>
(assert(recommendation (form HuddleHouse) (explanation "4748 N Kimball Ave, Chicago, IL 60625"))))

(defrule NorthsideGrill
(answer (ident American) (text yes))
    (theFreq ?x&: (fuzzy-match ?x "medium")) 
    (theDist ?d&: (fuzzy-match ?d "far")) 
 
    (or (answer (ident DineIn) (text yes)) (answer (ident DineIn) (text no)))   
=>
(assert(recommendation (form NorthsideGrill) (explanation "4351 N Elston Ave, Chicago, IL 60641"))))

(defrule Zebda
(answer (ident American) (text yes))
     (theFreq ?x&: (fuzzy-match ?x "high"))
    (theDist ?d&: (fuzzy-match ?d "far")) 

    (answer (ident DineIn) ( text yes))  
=>
(assert(recommendation (form Zebda) (explanation "4344 N Elston Ave, Chicago, IL 60641"))))









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
(focus finit startup interview recommend report)
    
(run))
s
(while TRUE
;(run)
(run-system))