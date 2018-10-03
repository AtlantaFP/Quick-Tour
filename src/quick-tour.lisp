#| 

Note: This talk does not address macros in any depth.

Goal: Walk through a problem, showing Lispy ways to do tasks.



Comments in Lisp

- Text between sharp-bar to bar-sharp is ignored by READ.|#

#| 
Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal.

Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this.

But, in a larger sense, we can not dedicate -- we can not consecrate -- we can not hallow -- this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us -- that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion -- that we here highly resolve that these dead shall not have died in vain -- that this nation, under God, shall have a new birth of freedom -- and that government of the people, by the people, for the people, shall not perish from the earth.
|#

#|
- All text on a line after a raw semicolon is ingnored by READ.

Conventional,but not universal:
- Sharp-bar to bar-sharp for extensive comments.
;;;; Four semicolons at left margin for headings.
;;; Three semicolons at left margin for descriptions of functions or macros headings.
;; Two semicolons aligned with line below to explain line below
; One semicolon at end of line to comment on that line




;;;; Our puzzle:

Given a URL,
- retrieve a text file,
- then analyze it, and
- return 
  - the ten words that occur most frequently, and
  - the ten least frequent.


Techniques:
- Hello Lisp
- Hello REPL
- load and use third-party libraries
- retrieve text via HTTP
- file operations                                         
- basic string operation (split into words)
- create a tally of words (we'll use a hash table)
- convert hash table to a list of (word count) pairs   --- Use LOOP macro/DSL
- List manipulation
  - sort a list ---- actually a list of lists          --- Use LOOP macro/DSL 
  - select subsequences
;;;  - flatten lists of lists into a lists of words    --- use destructuring-bind
- create and present a report                          --- use FORMAT




Hello Lisp!

Lisp evaluates atoms and lists.
Atoms are values that are not further evaluated.
-numbers              123 or 451.01 or #C(0 1)       --- #C(r i) is lisp notation for a complex number  
-characters           #\m                            --- #\x is Lisp notation for individual characters
-a string             "hello" 
-symbols              'BUILDING or :COUNT            --- symbols are NOT strings
-logical values       T and nil                      --- any non-nil value is NOT FALSE

Lists contain zero or more atoms or lists.
()
(1 2 3)
(1 (2 3))

Lists are usually evaluated:
(some-function operand-1 operand-2 ...)

This is prefix notation, and is a primitive syntax tree.

Lisp is eager-evaluation.

REPL> (+ 17 23)
40
Operands are recursively evaluated before being fed to function

REPL> (+ 17   (+ 20 3)  )
      (+ 17   23        ) ; intermediate evaluation is not usually displayed
40

REPL> (>  (+ 17  (+ 20 3))  39)
      (>  (+ 17  23      )  39)
      (>  40                39)
T


REPL stands for READ - EVALUATE - PRINT - LOOP

Invented in 1958.       



You're welcome.



Allows interactive development.

SLIME (Superior Lisp Interaction Mode for Emacs) provides a great REPL (and more) in emacs. Sylvester (Sly) is similar.

Emacs is de facto standard for Lisp development.



;;;; This will move fast.

;; Load a library that enables HTTP requests.
|#

;;; Quicklisp is a module that is not installed by default, but is of highest quality, and makes installing further modules painless.
;;; https://www.quicklisp.org/beta/#installation

;;; DRAKMA is a module that provides http-request. Vanilla version retrieves and returns response to a request.
(ql:quickload :DRAKMA)



;;; defparameter creates a variable and assigns it an initial, and mutable, value.
;;; Special (global) variables are, by convention, given earmuffs, or Mickey Mouse ears.
(defparameter *URI* "http://www.gutenberg.org/files/52263/52263-0.txt") 



#| 
Once set, the NAME of the variable can be entered into the REPL.  It is then evaluated, and the evaluation is the value that was set.

REPL> *URI*
"http://www.gutenberg.org/files/52263/52263-0.txt"
|#



;;; We will use a function provided by DRAKMA to retrieve a response.  Ours we know already is a text.  This will be returned as one LONG string.
;;; note the syntax for naming a function provided by a LOADed library:   name-of-library:function-name 
(defparameter text-as-string (drakma:http-request *URI*))



#|                   THIS IS BACKWARD 

 DANGER! If we evaluated just

(drakma:http-request *URI*)

at the REPL, it would work, but would return a LOOOONG string, and display it (and some other information about the result of the request).  This is a bit much. Since we wrap it with a defparameter, the LOOOONG string is assigned to a variable, and does not need to be shown.

We can test to see if anything happened by applying non-noisy functions to outr new variable.

REPL> (length text-as-string) ; this will be the count of characters
448952 

For strings, the function length returns the count of characters.
|#


#| 
Now that we have a string, we can save it to a file for future use.
We will use a standard macro that is flexible and safe.
|#

;;; defun DEFINES FUNCTIONS.
;;; Syntax:  (defun function-name (parameters)
;;;             body   -- this can be zero or more, potentially nested, symbolic expressions.  The last one evaluated is the return value.
(defun file<-string (file-name cntnt)
  (with-open-file (strm file-name                       ; with-open-file is a macro that takes care uf stuff, like making sure the stream is
			:direction :output              ; always closed after use. Kkeywords are used to name parameters that control 
			:if-exists :supersede           ; behavior with the stream to the file.
			:if-does-not-exist :create
			:element-type 'extended-char
			:external-format :UTF8)
			(write-string cntnt strm)))     ; this is what gets done with the stream (and the file).


(defparameter *LOCAL-FILE-NAME* "nietzsche.txt" )

;;; Save the LOOOONG string returned by the http-request into a file.
(file<-string *LOCAL-FILE-NAME* text-as-string)

(ql:quickload :ALEXANDRIA)

(defparameter huge-string (alexandria:read-file-into-string *LOCAL-FILE-NAME*))


;; (length huge-string)



#| 
Load another external library.  this one has a string splitter.
|#

(ql:quickload :cl-ppcre)

(defun words<-string (str)
  (cl-ppcre:split "\\s+" (string-upcase str)))

(defparameter text-as-words (words<-string huge-string))

;; (length text-as-words)


#| 
NOTE:  These DEFPARAMETERs that we are doing are for our ease of use during development and testing, and will mostly disappear when we build our final functions.
|#



#| 
We need a way to keep track of how many times each word occurs.  A great solution is a hash table. We will use 
-- words as hash-table keys, and 
-- increment the associated value of each key each time we find a copy of a key word.
|#


(defun mk-tally-hashtable ()
  "Makes a hash table, and uses the test 'equal, which is appropriate for strings."
  (make-hash-table :TEST 'equal))

(defparameter tally-ht (mk-tally-hashtable))



#|
We will use LOOP, a macro that iterates over one or more lists or other objects.
|#

(defun tally-words (words tally-table)
  (loop :for word :in words
	:do (if (gethash word tally-table)
		
		;;; When the predicate of an IF is not NIL, the form following the predicate is evaluated, and the next form IS NOT.
	       (incf (gethash word tally-table)) ; it has been found, so it should already have a number as its value.
					
	       ;; When the predicate of an IF     IS     NIL, the form immediately after the predicate is NOT evaluated, and the next one IS.
	       (setf (gethash word tally-table) 1))
	:finally (return tally-table)))



;;; This inflates the tally table with word counts.
(defparameter full-tally (tally-words text-as-words tally-ht))

;; Evaluating the following at the REPL will display info about the table.
;; full-tally

#| 
We need to create a list, to be sorted (but not yet), from the contents of the hash table. We will create a list of pairs (lists) with values like
(count word)
|#
;;; Extract a list of ordered key-value pairs from a hash-table. We want, in this case, ((v k)*)
(defun list<-hashtable (ht)
  (loop 
     :for key :being the hash-keys :of ht
     :for value :being the hash-values :of ht
     :collect (list value key)))

(defparameter word-list (list<-hashtable full-tally))

#| 
This list is not yet sorted by count.
|#

(subseq word-list 0 10) ; returns the first ten unsorted (count word) pairs.


(defun sort-pairs (list-of-pairs)
  ;; Sorting a list          using greater-than, and comparing first elements.
  (sort        list-of-pairs       #'>               :key    #'first)) ; HEY!  Syntax for grabbing functions by name is #'foo
  

(defparameter word-list-sorted-by-count (sort-pairs word-list))




#| 
Now we will grab the ten most frequent, and the ten rarest, words. 
NOTE:  Very many will actually occur only once, so which ten we choose will depend on the order they were extracted from the hashtable.
|#

;;; This returns two lists of count-word pairs.
(defun get-freqs-and-rares (lst)
  (list (subseq lst 0 10)
	(subseq (reverse lst) 0 10)))

(defparameter f-and-r (get-freqs-and-rares word-list-sorted-by-count))


#| We need to define a function to take two lists (of lists) and format and display the information we want.

|#

(defun output-lists (freqs-and-rares) ; input will be a list of lists.
  ;; DESTURCTURING-BIND evaluates assigns to symbols in a first struture (the FIRST form after "destructuring-bind")
  ;; corresponding values in a SECOND, isomorphic structure.
  ;; This second structure is the result of evaluating the SECOND form after "desctructuring-bind".
  (destructuring-bind (freqs rares) freqs-and-rares ; "freqs-and-rares" evaluates to a list of two lists of pairs.
    ;; FORMAT is an extremely powerful macro that uses instructions embedded in a control-string to produce text based on data.
    ;; Controls are tildes followed by characters.
    ;; ~a         means "one datum, presented 'aesthetically' (human-friendly)".
    ;; ~{ and ~}  start and stop iteration over data within a list. These can be layered.
    ;; ~%         emits a NEWLINE.
    ;; Other text in the string is returned without change.
    ;; format and return w/o sending to a stream a string constructed with this control string                                using these data
    (format   nil                                "FREQUENT:~%~{~{COUNT: ~a WORD: ~a~%~}~}RARE:~%~{~{COUNT: ~a WORD: ~a~%~}~}" freqs rares)))
  

;;;; Now to put it all together!

;;; This version will start with the saved file.
(defun report<-file (filename)
  (let ((tally-ht (mk-tally-hashtable)))
    (output-lists
     (get-freqs-and-rares
      (sort-pairs
       (list<-hashtable
	(tally-words (words<-string (alexandria:read-file-into-string *LOCAL-FILE-NAME*))

		     tally-ht)))))))

(report<-file *LOCAL-FILE-NAME*)


;;; This version will start with the URI.
(defun report<-URI (URI)
  (let ((tally-ht (mk-tally-hashtable)))
    (output-lists
     (get-freqs-and-rares
      (sort-pairs
       (list<-hashtable
	(tally-words (words<-string (drakma:http-request URI))
		     tally-ht)))))))
rgument about the differences between men and women; the encounter became a parable for the mutual incomprehension of the alt-right and metropolitan bien-pensants.


#|
Want to read more?
Peter Seibel's 'Practical Common Lisp' - http://www.gigamonkeys.com/book/
Paul Graham's 'On Lisp' - http://www.paulgraham.com/onlisp.html


#| 
Potential improvements:
- Abstract getting of words (parameterize source)
- Parameterize numbers of words returned
- Compose all functions from getting of words through selection of report set
- Abstract output (parameterize for file or standard output).

A call would look like
(do-report :FILENAME "an-output-file.txt" :SOURCE-TYPE :URI *URI* :FREQS 10 :RARES 100)
|#
