;;;; -*- Mode: Lisp; -*- 
;;;; Team Members: Chris Dodd, Farah Zahin, and Tom McDonald.
;;;;
;;;;
;;;; Submission Deadline: Sunday, December 8, 11:59:59pm
;;;; Report Deadline: Monday, December 9, 11:59:59pm
;;;; 
;;;; Please submit your code as a .lisp file in Blackboard.
;;;;
;;;;



;;;HELPFUL TOPLEVEL HINTS:

;;; To run your program, first load it into the LispWorks Editor,
;;; then click on the "Compile Buffer" button on the top of the
;;; Editor window.  Then, in the LISTENER, you can call any of
;;; the functions in the program file.
;;;
;;; If you find yourself in an error, and you want to know how
;;; you got to that point in the program, when the textual debugger
;;; message appears, you can click on the "Debug" button at the
;;; top of the LISTENER window.  You'll get a GUI debugger that
;;; will show you things like the call stack, the values of the
;;; local variables, and, if you click on a call stack entry,
;;; if the entry corresponds to a function in your program, the
;;; Editor will immediately jump you to the line of code where
;;; the error occured.  Ask in class if you have trouble with
;;; this neat feature. 


;;;HELPFUL PROGRAMMING HINTS:
;;; To create a person structure, use the automatically-generated
;;; function "make-person" as follows:
;;;
;;; (make-person :name xxx :parent1 yyy :parent2 zzz)
;;;
;;; where "xxx" is the string or symbol (or a variable holding it)
;;; for the name of the person, "yyy" is the string or symbol for
;;; the name of the person's first parent, and "zzz" is of course
;;; the name of the person's second parent.
;;;
;;; for example, to store a new person in a variable p, use this:
;;;
;;; (SETF p (make-person :name "Barbara" :parent1 "Fred" :parent2 "Carol"))
;;;
;;;

;;; The DEFSTRUCT function tells Lisp to autmatically create
;;; getter functions for each slot.  Their names are based on
;;; the names of the slots:
;;;
;;;  "person-name" will get the value stored in the NAME slot
;;;  "person-parent1" will get the value in the PARENT1 slot
;;;

;;; The LOOP function (macro) is used to iterate in many ways.
;;; Here are some examples:
;;;
;;; (LET ((newlist nil)
;;;       (mylist (LIST 1 2 3 4 5 6 7 8)))
;;;   (LOOP for i in mylist DOING
;;;     (SETF newlist (APPEND newlist (LIST (+ i 1)))))
;;;   newlist)
;;;
;;;  The above will make a new list that contains
;;;  numbers that are one more than their corresponding
;;;  elements in mylist.  Notice that the new sum is added
;;;  at the END of the growing new list!
;;;  This could also be done more elegantly in Lisp using
;;;  a nameless lambda function
;;;
;;;  (LET ((mylist (LIST 1 2 3 4 5 6 7 8)))
;;;    (MAPCAR #'(lambda (x) (+ x 1)) mylist))
;;;
;;; MAPCAR applies its first argument (a function) to each
;;; element in the second argument (a list), and collects
;;; the results of all the function calls into a new list
;;; and returns that list.
;;;
;;; Here is another LOOP example that does the same thing:
;;;
;;; (LET ((mylist (LIST 1 2 3 4 5 6 7 8)))
;;;  (LOOP for x in mylist collecting
;;;     (+ x 1)))
;;;


;;;-------------------------------
;;;PROJECT CODE STARTS HERE.
;;;-------------------------------


(DEFSTRUCT (person
    (:print-function print-person))
    (parent1 NIL) ; a symbol or string or NIL
    (parent2 NIL) ; a symbol or string or NIL
    (name NIL)    ; a symbol or string or NIL
    (children NIL) ; a symbol or list or NIL
    )

;;If you want to add more slots to the person
;;structure (say, children or spouse), use
;;the same syntax as you see above for the slots
;;to add them to the above definition.
;;It is likely that if you add a "children" slot,
;;it will hold a list or array of children names
;;rather than a single atom.


;;NOTE: This function is complete, no need to change it unless you
;;want to update it to show other slots you add to the person struct
;;definition.
(DEFUN print-person (item stream depth)
  "A helper function for Lispworks to be able to show you what is
in a person structure concisely."
    (DECLARE (IGNORE depth))
    (FORMAT stream "#<P name:~S p1:~S p2:~S children:~S>"
        (person-name item)
        (person-parent1 item)
        (person-parent2 item)
        (person-children item)
        )
    item)


;;;NOTE: This function is complete. No need to change it.
(DEFUN lookup-person (name tree)
  "Returns a PERSON structure corresponding to the key NAME in the hashtable TREE.
NAME must be a STRING or a SYMBOL. If there is no one in the tree with the name
in NAME, returns NIL."
  (GETHASH name tree nil))


;;;NOTE: This function is complete. No need to change it.
(DEFUN person-exists (name tree)
  "Returns T when the key NAME has an actual person struct stored in TREE.
Returns NIL (false) otherwise."
  (WHEN (lookup-person name tree)
    t))


;;;NOTE: This function is complete. No need to change it.
(DEFUN ancestors (name tree)
  "Returns a list of names (strings or symbols) of all the ancestors of NAME in TREE. 
Does dynamic type checking to see whether all the arguments are of the correct types."
  (WHEN (NOT (OR (SYMBOLP name) (STRINGP name)))
    (ERROR "ANCESTORS called with NAME (~A) that is not a SYMBOL or STRING." name))
  (WHEN (NOT (HASH-TABLE-P tree))
    (ERROR "ANCESTORS called with TREE (~A) that is not a HASH-TABLE." tree))
  (WHEN (person-exists name tree)
    (SORT (REMOVE-DUPLICATES(ancestorsb name tree)) #'string-lessp)
    ))


;;; Handles type checking, etc for descendants.
(DEFUN descendants (name tree)
  "Returns a list of names (strings or symbols) of all the descendants of NAME in TREE. 
Does dynamic type checking to see whether all the arguments are of the correct types."
  (WHEN (NOT (OR (SYMBOLP name) (STRINGP name)))
    (ERROR "ANCESTORS called with NAME (~A) that is not a SYMBOL or STRING." name))
  (WHEN (NOT (HASH-TABLE-P tree))
    (ERROR "ANCESTORS called with TREE (~A) that is not a HASH-TABLE." tree))
  (WHEN (person-exists name tree)
    (SORT (REMOVE-DUPLICATES(descendantsb name tree)) #'string-lessp)
    ))




;;;------------------------------------------------
;;; TEAM SHOULD PUT ALL NEW HELPER FUNCTION
;;; DEFINITIONS BELOW THIS COMMENT
;;;------------------------------------------------ 




(DEFUN add-person (name struct tree)
  "This should enter the person structure in STRUCT into
the hashtable in TREE with the key in NAME."
  (WHEN (NOT (HASH-TABLE-P tree))
    (ERROR "STORE-PERSON called with TREE (~A) that is not a HASH-TABLE." tree))
  (WHEN (NOT (person-p struct))
    (ERROR "STORE-PERSON called with STRUCT (~A) that is not a PERSON structure." struct))
  (WHEN (NOT (OR (SYMBOLP name) (STRINGP name)))
    (ERROR "STORE-PERSON called with NAME (~A) that is not a SYMBOL or a STRING." name))
  ;; NOTE1: TEAMS NEED TO WRITE THE NEXT LINE.
  ;;        Hint: a "setf" expression.
  (SETF (GETHASH name tree) struct)

  ;; Add the person to the parents' child lists, assuming the parents exist.
  (IF (person-parent1 (GETHASH name tree))
      (IF (person-children (GETHASH (person-parent1 (GETHASH name tree)) tree))
        (SETF
            (person-children (GETHASH (person-parent1 (GETHASH name tree)) tree))
            (NCONC (person-children (GETHASH (person-parent1 (GETHASH name tree)) tree)) (LIST name)))
        (SETF (person-children (GETHASH (person-parent1 (GETHASH name tree)) tree)) (LIST name))
        )
  )
  (IF (person-parent2 (GETHASH name tree))
      (IF (person-children (GETHASH (person-parent2 (GETHASH name tree)) tree))
        (SETF
            (person-children (GETHASH (person-parent2 (GETHASH name tree)) tree))
            (NCONC (person-children (GETHASH (person-parent2 (GETHASH name tree)) tree)) (LIST name)))
        (SETF (person-children (GETHASH (person-parent2 (GETHASH name tree)) tree)) (LIST name))
        )
  )

  ;; NOTE2: Leave this last line as "name" so
  ;;        that the name argument is what is
  ;;        returned by this function.
  name
  )



;;This function needs to be defined by your team.
(DEFUN ancestorsb (name tree)
  "A helper function for the ANCESTORS function. 
Returns a list of names (strings or symbols) of all the ancestors of NAME in TREE. 
Does not remove any duplicated names! Does not sort names!"
  (LET* ((p (lookup-person name tree))
         (parent1 (person-parent1 p))
         (parent2 (person-parent2 p))
         ancestorslist (LIST))

    ;; SKELETON CODE!

    (IF parent1
      ()
      )

    (IF parent1
      ()
      )

    ancestorslist
    )
  )


;;This function needs to be defined by your team.
(DEFUN descendantsb (name tree)
  "A helper function for the DESCENDANTS function. 
Returns a list of names (strings or symbols) of all the descendants of NAME in TREE. 
Does not remove any duplicated names! Does not sort names!"
  (LET* ((p (lookup-person name tree))
         (children (person-children p))
         descendantslist (LIST))
  
    ;; SKELETON CODE!

    (IF children
      ()
      )

    descendantslist
    )
  )


;;NOTE: This function needs to be defined by team
(DEFUN handle-E (linelist tree)
  "LINELIST is a LIST of strings. TREE is a hash-table."
    ;; If necessary, creates the first two people in the "marriage". If length is three,
    ;; also creates the child with the first two people as its parents.
    (IF (not (person-exists (nth 0 linelist) tree))
        (add-person (nth 0 linelist) (make-person :name (nth 0 linelist) :parent1 nil :parent2 nil) tree)
        )
    (IF (not (person-exists (nth 1 linelist) tree))
        (add-person (nth 1 linelist) (make-person :name (nth 1 linelist) :parent1 nil :parent2 nil) tree)
        )
    (IF (EQL (LIST-LENGTH linelist) 3)
        (add-person (nth 2 linelist) (
            make-person :name (nth 2 linelist) :parent1 (nth 0 linelist) :parent2 (nth 1 linelist)) tree)
        )
  )


;;NOTE: This function needs to be defined by team
(DEFUN handle-X (linelist tree)
  "LINELIST is a LIST of strings. TREE is a hash-table."
  (CASE (nth 1 linelist)
    ("child" ())
    ("sibling" ())
    ("ancestor" ())
    ("cousin" ())
    ("unrelated" ())
    )


;;NOTE: This function needs to be defined by team
(DEFUN handle-W (linelist tree)
  "LINELIST is a LIST of strings. TREE is a hash-table."
  (CASE (nth 0 linelist)
    ("child" ())
    ("sibling" ())
    ("ancestor" ())
    ("cousin" ())
    ("unrelated" ())
    )




;;;------------------------------------------------
;;; TEAM SHOULD PUT ALL NEW HELPER FUNCTION
;;; DEFINITIONS ABOVE THIS COMMENT
;;;------------------------------------------------ 




;;;THE TOP LEVEL FUNCTION OF THE WHOLE PROGRAM
;;NOTE: This function is complete.
(DEFUN family (stream)
  "This is the top-level function for the whole Lisp program. Reads
each line from the file opened in STREAM."
  (LET ((tree (MAKE-HASH-TABLE :size 1000 :test #'equal))
        (line-items (SPLIT-SEQUENCE " " (READ-LINE stream nil "") :test #'equal)))
  (LOOP
    (CASE (FIRST line-items)
      ("E" (handle-E (REST line-items) tree))
      ("W" (handle-W (REST line-items) tree))
      ("X" (handle-X (REST line-items) tree))
      (t (RETURN nil))) ; end of file reached
    (SETF line-items (SPLIT-SEQUENCE " " (READ-LINE stream nil "") :test #'equal)))
  )
)


;;How Dr. Klassner and Jenish will test your code in the Listener:
;;
;;(family (open "~/Documents/School/CSC\ 1800-002/Projects/Project3/tests/test.txt"))
;;
;; NOTE: The FilePath for OPEN is just an example. 
;; Use your own laptop directory to where you keep
;; your project's test files.


;;;A helpful tester function for debugging your tree.
(DEFUN test-tree ()
  (LET ((tree (MAKE-HASH-TABLE :size 1000 :test #'equal)))
    (handle-E '("Zebulon" "Zenobia") tree)
    (handle-E '("Zebulon" "Zenobia" "Mary") tree)
    (handle-E '("Fred" "Mary" "Karen") tree)
    (handle-E '("Fred" "Mary" "Kelly") tree)
    (handle-E '("Fred" "Mary" "Brenda") tree)
    (handle-E '("Karen" "Bill" "Benjamin") tree)
    (handle-E '("Karen" "Bill" "Alex") tree)
    ;; if "add-person" is defined correctly and "ancestorsb" is defined correctly,
    ;; this last call should make test-tree return a list containing the following
    ;; in some arbitrary order when you call test-tree in the Listener:
    ;;   ("Karen" "Bill" "Fred" "Mary" "Zebulon" "Zenobia")
    ;; (ancestors "Alex" tree)
    ))
