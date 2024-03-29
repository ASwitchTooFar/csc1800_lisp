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
    (SETF ancestorlist (LIST))

    (ancestorsb name tree)
    ))


;;; Handles type checking, etc for descendants.
(DEFUN descendants (name tree)
  "Returns a list of names (strings or symbols) of all the descendants of NAME in TREE.
Does dynamic type checking to see whether all the arguments are of the correct types."
  (WHEN (NOT (OR (SYMBOLP name) (STRINGP name)))
    (ERROR "DESCENDANTS called with NAME (~A) that is not a SYMBOL or STRING." name))
  (WHEN (NOT (HASH-TABLE-P tree))
    (ERROR "DESCENDANTS called with TREE (~A) that is not a HASH-TABLE." tree))
  (WHEN (person-exists name tree)
    (SETF descendantlist (LIST))

    (descendantsb name tree)
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

  (LET* ((p (lookup-person name tree)))

    (IF (person-parent1 p)
      (SETF (person-children (lookup-person (person-parent1 p) tree))
        (APPEND (person-children (lookup-person (person-parent1 p) tree)) (LIST name)))
      )
    (IF (person-parent2 p)
      (SETF (person-children (lookup-person (person-parent2 p) tree))
        (APPEND (person-children (lookup-person (person-parent2 p) tree)) (LIST name)))
      )
    )

  name
  )


(DEFUN printlist (personlist)
  "A helper function for printing out lists. Removes duplicates and sorts before
printing."
  (FORMAT T "~{~a~%~}" (SORT (REMOVE-DUPLICATES personlist :test #'equal) #'string-lessp))
  )



;;This function needs to be defined by your team.
(DEFUN ancestorsb (name tree)
  "A helper function for the ANCESTORS function.
Returns a list of names (strings or symbols) of all the ancestors of NAME in TREE.
Does not implicitly remove any duplicated names! Does not sort names!"
  (LET* ((p (lookup-person name tree))
         (parent1 (person-parent1 p))
         (parent2 (person-parent2 p)))

    (IF parent1
      (IF (not (MEMBER parent1 ancestorlist :test #'equal))
        (PROGN
          (SETF ancestorlist (APPEND ancestorlist (LIST parent1)))
          (ancestorsb parent1 tree)
          )
        )
      )

    (IF parent2
      (IF (not (MEMBER parent2 ancestorlist :test #'equal))
        (PROGN
          (SETF ancestorlist (APPEND ancestorlist (LIST parent2)))
          (ancestorsb parent2 tree)
          )
        )
      )

    ancestorlist
    )
  )



(DEFUN children (name tree)
  "Returns a list of the children of NAME in TREE."
  (LET* ((p (lookup-person name tree)))
    (person-children p)
    )
  )


;;This function needs to be defined by your team.
(DEFUN descendantsb (name tree)
  "A helper function for the DESCENDANTS function.
Returns a list of names (strings or symbols) of all the descendants of NAME in TREE.
Does not implicitly remove any duplicated names! Does not sort names!"
  (LOOP for child in (children name tree) doing
    (IF (not (MEMBER child descendantlist :test #'equal))
      (PROGN
        (SETF descendantlist (APPEND descendantlist (LIST child)))
        (descendantsb child tree)
        )
      )
    )

  descendantlist
  )

(DEFUN siblings (name tree)
  "Returns a list of names (strings or symbols) of all the siblings of NAME in TREE.
Does dynamic type checking to see whether all the arguments are of the correct types."
  (WHEN (NOT (OR (SYMBOLP name) (STRINGP name)))
    (ERROR "SIBLINGS called with NAME (~A) that is not a SYMBOL or STRING." name))
  (WHEN (NOT (HASH-TABLE-P tree))
    (ERROR "SIBLINGS called with TREE (~A) that is not a HASH-TABLE." tree))
  (WHEN (person-exists name tree)    
    (LET* ((p (lookup-person name tree))
           (siblinglist (LIST)))

      (IF (person-parent1 p)
        (SETF siblinglist (APPEND siblinglist (children (person-parent1 p) tree)))
        )
      (IF (person-parent2 p)
        (SETF siblinglist (APPEND siblinglist (children (person-parent2 p) tree)))
        )

      (SETF siblinglist (REMOVE name siblinglist :test #'equal))

      siblinglist
      )
    )
  )


(DEFUN allpeople (tree)
  "Returns a list of all people in the tree."
  (LET* ((people (LIST)))
    (maphash #'(lambda (k v) (SETF people (APPEND people (LIST k)))) tree)

    people
    )
  )


(DEFUN unrelated (name tree)
  "Returns a list of names (strings or symbols) of all the people unrelated to NAME in TREE.
Does dynamic type checking to see whether all the arguments are of the correct types."
  (WHEN (NOT (OR (SYMBOLP name) (STRINGP name)))
    (ERROR "UNRELATED called with NAME (~A) that is not a SYMBOL or STRING." name))
  (WHEN (NOT (HASH-TABLE-P tree))
    (ERROR "UNRELATED called with TREE (~A) that is not a HASH-TABLE." tree))
  (WHEN (person-exists name tree)
    (LET* ((local_ancestors (ancestors name tree))
           (local_related (ancestors name tree))
           (local_unrelated (allpeople tree)))

      (LOOP for ancestor in local_ancestors doing
        (SETF local_related (APPEND local_related (descendants ancestor tree)))
        )

      (SETF local_related (REMOVE-DUPLICATES local_related :test #'equal))

      (LOOP for relative in local_related doing
        (SETF local_unrelated (REMOVE relative local_unrelated :test #'equal))
        )

      local_unrelated
      )
    )
  )

(DEFSTRUCT generation
  (name nil)
  (generation 0))

(DEFUN addancestor (name struct ancestorTree)
  (SETF (GETHASH name ancestorTree) struct))

(DEFUN ancweight (ancestorTree p gen tree)
  (COND ((and (person-exists p tree)
            (person-parent 1 (lookup-person p tree)))
       (addancestor (person-parent1 (lookup-person p tree))
                     (make-generation :name (person-parent1 (lookup-person p tree))
                                      :generation (+ gen 1))
                     ancestorTree)
       (addancestor (person-parent2 (lookup-person p tree))
                    (make-generation :name (person-parent2 (lookup-person p tree))
                                     :generation (+ gen 1))
                    ancestorTree)
       (ancweight ancestorTree (person-parent1 (lookup-person p tree)) (+ gen 1) tree)
       (ancweight ancestorTree (person-parent2 (lookup-person p tree)) (+ gen 1) tree)
       ancestorTree))
)

(DEFUN minof (a)
  (apply 'min a)
)

(DEFUN arecousins (name1 name2 degree tree)
  (COND ((or (string= name1 name2)
             (or (member name1 (ancestorsb name2 tree) :test #'equal)
                 (member name2 (ancestorsb name1 tree) :test #'equal)
             (not (person-exists name1 tree))
             (not (person-exists name2 tree))
             (not (person-parent1 (lookup-person name1 tree)))
             (not (person-parent1 (lookup-person name2 tree))))
         nil)
         (t
          (LET* ((ancestors1 (MAKE-HASH-TABLE :size 1000 :test #'equal))
                 (ancestors2 (MAKE-HASH-TABLE :size 1000 :test #'equal))
                 (weight1 (ancweight ancestors1 name1 0 tree))
                 (weight2 (ancweight ancestors2 name2 0 tree))
                 (mingen nil)
                 (list1 nil)
                 (list2 nil))
            (maphash #'(lambda (k v) (SETF list1 (append list1 (list k)))) weight1)
            (maphash #'(lambda (k v) (SETF list1 (append list2 (list k)))) weight2)
            (LOOP for person in list doing 
                  (SETF mingen
                        (append mingen
                                (list (min (ancgen (lookup-person person weight1))
                                           (ancgen (lookup-person person weight2)))))))
            (and (> (list-length mingen) 0)
                 (= (- (minof mingen) 1) (parse-integer degree)))))))
)

   
(DEFUN cousins (degree name tree)
  "Returns a list of names (strings or symbols) of all the cousins of NAME of the
specified degree in TREE. Does dynamic type checking to see whether all the arguments
are of the correct types."
  (SETF cousins nil)
  (SETF people (allPeople tree))
  (LOOP for person in people doing 
        (IF (arecousins name person degree tree)
            (SETF cousins (append cousins (list person)))))
  cousins
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
  (FORMAT T "X ~{~a ~}~%" linelist)

  (IF (STRING= (nth 1 linelist) "child")
    (IF (person-exists (nth 2 linelist) tree)
      (IF (MEMBER (nth 0 linelist) (children (nth 2 linelist) tree) :test #'equal)
        (FORMAT T "Yes~%")
        (FORMAT T "No~%")
        )
      (FORMAT T "~a does not exist in the family tree.~%" (nth 2 linelist))
      )
    )

  (IF (STRING= (nth 1 linelist) "sibling")
    (IF (person-exists (nth 2 linelist) tree)
      (IF (MEMBER (nth 0 linelist) (siblings (nth 2 linelist) tree) :test #'equal)
        (FORMAT T "Yes~%")
        (FORMAT T "No~%")
        )
      (FORMAT T "~a does not exist in the family tree.~%" (nth 2 linelist))
      )
    )

  (IF (STRING= (nth 1 linelist) "ancestor")
    (IF (person-exists (nth 2 linelist) tree)
      (IF (MEMBER (nth 0 linelist) (ancestors (nth 2 linelist) tree) :test #'equal)
        (FORMAT T "Yes~%")
        (FORMAT T "No~%")
        )
      (FORMAT T "~a does not exist in the family tree.~%" (nth 2 linelist))
      )
    )

  (IF (STRING= (nth 1 linelist) "cousin")
    (IF (person-exists (nth 3 linelist) tree)
      (IF (MEMBER (nth 0 linelist) (cousins (nth 2 linelist) (nth 3 linelist) tree) :test #'equal)
        (FORMAT T "Yes~%")
        (FORMAT T "No~%")
        )
      (FORMAT T "~a does not exist in the family tree.~%" (nth 2 linelist))
      )
    )

  (IF (STRING= (nth 1 linelist) "unrelated")
    (IF (person-exists (nth 2 linelist) tree)
      (IF (MEMBER (nth 0 linelist) (unrelated (nth 2 linelist) tree) :test #'equal)
        (FORMAT T "Yes~%")
        (FORMAT T "No~%")
        )
      (FORMAT T "~a does not exist in the family tree.~%" (nth 2 linelist))
      )
    )

  (TERPRI)
  )


;;NOTE: This function needs to be defined by team
(DEFUN handle-W (linelist tree)
  "LINELIST is a LIST of strings. TREE is a hash-table."
  (FORMAT T "W ~{~a ~}~%" linelist)

  (IF (STRING= (nth 0 linelist) "child")
    (IF (person-exists (nth 1 linelist) tree)
      (printlist (children (nth 1 linelist) tree))
      (FORMAT T "~a does not exist in the family tree.~%" (nth 1 linelist))
      )
    )

  (IF (STRING= (nth 0 linelist) "sibling")
    (IF (person-exists (nth 1 linelist) tree)
      (printlist (siblings (nth 1 linelist) tree))
      (FORMAT T "~a does not exist in the family tree.~%" (nth 1 linelist))
      )
    )

  (IF (STRING= (nth 0 linelist) "ancestor")
    (IF (person-exists (nth 1 linelist) tree)
      (printlist (ancestors (nth 1 linelist) tree))
      (FORMAT T "~a does not exist in the family tree.~%" (nth 1 linelist))
      )
    )

  (IF (STRING= (nth 0 linelist) "cousin")
    (IF (person-exists (nth 2 linelist) tree)
      (printlist (cousins (nth 1 linelist) (nth 2 linelist) tree))
      (FORMAT T "~a does not exist in the family tree.~%" (nth 2 linelist))
      )
    )

  (IF (STRING= (nth 0 linelist) "unrelated")
    (IF (person-exists (nth 1 linelist) tree)
      (printlist (unrelated (nth 1 linelist) tree))
      (FORMAT T "~a does not exist in the family tree.~%" (nth 1 linelist))
      )
    )

  (TERPRI)
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
    (COND
      ((STRING= (nth 0 line-items) "E")
        (handle-E (REST line-items) tree)
        )
      ((STRING= (nth 0 line-items) "W")
        (handle-W (REST line-items) tree)
        )
      ((STRING= (nth 0 line-items) "X")
        (handle-X (REST line-items) tree)
        )
      (t
        (RETURN nil) ; end of file reached
        )
      )
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

    (handle-W '("child" "Fred") tree)
    (handle-W '("sibling" "Karen") tree)
    (handle-W '("ancestor" "Alex") tree)
    (handle-W '("cousin" "Fred") tree)
    (handle-W '("sibling" "Arthur") tree) ;;Intentional nonexistant person.

    (handle-X '("Fred" "child" "Zenobia") tree)
    (handle-X '("Mary" "child" "Zenobia") tree)
    (handle-X '("Karen" "sibling" "Benjamin") tree)
    (handle-X '("Alex" "sibling" "Benjamin") tree)
    (handle-X '("Brenda" "ancestor" "Fred") tree)
    (handle-X '("Zebulon" "ancestor" "Brenda") tree)
    (handle-X '("Karen" "unrelated" "Mary") tree)
    (handle-X '("Bill" "unrelated" "Mary") tree)
    (handle-X '("Mary" "unrelated" "Arthur") tree) ;;Intentional nonexistant person.
    )
  )

