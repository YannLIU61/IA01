
(load "parsing-module-symbol.lisp")


(defun get-operator (source)
        ;(translationTable '(\! \" \# \$ \% \& \' \( \) \* \+ \, \- \. \/ \: \; \< \= \> \? \@ \[  \\ \] \^ \_ \` \{ \| \} \~))
        ; parse source
         (let ((characters '()) (f (open source :direction :input)) (num-lines 0))
        ; parse source
        (loop
            (let* ((line (read-line f NIL)) (trimed-line (string-trim " " line) (new-trimed-line '() ) ))
                (if line
                    (if (> (length trimed-line) 0)
                        (dotimes (x (length trimed-line)) 
                            (if x ; 相当于index
                                (let ((ch (intern (subseq trimed-line x (+ 1 x)))))
                                    
                                    (if !(assoc ch translationTable)
                                       ;;(substitute #\  #\o "Groucho Marx") 
                                        (replace trimed-line " " :start1 x :end1 (+ 1 x) :start2 0)  ;把当前位置字符替换
                                        (push (cons ch 1) characters)
                                    )
                                )  
                            )
                        )
                       (setf num-lines (+ num-lines 1)) 
                    )
                    (return)
                )
            )
        )
        characters
    )
  
)

(defun testreplace (string)
    (print string)
    ;(setq translationTable '( \! \" \# \$ \% \& \' \( \) \* \+ \, \- \. \/ \: \; \< \= \> \? \@ \[  \\ \] \^ \_ \` \{ \| \} \~))
    (setq translationTable '(b d ))
    (dotimes (x (length string)) 
            (if x ; 相当于index
                (let ((ch  (subseq string x (+ 1 x))))
                    (print x)
                    (print ch)
                    (if (member ch translationTable)
                       (setf string (replace string " " :start1 x :end1 (+ 1 x) :start2 0))  ;用空格把当前位置字符替换
                       (print "NO change")
                    )
                )  
            )
        )
    (print string)
    (print (car translationTable))


)
(testreplace "test,bca!vua*bk$")