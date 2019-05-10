;; <html>
;; <header>
;;     <title> Ma page </title>
;; </header>
;; <body>
;;     <h1> Un titre </h1>
;;     <p> Soror et aemula Romae </p>
;; </body>
;; </html>

(setq l 
  '(
        html
            (header
                (title ("Ma page") )     
            )
            (body
                ( 
                (h1 ("Un titre") )
                (p ("Soror et aemula Romae") )
                )     
            )
    )
)

(print l)
;;(HTML (HEADER (TITLE ("Ma page") ) )   (BODY ( ( H1 ("Un titre") )  (P ("Soror et aemula Romae") ) ) )     ) 
;; (defun make-html (l)
;;     (setq head '())
;;         (loop for x in l
;;             do (cond ( 
;;                         ( (not (listp x)) (append head (list x))  (concatenate 'string "<" x ">" )       )
;;                         ((listp x)      ( make-html x) )
;;                      ) 
;;                )    
;;         )
;;     (print string)
    
;; )

(defun make_html (l)

    (let ((balise (car l)))
        (format t "<~A>" balise)
        (if (stringp (cadr l))
            (format t "~A" (cadr l))
            (make_html (cdr l))
    
        )

        (format t "</~A>" balise)
    )


)
;; (defun make_html (l nom)

;;     (with-open-file( file nom :if-dose-not-exist :creat
;;                             :direction :output)
;;         (make-html l file)                  
;;     )
;; )

