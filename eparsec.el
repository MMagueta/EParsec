;;; eparsec.el --- Library for monadic parser combinators in Emacs Lisp.

;; Version: 1.0
;;; Commentary:
;;;
;;; Code:

;; let pchar (charToMatch,str) =
;;   if String.IsNullOrEmpty(str) then
;;     Failure "No more input"
;;   else
;;     let first = str.[0]
;;     if first = charToMatch then
;;       let remaining = str.[1..]
;;       Success (charToMatch,remaining)
;;     else
;;       let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
;;       Failure msg

(defmacro comment (&rest expressions)
  '())

(defun pchar (char-to-match input)
  (if (string-empty-p input)
      ()
    ()))

(provide 'eparsec)


(let* ((x 1)
       (y x)
       (z y))
  (+ x y z))

(defun monad/identity-bind (m f)
  (funcall f m))

(defun monad/identity-return (m)
  m)

(defun get-first-label (pair-list)
  ":PAIR-LIST -> symbol * object -> symbol."
  (car (car pair-list)))

(defun get-first-value (pair-list)
  ":PAIR-LIST -> symbol * object -> object."
  (car (cdr (car pair-list))))

(get-first-value '((x 1) (y 2)))
(get-first-label '((x 1) (y 2)))

(cl-defstruct result success failure)
(cl-deftype just value ())
(just "123")
(result-success (make-result :success "abc"))

(defun monad/do (bindings-list return-expression)
  (if bindings-list
      `(monad/identity-bind
	,(get-first-value bindings-list)
	#'(lambda (,(get-first-label bindings-list))
	    ,(monad/do (cdr bindings-list) return-expression)))
    `(monad/identity-return ,return-expression)))

(eval (monad/do '((x 1)
		  (y 2))
		`(+ x y)))

;;; eparsec.el ends here
