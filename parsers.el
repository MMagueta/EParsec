;;; Code:

;; let pchar (charToMatch,str) =
;;   if String.IsNullOrEmpty(str) then
;;     let msg = "No more input"
;;     (msg,"")
;;   else
;;     let first = str.[0]
;;     if first = charToMatch then
;;       let remaining = str.[1..]
;;       let msg = sprintf "Found %c" charToMatch
;;       (msg,remaining)
;;     else
;;       let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
;;       (msg,str)

(defun parseA (input)
  "Parse 'A' INPUT: string."
  (if (or (eq nil input) (string-empty-p input))
      "Empty"
    "Not empty"))

(parseA "")

;;; parsers.el ends here
