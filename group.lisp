(in-package #:tca)

;;; Modellierung von Gruppenelementen

;;; Inverse von Erzeugern sind von der Form ('- . a)

(defun i-p (x)
  (if (and (consp x) (eq '- (car x)))
      (cdr x)))

(defmacro def-group-op (name (var) symbol inverse list)
  `(defun ,name (,var)
     (cond ((symbolp ,var) ,symbol)
           ((i-p ,var) ,inverse)
           ((listp x) ,list))))

(def-group-op i (x)
  (cons '- x)
  (cdr x)
  (reverse (mapcar #'i x)))

(def-group-op gnorm (x)
  (list x)
  (list x)
  x)

(def-group-op gens (x)
  x
  (cdr x)
  (remove-duplicates (mapcan (compose #'mklist #'gens) x)))

(defun gappend (args)
  (mappend #'gnorm args ))

(defun m (&rest args)
  (greduce (gappend args)))

(defun greduce (glist)
  (labels ((reductor (start current rest)
             (cond ((and (null current) (null rest))
                    ;; Beide leer -> alle Elemente angeguckt
                    (nreverse start))
                   ((null current)
                    ;; schaue nächsten an
                    (reductor start (car rest) (cdr rest)))
                   (t
                    (let ((next (car rest)))
                      (if (equal (i current) next)
                          ;; Es wurde gekürzt: Teste, ob weiter
                          ;; gekürzt werden kann:
                          (reductor (cdr start) (car start) (cdr rest))
                          ;; Sonst versuche, den nächsten zu kürzen
                          (reductor (cons current start) next (cdr rest))))))))
    (reductor nil (car glist) (cdr glist))))

