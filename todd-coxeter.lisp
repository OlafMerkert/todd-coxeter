(in-package #:tca)

(defparameter *verbose* t)

(defmacro verbose (&body body)
  `(when *verbose*
     ,@(mapcar (lambda (x)
                 (if (stringp x)
                     `(format t ,(concatenate 'string x "~%"))
                     x))
               body)))

(defclass tca ()
  ;; Algorithm input
  ((generators :reader generators
               :initarg  :generators
               :initform nil)
   (relations :reader relations
              :initarg  :relations
              :initform nil)
   (subgroup :reader subgroup
             :initarg  :subgroup
             :initform nil)
   ;; helper data
   (header :reader header)
   (width :reader width)
   (relation-offsets :reader relation-offsets)
   ;; state
   (class-number :initform 0)
   (ident-queue :initform nil)
   ;; results
   (mappings :reader generator-mappings :initform (make-hash-table))
   (classes  :reader classes :initform nil)
   ))

(defmethod initialize-instance :after ((tca tca) &key)
  ;; Erzeuge die Kopfzeile aus Abbildungen
  (with-slots (relations subgroup generators
                         mappings header width)
      tca
   (let ((relations-and-subgroup
          (gappend (append relations subgroup))))
     (verbose (format t "~{~A~^  ~}~%" relations-and-subgroup))
     ;; Sammle alle Erzeuger, falls nicht bereits angegeben
     (unless generators
       (setf generators (gens relations-and-subgroup)))
     ;; Erzeuge Abbildungen für jeden Erzeuger
     (dolist (g generators)
       (setf (gethash g mappings) (mapping)))
     ;; Finde für alle Kopfzellen die richtige Abbildung:
     (setf header
           (map 'vector (lambda (x)
                          (aif (i-p x)
                               (inverse (gethash it mappings))
                               (gethash x mappings)))
                relations-and-subgroup)
           width (length header))))
  (with-slots (relations relation-offsets) tca
    (setf relation-offsets
          (cumulative-sums (map 'vector #'length (cons nil relations))))))

(defmethod subgroup-offsets ((tca tca))
  (with-slots (subgroup relation-offsets) tca
   (cumulative-sums (map 'vector #'length (cons nil subgroup))
                    (alast relation-offsets))))


(defmethod init-class ((tca tca) number)
  (let ((class (make-array (1+ (width tca))
                           :initial-element nil)))
    (loop for i across (relation-offsets tca)
         do (setf (aref class i) number))
    (when (eql number 1)
      (loop for i across (subgroup-offsets tca)
           do (setf (aref class i) number)))
    class))

(defmethod fill-class ((tca tca) class)
  ;; Vorwärts füllen
  (loop for g across (header tca)
     and i from 0 and j from 1
     for a = (aref class i)
     and b = (aref class j)
     when a do
     (let ((c (image g a)))
       (cond ((null c)) ; no image - do nothing
             ((null b)
              (setf (aref class j) c)
              (verbose
                "Einsetzen: Vorwärts"
                (print-table tca)))
             ((not (eql b c))
              (identify tca b c)))))
  ;; Rückwärts füllen
  (loop for j from (1- (width tca)) downto 0
     and i from (width tca) downto 1
     for g = (aref (header tca) j)
     for a = (aref class i)
     and b = (aref class j)
     when a do
     (let ((c (preimage g a)))
       (cond ((null c)) ; no preimage - do nothing
             ((null b)
              (setf (aref class j) c)
              (verbose
                "Einsetzen: Rückwärts"
                (print-table tca)))
             ((not (eql b c))
              (identify tca b c))))))

(defmethod add-class ((tca tca))
  (with-slots (class-number classes) tca
    (let* ((nr (incf class-number))
           (class (init-class tca nr)))
      (setf classes (append1 classes class))
      (verbose (format t "Neue Klasse: ~A~%" nr)
               (print-table tca))
      nr)))

(defmethod identify ((tca tca) a b)
  (with-slots (ident-queue) tca
    (unless (eql a b) (push (cons a b) ident-queue))
    (values)))

(defun clear-ident-queue (tca)
  (with-slots (ident-queue) tca
    (when ident-queue
      (let* ((pair (pop ident-queue))
             (a (car pair))
             (b (cdr pair)))
        (unless (= a b)
          (when (> a b) (rotatef a b))
          (nsubst a b ident-queue)
          (identify! tca a b)))
      (clear-ident-queue tca))))

(defmethod identify! ((tca tca) a b)
  (with-slots (classes mappings) tca
    ;; entferne Klasse
    (setf classes (delete b classes :key (lambda (x) (aref x 0))))
    ;; ersetze Zahlen in Klassen
    (dolist (c classes)
      (nsubstitute a b c))
    (verbose (format t "Gleichsetzen: ~A = ~A~%" a b)
             (print-table tca))  
    ;; identifiziere in den Abbildungen
    (maphash (lambda (k v) (identify-m tca v a b))
             mappings)))

(defmethod identify-m ((tca tca) (m mapping) a b)
  ;; Entferne alle Vorkommen von b
  (flet ((rpl (x) (if (eql x b) a x)))
    (let ((i-a (rpl (image m a)))
          (p-a (rpl (preimage m a)))
          (i-b (rpl (image m b)))
          (p-b (rpl (preimage m b))))
      (cond ((null i-b))                ; tue nichts
            ((null i-a) (setf (image m a) i-b))
            ((eql i-a i-b) ; a und b wurden nur permutiert
             (setf (image m a) i-b))
            ;; Sind die Bilder wirklich verschieden (auch nach Ident
            ;; von a und b), so müssen sie noch identifiziert werden.
            (t (setf (image m a) i-a)
               (identify tca i-a i-b)))
      (cond ((null p-b))                ; tue nichts
            ((null p-a) (setf (preimage m a) p-b))
            ((eql p-a p-b) ; a und b wurden nur permutiert
             (setf (preimage m a) p-b))
            ;; Sind die Urbilder wirklich verschieden (auch nach Ident
            ;; von a und b), so müssen sie noch identifiziert werden.
            (t (setf (preimage m a) p-a)
               (identify tca p-a p-b)))
      ;; Zum Schluss lösche b ganz aus der Abbildung
      (clr m b))))

(defun fill-and-identify (tca)
  (with-slots (classes ident-queue) tca
    ;; Zuerst fülle die Tabelle so weit wie möglich
    (dolist (c classes)
      (fill-class tca c))
    ;; Wenn identifiziert werden kann, so auf damit!
    (when ident-queue
      (clear-ident-queue tca)
      (fill-and-identify tca))))

(defmethod find-empty ((tca tca))
  (with-slots (classes) tca
    (let ((p (some (lambda (c) (aif (position nil c)
                                    (cons (1- it) (aref c (1- it)))))
                   classes)))
      (when p       ; leeres Feld gefunden -> setze Wert der Abbildung
        (let ((nr (add-class tca)))
          (setf (image (aref (header tca) (car p))
                       (cdr p)) nr))
        t))))

(defmethod algorithm ((tca tca))
  (do ()
      ((not (find-empty tca)))
    (fill-and-identify tca)))

(defun tca (relations subgroup)
  (let ((tca (make-instance 'tca
                            :relations relations
                            :subgroup subgroup)))
    (add-class tca)
    (algorithm tca)
    #|(renumber-classes tca)|#
    tca))

#|(defmethod renumber-classes ((tca tca))
  (with-slots (classes) tca
    ;; Sortiere Klassen aufsteigend nach Nummer
    (let ((class-nrs (sort (mapcar (lambda (c) (aref c 0)) classes) #'<)))
      (loop for i in class-nrs and j from 1 do
           (identify tca j i)
           (fill-and-identify tca)))))|#

(defmethod print-table ((tca tca))
  (dolist (c (classes tca))
    (print-table c))
  (format t "---~%"))

(defmethod print-table ((c array))
  (format t "~{~2D~^  ~}~%" (map 'list (lambda (x) (if x x "  ")) c)))

(defmethod print-generator-mappings ((tca tca))
  (maphash (lambda (k v)
             (format t "~A = " k)
             (print-cycles v))
           (generator-mappings tca)))

(defparameter *bsp-rels*
  (list (m 'x 'x 'x)
        (m 'y 'y 'y)
        (m 'x 'y 'x 'y)))

(defparameter *bsp-ug*
  (list (m 'y)))

#|(setf tca (tca *bsp-rels* *bsp-ug*))|#