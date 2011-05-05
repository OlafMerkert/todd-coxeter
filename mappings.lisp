(in-package #:tca)

;;; bijective mappings with two hash-maps
(defclass mapping ()
  ((image    :initarg :image
             :initform (make-hash-table :test 'equal))
   (preimage :initarg :preimage
             :initform (make-hash-table :test 'equal))))

(defun mapping ()
  (make-instance 'mapping))

(defgeneric inverse (m))

(defmethod inverse ((m mapping))
  (with-slots (image preimage) m
    (make-instance 'mapping :image preimage
                   :preimage image)))

(defgeneric image (m x))
(defgeneric preimage (m x))
(defgeneric set-image (m x y))
(defgeneric set-preimage (m x y))
(defgeneric clr (m x))

(defmethod image ((m mapping) x)
  (with-slots (image) m
    (nth-value 0 (gethash x image))))

(defmethod image ((m mapping) (x list))
  (mapcar (lambda (y) (image m y)) x))

(defmethod preimage ((m mapping) x)
  (with-slots (preimage) m
    (nth-value 0 (gethash x preimage))))

(defmethod preimage ((m mapping) (x list))
  (mapcar (lambda (y) (preimage m y)) x))

(defmethod set-preimage ((m mapping) x y)
  (set-image m y x)
  x)

(defmethod set-image ((m mapping) x y)
  (with-slots (image preimage) m
    (let ((i-x (gethash x image))
          (p-y (gethash y preimage)))
      (when p-y
        (setf (gethash p-y image) i-x))
      (when i-x
        (setf (gethash i-x preimage) p-y))
      (setf (gethash x image)    y
            (gethash y preimage) x)))
  y)

(defmethod set-image ((m mapping) (x list) (y list))
  (mapc (lambda (xx yy) (set-image m xx yy)) x y)
  y)

(defsetf image set-image)
(defsetf preimage set-preimage)

(defmethod clr ((m mapping) x)
  (with-slots (image preimage) m
   (let ((i-x (gethash x image))
         (p-x (gethash x preimage)))
     (when i-x
       (remhash i-x preimage))
     (when p-x
       (remhash p-x image))
     (remhash x image)
     (remhash x preimage)))
  m)

;;; print cycles from a given mapping
(defgeneric domain (m))
(defgeneric codomain (m))

(defmethod domain ((m mapping))
  (let ((dom (make-array 10 :adjustable t :fill-pointer 0)))
    (with-slots (image) m
      (maphash (lambda (k v)
                (when v (vector-push-extend k dom)))
              image))
    dom))

(defmethod codomain ((m mapping))
  (let ((dom (make-array 10 :adjustable t :fill-pointer 0)))
    (with-slots (preimage) m
      (maphash (lambda (k v)
                (when v (vector-push-extend k dom)))
              preimage))
    dom))

(defgeneric cycles (m))
(defmethod cycles ((m mapping))
  (let ((dom (sort (domain m) #'<)))
    (labels ((cycle (start now &optional acc)
               ;; cycles end when ...
               (cond ((null now) (nreverse acc)) ; ... we hit nil
                     ((and (eql start now) acc)  ; ... or start
                      (nreverse acc))
                     (t (nsubstitute nil now dom)
                        (cycle start (image m now)
                               (cons now acc)))))
             (next-cycle (acc)
               (aif (find nil dom :test-not #'eql)
                    (next-cycle
                     (cons (cycle it it)
                           acc))
                    (nreverse acc))))
      (next-cycle nil))))

(defgeneric print-cycles (m))
(defmethod print-cycles ((m mapping))
  (format t "~{~A~^ ~}~%" (cycles m)))