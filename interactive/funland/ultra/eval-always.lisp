(in-package :funland)
(export (quote eval-always))
(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     . ,body))
