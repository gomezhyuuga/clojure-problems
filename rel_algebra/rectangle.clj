;;;
;;; This program defines a record called Rectangle. Each instance of
;;; Rectangle has a width and a height. Support functions for this
;;; record type are: rectangle (factory), str, area, and perimeter.
;;;
;;; The REPL is reprogrammed so that the str function is used when
;;; printing instances of Rectangle.
;;;
;;; REPL usage example:
;;;
;;;     user=> (load-file "rectangle.clj")
;;;     :ok
;;;     user=> (def a (rectangle 3 5))
;;;     #'user/a
;;;     user=> (def b (rectangle 10 2))
;;;     #'user/b
;;;     user=> (str a)
;;;     "* * *\n* * *\n* * *\n* * *\n* * *"
;;;     user=> (str b)
;;;     "* * * * * * * * * *\n* * * * * * * * * *"
;;;     user=> a
;;;     * * *
;;;     * * *
;;;     * * *
;;;     * * *
;;;     * * *
;;;     user=> b
;;;     * * * * * * * * * *
;;;     * * * * * * * * * *
;;;     user=> (area a)
;;;     15
;;;     user=> (area b)
;;;     20
;;;     user=> (perimeter a)
;;;     16
;;;     user=> (perimeter b)
;;;     24
;;;

(use '[clojure.string :only (join)])

;--------------------------------------------------------------------
(defrecord Rectangle
  [width height]

  ;; Override Java's Object.toString method. This allows using
  ;; the 'str' function with Rectangle instances.
  Object
  (toString [this]
    (declare str-rectangle) ; str-rectangle is declared later.
    (str-rectangle this)))

;--------------------------------------------------------------------
;; print-method is used by the REPL to decide how to print
;; an object of a specific type (in this case Rectangle).
;; We use here the 'str' function (defined above as 'toString')
;; to privide a consistent way of printing and converting to
;; string.
(defmethod print-method Rectangle
  [this w]
  (print-simple (str this) w))

;--------------------------------------------------------------------
(defn check-argument
  "Check if condition is true. If not, throw an
  IllegalArgumentException with the given error-message."
  [condition error-message]
  (when (not condition)
    (throw (IllegalArgumentException. error-message))))

;--------------------------------------------------------------------
;; This function is the one that actually converts a
;; Rectangle instance into a string.
(defn str-rectangle
  "Convert an instance of Rectangle into its string representation."
  [rect]

  (check-argument
   (instance? Rectangle rect)
   (str "Parameter 'rect' must be an instance of Rectangle, not "
        (class rect)))

  (join "\n"
        (repeat (.height rect)
                (join " "
                      (repeat (.width rect) \*)))))

;--------------------------------------------------------------------
(defn rectangle
  "Factory function for creating instances of Rectangle."
  [width height]

  (check-argument
   (and (integer? width) (pos? width))
   (str "Parameter 'width' must be an integer greater than zero. "
        "Value given: "
        width))

  (check-argument
   (and (integer? height) (pos? height))
   (str "Parameter 'height' must be an integer greater than zero. "
        "Value given: "
        height))

  (->Rectangle width height))

;--------------------------------------------------------------------
(defn area
  "Given a rectangle, return its area."
  [rect]

  (check-argument
   (instance? Rectangle rect)
   (str "Parameter 'rect' must be an instance of Rectangle, not "
        (class rect)))

  (* (.width rect)
     (.height rect)))

;--------------------------------------------------------------------
(defn perimeter
  "Given a rectangle, return its perimeter."
  [rect]

  (check-argument
   (instance? Rectangle rect)
   (str "Parameter 'rect' must be an instance of Rectangle, not "
        (class rect)))

  (* 2
    (+ (.width rect)
       (.height rect))))

;--------------------------------------------------------------------
:ok
