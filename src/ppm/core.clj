(ns ppm.core
  (:use clojure.java.io)
  (:gen-class))

(use '[clojure.core.matrix :as m])
(require '[me.raynes.conch :refer [programs with-programs let-programs] :as sh])


(set-current-implementation :vectorz) 
(def DEFAULT_COLOR [255 0 0])
(def BLACK [0 0 0])
(def WHITE [255 255 255])


(comment "Images are done by coordinates from the top left corner:

         (0, 0) (1, 0) (2, 0)
         (0, 1) (1, 1) (2, 1)
         (0, 2) (1, 2) (2, 2)"
         )

(defn setColor [matrix x y COLOR]
  (m/mset! matrix x y 0 (int (m/select COLOR 0)))
  (m/mset! matrix x y 1 (int (m/select COLOR 1)))
  (m/mset! matrix x y 2 (int (m/select COLOR 2)))
  )

(defn createMatrix [x y]
  (def image  (matrix (repeat x (repeat y DEFAULT_COLOR))))
  )
(defn createPPM
"Initializes the ppm with size"
  [filename, image]
  (def x (dec (select (shape image) 0)))
  (println "x:
"x)
  (def y (dec (select (shape image) 1)))
  (println "y: " y)
  (def ppmFile (str filename ".ppm"))
  (def header (str "P3 " x " " y " 255" "\n"))
  (spit ppmFile header)
  (loop [iterY 0]
    
    (loop [iterX 0]
      (loop [iterRGB 0]
        (with-open [w (clojure.java.io/writer ppmFile :append true)]
          (.write w (str "  "(int (m/select image iterX iterY iterRGB)))))

        (if (> 2 iterRGB)
          (recur (inc iterRGB))
          ))
        
      (if (> x iterX)
        (recur (inc iterX))
        (with-open [w (clojure.java.io/writer ppmFile :append true)]
          (.write w "\n"))
        ))
    (if (> y iterY)
      (recur (inc iterY))
      (println "Loaded ppm"))
    )
  )


(defn distortImage
  [image distortionMatrix]
  (def x (dec (select (shape image) 0)))
  (println "x:
"x)
  (def y (dec (select (shape image) 1)))
  (println "y: " y)
  (loop [iterY 0]
    (loop [iterX 0]
      (def v (vector iterY iterX (+ iterX iterY)))
      (def product  (emap mod (m/mmul distortionMatrix v) 255))
      (m/mset! image iterX iterY 0 (m/select product 0) )
      (m/mset! image iterX iterY 1 (m/select product 1))
      (m/mset! image iterX iterY 2 (m/select product 2))
      (println "Product: " product )

        (recur (inc iterX))
        )
      
    (if (> y iterY)
      (recur (inc iterY))
    ))
  image
  )


; A = dy
; B = -dx
(defn drawHelper [image COLOR A B  d x0 y0 x1 y1 octant]
  (def -x0 (* -1 x0))
  (def -y0 (* -1 y0))
  (case octant
    1   (do (setColor image x0 y0 COLOR)
            (println x0 ", " y0))
    2    (do (setColor image y0 x0 COLOR)
             (println y0 ", " x0))
    3   (do (println y0 ", " x0)
          (setColor image y0 x0 COLOR))
    4   (do (setColor image -x0 y0 COLOR)
            (println -x0 ", " y0))
    5 (do (setColor image x0 y0 COLOR)
          (println -x0 ", " -y0))
    6 (do (setColor image x0 y0  COLOR)
          (println  x0 ", " y0))
    7   (do (setColor image  (- y1 y0) x0 COLOR)
            (println (-y1 y0) ", " x0))
    8   (do (setColor image x0 (- y1 y0) COLOR)
            (println x0 ", " (-y1 y0)))
    )
; (println "d: " d)
  (if (or (not= x0 x1) (not= y0 y1))
    (if (> d 0)
      (drawHelper image COLOR A B (+ d (* 2 B) (* 2 A) ) (+ x0 1) (+ y0 1 ) x1 y1 octant)
      (drawHelper image COLOR A B (+ d  (* 2 A) ) (+ x0 1) y0 x1 y1 octant)
      )
    )
  )


;\ 3 | 2   /
; \  |   / 
;4 \ |  / 1
;___\|/____
;5  /|\  8
;  / | \
; /  |  \
;/ 6 | 7 \ 

; Essentially, I'm using the same algorithm, but just swapping/inverting the x-y values as needed
(defn drawLine ([image COLOR  x0 y0 x1 y1]
                "Draw a line"
  (println " x0: " x0 " y0: " y0)
  (def dy (- y1 y0))
  (def dx (- x1 x0) )
  (println "dx: " dx " dy: " dy)              
  (if (>= dx 0)
    ; dx > 0
    (if (> dy 0)
     ;  dx > 0, dy > 0 --> First quadrant
      (if (> dx dy)
        ; dx > 0, dy > 0, dx > dy --> First octant
        (drawLine image COLOR x0 y0 x1 y1 1)
         ; dx > 0, dy > 0, dy > dx --> Second octant
        (drawLine image COLOR y0 x0 y1 x1 2)
      )
    ; dx > 0 dy < 0 --> Fourth quadrant
    (if (> dx (* -1 dy))
    ; dx > 0, dy < 0, dx > |dy| --> Eighth octant
      (drawLine image COLOR x0 y1 x1 y0 8)
    ; dx > 0, dy < 0, dx < |dy| -->  Seventh octant
      (drawLine  image COLOR y1 x0 y0 x1 7)
      )
    )
   ; dx < 0
    (if (> dy 0)
      ; dx < 0, dy > 0 --> Second quadrant
      (if (> (* -1 dx) dy)
        ; dx < 0, dy > 0, |dx| > dy --> Fourth octant
        (drawLine image COLOR x1 y0 x0 y1 4)
        ; dx < 0, dy > 0, |dx| < dy --> Third octant
        (drawLine image COLOR  y0 x1 y1 x0 3)
        )
      ; dx < 0, dy < 0 --> Third quadrant
      (if (> dx dy) ; aka |dy| > |dx| 
        ; dx < 0, dy < 0, |dy| > |dx|  --> Sixth octant
        (drawLine image COLOR y1 x1 y0 x0 6)
        ; dx < 0, dy < 0, |dx| > |dy| --> Fifth octant
        (drawLine image COLOR x1 y1 x0 y0 5)
        )
      )
    ))
  ( [image COLOR x0 y0 x1 y1 octant]
   (def A (- y1 y0))
   (def B (- x0 x1))
   (def d (+ (* 2 A) B))

  (println "A: " A " B: " B " d: " d " octant: " octant " x0: " x0 " y0: " y0)
   (drawHelper image COLOR A B d x0 y0 x1 y1 octant)
  ))


(defn display [filename]
  (programs display)
  (println filename)
  (display (str filename ".ppm") ))

(defn createImage
  []
  (def filename "image4")
  (def image (matrix (repeat 400 (repeat 400 BLACK))))
  (drawLine image WHITE  0 0 200 10)
  (drawLine image [255 0 255] 200 10 100 300)
  (createPPM filename image)
  (display filename)

  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (createImage))


