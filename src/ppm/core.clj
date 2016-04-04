(ns ppm.core
  (:use clojure.java.io)
 ; (:use swank.core)
  (:gen-class)
  (:require [clojure.string :as str]))

(use '[clojure.core.matrix :as m])
(require '[clojure.string :as str])
(require '[me.raynes.conch :refer [programs with-programs let-programs] :as sh])


(set-current-implementation :vectorz) 
(def DEFAULT_COLOR [255 0 255])
(def BLACK [0 0 0])
(def WHITE [255 255 255])
(def pi (Math/PI))

(comment "Images are done by coordinates from the top left corner:

         (0, 0) (1, 0) (2, 0)
         (0, 1) (1, 1) (2, 1)
         (0, 2) (1, 2) (2, 2)"
         )
                                        ; <------------------------ Matrix ------------------------>
(defn setColor [matrix x y COLOR]
  (m/mset! matrix x y 0 (m/select COLOR 0))
  (m/mset! matrix x y 1 (m/select COLOR 1))
  (m/mset! matrix x y 2 (m/select COLOR 2))  )

(defn createMatrix [x y]
  (def image  (matrix (repeat x (repeat y DEFAULT_COLOR))))
  )


                                        ; <------------------------ PPM ------------------------>
(defn createPPM 
"Initializes the ppm with size"
  [filename, image]
  (def x (dec (select (shape image) 0)))
  (println "x: "x)
  (def y  (dec (select (shape image) 1)))
  (println "y: " y)
  (def ppmFile (str "images/" filename ".ppm"))
  (def header (str "P3 " (inc x) " " (inc y) " 255 \n"))
  (spit ppmFile header)
  (loop [iterY 0]
      (with-open [w (clojure.java.io/writer ppmFile :append true)]
        (.write w (str (str/join " " (vec  (int-array (apply concat (m/slice image iterY))))) "\n")))
      

      (if (> y iterY)
      (recur (inc iterY)))
    )
  )
  

(comment 
  (loop [iterY 0]
    
    (loop [iterRGB 0]
      (def a (m/select image iterX iterY iterRGB))
      (with-open [w (clojure.java.io/writer ppmFile :append true)]
        (.write w  (str (int a) " ")))
      
      (if (> 2 iterRGB)
        (recur (inc iterRGB))
        ))
    
    (if (> y iterY)
      (recur (inc iterY))
      (with-open [w (clojure.java.io/writer ppmFile :append true)]
        (.write w "\n"))
      )))

                                        ; <------------------------ Line ------------------------>


; A = dy
; B = -dx
(defn drawHelper [image COLOR A B d x0 y0 x1 y1 octant shift]
 (def COLOR (matrix [x0 y0 x1]))
  (case octant
    1   (setColor image x0 y0 COLOR)
    2      (setColor image y0 x0 COLOR)
    3     (setColor image  (- shift y0) x0 COLOR)
    4    (setColor image  (- shift x0)  y0 COLOR)
    5  (setColor image x0 y0 COLOR)
    6  (setColor image y0 x0  COLOR)
    7    (setColor image  y0 (- shift x0) COLOR)
    8   (setColor image x0 (- shift y0) COLOR)

    )
  
  (if (or (not= x0 x1) (not= y0 y1))

    (if (> d 0)
      (drawHelper image COLOR  A B (+ d (* 2 B) (* 2 A) ) (+ x0 1) (+ y0 1 ) x1 y1 octant shift)
      (drawHelper image COLOR A B (+ d  (* 2 A) ) (+ x0 1) y0 x1 y1 octant shift)
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
(defn drawLine
  ([image COLOR x0 y0 x1 y1]
                "Draw a line"
                
  (def dy (- y1 y0))
  (def dx (- x1 x0) )
 ; (println "dx: " dx " dy: " dy)              
  (if (>= dx 0)
    ; dx > 0
    (if (>= dy 0)
     ;  dx > 0, dy > 0 --> First quadrant
      (if (> dx dy)
        ; dx > 0, dy > 0, dx > dy --> First octant
        (drawLine image COLOR x0 y0 x1 y1 1 0)
         ; dx > 0, dy > 0, dy > dx --> Second octant
        (drawLine image COLOR y0 x0 y1 x1 2 0)
      )
    ; dx > 0 dy < 0 --> Fourth quadrant
    (if (> dx (* -1 dy))
    ; dx > 0, dy < 0, dx > |dy| --> Eighth octant
      (drawLine image COLOR x0 0 x1 (- y0 y1) 8 y0)
      ; dx > 0, dy < 0, dx < |dy| -->  Seventh octant
      (drawLine  image COLOR 0  x0 (- y0 y1) x1 7 y0)
      )
    )
   ; dx =< 0
    (if (> dy 0)
      ; dx < 0, dy > 0 --> Second quadrant
      (if (> (* -1 dx) dy)
        ; dx < 0, dy > 0, |dx| > dy --> Fourth octant
        (drawLine image COLOR 0 y0 (- x0 x1) y1 4 x0)
        ; dx < 0, dy > 0, |dx| < dy --> Third octant
        (drawLine image COLOR y0 0 y1 (- x0 x1) 3 x0)
        )
      ; dx < 0, dy < 0 --> Third quadrant
      (if (> dx dy) ; aka |dy| > |dx| 
        ; dx < 0, dy < 0, |dy| > |dx|  --> Sixth octant
        (drawLine image COLOR y1 x1 y0 x0 6 0)
        ; dx < 0, dy < 0, |dx| > |dy| --> Fifth octant
        (drawLine image COLOR x1 y1 x0 y0 5 0)
        )
      )
    ))
  ( [image COLOR x0 y0 x1 y1 octant shift]
   (def A (- y1 y0))
   (def B (- x0 x1))
   (def d (+ (* 2 A) B))
    
   (drawHelper image COLOR A B d x0 y0 x1 y1 octant shift)
  ))


                                        ; <------------------------ Edge/Transformations ------------------------>
(defn addEdge
  ([x0 y0 z0 x1 y1 z1]
   (array [[x0 x1][y0 y1][z0 z1][1 1]])
   )
  ([edge x0 y0 z0 x1 y1 z1]
    (m/join-along 1 edge (array [[x0 x1][y0 y1][z0 z1][1 1]]))
   )

  )

(defn rotateEdge [ x_angle y_angle z_angle]
  (def x_matrix (matrix
                 [
                  [1 0 0 0]
                  [0 (Math/cos x_angle) (* -1 (Math/sin x_angle)) 0]
                  [0 (Math/sin x_angle)  (Math/cos x_angle) 0]
                  [0 0 0 1]
                  ]))
 
  (def y_matrix (matrix
                 [
                  [(Math/cos y_angle) 0 (* -1 (Math/sin y_angle)) 0]
                  [0 1 0  0]
                  [(Math/sin y_angle) 0 (Math/cos y_angle) 0]
                  [0 0 0 1]
                  ]))

  (def z_matrix (matrix
                 [
                  [(Math/cos z_angle)  (* -1 (Math/sin z_angle)) 0 0]
                  [(Math/sin z_angle)  (Math/cos z_angle) 0 0]
                  [0 0 1 0]
                  [0 0 0 1]
                  ]))

  (m/mmul x_matrix y_matrix z_matrix) 
  
 
  )

(defn scaleEdge [x_scale y_scale z_scale]
  (matrix [[x_scale 0 0 0][0 y_scale 0 0][0 0 z_scale 0][0 0 0 1]])
  )

(defn translateEdge [ x_shift y_shift z_shift]
  (matrix [[1 0 0 x_shift][0 1 0 y_shift][0 0 1  z_shift][0 0 0 1]])
)
(defn getEdge [edge x y]
  (int (mget (select edge x y)))
  )
(defn readEdge [edge image]
 ; (print edge "\n")
  (loop [ i 0]
    (drawLine
     image
     [255 255 255]
     (getEdge edge 0 i)
     (getEdge edge 1 i)
     (getEdge edge 0 (inc i))
     (getEdge edge 1 (inc i))
     )
    (if (< (+ i 3) (m/select (shape edge) 1 ))
        (recur (+ i 2))
      )
    )
  image
  )

                                        ; <------------------------ Hermite/Bezier Curves ------------------------>

(comment
  p0 = initial 
  p1 = final point
  m0 = initial derivative, 
  m1 = final derivative
  args = [p0 p1 m0 m1]
  )
(defn hermite [args t]
  (broadcast
   (m/mmul
    (matrix [ (Math/pow t 3) (Math/pow t 2) t 1 ] )
    (matrix
     [
      [2 -2 1 1]
      [-3 3 -2 -1]
      [0 0 1 0]
      [1 0 0 0]
      ]
     )
    args) [1 3])
  )

;  args are in the form:
 ; [P0 P1 P2 P3]

(defn bezier [args t]
  (broadcast 
   (m/mmul
  
    (matrix [(Math/pow t 3) (Math/pow t 2) t 1])
    (matrix [
             [-1 3 -3 1]
             [3 -6 3 0]
             [-3 3 0 0]
             [1 0 0 0]
             ])
    args
    )
   [1 4]
   )
  )


(defn addCurve
  ([funct args freq]
   (println funct)
  ; Time is between 0 and 1
   (def step (/ 1.0 freq))
   (addCurve funct args step 0 (funct args 0))
   )
  ( [funct args step i edge]
   (print edge "\n")
    (if (> (+ i step) 1)
    (transpose (join-along 0  edge   (funct args i)))
     (addCurve funct args step (+ i step)
                 (join-along 0 edge (funct args i) (funct args i)))
     )
   )
  )
                                        ; <------------------------ 3D Objects ------------------------>

(comment
  args = [x0 y0 z0 r R]
  If you want a torus, just make R != 0
  )

(defn sphere [args t0 t1]
   (def r (select args 3))
  (def R (select args 4))
  ; Just changing the rotation coefficient if it's a torus
  (if (= R 0)
    (def a 1)
    (def a 2)
    )
  (matrix [
           [(+ (select args 0) (* r  (Math/cos (* 2 pi t0) )))]
           [(+ (select args 1) (* (+ (* r (Math/sin (* 2 pi t0))) R) (Math/cos (* a pi t1))))]
           [(+ (select args 2) (* (+ (* r (Math/sin (* 2 pi t0))) R) (Math/sin (* a pi t1))))]
           [1]
           ])
  )

(comment
arguments = [x0 y0 z0 w h d]
  )
(defn drawBox [arguments & args]
  (def x0 (select arguments 0))
  (def y0 (select arguments 1))
  (def z0 (select arguments 2))
  (def w (select arguments 3))
  (def h (select arguments 4))
  (def d (select arguments 5))
  (matrix [
           [x0  x0  (+ x0 w) (+ x0 w) (+ x0 w)  (+ x0 w)  x0       x0         (+ x0 w) (+ x0 w)  x0       x0        (+ x0 w) (+ x0 w) x0       x0]
           [y0  y0  (+ y0 h) (+ y0 h)  y0       y0        (+ y0 h) (+ y0 h)   y0       y0        y0       y0        (+ y0 h) (+ y0 h) (+ y0 h) (+ y0 h)]
           [z0  z0  z0       z0        z0       z0        (+ z0 d) (+ z0 d )  (+ z0 d) (+ z0 d)  (+ z0 d) (+ z0 d)  (+ z0 d) (+ z0 d) z0       z0]
           [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]
           ])
  )


(defn add3D
  ([funct args freq]
   (def step (/ 1.0 freq))
   (add3D (join-along 1 (funct args 0 0) (funct args 0 0)) funct args step step 0)
   )
  ([edge funct args step t p]
   (if (> t 1)
     (if (> p 1)
       edge
       (add3D (join-along 1 edge (funct args 0 (+ step p)) (funct args 0 (+ step p))) funct args step 0 (+ step p))
       )
     (add3D (join-along 1 edge (funct args t p) (funct args t p)) funct args step (+ step t) p)
     )
   )
)



                                        ; <------------------------  Image Creation  ------------------------>

(defn displayImage [filename]
  (programs display)
  (println filename)
  (display (str "images/" filename ".ppm") ))

(defn createImage
  []
  (def filename "image6")
 
 (createPPM filename (readEdge
   (add3D sphere [50 50 50 25 0] 15)
   (matrix (repeat 300 (repeat 300 DEFAULT_COLOR)))))

   
 (displayImage filename)
  )

                                        ; <------------------------ Input ------------------------>

(defn getInput [prompt]
  (println prompt)
  (read-line))

(defn parseScript [script]
  (println "first command: "(select (str/split script #"\n") 0))
  )
(defn scriptReader
  ([]
   (def input (getInput "==>: "))
   (if (= input "done")
     (parseScript input)
     (do
       (println input)
       (scriptReader input)
       )
   )
   )

  ( [script]
   (def input (getInput "==>:  "))
   (if  (= input "done")
     (parseScript script)
     (do
       (println input)
       (scriptReader (str script "\n" input))
       )
     )
   )
  )




 
(println "Initialized")



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello!")
  (scriptReader))


