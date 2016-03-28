(ns ppm.core
  (:use clojure.java.io)
  (:gen-class))

(use '[clojure.core.matrix :as m])
(require '[clojure.core.matrix.protocols :as prot])
(require '[me.raynes.conch :refer [programs with-programs let-programs] :as sh])


(set-current-implementation :vectorz) 
(def DEFAULT_COLOR [255 0 0])
(def BLACK [0 0 0])
(def WHITE [255 255 255])
(def pi (Math/PI))

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
  (println "x: "x)
  (def y  (dec (select (shape image) 1)))
  (println "y: " y)
  (def ppmFile (str "images/" filename ".ppm"))
  (def header (str "P3 " (inc x) " " (inc y) " 255 \n"))
  (spit ppmFile header)
  (loop [iterY 0]
   
    (loop [iterX 0]
     
      (loop [iterRGB 0]
        (def a (m/select image iterX iterY iterRGB))
        (with-open [w (clojure.java.io/writer ppmFile :append true)]
          (.write w  (str (int a) " ")))
        
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






; A = dy
; B = -dx
(defn drawHelper [image COLOR A B  d x0 y0 x1 y1 octant shift]
 

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
(defn drawLine ([image COLOR  x0 y0 x1 y1]
                "Draw a line"
 ; (println " x0: " x0 " y0: " y0 " x1: " x1 " y1: " y1)
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
  ; (println "Start point: " x0 ", " y0 " End point: " x1 ", " y1 " Octant: " octant " shift: " shift)
 
   (drawHelper image COLOR A B d x0 y0 x1 y1 octant shift)
  ))

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
  (print edge "\n")
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
   [1 3]
   )
  )


(defn addCurve
  ([funct args freq]
   (println funct)
  ; Time is between 0 and 1
   (def step (/ 1.0 freq))
   (addCurve funct args step 0 (broadcast (funct args 0) [1 3]))
   )
  ( [funct args step i edge]
   (print edge "\n")
    (if (> (+ i step) 1)
    (join-along 0  edge   (funct args i))
     (addCurve funct args step (+ i step)
                 (join-along 0 edge (funct args i) (funct args i)))
     )
   )
  )


(defn displayImage [filename]
  (programs display)
  (println filename)
  (display (str "images/" filename ".ppm") ))

(defn createImage
  []
  (def filename "image6")
   
  (createPPM
   filename
   (readEdge
    (transpose (addCurve bezier (matrix [[0 0 0] [50 100 50] [150 250 7] [100 200 10]]) 25))
    (matrix (repeat 300 (repeat 300 DEFAULT_COLOR)))
    )
   )
 (displayImage filename)
  )
  
  



 
(println "Initialized")



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (createImage))


