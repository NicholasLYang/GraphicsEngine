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
    1   (setColor image x0 y0 [x0 y0 128])
    2      (setColor image y0 x0 [x0 y0 128])
    3     (setColor image  (- shift y0) x0 [x0 y0 128])
    4    (setColor image  (- shift x0)  y0 [x0 y0 128])
    5  (setColor image x0 y0 [x0 y0 128])
    6  (setColor image y0 x0  [x0 y0 128])
    7    (setColor image  y0 (- shift x0) [x0 y0 128])
    8   (setColor image x0 (- shift y0) [x0 y0 128])

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
  (println " x0: " x0 " y0: " y0 " x1: " x1 " y1: " y1)
  (def dy (- y1 y0))
  (def dx (- x1 x0) )
  (println "dx: " dx " dy: " dy)              
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
   (println "Start point: " x0 ", " y0 " End point: " x1 ", " y1 " Octant: " octant " shift: " shift)
 
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
  (loop [ i 0]
    (drawLine
     image
     [255 255 255]
     (getEdge edge 0 i)
     (getEdge edge 1 i)
     (getEdge edge 0 (inc i))
     (getEdge edge 1 (inc i))
     )
    (if (< (+ i 2) (m/select (shape edge) 1 ))
        (recur (+ i 2))
      )
    )
  image
  )



(defn h0 [t]
  (*
   (+ 1 (* 2 t))
   (Math/pow (- 1 t) 2)
   )
  )

(defn h1 [t]
  (*
   t
   (Math/pow
    (- 1 t)
    2             )
   )
  )

(defn h2 [t]
  (*
   (Math/pow t 2)
   (-
    3
    (* 2 t)
    ))
  )

(defn h3 [t]
  (*
   (Math/pow t 2)
   (- t 1))
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
    (m/mmul
     (translateEdge  200 200 0)
     (rotateEdge (/ pi 4) (/ pi 12) 0)
     (scaleEdge 0.75 0.75 0.75)
     ;(matrix [[100 200 200 200 200 100 100 100][100 100 100 200 200 200 200 100][1 1 1 1 1 1 1 1][1 1 1 1 1 1 1 1]]))
  (matrix [[100 100 100 200 100 100 200 100 100 100 100 200 200 400 100 400 100 400 100 400][100 100 100 100 100 200 100 200 200 100 100 100 100 400 100 400 200 400 200 400][1 100 1 1 1 1 1 1 1 100 100 1 1 300 1 300 1 300 1 300][1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]]))
    (matrix (repeat 500 (repeat 500 BLACK)))
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


