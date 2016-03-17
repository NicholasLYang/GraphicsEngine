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
  (def y  (dec (select (shape image) 1)))
  (println "y: " y)
  (def ppmFile (str "Images/" filename ".ppm"))
  (def header (str "P3 " (inc x) " " (inc y) " 255 \n"))
  (spit ppmFile header)
  (loop [iterY 0]
   
    (loop [iterX 0]
     
      (loop [iterRGB 0]
       
        (with-open [w (clojure.java.io/writer ppmFile :append true)]
          (.write w  (str (int (m/select image iterX iterY iterRGB)) " ")))
        
        (if (> 2 iterRGB)
          (recur (inc iterRGB))
          ))
       
      (if (> x iterX)
        (recur (inc iterX))
        (with-open [w (clojure.java.io/writer ppmFile :append true)]
          (.write w "\n"))
        ))
    (println iterY)
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
(defn drawHelper [image COLOR A B  d x0 y0 x1 y1 octant shift]
  (def -x0 (* -1 x0))
  (def -y0 (* -1 y0))
  (case octant
    1    (setColor image x0 y0 COLOR)
    2     (setColor image y0 x0 COLOR)
    3    (setColor image y0  (- x1 x0) COLOR)
    4    (setColor image  (- x1 x0) y0 COLOR)
    5  (setColor image x0 y0 COLOR)
    6  (setColor image x0 y0  COLOR)
    7    (setColor image (- shift y0) x0 COLOR)
    8   (setColor image x0 (- shift y0) COLOR)
        
    )
; (println "d: " d)
  (if (or (not= x0 x1) (not= y0 y1))
    (if (> d 0)
      (drawHelper image COLOR A B (+ d (* 2 B) (* 2 A) ) (+ x0 1) (+ y0 1 ) x1 y1 octant shift)
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
    (if (> dy 0)
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
   ; dx < 0
    (if (> dy 0)
      ; dx < 0, dy > 0 --> Second quadrant
      (if (> (* -1 dx) dy)
        ; dx < 0, dy > 0, |dx| > dy --> Fourth octant
        (drawLine image COLOR x1 y0 x0 y1 4 0)
        ; dx < 0, dy > 0, |dx| < dy --> Third octant
        (drawLine image COLOR  y0 x1 y1 x0 3 0)
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


(defn display [filename]
  (programs display)
  (println filename)
  (display (str filename ".ppm") ))

(defn createImage
  []
  (def filename "image6")

  (def image(matrix (repeat 256 (repeat 256 BLACK))))
  (loop [i 0]
    (drawLine image [255 i 0] 0 i 255 (- 255 i))
   (if (> 250 i)
   (recur (+ i 5))))

   (loop [i 0]
    (drawLine image [i 0 255] i 0  (- 255 i) 255)
    (if (> 251 i)
      (recur (+ i 5))))
  
  (createPPM filename image)
  (display  filename)


  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (createImage))


