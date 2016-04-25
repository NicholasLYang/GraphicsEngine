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
  "Sets color of image matrix. Probably more efficient way to do it with slices, will look into"
  (m/mset! matrix x y 0 (m/select COLOR 0))
  (m/mset! matrix x y 1 (m/select COLOR 1))
  (m/mset! matrix x y 2 (m/select COLOR 2))
  )

(defn createMatrix [x y]
  "Mainly used for debugging. Creates matrix called 'image' with specified size" 
  (def image  (matrix (repeat x (repeat y DEFAULT_COLOR))))
  )


                                        ; <------------------------ PPM ------------------------>
(defn createPPM 
"Creates the ppm. Uses java writer. Note that the x and y values are one smaller than the actual image.
  Before, I cycled through the x, y and rgb coordinates, which made things really long and tedious. Loading a 300 by 300 image took over a minute. However, I then implemented a really neat method of reading the array. Basically, I concatenate one row's rgb values into a single 1D array. Converting it to an int-array and then a vector makes sure it has integer values. Finally, I join it with spaces as the delimiters. I think I could do it all in one go, with some more core.matrix tricks, but that'll be for later.
  "
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
  image
  )
                                        ; <------------------------ Input ------------------------>

(defn parse-int [s]
  (Integer. (re-find  #"\d+" s )))


(defn getInput
  "Gets input with prompt. Todo: figure out how asynch works with prompts, make more secure"
  [prompt]
  (println prompt)
  (def out (read-line))
  (println out)
  out )




                                        ; <------------------------ Line ------------------------>


; A = dy
; B = -dx
(defn drawHelper [image COLOR A B d x0 y0 x1 y1 octant shift]
  "Line algorithm helper. Mainly for recursion. Because I'm an idiot, I used an overly complex method of shifting and rotating to get all 8 octants.
Was a bitch to debug. Essentially, any quadrant with one value being negative, must be build backwards (i.e. if the x values are going from 2 to 5, they need to be build so that they go from 5 to 2). I know, overly complicated. Would not recommend"

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
  
  (if (and (= x0 x1) (= y0 y1))
    image
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


(defn drawLine
  "Once again, used overly complex method of shifting and rotating. If you notice the print statements and overly detailed comments, that's because of this method.
  Took advantage of clojure's multiple operands options to put the function that determines the octant and the function that actually generates the constants in the same place.
  Generates constants A B and d. COLOR should be a 1D array with 3 elements (RGB values)"
  ([image COLOR x0 y0 x1 y1]
                "Draw a line"
  ; (println "Drawing line: x0 " x0 " y0 " y0 " x1" x1 " y1 " y1)
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
  "Adds an edge to the matrix. Tbh not very useful except for debugging"
  ([x0 y0 z0 x1 y1 z1]
   (array [[x0 x1][y0 y1][z0 z1][1 1]])
   )
  ([edge x0 y0 z0 x1 y1 z1]
    (m/join-along 1 edge (array [[x0 x1][y0 y1][z0 z1][1 1]]))
   )

  )

(defn rotateEdge [ x_angle y_angle z_angle]
  "Rotation. Using matricies described in notes. Probably should just implement quaternions at some point"
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
  "Scale matrix. Not very interesting"
  (matrix [[x_scale 0 0 0][0 y_scale 0 0][0 0 z_scale 0][0 0 0 1]])
  )

(defn translateEdge [ x_shift y_shift z_shift]
  "Translation matrix. Originally considered just adding to each value, but this was easier. Note: make mod/wraparound function"
  (matrix [[1 0 0 x_shift][0 1 0 y_shift][0 0 1  z_shift][0 0 0 1]])
  )

(defn getEdge [edge x y]
  "Gets edge for readEdge"
  (int (mget edge x y))
  )

(defn readEdge [edge image]
  "Reads edge and makes lines accordingly"
  (if (> (select (shape edge) 1) 0)
    (do 
      (def COLOR  (read-string (str "[" (getInput "r g b") "]" )))
      (loop [ i 0]
      (drawLine
     image
     COLOR
     (getEdge edge 0 i)
     (getEdge edge 1 i)
     (getEdge edge 0 (inc i))
     (getEdge edge 1 (inc i))
     )
    (if (< (+ i 3) (m/select (shape edge) 1 ))
      (recur (+ i 2))
      image
      )
    )
      )
    image
    )
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
  (matrix [  (join-along 0
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
                          args
                          )
                         [1])]))

                                        ;  args are in the form:
                                        ; [P0 P1 P2 P3]

(defn bezier [args t]
  (matrix 
   [(join-along 0
                (m/mmul
                 (matrix [(Math/pow t 3) (Math/pow t 2) t 1])
                 (matrix [
                          [-1 3 -3 1]
                          [3 -6 3 0]
                          [-3 3 0 0]
                          [1 0 0 0]
                          ])
                 args)[1])]))



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
    (transpose (join-along 0  edge (funct args i)))
     (addCurve funct args step (+ i step)
                 (join-along 0 edge (funct args i) (funct args i)))
     )
   )
  (
   [edge funct args freq]
   (join-along 1 edge
               (addCurve funct args freq)
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
  ; Just changing the rotation coefficient if it's a torus
  ; 4 args = sphere, 5 args = torus
  (if (= (select (shape args) 0) 4)
    (do (def a 1)
        (def R 0)
        )
    (do  (def a 2)
         (def R (select args 4))
         )
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
(defn drawBox
  ([arguments]
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
  (
   [edge arguments]
   (join-along 1 edge
               (drawBox arguments)
               )
   )
  )


(defn add2d [funct args step  t1]
  (loop [t0 0 out (matrix [[][][][]])]
    (if (> t0 1)
       out
      (recur (+ t0 step) (join-along 1 out (funct args t0 t1)))
      )
    )
  )



(comment
  [
   [
    [x0 x1 x2 x3 x4 x5 x6 x7 x8 x9]
    [y0 y1 y2 y3 y4 y5 y6 y7 y8 y9]
    [z0 z1 z2 z3 z4 z5 z6 z7 z8 z9]
    ]
   [
    [x10 x11 x12 ..... x19]
    [y10 y11 y12 ..... y19]
    [z10 z11 z12 ..... z19]
    ]
   ]

  )

                                        ;(matrix [[[(select args 0)][(select args 1)][(select args 2)][(select args 3)]]])
(defn add3d
  ([funct args freq]
  (def step (/ 1.0 freq))
   (loop [t1 0 out []]
    (if (> t1 1)
      (matrix out)
      (recur (+ t1 step) (conj out (add2d funct args step t1)))
      )
    )
   )
  ([polygon funct args freq]
   (conj polygon (add3d funct args freq) )
   )
  )


                                        ; <------------------------  Polygon Matrix  ------------------------>




(defn drawTriangle [image COLOR p0 p1 p2]
 ; (println p0)
  ;(println p1)
  ;(println p2)
  (drawLine
   (drawLine
    (drawLine
     image COLOR (select p0 0) (select p0 1) (select p1 0) (select p1 1))
    COLOR (select p1 0) (select p1 1) (select p2 0) (select p2 1))
   COLOR (select p2 0) (select p2 1) (select p0 0) (select p0 1))
  
  )

(defn drawPolyHelper [p_matrix image COLOR y0 y1 x]
  (println "POLY HELPER:")
  (println)
  (print p_matrix)
  (println)
;  (print  (m/slice p_matrix y0 ))
  (loop [iterX 0 img image]
    (if (= iterX (dec x))
      image
      (recur (inc iterX) (drawTriangle image COLOR
                                       (int-array (m/slice (columns (m/slice p_matrix y0) ) iterX))
                                       (int-array (m/slice (columns (m/slice p_matrix y0)) (inc iterX)))
                                       (int-array (m/slice (columns (m/slice p_matrix y1)) (inc iterX)))
                                       ))
      )

    )
  )

(defn drawPolygon
  "x -> amount of terms in 2d slice
  y -> amount of slices

  i.e.

  [[
  [x0 x1 x2]
  [y0 y1 y2]
  [z0 z1 z2]
  [1  1  1 ]
  ][
  [x3 x4 x5]
  [y3 y4 y5]
  [z3 z4 z5]
  ]]

  x = 3
  y = 2
  "
  [p_matrix image]
  (println "DRAW POLYGON INPUT")
  (print p_matrix)
  (def COLOR (read-string (str "[ " (getInput "r g b") " ]"  )))
  (def x  (select (shape (m/slice (m/slice p_matrix 1) 1)) 0))
  (def y (dec (select  (shape p_matrix) 1)))
  (println "Y: " y " X:" x)
  (loop [iterY 1 img image]
    (if (= iterY (dec y))
                                        ;(drawPolyHelper p_matrix image COLOR iterY 1 x)
        image
      (recur (inc iterY ) (drawPolyHelper p_matrix image COLOR iterY (inc iterY) x))
      )
    )
  )



  (defn readPolygon
   "Right now I'm just using the same color, but maybe later I'll add a prompt that asks which color to use"
    [polygon image]
    (println "INPUT READ POLYGON")
    (print (peek polygon))
    (if (empty? polygon)
        image
      (readPolygon (pop polygon)  (drawPolygon (peek polygon) image ) )
      )
    )

(defn distortHelper
"Converts edge distortion matrices into polygon form (3 edge matrices)"
  [funct args p_matrix]
  (def x (select args 0))
  (def y (select args 1))
  (def z (select args 2))
  (matrix [
           (m/mmul (funct x y z) (m/slice p_matrix 0))
           (m/mmul (funct x y z) (m/slice p_matrix 1))
           (m/mmul (funct x y z) (m/slice p_matrix 2))]
          )
  )



 (defn distortPolygon
  "Distorts polygon"
   ([polygon]
    (println "DISTORT IN:")
    (print polygon)
   (distortPolygon  polygon (peek polygon) ())
   )
   ([polygon p_matrix out]
    (println)
    (if (empty? polygon)
      (do
        (println)
        (println "DISTORT OUT:")
        (print out)
        (println "DISTORT SIZE:")
        (print (shape out))
        out)
      (distortPolygon (pop polygon ) (peek (pop polygon) )
                      (conj out  (distortHelper rotateEdge (read-string (str "[" (getInput "x rotation y rotation z rotation") "]"))
                                                (distortHelper translateEdge (read-string (str "[" (getInput "x shift y shift z shift") "]"))
                                                               (distortHelper scaleEdge (read-string (str "[" (getInput "x scale y scale z scale") "]")) p_matrix))))
                              
                     ))
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
 (comment 
 (createPPM filename (readEdge
                      (matrix [[300 100 100 100 100 300 100 300 300 300 300 100]
                               [200 200 200 200 200 200 200 200 200 200 200 200]
                               [200 200 200 200 200 200 200 200 200 200 200 200]
                               [1   1   1   1   1   1   1   1   1   1   1   1]
                               ])

                      (matrix (repeat 500 (repeat 500 BLACK))))
                      )
            
   

 )
  (createPPM filename (drawLine (matrix (repeat 500 (repeat 500 BLACK))) [255 255 255] 300 300 400 200))
  (displayImage filename)
  )




(defn scriptReader
  "Basically takes various commands, adds the respective shape to either the edge or polygon matrix. If you use the command write, it writes it to the image with the specified color. polygon is a stack of polygon matrices, while edge is a single edge matrix"
  ([image edge polygon]

   (def input (getInput "==>: "))
   (case input
     "line"  (scriptReader  image (addEdge edge (read-string (getInput "x0 y0 z0 x1 y1 z1:  " ) )) polygon)


     "sphere"   (scriptReader image edge (add3d polygon sphere (read-string (str "["(getInput "x0 y0 z0 r") "]")) (read-string (getInput "Frequency"))))


     "torus" (scriptReader  image edge (add3d polygon  sphere (read-string (str "["(getInput "x0 y0 z0 r0 r1") "]")) (read-string (getInput "Frequency"))))


     "bezier" (scriptReader image (addCurve edge bezier (read-string (str "[" (getInput "[y0 x0 z0] [y1 x1 z1] [y2 x2 z2] [y3 x3 z3]:  ") "]" ) ) (read-string (getInput "Frequency: ")  )))


     "hermite" (scriptReader image (addCurve edge hermite (read-string (str "[" (getInput "[y0 x0 z0] [y1 x1 z1] [y0' x0' z0'] [y1' x1' z1']:  ") "]"))  (read-string (getInput "Frequency: ")) ))


     "box" (scriptReader image edge (drawBox polygon (read-string (str "[" (getInput "x0 y0 z0 w h d: " ) "]" ))))

     "print polygon" (do (print polygon)
                 (scriptReader image edge polygon)
                 )
     "print image" (do
                     (print image)
                     (scriptReader image edge polygon)
                     )

     "write" (do
               (println "SCRIPTREADER OUTPUT")
               (print polygon)
               (scriptReader (readPolygon polygon (readEdge edge image )) (matrix [[][][][]]) ()  ))

     "" (do
          (def filename (getInput "filename: "))
          (createPPM
           filename
           image
           )
          (displayImage filename)
            )
     )
   )
  ( []
   (scriptReader (matrix (repeat (read-string (getInput "x:")) (repeat (read-string (getInput "y:" ) ) (read-string (str "["(getInput "r g b") "]"))))) (matrix[[][][][]]) (list ))

   
  )
)



 
(println "Initialized")



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello!")
 
  (scriptReader))


