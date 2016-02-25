(ns ppm.core
  (:use clojure.java.io)
  
  (:gen-class
   :name ppm.core
   :methods [#^{:static true} [createImage ]]))

(use '[clojure.core.matrix :as m])
(require '[me.raynes.conch :refer [programs with-programs let-programs] :as sh])


(set-current-implementation :vectorz) 
(def DEFAULT_COLOR [138 200 15])



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (createImage))

(defn initializePPM
"Initializes the ppm with size"
  [filename, image]
  (def x (dec (select (shape image) 0)))
  (println "x:
"x)
  (def y (dec (select (shape image) 1)))
  (println "y: " y)
  (def ppmFile (str filename ".ppm"))
  (def header (str "P3 " x " " y " 255"))
  (spit ppmFile header)
  (loop [iterY 0]
    (def tempLine "")
    (loop [iterX 0]
      (loop [iterRGB 0]
        (def tempLine (str tempLine  " " (m/select image iterX iterY iterRGB)))
        (if (> 2 iterRGB)
          (recur (inc iterRGB))
          )
        )
      (if (> x iterX)
        (recur (inc iterX))
        (do
          (println tempLine)
          (spit ppmFile tempLine :append true))))
    (if (> y iterY)
      (recur (inc iterY))
      (println "Loaded ppm"))
    ))

(defn distortImage
  [image distortionMatrix]
  (def x (dec (select (shape image) 0)))
  (println "x:
"x)
  (def y (dec (select (shape image) 1)))
  (println "y: " y)
  (loop [iterY 0]
    (loop [iterX 0]
      (def v (vector (* iterX iterX) (* iterY iterY)  (* iterX iterY)))
      (def product  (emap mod (m/mmul distortionMatrix v) 255))
      (m/mset! image iterX iterY 0 (m/select product 0) )
      (m/mset! image iterX iterY 1 (m/select product 1))
      (m/mset! image iterX iterY 2 (m/select product 2))
      (println "Product: " product )
      (if (> x iterX)
        (recur (inc iterX))
        )
      )
    (if (> y iterY)
      (recur (inc iterY)))
    )
  image
  )
(defn -createImage
  []
  (def image (matrix (repeat 200 (repeat 200 DEFAULT_COLOR))))
  (initializePPM "image2" (distortImage image distortionMatrix))
  (programs display)
  (display "image2.ppm")

  )

(defn setVal
  [a [x y z] newVal]
  (def a (assoc a x  (assoc (get a x) y (assoc (get (get a x) y) z newVal))))
  )



