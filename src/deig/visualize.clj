(ns deig.visualize
  (:require [quil.core :as q :include-macros true]          ;; add this
            [quil.middleware :as middleware]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]))

(def viz-width 28)                                          ;; width of the visualization window in pixels
(def viz-height 28)                                         ;; height of the visualization window in pixels


;; made it 1 instead of 0 so that the file would recognize it as a color
(defn rand-color []
  (if (> (rand-int 2) 0) 255 0))

(defn draw []
  (q/background 255)
  (let [size 28
        gr (q/create-graphics 28 28)
        im (q/create-image viz-width viz-height :rgb)]
    (q/with-graphics gr
                     ;; make it hsb
                     (q/color-mode :hsb)

                     ;(q/fill 0 0 (rand-color))
                     (q/fill 0 0 (rand-color))
                     ;; no outside line
                     (q/stroke nil)

                     ; draw arc
                     ;; x, y, width, height, start, stop, mode (open, chord, pie)
                     ;(q/arc 28 28 20 17 0 q/QUARTER-PI)
                     ; draw different arc modes

                     (doseq [[ind mode] [[0 :open] [1 :chord] [2 :pie]]]
                       (q/arc (rand-int 28) (rand-int 28) (rand-int 50) (rand-int 200) 0 q/QUARTER-PI mode)))

    (q/image gr 0 0)


    (pp/pprint (q/pixels gr) (clojure.java.io/writer "quilvec.txt"))

    (q/no-loop)
    (q/exit)
    ))


(defn start-visualization []
  "Create the Quil sketch."
  (q/sketch
    :size [28 28]
    :draw draw))



(start-visualization)




