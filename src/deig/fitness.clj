(ns deig.fitness
    (:require
      [libpython-clj.require :refer [require-python]]
      [libpython-clj.python :as py :refer [call-attr]]
      [quil.core :as q]))

(require-python
  '[numpy :as np]
  '[keras_preprocessing.image :as image-processing]
  '[tensorflow.keras.models :as models])

;; Load default Keras MNIST classifier
(def model (models/load_model "mnist.h5"))

(defn fitness [img]
  "Evaluate an image based on its best categorical score"
    (let [x-array (image-processing/img_to_array img)
          x (np/expand_dims (np/array x-array) 0)
          probabilities (first (call-attr model "predict_proba" x))
          fitness (* (apply max probabilities) 100)]
         fitness))

(defn predict-class [img]
 "Evaluate an image based on its best categorical score"
   (let [x-array (image-processing/img_to_array img)
         x (np/expand_dims (np/array x-array) 0)
         class (first (call-attr model "predict_classes" x))]
        class))

(defn save-image [genome generation class]
  "Save individual's genome to file"
  (call-attr (image-processing/array_to_img genome) "save" (str "./individuals/gen" generation "predicted" class ".png")))

;; Example code
(defn grayscale-genome [dimension]
  "Returns random grayscale genome of dimensions dimension*dimension"
  (vec (repeatedly dimension
                   #(vec (repeatedly dimension (fn [] (vector (* (rand) 256))))))))

(defn line-genome []
  "Returns a genome with a centered vertical white line."
  (let [genome (grayscale-genome 28)]
    (vec (map-indexed (fn [i col]
                        (map-indexed (fn [j pixel]
                                       (if (and (< j 16) (> j 13) (> i 4) (< i 26))
                                         [255]
                                         [0])) col)) genome))))

(defn trial
    "Run a practice fitness run"
    []
  (fitness (line-genome)))


;: Should return 100.
#_ (trial)                                                  ;
