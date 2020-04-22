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
          x (np/expand_dims x-array 0)
          probabilities (first (call-attr model "predict_proba" x))
          fitness (* (apply max probabilities) 100)]
         fitness))

;; Example code
(defn grayscale-genome [dimension]
  "Returns random grayscale genome of dimensions dimension*dimension"
  (vec (repeatedly dimension
                   #(vec (repeatedly dimension (fn [] (vector (rand-int 256))))))))

(defn partitionv [n x]
  "Partitions into vecs within vecs"
  (mapv #(vec %) (partition n x)))


(defn trial
    "Run a practice fitness run"
    []
  (fitness (grayscale-genome 28)))

;: Should return 0.
#_ (trial)                                                  ;

