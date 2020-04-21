(ns deig.core
  (:require [deig.fitness :refer [fitness]]))

;; Individual creation
(defn grayscale-genome [dimension]
  "Returns random grayscale genome of dimensions dimension*dimension"
  (vec (repeatedly dimension
                   #(vec (repeatedly dimension (fn [] (vector (rand-int 256))))))))

(defn rbg-genome [dimension]
  "Returns random grayscale genome of dimensions dimension*dimension"
  (vec (repeatedly dimension
                   #(vec (repeatedly dimension (fn [] (vec (repeatedly 3 (rand-int 256)))))))))

(defn new-individual [size & {:keys [grayscale] :or {grayscale false}}]
  "Creates a new individual with a genome of a random size*size image"
  (let [genome (if (true? grayscale) (grayscale-genome size) (rbg-genome size))]
    {:genome  genome
     :fitness (fitness genome)}))

;; Selection
(defn fittest [individuals]
  "Returns the fittest of the given individuals."
  (reduce (fn [i1 i2]
            (if (> (:fitness i1) (:fitness i2))
              i1
              i2))
          individuals))

(defn select [population]
  "Returns an individual selected from population using a tournament of 10."
  (fittest (repeatedly 10 #(rand-nth population))))

;; Mutation
(defn mutate-channel [pixel]
  "Creates a mutated version of an individual's pixel. Mutates each channel randomly."
  (let [mutation-rate 0.2]
    (if (< (rand) mutation-rate)
      ;; Case where the pixel mutates, you change each of its color channels
      (vec (map (fn [channel]
                  ;; Our changes to the color are bounded within a range of (-30, 30)
                  (let [change (+ (* (- 30 -30) (rand)) -30)]
                    (cond
                      ;; If the change is outside of our range, stick to our range
                      (> (+ channel change) 255) 255
                      (< (+ channel change) 0) 0
                      ;; Otherwise, just use the change
                      :else (+ channel change)))) pixel))
      ;; Case where it doesn't
      pixel)))

(defn mutate-uniform [genome]
  "Mutate each pixel in genome with the same mutation rate."
  (map (fn [column]
         (map
           (fn [row]
             (map (fn [channel] (mutate-channel channel))
                  row)))
         column)
       genome))

(defn mutate [genome]
  "Randomly selects a mutation to mutate an image with"
  (let [type (rand)]
    (cond
      (< type 0.2) (mutate-uniform genome)
      ;; More mutations
      :else (mutate-uniform genome))))

(defn make-child [population]
  "Returns a new, evaluated child, produced by mutating the result
  of crossing over parents that are selected from the given population."
  (let [new-genome (vec (mutate (:genome (select population))))]
    {:genome  new-genome
     :fitness (fitness new-genome)}))

(defn populate [population-size]
  (repeatedly population-size
              #(new-individual 28 :grayscale true)))

(defn re-populate [population population-size]
  (conj (repeatedly (dec population-size)
                    #(make-child population))
        (fittest population)))

;; Output
(defn report [generation population]
  "Prints a report on the status of the population at the given generation."
  (println {:generation generation :best-fitness (:fitness (fittest population))}))

;; Main functions
(defn evolve [population-size generations]
  "Runs a genetic algorithm to generate an image according to the
  criteria of a deep classifier model."
  (loop [population (populate population-size)
         generation 0]
    (report generation population)
    (if (>= generation generations)
      (fittest population)
      (recur (re-populate population population-size)
             (inc generation)))))

(defn -main
  "Run the evolutionary algorithm"
  []
  (println "Starting evolution.")
  (evolve 1000 30))

;: Evolve.
#_(-main)
