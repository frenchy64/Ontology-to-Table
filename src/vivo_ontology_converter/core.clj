(ns vivo-ontology-converter.core
  (:require [clojure.java.io :as io])
  (:use [clojure.contrib.str-utils :only (str-join)]
        [plaza.rdf core sparql]
        [plaza.rdf.implementations.jena]))

(init-jena-framework)

(defn model-from-file
  "Extracts model from local file" 
  [path format]
  (document-to-model (io/reader path) format))

(defn get-defined-classes
  "Returns all defined classes in a model"
  [model]
  (let [q (defquery
            (query-set-vars [?s])
            (query-set-pattern (make-pattern [[?s 
                                               rdf:type 
                                               "http://www.w3.org/2002/07/owl#Class"]]))
            (query-set-type :select))]
    (map :?s (query model q))))

(defn get-class-properties
  "Returns properties defined by rdf-class
  in the model"
  [model rdf-class]
  (let [q (defquery
            (query-set-vars [?p ?o])
            (query-set-pattern (make-pattern [[(. rdf-class toString)
                                               ?p
                                               ?o]]))
            (query-set-type :select))]
    (query model q)))

(def col-sep "|")
(def headers
  (list "Class"
        "Superclasses"
        "Properties"
        "Description"))

(defn interleave-before-after
  "Like str-join but puts seperator before
  and after"
  [sep strs]
  (str col-sep
       (str-join sep strs) 
       col-sep))

(defn write-header
  "Returns headers with seperators before and after"
  []
  (interleave-before-after col-sep headers))

(defn property-pairs
  "Returns a list of pairs (property, description) 
  about properties defined by rdf-class"
  [m rdf-class]
  (map #(list (.toString (% :?p)) (.toString (% :?o)))
       (get-class-properties m rdf-class)))
        
(defn parse-model
  "Returns a map with information about every 
  class defined in a model.

  ({:class class-obj
    :superclasses list-of-superclasses
    :properties property-description-pairs})"
  [m]
  (for [c (get-defined-classes m)]
    {:class c
     :superclasses nil
     :properties (property-pairs m c)}))


; hacky but works, TODO
(defn output-document
  [model-file out]
  (let [m (model-from-file model-file :owl)
        f (io/file out)
        content (map #(apply str %) (for [{c :class
                                          superclasses :superclasses
                                          props :properties} (parse-model m)]
                                      (concat 
                                        (let [p (first props)]
                                          (str (interleave-before-after col-sep (list c 
                                                                                      superclasses 
                                                                                      (first p) 
                                                                                      (second p)))
                                               "\n"))
                                        (for [p (rest props)]
                                          (str (interleave-before-after col-sep (list "" 
                                                                                      superclasses 
                                                                                      (first p) 
                                                                                      (second p)))
                                               "\n")))))]
    (spit f (apply str content))))

