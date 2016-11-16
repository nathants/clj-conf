(ns confs.core
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(def *conf* nil)
(def *paths* nil)

(defmacro conf
  [& ks]
  `(doto (-> *conf* ~@ks)
     (-> nil? not (assert (str "there is no value for get-in " ~(vec ks) " in paths " (vec *paths*))))))

(defn -deep-merge
  "maps later in list override maps earlier in list, like clojure.core/merge"
  [& maps]
  (into {} (for [k (apply set/union (map set (map keys maps)))]
             (let [vs (remove nil? (map #(get % k) maps))]
               [k (if (map? (last vs))
                    (apply -deep-merge vs)
                    (last vs))]))))

(defn -keys-in
  [m]
  (->> ((fn f [k v]
          (->> (if (map? v)
                 (map #(f (cons (first %) k) (second %)) v)
                 [k ::end])))
        nil m)
    flatten
    (partition-by #(= % ::end))
    (remove #(= % [::end]))
    (map reverse)))

(defn -validate
  "makes sure all (butlast confs) contain only subsets of keys
  in (last confs), where keys are seqs of keys like the arg to
  get-in."
  [confs]
  (or (= 0 (count confs)) ;; no confs
      (= 1 (count confs)) ;; one conf, so no overrides to check
      (let [confs-to-validate (butlast confs)
            base (last confs)]
        (doseq [conf confs-to-validate]
          (assert (set/subset? (set (-keys-in conf))
                               (set (-keys-in base)))
                  (str "get-in " (vec (-keys-in conf))
                       " is an override which is not "
                       "defined in the base conf. overriding conf:\n"
                       (with-out-str (pprint/pprint (first confs)))
                       "base conf:\n"
                       (with-out-str (pprint/pprint (last confs)))))))))

(defn reset!
  "paths earlier in list override paths later in list. last path in
  list defines all possible keys and default values, and overrides
  must only use defined keys."
  [& paths]
  (let [paths (if (empty? paths) ["{}"] paths)
        first-is-edn-str (not (.exists (io/as-file (first paths))))
        paths (if first-is-edn-str
                (let [path (.getAbsolutePath (java.io.File/createTempFile "temp" ""))]
                  (spit path (pr-str (or (edn/read-string (first paths)) {})))
                  (cons path (rest paths)))
                paths)
        confs (mapv #(edn/read-string (slurp %)) paths)]
    (-validate confs)
    (def *conf* (apply -deep-merge (reverse confs)))
    (def *paths* paths)
    nil))
