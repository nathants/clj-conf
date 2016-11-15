(ns confs.core
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.java.io :as io]))

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

(def -load)
(def -validate-conf-overrides)

(defn -validate-conf-overrides
  [paths confs]
  (or (= 0 (count paths)) ;; no confs
      (= 1 (count paths)) ;; one conf, so no overrides to check
      (let [conf (apply -load (drop 1 paths))]
        (->> confs
          first
          -keys-in
          (mapv #(try
                   (apply conf %)
                   (catch AssertionError _
                     (throw
                      (AssertionError.
                       (str "get-in " (vec %) " is an override which "
                            "is not defined in the base conf. "
                            "overriding conf:\n"
                            (with-out-str (pprint/pprint (first confs)))
                            "base conf:\n"
                            (with-out-str (pprint/pprint (last confs)))))))))))))

(defn -load
  [& paths]
  (let [confs (mapv #(edn/read-string (slurp %)) paths)]
    (-validate-conf-overrides paths confs)
    (with-meta
      (fn [& ks]
        (doto (->> confs (map #(get-in % ks)) (remove nil?) first)
          (-> nil? not (assert (str "there is no value for get-in " (vec ks) " in paths " (vec paths))))))
      {:confs confs})))

(defn load
  [& paths]
  (let [paths (if (empty? paths)
                ["{}"]
                paths)
        first-is-str (->> paths first io/as-file .exists not)
        edn-str (if first-is-str
                  (first paths)
                  "")
        paths (if first-is-str
                (rest paths)
                paths)]
    (if-not (edn/read-string edn-str)
      (apply -load paths)
      (let [path (.getAbsolutePath (java.io.File/createTempFile "temp" ""))]
        (spit path edn-str)
        (apply -load (cons path paths))))))
