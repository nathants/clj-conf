(ns clj-confs.core
  (:require [clojure.edn :as edn]
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

(defn -load
  [& paths]
  (let [confs (mapv #(edn/read-string (slurp %)) paths)]
    (or (= 1 (count paths))
        (let [conf (apply -load (drop 1 paths))]
          (mapv #(apply conf %) (-keys-in (first confs)))))
    (with-meta
      (fn [& ks]
        (doto (->> confs (map #(get-in % ks)) (remove nil?) first)
          (-> nil? not (assert (str "there is no value for keys " (vec ks) " in paths " (vec paths))))))
      {:confs confs})))

(defn load
  [& paths]
  (let [paths (if (empty? paths)
                ["{}"]
                paths)
        last-is-str (->> paths first io/as-file .exists not)
        edn-str (if last-is-str
                  (first paths)
                  "")
        paths (if last-is-str
                (rest paths)
                paths)]
    (if-not (edn/read-string edn-str)
      (apply -load paths)
      (let [path (.getAbsolutePath (java.io.File/createTempFile "temp" ""))]
        (spit path edn-str)
        (apply -load (cons path paths))))))
