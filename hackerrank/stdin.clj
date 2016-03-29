(doseq [line (line-seq (java.io.BufferedReader. *in*))]
  (println (method line)))

(let [[m n] (map read-string (re-seq #"\d+" (read-line)))])
