(doseq [line (line-seq (java.io.BufferedReader. *in*))]
  (println (method line)))
