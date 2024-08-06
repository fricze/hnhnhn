
(defn replace-parent-fields [form]
  (clojure.walk/postwalk
   (fn [x]
     (if (list? x)
       (let [[field rec & body] x
             [fchar & field-name] (str field)]
         (if (and (= fchar \.) (= :accessor (eval x)))
           (symbol (apply str field-name))
           x))
       x))
   form))
