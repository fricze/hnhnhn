(ns types)

(defmacro new-type [name]
  `(deftype ~name []))

(defmacro new-type-with-fields [name fields]
  `(deftype ~name ~(vec fields)))

(def Figure
  {:fields [:size]})

(defmacro new-type-with-parent [name fields parent]
  `(deftype ~name ~(vec (concat fields (map symbol (:fields (some-> parent resolve deref)))))))


(defmacro new-type-with-fields-def [name & fields]
  `(do
     ~@fields
     (deftype ~name [])))


(new-type Rect)

(defmacro fields
  {:clj-kondo/lint-as 'clojure.core/declare}
  [& name]
  `(declare ~@name))

(defmacro accessors
  {:clj-kondo/lint-as 'clojure.core/declare}
  [& name]
  `(do
     ~@(map (fn [n] `(def ~n 34)) name)))

(accessors ex exxx)

(defprotocol ISetX (set-x [this o]))

(defmacro deftype-with-accessors [])

(deftype Test [^:unsynchronized-mutable x]
  ISetX
  (set-x [_ o] (set! ex o))
  #_(set-x [_ o] (set! x o)))


(* ex exxx)

(field hey)

(macroexpand
 '(new-type-with-fields-def Figa
                           (field figa-field)))


(new-type-with-fields-def Figa
                          (field bounds))


(fields field1 field2 field3 field4)

#_(* field1 field2 field3 field4)


(def x (new-type-with-fields Rect1 [one two]))

(def y (new-type-with-parent Rect1 [one two] Figure))


(defprotocol Bounder
             (get-bounds [this]))


(deftype Triangle [size]
         Bounder
         (get-bounds [this]
           bounds))

(def T (Triangle. 34))

(get-bounds T)

(clojure.reflect/reflect Rect)

(clojure.reflect/reflect x)

(clojure.reflect/reflect y)




