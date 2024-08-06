(ns oop
  (:require [clojure.set :as set]
            [io.github.humbleui.protocols :as protocols]
            [clojure.walk :as walk]))

(defn replace-parent-fields [form]
  (walk/postwalk
   (fn [x]
     (if (list? x)
       (let [[field rec & body] x
             [fchar & field-name] (str field)]
         (if (and (= fchar \.) (= :accessor (eval x)))
           (symbol (apply str field-name))
           x))
       x))
   form))

(defn- qualify-symbol
  "Converts symbol to fully-qualified form in current namespace via resolve"
  [s]
  (let [o (resolve s)]
    (cond
      (nil? o)   s
      (class? o) (symbol (.getName ^Class o))
      (var? o)   (symbol o)
      :else      s)))

(defn- signature [method]
  (let [[sym args & _] method]
    [sym (count args)]))


(defn- merge-options [parent child]
  {:fields    (vec (set (concat (:fields child) (:fields parent))))
   :protocols (set/union (:protocols parent) (:protocols child))
   :methods   (merge (:methods parent) (:methods child))})


(defmacro defparent
  "Defines base “class” that deftype+ can extend from.
   Supports extra field and protocols which deftype+ can partially override.
   If calling external functions, use fully-qualified or namespaced symbol"
  [sym & body]
  (let [[doc body]    (if (string? (first body)) [(first body) (next body)] ["" body])
        [fields body] [(first body) (next body)]
        [parent body] (if (= :extends (first body)) [(second body) (nnext body)] [nil body])
        protocols     (into (:protocols parent #{})
                            (->> body (filter symbol?) (map qualify-symbol)))
        methods       (->> body
                           (remove symbol?)
                           (map #(vector (list 'quote (signature %)) (cons 'fn %)))
                           (into {}))
        definition    {:fields    (list 'quote fields)
                       :protocols (list 'quote protocols)
                       :methods   methods}]
    (if parent
      `(do
         (defrecord ~(symbol (str "Rec" sym)) ~(conj fields 'opts)
           ~@body)
         (def ~sym
           ~doc
           (new ~(symbol (str "Rec" sym))
                ~@(map (fn [_] :accessor) fields)
                (#'merge-options (.opts ~parent) ~definition))))

      `(do
         (defrecord ~(symbol (str "Rec" sym)) ~(conj fields 'opts)
           ~@body)
         (def ~sym
           ~doc
           (new ~(symbol (str "Rec" sym))
                ~@(map (fn [_] :accessor) fields)
                ~definition))))))



(defn- untag-symbol [sym]
  (vary-meta sym dissoc :tag))

(defn- untag-method
  "Converts typed argument lists to untyped ones + top-level let with tags"
  [method]
  (let [[name args & body] method]
    (if-some [typed-args (filter #(:tag (meta %)) args)]
      (let [untyped-args (mapv untag-symbol args)
            bindings     (vec (mapcat #(list % (untag-symbol %)) typed-args))]
        (list name untyped-args
              (list* 'clojure.core/let bindings
                     body)))
      method)))

(defmacro deftype+
  "Same as deftype, but:

   1. Can “inherit” default protocols/method impls from parent (:extends)
   2. Uses ^:mut instead of ^:unsynchronized-mutable
   3. Allows using type annotations in protocol arglist
   4. Read mutable fields through ILookup: (:field instance)
   5. Write to mutable fields from outside through ISettable: (-set! instance key value)
   6. Allow with-meta"
  [name fields & body]
  (let [body          (replace-parent-fields body)
        [parent body] (if (= :extends (first body))
                        (let [[_ sym & body'] body]
                          [(or (some-> sym resolve deref)
                               (throw (ex-info (str "Can't resolve parent symbol: " sym) {:symbol sym})))
                           body'])
                        [nil body])
        parent        (:opts parent)
        update-field  #(vary-meta % set/rename-keys {:mut :unsynchronized-mutable})
        fields        (->> (concat fields (:fields parent))
                           (map update-field)
                           vec)
        mut-fields    (filter #(:unsynchronized-mutable (meta %)) fields)
        protocols     (->> body
                           (filter symbol?)
                           (map qualify-symbol)
                           (set)
                           (set/union (:protocols parent)))
        methods       (->> body
                           (remove symbol?)
                           (map untag-method))
        signatures    (set (map signature methods))
        value-sym     (gensym 'value)]
    `(do
       (deftype ~name ~(conj fields '__m)
         ~@protocols

         ;; all parent methods
         ~@(for [[sig body] (:methods parent)
                 :when      (not (signatures sig))
                 :let       [[name cnt] sig
                             args (vec (repeatedly cnt gensym))]]
             `(~name ~args
                     (~body ~@args)))

         ;; own methods
         ~@methods

         clojure.lang.IMeta
         (meta [_] ~'__m)

         clojure.lang.IObj
         (withMeta [_ meta#]
           (new ~name ~@fields meta#))

         clojure.lang.ILookup
         (valAt [_# key# notFound#]
           (case key#
             ~@(mapcat #(vector (keyword %) %) fields)
             notFound#))
         (valAt [this# key#]
           (.valAt this# key# nil))

         protocols/ISettable
         (-set! [_# key# ~value-sym]
           (case key#
             ~@(mapcat #(vector (keyword %) (list 'set! % value-sym)) mut-fields))))

       (defn ~(symbol (str '-> name)) ~fields
         (new ~name ~@fields nil)) ;; __m

       (defn ~(symbol (str 'map-> name)) [m#]
         (let [{:keys ~fields} m#]
           (new ~name ~@fields nil))))))


(comment

  (defparent Wrapper [bounds size]
    protocols/IComponent
    (-draw [_ ctx bounds viewport canvas]
           ctx)
    (-context [_ ctx]
              ctx))

  (.opts Wrapper)

  (macroexpand-1
   '(defparent CNode [field1 field2]
      :extends Wrapper
      protocols/IComponent
      (-context [_ ctx]
                ctx)))

  (defparent CNode [field1 field2 size]
    :extends Wrapper
    protocols/IComponent
    (-context [_ ctx]
              ctx))

  (.field1 CNode))
