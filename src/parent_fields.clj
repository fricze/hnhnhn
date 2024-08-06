(ns parent-fields
  (:require [clojure.set :as set]
            [io.github.humbleui.protocols :as protocols]
            [clojure.walk :as walk]))


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

(defn- merge-parents [parent child]
  {:fields    (vec (concat (:fields child) (:fields parent)))
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
    `(do
       (defrecord ~(symbol (str "Rec" sym)) ~(conj fields 'opts)
         ~@body)
       (def ~sym (new ~(symbol (str "Rec" sym)) ~@(map (fn [_] :accessor) fields) ~definition)))

    #_(if parent
        `(def ~sym ~doc
           (#'merge-parents ~parent ~definition))
        `(defrecord ~sym ~doc
           ~definition))))

(comment
  (macroexpand-1
   '(def-parent CNode [field1 field2]
      protocols/IComponent
      (-context [_ ctx]
                ctx))))

(def-parent CNode [field1 field2]
  protocols/IComponent
  (-context [_ ctx]
            ctx))

#_(:opts CNode)

(defn replace-parent-fields [form]
  (clojure.walk/postwalk
   (fn [x]
     (if-let [l? (list? x)]
       (if l?
         (let [[field rec & body] x
               [fchar & field-name] (str field)]
           (if (and (= fchar \.) (= :accessor (eval x)))
             (symbol (apply str field-name))
             x))
         x)
       x))
   form))

(comment
  (replace-parent-fields
   '(let [x 2
          y 3
          z 24]
      (set! (.field1 CNode) 334))))




(defparent NodeParent
  [^:mut element
   ^:mut mounted?
   ^:mut bounds
   ^:mut key
   ^:mut dirty?
   ^:mut child]

  protocols/IComponent
  (-context [_ ctx]
    ctx)

  (-measure [this ctx cs]
    (let [ctx (protocols/-context this ctx)]
      (ui/maybe-render this ctx)
      (protocols/-measure-impl this ctx cs)))

  (-draw [this ctx bounds' viewport canvas]
    (protocols/-set! this :bounds bounds')
    (when (util/irect-intersect bounds' viewport)
      (let [ctx (protocols/-context this ctx)]
        (ui/maybe-render this ctx)
        (protocols/-draw-impl this ctx bounds' viewport canvas))))

  (-event [this ctx event]
    (let [ctx (protocols/-context this ctx)]
      (protocols/-event-impl this ctx event)))

  (-event-impl [this ctx event]
    nil)

  (-should-reconcile? [_this _ctx _element]
    true)

  (-reconcile [this ctx new-element]
    (when (not (identical? (:element this) new-element))
      (let [ctx (protocols/-context this ctx)]
        (protocols/-reconcile-impl this ctx new-element)
        (protocols/-update-element this ctx new-element)
        (protocols/-set! this :element new-element)))
    this)

  (-reconcile-impl [this _ctx element]
    (throw (ex-info "Not implemented" {:element (:element this)})))

  (-update-element [this ctx new-el]
    :nop)

  (-unmount [this]
    (protocols/-unmount-impl this))

  (-unmount-impl [_this]))

(comment
  (macroexpand-1
   '(deftype+ Rectangle [^:mut clip?
                         ^:mut offset-px
                         ^:mut offset
                         ^:mut child-size]
      :extends NodeParent
      (-measure-impl [_ ctx cs]
                     (let [child-cs     (assoc cs :height Integer/MAX_VALUE)
                           element-size (:size (.element NodeParent))]
                       (set! child-size (protocols/-measure child ctx child-cs))
                       (util/ipoint
                        (:width child-size)
                        (min
                         (:height child-size)
                         (:height cs))))))))

(deftype+ Rectangle [^:mut clip?
                     ^:mut offset-px
                     ^:mut offset
                     ^:mut child-size]
  :extends NodeParent
  (-measure-impl [_ ctx cs]
                 (let [child-cs     (assoc cs :height Integer/MAX_VALUE)]
                   (set! child-size (protocols/-measure (.child NodeParent) ctx child-cs))
                   (util/ipoint
                    (:width child-size)
                    (min
                     (:height child-size)
                     (:height cs))))))
