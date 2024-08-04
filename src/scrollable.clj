(ns scrollable
  (:require [clojure.math :as math]
            [io.github.humbleui.ui :as ui]
            [clojure.set :as set]
            [io.github.humbleui.util :as util]
            [io.github.humbleui.canvas :as canvas]
            [io.github.humbleui.protocols :as protocols]
            [io.github.humbleui.signal :as signal]
            [io.github.humbleui.window :as window]))



#_(defmacro extends
  [parent & body]
  `(do
     ~@body))


(macroexpand
 '(extends ui/AWrapperNode
         [2342]
         [23432]
         (do (+ 3 4))))

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

(defmacro extends
  "Defines base “class” that deftype+ can extend from.
   Supports extra field and protocols which deftype+ can partially override.
   If calling external functions, use fully-qualified or namespaced symbol"
  [parent & body]
  (let [[doc body]    (if (string? (first body)) [(first body) (next body)] ["" body])
        [fields body] [(first body) (next body)]
        [_parent body] (if (= :extends (first body)) [(second body) (nnext body)] [nil body])
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
      `(#'merge-parents ~parent ~definition)
      definition)))

(def x 34)


(clojure.walk/prewalk
 (fn [token]
   (let [sym? (symbol? token)]
     (if (and sym? (= 'x token))
       'new-sym
       token)))
 '(extends ui/AWrapperNode [child]
           (-measure-impl [_ ctx cs]
                          (let [child-cs (assoc cs :height Integer/MAX_VALUE)]
                            (println x)
                            child-cs))))


(macroexpand
 '(extends ui/AWrapperNode [child]
           (-measure-impl [_ ctx cs]
                          (let [child-cs (assoc cs :height Integer/MAX_VALUE)]
                            (println x)
                            child-cs))))


(util/deftype+ Scroll [^:mut clip?
                       ^:mut offset-px
                       ^:mut offset
                       ^:mut child-size]
  (extends ui/AWrapperNode [^:mut child]
           (-measure-impl [_ ctx cs]
                          (let [child-cs (assoc cs :height Integer/MAX_VALUE)]
                            (set! child-size (protocols/-measure child ctx child-cs))
                            (util/ipoint
                             (:width child-size)
                             (min
                              (:height child-size)
                              (:height cs)))))))



(util/deftype+ HScrollable [^:mut clip?
                            ^:mut offset-px
                            ^:mut offset
                            ^:mut child-size]

  :extends ui/AWrapperNode

  (-measure-impl [_ ctx cs]
                 (let [child-cs (assoc cs :height Integer/MAX_VALUE)]
                   (set! child-size (protocols/-measure child ctx child-cs))
                   (util/ipoint
                    (:width child-size)
                    (min
                     (:height child-size)
                     (:height cs)))))

  (draw-impl [_ ctx bounds viewport canvas]
             (set! child-size (protocols/-measure child ctx (util/ipoint (:width bounds) Integer/MAX_VALUE)))
             (set! offset-px (util/clamp
                              (ui/scaled (or @offset 0))
                              0
                              (Math/abs (- (:width child-size) (:width bounds)))))

             (canvas/with-canvas canvas
               (when clip?
                 (canvas/clip-rect canvas bounds))

               (let [child-bounds (-> bounds
                                      (update :x - offset-px)
                                      (update :x math/round)
                                      (assoc :width (:width child-size)))]
                 (ui/draw child ctx child-bounds (util/irect-intersect viewport bounds) canvas))))

  (-event-impl [_ ctx event]
               (case (:event event)
                 :mouse-scroll
                 (when (util/rect-contains? bounds (util/ipoint (:x event) (:y event)))
                   (or
                    (ui/event child ctx event)
                    (let [offset-px' (-> offset-px
                                         (- (:delta-x event))
                                         (util/clamp 0 (- (:width child-size) (:width bounds))))]

                      (when (not= offset-px offset-px')
                        (set! offset-px offset-px')
                        (reset! offset (ui/descaled (math/round offset-px')))
                        (window/request-frame (:window ctx))))))

                 :mouse-button
                 (when (util/rect-contains? bounds (util/ipoint (:x event) (:y event)))
                   (ui/event child ctx event))

                 #_:else
                 (ui/event child ctx event)))

  (-update-element [_this _ctx new-element]
                   (let [opts (ui/parse-opts element)]
                     (set! clip? (:clip? opts true))
                     (when-some [offset' (:offset opts)]
                       (set! offset offset')))))

(defn hscrollable
  ([child]
   (hscrollable {} child))
  ([opts child]
   (map->HScrollable
    {:offset-px 0
     :offset    (or
                 (:offset opts)
                 (signal/signal 0))})))
