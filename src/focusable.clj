(ns focusable
  (:require [io.github.humbleui.util :as util]
            [io.github.humbleui.protocols :as protocols]
            [io.github.humbleui.ui :as ui]))

#_(in-ns 'io.github.humbleui.ui)

#_(comment
  (util/deftype+ Focusable [^:mut focused]
    :extends AWrapperNode
    protocols/IComponent
    (-context [_ ctx]
              (cond-> ctx
                focused (assoc :hui/focused? true)))

    (-draw-impl [this ctx bounds viewport canvas]
                (some-> (::*focused ctx)
                        (cond->
                         focused (vswap! conj this)))
                (draw child ctx bounds viewport canvas))

    (-event-impl [this ctx event]
                 (util/eager-or
                  (when (and
                         (= :mouse-button (:event event))
                         (:pressed? event)
                         (not focused)
                         (util/rect-contains? bounds (util/ipoint (:x event) (:y event))))
                    (set! focused (util/now))
                    (invoke-callback this :on-focus)
                    true)
                  (let [event' (cond-> event
                                 focused (assoc :focused? true))]
                    (ui/event child ctx event'))))

    (-child-elements [this ctx new-element]
                     (let [[_ _ [child-ctor-or-el]] (parse-element new-element)]
                       (if (fn? child-ctor-or-el)
                         [[child-ctor-or-el (if focused #{:focused} #{})]]
                         [child-ctor-or-el]))))

  (defn focusable-ctor
    ([child]
     (map->Focusable {}))
    ([{:keys [focused on-focus on-blur] :as opts} child]
     (map->Focusable {:focused focused})))

  (defn focused [this ctx]
    (let [*acc (volatile! [])]
      (iterate this ctx
               (fn [comp]
                 (when (and (instance? Focusable comp) (:focused comp))
                   (vswap! *acc conj comp)
                   false)))
      @*acc)))

#_(util/deftype+ FocusController []
  :extends ui/AWrapperNode
  protocols/IComponent
  (protocols/-draw-impl [_ ctx bounds viewport canvas]
                        (let [*focused (volatile! [])
                              ctx'     (assoc ctx ::*focused *focused)
                              res      (ui/draw child ctx' bounds viewport canvas)
                              focused  (sort-by :focused @*focused)]
                          (doseq [comp (butlast focused)]
                            (util/set!! comp :focused nil)
                            (ui/invoke-callback comp :on-blur))
                          res))

  (protocols/-event-impl [this ctx event]
               (if (and
                    (= :mouse-button (:event event))
                    (:pressed? event)
                    (util/rect-contains? bounds (util/ipoint (:x event) (:y event))))
                 (let [focused-before (ui/focused this ctx)
                       res            (ui/event child ctx event)
                       focused-after  (ui/focused this ctx)]
                   (when (< 1 (count focused-after))
                     (doseq [comp focused-before]
                       (util/set!! comp :focused nil)
                       (ui/invoke-callback comp :on-blur)))
                   (or
                    res
                    (< 1 (count focused-after))))
                 (ui/event child ctx event))))

(defn focus-prev [this ctx]
  (let [*prev    (volatile! nil)
        *focused (volatile! nil)]
    (iterate this ctx
      (fn [comp]
        (when (instance? ui/Focusable comp)
          (if (:focused comp)
            (do
              (vreset! *focused comp)
              (some? @*prev))
            (do
              (vreset! *prev comp)
              false)))))
    (when-some [focused @*focused]
      (util/set!! focused :focused nil)
      (ui/invoke-callback focused :on-blur))
    (when-some [prev @*prev]
      (util/set!! prev :focused (util/now))
      (ui/invoke-callback prev :on-focus))))

(defn focus-next [this ctx]
  (let [*first   (volatile! nil)
        *focused (volatile! nil)
        *next    (volatile! nil)]
    (iterate this ctx
      (fn [comp]
        (when (instance? ui/Focusable comp)
          (when (nil? @*first)
            (vreset! *first comp)
            false)
          (if (:focused comp)
            (do
              (vreset! *focused comp)
              false)
            (when @*focused
              (vreset! *next comp)
              true)))))
    (when-some [focused @*focused]
      (util/set!! focused :focused nil)
      (ui/invoke-callback focused :on-blur))
    (when-some [next (or @*next @*first)]
      (util/set!! next :focused (util/now))
      (ui/invoke-callback next :on-focus))))

(defn focus-controller-impl [child]
  (ui/map->FocusController {}))

(ui/defcomp focus-controller-ctor [child]
  [ui/event-listener
   {:event    :key
    :on-event (fn [e ctx]
                (when (and
                       (:pressed? e)
                       (= :tab (:key e)))
                  (if (:shift (:modifiers e))
                    (focus-prev (-> &node :child :child :child) ctx)
                    (focus-next (-> &node :child :child :child) ctx))
                  true))
    :capture? true}
   [focus-controller-impl
    child]])
