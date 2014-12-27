;; Boilerplate example

(defn foobar
  []
  (let [foo 3
        bar 7]
    (defn getbar1 [& args] bar)   ; fake these names
    (defn times31 [x] (* x foo))  ; so the repl doesn't get confused
    (fn [message & args]
      (cond
       (= message :getbar) (apply getbar1 args)
       (= message :times3) (apply times31 args)
       true (bad_arg "Unknown method, " message)))))

(defn getbar [foobar & args] (apply foobar :getbar args))

(defn times3 [foobar & args] (apply foobar :times3 args))

;; Code to generate objects

(defn makesym [name] (symbol (str name "__internal")))

(defn make-dispatch [method-names]
  (let [message (gensym 'message)
        args (gensym 'args)]
    `(fn [~message & ~args]
       (cond ~@(apply
                concat
                (for [name method-names]
                  [`(= ~message ~(keyword name))
                   `(apply ~(makesym name) ~args)]))))))

(defn transform-method [[_ name & rest]]
  `(defn ~(makesym name) ~@rest))

(defmacro defobj [name bindings & methods]
  (let [method-names (map second methods)
        dispatch (make-dispatch method-names)]
    `(do
       (defn ~name []
         (let ~bindings
           ;; ~@(map transform-method methods)
           ~@(for [[_ name & rest] methods]
               `(defn ~(makesym name) ~@rest))
           ~dispatch))
       ~@(for [method-name method-names]
           `(defn ~method-name
              [obj# & args#]
              (apply obj# ~(keyword method-name) args#))))))

;; Example object

(macroexpand
 '(defobj foobar
    [foo 3
     bar 7]
    (defn getbar [] bar)
    (defn times3 [x] (* x foo))
    ))

(defobj foobar
     [foo 3
      bar 7]
     (defn getbar [] bar)
     (defn times3 [x] (* x foo)))

(defmacro dispatch
  [& functions]
  (let [message (gensym)]
    `(fn [~message]
       (cond
        ~@(apply concat
                 (for [function# functions]
                   `((= ~message ~(keyword function#)) function#)))))))

(defmacro dispatch-object
  [& functions]
  `(do ~@(for [function functions]
           `(defn ~function [object# & args#]
              (apply object# ~(keyword function) args#)))))
