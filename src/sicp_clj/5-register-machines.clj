(ns sicp-clj.5-register-machines
  "This uses macros to expand and modify Clojure's syntax to support
   Scheme syntax.  Mutability is not yet supported"
  ;; (:refer [clojure.core])
  (:require [clojure.repl :refer (doc)])
  ;; (:require [sicp-clj.core :refer (cons pair? car cdr set-car! set-cdr!)])
  )

(comment
;; cider-repl-toggle-pretty-printing
(in-ns 'sicp-clj.5-register-machines)
)

(defn bad_arg [& text]
  (throw (IllegalArgumentException. (apply str text))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Register stuff (no dependencies)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This returns a function, how is it supposed to be called?
(defn make-register
  [name]
  (let [contents (atom nil)]
    (fn [message & args]
      (cond
       (= message :get) @contents
       (= message :set) (apply reset! contents args)
       true (bad_arg "Unknown request -- register " name ": " message)
       ))))

(defn get-contents [register] (register :get))

(defn set-contents! [register value] (register :set value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stack stuff (no dependencies)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-stack
  []
  (let [s (atom '())]
    (defn stack-push [x] (swap! s #(cons x %)))
    (defn stack-pop [] (let [[head & tail] @s]
                         (if (nil? head)
                           (bad_arg "Empty stack, can't pop")
                           (do (reset! s tail)
                               head))))
    (defn initialize [] (reset! s '()))
    (fn [message]
      (cond
       (= message :push) stack-push
       (= message :pop) stack-pop
       (= message :initialize) initialize
       true (bad_arg "Unknown request -- stack: " message)))))

(defn stack-pop! [stack] ((stack :pop)))
(defn stack-push! [stack value] ((stack :push) value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc no-dependencies stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-instruction [text] (atom (list text)))
(defn instruction-text [instruction] (first @instruction))
(defn instruction-proc [instruction] (second @instruction))
(defn set-instruction-proc! [instruction proc]
  (swap! instruction #(list (first %) proc)))

(defn start [machine] (machine :start))
(defn get-register [machine reg-name]
  ((machine :get-register) reg-name))
(defn get-register-contents [machine register-name]
  (get-contents (get-register machine register-name)))
(defn set-register-contents! [machine register-name value]
  (do (set-contents! (get-register machine register-name) value)
      :done))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Execution procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; don't use these, just enter it in the labels dict
(defn make-label-entry [label-name instructions])
(defn lookup-label [labels label-name])

(defn assign-reg-name [[_ inst & exps]] inst)
;; TODO Do I need to unpack this? (yes, apparently)
(defn assign-value-exp [exp] (rest (rest exp)))

;; This is basically just an atom - in clojure this level of abstraction
;; can probably just be removed
(defn advance-pc [pc]
  (set-contents! pc (rest (get-contents pc))))

(defn tagged-list? [exp tag]
  (if (list? exp)
    (= (first exp) tag)
    false))

(defn register-exp? [exp] (tagged-list? exp 'reg))
(defn constant-exp? [exp] (tagged-list? exp 'const))
(defn label-exp? [exp] (tagged-list? exp 'label))

(defn operation-exp? [exp]
  (and (list? exp)
       (tagged-list? (first exp) 'op)))

(defn operation-exp-op [operation-exp]
  (second (first operation-exp)))
(defn operation-exp-operands [operation-exp]
  (rest operation-exp))

(defn make-primitive-exp [exp machine labels]
  (cond
    (constant-exp? exp) (let [constant (second exp)]
                          (fn [] constant))
    (label-exp? exp) (let [insts (get labels (second exp))]
                       (fn [] insts))
    (register-exp? exp) (let [reg (get-register machine (second exp))]
                          (fn [] (get-contents reg)))
    true (bad_arg "Unknown expression type -- ASSEMBLE " exp)))


(defn make-operation-exp [exp machine labels operations]
  (let [op (get operations (operation-exp-op exp))
        aprocs (map #(make-primitive-exp % machine labels)
                    (operation-exp-operands exp))]
    (fn [] (apply op (map #(%) aprocs)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific instruction types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-assign [inst machine labels ops pc]
  (let [target (get-register machine (assign-reg-name inst))
        value-exp (assign-value-exp inst)
        value-proc (if (operation-exp? value-exp)
                     (make-operation-exp value-exp machine labels ops)
                     (make-primitive-exp (first value-exp) machine labels))]
    ;; return execution procedure for `assign`
    (fn [] (do (set-contents! target (value-proc))
               (advance-pc pc)))))

(defn make-test [inst machine labels operations flag pc]
  (let [condition (rest inst)]
    (if (operation-exp? condition)
      (let [condition-proc (make-operation-exp condition machine labels operations)]
        (fn [] (do (set-contents! flag (condition-proc))
                   (advance-pc pc))))
      (bad_arg "Bad TEST instruction -- ASSEMBLE " inst))))

(defn make-branch [inst machine labels flag pc]
  (let [dest (second inst)]
    (if (label-exp? dest)
      (let [insts (get labels (second dest))]
        (fn [] 
          (if (get-contents flag)
            (set-contents! pc insts)
            (advance-pc pc))))
      (bad_arg "Bad BRANCH instruction -- ASSEMBLE " inst))))

(defn make-goto [inst machine labels pc]
  (let [dest (second inst)]
    (cond
      (label-exp? dest) (let [insts (get labels (second dest))]
                          (fn [] (set-contents! pc insts)))
      (register-exp? dest) (let [reg (get-register machine (second dest))]
                             (fn [] (set-contents! pc (get-contents reg))))
      true (bad_arg "Bad GOTO instruction -- ASSEMBLE " inst))))

(defn make-save [inst machine stack pc]
  (let [reg (get-register machine (second inst))]
    (fn [] (do (stack-push! stack (get-contents reg))
               (advance-pc pc)))))

(defn make-restore [inst machine stack pc]
  (let [reg (get-register machine (second inst))]
    (fn [] (do (set-contents! reg (stack-pop! stack))
               (advance-pc pc)))))

(defn make-perform [inst machine labels operations pc]
  (let [action (second inst)]
    (if (operation-exp? action)
      (let [action-proc (make-operation-exp action machine labels operations)]
        (fn [] (action-proc) (advance-pc pc)))
      (bad_arg "Bad PERFORM instruction -- ASSEMBLE " inst))))

(defn make-execution-procedure
  [inst labels machine pc flag stack ops]
  (case (keyword (first inst))
    :assign (make-assign inst machine labels ops pc)
    :test (make-test inst machine labels ops flag pc)
    :branch (make-branch inst machine labels flag pc)
    :goto (make-goto inst machine labels pc)
    :save (make-save inst machine stack pc)
    :restore (make-restore inst machine stack pc)
    :perform (make-perform inst machine labels ops pc)
    (bad_arg "Unknown instruction type -- ASSEMBLE" inst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instruction stuff 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn update-instructions! [insts labels machine]
  (let [pc (get-register machine :pc)
        flag (get-register machine :flag)
        stack (machine :stack)
        ops (machine :operations)]
    (doseq [inst insts]
      (set-instruction-proc!
       inst
       (make-execution-procedure
        (instruction-text inst) labels machine pc flag stack ops)))))


(defn extract-labels
  "text is the sequence of controller instruction expressions
  receive is a function which accepts:
    instructions - a list of instruction data
    labels - a table associating each label with the position in instructions"
  [text receive]
  (if (empty? text)
    (receive '() {})
    (extract-labels
     (rest text)
     (fn [instructions labels]
       (let [next-inst (first text)]
         (if (symbol? next-inst)
           (receive instructions
                    (assoc labels next-inst instructions))
           (receive (cons (make-instruction next-inst)
                          instructions)
                    labels)))))))

(defn assemble [controller-text machine]
  (extract-labels
   controller-text
   (fn [instructions labels]
     (do (update-instructions! instructions labels machine)
         instructions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Machine operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-new-machine
  []
  (let [pc (make-register :pc) ; program counter
        ;; used in branching - "Test" should set, "Branch" should read
        flag (make-register :flag)
        stack (make-stack)
        instructions (atom '())
        the-ops (atom {:initialize-stack (fn [] stack :initialize)})
        register-table (atom {:pc pc
                              :flag flag})]
    (defn allocate-register [name]
      (if (contains? @register-table name)
        (throw (IllegalAccessException.
                (str "Multiply defined register: " name)))
        (swap! register-table #(into % {name (make-register name)})))
      :register-allocated)
    (defn lookup-register [name]
      (if-let [val (get @register-table name)]
        val
        (bad_arg "Unknown register: " name)))
    (defn execute []
      (let [insts (get-contents pc)]
        (if (or (empty? insts) (nil? insts))
          :done
          (do ;(println (instruction-proc (first insts)))
              ((instruction-proc (first insts)))
              (execute)))))
    (fn [message]
      (case message
        ;; TODO should this be instructions or @instructions
        :start (do (set-contents! pc @instructions)
                   (execute)) 
        :install-instructions (fn [seq] (reset! instructions seq))
        :allocate-register allocate-register
        :get-register lookup-register
        :install-operations (fn [ops] (swap! the-ops #(conj % ops)))
        :stack stack
        :operations @the-ops
        (bad_arg "Unknown request: MACHINE " message)))))

(defn make-machine
  [register-names ops controller-text]
  (let [machine (make-new-machine)]
    (doseq [register-name register-names]
      ((machine :allocate-register) register-name))
    ((machine :install-operations) ops)
    ((machine :install-instructions) (assemble controller-text machine))
    machine))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def gcd-machine
  (make-machine
   '(a b t)
   {'mod mod '= =}
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op mod) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))

(do
  (set-register-contents! gcd-machine 'a 12)
  (set-register-contents! gcd-machine 'b 30)
  (start gcd-machine)
  (get-register-contents gcd-machine 'a))

