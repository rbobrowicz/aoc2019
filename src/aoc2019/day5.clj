(ns aoc2019.day5
  (:require [clojure.string :as str]))

(defn- make-param-exprs [params]
  (mapcat #(list (symbol (str "p" %2)) `(nth ~'mem (+ ~'ip ~(+ %2 1))))
          params
          (range (.length params))))

(defn- make-mode-exprs [params]
  (mapcat #(list (symbol (str "m" %2)) `(-> ~'modes ~@(replicate %2 `(quot 10)) (rem 10)))
          params
          (range (.length params))))

(defn- make-val-exprs [params]
  (mapcat identity
          (filter #(not= \w ((comp first str first) %))
                  (map #(list (symbol (str %1 %2)) `(if (= ~(symbol (str "m" %2)) 1)
                                                      ~(symbol (str "p" %2))
                                                      (nth ~'mem ~(symbol (str "p" %2)))))
                       params
                       (range (.length params))))))

(defn- make-body-exprs [body]
  (if-let [e (first body)]
    (case (first e)
      set (cons `(aset ~'mem ~(nth e 1) ~(nth e 2))
                 (make-body-exprs (rest body)))
      goto `((let [~'ip (or ~(nth e 1) ~'ip)]
               ~@(make-body-exprs (rest body))))
      out `((let [~'output (conj ~'output ~(nth e 1))]
              ~@(make-body-exprs (rest body))))
      in (cons `(aset ~'mem ~(nth e 1) (first ~'input))
               `((let [~'input (rest ~'input)]
                   ~@(make-body-exprs (rest body)))))
      recur `(~e))))

(defmacro instruction
  "Changes expressions like:

  (instruction rrw (set p2 (+ r0 r1)))

  into:

  (let [p0 (nth mem (+ ip 1))
        p1 (nth mem (+ ip 2))
        p2 (nth mem (+ ip 3))
        m0 (-> modes (rem 10))
        m1 (-> modes (quot 10) (rem 10))
        m2 (-> modes (quot 10) (quot 10) (rem 10))
        r0 (if (= m0 1) p0 (nth mem p0))
        r1 (if (= m1 1) p1 (nth mem p1))
        ip (+ ip 4)]
    (aset mem p2 (+ r0 r1))
    (recur ip mem input output))

  Meant to be ran within the main interpreter function as it expects hardcoded
  locals for memory, instruction pointer, input and output.

  First argument is parameter format for operands (rrw means two read parameters
  and a write paramter).

  Generates new locals available in macro body:
  p0 - pN for raw parameters
  m0 - mN for parameter mode for each parameter
  r0 - rN for each value of a read parameter

  Body can be made up of the following clauses:
  (set p r) - store value r in parameter p
  (goto e) - set instruction pointer to e (if e is non-nil)
  (in p) - store next input in p
  (out r) - write r to output
  "
  [params & body]
  `(let [~@(make-param-exprs (str params))
         ~@(make-mode-exprs (str params))
         ~@(make-val-exprs (str params))
         ~'ip (+ ~'ip ~(+ 1 (.length (str params))))
         ]
     ~@(make-body-exprs (concat body `((recur ~'ip ~'mem ~'input ~'output))))))

(defn run-intcode [mem input]
  (loop [ip 0
         mem (into-array mem)
         input input
         output []]
    (let [instr (nth mem ip)
          opcode (rem instr 100)
          modes (quot instr 100)]
      (case opcode
        99 output
        ; add
        1  (instruction rrw (set p2 (+ r0 r1)))
        ; mul
        2  (instruction rrw (set p2 (* r0 r1)))
        ; input
        3  (instruction w (in p0))
        ; output
        4  (instruction r (out r0))
        ; jump if true
        5  (instruction rr (goto (if (not= 0 r0) r1)))
        ; jump if false
        6  (instruction rr (goto (if (= 0 r0) r1)))
        ; less than
        7  (instruction rrw (set p2 (if (< r0 r1) 1 0)))
        ; equal
        8  (instruction rrw (set p2 (if (= r0 r1) 1 0)))
        ))))


(defn read-input-file [path]
  (-> (slurp path)
      (str/split #",")
      ((fn [xs] (map #(Long/parseLong %) xs)))))

(comment

  (run-intcode
   (read-input-file "./resources/day5.input")
   '(1)
   )

  (run-intcode
   (read-input-file "./resources/day5.input")
   '(5)
   )
)
