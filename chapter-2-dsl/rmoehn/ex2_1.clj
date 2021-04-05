(ns ex2-1)

;;; I aborted this exercise when I noticed that it would have taught me more
;;; about Clojure internals than about software design for flexibility.
;;; Unfortunately, most of the exercise teach more about trivia than about the
;;; core ideas of the book.

(defn bla [a b c]
  a)

(meta #'bla)

(defn blu
  ([a] :a)
  ([a & args]
   (first [:a :b])))

(defn blo [& args])

(type (ffirst (:arglists (meta #'blu))))

(defn determine-arity [arglists]
  (count (first arglists)))

(defn arity [proc]
  (get (meta proc) ::arity (determine-arity (:arglists (meta proc)))))

(defn with-arity [proc n]
  (with-meta proc ))

(meta (with-meta bla {::arity 3}))

(map #(.getName %) (.getDeclaredMethods (class blu)))

(.getRequiredArity blu)

(map #(vector (.getName %) (count (.getParameterTypes %))) (.getDeclaredMethods (class blu)))
; min-args is smallest number for `invoke` or the value of getRequiredArity, if defined
; max-args is largest number for `invoke` or ‘infinity’ if doInvoke is defined

; It's exactly like in scheme. I can get the arity using primitives. But I
; cannot attach arity information using primitives.