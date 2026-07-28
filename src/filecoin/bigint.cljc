(ns filecoin.bigint
  "Filecoin token amounts, on two runtimes that cannot hold them.

  A `Value` on the chain is attoFIL: 10^18 to the FIL. The total supply is
  2 × 10^27 attoFIL, which is past 2^53 by nine orders of magnitude, so a
  JavaScript `Number` cannot carry a balance — not approximately, at all.
  Amounts therefore travel through this library as **decimal strings**, and
  the platform's arbitrary-precision integer is used only inside this
  namespace (`BigInteger` on :clj, `BigInt` on :cljs).

  The wire form is `go-state-types/big`: a byte string that is *empty* for
  zero, and otherwise a sign byte (0 positive, 1 negative) followed by the
  big-endian magnitude. Note that this is a sign-magnitude encoding — not
  two's complement — so it is not the JVM's `BigInteger.toByteArray` and not
  JavaScript's `BigInt.asUintN`; both need adjusting, which is most of what
  this file does.")

(def ^:const max-serialized-len
  "go-state-types' BigIntMaxSerializedLen. A value needing more bytes than
  this is refused by every node on the network, so refusing it here too is
  cheaper than finding out later."
  128)

(defn- ->ints [data]
  (if (vector? data) data (mapv #(bit-and (int %) 0xff) (seq data))))

(defn- magnitude-bytes
  "The big-endian magnitude of a non-negative decimal string, with no
  leading zero byte."
  [s]
  #?(:clj
     (let [bs (.toByteArray (java.math.BigInteger. ^String s))
           ints (mapv #(bit-and (int %) 0xff) bs)]
       ;; toByteArray is two's complement, so a value whose top bit is set
       ;; arrives with an extra 0x00 in front. Go's Int.Bytes() does not.
       (if (and (> (count ints) 1) (zero? (first ints)))
         (vec (rest ints))
         ints))
     :cljs
     (let [h (.toString (js/BigInt s) 16)
           h (if (odd? (count h)) (str "0" h) h)]
       (mapv #(js/parseInt (apply str %) 16) (partition 2 h)))))

(defn ->wire
  "A decimal string (or an integer small enough to be exact) → the byte
  string Filecoin's CBOR carries, as a vector of ints."
  [v]
  (let [s (str v)
        neg? (= \- (first s))
        mag (if neg? (subs s 1) s)]
    (when-not (and (seq mag) (every? #(<= (int \0) (int %) (int \9)) mag))
      (throw (ex-info "bigint: not a decimal integer" {:value v})))
    (if (every? #(= \0 %) mag)
      []                                  ; zero is the empty byte string
      (let [out (into [(if neg? 1 0)] (magnitude-bytes mag))]
        (when (> (count out) max-serialized-len)
          (throw (ex-info "bigint: too large for the chain's encoding"
                          {:bytes (count out) :max max-serialized-len})))
        out))))

(defn <-wire
  "The inverse: wire bytes → a decimal string."
  [bs]
  (let [ints (->ints bs)]
    (cond
      (empty? ints) "0"
      :else
      (let [sign (first ints)
            mag (vec (rest ints))]
        (when-not (<= sign 1)
          (throw (ex-info "bigint: invalid sign byte" {:sign sign})))
        (when (empty? mag)
          (throw (ex-info "bigint: sign byte with no magnitude" {})))
        (let [hex (apply str (map #(let [h #?(:clj (Integer/toHexString %)
                                              :cljs (.toString % 16))]
                                     (if (= 1 (count h)) (str "0" h) h))
                                  mag))
              n #?(:clj (java.math.BigInteger. hex 16)
                   :cljs (js/BigInt (str "0x" hex)))]
          (str (when (= 1 sign) "-") n))))))

(def ^:const atto-per-fil "1000000000000000000")

(defn fil->atto
  "\"0.5\" FIL → \"500000000000000000\" attoFIL. Decimal string in, decimal
  string out; no floating point touches the value."
  [s]
  ;; `negative?` rather than `neg?`: binding the latter shadows
  ;; `clojure.core/neg?` for the rest of the `let`, and the next line calls
  ;; it — which fails as a ClassCastException on a Boolean, at runtime,
  ;; nowhere near the name that caused it.
  (let [s (str s)
        negative? (= \- (first s))
        s (if negative? (subs s 1) s)
        [whole frac] (let [i (.indexOf ^String s ".")]
                       (if (neg? i) [s ""] [(subs s 0 i) (subs s (inc i))]))]
    (when (> (count frac) 18)
      (throw (ex-info "bigint: FIL has 18 decimal places, no more"
                      {:value s})))
    (let [digits (str (if (seq whole) whole "0")
                      frac
                      (apply str (repeat (- 18 (count frac)) "0")))
          trimmed (or (seq (drop-while #(= \0 %) digits)) [\0])]
      (str (when (and negative? (not= [\0] (vec trimmed))) "-")
           (apply str trimmed)))))
