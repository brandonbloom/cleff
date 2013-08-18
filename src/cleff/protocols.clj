(ns cleff.protocols)

(defprotocol IEffect)

(defprotocol IHandler
  (-value [this])
  (-operation [this effect name]))
