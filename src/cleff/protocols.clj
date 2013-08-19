(ns cleff.protocols)

(defprotocol IEffect)

(defprotocol IHandler
  (-value [this])
  (-finally [this])
  (-operation [this effect name]))
