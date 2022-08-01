;;;;
;;;; Collections package for Kale
;;;;
(defpackage #:com.kjcjohnson.kale.collections
  (:use #:cl)

  (:shadow #:remove
           #:get)
  
  (:local-nicknames (#:kl #:com.kjcjohnson.kale)
                    (#:kl/oo #:com.kjcjohnson.kale.oo)))

