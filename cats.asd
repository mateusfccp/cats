(defsystem :cats
  :description "Generators for DestructHub/cats"
  :version "0.0.1"
  :author "Mateus Felipe C C Pinto <mateusfccp@gmail.com>"
  :licence "MIT"
  :components ((:file "generators" :depends-on ("utils"))
               (:file "utils")))
