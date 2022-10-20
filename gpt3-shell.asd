;; GPT3 shell
;; Packaged on 2022-10-12

(asdf:defsystem :gpt3-shell
  :name "gpt3-shell"
  :version "0.0.1"
  :author "Benjamin Kane"
  :depends-on (:py4cl)
  :components ((:file "package")
               (:file "gpt3-shell")))

