(asdf:defsystem "tca"
    :depends-on ("ol-utils")
    :serial t
    :components
    ((:file "package")
     (:file "mappings")
     (:file "group")))