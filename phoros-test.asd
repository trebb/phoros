(defsystem :phoros-test
  :description "PHOROS tests"
  :author "Bert Burgemeister"
  :maintainer "Bert Burgemeister"
  :licence "GPL"
  :serial t
  :components ((:file "test"))
  :depends-on (:phoros
               :rt))                  ;TODO: use sb-rt where available
