;; Data path: 
;; - defines the flow of data between registers and operations that 
;;   manipulate this data

;; Controller:
;; - defines the sequence of steps the machine follows during its execution

;; GCD machine
(data-paths
 (registers 
  ((name a)
   (buttons ((name a<-b) (source (register b)))))
  ((name b)
   (buttons ((name b<-t) (source (register t)))))
  ((name t)
   (buttons ((name t<-r) (source (operation rem))))))
 (operations
  ((name rem) (inputs (register a) (register b)))
  ((name =) (inputs (register b) (constant 0)))))

(controller
 test-b                       ; label
  (test =)                    ; test
  (branch (label gcd-done))   ; conditional branch
  (t<-r)                      ; button push
  (a<-b)                      ; button push
  (b<-t)                      ; button push
  (goto (label test-b))       ; unconditional branch
 gcd-done)                    ; label

;; Without data-path description 
(controller
 test-b
  (test (op =) (reg b) (const 0))
  (branch (label gcd-done))
  (assign t (op rem) (reg a) (reg b))
  (assign a (reg b))
  (assign b (reg t))
  (goto (label test-b))
 gcd-done)
