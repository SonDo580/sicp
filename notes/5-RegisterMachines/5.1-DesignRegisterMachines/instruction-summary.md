```clojure
(assign ⟨register-name⟩ (reg ⟨register-name⟩))
(assign ⟨register-name⟩ (const ⟨constant-value⟩))
(assign ⟨register-name⟩ (op ⟨operation-name⟩) ⟨input1⟩ . . . ⟨inputn⟩)
(perform (op ⟨operation-name⟩) ⟨input1⟩ . . . ⟨inputn⟩)
(test (op ⟨operation-name⟩) ⟨input1⟩ . . . ⟨inputn⟩)
(branch (label ⟨label-name⟩))
(goto (label ⟨label-name⟩))

;; Inputs
(reg ⟨register-name⟩)
(const ⟨constant-value⟩)

;; Use registers to hold labels
(assign ⟨register-name⟩ (label ⟨label-name⟩))
(goto (reg ⟨register-name⟩))

;; Use the stack
(save ⟨register-name⟩)
(restore ⟨register-name⟩)

;; Constants
(const 1) ; number 1
(const "abc") ; string "abc",
(const abc) ; symbol abc,
(const (a b c)) ; list (a b c),
(const ()) ; empty list
```