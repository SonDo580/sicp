;; A machine with 2 GCD computations
(controller
 ;...
 gcd-1
   (test (op =) (reg b) (const 0))
   (branch (label after-gcd-1))
   (assign t (op rem) (reg a) (reg b))
   (assign a (reg b))
   (assign b (reg t))
   (goto (label gcd-1))
 after-gcd-1
 ;...
 gcd-2
   (test (op =) (reg d) (const 0))
   (branch (label after-gcd-2))
   (assign s (op rem) (reg c) (reg d))
   (assign c (reg d))
   (assign d (reg s))
   (goto (label gcd-2))
 after-gcd-2
 ;...
 )

;; => duplicated data-path components

;; => Improve:
;; If the values in register a and b are not needed after gcd 1,
;; or they can be moved somewhere else for safekeeping, 
;; the machine can keep using register a and b
(controller
 ;...
 gcd-1
   (test (op =) (reg b) (const 0))
   (branch (label after-gcd-1))
   (assign t (op rem) (reg a) (reg b))
   (assign a (reg b))
   (assign b (reg t))
   (goto (label gcd-1))
 after-gcd-1
 ;...
 gcd-2
   (test (op =) (reg b) (const 0))
   (branch (label after-gcd-2))
   (assign t (op rem) (reg a) (reg b))
   (assign a (reg b))
   (assign b (reg t))
   (goto (label gcd-2))
 after-gcd-2
 ;...
 )
;; => 2 GCD sequences only differ in their entry-point labels

;; => Improve:
;; Use a single sequence - a GCD subroutine
;; register 'continue': determine which label to return to
(controller
 ;...
 gcd
   (test (op =) (reg b) (const 0))
   (branch (label after-gcd-1))
   (assign t (op rem) (reg a) (reg b))
   (assign a (reg b))
   (assign b (reg t))
   (goto (label gcd-1))
 after-gcd
   (test (op =) (reg continue) (const 0))
   (branch (label after-gcd-1))
   (goto (label after-gcd-2))
 ;; ...
 ;; assign 0 to 'continue' register before branching to gcd (first time)
   (assign continue (const 0))
   (goto (label gcd))
 after-gcd-1
 ;; ...
 ;; assign 1 to 'continue' register before branching to gcd (second time)
   (assign continue (const 1))
   (goto (label gcd))
 after-gcd-2
 )
;; => This will be a problem if there are many GCD computations

;; => Improve:
;; Store the label to continue after the subroutine is finished
;; (extend 'assign' instruction to allow assining label to a register)
(controller
 ;...
 gcd
   (test (op =) (reg b) (const 0))
   (branch (label after-gcd-1))
   (assign t (op rem) (reg a) (reg b))
   (assign a (reg b))
   (assign b (reg t))
   (goto (label gcd-1))
 after-gcd
   (goto (reg continue))
 ;; ...
 ;; assign after-gcd-1 to 'continue'
   (assign continue (label after-gcd-1))
   (goto (label gcd))
 after-gcd-1
 ;; ...
 ;; assign after-gcd-2 to 'continue'
   (assign continue (label after-gcd-2))
   (goto (label gcd))
 after-gcd-2)