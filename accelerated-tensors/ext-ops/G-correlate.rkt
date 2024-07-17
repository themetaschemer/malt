#lang racket

(require string-interpolation)
(require "../tensors/0-vectors.rkt")
(require (only-in "../tensors.rkt" ext2-ρ len))
(require "../autodiff.rkt")

;; Correlation is written taking into account how ext2 works
;; Ext2 is responsible for producing the i-out'th output from
;; v0[i0] and v1[i1], we take advantage of this. The shape constants
;; n b m d are pre-calculated the striding constants nd md and qd
;; are calculated.

(define correlate-3-1-ρ
  (λ (nd md qd)
      (λ (v0 i0 _
          v1 i1 d
          v-out i-out b)
        (let* ((i1-min (- i1 (modulo i1 nd)))
               (i1-max (+ i1-min nd)))
          (for ((i (in-range 0 b)))
            (vset! v-out (+ i-out i)
              (for/fold ([sum 0.0]) ([j (in-range 0 md)])
                (let ((ai (+ i0 (* i md) j))
                      (bi (- (+ i1 j) qd)))
                  (cond
                    ((and (>= bi i1-min) (< bi i1-max))
                     (let ((a (vref v0 ai))
                           (b (vref v1 bi)))
                       (+ sum (* a b))))
                    (else sum))))))))))

(define correlate-3-1-ρ-acc
  (λ (nd md qd)
      (λ (v0 i0 _
          v1 i1 d
          v-out i-out b)
        #<<EOF
    int i1_min = @{i1} - @{i1} % @{nd};
    int i1_max = i1_min + @{nd};
    for(int i=0; i<@{b}; i++) {
        float sum = 0.0;
        for(int j=0; j<@{md}; j++) {
            int ai = @{i0} + i * @{md} + j;
            int bi = @{i1} + j - @{qd};
            if(bi >= i1_min && bi < i1_max) {
                sum += @{v0}[ai] * @{v1}[bi];
            }
        }
        @{v-out}[@{i-out}+i] = sum;
    }
EOF
        )))

(define correlate-3-1-∇
  (λ (nd md qd)
    (λ (g0 g1
        v0 i0 bmd
        v1 i1 d
        vz iz b)
      (let* ((i1-min (- i1 (modulo i1 nd)))
             (i1-max (+ i1-min nd)))
        (for ((i (in-range 0 b)))
          (let ((z (vref vz (+ iz i))))
            (for ([j (in-range 0 md)])
              (let ((ai (+ i0 (* i md) j))
                    (bi (- (+ i1 j) qd)))
                (when (and (>= bi i1-min) (< bi i1-max))
                  (let ((a (vref v0 ai))
                        (b (vref v1 bi)))
                    (vset! g0 ai
                      (+ (vref g0 ai) (* z b)))
                    (vset! g1 bi
                      (+ (vref g1 bi) (* z a)))))))))))))

(define correlate-3-1-∇-acc
  (λ (nd md qd)
    (λ (g
        v0 i0 bmd
        v1 i1 d
        vz iz b)
      (values
      #<<EOF
    int i1_min = @{i1} - @{i1} % @{nd};
    int i1_max = i1_min + @{nd};
    for(int i=0; i<@{b}; i++) {
        float z = @{vz}[@{iz}+1];
        for(int j=0; j<@{md}; j++) {
            int ai = @{i0} + i * @{md} + j;
            int bi = @{i1} + j - @{qd};
            if(bi >= i1_min && bi < i1_max) {
                @{g}[ai] += z * @{v1}[bi];
            }
        }
    }
EOF

      #<<EOF
    int i1_min = @{i1} - @{i1} % @{nd};
    int i1_max = i1_min + @{nd};
    for(int i=0; i<@{b}; i++) {
        float z = @{vz}[@{iz}+1];
        for(int j=0; j<@{md}; j++) {
            int ai = @{i0} + i * @{md} + j;
            int bi = @{i1} + j - @{qd};
            if(bi >= i1_min && bi < i1_max) {
                @{g}[bi] += z * @{v0}[ai];
            }
        }
    }
EOF
       ))))

(define correlate-shape
  (λ (bmd nd)
    (list (car bmd))))

(define correlate-3-1
  (λ (nd md qd)
    (prim2
     (correlate-3-1-ρ nd md qd)
     (correlate-3-1-ρ-acc nd md qd)
     (correlate-3-1-∇ nd md qd)
     (correlate-3-1-∇-acc nd md qd)
     correlate-shape)))

(define d-correlate
  (λ (bank signal)
    (let* ((b-m-d (last 3 (shape (ρ bank))))
           (n-d (last 2 (shape (ρ signal))))
           (d (ref n-d 1))
           (nd (* d (ref n-d 0)))
           (m (ref b-m-d 1))
           (q (/ (- m 1) 2)) ;; This is the padding.
           (qd (* q d))
           (md (* m d)))
      ((ext2 (correlate-3-1 nd md qd) 3 1) bank signal))))

(define correlate-ρ
  (λ (bank signal)
    (let* ((b-m-d (last 3 (shape (ρ bank))))
           (n-d (last 2 (shape (ρ signal))))
           (d (ref n-d 1))
           (nd (* d (ref n-d 0)))
           (m (ref b-m-d 1))
           (q (/ (- m 1) 2)) ;; This is the padding.
           (qd (* q d))
           (md (* m d)))
      ((ext2-ρ (correlate-3-1-ρ nd md qd) (correlate-3-1-ρ-acc nd md qd) 3 1 correlate-shape)
       bank signal))))

(define last
  (λ (n s)
    (refr s (- (len s) n))))

(include "test/test-G-correlate.rkt")

(provide d-correlate correlate-ρ)
