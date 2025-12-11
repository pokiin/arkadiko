;; NodeSats Lending Pool - Simple P2P Micro-Lend for BTC Node Runners
;; Fork-inspired minimal contract. Deploys on Stacks L2, settles to BTC.
;; Collateral: Signed node uptime proof via oracle.
;; Fees: 0.5% to protocol wallet on every repayment.

(define-data-var protocol-fee-percent uint u50) ;; 0.5% = 50 basis points (out of 10,000)
(define-data-var protocol-wallet principal SP3X4SQFXY9KS44PBKHX9E54NJHDG28WD76AAM8YX) ;; ← REPLACE WITH YOUR LEATHER STX ADDRESS FROM STEP 1

;; Simple map for liquidity pool (lender deposits)
(define-map pool uint uint) ;; key: lender tx-sender hash, value: supplied amount in micro-STX (sats equiv)

;; Map for active loans: borrower -> (amount borrowed, collateral node-id, due-date, interest-rate)
(define-map loans 
  (tuple (borrower principal) (node-id (string-ascii 64)))
  (tuple (amount uint) (due uint) (rate uint))
)

;; Events
(define-public (supply (amount uint))
  (let ((sender tx-sender))
    (asserts! (> amount u0) (err u101)) ;; Can't supply 0
    (try! (stx-transfer? amount tx-sender .nodesats-treasury)) ;; Temp treasury contract for pool (deploy separately if needed)
    (map-set pool (get sender (hash160 sender)) (+ (map-get? pool (get sender (hash160 sender))) amount))
    (print { type: "supply", sender: sender, amount: amount })
    (ok true)
  )
)

;; Borrow function with node collateral check
(define-public (borrow (amount uint) (node-id (string-ascii 64)) (duration-days uint))
  (let* (
    (borrower tx-sender)
    (due (+ block-height (* duration-days u144))) ;; ~144 blocks/day
    (rate u200) ;; 2% interest flat
    (total-repay (+ amount (* amount (/ rate u10000))))
  )
    ;; Verify node uptime via simple oracle call (assumes SP000...oracle-node-uptime contract exists; free/public in 2025)
    (asserts! (contract-call? 'SP000000000000000000002Q6VF78.oracle-node-uptime verify node-id) (err u102)) ;; Replace with real oracle if needed
    (asserts! (> amount u0) (err u101))
    (map-set loans { borrower: borrower, node-id: node-id } { amount: amount, due: due, rate: rate })
    (print { type: "borrow", borrower: borrower, amount: amount, node-id: node-id })
    (ok total-repay) ;; Borrower sees what to repay
  )
)

;; Repay function - pays interest + fee
(define-public (repay (amount uint) (node-id (string-ascii 64)))
  (let* (
    (borrower tx-sender)
    (loan (unwrap! (map-get? loans { borrower: borrower, node-id: node-id }) (err u103)))
    (principal (get amount loan))
    (interest (* principal (get rate loan) (/ u1 u10000)))
    (total (+ principal interest))
    (fee (* total (var-get protocol-fee-percent) (/ u1 u10000))) ;; 0.5% fee
    (net-to-pool (- total fee))
  )
    (asserts! (>= amount total) (err u104)) ;; Must repay full
    ;; Transfer to pool (simplified; in prod, distribute to lenders pro-rata)
    (try! (stx-transfer? net-to-pool tx-sender .nodesats-treasury))
    ;; Fee to protocol
    (try! (stx-transfer? fee tx-sender (var-get protocol-wallet)))
    (map-delete loans { borrower: borrower, node-id: node-id })
    (print { type: "repay", borrower: borrower, amount: total, fee: fee })
    (ok { repaid: total, fee-earned: fee })
  )
)

;; Liquidate on default (slash collateral - simplified, calls oracle to burn/sl ash node proof)
(define-public (liquidate (borrower principal) (node-id (string-ascii 64)))
  (let ((loan (unwrap! (map-get? loans { borrower: borrower, node-id: node-id }) (err u105)))
        (current-height block-height)
        (due (get due loan)))
    (asserts! (> current-height due) (err u106)) ;; Only if overdue
    ;; Oracle slashes collateral (e.g., marks node invalid, rewards liquidator)
    (try! (contract-call? 'SP000000000000000000002Q6VF78.oracle-node-uptime slash node-id))
    (map-delete loans { borrower: borrower, node-id: node-id })
    (print { type: "liquidate", borrower: borrower })
    (ok true)
  )
)

;; Admin: Update fee wallet (only deployer)
(define-public (set-protocol-wallet (new-wallet principal))
  (asserts! (is-eq tx-sender (var-get protocol-wallet)) (err u107))
  (var-set protocol-wallet new-wallet)
  (ok true)
)
