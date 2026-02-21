;; TokenSoar Supply Chain Batch Tokenization Platform

;;---------------------------------------------------------------------------
;; Constants
;;---------------------------------------------------------------------------

(define-constant CONTRACT-OWNER tx-sender)

(define-constant ERR-NOT-AUTHORIZED        (err u100))
(define-constant ERR-BATCH-NOT-FOUND       (err u101))
(define-constant ERR-ALREADY-EXISTS        (err u102))
(define-constant ERR-INVALID-SCORE         (err u103))
(define-constant ERR-ATTESTATION-LOCKED    (err u104))
(define-constant ERR-INSUFFICIENT-STAKE    (err u105))
(define-constant ERR-INVALID-PARAMS        (err u106))
(define-constant ERR-ATTESTATION-NOT-FOUND (err u107))

;; Compliance score range: 0-100
(define-constant MAX-SCORE u100)
(define-constant MIN-STAKE  u1000000) ;; 1 STX in micro-STX

;;---------------------------------------------------------------------------
;; Data Maps and Variables
;;---------------------------------------------------------------------------

;; Auto-incrementing token ID counter
(define-data-var last-token-id uint u0)

;; Batch token core metadata (NFT-like record)
(define-map batches
  { token-id: uint }
  {
    owner:            principal,
    origin:           (string-ascii 64),
    product-type:     (string-ascii 32),  ;; e.g. "pharma", "food", "luxury"
    created-at:       uint,               ;; block height
    compliance-score: uint,               ;; 0-100
    shelf-life-blocks: uint,              ;; predicted shelf life in blocks
    active:           bool
  }
)

;; Environmental exposure log per batch
(define-map environmental-logs
  { token-id: uint, log-index: uint }
  {
    recorder:    principal,
    temperature: int,   ;; Celsius * 10 to avoid decimals (e.g. 205 = 20.5C)
    humidity:    uint,  ;; percentage 0-100
    recorded-at: uint   ;; block height
  }
)

(define-map batch-log-count
  { token-id: uint }
  { count: uint }
)

;; Multi-signature batch creation approvals
;; Requires a configurable threshold of approvers before minting
(define-map batch-approvals
  { token-id: uint, approver: principal }
  { approved: bool }
)

(define-map batch-approval-count
  { token-id: uint }
  { count: uint }
)

;; Required approvals threshold (contract-wide setting)
(define-data-var approval-threshold uint u2)

;; Time-locked quality attestations
(define-map attestations
  { token-id: uint, attestation-id: uint }
  {
    attester:   principal,
    quality:    uint,       ;; 0-100
    note:       (string-ascii 128),
    unlock-at:  uint,       ;; block height when attestation becomes public
    revealed:   bool
  }
)

(define-map batch-attestation-count
  { token-id: uint }
  { count: uint }
)

;; Reputation staking - suppliers lock STX to vouch for batch accuracy
(define-map stakes
  { staker: principal, token-id: uint }
  {
    amount:     uint,
    staked-at:  uint,
    slashed:    bool
  }
)

;; Total staked per batch (used for pricing signals)
(define-map batch-total-stake
  { token-id: uint }
  { total: uint }
)

;; Registered oracles that can push compliance score updates
(define-map oracles
  { oracle: principal }
  { active: bool }
)

;; Transfer allowances (simple operator approval)
(define-map token-operators
  { token-id: uint, operator: principal }
  { approved: bool }
)

;;---------------------------------------------------------------------------
;; Private Helpers
;;---------------------------------------------------------------------------

(define-private (is-oracle (caller principal))
  (default-to false (get active (map-get? oracles { oracle: caller })))
)

(define-private (batch-exists (token-id uint))
  (is-some (map-get? batches { token-id: token-id }))
)

(define-private (get-log-count (token-id uint))
  (default-to u0 (get count (map-get? batch-log-count { token-id: token-id })))
)

(define-private (get-attestation-count (token-id uint))
  (default-to u0 (get count (map-get? batch-attestation-count { token-id: token-id })))
)

(define-private (get-approval-count (token-id uint))
  (default-to u0 (get count (map-get? batch-approval-count { token-id: token-id })))
)

(define-private (get-batch-stake (token-id uint))
  (default-to u0 (get total (map-get? batch-total-stake { token-id: token-id })))
)

;;---------------------------------------------------------------------------
;; Admin Functions
;;---------------------------------------------------------------------------

;; Register or deactivate an oracle
(define-public (set-oracle (oracle principal) (active bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set oracles { oracle: oracle } { active: active })
    (ok true)
  )
)

;; Update the multi-sig approval threshold
(define-public (set-approval-threshold (threshold uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (>= threshold u1) ERR-INVALID-PARAMS)
    (var-set approval-threshold threshold)
    (ok true)
  )
)

;;---------------------------------------------------------------------------
;; Batch Creation (Multi-Signature Flow)
;;---------------------------------------------------------------------------

;; Step 1: Propose a new batch. Assigns a pending token-id.
;; The batch is inactive until enough approvals are collected.
(define-public (propose-batch
    (origin          (string-ascii 64))
    (product-type    (string-ascii 32))
    (shelf-life-blocks uint))
  (let ((new-id (+ (var-get last-token-id) u1)))
    (asserts! (not (batch-exists new-id)) ERR-ALREADY-EXISTS)
    (var-set last-token-id new-id)
    (map-set batches { token-id: new-id }
      {
        owner:            tx-sender,
        origin:           origin,
        product-type:     product-type,
        created-at:       block-height,
        compliance-score: u100,
        shelf-life-blocks: shelf-life-blocks,
        active:           false
      }
    )
    (map-set batch-approval-count { token-id: new-id } { count: u0 })
    (ok new-id)
  )
)

;; Step 2: Approvers call this to sign off on a pending batch.
;; Once the threshold is reached the batch becomes active.
(define-public (approve-batch (token-id uint))
  (let (
    (batch      (unwrap! (map-get? batches { token-id: token-id }) ERR-BATCH-NOT-FOUND))
    (already    (default-to false (get approved (map-get? batch-approvals { token-id: token-id, approver: tx-sender }))))
    (cur-count  (get-approval-count token-id))
    (new-count  (+ cur-count u1))
    (threshold  (var-get approval-threshold))
  )
    (asserts! (not already) ERR-ALREADY-EXISTS)
    (map-set batch-approvals { token-id: token-id, approver: tx-sender } { approved: true })
    (map-set batch-approval-count { token-id: token-id } { count: new-count })
    ;; Activate batch when threshold is met
    (if (>= new-count threshold)
      (map-set batches { token-id: token-id }
        (merge batch { active: true })
      )
      false
    )
    (ok new-count)
  )
)

;;---------------------------------------------------------------------------
;; Token Transfer
;;---------------------------------------------------------------------------

;; Approve an operator to transfer a specific token
(define-public (set-operator (token-id uint) (operator principal) (approved bool))
  (let ((batch (unwrap! (map-get? batches { token-id: token-id }) ERR-BATCH-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner batch)) ERR-NOT-AUTHORIZED)
    (map-set token-operators { token-id: token-id, operator: operator } { approved: approved })
    (ok true)
  )
)

;; Transfer ownership of a batch token
(define-public (transfer (token-id uint) (recipient principal))
  (let (
    (batch    (unwrap! (map-get? batches { token-id: token-id }) ERR-BATCH-NOT-FOUND))
    (is-owner (is-eq tx-sender (get owner batch)))
    (is-op    (default-to false (get approved (map-get? token-operators { token-id: token-id, operator: tx-sender }))))
  )
    (asserts! (get active batch) ERR-NOT-AUTHORIZED)
    (asserts! (or is-owner is-op) ERR-NOT-AUTHORIZED)
    (map-set batches { token-id: token-id } (merge batch { owner: recipient }))
    ;; Revoke operator approval on transfer
    (map-set token-operators { token-id: token-id, operator: tx-sender } { approved: false })
    (ok true)
  )
)

;;---------------------------------------------------------------------------
;; Oracle-Driven Compliance Score Updates
;;---------------------------------------------------------------------------

;; Oracle pushes a new compliance score (0-100) based on real-time conditions
(define-public (update-compliance-score (token-id uint) (new-score uint))
  (let ((batch (unwrap! (map-get? batches { token-id: token-id }) ERR-BATCH-NOT-FOUND)))
    (asserts! (is-oracle tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-score MAX-SCORE) ERR-INVALID-SCORE)
    (map-set batches { token-id: token-id } (merge batch { compliance-score: new-score }))
    (ok true)
  )
)

;;---------------------------------------------------------------------------
;; Environmental Logging
;;---------------------------------------------------------------------------

;; Any authorized party (owner or oracle) logs environmental conditions
(define-public (log-environment
    (token-id    uint)
    (temperature int)
    (humidity    uint))
  (let (
    (batch     (unwrap! (map-get? batches { token-id: token-id }) ERR-BATCH-NOT-FOUND))
    (log-idx   (get-log-count token-id))
  )
    (asserts! (or (is-eq tx-sender (get owner batch)) (is-oracle tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (<= humidity u100) ERR-INVALID-PARAMS)
    (map-set environmental-logs
      { token-id: token-id, log-index: log-idx }
      {
        recorder:    tx-sender,
        temperature: temperature,
        humidity:    humidity,
        recorded-at: block-height
      }
    )
    (map-set batch-log-count { token-id: token-id } { count: (+ log-idx u1) })
    (ok log-idx)
  )
)

;;---------------------------------------------------------------------------
;; Time-Locked Quality Attestations
;;---------------------------------------------------------------------------

;; Create a time-locked attestation. The note/quality is stored but gated
;; by the unlock-at block height before it can be revealed.
(define-public (create-attestation
    (token-id   uint)
    (quality    uint)
    (note       (string-ascii 128))
    (unlock-at  uint))
  (let (
    (batch   (unwrap! (map-get? batches { token-id: token-id }) ERR-BATCH-NOT-FOUND))
    (att-idx (get-attestation-count token-id))
  )
    (asserts! (get active batch) ERR-NOT-AUTHORIZED)
    (asserts! (<= quality MAX-SCORE) ERR-INVALID-SCORE)
    (asserts! (> unlock-at block-height) ERR-INVALID-PARAMS)
    (map-set attestations
      { token-id: token-id, attestation-id: att-idx }
      {
        attester:  tx-sender,
        quality:   quality,
        note:      note,
        unlock-at: unlock-at,
        revealed:  false
      }
    )
    (map-set batch-attestation-count { token-id: token-id } { count: (+ att-idx u1) })
    (ok att-idx)
  )
)

;; Reveal an attestation once its time lock has passed
(define-public (reveal-attestation (token-id uint) (attestation-id uint))
  (let (
    (att (unwrap! (map-get? attestations { token-id: token-id, attestation-id: attestation-id }) ERR-ATTESTATION-NOT-FOUND))
  )
    (asserts! (>= block-height (get unlock-at att)) ERR-ATTESTATION-LOCKED)
    (asserts! (not (get revealed att)) ERR-ALREADY-EXISTS)
    (map-set attestations
      { token-id: token-id, attestation-id: attestation-id }
      (merge att { revealed: true })
    )
    (ok true)
  )
)

;;---------------------------------------------------------------------------
;; Reputation Staking
;;---------------------------------------------------------------------------

;; Stake STX against a batch to signal confidence in its accuracy.
;; Stake is locked in the contract.
(define-public (stake-on-batch (token-id uint) (amount uint))
  (let (
    (batch       (unwrap! (map-get? batches { token-id: token-id }) ERR-BATCH-NOT-FOUND))
    (cur-stake   (get-batch-stake token-id))
    (existing    (map-get? stakes { staker: tx-sender, token-id: token-id }))
  )
    (asserts! (get active batch) ERR-NOT-AUTHORIZED)
    (asserts! (>= amount MIN-STAKE) ERR-INSUFFICIENT-STAKE)
    (asserts! (is-none existing) ERR-ALREADY-EXISTS)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set stakes
      { staker: tx-sender, token-id: token-id }
      { amount: amount, staked-at: block-height, slashed: false }
    )
    (map-set batch-total-stake { token-id: token-id } { total: (+ cur-stake amount) })
    (ok true)
  )
)

;; Contract owner can slash a stake if the supplier reported inaccurate data
(define-public (slash-stake (staker principal) (token-id uint))
  (let (
    (stake-entry (unwrap! (map-get? stakes { staker: staker, token-id: token-id }) ERR-BATCH-NOT-FOUND))
    (cur-stake   (get-batch-stake token-id))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (not (get slashed stake-entry)) ERR-INVALID-PARAMS)
    (map-set stakes
      { staker: staker, token-id: token-id }
      (merge stake-entry { slashed: true })
    )
    ;; Slashed funds remain in contract (could be sent to treasury in production)
    (map-set batch-total-stake
      { token-id: token-id }
      { total: (if (>= cur-stake (get amount stake-entry))
                 (- cur-stake (get amount stake-entry))
                 u0) }
    )
    (ok true)
  )
)

;; Withdraw a non-slashed stake (simple withdrawal, no lock period here)
(define-public (withdraw-stake (token-id uint))
  (let (
    (stake-entry (unwrap! (map-get? stakes { staker: tx-sender, token-id: token-id }) ERR-BATCH-NOT-FOUND))
    (amount      (get amount stake-entry))
    (cur-stake   (get-batch-stake token-id))
  )
    (asserts! (not (get slashed stake-entry)) ERR-NOT-AUTHORIZED)
    (map-delete stakes { staker: tx-sender, token-id: token-id })
    (map-set batch-total-stake
      { token-id: token-id }
      { total: (if (>= cur-stake amount) (- cur-stake amount) u0) }
    )
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (ok amount)
  )
)

;;---------------------------------------------------------------------------
;; Read-Only Functions
;;---------------------------------------------------------------------------

(define-read-only (get-batch (token-id uint))
  (map-get? batches { token-id: token-id })
)

(define-read-only (get-compliance-score (token-id uint))
  (match (map-get? batches { token-id: token-id })
    batch (some (get compliance-score batch))
    none
  )
)

(define-read-only (get-env-log (token-id uint) (log-index uint))
  (map-get? environmental-logs { token-id: token-id, log-index: log-index })
)

(define-read-only (get-attestation (token-id uint) (attestation-id uint))
  (let ((att (map-get? attestations { token-id: token-id, attestation-id: attestation-id })))
    ;; Return attestation details; if not yet revealed, mask sensitive fields
    (match att
      entry
      (if (get revealed entry)
        (some entry)
        (some (merge entry { quality: u0, note: "" }))
      )
      none
    )
  )
)

(define-read-only (get-stake-info (staker principal) (token-id uint))
  (map-get? stakes { staker: staker, token-id: token-id })
)

(define-read-only (get-total-stake (token-id uint))
  (get-batch-stake token-id)
)

(define-read-only (get-last-token-id)
  (var-get last-token-id)
)

(define-read-only (is-batch-active (token-id uint))
  (match (map-get? batches { token-id: token-id })
    batch (get active batch)
    false
  )
)
