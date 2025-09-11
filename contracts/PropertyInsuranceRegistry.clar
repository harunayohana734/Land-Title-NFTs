;; Property Insurance Registry Contract
;; Manages insurance policies for land title properties
;; Provides coverage tracking, claims processing, and risk assessment

;; Error constants
(define-constant err-not-authorized (err u500))
(define-constant err-policy-not-found (err u501))
(define-constant err-policy-expired (err u502))
(define-constant err-policy-exists (err u503))
(define-constant err-insufficient-premium (err u504))
(define-constant err-claim-not-found (err u505))
(define-constant err-claim-already-processed (err u506))
(define-constant err-invalid-claim-amount (err u507))
(define-constant err-title-not-found (err u508))
(define-constant err-insurer-not-registered (err u509))
(define-constant err-policy-not-active (err u510))

;; Contract owner
(define-constant contract-owner tx-sender)

;; Data variables
(define-data-var next-policy-id uint u1)
(define-data-var next-claim-id uint u1)

;; Insurance policies registry
(define-map insurance-policies
  uint
  {
    policy-holder: principal,
    title-id: uint,
    insurer: principal,
    policy-type: (string-ascii 32),
    coverage-amount: uint,
    annual-premium: uint,
    start-date: uint,
    end-date: uint,
    risk-level: (string-ascii 16),
    deductible: uint,
    status: (string-ascii 16),
    last-premium-payment: uint,
    total-claims-paid: uint
  }
)

;; Insurance claims registry
(define-map insurance-claims
  uint
  {
    policy-id: uint,
    claimant: principal,
    claim-type: (string-ascii 32),
    claim-amount: uint,
    incident-date: uint,
    filed-date: uint,
    description: (string-ascii 256),
    evidence-hash: (optional (buff 32)),
    adjuster: (optional principal),
    status: (string-ascii 16),
    approved-amount: uint,
    settlement-date: (optional uint),
    notes: (string-ascii 256)
  }
)

;; Map policies by property title
(define-map title-policies
  uint
  (list 10 uint)
)

;; Map policies by holder
(define-map holder-policies
  principal
  (list 20 uint)
)

;; Map claims by policy
(define-map policy-claims
  uint
  (list 20 uint)
)

;; Registered insurance providers
(define-map registered-insurers
  principal
  {
    company-name: (string-ascii 64),
    license-number: (string-ascii 32),
    registration-date: uint,
    is-active: bool,
    coverage-types: (list 10 (string-ascii 32)),
    minimum-coverage: uint,
    maximum-coverage: uint
  }
)

;; Risk assessment factors by jurisdiction
(define-map risk-factors
  (string-ascii 64)
  {
    flood-risk: uint,
    fire-risk: uint,
    earthquake-risk: uint,
    crime-risk: uint,
    base-premium-rate: uint,
    last-updated: uint
  }
)

;; Read-only functions

(define-read-only (get-policy-details (policy-id uint))
  (map-get? insurance-policies policy-id)
)

(define-read-only (get-claim-details (claim-id uint))
  (map-get? insurance-claims claim-id)
)

(define-read-only (get-title-policies (title-id uint))
  (default-to (list) (map-get? title-policies title-id))
)

(define-read-only (get-holder-policies (holder principal))
  (default-to (list) (map-get? holder-policies holder))
)

(define-read-only (get-policy-claims (policy-id uint))
  (default-to (list) (map-get? policy-claims policy-id))
)

(define-read-only (get-insurer-details (insurer principal))
  (map-get? registered-insurers insurer)
)

(define-read-only (get-risk-factors (jurisdiction (string-ascii 64)))
  (map-get? risk-factors jurisdiction)
)

(define-read-only (is-policy-active (policy-id uint))
  (match (map-get? insurance-policies policy-id)
    policy (and (is-eq (get status policy) "active") 
                (> (get end-date policy) stacks-block-height))
    false
  )
)

(define-read-only (calculate-premium (title-id uint) (coverage-amount uint) (jurisdiction (string-ascii 64)))
  (match (map-get? risk-factors jurisdiction)
    factors (let
      ((base-rate (get base-premium-rate factors))
       (risk-multiplier (/ (+ (get flood-risk factors) 
                              (get fire-risk factors) 
                              (get earthquake-risk factors) 
                              (get crime-risk factors)) u4))
       (coverage-factor (/ coverage-amount u100000)))
      (ok (+ (* base-rate coverage-factor) (* risk-multiplier coverage-factor))))
    (ok u1000) ;; Default premium
  )
)

;; Public functions

(define-public (register-insurer 
  (insurer principal)
  (company-name (string-ascii 64))
  (license-number (string-ascii 32))
  (coverage-types (list 10 (string-ascii 32)))
  (minimum-coverage uint)
  (maximum-coverage uint)
  )
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (map-set registered-insurers insurer {
      company-name: company-name,
      license-number: license-number,
      registration-date: stacks-block-height,
      is-active: true,
      coverage-types: coverage-types,
      minimum-coverage: minimum-coverage,
      maximum-coverage: maximum-coverage
    })
    (ok true)
  )
)

(define-public (create-insurance-policy
  (title-id uint)
  (insurer principal)
  (policy-type (string-ascii 32))
  (coverage-amount uint)
  (duration-blocks uint)
  (risk-level (string-ascii 16))
  (deductible uint)
  )
  (let
    (
      (policy-id (var-get next-policy-id))
      (current-block stacks-block-height)
      (end-block (+ current-block duration-blocks))
      (insurer-data (unwrap! (map-get? registered-insurers insurer) err-insurer-not-registered))
      (current-title-policies (default-to (list) (map-get? title-policies title-id)))
      (current-holder-policies (default-to (list) (map-get? holder-policies tx-sender)))
    )
    
    (asserts! (get is-active insurer-data) err-insurer-not-registered)
    (asserts! (>= coverage-amount (get minimum-coverage insurer-data)) err-insufficient-premium)
    (asserts! (<= coverage-amount (get maximum-coverage insurer-data)) err-insufficient-premium)
    
    (map-set insurance-policies policy-id {
      policy-holder: tx-sender,
      title-id: title-id,
      insurer: insurer,
      policy-type: policy-type,
      coverage-amount: coverage-amount,
      annual-premium: u0, ;; Will be calculated separately
      start-date: current-block,
      end-date: end-block,
      risk-level: risk-level,
      deductible: deductible,
      status: "pending",
      last-premium-payment: u0,
      total-claims-paid: u0
    })
    
    (map-set title-policies title-id
      (unwrap-panic (as-max-len? (append current-title-policies policy-id) u10)))
    
    (map-set holder-policies tx-sender
      (unwrap-panic (as-max-len? (append current-holder-policies policy-id) u20)))
    
    (var-set next-policy-id (+ policy-id u1))
    (ok policy-id)
  )
)

(define-public (activate-policy (policy-id uint) (premium-payment uint))
  (let
    (
      (policy (unwrap! (map-get? insurance-policies policy-id) err-policy-not-found))
      (policy-holder (get policy-holder policy))
      (insurer (get insurer policy))
    )
    
    (asserts! (is-eq tx-sender policy-holder) err-not-authorized)
    (asserts! (is-eq (get status policy) "pending") err-policy-not-active)
    (asserts! (>= premium-payment u100) err-insufficient-premium)
    
    (try! (stx-transfer? premium-payment tx-sender insurer))
    
    (map-set insurance-policies policy-id
      (merge policy {
        annual-premium: premium-payment,
        status: "active",
        last-premium-payment: stacks-block-height
      }))
    
    (ok true)
  )
)

(define-public (file-insurance-claim
  (policy-id uint)
  (claim-type (string-ascii 32))
  (claim-amount uint)
  (incident-date uint)
  (description (string-ascii 256))
  (evidence-hash (optional (buff 32)))
  )
  (let
    (
      (claim-id (var-get next-claim-id))
      (policy (unwrap! (map-get? insurance-policies policy-id) err-policy-not-found))
      (current-policy-claims (default-to (list) (map-get? policy-claims policy-id)))
    )
    
    (asserts! (is-eq tx-sender (get policy-holder policy)) err-not-authorized)
    (asserts! (is-policy-active policy-id) err-policy-not-active)
    (asserts! (> claim-amount u0) err-invalid-claim-amount)
    (asserts! (<= claim-amount (get coverage-amount policy)) err-invalid-claim-amount)
    
    (map-set insurance-claims claim-id {
      policy-id: policy-id,
      claimant: tx-sender,
      claim-type: claim-type,
      claim-amount: claim-amount,
      incident-date: incident-date,
      filed-date: stacks-block-height,
      description: description,
      evidence-hash: evidence-hash,
      adjuster: none,
      status: "filed",
      approved-amount: u0,
      settlement-date: none,
      notes: ""
    })
    
    (map-set policy-claims policy-id
      (unwrap-panic (as-max-len? (append current-policy-claims claim-id) u20)))
    
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

(define-public (process-claim 
  (claim-id uint) 
  (approve bool)
  (approved-amount uint)
  (notes (string-ascii 256))
  )
  (let
    (
      (claim (unwrap! (map-get? insurance-claims claim-id) err-claim-not-found))
      (policy-id (get policy-id claim))
      (policy (unwrap! (map-get? insurance-policies policy-id) err-policy-not-found))
      (insurer (get insurer policy))
      (claimant (get claimant claim))
    )
    
    (asserts! (is-eq tx-sender insurer) err-not-authorized)
    (asserts! (is-eq (get status claim) "filed") err-claim-already-processed)
    
    (if approve
      (begin
        (try! (stx-transfer? approved-amount insurer claimant))
        (map-set insurance-policies policy-id
          (merge policy {
            total-claims-paid: (+ (get total-claims-paid policy) approved-amount)
          }))
        (map-set insurance-claims claim-id
          (merge claim {
            status: "approved",
            approved-amount: approved-amount,
            settlement-date: (some stacks-block-height),
            notes: notes
          }))
      )
      (map-set insurance-claims claim-id
        (merge claim {
          status: "denied",
          notes: notes
        }))
    )
    
    (ok true)
  )
)

(define-public (renew-policy (policy-id uint) (premium-payment uint))
  (let
    (
      (policy (unwrap! (map-get? insurance-policies policy-id) err-policy-not-found))
      (policy-holder (get policy-holder policy))
      (insurer (get insurer policy))
      (duration (- (get end-date policy) (get start-date policy)))
    )
    
    (asserts! (is-eq tx-sender policy-holder) err-not-authorized)
    (asserts! (is-eq (get status policy) "active") err-policy-not-active)
    (asserts! (>= premium-payment (get annual-premium policy)) err-insufficient-premium)
    
    (try! (stx-transfer? premium-payment tx-sender insurer))
    
    (map-set insurance-policies policy-id
      (merge policy {
        start-date: stacks-block-height,
        end-date: (+ stacks-block-height duration),
        last-premium-payment: stacks-block-height
      }))
    
    (ok true)
  )
)

(define-public (update-risk-factors 
  (jurisdiction (string-ascii 64))
  (flood-risk uint)
  (fire-risk uint)
  (earthquake-risk uint)
  (crime-risk uint)
  (base-premium-rate uint)
  )
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    
    (map-set risk-factors jurisdiction {
      flood-risk: flood-risk,
      fire-risk: fire-risk,
      earthquake-risk: earthquake-risk,
      crime-risk: crime-risk,
      base-premium-rate: base-premium-rate,
      last-updated: stacks-block-height
    })
    
    (ok true)
  )
)

(define-public (cancel-policy (policy-id uint))
  (let
    (
      (policy (unwrap! (map-get? insurance-policies policy-id) err-policy-not-found))
      (policy-holder (get policy-holder policy))
    )
    
    (asserts! (is-eq tx-sender policy-holder) err-not-authorized)
    (asserts! (is-eq (get status policy) "active") err-policy-not-active)
    
    (map-set insurance-policies policy-id
      (merge policy { status: "cancelled" }))
    
    (ok true)
  )
)
