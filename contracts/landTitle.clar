
;; title: landTitle


(define-non-fungible-token land-title uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-registered (err u103))
(define-constant err-invalid-title (err u104))
(define-constant err-not-for-sale (err u105))
(define-constant err-insufficient-payment (err u106))

(define-data-var next-title-id uint u1)

(define-constant err-rental-not-found (err u301))
(define-constant err-rental-exists (err u302))
(define-constant err-rental-not-active (err u303))
(define-constant err-rental-expired (err u304))
(define-constant err-not-tenant (err u305))
(define-constant err-rent-paid (err u306))
(define-constant err-rent-overdue (err u307))
(define-constant err-property-not-rentable (err u308))
(define-constant err-deposit-insufficient (err u309))

(define-data-var next-rental-id uint u1)

(define-map rental-agreements
  uint
  {
    title-id: uint,
    landlord: principal,
    tenant: principal,
    monthly-rent: uint,
    security-deposit: uint,
    start-date: uint,
    end-date: uint,
    next-payment-due: uint,
    status: (string-ascii 12),
    deposit-paid: bool,
    last-payment-date: uint,
    total-paid: uint,
    late-fee: uint
  }
)

(define-map landlord-rentals
  principal
  (list 20 uint)
)

(define-map tenant-rentals
  principal
  (list 10 uint)
)

(define-map title-rental-status
  uint
  {
    is-rentable: bool,
    current-rental-id: (optional uint),
    monthly-rent: uint,
    security-deposit: uint,
    rental-terms: (string-ascii 256)
  }
)

(define-map rental-payments
  uint
  (list 60 {
    payment-date: uint,
    amount: uint,
    payment-type: (string-ascii 16),
    period-start: uint,
    period-end: uint
  })
)

(define-map title-registry
  uint
  {
    owner: principal,
    property-address: (string-ascii 256),
    property-size: uint,
    registration-date: uint,
    last-transfer-date: uint,
    for-sale: bool,
    price: uint,
    property-type: (string-ascii 64),
    jurisdiction: (string-ascii 64),
    verified: bool
  }
)



(define-constant err-escrow-not-found (err u201))
(define-constant err-invalid-amount (err u202))
(define-constant err-escrow-expired (err u203))
(define-constant err-escrow-not-active (err u204))
(define-constant err-insufficient-funds (err u205))
(define-constant err-title-not-for-sale (err u206))

(define-data-var next-escrow-id uint u1)

(define-map escrow-agreements
  uint
  {
    buyer: principal,
    seller: principal,
    title-id: uint,
    amount: uint,
    deposit-date: uint,
    expiry-date: uint,
    status: (string-ascii 10),
    land-title-contract: principal
  }
)

(define-map buyer-escrows
  principal
  (list 20 uint)
)

(define-map seller-escrows
  principal
  (list 20 uint)
)

(define-map property-address-to-id
  (string-ascii 256)
  uint
)

(define-map principal-titles
  principal
  (list 100 uint)
)

(define-map title-verification-authorities
  principal
  bool
)

(define-read-only (get-title-details (title-id uint))
  (match (map-get? title-registry title-id)
    title-data (ok title-data)
    err-not-found
  )
)

(define-read-only (get-title-owner (title-id uint))
  (match (map-get? title-registry title-id)
    title-data (ok (get owner title-data))
    err-not-found
  )
)

(define-read-only (get-titles-by-owner (owner principal))
  (match (map-get? principal-titles owner)
    title-list (ok title-list)
    (ok (list))
  )
)

(define-read-only (get-title-by-address (property-address (string-ascii 256)))
  (match (map-get? property-address-to-id property-address)
    title-id (ok title-id)
    err-not-found
  )
)

(define-read-only (is-title-for-sale (title-id uint))
  (match (map-get? title-registry title-id)
    title-data (ok (get for-sale title-data))
    err-not-found
  )
)

(define-read-only (get-title-price (title-id uint))
  (match (map-get? title-registry title-id)
    title-data (ok (get price title-data))
    err-not-found
  )
)

(define-read-only (is-verification-authority (authority principal))
  (default-to false (map-get? title-verification-authorities authority))
)

(define-public (register-title 
    (property-address (string-ascii 256))
    (property-size uint)
    (property-type (string-ascii 64))
    (jurisdiction (string-ascii 64)))
  (let
    (
      (title-id (var-get next-title-id))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-none (map-get? property-address-to-id property-address)) err-already-registered)
    
    (try! (nft-mint? land-title title-id tx-sender))
    
    (map-set title-registry title-id {
      owner: tx-sender,
      property-address: property-address,
      property-size: property-size,
      registration-date: current-time,
      last-transfer-date: current-time,
      for-sale: false,
      price: u0,
      property-type: property-type,
      jurisdiction: jurisdiction,
      verified: false
    })
    
    (map-set property-address-to-id property-address title-id)
    
    (map-set principal-titles tx-sender 
      (unwrap-panic (as-max-len? 
        (append (default-to (list) (map-get? principal-titles tx-sender)) title-id)
        u100)))
    
    (var-set next-title-id (+ title-id u1))
    (ok title-id)
  )
)

(define-private (not-equal-to-id (id uint) (title-id uint))
  ;; Helper function to filter out the title ID from the list
  (not (is-eq id title-id))
)

(define-public (transfer-title (title-id uint) (recipient principal))
  (let
    (
      (current-owner (unwrap! (nft-get-owner? land-title title-id) err-not-found))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-eq tx-sender current-owner) err-unauthorized)
    
    (try! (nft-transfer? land-title title-id current-owner recipient))
    
    (match (map-get? title-registry title-id)
      title-data
        (begin
          (map-set title-registry title-id 
            (merge title-data {
              owner: recipient,
              last-transfer-date: current-time,
              for-sale: false,
              price: u0
            }))
          
        ;;   (map-set principal-titles current-owner
        ;;     (filter not-equal-to-id (default-to (list) (map-get? principal-titles current-owner))))
          
          (map-set principal-titles recipient
            (unwrap-panic (as-max-len? 
              (append (default-to (list) (map-get? principal-titles recipient)) title-id)
              u100)))
          
          (ok true))
      err-not-found
    )
  )
)(define-public (list-title-for-sale (title-id uint) (price uint))
  (let
    ((current-owner (unwrap! (nft-get-owner? land-title title-id) err-not-found)))
    (asserts! (is-eq tx-sender current-owner) err-unauthorized)
    (asserts! (> price u0) err-invalid-title)
    
    (match (map-get? title-registry title-id)
      title-data
        (begin
          (map-set title-registry title-id 
            (merge title-data {
              for-sale: true,
              price: price
            }))
          (ok true))
      err-not-found
    )
  )
)

(define-public (cancel-sale (title-id uint))
  (let
    ((current-owner (unwrap! (nft-get-owner? land-title title-id) err-not-found)))
    (asserts! (is-eq tx-sender current-owner) err-unauthorized)
    
    (match (map-get? title-registry title-id)
      title-data
        (begin
          (map-set title-registry title-id 
            (merge title-data {
              for-sale: false,
              price: u0
            }))
          (ok true))
      err-not-found
    )
  )
)

(define-public (buy-title (title-id uint))
  (let
    (
      (title-data (unwrap! (map-get? title-registry title-id) err-not-found))
      (seller (get owner title-data))
      (sale-price (get price title-data))
      (is-selling (get for-sale title-data))
    ;;   (current-time (unwrap-panic (get-stacks-block-info? block-height u0)))
    )
    (asserts! is-selling err-not-for-sale)
    (asserts! (is-eq (stx-get-balance tx-sender) sale-price) err-insufficient-payment)
    
    (try! (stx-transfer? sale-price tx-sender seller))
    (try! (nft-transfer? land-title title-id seller tx-sender))
    
    (map-set title-registry title-id 
      (merge title-data {
        owner: tx-sender,
        last-transfer-date: stacks-block-height,
        for-sale: false,
        price: u0
      }))
    
    ;; (map-set principal-titles seller
    ;;   (filter not-equal-to-id (default-to (list) (map-get? principal-titles seller))))
    
    (map-set principal-titles tx-sender
      (unwrap-panic (as-max-len? 
        (append (default-to (list) (map-get? principal-titles tx-sender)) title-id)
        u100)))
    
    (ok true)
  )
)


(define-public (add-verification-authority (authority principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set title-verification-authorities authority true)
    (ok true)
  )
)

(define-public (remove-verification-authority (authority principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-delete title-verification-authorities authority)
    (ok true)
  )
)

(define-public (verify-title (title-id uint))
  (begin
    (asserts! (is-verification-authority tx-sender) err-unauthorized)
    
    (match (map-get? title-registry title-id)
      title-data
        (begin
          (map-set title-registry title-id 
            (merge title-data { verified: true }))
          (ok true))
      err-not-found
    )
  )
)


(define-map title-history
  uint 
  (list 50 {
    timestamp: uint,
    action: (string-ascii 12),
    from: principal,
    to: (optional principal),
    price: uint
  })
)

(define-read-only (get-title-history (title-id uint))
  (match (map-get? title-history title-id)
    history (ok history)
    (ok (list))
  )
)

(define-private (record-title-event 
    (title-id uint)
    (action (string-ascii 12))
    (from principal)
    (to (optional principal))
    (price uint)
  )
  (let
    ((current-history (default-to (list) (map-get? title-history title-id))))
    (map-set title-history title-id
      (unwrap-panic (as-max-len?
        (append current-history {
          timestamp: stacks-block-height,
          action: action,
          from: from,
          to: to,
          price: price
        })
        u50)))
    true
  )
)



(define-public (get-title-verification-status (title-id uint))
  (match (map-get? title-registry title-id)
    title-data (ok (get verified title-data))
    err-not-found
  )
)


(define-map title-liens
  uint
  (list 10 {
    lender: principal,
    amount: uint,
    start-date: uint,
    end-date: uint,
    active: bool
  })
)

(define-read-only (get-title-liens (title-id uint))
  (match (map-get? title-liens title-id)
    liens (ok liens)
    (ok (list))
  )
)

(define-public (register-lien 
    (title-id uint)
    (amount uint)
    (end-date uint)
  )
  (let
    ((title-data (unwrap! (map-get? title-registry title-id) err-not-found))
     (current-liens (default-to (list) (map-get? title-liens title-id))))
    
    (asserts! (is-verification-authority tx-sender) err-unauthorized)
    
    (map-set title-liens title-id
      (unwrap-panic (as-max-len?
        (append current-liens {
          lender: tx-sender,
          amount: amount,
          start-date: stacks-block-height,
          end-date: end-date,
          active: true
        })
        u10)))
    (ok true)
  )
)


(define-read-only (get-escrow-details (escrow-id uint))
  (match (map-get? escrow-agreements escrow-id)
    escrow-data (ok escrow-data)
    err-escrow-not-found
  )
)

(define-read-only (get-buyer-escrows (buyer principal))
  (match (map-get? buyer-escrows buyer)
    escrow-list (ok escrow-list)
    (ok (list))
  )
)

(define-read-only (get-seller-escrows (seller principal))
  (match (map-get? seller-escrows seller)
    escrow-list (ok escrow-list)
    (ok (list))
  )
)

(define-public (create-escrow 
    (title-id uint)
    (seller principal)
    (amount uint)
    (duration-blocks uint)
    (land-title-contract principal)
  )
  (let
    (
      (escrow-id (var-get next-escrow-id))
      (current-block stacks-block-height)
      (expiry-block (+ current-block duration-blocks))
    )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= (stx-get-balance tx-sender) amount) err-insufficient-funds)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set escrow-agreements escrow-id {
      buyer: tx-sender,
      seller: seller,
      title-id: title-id,
      amount: amount,
      deposit-date: current-block,
      expiry-date: expiry-block,
      status: "active",
      land-title-contract: land-title-contract
    })
    
    (map-set buyer-escrows tx-sender
      (unwrap-panic (as-max-len?
        (append (default-to (list) (map-get? buyer-escrows tx-sender)) escrow-id)
        u20)))
    
    (map-set seller-escrows seller
      (unwrap-panic (as-max-len?
        (append (default-to (list) (map-get? seller-escrows seller)) escrow-id)
        u20)))
    
    (var-set next-escrow-id (+ escrow-id u1))
    (ok escrow-id)
  )
)

(define-public (complete-escrow-transfer (escrow-id uint))
  (let
    (
      (escrow-data (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found))
      (buyer (get buyer escrow-data))
      (seller (get seller escrow-data))
      (title-id (get title-id escrow-data))
      (amount (get amount escrow-data))
      (status (get status escrow-data))
      (land-title-contract (get land-title-contract escrow-data))
    )
    (asserts! (or (is-eq tx-sender buyer) (is-eq tx-sender seller)) err-unauthorized)
    (asserts! (is-eq status "active") err-escrow-not-active)
    (asserts! (<= stacks-block-height (get expiry-date escrow-data)) err-escrow-expired)
    
    (try! (as-contract (stx-transfer? amount tx-sender seller)))
    
    (map-set escrow-agreements escrow-id
      (merge escrow-data { status: "completed" }))
    
    (ok true)
  )
)

(define-public (cancel-escrow (escrow-id uint))
  (let
    (
      (escrow-data (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found))
      (buyer (get buyer escrow-data))
      (amount (get amount escrow-data))
      (status (get status escrow-data))
    )
    (asserts! (is-eq tx-sender buyer) err-unauthorized)
    (asserts! (is-eq status "active") err-escrow-not-active)
    
    (try! (as-contract (stx-transfer? amount tx-sender buyer)))
    
    (map-set escrow-agreements escrow-id
      (merge escrow-data { status: "cancelled" }))
    
    (ok true)
  )
)

(define-public (refund-expired-escrow (escrow-id uint))
  (let
    (
      (escrow-data (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found))
      (buyer (get buyer escrow-data))
      (amount (get amount escrow-data))
      (status (get status escrow-data))
      (expiry-date (get expiry-date escrow-data))
    )
    (asserts! (is-eq status "active") err-escrow-not-active)
    (asserts! (> stacks-block-height expiry-date) err-escrow-expired)
    
    (try! (as-contract (stx-transfer? amount tx-sender buyer)))
    
    (map-set escrow-agreements escrow-id
      (merge escrow-data { status: "expired" }))
    
    (ok true)
  )
)

(define-public (dispute-escrow (escrow-id uint))
  (let
    (
      (escrow-data (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found))
      (buyer (get buyer escrow-data))
      (seller (get seller escrow-data))
      (status (get status escrow-data))
    )
    (asserts! (or (is-eq tx-sender buyer) (is-eq tx-sender seller)) err-unauthorized)
    (asserts! (is-eq status "active") err-escrow-not-active)
    
    (map-set escrow-agreements escrow-id
      (merge escrow-data { status: "disputed" }))
    
    (ok true)
  )
)

(define-public (resolve-dispute 
    (escrow-id uint)
    (award-to-buyer bool)
  )
  (let
    (
      (escrow-data (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found))
      (buyer (get buyer escrow-data))
      (seller (get seller escrow-data))
      (amount (get amount escrow-data))
      (status (get status escrow-data))
      (recipient (if award-to-buyer buyer seller))
    )
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (asserts! (is-eq status "disputed") err-escrow-not-active)
    
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    
    (map-set escrow-agreements escrow-id
      (merge escrow-data { status: "resolved" }))
    
    (ok true)
  )
)

(define-read-only (get-escrow-balance (escrow-id uint))
  (let
    ((escrow-data (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found)))
    (if (is-eq (get status escrow-data) "active")
      (ok (get amount escrow-data))
      (ok u0)
    )
  )
)

(define-read-only (is-escrow-active (escrow-id uint))
  (match (map-get? escrow-agreements escrow-id)
    escrow-data (ok (is-eq (get status escrow-data) "active"))
    err-escrow-not-found
  )
)


(define-read-only (get-rental-details (rental-id uint))
  (match (map-get? rental-agreements rental-id)
    rental-data (ok rental-data)
    err-rental-not-found
  )
)

(define-read-only (get-title-rental-status (title-id uint))
  (match (map-get? title-rental-status title-id)
    rental-status (ok rental-status)
    (ok {
      is-rentable: false,
      current-rental-id: none,
      monthly-rent: u0,
      security-deposit: u0,
      rental-terms: ""
    })
  )
)

(define-read-only (get-landlord-rentals (landlord principal))
  (match (map-get? landlord-rentals landlord)
    rental-list (ok rental-list)
    (ok (list))
  )
)

(define-read-only (get-tenant-rentals (tenant principal))
  (match (map-get? tenant-rentals tenant)
    rental-list (ok rental-list)
    (ok (list))
  )
)

(define-read-only (get-rental-payment-history (rental-id uint))
  (match (map-get? rental-payments rental-id)
    payment-history (ok payment-history)
    (ok (list))
  )
)

(define-read-only (calculate-rent-due (rental-id uint))
  (match (map-get? rental-agreements rental-id)
    rental-data
      (let
        (
          (current-time stacks-block-height)
          (next-due (get next-payment-due rental-data))
          (monthly-rent (get monthly-rent rental-data))
          (late-fee (get late-fee rental-data))
        )
        (if (> current-time next-due)
          (ok (+ monthly-rent late-fee))
          (ok monthly-rent)
        )
      )
    err-rental-not-found
  )
)

(define-public (make-property-rentable 
    (title-id uint)
    (monthly-rent uint)
    (security-deposit uint)
    (rental-terms (string-ascii 256))
  )
  (let
    ((title-data (unwrap! (map-get? title-registry title-id) err-not-found))
     (title-owner (get owner title-data)))
    
    (asserts! (is-eq tx-sender title-owner) err-unauthorized)
    (asserts! (> monthly-rent u0) err-invalid-amount)
    
    (map-set title-rental-status title-id {
      is-rentable: true,
      current-rental-id: none,
      monthly-rent: monthly-rent,
      security-deposit: security-deposit,
      rental-terms: rental-terms
    })
    
    (ok true)
  )
)

(define-public (create-rental-agreement 
    (title-id uint)
    (tenant principal)
    (duration-blocks uint)
    (late-fee uint)
  )
  (let
    (
      (title-data (unwrap! (map-get? title-registry title-id) err-not-found))
      (rental-status (unwrap! (map-get? title-rental-status title-id) err-property-not-rentable))
      (rental-id (var-get next-rental-id))
      (current-time stacks-block-height)
      (end-date (+ current-time duration-blocks))
      (monthly-rent (get monthly-rent rental-status))
      (security-deposit (get security-deposit rental-status))
    )
    
    (asserts! (is-eq tx-sender (get owner title-data)) err-unauthorized)
    (asserts! (get is-rentable rental-status) err-property-not-rentable)
    (asserts! (is-none (get current-rental-id rental-status)) err-rental-exists)
    
    (map-set rental-agreements rental-id {
      title-id: title-id,
      landlord: tx-sender,
      tenant: tenant,
      monthly-rent: monthly-rent,
      security-deposit: security-deposit,
      start-date: current-time,
      end-date: end-date,
      next-payment-due: (+ current-time u4320),
      status: "pending",
      deposit-paid: false,
      last-payment-date: u0,
      total-paid: u0,
      late-fee: late-fee
    })
    
    (map-set landlord-rentals tx-sender
      (unwrap-panic (as-max-len?
        (append (default-to (list) (map-get? landlord-rentals tx-sender)) rental-id)
        u20)))
    
    (map-set tenant-rentals tenant
      (unwrap-panic (as-max-len?
        (append (default-to (list) (map-get? tenant-rentals tenant)) rental-id)
        u10)))
    
    (map-set title-rental-status title-id
      (merge rental-status { current-rental-id: (some rental-id) }))
    
    (var-set next-rental-id (+ rental-id u1))
    (ok rental-id)
  )
)

(define-public (pay-security-deposit (rental-id uint))
  (let
    (
      (rental-data (unwrap! (map-get? rental-agreements rental-id) err-rental-not-found))
      (tenant (get tenant rental-data))
      (deposit-amount (get security-deposit rental-data))
      (landlord (get landlord rental-data))
    )
    
    (asserts! (is-eq tx-sender tenant) err-not-tenant)
    (asserts! (is-eq (get status rental-data) "pending") err-rental-not-active)
    (asserts! (>= (stx-get-balance tx-sender) deposit-amount) err-deposit-insufficient)
    
    (try! (stx-transfer? deposit-amount tx-sender landlord))
    
    (map-set rental-agreements rental-id
      (merge rental-data { 
        deposit-paid: true,
        status: "active"
      }))
    
    (ok true)
  )
)

(define-public (pay-rent (rental-id uint))
  (let
    (
      (rental-data (unwrap! (map-get? rental-agreements rental-id) err-rental-not-found))
      (tenant (get tenant rental-data))
      (landlord (get landlord rental-data))
      (monthly-rent (get monthly-rent rental-data))
      (late-fee (get late-fee rental-data))
      (next-due (get next-payment-due rental-data))
      (current-time stacks-block-height)
      (is-late (> current-time next-due))
      (payment-amount (if is-late (+ monthly-rent late-fee) monthly-rent))
      (current-payments (default-to (list) (map-get? rental-payments rental-id)))
    )
    
    (asserts! (is-eq tx-sender tenant) err-not-tenant)
    (asserts! (is-eq (get status rental-data) "active") err-rental-not-active)
    (asserts! (>= (stx-get-balance tx-sender) payment-amount) err-insufficient-funds)
    
    (try! (stx-transfer? payment-amount tx-sender landlord))
    
    (map-set rental-payments rental-id
      (unwrap-panic (as-max-len?
        (append current-payments {
          payment-date: current-time,
          amount: payment-amount,
          payment-type: (if is-late "late-payment" "regular"),
          period-start: current-time,
          period-end: (+ current-time u4320)
        })
        u60)))
    
    (map-set rental-agreements rental-id
      (merge rental-data {
        next-payment-due: (+ current-time u4320),
        last-payment-date: current-time,
        total-paid: (+ (get total-paid rental-data) payment-amount)
      }))
    
    (ok true)
  )
)

(define-public (terminate-rental (rental-id uint))
  (let
    (
      (rental-data (unwrap! (map-get? rental-agreements rental-id) err-rental-not-found))
      (landlord (get landlord rental-data))
      (tenant (get tenant rental-data))
      (title-id (get title-id rental-data))
    )
    
    (asserts! (or (is-eq tx-sender landlord) (is-eq tx-sender tenant)) err-unauthorized)
    (asserts! (is-eq (get status rental-data) "active") err-rental-not-active)
    
    (map-set rental-agreements rental-id
      (merge rental-data { status: "terminated" }))
    
    (match (map-get? title-rental-status title-id)
      rental-status
        (map-set title-rental-status title-id
          (merge rental-status { current-rental-id: none }))
      true)
    
    (ok true)
  )
)

(define-public (evict-tenant (rental-id uint))
  (let
    (
      (rental-data (unwrap! (map-get? rental-agreements rental-id) err-rental-not-found))
      (landlord (get landlord rental-data))
      (next-due (get next-payment-due rental-data))
      (current-time stacks-block-height)
      (title-id (get title-id rental-data))
    )
    
    (asserts! (is-eq tx-sender landlord) err-unauthorized)
    (asserts! (is-eq (get status rental-data) "active") err-rental-not-active)
    (asserts! (> current-time (+ next-due u1440)) err-rent-overdue)
    
    (map-set rental-agreements rental-id
      (merge rental-data { status: "evicted" }))
    
    (match (map-get? title-rental-status title-id)
      rental-status
        (map-set title-rental-status title-id
          (merge rental-status { current-rental-id: none }))
      true)
    
    (ok true)
  )
)

(define-public (extend-rental 
    (rental-id uint)
    (additional-blocks uint)
  )
  (let
    (
      (rental-data (unwrap! (map-get? rental-agreements rental-id) err-rental-not-found))
      (landlord (get landlord rental-data))
      (tenant (get tenant rental-data))
      (current-end-date (get end-date rental-data))
    )
    
    (asserts! (or (is-eq tx-sender landlord) (is-eq tx-sender tenant)) err-unauthorized)
    (asserts! (is-eq (get status rental-data) "active") err-rental-not-active)
    
    (map-set rental-agreements rental-id
      (merge rental-data { 
        end-date: (+ current-end-date additional-blocks)
      }))
    
    (ok true)
  )
)