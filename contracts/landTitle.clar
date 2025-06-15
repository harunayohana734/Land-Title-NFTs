
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