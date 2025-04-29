
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