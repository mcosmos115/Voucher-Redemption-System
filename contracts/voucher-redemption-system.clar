(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_VOUCHER_NOT_FOUND (err u101))
(define-constant ERR_VOUCHER_ALREADY_REDEEMED (err u102))
(define-constant ERR_VOUCHER_EXPIRED (err u103))
(define-constant ERR_INVALID_AMOUNT (err u104))
(define-constant ERR_INSUFFICIENT_BALANCE (err u105))
(define-constant ERR_PRODUCT_NOT_FOUND (err u106))
(define-constant ERR_PRODUCT_OUT_OF_STOCK (err u107))

(define-data-var voucher-counter uint u0)
(define-data-var product-counter uint u0)

(define-map vouchers
  { voucher-id: uint }
  {
    owner: principal,
    product-id: uint,
    value: uint,
    expiry-block: uint,
    redeemed: bool,
    redeemed-at: (optional uint),
    created-at: uint
  }
)

(define-map products
  { product-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    price: uint,
    stock: uint,
    active: bool,
    created-by: principal
  }
)

(define-map user-vouchers
  { user: principal }
  { voucher-ids: (list 100 uint) }
)

(define-map redemption-history
  { voucher-id: uint }
  {
    redeemed-by: principal,
    product-id: uint,
    redeemed-at: uint,
    transaction-hash: (buff 32)
  }
)

(define-read-only (get-voucher (voucher-id uint))
  (map-get? vouchers { voucher-id: voucher-id })
)

(define-read-only (get-product (product-id uint))
  (map-get? products { product-id: product-id })
)

(define-read-only (get-user-vouchers (user principal))
  (default-to { voucher-ids: (list) } (map-get? user-vouchers { user: user }))
)

(define-read-only (get-redemption-history (voucher-id uint))
  (map-get? redemption-history { voucher-id: voucher-id })
)

(define-read-only (get-voucher-counter)
  (var-get voucher-counter)
)

(define-read-only (get-product-counter)
  (var-get product-counter)
)

(define-read-only (is-voucher-valid (voucher-id uint))
  (match (get-voucher voucher-id)
    voucher-data
    (and
      (not (get redeemed voucher-data))
      (> (get expiry-block voucher-data) stacks-block-height)
    )
    false
  )
)

(define-read-only (can-redeem-voucher (voucher-id uint) (user principal))
  (match (get-voucher voucher-id)
    voucher-data
    (and
      (is-eq (get owner voucher-data) user)
      (is-voucher-valid voucher-id)
      (match (get-product (get product-id voucher-data))
        product-data
        (and
          (get active product-data)
          (> (get stock product-data) u0)
          (>= (get value voucher-data) (get price product-data))
        )
        false
      )
    )
    false
  )
)

(define-public (create-product (name (string-ascii 50)) (description (string-ascii 200)) (price uint) (stock uint))
  (let
    (
      (new-product-id (+ (var-get product-counter) u1))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> price u0) ERR_INVALID_AMOUNT)
    (map-set products
      { product-id: new-product-id }
      {
        name: name,
        description: description,
        price: price,
        stock: stock,
        active: true,
        created-by: tx-sender
      }
    )
    (var-set product-counter new-product-id)
    (ok new-product-id)
  )
)

(define-public (update-product-stock (product-id uint) (new-stock uint))
  (match (get-product product-id)
    product-data
    (begin
      (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
      (map-set products
        { product-id: product-id }
        (merge product-data { stock: new-stock })
      )
      (ok true)
    )
    ERR_PRODUCT_NOT_FOUND
  )
)

(define-public (toggle-product-status (product-id uint))
  (match (get-product product-id)
    product-data
    (begin
      (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
      (map-set products
        { product-id: product-id }
        (merge product-data { active: (not (get active product-data)) })
      )
      (ok (not (get active product-data)))
    )
    ERR_PRODUCT_NOT_FOUND
  )
)

(define-public (issue-voucher (recipient principal) (product-id uint) (value uint) (validity-blocks uint))
  (let
    (
      (new-voucher-id (+ (var-get voucher-counter) u1))
      (expiry-block (+ stacks-block-height validity-blocks))
      (current-vouchers (get voucher-ids (get-user-vouchers recipient)))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> value u0) ERR_INVALID_AMOUNT)
    (asserts! (> validity-blocks u0) ERR_INVALID_AMOUNT)
    (asserts! (is-some (get-product product-id)) ERR_PRODUCT_NOT_FOUND)
    (map-set vouchers
      { voucher-id: new-voucher-id }
      {
        owner: recipient,
        product-id: product-id,
        value: value,
        expiry-block: expiry-block,
        redeemed: false,
        redeemed-at: none,
        created-at: stacks-block-height
      }
    )
    (map-set user-vouchers
      { user: recipient }
      { voucher-ids: (unwrap! (as-max-len? (append current-vouchers new-voucher-id) u100) ERR_INSUFFICIENT_BALANCE) }
    )
    (var-set voucher-counter new-voucher-id)
    (ok new-voucher-id)
  )
)

(define-public (redeem-voucher (voucher-id uint))
  (let
    (
      (voucher-data (unwrap! (get-voucher voucher-id) ERR_VOUCHER_NOT_FOUND))
      (product-data (unwrap! (get-product (get product-id voucher-data)) ERR_PRODUCT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner voucher-data)) ERR_UNAUTHORIZED)
    (asserts! (not (get redeemed voucher-data)) ERR_VOUCHER_ALREADY_REDEEMED)
    (asserts! (> (get expiry-block voucher-data) stacks-block-height) ERR_VOUCHER_EXPIRED)
    (asserts! (get active product-data) ERR_PRODUCT_NOT_FOUND)
    (asserts! (> (get stock product-data) u0) ERR_PRODUCT_OUT_OF_STOCK)
    (asserts! (>= (get value voucher-data) (get price product-data)) ERR_INSUFFICIENT_BALANCE)
    (map-set vouchers
      { voucher-id: voucher-id }
      (merge voucher-data {
        redeemed: true,
        redeemed-at: (some stacks-block-height)
      })
    )
    (map-set products
      { product-id: (get product-id voucher-data) }
      (merge product-data { stock: (- (get stock product-data) u1) })
    )
    (map-set redemption-history
      { voucher-id: voucher-id }
      {
        redeemed-by: tx-sender,
        product-id: (get product-id voucher-data),
        redeemed-at: stacks-block-height,
        transaction-hash: 0x0000000000000000000000000000000000000000000000000000000000000000
      }
    )
    (ok true)
  )
)

(define-public (transfer-voucher (voucher-id uint) (new-owner principal))
  (let
    (
      (voucher-data (unwrap! (get-voucher voucher-id) ERR_VOUCHER_NOT_FOUND))
      (current-owner-vouchers (get voucher-ids (get-user-vouchers (get owner voucher-data))))
      (new-owner-vouchers (get voucher-ids (get-user-vouchers new-owner)))
    )
    (asserts! (is-eq tx-sender (get owner voucher-data)) ERR_UNAUTHORIZED)
    (asserts! (not (get redeemed voucher-data)) ERR_VOUCHER_ALREADY_REDEEMED)
    (asserts! (> (get expiry-block voucher-data) stacks-block-height) ERR_VOUCHER_EXPIRED)
    (map-set vouchers
      { voucher-id: voucher-id }
      (merge voucher-data { owner: new-owner })
    )
    (map-set user-vouchers
      { user: new-owner }
      { voucher-ids: (unwrap! (as-max-len? (append new-owner-vouchers voucher-id) u100) ERR_INSUFFICIENT_BALANCE) }
    )
    (ok true)
  )
)