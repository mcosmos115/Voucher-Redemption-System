(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_VOUCHER_NOT_FOUND (err u101))
(define-constant ERR_VOUCHER_ALREADY_REDEEMED (err u102))
(define-constant ERR_VOUCHER_EXPIRED (err u103))
(define-constant ERR_INVALID_AMOUNT (err u104))
(define-constant ERR_INSUFFICIENT_BALANCE (err u105))
(define-constant ERR_PRODUCT_NOT_FOUND (err u106))
(define-constant ERR_PRODUCT_OUT_OF_STOCK (err u107))
(define-constant ERR_BATCH_SIZE_EXCEEDED (err u108))
(define-constant ERR_VOUCHER_ALREADY_STAKED (err u109))
(define-constant ERR_VOUCHER_NOT_STAKED (err u110))
(define-constant ERR_STAKING_PERIOD_NOT_COMPLETE (err u111))
(define-constant ERR_VOUCHER_NOT_FOR_SALE (err u112))
(define-constant ERR_CANNOT_BUY_OWN_VOUCHER (err u113))
(define-constant ERR_MARKETPLACE_FEE_FAILED (err u114))
(define-constant ERR_ALREADY_REFERRED (err u115))
(define-constant ERR_CANNOT_SELF_REFER (err u116))
(define-constant ERR_NO_REWARDS_AVAILABLE (err u117))

(define-data-var voucher-counter uint u0)
(define-data-var product-counter uint u0)
(define-data-var marketplace-fee-rate uint u300)
(define-data-var referral-reward-rate uint u500)

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

(define-map staked-vouchers
  { voucher-id: uint }
  {
    staker: principal,
    staked-at: uint,
    unlock-block: uint,
    original-value: uint,
    reward-rate: uint
  }
)

(define-map user-staked-vouchers
  { user: principal }
  { staked-voucher-ids: (list 50 uint) }
)

(define-map marketplace-listings
  { voucher-id: uint }
  {
    seller: principal,
    price: uint,
    listed-at: uint,
    active: bool
  }
)

(define-map trade-history
  { trade-id: uint }
  {
    voucher-id: uint,
    seller: principal,
    buyer: principal,
    price: uint,
    traded-at: uint,
    fee-paid: uint
  }
)

(define-data-var trade-counter uint u0)

(define-map referrals
  { referee: principal }
  {
    referrer: principal,
    joined-at: uint,
    total-rewards-earned: uint
  }
)

(define-map referrer-stats
  { referrer: principal }
  {
    total-referrals: uint,
    pending-rewards: uint,
    claimed-rewards: uint
  }
)

(define-map referral-activity
  { referee: principal, activity-type: (string-ascii 20) }
  {
    referrer: principal,
    reward-amount: uint,
    activity-at: uint
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

(define-read-only (get-staked-voucher (voucher-id uint))
  (map-get? staked-vouchers { voucher-id: voucher-id })
)

(define-read-only (get-user-staked-vouchers (user principal))
  (default-to { staked-voucher-ids: (list) } (map-get? user-staked-vouchers { user: user }))
)

(define-read-only (calculate-staking-reward (voucher-id uint))
  (match (get-staked-voucher voucher-id)
    stake-data
    (let
      (
        (blocks-staked (- stacks-block-height (get staked-at stake-data)))
        (reward-multiplier (/ (* blocks-staked (get reward-rate stake-data)) u10000))
        (reward-amount (/ (* (get original-value stake-data) reward-multiplier) u100))
      )
      (some (+ (get original-value stake-data) reward-amount))
    )
    none
  )
)

(define-read-only (is-voucher-staked (voucher-id uint))
  (is-some (get-staked-voucher voucher-id))
)

(define-read-only (get-marketplace-listing (voucher-id uint))
  (map-get? marketplace-listings { voucher-id: voucher-id })
)

(define-read-only (get-trade-history (trade-id uint))
  (map-get? trade-history { trade-id: trade-id })
)

(define-read-only (get-marketplace-fee-rate)
  (var-get marketplace-fee-rate)
)

(define-read-only (is-voucher-listed (voucher-id uint))
  (match (get-marketplace-listing voucher-id)
    listing-data (get active listing-data)
    false
  )
)

(define-read-only (get-referral (referee principal))
  (map-get? referrals { referee: referee })
)

(define-read-only (get-referrer-stats (referrer principal))
  (default-to { total-referrals: u0, pending-rewards: u0, claimed-rewards: u0 }
    (map-get? referrer-stats { referrer: referrer }))
)

(define-read-only (get-referral-reward-rate)
  (var-get referral-reward-rate)
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

(define-public (batch-issue-vouchers (recipients (list 10 principal)) (product-id uint) (value uint) (validity-blocks uint))
  (let
    (
      (batch-size (len recipients))
      (starting-counter (var-get voucher-counter))
      (result (fold batch-issue-voucher-helper recipients {
        product-id: product-id,
        value: value,
        validity-blocks: validity-blocks,
        counter: starting-counter,
        issued-ids: (list)
      }))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> value u0) ERR_INVALID_AMOUNT)
    (asserts! (> validity-blocks u0) ERR_INVALID_AMOUNT)
    (asserts! (is-some (get-product product-id)) ERR_PRODUCT_NOT_FOUND)
    (asserts! (<= batch-size u10) ERR_BATCH_SIZE_EXCEEDED)
    (ok (get issued-ids result))
  )
)

(define-private (batch-issue-voucher-helper (recipient principal) (state { product-id: uint, value: uint, validity-blocks: uint, counter: uint, issued-ids: (list 10 uint) }))
  (let
    (
      (new-voucher-id (+ (get counter state) u1))
      (expiry-block (+ stacks-block-height (get validity-blocks state)))
      (current-vouchers (get voucher-ids (get-user-vouchers recipient)))
    )
    (map-set vouchers
      { voucher-id: new-voucher-id }
      {
        owner: recipient,
        product-id: (get product-id state),
        value: (get value state),
        expiry-block: expiry-block,
        redeemed: false,
        redeemed-at: none,
        created-at: stacks-block-height
      }
    )
    (map-set user-vouchers
      { user: recipient }
      { voucher-ids: (unwrap-panic (as-max-len? (append current-vouchers new-voucher-id) u100)) }
    )
    (var-set voucher-counter new-voucher-id)
    {
      product-id: (get product-id state),
      value: (get value state),
      validity-blocks: (get validity-blocks state),
      counter: new-voucher-id,
      issued-ids: (unwrap-panic (as-max-len? (append (get issued-ids state) new-voucher-id) u10))
    }
  )
)

(define-public (batch-redeem-vouchers (voucher-ids (list 5 uint)))
  (let
    (
      (batch-size (len voucher-ids))
    )
    (asserts! (<= batch-size u5) ERR_BATCH_SIZE_EXCEEDED)
    (ok (fold batch-redeem-voucher-helper voucher-ids { success-count: u0, failed-count: u0 }))
  )
)

(define-private (batch-redeem-voucher-helper (voucher-id uint) (state { success-count: uint, failed-count: uint }))
  (match (redeem-voucher voucher-id)
    success-result
    { success-count: (+ (get success-count state) u1), failed-count: (get failed-count state) }
    error-result
    { success-count: (get success-count state), failed-count: (+ (get failed-count state) u1) }
  )
)

(define-public (batch-transfer-vouchers (voucher-data (list 5 { voucher-id: uint, new-owner: principal })))
  (let
    (
      (batch-size (len voucher-data))
    )
    (asserts! (<= batch-size u5) ERR_BATCH_SIZE_EXCEEDED)
    (ok (fold batch-transfer-voucher-helper voucher-data { success-count: u0, failed-count: u0 }))
  )
)

(define-private (batch-transfer-voucher-helper (transfer-data { voucher-id: uint, new-owner: principal }) (state { success-count: uint, failed-count: uint }))
  (match (transfer-voucher (get voucher-id transfer-data) (get new-owner transfer-data))
    success-result
    { success-count: (+ (get success-count state) u1), failed-count: (get failed-count state) }
    error-result
    { success-count: (get success-count state), failed-count: (+ (get failed-count state) u1) }
  )
)

(define-public (stake-voucher (voucher-id uint) (staking-blocks uint))
  (let
    (
      (voucher-data (unwrap! (get-voucher voucher-id) ERR_VOUCHER_NOT_FOUND))
      (reward-rate u25)
      (current-staked-vouchers (get staked-voucher-ids (get-user-staked-vouchers tx-sender)))
    )
    (asserts! (is-eq tx-sender (get owner voucher-data)) ERR_UNAUTHORIZED)
    (asserts! (not (get redeemed voucher-data)) ERR_VOUCHER_ALREADY_REDEEMED)
    (asserts! (> (get expiry-block voucher-data) stacks-block-height) ERR_VOUCHER_EXPIRED)
    (asserts! (not (is-voucher-staked voucher-id)) ERR_VOUCHER_ALREADY_STAKED)
    (asserts! (> staking-blocks u0) ERR_INVALID_AMOUNT)
    (map-set staked-vouchers
      { voucher-id: voucher-id }
      {
        staker: tx-sender,
        staked-at: stacks-block-height,
        unlock-block: (+ stacks-block-height staking-blocks),
        original-value: (get value voucher-data),
        reward-rate: reward-rate
      }
    )
    (map-set user-staked-vouchers
      { user: tx-sender }
      { staked-voucher-ids: (unwrap! (as-max-len? (append current-staked-vouchers voucher-id) u50) ERR_INSUFFICIENT_BALANCE) }
    )
    (ok true)
  )
)

(define-public (unstake-voucher (voucher-id uint))
  (let
    (
      (stake-data (unwrap! (get-staked-voucher voucher-id) ERR_VOUCHER_NOT_STAKED))
      (voucher-data (unwrap! (get-voucher voucher-id) ERR_VOUCHER_NOT_FOUND))
      (new-value (unwrap! (calculate-staking-reward voucher-id) ERR_VOUCHER_NOT_STAKED))
    )
    (asserts! (is-eq tx-sender (get staker stake-data)) ERR_UNAUTHORIZED)
    (asserts! (>= stacks-block-height (get unlock-block stake-data)) ERR_STAKING_PERIOD_NOT_COMPLETE)
    (map-delete staked-vouchers { voucher-id: voucher-id })
    (map-set vouchers
      { voucher-id: voucher-id }
      (merge voucher-data { value: new-value })
    )
    (ok new-value)
  )
)

(define-public (emergency-unstake (voucher-id uint))
  (let
    (
      (stake-data (unwrap! (get-staked-voucher voucher-id) ERR_VOUCHER_NOT_STAKED))
      (voucher-data (unwrap! (get-voucher voucher-id) ERR_VOUCHER_NOT_FOUND))
      (penalty-rate u10)
      (penalty-amount (/ (* (get original-value stake-data) penalty-rate) u100))
      (penalized-value (- (get original-value stake-data) penalty-amount))
    )
    (asserts! (is-eq tx-sender (get staker stake-data)) ERR_UNAUTHORIZED)
    (map-delete staked-vouchers { voucher-id: voucher-id })
    (map-set vouchers
      { voucher-id: voucher-id }
      (merge voucher-data { value: penalized-value })
    )
    (ok penalized-value)
  )
)

(define-public (list-voucher-for-sale (voucher-id uint) (price uint))
  (let
    (
      (voucher-data (unwrap! (get-voucher voucher-id) ERR_VOUCHER_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner voucher-data)) ERR_UNAUTHORIZED)
    (asserts! (not (get redeemed voucher-data)) ERR_VOUCHER_ALREADY_REDEEMED)
    (asserts! (> (get expiry-block voucher-data) stacks-block-height) ERR_VOUCHER_EXPIRED)
    (asserts! (not (is-voucher-staked voucher-id)) ERR_VOUCHER_ALREADY_STAKED)
    (asserts! (not (is-voucher-listed voucher-id)) ERR_VOUCHER_ALREADY_STAKED)
    (asserts! (> price u0) ERR_INVALID_AMOUNT)
    (map-set marketplace-listings
      { voucher-id: voucher-id }
      {
        seller: tx-sender,
        price: price,
        listed-at: stacks-block-height,
        active: true
      }
    )
    (ok true)
  )
)

(define-public (cancel-listing (voucher-id uint))
  (let
    (
      (listing-data (unwrap! (get-marketplace-listing voucher-id) ERR_VOUCHER_NOT_FOR_SALE))
    )
    (asserts! (is-eq tx-sender (get seller listing-data)) ERR_UNAUTHORIZED)
    (asserts! (get active listing-data) ERR_VOUCHER_NOT_FOR_SALE)
    (map-set marketplace-listings
      { voucher-id: voucher-id }
      (merge listing-data { active: false })
    )
    (ok true)
  )
)

(define-public (buy-voucher (voucher-id uint))
  (let
    (
      (listing-data (unwrap! (get-marketplace-listing voucher-id) ERR_VOUCHER_NOT_FOR_SALE))
      (voucher-data (unwrap! (get-voucher voucher-id) ERR_VOUCHER_NOT_FOUND))
      (sale-price (get price listing-data))
      (fee-amount (/ (* sale-price (var-get marketplace-fee-rate)) u10000))
      (seller-amount (- sale-price fee-amount))
      (new-trade-id (+ (var-get trade-counter) u1))
      (current-buyer-vouchers (get voucher-ids (get-user-vouchers tx-sender)))
    )
    (asserts! (get active listing-data) ERR_VOUCHER_NOT_FOR_SALE)
    (asserts! (not (is-eq tx-sender (get seller listing-data))) ERR_CANNOT_BUY_OWN_VOUCHER)
    (asserts! (not (get redeemed voucher-data)) ERR_VOUCHER_ALREADY_REDEEMED)
    (asserts! (> (get expiry-block voucher-data) stacks-block-height) ERR_VOUCHER_EXPIRED)
    (try! (stx-transfer? sale-price tx-sender (get seller listing-data)))
    (map-set vouchers
      { voucher-id: voucher-id }
      (merge voucher-data { owner: tx-sender })
    )
    (map-set user-vouchers
      { user: tx-sender }
      { voucher-ids: (unwrap! (as-max-len? (append current-buyer-vouchers voucher-id) u100) ERR_INSUFFICIENT_BALANCE) }
    )
    (map-set marketplace-listings
      { voucher-id: voucher-id }
      (merge listing-data { active: false })
    )
    (map-set trade-history
      { trade-id: new-trade-id }
      {
        voucher-id: voucher-id,
        seller: (get seller listing-data),
        buyer: tx-sender,
        price: sale-price,
        traded-at: stacks-block-height,
        fee-paid: fee-amount
      }
    )
    (var-set trade-counter new-trade-id)
    (ok true)
  )
)

(define-public (update-marketplace-fee (new-fee-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-fee-rate u1000) ERR_INVALID_AMOUNT)
    (var-set marketplace-fee-rate new-fee-rate)
    (ok true)
  )
)

(define-public (register-with-referrer (referrer principal))
  (begin
    (asserts! (not (is-eq tx-sender referrer)) ERR_CANNOT_SELF_REFER)
    (asserts! (is-none (get-referral tx-sender)) ERR_ALREADY_REFERRED)
    (map-set referrals
      { referee: tx-sender }
      {
        referrer: referrer,
        joined-at: stacks-block-height,
        total-rewards-earned: u0
      }
    )
    (let
      (
        (current-stats (get-referrer-stats referrer))
      )
      (map-set referrer-stats
        { referrer: referrer }
        {
          total-referrals: (+ (get total-referrals current-stats) u1),
          pending-rewards: (get pending-rewards current-stats),
          claimed-rewards: (get claimed-rewards current-stats)
        }
      )
    )
    (ok true)
  )
)

(define-private (process-referral-reward (referee principal) (reward-base uint) (activity-type (string-ascii 20)))
  (match (get-referral referee)
    referral-data
    (let
      (
        (referrer (get referrer referral-data))
        (reward-amount (/ (* reward-base (var-get referral-reward-rate)) u10000))
        (current-referrer-stats (get-referrer-stats referrer))
      )
      (map-set referrer-stats
        { referrer: referrer }
        {
          total-referrals: (get total-referrals current-referrer-stats),
          pending-rewards: (+ (get pending-rewards current-referrer-stats) reward-amount),
          claimed-rewards: (get claimed-rewards current-referrer-stats)
        }
      )
      (map-set referrals
        { referee: referee }
        (merge referral-data { total-rewards-earned: (+ (get total-rewards-earned referral-data) reward-amount) })
      )
      (map-set referral-activity
        { referee: referee, activity-type: activity-type }
        {
          referrer: referrer,
          reward-amount: reward-amount,
          activity-at: stacks-block-height
        }
      )
      true
    )
    false
  )
)

(define-public (redeem-voucher-with-referral (voucher-id uint))
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
    (process-referral-reward tx-sender (get value voucher-data) "redemption")
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

(define-public (buy-voucher-with-referral (voucher-id uint))
  (let
    (
      (listing-data (unwrap! (get-marketplace-listing voucher-id) ERR_VOUCHER_NOT_FOR_SALE))
      (voucher-data (unwrap! (get-voucher voucher-id) ERR_VOUCHER_NOT_FOUND))
      (sale-price (get price listing-data))
      (fee-amount (/ (* sale-price (var-get marketplace-fee-rate)) u10000))
      (seller-amount (- sale-price fee-amount))
      (new-trade-id (+ (var-get trade-counter) u1))
      (current-buyer-vouchers (get voucher-ids (get-user-vouchers tx-sender)))
    )
    (asserts! (get active listing-data) ERR_VOUCHER_NOT_FOR_SALE)
    (asserts! (not (is-eq tx-sender (get seller listing-data))) ERR_CANNOT_BUY_OWN_VOUCHER)
    (asserts! (not (get redeemed voucher-data)) ERR_VOUCHER_ALREADY_REDEEMED)
    (asserts! (> (get expiry-block voucher-data) stacks-block-height) ERR_VOUCHER_EXPIRED)
    (process-referral-reward tx-sender sale-price "purchase")
    (try! (stx-transfer? sale-price tx-sender (get seller listing-data)))
    (map-set vouchers
      { voucher-id: voucher-id }
      (merge voucher-data { owner: tx-sender })
    )
    (map-set user-vouchers
      { user: tx-sender }
      { voucher-ids: (unwrap! (as-max-len? (append current-buyer-vouchers voucher-id) u100) ERR_INSUFFICIENT_BALANCE) }
    )
    (map-set marketplace-listings
      { voucher-id: voucher-id }
      (merge listing-data { active: false })
    )
    (map-set trade-history
      { trade-id: new-trade-id }
      {
        voucher-id: voucher-id,
        seller: (get seller listing-data),
        buyer: tx-sender,
        price: sale-price,
        traded-at: stacks-block-height,
        fee-paid: fee-amount
      }
    )
    (var-set trade-counter new-trade-id)
    (ok true)
  )
)

(define-public (claim-referral-rewards)
  (let
    (
      (stats (get-referrer-stats tx-sender))
      (pending-amount (get pending-rewards stats))
    )
    (asserts! (> pending-amount u0) ERR_NO_REWARDS_AVAILABLE)
    (try! (as-contract (stx-transfer? pending-amount tx-sender tx-sender)))
    (map-set referrer-stats
      { referrer: tx-sender }
      {
        total-referrals: (get total-referrals stats),
        pending-rewards: u0,
        claimed-rewards: (+ (get claimed-rewards stats) pending-amount)
      }
    )
    (ok pending-amount)
  )
)

(define-public (update-referral-reward-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-rate u2000) ERR_INVALID_AMOUNT)
    (var-set referral-reward-rate new-rate)
    (ok true)
  )
)

(define-map voucher-approvals
  { voucher-id: uint }
  { approved: principal }
)

(define-public (approve-voucher (voucher-id uint) (spender principal))
  (let
    (
      (voucher-data (unwrap! (get-voucher voucher-id) ERR_VOUCHER_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner voucher-data)) ERR_UNAUTHORIZED)
    (map-set voucher-approvals { voucher-id: voucher-id } { approved: spender })
    (ok true)
  )
)

(define-read-only (get-approved (voucher-id uint))
  (map-get? voucher-approvals { voucher-id: voucher-id })
)

(define-public (revoke-voucher-approval (voucher-id uint))
  (let
    (
      (voucher-data (unwrap! (get-voucher voucher-id) ERR_VOUCHER_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner voucher-data)) ERR_UNAUTHORIZED)
    (map-delete voucher-approvals { voucher-id: voucher-id })
    (ok true)
  )
)

(define-public (transfer-voucher-from (voucher-id uint) (from principal) (to principal))
  (let
    (
      (voucher-data (unwrap! (get-voucher voucher-id) ERR_VOUCHER_NOT_FOUND))
      (approval (map-get? voucher-approvals { voucher-id: voucher-id }))
      (to-vouchers (get voucher-ids (get-user-vouchers to)))
    )
    (asserts! (is-eq from (get owner voucher-data)) ERR_UNAUTHORIZED)
    (asserts! (or (is-eq tx-sender from) (match approval a (is-eq tx-sender (get approved a)) false)) ERR_UNAUTHORIZED)
    (asserts! (not (get redeemed voucher-data)) ERR_VOUCHER_ALREADY_REDEEMED)
    (asserts! (> (get expiry-block voucher-data) stacks-block-height) ERR_VOUCHER_EXPIRED)
    (map-set vouchers { voucher-id: voucher-id } (merge voucher-data { owner: to }))
    (map-set user-vouchers { user: to } { voucher-ids: (unwrap! (as-max-len? (append to-vouchers voucher-id) u100) ERR_INSUFFICIENT_BALANCE) })
    (map-delete voucher-approvals { voucher-id: voucher-id })
    (ok true)
  )
)
