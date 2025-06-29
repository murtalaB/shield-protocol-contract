;; Shield Protocol Smart Contract
;; Implements coverage management, settlement processing, and contribution handling

;; Error codes
(define-constant ERR-UNAUTHORIZED-ACCESS (err u100))
(define-constant ERR-COVERAGE-EXISTS (err u101))
(define-constant ERR-COVERAGE-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-CONTRIBUTION (err u103))
(define-constant ERR-COVERAGE-INACTIVE (err u104))
(define-constant ERR-INVALID-SETTLEMENT (err u105))
(define-constant ERR-SETTLEMENT-ALREADY-PROCESSED (err u106))
(define-constant ERR-INVALID-PRINCIPAL (err u107))
(define-constant ERR-INVALID-AMOUNT (err u108))
(define-constant ERR-INVALID-DURATION (err u109))

;; Data structures
(define-map coverage-plans
    { plan-id: uint, holder: principal }
    {
        protection-limit: uint,
        contribution-fee: uint,
        activation-height: uint,
        expiration-height: uint,
        is-enabled: bool
    }
)

(define-map settlements
    { settlement-id: uint, plan-id: uint }
    {
        payout-amount: uint,
        incident-details: (string-ascii 256),
        review-status: (string-ascii 20),
        is-finalized: bool,
        plan-id: uint
    }
)

;; Storage variables
(define-data-var next-plan-id uint u1)
(define-data-var next-settlement-id uint u1)
(define-data-var protocol-admin principal tx-sender)
(define-data-var accumulated-contributions uint u0)
(define-data-var distributed-payouts uint u0)

;; Administrative functions
(define-public (update-protocol-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get protocol-admin)) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (not (is-eq new-admin 'SP000000000000000000002Q6VF78)) ERR-INVALID-PRINCIPAL)
        (var-set protocol-admin new-admin)
        (ok true)
    )
)

;; Coverage management functions
(define-public (establish-coverage (protection-limit uint) (contribution-fee uint) (validity-period uint))
    (let
        (
            (plan-id (var-get next-plan-id))
            (activation-height block-height)
            (expiration-height (+ block-height validity-period))
        )
        (asserts! (> protection-limit u0) ERR-INVALID-AMOUNT)
        (asserts! (> contribution-fee u0) ERR-INVALID-AMOUNT)
        (asserts! (> validity-period u0) ERR-INVALID-DURATION)
        
        (map-insert coverage-plans
            { plan-id: plan-id, holder: tx-sender }
            {
                protection-limit: protection-limit,
                contribution-fee: contribution-fee,
                activation-height: activation-height,
                expiration-height: expiration-height,
                is-enabled: true
            }
        )
        
        (var-set next-plan-id (+ plan-id u1))
        (ok plan-id)
    )
)

(define-public (submit-contribution (plan-id uint))
    (let
        (
            (coverage-plan (unwrap! (get-coverage-plan plan-id) ERR-COVERAGE-NOT-FOUND))
            (contribution-fee (get contribution-fee coverage-plan))
        )
        (asserts! (unwrap! (is-coverage-valid plan-id) ERR-COVERAGE-NOT-FOUND) ERR-COVERAGE-INACTIVE)
        (try! (stx-transfer? contribution-fee tx-sender (var-get protocol-admin)))
        (var-set accumulated-contributions (+ (var-get accumulated-contributions) contribution-fee))
        (ok true)
    )
)

;; Settlement processing functions
(define-public (file-settlement (plan-id uint) (payout-amount uint) (incident-details (string-ascii 256)))
    (let
        (
            (settlement-id (var-get next-settlement-id))
            (coverage-plan (unwrap! (get-coverage-plan plan-id) ERR-COVERAGE-NOT-FOUND))
            (validated-details (unwrap-panic (as-max-len? incident-details u256)))
        )
        (asserts! (unwrap! (is-coverage-valid plan-id) ERR-COVERAGE-NOT-FOUND) ERR-COVERAGE-INACTIVE)
        (asserts! (<= payout-amount (get protection-limit coverage-plan)) ERR-INVALID-SETTLEMENT)
        (asserts! (> payout-amount u0) ERR-INVALID-AMOUNT)
        
        (map-insert settlements
            { settlement-id: settlement-id, plan-id: plan-id }
            {
                payout-amount: payout-amount,
                incident-details: validated-details,
                review-status: "PENDING",
                is-finalized: false,
                plan-id: plan-id
            }
        )
        
        (var-set next-settlement-id (+ settlement-id u1))
        (ok settlement-id)
    )
)

(define-public (review-settlement (settlement-id uint) (plan-id uint) (is-approved bool))
    (let
        (
            (settlement-key { settlement-id: settlement-id, plan-id: plan-id })
            (settlement-request (unwrap! (map-get? settlements settlement-key) ERR-INVALID-SETTLEMENT))
            (coverage-key { plan-id: plan-id, holder: tx-sender })
            (coverage-plan (unwrap! (map-get? coverage-plans coverage-key) ERR-COVERAGE-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (var-get protocol-admin)) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (not (get is-finalized settlement-request)) ERR-SETTLEMENT-ALREADY-PROCESSED)
        (asserts! (is-eq (get plan-id settlement-request) plan-id) ERR-INVALID-SETTLEMENT)
        
        (if is-approved
            (let
                (
                    (payout-amount (get payout-amount settlement-request))
                    (plan-holder (unwrap! (get-verified-plan-holder plan-id) ERR-COVERAGE-NOT-FOUND))
                )
                (try! (stx-transfer? payout-amount (var-get protocol-admin) plan-holder))
                (var-set distributed-payouts (+ (var-get distributed-payouts) payout-amount))
                (map-set settlements
                    settlement-key
                    (combine-settlement-data settlement-request { review-status: "APPROVED", is-finalized: true })
                )
                (ok true)
            )
            (begin
                (map-set settlements
                    settlement-key
                    (combine-settlement-data settlement-request { review-status: "REJECTED", is-finalized: true })
                )
                (ok true)
            )
        )
    )
)

;; Read-only functions
(define-read-only (get-coverage-plan (plan-id uint))
    (map-get? coverage-plans { plan-id: plan-id, holder: tx-sender })
)

(define-read-only (get-settlement-request (settlement-id uint))
    (begin
        (asserts! (> settlement-id u0) (err u110))
        (let 
            (
                (settlement-key { settlement-id: settlement-id, plan-id: u0 })
                (settlement-data (map-get? settlements settlement-key))
            )
            (ok settlement-data)
        )
    )
)

(define-read-only (get-plan-holder (plan-id uint))
    (let ((plan-key { plan-id: plan-id, holder: tx-sender }))
        (match (map-get? coverage-plans plan-key)
            coverage-plan (ok tx-sender)
            ERR-COVERAGE-NOT-FOUND
        )
    )
)

(define-read-only (get-verified-plan-holder (plan-id uint))
    (let 
        (
            (admin-key { plan-id: plan-id, holder: (var-get protocol-admin) })
            (admin-plan (map-get? coverage-plans admin-key))
        )
        (match admin-plan
            plan-data (ok (var-get protocol-admin))
            (let 
                (
                    (current-caller-key { plan-id: plan-id, holder: tx-sender })
                    (caller-plan (map-get? coverage-plans current-caller-key))
                )
                (match caller-plan
                    plan-data (ok tx-sender)
                    ERR-COVERAGE-NOT-FOUND
                )
            )
        )
    )
)

(define-read-only (is-coverage-valid (plan-id uint))
    (match (get-coverage-plan plan-id)
        coverage-plan (ok (and
            (get is-enabled coverage-plan)
            (<= block-height (get expiration-height coverage-plan))
        ))
        ERR-COVERAGE-NOT-FOUND
    )
)

;; Helper functions
(define-private (combine-settlement-data (settlement-info {
        payout-amount: uint,
        incident-details: (string-ascii 256),
        review-status: (string-ascii 20),
        is-finalized: bool,
        plan-id: uint
    }) 
    (modifications {
        review-status: (string-ascii 20),
        is-finalized: bool
    }))
    {
        payout-amount: (get payout-amount settlement-info),
        incident-details: (get incident-details settlement-info),
        review-status: (get review-status modifications),
        is-finalized: (get is-finalized modifications),
        plan-id: (get plan-id settlement-info)
    }
)