;; VerifyBranchMark - Decentralized Identity Verification System
;; A tree-based reputation architecture for professional credentialing

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-stake (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-expired (err u105))
(define-constant err-invalid-proof (err u106))

;; Minimum stake required for validators (in microSTX)
(define-constant min-validator-stake u1000000)

;; Reputation decay rate (percentage per block)
(define-constant decay-rate u1)

;; Data Variables
(define-data-var badge-nonce uint u0)
(define-data-var branch-nonce uint u0)
(define-data-var verification-nonce uint u0)

;; Data Maps

;; Branch structure (skill domains)
(define-map branches
    uint
    {
        name: (string-ascii 64),
        description: (string-ascii 256),
        creator: principal,
        created-at: uint,
        active: bool
    }
)

;; Badge structure (individual credentials)
(define-map badges
    uint
    {
        owner: principal,
        branch-id: uint,
        skill-name: (string-ascii 64),
        metadata-hash: (buff 32),
        issued-at: uint,
        expires-at: (optional uint),
        reputation-score: uint,
        verified: bool,
        verifier: (optional principal)
    }
)

;; User badge ownership mapping
(define-map user-badges
    { user: principal, badge-id: uint }
    bool
)

;; Branch membership (users in skill domains)
(define-map branch-members
    { branch-id: uint, user: principal }
    {
        joined-at: uint,
        reputation: uint,
        last-decay-block: uint
    }
)

;; Validator stakes
(define-map validator-stakes
    principal
    {
        staked-amount: uint,
        total-verifications: uint,
        successful-verifications: uint,
        locked: bool
    }
)

;; Verification requests
(define-map verification-requests
    uint
    {
        badge-id: uint,
        requester: principal,
        validator: (optional principal),
        proof-hash: (buff 32),
        status: (string-ascii 20),
        created-at: uint,
        completed-at: (optional uint)
    }
)

;; Reputation scores aggregated by user
(define-map user-reputation
    principal
    {
        total-score: uint,
        badges-count: uint,
        last-updated: uint
    }
)

;; Read-only functions

(define-read-only (get-branch (branch-id uint))
    (map-get? branches branch-id)
)

(define-read-only (get-badge (badge-id uint))
    (map-get? badges badge-id)
)

(define-read-only (get-user-reputation (user principal))
    (default-to 
        { total-score: u0, badges-count: u0, last-updated: u0 }
        (map-get? user-reputation user)
    )
)

(define-read-only (get-validator-stake (validator principal))
    (map-get? validator-stakes validator)
)

(define-read-only (get-verification-request (request-id uint))
    (map-get? verification-requests request-id)
)

(define-read-only (has-badge (user principal) (badge-id uint))
    (default-to false (map-get? user-badges { user: user, badge-id: badge-id }))
)

(define-read-only (get-branch-membership (branch-id uint) (user principal))
    (map-get? branch-members { branch-id: branch-id, user: user })
)

;; Calculate decayed reputation
(define-read-only (calculate-decayed-reputation (original-rep uint) (blocks-passed uint))
    (let
        (
            (decay-factor (/ (* blocks-passed decay-rate) u100))
            (decayed-amount (/ (* original-rep decay-factor) u100))
        )
        (if (> decayed-amount original-rep)
            u0
            (- original-rep decayed-amount)
        )
    )
)

;; Public functions

;; Create a new skill branch (domain)
(define-public (create-branch (name (string-ascii 64)) (description (string-ascii 256)))
    (let
        (
            (new-branch-id (+ (var-get branch-nonce) u1))
        )
        (map-set branches new-branch-id
            {
                name: name,
                description: description,
                creator: tx-sender,
                created-at: block-height,
                active: true
            }
        )
        (var-set branch-nonce new-branch-id)
        (ok new-branch-id)
    )
)

;; Join a branch (skill domain)
(define-public (join-branch (branch-id uint))
    (let
        (
            (branch (unwrap! (map-get? branches branch-id) err-not-found))
        )
        (asserts! (get active branch) err-not-found)
        (asserts! (is-none (map-get? branch-members { branch-id: branch-id, user: tx-sender })) err-already-exists)
        
        (map-set branch-members
            { branch-id: branch-id, user: tx-sender }
            {
                joined-at: block-height,
                reputation: u0,
                last-decay-block: block-height
            }
        )
        (ok true)
    )
)

;; Issue a new badge (self-attestation)
(define-public (issue-badge 
    (branch-id uint) 
    (skill-name (string-ascii 64)) 
    (metadata-hash (buff 32))
    (expires-at (optional uint)))
    (let
        (
            (new-badge-id (+ (var-get badge-nonce) u1))
            (branch (unwrap! (map-get? branches branch-id) err-not-found))
        )
        (asserts! (get active branch) err-not-found)
        
        ;; Create badge
        (map-set badges new-badge-id
            {
                owner: tx-sender,
                branch-id: branch-id,
                skill-name: skill-name,
                metadata-hash: metadata-hash,
                issued-at: block-height,
                expires-at: expires-at,
                reputation-score: u0,
                verified: false,
                verifier: none
            }
        )
        
        ;; Mark ownership
        (map-set user-badges
            { user: tx-sender, badge-id: new-badge-id }
            true
        )
        
        ;; Update user reputation
        (let
            (
                (current-rep (get-user-reputation tx-sender))
            )
            (map-set user-reputation tx-sender
                {
                    total-score: (get total-score current-rep),
                    badges-count: (+ (get badges-count current-rep) u1),
                    last-updated: block-height
                }
            )
        )
        
        (var-set badge-nonce new-badge-id)
        (ok new-badge-id)
    )
)

;; Stake tokens to become a validator
(define-public (stake-as-validator (amount uint))
    (begin
        (asserts! (>= amount min-validator-stake) err-insufficient-stake)
        
        ;; Transfer STX to contract
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        ;; Record stake
        (map-set validator-stakes tx-sender
            {
                staked-amount: amount,
                total-verifications: u0,
                successful-verifications: u0,
                locked: false
            }
        )
        (ok true)
    )
)

;; Request badge verification
(define-public (request-verification (badge-id uint) (proof-hash (buff 32)))
    (let
        (
            (badge (unwrap! (map-get? badges badge-id) err-not-found))
            (new-request-id (+ (var-get verification-nonce) u1))
        )
        (asserts! (is-eq (get owner badge) tx-sender) err-unauthorized)
        
        (map-set verification-requests new-request-id
            {
                badge-id: badge-id,
                requester: tx-sender,
                validator: none,
                proof-hash: proof-hash,
                status: "pending",
                created-at: block-height,
                completed-at: none
            }
        )
        
        (var-set verification-nonce new-request-id)
        (ok new-request-id)
    )
)

;; Verify a badge (validator action)
(define-public (verify-badge (request-id uint) (approved bool))
    (let
        (
            (request (unwrap! (map-get? verification-requests request-id) err-not-found))
            (badge-id (get badge-id request))
            (badge (unwrap! (map-get? badges badge-id) err-not-found))
            (validator-info (unwrap! (map-get? validator-stakes tx-sender) err-unauthorized))
        )
        (asserts! (>= (get staked-amount validator-info) min-validator-stake) err-insufficient-stake)
        (asserts! (is-eq (get status request) "pending") err-invalid-proof)
        
        ;; Update verification request
        (map-set verification-requests request-id
            (merge request {
                validator: (some tx-sender),
                status: (if approved "approved" "rejected"),
                completed-at: (some block-height)
            })
        )
        
        ;; If approved, update badge
        (if approved
            (begin
                (map-set badges badge-id
                    (merge badge {
                        verified: true,
                        verifier: (some tx-sender),
                        reputation-score: u100
                    })
                )
                
                ;; Update validator stats
                (map-set validator-stakes tx-sender
                    (merge validator-info {
                        total-verifications: (+ (get total-verifications validator-info) u1),
                        successful-verifications: (+ (get successful-verifications validator-info) u1)
                    })
                )
                
                ;; Update user reputation
                (let
                    (
                        (current-rep (get-user-reputation (get owner badge)))
                    )
                    (map-set user-reputation (get owner badge)
                        {
                            total-score: (+ (get total-score current-rep) u100),
                            badges-count: (get badges-count current-rep),
                            last-updated: block-height
                        }
                    )
                )
            )
            ;; If rejected, just update validator stats
            (map-set validator-stakes tx-sender
                (merge validator-info {
                    total-verifications: (+ (get total-verifications validator-info) u1),
                    successful-verifications: (get successful-verifications validator-info)
                })
            )
        )
        
        (ok approved)
    )
)

;; Unstake validator tokens
(define-public (unstake-validator)
    (let
        (
            (validator-info (unwrap! (map-get? validator-stakes tx-sender) err-not-found))
        )
        (asserts! (not (get locked validator-info)) err-unauthorized)
        
        ;; Transfer STX back to validator
        (try! (as-contract (stx-transfer? (get staked-amount validator-info) tx-sender tx-sender)))
        
        ;; Remove stake record
        (map-delete validator-stakes tx-sender)
        (ok true)
    )
)

;; Update reputation with decay
(define-public (update-reputation-decay (user principal) (branch-id uint))
    (let
        (
            (membership (unwrap! (map-get? branch-members { branch-id: branch-id, user: user }) err-not-found))
            (blocks-passed (- block-height (get last-decay-block membership)))
            (current-rep (get reputation membership))
            (new-rep (calculate-decayed-reputation current-rep blocks-passed))
        )
        (map-set branch-members
            { branch-id: branch-id, user: user }
            (merge membership {
                reputation: new-rep,
                last-decay-block: block-height
            })
        )
        (ok new-rep)
    )
)

;; Revoke a badge (owner only)
(define-public (revoke-badge (badge-id uint))
    (let
        (
            (badge (unwrap! (map-get? badges badge-id) err-not-found))
        )
        (asserts! (is-eq (get owner badge) tx-sender) err-unauthorized)
        
        (map-delete badges badge-id)
        (map-delete user-badges { user: tx-sender, badge-id: badge-id })
        
        ;; Update user reputation
        (let
            (
                (current-rep (get-user-reputation tx-sender))
            )
            (map-set user-reputation tx-sender
                {
                    total-score: (if (>= (get total-score current-rep) (get reputation-score badge))
                                    (- (get total-score current-rep) (get reputation-score badge))
                                    u0),
                    badges-count: (if (> (get badges-count current-rep) u0)
                                    (- (get badges-count current-rep) u1)
                                    u0),
                    last-updated: block-height
                }
            )
        )
        (ok true)
    )
)