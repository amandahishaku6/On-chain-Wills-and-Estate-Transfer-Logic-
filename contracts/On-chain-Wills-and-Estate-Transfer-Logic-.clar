;; Token trait
(define-trait token-trait
    (
        (transfer (uint principal principal (optional (buff 34))) (response bool uint))
        (get-balance (principal) (response uint uint))
    )
)

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-REGISTERED (err u101))
(define-constant ERR-NOT-REGISTERED (err u102))
(define-constant ERR-INVALID-BENEFICIARY (err u103))
(define-constant ERR-ALREADY-DECEASED (err u104))
(define-constant ERR-STILL-ALIVE (err u105))
(define-constant ERR-INVALID-TOKEN (err u106))
(define-constant ERR-INSUFFICIENT-BALANCE (err u107))
(define-constant ERR-INSUFFICIENT-VOTES (err u108))
(define-constant ERR-RECOVERY-PERIOD-ACTIVE (err u109))
(define-constant ERR-NOT-BACKUP-EXECUTOR (err u110))
(define-constant ERR-INVALID-ENCRYPTION-KEY (err u111))
(define-constant ERR-WILL-NOT-ENCRYPTED (err u112))
(define-constant ERR-ENCRYPTION-ALREADY-SET (err u113))
(define-constant ERR-AMENDMENT-NOT-FOUND (err u114))
(define-constant ERR-AMENDMENT-ALREADY-ACTIVE (err u115))
(define-constant ERR-GRACE-PERIOD-ACTIVE (err u116))
(define-constant ERR-AMENDMENT-EXPIRED (err u117))
(define-constant ERR-TIME-LOCK-ACTIVE (err u118))
(define-constant ERR-NO-CLAIMABLE-AMOUNT (err u119))
(define-constant ERR-ALREADY-CLAIMED (err u120))
(define-constant ERR-WILL-NOT-EXECUTED (err u121))

;; Data variables
(define-data-var oracle-address principal 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
(define-data-var dao-address principal 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
(define-data-var emergency-recovery-period uint u1008)
(define-data-var dispute-counter uint u0)
(define-data-var emergency-request-counter uint u0)
(define-data-var amendment-counter uint u0)
(define-data-var amendment-grace-period uint u144)

;; Main wills map
(define-map wills
    principal
    {
        beneficiaries: (list 10 { address: principal, share: uint }),
        last-activity: uint,
        inactivity-threshold: uint,
        stx-balance: uint,
        is-deceased: bool,
        backup-executors: (list 3 principal),
        emergency-contact: principal
    }
)

(define-map pending-disputes
    uint
    {
        will-owner: principal,
        challenger: principal,
        status: (string-ascii 20),
        resolution-votes: uint
    }
)

;; Token allocation map
(define-map token-allocations
    { owner: principal, token-contract: principal }
    {
        amount: uint,
        beneficiary-shares: (list 10 { address: principal, share: uint })
    }
)

;; Approved tokens map
(define-map approved-tokens
    principal
    {
        token-name: (string-ascii 32),
        is-active: bool
    }
)

;; Emergency requests map
(define-map emergency-requests
    uint
    {
        will-owner: principal,
        requester: principal,
        request-type: (string-ascii 20),
        created-at: uint,
        votes: uint,
        required-votes: uint,
        is-executed: bool
    }
)

;; Executor votes map
(define-map executor-votes
    { request-id: uint, executor: principal }
    bool
)

;; Backup executors map
(define-map backup-executors
    principal
    {
        is-active: bool,
        reputation-score: uint,
        executed-wills: uint
    }
)

;; Token allocation map 
(define-map token-allocation-records
    { owner: principal, token-contract: principal }
    {
        amount: uint,
        beneficiary-shares: (list 10 { address: principal, share: uint })
    }
)

;; Encrypted wills map
(define-map encrypted-wills
    principal
    {
        encrypted-data: (buff 1024),
        encryption-key-hash: (buff 32),
        is-encrypted: bool,
        decryption-authorized: bool
    })

(define-map will-amendments
    uint
    {
        will-owner: principal,
        amendment-type: (string-ascii 20),
        new-beneficiaries: (optional (list 10 { address: principal, share: uint })),
        new-threshold: (optional uint),
        new-backup-executors: (optional (list 3 principal)),
        new-emergency-contact: (optional principal),
        proposed-at: uint,
        effective-at: uint,
        is-active: bool,
        version: uint
    })

(define-map will-versions
    principal
    uint)

(define-map time-lock-configs
    principal
    {
        time-lock-period: uint,
        execution-block: uint,
        is-time-locked: bool
    })

(define-map claim-records
    { will-owner: principal, beneficiary: principal }
    {
        total-amount: uint,
        claimed-amount: uint,
        claimable-at: uint,
        has-claimed: bool
    })



(define-public (register-will (beneficiaries (list 10 { address: principal, share: uint })) 
                            (inactivity-threshold uint)
                            (backup-executor-list (list 3 principal))
                            (emergency-contact principal))
    (let ((existing-will (get-will tx-sender)))
        (asserts! (is-none existing-will) ERR-ALREADY-REGISTERED)
        (asserts! (> (len beneficiaries) u0) ERR-INVALID-BENEFICIARY)
        (map-set wills tx-sender {
            beneficiaries: beneficiaries,
            last-activity: burn-block-height,
            inactivity-threshold: inactivity-threshold,
            stx-balance: (stx-get-balance tx-sender),
            is-deceased: false,
            backup-executors: backup-executor-list,
            emergency-contact: emergency-contact
        })
        (ok true)))

(define-public (add-token-to-will (token-contract principal) 
                                 (amount uint) 
                                 (beneficiary-shares (list 10 { address: principal, share: uint })))
    (let ((will (unwrap! (get-will tx-sender) ERR-NOT-REGISTERED))
          (token-info (unwrap! (map-get? approved-tokens token-contract) ERR-INVALID-TOKEN)))
        (asserts! (get is-active token-info) ERR-INVALID-TOKEN)
        (asserts! (not (get is-deceased will)) ERR-ALREADY-DECEASED)
        (map-set token-allocations 
            { owner: tx-sender, token-contract: token-contract }
            {
                amount: amount,
                beneficiary-shares: beneficiary-shares
            })
        (ok true)))

(define-public (approve-token (token-contract principal) (token-name (string-ascii 32)))
    (begin
        (asserts! (is-eq tx-sender (var-get dao-address)) ERR-NOT-AUTHORIZED)
        (map-set approved-tokens token-contract {
            token-name: token-name,
            is-active: true
        })
        (ok true)))

(define-public (register-backup-executor)
    (begin
        (map-set backup-executors tx-sender {
            is-active: true,
            reputation-score: u100,
            executed-wills: u0
        })
        (ok true)))

(define-public (create-emergency-request (will-owner principal) (request-type (string-ascii 20)))
    (let ((will (unwrap! (get-will will-owner) ERR-NOT-REGISTERED))
          (request-id (+ (var-get emergency-request-counter) u1))
          (executor-info (unwrap! (map-get? backup-executors tx-sender) ERR-NOT-BACKUP-EXECUTOR)))
        (asserts! (get is-active executor-info) ERR-NOT-BACKUP-EXECUTOR)
        (asserts! (not (get is-deceased will)) ERR-ALREADY-DECEASED)
        (var-set emergency-request-counter request-id)
        (map-set emergency-requests request-id {
            will-owner: will-owner,
            requester: tx-sender,
            request-type: request-type,
            created-at: burn-block-height,
            votes: u1,
            required-votes: u2,
            is-executed: false
        })
        (map-set executor-votes { request-id: request-id, executor: tx-sender } true)
        (ok request-id)))

(define-public (vote-emergency-request (request-id uint))
    (let ((request (unwrap! (map-get? emergency-requests request-id) ERR-NOT-REGISTERED))
          (executor-info (unwrap! (map-get? backup-executors tx-sender) ERR-NOT-BACKUP-EXECUTOR))
          (existing-vote (map-get? executor-votes { request-id: request-id, executor: tx-sender })))
        (asserts! (get is-active executor-info) ERR-NOT-BACKUP-EXECUTOR)
        (asserts! (is-none existing-vote) ERR-ALREADY-REGISTERED)
        (asserts! (not (get is-executed request)) ERR-ALREADY-DECEASED)
        (let ((new-votes (+ (get votes request) u1)))
            (map-set emergency-requests request-id (merge request { votes: new-votes }))
            (map-set executor-votes { request-id: request-id, executor: tx-sender } true)
            (if (>= new-votes (get required-votes request))
                (execute-emergency-request request-id)
                (ok false)))))

(define-public (emergency-recovery-execute (will-owner principal))
    (let ((will (unwrap! (get-will will-owner) ERR-NOT-REGISTERED)))
        (asserts! (is-eq tx-sender (get emergency-contact will)) ERR-NOT-AUTHORIZED)
        (asserts! (> (- burn-block-height (get last-activity will)) 
                     (+ (get inactivity-threshold will) (var-get emergency-recovery-period))) 
                  ERR-RECOVERY-PERIOD-ACTIVE)
        (unwrap! (execute-will will-owner) (err u500))
        (ok true)))

(define-public (update-activity)
    (let ((existing-will (unwrap! (get-will tx-sender) ERR-NOT-REGISTERED)))
        (map-set wills tx-sender (merge existing-will {
            last-activity: burn-block-height
        }))
        (ok true)))

(define-public (report-death (deceased principal))
    (let ((will (unwrap! (get-will deceased) ERR-NOT-REGISTERED)))
        (asserts! (is-eq tx-sender (var-get oracle-address)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get is-deceased will)) ERR-ALREADY-DECEASED)
        (unwrap! (execute-will deceased) (err u500))
        (ok true)))

(define-public (check-inactivity (owner principal))
    (let ((will (unwrap! (get-will owner) ERR-NOT-REGISTERED)))
        (if (> (- burn-block-height (get last-activity will)) 
               (get inactivity-threshold will))
            (execute-will owner)
            (ok false))))

(define-public (raise-dispute (will-owner principal) (reason (string-ascii 50)))
    (let ((dispute-id (+ (var-get dispute-counter) u1)))
        (var-set dispute-counter dispute-id)
        (map-set pending-disputes dispute-id {
            will-owner: will-owner,
            challenger: tx-sender,
            status: "PENDING",
            resolution-votes: u0
        })
        (ok dispute-id)))

(define-public (encrypt-will-data (encrypted-data (buff 1024)) (encryption-key-hash (buff 32)))
    (let ((existing-will (unwrap! (get-will tx-sender) ERR-NOT-REGISTERED))
          (existing-encryption (map-get? encrypted-wills tx-sender)))
        (asserts! (is-none existing-encryption) ERR-ENCRYPTION-ALREADY-SET)
        (asserts! (> (len encrypted-data) u0) ERR-INVALID-ENCRYPTION-KEY)
        (asserts! (> (len encryption-key-hash) u0) ERR-INVALID-ENCRYPTION-KEY)
        (map-set encrypted-wills tx-sender {
            encrypted-data: encrypted-data,
            encryption-key-hash: encryption-key-hash,
            is-encrypted: true,
            decryption-authorized: false
        })
        (ok true)))

(define-public (authorize-decryption (will-owner principal))
    (let ((will (unwrap! (get-will will-owner) ERR-NOT-REGISTERED))
          (encrypted-will (unwrap! (map-get? encrypted-wills will-owner) ERR-WILL-NOT-ENCRYPTED)))
        (asserts! (or (is-eq tx-sender will-owner)
                     (is-eq tx-sender (var-get oracle-address))
                     (> (- burn-block-height (get last-activity will)) (get inactivity-threshold will)))
                 ERR-NOT-AUTHORIZED)
        (map-set encrypted-wills will-owner (merge encrypted-will { decryption-authorized: true }))
        (ok true)))

(define-public (verify-encryption-key (will-owner principal) (encryption-key (buff 32)))
    (let ((encrypted-will (unwrap! (map-get? encrypted-wills will-owner) ERR-WILL-NOT-ENCRYPTED)))
        (asserts! (get decryption-authorized encrypted-will) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (sha256 encryption-key) (get encryption-key-hash encrypted-will)) ERR-INVALID-ENCRYPTION-KEY)
        (ok (get encrypted-data encrypted-will))))

(define-read-only (get-will (owner principal))
    (map-get? wills owner))

(define-read-only (get-token-allocation (owner principal) (token-contract principal))
    (map-get? token-allocations { owner: owner, token-contract: token-contract }))

(define-read-only (get-approved-token (token-contract principal))
    (map-get? approved-tokens token-contract))

(define-read-only (get-dispute (dispute-id uint))
    (map-get? pending-disputes dispute-id))

(define-read-only (get-emergency-request (request-id uint))
    (map-get? emergency-requests request-id))

(define-read-only (get-backup-executor (executor principal))
    (map-get? backup-executors executor))

(define-read-only (get-encrypted-will (owner principal))
    (map-get? encrypted-wills owner))

(define-read-only (get-will-amendment (amendment-id uint))
    (map-get? will-amendments amendment-id))

(define-read-only (get-will-version (owner principal))
    (default-to u1 (map-get? will-versions owner)))

(define-read-only (get-time-lock-config (owner principal))
    (map-get? time-lock-configs owner))

(define-read-only (get-claim-record (will-owner principal) (beneficiary principal))
    (map-get? claim-records { will-owner: will-owner, beneficiary: beneficiary }))

(define-private (execute-will (owner principal))
    (let ((will (unwrap! (get-will owner) ERR-NOT-REGISTERED))
          (time-lock-config (map-get? time-lock-configs owner)))
        (map-set wills owner (merge will { is-deceased: true }))
        (match time-lock-config
            lock-config
            (if (get is-time-locked lock-config)
                (begin
                    (map-set time-lock-configs owner (merge lock-config { execution-block: burn-block-height }))
                    (unwrap! (setup-beneficiary-claims owner (get beneficiaries will) (get time-lock-period lock-config)) (err u500))
                    (ok true))
                (begin
                    (unwrap! (distribute-assets owner (get beneficiaries will)) (err u500))
                    (ok true)))
            (begin
                (unwrap! (distribute-assets owner (get beneficiaries will)) (err u500))
                (ok true)))))

(define-private (distribute-assets (owner principal) (beneficiaries (list 10 { address: principal, share: uint })))
    (let ((total-stx (stx-get-balance owner)))
        (map distribute-stx-share beneficiaries)
        (ok true)))

(define-private (distribute-stx-share (beneficiary { address: principal, share: uint }))
    (let ((amount (* (get share beneficiary) u1000)))
        (stx-transfer? amount tx-sender (get address beneficiary))))

(define-private (distribute-token-assets (owner principal) (token-contract <token-trait>))
    (let ((allocation (unwrap! (get-token-allocation owner (contract-of token-contract)) (err u404))))
        (fold distribute-token-to-beneficiary (get beneficiary-shares allocation) { token-contract: token-contract, owner: owner })
        (ok true)))
(define-private (distribute-token-share (token-contract <token-trait>) (beneficiary { address: principal, share: uint }))
    (let ((amount (* (get share beneficiary) u100)))
        (contract-call? token-contract transfer amount tx-sender (get address beneficiary) none)))

(define-private (distribute-token-to-beneficiary (beneficiary { address: principal, share: uint }) (context { token-contract: <token-trait>, owner: principal }))
    (begin
        (unwrap-panic (distribute-token-share (get token-contract context) beneficiary))
        context))

(define-private (execute-emergency-request (request-id uint))
    (let ((request (unwrap! (map-get? emergency-requests request-id) ERR-NOT-REGISTERED)))
        (map-set emergency-requests request-id (merge request { is-executed: true }))
        (execute-will (get will-owner request))))

(define-public (propose-will-amendment (amendment-type (string-ascii 20))
                                      (new-beneficiaries (optional (list 10 { address: principal, share: uint })))
                                      (new-threshold (optional uint))
                                      (new-backup-executors (optional (list 3 principal)))
                                      (new-emergency-contact (optional principal)))
    (let ((existing-will (unwrap! (get-will tx-sender) ERR-NOT-REGISTERED))
          (amendment-id (+ (var-get amendment-counter) u1))
          (current-version (get-will-version tx-sender))
          (effective-block (+ burn-block-height (var-get amendment-grace-period))))
        (asserts! (not (get is-deceased existing-will)) ERR-ALREADY-DECEASED)
        (var-set amendment-counter amendment-id)
        (map-set will-amendments amendment-id {
            will-owner: tx-sender,
            amendment-type: amendment-type,
            new-beneficiaries: new-beneficiaries,
            new-threshold: new-threshold,
            new-backup-executors: new-backup-executors,
            new-emergency-contact: new-emergency-contact,
            proposed-at: burn-block-height,
            effective-at: effective-block,
            is-active: false,
            version: (+ current-version u1)
        })
        (ok amendment-id)))

(define-public (activate-amendment (amendment-id uint))
    (let ((amendment (unwrap! (map-get? will-amendments amendment-id) ERR-AMENDMENT-NOT-FOUND))
          (existing-will (unwrap! (get-will (get will-owner amendment)) ERR-NOT-REGISTERED)))
        (asserts! (is-eq tx-sender (get will-owner amendment)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get is-active amendment)) ERR-AMENDMENT-ALREADY-ACTIVE)
        (asserts! (>= burn-block-height (get effective-at amendment)) ERR-GRACE-PERIOD-ACTIVE)
        (asserts! (not (get is-deceased existing-will)) ERR-ALREADY-DECEASED)
        (let ((updated-will (merge existing-will {
                beneficiaries: (default-to (get beneficiaries existing-will) (get new-beneficiaries amendment)),
                inactivity-threshold: (default-to (get inactivity-threshold existing-will) (get new-threshold amendment)),
                backup-executors: (default-to (get backup-executors existing-will) (get new-backup-executors amendment)),
                emergency-contact: (default-to (get emergency-contact existing-will) (get new-emergency-contact amendment))
            })))
            (map-set wills (get will-owner amendment) updated-will)
            (map-set will-amendments amendment-id (merge amendment { is-active: true }))
            (map-set will-versions (get will-owner amendment) (get version amendment))
            (ok true))))

(define-public (cancel-amendment (amendment-id uint))
    (let ((amendment (unwrap! (map-get? will-amendments amendment-id) ERR-AMENDMENT-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get will-owner amendment)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get is-active amendment)) ERR-AMENDMENT-ALREADY-ACTIVE)
        (asserts! (< burn-block-height (get effective-at amendment)) ERR-AMENDMENT-EXPIRED)
        (map-delete will-amendments amendment-id)
        (ok true)))

(define-public (set-time-lock (time-lock-period uint))
    (let ((existing-will (unwrap! (get-will tx-sender) ERR-NOT-REGISTERED)))
        (asserts! (not (get is-deceased existing-will)) ERR-ALREADY-DECEASED)
        (asserts! (> time-lock-period u0) ERR-INVALID-BENEFICIARY)
        (map-set time-lock-configs tx-sender {
            time-lock-period: time-lock-period,
            execution-block: u0,
            is-time-locked: true
        })
        (ok true)))

(define-public (claim-inheritance (will-owner principal))
    (let ((will (unwrap! (get-will will-owner) ERR-NOT-REGISTERED))
          (claim-record (unwrap! (map-get? claim-records { will-owner: will-owner, beneficiary: tx-sender }) ERR-NOT-REGISTERED))
          (time-lock-config (unwrap! (map-get? time-lock-configs will-owner) ERR-WILL-NOT-EXECUTED)))
        (asserts! (get is-deceased will) ERR-STILL-ALIVE)
        (asserts! (get is-time-locked time-lock-config) ERR-WILL-NOT-EXECUTED)
        (asserts! (>= burn-block-height (get claimable-at claim-record)) ERR-TIME-LOCK-ACTIVE)
        (asserts! (not (get has-claimed claim-record)) ERR-ALREADY-CLAIMED)
        (let ((claimable-amount (- (get total-amount claim-record) (get claimed-amount claim-record))))
            (asserts! (> claimable-amount u0) ERR-NO-CLAIMABLE-AMOUNT)
            (unwrap! (as-contract (stx-transfer? claimable-amount will-owner tx-sender)) (err u500))
            (map-set claim-records { will-owner: will-owner, beneficiary: tx-sender }
                (merge claim-record {
                    claimed-amount: (get total-amount claim-record),
                    has-claimed: true
                }))
            (ok claimable-amount))))

(define-private (setup-beneficiary-claims (owner principal) (beneficiaries (list 10 { address: principal, share: uint })) (time-lock-period uint))
    (let ((total-stx (stx-get-balance owner))
          (claimable-at (+ burn-block-height time-lock-period)))
        (map setup-claim-record beneficiaries)
        (ok true)))

(define-private (setup-claim-record (beneficiary { address: principal, share: uint }))
    (let ((amount (* (get share beneficiary) u1000))
          (will-owner tx-sender)
          (time-lock-config (unwrap-panic (map-get? time-lock-configs will-owner)))
          (claimable-at (+ (get execution-block time-lock-config) (get time-lock-period time-lock-config))))
        (map-set claim-records { will-owner: will-owner, beneficiary: (get address beneficiary) }
            {
                total-amount: amount,
                claimed-amount: u0,
                claimable-at: claimable-at,
                has-claimed: false
            })
        true))

