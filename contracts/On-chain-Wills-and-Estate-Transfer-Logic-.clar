(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-REGISTERED (err u101))
(define-constant ERR-NOT-REGISTERED (err u102))
(define-constant ERR-INVALID-BENEFICIARY (err u103))
(define-constant ERR-ALREADY-DECEASED (err u104))
(define-constant ERR-STILL-ALIVE (err u105))

(define-data-var oracle-address principal 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
(define-data-var dao-address principal 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)

(define-map wills
    principal
    {
        beneficiaries: (list 10 { address: principal, share: uint }),
        last-activity: uint,
        inactivity-threshold: uint,
        stx-balance: uint,
        is-deceased: bool
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

(define-data-var dispute-counter uint u0)

(define-public (register-will (beneficiaries (list 10 { address: principal, share: uint })) 
                            (inactivity-threshold uint))
    (let ((existing-will (get-will tx-sender)))
        (asserts! (is-none existing-will) ERR-ALREADY-REGISTERED)
        (asserts! (> (len beneficiaries) u0) ERR-INVALID-BENEFICIARY)
        (map-set wills tx-sender {
            beneficiaries: beneficiaries,
            last-activity: burn-block-height,
            inactivity-threshold: inactivity-threshold,
            stx-balance: (stx-get-balance tx-sender),
            is-deceased: false
        })
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

(define-read-only (get-will (owner principal))
    (map-get? wills owner))

(define-read-only (get-dispute (dispute-id uint))
    (map-get? pending-disputes dispute-id))

(define-private (execute-will (owner principal))
    (let ((will (unwrap! (get-will owner) ERR-NOT-REGISTERED)))
        (map-set wills owner (merge will { is-deceased: true }))
        (unwrap! (distribute-assets owner (get beneficiaries will)) (err u500))
        (ok true)))
(define-private (distribute-assets (owner principal) (beneficiaries (list 10 { address: principal, share: uint })))
    (let ((total-stx (stx-get-balance owner)))
        (map distribute-share beneficiaries)
        (ok true)))

(define-private (distribute-share (beneficiary { address: principal, share: uint }))
    (let ((amount (* (get share beneficiary) u1000)))
        (stx-transfer? amount tx-sender (get address beneficiary))))
