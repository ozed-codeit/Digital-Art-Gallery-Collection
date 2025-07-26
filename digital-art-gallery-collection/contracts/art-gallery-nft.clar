;; Art Gallery Collection NFT Contract
;; NFT contract for curated digital art collections with exhibition tracking

(impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)

(define-non-fungible-token gallery-art uint)

(define-data-var last-token-id uint u0)
(define-data-var contract-owner principal tx-sender)
(define-data-var base-uri (string-ascii 256) "https://api.artgallery.stacks/art/")

(define-map artwork-details uint {
    artist-name: (string-ascii 50),
    artwork-title: (string-ascii 100),
    medium: (string-ascii 30),
    creation-year: uint,
    dimensions: (string-ascii 50),
    gallery-curator: principal
})

(define-map exhibition-history uint (list 10 {
    exhibition-name: (string-ascii 100),
    venue: (string-ascii 50),
    exhibition-year: uint
}))

(define-map artwork-provenance uint (list 5 principal))

(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INVALID-PARAMS (err u400))
(define-constant ERR-TOO-MANY-EXHIBITIONS (err u413))

(define-read-only (get-last-token-id)
    (ok (var-get last-token-id))
)

(define-read-only (get-token-uri (token-id uint))
    (ok (some (concat (var-get base-uri) (uint-to-ascii token-id))))
)

(define-read-only (get-owner (token-id uint))
    (ok (nft-get-owner? gallery-art token-id))
)

(define-read-only (get-artwork-details (token-id uint))
    (map-get? artwork-details token-id)
)

(define-read-only (get-exhibition-history (token-id uint))
    (map-get? exhibition-history token-id)
)

(define-read-only (get-artwork-provenance (token-id uint))
    (map-get? artwork-provenance token-id)
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (let
        (
            (current-provenance (default-to (list) (map-get? artwork-provenance token-id)))
        )
        (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
        (asserts! (is-some (nft-get-owner? gallery-art token-id)) ERR-NOT-FOUND)
        
        ;; Update provenance chain
        (if (< (len current-provenance) u5)
            (map-set artwork-provenance token-id (unwrap-panic (as-max-len? (append current-provenance recipient) u5)))
            (map-set artwork-provenance token-id (unwrap-panic (as-max-len? (append (unwrap-panic (slice? current-provenance u1 u5)) recipient) u5)))
        )
        
        (nft-transfer? gallery-art token-id sender recipient)
    )
)

(define-public (mint-artwork
    (recipient principal)
    (artist-name (string-ascii 50))
    (artwork-title (string-ascii 100))
    (medium (string-ascii 30))
    (creation-year uint)
    (dimensions (string-ascii 50))
)
    (let
        (
            (next-id (+ (var-get last-token-id) u1))
        )
        (asserts! (> creation-year u1800) ERR-INVALID-PARAMS)
        (asserts! (<= creation-year u2030) ERR-INVALID-PARAMS)
        
        (try! (nft-mint? gallery-art next-id recipient))
        
        (map-set artwork-details next-id {
            artist-name: artist-name,
            artwork-title: artwork-title,
            medium: medium,
            creation-year: creation-year,
            dimensions: dimensions,
            gallery-curator: tx-sender
        })
        
        ;; Initialize provenance with first owner
        (map-set artwork-provenance next-id (list recipient))
        
        (var-set last-token-id next-id)
        (ok next-id)
    )
)

(define-public (add-to-exhibition
    (token-id uint)
    (exhibition-name (string-ascii 100))
    (venue (string-ascii 50))
    (exhibition-year uint)
)
    (let
        (
            (current-exhibitions (default-to (list) (map-get? exhibition-history token-id)))
            (new-exhibition {
                exhibition-name: exhibition-name,
                venue: venue,
                exhibition-year: exhibition-year
            })
        )
        (asserts! (is-some (nft-get-owner? gallery-art token-id)) ERR-NOT-FOUND)
        (asserts! (> exhibition-year u2000) ERR-INVALID-PARAMS)
        (asserts! (< (len current-exhibitions) u10) ERR-TOO-MANY-EXHIBITIONS)
        
        (map-set exhibition-history token-id 
            (unwrap-panic (as-max-len? (append current-exhibitions new-exhibition) u10)))
        (ok true)
    )
)

(define-public (update-base-uri (new-uri (string-ascii 256)))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set base-uri new-uri)
        (ok true)
    )
)