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

;; NEW FUNCTION 1: Get artworks by artist
;; Returns a list of token IDs for artworks created by a specific artist
(define-read-only (get-artworks-by-artist (artist-name (string-ascii 50)))
    (let
        (
            (max-id (var-get last-token-id))
        )
        (filter is-artwork-by-artist 
            (map uint-to-token-id (generate-uint-list max-id)))
    )
)

;; Helper function for get-artworks-by-artist
(define-private (is-artwork-by-artist (token-id uint))
    (match (get-artwork-details token-id)
        artwork-info (is-eq (get artist-name artwork-info) artist-name)
        false
    )
)

;; Helper function to generate a list of uints from 1 to n
(define-private (generate-uint-list (n uint))
    (map + (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 
              u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32 u33 u34 u35 u36 u37 u38 u39 u40
              u41 u42 u43 u44 u45 u46 u47 u48 u49 u50 u51 u52 u53 u54 u55 u56 u57 u58 u59 u60
              u61 u62 u63 u64 u65 u66 u67 u68 u69 u70 u71 u72 u73 u74 u75 u76 u77 u78 u79 u80
              u81 u82 u83 u84 u85 u86 u87 u88 u89 u90 u91 u92 u93 u94 u95 u96 u97 u98 u99 u100)
           (list u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 
              u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0
              u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0
              u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0
              u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0))
)

(define-private (uint-to-token-id (n uint))
    (if (<= n (var-get last-token-id)) n u0)
)

;; NEW FUNCTION 2: Burn artwork (only by owner)
;; Allows the current owner to permanently destroy/burn an NFT
(define-public (burn-artwork (token-id uint))
    (let
        (
            (current-owner (unwrap! (nft-get-owner? gallery-art token-id) ERR-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender current-owner) ERR-NOT-AUTHORIZED)
        
        ;; Remove artwork from all maps
        (map-delete artwork-details token-id)
        (map-delete exhibition-history token-id)
        (map-delete artwork-provenance token-id)
        
        ;; Burn the NFT
        (nft-burn? gallery-art token-id current-owner)
    )
)

(define-read-only (uint-to-ascii (value uint))
    (if (<= value u9)
        (unwrap-panic (element-at "0123456789" value))
        (get r (fold uint-to-ascii-inner 
            0x000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
            {r: "", v: value}))
)

(define-private (uint-to-ascii-inner (i (buff 1)) (d {r: (string-ascii 39), v: uint}))
    (if (> (get v d) u0)
        {
            r: (unwrap-panic (as-max-len? (concat (unwrap-panic (element-at "0123456789" (mod (get v d) u10))) (get r d)) u39)),
            v: (/ (get v d) u10)
        }
        d
    )
)