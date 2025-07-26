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