# Art Gallery Collection NFT Contract

A Clarity smart contract for managing curated digital art collections with exhibition history and provenance tracking.

## Features

- Mint artwork NFTs with complete artistic metadata
- Track exhibition history (up to 10 exhibitions per artwork)
- Automatic provenance chain recording on transfers
- Artist and curator information storage
- Medium, dimensions, and creation year tracking
- Standard NFT trait compliance

## Contract Functions

### Public Functions
- `mint-artwork`: Create a new artwork NFT with metadata
- `transfer`: Transfer artwork (automatically updates provenance)
- `add-to-exhibition`: Add artwork to exhibition history
- `update-base-uri`: Update metadata base URI (owner only)

### Read-Only Functions
- `get-last-token-id`: Get the latest minted token ID
- `get-token-uri`: Get metadata URI for an artwork
- `get-owner`: Get current owner of an artwork
- `get-artwork-details`: Get artwork and artist information
- `get-exhibition-history`: Get complete exhibition history
- `get-artwork-provenance`: Get ownership history chain

## Usage

Call `mint-artwork` with artist details and artwork information to create an NFT. Use `add-to-exhibition` to record when artworks are displayed in galleries or shows.