# 🏡 Land Title NFT Smart Contract

## 🌟 Overview
This smart contract enables the digitization and secure transfer of property titles on the Stacks blockchain. Property titles are represented as non-fungible tokens (NFTs) with detailed metadata about each property.

## 🔑 Key Features
- ✅ Register new land titles as NFTs
- 🔄 Transfer ownership of titles
- 💰 List titles for sale and purchase them
- 🔐 Verification system for authorities to validate titles
- 🔍 Query functionality to view title details

## 📋 Contract Functions

### 📖 Read-Only Functions
- `get-title-details`: Get complete details of a specific title
- `get-title-owner`: Find the current owner of a title
- `get-titles-by-owner`: List all titles owned by a specific address
- `get-title-by-address`: Find a title ID using a property address
- `is-title-for-sale`: Check if a title is listed for sale
- `get-title-price`: Get the asking price of a title
- `is-verification-authority`: Check if an address is authorized to verify titles

### ✏️ Public Functions
- `register-title`: Create a new land title NFT
- `transfer-title`: Transfer a title to another owner
- `list-title-for-sale`: Put a title up for sale with a price
- `cancel-sale`: Remove a title from sale
- `buy-title`: Purchase a title that's for sale
- `add-verification-authority`: Add an address that can verify titles (owner only)
- `remove-verification-authority`: Remove a verification authority (owner only)
- `verify-title`: Mark a title as verified (verification authorities only)

## 🚀 Usage Examples

### Registering a New Title
```clarity
(contract-call? .landTitle register-title "123 Main St, City, Country" u1000 "Residential" "New York")
```

### Listing a Title for Sale
```clarity
(contract-call? .landTitle list-title-for-sale u1 u50000000)
```

### Buying a Title
```clarity
(contract-call? .landTitle buy-title u1)
```

### Transferring a Title
```clarity
(contract-call? .landTitle transfer-title u1 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

## ⚠️ Important Notes
- The contract owner is set to the deploying address
- Property addresses must be unique
- Titles must be verified by an authorized authority for full legal recognition
- Prices are denominated in microSTX (1 STX = 1,000,000 microSTX)

## 🔒 Error Codes
- `u100`: Not the contract owner
- `u101`: Title not found
- `u102`: Unauthorized operation
- `u103`: Property already registered
- `u104`: Invalid title data
- `u105`: Title not for sale
- `u106`: Insufficient payment


