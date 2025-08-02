# 🎫 Voucher Redemption System

A smart contract system built on Stacks blockchain that enables the creation, management, and redemption of digital vouchers for products and services. Perfect for learning about redeemable tokens and digital asset management! 

## 🌟 Features

- 🏪 **Product Management**: Create and manage products with stock tracking
- 🎟️ **Voucher Issuance**: Issue digital vouchers with expiration dates
- 🔄 **Voucher Redemption**: Redeem vouchers for products/services
- 📊 **Transfer System**: Transfer vouchers between users
- 📈 **History Tracking**: Complete redemption history
- 🔍 **Query Functions**: Check voucher validity and user holdings

## 🚀 Quick Start

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Basic understanding of Clarity smart contracts

### Installation

```bash
git clone <your-repo>
cd voucher-redemption-system
clarinet check
```

## 📋 Contract Functions

### 🏪 Product Management (Owner Only)

#### Create Product
```clarity
(contract-call? .voucher-redemption-system create-product "Coffee" "Premium coffee voucher" u500 u100)
```

#### Update Stock
```clarity
(contract-call? .voucher-redemption-system update-product-stock u1 u50)
```

#### Toggle Product Status
```clarity
(contract-call? .voucher-redemption-system toggle-product-status u1)
```

### 🎫 Voucher Operations

#### Issue Voucher (Owner Only)
```clarity
(contract-call? .voucher-redemption-system issue-voucher 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM u1 u500 u1000)
```

#### Redeem Voucher
```clarity
(contract-call? .voucher-redemption-system redeem-voucher u1)
```

#### Transfer Voucher
```clarity
(contract-call? .voucher-redemption-system transfer-voucher u1 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG)
```

### 🔍 Query Functions

#### Get Voucher Details
````clarity
(contract-call? .voucher-redemption-system get-

