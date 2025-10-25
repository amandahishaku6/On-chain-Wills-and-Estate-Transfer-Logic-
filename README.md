# On-chain Wills and Estate Transfer Logic 
# 🏛️ On-chain Wills and Estate Transfer Logic 

A decentralized solution for managing digital asset inheritance using smart contracts on the Stacks blockchain.

## 🎯 Features

- ✨ Register digital wills with multiple beneficiaries
- 🔄 Automatic asset distribution on death confirmation
- ⏰ Inactivity monitoring and triggers
- ⚖️ DAO-based dispute resolution system
- 🔐 Secure and transparent execution

## 📝 Contract Functions

### For Will Creators
- `register-will`: Create a new will with beneficiaries and inactivity threshold
- `update-activity`: Reset the inactivity counter

### For Oracles
- `report-death`: Trigger will execution upon confirmed death
- `check-inactivity`: Check and execute will if inactivity threshold exceeded

### For Beneficiaries
- `raise-dispute`: Contest will execution or terms
- `get-will`: View will details
- `get-dispute`: Check dispute status

## 🚀 Getting Started

1. Deploy the contract using Clarinet:
```bash
clarinet deploy
```

2. Register your will:
```bash
clarinet contract-call .on-chain-wills register-will [beneficiaries] [threshold]
```

3. Monitor status:
```bash
clarinet contract-call .on-chain-wills get-will [address]
```

## 🔒 Security Considerations

- Oracle addresses must be trusted entities
- Beneficiary addresses should be verified
- Regular activity updates recommended
- Keep private keys secure

## 🤝 Contributing

PRs welcome! Please ensure tests pass and follow coding standards.

## 📜 License

MIT
```


