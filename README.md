# Minimal Blockchain written in Haskell

This is a MVP blockchain (no p2p communication, no concensus) written in Haskell. Written for educational purposes only.

# Running bch
```
stack build
stack exec bch-exe
```

# API
```
GET 127.0.0.1:8080/           - Displays the entire blockchain
GET 127.0.0.1:8080/current/   - Displays the current block head

POST 127.0.0.1:8080/mine/     - Mines the list of transactions in 
                                the current block head
POST 127.0.0.1:8080/addtx/    - Adds transaction to current block head                                
```