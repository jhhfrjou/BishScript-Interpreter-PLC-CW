import R.csv |
take [x1,x2,x3,x4,x5] where R [x1, x2] ^ R[x2, x3] ^ R[x3, x4] ^ R[x4, x5] ^ x1 = x5 |