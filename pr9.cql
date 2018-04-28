import A.csv |
import B.csv |
import C.csv |
take [x1,x2] where exists [z1,z2] .  A[x1, z1] ^ B[z1, z2] ^ C[z2, x2] |