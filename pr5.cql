import A.csv as A |
import B.csv as B |
take [x1,x2] where exists [z] . A [x1,z] ^ B [z , x2]|
