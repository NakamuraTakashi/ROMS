V24 m_matattrvectmul
20 m_MatAttrVectMul.F90 S582 0
01/06/2014  21:00:28
use m_sparsematrix private
use m_sparsematrixplus private
use m_attrvect private
use m_sparsematrix private
use m_sparsematrixplus private
use m_attrvect private
enduse
D 44 18 12
D 184 24 977 624 976 7
D 438 24 977 624 976 7
D 444 24 1293 1056 1292 7
D 770 24 2112 6144 2111 7
S 582 24 0 0 0 6 1 0 4659 10005 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 32 0 0 0 0 0 0 m_matattrvectmul
S 583 19 0 0 0 8 1 582 4676 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 4 2 0 0 0 0 0 582 0 0 0 0 smatavmult
O 583 2 585 584
S 584 27 0 0 0 8 1616 582 4687 10010 0 0 0 247 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 smatavmult_datalocal_
Q 584 583 0
S 585 27 0 0 0 8 2172 582 4709 10010 0 0 0 328 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 smatavmult_smplus_
Q 585 583 0
S 586 3 0 0 0 6 0 1 0 0 0 0 0 0 0 0 21 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 587 3 0 0 0 44 0 1 0 0 0 0 0 0 0 4728 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 21 4d 43 54 3a 3a 6d 5f 4d 61 74 41 74 74 72 56 65 63 74 4d 75 6c
S 588 16 0 0 0 44 1 582 4750 14 440000 0 0 0 0 587 13 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 myname
R 976 25 5 m_attrvect attrvect
R 977 5 6 m_attrvect ilist attrvect
R 978 5 7 m_attrvect rlist attrvect
R 981 5 10 m_attrvect iattr attrvect
R 982 5 11 m_attrvect iattr$sd attrvect
R 983 5 12 m_attrvect iattr$p attrvect
R 984 5 13 m_attrvect iattr$o attrvect
R 988 5 17 m_attrvect rattr attrvect
R 989 5 18 m_attrvect rattr$sd attrvect
R 990 5 19 m_attrvect rattr$p attrvect
R 991 5 20 m_attrvect rattr$o attrvect
R 1292 25 3 m_sparsematrix sparsematrix
R 1293 5 4 m_sparsematrix nrows sparsematrix
R 1294 5 5 m_sparsematrix ncols sparsematrix
R 1295 5 6 m_sparsematrix data sparsematrix
R 1296 5 7 m_sparsematrix vecinit sparsematrix
R 1298 5 9 m_sparsematrix row_s sparsematrix
R 1299 5 10 m_sparsematrix row_s$sd sparsematrix
R 1300 5 11 m_sparsematrix row_s$p sparsematrix
R 1301 5 12 m_sparsematrix row_s$o sparsematrix
R 1303 5 14 m_sparsematrix row_e sparsematrix
R 1305 5 16 m_sparsematrix row_e$sd sparsematrix
R 1306 5 17 m_sparsematrix row_e$p sparsematrix
R 1307 5 18 m_sparsematrix row_e$o sparsematrix
R 1311 5 22 m_sparsematrix tcol sparsematrix
R 1312 5 23 m_sparsematrix tcol$sd sparsematrix
R 1313 5 24 m_sparsematrix tcol$p sparsematrix
R 1314 5 25 m_sparsematrix tcol$o sparsematrix
R 1318 5 29 m_sparsematrix twgt sparsematrix
R 1319 5 30 m_sparsematrix twgt$sd sparsematrix
R 1320 5 31 m_sparsematrix twgt$p sparsematrix
R 1321 5 32 m_sparsematrix twgt$o sparsematrix
R 1323 5 34 m_sparsematrix row_max sparsematrix
R 1324 5 35 m_sparsematrix row_min sparsematrix
R 1325 5 36 m_sparsematrix tbl_end sparsematrix
S 1616 23 5 0 0 0 1623 582 4687 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smatavmult_datalocal_
S 1617 1 3 1 0 438 1 1616 9066 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 xav
S 1618 1 3 3 0 444 1 1616 8627 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smat
S 1619 1 3 3 0 438 1 1616 9070 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 yav
S 1620 1 3 1 0 16 1 1616 7103 80000014 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 vector
S 1621 1 3 1 0 28 1 1616 6187 80000014 43000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rlist
S 1622 1 3 1 0 28 1 1616 7110 80000014 43000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 trlist
S 1623 14 5 0 0 0 1 1616 4687 10 400000 0 0 0 440 6 0 0 0 0 0 0 0 0 0 0 0 0 96 0 582 0 0 0 0 smatavmult_datalocal_
F 1623 6 1617 1618 1619 1620 1621 1622
R 2111 25 4 m_sparsematrixplus sparsematrixplus
R 2112 5 5 m_sparsematrixplus strategy sparsematrixplus
R 2113 5 6 m_sparsematrixplus xprimelength sparsematrixplus
R 2114 5 7 m_sparsematrixplus xtoxprime sparsematrixplus
R 2115 5 8 m_sparsematrixplus yprimelength sparsematrixplus
R 2116 5 9 m_sparsematrixplus yprimetoy sparsematrixplus
R 2117 5 10 m_sparsematrixplus matrix sparsematrixplus
R 2118 5 11 m_sparsematrixplus tag sparsematrixplus
S 2172 23 5 0 0 0 2179 582 4709 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smatavmult_smplus_
S 2173 1 3 1 0 184 1 2172 9066 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 xav
S 2174 1 3 3 0 770 1 2172 11496 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smatplus
S 2175 1 3 3 0 184 1 2172 9070 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 yav
S 2176 1 3 1 0 16 1 2172 7103 80000014 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 vector
S 2177 1 3 1 0 28 1 2172 6187 80000014 43000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rlist
S 2178 1 3 1 0 28 1 2172 7110 80000014 43000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 trlist
S 2179 14 5 0 0 0 1 2172 4709 10 400000 0 0 0 621 6 0 0 0 0 0 0 0 0 0 0 0 0 427 0 582 0 0 0 0 smatavmult_smplus_
F 2179 6 2173 2174 2175 2176 2177 2178
A 12 2 0 0 0 6 586 0 0 0 12 0 0 0 0 0 0 0 0 0
A 13 2 0 0 0 44 587 0 0 0 13 0 0 0 0 0 0 0 0 0
Z
Z
