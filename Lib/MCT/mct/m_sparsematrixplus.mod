V24 m_sparsematrixplus
22 m_SparseMatrixPlus.F90 S582 0
01/06/2014  21:00:27
use m_globalsegmap private
use m_rearranger private
use m_sparsematrix private
use m_string private
use m_globalsegmap private
use m_rearranger private
use m_sparsematrix private
use m_string private
enduse
D 44 24 599 88 597 7
D 53 21 6 1 0 15 0 0 0 0 0
 0 15 0 3 15 0
D 291 24 1291 1056 1290 7
D 436 24 1619 280 1616 7
D 687 24 2043 2488 2042 7
D 710 24 2091 6144 2090 7
D 716 18 789
D 718 18 119
D 720 21 6 1 0 3 0 0 0 0 0
 0 3 0 3 3 0
D 723 18 802
S 582 24 0 0 0 6 1 0 4659 10005 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 104 0 0 0 0 0 0 m_sparsematrixplus
S 584 23 0 0 0 8 597 582 4687 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 string
S 586 23 0 0 0 8 1290 582 4709 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 sparsematrix
S 588 23 0 0 0 8 2042 582 4735 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 rearranger
S 589 3 0 0 0 6 1 1 0 0 0 0 0 0 0 0 18 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 597 25 1 m_string string
R 599 5 3 m_string c string
R 600 5 4 m_string c$sd string
R 601 5 5 m_string c$p string
R 602 5 6 m_string c$o string
S 641 14 5 0 0 6 1 0 5061 40003814 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 pghpf_size
S 762 3 0 0 0 6 1 1 0 0 0 0 0 0 0 0 23 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 1290 25 3 m_sparsematrix sparsematrix
R 1291 5 4 m_sparsematrix nrows sparsematrix
R 1292 5 5 m_sparsematrix ncols sparsematrix
R 1293 5 6 m_sparsematrix data sparsematrix
R 1294 5 7 m_sparsematrix vecinit sparsematrix
R 1296 5 9 m_sparsematrix row_s sparsematrix
R 1297 5 10 m_sparsematrix row_s$sd sparsematrix
R 1298 5 11 m_sparsematrix row_s$p sparsematrix
R 1299 5 12 m_sparsematrix row_s$o sparsematrix
R 1301 5 14 m_sparsematrix row_e sparsematrix
R 1303 5 16 m_sparsematrix row_e$sd sparsematrix
R 1304 5 17 m_sparsematrix row_e$p sparsematrix
R 1305 5 18 m_sparsematrix row_e$o sparsematrix
R 1309 5 22 m_sparsematrix tcol sparsematrix
R 1310 5 23 m_sparsematrix tcol$sd sparsematrix
R 1311 5 24 m_sparsematrix tcol$p sparsematrix
R 1312 5 25 m_sparsematrix tcol$o sparsematrix
R 1316 5 29 m_sparsematrix twgt sparsematrix
R 1317 5 30 m_sparsematrix twgt$sd sparsematrix
R 1318 5 31 m_sparsematrix twgt$p sparsematrix
R 1319 5 32 m_sparsematrix twgt$o sparsematrix
R 1321 5 34 m_sparsematrix row_max sparsematrix
R 1322 5 35 m_sparsematrix row_min sparsematrix
R 1323 5 36 m_sparsematrix tbl_end sparsematrix
R 1616 25 1 m_globalsegmap globalsegmap
R 1619 5 4 m_globalsegmap comp_id globalsegmap
R 1620 5 5 m_globalsegmap gsize globalsegmap
R 1625 5 10 m_globalsegmap ngseg globalsegmap
R 1638 5 23 m_globalsegmap start globalsegmap
R 1639 5 24 m_globalsegmap start$sd globalsegmap
R 1640 5 25 m_globalsegmap start$p globalsegmap
R 1641 5 26 m_globalsegmap start$o globalsegmap
R 1644 5 29 m_globalsegmap length globalsegmap
R 1645 5 30 m_globalsegmap length$sd globalsegmap
R 1646 5 31 m_globalsegmap length$p globalsegmap
R 1647 5 32 m_globalsegmap length$o globalsegmap
R 1650 5 35 m_globalsegmap pe_loc globalsegmap
R 1651 5 36 m_globalsegmap pe_loc$sd globalsegmap
R 1652 5 37 m_globalsegmap pe_loc$p globalsegmap
R 1653 5 38 m_globalsegmap pe_loc$o globalsegmap
R 2042 25 2 m_rearranger rearranger
R 2043 5 3 m_rearranger sendrouter rearranger
R 2044 5 4 m_rearranger recvrouter rearranger
R 2047 5 7 m_rearranger localpack rearranger
R 2048 5 8 m_rearranger localpack$sd rearranger
R 2049 5 9 m_rearranger localpack$p rearranger
R 2050 5 10 m_rearranger localpack$o rearranger
R 2052 5 12 m_rearranger localsize rearranger
S 2090 25 0 0 0 710 1 582 11120 10000004 800014 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2120 0 0 0 582 0 0 0 0 sparsematrixplus
S 2091 5 0 0 0 44 2092 582 11137 800004 0 0 0 0 0 710 0 0 0 0 0 0 0 0 0 0 0 1 2091 0 582 0 0 0 0 strategy
S 2092 5 0 0 0 6 2093 582 11146 800004 0 0 88 0 0 710 0 0 0 0 0 0 0 0 0 0 0 2091 2092 0 582 0 0 0 0 xprimelength
S 2093 5 0 0 0 687 2094 582 11159 800004 0 0 96 0 0 710 0 0 0 0 0 0 0 0 0 0 0 2092 2093 0 582 0 0 0 0 xtoxprime
S 2094 5 0 0 0 6 2095 582 11169 800004 0 0 2584 0 0 710 0 0 0 0 0 0 0 0 0 0 0 2093 2094 0 582 0 0 0 0 yprimelength
S 2095 5 0 0 0 687 2096 582 11182 800004 0 0 2592 0 0 710 0 0 0 0 0 0 0 0 0 0 0 2094 2095 0 582 0 0 0 0 yprimetoy
S 2096 5 0 0 0 291 2097 582 11192 800004 0 0 5080 0 0 710 0 0 0 0 0 0 0 0 0 0 0 2095 2096 0 582 0 0 0 0 matrix
S 2097 5 0 0 0 6 1 582 11107 800004 0 0 6136 0 0 710 0 0 0 0 0 0 0 0 0 0 0 2096 2097 0 582 0 0 0 0 tag
S 2098 19 0 0 0 6 1 582 4815 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 317 2 0 0 0 0 0 582 0 0 0 0 init
O 2098 2 2104 2103
S 2099 19 0 0 0 8 1 582 7300 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 319 1 0 0 0 0 0 582 0 0 0 0 vecinit
O 2099 1 2105
S 2100 19 0 0 0 8 1 582 4833 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 321 1 0 0 0 0 0 582 0 0 0 0 clean
O 2100 1 2106
S 2101 19 0 0 0 6 1 582 11199 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 323 1 0 0 0 0 0 582 0 0 0 0 initialized
O 2101 1 2107
S 2102 19 0 0 0 8 1 582 11211 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 325 1 0 0 0 0 0 582 0 0 0 0 exportstrategytochar
O 2102 1 2108
S 2103 27 0 0 0 6 2121 582 11232 10000 0 0 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 initfromroot_
Q 2103 2098 0
S 2104 27 0 0 0 6 2132 582 11246 10000 0 0 0 327 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 initdistributed_
Q 2104 2098 0
S 2105 27 0 0 0 8 2142 582 7880 10000 0 0 0 328 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 vecinit_
Q 2105 2099 0
S 2106 27 0 0 0 8 2145 582 4941 10000 0 0 0 329 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 clean_
Q 2106 2100 0
S 2107 27 0 0 0 6 2149 582 11263 10000 0 0 0 330 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 initialized_
Q 2107 2101 0
S 2108 27 0 0 0 8 2153 582 11276 10000 0 0 0 331 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 exportstrategytochar_
Q 2108 2102 0
S 2109 16 0 0 0 716 1 582 11298 4 440000 0 0 0 0 2115 790 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 xonly
S 2110 16 0 0 0 716 1 582 11304 4 440000 0 0 0 0 2116 792 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 yonly
S 2111 16 0 0 0 716 1 582 11310 4 440000 0 0 0 0 2117 794 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 xandy
S 2112 16 0 0 0 6 1 582 10976 4 400000 0 0 0 0 700 787 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 defaulttag
S 2113 3 0 0 0 6 0 1 0 0 0 0 0 0 0 0 700 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 2114 3 0 0 0 6 0 1 0 0 0 0 0 0 0 0 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 2115 3 0 0 0 716 0 1 0 0 0 0 0 0 0 11316 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 5 58 6f 6e 6c 79
S 2116 3 0 0 0 716 0 1 0 0 0 0 0 0 0 11322 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 5 59 6f 6e 6c 79
S 2117 3 0 0 0 716 0 1 0 0 0 0 0 0 0 11328 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 5 58 61 6e 64 59
S 2118 3 0 0 0 718 0 1 0 0 0 0 0 0 0 11334 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 23 4d 43 54 3a 3a 6d 5f 53 70 61 72 73 65 4d 61 74 72 69 78 50 6c 75 73
S 2119 16 0 0 0 718 1 582 5019 4 440000 0 0 0 0 2118 796 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 myname
S 2120 8 5 0 0 720 1 582 11358 40022004 1220 0 0 0 710 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 m_sparsematrixplus$sparsematrixplus$td
S 2121 23 5 0 0 0 2131 582 11232 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 initfromroot_
S 2122 1 3 2 0 710 1 2121 11397 4 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smatplus
S 2123 1 3 3 0 291 1 2121 8579 4 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smat
S 2124 1 3 1 0 436 1 2121 11406 4 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 xgsmap
S 2125 1 3 1 0 436 1 2121 11413 4 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ygsmap
S 2126 1 3 1 0 28 1 2121 11137 4 43000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 strategy
S 2127 1 3 1 0 6 1 2121 5125 4 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 2128 1 3 1 0 6 1 2121 5130 4 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 2129 1 3 1 0 6 1 2121 11420 4 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 componentid
S 2130 1 3 1 0 6 1 2121 11107 80000004 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tag
S 2131 14 5 0 0 0 1 2121 11232 0 400000 0 0 0 586 9 0 0 0 0 0 0 0 0 0 0 0 0 212 0 582 0 0 0 0 initfromroot_
F 2131 9 2122 2123 2124 2125 2126 2127 2128 2129 2130
S 2132 23 5 0 0 0 2141 582 11246 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 initdistributed_
S 2133 1 3 2 0 710 1 2132 11397 4 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smatplus
S 2134 1 3 3 0 291 1 2132 8579 4 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smat
S 2135 1 3 1 0 436 1 2132 11406 4 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 xgsmap
S 2136 1 3 1 0 436 1 2132 11413 4 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ygsmap
S 2137 1 3 1 0 6 1 2132 5125 4 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 2138 1 3 1 0 6 1 2132 5130 4 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 2139 1 3 1 0 6 1 2132 11420 4 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 componentid
S 2140 1 3 1 0 6 1 2132 11107 80000004 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tag
S 2141 14 5 0 0 0 1 2132 11246 0 400000 0 0 0 596 8 0 0 0 0 0 0 0 0 0 0 0 0 408 0 582 0 0 0 0 initdistributed_
F 2141 8 2133 2134 2135 2136 2137 2138 2139 2140
S 2142 23 5 0 0 0 2144 582 7880 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 vecinit_
S 2143 1 3 3 0 710 1 2142 11432 4 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smatp
S 2144 14 5 0 0 0 1 2142 7880 0 400000 0 0 0 605 1 0 0 0 0 0 0 0 0 0 0 0 0 553 0 582 0 0 0 0 vecinit_
F 2144 1 2143
S 2145 23 5 0 0 0 2148 582 4941 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 clean_
S 2146 1 3 3 0 710 1 2145 11432 4 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smatp
S 2147 1 3 2 0 6 1 2145 5939 80000004 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 status
S 2148 14 5 0 0 0 1 2145 4941 0 400000 0 0 0 607 2 0 0 0 0 0 0 0 0 0 0 0 0 595 0 582 0 0 0 0 clean_
F 2148 2 2146 2147
S 2149 23 5 0 0 16 2151 582 11263 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 initialized_
S 2150 1 3 1 0 710 1 2149 11397 4 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smatplus
S 2151 14 5 0 0 16 1 2149 11263 4 400000 0 0 0 610 1 0 0 2152 0 0 0 0 0 0 0 0 0 751 0 582 0 0 0 0 initialized_
F 2151 1 2150
S 2152 1 3 0 0 16 1 2149 11263 4 1003000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 initialized_
S 2153 23 5 0 0 8 2155 582 11276 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 exportstrategytochar_
S 2154 6 3 1 0 710 1 2153 11397 800004 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smatplus
S 2155 14 5 0 0 723 1 2153 11276 4 480000 0 0 0 612 1 0 0 2156 0 0 0 0 0 0 0 0 0 822 0 582 0 0 0 0 exportstrategytochar_
F 2155 1 2154
S 2156 1 3 0 0 723 1 2153 11276 4 1083000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 exportstrategytochar_
A 15 2 0 0 0 6 589 0 0 0 15 0 0 0 0 0 0 0 0 0
A 16 1 0 1 0 53 600 0 0 0 0 0 0 0 0 0 0 0 0 0
A 29 1 0 0 0 6 641 0 0 0 0 0 0 0 0 0 0 0 0 0
A 119 2 0 0 0 6 762 0 0 0 119 0 0 0 0 0 0 0 0 0
A 787 2 0 0 549 6 2113 0 0 0 787 0 0 0 0 0 0 0 0 0
A 789 2 0 0 551 6 2114 0 0 0 789 0 0 0 0 0 0 0 0 0
A 790 2 0 0 0 716 2115 0 0 0 790 0 0 0 0 0 0 0 0 0
A 792 2 0 0 0 716 2116 0 0 0 792 0 0 0 0 0 0 0 0 0
A 794 2 0 0 623 716 2117 0 0 0 794 0 0 0 0 0 0 0 0 0
A 796 2 0 0 772 718 2118 0 0 0 796 0 0 0 0 0 0 0 0 0
A 798 1 0 0 0 710 2154 0 0 0 0 0 0 0 0 0 0 0 0 0
A 799 1 0 0 0 44 2091 0 0 0 0 0 0 0 0 0 0 0 0 0
A 800 9 0 0 0 44 798 799 0 0 0 0 0 0 0 0 0 0 0 0
A 801 9 0 1 202 53 800 16 0 0 0 0 0 0 0 0 0 0 0 0
A 802 13 0 0 0 6 29 0 0 0 0 0 0 0 2 16 0 0 0 0
W 2 5 801
Z
Z
