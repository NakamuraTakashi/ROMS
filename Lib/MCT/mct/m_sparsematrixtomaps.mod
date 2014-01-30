V24 m_sparsematrixtomaps
24 m_SparseMatrixToMaps.F90 S582 0
01/06/2014  21:00:27
use m_globalsegmap private
use m_sparsematrix private
use m_globalsegmap private
use m_sparsematrix private
enduse
D 291 24 1288 1056 1287 7
D 436 18 502
D 438 24 1623 280 1620 7
D 550 24 1623 280 1620 7
D 556 21 6 1 639 642 1 1 0 0 1
 3 640 3 3 640 641
D 559 21 6 1 644 650 0 1 0 0 1
 645 648 649 645 648 646
D 562 21 6 1 0 15 0 0 0 0 0
 0 15 0 3 15 0
D 565 21 6 1 652 658 0 1 0 0 1
 653 656 657 653 656 654
D 568 21 6 1 0 15 0 0 0 0 0
 0 15 0 3 15 0
S 582 24 0 0 0 6 1 0 4659 10005 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 23 0 0 0 0 0 0 m_sparsematrixtomaps
S 584 23 0 0 0 8 1287 582 4695 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 sparsematrix
S 586 3 0 0 0 6 1 1 0 0 0 0 0 0 0 0 18 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 587 3 0 0 0 6 1 1 0 0 0 0 0 0 0 0 13 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 588 3 0 0 0 6 1 1 0 0 0 0 0 0 0 0 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 589 3 0 0 0 6 1 1 0 0 0 0 0 0 0 0 17 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 590 3 0 0 0 6 1 1 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 591 3 0 0 0 6 1 1 0 0 0 0 0 0 0 0 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 1287 25 3 m_sparsematrix sparsematrix
R 1288 5 4 m_sparsematrix nrows sparsematrix
R 1289 5 5 m_sparsematrix ncols sparsematrix
R 1290 5 6 m_sparsematrix data sparsematrix
R 1291 5 7 m_sparsematrix vecinit sparsematrix
R 1293 5 9 m_sparsematrix row_s sparsematrix
R 1294 5 10 m_sparsematrix row_s$sd sparsematrix
R 1295 5 11 m_sparsematrix row_s$p sparsematrix
R 1296 5 12 m_sparsematrix row_s$o sparsematrix
R 1298 5 14 m_sparsematrix row_e sparsematrix
R 1300 5 16 m_sparsematrix row_e$sd sparsematrix
R 1301 5 17 m_sparsematrix row_e$p sparsematrix
R 1302 5 18 m_sparsematrix row_e$o sparsematrix
R 1306 5 22 m_sparsematrix tcol sparsematrix
R 1307 5 23 m_sparsematrix tcol$sd sparsematrix
R 1308 5 24 m_sparsematrix tcol$p sparsematrix
R 1309 5 25 m_sparsematrix tcol$o sparsematrix
R 1313 5 29 m_sparsematrix twgt sparsematrix
R 1314 5 30 m_sparsematrix twgt$sd sparsematrix
R 1315 5 31 m_sparsematrix twgt$p sparsematrix
R 1316 5 32 m_sparsematrix twgt$o sparsematrix
R 1318 5 34 m_sparsematrix row_max sparsematrix
R 1319 5 35 m_sparsematrix row_min sparsematrix
R 1320 5 36 m_sparsematrix tbl_end sparsematrix
S 1611 19 0 0 0 8 1 582 8996 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 245 1 0 0 0 0 0 582 0 0 0 0 sparsematrixtoxglobalsegmap
O 1611 1 1613
S 1612 19 0 0 0 8 1 582 9024 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 247 1 0 0 0 0 0 582 0 0 0 0 sparsematrixtoyglobalsegmap
O 1612 1 1614
S 1613 27 0 0 0 8 1909 582 9052 10010 0 0 0 300 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 sparsematrixtoxglobalsegmap_
Q 1613 1611 0
S 1614 27 0 0 0 8 1916 582 9081 10010 0 0 0 301 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 sparsematrixtoyglobalsegmap_
Q 1614 1612 0
S 1615 3 0 0 0 6 0 1 0 0 0 0 0 0 0 0 25 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 1616 3 0 0 0 436 0 1 0 0 0 0 0 0 0 9110 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 25 4d 43 54 3a 3a 6d 5f 53 70 61 72 73 65 4d 61 74 72 69 78 54 6f 4d 61 70 73
S 1617 16 0 0 0 436 1 582 4997 14 440000 0 0 0 0 1616 503 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 myname
R 1620 25 1 m_globalsegmap globalsegmap
R 1623 5 4 m_globalsegmap comp_id globalsegmap
R 1624 5 5 m_globalsegmap gsize globalsegmap
R 1629 5 10 m_globalsegmap ngseg globalsegmap
R 1642 5 23 m_globalsegmap start globalsegmap
R 1643 5 24 m_globalsegmap start$sd globalsegmap
R 1644 5 25 m_globalsegmap start$p globalsegmap
R 1645 5 26 m_globalsegmap start$o globalsegmap
R 1648 5 29 m_globalsegmap length globalsegmap
R 1649 5 30 m_globalsegmap length$sd globalsegmap
R 1650 5 31 m_globalsegmap length$p globalsegmap
R 1651 5 32 m_globalsegmap length$o globalsegmap
R 1654 5 35 m_globalsegmap pe_loc globalsegmap
R 1655 5 36 m_globalsegmap pe_loc$sd globalsegmap
R 1656 5 37 m_globalsegmap pe_loc$p globalsegmap
R 1657 5 38 m_globalsegmap pe_loc$o globalsegmap
S 1909 23 5 0 0 0 1915 582 9052 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 sparsematrixtoxglobalsegmap_
S 1910 1 3 3 0 291 1 1909 8557 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smat
S 1911 1 3 2 0 550 1 1909 10023 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 xgsmap
S 1912 1 3 1 0 6 1 1909 5103 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1913 1 3 1 0 6 1 1909 5108 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1914 1 3 1 0 6 1 1909 9179 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comp_id
S 1915 14 5 0 0 0 1 1909 9052 10 400000 0 0 0 549 5 0 0 0 0 0 0 0 0 0 0 0 0 67 0 582 0 0 0 0 sparsematrixtoxglobalsegmap_
F 1915 5 1910 1911 1912 1913 1914
S 1916 23 5 0 0 0 1922 582 9081 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 sparsematrixtoyglobalsegmap_
S 1917 1 3 3 0 291 1 1916 8557 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smat
S 1918 1 3 2 0 438 1 1916 10030 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ygsmap
S 1919 1 3 1 0 6 1 1916 5103 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1920 1 3 1 0 6 1 1916 5108 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1921 1 3 1 0 6 1 1916 9179 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comp_id
S 1922 14 5 0 0 0 1 1916 9081 10 400000 0 0 0 555 5 0 0 0 0 0 0 0 0 0 0 0 0 211 0 582 0 0 0 0 sparsematrixtoyglobalsegmap_
F 1922 5 1917 1918 1919 1920 1921
S 1923 23 5 0 0 0 1929 582 10037 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 computesegments_
S 1924 7 3 1 0 556 1 1923 5859 20000014 10003000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 indices
S 1925 1 3 1 0 6 1 1923 10054 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 num_indices
S 1926 1 3 2 0 6 1 1923 10066 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nsegs
S 1927 7 3 0 0 559 1 1923 10072 10800014 3014 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1934 0 0 0 0 0 0 0 0 starts
S 1928 7 3 0 0 565 1 1923 10079 10800014 3014 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1938 0 0 0 0 0 0 0 0 lengths
S 1929 14 5 0 0 0 1 1923 10037 20000010 400000 0 0 0 561 5 0 0 0 0 0 0 0 0 0 0 0 0 356 0 582 0 0 0 0 computesegments_
F 1929 5 1924 1925 1926 1927 1928
S 1930 6 1 0 0 6 1 1923 8936 40800016 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_0_3
S 1931 6 1 0 0 6 1 1923 8944 40800016 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_2_3
S 1932 6 1 0 0 6 1 1923 8952 40800016 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_3_2
S 1933 6 1 0 0 6 1 1923 10087 40800016 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1110
S 1934 8 1 0 0 562 1 1923 10096 40822014 1020 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 starts$sd
S 1938 8 1 0 0 568 1 1923 10138 40822014 1020 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 lengths$sd
A 15 2 0 0 0 6 586 0 0 0 15 0 0 0 0 0 0 0 0 0
A 17 2 0 0 0 6 591 0 0 0 17 0 0 0 0 0 0 0 0 0
A 19 2 0 0 0 6 587 0 0 0 19 0 0 0 0 0 0 0 0 0
A 21 2 0 0 0 6 588 0 0 0 21 0 0 0 0 0 0 0 0 0
A 25 2 0 0 0 6 589 0 0 0 25 0 0 0 0 0 0 0 0 0
A 27 2 0 0 0 6 590 0 0 0 27 0 0 0 0 0 0 0 0 0
A 502 2 0 0 223 6 1615 0 0 0 502 0 0 0 0 0 0 0 0 0
A 503 2 0 0 0 436 1616 0 0 0 503 0 0 0 0 0 0 0 0 0
A 639 1 0 0 383 6 1932 0 0 0 0 0 0 0 0 0 0 0 0 0
A 640 1 0 0 0 6 1930 0 0 0 0 0 0 0 0 0 0 0 0 0
A 641 1 0 0 0 6 1933 0 0 0 0 0 0 0 0 0 0 0 0 0
A 642 1 0 0 0 6 1931 0 0 0 0 0 0 0 0 0 0 0 0 0
A 643 1 0 1 0 562 1934 0 0 0 0 0 0 0 0 0 0 0 0 0
A 644 10 0 0 33 6 643 1 0 0 0 0 0 0 0 0 0 0 0 0
X 1 17
A 645 10 0 0 644 6 643 4 0 0 0 0 0 0 0 0 0 0 0 0
X 1 19
A 646 10 0 0 645 6 643 7 0 0 0 0 0 0 0 0 0 0 0 0
X 1 21
A 647 4 0 0 555 6 646 0 3 0 0 0 0 2 0 0 0 0 0 0
A 648 4 0 0 0 6 645 0 647 0 0 0 0 1 0 0 0 0 0 0
A 649 10 0 0 646 6 643 10 0 0 0 0 0 0 0 0 0 0 0 0
X 1 25
A 650 10 0 0 649 6 643 13 0 0 0 0 0 0 0 0 0 0 0 0
X 1 27
A 651 1 0 1 500 568 1938 0 0 0 0 0 0 0 0 0 0 0 0 0
A 652 10 0 0 0 6 651 1 0 0 0 0 0 0 0 0 0 0 0 0
X 1 17
A 653 10 0 0 652 6 651 4 0 0 0 0 0 0 0 0 0 0 0 0
X 1 19
A 654 10 0 0 653 6 651 7 0 0 0 0 0 0 0 0 0 0 0 0
X 1 21
A 655 4 0 0 0 6 654 0 3 0 0 0 0 2 0 0 0 0 0 0
A 656 4 0 0 0 6 653 0 655 0 0 0 0 1 0 0 0 0 0 0
A 657 10 0 0 654 6 651 10 0 0 0 0 0 0 0 0 0 0 0 0
X 1 25
A 658 10 0 0 657 6 651 13 0 0 0 0 0 0 0 0 0 0 0 0
X 1 27
Z
Z
