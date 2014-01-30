V24 m_sparsematrixcomms
23 m_SparseMatrixComms.F90 S582 0
01/06/2014  21:00:27
use m_globalmap private
use m_sparsematrix private
use m_globalsegmap private
use m_globalmap private
use m_sparsematrix private
use m_globalsegmap private
enduse
D 44 18 12
D 293 24 1298 1056 1297 7
D 438 24 1626 280 1623 7
D 550 24 1626 280 1623 7
D 556 24 1298 1056 1297 7
D 593 24 1932 192 1931 7
S 582 24 0 0 0 6 1 0 4659 10005 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 25 0 0 0 0 0 0 m_sparsematrixcomms
S 583 19 0 0 0 8 1 582 4679 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1 0 0 0 0 0 582 0 0 0 0 scatterbycolumn
O 583 1 587
S 584 19 0 0 0 8 1 582 4695 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 4 1 0 0 0 0 0 582 0 0 0 0 scatterbyrow
O 584 1 588
S 585 19 0 0 0 8 1 582 4708 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 8 2 0 0 0 0 0 582 0 0 0 0 gather
O 585 2 590 589
S 586 19 0 0 0 8 1 582 4715 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 10 1 0 0 0 0 0 582 0 0 0 0 bcast
O 586 1 591
S 587 27 0 0 0 8 1912 582 4721 10010 0 0 0 305 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 scatterbycolumngsmap_
Q 587 583 0
S 588 27 0 0 0 8 1920 582 4743 10010 0 0 0 306 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 scatterbyrowgsmap_
Q 588 584 0
S 589 27 0 0 0 8 2023 582 4762 10010 0 0 0 325 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 gm_gather_
Q 589 585 0
S 590 27 0 0 0 8 2031 582 4773 10010 0 0 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 gsm_gather_
Q 590 585 0
S 591 27 0 0 0 8 2039 582 4785 10010 0 0 0 327 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 bcast_
Q 591 586 0
S 592 3 0 0 0 6 1 1 0 0 0 0 0 0 0 0 24 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 593 3 0 0 0 44 0 1 0 0 0 0 0 0 0 4792 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 24 4d 43 54 3a 3a 6d 5f 53 70 61 72 73 65 4d 61 74 72 69 78 43 6f 6d 6d 73
S 594 16 0 0 0 44 1 582 4817 14 440000 0 0 0 0 593 13 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 myname
R 1297 25 3 m_sparsematrix sparsematrix
R 1298 5 4 m_sparsematrix nrows sparsematrix
R 1299 5 5 m_sparsematrix ncols sparsematrix
R 1300 5 6 m_sparsematrix data sparsematrix
R 1301 5 7 m_sparsematrix vecinit sparsematrix
R 1303 5 9 m_sparsematrix row_s sparsematrix
R 1304 5 10 m_sparsematrix row_s$sd sparsematrix
R 1305 5 11 m_sparsematrix row_s$p sparsematrix
R 1306 5 12 m_sparsematrix row_s$o sparsematrix
R 1308 5 14 m_sparsematrix row_e sparsematrix
R 1310 5 16 m_sparsematrix row_e$sd sparsematrix
R 1311 5 17 m_sparsematrix row_e$p sparsematrix
R 1312 5 18 m_sparsematrix row_e$o sparsematrix
R 1316 5 22 m_sparsematrix tcol sparsematrix
R 1317 5 23 m_sparsematrix tcol$sd sparsematrix
R 1318 5 24 m_sparsematrix tcol$p sparsematrix
R 1319 5 25 m_sparsematrix tcol$o sparsematrix
R 1323 5 29 m_sparsematrix twgt sparsematrix
R 1324 5 30 m_sparsematrix twgt$sd sparsematrix
R 1325 5 31 m_sparsematrix twgt$p sparsematrix
R 1326 5 32 m_sparsematrix twgt$o sparsematrix
R 1328 5 34 m_sparsematrix row_max sparsematrix
R 1329 5 35 m_sparsematrix row_min sparsematrix
R 1330 5 36 m_sparsematrix tbl_end sparsematrix
R 1623 25 1 m_globalsegmap globalsegmap
R 1626 5 4 m_globalsegmap comp_id globalsegmap
R 1627 5 5 m_globalsegmap gsize globalsegmap
R 1632 5 10 m_globalsegmap ngseg globalsegmap
R 1645 5 23 m_globalsegmap start globalsegmap
R 1646 5 24 m_globalsegmap start$sd globalsegmap
R 1647 5 25 m_globalsegmap start$p globalsegmap
R 1648 5 26 m_globalsegmap start$o globalsegmap
R 1651 5 29 m_globalsegmap length globalsegmap
R 1652 5 30 m_globalsegmap length$sd globalsegmap
R 1653 5 31 m_globalsegmap length$p globalsegmap
R 1654 5 32 m_globalsegmap length$o globalsegmap
R 1657 5 35 m_globalsegmap pe_loc globalsegmap
R 1658 5 36 m_globalsegmap pe_loc$sd globalsegmap
R 1659 5 37 m_globalsegmap pe_loc$p globalsegmap
R 1660 5 38 m_globalsegmap pe_loc$o globalsegmap
S 1912 23 5 0 0 0 1919 582 4721 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 scatterbycolumngsmap_
S 1913 1 3 1 0 550 1 1912 10007 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 columngsmap
S 1914 1 3 3 0 556 1 1912 10019 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmat
S 1915 1 3 2 0 556 1 1912 10025 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 lsmat
S 1916 1 3 1 0 6 1 1912 5286 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1917 1 3 1 0 6 1 1912 5291 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1918 1 3 2 0 6 1 1912 5296 80000014 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1919 14 5 0 0 0 1 1912 4721 10 400000 0 0 0 549 6 0 0 0 0 0 0 0 0 0 0 0 0 84 0 582 0 0 0 0 scatterbycolumngsmap_
F 1919 6 1913 1914 1915 1916 1917 1918
S 1920 23 5 0 0 0 1927 582 4743 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 scatterbyrowgsmap_
S 1921 1 3 1 0 438 1 1920 10031 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rowgsmap
S 1922 1 3 3 0 293 1 1920 10019 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmat
S 1923 1 3 2 0 293 1 1920 10025 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 lsmat
S 1924 1 3 1 0 6 1 1920 5286 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1925 1 3 1 0 6 1 1920 5291 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1926 1 3 2 0 6 1 1920 5296 80000014 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1927 14 5 0 0 0 1 1920 4743 10 400000 0 0 0 556 6 0 0 0 0 0 0 0 0 0 0 0 0 249 0 582 0 0 0 0 scatterbyrowgsmap_
F 1927 6 1921 1922 1923 1924 1925 1926
R 1931 25 1 m_globalmap globalmap
R 1932 5 2 m_globalmap comp_id globalmap
R 1933 5 3 m_globalmap gsize globalmap
R 1934 5 4 m_globalmap lsize globalmap
R 1936 5 6 m_globalmap counts globalmap
R 1937 5 7 m_globalmap counts$sd globalmap
R 1938 5 8 m_globalmap counts$p globalmap
R 1939 5 9 m_globalmap counts$o globalmap
R 1942 5 12 m_globalmap displs globalmap
R 1943 5 13 m_globalmap displs$sd globalmap
R 1944 5 14 m_globalmap displs$p globalmap
R 1945 5 15 m_globalmap displs$o globalmap
S 2023 23 5 0 0 0 2030 582 4762 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gm_gather_
S 2024 1 3 1 0 293 1 2023 10025 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 lsmat
S 2025 1 3 2 0 293 1 2023 10019 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmat
S 2026 1 3 1 0 593 1 2023 10263 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gmap
S 2027 1 3 1 0 6 1 2023 5286 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 2028 1 3 1 0 6 1 2023 5291 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 2029 1 3 2 0 6 1 2023 5296 80000014 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 2030 14 5 0 0 0 1 2023 4762 10 400000 0 0 0 599 6 0 0 0 0 0 0 0 0 0 0 0 0 406 0 582 0 0 0 0 gm_gather_
F 2030 6 2024 2025 2026 2027 2028 2029
S 2031 23 5 0 0 0 2038 582 4773 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsm_gather_
S 2032 1 3 1 0 293 1 2031 10025 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 lsmat
S 2033 1 3 2 0 293 1 2031 10019 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmat
S 2034 1 3 1 0 438 1 2031 9693 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 2035 1 3 1 0 6 1 2031 5286 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 2036 1 3 1 0 6 1 2031 5291 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 2037 1 3 2 0 6 1 2031 5296 80000014 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 2038 14 5 0 0 0 1 2031 4773 10 400000 0 0 0 606 6 0 0 0 0 0 0 0 0 0 0 0 0 500 0 582 0 0 0 0 gsm_gather_
F 2038 6 2032 2033 2034 2035 2036 2037
S 2039 23 5 0 0 0 2044 582 4785 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 bcast_
S 2040 1 3 3 0 293 1 2039 8681 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smat
S 2041 1 3 1 0 6 1 2039 5286 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 2042 1 3 1 0 6 1 2039 5291 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 2043 1 3 2 0 6 1 2039 5296 80000014 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 2044 14 5 0 0 0 1 2039 4785 10 400000 0 0 0 613 4 0 0 0 0 0 0 0 0 0 0 0 0 595 0 582 0 0 0 0 bcast_
F 2044 4 2040 2041 2042 2043
A 12 2 0 0 0 6 592 0 0 0 12 0 0 0 0 0 0 0 0 0
A 13 2 0 0 0 44 593 0 0 0 13 0 0 0 0 0 0 0 0 0
Z
Z
