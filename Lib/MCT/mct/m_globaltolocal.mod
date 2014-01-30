V24 m_globaltolocal
19 m_GlobalToLocal.F90 S582 0
01/06/2014  21:00:25
use m_sparsematrix private
use m_navigator private
use m_globalsegmap private
use m_globalmap private
use m_sparsematrix private
use m_navigator private
use m_globalsegmap private
use m_globalmap private
enduse
D 44 18 12
D 46 24 606 280 603 7
D 158 24 606 280 603 7
D 164 21 6 1 156 162 0 1 0 0 1
 157 160 161 157 160 158
D 167 21 6 1 0 18 0 0 0 0 0
 0 18 0 3 18 0
D 170 21 6 1 164 170 0 1 0 0 1
 165 168 169 165 168 166
D 173 21 6 1 0 18 0 0 0 0 0
 0 18 0 3 18 0
D 176 21 6 1 171 174 1 1 0 0 1
 3 172 3 3 172 173
D 179 21 6 1 175 178 1 1 0 0 1
 3 176 3 3 176 177
D 213 24 931 192 930 7
D 254 24 1031 184 1030 7
D 652 24 1817 1056 1816 7
S 582 24 0 0 0 6 1 0 4659 10005 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 25 0 0 0 0 0 0 m_globaltolocal
S 583 19 0 0 0 8 1 582 4675 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 10 2 0 0 0 0 0 582 0 0 0 0 globaltolocalindex
O 583 2 590 589
S 584 19 0 0 0 8 1 582 4694 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 6 3 0 0 0 0 0 582 0 0 0 0 globaltolocalindices
O 584 3 588 587 586
S 585 19 0 0 0 8 1 582 4715 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 12 1 0 0 0 0 0 582 0 0 0 0 globaltolocalmatrix
O 585 1 591
S 586 27 0 0 0 8 892 582 4735 10010 0 0 0 65 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 globalsegmaptoindices_
Q 586 584 0
S 587 27 0 0 0 8 1115 582 4758 10010 0 0 0 107 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 globalsegmaptonavigator_
Q 587 584 0
S 588 27 0 0 0 8 912 582 4783 10010 0 0 0 67 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 globalsegmaptoindexarr_
Q 588 584 0
S 589 27 0 0 0 8 906 582 4807 10010 0 0 0 66 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 globalsegmaptoindex_
Q 589 583 0
S 590 27 0 0 0 8 1022 582 4828 10010 0 0 0 86 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 globalmaptoindex_
Q 590 583 0
S 591 27 0 0 0 8 2140 582 4846 10010 0 0 0 350 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 globalsegmaptolocalmatrix_
Q 591 585 0
S 592 3 0 0 0 6 1 1 0 0 0 0 0 0 0 0 20 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 593 3 0 0 0 44 0 1 0 0 0 0 0 0 0 4873 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 20 4d 43 54 3a 3a 6d 5f 47 6c 6f 62 61 6c 54 6f 4c 6f 63 61 6c
S 594 16 0 0 0 44 1 582 4894 14 440000 0 0 0 0 593 13 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 myname
S 596 3 0 0 0 6 1 1 0 0 0 0 0 0 0 0 18 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 597 3 0 0 0 6 1 1 0 0 0 0 0 0 0 0 13 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 598 3 0 0 0 6 1 1 0 0 0 0 0 0 0 0 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 599 3 0 0 0 6 1 1 0 0 0 0 0 0 0 0 17 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 600 3 0 0 0 6 1 1 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 601 3 0 0 0 6 1 1 0 0 0 0 0 0 0 0 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 603 25 1 m_globalsegmap globalsegmap
R 606 5 4 m_globalsegmap comp_id globalsegmap
R 607 5 5 m_globalsegmap gsize globalsegmap
R 612 5 10 m_globalsegmap ngseg globalsegmap
R 625 5 23 m_globalsegmap start globalsegmap
R 626 5 24 m_globalsegmap start$sd globalsegmap
R 627 5 25 m_globalsegmap start$p globalsegmap
R 628 5 26 m_globalsegmap start$o globalsegmap
R 631 5 29 m_globalsegmap length globalsegmap
R 632 5 30 m_globalsegmap length$sd globalsegmap
R 633 5 31 m_globalsegmap length$p globalsegmap
R 634 5 32 m_globalsegmap length$o globalsegmap
R 637 5 35 m_globalsegmap pe_loc globalsegmap
R 638 5 36 m_globalsegmap pe_loc$sd globalsegmap
R 639 5 37 m_globalsegmap pe_loc$p globalsegmap
R 640 5 38 m_globalsegmap pe_loc$o globalsegmap
S 892 23 5 0 0 0 897 582 4735 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 globalsegmaptoindices_
S 893 1 3 1 0 158 1 892 5565 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 894 1 3 1 0 6 1 892 5781 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 895 7 3 0 0 164 1 892 5117 10800014 3014 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 898 0 0 0 0 0 0 0 0 start
S 896 7 3 0 0 170 1 892 3870 10800014 3014 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 902 0 0 0 0 0 0 0 0 length
S 897 14 5 0 0 0 1 892 4735 10 400000 0 0 0 111 4 0 0 0 0 0 0 0 0 0 0 0 0 98 0 582 0 0 0 0 globalsegmaptoindices_
F 897 4 893 894 895 896
S 898 8 1 0 0 167 1 892 5952 40822014 1020 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 start$sd1
S 902 8 1 0 0 173 1 892 5994 40822014 1020 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 length$sd5
S 906 23 5 0 0 6 910 582 4807 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 globalsegmaptoindex_
S 907 1 3 1 0 46 1 906 5565 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 908 1 3 1 0 6 1 906 5786 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 i_g
S 909 1 3 1 0 6 1 906 5781 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 910 14 5 0 0 6 1 906 4807 14 400000 0 0 0 116 3 0 0 911 0 0 0 0 0 0 0 0 0 201 0 582 0 0 0 0 globalsegmaptoindex_
F 910 3 907 908 909
S 911 1 3 0 0 6 1 906 4807 14 1003000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 globalsegmaptoindex_
S 912 23 5 0 0 0 918 582 4783 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 globalsegmaptoindexarr_
S 913 1 3 1 0 46 1 912 5565 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 914 7 3 1 0 176 1 912 6040 20000014 10003000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 i_global
S 915 7 3 2 0 179 1 912 6049 20000014 10003000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 i_local
S 916 1 3 1 0 6 1 912 2037 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nindex
S 917 1 3 1 0 6 1 912 5781 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 918 14 5 0 0 0 1 912 4783 20000010 400000 0 0 0 120 5 0 0 0 0 0 0 0 0 0 0 0 0 320 0 582 0 0 0 0 globalsegmaptoindexarr_
F 918 5 913 914 915 916 917
S 919 6 1 0 0 6 1 912 5590 40800016 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_0_1
S 920 6 1 0 0 6 1 912 5598 40800016 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_2_1
S 921 6 1 0 0 6 1 912 5606 40800016 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_3
S 922 6 1 0 0 6 1 912 6057 40800016 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_652
S 923 6 1 0 0 6 1 912 5620 40800016 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_4
S 924 6 1 0 0 6 1 912 5626 40800016 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_6
S 925 6 1 0 0 6 1 912 5632 40800016 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_7
S 926 6 1 0 0 6 1 912 6065 40800016 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_659
R 930 25 1 m_globalmap globalmap
R 931 5 2 m_globalmap comp_id globalmap
R 932 5 3 m_globalmap gsize globalmap
R 933 5 4 m_globalmap lsize globalmap
R 935 5 6 m_globalmap counts globalmap
R 936 5 7 m_globalmap counts$sd globalmap
R 937 5 8 m_globalmap counts$p globalmap
R 938 5 9 m_globalmap counts$o globalmap
R 941 5 12 m_globalmap displs globalmap
R 942 5 13 m_globalmap displs$sd globalmap
R 943 5 14 m_globalmap displs$p globalmap
R 944 5 15 m_globalmap displs$o globalmap
S 1022 23 5 0 0 6 1026 582 4828 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 globalmaptoindex_
S 1023 1 3 1 0 213 1 1022 6296 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gmap
S 1024 1 3 1 0 6 1 1022 5786 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 i_g
S 1025 1 3 1 0 6 1 1022 5781 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1026 14 5 0 0 6 1 1022 4828 14 400000 0 0 0 162 3 0 0 1027 0 0 0 0 0 0 0 0 0 476 0 582 0 0 0 0 globalmaptoindex_
F 1026 3 1023 1024 1025
S 1027 1 3 0 0 6 1 1022 4828 14 1003000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 globalmaptoindex_
R 1030 25 1 m_navigator navigator
R 1031 5 2 m_navigator numsegments navigator
R 1032 5 3 m_navigator vectorlength navigator
R 1034 5 5 m_navigator displs navigator
R 1035 5 6 m_navigator displs$sd navigator
R 1036 5 7 m_navigator displs$p navigator
R 1037 5 8 m_navigator displs$o navigator
R 1040 5 11 m_navigator counts navigator
R 1041 5 12 m_navigator counts$sd navigator
R 1042 5 13 m_navigator counts$p navigator
R 1043 5 14 m_navigator counts$o navigator
S 1115 23 5 0 0 0 1119 582 4758 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 globalsegmaptonavigator_
S 1116 1 3 1 0 46 1 1115 5565 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 1117 1 3 1 0 6 1 1115 5781 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1118 1 3 2 0 254 1 1115 6672 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 onav
S 1119 14 5 0 0 0 1 1115 4758 10 400000 0 0 0 198 3 0 0 0 0 0 0 0 0 0 0 0 0 551 0 582 0 0 0 0 globalsegmaptonavigator_
F 1119 3 1116 1117 1118
R 1816 25 3 m_sparsematrix sparsematrix
R 1817 5 4 m_sparsematrix nrows sparsematrix
R 1818 5 5 m_sparsematrix ncols sparsematrix
R 1819 5 6 m_sparsematrix data sparsematrix
R 1820 5 7 m_sparsematrix vecinit sparsematrix
R 1822 5 9 m_sparsematrix row_s sparsematrix
R 1823 5 10 m_sparsematrix row_s$sd sparsematrix
R 1824 5 11 m_sparsematrix row_s$p sparsematrix
R 1825 5 12 m_sparsematrix row_s$o sparsematrix
R 1827 5 14 m_sparsematrix row_e sparsematrix
R 1829 5 16 m_sparsematrix row_e$sd sparsematrix
R 1830 5 17 m_sparsematrix row_e$p sparsematrix
R 1831 5 18 m_sparsematrix row_e$o sparsematrix
R 1835 5 22 m_sparsematrix tcol sparsematrix
R 1836 5 23 m_sparsematrix tcol$sd sparsematrix
R 1837 5 24 m_sparsematrix tcol$p sparsematrix
R 1838 5 25 m_sparsematrix tcol$o sparsematrix
R 1842 5 29 m_sparsematrix twgt sparsematrix
R 1843 5 30 m_sparsematrix twgt$sd sparsematrix
R 1844 5 31 m_sparsematrix twgt$p sparsematrix
R 1845 5 32 m_sparsematrix twgt$o sparsematrix
R 1847 5 34 m_sparsematrix row_max sparsematrix
R 1848 5 35 m_sparsematrix row_min sparsematrix
R 1849 5 36 m_sparsematrix tbl_end sparsematrix
S 2140 23 5 0 0 0 2145 582 4846 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 globalsegmaptolocalmatrix_
S 2141 1 3 3 0 652 1 2140 10365 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smat
S 2142 1 3 1 0 46 1 2140 5565 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 2143 1 3 1 0 28 1 2140 10804 14 43000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rcflag
S 2144 1 3 1 0 6 1 2140 5781 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 2145 14 5 0 0 0 1 2140 4846 10 400000 0 0 0 640 4 0 0 0 0 0 0 0 0 0 0 0 0 628 0 582 0 0 0 0 globalsegmaptolocalmatrix_
F 2145 4 2141 2142 2143 2144
A 12 2 0 0 0 6 592 0 0 0 12 0 0 0 0 0 0 0 0 0
A 13 2 0 0 0 44 593 0 0 0 13 0 0 0 0 0 0 0 0 0
A 15 2 0 0 0 6 598 0 0 0 15 0 0 0 0 0 0 0 0 0
A 18 2 0 0 0 6 596 0 0 0 18 0 0 0 0 0 0 0 0 0
A 20 2 0 0 0 6 601 0 0 0 20 0 0 0 0 0 0 0 0 0
A 22 2 0 0 0 6 597 0 0 0 22 0 0 0 0 0 0 0 0 0
A 27 2 0 0 0 6 599 0 0 0 27 0 0 0 0 0 0 0 0 0
A 29 2 0 0 0 6 600 0 0 0 29 0 0 0 0 0 0 0 0 0
A 155 1 0 1 0 167 898 0 0 0 0 0 0 0 0 0 0 0 0 0
A 156 10 0 0 0 6 155 1 0 0 0 0 0 0 0 0 0 0 0 0
X 1 20
A 157 10 0 0 156 6 155 4 0 0 0 0 0 0 0 0 0 0 0 0
X 1 22
A 158 10 0 0 157 6 155 7 0 0 0 0 0 0 0 0 0 0 0 0
X 1 15
A 159 4 0 0 0 6 158 0 3 0 0 0 0 2 0 0 0 0 0 0
A 160 4 0 0 7 6 157 0 159 0 0 0 0 1 0 0 0 0 0 0
A 161 10 0 0 158 6 155 10 0 0 0 0 0 0 0 0 0 0 0 0
X 1 27
A 162 10 0 0 161 6 155 13 0 0 0 0 0 0 0 0 0 0 0 0
X 1 29
A 163 1 0 1 0 173 902 0 0 0 0 0 0 0 0 0 0 0 0 0
A 164 10 0 0 0 6 163 1 0 0 0 0 0 0 0 0 0 0 0 0
X 1 20
A 165 10 0 0 164 6 163 4 0 0 0 0 0 0 0 0 0 0 0 0
X 1 22
A 166 10 0 0 165 6 163 7 0 0 0 0 0 0 0 0 0 0 0 0
X 1 15
A 167 4 0 0 0 6 166 0 3 0 0 0 0 2 0 0 0 0 0 0
A 168 4 0 0 0 6 165 0 167 0 0 0 0 1 0 0 0 0 0 0
A 169 10 0 0 166 6 163 10 0 0 0 0 0 0 0 0 0 0 0 0
X 1 27
A 170 10 0 0 169 6 163 13 0 0 0 0 0 0 0 0 0 0 0 0
X 1 29
A 171 1 0 0 0 6 921 0 0 0 0 0 0 0 0 0 0 0 0 0
A 172 1 0 0 0 6 919 0 0 0 0 0 0 0 0 0 0 0 0 0
A 173 1 0 0 0 6 922 0 0 0 0 0 0 0 0 0 0 0 0 0
A 174 1 0 0 0 6 920 0 0 0 0 0 0 0 0 0 0 0 0 0
A 175 1 0 0 0 6 925 0 0 0 0 0 0 0 0 0 0 0 0 0
A 176 1 0 0 0 6 923 0 0 0 0 0 0 0 0 0 0 0 0 0
A 177 1 0 0 0 6 926 0 0 0 0 0 0 0 0 0 0 0 0 0
A 178 1 0 0 0 6 924 0 0 0 0 0 0 0 0 0 0 0 0 0
Z
Z
