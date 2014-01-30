V24 m_convertmaps
17 m_ConvertMaps.F90 S582 0
01/06/2014  21:00:25
use m_globalsegmap private
use m_globalmap private
use m_globalsegmap private
use m_globalmap private
enduse
D 44 24 596 192 595 7
D 75 24 691 280 688 7
D 187 18 25
S 582 24 0 0 0 6 1 0 4659 10005 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 22 0 0 0 0 0 0 m_convertmaps
S 584 23 0 0 0 8 595 582 4685 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 globalmap
S 586 23 0 0 0 8 688 582 4710 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 globalsegmap
S 590 3 0 0 0 6 1 1 0 0 0 0 0 0 0 0 17 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 595 25 1 m_globalmap globalmap
R 596 5 2 m_globalmap comp_id globalmap
R 597 5 3 m_globalmap gsize globalmap
R 598 5 4 m_globalmap lsize globalmap
R 600 5 6 m_globalmap counts globalmap
R 601 5 7 m_globalmap counts$sd globalmap
R 602 5 8 m_globalmap counts$p globalmap
R 603 5 9 m_globalmap counts$o globalmap
R 606 5 12 m_globalmap displs globalmap
R 607 5 13 m_globalmap displs$sd globalmap
R 608 5 14 m_globalmap displs$p globalmap
R 609 5 15 m_globalmap displs$o globalmap
R 688 25 1 m_globalsegmap globalsegmap
R 691 5 4 m_globalsegmap comp_id globalsegmap
R 692 5 5 m_globalsegmap gsize globalsegmap
R 697 5 10 m_globalsegmap ngseg globalsegmap
R 710 5 23 m_globalsegmap start globalsegmap
R 711 5 24 m_globalsegmap start$sd globalsegmap
R 712 5 25 m_globalsegmap start$p globalsegmap
R 713 5 26 m_globalsegmap start$o globalsegmap
R 716 5 29 m_globalsegmap length globalsegmap
R 717 5 30 m_globalsegmap length$sd globalsegmap
R 718 5 31 m_globalsegmap length$p globalsegmap
R 719 5 32 m_globalsegmap length$o globalsegmap
R 722 5 35 m_globalsegmap pe_loc globalsegmap
R 723 5 36 m_globalsegmap pe_loc$sd globalsegmap
R 724 5 37 m_globalsegmap pe_loc$p globalsegmap
R 725 5 38 m_globalsegmap pe_loc$o globalsegmap
S 977 19 0 0 0 8 1 582 6044 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 74 1 0 0 0 0 0 582 0 0 0 0 globalmaptoglobalsegmap
O 977 1 979
S 978 19 0 0 0 8 1 582 6068 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 76 1 0 0 0 0 0 582 0 0 0 0 globalsegmaptoglobalmap
O 978 1 980
S 979 27 0 0 0 8 983 582 6092 10010 0 0 0 77 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 globalmaptoglobalsegmap_
Q 979 977 0
S 980 27 0 0 0 8 987 582 6117 10010 0 0 0 78 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 globalsegmaptoglobalmap_
Q 980 978 0
S 981 3 0 0 0 187 0 1 0 0 0 0 0 0 0 6142 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 17 4d 43 54 3a 3a 6d 5f 43 6f 6e 76 65 72 74 4d 61 70
S 982 16 0 0 0 187 1 582 4976 14 440000 0 0 0 0 981 179 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 myname
S 983 23 5 0 0 0 986 582 6092 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 globalmaptoglobalsegmap_
S 984 1 3 1 0 44 1 983 5023 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gmap
S 985 1 3 2 0 75 1 983 5698 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 986 14 5 0 0 0 1 983 6092 10 400000 0 0 0 147 2 0 0 0 0 0 0 0 0 0 0 0 0 82 0 582 0 0 0 0 globalmaptoglobalsegmap_
F 986 2 984 985
S 987 23 5 0 0 0 991 582 6117 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 globalsegmaptoglobalmap_
S 988 1 3 1 0 75 1 987 5698 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 989 1 3 2 0 44 1 987 5023 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gmap
S 990 1 3 2 0 6 1 987 6160 80000014 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 status
S 991 14 5 0 0 0 1 987 6117 10 400000 0 0 0 150 3 0 0 0 0 0 0 0 0 0 0 0 0 212 0 582 0 0 0 0 globalsegmaptoglobalmap_
F 991 3 988 989 990
A 25 2 0 0 0 6 590 0 0 0 25 0 0 0 0 0 0 0 0 0
A 179 2 0 0 0 187 981 0 0 0 179 0 0 0 0 0 0 0 0 0
Z
Z
