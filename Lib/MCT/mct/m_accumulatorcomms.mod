V24 m_accumulatorcomms
22 m_AccumulatorComms.F90 S582 0
01/06/2014  21:00:22
use m_globalmap private
use m_globalsegmap private
use m_accumulator private
use m_globalmap private
use m_globalsegmap private
use m_accumulator private
enduse
D 44 18 12
D 46 24 604 192 603 7
D 322 24 1391 808 1390 7
D 421 24 604 192 603 7
D 427 24 1391 808 1390 7
D 433 24 1651 280 1648 7
D 545 24 1651 280 1648 7
S 582 24 0 0 0 6 1 0 4659 10005 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 29 0 0 0 0 0 0 m_accumulatorcomms
S 583 19 0 0 0 8 1 582 4678 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 4 2 0 0 0 0 0 582 0 0 0 0 gather
O 583 2 587 586
S 584 19 0 0 0 8 1 582 4685 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 8 2 0 0 0 0 0 582 0 0 0 0 scatter
O 584 2 589 588
S 585 19 0 0 0 8 1 582 4693 4000 0 0 0 0 0 0 0 0 0 0 0 0 0 10 1 0 0 0 0 0 582 0 0 0 0 bcast
O 585 1 590
S 586 27 0 0 0 8 1638 582 4699 10010 0 0 0 253 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 gm_gather_
Q 586 583 0
S 587 27 0 0 0 8 1937 582 4710 10010 0 0 0 306 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 gsm_gather_
Q 587 583 0
S 588 27 0 0 0 8 1945 582 4722 10010 0 0 0 307 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 gm_scatter_
Q 588 584 0
S 589 27 0 0 0 8 1953 582 4734 10010 0 0 0 308 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 gsm_scatter_
Q 589 584 0
S 590 27 0 0 0 8 1961 582 4747 10010 0 0 0 309 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 bcast_
Q 590 585 0
S 591 3 0 0 0 6 1 1 0 0 0 0 0 0 0 0 23 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 592 3 0 0 0 44 0 1 0 0 0 0 0 0 0 4754 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 23 4d 43 54 3a 3a 6d 5f 41 63 63 75 6d 75 6c 61 74 6f 72 43 6f 6d 6d 73
S 593 16 0 0 0 44 1 582 4778 14 440000 0 0 0 0 592 13 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 myname
R 603 25 1 m_globalmap globalmap
R 604 5 2 m_globalmap comp_id globalmap
R 605 5 3 m_globalmap gsize globalmap
R 606 5 4 m_globalmap lsize globalmap
R 608 5 6 m_globalmap counts globalmap
R 609 5 7 m_globalmap counts$sd globalmap
R 610 5 8 m_globalmap counts$p globalmap
R 611 5 9 m_globalmap counts$o globalmap
R 614 5 12 m_globalmap displs globalmap
R 615 5 13 m_globalmap displs$sd globalmap
R 616 5 14 m_globalmap displs$p globalmap
R 617 5 15 m_globalmap displs$o globalmap
R 1390 25 6 m_accumulator accumulator
R 1391 5 7 m_accumulator num_steps accumulator
R 1392 5 8 m_accumulator steps_done accumulator
R 1394 5 10 m_accumulator iaction accumulator
R 1395 5 11 m_accumulator iaction$sd accumulator
R 1396 5 12 m_accumulator iaction$p accumulator
R 1397 5 13 m_accumulator iaction$o accumulator
R 1400 5 16 m_accumulator raction accumulator
R 1401 5 17 m_accumulator raction$sd accumulator
R 1402 5 18 m_accumulator raction$p accumulator
R 1403 5 19 m_accumulator raction$o accumulator
R 1405 5 21 m_accumulator data accumulator
S 1638 23 5 0 0 0 1645 582 4699 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gm_gather_
S 1639 1 3 1 0 427 1 1638 8293 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ic
S 1640 1 3 2 0 427 1 1638 8296 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 oc
S 1641 1 3 1 0 421 1 1638 5100 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gmap
S 1642 1 3 1 0 6 1 1638 5117 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1643 1 3 1 0 6 1 1638 5108 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1644 1 3 2 0 6 1 1638 5212 80000014 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1645 14 5 0 0 0 1 1638 4699 10 400000 0 0 0 449 6 0 0 0 0 0 0 0 0 0 0 0 0 95 0 582 0 0 0 0 gm_gather_
F 1645 6 1639 1640 1641 1642 1643 1644
R 1648 25 1 m_globalsegmap globalsegmap
R 1651 5 4 m_globalsegmap comp_id globalsegmap
R 1652 5 5 m_globalsegmap gsize globalsegmap
R 1657 5 10 m_globalsegmap ngseg globalsegmap
R 1670 5 23 m_globalsegmap start globalsegmap
R 1671 5 24 m_globalsegmap start$sd globalsegmap
R 1672 5 25 m_globalsegmap start$p globalsegmap
R 1673 5 26 m_globalsegmap start$o globalsegmap
R 1676 5 29 m_globalsegmap length globalsegmap
R 1677 5 30 m_globalsegmap length$sd globalsegmap
R 1678 5 31 m_globalsegmap length$p globalsegmap
R 1679 5 32 m_globalsegmap length$o globalsegmap
R 1682 5 35 m_globalsegmap pe_loc globalsegmap
R 1683 5 36 m_globalsegmap pe_loc$sd globalsegmap
R 1684 5 37 m_globalsegmap pe_loc$p globalsegmap
R 1685 5 38 m_globalsegmap pe_loc$o globalsegmap
S 1937 23 5 0 0 0 1944 582 4710 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsm_gather_
S 1938 1 3 1 0 322 1 1937 8293 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ic
S 1939 1 3 2 0 322 1 1937 8296 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 oc
S 1940 1 3 1 0 545 1 1937 8816 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 1941 1 3 1 0 6 1 1937 5117 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1942 1 3 1 0 6 1 1937 5108 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1943 1 3 2 0 6 1 1937 5212 80000014 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1944 14 5 0 0 0 1 1937 4710 10 400000 0 0 0 565 6 0 0 0 0 0 0 0 0 0 0 0 0 199 0 582 0 0 0 0 gsm_gather_
F 1944 6 1938 1939 1940 1941 1942 1943
S 1945 23 5 0 0 0 1952 582 4722 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gm_scatter_
S 1946 1 3 1 0 322 1 1945 8293 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ic
S 1947 1 3 2 0 322 1 1945 8296 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 oc
S 1948 1 3 1 0 46 1 1945 5100 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gmap
S 1949 1 3 1 0 6 1 1945 5117 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1950 1 3 1 0 6 1 1945 5108 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1951 1 3 2 0 6 1 1945 5212 80000014 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1952 14 5 0 0 0 1 1945 4722 10 400000 0 0 0 572 6 0 0 0 0 0 0 0 0 0 0 0 0 299 0 582 0 0 0 0 gm_scatter_
F 1952 6 1946 1947 1948 1949 1950 1951
S 1953 23 5 0 0 0 1960 582 4734 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsm_scatter_
S 1954 1 3 1 0 322 1 1953 8293 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ic
S 1955 1 3 2 0 322 1 1953 8296 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 oc
S 1956 1 3 1 0 433 1 1953 8816 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 1957 1 3 1 0 6 1 1953 5117 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1958 1 3 1 0 6 1 1953 5108 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1959 1 3 2 0 6 1 1953 5212 80000014 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1960 14 5 0 0 0 1 1953 4734 10 400000 0 0 0 579 6 0 0 0 0 0 0 0 0 0 0 0 0 407 0 582 0 0 0 0 gsm_scatter_
F 1960 6 1954 1955 1956 1957 1958 1959
S 1961 23 5 0 0 0 1966 582 4747 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 bcast_
S 1962 1 3 3 0 322 1 1961 8071 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ac
S 1963 1 3 1 0 6 1 1961 5117 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1964 1 3 1 0 6 1 1961 5108 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1965 1 3 2 0 6 1 1961 5212 80000014 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1966 14 5 0 0 0 1 1961 4747 10 400000 0 0 0 586 4 0 0 0 0 0 0 0 0 0 0 0 0 509 0 582 0 0 0 0 bcast_
F 1966 4 1962 1963 1964 1965
S 1967 23 5 0 0 0 1972 582 9112 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 bcastp_
S 1968 1 3 3 0 322 1 1967 8071 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ac
S 1969 1 3 1 0 6 1 1967 5117 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1970 1 3 1 0 6 1 1967 5108 14 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1971 1 3 2 0 6 1 1967 5212 80000014 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1972 14 5 0 0 0 1 1967 9112 10 400000 0 0 0 591 4 0 0 0 0 0 0 0 0 0 0 0 0 601 0 582 0 0 0 0 bcastp_
F 1972 4 1968 1969 1970 1971
A 12 2 0 0 0 6 591 0 0 0 12 0 0 0 0 0 0 0 0 0
A 13 2 0 0 0 44 592 0 0 0 13 0 0 0 0 0 0 0 0 0
Z
Z
